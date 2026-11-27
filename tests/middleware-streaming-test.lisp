;;;; SESSION-HANDLER and Clack streaming responses: FINISH-RESPONSE runs
;;;; through MAP-RESPONSE, so the commit happens when the app calls its
;;;; responder, before the body; later writes signal SESSION-WRITE-ERROR.

(in-package #:session-protocol/middleware/tests)

(deftest streaming-commits-when-the-app-responds
  (with-world ()
    (let* ((app (session-app (streaming-app (setter "k" 1) #'ignore-session)))
           (puts-at-first-chunk nil)
           (*on-first-chunk* (lambda () (setf puts-at-first-chunk (second (calls *cache*)))))
           (response (funcall app (make-env))))
      (ok (functionp response) "a streaming response stays one through the handler")
      (ok (equal '(0 0 0) (calls *cache*)) "begun, but nothing committed before the server runs it")
      (multiple-value-bind (head chunks) (serve-streaming response)
        (ok (= 200 (first head)))
        (let ((id (cookie-id head)))
          (ok (valid-session-id-p id) "the Set-Cookie is in the head the server gets")
          (ok (json-equal (table "k" 1) (stored-data id))))
        (ok (eql 1 puts-at-first-chunk) "saved before the first chunk is written")
        (ok (equal '("data: 1" "data: 2") chunks) "the body is untouched")
        (ok (equal "text/event-stream" (getf (second head) :content-type)))))))

(deftest streaming-writes-after-the-responder-signal
  (with-world ()
    (let* ((late-error nil)
           (app (session-app
                 (streaming-app (setter "k" 1)
                                (lambda (s)
                                  (setf late-error
                                        (capture (session-write-error)
                                          (setf (session-value s "late") 2)))
                                  (handler-bind ((session-write-error #'continue))
                                    (setf (session-value s "late2") 3))))))
           (b (make-browser)))
      (hit-streaming b app)
      (ok (typep late-error 'session-write-error)
          "a change after the responder call signals: the session is committed")
      (ok (valid-session-id-p (browser-sid b)))
      (incf *now* 1)
      (ok (equal '(1 nil nil)
                 (hit b (visitor-app)
                      :do (lambda (s) (list (session-value s "k")
                                            (session-value s "late")
                                            (session-value s "late2")))))
          "nothing after the commit was persisted"))))

(deftest streaming-app-that-never-responds-commits-nothing
  (with-world ()
    (let* ((app (session-app (lambda (env)
                               (setf (session-value (env-session env) "k") 1)
                               (lambda (responder) (declare (ignore responder)) nil))))
           (handed (serve-streaming (funcall app (make-env)))))
      (ok (null handed) "the responder was never called")
      (ok (equal '(0 0 0) (calls *cache*)) "no commit"))))

(deftest streaming-read-only-hands-the-apps-head-through
  (with-world ()
    (let* ((head (list 200 (list :content-type "text/event-stream")))
           (app (session-app (streaming-app #'ignore-session #'ignore-session :head head))))
      (ok (eq head (serve-streaming (funcall app (make-env))))
          "no Set-Cookie: the app's head as it is")
      (ok (equal '(0 0 0) (calls *cache*))))))

(deftest streaming-on-a-loaded-session
  (with-world ()
    (let* ((b (started (visitor-app) "n" 1))
           (id (browser-sid b))
           (app (session-app (streaming-app (counter "n") #'ignore-session))))
      (incf *now* 120)
      (let ((head (hit-streaming b app)))
        (ok (equal id (cookie-id head)) "the same session")
        (ok (eql 2 (gethash "n" (stored-data id))))))))

(deftest streaming-returns-the-servers-writer
  (with-world ()
    (let* ((server-writer (lambda (chunk &key close) (declare (ignore chunk close)) nil))
           (got nil)
           (app (session-app (lambda (env)
                               (setf (session-value (env-session env) "k") 1)
                               (lambda (responder)
                                 (setf got (funcall responder
                                                    (list 200 (list :content-type "text/plain")))))))))
      (funcall (funcall app (make-env)) (lambda (head) (declare (ignore head)) server-writer))
      (ok (eq server-writer got) "the app gets what the server's responder returned"))))

(deftest-parametrize streaming-head-through-other-mixins
    ((mixins) :ids ("session-outside" "session-inside" "session-between")
     ('(session-handler timing-handler response-headers-handler clack-handler))
     ('(timing-handler response-headers-handler session-handler clack-handler))
     ('(response-headers-handler session-handler timing-handler clack-handler)))
  (with-world ()
    (let* ((reported '())
           (app (session-app
                 (streaming-app (setter "k" 1) #'ignore-session
                                :head (list 200 (list :content-type "text/event-stream"
                                                      :set-cookie "theme=dark")))
                 :mixins mixins
                 :initargs (list :response-headers '(:x-frame-options "DENY"
                                                     :content-type "text/plain")
                                 :timing-report (lambda (request response seconds)
                                                  (declare (ignore request seconds))
                                                  (push response reported)))))
           (b (make-browser)))
      (multiple-value-bind (head chunks) (hit-streaming b app)
        (ok (equal '(:content-type :set-cookie :set-cookie :x-frame-options)
                   (header-names head))
            "the app's headers, the session cookie, then the added headers")
        (ok (equal "text/event-stream" (getf (second head) :content-type)))
        (ok (equal (list "theme=dark" (session-cookie head)) (set-cookies head)))
        (ok (valid-session-id-p (browser-sid b)))
        (ok (equal '("data: 1" "data: 2") chunks))
        (ok (= 1 (length reported)) "one timing report")
        (ok (session-cookie (first reported))
            "timing saw the head with the session cookie, wherever it sits")))))

(deftest streaming-store-failure-at-the-responder
  ;; The commit of a streamed response runs in the responder call, outside
  ;; HANDLE: a store failure propagates from there with S0's restarts.
  (with-world ()
    (let ((app (session-app (streaming-app (setter "k" 1) #'ignore-session))))
      (let ((response (funcall app (make-env))))
        (fail-next *cache* :put)
        (let ((c (capture (session-store-error) (serve-streaming response))))
          (ok (typep c 'session-store-error) "it propagates out of the responder call")
          (ok (eq :save (session-store-error-operation c)))))
      (let ((response (funcall app (make-env))))
        (fail-next *cache* :put)
        (let ((head (handler-bind ((session-store-error #'continue))
                      (serve-streaming response))))
          (ok (valid-session-id-p (cookie-id head)) "CONTINUE: the cookie is still sent")
          (ok (null (peek (cookie-id head))) "and nothing is stored"))))))
