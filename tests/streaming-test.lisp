;;;; Streaming Clack responses: a function of a responder. The commit runs
;;;; when the app calls its responder, before any body is written.

(in-package #:session-protocol/tests)

(defvar *on-first-chunk* nil "Called by SERVE-STREAMING before the first chunk.")

(defun serve-streaming (response)
  "Run a streaming RESPONSE as a Clack server does. Returns (values the list
handed to the server's responder, the chunks written)."
  (let ((handed nil) (chunks '()) (first t))
    (funcall response
             (lambda (list)
               (setf handed list)
               (if (cddr list)
                   (progn (setf chunks (reverse (third list))) nil)
                   (lambda (chunk &key close)
                     (declare (ignore close))
                     (when (and first *on-first-chunk*)
                       (funcall *on-first-chunk*))
                     (setf first nil)
                     (push chunk chunks)))))
    (values handed (reverse chunks))))

(defun streaming-app (before after &key (head '(200 (:content-type "text/event-stream"))))
  "An app that calls BEFORE on the session, answers a streaming response that
hands HEAD to its responder, then calls AFTER on the session and writes two
chunks."
  (lambda (env)
    (let ((s (env-session env)))
      (funcall before s)
      (lambda (responder)
        (let ((writer (funcall responder head)))
          (funcall after s)
          (when writer
            (funcall writer "data: 1")
            (funcall writer "data: 2" :close t)))))))

(defun ignore-session (s) (declare (ignore s)) nil)

(deftest streaming-commits-when-the-app-responds
  (with-world ()
    (let* ((puts-at-first-chunk nil)
           (*on-first-chunk* (lambda () (setf puts-at-first-chunk (cache-puts *cache*))))
           (response (call-app (streaming-app (lambda (s) (setf (session-value s "k") 1))
                                              #'ignore-session)
                               (make-env))))
      (ok (functionp response) "a streaming response stays one")
      (ok (equal '(0 0 0) (calls *cache*)) "nothing committed before the server runs it")
      (multiple-value-bind (handed chunks) (serve-streaming response)
        (ok (= 200 (first handed)))
        (let ((id (cookie-id handed)))
          (ok (valid-session-id-p id) "the Set-Cookie is in the headers the server gets")
          (ok (json-equal (table "k" 1) (stored-data id))))
        (ok (eql 1 puts-at-first-chunk) "saved before the first chunk is written")
        (ok (equal '("data: 1" "data: 2") chunks) "the body is untouched")
        (ok (equal "text/event-stream" (getf (second handed) :content-type)))))))

(deftest streaming-writes-after-the-responder-signal
  (with-world ()
    (let* ((b (make-browser))
           (late-error nil)
           (response (browser-apply
                      b
                      (multiple-value-bind (handed)
                          (serve-streaming
                           (call-app (streaming-app
                                      (lambda (s) (setf (session-value s "k") 1))
                                      (lambda (s)
                                        (setf late-error
                                              (capture (session-write-error)
                                                (setf (session-value s "late") 2)))
                                        (handler-bind ((session-write-error #'continue))
                                          (setf (session-value s "late2") 3))))
                                     (make-env)))
                        handed))))
      (ok (typep late-error 'session-write-error)
          "a change after the commit signals: the session is committed")
      (ok (cookie-id response))
      (incf *now* 1)
      (ok (equal '(1 nil nil)
                 (visit b (lambda (s) (list (session-value s "k")
                                            (session-value s "late")
                                            (session-value s "late2")))))
          "nothing after the commit was persisted"))))

(deftest streaming-app-that-never-responds-commits-nothing
  (with-world ()
    (let ((response (call-app (lambda (env)
                                (setf (session-value (env-session env) "k") 1)
                                (lambda (responder) (declare (ignore responder)) nil))
                              (make-env))))
      (serve-streaming response)
      (ok (equal '(0 0 0) (calls *cache*))))))

(deftest streaming-read-only-hands-the-apps-list-through
  (with-world ()
    (let* ((head (list 200 (list :content-type "text/event-stream")))
           (handed (serve-streaming
                    (call-app (streaming-app #'ignore-session #'ignore-session :head head)
                              (make-env)))))
      (ok (eq head handed) "no Set-Cookie: the app's list as is"))))

(deftest streaming-returns-the-servers-writer
  (with-world ()
    (let* ((server-writer (lambda (chunk &key close) (declare (ignore chunk close)) nil))
           (got nil)
           (response (call-app (lambda (env)
                                 (setf (session-value (env-session env) "k") 1)
                                 (lambda (responder)
                                   (setf got (funcall responder
                                                      '(200 (:content-type "text/plain"))))))
                               (make-env))))
      (funcall response (lambda (list) (declare (ignore list)) server-writer))
      (ok (eq server-writer got) "the app gets what the server's responder returned"))))

(deftest streaming-keeps-the-apps-headers-and-body
  (with-world ()
    (let* ((head (list 200 (list :content-type "text/plain" :set-cookie "theme=dark")
                       (list "whole body")))
           (copy (copy-tree head))
           (response (call-app (streaming-app (lambda (s) (setf (session-value s "k") 1))
                                              #'ignore-session :head head)
                               (make-env))))
      (multiple-value-bind (handed chunks) (serve-streaming response)
        (ok (equal copy head) "the app's list is not modified")
        (ok (equal '("whole body") chunks))
        (ok (equal (list "theme=dark" (session-cookie handed)) (set-cookies handed)))
        (ok (equal '(:content-type "text/plain" :set-cookie "theme=dark")
                   (subseq (second handed) 0 4)))))))

(deftest streaming-on-a-loaded-session
  (with-world ()
    (let ((b (started-browser "n" 1)))
      (incf *now* 120)
      (let ((handed (serve-streaming
                     (call-app (streaming-app (lambda (s) (incf (session-value s "n")))
                                              #'ignore-session)
                               (make-env :cookie (browser-cookie-header b))))))
        (ok (equal (browser-sid b) (cookie-id handed)))
        (ok (eql 2 (gethash "n" (stored-data (browser-sid b)))))))))
