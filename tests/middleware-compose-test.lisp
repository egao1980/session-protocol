;;;; SESSION-HANDLER next to other mixins: the hook order, class precedence
;;;; variants, response headers, timing, a short-circuiting gate, error
;;;; mapping, store failures, and requests that are not Clack requests.

(in-package #:session-protocol/middleware/tests)

;;; The hook order

(deftest hooks-around-the-session
  (with-world ()
    (let ((app (visitor-app :mixins '(outer-probe session-handler inner-probe clack-handler)))
          (*probes* '()))
      (hit (make-browser) app :do (setter "k" 1))
      (ok (equal '((:prepare outer-probe nil nil)
                   (:prepare inner-probe t t)
                   (:finish inner-probe nil)
                   (:finish outer-probe t))
                 (reverse *probes*))
          "inner PREPARE-REQUESTs see the session on the request and in the env;
inner FINISH-RESPONSEs see the response before the Set-Cookie, outer ones after"))))

(deftest finish-response-without-a-session-passes-through
  (with-world ()
    (let ((handler (make-handler '(session-handler clack-handler)
                                 :app #'visitor :session-manager *manager*))
          (response (ok-response)))
      (ok (eq response (finish-response handler
                                        (make-instance 'clack-request :env (make-env))
                                        response))
          "a request with no session: the response unchanged")
      (ok (equal '(0 0 0) (calls *cache*))))))

;;; Class precedence variants: the same headers and cookies

(defparameter *app-headers*
  '(:content-type "text/html" :set-cookie "theme=dark" :set-cookie "lang=en"))

(defparameter *added-headers*
  '(:x-frame-options "DENY" :content-type "text/plain"))

(defclass named-session-app (response-headers-handler session-handler clack-handler) ()
  (:documentation "A handler class written with DEFCLASS, not MAKE-HANDLER."))

(defun app-for (mixins)
  "The Clack app over VISITOR for MIXINS, or for the class NAMED-SESSION-APP."
  (if (eq mixins 'named-session-app)
      (clack-app (make-instance 'named-session-app :app #'visitor
                                                   :session-manager *manager*
                                                   :response-headers *added-headers*))
      (visitor-app :mixins mixins
                   :initargs (append
                              (and (member 'response-headers-handler mixins)
                                   (list :response-headers *added-headers*))
                              (and (member 'error-handler mixins)
                                   (list :error-function #'error-page))))))

(deftest-parametrize class-precedence-variants-give-the-same-cookies
    ((mixins) :ids ("session-outside" "session-inside" "with-timing" "with-errors"
                    "defclass")
     ('(session-handler response-headers-handler clack-handler))
     ('(response-headers-handler session-handler clack-handler))
     ('(session-handler timing-handler response-headers-handler clack-handler))
     ('(response-headers-handler timing-handler session-handler error-handler clack-handler))
     ('named-session-app))
  (with-world ()
    (let* ((app (app-for mixins))
           (b (make-browser))
           (*response* (list 200 (copy-list *app-headers*) (list "<p>hi</p>")))
           (copy (copy-tree *response*)))
      (multiple-value-bind (v response) (hit b app :do (setter "k" 1))
        (declare (ignore v))
        (ok (equal '(:content-type :set-cookie :set-cookie :set-cookie :x-frame-options)
                   (header-names response))
            "the app's headers in order, the session cookie, then the added header")
        (ok (equal "text/html" (getf (headers-of response) :content-type))
            "the app's Content-Type wins")
        (ok (equal (list "theme=dark" "lang=en" (session-cookie response))
                   (set-cookies response))
            "every Set-Cookie of the app kept, the session's last")
        (ok (equal (format-session-cookie (make-cookie-policy) (make-env)
                                          (cookie-id response) 86400)
                   (session-cookie response))
            "the session cookie exactly")
        (ok (equal '("<p>hi</p>") (third response)))
        (ok (equal copy *response*) "the app's lists are not modified"))
      (ok (equal '("lang" "sid" "theme")
                 (sort (mapcar #'car (browser-cookies b)) #'string<)))
      (incf *now* 1)
      (let ((*response* nil))
        (ok (eql 1 (hit b app :do (reader "k"))) "and the session persists")))))

(deftest an-app-cookie-with-the-sessions-name-loses
  (with-world ()
    (let* ((app (visitor-app :mixins '(response-headers-handler session-handler clack-handler)
                             :initargs (list :response-headers *added-headers*)))
           (b (make-browser))
           (*response* (list 200 (list :content-type "text/plain" :set-cookie "sid=forged")
                             (list "ok"))))
      (hit b app :do (setter "k" 1))
      (ok (valid-session-id-p (browser-sid b)) "the session's cookie comes last and wins"))))

(deftest-parametrize timing-sees-the-session-cookie
    ((mixins) :ids ("timing-outside" "timing-inside")
     ('(timing-handler session-handler clack-handler))
     ('(session-handler timing-handler clack-handler)))
  (with-world ()
    (let* ((reported '())
           (app (visitor-app :mixins mixins
                             :initargs (list :timing-report
                                             (lambda (request response seconds)
                                               (declare (ignore seconds))
                                               (push (list request response) reported)))))
           (b (make-browser)))
      (hit b app :do (setter "k" 1))
      (ok (= 1 (length reported)))
      (destructuring-bind (request response) (first reported)
        (ok (session-cookie response) "the reported response carries the Set-Cookie")
        (ok (typep (request-session request) 'session)
            "the reported request carries the session"))
      (setf reported '())
      (let ((*response* (ok-response)))
        (incf *now* 1)
        (hit b app :do (reader "k"))
        (ok (eq *response* (second (first reported)))
            "nothing written: timing saw the app's response itself")))))

;;; A short-circuit skips the begin and the commit

(deftest-parametrize short-circuit-skips-begin-and-commit
    ((mixins) :ids ("gate-outside" "gate-inside" "gate-inside-headers")
     ('(gate-handler session-handler clack-handler))
     ('(session-handler gate-handler clack-handler))
     ('(response-headers-handler gate-handler session-handler clack-handler)))
  (with-world ()
    (let* ((app (app-for mixins))
           (b (started app "k" 1))
           (before (calls *cache*))
           (reached nil)
           (*gate-saw* :unset))
      (ok (valid-session-id-p (browser-sid b)) "the session was stored through the same app")
      (incf *now* 120)
      (multiple-value-bind (v response)
          (hit b app :path "/forbidden"
                     :do (lambda (s) (setf reached t) (setf (session-value s "x") 1)))
        (declare (ignore v))
        (ok (= 403 (first response)) "the gate answered")
        (ok (not reached) "the app did not run")
        (ok (null *gate-saw*) "the gate saw no session on the request")
        (ok (null (set-cookies response)) "no Set-Cookie")
        (ok (equal before (calls *cache*)) "no store call: nothing begun, nothing committed")
        (when (member 'response-headers-handler mixins)
          (ok (equal "DENY" (getf (headers-of response) :x-frame-options))
              "an outer :AROUND still maps the 403")))
      (incf *now* 1)
      (ok (equal '(1 nil)
                 (hit b app :do (lambda (s) (list (session-value s "k") (session-value s "x")))))
          "the next request sees the session as it was"))))

(deftest short-circuit-for-a-new-browser
  (with-world ()
    (let ((app (app-for '(gate-handler session-handler clack-handler)))
          (*gate-saw* :unset))
      (multiple-value-bind (v response) (hit (make-browser) app :path "/forbidden")
        (declare (ignore v))
        (ok (= 403 (first response)))
        (ok (null (set-cookies response))))
      (ok (null *gate-saw*))
      (ok (equal '(0 0 0) (calls *cache*))))))

;;; Errors: an error mapped to a response does not commit

(deftest-parametrize a-mapped-error-commits-nothing
    ((mixins) :ids ("errors-outside" "errors-inside" "errors-inside-headers")
     ('(error-handler session-handler clack-handler))
     ('(session-handler error-handler clack-handler))
     ('(response-headers-handler error-handler session-handler clack-handler)))
  (with-world ()
    (let* ((app (app-for mixins))
           (b (started app "k" 1))
           (id (browser-sid b))
           (before (calls *cache*)))
      (incf *now* 120)
      (multiple-value-bind (v response) (hit b app :do (then-error (setter "k" 2)))
        (declare (ignore v))
        (ok (= 500 (first response)) "ERROR-HANDLER mapped the error")
        (ok (null (set-cookies response)) "no session Set-Cookie on the mapped response")
        (when (member 'response-headers-handler mixins)
          (ok (equal "DENY" (getf (headers-of response) :x-frame-options)))))
      (ok (= (second before) (second (calls *cache*))) "no save")
      (ok (= (third before) (third (calls *cache*))) "no delete")
      (ok (json-equal (table "k" 1) (stored-data id)) "the stored session is as it was")
      (ok (equal id (browser-sid b)))
      (incf *now* 1)
      (ok (eql 1 (hit b app :do (reader "k"))) "the change made before the error is lost"))))

(deftest a-mapped-error-for-a-new-browser
  (with-world ()
    (let* ((seen :unset)
           (app (visitor-app :mixins '(error-handler session-handler clack-handler)
                             :initargs (list :error-function
                                             (lambda (request condition)
                                               (setf seen (request-session request))
                                               (error-page request condition)))))
           (b (make-browser)))
      (multiple-value-bind (v response) (hit b app :do (then-error (setter "k" 1)))
        (declare (ignore v))
        (ok (= 500 (first response)))
        (ok (null (set-cookies response))))
      (ok (typep seen 'session) "the error function sees the begun session")
      (ok (and (typep seen 'session) (not (session-committed-p seen))) "not committed")
      (ok (equal '(0 0 0) (calls *cache*)) "no store entry")
      (ok (null (browser-cookies b))))))

(deftest an-error-page-may-commit-the-session-itself
  (with-world ()
    (let* ((app (visitor-app
                 :mixins '(error-handler session-handler clack-handler)
                 :initargs (list :error-function
                                 (lambda (request condition)
                                   (finish-session *manager* (request-session request)
                                                   (clack-request-env request)
                                                   (error-page request condition))))))
           (b (make-browser)))
      (multiple-value-bind (v response) (hit b app :do (then-error (setter "k" 1)))
        (declare (ignore v))
        (ok (= 500 (first response)))
        (ok (valid-session-id-p (cookie-id response)) "FINISH-SESSION by hand sets the cookie"))
      (ok (json-equal (table "k" 1) (stored-data (browser-sid b))))
      (incf *now* 1)
      (ok (eql 1 (hit b app :do (reader "k")))))))

(deftest without-error-handler-the-error-propagates
  (with-world ()
    (let* ((app (visitor-app))
           (b (started app "k" 1))
           (before (calls *cache*)))
      (incf *now* 120)
      (ok (signals (hit b app :do (then-error (setter "k" 2))) 'app-error)
          "the app's condition, unchanged")
      (ok (= (second before) (second (calls *cache*))) "no save")
      (ok (json-equal (table "k" 1) (stored-data (browser-sid b)))))))

;;; Store failures: S0's restarts stay live through the middleware

(deftest a-load-failure-through-the-handler
  (with-world ()
    (let* ((app (visitor-app))
           (b (started app "k" 1)))
      (incf *now* 120)
      (fail-next *cache* :get)
      (let ((c (capture (session-store-error) (hit b app :do (reader "k")))))
        (ok (typep c 'session-store-error) "it propagates out of the Clack app")
        (ok (eq :load (session-store-error-operation c))))
      (fail-next *cache* :get)
      (ok (equal '(t nil)
                 (handler-bind ((session-store-error #'continue))
                   (hit b app :do (lambda (s) (list (session-new-p s) (session-value s "k"))))))
          "CONTINUE: as if the store had no session")
      (fail-next *cache* :get)
      (ok (eql 1 (handler-bind ((session-store-error (lambda (c) (stack-session:retry c))))
                   (hit b app :do (reader "k"))))
          "RETRY: loaded on the second try"))))

(deftest a-save-failure-decided-by-an-error-function
  ;; HANDLE-ERROR runs before unwinding (middleware D9), so an error function
  ;; can invoke the commit's CONTINUE restart.
  (with-world ()
    (let* ((app (visitor-app
                 :mixins '(error-handler session-handler clack-handler)
                 :initargs (list :error-function
                                 (lambda (request condition)
                                   (if (typep condition 'session-store-error)
                                       (continue condition)
                                       (error-page request condition))))))
           (b (make-browser)))
      (fail-next *cache* :put)
      (multiple-value-bind (v response) (hit b app :do (setter "k" 1))
        (declare (ignore v))
        (ok (= 200 (first response)) "the app's response")
        (ok (valid-session-id-p (cookie-id response)) "CONTINUE: the cookie is still sent")
        (ok (null (peek (cookie-id response))) "and nothing is stored")))))

;;; Requests that are not Clack requests pass through

(defclass other-request (request) ())

(deftest-parametrize other-requests-pass-through
    ((make-request) :ids ("plain-request" "request-subclass" "string" "plist")
     ((lambda () (make-instance 'request)))
     ((lambda () (make-instance 'other-request)))
     ((lambda () "GET /"))
     ((lambda () (list :path-info "/"))))
  (with-world ()
    (let* ((answer (list 200 (list :content-type "text/plain") (list "plain")))
           (handler (make-handler '(session-handler function-handler)
                                  :function (constantly answer)
                                  :session-manager *manager*))
           (request (funcall make-request)))
      (ok (eq answer (handle handler request)) "the handler's answer, unchanged")
      (ok (equal '(0 0 0) (calls *cache*)) "the manager is not called")
      (ok (null (request-session request)) "no session on it"))))
