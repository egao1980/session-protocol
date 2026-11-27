;;;; session-protocol S0b tests: package and helpers for SESSION-HANDLER.
;;;;
;;;; The suite is the spec of slice S0b (brief: cl-stack/docs/capabilities/
;;;; session.md, S0b; src/middleware.lisp's header). It drives real Clack apps
;;;; built with middleware-protocol: (CLACK-APP (MAKE-HANDLER mixins ...)),
;;;; with SESSION-HANDLER alone or next to RESPONSE-HEADERS-HANDLER,
;;;; TIMING-HANDLER, ERROR-HANDLER and a test gate that short-circuits.
;;;;
;;;; The world (test clock *NOW*, a counting TEST-CACHE, a store and a
;;;; manager), the env builder, the Set-Cookie parsers and the BROWSER (a
;;;; cookie jar) come from the S0 suite (package SESSION-PROTOCOL/TESTS).
;;;; HIT sends one request of a browser through a Clack app and keeps the
;;;; cookies it sets.

(defpackage #:session-protocol/middleware/tests
  (:use #:cl #:rove
        #:session-protocol
        #:middleware-protocol
        #:middleware-protocol/clack
        #:session-protocol/middleware)
  (:import-from #:session-protocol/tests
                ;; time and the world
                #:+t0+ #:*now* #:test-now #:make-test-cache #:with-world
                #:*cache* #:*store* #:*manager* #:calls #:fail-next
                #:peek #:stored #:stored-data
                ;; envs, responses, cookies
                #:make-env #:ok-response #:headers-of #:set-cookies
                #:parse-set-cookie #:session-cookie #:cookie-id
                #:cookie-attributes #:attribute
                ;; the cookie jar
                #:make-browser #:browser-cookies #:browser-scheme
                #:browser-cookie-header #:browser-apply #:browser-sid
                ;; values and conditions
                #:json-equal #:table #:capture
                ;; streaming
                #:serve-streaming #:*on-first-chunk* #:streaming-app
                #:ignore-session))

(in-package #:session-protocol/middleware/tests)

;;; Clack apps behind a handler

(defun session-app (inner &key (mixins '(session-handler clack-handler))
                               (manager *manager*) initargs request-class)
  "A Clack app: (CLACK-APP (MAKE-HANDLER MIXINS :APP INNER :SESSION-MANAGER
MANAGER . INITARGS)). MANAGER NIL leaves :SESSION-MANAGER out."
  (let ((handler (apply #'make-handler mixins :app inner
                        (append (and manager (list :session-manager manager))
                                initargs))))
    (if request-class
        (clack-app handler :request-class request-class)
        (clack-app handler))))

(defvar *visit* nil
  "The function of the session that VISITOR calls, or NIL.")

(defvar *visit-value* nil "What *VISIT* returned on the last request.")

(defvar *seen-env* nil "The env VISITOR got on the last request.")

(defvar *response* nil
  "The response VISITOR answers, or NIL for a fresh (OK-RESPONSE).")

(defun visitor (env)
  "The tests' inner Clack app: keeps ENV in *SEEN-ENV*, calls *VISIT* on
(ENV-SESSION env), answers *RESPONSE*."
  (setf *seen-env* env
        *visit-value* (and *visit* (funcall *visit* (env-session env))))
  (or *response* (ok-response)))

(defun visitor-app (&rest args)
  "SESSION-APP over VISITOR."
  (apply #'session-app #'visitor args))

(defun browser-env (browser &key (path "/"))
  (make-env :cookie (browser-cookie-header browser)
            :scheme (browser-scheme browser)
            :path path))

(defun hit (browser app &key do (path "/"))
  "One request of BROWSER through the Clack APP, whose inner VISITOR calls DO
on the session. An immediate response's cookies go into the jar. Returns DO's
value and the response (a streaming response is returned as it is: see
HIT-STREAMING)."
  (let* ((*visit* do)
         (*visit-value* nil)
         (response (funcall app (browser-env browser :path path))))
    (values *visit-value*
            (if (functionp response)
                response
                (browser-apply browser response)))))

(defun hit-streaming (browser app &key (path "/"))
  "One request of BROWSER through APP, whose response must be a streaming one:
it is served as a Clack server does (SERVE-STREAMING), and the cookies of the
head go into the jar. Returns the head and the chunks."
  (let ((response (funcall app (browser-env browser :path path))))
    (multiple-value-bind (head chunks) (serve-streaming response)
      (browser-apply browser head)
      (values head chunks))))

(defun setter (key value)
  (lambda (s) (setf (session-value s key) value)))

(defun reader (key)
  (lambda (s) (session-value s key)))

(defun counter (key)
  (lambda (s) (incf (session-value s key 0))))

(defun started (app &rest kvs)
  "A browser whose session holds KVS, stored through APP at the current time."
  (let ((b (make-browser)))
    (hit b app :do (lambda (s)
                     (loop for (k v) on kvs by #'cddr
                           do (setf (session-value s k) v))))
    b))

(defun header-names (response)
  (loop for (name) on (headers-of response) by #'cddr collect name))

;;; A gate that short-circuits

(defvar *gate-saw* :unset
  "(REQUEST-SESSION request) as the gate saw it when it answered 403.")

(defclass gate-handler (handler) ()
  (:documentation "Answers 403 to /forbidden without calling the inner layers."))

(defmethod handle :around ((handler gate-handler) (request clack-request))
  (if (equal "/forbidden" (getf (clack-request-env request) :path-info))
      (progn (setf *gate-saw* (request-session request))
             (list 403 (list :content-type "text/plain") (list "forbidden")))
      (call-next-method)))

;;; Probes: record what each hook sees

(defvar *probes* '() "Probe events, most recent first.")

(defmacro define-probe (name)
  `(progn
     (defclass ,name (handler) ())
     (defmethod prepare-request ((handler ,name) (request clack-request))
       (push (list :prepare ',name
                   (and (request-session request) t)
                   (and (env-session (clack-request-env request)) t))
             *probes*)
       (call-next-method))
     (defmethod finish-response ((handler ,name) (request clack-request) response)
       (let ((response (call-next-method)))
         (push (list :finish ',name (and (session-cookie response) t)) *probes*)
         response))))

(define-probe outer-probe)
(define-probe inner-probe)

;;; Errors

(define-condition app-error (error) ()
  (:report "test: the app failed"))

(defun error-page (request condition)
  "The tests' :ERROR-FUNCTION: a 500 response."
  (declare (ignore request condition))
  (list 500 (list :content-type "text/plain") (list "internal error")))

(defun then-error (fn)
  "A *VISIT* that calls FN on the session, then signals APP-ERROR."
  (lambda (s) (funcall fn s) (error 'app-error)))
