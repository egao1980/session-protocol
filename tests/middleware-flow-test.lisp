;;;; SESSION-HANDLER over several requests: configuration, persistence, a new
;;;; browser, lazy creation, rotation and ending, what the app sees.

(in-package #:session-protocol/middleware/tests)

;;; Configuration (given)

(deftest session-handler-configuration
  (with-world ()
    (let ((handler (make-handler '(session-handler clack-handler)
                                 :app #'visitor :session-manager *manager*)))
      (ok (typep handler 'session-handler))
      (ok (typep handler 'handler))
      (ok (eq *manager* (session-handler-manager handler))
          ":SESSION-MANAGER is the handler's manager"))
    (let ((a (make-handler '(session-handler clack-handler) :app #'visitor))
          (b (make-handler '(session-handler clack-handler) :app #'visitor)))
      (ok (typep (session-handler-manager a) 'session-manager)
          "the default is a manager of its own")
      (ok (not (eq (session-handler-manager a) (session-handler-manager b)))
          "a fresh one per handler instance"))
    (ok (signals (make-handler '(session-handler clack-handler)
                               :app #'visitor :session-manager 42)
                 'type-error))
    (ok (signals (make-handler '(session-handler clack-handler)
                               :app #'visitor :session-manager nil)
                 'type-error))))

(deftest session-handler-works-through-the-hooks-only
  ;; No HANDLE method of its own: the hooks run inside middleware-protocol's
  ;; base method, which is what the short-circuit and error rules rest on.
  (let ((class (find-class 'session-handler)))
    (ok (subtypep class 'handler))
    (dolist (qualifiers '(() (:around) (:before) (:after)))
      (dolist (request-class (list (find-class t) (find-class 'clack-request)))
        (ok (null (find-method #'handle qualifiers (list class request-class) nil))
            (format nil "no HANDLE ~{~s ~}method on (SESSION-HANDLER ~s)"
                    qualifiers (class-name request-class)))))
    (ok (find-method #'prepare-request '() (list class (find-class 'clack-request)) nil))
    (ok (find-method #'finish-response '()
                     (list class (find-class 'clack-request) (find-class t)) nil))))

;;; Persistence

(deftest values-persist-across-requests
  (with-world ()
    (let ((app (visitor-app))
          (b (make-browser)))
      (multiple-value-bind (n response) (hit b app :do (counter "visits"))
        (ok (eql 1 n))
        (ok (valid-session-id-p (cookie-id response)) "the first write sets the cookie"))
      (let ((id (browser-sid b)))
        (incf *now* 5)
        (ok (eql 2 (hit b app :do (counter "visits"))))
        (incf *now* 5)
        (ok (eql 3 (hit b app :do (counter "visits"))))
        (ok (equal id (browser-sid b)) "one session, one id")
        (ok (json-equal (table "visits" 3) (stored-data id)))
        (incf *now* 5)
        (ok (eql 3 (hit b app :do (reader "visits"))) "read back on a later request")))))

(deftest a-new-browser-gets-a-new-session
  (with-world ()
    (let* ((app (visitor-app))
           (one (started app "who" "one"))
           (two (make-browser)))
      (incf *now* 1)
      (ok (equal '(t nil)
                 (hit two app :do (lambda (s) (list (session-new-p s)
                                                    (session-value s "who")))))
          "no cookie: a new, empty session")
      (hit two app :do (setter "who" "two"))
      (ok (valid-session-id-p (browser-sid two)))
      (ok (not (equal (browser-sid one) (browser-sid two))) "an id of its own")
      (incf *now* 1)
      (ok (equal "one" (hit one app :do (reader "who"))))
      (ok (equal "two" (hit two app :do (reader "who")))))))

(deftest a-request-that-stores-nothing-sets-no-cookie
  (with-world ()
    (let* ((app (visitor-app))
           (b (make-browser))
           (*response* (ok-response)))
      (multiple-value-bind (value response) (hit b app :do (reader "anything"))
        (ok (null value))
        (ok (eq *response* response) "the app's response comes back as it is")
        (ok (null (set-cookies response)) "no Set-Cookie"))
      (ok (equal '(0 0 0) (calls *cache*)) "no store entry, no store call")
      (ok (null (browser-cookies b))))))

(deftest a-read-of-a-loaded-session-sets-no-cookie
  (with-world (:refresh-interval 60)
    (let* ((app (visitor-app))
           (b (started app "k" 1))
           (before (calls *cache*))
           (*response* (ok-response)))
      (incf *now* 10)
      (multiple-value-bind (value response) (hit b app :do (reader "k"))
        (ok (eql 1 value))
        (ok (eq *response* response) "within the refresh interval nothing is written"))
      (ok (equal (list (1+ (first before)) (second before) (third before))
                 (calls *cache*))
          "one load, no save"))))

;;; Rotation and ending

(deftest rotation-through-the-handler
  (with-world ()
    (let* ((app (visitor-app))
           (b (started app "k" 1))
           (old (browser-sid b)))
      (incf *now* 30)
      (multiple-value-bind (value response) (hit b app :do #'regenerate-session-id)
        (declare (ignore value))
        (let ((new (cookie-id response)))
          (ok (valid-session-id-p new))
          (ok (not (equal old new)) "a new id in the cookie")
          (ok (null (peek old)) "the old id is gone from the store")
          (ok (json-equal (table "k" 1) (stored-data new)) "the data moved with it")
          (ok (= +t0+ (gethash "created" (stored new))) "the creation time is kept")))
      (incf *now* 1)
      (ok (eql 1 (hit b app :do (reader "k"))) "the next request finds it under the new id"))))

(deftest ending-through-the-handler
  (with-world ()
    (let* ((app (visitor-app))
           (b (started app "k" 1))
           (id (browser-sid b)))
      (incf *now* 30)
      (multiple-value-bind (value response) (hit b app :do #'end-session)
        (declare (ignore value))
        (ok (equal "" (cookie-id response)) "the expiry cookie")
        (ok (equal "0" (attribute response "max-age")))
        (ok (null (peek id)) "the record is gone"))
      (ok (null (browser-sid b)) "the browser dropped the cookie")
      (incf *now* 1)
      (ok (equal '(t nil)
                 (hit b app :do (lambda (s) (list (session-new-p s) (session-value s "k")))))
          "the next request starts afresh"))))

;;; What the inner app sees

(deftest the-app-sees-the-session-on-the-env-and-the-request
  (with-world ()
    (let* ((app (visitor-app))
           (env (make-env :path "/page"))
           (copy (copy-list env))
           (seen nil))
      (let ((*visit* (lambda (s) (setf seen s))))
        (funcall app env))
      (let ((request (env-request *seen-env*)))
        (ok (typep seen 'session))
        (ok (eq seen (env-session *seen-env*)) "ENV-SESSION in the app's env")
        (ok (typep request 'clack-request) "the request under :MIDDLEWARE.REQUEST")
        (ok (eq seen (request-session request)) "and the request's :SESSION property")
        (ok (eq seen (request-property request :session)))
        (ok (eq request (getf *seen-env* :middleware.request)))
        (ok (eq (clack-request-env request) (cddr *seen-env*))
            "the app's env is the request's env behind :MIDDLEWARE.REQUEST")
        (ok (eq seen (getf (clack-request-env request) :session))
            "the request's env carries :SESSION")
        (ok (eq env (cddr (clack-request-env request)))
            "the request's env is (:SESSION s . the server's env)")
        (loop for (key value) on env by #'cddr
              do (ok (eq value (getf *seen-env* key)) (format nil "env key ~s kept" key)))
        (ok (equal copy env) "the server's env is not modified")
        (ok (null (getf env :session)))))))

(deftest https-requests-get-a-secure-cookie
  ;; FINISH-SESSION reads :URL-SCHEME from the request's env.
  (with-world ()
    (let ((app (visitor-app))
          (plain (make-browser))
          (tls (make-browser :scheme "https")))
      (multiple-value-bind (v response) (hit tls app :do (setter "k" 1))
        (declare (ignore v))
        (ok (nth-value 1 (attribute response "secure")) "Secure on https"))
      (multiple-value-bind (v response) (hit plain app :do (setter "k" 1))
        (declare (ignore v))
        (ok (not (nth-value 1 (attribute response "secure"))) "not on http")))))

(defclass flow-clack-request (clack-request) ())

(deftest a-subclass-of-clack-request-gets-sessions
  (with-world ()
    (let ((app (visitor-app :request-class 'flow-clack-request))
          (b (make-browser)))
      (hit b app :do (setter "k" 1))
      (ok (typep (env-request *seen-env*) 'flow-clack-request))
      (ok (valid-session-id-p (browser-sid b)))
      (incf *now* 1)
      (ok (eql 1 (hit b app :do (reader "k")))))))

(deftest the-default-manager-keeps-sessions
  ;; No :SESSION-MANAGER: a process-local manager of the handler's own, on
  ;; the real clock.
  (let ((app (visitor-app :manager nil))
        (b (make-browser)))
    (ok (eql 1 (hit b app :do (counter "n"))))
    (ok (valid-session-id-p (browser-sid b)))
    (ok (eql 2 (hit b app :do (counter "n"))))))

(deftest request-session-reads-the-property
  (let ((request (make-instance 'request)))
    (ok (null (request-session request)) "no :SESSION property: NIL")
    (setf (request-property request :session) :a-session)
    (ok (eq :a-session (request-session request)))
    (ok (= 1 (length (multiple-value-list (request-session request))))
        "one value"))
  (ok (null (request-session (make-instance 'clack-request :env (make-env)))))
  (dolist (other (list nil 42 "a string" '(:session :x) (make-env)))
    (ok (null (request-session other))
        (format nil "not a request: NIL (~a)" (type-of other)))))
