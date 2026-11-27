;;;; Failures: store errors with RETRY and CONTINUE in both phases, a corrupt
;;;; record, a value that went bad before the commit; no id in any report.

(in-package #:session-protocol/tests)

(defun returning-browser ()
  "A browser with a stored session holding k = 1, 120 s later."
  (prog1 (started-browser "k" 1)
    (incf *now* 120)))

(defun env-of (browser)
  (make-env :cookie (browser-cookie-header browser)))

;;; BEGIN-SESSION: the load

(deftest load-failure-propagates
  (with-world ()
    (let ((b (returning-browser)))
      (fail-next *cache* :get)
      (let ((c (capture (session-store-error) (begin-session *manager* (env-of b)))))
        (ok (typep c 'session-store-error))
        (ok (and c (eq :load (session-store-error-operation c))))
        (ok (and c (typep (session-store-error-cause c)
                          'stack-cache:cache-connection-error)))
        (ok (and c (no-id-in (princ-to-string c) (browser-sid b))))))))

(deftest load-failure-retry
  (with-world ()
    (let ((b (returning-browser)) (n 0))
      (fail-next *cache* :get 2)
      (let ((s (handler-bind ((session-store-error
                                (lambda (c) (incf n) (retry c))))
                 (begin-session *manager* (env-of b)))))
        (ok (= 2 n) "two failures, two retries")
        (ok (not (session-new-p s)) "the third load found the session")
        (ok (eql 1 (session-value s "k")))))))

(deftest load-failure-continue
  (with-world ()
    (let ((b (returning-browser)))
      (fail-next *cache* :get)
      (let ((s (handler-bind ((session-store-error #'continue))
                 (begin-session *manager* (env-of b)))))
        (ok (session-new-p s) "CONTINUE: as if there were no session")
        (ok (string/= (browser-sid b) (session-id s)))))))

(deftest corrupt-record
  (with-world ()
    (let ((id (generate-session-id)))
      (stack-cache:cache-put *cache* (cache-key id) "{\"v\":1,\"id\":")
      (let ((env (make-env :cookie (format nil "sid=~a" id))))
        (let ((c (capture (session-store-error) (begin-session *manager* env))))
          (ok (typep c 'session-store-error))
          (ok (and c (eq :load (session-store-error-operation c))))
          (ok (and c (no-id-in (princ-to-string c) id))))
        (let ((s (handler-bind ((session-store-error #'continue))
                   (begin-session *manager* env))))
          (ok (session-new-p s)))))))

(deftest expired-record-delete-failure
  (with-world (:idle-ttl 600 :cache (frozen-cache))
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 600))
      (fail-next *cache* :delete)
      (ok (signals (begin-session *manager* (env-of b)) 'session-store-error))
      (fail-next *cache* :delete)
      (let ((s (handler-bind ((session-store-error #'continue))
                 (begin-session *manager* (env-of b)))))
        (ok (session-new-p s) "still a new session")
        (ok (stringp (peek id)) "CONTINUE left the record"))
      (fail-next *cache* :delete)
      (handler-bind ((session-store-error #'retry))
        (begin-session *manager* (env-of b)))
      (ok (null (peek id)) "RETRY deleted it"))))

;;; FINISH-SESSION: the save and the deletes

(defun commit-with (browser fn &rest handler-args)
  "Begin, call FN on the session, finish with the handlers HANDLER-ARGS (a
condition type and a function) bound around FINISH-SESSION."
  (multiple-value-bind (s env) (begin-session *manager* (env-of browser))
    (funcall fn s)
    (if handler-args
        (handler-bind ((session-store-error (second handler-args)))
          (finish-session *manager* s env (ok-response)))
        (finish-session *manager* s env (ok-response)))))

(deftest save-failure-propagates-and-retries
  (with-world ()
    (let ((b (returning-browser)))
      (fail-next *cache* :put)
      (let ((c (capture (session-store-error)
                 (commit-with b (lambda (s) (setf (session-value s "k") 2))))))
        (ok (typep c 'session-store-error))
        (ok (and c (eq :save (session-store-error-operation c)))))
      (ok (eql 1 (gethash "k" (stored-data (browser-sid b)))) "not saved")
      (fail-next *cache* :put)
      (let ((response (commit-with b (lambda (s) (setf (session-value s "k") 3))
                                   'session-store-error #'retry)))
        (ok (equal (browser-sid b) (cookie-id response)))
        (ok (eql 3 (gethash "k" (stored-data (browser-sid b)))) "RETRY saved it")))))

(deftest save-failure-continue-still-sends-the-cookie
  (with-world ()
    (fail-next *cache* :put)
    (multiple-value-bind (s env) (begin-session *manager* (make-env))
      (setf (session-value s "k") 1)
      (let ((response (handler-bind ((session-store-error #'continue))
                        (finish-session *manager* s env (ok-response)))))
        (ok (equal (session-id s) (cookie-id response)) "the cookie is sent")
        (ok (null (peek (session-id s))) "but nothing was stored")
        (ok (session-committed-p s))))))

(deftest end-delete-failure
  (with-world ()
    (let* ((b (returning-browser))
           (id (browser-sid b)))
      (fail-next *cache* :delete)
      (ok (signals (commit-with b #'end-session) 'session-store-error))
      (fail-next *cache* :delete)
      (let ((response (commit-with b #'end-session 'session-store-error #'continue)))
        (ok (equal "" (cookie-id response)) "CONTINUE: the cookie still expires")
        (ok (stringp (peek id)) "the record stays, to its TTL")))))

(deftest rotation-delete-failure
  (with-world ()
    (let* ((b (returning-browser))
           (old (browser-sid b)))
      (fail-next *cache* :delete)
      (let ((response (commit-with b #'regenerate-session-id
                                   'session-store-error #'continue)))
        (ok (stringp (peek (cookie-id response))) "CONTINUE: the new id is saved")
        (ok (stringp (peek old)) "the old record stays: the caller chose so"))
      (fail-next *cache* :delete)
      (commit-with b #'regenerate-session-id 'session-store-error #'retry)
      (ok (null (peek old)) "RETRY deleted the old id"))))

;;; A value that went bad between the SETF and the commit

(deftest bad-value-at-commit
  (with-world ()
    (flet ((poisoned (s)
             (let ((h (table "ok" 1)))
               (setf (session-value s "h") h)
               (setf (gethash "bad" h) #\x))))
      (multiple-value-bind (s env) (begin-session *manager* (make-env))
        (poisoned s)
        (ok (signals (finish-session *manager* s env (ok-response)) 'session-value-error))
        (ok (equal '(0 0 0) (calls *cache*)) "nothing stored")
        (ok (not (session-committed-p s))))
      (multiple-value-bind (s env) (begin-session *manager* (make-env))
        (poisoned s)
        (let ((response (handler-bind ((session-value-error #'skip-value))
                          (finish-session *manager* s env (ok-response)))))
          (ok (null (set-cookies response)) "SKIP-VALUE: the key went, the session is empty")
          (ok (equal '(0 0 0) (calls *cache*)))))
      (multiple-value-bind (s env) (begin-session *manager* (make-env))
        (poisoned s)
        (let ((response (handler-bind ((session-value-error
                                         (lambda (c) (use-value "replaced" c))))
                          (finish-session *manager* s env (ok-response)))))
          (ok (json-equal (table "h" "replaced") (stored-data (cookie-id response)))
              "USE-VALUE: the replacement is stored"))))))

;;; No id in what the library prints

(deftest no-session-id-is-printed
  (with-world ()
    (let* ((b (returning-browser))
           (id (browser-sid b))
           (s (begin-session *manager* (env-of b))))
      (ok (no-id-in (prin1-to-string s) id))
      (ok (no-id-in (princ-to-string s) id))
      (end-session s)
      (let ((c (capture (session-write-error) (setf (session-value s "k") 1))))
        (ok (and c (no-id-in (princ-to-string c) id))))
      (fail-next *cache* :put)
      (multiple-value-bind (s2 env) (begin-session *manager* (env-of b))
        (setf (session-value s2 "k") 2)
        (let ((c (capture (session-store-error)
                   (finish-session *manager* s2 env (ok-response)))))
          (ok (and c (no-id-in (princ-to-string c) id)))
          (ok (and c (no-id-in (princ-to-string c) (session-id s2)))))))))
