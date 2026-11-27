;;;; Conditions and restart functions (given in the contract).

(in-package #:session-protocol/tests)

(deftest-parametrize condition-tree
    ((type parent)
     :ids ("value-error" "store-error" "write-error" "root")
     ('session-value-error 'session-error)
     ('session-store-error 'session-error)
     ('session-write-error 'session-error)
     ('session-error 'error))
  (ok (subtypep type parent) (format nil "~s is a ~s" type parent)))

(deftest condition-readers
  (let ((v (make-condition 'session-value-error :key "k" :value #\x :message "m"))
        (s (make-condition 'session-store-error :store :the-store :operation :save
                                                :cause :the-cause))
        (w (make-condition 'session-write-error :session :the-session)))
    (ok (equal "k" (session-value-error-key v)))
    (ok (eql #\x (session-value-error-value v)))
    (ok (equal "m" (session-error-message v)))
    (ok (eq :the-store (session-store-error-store s)))
    (ok (eq :save (session-store-error-operation s)))
    (ok (eq :the-cause (session-store-error-cause s)))
    (ok (null (session-error-message s)))
    (ok (eq :the-session (session-write-error-session w)))))

(deftest value-error-report-names-key-and-type-not-value
  (let ((report (princ-to-string
                 (make-condition 'session-value-error
                                 :key "cart" :value 'very-secret-symbol))))
    (ok (search "\"cart\"" report))
    (ok (search "SYMBOL" report))
    (ok (not (search "VERY-SECRET-SYMBOL" report)))))

(deftest store-error-report-hides-the-cause-report
  (let* ((id (make-string 43 :initial-element #\Q))
         (cause (make-condition 'stack-cache:cache-connection-error
                                :key (cache-key id) :message "refused"))
         (report (princ-to-string
                  (make-condition 'session-store-error :operation :load
                                                       :cause cause
                                                       :message "cache failed"))))
    (ok (search "load" report))
    (ok (search "cache failed" report))
    (ok (no-id-in report id) "the cache key (and so the id) is not in the report")))

(deftest write-error-report-says-ended-or-committed
  (let ((ended (make-session :id "x" :created-at 0 :last-seen 0))
        (committed (make-session :id "y" :created-at 0 :last-seen 0)))
    (setf (slot-value ended 'session-protocol::ended) t)
    (mark-committed committed)
    (ok (search "ended" (princ-to-string
                         (make-condition 'session-write-error :session ended))))
    (ok (search "committed" (princ-to-string
                             (make-condition 'session-write-error :session committed))))))

(deftest restart-functions-invoke-or-return-nil
  (ok (null (skip-value)) "no SKIP-VALUE restart: NIL")
  (ok (null (retry)) "no RETRY restart: NIL")
  (ok (eq :skipped (restart-case (skip-value)
                     (session-protocol::skip-value () :skipped))))
  (ok (eq :retried (restart-case (retry)
                     (session-protocol::retry () :retried))))
  (let ((c (make-condition 'session-value-error :key "k" :value #\x)))
    (ok (eq :for-c
            (restart-case (with-condition-restarts c
                              (list (find-restart 'session-protocol::skip-value))
                            (skip-value c))
              (session-protocol::skip-value () :for-c))))))
