;;;; CACHE-SESSION-STORE over an in-memory cache-protocol cache.

(in-package #:session-protocol/tests)

(defun stored-session (&rest kvs)
  (let ((s (make-session :id (generate-session-id) :created-at *now*
                         :last-seen *now*)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k (session-data s)) v))
    s))

(deftest cache-store-save-writes-the-record-text
  (with-world ()
    (let ((s (stored-session "cart" (list 1 2))))
      (ok (eq s (session-store-save *store* s 600)) "save returns the session")
      (let ((text (peek (session-id s))))
        (ok (stringp text) "the cached value is a string")
        (ok (equal (encode-session-record s) text) "exactly the record text")))))

(deftest cache-store-load
  (with-world ()
    (let ((s (stored-session "cart" (list 1 2) "who" (table "n" "x"))))
      (session-store-save *store* s 600)
      (let ((a (session-store-load *store* (session-id s)))
            (b (session-store-load *store* (session-id s))))
        (ok (typep a 'session))
        (ok (equal (session-id s) (session-id a)))
        (ok (not (session-new-p a)))
        (ok (= +t0+ (session-created-at a)))
        (ok (json-equal (session-data s) (session-data a)))
        (ok (not (eq a b)) "each load is a fresh object")
        (ok (not (eq (session-data a) (session-data b))))))))

(deftest cache-store-load-unknown
  (with-world ()
    (ok (null (session-store-load *store* (generate-session-id))))))

(deftest cache-store-ttl
  (with-world ()
    (let* ((s (stored-session "k" 1))
           (id (session-id s)))
      (session-store-save *store* s 600)
      (setf *now* (+ +t0+ 599))
      (ok (session-store-load *store* id) "alive at 599 s")
      (setf *now* (+ +t0+ 600))
      (ok (null (session-store-load *store* id)) "gone at 600 s")
      (ok (null (peek id))))))

(deftest cache-store-save-replaces-and-resets-ttl
  (with-world ()
    (let* ((s (stored-session "k" 1))
           (id (session-id s)))
      (session-store-save *store* s 600)
      (setf *now* (+ +t0+ 500))
      (setf (gethash "k" (session-data s)) 2)
      (session-store-save *store* s 600)
      (ok (eql 2 (gethash "k" (session-data (session-store-load *store* id)))))
      (setf *now* (+ +t0+ 1099))
      (ok (session-store-load *store* id) "the TTL counts from the last save")
      (setf *now* (+ +t0+ 1100))
      (ok (null (session-store-load *store* id))))))

(deftest cache-store-delete
  (with-world ()
    (let* ((s (stored-session "k" 1))
           (id (session-id s)))
      (session-store-save *store* s 600)
      (ok (eq t (session-store-delete *store* id)))
      (ok (null (peek id)))
      (ok (null (session-store-load *store* id)))
      (ok (null (session-store-delete *store* id)) "nothing left to delete"))))

(deftest-parametrize cache-store-prefix
    ((prefix) :ids ("custom" "empty") ("app1:sess:") (""))
  (with-world ()
    (let* ((store (make-cache-session-store *cache* :prefix prefix))
           (s (stored-session "k" 1))
           (id (session-id s)))
      (session-store-save store s 60)
      (ok (stringp (peek id :prefix prefix)) "keyed by prefix + id")
      (ok (null (peek id)) "not under the default prefix")
      (ok (session-store-load store id))
      (ok (null (session-store-load *store* id))
          "a store with another prefix does not see it")
      (ok (session-store-delete store id))
      (ok (null (peek id :prefix prefix))))))

(deftest cache-store-refuses-a-non-string-value
  (with-world ()
    (let ((id (generate-session-id)))
      (stack-cache:cache-put *cache* (cache-key id) 42)
      (let ((c (capture (session-store-error) (session-store-load *store* id))))
        (ok (typep c 'session-store-error))
        (ok (and c (eq :load (session-store-error-operation c))))
        (ok (and c (eq *store* (session-store-error-store c))))))))

(deftest cache-store-refuses-a-corrupt-record
  (with-world ()
    (let ((id (generate-session-id)))
      (stack-cache:cache-put *cache* (cache-key id) "{not json")
      (let ((c (capture (session-store-error) (session-store-load *store* id))))
        (ok (typep c 'session-store-error))
        (ok (and c (eq :load (session-store-error-operation c))))
        (ok (and c (eq *store* (session-store-error-store c))) "STORE is set")
        (ok (and c (typep (session-store-error-cause c) 'stack-json:json-error))))
      (stack-cache:cache-put *cache* (cache-key id) "{\"v\":9}")
      (let ((c (capture (session-store-error) (session-store-load *store* id))))
        (ok (and c (eq *store* (session-store-error-store c))))
        (ok (and c (null (session-store-error-cause c))))))))

(deftest-parametrize cache-store-wraps-cache-errors
    ((op operation call)
     :ids ("load" "save" "delete")
     (:get :load (lambda (s) (session-store-load *store* (session-id s))))
     (:put :save (lambda (s) (session-store-save *store* s 60)))
     (:delete :delete (lambda (s) (session-store-delete *store* (session-id s)))))
  (with-world ()
    (let ((s (stored-session "k" 1)))
      (fail-next *cache* op)
      (let ((c (capture (session-store-error) (funcall call s))))
        (ok (typep c 'session-store-error))
        (ok (and c (eq operation (session-store-error-operation c))))
        (ok (and c (eq *store* (session-store-error-store c))))
        (ok (and c (typep (session-store-error-cause c)
                          'stack-cache:cache-connection-error)))
        (ok (and c (no-id-in (princ-to-string c) (session-id s)))
            "the report has no id")))))

(defclass restarting-cache (test-cache) ())

(defmethod stack-cache:cache-get :around ((cache restarting-cache) key &key)
  (declare (ignore key))
  (restart-case (call-next-method)
    (test-reconnect () :report "Test: reconnect, find nothing" nil)))

(deftest cache-store-keeps-the-cache-restarts-live
  (with-world (:cache (make-instance 'restarting-cache :clock 'test-now))
    (fail-next *cache* :get)
    (let* ((seen nil)
           (result (handler-bind ((session-store-error
                                    (lambda (c)
                                      (declare (ignore c))
                                      (let ((r (find-restart 'test-reconnect)))
                                        (setf seen (and r t))
                                        (when r (invoke-restart r))))))
                     (session-store-load *store* (generate-session-id)))))
      (ok seen "the cache's restart is visible from a SESSION-STORE-ERROR handler")
      (ok (null result) "and the caller can take it: the load found nothing"))))

(deftest cache-store-lets-value-errors-through
  (with-world ()
    (let ((s (stored-session "k" 1)))
      (setf (gethash "bad" (session-data s)) #\x)
      (ok (signals (session-store-save *store* s 60) 'session-value-error))
      (ok (null (peek (session-id s))) "nothing was written")
      (ok (= 0 (cache-puts *cache*)) "the encoding runs before the cache is called")
      (handler-bind ((session-value-error #'skip-value))
        (session-store-save *store* s 60))
      (ok (stringp (peek (session-id s))))
      (ok (json-equal (table "k" 1) (stored-data (session-id s)))))))
