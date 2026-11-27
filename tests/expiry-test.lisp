;;;; Lifetimes on an injected clock: idle expiry, the refresh-write rule,
;;;; the absolute lifetime, a store whose TTL lags.

(in-package #:session-protocol/tests)

(defun started-browser (&rest kvs)
  "A browser whose session (holding KVS) was written at the current time."
  (let ((b (make-browser)))
    (visit b (lambda (s) (loop for (k v) on kvs by #'cddr
                               do (setf (session-value s k) v))))
    b))

(defun frozen-cache ()
  "A cache whose own clock never moves: its entries never expire, as in a
store whose TTL lags."
  (make-test-cache :clock (constantly 0)))

;;; Idle expiry

(deftest idle-expiry-boundary
  (with-world (:idle-ttl 600 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 599))
      (ok (eql 1 (visit b (lambda (s) (session-value s "k")))) "alive at 599 s")
      (ok (equal id (browser-sid b)))))
  (with-world (:idle-ttl 600 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 600))
      (multiple-value-bind (seen response)
          (visit b (lambda (s) (list (session-new-p s) (session-value s "k"))))
        (ok (equal '(t nil) seen) "gone at 600 s: a new session, no error")
        (ok (null (set-cookies response)) "the new one stores nothing, no cookie"))
      (ok (null (peek id))))))

(deftest a-busy-session-slides
  (with-world (:idle-ttl 600 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (loop repeat 6
            do (incf *now* 599)
               (ok (eql 1 (visit b (lambda (s) (session-value s "k"))))))
      (ok (equal id (browser-sid b)) "one session for 3594 s of 599 s gaps"))))

;;; The refresh-write rule

(deftest unchanged-session-is-refreshed-at-most-once-per-interval
  (with-world (:idle-ttl 600 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b))
           (puts (cache-puts *cache*)))
      (setf *now* (+ +t0+ 30))
      (multiple-value-bind (v response) (visit b (lambda (s) (session-value s "k")))
        (ok (eql 1 v))
        (ok (null (set-cookies response)) "30 s: no Set-Cookie")
        (ok (= puts (cache-puts *cache*)) "30 s: no write"))
      (setf *now* (+ +t0+ 59))
      (ok (null (set-cookies (nth-value 1 (visit b #'session-id)))) "59 s: none")
      (ok (= puts (cache-puts *cache*)))
      (setf *now* (+ +t0+ 60))
      (let ((response (nth-value 1 (visit b #'session-id))))
        (ok (= (1+ puts) (cache-puts *cache*)) "60 s: written again")
        (ok (equal id (cookie-id response)) "same id")
        (ok (equal "600" (attribute response "max-age"))
            "and the cookie re-sent with a full Max-Age"))
      (ok (= (+ +t0+ 60) (gethash "seen" (stored id))) "LAST-SEEN is the write time")
      (ok (= +t0+ (gethash "created" (stored id))) "CREATED never moves")
      (setf *now* (+ +t0+ 100))
      (visit b #'session-id)
      (ok (= (1+ puts) (cache-puts *cache*)) "100 s: 40 s after the write, none")
      (setf *now* (+ +t0+ 120))
      (visit b #'session-id)
      (ok (= (+ 2 puts) (cache-puts *cache*)) "120 s: written again")
      (setf *now* (+ +t0+ 120 600))
      (ok (session-new-p (visit b #'identity))
          "idle TTL counts from the last write"))))

(deftest a-change-is-written-whatever-the-interval
  (with-world (:idle-ttl 600 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (puts (cache-puts *cache*)))
      (setf *now* (+ +t0+ 1))
      (let ((response (nth-value 1 (visit b (lambda (s) (setf (session-value s "k") 2))))))
        (ok (= (1+ puts) (cache-puts *cache*)))
        (ok (cookie-id response) "a write always sends the cookie")))))

(deftest refresh-interval-zero-writes-every-request
  (with-world (:idle-ttl 600 :refresh-interval 0)
    (let* ((b (started-browser "k" 1))
           (puts (cache-puts *cache*)))
      (visit b #'session-id)
      (visit b #'session-id)
      (ok (= (+ 2 puts) (cache-puts *cache*)) "even in the same second"))))

(deftest a-new-session-is-never-refreshed
  (with-world (:idle-ttl 600 :refresh-interval 0)
    (call-app (reader-app) (make-env))
    (ok (equal '(0 0 0) (calls *cache*)) "the rule is for loaded sessions only")))

;;; A store whose TTL lags: the manager checks the times itself

(deftest idle-expiry-is-checked-on-load
  (with-world (:idle-ttl 600 :refresh-interval 60 :cache (frozen-cache))
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 600))
      (ok (stringp (peek id)) "the cache still holds the record")
      (ok (session-new-p (visit b #'identity)) "but it is expired")
      (ok (null (peek id)) "and it was deleted"))))

;;; The absolute lifetime

(deftest absolute-lifetime-ends-a-busy-session
  (with-world (:idle-ttl 86400 :absolute-ttl 300 :refresh-interval 0)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (ok (equal "300" (attribute (nth-value 1 (visit b #'session-id)) "max-age"))
          "Max-Age is capped by the absolute lifetime")
      (setf *now* (+ +t0+ 100))
      (ok (equal "200" (attribute (nth-value 1 (visit b #'session-id)) "max-age"))
          "Max-Age = what is left of it")
      (setf *now* (+ +t0+ 299))
      (ok (eql 1 (visit b (lambda (s) (session-value s "k")))) "alive at 299 s")
      (ok (equal "1" (attribute (nth-value 1 (visit b #'session-id)) "max-age")))
      (setf *now* (+ +t0+ 300))
      (ok (session-new-p (visit b #'identity)) "gone at 300 s")
      (ok (null (peek id))))))

(deftest absolute-lifetime-caps-the-store-ttl
  (with-world (:idle-ttl 86400 :absolute-ttl 300 :refresh-interval 0)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 100))
      (visit b #'session-id)
      (setf *now* (+ +t0+ 299))
      (ok (stringp (peek id)))
      (setf *now* (+ +t0+ 300))
      (ok (null (peek id)) "the cache entry ends with the lifetime"))))

(deftest absolute-lifetime-is-checked-on-load
  (with-world (:idle-ttl 86400 :absolute-ttl 300 :refresh-interval 60
               :cache (frozen-cache))
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 300))
      (ok (stringp (peek id)))
      (ok (session-new-p (visit b #'identity)) "expired by CREATED + ABSOLUTE-TTL")
      (ok (null (peek id)) "and deleted"))))

(deftest absolute-lifetime-running-out-during-a-request
  (with-world (:idle-ttl 86400 :absolute-ttl 300 :refresh-interval 60)
    (let* ((b (started-browser "k" 1))
           (id (browser-sid b)))
      (setf *now* (+ +t0+ 299))
      (let ((response (browse b (app (lambda (s)
                                       (setf (session-value s "k") 2)
                                       (setf *now* (+ +t0+ 300)))))))
        (ok (equal "" (cookie-id response)) "the session ends: the expiry cookie")
        (ok (equal "0" (attribute response "max-age")))
        (ok (null (peek id)) "and its record is deleted")
        (ok (null (browser-sid b)))))))

(deftest rotation-does-not-extend-the-absolute-lifetime
  (with-world (:idle-ttl 86400 :absolute-ttl 300 :refresh-interval 60)
    (let ((b (started-browser "k" 1)))
      (setf *now* (+ +t0+ 200))
      (let ((response (nth-value 1 (visit b #'regenerate-session-id))))
        (ok (equal "100" (attribute response "max-age")))
        (ok (= +t0+ (gethash "created" (stored (browser-sid b))))))
      (setf *now* (+ +t0+ 300))
      (ok (session-new-p (visit b #'identity))))))
