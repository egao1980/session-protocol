;;;; Constructors and their argument checks (given in the contract).

(in-package #:session-protocol/tests)

;;; Cookie policy

(deftest cookie-policy-defaults
  (let ((p (make-cookie-policy)))
    (ok (equal "sid" (cookie-policy-name p)))
    (ok (eq :lax (cookie-policy-same-site p)))
    (ok (eq :auto (cookie-policy-secure p)))))

(deftest-parametrize cookie-policy-accepts
    ((args)
     :ids ("plain" "one-char" "underscore-hyphen" "digits" "strict" "secure"
           "host-prefix-secure" "secure-prefix-secure")
     ('(:name "sid"))
     ('(:name "s"))
     ('(:name "my_session-1"))
     ('(:name "0123"))
     ('(:same-site :strict))
     ('(:secure t))
     ('(:name "__Host-sid" :secure t))
     ('(:name "__Secure-sid" :secure t)))
  (ok (typep (apply #'make-cookie-policy args) 'cookie-policy)))

(deftest-parametrize cookie-policy-refuses
    ((args)
     :ids ("empty-name" "space" "semicolon" "equals" "comma" "non-ascii" "quote"
           "symbol-name" "number-name" "nil-name"
           "host-prefix-auto" "host-prefix-lowercase" "secure-prefix-auto"
           "same-site-none" "same-site-string" "same-site-nil"
           "secure-nil" "secure-other")
     ('(:name ""))
     ('(:name "s id"))
     ('(:name "s;id"))
     ('(:name "a=b"))
     ('(:name "a,b"))
     ('(:name "sïd"))
     ('(:name "\"sid\""))
     ('(:name sid))
     ('(:name 42))
     ('(:name nil))
     ('(:name "__Host-sid"))
     ('(:name "__host-sid"))
     ('(:name "__Secure-sid" :secure :auto))
     ('(:same-site :none))
     ('(:same-site "Lax"))
     ('(:same-site nil))
     ('(:secure nil))
     ('(:secure :always)))
  (ok (signals (apply #'make-cookie-policy args) 'type-error)))

;;; Cache store

(deftest cache-session-store-constructor
  (let* ((cache (stack-cache:make-in-memory-ttl-cache))
         (s (make-cache-session-store cache)))
    (ok (typep s 'session-store))
    (ok (eq cache (cache-session-store-cache s)))
    (ok (equal "session:" (cache-session-store-prefix s)))
    (ok (equal "" (cache-session-store-prefix
                   (make-cache-session-store cache :prefix ""))))
    (ok (equal "app1:sess:" (cache-session-store-prefix
                             (make-cache-session-store cache :prefix "app1:sess:"))))
    (ok (signals (make-cache-session-store :not-a-cache) 'type-error))
    (ok (signals (make-cache-session-store (make-hash-table)) 'type-error))
    (ok (signals (make-cache-session-store cache :prefix :session) 'type-error))
    (ok (signals (make-cache-session-store cache :prefix nil) 'type-error))))

;;; Manager

(deftest session-manager-defaults
  (let ((m (make-session-manager)))
    (ok (= 86400 (session-manager-idle-ttl m)))
    (ok (null (session-manager-absolute-ttl m)))
    (ok (= 60 (session-manager-refresh-interval m)))
    (ok (typep (session-manager-cookie-policy m) 'cookie-policy))
    (ok (equal "sid" (cookie-policy-name (session-manager-cookie-policy m))))
    (ok (typep (session-manager-store m) 'cache-session-store))
    (ok (typep (cache-session-store-cache (session-manager-store m))
               'stack-cache:in-memory-ttl-cache))
    (ok (not (eq (session-manager-store m)
                 (session-manager-store (make-session-manager))))
        "each manager gets its own default store")
    (let ((now (funcall (session-manager-clock m)))
          (unix (- (get-universal-time) 2208988800)))
      (ok (realp now))
      (ok (<= (abs (- now unix)) 2) "the default clock is Unix time"))))

(deftest-parametrize session-manager-refresh-default
    ((idle expected)
     :ids ("one-day" "two-minutes" "ten-seconds" "three-seconds" "one-second")
     (86400 60)
     (120 60)
     (10 5)
     (3 1)
     (1 0))
  (ok (= expected (session-manager-refresh-interval
                   (make-session-manager :idle-ttl idle)))))

(deftest session-manager-keeps-arguments
  (let* ((store (make-cache-session-store (stack-cache:make-in-memory-ttl-cache)))
         (policy (make-cookie-policy :name "visitor"))
         (m (make-session-manager :store store :cookie-policy policy
                                  :idle-ttl 600 :absolute-ttl 3600
                                  :refresh-interval 0 :clock 'test-now)))
    (ok (eq store (session-manager-store m)))
    (ok (eq policy (session-manager-cookie-policy m)))
    (ok (= 600 (session-manager-idle-ttl m)))
    (ok (= 3600 (session-manager-absolute-ttl m)))
    (ok (= 0 (session-manager-refresh-interval m)))
    (ok (eq 'test-now (session-manager-clock m)))))

(deftest-parametrize session-manager-refuses
    ((args)
     :ids ("idle-zero" "idle-negative" "idle-float" "idle-nil" "absolute-zero"
           "absolute-float" "refresh-equal-idle" "refresh-above-idle"
           "refresh-negative" "store" "policy" "clock")
     ('(:idle-ttl 0))
     ('(:idle-ttl -5))
     ('(:idle-ttl 1.5))
     ('(:idle-ttl nil))
     ('(:absolute-ttl 0))
     ('(:absolute-ttl 10.0))
     ('(:idle-ttl 100 :refresh-interval 100))
     ('(:idle-ttl 100 :refresh-interval 500))
     ('(:refresh-interval -1))
     ('(:store :not-a-store))
     ('(:cookie-policy "sid"))
     ('(:clock 42)))
  (ok (signals (apply #'make-session-manager args) 'type-error)))

;;; Session construction and printing

(deftest make-session-checks-and-defaults
  (let ((s (make-session :id "an-id" :created-at 10 :last-seen 20)))
    (ok (equal "an-id" (session-id s)))
    (ok (= 10 (session-created-at s)))
    (ok (= 20 (session-last-seen s)))
    (ok (hash-table-p (session-data s)))
    (ok (eq 'equal (hash-table-test (session-data s))))
    (ok (zerop (hash-table-count (session-data s))))
    (ok (not (session-new-p s)))
    (ok (not (or (session-changed-p s) (session-rotated-p s)
                 (session-ended-p s) (session-committed-p s)))))
  (let ((data (table "a" 1)))
    (ok (eq data (session-data (make-session :id "x" :data data
                                             :created-at 0 :last-seen 0)))
        "DATA is used as is"))
  (ok (session-new-p (make-session :id "x" :created-at 0 :last-seen 0 :new-p t)))
  (ok (signals (make-session :id 42 :created-at 0 :last-seen 0) 'type-error))
  (ok (signals (make-session :id "x" :created-at 0) 'type-error))
  (ok (signals (make-session :id "x" :created-at 1.5 :last-seen 0) 'type-error))
  (ok (signals (make-session :id "x" :data (make-hash-table) :created-at 0
                             :last-seen 0)
               'type-error)
      "an EQL table is refused")
  (ok (signals (make-session :id "x" :data '(("a" . 1)) :created-at 0 :last-seen 0)
               'type-error)))

(deftest session-prints-without-its-id
  (let* ((id "AbCdEfGhIjKlMnOpQrStUvWxYz0123456789_-abcd")
         (s (make-session :id id :data (table "a" 1 "b" 2) :created-at 0
                          :last-seen 0 :new-p t))
         (printed (prin1-to-string s)))
    (ok (no-id-in printed id))
    (ok (search "SESSION" printed))
    (ok (search "new" printed))
    (ok (search "2 keys" printed))))
