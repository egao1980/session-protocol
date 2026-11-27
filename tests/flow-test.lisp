;;;; BEGIN-SESSION and FINISH-SESSION over several requests: lazy creation,
;;;; a multi-page flow, cookies exactly, ids that are malformed, forged or
;;;; mismatched, the app's headers.

(in-package #:session-protocol/tests)

;;; One request by hand

(deftest begin-session-without-cookie
  (with-world ()
    (let ((env (make-env)))
      (multiple-value-bind (s env*) (begin-session *manager* env)
        (ok (typep s 'session))
        (ok (session-new-p s))
        (ok (valid-session-id-p (session-id s)) "a new session has an id at once")
        (ok (= +t0+ (session-created-at s)))
        (ok (= +t0+ (session-last-seen s)))
        (ok (zerop (hash-table-count (session-data s))))
        (ok (not (or (session-changed-p s) (session-rotated-p s)
                     (session-ended-p s) (session-committed-p s))))
        (ok (eq s (getf env* :session)) "the session is under :SESSION")
        (ok (eq s (env-session env*)))
        (ok (eq env (cddr env*)) "the env is ENV with (:SESSION s) in front")
        (ok (null (getf env :session)) "ENV itself is not modified")
        (ok (null (env-session env)))
        (ok (equal '(0 0 0) (calls *cache*)) "no cookie: no store call at all")))))

(deftest env-session-reads-the-key
  (ok (null (env-session (make-env))))
  (ok (eq :x (env-session (list* :session :x (make-env))))))

(deftest a-request-that-stores-nothing-sets-nothing
  (with-world ()
    (let* ((response (ok-response))
           (result (call-app (app (lambda (s) (session-value s "anything")) response)
                             (make-env))))
      (ok (eq response result) "the app's response is returned as is")
      (ok (null (set-cookies result)) "no Set-Cookie")
      (ok (equal '(0 0 0) (calls *cache*)) "no store entry, no store call"))))

(deftest a-request-that-stores-sets-the-cookie
  (with-world ()
    (let* ((response (call-app (setter-app "step" 1) (make-env)))
           (id (cookie-id response)))
      (ok (valid-session-id-p id))
      (ok (= 1 (length (set-cookies response))) "one Set-Cookie")
      (ok (equal (format-session-cookie (make-cookie-policy) (make-env) id 86400)
                 (session-cookie response))
          "Max-Age is the idle TTL")
      (let ((record (stored id)))
        (ok record "the record is stored under the cookie's id")
        (ok (eql 1 (gethash "v" record)))
        (ok (equal id (gethash "id" record)))
        (ok (= +t0+ (gethash "created" record)))
        (ok (= +t0+ (gethash "seen" record)))
        (ok (json-equal (table "step" 1) (gethash "data" record))))
      (ok (equal '(0 1 0) (calls *cache*)) "one put, nothing else"))))

(deftest the-cookie-attributes-exactly
  (with-world ()
    (let* ((response (call-app (setter-app "k" "v") (make-env)))
           (attributes (cookie-attributes response)))
      (ok (same-attributes-p '(("max-age" . "86400") ("path" . "/")
                               ("samesite" . "Lax") ("httponly"))
                             attributes)
          "Max-Age, Path=/, SameSite=Lax, HttpOnly; no Secure on http, no Domain")))
  (with-world ()
    (let ((response (call-app (setter-app "k" "v") (make-env :scheme "https"))))
      (ok (same-attributes-p '(("max-age" . "86400") ("path" . "/")
                               ("samesite" . "Lax") ("httponly") ("secure"))
                             (cookie-attributes response))
          "Secure on https")))
  (with-world (:cookie-policy (make-cookie-policy :secure t :same-site :strict
                                                  :name "visitor")
               :idle-ttl 600)
    (let ((response (call-app (setter-app "k" "v") (make-env))))
      (ok (null (session-cookie response "sid")))
      (ok (same-attributes-p '(("max-age" . "600") ("path" . "/")
                               ("samesite" . "Strict") ("httponly") ("secure"))
                             (cookie-attributes response "visitor"))
          "the policy's name, SameSite and forced Secure; Max-Age the idle TTL"))))

;;; A multi-page flow

(deftest a-flow-keeps-its-values
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "step") 1
                                 (session-value s "answers") (list "a"))))
      (let ((id (browser-sid b)))
        (ok (valid-session-id-p id))
        (incf *now* 10)
        (multiple-value-bind (seen response)
            (visit b (lambda (s)
                       (prog1 (list (session-new-p s) (session-value s "step")
                                    (session-value s "answers"))
                         (incf (session-value s "step"))
                         (push "b" (session-value s "answers")))))
          (ok (equal '(nil 1 ("a")) seen) "page 2 sees page 1's values")
          (ok (equal id (cookie-id response)) "a write re-sends the same id"))
        (incf *now* 10)
        (let ((seen (visit b (lambda (s) (list (session-id s) (session-value s "step")
                                               (session-value s "answers"))))))
          (ok (equal (list id 2 '("b" "a")) seen) "page 3 sees page 2's values"))
        (ok (equal id (browser-sid b)) "one session for the whole flow")))))

(deftest another-browser-gets-another-session
  (with-world ()
    (let ((alice (make-browser)) (bob (make-browser)))
      (visit alice (lambda (s) (setf (session-value s "who") "alice")))
      (visit bob (lambda (s) (setf (session-value s "who") "bob")))
      (ok (string/= (browser-sid alice) (browser-sid bob)))
      (ok (equal "alice" (visit alice (lambda (s) (session-value s "who")))))
      (ok (equal "bob" (visit bob (lambda (s) (session-value s "who")))))
      (ok (null (visit (make-browser) (lambda (s) (session-value s "who"))))
          "a third browser sees nothing"))))

(deftest a-loaded-session-is-not-new
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "k") 1)))
      (incf *now* 5)
      (let ((flags (visit b (lambda (s) (list (session-new-p s) (session-created-at s)
                                              (session-last-seen s))))))
        (ok (equal (list nil +t0+ +t0+) flags))))))

;;; Ids the client sent that the server never issued

(deftest-parametrize malformed-cookies-never-reach-the-store
    ((value)
     :ids ("short" "long" "plus" "slash" "padding" "empty" "space-inside" "quoted")
     ("abc")
     ((make-string 44 :initial-element #\A))
     ((concatenate 'string (make-string 42 :initial-element #\A) "+"))
     ((concatenate 'string (make-string 42 :initial-element #\A) "/"))
     ((concatenate 'string (make-string 42 :initial-element #\A) "="))
     ("")
     ((concatenate 'string (make-string 20 :initial-element #\A) "%20"
                   (make-string 20 :initial-element #\A)))
     ((concatenate 'string "\"" (make-string 41 :initial-element #\A) "\"")))
  (with-world ()
    (let ((seen nil))
      (let ((response (call-app (app (lambda (s) (setf seen s)))
                                (make-env :cookie (format nil "sid=~a" value)))))
        (ok (equal '(0 0 0) (calls *cache*)) "no lookup")
        (ok (session-new-p seen) "a fresh session, no error")
        (ok (string/= value (session-id seen)))
        (ok (null (set-cookies response)) "nothing stored, no cookie")))))

(deftest a-well-formed-cookie-is-looked-up-once
  (with-world ()
    (call-app (reader-app) (make-env :cookie (format nil "sid=~a" (generate-session-id))))
    (ok (equal '(1 0 0) (calls *cache*)))))

(deftest a-forged-id-is-never-adopted
  (with-world ()
    (let* ((forged (generate-session-id))
           (seen nil)
           (response (call-app (app (lambda (s)
                                      (setf seen s)
                                      (setf (session-value s "k") 1)))
                               (make-env :cookie (format nil "sid=~a" forged)))))
      (ok (session-new-p seen) "unknown id: a new session, no error")
      (ok (string/= forged (session-id seen)) "with a fresh id")
      (ok (equal (session-id seen) (cookie-id response)) "the cookie carries the fresh id")
      (ok (null (peek forged)) "nothing is ever stored under the forged id"))))

(deftest a-record-under-another-id-is-not-handed-out
  (with-world ()
    (let* ((victim (stored-session "secret" "theirs"))
           (asked (generate-session-id)))
      ;; A loose store: the record found under ASKED says it is VICTIM's.
      (stack-cache:cache-put *cache* (cache-key asked) (encode-session-record victim))
      (let ((seen (visit (make-browser :cookies (list (cons "sid" asked)))
                         (lambda (s) (list (session-new-p s) (session-value s "secret"))))))
        (ok (equal '(t nil) seen) "a mismatched id is unknown")
        (ok (stringp (peek asked)) "and the record is left alone")))))

(deftest only-the-first-cookie-of-the-name-counts
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "k") "real")))
      (let ((real (browser-sid b)))
        (ok (equal "real"
                   (multiple-value-bind (s) (begin-session *manager*
                                                           (make-env :cookie (format nil "theme=1; sid=~a; sid=~a" real (generate-session-id))))
                     (session-value s "k"))))
        (ok (null (multiple-value-bind (s) (begin-session *manager*
                                                          (make-env :cookie (format nil "sid=~a; sid=~a" (generate-session-id) real)))
                    (session-value s "k")))
            "a later cookie of the same name is ignored")))))

;;; The response

(deftest the-apps-headers-are-kept
  (with-world ()
    (let* ((original (list 201 (list :content-type "text/html" :set-cookie "theme=dark; Path=/"
                                     :x-trace "t1" :set-cookie "lang=en")
                           (list "<p>hi</p>")))
           (copy (copy-tree original))
           (response (call-app (app (lambda (s) (setf (session-value s "k") 1)) original)
                               (make-env))))
      (ok (equal copy original) "the app's list is not modified")
      (ok (not (eq original response)) "a fresh list")
      (ok (= 201 (first response)))
      (ok (equal '("<p>hi</p>") (third response)) "same body")
      (ok (= 3 (length response)))
      (let ((headers (headers-of response)))
        (ok (equal (subseq headers 0 8) (second copy)) "the app's headers first, unchanged")
        (ok (eq :set-cookie (nth 8 headers)) "then ours")
        (ok (= 10 (length headers)))
        (ok (equal (list "theme=dark; Path=/" "lang=en" (session-cookie response))
                   (set-cookies response))
            "every Set-Cookie of the app survives, ours last")))))

(deftest a-response-without-body-stays-without-body
  (with-world ()
    (let ((response (call-app (app (lambda (s) (setf (session-value s "k") 1))
                                   (list 204 (list :x-a "1")))
                              (make-env))))
      (ok (= 2 (length response)))
      (ok (= 204 (first response)))
      (ok (equal :x-a (first (headers-of response))))
      (ok (cookie-id response)))))

(deftest an-error-status-commits-too
  (with-world ()
    (let ((response (call-app (app (lambda (s) (setf (session-value s "k") 1))
                                   (list 500 nil (list "oops")))
                              (make-env))))
      (ok (= 500 (first response)))
      (ok (json-equal (table "k" 1) (stored-data (cookie-id response)))))))

(deftest finish-session-commits-once
  (with-world ()
    (multiple-value-bind (s env) (begin-session *manager* (make-env))
      (setf (session-value s "k") 1)
      (let* ((first (finish-session *manager* s env (ok-response)))
             (calls (calls *cache*))
             (response (ok-response))
             (second (finish-session *manager* s env response)))
        (ok (cookie-id first))
        (ok (session-committed-p s))
        (ok (eq response second) "a second finish returns the response unchanged")
        (ok (equal calls (calls *cache*)) "and calls no store")))))

(deftest-parametrize finish-session-refuses-a-non-response
    ((response) :ids ("string" "number" "nil" "symbol") ("ok") (42) (nil) (:ok))
  (with-world ()
    (multiple-value-bind (s env) (begin-session *manager* (make-env))
      (ok (signals (finish-session *manager* s env response) 'type-error)))))

(deftest data-changed-in-place-is-saved
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "cart") (table "items" (list "a")))))
      (incf *now* 5)
      (let ((puts (cache-puts *cache*)))
        (visit b (lambda (s)
                   (push "b" (gethash "items" (session-value s "cart")))
                   (ok (not (session-changed-p s)) "no accessor ran")))
        (ok (= (1+ puts) (cache-puts *cache*)) "but the commit noticed and saved"))
      (incf *now* 5)
      (ok (equal '("b" "a")
                 (visit b (lambda (s) (gethash "items" (session-value s "cart")))))))))

(deftest data-changed-through-session-data-is-saved
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "a") 1)))
      (incf *now* 5)
      (visit b (lambda (s) (setf (gethash "b" (session-data s)) 2)))
      (incf *now* 5)
      (ok (eql 2 (visit b (lambda (s) (session-value s "b"))))))))

(deftest last-write-wins
  (with-world ()
    (let ((b (make-browser)))
      (visit b (lambda (s) (setf (session-value s "base") 0)))
      (incf *now* 5)
      (let ((env (make-env :cookie (browser-cookie-header b))))
        ;; Two tabs: both requests load the session before either commits.
        (multiple-value-bind (s1 e1) (begin-session *manager* env)
          (multiple-value-bind (s2 e2) (begin-session *manager* env)
            (setf (session-value s1 "tab") 1)
            (setf (session-value s2 "tab") 2
                  (session-value s2 "only-2") t)
            (finish-session *manager* s1 e1 (ok-response))
            (finish-session *manager* s2 e2 (ok-response))))
        (ok (json-equal (table "base" 0 "tab" 2 "only-2" t)
                        (stored-data (browser-sid b)))
            "the later commit's data, whole")))))
