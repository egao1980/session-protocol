;;;; session-protocol S0 tests: package and helpers.
;;;;
;;;; The suite is the spec of slice S0 (brief: cl-stack/docs/capabilities/
;;;; session.md). It loads json-backend-jzon and crypto-backend-ironclad, the
;;;; backends an application would pick. Time is a special, *NOW* (Unix
;;;; seconds), read by both the manager and the in-memory cache, so expiry is
;;;; tested without sleeping.
;;;;
;;;; CALL-APP composes the two phases around a fake Clack app, as a middleware
;;;; would; it is test code, not part of the protocol. A BROWSER keeps the
;;;; cookies a response sets and sends them back, so a test walks a flow of
;;;; several requests.

(defpackage #:session-protocol/tests
  (:use #:cl #:rove #:session-protocol))

(in-package #:session-protocol/tests)

;;; Time

(defconstant +t0+ 1700000000 "The tests' start time, Unix seconds.")

(defvar *now* +t0+ "Current test time, Unix seconds.")

(defun test-now () *now*)

;;; A cache that counts calls and fails on demand

(defclass test-cache (stack-cache:in-memory-ttl-cache)
  ((gets :initform 0 :accessor cache-gets)
   (puts :initform 0 :accessor cache-puts)
   (deletes :initform 0 :accessor cache-deletes)
   ;; plist (:get n :put n :delete n): the next n calls of that kind signal
   ;; STACK-CACHE:CACHE-CONNECTION-ERROR.
   (failures :initform nil :accessor cache-failures)))

(defun make-test-cache (&key (clock 'test-now))
  (make-instance 'test-cache :clock clock))

(defun %maybe-fail (cache op key)
  (let ((n (getf (cache-failures cache) op 0)))
    (when (plusp n)
      (setf (getf (cache-failures cache) op) (1- n))
      (error 'stack-cache:cache-connection-error
             :key key :message "test: connection lost"))))

(defmethod stack-cache:cache-get :before ((cache test-cache) key &key)
  (incf (cache-gets cache))
  (%maybe-fail cache :get key))

(defmethod stack-cache:cache-put :before ((cache test-cache) key value &key ttl)
  (declare (ignore value ttl))
  (incf (cache-puts cache))
  (%maybe-fail cache :put key))

(defmethod stack-cache:cache-delete :before ((cache test-cache) key &key)
  (incf (cache-deletes cache))
  (%maybe-fail cache :delete key))

(defun fail-next (cache op &optional (n 1))
  (setf (getf (cache-failures cache) op) n))

(defun calls (cache)
  "(gets puts deletes) so far."
  (list (cache-gets cache) (cache-puts cache) (cache-deletes cache)))

;;; The world of one test

(defvar *cache*)
(defvar *store*)
(defvar *manager*)

(defmacro with-world ((&rest manager-args &key (cache '(make-test-cache))
                       &allow-other-keys)
                      &body body)
  "Run BODY with *NOW* at +T0+, a fresh TEST-CACHE, a CACHE-SESSION-STORE over
it, and a SESSION-MANAGER on the test clock (MANAGER-ARGS passed on)."
  (let ((args (loop for (k v) on manager-args by #'cddr
                    unless (eq k :cache) append (list k v))))
    `(let* ((*now* +t0+)
            (*cache* ,cache)
            (*store* (make-cache-session-store *cache*))
            (*manager* (make-session-manager :store *store* :clock 'test-now
                                             ,@args)))
       ,@body)))

(defun cache-key (id &optional (prefix "session:"))
  (concatenate 'string prefix id))

(defun peek (id &key (cache *cache*) (prefix "session:"))
  "The cached value for ID, without counting the call."
  (let ((n (cache-gets cache)))
    (prog1 (stack-cache:cache-get cache (cache-key id prefix))
      (setf (cache-gets cache) n))))

(defun stored (id &key (cache *cache*) (prefix "session:"))
  "The stored record for ID decoded as JSON (a hash-table), or NIL."
  (let ((text (peek id :cache cache :prefix prefix)))
    (and text (stack-json:decode text))))

(defun stored-data (id)
  (let ((record (stored id)))
    (and record (gethash "data" record))))

;;; Clack envs and responses

(defun make-env (&key cookie (scheme "http") (path "/") (method :get)
                      (headers t))
  "A Clack env. COOKIE is the Cookie header text or NIL. HEADERS NIL leaves
out :HEADERS altogether."
  (let ((table (make-hash-table :test 'equal)))
    (when cookie
      (setf (gethash "cookie" table) cookie))
    (append (list :request-method method :script-name "" :path-info path
                  :request-uri path :query-string nil
                  :server-name "localhost" :server-port 80
                  :server-protocol :http/1.1 :url-scheme scheme
                  :remote-addr "127.0.0.1" :remote-port 50000)
            (and headers (list :headers table)))))

(defun call-app (app env &key (manager *manager*))
  "BEGIN-SESSION, the app on the env that carries the session, FINISH-SESSION:
what a middleware does. Test code."
  (multiple-value-bind (session env*) (begin-session manager env)
    (finish-session manager session env* (funcall app env*))))

(defun ok-response (&rest headers)
  (list 200 (append '(:content-type "text/plain") headers) '("ok")))

(defun app (fn &optional (response (ok-response)))
  "An app that calls FN on the session, then answers RESPONSE."
  (lambda (env)
    (funcall fn (env-session env))
    response))

(defun reader-app () (app (lambda (s) (session-value s "anything"))))

(defun setter-app (key value)
  (app (lambda (s) (setf (session-value s key) value))))

(defun headers-of (response) (second response))

(defun set-cookies (response)
  "Every :SET-COOKIE value of RESPONSE's headers, in order."
  (loop for (k v) on (headers-of response) by #'cddr
        when (eq k :set-cookie) collect v))

(defun split-attributes (text)
  (loop with start = 0
        for pos = (position #\; text :start start)
        collect (string-trim " " (subseq text start pos))
        while pos
        do (setf start (1+ pos))))

(defun parse-set-cookie (text)
  "(values name value attributes) of a Set-Cookie value. ATTRIBUTES is a list
of (lowercased-name . value-or-NIL) in order."
  (destructuring-bind (pair &rest attributes) (split-attributes text)
    (let ((eq (position #\= pair)))
      (values (subseq pair 0 eq)
              (subseq pair (1+ eq))
              (loop for a in attributes
                    for e = (position #\= a)
                    collect (cons (string-downcase (subseq a 0 e))
                                  (and e (subseq a (1+ e)))))))))

(defun session-cookie (response &optional (name "sid"))
  "The last Set-Cookie of RESPONSE for NAME, or NIL."
  (car (last (remove-if-not (lambda (c)
                              (string= name (nth-value 0 (parse-set-cookie c))))
                            (set-cookies response)))))

(defun cookie-id (response &optional (name "sid"))
  "The value of RESPONSE's session cookie, or NIL."
  (let ((c (session-cookie response name)))
    (and c (nth-value 1 (parse-set-cookie c)))))

(defun cookie-attributes (response &optional (name "sid"))
  (let ((c (session-cookie response name)))
    (and c (nth-value 2 (parse-set-cookie c)))))

(defun attribute (response name &optional (cookie "sid"))
  "The value of attribute NAME (lowercase) of the session cookie; second value
T when present."
  (let ((a (assoc name (cookie-attributes response cookie) :test #'string=)))
    (values (cdr a) (and a t))))

(defun same-attributes-p (a b)
  "A and B hold the same (name . value) attributes, order aside."
  (and (= (length a) (length b))
       (every (lambda (x) (member x b :test #'equal)) a)))

;;; A browser

(defstruct browser
  (cookies nil)            ; alist (name . value), oldest first
  (scheme "http"))

(defun browser-cookie-header (browser)
  (and (browser-cookies browser)
       (format nil "~{~a~^; ~}"
               (mapcar (lambda (c) (format nil "~a=~a" (car c) (cdr c)))
                       (browser-cookies browser)))))

(defun browser-apply (browser response)
  (dolist (text (set-cookies response))
    (multiple-value-bind (name value attributes) (parse-set-cookie text)
      (let ((max-age (cdr (assoc "max-age" attributes :test #'string=))))
        (setf (browser-cookies browser)
              (remove name (browser-cookies browser) :key #'car :test #'string=))
        (unless (and max-age (<= (parse-integer max-age) 0))
          (setf (browser-cookies browser)
                (append (browser-cookies browser) (list (cons name value))))))))
  response)

(defun browse (browser app &key (manager *manager*))
  "One request of BROWSER through APP; the response's cookies are kept."
  (browser-apply browser
                 (call-app app (make-env :cookie (browser-cookie-header browser)
                                         :scheme (browser-scheme browser))
                           :manager manager)))

(defun browser-sid (browser &optional (name "sid"))
  (cdr (assoc name (browser-cookies browser) :test #'string=)))

(defun visit (browser fn)
  "One request whose app calls FN on the session; returns FN's value and the
response."
  (let* ((result nil)
         (response (browse browser (app (lambda (s) (setf result (funcall fn s)))))))
    (values result response)))

;;; Values

(defun json-equal (a b)
  "A and B are the same JSON data: strings by STRING=, numbers by =, lists
and vectors element by element (a vector equals the list of its elements),
hash-tables by key set and values."
  (when (and (vectorp a) (not (stringp a))) (setf a (coerce a 'list)))
  (when (and (vectorp b) (not (stringp b))) (setf b (coerce b 'list)))
  (cond ((and (stringp a) (stringp b)) (string= a b))
        ((and (numberp a) (numberp b)) (= a b))
        ((and (hash-table-p a) (hash-table-p b))
         (and (= (hash-table-count a) (hash-table-count b))
              (loop for k being the hash-keys of a using (hash-value v)
                    always (multiple-value-bind (w present) (gethash k b)
                             (and present (json-equal v w))))))
        ((and (consp a) (consp b))
         (and (= (length a) (length b)) (every #'json-equal a b)))
        (t (eq a b))))

(defun table (&rest kvs)
  "An EQUAL hash-table of KVS."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun nested-lists (depth &optional (leaf 1))
  "LEAF inside DEPTH nested one-element lists."
  (let ((v leaf))
    (dotimes (i depth v) (setf v (list v)))))

(defun level (value n)
  "The list at nesting level N (1 = VALUE itself) of NESTED-LISTS output."
  (dotimes (i (1- n) value) (setf value (first value))))

(defun fresh-session (&rest kvs)
  "A new session holding KVS, as BEGIN-SESSION starts one (not stored)."
  (let ((s (make-session :id (generate-session-id) :created-at *now*
                         :last-seen *now* :new-p t)))
    (loop for (k v) on kvs by #'cddr do (setf (session-value s k) v))
    s))

(defun mark-committed (session)
  (setf (slot-value session 'session-protocol::committed) t)
  session)

(defmacro capture ((type) &body body)
  "The condition of TYPE BODY signals (unwinding), or NIL."
  `(handler-case (progn ,@body nil)
     (,type (c) c)))

(defun no-id-in (string id)
  (not (search id string)))
