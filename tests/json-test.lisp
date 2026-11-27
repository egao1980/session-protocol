;;;; JSON: SESSION-DATA-JSON, ENCODE-SESSION-RECORD, DECODE-SESSION-RECORD.

(in-package #:session-protocol/tests)

(defun session-with (&rest kvs)
  (let ((s (make-session :id "IdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdId0"
                         :created-at 1700000000 :last-seen 1700000300)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k (session-data s)) v))
    s))

(deftest-parametrize data-json-text
    ((kvs expected)
     :ids ("empty" "sorted-keys" "scalars" "list-is-array" "vector-is-array"
           "list-of-lists-is-not-an-object" "nested-table-sorted" "string-escapes"
           "empty-containers")
     ('() "{}")
     ('("b" 2 "a" 1 "c" 3) "{\"a\":1,\"b\":2,\"c\":3}")
     ('("t" t "f" nil "n" :null "s" "x" "i" -12) "{\"f\":false,\"i\":-12,\"n\":null,\"s\":\"x\",\"t\":true}")
     ('("l" (1 "two" t)) "{\"l\":[1,\"two\",true]}")
     ((list "v" (vector 1 2)) "{\"v\":[1,2]}")
     ('("pairs" (("a" 1) ("b" 2))) "{\"pairs\":[[\"a\",1],[\"b\",2]]}")
     ((list "h" (table "z" 1 "a" (table "y" (list) "b" (vector))))
      "{\"h\":{\"a\":{\"b\":[],\"y\":false},\"z\":1}}")
     ('("q" "say \"hi\"\\") "{\"q\":\"say \\\"hi\\\"\\\\\"}")
     ((list "e" (table) "w" (vector)) "{\"e\":{},\"w\":[]}"))
  (ok (equal expected (session-data-json (apply #'session-with kvs)))))

(deftest data-json-is-deterministic
  (let ((a (session-with "x" 1 "y" (table "p" 1 "q" 2)))
        (b (session-with "y" (table "q" 2 "p" 1) "x" 1)))
    (ok (equal (session-data-json a) (session-data-json b))
        "insertion order does not matter")))

(deftest record-text
  (let ((s (session-with "b" (list 1 2) "a" "x")))
    (ok (equal (concatenate 'string
                            "{\"v\":1,\"id\":\"IdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdId0\","
                            "\"created\":1700000000,\"seen\":1700000300,"
                            "\"data\":{\"a\":\"x\",\"b\":[1,2]}}")
               (encode-session-record s)))
    (ok (equal "{\"v\":1,\"id\":\"IdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdIdId0\",\"created\":1700000000,\"seen\":1700000300,\"data\":{}}"
               (encode-session-record (session-with))))))

(deftest record-round-trip
  (let* ((data-in (list "str" (format nil "h~cllo" (code-char 233))
                        "int" 42 "neg" -3 "big" (expt 2 60)
                        "dbl" 1.5d0 "sgl" 0.25f0
                        "t" t "f" nil "null" :null
                        "list" (list 1 (list 2 (list 3)) "x")
                        "vec" (vector "a" (vector "b"))
                        "obj" (table "k" (list (table "deep" t)))
                        "empty-list" (list) "empty-obj" (table)))
         (s (apply #'session-with data-in))
         (back (decode-session-record (encode-session-record s)))
         (d (session-data back)))
    (ok (typep back 'session))
    (ok (not (session-new-p back)) "a decoded session is not new")
    (ok (not (or (session-changed-p back) (session-rotated-p back)
                 (session-ended-p back) (session-committed-p back))))
    (ok (equal (session-id s) (session-id back)))
    (ok (= 1700000000 (session-created-at back)))
    (ok (= 1700000300 (session-last-seen back)))
    (ok (eq 'equal (hash-table-test d)))
    (ok (= 14 (hash-table-count d)))
    (ok (equal (gethash "str" d) (second data-in)) "strings")
    (ok (eql 42 (gethash "int" d)))
    (ok (eql -3 (gethash "neg" d)))
    (ok (eql (expt 2 60) (gethash "big" d)))
    (ok (eql 1.5d0 (gethash "dbl" d)) "a double comes back eql")
    (ok (eql 0.25d0 (gethash "sgl" d)) "a single-float comes back as a double")
    (ok (eq t (gethash "t" d)))
    (ok (equal '(nil t) (multiple-value-list (gethash "f" d))))
    (ok (eq :null (gethash "null" d)))
    (ok (equal '(1 (2 (3)) "x") (gethash "list" d)) "lists come back as lists")
    (ok (equal '("a" ("b")) (gethash "vec" d)) "vectors come back as lists")
    (ok (listp (gethash "vec" d)))
    (let ((obj (gethash "obj" d)))
      (ok (hash-table-p obj))
      (ok (eq 'equal (hash-table-test obj)))
      (ok (json-equal (table "k" (list (table "deep" t))) obj)))
    (ok (equal '(nil t) (multiple-value-list (gethash "empty-list" d)))
        "an empty array loads as NIL")
    (ok (json-equal (table) (gethash "empty-obj" d)))
    (ok (equal (session-data-json s) (session-data-json back))
        "same text after a round trip")))

(deftest decode-ignores-extra-members
  (let ((s (decode-session-record
            "{\"v\":1,\"extra\":[1,2],\"id\":\"abc\",\"seen\":5,\"created\":4,\"data\":{\"k\":\"v\"},\"later\":{}}")))
    (ok (equal "abc" (session-id s)) "the id is not validated by the decoder")
    (ok (= 4 (session-created-at s)))
    (ok (= 5 (session-last-seen s)))
    (ok (equal "v" (gethash "k" (session-data s))))))

(deftest-parametrize decode-refuses
    ((text)
     :ids ("not-a-string" "nil" "octets" "not-json" "truncated" "array" "string"
           "number" "null" "empty-object" "v-missing" "v-2" "v-string" "id-missing"
           "id-number" "created-missing" "created-string" "created-float"
           "seen-missing" "seen-null" "data-missing" "data-array" "data-string")
     (42)
     (nil)
     ((make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(123 125)))
     ("not json")
     ("{\"v\":1,\"id\":\"a\"")
     ("[1,2]")
     ("\"x\"")
     ("7")
     ("null")
     ("{}")
     ("{\"id\":\"a\",\"created\":1,\"seen\":1,\"data\":{}}")
     ("{\"v\":2,\"id\":\"a\",\"created\":1,\"seen\":1,\"data\":{}}")
     ("{\"v\":\"1\",\"id\":\"a\",\"created\":1,\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"created\":1,\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":5,\"created\":1,\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"created\":\"1\",\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1.5,\"seen\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1,\"seen\":null,\"data\":{}}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1,\"seen\":1}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1,\"seen\":1,\"data\":[]}")
     ("{\"v\":1,\"id\":\"a\",\"created\":1,\"seen\":1,\"data\":\"x\"}"))
  (let ((c (capture (session-store-error) (decode-session-record text))))
    (ok (typep c 'session-store-error))
    (ok (and c (eq :load (session-store-error-operation c))))
    (ok (and c (null (session-store-error-store c))))))

(deftest decode-error-cause-is-the-json-error
  (let ((c (capture (session-store-error) (decode-session-record "{oops"))))
    (ok (typep (and c (session-store-error-cause c)) 'stack-json:json-error))
    (let ((shape (capture (session-store-error) (decode-session-record "[]"))))
      (ok (and shape (null (session-store-error-cause shape)))
          "a record of the wrong shape has no cause"))))

;;; Values that went bad after they were stored (changed in place)

(deftest data-json-refuses-and-restarts
  (let ((s (session-with "a" 1 "b" (list 1 'oops) "c" 3)))
    (let ((c (capture (session-value-error) (session-data-json s))))
      (ok (typep c 'session-value-error))
      (ok (and c (equal "b" (session-value-error-key c))))
      (ok (and c (eq 'oops (session-value-error-value c)))))
    ;; USE-VALUE replaces the value under KEY in the data
    (let ((text (handler-bind ((session-value-error
                                 (lambda (c) (use-value (list 1 2) c))))
                  (session-data-json s))))
      (ok (equal "{\"a\":1,\"b\":[1,2],\"c\":3}" text))
      (ok (equal '(1 2) (gethash "b" (session-data s))))
      (ok (not (session-changed-p s)) "a restart here is not a data change")))
  (let ((s (session-with "a" 1 "b" #\x "c" 3)))
    (let ((text (handler-bind ((session-value-error #'skip-value))
                  (session-data-json s))))
      (ok (equal "{\"a\":1,\"c\":3}" text))
      (ok (null (nth-value 1 (gethash "b" (session-data s))))
          "SKIP-VALUE removed the key"))))

(deftest data-json-checks-keys-in-order
  (let* ((s (session-with "z" 'last "m" 'middle "a" 'first))
         (keys '())
         (text (handler-bind ((session-value-error
                                (lambda (c)
                                  (push (session-value-error-key c) keys)
                                  (skip-value c))))
                 (session-data-json s))))
    (ok (equal "{}" text))
    (ok (equal '("a" "m" "z") (reverse keys)) "top-level keys in STRING< order")))

(deftest data-json-replacement-is-checked-again
  (let* ((s (session-with "k" 'bad))
         (n 0)
         (text (handler-bind ((session-value-error
                                (lambda (c)
                                  (incf n)
                                  (use-value (if (= n 1) #\x "good") c))))
                 (session-data-json s))))
    (ok (= 2 n))
    (ok (equal "{\"k\":\"good\"}" text))))

(deftest data-json-non-string-top-level-key
  (let ((s (session-with "ok" 1)))
    (setf (gethash :sym (session-data s)) 2)
    (let ((c (capture (session-value-error) (session-data-json s))))
      (ok (typep c 'session-value-error))
      (ok (and c (eq :sym (session-value-error-key c))))
      (ok (and c (eq :sym (session-value-error-value c)))))
    (ok (equal "{\"ok\":1}" (handler-bind ((session-value-error #'skip-value))
                              (session-data-json s))))
    (ok (null (nth-value 1 (gethash :sym (session-data s)))))))

(deftest record-encoding-signals-like-data-json
  (let ((s (session-with "a" 'bad)))
    (ok (signals (encode-session-record s) 'session-value-error))
    (ok (search "\"data\":{}"
                (handler-bind ((session-value-error #'skip-value))
                  (encode-session-record s))))))
