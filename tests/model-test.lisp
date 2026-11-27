;;;; The session model: values, flags, rotation, ending, write errors, the
;;;; value domain at SETF time.

(in-package #:session-protocol/tests)

(defun blank-session ()
  (make-session :id (make-string 43 :initial-element #\A)
                :created-at +t0+ :last-seen +t0+))

(deftest session-value-reads
  (let ((s (blank-session)))
    (ok (equal '(nil nil) (multiple-value-list (session-value s "missing"))))
    (ok (equal '(:dflt nil) (multiple-value-list (session-value s "missing" :dflt))))
    (setf (gethash "k" (session-data s)) "v")
    (ok (equal '("v" t) (multiple-value-list (session-value s "k" :dflt))))
    (setf (gethash "f" (session-data s)) nil)
    (ok (equal '(nil t) (multiple-value-list (session-value s "f" :dflt)))
        "a stored false is present")
    (ok (equal '(nil nil) (multiple-value-list (session-value s "K")))
        "keys are case-sensitive")))

(deftest setf-session-value
  (let ((s (blank-session)))
    (ok (not (session-changed-p s)))
    (ok (equal "v" (setf (session-value s "k") "v")) "SETF returns the value")
    (ok (session-changed-p s))
    (ok (equal "v" (gethash "k" (session-data s))))
    (setf (session-value s "k") "w")
    (ok (equal "w" (session-value s "k")) "a second SETF replaces")
    (let ((list (list 1 2 3)))
      (setf (session-value s "l") list)
      (ok (eq list (session-value s "l")) "the value itself is stored, not a copy"))
    (incf (session-value s "n" 0))
    (incf (session-value s "n" 0))
    (ok (eql 2 (session-value s "n")) "INCF with a default")))

(deftest setf-same-value-still-marks-changed
  (let ((s (blank-session)))
    (setf (gethash "k" (session-data s)) 1)
    (setf (session-value s "k") 1)
    (ok (session-changed-p s))))

(deftest-parametrize keys-must-be-strings
    ((key) :ids ("keyword" "symbol" "number" "nil" "character")
     (:k) ('k) (1) (nil) (#\k))
  (let ((s (blank-session)))
    (ok (signals (session-value s key) 'type-error))
    (ok (signals (setf (session-value s key) 1) 'type-error))
    (ok (signals (remove-session-value s key) 'type-error))
    (ok (zerop (hash-table-count (session-data s))))))

(deftest remove-session-value-and-changed
  (let ((s (blank-session)))
    (setf (gethash "a" (session-data s)) 1)
    (ok (null (remove-session-value s "zzz")))
    (ok (not (session-changed-p s)) "removing nothing changes nothing")
    (ok (eq t (remove-session-value s "a")))
    (ok (session-changed-p s))
    (ok (null (nth-value 1 (session-value s "a"))))))

(deftest clear-session-and-changed
  (let ((s (blank-session)))
    (ok (eq s (clear-session s)))
    (ok (not (session-changed-p s)) "clearing empty data changes nothing")
    (setf (gethash "a" (session-data s)) 1
          (gethash "b" (session-data s)) 2)
    (ok (eq s (clear-session s)))
    (ok (session-changed-p s))
    (ok (zerop (hash-table-count (session-data s))))))

;;; The value domain at SETF time

(defparameter *json-values*
  (list "" "text" (string (code-char 128512)) 0 -7 (expt 2 62) 1.5d0 -0.25d0 1.5f0
        t nil :null
        (list 1 "two" t nil :null)
        (vector 1 2 3)
        (make-array 2 :initial-contents '("a" "b") :adjustable t :fill-pointer 2)
        (list (list 1 2) (list (list "deep")))
        (table "a" 1 "b" (list 1 2) "c" (table "d" (vector nil t)))
        (let ((h (make-hash-table :test 'eql))) h)
        (let ((h (make-hash-table :test 'equalp))) (setf (gethash "X" h) 1) h)
        (list (table) (vector) "")))

(deftest-parametrize json-values-are-accepted
    ((value) :rows (mapcar #'list *json-values*))
  (let ((s (blank-session)))
    (ok (eq value (setf (session-value s "k") value)))
    (ok (eq value (session-value s "k")))))

(defstruct opaque a)

(defparameter *non-json-values*
  ;; (value offending-object-finder description)
  (list (list 'sym #'identity "a symbol")
        (list :keyword #'identity "a keyword other than :null")
        (list #\c #'identity "a character")
        (list 1/3 #'identity "a ratio")
        (list #c(1 2) #'identity "a complex")
        (list (make-opaque :a 1) #'identity "a structure")
        (list #'car #'identity "a function")
        (list #p"/tmp/x" #'identity "a pathname")
        (list (cons 1 2) #'identity "a dotted pair")
        (list (list* 1 2 3) #'identity "a dotted list")
        (list (list 1 'sym 3) #'second "a symbol in a list")
        (list (vector 1 (list 2 #\c)) (lambda (v) (second (aref v 1))) "a character deep in a vector")
        (list (table "a" (table "b" 'x)) (lambda (h) (gethash "b" (gethash "a" h))) "a symbol in a nested table")
        (list (let ((h (make-hash-table :test 'equal))) (setf (gethash :k h) 1) h)
              (constantly :k) "a keyword key")
        (list (let ((h (make-hash-table :test 'eql))) (setf (gethash 1 h) 1) h)
              (constantly 1) "a number key")
        (list (list (make-opaque)) #'first "a structure in a list")))

(deftest-parametrize non-json-values-are-refused
    ((value find description) :rows *non-json-values*)
  (let* ((s (blank-session))
         (c (capture (session-value-error) (setf (session-value s "k") value))))
    (ok (typep c 'session-value-error) description)
    (ok (and c (equal "k" (session-value-error-key c))))
    (ok (and c (eql (funcall find value) (session-value-error-value c)))
        "VALUE is the offending object")
    (ok (null (nth-value 1 (session-value s "k"))) "nothing was stored")
    (ok (not (session-changed-p s)))))

#+sbcl
(defvar *infinity* sb-ext:double-float-positive-infinity
  "A special, so the compiler does not fold arithmetic on it.")

(deftest non-finite-floats-are-refused
  #+sbcl
  (let ((s (blank-session)))
    (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
      (let* ((inf *infinity*)
             (nan (- inf inf)))
        (ok (signals (setf (session-value s "k") inf) 'session-value-error))
        (ok (signals (setf (session-value s "k") (- inf)) 'session-value-error))
        (ok (signals (setf (session-value s "k") nan) 'session-value-error))
        (ok (signals (setf (session-value s "k") (list 1 inf)) 'session-value-error)))))
  #-sbcl (skip "float infinities need SBCL here"))

(deftest depth-limit
  (let ((s (blank-session)))
    (ok (= 64 +max-value-depth+))
    (let ((deep (nested-lists 64)))
      (ok (eq deep (setf (session-value s "ok") deep)) "64 levels are fine"))
    (let* ((deeper (nested-lists 65))
           (c (capture (session-value-error) (setf (session-value s "no") deeper))))
      (ok (typep c 'session-value-error) "65 levels are not")
      (ok (and c (eq (level deeper 65) (session-value-error-value c)))
          "the level-65 list is the offending object"))
    (let ((mixed (vector (table "a" (nested-lists 62)))))
      (ok (eq mixed (setf (session-value s "mixed") mixed))
          "vector, table and 62 lists: 64 levels"))))

(deftest cycles-are-refused
  (let ((s (blank-session)))
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "self" h) h)
      (ok (signals (setf (session-value s "h") h) 'session-value-error)
          "a table that contains itself"))
    (let ((l (list 1 2 3)))
      (setf (cdr (last l)) l)
      (let ((c (capture (session-value-error) (setf (session-value s "l") l))))
        (ok (typep c 'session-value-error) "a circular list")
        (ok (and c (eq l (session-value-error-value c))))))
    (ok (zerop (hash-table-count (session-data s))))))

(deftest setf-use-value-restart
  (let* ((s (blank-session))
         (result (handler-bind ((session-value-error
                                  (lambda (c) (use-value "fixed" c))))
                   (setf (session-value s "k") 'bad))))
    (ok (equal "fixed" result) "SETF returns the replacement")
    (ok (equal "fixed" (session-value s "k")))
    (ok (session-changed-p s))))

(deftest setf-use-value-is-checked-again
  (let* ((s (blank-session))
         (seen '())
         (result (handler-bind ((session-value-error
                                  (lambda (c)
                                    (push (session-value-error-value c) seen)
                                    (if (eq (session-value-error-value c) 'bad)
                                        (use-value 'still-bad c)
                                        (use-value 7 c)))))
                   (setf (session-value s "k") 'bad))))
    (ok (equal '(still-bad bad) seen) "the replacement was checked")
    (ok (eql 7 result))
    (ok (eql 7 (session-value s "k")))))

(deftest setf-skip-value-restart
  (let ((s (blank-session)))
    (setf (gethash "k" (session-data s)) "old")
    (let ((result (handler-bind ((session-value-error
                                   (lambda (c) (skip-value c))))
                    (setf (session-value s "k") 'bad))))
      (ok (null result) "SETF returns NIL")
      (ok (equal "old" (session-value s "k")) "the previous value stays")
      (ok (not (session-changed-p s))))
    (handler-bind ((session-value-error #'skip-value))
      (setf (session-value s "new") #\c))
    (ok (null (nth-value 1 (session-value s "new"))) "an absent key stays absent")))

;;; Rotation

(deftest regenerate-session-id-keeps-data
  (let* ((s (blank-session))
         (old (session-id s)))
    (setf (gethash "a" (session-data s)) 1)
    (let ((new (regenerate-session-id s)))
      (ok (valid-session-id-p new))
      (ok (string/= old new))
      (ok (equal new (session-id s)))
      (ok (session-rotated-p s))
      (ok (eql 1 (session-value s "a")) "data kept")
      (ok (= +t0+ (session-created-at s)) "created-at kept")
      (ok (= +t0+ (session-last-seen s)))
      (ok (not (session-changed-p s)) "rotation is not a data change"))
    (let ((third (regenerate-session-id s)))
      (ok (equal third (session-id s)) "rotating again gives another id"))))

;;; Ending

(deftest end-session-clears-and-is-idempotent
  (let ((s (blank-session)))
    (setf (gethash "a" (session-data s)) 1)
    (ok (eq s (end-session s)))
    (ok (session-ended-p s))
    (ok (zerop (hash-table-count (session-data s))))
    (ok (equal '(:d nil) (multiple-value-list (session-value s "a" :d)))
        "reads still work, on empty data")
    (ok (eq s (end-session s)) "ending again is a no-op")
    (ok (session-ended-p s))))

(defparameter *writes*
  (list (list "setf" (lambda (s) (setf (session-value s "k") 1)))
        (list "remove" (lambda (s) (remove-session-value s "k")))
        (list "clear" (lambda (s) (clear-session s)))
        (list "regenerate" (lambda (s) (regenerate-session-id s)))))

(deftest-parametrize writes-after-end-signal
    ((name write) :rows *writes*)
  (let ((s (blank-session)))
    (setf (gethash "k" (session-data s)) 0)
    (end-session s)
    (let ((id (session-id s)))
      (ok (signals (funcall write s) 'session-write-error) name)
      (let ((result (handler-bind ((session-write-error #'continue))
                      (funcall write s))))
        (ok (null result) "CONTINUE ignores the write and returns NIL")
        (ok (zerop (hash-table-count (session-data s))))
        (ok (equal id (session-id s)))
        (ok (not (session-rotated-p s)))))))

(deftest-parametrize writes-after-commit-signal
    ((name write)
     :rows (append *writes* (list (list "end" (lambda (s) (end-session s))))))
  (let ((s (blank-session)))
    (setf (gethash "k" (session-data s)) 0)
    (mark-committed s)
    (let ((id (session-id s)))
      (ok (signals (funcall write s) 'session-write-error) name)
      (let ((result (handler-bind ((session-write-error #'continue))
                      (funcall write s))))
        (ok (null result))
        (ok (eql 0 (session-value s "k")) "data unchanged")
        (ok (equal id (session-id s)))
        (ok (not (session-ended-p s)))
        (ok (not (session-changed-p s)))))))

(deftest write-error-before-type-error
  (let ((s (blank-session)))
    (end-session s)
    (ok (signals (setf (session-value s :not-a-string) 'bad) 'session-write-error)
        "an ended session is reported before the key and value checks")))
