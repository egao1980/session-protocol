;;;; S0 JSON: the value domain, the data's JSON text, the stored record.
;;;;
;;;; Session data is stored as JSON text written and read by json-protocol
;;;; (STACK-JSON:ENCODE / STACK-JSON:DECODE), never by the Lisp reader or
;;;; printer. session-protocol depends on json-protocol, not on a backend: the
;;;; application loads one (json-backend-jzon), as for every stack protocol.
;;;;
;;;; The value domain. A value is JSON data when it is, recursively:
;;;;   a string                          -> a JSON string
;;;;   an integer                        -> a number
;;;;   a finite float                    -> a number (it loads as a DOUBLE-FLOAT)
;;;;   T / NIL                           -> true / false
;;;;   :NULL                             -> null
;;;;   a proper list other than NIL, or a vector that is not a string
;;;;                                     -> an array, elements in order
;;;;   a hash-table whose keys are all strings (any test)
;;;;                                     -> an object
;;;; NIL is false; an empty array loads as NIL too (in Lisp they are one
;;;; object). Nothing else is JSON data: other symbols (keywords other than
;;;; :NULL included), characters, ratios, complexes, NaN and infinite floats,
;;;; structures, standard objects, functions, pathnames, dotted and circular
;;;; lists, hash-tables with a key that is not a string.
;;;; Depth: an array or an object is one level. A value may nest at most
;;;; +MAX-VALUE-DEPTH+ (64) levels; an array or object at level 65 is not
;;;; JSON data (it is the offending object), so a table that contains itself
;;;; is refused rather than walked for ever.
;;;; The offending object (SESSION-VALUE-ERROR-VALUE) is the innermost one:
;;;; the element, the non-string key, the level-65 array or object, the
;;;; dotted or circular list. Lists and vectors are walked in element order;
;;;; in a hash-table every key is checked before any value, then values in
;;;; STRING< order of their keys.
;;;;
;;;; Loading. Arrays load as fresh lists, objects as fresh EQUAL hash-tables,
;;;; null as :NULL, true and false as T and NIL, numbers as json-protocol
;;;; reads them (integers exact, floats as DOUBLE-FLOATs). So a value round
;;;; trips unchanged except that vectors come back as lists, single-floats as
;;;; double-floats, and any hash-table as an EQUAL one.
;;;;
;;;; (SESSION-DATA-JSON session) -> a string: the session's data as one JSON
;;;;   object, written by STACK-JSON:ENCODE from a tree in which every list
;;;;   and vector is a vector and every object an EQUAL hash-table, so the
;;;;   output is json-protocol's deterministic form: compact, members sorted
;;;;   by STRING< of their keys, nested objects too. Empty data -> "{}".
;;;;   Top-level keys are checked in STRING< order. A value outside the domain
;;;;   signals SESSION-VALUE-ERROR (KEY, the offending object) inside the
;;;;   restarts USE-VALUE (the replacement becomes the value under KEY in the
;;;;   session's data and is checked again) and SKIP-VALUE (KEY is removed
;;;;   from the session's data). A top-level key that is not a string is
;;;;   reported the same way, with KEY and VALUE that key; SKIP-VALUE removes
;;;;   it. A restart taken here changes SESSION-DATA but not
;;;;   SESSION-CHANGED-P.
;;;;
;;;; (ENCODE-SESSION-RECORD session) -> a string, the stored form, exactly
;;;;     {"v":1,"id":<id>,"created":<created-at>,"seen":<last-seen>,"data":<data>}
;;;;   members in that order, compact, <id> a JSON string, <created-at> and
;;;;   <last-seen> integers, <data> the text of (SESSION-DATA-JSON session)
;;;;   (with its conditions and restarts).
;;;;
;;;; (DECODE-SESSION-RECORD text) -> a SESSION (MAKE-SESSION, NEW-P NIL).
;;;;   TEXT must be a string holding a JSON object whose member "v" is 1, "id"
;;;;   a string, "created" and "seen" integers and "data" an object; other
;;;;   members are ignored (a later version may add some). Values load as
;;;;   above. Anything else signals SESSION-STORE-ERROR with OPERATION :LOAD,
;;;;   STORE NIL and CAUSE the json-protocol condition when TEXT is not JSON
;;;;   (else NIL): a non-string, text that is not JSON, JSON that is not an
;;;;   object, a missing or mistyped member, "v" other than 1. The id is not
;;;;   validated here: BEGIN-SESSION compares it with the requested one.

(in-package #:session-protocol)

(defconstant +max-value-depth+ 64
  "Deepest nesting of arrays and objects a session value may have.")

;;; The value domain, in one walker. (SETF SESSION-VALUE) runs it check-only;
;;; SESSION-DATA-JSON and the record run it to build the tree they encode, so
;;; a value the SETF accepts is a value the encoding accepts.

(defun %finite-float-p (x)
  "True when the float X is neither a NaN nor an infinity. Never traps."
  #+sbcl (not (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x)))
  #-sbcl (handler-case (and (= x x) (<= (abs x) most-positive-long-float))
           (arithmetic-error () nil)))

(defun %proper-list-p (list)
  "True when the cons LIST is a proper list; NIL when it is dotted or
circular. A tortoise and hare walk: it never signals and always ends."
  (do ((fast list (cddr fast))
       (slow list (cdr slow))
       (start t nil))
      (nil)
    (cond ((null fast) (return t))
          ((atom fast) (return nil))
          ((null (cdr fast)) (return t))
          ((atom (cdr fast)) (return nil))
          ((and (not start) (eq fast slow)) (return nil)))))

(defun %json-tree (value &key (build t))
  "Walk VALUE against the value domain (file header).
JSON data -> (values TREE NIL NIL NIL): TREE, when BUILD is true, is what
STACK-JSON:ENCODE gets (every list and vector a fresh simple vector, every
object a fresh EQUAL hash-table, scalars as they are); else NIL.
Not JSON data -> (values NIL T OFFENDER REASON): OFFENDER the innermost
offending object (it may be NIL: a NIL hash-table key), REASON a fixed phrase."
  (block walk
    (labels ((refuse (object reason)
               (return-from walk (values nil t object reason)))
             (enter (container level)
               (when (> level +max-value-depth+)
                 (refuse container (format nil "nested deeper than ~d levels"
                                           +max-value-depth+))))
             (elements (items level)
               (if build
                   (map 'simple-vector (lambda (x) (node x (1+ level))) items)
                   (map nil (lambda (x) (node x (1+ level))) items)))
             (object (table level)
               ;; Every key before any value, then the values in STRING<
               ;; order of their keys.
               (let ((keys '()))
                 (maphash (lambda (key x)
                            (declare (ignore x))
                            (if (stringp key)
                                (push key keys)
                                (refuse key "a hash-table key that is not a string")))
                          table)
                 (setf keys (sort keys #'string<))
                 ;; Only an EQ or EQL table can hold two STRING= keys: they
                 ;; would name one JSON member.
                 (loop for (a b) on keys
                       when (and b (string= a b))
                         do (refuse b "two hash-table keys name the same member"))
                 (let ((out (and build (make-hash-table :test 'equal))))
                   (dolist (key keys out)
                     (let ((tree (node (gethash key table) (1+ level))))
                       (when build
                         (setf (gethash key out) tree)))))))
             (node (v level)
               ;; NIL before CONS, a string before VECTOR.
               (typecase v
                 ((or null (eql t) (eql :null) string integer) v)
                 (float (if (%finite-float-p v)
                            v
                            (refuse v "a float that is not finite")))
                 (cons (enter v level)
                       (unless (%proper-list-p v)
                         (refuse v "a dotted or circular list"))
                       (elements v level))
                 (vector (enter v level)
                         (elements v level))
                 (hash-table (enter v level)
                             (object v level))
                 (t (refuse v "no JSON form")))))
      (let ((tree (node value 1)))
        (values (and build tree) nil nil nil)))))

(defun %data-tree (session)
  "The encode tree of SESSION's data: an EQUAL hash-table of the top-level
keys. A value outside the domain signals SESSION-VALUE-ERROR inside USE-VALUE
and SKIP-VALUE, which change SESSION-DATA (not SESSION-CHANGED-P). Shared by
SESSION-DATA-JSON and ENCODE-SESSION-RECORD."
  (let ((data (session-data session))
        (tree (make-hash-table :test 'equal))
        (strings '())
        (others '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (if (stringp key) (push key strings) (push key others)))
             data)
    ;; A key that is not a string first. Only SKIP-VALUE: no replacement
    ;; value can fix a key, so a USE-VALUE here could loop for ever.
    (dolist (key others)
      (restart-case (error 'session-value-error :key key :value key
                                                :message "the key is not a string")
        (skip-value ()
          :report "Remove this key from the session data."
          (remhash key data))))
    (dolist (key (sort strings #'string<) tree)
      (loop
        (multiple-value-bind (subtree offending-p offender reason)
            (%json-tree (gethash key data))
          (unless offending-p
            (setf (gethash key tree) subtree)
            (return))
          (restart-case (error 'session-value-error :key key :value offender
                                                    :message reason)
            (use-value (replacement)
              :report "Use another value under this key (it is checked again)."
              :interactive %read-replacement
              (setf (gethash key data) replacement))
            (skip-value ()
              :report "Remove this key from the session data."
              (remhash key data)
              (return))))))))

(defun session-data-json (session)
  "SESSION's data as deterministic JSON text. Signals SESSION-VALUE-ERROR
(USE-VALUE, SKIP-VALUE). See the file header."
  (stack-json:encode (%data-tree session)))

(defun encode-session-record (session)
  "The stored JSON record of SESSION:
{\"v\":1,\"id\":...,\"created\":...,\"seen\":...,\"data\":{...}}. See the file header."
  ;; An alist: json-protocol writes its members in order.
  (stack-json:encode (list (cons "v" 1)
                           (cons "id" (session-id session))
                           (cons "created" (session-created-at session))
                           (cons "seen" (session-last-seen session))
                           (cons "data" (%data-tree session)))))

(defun %refuse-record (cause)
  "Signal the SESSION-STORE-ERROR of a record DECODE-SESSION-RECORD refuses.
The message is a fixed phrase: the record text holds the id, and so may the
report of CAUSE."
  (error 'session-store-error
         :operation :load :store nil :cause cause
         :message (if cause
                      "the record could not be decoded as JSON"
                      "the record is not a version 1 session record")))

(defun %loaded-value (value)
  "VALUE as json-protocol decoded it, with arrays as fresh lists (an empty
one is NIL) and objects as fresh EQUAL hash-tables, recursively."
  (typecase value
    (string value)
    (vector (map 'list #'%loaded-value value))
    (hash-table (let ((table (make-hash-table :test 'equal)))
                  (maphash (lambda (key x)
                             (setf (gethash key table) (%loaded-value x)))
                           value)
                  table))
    (t value)))

(defun decode-session-record (text)
  "The SESSION stored as TEXT. Signals SESSION-STORE-ERROR (:LOAD) for anything
that is not a version 1 record. See the file header."
  ;; A string first: the JSON backend would read octets, and signal a bare
  ;; TYPE-ERROR on anything else.
  (unless (stringp text)
    (%refuse-record nil))
  ;; Explicit limits, so an application's caps on untrusted JSON (the
  ;; json-protocol specials) never apply to our own records. The record is
  ;; level 1, its data level 2, so a value's level L is level L + 2: 66
  ;; admits exactly the +MAX-VALUE-DEPTH+ levels the domain allows.
  (let ((record (handler-case
                    (stack-json:decode text :max-depth (+ 2 +max-value-depth+)
                                            :max-string-length nil)
                  (stack-json:json-error (e) (%refuse-record e)))))
    (unless (hash-table-p record)
      (%refuse-record nil))
    (flet ((field (name type)
             (multiple-value-bind (value present-p) (gethash name record)
               (if (and present-p (typep value type))
                   value
                   (%refuse-record nil)))))
      (field "v" '(eql 1))
      (let ((id (field "id" 'string))
            (created (field "created" 'integer))
            (seen (field "seen" 'integer))
            (data (%loaded-value (field "data" 'hash-table))))
        ;; Whatever the JSON backend: a session handed out is in the domain
        ;; (a backend may read 1e999 as an infinity).
        (when (loop for value being the hash-values of data
                    thereis (nth-value 1 (%json-tree value :build nil)))
          (%refuse-record nil))
        (make-session :id id :data data :created-at created :last-seen seen)))))
