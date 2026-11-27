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

(defun session-data-json (session)
  "SESSION's data as deterministic JSON text. Signals SESSION-VALUE-ERROR
(USE-VALUE, SKIP-VALUE). See the file header."
  (declare (ignorable session))
  (%s0-pending 'session-data-json))

(defun encode-session-record (session)
  "The stored JSON record of SESSION:
{\"v\":1,\"id\":...,\"created\":...,\"seen\":...,\"data\":{...}}. See the file header."
  (declare (ignorable session))
  (%s0-pending 'encode-session-record))

(defun decode-session-record (text)
  "The SESSION stored as TEXT. Signals SESSION-STORE-ERROR (:LOAD) for anything
that is not a version 1 record. See the file header."
  (declare (ignorable text))
  (%s0-pending 'decode-session-record))
