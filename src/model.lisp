;;;; S0 session model.
;;;;
;;;; A SESSION (given: the class, MAKE-SESSION, the readers, PRINT-OBJECT):
;;;;   SESSION-ID          the id, a string (the current one: REGENERATE-SESSION-ID
;;;;                       changes it)
;;;;   SESSION-DATA        the live EQUAL hash-table of string keys -> values.
;;;;                       Changing it directly is allowed; the commit notices
;;;;                       (manager.lisp, What a commit writes), but no value
;;;;                       check runs until then.
;;;;   SESSION-CREATED-AT  Unix seconds, an integer: when the session was first
;;;;                       started. Kept by rotation; never changed by a save.
;;;;   SESSION-LAST-SEEN   Unix seconds, an integer: when the store last wrote
;;;;                       the session (a new session: the time it was started).
;;;;                       FINISH-SESSION sets it to the commit time when it
;;;;                       saves.
;;;; Flags, each about the current request:
;;;;   SESSION-NEW-P        started by this request (no valid cookie, an unknown
;;;;                        or expired id), not loaded from the store
;;;;   SESSION-CHANGED-P    (SETF SESSION-VALUE) ran, or REMOVE-SESSION-VALUE or
;;;;                        CLEAR-SESSION removed at least one key
;;;;   SESSION-ROTATED-P    REGENERATE-SESSION-ID ran
;;;;   SESSION-ENDED-P      END-SESSION ran
;;;;   SESSION-COMMITTED-P  FINISH-SESSION's commit for this session completed
;;;;
;;;; (MAKE-SESSION &key id data created-at last-seen new-p) -> a SESSION.
;;;;   For stores (decoding a record) and for BEGIN-SESSION. ID a string;
;;;;   DATA an EQUAL hash-table (default: a fresh empty one), used as is, not
;;;;   copied; CREATED-AT and LAST-SEEN integers; NEW-P a generalized boolean
;;;;   (default NIL: a session loaded from a store). Wrong types: TYPE-ERROR.
;;;;   Every other flag starts NIL.
;;;; PRINT-OBJECT: #<SESSION [new ][rotated ][ended ][committed ]N keys>, never
;;;;   the id.
;;;;
;;;; Values: the JSON value domain of json.lisp. Keys: strings, compared with
;;;; EQUAL (case-sensitive); any other key is a TYPE-ERROR (CHECK-TYPE).
;;;;
;;;; (SESSION-VALUE session key &optional default) -> (values value present-p)
;;;;   The value stored under KEY, T; or DEFAULT, NIL. Reading never signals
;;;;   SESSION-WRITE-ERROR (after END-SESSION the data is empty).
;;;; ((SETF SESSION-VALUE) value session key &optional default) -> value
;;;;   DEFAULT is ignored (so (INCF (SESSION-VALUE s "n" 0)) works). In order:
;;;;   1. session ended or committed -> SESSION-WRITE-ERROR (CERROR; CONTINUE
;;;;      changes nothing and returns NIL);
;;;;   2. KEY not a string -> TYPE-ERROR;
;;;;   3. VALUE outside the value domain -> SESSION-VALUE-ERROR (KEY, the
;;;;      offending object) with USE-VALUE (store the replacement, checked
;;;;      again; the SETF returns it) and SKIP-VALUE (store nothing; NIL);
;;;;   4. store VALUE itself (not a copy) under KEY; SESSION-CHANGED-P becomes
;;;;      T, even when the old value was the same; return VALUE.
;;;; (REMOVE-SESSION-VALUE session key) -> T when KEY was present, else NIL.
;;;;   Steps 1 and 2 as above. Marks the session changed only when it removed
;;;;   something.
;;;; (CLEAR-SESSION session) -> SESSION. Step 1 as above. Removes every key;
;;;;   marks the session changed only when there was a key.
;;;; (REGENERATE-SESSION-ID session) -> the new id.
;;;;   Step 1 as above. Sets the id to a fresh (GENERATE-SESSION-ID) and
;;;;   SESSION-ROTATED-P to T; data, CREATED-AT and LAST-SEEN are kept. The
;;;;   store is not touched until the commit, which deletes the id the session
;;;;   was loaded under (never an intermediate id: rotating twice in one
;;;;   request deletes one old id) and saves under the new one. Call it when
;;;;   the visitor's privileges change (a login on top of the session), so an
;;;;   id planted before the change is worthless after it (session fixation).
;;;; (END-SESSION session) -> SESSION.
;;;;   After the commit -> SESSION-WRITE-ERROR (as step 1). On an ended
;;;;   session: nothing (idempotent). Else: every key removed and
;;;;   SESSION-ENDED-P T. The commit deletes the stored session (when it was
;;;;   loaded) and expires the cookie. Later writes in the same request signal
;;;;   SESSION-WRITE-ERROR.

(in-package #:session-protocol)

(defclass session ()
  ((id :initarg :id :reader session-id)
   (data :initarg :data :reader session-data)
   (created-at :initarg :created-at :reader session-created-at)
   (last-seen :initarg :last-seen :reader session-last-seen)
   (new-p :initarg :new-p :initform nil :reader session-new-p)
   (changed :initform nil :reader session-changed-p)
   (rotated :initform nil :reader session-rotated-p)
   (ended :initform nil :reader session-ended-p)
   (committed :initform nil :reader session-committed-p)
   ;; The id the session was loaded under (NIL for a new session): the one a
   ;; rotation or an end deletes.
   (loaded-id :initarg :loaded-id :initform nil)
   ;; SESSION-DATA-JSON of the data as loaded ("{}" for a new session), set
   ;; by BEGIN-SESSION: the commit compares against it.
   (baseline :initform nil))
  (:documentation "A visitor's server-side session: id, data (string keys ->
JSON values), creation and last-seen times, per-request flags. See model.lisp."))

(defun %equal-hash-table-p (object)
  "True for a hash-table whose test is EQUAL (CLHS: always the symbol for the
four standard tests)."
  (and (hash-table-p object)
       (eq (hash-table-test object) 'equal)))

(defun make-session (&key id (data (make-hash-table :test 'equal))
                          created-at last-seen new-p)
  "A SESSION. ID a string, DATA an EQUAL hash-table (used as is), CREATED-AT
and LAST-SEEN integers (Unix seconds), NEW-P true for a session started by
this request. See the file header."
  (check-type id string)
  (check-type data (and hash-table (satisfies %equal-hash-table-p)))
  (check-type created-at integer)
  (check-type last-seen integer)
  (make-instance 'session :id id :data data
                          :created-at created-at :last-seen last-seen
                          :new-p (and new-p t)
                          :loaded-id (if new-p nil id)))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~:[~;new ~]~:[~;rotated ~]~:[~;ended ~]~:[~;committed ~]~d key~:p"
            (session-new-p session) (session-rotated-p session)
            (session-ended-p session) (session-committed-p session)
            (hash-table-count (session-data session)))))

(defun session-value (session key &optional default)
  "(values value present-p) for KEY (a string) in SESSION's data, or
(values DEFAULT NIL). See the file header."
  (declare (ignorable session key default))
  (%s0-pending 'session-value))

(defun (setf session-value) (value session key &optional default)
  "Store VALUE (JSON data) under KEY. Signals SESSION-WRITE-ERROR (CONTINUE),
TYPE-ERROR, SESSION-VALUE-ERROR (USE-VALUE, SKIP-VALUE). See the file header."
  (declare (ignorable value session key default))
  (%s0-pending '(setf session-value)))

(defun remove-session-value (session key)
  "Remove KEY; T when it was present. See the file header."
  (declare (ignorable session key))
  (%s0-pending 'remove-session-value))

(defun clear-session (session)
  "Remove every key. Returns SESSION. See the file header."
  (declare (ignorable session))
  (%s0-pending 'clear-session))

(defun regenerate-session-id (session)
  "Give SESSION a fresh id, keeping its data; the commit deletes the old one.
Returns the new id. See the file header."
  (declare (ignorable session))
  (%s0-pending 'regenerate-session-id))

(defun end-session (session)
  "End SESSION: data removed; the commit deletes it and expires the cookie.
Returns SESSION. See the file header."
  (declare (ignorable session))
  (%s0-pending 'end-session))
