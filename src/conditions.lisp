;;;; S0 conditions (given). CLHS 9: one root, slots with readers, a :REPORT
;;;; for each, restarts established where the recovery is possible.
;;;;
;;;; session-error (error)              message
;;;; ├── session-value-error            key, value
;;;; │     A value with no JSON form (json.lisp, The value domain). Signalled
;;;; │     by (SETF SESSION-VALUE) inside the restarts
;;;; │       USE-VALUE v   store V instead (V is checked again);
;;;; │       SKIP-VALUE    store nothing: the key keeps its previous value
;;;; │                     (or stays absent); the SETF returns NIL;
;;;; │     and by SESSION-DATA-JSON, when a value changed in place became
;;;; │     invalid after it was stored, inside the restarts
;;;; │       USE-VALUE v   replace the value under KEY in the session's data
;;;; │                     with V (checked again);
;;;; │       SKIP-VALUE    remove KEY from the session's data.
;;;; │     KEY is the top-level data key; VALUE the offending object itself
;;;; │     (the innermost one: a symbol inside a list inside a table, a
;;;; │     non-string hash-table key, the first array or object past
;;;; │     +MAX-VALUE-DEPTH+, a circular or dotted list).
;;;; ├── session-store-error            store, operation, cause
;;;; │     A store operation failed. OPERATION is :LOAD, :SAVE or :DELETE;
;;;; │     CAUSE the underlying condition (a STACK-CACHE:CACHE-ERROR, a
;;;; │     json-protocol error) or NIL (a record of the wrong shape). Signalled
;;;; │     by store methods; BEGIN-SESSION and FINISH-SESSION establish, around
;;;; │     each store call,
;;;; │       RETRY         run the store operation again;
;;;; │       CONTINUE      go on without it (manager.lisp says what that
;;;; │                     means for each call).
;;;; └── session-write-error            session
;;;;       A change to a session that is ended or already committed:
;;;;       (SETF SESSION-VALUE), REMOVE-SESSION-VALUE, CLEAR-SESSION,
;;;;       REGENERATE-SESSION-ID, END-SESSION after the commit. Signalled with
;;;;       CERROR: CONTINUE ignores the change (the function returns NIL).
;;;;
;;;; Reports never contain a session id (package.lisp, Secrets). The report
;;;; of SESSION-STORE-ERROR names the operation and the message, not the
;;;; cause's report (a cache error's report prints its key, which holds the
;;;; id); the cause stays available through its reader. The report of
;;;; SESSION-VALUE-ERROR names the key and the type of the value, not the
;;;; value.
;;;;
;;;; Restart functions, CLHS style (as USE-VALUE and CONTINUE): invoke the
;;;; restart of that name associated with CONDITION (or the most recent one)
;;;; when there is one, else return NIL.
;;;;   (SKIP-VALUE &optional condition)
;;;;   (RETRY &optional condition)
;;;; Both restart names are symbols of this package. From a package that
;;;; uses ROVE, call these functions (or STACK-SESSION:RETRY): an unqualified
;;;; 'SKIP there may name ROVE:SKIP.

(in-package #:session-protocol)

(define-condition session-error (error)
  ((message :initarg :message :initform nil :reader session-error-message))
  (:report (lambda (c s)
             (format s "session error~@[: ~a~]" (session-error-message c))))
  (:documentation "Root of session-protocol's conditions."))

(define-condition session-value-error (session-error)
  ((key :initarg :key :initform nil :reader session-value-error-key)
   (value :initarg :value :initform nil :reader session-value-error-value))
  (:report (lambda (c s)
             (format s "session value under key ~s is not JSON data (a ~s)~@[: ~a~]"
                     (session-value-error-key c)
                     (type-of (session-value-error-value c))
                     (session-error-message c))))
  (:documentation "A session value with no JSON form. Restarts USE-VALUE and
SKIP-VALUE (see the file header)."))

(define-condition session-store-error (session-error)
  ((store :initarg :store :initform nil :reader session-store-error-store)
   (operation :initarg :operation :initform nil
              :reader session-store-error-operation)
   (cause :initarg :cause :initform nil :reader session-store-error-cause))
  (:report (lambda (c s)
             (format s "session store ~(~a~) failed~@[: ~a~]"
                     (or (session-store-error-operation c) "operation")
                     (session-error-message c))))
  (:documentation "A session store operation (:LOAD, :SAVE, :DELETE) failed.
CAUSE is the underlying condition or NIL. BEGIN-SESSION and FINISH-SESSION
offer RETRY and CONTINUE."))

(define-condition session-write-error (session-error)
  ((session :initarg :session :initform nil :reader session-write-error-session))
  (:report (lambda (c s)
             (let ((session (session-write-error-session c)))
               (format s "session cannot change: it is ~a~@[ (~a)~]"
                       (if (and session (slot-boundp session 'ended)
                                (slot-value session 'ended))
                           "ended"
                           "committed")
                       (session-error-message c)))))
  (:documentation "A change to an ended or committed session. Signalled with
CERROR; CONTINUE ignores the change."))

(defun skip-value (&optional condition)
  "Invoke the SKIP-VALUE restart for CONDITION, or return NIL when there is none."
  (let ((restart (find-restart 'skip-value condition)))
    (when restart
      (invoke-restart restart))))

(defun retry (&optional condition)
  "Invoke the RETRY restart for CONDITION, or return NIL when there is none."
  (let ((restart (find-restart 'retry condition)))
    (when restart
      (invoke-restart restart))))
