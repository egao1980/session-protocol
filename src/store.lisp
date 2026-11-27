;;;; S0 store protocol, and CACHE-SESSION-STORE over cache-protocol.
;;;;
;;;; A SESSION-STORE keeps sessions by id. Generic functions (every store):
;;;;   (SESSION-STORE-LOAD store id) -> a SESSION, or NIL
;;;;     The session stored under ID, as a fresh SESSION with NEW-P NIL (two
;;;;     loads never share an object), or NIL when there is none: never
;;;;     stored, deleted, or its TTL elapsed.
;;;;   (SESSION-STORE-SAVE store session ttl) -> SESSION
;;;;     Store SESSION (id, created-at, last-seen, data) under its current
;;;;     SESSION-ID, replacing any record there, gone TTL seconds from now (an
;;;;     integer >= 1). The value-domain conditions of ENCODE-SESSION-RECORD
;;;;     pass through with their restarts: they are not store failures.
;;;;   (SESSION-STORE-DELETE store id) -> T when a live record was there, else NIL
;;;; A store gets only ids that pass VALID-SESSION-ID-P (the manager checks
;;;; first); it need not check them again. Each call is one atomic operation
;;;; of the underlying store. Nothing locks a session across a request: when
;;;; two requests of one visitor both save, the later save wins (last write
;;;; wins), as with Lack, Rack and express-session stores.
;;;; A failure of the underlying store signals SESSION-STORE-ERROR with
;;;; OPERATION :LOAD, :SAVE or :DELETE. Restarts are the manager's
;;;; (manager.lisp), not the store's.
;;;; Later stores (a SQL table) implement the same three functions; they may
;;;; use ENCODE-SESSION-RECORD / DECODE-SESSION-RECORD for the record.
;;;;
;;;; CACHE-SESSION-STORE (given: the class, the constructor, the readers):
;;;; (MAKE-CACHE-SESSION-STORE cache &key (prefix "session:"))
;;;;   CACHE a STACK-CACHE:CACHE-BACKEND (STACK-CACHE:IN-MEMORY-TTL-CACHE,
;;;;   cache-backend-redis's REDIS-CACHE-BACKEND, ...), PREFIX a string
;;;;   (possibly empty); else TYPE-ERROR. Readers CACHE-SESSION-STORE-CACHE,
;;;;   CACHE-SESSION-STORE-PREFIX.
;;;; The cache key of ID is PREFIX followed by ID. The cached value is the
;;;; JSON text of ENCODE-SESSION-RECORD, a string, so it is the same on every
;;;; cache backend. Methods:
;;;;   load    (STACK-CACHE:CACHE-GET cache key): NIL -> NIL; a string ->
;;;;           (DECODE-SESSION-RECORD it); any other value -> SESSION-STORE-ERROR
;;;;           (:LOAD, CAUSE NIL). A record DECODE-SESSION-RECORD refuses ->
;;;;           SESSION-STORE-ERROR :LOAD with STORE this store and that
;;;;           condition's CAUSE and MESSAGE.
;;;;   save    (STACK-CACHE:CACHE-PUT cache key (ENCODE-SESSION-RECORD session)
;;;;           :ttl ttl); the encoding runs before the cache is called. Sliding
;;;;           idle expiry is this TTL, put again on every save.
;;;;   delete  (STACK-CACHE:CACHE-DELETE cache key), its result as a boolean.
;;;; A STACK-CACHE:CACHE-ERROR (CACHE-CONNECTION-ERROR included) from the cache
;;;; becomes SESSION-STORE-ERROR with that OPERATION, STORE this store and
;;;; CAUSE the cache error, signalled from a HANDLER-BIND handler while the
;;;; cache's frames are live, so the cache's own restarts (cache-backend-
;;;; redis's RECONNECT) stay available to the caller's handlers. Other
;;;; conditions pass through unchanged.

(in-package #:session-protocol)

(defclass session-store () ()
  (:documentation "Base class of session stores. See store.lisp."))

(defgeneric session-store-load (store id)
  (:documentation "The SESSION stored under ID (fresh, NEW-P NIL), or NIL.
Signals SESSION-STORE-ERROR (:LOAD)."))

(defgeneric session-store-save (store session ttl)
  (:documentation "Store SESSION under its id for TTL seconds (an integer >= 1),
replacing any record. Returns SESSION. Signals SESSION-STORE-ERROR (:SAVE)."))

(defgeneric session-store-delete (store id)
  (:documentation "Delete the record under ID; T when one was there. Signals
SESSION-STORE-ERROR (:DELETE)."))

(defclass cache-session-store (session-store)
  ((cache :initarg :cache :reader cache-session-store-cache)
   (prefix :initarg :prefix :reader cache-session-store-prefix))
  (:documentation "Sessions as JSON text in a cache-protocol cache, under
PREFIX followed by the id, with the idle TTL."))

(defun make-cache-session-store (cache &key (prefix "session:"))
  "A CACHE-SESSION-STORE over CACHE (a STACK-CACHE:CACHE-BACKEND); keys are
PREFIX followed by the session id."
  (check-type cache stack-cache:cache-backend)
  (check-type prefix string)
  (make-instance 'cache-session-store :cache cache :prefix prefix))

(defmethod session-store-load ((store cache-session-store) id)
  (declare (ignorable id))
  (%s0-pending 'session-store-load))

(defmethod session-store-save ((store cache-session-store) session ttl)
  (declare (ignorable session ttl))
  (%s0-pending 'session-store-save))

(defmethod session-store-delete ((store cache-session-store) id)
  (declare (ignorable id))
  (%s0-pending 'session-store-delete))
