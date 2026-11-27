;;;; session-protocol/middleware: sessions as a middleware-protocol mixin
;;;; (slice S0b). Brief: cl-stack/docs/capabilities/session.md, S0b.
;;;;
;;;; System session-protocol/middleware, package SESSION-PROTOCOL/MIDDLEWARE
;;;; (nickname STACK-SESSION-MIDDLEWARE). It depends on session-protocol,
;;;; middleware-protocol and middleware-protocol/clack; the core system
;;;; session-protocol does not depend on middleware-protocol. BEGIN-SESSION
;;;; and FINISH-SESSION stay the transport-neutral core (manager.lisp); this
;;;; file binds them to middleware-protocol's two hooks for Clack requests.
;;;;
;;;; SESSION-HANDLER (a mixin, subclass of STACK-MIDDLEWARE:HANDLER)   (given)
;;;;   Initarg :SESSION-MANAGER, accessor SESSION-HANDLER-MANAGER: the
;;;;   SESSION-MANAGER of every request this handler sees. Default: a fresh
;;;;   (MAKE-SESSION-MANAGER) per handler instance, process-local (its store
;;;;   is an in-memory cache): zero-configuration development. Production
;;;;   passes a manager over a shared store. Anything that is not a
;;;;   SESSION-MANAGER: TYPE-ERROR from MAKE-INSTANCE (and so from
;;;;   STACK-MIDDLEWARE:MAKE-HANDLER). The slot name is a symbol of this
;;;;   package and the initarg is named after the mixin (middleware.md, "Slot
;;;;   names"), so one MAKE-HANDLER call configures it next to the other
;;;;   mixins:
;;;;     (make-handler '(session-handler clack-handler)
;;;;                   :session-manager manager :app #'my-app)
;;;;   SESSION-HANDLER has no HANDLE method of its own (no :AROUND, no
;;;;   :BEFORE / :AFTER): it works through the two hooks below only.
;;;;
;;;; PREPARE-REQUEST on (SESSION-HANDLER CLACK-REQUEST) -> request
;;;;   1. (BEGIN-SESSION (SESSION-HANDLER-MANAGER handler)
;;;;                     (CLACK-REQUEST-ENV request))
;;;;      -> session, env*.
;;;;   2. Puts SESSION on the request: (SETF (REQUEST-PROPERTY request
;;;;      :SESSION) session).
;;;;   3. Installs env* as the request's env: (SETF (CLACK-REQUEST-ENV
;;;;      request) env*). env* is (LIST* :SESSION session env): the env the
;;;;      server gave is not modified (it is the tail of the new one).
;;;;   4. Returns (CALL-NEXT-METHOD): the request, as the PREPARE-REQUEST
;;;;      methods of inner mixins return it.
;;;;   So the session is begun before the PREPARE-REQUEST methods of the
;;;;   mixins after SESSION-HANDLER in the class precedence list (inner
;;;;   mixins) run, and they see it on the request and in its env. A mixin
;;;;   before it (outer) sees the session only after its own CALL-NEXT-METHOD
;;;;   returns. CLACK-HANDLER calls the app with (LIST* :MIDDLEWARE.REQUEST
;;;;   request env*), so inside the app
;;;;     (ENV-SESSION env)                          ; S0, the env key
;;;;     (REQUEST-SESSION (ENV-REQUEST env))        ; the request property
;;;;   are the same session (EQ), and every key of the server's env is still
;;;;   there.
;;;;   A failure of BEGIN-SESSION (a SESSION-STORE-ERROR with its RETRY and
;;;;   CONTINUE restarts) propagates unchanged, as middleware-protocol
;;;;   propagates every condition; an ERROR-HANDLER outside may map it.
;;;;
;;;; FINISH-RESPONSE on (SESSION-HANDLER CLACK-REQUEST T) -> response
;;;;   response* = (CALL-NEXT-METHOD): the response after the FINISH-RESPONSE
;;;;   methods of inner mixins. With session = (REQUEST-SESSION request):
;;;;   - a session -> (FINISH-SESSION manager session (CLACK-REQUEST-ENV
;;;;     request) response*): the commit, and the response with the session's
;;;;     Set-Cookie last among the app's headers (manager.lisp, step 7);
;;;;   - no session (a request an inner PREPARE-REQUEST replaced without
;;;;     carrying the :SESSION property over, or a direct call) ->
;;;;     response* unchanged (EQ): nothing to commit.
;;;;   middleware-protocol's base method calls FINISH-RESPONSE through
;;;;   MAP-RESPONSE, so RESPONSE is always an immediate list: for a Clack
;;;;   streaming response it is the head the app hands its responder, and
;;;;   FINISH-RESPONSE runs when the app calls the responder, before any of
;;;;   the body is written. FINISH-SESSION's own streaming case is not used
;;;;   here (it is for callers that do not go through MAP-RESPONSE). As in
;;;;   S0: a change to the session after that signals SESSION-WRITE-ERROR (a
;;;;   streamed body cannot change the session); an app that never calls its
;;;;   responder commits nothing; a store failure during a streamed commit
;;;;   propagates out of the responder call, outside HANDLE's dynamic extent
;;;;   (so an ERROR-HANDLER does not see it: middleware M2).
;;;;   FINISH-RESPONSE methods of inner mixins see the response before the
;;;;   session's Set-Cookie is added; those of outer mixins, and every :AROUND
;;;;   that maps the response (RESPONSE-HEADERS-HANDLER, TIMING-HANDLER), see
;;;;   it with the Set-Cookie, wherever they are in the class precedence list:
;;;;   the hooks run inside the base method, the innermost :AROUND.
;;;;
;;;; When nothing is begun or committed (locked)
;;;;   * Short-circuit: an :AROUND that answers without CALL-NEXT-METHOD
;;;;     (a gate's 403), wherever it sits in the class precedence list, skips
;;;;     middleware-protocol's base method, so neither hook runs: no
;;;;     BEGIN-SESSION, no store call, no commit, no Set-Cookie, and the
;;;;     request has no session. Outer :AROUNDs still map that answer
;;;;     (RESPONSE-HEADERS-HANDLER adds its headers to the 403).
;;;;   * A condition escaping the app (or an inner layer) unwinds the base
;;;;     method before FINISH-RESPONSE: the session is begun but not
;;;;     committed. That holds also when an ERROR-HANDLER maps the condition
;;;;     to a response, wherever it sits in the class precedence list: its
;;;;     :AROUND is outside the base method, so its response (a 500) has no
;;;;     session Set-Cookie, nothing is stored, the request's changes to the
;;;;     session are lost and the visitor's stored session is as it was. The
;;;;     error function still sees the begun session through
;;;;     (REQUEST-SESSION request); an error page that must keep the session
;;;;     commits it itself with FINISH-SESSION.
;;;;   * A request that is not a CLACK-REQUEST (a plain STACK-MIDDLEWARE:REQUEST,
;;;;     an rpc request later, any other object) passes through: neither
;;;;     method applies, the handler's answer is unchanged and the manager is
;;;;     not called. A subclass of CLACK-REQUEST (CLACK-APP's :REQUEST-CLASS)
;;;;     gets sessions.
;;;;
;;;; REQUEST-SESSION request -> session or NIL
;;;;   For a STACK-MIDDLEWARE:REQUEST, the value of its :SESSION property
;;;;   (the primary value only), or NIL when it has none. For any other
;;;;   object, NIL (no error): code that may get any request can ask.
;;;;
;;;; No special variable is bound: the channels are the request property and
;;;; the env key, which a streamed body still reaches (middleware.md, D6).
;;;;
;;;; Given in the S0b contract: the package, SESSION-HANDLER with its slot,
;;;; initarg, default, accessor and type check. The two hook methods and
;;;; REQUEST-SESSION implement the rules above; the Rove suite in tests/ (the
;;;; middleware-*.lisp files) is the spec.

(defpackage #:session-protocol/middleware
  (:use #:cl #:session-protocol #:middleware-protocol #:middleware-protocol/clack)
  (:nicknames #:stack-session-middleware)
  (:documentation "Sessions as a middleware-protocol mixin (slice S0b).

SESSION-HANDLER begins a session before the handler's primary method
(PREPARE-REQUEST: BEGIN-SESSION on the Clack env, the session on the request
as :SESSION and in the env) and commits it on the way out (FINISH-RESPONSE:
FINISH-SESSION through MAP-RESPONSE, so a streamed response commits when the
app calls its responder). REQUEST-SESSION reads it back from a request.")
  (:export #:session-handler
           #:session-handler-manager
           #:request-session))

(in-package #:session-protocol/middleware)

(defclass session-handler (handler)
  ((%session-manager :initarg :session-manager
                     :initform (make-session-manager)
                     :accessor session-handler-manager))
  (:documentation "Mixin that begins a session in PREPARE-REQUEST and commits it in
FINISH-RESPONSE, for Clack requests (file header)."))

(defmethod initialize-instance :after ((handler session-handler) &key)
  (check-type (slot-value handler '%session-manager) session-manager))

(defun request-session (request)
  "The session SESSION-HANDLER put on REQUEST (its :SESSION property), or NIL;
NIL for an object that is not a STACK-MIDDLEWARE:REQUEST (file header)."
  (if (typep request 'request)
      ;; VALUES: the primary value only, not REQUEST-PROPERTY's PRESENT-P.
      (values (request-property request :session))
      nil))

(defmethod prepare-request ((handler session-handler) (request clack-request))
  "Begin the session: on the request as :SESSION and in its env (file header)."
  (multiple-value-bind (session env*)
      (begin-session (session-handler-manager handler) (clack-request-env request))
    ;; Steps 2 and 3, only once BEGIN-SESSION has returned (its RETRY and
    ;; CONTINUE are inside it), on the request object every outer layer holds.
    ;; ENV* is BEGIN-SESSION's (LIST* :SESSION session <the server's env>),
    ;; installed as is.
    (setf (request-property request :session) session
          (clack-request-env request) env*))
  ;; Step 4: the PREPARE-REQUEST methods of inner mixins see the session.
  (call-next-method))

(defmethod finish-response ((handler session-handler) (request clack-request) response)
  "Commit the request's session into the response, an immediate list (file header)."
  (declare (ignorable response))        ; passed on by CALL-NEXT-METHOD
  (let* ((response* (call-next-method)) ; inner mixins finish first
         (session (request-session request)))
    (if (typep session 'session)
        (finish-session (session-handler-manager handler) session
                        (clack-request-env request) response*)
        response*)))
