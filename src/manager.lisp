;;;; S0 session manager: the request phase and the response phase.
;;;;
;;;; (MAKE-SESSION-MANAGER &key store cookie-policy (idle-ttl 86400)
;;;;                            absolute-ttl refresh-interval clock)   (given)
;;;;   STORE             a SESSION-STORE. Default: a fresh
;;;;                     (MAKE-CACHE-SESSION-STORE (STACK-CACHE:MAKE-IN-MEMORY-TTL-CACHE)),
;;;;                     process-local: gone on restart, not shared between
;;;;                     processes.
;;;;   COOKIE-POLICY     a COOKIE-POLICY. Default (MAKE-COOKIE-POLICY).
;;;;   IDLE-TTL          an integer >= 1, seconds: a session not written for
;;;;                     this long is gone. Default 86400 (one day).
;;;;   ABSOLUTE-TTL      NIL (the default: no absolute lifetime) or an integer
;;;;                     >= 1, seconds from SESSION-CREATED-AT after which the
;;;;                     session is gone however busy it is. Rotation keeps
;;;;                     CREATED-AT, so it does not extend this.
;;;;   REFRESH-INTERVAL  an integer, 0 <= r < IDLE-TTL: how stale the store's
;;;;                     LAST-SEEN may get before an unchanged session is
;;;;                     written again. Default (MIN 60 (FLOOR IDLE-TTL 2)).
;;;;   CLOCK             a function designator of no arguments returning Unix
;;;;                     seconds, a real. Default: Unix time from
;;;;                     GET-UNIVERSAL-TIME. Each phase reads it once and uses
;;;;                     now = (FLOOR (FUNCALL clock)). Tests inject one (and
;;;;                     give STACK-CACHE:MAKE-IN-MEMORY-TTL-CACHE the same one).
;;;;   Anything else: TYPE-ERROR. Readers SESSION-MANAGER-STORE,
;;;;   -COOKIE-POLICY, -IDLE-TTL, -ABSOLUTE-TTL, -REFRESH-INTERVAL, -CLOCK.
;;;; A manager is shared by every request and holds no per-request state; a
;;;; SESSION belongs to one request.
;;;;
;;;; (BEGIN-SESSION manager env) -> (values session env-with-session)
;;;;   ENV is a Clack env (http-server-protocol: its keys are normative). Steps:
;;;;   1. raw = (READ-SESSION-COOKIE policy env).
;;;;   2. Only when raw passes VALID-SESSION-ID-P:
;;;;      loaded = (SESSION-STORE-LOAD store raw), inside the restarts
;;;;        RETRY     load again;
;;;;        CONTINUE  go on as if the store had no session (loaded = NIL).
;;;;      A missing, malformed or forged-looking value never reaches the store.
;;;;   3. loaded is dropped (as if NIL) when
;;;;      a. its SESSION-ID is not raw, compared with
;;;;         SECRETS-PROTOCOL:CONSTANT-TIME-EQUAL on the character codes (a
;;;;         store matching loosely, a case-insensitive SQL collation, never
;;;;         hands out another visitor's session); the record is left alone;
;;;;      b. now >= LAST-SEEN + IDLE-TTL (idle expiry, checked even when the
;;;;         store's TTL lags), or
;;;;      c. ABSOLUTE-TTL is set and now >= CREATED-AT + ABSOLUTE-TTL;
;;;;      for b and c the record is deleted: (SESSION-STORE-DELETE store raw)
;;;;      inside RETRY / CONTINUE (CONTINUE leaves it to its TTL).
;;;;   4. No session left -> a new one: (MAKE-SESSION :id (GENERATE-SESSION-ID)
;;;;      :created-at now :last-seen now :new-p t). Nothing is stored and no
;;;;      cookie is set here: the commit decides (lazy creation). An invalid,
;;;;      unknown, mismatched or expired cookie is never an error for the
;;;;      client; it gets a new session.
;;;;   5. The session's baseline for the commit is (SESSION-DATA-JSON session)
;;;;      of the data as loaded ("{}" for a new session).
;;;;   6. Returns the session and (LIST* :SESSION session ENV): ENV itself is
;;;;      not modified. The env key is :SESSION.
;;;;
;;;; (ENV-SESSION env) -> (GETF env :SESSION): the session BEGIN-SESSION put in
;;;;   ENV, or NIL.
;;;;
;;;; (FINISH-SESSION manager session env response) -> a Clack response
;;;;   ENV is the request's env (only :URL-SCHEME is read, for Secure).
;;;;   RESPONSE is a list (status headers [body]) or a function of a responder
;;;;   (a streaming response); else TYPE-ERROR.
;;;;   - SESSION already committed -> RESPONSE unchanged (EQ), no store call:
;;;;     a session commits at most once.
;;;;   - A list -> the commit runs now; the result is described in step 7.
;;;;   - A function -> returned at once: a new function of a responder R. It
;;;;     calls RESPONSE with a responder R2 that takes the response list the
;;;;     app hands it (status headers [body]), runs the commit, calls R with
;;;;     that list completed as in step 7 and returns what R returns (for a
;;;;     streamed body, its writer). So the commit runs when the app starts its
;;;;     response, before any of the body is written, and not at all when the
;;;;     app never calls its responder. A change to the session after that
;;;;     signals SESSION-WRITE-ERROR: a streamed body cannot change the session.
;;;;   The commit runs whatever the status (a 404 or 500 response commits).
;;;;   When a condition escapes the app, FINISH-SESSION is not reached and
;;;;   nothing commits.
;;;;
;;;; The commit. now = (FLOOR (FUNCALL clock)); in order:
;;;;   1. Not ended: (SESSION-DATA-JSON session), with its restarts
;;;;      (json.lisp): an invalid value is fixed or dropped before anything is
;;;;      stored.
;;;;   2. ttl = IDLE-TTL, or with ABSOLUTE-TTL
;;;;      (MIN IDLE-TTL (- (+ CREATED-AT ABSOLUTE-TTL) now)). ttl <= 0 (the
;;;;      absolute lifetime ran out during the request) -> the session ends
;;;;      here (SESSION-ENDED-P becomes T).
;;;;   3. Ended: when the session was loaded, (SESSION-STORE-DELETE store
;;;;      <the id it was loaded under>) inside RETRY / CONTINUE (CONTINUE skips
;;;;      the delete); Set-Cookie = (FORMAT-SESSION-COOKIE policy env "" 0),
;;;;      always, new session or not. Then step 6.
;;;;   4. Does it write?
;;;;      new session     -> when its data is not empty. A new session that
;;;;                         stores nothing gets no store entry and no cookie.
;;;;      loaded session  -> when it was rotated, or SESSION-CHANGED-P, or the
;;;;                         text of step 1 differs from the baseline (data
;;;;                         changed in place through SESSION-DATA or a nested
;;;;                         table or list), or now - LAST-SEEN >=
;;;;                         REFRESH-INTERVAL (the refresh rule).
;;;;      No write -> no store call and no Set-Cookie.
;;;;   5. Write: a rotated loaded session first deletes the id it was loaded
;;;;      under (RETRY / CONTINUE: CONTINUE leaves the old record, valid until
;;;;      its TTL: the caller's choice). Then LAST-SEEN = now and
;;;;      (SESSION-STORE-SAVE store session ttl) inside RETRY / CONTINUE
;;;;      (CONTINUE: not saved; the cookie is still sent and the next request
;;;;      starts afresh). Set-Cookie = (FORMAT-SESSION-COOKIE policy env
;;;;      (SESSION-ID session) ttl).
;;;;   6. SESSION-COMMITTED-P becomes T.
;;;;   7. With a Set-Cookie, the result is a fresh list: status, then a fresh
;;;;      copy of the app's headers plist followed by :SET-COOKIE <value>, then
;;;;      the rest of the app's list (the body, when there is one). The app's
;;;;      headers keep their order and values, every :SET-COOKIE the app set
;;;;      included (Clack writes each :SET-COOKIE entry as its own header; a
;;;;      cookie of the app with the session's name loses to ours, which comes
;;;;      last). The app's list is not modified. Without a Set-Cookie the
;;;;      app's list is returned as is.
;;;;
;;;; Cookie lifetime: Max-Age is the ttl of the save that sends it, so the
;;;; browser's cookie and the store entry expire together and slide forward
;;;; together on each write. A request that writes nothing sends no cookie;
;;;; a session therefore lives at least IDLE-TTL - REFRESH-INTERVAL after its
;;;; last request and at most IDLE-TTL (with ABSOLUTE-TTL, never past
;;;; CREATED-AT + ABSOLUTE-TTL).
;;;;
;;;; Store failures (SESSION-STORE-ERROR) not handled by the caller propagate
;;;; out of BEGIN-SESSION or FINISH-SESSION (for a streaming response, out of
;;;; the responder call), like any error of the app.
;;;;
;;;; Concurrency: store calls are the store's atomic operations; nothing
;;;; locks a session across a request; of two requests of one visitor that
;;;; both write, the later write wins.

(in-package #:session-protocol)

(defun %unix-time ()
  "Unix seconds from GET-UNIVERSAL-TIME: the default clock."
  ;; 2208988800 = (encode-universal-time 0 0 0 1 1 1970 0)
  (- (get-universal-time) 2208988800))

(defclass session-manager ()
  ((store :initarg :store :reader session-manager-store)
   (cookie-policy :initarg :cookie-policy :reader session-manager-cookie-policy)
   (idle-ttl :initarg :idle-ttl :reader session-manager-idle-ttl)
   (absolute-ttl :initarg :absolute-ttl :reader session-manager-absolute-ttl)
   (refresh-interval :initarg :refresh-interval
                     :reader session-manager-refresh-interval)
   (clock :initarg :clock :reader session-manager-clock))
  (:documentation "A store, a cookie policy, the lifetimes and a clock. See
manager.lisp."))

(defun make-session-manager (&key store cookie-policy (idle-ttl 86400)
                                  absolute-ttl refresh-interval clock)
  "A SESSION-MANAGER. See the file header for each argument and its default."
  (let ((store (or store
                   (make-cache-session-store
                    (stack-cache:make-in-memory-ttl-cache))))
        (cookie-policy (or cookie-policy (make-cookie-policy)))
        (clock (or clock '%unix-time)))
    (check-type store session-store)
    (check-type cookie-policy cookie-policy)
    (check-type idle-ttl (integer 1))
    (check-type absolute-ttl (or null (integer 1)))
    (let ((refresh-interval (if refresh-interval
                                refresh-interval
                                (min 60 (floor idle-ttl 2)))))
      (check-type refresh-interval (integer 0))
      (unless (< refresh-interval idle-ttl)
        (error 'type-error :datum refresh-interval
                           :expected-type `(integer 0 ,(1- idle-ttl))))
      (check-type clock (or function (and symbol (not null))))
      (make-instance 'session-manager
                     :store store :cookie-policy cookie-policy
                     :idle-ttl idle-ttl :absolute-ttl absolute-ttl
                     :refresh-interval refresh-interval :clock clock))))

(defun begin-session (manager env)
  "The request phase: read the session cookie of ENV, load the session or
start a new one (stored only if the commit writes it). Returns the session
and ENV with the session under :SESSION. See the file header."
  (declare (ignorable manager env))
  (%s0-pending 'begin-session))

(defun env-session (env)
  "The session BEGIN-SESSION put in ENV (under :SESSION), or NIL."
  (declare (ignorable env))
  (%s0-pending 'env-session))

(defun finish-session (manager session env response)
  "The response phase: commit SESSION (save, rotate or delete) and add its
Set-Cookie to RESPONSE, a Clack response list or streaming function. See the
file header."
  (declare (ignorable manager session env response))
  (%s0-pending 'finish-session))
