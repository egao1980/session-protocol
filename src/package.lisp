;;;; session-protocol: anonymous server-side sessions for Clack apps on
;;;; cl-stack. Brief: cl-stack/docs/capabilities/session.md, slice S0.
;;;;
;;;; A visitor's browser holds one cookie with a random session id; the
;;;; session's data lives on the server, in a store. The protocol is
;;;; transport-neutral: two phases over a Clack env and a Clack response,
;;;; BEGIN-SESSION (read the cookie, load or start a session) and
;;;; FINISH-SESSION (commit: save, rotate or delete, and add the Set-Cookie).
;;;; A middleware binds them (slice S0b, a SESSION-HANDLER mixin on
;;;; middleware-protocol); this package defines no middleware, no Lack
;;;; middleware and nothing specific to a web framework.
;;;;
;;;; Files:
;;;;   conditions.lisp  SESSION-ERROR and its subtypes; restart functions
;;;;                    SKIP-VALUE and RETRY (given)
;;;;   id.lisp          GENERATE-SESSION-ID, VALID-SESSION-ID-P
;;;;   model.lisp       SESSION (given: class, MAKE-SESSION, readers);
;;;;                    SESSION-VALUE and its SETF, REMOVE-SESSION-VALUE,
;;;;                    CLEAR-SESSION, REGENERATE-SESSION-ID, END-SESSION
;;;;   json.lisp        the value domain; SESSION-DATA-JSON,
;;;;                    ENCODE-SESSION-RECORD, DECODE-SESSION-RECORD
;;;;   store.lisp       SESSION-STORE and its generic functions;
;;;;                    CACHE-SESSION-STORE over any cache-protocol cache
;;;;   cookie.lisp      COOKIE-POLICY (given: class, MAKE-COOKIE-POLICY);
;;;;                    READ-SESSION-COOKIE, FORMAT-SESSION-COOKIE
;;;;   manager.lisp     SESSION-MANAGER (given: class, MAKE-SESSION-MANAGER);
;;;;                    BEGIN-SESSION, FINISH-SESSION, ENV-SESSION
;;;;
;;;; Given in the S0 contract: the package, the conditions and restart
;;;; functions, the classes with their constructors (argument checks
;;;; included) and readers. Each file header states the rules its
;;;; functions implement, and the Rove suite in tests/ is the spec.
;;;;
;;;; Secrets. A session id is a bearer credential. No report, message,
;;;; PRINT-OBJECT output or condition slot of this library contains one; it
;;;; travels only in the Set-Cookie header and in the store key.

(defpackage #:session-protocol
  (:use #:cl)
  (:nicknames #:stack-session)
  (:documentation "Anonymous server-side sessions for Clack apps (slice S0).

A SESSION holds string keys mapped to JSON values, with an id, a creation
time, a last-seen time and per-request flags. A SESSION-STORE loads, saves
(with a TTL) and deletes sessions by id; CACHE-SESSION-STORE keeps them as
JSON text in any cache-protocol cache. A SESSION-MANAGER joins a store, a
COOKIE-POLICY and the lifetimes; BEGIN-SESSION and FINISH-SESSION are the
request and response phases a middleware calls.")
  (:export
   ;; conditions.lisp
   #:session-error
   #:session-error-message
   #:session-value-error
   #:session-value-error-key
   #:session-value-error-value
   #:session-store-error
   #:session-store-error-store
   #:session-store-error-operation
   #:session-store-error-cause
   #:session-write-error
   #:session-write-error-session
   #:skip-value
   #:retry
   ;; id.lisp
   #:+session-id-length+
   #:generate-session-id
   #:valid-session-id-p
   ;; model.lisp
   #:session
   #:make-session
   #:session-id
   #:session-data
   #:session-created-at
   #:session-last-seen
   #:session-new-p
   #:session-changed-p
   #:session-rotated-p
   #:session-ended-p
   #:session-committed-p
   #:session-value
   #:remove-session-value
   #:clear-session
   #:regenerate-session-id
   #:end-session
   ;; json.lisp
   #:+max-value-depth+
   #:session-data-json
   #:encode-session-record
   #:decode-session-record
   ;; store.lisp
   #:session-store
   #:session-store-load
   #:session-store-save
   #:session-store-delete
   #:cache-session-store
   #:make-cache-session-store
   #:cache-session-store-cache
   #:cache-session-store-prefix
   ;; cookie.lisp
   #:cookie-policy
   #:make-cookie-policy
   #:cookie-policy-name
   #:cookie-policy-same-site
   #:cookie-policy-secure
   #:read-session-cookie
   #:format-session-cookie
   ;; manager.lisp
   #:session-manager
   #:make-session-manager
   #:session-manager-store
   #:session-manager-cookie-policy
   #:session-manager-idle-ttl
   #:session-manager-absolute-ttl
   #:session-manager-refresh-interval
   #:session-manager-clock
   #:begin-session
   #:finish-session
   #:env-session))

(in-package #:session-protocol)
