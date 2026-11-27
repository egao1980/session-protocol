;;;; S0 session ids.
;;;;
;;;; (GENERATE-SESSION-ID) -> a fresh string:
;;;;   (SECRETS-PROTOCOL:TOKEN-URLSAFE 32), 32 octets (256 bits) from the
;;;;   stack's CSPRNG, base64url without padding: +SESSION-ID-LENGTH+ (43)
;;;;   characters of [A-Za-z0-9_-]. session-protocol depends on
;;;;   secrets-protocol, not on a backend: with no secrets backend loaded this
;;;;   signals SECRETS-PROTOCOL:SECRETS-ERROR (loading crypto-backend-ironclad
;;;;   installs one). Never derived from anything the request sent.
;;;;
;;;; (VALID-SESSION-ID-P object) -> generalized boolean, true exactly when
;;;;   OBJECT is a string of +SESSION-ID-LENGTH+ characters, each in
;;;;   [A-Za-z0-9_-] (ASCII letters and digits, hyphen, underscore). Never
;;;;   signals, whatever OBJECT is. It does not check that the last character
;;;;   is a canonical base64url final digit: a non-canonical id is simply
;;;;   unknown to the store.
;;;;   BEGIN-SESSION runs it on the cookie value before any store lookup: a
;;;;   value that fails it never reaches the store (manager.lisp).

(in-package #:session-protocol)

(defconstant +session-id-length+ 43
  "Characters in a session id: base64url of 32 octets without padding.")

(defun generate-session-id ()
  "A fresh session id: (SECRETS-PROTOCOL:TOKEN-URLSAFE 32). See the file header."
  (%s0-pending 'generate-session-id))

(defun valid-session-id-p (object)
  "True when OBJECT is a string of +SESSION-ID-LENGTH+ characters of
[A-Za-z0-9_-]. Never signals. See the file header."
  (declare (ignorable object))
  (%s0-pending 'valid-session-id-p))
