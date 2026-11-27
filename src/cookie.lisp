;;;; S0 cookie policy: the one cookie that carries the session id.
;;;;
;;;; (MAKE-COOKIE-POLICY &key (name "sid") (same-site :lax) (secure :auto))
;;;;   (given) -> a COOKIE-POLICY; readers COOKIE-POLICY-NAME,
;;;;   COOKIE-POLICY-SAME-SITE, COOKIE-POLICY-SECURE. TYPE-ERROR for:
;;;;   NAME       not a non-empty string of [A-Za-z0-9_-]; or a name that
;;;;              starts with "__Host-" or "__Secure-" (any case, as browsers
;;;;              compare) while SECURE is not T: browsers drop such a cookie
;;;;              unless it is Secure, and :AUTO leaves Secure off on http.
;;;;   SAME-SITE  not :LAX or :STRICT. :NONE is refused in 0.1.0: it needs
;;;;              Secure and lets every other site's page send the cookie.
;;;;   SECURE     not :AUTO or T. :AUTO -> Secure exactly when the request's
;;;;              scheme is https; T -> always (behind a TLS-terminating proxy
;;;;              the app sees http: use T there; X-Forwarded-Proto is not read,
;;;;              trusting it needs a trusted-proxy list).
;;;; Fixed, not options: Path=/ (one session per origin, every route), HttpOnly
;;;; (no script reads the id), no Domain (host-only: subdomains never get
;;;; it), no Expires (Max-Age only), no Partitioned.
;;;;
;;;; (READ-SESSION-COOKIE policy env) -> the raw cookie value, a string, or NIL
;;;;   ENV is a Clack env. The request's cookies are the "cookie" entry of its
;;;;   :HEADERS hash-table (Clack lowercases header names). No :HEADERS, no
;;;;   entry, or an entry that is not a string -> NIL. The entry is split as
;;;;   rpc-backend-htmx's token gate splits it:
;;;;     (QURI:URL-DECODE-PARAMS entry :delimiter #\; :lenient t :percent-decode nil)
;;;;   names trimmed of spaces and tabs; values raw (not trimmed, not
;;;;   percent-decoded, not unquoted). The result is the value of the FIRST
;;;;   pair whose name is exactly (case-sensitive) the policy's name and which
;;;;   has a "="; NIL when there is none. Later pairs of that name are ignored
;;;;   (express's cookie parser and Rack keep the first too). Never signals on
;;;;   any header text. The value is not validated here.
;;;;
;;;; (FORMAT-SESSION-COOKIE policy env value max-age) -> a Set-Cookie value
;;;;   VALUE a string (a session id; "" for the expiry cookie), MAX-AGE an
;;;;   integer >= 0; else TYPE-ERROR. The result is exactly
;;;;     (CL-COOKIE:WRITE-SET-COOKIE-HEADER
;;;;      (CL-COOKIE:MAKE-COOKIE :name NAME :value VALUE :path "/"
;;;;                             :max-age MAX-AGE :same-site SAME-SITE
;;;;                             :secure-p SECURE :httponly-p t))
;;;;   with SECURE true when the policy's secure is T, or it is :AUTO and ENV's
;;;;   :URL-SCHEME is "https" (STRING-EQUAL: any case); false otherwise (no
;;;;   :URL-SCHEME, "http"). With cl-cookie 0.2.0 that is
;;;;     sid=<value>; Max-age=<n>; Path=/; SameSite=Lax[; Secure]; HttpOnly
;;;;   (cl-cookie spells the attribute "Max-age"; attribute names are
;;;;   case-insensitive, RFC 6265 5.2). The expiry cookie is VALUE "" with
;;;;   MAX-AGE 0: same name, Path, SameSite, Secure and HttpOnly, so the
;;;;   browser replaces and drops the cookie it holds.

(in-package #:session-protocol)

(defclass cookie-policy ()
  ((name :initarg :name :reader cookie-policy-name)
   (same-site :initarg :same-site :reader cookie-policy-same-site)
   (secure :initarg :secure :reader cookie-policy-secure))
  (:documentation "The session cookie: NAME, SAME-SITE (:LAX or :STRICT),
SECURE (:AUTO or T). Path=/ and HttpOnly always; no Domain. See cookie.lisp."))

(defun %cookie-name-p (object)
  "A non-empty string of [A-Za-z0-9_-]."
  (and (stringp object)
       (plusp (length object))
       (every (lambda (c)
                (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9)
                    (char= c #\-) (char= c #\_)))
              object)))

(defun %prefixed-cookie-name-p (name)
  "True when NAME starts with __Host- or __Secure-, in any case."
  (flet ((starts (prefix)
           (and (>= (length name) (length prefix))
                (string-equal prefix name :end2 (length prefix)))))
    (or (starts "__Host-") (starts "__Secure-"))))

(defun make-cookie-policy (&key (name "sid") (same-site :lax) (secure :auto))
  "A COOKIE-POLICY. NAME a non-empty string of [A-Za-z0-9_-] (a __Host- or
__Secure- name needs SECURE T), SAME-SITE :LAX or :STRICT, SECURE :AUTO or T.
TYPE-ERROR otherwise. See the file header."
  (check-type name (satisfies %cookie-name-p))
  (check-type same-site (member :lax :strict))
  (check-type secure (member :auto t))
  (when (and (%prefixed-cookie-name-p name) (not (eq secure t)))
    (error 'type-error :datum secure :expected-type '(eql t)))
  (make-instance 'cookie-policy :name name :same-site same-site :secure secure))

(defun read-session-cookie (policy env)
  "The raw value of the first cookie named (COOKIE-POLICY-NAME POLICY) in ENV's
Cookie header, or NIL. Never signals on header text. See the file header."
  (let* ((headers (getf env :headers))
         (text (and (hash-table-p headers) (gethash "cookie" headers))))
    (when (stringp text)
      (let ((name (cookie-policy-name policy)))
        ;; With :LENIENT T and no percent-decoding, quri does not signal on
        ;; any string. A field without "=" comes back as (name . NIL).
        (loop for (field . value)
                in (quri:url-decode-params text :delimiter #\; :lenient t
                                                :percent-decode nil)
              when (and value
                        (string= name (string-trim '(#\Space #\Tab) field)))
                return value)))))

(defun format-session-cookie (policy env value max-age)
  "The Set-Cookie header value for VALUE with MAX-AGE seconds, written by
CL-COOKIE:WRITE-SET-COOKIE-HEADER. See the file header."
  (check-type value string)
  (check-type max-age (integer 0))
  (let* ((scheme (getf env :url-scheme))
         ;; A real boolean: cl-cookie declares SECURE-P a BOOLEAN slot.
         (secure (or (eq (cookie-policy-secure policy) t)
                     (and (stringp scheme) (string-equal scheme "https")))))
    (cl-cookie:write-set-cookie-header
     (cl-cookie:make-cookie :name (cookie-policy-name policy) :value value
                            :path "/" :max-age max-age
                            :same-site (cookie-policy-same-site policy)
                            :secure-p (and secure t) :httponly-p t))))
