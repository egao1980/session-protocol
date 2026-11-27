;;;; The cookie: READ-SESSION-COOKIE, FORMAT-SESSION-COOKIE.

(in-package #:session-protocol/tests)

(deftest-parametrize read-session-cookie-from-header
    ((header expected)
     :ids ("alone" "second" "no-space" "leading-space" "leading-tab"
           "first-of-two" "case-sensitive-name" "longer-name" "suffix-name"
           "no-equals" "no-equals-then-pair" "empty-value" "percent-raw"
           "quotes-raw" "trailing-space-raw" "equals-in-value" "only-separators"
           "garbage" "empty-header" "bare-equals")
     ("sid=abc" "abc")
     ("theme=dark; sid=abc" "abc")
     ("theme=dark;sid=abc" "abc")
     ("  sid=abc" "abc")
     ((format nil "a=1;~csid=abc" #\Tab) "abc")
     ("sid=abc; sid=def" "abc")
     ("SID=abc; Sid=def" nil)
     ("sidx=abc" nil)
     ("xsid=abc" nil)
     ("sid" nil)
     ("sid; sid=abc" "abc")
     ("sid=" "")
     ("sid=a%20b" "a%20b")
     ("sid=\"abc\"" "\"abc\"")
     ("sid=abc ; theme=dark" "abc ")
     ("sid=a=b" "a=b")
     ("; ;;" nil)
     ("%%%=;=;\\" nil)
     ("" nil)
     ("=abc; sid=x" "x"))
  (ok (equal expected (read-session-cookie (make-cookie-policy)
                                           (make-env :cookie header)))))

(deftest read-session-cookie-custom-name
  (let ((p (make-cookie-policy :name "my_sid-2")))
    (ok (equal "v" (read-session-cookie p (make-env :cookie "sid=x; my_sid-2=v"))))
    (ok (null (read-session-cookie p (make-env :cookie "sid=x"))))))

(deftest read-session-cookie-without-a-header
  (let ((p (make-cookie-policy)))
    (ok (null (read-session-cookie p (make-env))) "no Cookie header")
    (ok (null (read-session-cookie p (make-env :headers nil))) "no :HEADERS at all")
    (let ((env (make-env)))
      (setf (gethash "cookie" (getf env :headers)) 42)
      (ok (null (read-session-cookie p env)) "a header that is not a string"))
    (let ((env (make-env)))
      (setf (gethash "Cookie" (getf env :headers)) "sid=abc")
      (ok (null (read-session-cookie p env))
          "only the lowercase \"cookie\" entry is read (Clack lowercases names)"))))

(defun expected-cookie (value max-age &key (name "sid") (same-site :lax) secure)
  (cl-cookie:write-set-cookie-header
   (cl-cookie:make-cookie :name name :value value :path "/" :max-age max-age
                          :same-site same-site :secure-p secure :httponly-p t)))

(defparameter +an-id+ "Zm9vYmFyYmF6cXV4cXV1eGZvb2JhcmJhenF1eHF1dXg")

(deftest-parametrize format-session-cookie-is-cl-cookies
    ((policy-args scheme secure)
     :ids ("http" "https" "https-uppercase" "no-scheme" "forced-on-http"
           "forced-on-https" "strict" "custom-name")
     ('() "http" nil)
     ('() "https" t)
     ('() "HTTPS" t)
     ('() nil nil)
     ('(:secure t) "http" t)
     ('(:secure t) "https" t)
     ('(:same-site :strict) "http" nil)
     ('(:name "visitor") "https" t))
  (let* ((policy (apply #'make-cookie-policy policy-args))
         (env (let ((e (make-env :scheme scheme)))
                (if scheme e (progn (remf e :url-scheme) e))))
         (text (format-session-cookie policy env +an-id+ 86400)))
    (ok (equal (expected-cookie +an-id+ 86400
                                :name (cookie-policy-name policy)
                                :same-site (cookie-policy-same-site policy)
                                :secure secure)
               text)
        "written by cl-cookie's WRITE-SET-COOKIE-HEADER")
    (multiple-value-bind (name value attributes) (parse-set-cookie text)
      (ok (equal (cookie-policy-name policy) name))
      (ok (equal +an-id+ value))
      (ok (same-attributes-p
           (append (list (cons "max-age" "86400") (cons "path" "/")
                         (cons "samesite" (if (eq :strict (cookie-policy-same-site policy))
                                              "Strict" "Lax"))
                         (cons "httponly" nil))
                   (and secure (list (cons "secure" nil))))
           attributes)
          "exactly Max-Age, Path=/, SameSite, HttpOnly and Secure when due"))))

(deftest format-session-cookie-attributes-literally
  (ok (equal (format nil "sid=~a; Max-age=600; Path=/; SameSite=Lax; HttpOnly" +an-id+)
             (format-session-cookie (make-cookie-policy) (make-env) +an-id+ 600)))
  (ok (equal (format nil "sid=~a; Max-age=600; Path=/; SameSite=Strict; Secure; HttpOnly" +an-id+)
             (format-session-cookie (make-cookie-policy :same-site :strict)
                                    (make-env :scheme "https") +an-id+ 600))))

(deftest format-session-cookie-never-sets-domain-or-expires
  (dolist (scheme '("http" "https"))
    (let ((attributes (nth-value 2 (parse-set-cookie
                                    (format-session-cookie (make-cookie-policy)
                                                           (make-env :scheme scheme)
                                                           +an-id+ 60)))))
      (ok (not (assoc "domain" attributes :test #'string=)))
      (ok (not (assoc "expires" attributes :test #'string=)))
      (ok (not (assoc "partitioned" attributes :test #'string=))))))

(deftest format-session-cookie-expiry-form
  (let ((text (format-session-cookie (make-cookie-policy) (make-env :scheme "https") "" 0)))
    (ok (equal (expected-cookie "" 0 :secure t) text))
    (multiple-value-bind (name value attributes) (parse-set-cookie text)
      (ok (equal "sid" name))
      (ok (equal "" value))
      (ok (equal "0" (cdr (assoc "max-age" attributes :test #'string=))))
      (ok (assoc "httponly" attributes :test #'string=))
      (ok (assoc "secure" attributes :test #'string=)))))

(deftest format-session-cookie-checks-arguments
  (let ((p (make-cookie-policy)) (env (make-env)))
    (ok (signals (format-session-cookie p env 42 60) 'type-error))
    (ok (signals (format-session-cookie p env nil 60) 'type-error))
    (ok (signals (format-session-cookie p env +an-id+ -1) 'type-error))
    (ok (signals (format-session-cookie p env +an-id+ 1.5) 'type-error))
    (ok (signals (format-session-cookie p env +an-id+ nil) 'type-error))))
