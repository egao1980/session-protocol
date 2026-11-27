;;;; Session ids: GENERATE-SESSION-ID, VALID-SESSION-ID-P.

(in-package #:session-protocol/tests)

(defun id-alphabet-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9)
      (char= c #\-) (char= c #\_)))

(deftest generated-ids-are-43-base64url-characters
  (ok (= 43 +session-id-length+))
  (let ((ids (loop repeat 200 collect (generate-session-id))))
    (ok (every #'stringp ids))
    (ok (every (lambda (id) (= 43 (length id))) ids))
    (ok (every (lambda (id) (every #'id-alphabet-p id)) ids))
    (ok (every #'valid-session-id-p ids))
    (ok (= 200 (length (remove-duplicates ids :test #'string=)))
        "200 ids, all distinct")))

(deftest generated-ids-are-token-urlsafe-32
  ;; Same shape as the secrets-protocol call they come from: 32 octets in
  ;; base64url without padding (the last character carries 2 bits, so it is
  ;; one of 16 characters).
  (let ((ids (loop repeat 200 collect (generate-session-id))))
    (ok (every (lambda (id) (find (char id 42) "AEIMQUYcgkosw048")) ids))
    (ok (= 43 (length (stack-secrets:token-urlsafe 32))))))

(deftest-parametrize valid-session-ids
    ((id)
     :ids ("generated" "all-a" "every-class" "hyphens" "underscores")
     ((generate-session-id))
     ((make-string 43 :initial-element #\A))
     ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJ0123_-Z")
     ((make-string 43 :initial-element #\-))
     ((make-string 43 :initial-element #\_)))
  (ok (valid-session-id-p id)))

(deftest-parametrize invalid-session-ids
    ((object)
     :ids ("42-chars" "44-chars" "empty" "plus" "slash" "padding" "space" "dot"
           "non-ascii" "newline" "nil" "symbol" "number" "octets" "list"
           "character")
     ((make-string 42 :initial-element #\A))
     ((make-string 44 :initial-element #\A))
     ("")
     ((concatenate 'string (make-string 42 :initial-element #\A) "+"))
     ((concatenate 'string "/" (make-string 42 :initial-element #\A)))
     ((concatenate 'string (make-string 42 :initial-element #\A) "="))
     ((concatenate 'string (make-string 21 :initial-element #\A) " "
                   (make-string 21 :initial-element #\A)))
     ((concatenate 'string (make-string 42 :initial-element #\A) "."))
     ((concatenate 'string (make-string 42 :initial-element #\A)
                   (string (code-char 233))))
     ((concatenate 'string (make-string 42 :initial-element #\A)
                   (string #\Newline)))
     (nil)
     ('abc)
     (42)
     ((make-array 43 :element-type '(unsigned-byte 8) :initial-element 65))
     ('(#\A #\B))
     (#\A))
  (ok (null (valid-session-id-p object))))
