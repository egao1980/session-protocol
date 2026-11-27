(defpackage #:session-protocol/tests
  (:use #:cl #:rove))

(in-package #:session-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:session-protocol)))
