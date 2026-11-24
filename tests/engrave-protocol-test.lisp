(defpackage #:engrave-protocol/tests
  (:use #:cl #:rove))

(in-package #:engrave-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:engrave-protocol)))
