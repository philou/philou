(in-package :cl-user)

(defpackage :com.gigamonkeys.test
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :deftest :check :test-everything :test-package :test-current-package))
