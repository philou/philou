(defpackage :com.gigamonkeys.my-binary-data-system (:use :asdf :cl))
(in-package :com.gigamonkeys.my-binary-data-system)

(defsystem my-binary-data
  :name "my-binary-data"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Parser for binary data files. "
  :long-description ""
  :components
  ((:file "my-packages")
   (:file "my-binary-data" :depends-on ("my-packages")))
  :depends-on (:macro-utilities))

        
