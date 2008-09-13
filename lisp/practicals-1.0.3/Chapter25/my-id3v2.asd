(defpackage :com.gigamonkeys.my-id3v2-system (:use :asdf :cl))
(in-package :com.gigamonkeys.my-id3v2-system)

(defsystem my-id3v2
	:name "id3"
	:author "Peter Seibel <peter@gigamonkeys.com>"
	:version "1.0"
	:maintainer "Peter Seibel <peter@gigamonkeys.com>"
	:licence "BSD"
	:description "ID3v2 parser. "
	:long-description ""
	:components
	((:file "my-packages")
	 (:file "my-id3v2" :depends-on ("my-packages")))
	:depends-on (:my-binary-data :pathnames))
	