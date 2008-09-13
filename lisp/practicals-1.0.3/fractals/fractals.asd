(defpackage :com.philou.fractals-system (:use :asdf :cl))
(in-package :com.philou.fractals-system)

(defsystem fractals
	:name "fractals"
	:author "Philou"
	:version "1.0"
	:maintainer "Philou"
	:licence "BSD"
	:description "Fractals test thing ..."
	:long-description ""
	:components
	((:file "packages")
	 (:file "fractals" :depends-on ("packages")))
	:depends-on (:test-framework))
	