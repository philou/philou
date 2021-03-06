(in-package :cl-user)

(defpackage :com.gigamonkeys.my-id3v2
  (:use :common-lisp
        :com.gigamonkeys.my-binary-data
        :com.gigamonkeys.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
