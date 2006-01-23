
(defpackage #:ch-asdf-system (:use #:asdf #:cl))
(in-package #:ch-asdf-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-asdf-cl-source-file
;;;;
(defclass ch-asdf-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-asdf-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c ch-asdf-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defsystem #:ch-asdf
  :name "ch-asdf"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.1.0-20051115"
  :depends-on (:ch-util :gcc-xml-ffi :puri)
  :licence "BSD"
  :description "ASDF Extensions from Cyrus Harmon"
  :components
  ((:module :src
	    :components
	    ((:ch-asdf-cl-source-file "defpackage")
	     (:ch-asdf-cl-source-file "asdf-util"
                                      :depends-on ("defpackage"))
	     (:ch-asdf-cl-source-file "ch-asdf"
                                      :depends-on ("defpackage" "asdf-util"))
	     (:ch-asdf-cl-source-file "gcc-xml"
                                      :depends-on ("defpackage" "ch-asdf"))))))

