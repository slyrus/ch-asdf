
(in-package #:cl-user)

(defpackage #:ch-asdf
  (:use #:cl #:asdf)
  (:export #:ch-cl-source-file

           #:unix-name
           #:absolute-path-p
           #:asdf-lookup
           #:with-component-directory

           #:unix-dso
           #:dso-type
           #:dso-name
           #:dso-directory
           #:include-directories
           #:system-include-directories
           
           #:gcc-xml-c-source-file
           #:gcc-xml-xml-file
           #:gcc-xml-cl-source-file

           #:generated-source-file
           #:pdf-file
           #:xhtml-file

           ))

