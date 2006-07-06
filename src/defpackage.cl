
(in-package #:cl-user)

(defpackage #:ch-asdf
  (:use #:cl #:asdf)
  (:export #:ch-cl-source-file
           #:ch-lisp-source-file
     
           #:unix-name
           #:absolute-path-p
           #:asdf-lookup
           #:asdf-lookup-path
           #:with-component-directory
           #:asdf-load
           #:asdf-compile
           
           #:unix-dso
           #:dso-type
           #:dso-name
           #:dso-directory
           #:include-directories
           #:system-include-directories
           
           #:gcc-xml-c-source-file
           #:gcc-xml-xml-file
           #:gcc-xml-cl-source-file

           #:jpeg-file
           #:png-file
           #:tiff-file

           #:generated-file
           #:generated-source-file

           #:pdf-file
           #:css-file
           #:xhtml-file

           #:markup-file
           #:markup-latex-file
           #:markup-pdf-file
           #:markup-xhtml-file

           #:tinaa-directory

           #:object-component
           #:object-from-variable
           #:object-from-file
           #:object-to-file

           #:graphviz-dot-file))

