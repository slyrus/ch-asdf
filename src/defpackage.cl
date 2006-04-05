
(in-package #:cl-user)

(defpackage #:ch-asdf
  (:use #:cl #:asdf)
  (:export #:ch-cl-source-file

           #:unix-name
           #:absolute-path-p
           #:asdf-lookup
           #:asdf-lookup-path
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

           #:jpeg-file
           #:png-file
           #:tiff-file

           #:generated-file
           #:generated-source-file

           #:pdf-file
           #:xhtml-file

           #:markup-file
           #:markup-latex-file
           #:markup-pdf-file
           #:markup-xhtml-file

           ))

