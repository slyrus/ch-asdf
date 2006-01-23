;;
;; file: asdf-util.cl
;; author: cyrus harmon
;; package: ch-asdf
;;

(in-package :ch-asdf)

(defclass gcc-xml-c-source-file (c-source-file)
  ((xml-output-file-path :accessor xml-output-file-path :initarg :xml-output-file-path)))

(defclass gcc-xml-cl-source-file (ch-cl-source-file) ())
  
;;; we need the following to keep operation-done-p happy.
;;; it pukes if we try to do operation-done-p on files
;;; that don't yet exist, and we have one is going to get generated
;;; from the xml file
(defmethod operation-done-p ((o operation) (c gcc-xml-cl-source-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

(defclass gcc-xml-xml-file (source-file)
  ((output-file-path :accessor output-file-path :initarg :output-file-path)
   (dest-package :accessor dest-package :initarg :dest-package)))

(defmethod operation-done-p ((o operation) (c gcc-xml-xml-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

(defmethod source-file-type ((c gcc-xml-xml-file) (s module)) "xml")

(defmethod output-files ((operation compile-op) (c gcc-xml-xml-file))
  (if (output-file-path c)
      (list (merge-pathnames (output-file-path c) (component-pathname c)))
      (list (merge-pathnames (make-pathname :type "lisp") (component-pathname c)))))

(defmethod perform ((op compile-op) (c gcc-xml-xml-file))
  (let ((decls (gcc-xml-ffi:parse-gcc-xml-file (component-pathname c))))
    (with-open-file (defout (car (output-files op c)) :direction :output :if-exists :supersede)
      (format defout "(cl:in-package :~A)" (dest-package c))
      (let ((*package* (find-package (dest-package c))))
        (gcc-xml-ffi::write-sb-alien-declarations decls defout)))))
  
(defmethod perform ((op load-op) (c gcc-xml-xml-file))
  nil)

(defmethod output-files ((operation compile-op) (c gcc-xml-c-source-file))
  (if (slot-boundp c 'xml-output-file-path)
      (list
       (merge-pathnames
        (xml-output-file-path c)
        (component-pathname (component-parent c))))
      (let ((dir (component-pathname c)))
        (list
         (make-pathname :type "xml"
                        :name (car (last (pathname-directory c)))
                        :directory (butlast (pathname-directory c))
                        :defaults dir)))))

(defgeneric include-directories (component))
(defmethod include-directories ((c component))
  nil)

(defgeneric system-include-directories (component))
(defmethod system-include-directories ((c component))
  nil)

(defgeneric link-library-directories (component))
(defmethod link-library-directories ((c component))
  nil)

(defgeneric link-libraries (component))
(defmethod link-libraries ((c component))
  nil)


(defmethod perform ((op compile-op) (c gcc-xml-c-source-file))
  (print c)
  (unless
      (= 0 (run-shell-command
            (concatenate 'string 
                         (format nil "~A ~A -fxml=~A "
                                 gcc-xml-ffi::*gccxml-executable*
                                 (ch-util:unix-name (component-pathname c))
                                 (ch-util:unix-name (car (output-files op c))))
                         (format nil "~{-L~A~^ ~}" (link-library-directories c))
                         (format nil "~{-l~A~^ ~}" (link-libraries c))
                         (format nil " ~{-isystem ~A~^ ~} ~{-I~A~^ ~} "
                                 (mapcar #'unix-name (system-include-directories c))
                                 (mapcar #'unix-name (include-directories c))))))
    (error 'operation-error :operation op :component c)))

