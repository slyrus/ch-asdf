
(In-package :ch-asdf)

(defclass clean-op (operation) ())

(defmethod perform ((operation clean-op) (c component))
  nil)

(defclass ch-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-cl-source-file) (s module)) "cl")

(defmethod output-files :around ((operation compile-op) (c ch-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))


(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defun absolute-path-p (path)
  (when (listp (pathname-directory path))
    (eql (car (car (pathname-directory path)))
         :absolute)))


;;;; C source file compilation section
;;;; ripped from sb-posix.asd in the sbcl source code

(defclass unix-dso (module)
  ((dso-name :accessor dso-name :initarg :dso-name)
   (dso-directory :accessor dso-directory :initarg :dso-directory)
   (include-directories :accessor include-directories :initarg :include-directories :initform nil)
   (link-library-directories :accessor link-library-directories :initarg :link-library-directories :initform nil)
   (link-libraries :accessor link-libraries :initarg :link-libraries :initform nil)
   (dso-type :accessor dso-type :initarg :dso-type :initform
             ;; fill appropriate OS specific types in here
             #+darwin "so"
             #-darwin "so")))

(defmethod input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type (dso-type dso)
		    :name (if (slot-boundp dso 'dso-name)
                              (dso-name dso)
                              (car (last (pathname-directory dir))))
                    :directory (cond
                                 ((slot-boundp dso 'dso-directory)
                                  (let ((dso-pathname
                                         (merge-pathnames (dso-directory dso)
                                                          (component-pathname dso))))
                                    (ensure-directories-exist dso-pathname)
                                    (pathname-directory dso-pathname)))
                                 ((and (slot-boundp dso 'dso-name)
                                       (absolute-path-p (dso-name dso)))
                                  nil)
                                 (t (butlast (pathname-directory dir))))
		    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      "gcc ~A -o ~S ~{~S ~}"
	      (concatenate 'string
                           ;; This really should be specified as an initarg of the unix-dso
                           ;; rather than hard coded here!
                           ;; e.g. :components (... (:unix-library "R" :library-directory *r-dir*))
			   (sb-ext:posix-getenv "EXTRA_LDFLAGS")
			   " "
                           (format nil " ~{-L~A~^ ~} " (link-library-directories dso))
                           (format nil " ~{-l~A~^ ~} " (link-libraries dso))
			   #+sunos " -shared -lresolv -lsocket -lnsl "
			   #+darwin " -bundle "
			   #-(or darwin sunos) " -shared ")
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
      (error 'operation-error :operation operation :component dso))))

;;; if this goes into the standard asdf, it could reasonably be extended
;;; to allow cflags to be set somehow
(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))


(defgeneric get-include-directories (c))

(defmethod get-include-directories ((c c-source-file))
  (when (and 
         (slot-exists-p (component-parent c) 'include-directories)
         (slot-boundp (component-parent c) 'include-directories))
    (mapcar
     #'ch-util:unix-name
     (include-directories
      (component-parent c)))))
  
(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command
            (concatenate 'string
                         (format nil "gcc ~A -o ~S -c ~S"
                                 (concatenate
                                  'string
                                  (format nil "~{-I~A~^ ~}" (get-include-directories c))
                                  " " (sb-ext:posix-getenv "EXTRA_CFLAGS")
                                  " " "-fPIC"
                                  (format nil "~{-L~A~^ ~}" (link-library-directories c))
                                  (format nil "~{-l~A~^ ~}" (link-libraries c))
                                  (format nil " ~{-isystem ~A~^ ~} ~{-I~A~^ ~} "
                                          (mapcar #'unix-name (system-include-directories c))
                                          (mapcar #'unix-name (include-directories c))))
                                 (unix-name (car (output-files op c)))
                                 (unix-name (component-pathname c))))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-shared-object filename))))

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
