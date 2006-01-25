;;
;; file: ch-asdf.cl
;; author: cyrus harmon
;;

(in-package :ch-asdf)

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
                           (format nil " ~{-Xlinker -rpath -Xlinker ~A~^ ~} " (link-library-directories dso))
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


;;; generated source files

(defclass generated-source-file (source-file) ())
(defmethod operation-done-p ((o operation) (c generated-source-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

;;; pdf files

(defclass pdf-file (source-file) ())
(defmethod source-file-type ((c pdf-file) (s module)) "pdf")

;;; xhtml files

(defclass xhtml-file (html-file) ())
(defmethod source-file-type ((c xhtml-file) (s module)) "xhtml")

;;; tiff files

(defclass tiff-file (static-file) ())
(defmethod source-file-type ((c tiff-file) (s module)) "tiff")

;;; jpeg files

(defclass jpeg-file (static-file) ())
(defmethod source-file-type ((c jpeg-file) (s module)) "jpg")

;;; markup files

(defclass markup-file (source-file) ())
(defmethod source-file-type ((c markup-file) (s module)) "gmarkup")

(defclass markup-latex-file (generated-source-file) ())
(defmethod source-file-type ((c markup-latex-file) (s module)) "tex")

(defclass markup-pdf-file (pdf-file generated-source-file) ())
(defclass markup-xhtml-file (xhtml-file) ())
