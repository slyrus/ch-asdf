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

(defclass ch-lisp-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod output-files :around ((operation compile-op) (c ch-lisp-source-file))
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
                           #-darwin
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

(defmethod perform ((op load-op) (c c-source-file)))

(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-shared-object filename))))


;;; generated source files

(defclass generated-file (source-file) ())

(defclass generated-source-file (generated-file) ())
(defmethod operation-done-p ((o operation) (c generated-source-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

(defmethod source-file-type ((c generated-file) (s module)) "")

(defmethod perform ((op compile-op) (c generated-file)))

(defmethod perform ((op load-op) (c generated-file)))

;;; pdf files

(defclass pdf-file (source-file) ())
(defmethod source-file-type ((c pdf-file) (s module)) "pdf")

;;; css files

(defclass css-file (static-file) ())
(defmethod source-file-type ((c css-file) (s module)) "css")

;;; xhtml files

(defclass xhtml-file (html-file) ())
(defmethod source-file-type ((c xhtml-file) (s module)) "xhtml")

;;; tiff files

(defclass tiff-file (static-file) ())
(defmethod source-file-type ((c tiff-file) (s module)) "tiff")

;;; jpeg files

(defclass jpeg-file (static-file) ())
(defmethod source-file-type ((c jpeg-file) (s module)) "jpg")

;;; png files

(defclass png-file (static-file) ())
(defmethod source-file-type ((c png-file) (s module)) "png")

;;; markup files

(defclass markup-file (source-file) ())
(defmethod source-file-type ((c markup-file) (s module)) "gmarkup")

(defclass markup-latex-file (generated-source-file) ())
(defmethod source-file-type ((c markup-latex-file) (s module)) "tex")

(defclass markup-pdf-file (pdf-file generated-source-file) ())
(defclass markup-xhtml-file (xhtml-file) ())

;;; tinaa documentation

(defclass tinaa-directory (module) ())

;;; Need a generic ASDF object that reads a file and associates an
;;; in-memory object with the file. It should cache the creation date
;;; of the object and reload the object if the modification date of
;;; the file is newer than the creation date of the in-memory object.

(defclass object-component (source-file)
  ((symbol :accessor object-symbol :initarg :symbol)))

(defmethod source-file-type ((c object-component) (s module)) nil)

(defun make-symbol-from-name (name)
  (intern (string (read-from-string name))))

(defmethod shared-initialize :after ((c object-component) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignore force))
  (when (slot-boundp c 'asdf::name)
    (unless (slot-boundp c 'symbol)
      (setf (object-symbol c)
            (make-symbol-from-name (asdf::component-name c))))))

(defmethod perform ((op compile-op) (c object-component)))
(defmethod perform ((op load-op) (c object-component))
  (setf (component-property c 'last-loaded)
        (get-universal-time)))

;;; An object-from-file is the file-based representation of an object. The
;;; load-op 
(defclass object-from-file (object-component)
  ((load-date :accessor object-load-date :initarg :load-date)))

(defmethod perform ((op compile-op) (c object-from-file)))

(defmethod perform ((op load-op) (c object-from-file))
  (with-open-file (input-stream (component-pathname c))
    (setf (symbol-value (object-symbol c))
          (read input-stream)))
  (call-next-method))

(defclass object-to-file (object-component)
  ((write-date :accessor object-write-date :initarg :write-date)))



(defclass object-from-variable (ch-asdf:object-component)
  ((input-object :accessor object-input-object :initarg :input-object)))

(defmethod operation-done-p ((o compile-op) (c object-from-variable))
  t)
(defmethod operation-done-p ((o load-op) (c object-from-variable))
  (let ((input-object-last-load-time
         (asdf::component-property
          (find-component (component-parent c)
                          (asdf::coerce-name (object-input-object c)))
          'ch-asdf::last-loaded))
        (my-last-load-time (asdf::component-property c 'ch-asdf::last-loaded)))
    (and input-object-last-load-time
         my-last-load-time
         (> my-last-load-time input-object-last-load-time))))

(defmethod perform ((op compile-op) (c object-from-variable)))
(defmethod perform ((op load-op) (c object-from-variable))
  (let ((sexp
         (symbol-value
          (ch-asdf::object-symbol
           (find-component (component-parent c)
                           (asdf::coerce-name (object-input-object c)))))))
    (setf (symbol-value (ch-asdf::object-symbol c)) sexp))
  (call-next-method))


;;; quote macro reader

(defun get-delimiter (char)
  (case char
    (#\{ #\})
    (#\( #\))
    (#\[ #\])
    (#\< #\>)
    (t char)))

(defun enable-quote-reader-macro ()
  (set-dispatch-macro-character #\# #\q 
                                #'(lambda (in c n)
                                    (declare (ignore c n))
                                    (let ((delimiter (get-delimiter (read-char in))))
                                      (let ((string (make-array '(0) :element-type 'character
                                                                :fill-pointer 0 :adjustable t)))
                                        (with-output-to-string (string-stream string)
                                          (loop for char = (read-char in nil)
                                             while (and char (not (char-equal char delimiter)))
                                             do
                                             (princ char string-stream)))
                                        string)))))
