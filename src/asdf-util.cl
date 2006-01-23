;;
;; file: asdf-util.cl
;; author: cyrus harmon
;;

(in-package :ch-asdf)

(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defun absolute-path-p (path)
  (when (listp (pathname-directory path))
    (eql (car (car (pathname-directory path)))
         :absolute)))

(defun asdf-lookup (path)
  (cond ((and path (listp path))
         (reduce #'asdf:find-component (cdr path)
                 :initial-value (asdf:find-system (car path))))
        ((stringp path)
         (let ((uri (puri:parse-uri path)))
           (when uri
             (let ((scheme (puri:uri-scheme uri)))
               (when (and (or (null scheme)
                              (eql scheme :asdf))
                          (puri:uri-parsed-path uri))
                 (asdf-lookup (cdr (puri:uri-parsed-path uri))))))))))
