(in-package #:ocml)

(defun ocml-warn (string &rest format-args)
  (apply #'warn string format-args))

(defun ocml-output (string &rest format-args)
  (apply #'format t  string format-args))

(defun get-source-pathname (dspec)
  #+:lispworks-dspec
  (cadar (dspec:find-dspec-locations dspec)))

(defun ocml-record-source-file (name type &optional ontology)
  #+:sbcl (declare (ignore name type))
  (when (and (null ontology)
             *ocml-initialized*)
    (setf ontology (name *current-ontology*)))
  #+:mcl (CCL:RECORD-SOURCE-FILE name type)
  #+(and :lispworks (not :lispworks-dspec))
  (eval `(lw::top-level-form (,type ,name ,ontology) nil))
  #+:lispworks-dspec
  (lispworks:record-definition (list type name ontology)
			       (lispworks::current-pathname)
			       :check-redefinition-p nil)
  #+:allegro(cl-user::record-source-file name :type type))

(defun ocml-record-instance-source-file (name parent type &optional ontology)
  #+:sbcl (declare (ignore name parent type))
  (when (and (null ontology)
             *ocml-initialized*)
    (setf ontology (name *current-ontology*)))
  #+:mcl (CCL:RECORD-SOURCE-FILE name type)
  #+(and :lispworks (not :lispworks-dspec))
  (eval `(lw::top-level-form (,type ,name ,parent ,ontology) nil))
  #+:lispworks-dspec
  (lispworks:record-definition (list type name parent ontology)
			       (lispworks::current-pathname)
			       :check-redefinition-p nil)
  #+:allegro(cl-user::record-source-file name :type type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 #+:lispworks(eval-when (eval load)
               ;;;;;;;;;;(editor::setup-indent 'def-role 1 4 4)
               (editor::setup-indent 'def-rule 1 4 4))

#+:lispworks-dspec
(dspec:define-dspec-class def-class nil "OCML Def-class"
  :pretty-name "Def-class"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-class ,name ,ontology)))
  :definedp #'(lambda (name) (get-domain-class name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-ontology nil "OCML Def-ontology"
  :pretty-name "Def-ontology"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-ontology ,name ,ontology)))
  :definedp #'(lambda (name) (get-ontology name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-instance nil "OCML Def-instance"
  :pretty-name "Def-instance"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (parent (third dspec))
                          (ontology (fourth dspec)))
                      `(def-instance ,name ,parent ,ontology)))
  :definedp #'(lambda (name) (find-current-instance name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-relation nil "OCML Def-relation"
  :pretty-name "Def-relation"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-relation ,name ,ontology)))
  :definedp #'(lambda (name) (get-relation name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-function nil "OCML Def-function"
  :pretty-name "Def-function"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-function ,name ,ontology)))
  :definedp #'(lambda (name) (get-function name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-procedure nil "OCML Def-procedure"
  :pretty-name "Def-procedure"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-procedure ,name ,ontology)))
  :definedp #'(lambda (name) (get-function name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-axiom nil "OCML Def-axiom"
  :pretty-name "Def-axiom"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-axiom ,name ,ontology)))
  :definedp #'(lambda (name) (get-axiom name)))

#+:lispworks-dspec
(dspec:define-dspec-class def-rule nil "OCML Def-rule"
  :pretty-name "Def-Rule"
  :canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(def-rule ,name ,ontology)))
  :definedp #'(lambda (name) (get-rule name)))


#+:lispworks-dspec
(dspec:define-dspec-class def-relation-instances nil
  "OCML def-relation-instances"
  :pretty-name "def-relation-instances"
  :canonicalize #'(lambda (dspec)
		    (let ((name (second dspec))
                          (ontology (third dspec)))
                      `(def-relation-instances ,name ,ontology)))
  :definedp #'(lambda (x) (and (listp x) (get-relation-instance x))))

;;; {{{ Ontology file search and loading

(defun setup-ontology-path ()
  (dolist (type +ontology-types+)
    (push (logical-pathname
           (format nil "~A~A;" ocml::*library-pathname* type))
          *ontology-path*))
  (push *web-library-pathname* *ontology-path*))

(defun load-ontology-by-name (ontology-name)
  "Search for and load the ontology called ONTOLOGY-NAME."
  (let ((dir (find-ontology-directory ontology-name)))
    (if dir
        (load (format nil "~A~A" dir *load-filename*))
        (error "Cannot find ontology called ~A." ontology-name))))

#+:ocml-with-drakma
(defun download-ontology-by-url (url)
  "URL is the directory containing the ontology, with trailing '/'."
  ;; Idea here is to grab the load file, figure out the ontology name
  ;; and files, and then create a local copy of the load.lisp and
  ;; those files.
  (multiple-value-bind (content response-code)
      (drakma:http-request (format nil "~A~A" url *load-filename*))
    (unless (= 200 response-code)
      (error "Cannot read from URL ~A." url))
    (let* ((*package* (find-package :ocml))
           (offset 0)
           def-ontology-form)
      (loop for (form read) = (multiple-value-list (read-from-string content t nil :start offset))
         do (setf offset (+ offset read))
           (when (eq (first form) 'ocml::def-ontology)
             (setf def-ontology-form form)
             (return)))
      (let ((ontology-name (second def-ontology-form))
            (files (second (member :files def-ontology-form))))
        (download-ontology url (string-downcase ontology-name) files)))))

#+:ocml-with-drakma
(defun download-ontology (url ontology-name files)
  (create-directory (format nil "~A~A;" *web-library-pathname* ontology-name))
  (dolist (file (cons *load-filename*
                      (mapcar #'(lambda (f)
                                  (format nil "~A.~A" f *lisp-suffix*))
                              files)))
    (download-file (format nil "~A~A" url file)
                   (format nil "~A~A;~A" *web-library-pathname* ontology-name file))))

#+:ocml-with-drakma
(defun download-file (url filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (multiple-value-bind (content response-code)
        (drakma:http-request url)
      (when (not (= 200 response-code))
        (error "Unable to get URL ~A." url))
      (write-sequence content stream)
      (values))))

(ocml::define-constant +directory-separator+
    #+:win32 #\\
    #+:unix #\/)

;; We have to return the true pathname here, because it contains case
;; information that would be lost by converting to a logical pathname.
(defun find-ontology-directory (ontology-name)
  "Return true pathname of the directory holding ONTOLOGY-NAME files
if it can be found, or NIL."
   (dolist (path *ontology-path*)
     (let ((dir (find-truename-ci path ontology-name)))
       (when dir
         (return dir)))))

(defun find-truename-ci (path ext)
  "Return the local name of EXT in directory in PATH where the local
  matches EXT, ignoring case."
  (let* ((paths (directory
                 #-:clisp (merge-pathnames path "*")
                 #+:clisp (logical-pathname (format nil "~A**;" path))))
         (finds (remove-if #'(lambda (path)
                               (not (string-equal ext (file-basename path))))
                           paths))
         (n (length finds)))
    (cond ((= 0 n)
           nil)
          ((= 1 n)
	   (format nil "~A" (first finds)))
          (t
           (error "Ambiguous directory or filename for ~S in directory ~A." ext path)))))

(defun file-basename (path)
  (let* ((string (format nil "~A" path))
         (base (subseq string (position +directory-separator+
                                        (string-right-trim (list +directory-separator+) string)
                                        :from-end t))))
    (string-trim (list +directory-separator+) base)))

;;; }}}

(defun rm-file (name)
  "Remove file NAME."
  (when (probe-file name)
    (delete-file name)))
