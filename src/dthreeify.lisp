;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


(defun dthreeify-ontolingua-ontology (ontology-name)
  (let ((ontology (get-ontology ontology-name)))
    (cond (ontology
           (let* ((load-file (default-ontology-load-file   
                               ontology-name 
                               (ontology-type ontology)))
                  (target-directory (make-pathname :directory (concatenate 'string 
                                                                           (directory-namestring load-file)
                                                                           *default-ontolingua-sub-directory*)
                                                   :host (pathname-host load-file))))
             (dthreeify-ontology ontology-name ontology
                                 (ontolingua-pathname load-file target-directory)
                                 (mapcar #'(lambda (file)
                                             (ontolingua-pathname
                                              (merge-pathnames 
                                               (ontology-pathname ontology)
                                               (make-pathname :name file
                                                              :type *lisp-suffix*))
                                              target-directory))
                                         (ontology-files ontology))
                                 'ontolingua
                                 target-directory)))
          
          (t
           (error "~s is not a known ontology" ontology-name)))))

(defun ontolingua-pathname (source-pathname target-directory)
  (make-pathname   :host (pathname-host source-pathname)
                   :directory (pathname-directory target-directory)
                   :name (Pathname-name source-pathname)
                   :type *ontolingua-suffix*))


(defun dthreeify-ocml-ontology (ontology-name)
  (let ((ontology (get-ontology ontology-name)))
    (cond (ontology
           (dthreeify-ontology
            ontology-name ontology
            (default-ontology-load-file   
              ontology-name 
              (ontology-type ontology))
            (mapcar #'(lambda (file)
                        (merge-pathnames 
                         (ontology-pathname ontology)
                         (make-pathname :name file
                                        :type *lisp-suffix*)))
                    (ontology-files ontology))
            'ocml))
          
          (t
           (error "~s is not a known ontology" ontology-name)))))

(defun dthreeify-ontology (ontology-name ontology load-file files &optional (type 'ocml) (directory (directory-namestring   load-file)))
  (let* ((max-length (- 26 (length (format nil "~s" type))))
         (s-name (format nil "~s" ontology-name))
        (t-name (if (< (length s-name) max-length)
                  s-name
                  (subseq s-name 0 (- max-length 1))))
        (target-pathname (make-pathname   :host (pathname-host load-file)
                                          :directory (pathname-directory directory)
                                          :name (format nil "~a-~s"
                                                        t-name type)
                                          :type "html")))
     
     (with-open-file (ofile target-pathname
                             :if-exists :supersede
                             :direction :output)
       (format ofile
                "<HTML>~%<HEAD>~%<TITLE>~s v~s - ~s</TITLE>~%</HEAD>~%<BODY BGCOLOR=\"#FFFFFF\">~%<pre>~%"
                ontology-name (ontology-version-number ontology) type)
       (loop for file in (cons load-file files)
              do
              (dthreeify-ontology-file ontology-name file ofile (def-string-from-type type)
                                       ))
        (format ofile "~%</pre>~%</body>~%</html>"))))

(defun def-string-from-type (type)
  (case type 
    (ocml "(DEF-")
    (ontolingua "(DEFINE-")
    (t (error "Unknown ontology type ~s" type))))
    
(defun dthreeify-ontology-file  (ontology-name file ofile def-string &aux (length (length def-string)))
  (with-open-file (ifile file
                         :direction :input)
    (format ofile "~%</pre>~2%<H1>~:@(~a~)</H1>~%<pre>~%"
            (pathname-name file))
    (loop ;;;;;with l = (length def-string)
          for line = (read-line ifile nil :end-of-file)
          until (eq line :end-of-file)
          do
         ;;(print line)
          (cond ((and (> (length line) length)
                      (string-equal  def-string (subseq line 0 length)))
                 (Let* ((index1 (position  #\space line))
                        (type (read-from-string (subseq line length)))
                        (name (read-from-string (subseq line (+ 1 index1 )))))
                   (format ofile "~%</pre>~2%<H2><A HREF=\"http://eldora.open.ac.uk:3000/webonto?ontology=~s?type=~s?name=~s\">"
                            ontology-name type name)
                    (format ofile "<IMG SRC=\"../../img/webonto.jpg\" ALIGN=\"middle\" BORDER=0></A> ")
                    (format ofile "~(~s~) ~s</H2>~%<pre>" type name)
                   (format ofile "~%~a" line)))                 
                (t
                 (format ofile "~%~a" line))))))

        



;;(Let* ((index1 (Print (position  #\space line)))
;;                        (index2 (or (position #\space (subseq line (+ 1 index1 )))
;;                                   (length (subseq line (+ 1 index1))))))
;;                   (format ofile "<H3>~a</H3>~%" 
;;                                (subseq line (+ 1 index1) (+ index1 index2)))
;;                   (format ofile "~%~a" line))
;;