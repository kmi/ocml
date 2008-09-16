;;; Copyright © 2007,2008 The Open University.

(in-package :ocml)

(defvar *this-instance-number*)

(defvar *root-node*)

(defvar *ht*)

(defvar *ht-code*); a table of tables of code

(defmethod translate ((src (eql :xml)) (dst (eql :ocml)) (thing t)
                      &key &allow-other-keys)
  (let ((*ht* (make-hash-table :test 'equal))
        (*ht-code* (make-hash-table :test 'equal)))
    (generate-instances-from-xml thing)))

(defun dom-children (node)
  "Get children of NODE as a list."
  (let ((children '()))
    (dom:do-node-list (child (dom:child-nodes node))
      (push child children))
    (nreverse children)))

(defun process-node (node &optional parent-class parent-inst-number)
  ;; for *root-node* don't need parent-class and parent-inst-number
  (if (dom:element-p node)
      (let ((children (dom-children node))
            (node-class (dom:local-name node)))
	(cond					
	  ((not children)     ; if the node is empty, i.e. no children
	   (if (not (equalp *root-node* node)) 
	       (process-empty-node node parent-class parent-inst-number)
	       ;; else process root-node as empty node here
	       (let ((attr (dom:attributes node)))
		 (setf (gethash node-class *ht*) (make-hash-table :test 'equal))
		 ;; add an entry into its table as a list of value pairs
		 (setf (gethash 0 (gethash node-class *ht*)) (process-attributes attr)))))
	  ;; if the node has one child of type CONS, containing the
	  ;; value of the slot; so no new instance to be added to *ht*
	  ((not (cdr children))
	   (setf (gethash parent-inst-number (gethash parent-class *ht*))
		 (append (gethash parent-inst-number (gethash parent-class *ht*))
			 (list (list node-class ;; XXX (car children)
                                     (get-first-text-child node))))))
	  ;; if the node has children
	  ((dom:element-p (cadr children)) 
	   (when (not (gethash node-class *ht*)) 
	     ;; if no node-class entry exists in *ht*, create it now
	     ;; for the first time as a hash-table itself
	     (setf (gethash node-class *ht*) (make-hash-table :test 'equal)))
	   (let ((*this-instance-number* (hash-table-count (gethash node-class *ht*))))
	     (setf (gethash *this-instance-number* (gethash node-class *ht*)) '())
	     (when (not (equalp node *root-node*))
	       (push (list node-class *this-instance-number*)
		     (gethash parent-inst-number (gethash parent-class *ht*))))
	     (dolist (child children) 
	       (process-node child node-class *this-instance-number*))))))))

(defun process-empty-node (node parent-class parent-inst-number)
  ;; an empty node is a node without an end tag (i.e. with no
  ;; children). It must have attributes, so it is an instance itself
  ;; parent-table is the table of the parent where the new entry will
  ;; be added instance-number is the index of that entry, i.e. the
  ;; number of the parent instance
  (let* ((node-class (dom:local-name node))
         (attr (dom:attributes node)))
    (if (not (gethash node-class *ht*)) 
        ;; if no node-class entry exists in *ht*, create it now
        ;; for the first time as a hash-table itself
        (setf (gethash node-class *ht*) (make-hash-table :test 'equal)))
    (setf *this-instance-number* (hash-table-count (gethash node-class *ht*)))
    (setf (gethash *this-instance-number* (gethash node-class *ht*))
          (process-attributes attr))
    ;; add a node-class entry to the parent-table; if parent-table
    ;; is *ht*, then don't add anything assume parent-table is
    ;; already created
    (setf (gethash parent-inst-number (gethash parent-class *ht*)) 
          (append (gethash parent-inst-number (gethash parent-class *ht*)) (list (list node-class *this-instance-number*))))))

; process-empty-node wont' be called for *root-node* !!!!
; if a node has children, then create its hashtable before calling dolist to process the children. 

(defun get-first-text-child (element)
  (let* ((children (dom-children element))
         (text (first children)))
    (if (and (= 1 (length children))
             (member (type-of text)
                     '(rune-dom::text rune-dom::cdata-section)))
        (dom:node-value text)
        nil)))

(defun process-attributes (attributes)
  (mapcar #'(lambda (a)
              (list (dom:local-name a) (get-first-text-child a)))
          (dom::items attributes)))

(defun create-hash-table-code ()
  (maphash #'create-entries-hash-table-code *ht*)
  (apply #'concatenate 'list (maphash* #'create-class-instances-code *ht*)))

(defun create-entries-hash-table-code (key value)
  (declare (ignore value))
  (setf (gethash key *ht-code*) (make-hash-table :test 'equal)))

(defun create-class-instances-code (class-name class-table)
  (maphash* #'(lambda (key value)
		(list 'def-instance (intern (format nil "~A~A" (get-symbol-from-string class-name)
						    (get-symbol-from-string key))  :ocml) ;; key is the instance number
		      (get-symbol-from-string class-name)
		      (get-code-from-slot-list value)))
	    class-table))

(defun maphash* (function hashtable)
  (with-hash-table-iterator (next-entry hashtable)
    (let ((result '()))
      (loop (multiple-value-bind (more? key value) (next-entry)
	      (unless more? (return (reverse result)))
	      (push (funcall function key value) result))))))

(defun get-code-from-slot-list (slot-list)
  (mapcar #'(lambda (slot)
              (list (get-symbol-from-string (car slot)) :value
                    (if (gethash (car slot) *ht*)
			(intern (format nil "~A~A" (return-symbol-if-class (car slot))
					(return-symbol-if-class (cadr slot))))
                        (return-symbol-if-class (cadr slot)))))
          slot-list))

(defun return-symbol-if-class (string-value)
  (if (gethash string-value *ht*)
      (get-symbol-from-string string-value)
      string-value))

(defun get-symbol-from-string (x)
  (when x
    (let ((*package* (find-package "OCML")))
      (handler-case (read-from-string x)
        (error (c) x)
        (serious-condition (c) x)))))

(defun generate-instances-from-xml (xmldoc)
  (setq *root-node*
        ;; Depending on the version of CXML in use, we may have access
        ;; to string-to-octets from flexi-streams (older CXML), or
        ;; babel (newer CXML).
        (first (dom-children
                (cxml:parse
                 #+:flexi-streams (flexi-streams:string-to-octets xmldoc)
                 #-:flexi-streams (babel:string-to-octets xmldoc)
                 (cxml-dom:make-dom-builder)))))
  (process-node *root-node* 'topclass)
  (create-hash-table-code))
