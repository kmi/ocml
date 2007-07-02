
(in-package :ocml)

(defun process-node (node &optional parent-class parent-inst-number)
; for *root-node* don't need parent-class and parent-inst-number

  (if (typep node 'xqdm:elem-node)
      
      (let ((children (xmlp::children node))
            (node-class (xmlp::local-part node)))

       (cond
         ; if the node is empty, i.e. no children
         ((not children) 
            (progn
               
               (if (not (equalp *root-node* node)) 

                  (process-empty-node node parent-class parent-inst-number)
                  ; else process root-node as empty node here
                  (let ((attr (xmlp::attributes node)))
                    (setf (gethash node-class *ht*) (make-hash-table :test 'equal))
                     ; add an entry into its table as a list of value pairs
                    (setf (gethash 0 (gethash node-class *ht*)) (process-attributes attr))))))

         ; if the node has one child of type CONS, containing the value of the slot; so no new instance to be added to *ht*
         ((not (cdr children))
             (setf (gethash parent-inst-number (gethash parent-class *ht*)) (append (gethash parent-inst-number (gethash parent-class *ht*)) (list (list node-class (car children))))))

         ; if the node has children
         ((typep (cadr children) 'xqdm:elem-node) 
          (progn

            (format t "~%~%PN call starts for: Node ~a Parent-node ~a PIN ~d " node-class parent-class parent-inst-number)
            (if (not (gethash node-class *ht*)) 
                ; if no node-class entry exists in *ht*, create it now for the first time as a hash-table itself
                 (setf (gethash node-class *ht*) (make-hash-table :test 'equal)))
     
            (let ((this-instance-number (hash-table-count (gethash node-class *ht*))))
            (setf (gethash this-instance-number (gethash node-class *ht*)) '())

            (if (not (equalp node *root-node*))
                (setf (gethash parent-inst-number (gethash parent-class *ht*))
                  (append (gethash parent-inst-number (gethash parent-class *ht*)) (list (list node-class this-instance-number)))))

            (dolist (child children) 
                (process-node child node-class this-instance-number)))

            (format t "~%~%PN call finished for: Node ~a Parent-node ~a PIN ~d TIN ~d " node-class parent-class parent-inst-number this-instance-number)
 ))))))
    


(defun process-empty-node (node parent-class parent-inst-number)
; an empty node is a node without an end tag (i.e. with no children). It must have attributes, so it is an instance itself
; parent-table is the table of the parent where the new entry will be added
; instance-number is the index of that entry, i.e. the number of the parent instance

  (if (typep node 'xqdm:elem-node)
      
      (let* ((node-class (xmlp::local-part node))
            (attr (xmlp::attributes node)))
           
        ;(format t "~%~%PEN call starts for: Node ~a Parent-node ~a PIN ~d " node-class parent-class parent-inst-number) 
        (if (not (gethash node-class *ht*)) 
                ; if no node-class entry exists in *ht*, create it now for the first time as a hash-table itself
                (setf (gethash node-class *ht*) (make-hash-table :test 'equal)))
            
         (setf this-instance-number (hash-table-count (gethash node-class *ht*)))
         (setf (gethash this-instance-number (gethash node-class *ht*)) (process-attributes attr))

            ; add a node-class entry to the parent-table; if parent-table is *ht*, then don't add anything
            ; assume parent-table is already created
         
         (setf (gethash parent-inst-number (gethash parent-class *ht*)) 
                  (append (gethash parent-inst-number (gethash parent-class *ht*)) (list (list node-class this-instance-number))))
         ;(format t "~%~%PEN call finished for: Node ~a Parent-node ~a PIN ~d TIN ~d " node-class parent-class parent-inst-number this-instance-number)
)))




; process-empty-node wont' be called for *root-node* !!!!
; if a node has children, then create its hashtable before calling dolist to process the children. 


;(defparameter *instance-counter* 0)
(defparameter *ht* (make-hash-table :test 'equal))

(defun process-attributes (attributes)
  (let ((slot-list nil))
    (dolist (a attributes)
      (setf slot-list (append slot-list (list (list (xmlp::local-part a) (xmlp::value a))))))
     slot-list))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(gethash "a" *ht*)
;(gethash 0 (gethash "a" *ht*))
;(gethash 0 (gethash "b" *ht*)); ((c 0) (c 1))
;(gethash 0 (gethash "c" *ht*)); ((d 0); (d 1))
;(gethash 1 (gethash "b" *ht*)); nil
;(gethash 0 (gethash "d" *ht*)); ((d1 v1))
;(gethash 1 (gethash "d" *ht*)); ((d1 v2))
;(gethash 1 (gethash "c" *ht*)); ((d 2))
;(gethash 2 (gethash "d" *ht*)); ((d1 v3))


(defparameter *ht-code* (make-hash-table :test 'equal)); a table of tables of code


(defun create-hash-table-code ()
  (maphash #'create-entries-hash-table-code *ht*)
  (maphash #'create-class-instances-code *ht*)
  (maphash #'add-slots-code *ht*) 
) 

(defun create-entries-hash-table-code (key value)    
  (setf (gethash key *ht-code*) (make-hash-table :test 'equal)))


(defun create-class-instances-code (class-name class-table)   
  (maphash #'(lambda (key value) 
               (setf (gethash key (gethash class-name *ht-code*)) (format nil "~%(def-instance ~s~s ~s (" (get-symbol-from-string class-name) (get-symbol-from-string key) (get-symbol-from-string class-name)))) 
               class-table))


(defun add-slots-code (class-name class-table)
  (maphash #'(lambda (key value)
               (setf (gethash key (gethash class-name *ht-code*)) (concatenate 'string (gethash key (gethash class-name *ht-code*)) 
                                                                               (get-code-from-slot-list "" value))))
               class-table))


(defun get-code-from-slot-list (code slot-list)
 (dolist (slot slot-list)
   (if (gethash (car slot)  *ht*)
       (setf code (concatenate 'string code (format nil "~%(~s :value ~s~s)" (get-symbol-from-string (car slot)) (return-symbol-if-class (car slot)) (return-symbol-if-class (cadr slot)))))
       (setf code (concatenate 'string code (format nil "~%(~s :value ~s)" (get-symbol-from-string (car slot)) (return-symbol-if-class (cadr slot)))))))
 code)

(defun return-symbol-if-class (string-value)
  (if (gethash string-value *ht*)
      (get-symbol-from-string string-value)
      string-value))

;(create-hash-table-code)

;(gethash 0 (gethash "a" *ht-code*))
;(gethash 0 (gethash "b" *ht-code*))
;(gethash 0 (gethash "c" *ht-code*))
;(gethash 1 (gethash "c" *ht-code*))
;(gethash 0 (gethash "d" *ht-code*))
;(gethash 1 (gethash "d" *ht-code*))
;(gethash 2 (gethash "d" *ht-code*))

;(gethash "a" *ht-code*)



(defun get-symbol-from-string (x)
  (when x
    (let ((*package* (find-package "OCML")))
      (handler-case (read-from-string x)
        (error (c) x)
        (serious-condition (c) x)))))

(get-symbol-from-string "a")
 
(defun final-code-conversion ()
 (maphash #'(lambda (key value)
              (maphash #'(lambda (key1 value1)
                           (setf all-instances-code (concatenate 'string all-instances-code value1 (format nil "~%") )))
                         value)
              (setf all-instances-code (concatenate 'string all-instances-code (format nil "))~%"))))
          *ht-code*)
)



(defun generate-instances-from-xml (xmldoc)
  (setq *root-node* (xmlp::root (xmlp::document-parser xmldoc)))
  (process-node *root-node* 'topclass)
  (create-hash-table-code)
  (final-code-conversion)
  
)

(defun store-source-code (ontology source-code author password &optional uses (type :goal) (format 'ocml) allowed-editors
                                   (upload-mode 'ocml::new) (stream *standard-output*))
  (ip::internal-upload ontology uses type format source-code upload-mode author password allowed-editors stream))


(defun process-xml (xmldoc)
  (defparameter *ht* (make-hash-table :test 'equal))
  (defparameter *ht-code* (make-hash-table :test 'equal))
  (defparameter all-instances-code "")
  (generate-instances-from-xml xmldoc)
  all-instances-code
)


; to call in this order:

(defparameter *example*
 "<a>
     <b>
       <c>
         <d d1=\"v1\"/>
         <d d1=\"v2\"/>
       </c>
       <c>
         <d d1=\"v3\"/>
       </c>
     </b>
  </a>")

(process-xml *example*)


