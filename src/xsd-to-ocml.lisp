


(in-package :ocml)





;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;; Tests if a certain attribute exists in the list of attributes of a node; returns T or nil.

(defun is-name (attr)
  (string= "name" (remove-prefix (xmlp::local-part attr))))

(defun is-type (attr)
  (string= "type" (remove-prefix (xmlp::local-part attr))))

(defun is-default (attr)
  (string= "default" (remove-prefix (xmlp::local-part attr))))

(defun is-fixed (attr)
  (string= "fixed" (remove-prefix (xmlp::local-part attr))))

(defun is-use (attr)
  (string= "use" (remove-prefix (xmlp::local-part attr))))

(defun is-ref (attr)
  (string= "ref" (remove-prefix (xmlp::local-part attr))))

(defun is-minOccurs (attr)
  (string= "minOccurs" (remove-prefix (xmlp::local-part attr))))

(defun is-maxOccurs (attr)
  (string= "maxOccurs" (remove-prefix (xmlp::local-part attr))))

(defun is-base (attr)
  (string= "base" (remove-prefix (xmlp::local-part attr))))

(defun is-targetNamespace (attr)
  (string= "targetNamespace" (remove-prefix (xmlp::local-part attr))))

(defun is-namespace (attr)
  (string= "namespace" (remove-prefix (xmlp::local-part attr))))

(defun is-attribute (attr attrlist)
	(cond 
		((string= "name" attr) (member-if #'is-name attrlist))
		((string= "type" attr) (member-if #'is-type attrlist))
         	((string= "default" attr) (member-if #'is-default attrlist))
	        ((string= "fixed" attr) (member-if #'is-fixed attrlist))
	        ((string= "use" attr) (member-if #'is-use attrlist))
	        ((string= "ref" attr) (member-if #'is-ref attrlist))
                ((string= "minOccurs" attr) (member-if #'is-minOccurs attrlist))
		((string= "maxOccurs" attr) (member-if #'is-maxOccurs attrlist))
                ((string= "base" attr) (member-if #'is-base attrlist))
                ((string= "targetNamespace" attr) (member-if #'is-targetNamespace attrlist))
                ((string= "namespace" attr) (member-if #'is-namespace attrlist))))

(defun test-complex (node)
  (and 
    (typep node 'xqdm:elem-node)
    (string= "element" (remove-prefix (xmlp::local-part node)))
    (string= "complexType" (remove-prefix (xmlp::local-part (second (xmlp::children node)))))))

(defun test-element (node)
  (and
   (typep node 'xqdm:elem-node)
   (string= "element" (remove-prefix (xmlp::local-part node)))))


(defun get-complex-children (node)
  (setq cpx-nodes (loop for child in (xmlp::children node) when (test-complex child) collect child)))


(defun get-all-children (node)
;;; children of type <element>
  (setq children (loop for child in (xmlp::children node) when (test-element child) collect child)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Node processing functions

(defun process-empty-node (node)
   ;; A node which has no children. If it's an <attribute> or an <element> it becomes an ocml slot of the parent
   
   (if (or (string= "element" (remove-prefix (xmlp::local-part node))) (string= "attribute" (remove-prefix (xmlp::local-part node))))
   
   ;; if non-empty list of attributes:
   (if (xmlp::attributes node)
               
       (progn 
       (setq slot ())
       
       ;;; if there is a 'ref'
       (if (setq ref-cons (car (is-attribute "ref" (xmlp::attributes node))))
		;;T
                (if (setq ref-node (find-node-named (xmlp::value ref-cons) *root-node*))
                    ;; if the node with the name referenced here was found in the overall doc; i look for it here because if it is in the current 
                    ;; doc and of a built-in type we need to add to specify the built-in type of the currently generated slot; if it is of built-in
                    ;; type but in another doc then we can't do anything about it here!
                    
                    ;; create the slot with type
                    (setf (getf slot ':slot-name) (get-class-part-value ref-cons) (getf slot ':type) (get-type ref-node))

                    ;; otherwise create the slot only with name and no type
                    (setf (getf slot ':slot-name) (get-class-part-value ref-cons) (getf slot ':type) (get-symbol-from-string (remove-prefix (xmlp::value ref-cons))))))
       
      ;;; if there is a 'name' 
       (if (setq name-cons (car (is-attribute "name" (xmlp::attributes node))))
           (setf (getf slot ':slot-name) (get-class-part-value name-cons)))
       
       ;;; if there is a 'type'
       (if (setq type-cons (car (is-attribute "type" (xmlp::attributes node))))
           (setf (getf slot ':type) (get-symbol-from-string (get-ocml-type (remove-prefix (xmlp::value type-cons))))))

       ;;; if there is a 'default'
       (if (setq default-cons (car (is-attribute "default" (xmlp::attributes node))))
           (setf (getf slot ':default-value) (get-class-part-value default-cons)))

       ;;; if there is a 'fixed'
       (if (setq fixed-cons (car (is-attribute "fixed" (xmlp::attributes node))))
           (setf (getf slot ':value) (get-class-part-value fixed-cons)))

       ;;; if there is a 'use'
       (if (setq use-cons (car (is-attribute "use" (xmlp::attributes node))))
           (if (string= "optional" (remove-prefix (xmlp::value use-cons)))
               (setf (getf slot ':min-cardinality) 0)

		(if (string= "required" (remove-prefix (xmlp::value use-cons)))
                    (setf (getf slot ':min-cardinality) 1))))

       ;;; if there is a 'minOccurs' and a 'maxOccurs'
       (if (setq min-card-cons (car (is-attribute "minOccurs" (xmlp::attributes node))))
           (if (setq max-card-cons (car (is-attribute "maxOccurs" (xmlp::attributes node))))
               (if (equal (xmlp::value min-card-cons) (xmlp::value max-card-cons))

                   ; if min and max are equal then set only the cardinality facet
                   (if (string-not-equal "unbounded" (xmlp::value min-card-cons))
                       (setf (getf slot ':cardinality) (get-class-part-value min-card-cons)))
                   ; otherwise set the mincard and maxcard facets individually
                   (and
                    (if (string-not-equal "unbounded" (xmlp::value min-card-cons))
                        (setf (getf slot ':min-cardinality) (get-class-part-value min-card-cons)))
                    (if (string-not-equal "unbounded" (xmlp::value max-card-cons))
                        (setf (getf slot ':max-cardinality) (get-class-part-value max-card-cons)))))

               ; there is no max but there is a min
               (if (string-not-equal "unbounded" (xmlp::value min-card-cons))
                   (setf (getf slot ':min-cardinality) (get-class-part-value min-card-cons))))

           ; there is no min but there may be a max
           (if (setq max-card-cons (car (is-attribute "maxOccurs" (xmlp::attributes node))))
               (if (string-not-equal "unbounded" (xmlp::value max-card-cons))
                   (setf (getf slot ':max-cardinality) (get-class-part-value max-card-cons)))))

   
       ;;; returns slot as a property list which will be added in the *ht* in the calling function
       slot))))




;;;;;;;;;;;;;;;;; Functions for finding a node with a certain name in the xsd doc, by checking all descendants.

(defun find-node-named (name node)
  ;;; Finds the (first) node named 'name' which is a child of 'node', or returns nil if there isn't any in the whole document.
  ;; assuming no 'xs' or 'xsd' prefix is used
  (if (typep node 'xqdm:elem-node)
      (if (equal (get-prefix name) nil)
          ; we only look for the node in the current doc if the name doesn't have a prefix; if it does it means it's defined in another doc. 
          (if (setq returned-node (test-node-named name node))
              returned-node
              (check-children-list name (xmlp::children node))))))


(defun check-children-list (name clist)
	(if (not clist)
	    nil
	    (if (setq returned-node (find-node-named name (car clist)))
		returned-node
		(check-children-list name (cdr clist)))))


(defun test-node-named (name node)
  (if (string= name (remove-prefix (xmlp::value (car (is-attribute "name" (xmlp::attributes node))))))
	node))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for generating an ocml type from an xsd string

(defun get-class-part-value (x)
  (when x
    (let ((*package* (find-package "OCML")))
      (handler-case (read-from-string (remove-prefix (xmlp::value x)))
        (error (c) x)
        (serious-condition (c) x)))))


(defun get-symbol-from-string (x)
  (when x
    (let ((*package* (find-package "OCML")))
      (handler-case (read-from-string x)
        (error (c) x)
        (serious-condition (c) x)))))



(defun get-type (node)
;this function is used when processing a 'ref' node
  (if (and node (setq type (car (is-attribute "type" (xmlp::attributes node)))))
      ;; if it has a 'type'
      (get-ocml-type (get-class-part-value type))
      ;; otherwise it's a complex (or simple) type, so return its name
      (get-ocml-type (get-class-part-value (car (is-attribute "name" (xmlp::attributes node)))))))



(defun get-ocml-type (xsd-type)
; converts an xsd type (in a string form) into an ocml type
; many ocml types have yet to be defined in ocml
  (cond
   ((string= "string" xsd-type) "string")
   ((string= "normalizedString" xsd-type) "string")
   ((string= "token" xsd-type) "string")

   ((string= "boolean" xsd-type) "boolean")

   ((string= "decimal" xsd-type) "real-number")
   ((string= "anyURI" xsd-type) "url")
   ((string= "double" xsd-type) "real-number")
   ((string= "float" xsd-type) "real-number")
   ((string= "integer" xsd-type) "integer")
   ((string= "short" xsd-type) "integer")
   ((string= "long" xsd-type) "integer")
   ((string= "byte" xsd-type) "integer")
   ((string= "int" xsd-type) "integer")
   ((string= "unsignedLong" xsd-type) "integer")
   ((string= "unsignedInt" xsd-type) "integer")
   ((string= "unsignedShort" xsd-type) "integer")
   ((string= "unsignedByte" xsd-type) "integer")
   ((string= "nonNegativeInteger" xsd-type) "non-negative-integer")
   ((string= "negativeInteger" xsd-type) "integer")                           
   ((string= "positiveInteger" xsd-type) "positive-integer")
   ((string= "nonPositiveInteger" xsd-type) "integer")                        

   ((string= "date" xsd-type) "calendar-date")
   ((string= "dateTime" xsd-type) "time-point")
   ((string= "time" xsd-type) "time-point" )                                       ;; time (without date) has yet to be defined
   ((string= "duration" xsd-type) "time-interval")
   (t xsd-type)))




(defun get-xsd-type (type-name)
;; returns the built-in type contained in the 'type-name', checking for prefixes; it's the same as 'remove-prefix'
  (let ((pos (search ":" type-name)))
    (if pos 
        (subseq type-name (+ 1 pos))
        type-name)))


(defun remove-prefix (name)
; removes the prefix from a string if there is one, otherwise returns the string as it is; most useful when looking for a certain attribute
  (let ((pos (search ":" name)))
    (if pos
        (subseq name (+ 1 pos))
        name)))


(defun get-prefix (name)
; returns the prefix contained in 'name' if it exists, otherwise nil
  (let ((pos (search ":" name)))
    (if pos 
        (subseq name 0 pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; code for specific xsd restrictions on data types

(defun process-restriction (node class-name)
;; this function is called for a <restriction> node, from the function which processes <simpleType>
;; 'class-name' is the name of the class which is a subclass of the 'base' type, i.e. the  name of the simpleType
;; restrictions on other simpleTypes than integer and string and on user-defined classes (<complexTypes>) are yet to be implemented

  (let ((children (xmlp::children node)))
    (cond
     ((or 
       (string= "minInclusive" (remove-prefix (xmlp::local-part (cadr children))))
       (string= "minExclusive" (remove-prefix (xmlp::local-part (cadr children))))     
       (string= "maxInclusive" (remove-prefix (xmlp::local-part (cadr children))))
       (string= "maxExclusive" (remove-prefix (xmlp::local-part (cadr children))))) (process-interval node class-name)) 

     ((string= "enumeration" (remove-prefix (xmlp::local-part (cadr children)))) (process-enumeration node class-name))
     
     ((string= "pattern" (remove-prefix (xmlp::local-part (cadr children)))) (process-pattern node class-name)) ))) 


(defun process-interval (node class-name)
; it applies to numeric values (i.e. integers, reals, etc, not e.g. strings)
  (let ((children (xmlp::children node))
        (base (remove-prefix (xmlp::value (car (is-attribute "base" (xmlp::attributes node))))))
        (properties ()))
 
   (setf (getf properties ':base) base)
   
    (dolist (child (xmlp::children node))
      (if (and
           (typep node 'xqdm:elem-node)
           (setq tag (remove-prefix (xmlp::local-part child))))
          (cond
           ((string= "minInclusive" tag) (setf (getf properties ':minInclusive) (get-class-part-value (car (is-attribute "value" (xmlp::attributes child))))))
           ((string= "minExclusive" tag) (setf (getf properties ':minExclusive) (get-class-part-value (car (is-attribute "value" (xmlp::attributes child))))))
           ((string= "maxInclusive" tag) (setf (getf properties ':maxInclusive) (get-class-part-value (car (is-attribute "value" (xmlp::attributes child))))))
           ((string= "maxExclusive" tag) (setf (getf properties ':maxExclusive) (get-class-part-value (car (is-attribute "value" (xmlp::attributes child)))))))))

    ; create new class
    (setf (gethash class-name *ht*) (list properties))))



(defun process-enumeration (node class-name)
  (let ((children (xmlp::children node))
        (base (remove-prefix (xmlp::value (car (is-attribute "base" (xmlp::attributes node))))))
        (enum_list ()))
 
    (setf (getf properties ':base) base)
    (setf (getf properties ':enumeration) nil)
   
    (dolist (child (xmlp::children node))
      (if (and
           (typep node 'xqdm:elem-node)
           (setq tag (remove-prefix (xmlp::local-part child))))
          (if
           (string= "enumeration" tag) 
           (setf (getf properties ':enumeration) (push (getf properties ':enumeration) (remove-prefix (xmlp::value (car (is-attribute "value" child)))))))))

    ; create new class
    (setf (gethash class-name *ht*) (list properties))))



;; the code generating section
;     '(def-class ,class-name (get-symbol-from-string (,base))
;        :lisp-fun #'(lambda (x env)
;                      (let ((y (instantiate x env)))
;                        (if (and (integerp y)
;                                 (> y 0))
;;                            (list env)
;                          :fail))))
    

;(def-class POSITIVE-INTEGER (integer)
;   "The class of all integers > 0"
;   :lisp-fun  #'(lambda (x env)
;                  (let ((y (instantiate x env)))
;                 (if (and (integerp y)
;                          (>= y 0)
;                          (<= y 100))
;                     (list env)
;                     :fail))))







;;;;;;;;;;;;;;;;;; generating OCML code as a list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-slot-name (single-slot-info)
  (getf single-slot-info ':slot-name))

(defun get-slot-type (single-slot-info)
  (getf single-slot-info ':type))

(defun get-parents (class-name)
  (let ((parents-list (has-parents (gethash class-name *ht*))))
    (if parents-list
        (getf parents-list ':parents)))) 

(defun get-slot-default-value (single-slot-info)
  (getf single-slot-info ':default-value))

(defun get-slot-value (single-slot-info)
  (getf single-slot-info ':value))

(defun get-slot-min-cardinality (single-slot-info)
  (getf single-slot-info ':min-cardinality))

(defun get-slot-max-cardinality (single-slot-info)
  (getf single-slot-info ':max-cardinality))

(defun get-slot-cardinality (single-slot-info)
  (getf single-slot-info ':cardinality))

(defun has-parents (class-value)
 (if class-value
     (if (string= ':parents (caar class-value))
         (car class-value)
         (has-parents (cdr class-value)))))


(defun get-base () ())


(defun generate-class-with-parents (class-name slot-list)                     
; returns the ocml code of the class (with parents), as a list: (def-class....)
  (if (setq parents (get-parents class-name))
    ;; if there are parents  
    `(def-class ,class-name (,parents)
     ,(mapcar #'(lambda (single-slot-info)
                  `(,(get-slot-name single-slot-info) :type ,(get-slot-type single-slot-info) 

                                                      ;:default-value ,(get-slot-default-value single-slot-info)
                                                      ,@(if (get-slot-default-value single-slot-info)
                                                            (list ':default-value (get-slot-default-value single-slot-info)))
                                                      ;:value ,(get-slot-value single-slot-info)
                                                      ,@(if (get-slot-value single-slot-info)
                                                            (list ':value (get-slot-value single-slot-info)))
                                                      ;:min-cardinality ,(get-slot-min-cardinality single-slot-info)
                                                      ,@(if (get-slot-min-cardinality single-slot-info)
                                                            (list ':min-cardinality (get-slot-min-cardinality single-slot-info)))
                                                      ;:max-cardinality ,(get-slot-max-cardinality single-slot-info)
                                                      ,@(if (get-slot-max-cardinality single-slot-info)
                                                            (list ':max-cardinality (get-slot-max-cardinality single-slot-info)))
                                                      ;:cardinality ,(get-slot-cardinality single-slot-info)))
                                                      ,@(if (get-slot-cardinality single-slot-info)
                                                            (list ':cardinality (get-slot-cardinality single-slot-info)))))
              (cdr slot-list)))
 
   ;; if there are no parents
    `(def-class ,class-name ()
     ,(mapcar #'(lambda (single-slot-info)
                  `(,(get-slot-name single-slot-info) :type ,(get-slot-type single-slot-info) 

                                                      ;:default-value ,(get-slot-default-value single-slot-info)
                                                      ,@(if (get-slot-default-value single-slot-info)
                                                            (list ':default-value (get-slot-default-value single-slot-info)))
                                                      ;:value ,(get-slot-value single-slot-info)
                                                      ,@(if (get-slot-value single-slot-info)
                                                            (list ':value (get-slot-value single-slot-info)))
                                                      ;:min-cardinality ,(get-slot-min-cardinality single-slot-info)
                                                      ,@(if (get-slot-min-cardinality single-slot-info)
                                                            (list ':min-cardinality (get-slot-min-cardinality single-slot-info)))
                                                      ;:max-cardinality ,(get-slot-max-cardinality single-slot-info)
                                                      ,@(if (get-slot-max-cardinality single-slot-info)
                                                            (list ':max-cardinality (get-slot-max-cardinality single-slot-info)))
                                                      ;:cardinality ,(get-slot-cardinality single-slot-info)))
                                                      ,@(if (get-slot-cardinality single-slot-info)
                                                            (list ':cardinality (get-slot-cardinality single-slot-info)))))
              slot-list))))    


(defun generate-ordered-ocml (class-store)
  (ordering-algorithm (get-all-class-names class-store)))


(defun get-all-class-names (class-store)
; returns the list of all the class-names in the hashtable; class-store is the hashtable
  (let ((class-names nil))
    (maphash #'(lambda (key value) 
                 (if (not (equal key 'top-class)) (push key class-names))) 
             class-store)
    class-names))          


(defun order (E)
;;; order (E) returns: a) the list C of class names without parents and classes without parents in current set; and b) the list of ocml code, the two together as a list

  (let ((C nil) (ocml-code nil))
    (dolist (class-name E)
      (if (or (no-parents class-name) (no-parents-in-current-set class-name E))
          (progn
            (setq ocml-code (append ocml-code (list (generate-class-with-parents class-name (gethash class-name *ht*)))))
            (push class-name C))))
    (list C ocml-code)))


(defun ordering-algorithm (E)
  (if E 
      (progn
        (setq order-result (order E))
        (setq C (car order-result))
        (setq ocml-code (cadr order-result)) 
        (append ocml-code (ordering-algorithm (minus E C))))))


(defun minus (A B)
;;; returns A-B, where A and B are lists
  (if A
      (if (not (member (car A) B :test #'equal))
          (append (list (car A)) (minus (cdr A) B))
          (minus (cdr A) B))))                             



(defun no-parents (class-name)
; returns T if class-name has no parents

  (and 
   (gethash class-name *ht*)
   (not (equal :parents (caar (gethash class-name *ht*))))))                


(defun no-parents-in-current-set (class-name class-names-set)                           
;;; returns T if the class-name does not have any parents in the class-names-set

  (if (gethash class-name *ht*)
      (let ((result T)
            (parent (getf (car (gethash class-name *ht*)) :parents)))
            (if (member parent class-names-set :test #'equal)
                (setq result nil))
            result))) 



;;;;;;;;;;;;;;;;; Unimplemented functions

(defun process-simpleContent (node class-name) ())
(defun process-group (node) ())
(defun process-pattern (node class-name)())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-attribute (node class-name)
  (if (not (xmlp::children node))
      (progn
        (setq slot (process-empty-node node))
        (setf (gethash class-name *ht*) (append (gethash class-name *ht*) (list slot))))))




(defun process-non-empty-element (node class-name)
 (progn
    ;; first create a slot in parent
    (setq slot ())
    (setf name  (car (is-attribute "name" (xmlp::attributes node))))
    (setf (getf slot ':slot-name) (get-class-part-value name) (getf slot ':type) (get-class-part-value name))
    (setf (gethash class-name *ht*) (append (gethash class-name *ht*) (list slot)))
    (dolist (child (xmlp::children node))
        (process-node child (get-class-part-value name))))) 



(defun process-complexType (node class-name)
  (progn
    (if (setq new-name (get-class-part-value (car (is-attribute "name" (xmlp::attributes node)))))
            (progn
              (setq class-name new-name)
              (setf (gethash class-name *ht*) '())))
    (dolist (child (xmlp::children node))
      (process-node child class-name))))


(defun process-simpleType (node class-name)
  (progn
    (if (setq new-name (get-class-part-value (car (is-attribute "name" (xmlp::attributes node)))))
            (progn
              (setq class-name new-name)
              (setf (gethash class-name *ht*) '())))
    (dolist (child (xmlp::children node))
      (process-node child class-name))))


(defun process-sequence (node class-name)
  (dolist (child (xmlp::children node))
    (process-node child class-name)))


(defun process-element (node class-name)
  (if (xmlp::children node)
      (process-non-empty-element node class-name)
      (progn
        (setq slot (process-empty-node node))
        (setf (gethash class-name *ht*) (append (gethash class-name *ht*) (list slot))))))



(defun process-schema (node class-name)
  (progn
    (setf (gethash class-name *ht*) '())

    (if (xmlp::children node)
        (dolist (child (xmlp::children node))
          (process-node child class-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; process-node returns nil if called with anything but an xsd node

(defun process-node (node &optional class-name)
  (if (and
       (typep node 'xqdm:elem-node)
       (setq tag (remove-prefix (xmlp::local-part node))))
      (cond 
       ((string= "schema" tag) (process-schema node class-name))
       ((string= "element" tag) (process-element node class-name))
       ((string= "attribute" tag) (process-attribute node class-name))
       ((string= "complexType" tag)(process-complexType node class-name))
       ((string= "simpleType" tag) (process-simpleType node class-name))
       ((string= "sequence" tag) (process-sequence node class-name))
       ((string= "complexContent" tag) (process-complexContent node class-name))
       ((string= "simpleContent" tag) (process-simpleContent node class-name))
       ((string= "extension" tag) (process-extension node class-name))
       ((string= "restriction" tag) (process-restriction node class-name))
       ((string= "group" tag) (process-group node))
       ((string= "import" tag) (process-import node class-name))
       ((string= "include" tag) (process-include)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class inheritance


(defun process-complexContent (node class-name)
  (if (xmlp::children node)
      (dolist (child (xmlp::children node))
        (process-node child class-name))))


(defun process-extension (node class-name)
  (if (setq base (car (is-attribute "base" (xmlp::attributes node))))
    (progn    
      (setq parents-list ())
      (setf (getf parents-list ':parents) (get-class-part-value base))
      (setf (gethash class-name *ht*) (append (gethash class-name *ht*) (list parents-list)))
      (if (xmlp::children node)
          (dolist (child (xmlp::children node))
            (process-node child class-name))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Namespace processing functions

(defun process-import (node class-name)
  ; imports an ontology associated with the namespace declared in the <import> node
  (push (generate-symbol (xmlp::value (car (is-attribute "namespace" (xmlp::attributes node))))) (car *used-ontologies*))
)


(defun process-include ())


;(defparameter *example* 
;"<element name=\"age\">
;<simpleType>
;  <restriction base=\"xs:integer\">
;    <minInclusive value=\"0\"/>
;    <maxInclusive value=\"100\"/>
;  </restriction>
;</simpleType>
;</element>")



;;;;;;;;;;;;;;;;; Top level function:

(defun process-xsd (xmldoc &optional ontology parents include-namespace-in-names-p)
  (progn
    (defparameter *ht* (make-hash-table :test 'equal))
    
    ;(defarameter *used-ontologies* parents)

    (setq *root-node* (xmlp::root (xmlp::document-parser xmldoc)))      ;;; *root-node* is a global var necessary to process a 'ref'-ed node
    
    (process-node *root-node* 'top-class)
    
    (setq ocml-code (format nil "~(~{~s~%~%~}~)" (generate-ordered-ocml *ht*)))

    (store-source-code ontology ocml-code "roxana" "roxana" nil :goal 'ocml nil 
                   (if (ocml::get-ontology 'ocml::xsd)
                       'ocml::append
                     'ocml::new))

    ocml-code))



(defun store-source-code (ontology source-code author password &optional uses (type :goal) (format 'ocml) allowed-editors
                                   (upload-mode 'ocml::new) (stream *standard-output*))
  (ip::internal-upload ontology uses type format source-code upload-mode author password allowed-editors stream))


(store-source-code 'ocml::xsd "(def-class foo ())" "roxana" "roxana" )

(get-symbol-from-string "xsd")

(store-source-code (get-symbol-from-string "xsd") "(def-class foo ())" "roxana" "roxana") ;; works!




;(format nil "~a" ocml-code)

;(format nil "~(~{~s~%~%~}~)" (generate-ordered-ocml *ht*))

