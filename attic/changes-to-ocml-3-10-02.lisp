(defmethod find-alpha-node-candidates ((relation ocml-relation)   &aux class)
  (with-slots (slot-of  relation-instances lisp-fun name) relation
    (unless lisp-fun                     ;;If it is defined by a lisp fun, we ignore it
      (if relation-instances
          (values relation-instances :relation-instances)
          (if slot-of
              (values 
               (loop for class in (filter-active-classes slot-of)
		     appending (get-current-direct-instances class))
               :slot)
              (when (setf class (get-domain-class name))
                (values (get-current-instances class)
                        :class)))))))