(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-tasks (*problem* type)
  (let ((type (query-type (domain *problem*) type)))
    (mapcar (curry #'mapcar (rcurry #'task *problem*))
            (remove-if-not
             (lambda (acs)
                (some (lambda (ac)
                        (when-let ((seed (abstract-component-seed ac)))
                          (eq type (type seed))))
                      acs))
             (abstract-components-with-seed *problem* type)))))


@export
(defun categorize-tasks (tasks method)
  (categorize-by-equality
   tasks
   (case method
     (:strict #'abstract-component-task-strict=)
     (:weak   #'abstract-component-task=)
     (:loose  #'abstract-component-task<=>))
   :transitive nil))

(defun abstract-component-task-strict= (t1 t2)
  (and (abstract-component-task= t1 t2)
       (strictly-task= t1 t2)))

(defun strictly-task= (t1 t2)
  (and (state-equal-except-components
        (abstract-component-task-multiary-init t1)
        (abstract-component-task-multiary-init t2)
        (abstract-component-components 
         (abstract-component-task-ac t1))
        (abstract-component-components 
         (abstract-component-task-ac t2)))
       (state-equal-except-components
        (abstract-component-task-multiary-goal t1)
        (abstract-component-task-multiary-goal t2)
        (abstract-component-components 
         (abstract-component-task-ac t1))
        (abstract-component-components 
         (abstract-component-task-ac t2)))))

;; abstract-component-task-multiary-goal

(defun state-equal-except-components (state1 state2 comps1 comps2)
  (iter (for f1 in state1)
        (for f2 in state2)
        (always
         (iter (for p1 in (parameters f1))
               (for p2 in (parameters f2))
               (always
                (or (and (find p1 comps1) (find p2 comps2))
                    (eq p1 p2)))))))

