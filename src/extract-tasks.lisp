(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-tasks (*problem* type)
  (let ((type (query-type (domain *problem*) type)))
    (mapcar (rcurry #'task *problem*)
             (find-if
              (lambda (acs)
                (some (lambda (ac)
                        (eq type (type (abstract-component-seed ac))))
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
       ))