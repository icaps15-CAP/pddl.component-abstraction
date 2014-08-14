(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-tasks (*problem* type) 
  "problem, pddl-type -> (list a-task) For each task, the core component is
grown from the given seed type.  However, it does not guarantee the task
eqality (because they might have the different sets of attibutes, init and
goals)"
  (mapcar (lambda (ac) (task ac *problem*))
          (pick-those-from-seed-type
           (query-type (domain *problem*) type)
           (abstract-components-with-seed *problem* type))))

(defun pick-those-from-seed-type (type ac-sets) ; (list (list ac)) -> (list ac)
  (let ((result (remove-if-not
                 (lambda (acs)
                   ;; list of acs. Contains ac with the same seed-type, but the seed-type
                   ;; is not necessarily of the target type.
                   (some (lambda (ac) (when-let ((seed (abstract-component-seed ac)))
                                        (eq type (type seed))))
                         acs))
                 ;; list of list of acs
                 ac-sets)))
    (unless (= 1 (length result))
      (let ((*print-length* 4) (*print-escape* t))
        (warn "Components from the same seed w/ different abstract-type!"
              ;; (mapcar (lambda (bucket) (list* :length (length bucket) :examples bucket)) result)
              )))
    (flatten result)))


@export
(defun categorize-tasks (tasks &optional method)
  @ignore method
  (categorize-by-equality
   tasks #'abstract-component-task= :transitive nil))

