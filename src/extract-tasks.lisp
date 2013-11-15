(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun facts-concerning (ac facts)
  (remove-if-not
   (lambda (f)
     (some (lambda (p)
             (member p (parameters ac)))
           (parameters f)))
   (set-difference facts (abstract-component-facts ac))))


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

