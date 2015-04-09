(in-package :pddl.component-abstraction)

(defun abstract-tasks-single-node (*problem* type) 
  "Pseudo abstraction algorithm which do not extend the components."
  (mapcar (lambda (ac) (task ac *problem*))
          (reduce #'append (abstract-components-single-node *problem* type)
                  :from-end t)))

(defun abstract-components-single-node (*problem* seed-type)
  (let* ((sfs (static-facts *problem*))
         (objects (objects *problem*))
         (seed-type (query-type (domain *problem*) seed-type))
         (acs
          (cluster-objects-single-node
           seed-type
           (remove seed-type (%all-types objects))
           objects
           sfs
           (static-predicates *problem*))))
    (format t "~&~a abstract components, no junks (seed only)" (length acs))
    (force-output)
    (categorize-by-equality acs #'abstract-type=/fast :transitive t)))

(defun cluster-objects-single-node
    (seed other-types static-objects static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (declare (ignorable other-types))
  (format t "~&~2tComponent Abstraction search with seed = ~a" seed)
  (let* ((acs nil)
         (closed nil)
         (tried-preds nil)
         (type seed))
    (more-labels () (%facts-instantiating
                     %collect-seed-components
                     %untried-predicates
                     %update-components*)
      (%collect-seed-components type))
    (format t "~&~4t~a Established Abstract Components" (length acs))
    (format t "~&~4tof which there are ~a objects"
            (mapcar (compose #'length
                             #'abstract-component-components) acs))
    acs))
