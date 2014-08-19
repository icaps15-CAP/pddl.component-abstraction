(in-package :pddl.component-abstraction)

(defun abstract-components-seed-only (*problem* seed-type)
  (let* ((sfs (static-facts *problem*))
         (static-objects (%static-objects sfs))
         (fluent-objects
          (set-difference (objects *problem*) static-objects))
         (acs
          (cluster-objects-seed-only
           (query-type (domain *problem*) seed-type)
           (remove seed-type (%all-types static-objects))
           static-objects
           sfs
           (static-predicates *problem*))))
    (format t "~&~a abstract components, including junks" (length acs))
    (categorize-by-equality acs #'has-same-number-of-attributes :transitive nil)))

(defun cluster-objects-seed-only
    (seed other-types static-objects static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (declare (ignorable other-types))
  (format t "~&~2tComponent Abstraction search with seed = ~a" seed)
  (let ((*print-length* 5))
    (let* ((acs nil)
           (closed nil)
           (tried-preds nil)
           (type seed))
      (more-labels () (%facts-instantiating
                       %collect-seed-components
                       %untried-predicates
                       %update-components)
        (iter (with open = nil)
              (initially (push type open)
                         (%collect-seed-components type))
              (while open)
              (for t1 = (pop open))
              (push t1 closed)
              (format t "~%~6tOpening : t1 = ~a" t1)
              (iter (for p in (%untried-predicates t1))
                    (push p tried-preds)
                    (for p-facts = (%facts-instantiating p))
                    (when p-facts
                      ;;(format t "~%~8tAll predicates tried: ~a" (mapcar #'name tried-preds))
                      (format t "~%~8tExtend: ~a facts / ~a" (length p-facts) p)
                      (multiple-value-setq (acs open)
                        (%update-components p p-facts open))))))
      (format t "~&~4tClosed: ~a" (mapcar #'name closed))
      (format t "~&~4t~a Established Abstract Components" (length acs))
      acs)))
