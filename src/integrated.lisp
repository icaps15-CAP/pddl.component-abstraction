(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-components (*problem*)
  (mapcar (rcurry #'categorize-by-equality #'abstract-type<=> :transitive nil)
          (cluster-objects (static-facts *problem*)
                           (static-predicates *problem*))))

@export
(defun abstract-components-with-seed (*problem* seed-type)
  (let* ((sfs (static-facts *problem*))
         (static-objects (%static-objects sfs))
         (fluent-objects
          (set-difference (objects *problem*) static-objects))
         (acs
          (concatenate
           'vector
           ;; adding objects that do not appear in the static graph and
           ;; ensure all objects in the domain are included in the result
           (map 'vector
                (lambda (o)
                  (make-abstract-component
                   :seed o :components (list o)))
                fluent-objects)
           (cluster-objects-with-seed
            (query-type (domain *problem*) seed-type)
            (remove seed-type (%all-types static-objects))
            static-objects
            sfs
            (static-predicates *problem*)))))
    (format t "~&~a abstract components, including junks" (length acs))
    (categorize-by-equality acs #'abstract-type= :transitive nil)))

@export
(defun best-abstract-components (*problem*)
  (first
   (sort (abstract-components *problem*)
         #'>
         :key (lambda (vector)
                (reduce #'* (map 'list #'length vector))))))
