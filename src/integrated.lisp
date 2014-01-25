(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-components (*problem*)
  (mapcar (rcurry #'categorize-by-equality #'abstract-type<=> :transitive nil)
          (cluster-objects (static-facts *problem*)
                           (static-predicates *problem*))))

@export
(defun abstract-components-with-seed (*problem* seed-type)
  (categorize-by-equality
   (let* ((sfs (static-facts *problem*))
          (constants (%constants sfs))
          (non-constants
           (set-difference (objects *problem*) constants)))
     (concatenate
      'vector
      (map 'vector
       (lambda (o)
         (make-abstract-component :components (list o)
                                  :seed o))
       non-constants)
      (cluster-objects-with-seed
       (query-type (domain *problem*) seed-type)
       (remove seed-type (%all-types constants))
       constants
       sfs
       (static-predicates *problem*))))
   #'abstract-type= :transitive nil))



@export
(defun best-abstract-components (*problem*)
  (first
   (sort (abstract-components *problem*)
         #'>
         :key (lambda (vector)
                (reduce #'* (map 'list #'length vector))))))
