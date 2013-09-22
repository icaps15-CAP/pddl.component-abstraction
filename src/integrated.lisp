(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun abstract-components (*problem*)
  (mapcar (rcurry #'categorize-by-equality #'abstract-type<=>)
          (cluster-objects (static-facts *problem*)
                           (static-predicates *problem*))))

@export
(defun best-abstract-components (*problem*)
  (first
   (sort (abstract-components *problem*)
         #'>
         :key (lambda (vector)
                (reduce #'* (map 'list #'length vector))))))
