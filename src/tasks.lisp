(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defstruct abstract-component-task
  attributes init goal ac)

@export '(abstract-component-task-init
          abstract-component-task-goal
          abstract-component-task-attributes
          abstract-component-task-ac
          make-abstract-component-task)

(defun print-ac-slot (s ac name body)
  (format s "~w " name)
  (let ((*print-escape* nil))
    (pprint-logical-block (s body :prefix "(" :suffix ")")
      (loop
         (pprint-exit-if-list-exhausted)
         (let ((f (pprint-pop)))
           (format s "(~{~a~^ ~})"
                   (cons (name f)
                         (mapcar #'name 
                                 (set-difference (parameters f)
                                                 (parameters ac)))))
           (pprint-exit-if-list-exhausted)
           (write-char #\Space s)
           (pprint-newline :fill s))))))

(defmethod print-object ((ac-task abstract-component-task) s)
  (print-unreadable-object (ac-task s :type t)
    (with-slots (ac attributes init goal) ac-task
      (pprint-logical-block (s nil)
        (format s "~w ~a " :ac ac)
        (pprint-newline :linear s)
        (print-ac-slot s ac :attr attributes)
        (pprint-newline :linear s)
        (print-ac-slot s ac :init init)
        (pprint-newline :linear s)
        (print-ac-slot s ac :goal goal)))))

@export
(defun task (ac *problem*)
  (make-abstract-component-task
   :ac ac
   :attributes (facts-concerning ac (static-facts *problem*))
   :init (facts-concerning ac (set-difference (init *problem*) (static-facts *problem*)))
   :goal (facts-concerning ac (positive-predicates (goal *problem*)))))

@export
(defun abstract-component-task<= (ac-t1 ac-t2)
  (with-slots ((a1 attributes) (i1 init) (g1 goal) (ac1 ac)) ac-t1
    (with-slots ((a2 attributes) (i2 init) (g2 goal) (ac2 ac)) ac-t2
      (and (<= (length a1) (length a2))
           (subsetp (mapcar #'name a1)
                    (mapcar #'name a2))
           (<= (length i1) (length i2))
           (subsetp (mapcar #'name i1)
                    (mapcar #'name i2))
           (<= (length g1) (length g2))
           (subsetp (mapcar #'name g1)
                    (mapcar #'name g2))))))

@export
(defun abstract-component-task>= (ac-t1 ac-t2)
  (abstract-component-task<= ac-t2 ac-t1))

@export
(defun abstract-component-task= (ac-t1 ac-t2)
  (and (abstract-component-task<= ac-t1 ac-t2)
       (abstract-component-task<= ac-t2 ac-t1)))
@export
(defun abstract-component-task<=> (ac-t1 ac-t2)
  (or (abstract-component-task<= ac-t1 ac-t2)
      (abstract-component-task<= ac-t2 ac-t1)))
@export
(defun abstract-component-task< (ac-t1 ac-t2)
  (and (abstract-component-task<= ac-t1 ac-t2)
       (abstract-component-task= ac-t1 ac-t2)))


