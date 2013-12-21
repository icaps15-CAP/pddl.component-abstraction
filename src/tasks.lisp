(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defstruct abstract-component-task
  (problem *problem*) init goal ac)

@export '(abstract-component-task-p
          abstract-component-task-init
          abstract-component-task-goal
          abstract-component-task-unary-init
          abstract-component-task-unary-goal
          abstract-component-task-multiary-init
          abstract-component-task-multiary-goal
          abstract-component-task-ac
          abstract-component-task-problem
          make-abstract-component-task)

(defmethod problem ((ac abstract-component-task))
  @inline abstract-component-task-problem
  (abstract-component-task-problem ac))

(defmethod domain((ac abstract-component-task))
  @inline abstract-component-task-problem
  (domain (abstract-component-task-problem ac)))

(defun unary-p (f)
  (match f
    ((pddl-atomic-state parameters)
     (<= (length parameters) 1))
    ((pddl-function-state parameters)
     (= 0 (length parameters)))))

(defun abstract-component-task-unary-goal (ac)
  (remove-if-not
   #'unary-p
   (abstract-component-task-goal ac)))

(defun abstract-component-task-unary-init (ac)
  (remove-if-not
   #'unary-p
   (abstract-component-task-init ac)))

(defun abstract-component-task-multiary-goal (ac)
  (remove-if
   #'unary-p
   (abstract-component-task-goal ac)))

(defun abstract-component-task-multiary-init (ac)
  (remove-if
   #'unary-p
   (abstract-component-task-init ac)))

(defun print-ac-task-slot (s ac-task name body)
  (format s "~w " name)
  (let ((*print-escape* nil)
        (*print-length* 5)
        (ac (abstract-component-task-ac ac-task)))
    (pprint-logical-block (s body :prefix "(" :suffix ")")
      (loop
         (pprint-exit-if-list-exhausted)
         (let ((f (pprint-pop)))
           (format s "~a"
                   (match f
                     ((pddl-function-state name parameters value)
                      (list* name
                             value
                             (mapcar #'name 
                                     (set-difference parameters
                                                     (parameters ac)))))
                     ((pddl-predicate name parameters)
                      (cons name
                            (mapcar #'name 
                                    (set-difference parameters
                                                    (parameters ac)))))))
           (pprint-exit-if-list-exhausted)
           (write-char #\Space s)
           (pprint-newline :fill s))))))

(defmethod print-object ((ac-task abstract-component-task) s)
  (print-unreadable-object (ac-task s :type t)
    (pprint-logical-block (s nil)
      (format s "~w ~w " :ac (abstract-component-task-ac ac-task))
      (print-ac-slot-nonnil s ac-task #'abstract-component-task-multiary-init :init #'print-ac-task-slot)
      (print-ac-slot-nonnil s ac-task #'abstract-component-task-multiary-goal :goal #'print-ac-task-slot)
      (print-ac-slot-nonnil s ac-task #'abstract-component-task-unary-init :unary-init #'print-ac-task-slot)
      (print-ac-slot-nonnil s ac-task #'abstract-component-task-unary-goal :unary-goal #'print-ac-task-slot)
      )))

@export
(defun facts-concerning (ac facts)
  (remove-if-not
   (lambda (f)
     (some (lambda (p)
             (member p (parameters ac)))
           (parameters f)))
   (set-difference facts (abstract-component-facts ac))))

@export
(defun task (ac *problem*)
  (make-abstract-component-task
   :problem *problem*
   :ac ac
   :init (facts-concerning ac (set-difference (init *problem*) (static-facts *problem*)))
   :goal (facts-concerning ac (positive-predicates (goal *problem*)))))

@export
(defun abstract-component-task<= (t1 t2)
  (match t1
    ((abstract-component-task
      (init i1) (goal g1)
      (ac (abstract-component (attribute-facts af1))))
     (match t2
       ((abstract-component-task
         (init i2) (goal g2)
         (ac (abstract-component (attribute-facts af2))))
        (and (and (<= (length af1) (length af2))
                  (subsetp af1 af2 :key #'name))
             (and (<= (length i1) (length i2))
                  (subsetp i1 i2 :key #'name))
             (and (<= (length g1) (length g2))
                  (subsetp g1 g2 :key #'name))))))))

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


