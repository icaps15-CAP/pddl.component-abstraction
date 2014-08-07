(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defstruct (abstract-component-task (:predicate abstract-component-task-p))
  "A structure for abstract-task.
 slot ac is the core component.
 slot init holds the relevant initial state, unrestored.
 slot goal holds the relevant goal."
  (problem *problem*) init goal ac)

(export '(ac
          abstract-component-task-p
          abstract-component-task-init
          abstract-component-task-goal
          abstract-component-task-unary-init
          abstract-component-task-unary-goal
          abstract-component-task-multiary-init
          abstract-component-task-multiary-goal
          abstract-component-task-ac
          abstract-component-task-problem
          make-abstract-component-task))

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

(defun visible-elements (components f)
  (match f
    ((pddl-function-state name parameters value)
     (list* name value (mapcar #'name (set-difference parameters components))))
    ((pddl-predicate name parameters)
     (list* name (mapcar #'name (set-difference parameters components))))))

(defmethod print-object ((ac-task abstract-component-task) s)
  (print-unreadable-object (ac-task s)
    (match ac-task
      ((abstract-component-task- (ac (and ac (abstract-component components)))
                                 multiary-init multiary-goal
                                 unary-init unary-goal)
       (let ((*print-escape* t))
         (format s "~<A-TASK ~;~@{~w ~w~^ ~:_~}~:>"
                 (list :ac ac
                       :init (mapcar (curry #'visible-elements components) multiary-init)
                       :goal (mapcar (curry #'visible-elements components) multiary-goal)
                       :init (mapcar (curry #'visible-elements components) unary-init)
                       :goal (mapcar (curry #'visible-elements components) unary-goal))))))))

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
  "defines a relationship between tasks.
 t1 <= t2 when attributes, inits, goals of t1 are the subset of that of t2.
 In KEPS paper, these corresponds to "
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
  "Same abstract type and same set of attributes, as in KEPS."
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


