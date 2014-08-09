(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

(defstruct (abstract-component-task (:predicate abstract-component-task-p))
  "A structure for abstract-task.
 slot ac is the core component.
 slot init holds the relevant initial state, unrestored.
 slot goal holds the relevant goal."
  (problem *problem*) init goal ac)

(export '(ac
          abstract-component-task
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


(defun facts-concerning (ac facts)
  (remove-if-not
   (lambda (f)
     (some (lambda (p)
             (member p (parameters ac)))
           (parameters f)))
   (set-difference facts (abstract-component-facts ac))))

(defun task (ac *problem*)
  (make-abstract-component-task
   :problem *problem*
   :ac ac
   :init (facts-concerning ac (set-difference (init *problem*) (static-facts *problem*)))
   :goal (facts-concerning ac (positive-predicates (goal *problem*)))))

(defun facts-subsetp (fs1 fs2 comp1 comp2 attr1 attr2)
  (and (<= (length fs1) (length fs2))
       (subsetp fs1 fs2
                :test (lambda (a b)
                        (and (eqname a b)
                             (every (lambda (pa pb)
                                      (cond
                                        ((and (member pa comp1) (member pb comp2))
                                         (= (position pa comp1) (position pb comp2)))
                                        ((and (member pa attr1) (member pb attr2))
                                         (= (position pa attr1) (position pb attr2)))
                                        (t t)))
                                    (parameters a)
                                    (parameters b)))))))

(defun abstract-component-task<= (t1 t2)
  "Defines a relationship between the tasks.
 t1 <= t2 when attributes, inits, goals of t1 are the subset of that of t2."
  (ematch t1
    ((abstract-component-task
      (init i1) (goal g1)
      (ac (abstract-component (components comp1)
                              (attributes attr1)
                              (attribute-facts af1))))
     (ematch t2
       ((abstract-component-task
         (init i2) (goal g2)
         (ac (abstract-component (components comp2)
                                 (attributes attr2)
                                 (attribute-facts af2))))
        (and (facts-subsetp af1 af2 comp1 comp2 attr1 attr2)
             (facts-subsetp i1 i2 comp1 comp2 attr1 attr2)
             (facts-subsetp g1 g2 comp1 comp2 attr1 attr2)))))))

(defun abstract-component-task= (ac-t1 ac-t2)
  "Same abstract type and same set of attributes, as in KEPS."
  (and (abstract-component-task<= ac-t1 ac-t2)
       (abstract-component-task<= ac-t2 ac-t1)))

