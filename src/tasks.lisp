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
(defstruct abstract-component-task
  init goal ac)

@export '(abstract-component-task-init
          abstract-component-task-goal
          abstract-component-task-ac
          make-abstract-component-task)

(defun print-ac-slot (s ac name body)
  (format s "~w " name)
  (let ((*print-escape* nil))
    (pprint-logical-block (s body :prefix "(" :suffix ")")
      (loop
         (let ((f (pprint-pop)))
           (format s "(~a~{~^ ~a~})"
                   (name f)
                   (mapcar #'name 
                           (set-difference (parameters f)
                                           (parameters ac))))
           (pprint-exit-if-list-exhausted)
           (write-char #\Space s)
           (pprint-newline :fill s))))))

(defmethod print-object ((ac-task abstract-component-task) s)
  (print-unreadable-object (ac-task s :type t)
    (with-slots (ac init goal) ac-task
      (pprint-logical-block (s nil)
        (print-ac-slot s ac :init init)
        (pprint-newline :linear s)
        (print-ac-slot s ac :goal goal)))))

@export
(defun task (ac *problem*)
  (make-abstract-component-task
   :ac ac
   :init (facts-concerning ac (init *problem*))
   :goal (facts-concerning ac (positive-predicates (goal *problem*)))))

@export
(defun abstract-component-task<= (ac-t1 ac-t2)
  (with-slots ((i1 init) (g1 goal) (ac1 ac)) ac-t1
    (with-slots ((i2 init) (g2 goal) (ac2 ac)) ac-t2
      (and (<= (length i1) (length i2))
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


@export
(defun abstract-tasks (*problem* type)
  (let ((type (query-type (domain *problem*) type)))
    (categorize-by-equality
     (mapcar (rcurry #'task *problem*)
             (find-if
              (lambda (acs)
                (some (lambda (ac)
                        (eq type (type (abstract-component-seed ac))))
                      acs))
              (abstract-components-with-seed *problem* type)))
     #'abstract-component-task<=> :transitive nil)))

(defvar *match* nil)

(define-condition parameter-not-found (condition)
  ((parameter :initarg :parameter)))

@export
(defun dereference-abstract-task-bucket (similar-tasks)
  (match (first (sort similar-tasks #'abstract-component-task>=))
    ((abstract-component-task ac init goal)
     (let* ((*match* nil)
            (some-object (find 'pddl-object
                               (abstract-component-components ac)
                               :key #'type-of))
            (*domain* (domain some-object))
            (*problem* (problem some-object)))
       (handler-bind ((parameter-not-found
                       (lambda (c)
                         (with-slots (parameter) c
                           (setf (getf *match* parameter)
                                 (pddl-variable
                                  :name (symbolicate
                                         '? (gensym
                                             (string-upcase
                                              (name (type parameter)))))
                                  :type (type parameter)))
                           (continue)))))
         (make-abstract-component-task
          :ac (dereference-abstract-component ac)
          :init (mapcar #'dereference-predicate init)
          :goal (mapcar #'dereference-predicate goal)))))))

(defun dereference-abstract-component (ac)
  (let ((new-ac (make-abstract-component)))
    (iter (for p in (abstract-component-components ac))
          (for var = (pddl-variable :name (symbolicate
                                           '? (gensym
                                               (string-upcase
                                                (name (type p)))))
                                    :type (type p)))
          (setf (getf *match* p) var)
          (push var (abstract-component-components new-ac)))
    (setf (abstract-component-facts new-ac)
          (mapcar #'dereference-predicate
                  (abstract-component-facts ac)))
    new-ac))

(defun dereference-predicate (f)
  (match f
    ((pddl-atomic-state name parameters)
     (pddl-predicate
      :name name
      :parameters (mapcar (lambda (p)
                            (assert (getf *match* p) nil
                                    'parameter-not-found :parameter p)
                            (getf *match* p))
                          parameters)))
    ((pddl-function-state name parameters type)
     (pddl-function-state
      :name name
      :type type
      :parameters (mapcar (lambda (p)
                            (assert (getf *match* p) nil
                                    'parameter-not-found :parameter p)
                            (getf *match* p))
                          parameters)))))
    