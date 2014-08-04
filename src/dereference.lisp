
(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dereferenciation


(defvar *match* nil)

(define-condition parameter-not-found (condition)
  ((parameter :initarg :parameter)))

@export
(defun dereference-abstract-task-bucket (similar-tasks)
  "For a list of abstract component tasks, take the first one and make its
static-facts into the predicate. Since the type is explicitly specified,
the predicate is not identical to those in the domain description and
should be newly created."
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
