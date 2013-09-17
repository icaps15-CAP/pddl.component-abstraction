#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.component-abstraction
  (:use :cl :pddl :optima :alexandria :iterate
        :pddl.plan-optimizer
        :guicho-utilities)
  (:shadow :minimize :maximize))
(in-package :pddl.component-abstraction)

(cl-syntax:use-syntax :annot)
;; blah blah blah.

@export
(defun all-instantiated-types (problem)
  (remove-duplicates (mapcar #'type (objects problem))))

;; all-instantiated-types are already at the lowest level
;; @export
;; (defun lowest-level-types (problem)
;;   (let (acc)
;;     (dolist (type (sort (all-instantiated-types problem)
;;                         #'pddl-supertype-p)
;;              acc)
;;       (unless (some (rcurry #'pddl-supertype-p type) acc)
;;         (push type acc)))))

@export
(defun all-instantiated-predicates (*problem*)
  (let ((*domain* (domain *problem*)))
    (iter (for pred in (predicates *domain*))
          (appending
           (apply
            #'map-product
            (lambda (&rest args)
              (pddl-predicate
               :name (name pred)
               :parameters
               (mapcar (lambda (type)
                         (pddl-variable :name (gensym (symbol-name (symbolicate '? (name type))))
                                        :type type))
                       args)))
            (mapcar
             (lambda (p)
               (remove-if-not
                (rcurry #'pddl-supertype-p (type p))
                (all-instantiated-types *problem*)))
             (parameters pred)))))))

@export
(defun fluent-predicate-p (predicate)
  (let ((*domain* (domain predicate)))
    (some (lambda (action)
            (let ((f (curry #'predicate-more-specific-p predicate))
                  (n (curry #'eqname predicate)))
              (or (some f (remove-if-not n (add-list action)))
                  (some f (remove-if-not n (delete-list action))))))
          (actions *domain*))))


@export
(defun predicate-ignored-p-JAIR-2415 (predicate)
  (or (= 0 (length (parameters predicate)))
      (= 1 (length (parameters predicate)))
      (and (< 1 (length (parameters predicate)))
           (block comb
             (map-combinations
              (lambda (list)
                (when (apply #'eq list)
                  (return-from comb t)))
              (mapcar #'type (parameters predicate)) :length 2)
             nil))))

@export
(defun static-facts (problem)
  (remove-if (disjoin (rcurry #'typep 'pddl-function-state)
                      #'fluent-predicate-p
                      #'predicate-ignored-p-JAIR-2415)
             (init problem)))

@export
(defun static-predicates (problem)
  (remove-if (disjoin #'fluent-predicate-p
                      #'predicate-ignored-p-JAIR-2415
                      ;; (lambda (pred)
                      ;;   (some (curry (conjoin #'eqname #'predicate-agrees-p) pred)
                      ;;         (static-facts problem)))
                      )
             (all-instantiated-predicates problem)))

(defstruct abstract-component
  (facts nil)
  (components nil))

(defmethod parameters ((ac abstract-component))
  (abstract-component-components ac))

(defmethod print-object ((ac abstract-component) s)
  (print-unreadable-object (ac s)
    (format s "(ABS ~w)" (parameters ac))))

(defun %constants (static-facts)
  (remove-duplicates
   (mappend #'parameters static-facts)))

(defun %all-types (constants)
  (remove-duplicates
   (mapcar #'type constants)))

@export
(defun cluster-objects (static-facts
                        static-predicates)
  ;; follow the expression by adi literally
  ;; 全部の順番からやってみて、ダメだったら最初からやり直しというアルゴリズム
  ;; greedy hill-climbingか
  (iter (with constants = (%constants static-facts))
        (for ac = nil)
        (for open = nil)
        (for closed = nil)
        (for tried = nil)
        (for type in (%all-types constants))
        (push type open)
        (iter (for o in (remove-if-not (rcurry #'pddl-typep type) constants))
              (push (make-abstract-component :components (list o)) ac))
        ;(break+ ac)
        (iter (while open)
              ;(break+ open closed tried)
              (for t1 = (pop open))
              (push t1 closed)
              (iter (for p in (set-difference
                               (remove-if-not
                                (lambda (pred)
                                  (some (rcurry #'pddl-typep t1)
                                        (parameters pred)))
                                static-predicates)
                               tried))
                    (push p tried)
                    (for p-facts = (remove-if-not
                                    (curry (conjoin
                                            #'eqname
                                            #'predicate-more-specific-p) p)
                                    static-facts))
                    (unless (predicate-connects-components p-facts ac)
                      (setf ac (extend-components p-facts ac))
                      (iter (for t2 in (remove-duplicates
                                        (mapcar #'type (parameters p))))
                            (unless (or (find t2 open)
                                        (find t2 closed))
                              (push t2 open))))))
        (collect ac)
        ;; (finding
        ;;  ac maximizing
        ;;  (reduce #'max ac :key (lambda (c)
        ;;                          (length
        ;;                           (remove-duplicates
        ;;                            (mapcar #'type (parameters c)))))))
        ;; (when (decomposition-satisfactory-p 4 ac)
        ;;   (return-from cluster-objects ac))
        ))

(defun static-fact-extends-ac-p (f ac)
  (intersection (parameters f) (parameters ac)))

(defun predicate-connects-components (facts ac)
  (some
   (lambda (f)
     (< 1 (length (remove-if-not (curry #'static-fact-extends-ac-p f) ac))))
   facts))

(defun extend-components (facts ac)
  (dolist (f facts)
    (match (find (curry #'static-fact-extends-ac-p f) ac)
      ((abstract-component (place facts) (place components))
       (push f facts)
       (unionf components (parameters f)))
      (nil
       (push (make-abstract-component
              :facts (list f)
              :components (parameters f))
             ac))))
  ac)

(defun decomposition-satisfactory-p (high ac)
  (every
   (lambda (component)
     (<= 2 (length (remove-duplicates (mapcar #'type (parameters component)))) high))
   ac))


