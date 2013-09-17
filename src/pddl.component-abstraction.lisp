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


(defun build-static-graph ()
  )

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
(defun static-predicates (problem)
  (remove-if #'predicate-ignored-p-JAIR-2415
             (remove-if #'fluent-predicate-p
                        (all-instantiated-predicates problem))))

@export
(defun static-facts (problem)
  (remove-if #'predicate-ignored-p-JAIR-2415
             (remove-if #'fluent-predicate-p
                        (init problem))))

@export
(defun statically-connected-to (static-facts &rest objects)
  (some (lambda (pred)
          (every #'pddl-supertype-p
                 objects
                 (parameters pred)))
        static-facts))

