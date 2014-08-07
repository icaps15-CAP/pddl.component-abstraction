

(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

;;; abstract-component structure definition

(export
 '(make-abstract-component
   abstract-component-facts
   abstract-component-components
   abstract-component-attributes
   abstract-component-attribute-facts
   abstract-component-seed
   facts
   attributes
   components
   seed))

(defstruct abstract-component
  (problem *problem*)
  (seed nil)
  (facts nil)
  (components nil)
  (attributes nil)
  (attribute-facts nil))

(defmethod problem ((ac abstract-component))
  @inline abstract-component-problem
  (abstract-component-problem ac))
(defmethod domain ((ac abstract-component))
  @inline abstract-component-problem
  (domain (abstract-component-problem ac)))

(defmethod parameters ((ac abstract-component))
  (abstract-component-components ac))
(defmethod (setf parameters) (new (ac abstract-component))
  (setf (abstract-component-components ac) new))

(defun print-ac-slot-nonnil (s ac accessor name printer &optional first)
  (let ((value (funcall accessor ac)))
    (when value
      (unless first
        (pprint-newline :linear s))
      (funcall printer s ac name value))))

(defun printer1 (s ac name value)
  (format s "~w " name)
  (let ((*print-escape* nil))
    (pprint-logical-block (s value :prefix "(" :suffix ")")
      (loop
         (pprint-exit-if-list-exhausted)
         (let ((f (pprint-pop)))
           (write (name f) :stream s)
           (pprint-exit-if-list-exhausted)
           (write-char #\Space s)
           (pprint-newline :fill s)))))
  (write-char #\Space s))

(defun printer2 (s ac name value)
  (format s "~w ~w " name value))

(defmethod print-object ((ac abstract-component) s)
  (print-unreadable-object (ac s)
    (pprint-logical-block (s nil :prefix "A-COMP ")
      (print-ac-slot-nonnil s ac #'abstract-component-components :objs #'printer1 t)
      (print-ac-slot-nonnil s ac #'abstract-component-seed :seed #'printer2)
      (print-ac-slot-nonnil s ac #'abstract-component-attributes :attrs #'printer1))))
