
(in-package :pddl.component-abstraction)

(cl-syntax:use-syntax :annot)

@export
(defun all-instantiated-types (problem)
  (remove-duplicates (mapcar #'type (objects problem))))

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
  (match predicate
    ((pddl-atomic-state parameters)
     (or (< (length parameters) 2)
         (block comb
           (map-combinations
            (lambda (list)
              (when (apply #'eq list)
                (return-from comb t)))
            (mapcar #'type parameters) :length 2)
           nil)))
    ((pddl-function-state parameters)
     (or (< (length parameters) 1)
         (and (<= 2 (length parameters))
              (block comb
                (map-combinations
                 (lambda (list)
                   (when (apply #'eq list)
                     (return-from comb t)))
                 (mapcar #'type parameters) :length 2)
                nil))))))

@export
(defun static-facts (problem)
  (remove-if (disjoin ;; (rcurry #'typep 'pddl-function-state)
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

@export
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

(defun %constants (static-facts)
  (remove-duplicates
   (mappend #'parameters static-facts)))

(defun %all-types (constants)
  (remove-duplicates
   (mapcar #'type constants)))

@export
(defun cluster-objects (static-facts
                        static-predicates)
  ;; literally following the formal expression by adi botea
  ;; 全部の順番からやってみて、ダメだったら最初からやり直しというアルゴリズム
  ;; greedy hill-climbingか
  (iter (with constants = (%constants static-facts))
        (for all-types on (%all-types constants))
        (collect (cluster-objects-with-seed
                  (car all-types)
                  (cdr all-types)
                  constants
                  static-facts static-predicates))))

(defun cluster-objects-with-seed
    (seed other-types constants static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (iter
    (with acs = nil)
    (with open = nil)
    (with closed = nil)
    (with tried-preds = nil)
    (with ptype = nil)
    (initially
     (format t "~& initializing component abstraction search with seed = ~a" seed))
    (for remaining = (set-difference other-types closed))
    (format t "~& Remaining: ~a" remaining)
    (format t "~& Closed: ~a" closed)
    (while remaining)
    (for type first seed then (first remaining))
    (until (eq type ptype))
    (setf ptype type)
    (push type open)
    (let ((constants-of-type (remove-if-not (rcurry #'pddl-typep type) constants)))
      (iter (for o in constants-of-type)
            (push (make-abstract-component :components (list o)
                                           :seed o) acs)))
    (iter (while open)
          (for t1 = (pop open))
          (format t "~% opening : t1 = ~a" t1)
          (push t1 closed)
          (iter (for p in (set-difference
                           (remove-if-not
                            (lambda (pred)
                              (some (rcurry #'pddl-typep t1)
                                    (parameters pred)))
                            static-predicates)
                           tried-preds))
                (push p tried-preds)
                (for p-facts =
                     (remove-if-not
                      (curry (conjoin #'eqname #'predicate-more-specific-p) p)
                      static-facts))
                (if (predicates-connect-components p-facts acs)
                    (setf acs (add-attributes p-facts acs))
                    (progn
                      (setf acs (extend-components p-facts acs))
                      (iter (for t2 in (remove-duplicates
                                        (mapcar #'type (parameters p))))
                            (unless (or (find t2 open)
                                        (find t2 closed))
                              (push t2 open)))))))
    (finally (return acs))))

(defun static-fact-extends-ac-p (f ac)
  (intersection (parameters f) (parameters ac)))

@export
(defun predicates-connect-components (facts acs)
  (mapl
   (lambda (list)
     (destructuring-bind (a . rest) list
       (when (some (curry #'intersection a) rest)
         (return-from predicates-connect-components t))))
   (mapcar
     (lambda (ps)
       (reduce
        (lambda (ps f)
          (let ((fps (parameters f)))
            (if (intersection fps ps)
                (unionf fps ps)
                ps)))
        facts :initial-value ps))
     (mapcar #'abstract-component-components acs)))
  nil)

(defun add-attributes (facts acs)
  (dolist (f facts)
    (match (find-if (curry #'static-fact-extends-ac-p f) acs)
      ((abstract-component (attribute-facts (place facts))
                           (attributes (place attrs))
                           components)
       (push f facts)
       (unionf attrs (set-difference (parameters f) components)))))
  acs)

(defun extend-components (facts acs)
  (dolist (f facts)
    (match (find-if (curry #'static-fact-extends-ac-p f) acs)
      ((abstract-component (facts (place facts)) (components (place components)))
       (push f facts)
       (unionf components (parameters f)))
      (nil
       (push (make-abstract-component
              :facts (list f)
              :components (parameters f))
             acs))))
  acs)

(defun decomposition-satisfactory-p (high acs)
  (every
   (lambda (component)
     (<= 2 (length (remove-duplicates (mapcar #'type (parameters component)))) high))
   acs))


