
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

@export 'make-abstract-component
@export '(abstract-component-facts
          abstract-component-components
          abstract-component-seed
          facts
          components
          seed)

@export
(defstruct abstract-component
  (seed nil)
  (facts nil)
  (components nil))

(defmethod parameters ((ac abstract-component))
  (abstract-component-components ac))
(defmethod (setf parameters) (new (ac abstract-component))
  (setf (abstract-component-components ac) new))

(defmethod print-object ((ac abstract-component) s)
  (print-unreadable-object (ac s)
    (format s "~<A-COMP ~;:objs ~w~_:seed ~w~;~:>"
            (list (parameters ac)
                  (abstract-component-seed ac)))))

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
        (for all-types on (%all-types constants))
        (collect (cluster-objects-with-seed
                  (car all-types)
                  (cdr all-types)
                  constants
                  static-facts static-predicates))))

(defun cluster-objects-with-seed
    (seed other-types constants static-facts static-predicates)
  (iter
    (with acs = nil)
    (with open = nil)
    (with closed = nil)
    (with tried-preds = nil)
    (with ptype = nil)
    (for remaining = (set-difference other-types closed))
    (while remaining)
    (for type first seed then (first remaining))
    (until (eq type ptype))
    (setf ptype type)
    (push type open)
    (iter (for o in (remove-if-not (rcurry #'pddl-typep type) constants))
          (push (make-abstract-component :components (list o)
                                         :seed o) acs))
    (iter (while open)
          (for t1 = (pop open))
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
                (unless (predicates-connect-components p-facts acs)
                  (setf acs (extend-components p-facts acs))
                  (iter (for t2 in (remove-duplicates
                                    (mapcar #'type (parameters p))))
                        (unless (or (find t2 open)
                                    (find t2 closed))
                          (push t2 open))))))
    (finally (return acs))))

(defun static-fact-extends-ac-p (f ac)
  (intersection (parameters f) (parameters ac)))

@export
(defun predicates-connect-components (facts acs)
  (when (and facts acs)
    (format *standard-output*
            "~2&testing components of type ~w,~& with static facts of type ~w"
            (mapcar #'type (parameters (first acs)))
            (mapcar #'type (parameters (first facts)))))
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
     (mapcar #'parameters acs)))
  nil)

(defun extend-components (facts acs)
  (terpri)
  (pprint-logical-block (*standard-output* facts :prefix "extending by:")
    (pprint-newline :mandatory)
    (dolist (f facts)
      (write f)
      (pprint-newline :mandatory)
      (match (find-if (curry #'static-fact-extends-ac-p f) acs)
        ((abstract-component (facts (place facts)) (components (place components)))
         (push f facts)
         (unionf components (parameters f)))
        (nil
         (format t "~%~4tneither c1 nor c2 are part of a previously built component,
~4ta new component containing f , c1 and c2.
~4t ~w" f)
         (push (make-abstract-component
                :facts (list f)
                :components (parameters f))
               acs)))))
  acs)

(defun decomposition-satisfactory-p (high acs)
  (every
   (lambda (component)
     (<= 2 (length (remove-duplicates (mapcar #'type (parameters component)))) high))
   acs))


