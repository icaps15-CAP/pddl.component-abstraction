
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

@export 'make-abstract-component

@export
(defstruct abstract-component
  seed
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
        (for ac = nil)                  ; (list abstract-component)
        (for open = nil)                ; (list type)
        (for closed = nil)              ; (list type)
        (for tried-preds = nil)         ; (list predicate)
        (for all-types on (%all-types constants))
        (iter
          (with ptype = nil)
          (for remaining =
               (set-difference
                all-types
                (remove-duplicates
                 (mapcar #'type (reduce #'union (mapcar #'parameters ac)
                                        :initial-value nil)))))
          (while remaining)
          (for type = (first remaining))
          (until (eq type ptype))
          (setf ptype type)
          ;; (break+ type ptype remaining)
          (push type open)
          (iter (for o in (remove-if-not (rcurry #'pddl-typep type) constants))
                (push (make-abstract-component
                       :components (list o)
                       :seed o) ac))
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
                      (unless (predicates-connect-components p-facts ac)
                        (setf ac (extend-components p-facts ac))
                        (iter (for t2 in (remove-duplicates
                                          (mapcar #'type (parameters p))))
                              (unless (or (find t2 open)
                                          (find t2 closed))
                                (push t2 open)))))))
        (collect ac)))

;; (finding
;;  ac maximizing
;;  (reduce #'max ac :key (lambda (c)
;;                          (length
;;                           (remove-duplicates
;;                            (mapcar #'type (parameters c)))))))
;; (when (decomposition-satisfactory-p 4 ac)
;;   (return-from cluster-objects ac))

(defun static-fact-extends-ac-p (f ac)
  (intersection (parameters f) (parameters ac)))

@export
(defun predicates-connect-components (facts ac)
  (format *standard-output*
          "~2&testing components of type ~w,~& with static facts of type ~w"
          (mapcar #'type (parameters (first ac)))
          (mapcar #'type (parameters (first facts))))
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
     (mapcar #'parameters ac)))
  nil)

(defun extend-components (facts ac)
  (terpri)
  (pprint-logical-block (*standard-output* facts :prefix "extending by:")
    (pprint-newline :mandatory)
    (dolist (f facts)
      (write f)
      (pprint-newline :mandatory)
      (match (find-if (curry #'static-fact-extends-ac-p f) ac)
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
               ac)))))
  ac)

(defun decomposition-satisfactory-p (high ac)
  (every
   (lambda (component)
     (<= 2 (length (remove-duplicates (mapcar #'type (parameters component)))) high))
   ac))


