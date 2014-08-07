
(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

;;; utility function

(defun some-pair (fn list &key key)
  "assumes a symmetric function."
  (flet ((key (e) (if key (funcall key e) e)))
    (iter (for list1 on list)
          (for (e1 . list2) = list1)
          (iter (for e2 in list2)
                (when (funcall fn (key e1) (key e2))
                  (return-from some-pair
                    (values t e1 e2)))))))

;; (some-pair (lambda (a b) (= a (* 2 b))) '(4 7 5 1 2 3)) => T, 4, 2

;;; extracting static predicates

(defun all-instantiated-types (problem)
  (remove-duplicates (mapcar #'type (objects problem))))

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

(defun fluent-predicate-p (predicate)
  (let ((*domain* (domain predicate)))
    (some (lambda (action)
            (let ((f (curry #'predicate-more-specific-p predicate))
                  (n (curry #'eqname predicate)))
              (or (some f (remove-if-not n (add-list action)))
                  (some f (remove-if-not n (delete-list action))))))
          (actions *domain*))))

(defun predicate-ignored-p-JAIR-2415 (predicate)
  "Ignores unary or 0-ary predicates"
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

(defun static-facts (problem)
  (remove-if (disjoin ;; (rcurry #'typep 'pddl-function-state)
                      #'fluent-predicate-p
                      #'predicate-ignored-p-JAIR-2415)
             (init problem)))

(defun static-predicates (problem)
  (remove-if (disjoin #'fluent-predicate-p
                      #'predicate-ignored-p-JAIR-2415
                      ;; (lambda (pred)
                      ;;   (some (curry (conjoin #'eqname #'predicate-agrees-p) pred)
                      ;;         (static-facts problem)))
                      )
             (all-instantiated-predicates problem)))

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

;;; cluster-objects

(defun %static-objects (static-facts)
  (remove-duplicates
   (mappend #'parameters static-facts)))

(defun %all-types (static-objects)
  (remove-duplicates
   (mapcar #'type static-objects)))

(defun cluster-objects (static-facts
                        static-predicates)
  ;; literally following the formal expression by adi botea
  ;; 全部の順番からやってみて、ダメだったら最初からやり直しというアルゴリズム
  ;; greedy hill-climbingか
  (iter (with static-objects = (%static-objects static-facts))
        (for all-types on (%all-types static-objects))
        (collect (cluster-objects-with-seed
                  (car all-types)
                  (cdr all-types)
                  static-objects
                  static-facts static-predicates))))

;;;; cluster-objects-with-seed

(defun cluster-objects-with-seed
    (seed other-types static-objects static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (format t "~&~2tComponent Abstraction search with seed = ~a" seed)
  (iter
    (with acs = nil)
    (with closed = nil)
    (with tried-preds = nil)
    (finally (return acs))
    (for remaining = (set-difference other-types closed))
    (format t "~&~4tRemaining: ~a" remaining)
    (format t "~&~4tClosed: ~a" closed)
    (format t "~&~4tEstablished Abstract Components:~{~&~6t~s~}" (or acs '(:nothing)))
    (while remaining)
    (for type first seed then (first remaining))
    (iter (with open = nil)
          (initially
           (push type open)
           (let ((static-objects-of-type
                  (remove-if-not (rcurry #'pddl-typep type) static-objects)))
             (iter (for o in static-objects-of-type)
                   (push (make-abstract-component :components (list o)
                                                  :seed o)
                         acs))))
          (while open)
          (for t1 = (pop open))
          (push t1 closed)
          (format t "~%~6tOpening : t1 = ~a" t1)
          (iter (for p in (set-difference
                           (remove-if-not
                            (lambda (pred) (find t1 (parameters pred) :key #'type))
                            static-predicates)
                           tried-preds))
                (format t "~%~8tAll predicates tried: ~a" tried-preds)
                (push p tried-preds)
                (format t "~%~8tTrying a predicate: ~a" p)
                (for p-facts =
                     (remove-if-not
                      (curry (conjoin #'eqname #'predicate-more-specific-p) p)
                      static-facts))
                (format t "~%~8tFacts of predicate ~a:~{~&~10t~s~}" p p-facts)
                (multiple-value-bind (connects? ac1 ac2)
                    (predicates-connect-components p-facts acs)
                  (if connects?
                      (progn
                        (format t "~%~8tAbove facts connects components: ~@{~&~10t~a~^,~}" ac1 ac2)
                        (setf acs (add-attributes p-facts acs)))
                      (progn
                        (format t "~%~8tExtending Components!")
                        (setf acs (extend-components p-facts acs))
                        (unionf open (set-difference
                                      (%all-types (parameters p))
                                      closed)))))))))

(defun static-fact-extends-ac-p (f ac)
  (intersection (parameters f) (parameters ac)))

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

;;;; predicates-connect-components

(define-local-function %add-new-nodes (ac)
  (iter (with nodes = (abstract-component-components ac))
        (for f in facts)
        (let ((fparams (parameters f)))
          (when (intersection fparams nodes)
            (unionf nodes fparams)))
        (finally (return nodes))))

(defun predicates-connect-components (facts acs)
  "Check if adding new edges results in sharing the node in some pair of
components"
  (more-labels () (%add-new-nodes)
    (some-pair
     #'intersection acs
     :key #'%add-new-nodes)))
