
(in-package :pddl.component-abstraction)

;; type inference module

(defun type-facts (*problem*)
  (remove-if-not (conjoin (lambda (p) (= 1 (length (parameters p))))
                          (complement #'fluent-predicate-p))
                 (init *problem*)))

(defun type-predicates (*domain*)
  (remove-if-not (conjoin (lambda (p) (= 1 (length (parameters p))))
                          (complement #'fluent-predicate-p))
                 (predicates *domain*)))

(defun typepred-parameter (pred)
  "Returns its only parameter"
  (match pred
    ((pddl-predicate (parameters (list p))) p)))

(defun parameters-of-type (pred action)
  (mapcar #'typepred-parameter
          (remove-if-not (curry #'eqname pred)
                         (positive-preconditions action))))

(defun typepred-subtypep (p1 p2 &aux (*domain* *domain*))
  (every (lambda (a)
           (subsetp ;; <
            (parameters-of-type p1 a)
            (parameters-of-type p2 a)))
         (actions *domain*)))

(define-pddl-class inferred-type (pddl-type)
  (origin))

(defun infer-type (pred)
  (inferred-type :name (name pred) :origin pred))

(defun hierachical-types (typepreds)
  (iter (with types = (mapcar #'infer-type typepreds))
        (for (t1 . rest) on types)
        (iter (for t2 in rest)
              (cond
                ((and (typepred-subtypep (origin t1) (origin t2))
                      (not (typepred-subtypep (origin t2) (origin t1))))
                 (setf (type t1) t2))
                ((and (typepred-subtypep (origin t2) (origin t1))
                      (not (typepred-subtypep (origin t1) (origin t2))))
                 (setf (type t2) t1))
                ;; ((and (typepred-subtypep (origin t2) (origin t1))
                ;;       (typepred-subtypep (origin t1) (origin t2)))
                ;;  ;; == duplicated types
                ;;  nil)
                ;; ((and (not (typepred-subtypep (origin t2) (origin t1)))
                ;;       (not (typepred-subtypep (origin t1) (origin t2))))
                ;;  ;; == irrelevant types
                ;;  nil)
                ))
        (finally (return types))))

(defun add-types-to-parameters (types action)
  (iter (for p in (parameters action))
        (for typed-ps =
             (iter (for t1 in types)
                   (when (and (every (lambda (t2)
                                       (not (typepred-subtypep
                                             (origin t2) (origin t1))))
                                     (remove t1 types))
                              (find-if 
                               (lambda-match
                                 ((pddl-predicate
                                   :name (eq (name t1))
                                   :parameters (list (eq p))) t))
                               (positive-preconditions action))
                              )
                     ;; TODO may miss duplicated p?
                     (collecting
                      (shallow-copy p :type t1)))))
        ;;(break+ types action p typed-ps)
        (collect
            (ematch typed-ps
              ((list x) x)
              ((list) p)
              ((list* x _ _)
               (warn "found an mix-in type, using only the first inheritance")
               x)))))

(defun add-types-to-action (types action)
  (ematch action
    ((pddl-action positive-preconditions)
     (change-class
      (ground-action
       (shallow-copy
        action :precondition
        `(and ,@(set-difference
                 positive-preconditions
                 (mapcar #'origin types)
                 :test #'eqname)))
       (add-types-to-parameters types action))
      'pddl-action))))

(defun add-types-to-domain (*domain*)
  (ematch *domain*
    ((pddl-domain types predicates actions)
     (assert (null types) ()
             "Currently we assume the domain is completely untyped!")
     (let* ((tps (type-predicates *domain*))
            (ts (hierachical-types tps)))
       (shallow-copy
        *domain*
        :predicates (set-difference predicates tps)
        :types ts
        :actions (mapcar (curry #'add-types-to-action ts) actions))))))


(defun add-types-to-problem (*problem* *domain*)
  (let ((new-domain (add-types-to-domain *domain*)))
    (ematch *problem*
      ((pddl-problem init objects)
       (values
        (shallow-copy
         *problem*
         :domain new-domain
         :init (set-difference init
                               (mapcar #'origin (types new-domain))
                               :test #'eqname)
         :objects (add-types-to-parameters
                   (types new-domain)
                   (pddl-action :parameters objects
                                :precondition `(and ,@init))))
        new-domain)))))

                  


;; we also have to implement edelkamp-helmert 1999


