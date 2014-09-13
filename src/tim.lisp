
(in-package :pddl.component-abstraction)

;; type inference module

(defun type-facts (*problem*)
  (remove-if-not (lambda (p)
                   (some (curry #'eqname p)
                         (type-predicates (domain *problem*))))
                 (init *problem*)))

(defun appear-in-actions (predicate)
  (some (lambda (a)
          (find predicate (positive-preconditions a) :test #'eqname))
        (actions *domain*)))
(defun type-predicates (*domain*)
  (remove-if-not (conjoin (lambda (p) (= 1 (length (parameters p))))
                          (complement #'fluent-predicate-p)
                          #'appear-in-actions)
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

(defgeneric add-types (thing))
(defmethod add-types ((o pddl-domain))
  (add-types-to-domain o))
(defmethod add-types ((o pddl-problem))
  (add-types-to-problem o (domain o)))

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
        (collect
            (ematch typed-ps
              ((list* _ _ _)
               (format t "~&found an mix-in type, using only the first inheritance~%~a~%"
                       typed-ps)
               typed-ps)
              ((list x) x)
              ((list) p)))))

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
       (let ((objects (add-types-to-parameters
                       (types new-domain)
                       (pddl-action :parameters objects
                                    :precondition `(and ,@init)))))
         (values
          (shallow-copy
           *problem*
           :domain new-domain
           :objects (mapcar (lambda-match
                              ((list* x _ _) x)
                              (x x)) objects)
           :init (add-types-to-init-preserving-mixin
                  init new-domain objects))
          new-domain))))))

(defun add-types-to-init-preserving-mixin (init new-domain objects)
  (remove-if-not
   (lambda (fact)
     (if (some (curry #'eqname fact) (mapcar #'origin (types new-domain)))
         (some (lambda (o)
                 (and (eqname (type o) fact)
                      (eqname o (typepred-parameter fact))))
               (mappend (lambda-match
                          ((list* _ rest) rest)) objects))
         t))
   init))


;; we also have to implement edelkamp-helmert 1999


