
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

(define-local-function %facts-instantiating (predicate)
  (remove-if-not
   (curry (conjoin #'eqname #'predicate-more-specific-p) predicate)
   static-facts))

(define-local-function %untried-predicates (t1)
  (iter (for p in static-predicates)
        (when (and (member t1 (parameters p) :key #'type)
                   (not (member p tried-preds)))
          (collect p))))

(define-local-function %collect-seed-components (type)
  (iter (for o in (remove-if-not (rcurry #'pddl-typep type) static-objects))
        (push (make-abstract-component :seed o :components (list o)) acs)))

(define-local-function %update-components (p p-facts open)
  (multiple-value-bind (connects? ac1 ac2)
      (predicates-connect-components p-facts acs)
    (if connects?
        (progn
          (format t "~%~8tAbove facts connects components: ~@{~&~10t~a~^,~}" ac1 ac2)
          (values (add-attributes p-facts acs) open))
        (progn
          (format t "~%~8tExtention Successful!")
          (values (extend-components p-facts acs)
                  (union open (set-difference (%all-types (parameters p)) closed)))))))

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
    (more-labels () (%facts-instantiating
                     %collect-seed-components
                     %untried-predicates
                     %update-components)
      (iter (with open = nil)
            (initially (push type open)
                       (%collect-seed-components type))
            (while open)
            (for t1 = (pop open))
            (push t1 closed)
            (format t "~%~6tOpening : t1 = ~a" t1)
            (iter (for p in (%untried-predicates t1))
                  (format t "~%~8tAll predicates tried: ~a" tried-preds)
                  (push p tried-preds)
                  (format t "~%~8tTrying a predicate: ~a" p)
                  (for p-facts = (%facts-instantiating p))
                  (format t "~%~8tFacts of predicate ~a:~{~&~10t~s~}" p p-facts)
                  (multiple-value-setq (acs open)
                    (%update-components p p-facts open)))))))

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
