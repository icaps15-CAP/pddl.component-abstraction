(in-package :pddl.component-abstraction)

;; Avoid calling abstract-task= by assuming there are only the components
;; with the same seed types.

(defun abstract-tasks-seed-only (*problem* type) 
  "problem, pddl-type -> (list a-task) For each task, the core component is
grown from the given seed type.  However, it does not guarantee the task
eqality (because they might have the different sets of attibutes, init and
goals)"
  (mapcar (lambda (ac) (task ac *problem*))
          (reduce #'append (abstract-components-seed-only *problem* type))))

(defun abstract-components-seed-only (*problem* seed-type)
  (let* ((sfs (static-facts *problem*))
         (static-objects (%static-objects sfs))
         (acs
          (cluster-objects-seed-only
           (query-type (domain *problem*) seed-type)
           (remove seed-type (%all-types static-objects))
           static-objects
           sfs
           (static-predicates *problem*))))
    (format t "~&~a abstract components, no junks (seed only)" (length acs))
    (categorize-by-equality acs #'abstract-type=/fast :transitive nil)))

(define-local-function %update-components* (p p-facts open)
  (multiple-value-bind (connects? ac1 ac2)
      (predicates-connect-components p-facts acs)
    (declare (ignore ac1 ac2)) ;; these might be useful again while debugging
    (if connects?
        (progn
          (format t ": Fail")
          (values (add-attributes* p-facts acs) open))
        (progn
          (format t ": Success")
          (values (extend-components* p-facts acs)
                  (union open (set-difference (%all-types (parameters p)) closed)))))))

(defun cluster-objects-seed-only
    (seed other-types static-objects static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (declare (ignorable other-types))
  (format t "~&~2tComponent Abstraction search with seed = ~a" seed)
  (let ((*print-length* 5))
    (let* ((acs nil)
           (closed nil)
           (tried-preds nil)
           (type seed))
      (more-labels () (%facts-instantiating
                       %collect-seed-components
                       %untried-predicates
                       %update-components*)
        (iter (with open = nil)
              (initially (push type open)
                         (%collect-seed-components type))
              (while open)
              (for t1 = (pop open))
              (push t1 closed)
              (format t "~%~6tOpening : t1 = ~a" t1)
              (iter (for p in (%untried-predicates t1))
                    (push p tried-preds)
                    (for p-facts = (%facts-instantiating p))
                    (when p-facts
                      ;;(format t "~%~8tAll predicates tried: ~a" (mapcar #'name tried-preds))
                      (format t "~%~8tExtend: ~a facts / ~a" (length p-facts) p)
                      (multiple-value-setq (acs open)
                        (%update-components* p p-facts open))))))
      (format t "~&~4t~a Established Abstract Components" (length acs))
      acs)))

(defun add-attributes* (facts acs)
  (dolist (f facts)
    (ematch (find-if (curry #'static-fact-extends-ac-p f) acs)
      ((abstract-component (attribute-facts (place facts))
                           (attributes (place attrs))
                           components)
       (push f facts)
       (unionf attrs (set-difference (parameters f) components)))
      (nil nil)))
  acs)

(defun extend-components* (facts acs)
  (dolist (f facts)
    (ematch (find-if (curry #'static-fact-extends-ac-p f) acs)
      ((abstract-component (facts (place facts)) (components (place components)))
       (push f facts)
       (unionf components (parameters f)))
      (nil nil)))
  acs)

(defun abstract-type=/fast (ac1 ac2)
  "Returns true when the structure of ac1 is a subgraph of that of ac2.
Follow the description in macroff-JAIR05.
Do not run the brute force search -- choose wisely."
  (ematch ac1
    ((abstract-component (facts fs1) (components cs1))
     (ematch ac2
       ((abstract-component (facts fs2) (components cs2))
        (and (= (length cs1) (length cs2))
             (= (length fs1) (length fs2))
             (%choose-object nil nil nil nil fs1 fs2 cs1 cs2)))))))

(defun same-pos-if-appear (o1 o2 f1 f2)
  (eq (position o1 (parameters f1))
      (position o2 (parameters f2)))) 

;;;; imperative version
;; (defun %choose (fs1 fs2 cs1 cs2)
;;   )

;;;; recursive version
(defun %choose-object (os1 os2 checked1 checked2 unknown1 unknown2 cs1 cs2)
  (if (null cs1) ; (and (null cs2))
      t
      (iter outer
            (for o1 in cs1)
            (iter (for o2 in cs2)
                  (in outer
                      (thereis
                       (and (eq (type o1) (type o2))
                            (every (curry #'same-pos-if-appear o1 o2) checked1 checked2)
                            (%choose-fact (cons o1 os1) (cons o2 os2)
                                          checked1 checked2
                                          unknown1 unknown2
                                          (remove o1 cs1)
                                          (remove o2 cs2)))))))))

(defun %choose-fact (os1 os2 checked1 checked2 unknown1 unknown2 cs1 cs2)
  (or (iter outer
            (for f1 in unknown1)
            (iter (for f2 in unknown2)
                  (in outer
                      (thereis
                       (and (eqname f1 f2)
                            (every (rcurry #'same-pos-if-appear f1 f2) os1 os2)
                            (%choose-fact os1 os2
                                          (cons f1 checked1)
                                          (cons f2 checked2)
                                          (remove f1 unknown1)
                                          (remove f2 unknown2)
                                          cs1 cs2))))))
      (%choose-object os1 os2 checked1 checked2 unknown1 unknown2 cs1 cs2)))



