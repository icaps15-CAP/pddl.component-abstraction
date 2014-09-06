(in-package :pddl.component-abstraction)

;; Avoid calling abstract-task= by assuming there are only the components
;; with the same seed types.

(defun abstract-tasks-seed-only (*problem* type) 
  "problem, pddl-type -> (list a-task) For each task, the core component is
grown from the given seed type.  However, it does not guarantee the task
eqality (because they might have the different sets of attibutes, init and
goals)"
  (mapcar (lambda (ac) (task ac *problem*))
          (reduce #'append (abstract-components-seed-only *problem* type)
                  :from-end t)))

(defun abstract-components-seed-only (*problem* seed-type)
  (let* ((sfs (static-facts *problem*))
         (objects (objects *problem*))
         (seed-type (query-type (domain *problem*) seed-type))
         (acs
          (cluster-objects-seed-only
           seed-type
           (remove seed-type (%all-types objects))
           objects
           sfs
           (static-predicates *problem*))))
    (format t "~&~a abstract components, no junks (seed only)" (length acs))
    (force-output)
    (categorize-by-equality acs #'abstract-type=/fast :transitive t)))

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
                  (union open (set-difference
                               (%all-types (parameters p))
                               closed)))))))

(defun cluster-objects-seed-only
    (seed other-types static-objects static-facts static-predicates)
  "static predicates are ungrounded while static facts are grounded."
  (declare (ignorable other-types))
  (format t "~&~2tComponent Abstraction search with seed = ~a" seed)
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
            (format t "~%~6tOpening : t1 = ~a" t1)
            (when-let ((t2 (prog1 (find-if (curry #'pddl-supertype-p t1)
                                           closed)
                             (push t1 closed))))
              (format t "~%~6tSupertype t2 = ~a is already checked." t2)
              (next-iteration))
            (iter (for p in (%untried-predicates t1))
                  (push p tried-preds)
                  (for p-facts = (%facts-instantiating p))
                  (when p-facts
                    ;(format t "~%~8tAll predicates tried: ~a" tried-preds)
                    (format t "~%~8tExtend: ~a facts / ~a -> ~a"
                            (length p-facts) (name t1) p)
                    ;; (format t "~%~8tFacts: ~a" p-facts)
                    (multiple-value-setq (acs open)
                      (%update-components* p p-facts open))))))
    (format t "~&~4t~a Established Abstract Components" (length acs))
    (format t "~&~4tof which there are ~a objects"
            (mapcar (compose #'length
                             #'abstract-component-components) acs))
    acs))

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

(defun static-fact-extends-ac-p* (f ac)
  "Returns true only when the extension is from the node that is already
added in the component"
  (intersection (parameters f) (parameters ac)))

(defun extend-components* (new-facts acs)
  (dolist (ac acs acs)
    (ematch ac
      ((abstract-component
        (facts (place facts))
        (components (place components)))
       (let ((extenders
              (remove-if-not (rcurry #'static-fact-extends-ac-p* ac)
                               new-facts)))
         ;;(format t "~&facts:~a~&c :~a" extenders ac)
         (appendf facts extenders)
         (unionf components
                 (reduce #'union (mapcar #'parameters extenders)
                         :initial-value nil)))))))

(defun bag-count-equal (list1 list2)
  (let ((alist1 (%bag-count-1 list1))
        (alist2 (%bag-count-1 list2)))
    (and (iter (for (key . count) in alist1)
               (for (key2 . count2) = (assoc key alist2))
               (always (and count2 (= count count2))))
         (iter (for (key . count) in alist2)
               (for (key2 . count2) = (assoc key alist1))
               (always (and count2 (= count count2)))))))

(defun %bag-count-1 (list)
  (let (plist)
    (dolist (e list (plist-alist plist))
      (incf (getf plist e 1)))))

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
             (bag-count-equal (mapcar #'name fs1) (mapcar #'name fs2))
             (bag-count-equal (mapcar (compose #'name #'type) cs1)
                              (mapcar (compose #'name #'type) cs2))
             (progn
               (when (< 3 (length cs1))
                 (format t "~&Heavy Comparison... ~a facts, ~a objs"
                         (length cs1) (length fs1))
                 (print (mapcar (compose #'name #'type) cs1))
                 (print (mapcar #'name fs1))
                 (print (mapcar (compose #'name #'type) cs2))
                 (print (mapcar #'name fs2))
                 (force-output))
               (reset-cache cs1 fs1 cs2 fs2)
               (%choose-object nil nil nil nil fs1 fs2 cs1 cs2))))))))

(defparameter *o-index-cache* (make-hash-table))
(defparameter *f-index-cache* (make-hash-table))
(defvar *of-table*)
(defun reset/hash (os1 os2 hash)
  (clrhash hash)
  (iter (for o in (append os1 os2))
        (for i from 0) (setf (gethash o hash) i)))
(defun reset-cache (cs1 fs1 cs2 fs2)
  (reset/hash cs1 cs2 *o-index-cache*)
  (reset/hash fs1 fs2 *f-index-cache*)
  (setf *of-table*
        (make-array
         (list (* 2 (length cs1)) (* 2 (length fs1)))
         :element-type 'fixnum
         :initial-element -1)))

(defun cached-position-param (o f)
  "same as (position o (parameters f)), but the result is cached
and returns -2 instead of NIL when position failed to find o."
  (let* ((oi (gethash o *o-index-cache*))
         (fi (gethash f *f-index-cache*))
         (cached (aref *of-table* oi fi)))
    (case cached
      ;; not computed.
      (-1 (let ((res (position o (parameters f))))
            (setf (aref *of-table* oi fi) (or res -2))))
      (t cached))))

(defun same-pos-if-appear (o1 o2 f1 f2)
  (= (cached-position-param o1 f1)
     (cached-position-param o2 f2)))

;; trivial version
;; (defun same-pos-if-appear (o1 o2 f1 f2)
;;   (eq (position o1 (parameters f1))
;;       (position o2 (parameters f2))))

;; (progn
;;   (sb-profile:reset)
;;   (sb-profile:unprofile)
;;   (sb-profile:profile %choose-fact %choose-object same-pos-if-appear))

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



