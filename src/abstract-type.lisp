(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

(defun mappings (n)
  (let (acc)
    (map-permutations
     (lambda (der)
       (push (lambda (cs1 cs2 c)
               (nth (nth (position c cs1) der) cs2))
             acc))
     (iota n))
    acc))

@export
(defun abstract-type= (ac1 ac2)
  (match ac1
    ((abstract-component (facts fs1) (components cs1))
     (match ac2
       ((abstract-component (facts fs2) (components cs2))
        @type list cs1
        @type list cs2
        @type list fs1
        @type list fs2
        (and (= (length cs1) (length cs2))
             (= (length fs1) (length fs2))
             (some (lambda (p)
                     (let* ((p (curry p cs1 cs2))
                            (~p (lambda (c2)
                                  (find-if (lambda (c1)
                                             (eq c2 (funcall p c1)))
                                           cs1))))
                       
                       (and (every (lambda (c)
                                     (eq (type c) (type (funcall p c))))
                                   cs1)
                            (every (lambda (f1)
                                     (let ((mapped (mapcar p (parameters f1))))
                                       (some (lambda (f2)
                                               (and (eqname f1 f2)
                                                    (equalp mapped (parameters f2))))
                                             fs2)))
                                   fs1)
                            (every (lambda (f2)
                                     (let ((mapped (mapcar ~p (parameters f2))))
                                       (some (lambda (f1)
                                               (and (eqname f1 f2)
                                                    (equalp mapped (parameters f1))))
                                             fs1)))
                                   fs2))))
                   (mappings (length cs1)))))))))


