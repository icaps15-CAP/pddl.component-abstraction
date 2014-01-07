(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun lpermutations-of (seq)
  "returns a lazy-list of sequences.
   each sequence is a permutation of the original sequence."
  (etypecase seq
    (cons (%lpermutations-of-cons seq))))

(defcached %lpermutations-of-cons (seq)
  (match seq
    ((list _)
     (llist seq))
    ((list* head rest-seq)
     (let* ((len (length seq))
            (n (1- len))) ; n -- 2, length -- 3
       (labels 
           ((insert (subperm pos)
              (let (acc (%subperm subperm))
                (dotimes (i pos)        ; i = 0
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (push head acc); pos = 1
                (dotimes (i (- n pos))  ; 2 - 1 = 1
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (nreverse acc)))
            (forward (subperms pos)
              (match subperms
                ((lcons subperm rest)
                 (if (< pos n)
                     (lcons (insert subperm pos) (forward subperms (1+ pos)))
                     (lcons (insert subperm pos) (backward rest pos))))))
            (backward (subperms pos)
              (match subperms
                ((lcons subperm rest)
                 (if (< 0 pos)
                     (lcons (insert subperm pos) (backward subperms (1- pos)))
                     (lcons (insert subperm pos) (forward rest pos)))))))
         (forward (%lpermutations-of-cons rest-seq) 0))))))

(declaim (ftype (function (fixnum fixnum) list) mappings))

@export
(defun mappings (from to)
  (assert (<= from to))
  (let (acc)
    (map-permutations
     (lambda (der)
       (push (lambda (cs1 cs2 c)
               (nth (nth (position c cs1) der) cs2))
             acc))
     (iota to) :length from)
    acc))

@export
(defun abstract-type= (ac1 ac2)
  (and (abstract-type<= ac1 ac2)
       (abstract-type<= ac2 ac1)))

@export
(defun abstract-type<=> (ac1 ac2)
  "Returns true when ac1 <= ac2 or ac1 => ac2 in a sense of abstract-type-<="
  (or (abstract-type<= ac1 ac2)
      (abstract-type<= ac2 ac1)))

@export
(defun abstract-type<= (ac1 ac2)
  "Returns true when the structure of ac1 is a subgraph of that of ac2"
  (match ac1
    ((abstract-component (facts fs1) (components cs1))
     (match ac2
       ((abstract-component (facts fs2) (components cs2))
        @type list cs1
        @type list cs2
        @type list fs1
        @type list fs2
        (and (<= (length cs1) (length cs2))
             (<= (length fs1) (length fs2))
             (some (lambda (p)
                     (let* ((p (curry p cs1 cs2)))
                       (and (every (lambda (c)
                                     (eq (type c) (type (funcall p c))))
                                   cs1)
                            (every (lambda (f1)
                                     (let ((mapped (mapcar p (parameters f1))))
                                       (some (lambda (f2)
                                               (and (eqname f1 f2)
                                                    (equalp mapped (parameters f2))))
                                             fs2)))
                                   fs1))))
                   (mappings (length cs1) (length cs2)))))))))

@export
(defun abstract-type< (ac1 ac2)
  "Returns true when the structure of ac1 is strictly a subgraph of that of ac2"
  (match ac1
    ((abstract-component (facts fs1) (components cs1))
     (match ac2
       ((abstract-component (facts fs2) (components cs2))
        @type list cs1
        @type list cs2
        @type list fs1
        @type list fs2
        (and (< (length cs1) (length cs2))
             (< (length fs1) (length fs2))
             (some (lambda (p)
                     (let* ((p (curry p cs1 cs2)))
                       (and (every (lambda (c)
                                     (eq (type c) (type (funcall p c))))
                                   cs1)
                            (every (lambda (f1)
                                     (let ((mapped (mapcar p (parameters f1))))
                                       (some (lambda (f2)
                                               (and (eqname f1 f2)
                                                    (equalp mapped (parameters f2))))
                                             fs2)))
                                   fs1))))
                   (mappings (length cs1) (length cs2)))))))))

