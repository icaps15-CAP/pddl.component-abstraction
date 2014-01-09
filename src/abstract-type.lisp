(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

@export
(defun lpermutations (seq &key (length (length seq)))
  "returns a lazy-list of sequences.
   each sequence is a permutation of the original sequence."
  (etypecase seq
    (cons (let ((seq-size (length seq)))
            (if (or (null length)
                    (= length seq-size))
                (%lpermutations-of-cons seq seq-size)
                (mappend (lambda (subseq)
                           (%lpermutations-of-cons subseq length))
                         (combinations seq :length length)))))))

(defun %lpermutations-of-cons (seq length)
  (match seq
    ((list _)
     (llist seq))
    ((list* head rest-seq)
     (let* ((n (1- length))) ; n -- 2, length -- 3
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
         (forward (%lpermutations-of-cons rest-seq n) 0))))))

(declaim (ftype (function (fixnum fixnum) list) mappings))

@export
(defcached mappings (from to)
  (assert (<= from to))
  (when (< 7 to)
    (warn "The parameter `to`(=~a) is big. proceed with caution..." to))
  (let (acc)
    (map-permutations
     (lambda (der)
       (push (lambda (cs1 cs2 c)
               (nth (nth (position c cs1) der) cs2))
             acc))
     (iota to) :length from)
    acc))

@export
(defcached lmappings (from to)
  (assert (<= from to))
  (lmapcar (lambda (permutation)
             (lambda (cs1 cs2 c)
                     (nth (nth (position c cs1) permutation) cs2)))
           (lpermutations (iota to) :length from)))

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
             (block some
               (let ((cs1-categories (categorize cs1 :key #'type))
                     (cs2-categories (categorize cs2 :key #'type)))
                 (maphash
                  (lambda (key objs1)
                    (let ((objs2 (gethash key cs2-categories)))
                      ;;(format t "~a, ~a~%" objs1 objs2)
                      (or
                       (and
                        (<= (length objs1) (length objs2))
                        (fsome
                         (lambda (p)
                           (let* ((p (curry p objs1 objs2)))
                             (every
                              (lambda (f1)
                                (let ((mapped1
                                       ;; filtered first, then mapped to objs2
                                       (mapcar p (remove-if-not
                                                  (rcurry #'member objs1)
                                                  (parameters f1)))))
                                  (some (lambda (f2)
                                          (and (eqname f1 f2)
                                               (let ((mapped2 (remove-if-not
                                                               (rcurry #'member objs2)
                                                               (parameters f2))))
                                                 (equalp mapped1 mapped2))))
                                        fs2)))
                              fs1)))
                         (lmappings (length objs1) (length objs2))))
                       (return-from some nil))))
                  cs1-categories)
                 (return-from some t)))))))))

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
             (block some
               (let ((cs1-categories (categorize cs1 :key #'type))
                     (cs2-categories (categorize cs2 :key #'type)))
                 (maphash
                  (lambda (key objs1)
                    (let ((objs2 (gethash key cs2-categories)))
                      (or
                       (and
                        (<= (length objs1) (length objs2))
                        (fsome
                         (lambda (p)
                           (let* ((p (curry p objs1 objs2)))
                             (every
                              (lambda (f1)
                                (let ((mapped1
                                       ;; filtered first, then mapped to objs2
                                       (mapcar p (remove-if-not
                                                  (rcurry #'member objs1)
                                                  (parameters f1)))))
                                  (some (lambda (f2)
                                          (and (eqname f1 f2)
                                               (let ((mapped2 (remove-if-not
                                                               (rcurry #'member objs2)
                                                               (parameters f2))))
                                                 (equalp mapped1 mapped2))))
                                        fs2)))
                              fs1)))
                         (lmappings (length objs1) (length objs2))))
                       (return-from some nil))))
                  cs1-categories)
                 (return-from some t)))))))))

