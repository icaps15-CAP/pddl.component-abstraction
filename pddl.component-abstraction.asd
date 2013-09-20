#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#

#|
  Author: Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.component-abstraction-asd
  (:use :cl :asdf))
(in-package :pddl.component-abstraction-asd)

(defsystem pddl.component-abstraction
  :version "0.1"
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl
               :pddl.plan-optimizer
               :optima
               :alexandria
               :guicho-utilities
               :iterate)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "pddl.component-abstraction")
                 (:file "abstract-type"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op pddl.component-abstraction-test))))


(defmethod asdf:perform ((op asdf:test-op)
			 (system (eql (asdf:find-system :pddl.component-abstraction))))
  (funcall (find-symbol "RUN!" (find-package :fiveam)) :pddl.component-abstraction)
  t)