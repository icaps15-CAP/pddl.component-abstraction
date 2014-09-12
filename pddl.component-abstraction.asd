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
               :eazylazy
               :eazylazy.optima
               :optima
               :bordeaux-threads
               :function-cache
               :alexandria
               :guicho-utilities
               :iterate)
  :components ((:module "src"
                :components
                ((:file :package)
                 (:file :static-predicates)
                 (:file :abstract-component)
                 (:file :cluster-objects)
                 (:file :abstract-type)
                 (:file :integrated)
                 (:file :for-component-planner)
                 (:file :tasks)
                 (:file :extract-tasks)
                 (:file :dereference)
                 (:file :tim))
                :serial t))
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
