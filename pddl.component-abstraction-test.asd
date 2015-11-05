#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.component-abstraction-test-asd
  (:use :cl :asdf))
(in-package :pddl.component-abstraction-test-asd)

(defsystem pddl.component-abstraction-test
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl.component-abstraction
               :guicho-utilities
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "pddl.component-abstraction"))))
  :perform (test-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.component-abstraction)"))
		    (asdf:clear-system c)))
