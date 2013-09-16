#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.component-abstraction-test
  (:use :cl
        :pddl.component-abstraction
        :fiveam))
(in-package :pddl.component-abstraction-test)

(def-suite :pddl.component-abstraction)
(in-suite :pddl.component-abstraction)

