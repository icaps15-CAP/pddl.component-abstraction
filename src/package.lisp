#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#
(in-package :cl-user)
(defpackage pddl.component-abstraction
  (:use :cl :pddl :optima :alexandria :iterate
        :eazylazy
        :guicho-utilities)
  (:import-from :function-cache :defcached)
  (:shadow :minimize :maximize)
  (:export
   :predicates-connect-components
   :static-facts
   :static-predicates
   :cluster-objects
   :binarize-domain
   :binarize-problem
   :binarize
   :dereference-abstract-task-bucket
   :abstract-type=/fast
   :abstract-components-seed-only
   :abstract-tasks-seed-only
   :pick-those-from-seed-type
   :type-predicates
   :type-facts
   :add-types-to-domain
   :add-types-to-problem
   :add-types))
