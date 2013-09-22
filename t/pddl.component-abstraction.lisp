#|
  This file is a part of pddl.component-abstraction project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.component-abstraction-test
  (:use :cl
        :pddl
        :pddl.component-abstraction
        :pddl.instances
        :repl-utilities
        :guicho-utilities
        :alexandria
        :iterate
        :fiveam)
  (:shadow :minimize :maximize))
(in-package :pddl.component-abstraction-test)

(def-suite :pddl.component-abstraction)
(in-suite :pddl.component-abstraction)

(test :predicate-connects-components
  (let* ((*domain* rover)
         (*problem* roverprob7126)
         (waypoint (object *problem* :waypoint0))

         (c0 (object *problem* :camera0))
         (r0 (object *problem* :rover0))
         (s0 (object *problem* :rover0store))
         (rc0 (pddl-atomic-state :name 'rc :parameters (list c0 r0)))
         (sr0 (pddl-atomic-state :name 'sr :parameters (list s0 r0)))
         (rw0 (pddl-atomic-state :name 'rw :parameters (list r0 waypoint)))
         (ac0 (make-abstract-component :components (list r0)))
         
         (c1 (object *problem* :camera1))
         (r1 (object *problem* :rover1))
         (s1 (object *problem* :rover1store))
         (rc1 (pddl-atomic-state :name 'rc :parameters (list c1 r1)))
         (sr1 (pddl-atomic-state :name 'sr :parameters (list s1 r1)))
         (rw1 (pddl-atomic-state :name 'rw :parameters (list r1 waypoint)))
         (ac1 (make-abstract-component :components (list r1)))
         
         (c2 (object *problem* :camera2))
         (r2 (object *problem* :rover2))
         (s2 (object *problem* :rover2store))
         (rc2 (pddl-atomic-state :name 'rc :parameters (list c2 r2)))
         (sr2 (pddl-atomic-state :name 'sr :parameters (list s2 r2)))
         (rw2 (pddl-atomic-state :name 'rw :parameters (list r2 waypoint)))
         (ac2 (make-abstract-component :facts (list sr2) :components (list r2 s2)))
         (ac3 (make-abstract-component :facts (list rc2) :components (list r2 c2)))
         
         (ac (list ac0 ac1)))

    (is-false (predicates-connect-components
               (list rc0 rc1) ac))

    ;; regression test
    (is-false (predicates-connect-components
               nil ac))

    (is-false (predicates-connect-components
               (list sr0 sr1) ac))
    
    (is-true (predicates-connect-components
               (list rw0 rw1) ac))

    (push c0 (parameters ac0))
    (push rc0 (abstract-component-facts ac0))
    (push c1 (parameters ac1))
    (push rc1 (abstract-component-facts ac1))
    
    (is-false (predicates-connect-components
               (list sr0 sr1) ac))
    
    (is-true (predicates-connect-components
               (list rw0 rw1) ac))

    (push s0 (parameters ac0))
    (push sr0 (abstract-component-facts ac0))
    (push s1 (parameters ac1))
    (push sr1 (abstract-component-facts ac1))
    
    (is-true (predicates-connect-components
              (list rw0 rw1) ac))

    (is-true (abstract-type= ac0 ac1))
    (is-true (abstract-type<= ac0 ac1))
    (is-true (abstract-type<= ac1 ac0))
    (is-true (abstract-type<=> ac1 ac0))
    (is-false (abstract-type< ac1 ac0))
    (is-false (abstract-type< ac0 ac1))

    ;; after the addition, ac0 > ac2
    (is-false (abstract-type= ac0 ac2))
    (is-true (abstract-type<= ac2 ac0))
    (is-false (abstract-type<= ac0 ac2))
    (is-true (abstract-type<=> ac2 ac0))
    (is-false (abstract-type< ac0 ac2))
    (is-true (abstract-type< ac2 ac0))


    (is-false (abstract-type= ac3 ac2))
    (is-false (abstract-type<= ac3 ac2))
    (is-false (abstract-type<= ac2 ac3))
    (is-false (abstract-type<=> ac2 ac3))
    (is-false (abstract-type< ac2 ac3))
    (is-false (abstract-type< ac3 ac2))
    ))

(test :cluster-objects
  (finishes
    (cluster-objects (static-facts roverprob3726)
                     (static-predicates roverprob3726))))