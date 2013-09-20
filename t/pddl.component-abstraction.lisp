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
        :alexandria
        :iterate
        :fiveam)
  (:shadow :minimize :maximize))
(in-package :pddl.component-abstraction-test)

(def-suite :pddl.component-abstraction)
(in-suite :pddl.component-abstraction)

(test predicate-connects-components
  (let* ((*domain* rover)
         (*problem* roverprob3726)
         (c0 (object *problem* :camera0))
         (r0 (object *problem* :rover0))
         (c1 (object *problem* :camera1))
         (r1 (object *problem* :rover1))
         (s0 (object *problem* :rover0store))
         (s1 (object *problem* :rover1store))
         (waypoint (object *problem* :waypoint0))

         (rc0 (pddl-atomic-state
              :parameters (list c0 r0)))
         (rc1 (pddl-atomic-state
              :parameters (list c1 r1)))
         (sr0 (pddl-atomic-state
              :parameters (list s0 r0)))
         (sr1 (pddl-atomic-state
              :parameters (list s1 r1)))
         (rw0 (pddl-atomic-state
              :parameters (list r0 waypoint)))
         (rw1 (pddl-atomic-state
              :parameters (list r1 waypoint)))
         (ac0 (make-abstract-component :components (list r0)))
         (ac1 (make-abstract-component :components (list r1)))
         (ac (list ac0 ac1)))

    (is-false (predicates-connect-components
               (list rc0 rc1) ac))

    (is-false (predicates-connect-components
               (list sr0 sr1) ac))
    
    (is-true (predicates-connect-components
               (list rw0 rw1) ac))

    (push c0 (parameters ac0))
    (push c1 (parameters ac1))
    
    (is-false (predicates-connect-components
               (list sr0 sr1) ac))
    
    (is-true (predicates-connect-components
               (list rw0 rw1) ac))))
               


(test cluster-objects
  (finishes
    (cluster-objects (static-facts roverprob3726)
                     (static-predicates roverprob3726))))