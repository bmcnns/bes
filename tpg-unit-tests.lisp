(defparameter *test-genotype-1* '((R1 SIN OBS1)))
(defparameter *test-genotype-2* '((R1 SIN OBS2)))
(defparameter *test-genotype-3* '((R1 SIN OBS3)))
(defparameter *test-genotype-4* '((R1 SIN OBS4)))
(defparameter *test-genotype-5* '((R1 SIN OBS5)))
(defparameter *test-genotype-6* '((R1 SIN OBS6)))
(defparameter *test-genotype-7* '((R1 SIN OBS7)))
(defparameter *test-genotype-8* '((R1 SIN OBS8)))

(defparameter *test-program-1* (make-program :genotype *test-genotype-1*))
(defparameter *test-program-2* (make-program :genotype *test-genotype-2*))
(defparameter *test-program-3* (make-program :genotype *test-genotype-3*))
(defparameter *test-program-4* (make-program :genotype *test-genotype-4*))
(defparameter *test-program-5* (make-program :genotype *test-genotype-5*))
(defparameter *test-program-6* (make-program :genotype *test-genotype-6*))
(defparameter *test-program-7* (make-program :genotype *test-genotype-7*))
(defparameter *test-program-8* (make-program :genotype *test-genotype-8*))

(defparameter *test-learner-1* (make-learner :program *test-program-1* :action 'TRIANGLE))
(defparameter *test-learner-2* (make-learner :program *test-program-2* :action '(GOTO "TEAM-3")))
(defparameter *test-learner-3* (make-learner :program *test-program-3* :action 'SQUARE))
(defparameter *test-learner-4* (make-learner :program *test-program-4* :action 'CIRCLE))
(defparameter *test-learner-5* (make-learner :program *test-program-5* :action 'RHOMBUS))
(defparameter *test-learner-6* (make-learner :program *test-program-6* :action 'TRIANGLE))

(defparameter *test-team-1* (make-team :learners (list *test-learner-3* *test-learner-4* *test-learner-5*)))
(defparameter *test-team-2* (make-team :learners (list *test-learner-2* *test-learner-4* *test-learner-6*)))
(defparameter *test-team-3* (make-team :learners (list *test-learner-1* *test-learner-3*)))

(add-learner *test-learner-1*)
(add-learner *test-learner-2*)
(add-learner *test-learner-3*)
(add-learner *test-learner-4*)
(add-learner *test-learner-5*)
(add-learner *test-learner-6*)
(add-team *test-team-1*)
(add-team *test-team-2*)
(add-team *test-team-3*)

(defparameter *activate-l1* '(1 0 0 0 0 0 0 0))
(defparameter *activate-l2* '(0 1 0 0 0 0 0 0))
(defparameter *activate-l3* '(0 0 1 0 0 0 0 0))
(defparameter *activate-l4* '(0 0 0 1 0 0 0 0))
(defparameter *activate-l5* '(0 0 0 0 1 0 0 0))
(defparameter *activate-l6* '(0 0 0 0 0 1 0 0))
(defparameter *activate-l7* '(0 0 0 0 0 0 1 0))
(defparameter *activate-l8* '(0 0 0 0 0 0 0 1))

TEAM1->LEARNER3->PROGRAM3->


(defparameter *cycle*
  '(TPG
    (LEARNERS
     (LEARNER L1 ((R1 SIN OBS1)) (GOTO T2))
     (LEARNER L2 ((R1 SIN OBS2)) (GOTO T1)))
    (TEAMS
     (TEAM T1 L1)
     (TEAM T2 L2))))

(defparameter *no-cycle*
  '(TPG
    (LEARNERS
     (LEARNER L1 ((R1 SIN OBS1)) (GOTO T2))
     (LEARNER L2 ((R1 SIN OBS2)) 'UP))
    (TEAMS
     (TEAM T1 L1)
     (TEAM T2 L2))))
