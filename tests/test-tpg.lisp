(in-package :bes)

(deftest correct-routes-are-taken
  "Test that the correct routes are taken when evaluating TPG."
  (let ((tpg '(TPG
              (LEARNERS
               (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
               (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (GOTO T3))
               (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) SQUARE)
               (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) CIRCLE)
               (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) RHOMBUS)
               (LEARNER L6 (PROGRAM P6 ((R1 SIN OBS6))) TRIANGLE))
              (TEAMS
               (TEAM T1 L3 L4 L5)
               (TEAM T2 L2 L4 L6)
               (TEAM T3 L1 L3)))))
    (check
     (assert-equal (execute-team tpg 'T1 '(0 0 1 0 0 0)) 'SQUARE)
     (assert-equal (execute-team tpg 'T1 '(0 0 0 1 0 0)) 'CIRCLE)
     (assert-equal (execute-team tpg 'T1 '(0 0 0 0 1 0)) 'RHOMBUS)
     (assert-equal (execute-team tpg 'T2 '(0 0 0 0 0 1)) 'TRIANGLE)
     (assert-equal (execute-team tpg 'T2 '(1 1 0 0 0 0)) 'TRIANGLE)
     (assert-equal (execute-team tpg 'T2 '(0 1 1 0 0 0)) 'SQUARE)
     (assert-equal (execute-team tpg 'T3 '(1 0 0 0 0 0)) 'TRIANGLE)
     (assert-equal (execute-team tpg 'T3 '(0 0 1 0 0 0)) 'SQUARE))))
     
(deftest unused-learners-are-deleted-during-cleanup
  "Check that LEARNERS that are not referenced by any TEAMS are removed
   when CLEANUP-LEARNERS is called."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) (GOTO T1))
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) CIRCLE)
                (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) RHOMBUS))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L5)))))
    (let ((learners-exist-before-clean-up nil)
          (learners-exist-after-clean-up nil))
      (setf learners-exist-before-clean-up (and
                                            (member 'L3 (mapcar #'learner-id (learners tpg)))
                                            (member 'L4 (mapcar #'learner-id (learners tpg)))))
      (setf learners-exist-after-clean-up (and
                                           (member 'L3 (mapcar #'learner-id (learners (remove-dangling-learners tpg))))
                                           (member 'L4 (mapcar #'learner-id (learners (remove-dangling-learners tpg))))))
      (check
       (assert-true learners-exist-before-clean-up)
       (assert-false learners-exist-after-clean-up)))))

(deftest allow-discrete-outputs
  "Test that TPG allows discrete action outputs for programs."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE))
               (TEAMS
                (TEAM T1 L1)))))
    (assert-equal (execute-team tpg 'T1 '(1 0 0)) 'TRIANGLE)))
  
(deftest allow-multi-action-continuous-outputs
  "Test that TPG supports multiple continuous outputs for a single action."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (0.1321 0.3472 0.127783 0.2378)))
               (TEAMS
                (TEAM T1 L1)))))
    (assert-equal (execute-team tpg 'T1 '(1 0 0)) '(0.1321 0.3472 0.127783 0.2378))))

(deftest allow-action-programs
  "Test that TPG supports action programs."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (PROGRAM P2 ((R1 SIN OBS1)))))
               (TEAMS
                (TEAM T1 L1)))))
    (assert-equal (execute-team tpg 'T1 '(1 0 0)) '(0.84147096 0.0 0.0))))

(deftest mutating-tpg-results-in-always-different-learners
  "Test that mutating a TPG always results in a functional
   difference to the LEARNERs belonging to the TPG."
  ;; This would be true iff mutate-learner-probability is 1.0
  ;; and mutate-team-probability is 1.0
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg
          '(TPG
            (LEARNERS
             (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
             (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE)
             (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE)
             (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) RHOMBUS))
            (TEAMS
             (TEAM T1 L1 L2)
             (TEAM T2 L3 L4)
             (TEAM T2 L2 L3)))))
    (setf (experiment-mutate-team-probability *experiment*) 1.0)
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (assert-always 100
      (let ((mutated-tpg (mutate-tpg tpg)))
        (not (equal (learners mutated-tpg) (learners tpg)))))))

(deftest mutating-tpg-results-in-always-different-teams
  "Test that mutating a TPG always results in a functional
   difference to the TEAMs belonging to the TPG."
  ;; This would be true iff mutate-learner-probability is 1.0
  ;; and mutate-team-probability is 1.0
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg
          '(TPG
            (LEARNERS
             (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
             (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE)
             (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE)
             (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) RHOMBUS))
            (TEAMS
             (TEAM T1 L1 L2)
             (TEAM T2 L3 L4)
             (TEAM T2 L2 L3)))))
    (setf (experiment-mutate-team-probability *experiment*) 1.0)
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (assert-always 100
      (let ((mutated-tpg (mutate-tpg tpg)))
        (not (equal (teams mutated-tpg) (teams tpg)))))))

;; mutating tpg never introduces a cycle
        
(deftest mutating-team-never-exceeds-maximum-learner-count
  "Test that adding a LEARNER to a TEAM will never exceed
   the maximum LEARNER count for that team."
  (let* ((*experiment* (copy-experiment *experiment*))
         (tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) 'SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) 'TRIANGLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) 'CIRCLE))
                (TEAMS
                 (TEAM T1 L1 L2)))))
    (setf (experiment-maximum-number-of-learners *experiment*) 2)
    (check
     (assert-equal (add-learner tpg '(TEAM T1 L1 L2)) '(TEAM T1 L1 L2) ))))

(deftest mutating-team-never-leaves-team-empty
  (let ((tpg
          '(TPG
            (LEARNERS
             (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE))
            (TEAMS
             (TEAM T1 L1)))))
    (assert-always 1000
      (> (length (team-learners (mutate-team tpg '(TEAM T1 L1)))) 0))))
  
(deftest adding-learner-never-results-in-duplicates
  "Test that adding a LEARNER will never result in duplicate learners on the same team."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) 'SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R2 SIN OBS2))) 'TRIANGLE))
               (TEAMS
                (TEAM T1 L1 L2)))))
    (assert-equal (add-learner tpg '(TEAM T1 L1 L2) ) '(TEAM T1 L1 L2) )))

(deftest mutate-learner-action-has-correct-frequency
  "Test that the LEARNER's action is mutated with the frequency
   defined by (1 - MUTATE-LEARNER-PROGRAM-VS-ACTION-PROBABILITY) in *EXPERIMENT*."
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L2 L3))))
        (mutate-action-probability (- 1 (experiment-mutate-learner-program-vs-action-probability *experiment*))))
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (assert-frequency 1000 mutate-action-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner original-learner-id) (mutate-learner tpg '(TEAM T1 L1 L2 L3) :return-original-learner-id t)
        (declare (ignore mutated-team))
        (not (equal (learner-action (find-learner-by-id tpg original-learner-id)) (learner-action mutated-learner)))))))
            
(deftest mutate-learner-action-has-correct-atomic-frequency
  "Test that the LEARNER's action is atomic after mutation with the frequency
   defined by LEARNER-ATOMIC-ACTION-PROBABILITY in *EXPERIMENT*."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L2 L3))))
        (learner-atomic-action-probability (experiment-learner-atomic-action-probability *experiment*)))
        (assert-frequency 10000 learner-atomic-action-probability 0.05
          (atomic-p (mutate-learner-action tpg '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE) '(TEAM T1 L1 L2))))))

(deftest mutate-learner-action-has-correct-goto-frequency
  "Test that the LEARNER's action is a reference to another TEAM
   after mutation with the frequency defined by MUTATE-LEARNER-ACTION-PROBABILITY
   in *EXPERIMENT*."
  (let* ((*experiment* (copy-experiment *experiment*))
         (tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE)
                 (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) SQUARE))
                (TEAMS
                 (TEAM T1 L1)
                 (TEAM T2 L2)
                 (TEAM T3 L2 L3 L4))))
         (learner-atomic-action-probability (experiment-learner-atomic-action-probability *experiment*))
         (learner-goto-action-probability (- 1 learner-atomic-action-probability)))
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (setf (experiment-mutate-learner-program-vs-action-probability *experiment*) 0.0)
    (assert-frequency 1000 learner-goto-action-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner) (mutate-learner tpg '(TEAM T3 L2 L3 L4))
        (declare (ignore mutated-team))
        (if mutated-learner
            (reference-p (learner-action mutated-learner))
            nil)))))

(deftest mutate-learner-action-always-different
  "Test that the LEARNER'S ACTION is always different
   after a mutation."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L2 L3))))
        (team '(TEAM T1 L1 L2 L3))
        (learner '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)))
    (assert-always 1000
      (let ((mutated-learner (mutate-learner-action tpg learner team)))
        (not (equal (learner-action learner) (learner-action mutated-learner)))))))

(deftest mutate-learner-program-has-correct-frequency
  "Test that the LEARNER's PROGRAM is mutated with the frequency
   defined by MUTATE-LEARNER-PROGRAM-PROBABILITY in *EXPERIMENT*.
   To be consider MUTATED it must have a new ID and different INSTRUCTIONS."
  (let* ((*experiment* (copy-experiment *experiment*))
         (tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1)
                                          (R2 ADD R1 R3)
                                          (R3 SUB R2 OBS2)
                                          (R4 SIN 3.14)
                                          (R5 COS 3.14)
                                          (R5 MUL R4 R5))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS1))) TRIANGLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CRICLE))
                (TEAMS
                 (TEAM T1 L1 L2 L3))))
         (mutate-program-probability (experiment-mutate-learner-program-vs-action-probability *experiment*)))
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (assert-frequency 1000 mutate-program-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner original-learner-id) (mutate-learner tpg '(TEAM T1 L1 L2 L3) :return-original-learner-id t)
        (declare (ignore mutated-team))
        (if mutated-learner
            (let ((original-learner (find-learner-by-id tpg original-learner-id))
                  (program (learner-program mutated-learner)))
              (and (not (equal (program-id program) (program-id (learner-program original-learner))))
                   (not (equal (program-instructions program) (program-instructions (learner-program original-learner))))))
            nil)))))
        
(deftest mutate-team-has-correct-frequency 
  "Test that TEAMs are mutated with the frequency MUTATE-TEAM-PROBABILITY
   defined in *EXPERIMENT.*"
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) 'SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) 'TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) 'CIRCLE))
               (TEAMS
                (TEAM T1 L1 L2 L3))))
        (mutate-team-probability (experiment-mutate-team-probability *experiment*)))
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (assert-frequency 100 mutate-team-probability 0.05
      (not (equal (teams (mutate-tpg tpg)) (teams tpg))))))

(deftest add-learner-has-correct-frequency
  "TEST that LEARNERS are added according to ADD-LEARNER-PROBABILITY
   defined in *EXPERIMENT*."
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L2 L3))))
        (add-learner-probability (experiment-add-learner-probability *experiment*)))
    (setf (experiment-remove-learner-probability *experiment*) 0.0)
    (setf (experiment-mutate-learner-probability *experiment*) 0.0)
    (assert-frequency 1000 add-learner-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner) (mutate-team tpg '(TEAM T1 L1 L2))
        (declare (ignore mutated-learner))
        (equal (length (team-learners mutated-team)) 3)))))

(deftest remove-learner-has-correct-frequency
  "TEST that LEARNERS are removed according to REMOVE-LEARNER-PROBABILITY
   defined in *EXPERIMENT*."
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE))
               (TEAMS
                (TEAM T1 L1 L2 L3))))
        (remove-learner-probability (experiment-remove-learner-probability *experiment*)))
    (setf (experiment-add-learner-probability *experiment*) 0.0)
    (setf (experiment-mutate-learner-probability *experiment*) 0.0)
    (assert-frequency 1000 remove-learner-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner) (mutate-team tpg '(TEAM T1 L1 L2 L3))
        (declare (ignore mutated-learner))
        (equal (length (team-learners mutated-team)) 2)))))

(deftest mutate-learner-has-correct-frequency
  "TEST that LEARNERS are mutated according to MUTATE-LEARNER-PROBABILITY
   defined in *EXPERIMENT*."
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L2 L3))))
        (mutate-learner-probability (experiment-mutate-learner-probability *experiment*)))
    (setf (experiment-remove-learner-probability *experiment*) 0.0)
    (setf (experiment-add-learner-probability *experiment*) 0.0)
    (assert-frequency 1000 mutate-learner-probability 0.05
      (multiple-value-bind (mutated-team mutated-learner) (mutate-team tpg '(TEAM T1 L1 L2))
        (and
         (not (member mutated-learner (learners tpg)))
         (not (equal (team-learners mutated-team) '(L1 L2))))))))

(deftest find-team-by-id-returns-correct-id
  "FIND-TEAM-BY-ID finds the correct TEAM."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE))
                (TEAMS
                 (TEAM T1 L1 L3)
                 (TEAM T2 L2 L3)))))
    (assert-equal (find-team-by-id tpg 'T1) '(TEAM T1 L1 L3))))

(deftest find-team-by-id-returns-error-when-not-found
  "FIND-TEAM-BY-ID should make an error when the
   specified TEAM-ID is not found."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))
                (TEAMS
                 (TEAM T1 L1)))))
    (assert-error (find-team-by-id tpg 'T2)
                  :expects "Team not found.")))

(deftest find-learner-by-id-returns-correct-id
  "FIND-LEARNER-BY-ID finds the correct LEARNER."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE))
                (TEAMS
                 (TEAM T1 L1 L3)
                 (TEAM T2 L2 L3)))))
    (assert-equal (find-learner-by-id tpg 'L1) '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))))

(deftest find-learner-by-id-returns-error-when-not-found
  "FIND-LEARNER-BY-ID should make an error when the
   specified LEARNER-ID is not found."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))
                (TEAMS
                 (TEAM T1 L1)))))
    (assert-error (find-learner-by-id tpg 'L2)
                  :expects "Learner not found.")))

(deftest different-id-after-mutating-learner
  "Verify a LEARNER has a different ID after MUTATION."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE))
                (TEAMS
                 (TEAM T1 L1 L2)
                 (TEAM T2 L2 L3)))))
    (multiple-value-bind (mutated-team mutated-learner)
        (mutate-learner tpg '(TEAM T1 L1 L2 L3))
      (declare (ignore mutated-learner))
      (assert-true (> (length (set-difference (mapcar #'learner-id (learners tpg)) (team-learners mutated-team))) 0)))))

(deftest different-id-after-mutating-team
  "Verify a TEAM has a different ID after MUTATION."
  (let* ((*experiment* (copy-experiment *experiment*))
         (tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) 'SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) 'TRIANGLE)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) 'CIRCLE))
                (TEAMS
                 (TEAM T1 L1 L2)
                 (TEAM T2 L2 L3))))
         (team '(TEAM T1 L1 L2)))
    (setf (experiment-mutate-learner-probability *experiment*) 1.0)
    (multiple-value-bind (mutated-team mutated-learner)
        (mutate-team tpg team)
      (declare (ignore mutated-learner))
      (assert-not-equal (team-id team) (team-id mutated-team)))))

(deftest different-id-after-mutating-program
  "Verify a PROGRAM has a different ID after MUTATION."
  (let* ((program '(PROGRAM P1 ((R1 SIN OBS1))))
         (mutated-program (mutate-program program)))
    (assert-not-equal (program-id program) (program-id mutated-program))))
        
(deftest program-id-returns-correct-id
  "PROGRAM-ID returns the correct ID if a PROGRAM is provided."
  (check
   (assert-equal (program-id '(PROGRAM P1 ((R1 SIN OBS1)))) 'P1)))

(deftest program-id-returns-error-if-not-program
  "PROGRAM-ID returns an ERROR if something other than a PROGRAM is provided."
  (check
   (assert-error (program-id '((R1 SIN OBS1)))
                 :expects "PROGRAM-ID expects a PROGRAM.")))
  
(deftest program-instructions-returns-correct-instructions
  "PROGRAM-INSTRUCTIONS returns the correct instructions if a PROGRAM is provided."
  (check
   (assert-equal (program-instructions '(PROGRAM P1 ((R1 SIN OBS1)))) '((R1 SIN OBS1)))))

(deftest program-instructions-returns-error-if-not-program
  "PROGRAM-INSTRUCTIONS returns an ERROR if something other than a PROGRAM is provided."
  (check
   (assert-error (program-instructions '((R1 SIN OBS1)))
                 :expects "PROGRAM-INSTRUCTIONS expects a PROGRAM.")))

(deftest learner-program-returns-correct-program
  "LEARNER-PROGRAM returns the correct CONTEXT PROGRAM if a LEARNER is provided."
  (check
   (assert-equal (learner-program '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))
                 '(PROGRAM P1 ((R1 SIN OBS1))))))

(deftest learner-program-returns-error-if-not-learner
  "LEARNER-PROGRAM returns an ERROR if something other than a LEARNER is provided."
  (check
   (assert-error (learner-program '(PROGRAM P1 ((R1 SIN OBS1))))
                 :expects "LEARNER-PROGRAM expects a LEARNER.")))

(deftest learner-action-returns-error-if-not-learner
  "LEARNER-ACTION returns an ERROR if something other than a LEARNER is provided."
  (check
   (assert-error (learner-action '(PROGRAM P1 ((R1 SIN OBS1))))
                 :expects "LEARNER-ACTION expects a LEARNER.")))

(deftest learner-action-returns-correct-action
  "LEARNER-ACTION returns the correct ACTION if a LEARNER is provided."
  (check
   ;; atomic action
   (assert-equal (learner-action '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)) 'SQUARE)
   ;; multi-action output
   (assert-equal (learner-action '(LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (3.14 3.14 3.14))) '(3.14 3.14 3.14))
   ;; goto reference
   (assert-equal (learner-action '(LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) (GOTO T1))) '(GOTO T1))
   ;; action program
   (assert-equal (learner-action '(LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) (PROGRAM P5 ((R1 SIN OBS5))))) '(PROGRAM P5 ((R1 SIN OBS5))))))

(deftest learner-id-returns-error-if-not-learner
  "LEARNER-ID returns an ERROR if something other than a LEARNER is provided."
  (check
   (assert-error (learner-id '(PROGRAM P1 ((R1 SIN OBS1))))
                 :expects "LEARNER-ID expects a LEARNER.")))

(deftest learner-id-returns-correct-id
  "LEARNER-ID returns correct LEARNER-ID if a LEARNER is provided."
  (check
   (assert-equal (learner-id '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)) 'L1)))

(deftest team-learners-returns-error-if-not-team
  "TEAM-LEARNERS returns an error if something other than a TEAM is provided."
  (check
   (assert-error (team-learners '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))
                 :expects "TEAM-LEARNERS expects a TEAM")))

(deftest team-learners-returns-correct-learner-ids
  "TEAM-LEARNERS returns the correct LEARNER-IDs if a TEAM is provided."
  (check
   (assert-equal (team-learners '(TEAM T1 L1 L2 L3)) '(L1 L2 L3))))

(deftest team-id-returns-error-if-not-team
  "TEAM-ID returns an ERROR if something other than a TEAM is provided."
  (check
   (assert-error (team-id '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE))
                 :expects "TEAM-ID expects a TEAM")))

(deftest team-id-returns-correct-team-id
  "TEAM-ID returns the correct TEAM-ID if a TEAM is provided."
  (check
   (assert-equal (team-id '(TEAM T1 L1 L2 L3)) 'T1)))

(deftest team-p-positive-case
  "TEAM-P returns T when a TEAM is provided to it."
   (assert-true (team-p '(TEAM T1 L1 L2 L3 L4))))

(deftest team-p-failure-cases
  "TEAM-P returns NIL when something other than a TEAM is provided to it."
  (check
   ;; missing team label
   (assert-false (team-p '(T1 L2 L3 L4)))
   ;; learners are not symbols
   (assert-false (team-p '(TEAM T1 3.0 4.0)))
   ;; learners are provided as list
   (assert-false (team-p '(TEAM T1 (L1 L2 L3))))))

(deftest learner-p-positive-case
  "LEARNER-P returns T when a LEARNER is provided to it."
  (check
   ;; multi-action output
   (assert-true (learner-p '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS2))) (0.0 0.0 0.0))))
   ;; atomic-action
   (assert-true (learner-p '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS2))) ATOMIC-ACTION)))
   ;; team reference
   (assert-true (learner-p '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (GOTO T1))))
   ;; action program
   (assert-true (learner-p '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (PROGRAM P2 ((R1 SIN OBS2))))))))

(deftest learner-p-failure-cases
  "LEARNER-P returns NIL when something other than a LEARNER is provided to it."
  (check
   ; missing learner tag
   (assert-false (learner-p '(L1 (PROGRAM P1 ((R1 SIN OBS2))) (0.0 0.0 0.0))))
   ;; missing learner name
   (assert-false (learner-p '(LEARNER (PROGRAM P1 ((R1 SIN OBS2))) TRIANGLE)))
   ;; missing context program
   (assert-false (learner-p '(LEARNER L1 TRIANGLE)))
   ;; missing action
   (assert-false (learner-p '(LEARNER L1 (PROGRAM P1 ((R1 SIN OBS2))))))
   ;; missing everything except tag
   (assert-false (learner-p '(LEARNER)))))
                                                                               
(deftest program-p-positive-case
  "PROGRAM-P returns T when a PROGRAM is provided to it."
  (assert-true (program-p '(PROGRAM P1 ((R1 SIN OBS1)
                                        (R2 ADD R3 R4)
                                        (R5 COS R1)
                                        (R3 DIV OBS2)
                                        (R5 ADD R5 3.14))))))

(deftest program-p-failure-cases
  "PROGRAM-P returns NIL when something other than a PROGRAM
   is provided to it."
  (check
   (assert-false (program-p '(PROGRAM ((R1 SIN OBS1)
                                       (R2 ADD R3 R4)))))
   (assert-false (program-p '(P1 ((R1 SIN OBS1)
                                  (R2 ADD R3 R4)))))
   (assert-false (program-p '(PROGRAM P1 (R1 SIN OBS1))))
   (assert-false (program-p '(PROGRAM P1 (R1 SIN OBS1) (R2 SIN OBS2))))))

(deftest tpg-p-positive-case
  "TPG-T returns T when a TPG is provided to it."
  (assert-true (tpg-p '(TPG
                        (LEARNERS
                         (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) LEFT)
                         (LEARNER L2 (PROGRAM P2 ((R2 SIN OBS2)
                                                  (R3 SIN OBS4)
                                                  (R4 ADD R1 R2))) RIGHT)
                         (LEARNER L3 (PROGRAM P3 ((R1 SIN R3))) (PROGRAM P4 ((R1 SIN OBS1)))))
                        (TEAMS
                         (TEAM T1 L1 L2)
 (TEAM T2 L2 L3))))))

(deftest tpg-p-failure-cases
  "TPG-P returns NIL when something other than a PROGRAM
   is provided to it."
  (check
   ;; no learners
   (assert-false (tpg-p '(TPG
                          (LEARNERS)
                          (TEAMS
                           (TEAM T1 L1 L2)))))
   ;; no teams
   (assert-false (tpg-p '(TPG
                          (LEARNERS
                           (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                           (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE))
                          (TEAMS))))
   ;; no tpg tag
   (assert-false (tpg-p '((LEARNERS
                          (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                           (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE))
                          (TEAMS
                           (TEAM T1 L1 L2)))))
   ;; teams without learners
   (assert-false (tpg-p '(TPG
                          (LEARNERS
                           (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                           (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE))
                          (TEAMS
                           (TEAM T1)))))
   ;; tpg with invalid learner
   (assert-false (tpg-p '(TPG
                          (LEARNER
                           (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))))
                           (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2)) TRIANGLE)))
                          (TEAMS
                           (TEAM T1 L1 L2)))))
   ;; tpg with invalid team
   (assert-false (tpg-p '(TPG
                          (LEARNERS
                           (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                           (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE))
                          (TEAMS
                           (TEAM T1 L1 L2)
                           (TEAM L2)))))))

(deftest learners-error-if-tpg-is-not-given
  "Test that LEARNERS will return an error instead of LEARNERs
   if something other than a TPG is given."
  (assert-error (learners '(SOME (LIST (BUT (NOT A TPG)))))
                :expects "Expecting a TPG."))
  
(deftest teams-error-if-tpg-is-not-given
  "Test that TEAMS will return an error instead of TEAMS
   if something other than a TPG is given."
  (assert-error (teams '(SOME (LIST (BUT (NOT A TPG)))))
                :expects "Expecting a TPG."))

(deftest teams-never-point-to-non-existent-learners
  "Test that no matter how many mutations occur, teams never
   point to a learner that doesn't exist."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE))
                (TEAMS
                 (TEAM T1 L1 L2)
                 (TEAM T2 L2 L1)
                 (TEAM T3 L2 L1))))
         (heavily-mutated-tpg (loop repeat 10
                                    for acc = tpg then (mutate-tpg acc)
                                    finally (return acc)))
         (learners-in-teams (remove-duplicates (apply #'append (mapcar #'team-learners (teams heavily-mutated-tpg)))))
         (learners-in-population (mapcar #'learner-id (learners heavily-mutated-tpg)))
         (missing-learners-in-population (set-difference learners-in-teams learners-in-population)))
    (assert-false missing-learners-in-population)))

(deftest team-count-atomic-actions-returns-correct-count
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) CIRCLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (GOTO T2))
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) SQUARE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) TRIANGLE)
                (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) (GOTO T2))
                (LEARNER L6 (PROGRAM P6 ((R1 SIN OBS6))) CIRCLE))
               (TEAMS
                (TEAM T1 L1 L2 L5 L4)
                (TEAM T2 L1 L3 L4 L6)))))
    (check
     (assert-equal (team-count-unique-atomic-actions tpg '(TEAM T1 L1 L2 L5 L4)) 2)
     (assert-equal (team-count-unique-atomic-actions tpg '(TEAM T2 L1 L3 L4)) 3))))
                   

(deftest at-least-two-different-atomic-actions
  "Test that mutating the TPG will never cause
   a team to have fewer than 2 actions."
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (GOTO T3))
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) SQUARE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) CIRCLE)
                (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) RHOMBUS)
                (LEARNER L6 (PROGRAM P6 ((R1 SIN OBS6))) TRIANGLE))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L1 L6 L2 L3)
                (TEAM T3 L1 L3)))))
    (setf (experiment-mutate-learner-program-vs-action-probability *experiment*) 0.0)
    (assert-always 10
      (let ((mutated-tpg (mutate-tpg tpg)))
        (every (lambda (team) (>= (team-count-unique-atomic-actions mutated-tpg team) 2))
               (teams mutated-tpg))))))

;; learners never refer to their own teams

(deftest removing-learners-only-if-more-than-two-atomic-actions
  "Test that learners will never be removed if it results in
   a team having less than two ATOMIC actions."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE))
               (TEAMS
                (TEAM T1 L1 L2)))))
    (assert-equal (remove-learner tpg '(TEAM T1 L1 L2)) '(TEAM T1 L1 L2))))


(deftest reachable-p-correct-behaviour
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) (GOTO T2))
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) (GOTO T3)))
               (TEAMS
                (TEAM T1 L1 L2 L3 L4)
                (TEAM T2 L1 L2 L4)
                (TEAM T3 L1 L2)))))
    (check
     (assert-true (reachable-p tpg 'T1 'T2))
     (assert-true (reachable-p tpg 'T1 'T3))
     (assert-true (reachable-p tpg 'T2 'T3))
     ;; trivial cases
     (assert-true (reachable-p tpg 'T1 'T1))
     (assert-true (reachable-p tpg 'T2 'T2))
     (assert-true (reachable-p tpg 'T3 'T3))
     ;; negative cases
     (assert-false (reachable-p tpg 'T2 'T1))
     (assert-false (reachable-p tpg 'T3 'T1))
     (assert-false (reachable-p tpg 'T3 'T2)))))
                            
(deftest would-create-cycle-identifies-cycles
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) SQUARE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) (GOTO T2)))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L1 L2)))))
    (check
     (assert-true (would-create-cycle-p tpg 'T2 'T1))
     (assert-true (would-create-cycle-p tpg 'T1 'T1))
     (assert-true (would-create-cycle-p tpg 'T2 'T2))
     (assert-false (would-create-cycle-p tpg 'T1 'T2)))))

(deftest safe-gotos-from-team
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (GOTO T2))
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (GOTO T3))
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) SQUARE))
               (TEAMS
                (TEAM T1 L3 L4 L1)
                (TEAM T2 L3 L4 L2)
                (TEAM T3 L3 L4)
                (TEAM T4 L3 L4)))))
    (check
     (assert-equal (safe-gotos-from-team tpg 'T1) '(T2 T3 T4))
     (assert-equal (safe-gotos-from-team tpg 'T2) '(T3 T4))
     (assert-equal (safe-gotos-from-team tpg 'T3) '(T4))
     (assert-equal (safe-gotos-from-team tpg 'T4) '(T1 T2 T3)))))
                
(deftest mutating-learner-will-never-goto-original-team
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L1 L2 L3 L4)))))
    (setf (experiment-mutate-learner-program-vs-action-probability *experiment*) 0.0)
    (setf (experiment-learner-atomic-action-probability *experiment*) 0.0)
    (assert-always 1000
      (multiple-value-bind (mutated-team mutated-learner) (mutate-learner tpg '(TEAM T1 L1 L2 L3))
        (declare (ignore mutated-team))
        (let ((new-action (learner-action mutated-learner)))
          (if (reference-p new-action)
              (not (equal (get-reference new-action) 'T1))
              t))))))


(deftest mutating-learner-will-not-introduce-cycles
  (let ((*experiment* (copy-experiment *experiment*))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) TRIANGLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) CIRCLE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1 L2 L3)
                (TEAM T2 L1 L2 L3 L4)))))
    (setf (experiment-mutate-learner-program-vs-action-probability *experiment*) 0.0)
    (setf (experiment-learner-atomic-action-probability *experiment*) 0.0)
    (assert-always 1000
      (multiple-value-bind (mutated-team mutated-learner) (mutate-learner tpg '(TEAM T1 L1 L2 L3))
        (declare (ignore mutated-team))
        (let ((new-action (learner-action mutated-learner)))
          (if (reference-p new-action)
              (not (equal (get-reference new-action) 'T2))
              t))))))

(deftest adding-learner-will-not-introduce-cycles
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) SQUARE)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) CIRCLE)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) TRIANGLE)
                (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) (GOTO T1))
                (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) (GOTO T2)))
               (TEAMS
                (TEAM T1 L1 L2 L3 L5)
                (TEAM T2 L1 L2 L3)))))
    (assert-always 1000
        (let* ((mutated-team (add-learner tpg '(TEAM T2 L1 L2 L3)))
               (learners (team-learners mutated-team)))
          (not (member 'L4 learners))))))

(deftest make-learner-always-atomic
  (assert-always 1000
    (atomic-p (make-learner))))

(deftest make-team-has-correct-bounds
  (let ((min-learners (experiment-initial-minimum-number-of-learners *experiment*))
        (max-learners (experiment-initial-maximum-number-of-learners *experiment*)))
    (assert-always 1000
      (multiple-value-bind (new-team new-learners) (make-team)
        (and (>= (length new-learners) min-learners)
             (<= (length new-learners) max-learners)
             (= (length new-learners)) (length (team-learners new-team)))))))
    
(deftest make-program-has-correct-program-length-bounds
  (let ((min-program-length (experiment-minimum-program-length *experiment*))
        (max-program-length (experiment-maximum-program-length *experiment*)))
    (assert-always 1000
      (let* ((program (make-program))
            (program-length (length (program-instructions program))))
        (and
         (>= program-length min-program-length)
         (<= program-length max-program-length))))))

(deftest make-tpg-never-has-duplicate-learner-ids
  (let* ((tpg (make-tpg))
         (learner-ids (mapcar #'learner-id (learners tpg))))
    (assert-false (has-duplicates-p learner-ids))))
  
(deftest make-tpg-never-has-duplicate-program-ids
    (let* ((tpg (make-tpg))
           (learners (learners tpg))
           (programs (mapcar #'learner-program learners))
           (program-ids (mapcar #'program-id programs)))
      (assert-false (has-duplicates-p program-ids))))
  
(deftest make-tpg-never-has-duplicate-team-ids
    (let* ((tpg (make-tpg))
           (teams (teams tpg))
           (team-ids (mapcar #'team-id teams)))
      (assert-false (has-duplicates-p team-ids))))

(deftest make-tpg-is-tpg
  (assert-true (tpg-p (make-tpg))))

(deftest make-tpg-has-correct-population-size
  (let ((population-size (experiment-population-size *experiment*)))
    (assert-equal (length (teams (make-tpg))) population-size)))

(deftest make-tpg-ensures-at-least-two-atomic-actions
  (let* ((tpg (make-tpg))
        (teams (teams tpg)))
    (assert-true (every (lambda (team) (>= (team-count-unique-atomic-actions tpg team) 2)) teams))))
