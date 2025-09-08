(deftest no-tpgs-with-cycles
  "Don't allow TPGs to be 'materialized' when they contain cycles."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) (GOTO T2))
                (LEARNER L2 ((R2 SIN OBS2)) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1)
                (TEAM T2 L2)))))
    (assert-error (materialize-tpg tpg))))

(deftest allow-valid-tpg
  "Allow the 'materializing' of a TPG when it is well-formed."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE)
                (LEARNER L2 ((R2 SIN OBS2)) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1)
                (TEAM T2 L2)))))
    (assert-true (materialize-tpg tpg))))

(deftest avoid-cycles-when-changing-action
  "Avoid creating a cycle when changing the action of a learner"
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE)
                (LEARNER L2 ((R2 SIN OBS2)) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1)
                (TEAM T2 L2)))))
    (materialize-tpg tpg)
    (assert-error (change-learner-action "L1" '(GOTO T2)))))

(deftest avoid-cycles-when-adding-learner-to-team
  "Avoid creating a cycle when adding a new learner to a team."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) (GOTO T2))
                (LEARNER L2 ((R1 SIN OBS2)) TRIANGLE)
                (LEARNER L3 ((R1 SIN OBS3)) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1)
                (TEAM T2 L2)))))
    (materialize-tpg tpg)
    (assert-error (add-learner-to-team "T2" "L3"))))
          
(deftest correct-routes-are-taken
  "Test that the correct routes are taken when evaluating TPG."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE)
                (LEARNER L2 ((R1 SIN OBS2)) (GOTO T3))
                (LEARNER L3 ((R1 SIN OBS3)) SQUARE)
                (LEARNER L4 ((R1 SIN OBS4)) CIRCLE)
                (LEARNER L5 ((R1 SIN OBS5)) RHOMBUS)
                (LEARNER L6 ((R1 SIN OBS6)) TRIANGLE))
               (TEAMS
                (TEAM T1 L3 L4 L5)
                (TEAM T2 L2 L4 L6)
                (TEAM T3 L1 L3)))))
    (materialize-tpg tpg)
    (and
     (assert-equal (team-phenotype "T1" '(0 0 1 0 0 0)) 'SQUARE)
     (assert-equal (team-phenotype "T1" '(0 0 0 1 0 0)) 'CIRCLE)
     (assert-equal (team-phenotype "T1" '(0 0 0 0 1 0)) 'RHOMBUS)
     (assert-equal (team-phenotype "T2" '(0 0 0 0 0 1)) 'TRIANGLE)
     (assert-equal (team-phenotype "T2" '(1 1 0 0 0 0)) 'TRIANGLE)
     (assert-equal (team-phenotype "T2" '(0 1 1 0 0 0)) 'SQUARE)
     (assert-equal (team-phenotype "T3" '(1 0 0 0 0 0)) 'TRIANGLE)
     (assert-equal (team-phenotype "T3" '(0 0 1 0 0 0)) 'SQUARE))))
     
(deftest unused-learners-are-deleted-during-cleanup
  "Check that LEARNERS that are not referenced by any TEAMS are removed
   when CLEANUP-LEARNERS is called."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE)
                (LEARNER L2 ((R1 SIN OBS2)) SQUARE)
                (LEARNER L3 ((R1 SIN OBS3)) (GOTO T1))
                (LEARNER L4 ((R1 SIN OBS4)) CIRCLE)
                (LEARNER L5 ((R1 SIN OBS5)) RHOMBUS))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L5)))))
    (materialize-tpg tpg)
    (let ((learners-exist-before-clean-up nil)
          (learners-exist-after-clean-up nil))
      (setf learners-exist-before-clean-up (and
                               (member "L3" (all-learner-ids) :test 'equalp)
                               (member "L4" (all-learner-ids) :test 'equalp)))
      (clean-up-learners)
      (setf learners-exist-after-clean-up (and
                              (member "L3" (all-learner-ids) :test 'equalp)
                              (member "L4" (all-learner-ids) :test 'equalp)))
      (and
       (assert-true learners-exist-before-clean-up)
       (assert-false learners-exist-after-clean-up)))))


(deftest allow-discrete-outputs
  "Test that TPG allows discrete action outputs for programs."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE))
               (TEAMS
                (TEAM T1 L1)))))
    (materialize-tpg tpg)
    (assert-equal (team-phenotype "T1" '(1 0 0)) 'TRIANGLE)))
  
(deftest allow-multi-action-continuous-outputs
  "Test that TPG supports multiple continuous outputs for a single action."
  (let ((*learners* (make-hash-table :test 'equal))
        (*teams* (make-hash-table :test 'equal))
        (tpg '(TPG
               (LEARNERS
                (LEARNER L1 ((R1 SIN OBS1)) (0.1321 0.3472 0.127783 0.2378)))
               (TEAMS
                (TEAM T1 L1)))))
    (materialize-tpg tpg)
    (assert-equal (team-phenotype "T1" '(1 0 0)) '(0.1321 0.3472 0.127783 0.2378))))


