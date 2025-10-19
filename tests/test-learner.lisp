(deftest check-atomic-learner-complexity
  "Check that an ATOMIC LEARNER's complexity is the length of its context program."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) LEFT)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2)(R3 SIN OBS4))) RIGHT))
                (TEAMS
                 (TEAM T1 L1 L2)))))
    (check
     (assert-equal (learner-complexity tpg 'L1) 1)
     (assert-equal (learner-complexity tpg 'L2) 2))))

(deftest check-learner-with-action-program-complexity
  "Check that a LEARNER with an ACTION PROGRAM's complexity is the length
   of its CONTEXT PROGRAM + the length of its ACTION PROGRAM."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) (PROGRAM P3 ((R1 ADD R2 R3)
                                                                       (R2 ADD R3 R4)
                                                                       (R4 SIN R6)
                                                                       (R7 PLUS R8)))))
                (TEAMS
                 (TEAM T1 L1 L2)))))
    (assert-equal (learner-complexity tpg 'L1) 5)))

(deftest check-learner-that-points-to-team-has-correct-complexity
  "Check that a NON-ATOMIC LEARNER that points to another TEAM is the
   length of its CONTEXT PROGRAM + the TEAM-COMPLEXITY of the TEAM."
  (let* ((tpg '(TPG
                (LEARNERS
                 (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) LEFT)
                 (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2)(R3 SIN OBS4))) RIGHT)
                 (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3)(R1 SIN OBS2)(R1 SIN OBS4))) (GOTO T1)))
                (TEAMS
                 (TEAM T1 L1 L2)
                 (TEAM T2 L1 L3)))))
    (assert-equal (learner-complexity tpg 'L3) 6)))
