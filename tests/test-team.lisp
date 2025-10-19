(deftest complexity-of-team-is-measured-correctly
  "Check that TEAMs have the correct measure of complexity:
   - Length of learners' context programs, action programs
   - and complexity of all teams the team points to."
  (let ((tpg '(TPG
               (LEARNERS
                (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) LEFT)
                (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2)(R3 SIN OBS4))) RIGHT)
                (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3)(R1 SIN OBS2)(R1 SIN OBS4))) (GOTO T1)))
               (TEAMS
                (TEAM T1 L1 L2)
                (TEAM T2 L1 L3)))))
    (check
     (assert-equal (team-complexity tpg 'T1) 3)
     (assert-equal (team-complexity tpg 'T2) 7))))
