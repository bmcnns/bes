(defparameter *agent*
  '(TPG
    (LEARNERS
     ;; LEARNER 1 (Action 0)
     (LEARNER L1 
      (PROGRAM P1 
       ((R1 MUL OBS1 1.0d0)   ; Initialize Sum with Term 1
        (R2 MUL OBS2 1.0d0)   ; Calculate Term 2
        (R1 ADD R1 R2)        ; Add Term 2 to Sum
        (R2 MUL OBS3 1.0d0)   ; Calculate Term 3
        (R1 ADD R1 R2)        ; Add Term 3 to Sum
        (R2 MUL OBS4 1.0d0)   ; ...
        (R1 ADD R1 R2)
        (R2 MUL OBS5 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 1.0d0)   ; Calculate Term 8
        (R1 ADD R1 R2)))      ; Final Sum
      0)

     ;; LEARNER 2 (Action 1) - Identical Program
     (LEARNER L2 
      (PROGRAM P2 
       ((R1 MUL OBS1 1.0d0)
        (R2 MUL OBS2 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 1.0d0)
        (R1 ADD R1 R2))) 
      1)

     ;; LEARNER 3 (Action 2) - Identical Program
     (LEARNER L3 
      (PROGRAM P3 
       ((R1 MUL OBS1 1.0d0)
        (R2 MUL OBS2 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 1.0d0)
        (R1 ADD R1 R2))) 
      2)

     ;; LEARNER 4 (Action 3) - Identical Program
     (LEARNER L4 
      (PROGRAM P4 
       ((R1 MUL OBS1 1.0d0)
        (R2 MUL OBS2 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 1.0d0)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 1.0d0)
        (R1 ADD R1 R2))) 
      3))

    (TEAMS
     (TEAM T1 L1 L2 L3 L4))))
