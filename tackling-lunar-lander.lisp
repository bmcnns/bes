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
        (R1 ADD R1 R2)
        (R1 ADD R1 1.0d0)))      ; Final Sum
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
        (R1 ADD R1 R2)
        (R1 ADD R1 1.0d0)))
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
        (R1 ADD R1 R2)
        (R1 ADD R1 1.0d0))) 
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
        (R1 ADD R1 R2)
        (R1 ADD R1 1.0d0))) 
      3))
    (TEAMS
     (TEAM T1 L1 L2 L3 L4))))

(defparameter *trained-agent*
  '(TPG
    (LEARNERS
     ;; ---------------------------------------------------------
     ;; LEARNER 1 (Action 0: Do Nothing)
     ;; Weights: Indices 0-7 | Bias: Index 32
     ;; ---------------------------------------------------------
     (LEARNER L1 
      (PROGRAM P1 
       ((R1 MUL OBS1 -6.98221339d+15)
        (R2 MUL OBS2 -1.36089269d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 -3.30881369d+16)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 8.08823430d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 2.13275975d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 -2.04500982d+16)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 3.54059614d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 -1.54119265d+17)
        (R1 ADD R1 R2)
        (R1 ADD R1 -1.72147236d+17))) ; Bias for L1
      0)

     ;; ---------------------------------------------------------
     ;; LEARNER 2 (Action 1: Fire Left Engine)
     ;; Weights: Indices 8-15 | Bias: Index 33
     ;; ---------------------------------------------------------
     (LEARNER L2 
      (PROGRAM P2 
       ((R1 MUL OBS1 9.61412851d+16)
        (R2 MUL OBS2 1.17658839d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 1.90719095d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 -4.43925562d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 -8.17650699d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 -8.19866514d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 -2.73612778d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 1.58162166d+17)
        (R1 ADD R1 R2)
        (R1 ADD R1 -8.39618509d+16))) ; Bias for L2
      1)

     ;; ---------------------------------------------------------
     ;; LEARNER 3 (Action 2: Fire Main Engine)
     ;; Weights: Indices 16-23 | Bias: Index 34
     ;; ---------------------------------------------------------
     (LEARNER L3 
      (PROGRAM P3 
       ((R1 MUL OBS1 1.96586286d+16)
        (R2 MUL OBS2 1.65184895d+16)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 -1.67093261d+16)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 -8.88282664d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 1.32324484d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 -1.70667139d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 -8.11121911d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 -5.77589707d+17)
        (R1 ADD R1 R2)
        (R1 ADD R1 -9.95940254d+16))) ; Bias for L3
      2)

     ;; ---------------------------------------------------------
     ;; LEARNER 4 (Action 3: Fire Right Engine)
     ;; Weights: Indices 24-31 | Bias: Index 35
     ;; ---------------------------------------------------------
     (LEARNER L4 
      (PROGRAM P4 
       ((R1 MUL OBS1 -1.59321061d+17)
        (R2 MUL OBS2 3.02230969d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS3 -4.71460472d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS4 3.46096510d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS5 5.39992669d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS6 7.70700423d+17)
        (R1 ADD R1 R2)
        (R2 MUL OBS7 4.89228331d+16)
        (R1 ADD R1 R2)
        (R2 MUL OBS8 -1.74060849d+17)
        (R1 ADD R1 R2)
        (R1 ADD R1 2.70761136d+16))) ; Bias for L4
      3))

    (TEAMS
     (TEAM T1 L1 L2 L3 L4))))
