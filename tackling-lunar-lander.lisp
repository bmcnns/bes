(defparameter *agent*
  (make-tpg
   :root-teams
   (list
    (%make-team
     :id 'T1
     :learners
     (list
      (make-learner
       :id 'L1 :action 0
       :program '(PROGRAM P1
                  ((R1 MUL OBS1 1.0d0) (R2 MUL OBS2 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS3 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS4 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS5 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS6 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS7 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS8 1.0d0) (R1 ADD R1 R2)
                   (R1 ADD R1 1.0d0))))
      (make-learner
       :id 'L2 :action 1
       :program '(PROGRAM P2
                  ((R1 MUL OBS1 1.0d0) (R2 MUL OBS2 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS3 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS4 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS5 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS6 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS7 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS8 1.0d0) (R1 ADD R1 R2)
                   (R1 ADD R1 1.0d0))))
      (make-learner
       :id 'L3 :action 2
       :program '(PROGRAM P3
                  ((R1 MUL OBS1 1.0d0) (R2 MUL OBS2 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS3 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS4 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS5 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS6 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS7 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS8 1.0d0) (R1 ADD R1 R2)
                   (R1 ADD R1 1.0d0))))
      (make-learner
       :id 'L4 :action 3
       :program '(PROGRAM P4
                  ((R1 MUL OBS1 1.0d0) (R2 MUL OBS2 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS3 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS4 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS5 1.0d0) (R1 ADD R1 R2)
                   (R2 MUL OBS6 1.0d0) (R1 ADD R1 R2) (R2 MUL OBS7 1.0d0)
                   (R1 ADD R1 R2) (R2 MUL OBS8 1.0d0) (R1 ADD R1 R2)
                   (R1 ADD R1 1.0d0)))))))))

(defparameter *team* (first (tpg-root-teams *agent*)))

(in-package :bes)

(defparameter *agent*
  (make-tpg
   :root-teams
   (list
    (%make-team
     :id 'T-SEED
     :learners
     (list
      ;; 1. GRAVITY FIGHTER (Action 2)
      ;; IF Vertical Velocity (OBS4) < -0.2 (falling), BID HIGH.
      (make-learner :id 'L-GRAV :action 2
       :program '(PROGRAM P1 ((R1 SUB -0.2d0 OBS4) (R1 MUL R1 20.0d0))))

      ;; 2. STABILIZER LEFT (Action 1)
      ;; IF Angle (OBS5) > 0.05 (tilting right), Fire Left.
      (make-learner :id 'L-STAB-L :action 1
       :program '(PROGRAM P2 ((R1 SUB OBS5 0.05d0) (R1 MUL R1 10.0d0))))

      ;; 3. STABILIZER RIGHT (Action 3)
      ;; IF Angle (OBS5) < -0.05 (tilting left), Fire Right.
      (make-learner :id 'L-STAB-R :action 3
       :program '(PROGRAM P3 ((R1 SUB -0.05d0 OBS5) (R1 MUL R1 10.0d0))))

      ;; 4. DO NOTHING (Anchor)
      ;; If everything is fine, don't fire.
      (make-learner :id 'L-IDLE :action 0
       :program '(PROGRAM P4 ((R1 ADD 1.0d0 0.0d0))))
      )))))


(defparameter *stripped-agent* 
  (%make-team
   
 :learners
 (list
  ;; Learner 1
  (make-learner
   :action 1
   :program '(PROGRAM P205095_S
              ((R1 ADD OBS2 OBS3))))

  ;; Learner 2
  (make-learner
   :action 3
   :program '(PROGRAM P207282_S
              ((R7 ADD OBS1 R3) (R1 ADD OBS6 OBS1)
               (R4 SUB OBS5 R1) (R8 ADD R7 R7)
               (R7 SUB R6 R8) (R5 SUB R4 OBS3)
               (R1 SUB R7 R5))))

  ;; Learner 3
  (make-learner
   :action 0
   :program '(PROGRAM P217007_S
              ((R7 MUL R8 OBS6) (R1 ADD R6 R7))))

  ;; Learner 4
  (make-learner
   :action 2
   :program '(PROGRAM P203418_S
              ((R1 MUL OBS8 -8.5521493517117d0))))

  ;; Learner 5
  (make-learner
   :action 0
   :program '(PROGRAM P185364_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 6
  (make-learner
   :action 3
   :program '(PROGRAM P193608_S
              ((R1 MUL OBS7 OBS1)
               (R1 MUL -1.7440876432835979d0 R1))))

  ;; Learner 7
  (make-learner
   :action 3
   :program '(PROGRAM P181192_S
              ((R8 ADD OBS6 R5) (R1 ADD OBS2 OBS2)
               (R1 ADD R1 R8))))

  ;; Learner 8
  (make-learner
   :action 2
   :program '(PROGRAM P192270_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 9
  (make-learner
   :action 0
   :program '(PROGRAM P208410_S
              ((R4 ADD OBS1 R2)
               (R1 MUL -2.622371740966042d0 R4))))

  ;; Learner 10
  (make-learner
   :action 1
   :program '(PROGRAM P161413_S
              ((R7 ADD R2 OBS8)
               (R1 ADD R7 -0.7844911441527148d0)
               (R4 SUB R1 -5.425131057873794d0)
               (R3 SUB OBS3 R4) (R1 MUL R3 OBS4))))

  ;; Learner 11
  (make-learner
   :action 2
   :program '(PROGRAM P193021_S
              ((R1 ADD OBS6 -0.514061188457467d0))))

  ;; Learner 12
  (make-learner
   :action 1
   :program '(PROGRAM P213984_S
              ((R1 ADD OBS6 OBS3))))

  ;; Learner 13
  (make-learner
   :action 0
   :program '(PROGRAM P205095_S
              ((R1 ADD OBS2 OBS3))))

  ;; Learner 14
  (make-learner
   :action 1
   :program '(PROGRAM P193608_S
              ((R1 MUL OBS7 OBS1)
               (R1 MUL -1.7440876432835979d0 R1))))

  ;; Learner 15
  (make-learner
   :action 3
   :program '(PROGRAM P202427_S
              ((R1 ADD OBS2 OBS2) (R1 ADD R1 OBS5))))

  ;; Learner 16
  (make-learner
   :action 1
   :program '(PROGRAM P206332_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 17
  (make-learner
   :action 0
   :program '(PROGRAM P215259_S
              ((R6 ADD R4 OBS7) (R1 ADD OBS6 OBS5)
               (R8 MUL R1 -6.01485245919763d0)
               (R5 MUL R8 OBS3) (R7 SUB R6 R8)
               (R1 SUB R7 R5))))

  ;; Learner 18
  (make-learner
   :action 0
   :program '(PROGRAM P197513_S
              ((R1 ADD OBS2 OBS2))))

  ;; Learner 19
  (make-learner
   :action 2
   :program '(PROGRAM P210529_S
              ((R1 SUB OBS6 3.85050291856895d0))))

  ;; Learner 20
  (make-learner
   :action 3
   :program '(PROGRAM P193021_S
              ((R1 ADD OBS6 -0.514061188457467d0))))

  ;; Learner 21
  (make-learner
   :action 1
   :program '(PROGRAM P191569_S
              ((R4 ADD OBS1 R2)
               (R1 MUL -2.622371740966042d0 R4))))

  ;; Learner 22
  (make-learner
   :action 2
   :program '(PROGRAM P187977_S
              ((R1 SUB R6 R8))))

  ;; Learner 23
  (make-learner
   :action 2
   :program '(PROGRAM P181192_S
              ((R8 ADD OBS6 R5) (R1 ADD OBS2 OBS2)
               (R1 ADD R1 R8))))

  ;; Learner 24
  (make-learner
   :action 3
   :program '(PROGRAM P217556_S
              ((R5 SUB OBS2 2.849840195371555d0)
               (R7 MUL -7.905800400499212d0 R5)
               (R1 MUL R7 OBS6))))

  ;; Learner 25
  (make-learner
   :action 2
   :program '(PROGRAM P203947_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 26
  (make-learner
   :action 2
   :program '(PROGRAM P162163_S
              ((R7 MUL R2 OBS8) (R7 SUB OBS6 R7)
               (R1 ADD R7 -0.7844911441527148d0)
               (R4 SUB R1 -5.438348560370116d0)
               (R3 SUB OBS3 R4) (R1 MUL R3 OBS4)))))))

(defparameter *tuned-stripped-agent* 
  (%make-team
   
 :learners
 (list
  ;; Learner 1
  (make-learner
   :action 1
   :program '(PROGRAM P205095_S
              ((R1 ADD OBS2 OBS3))))

  ;; Learner 2
  (make-learner
   :action 3
   :program '(PROGRAM P207282_S
              ((R7 ADD OBS1 R3) (R1 ADD OBS6 OBS1)
               (R4 SUB OBS5 R1) (R8 ADD R7 R7)
               (R7 SUB R6 R8) (R5 SUB R4 OBS3)
               (R1 SUB R7 R5))))

  ;; Learner 3
  (make-learner
   :action 0
   :program '(PROGRAM P217007_S
              ((R7 MUL R8 OBS6) (R1 ADD R6 R7))))

  ;; Learner 4
  (make-learner
   :action 2
   :program '(PROGRAM P203418_S
              ((R1 MUL OBS8 -8.5521493517117d0))))

  ;; Learner 5
  (make-learner
   :action 0
   :program '(PROGRAM P185364_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 6
  (make-learner
   :action 3
   :program '(PROGRAM P193608_S
              ((R1 MUL OBS7 OBS1)
               (R1 MUL -1.7440876432835979d0 R1))))

  ;; Learner 7
  (make-learner
   :action 3
   :program '(PROGRAM P181192_S
              ((R8 ADD OBS6 R5) (R1 ADD OBS2 OBS2)
               (R1 ADD R1 R8))))

  ;; Learner 8
  (make-learner
   :action 2
   :program '(PROGRAM P192270_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 9
  (make-learner
   :action 0
   :program '(PROGRAM P208410_S
              ((R4 ADD OBS1 R2)
               (R1 MUL -2.622371740966042d0 R4))))

  ;; Learner 10
  (make-learner
   :action 1
   :program '(PROGRAM P161413_S
              ((R7 ADD R2 OBS8)
               (R1 ADD R7 -0.7844911441527148d0)
               (R4 SUB R1 -5.425131057873794d0)
               (R3 SUB OBS3 R4) (R1 MUL R3 OBS4))))

  ;; Learner 11
  (make-learner
   :action 2
   :program '(PROGRAM P193021_S
              ((R1 ADD OBS6 -0.514061188457467d0))))

  ;; Learner 12
  (make-learner
   :action 1
   :program '(PROGRAM P213984_S
              ((R1 ADD OBS6 OBS3))))

  ;; Learner 13
  (make-learner
   :action 0
   :program '(PROGRAM P205095_S
              ((R1 ADD OBS2 OBS3))))

  ;; Learner 14
  (make-learner
   :action 1
   :program '(PROGRAM P193608_S
              ((R1 MUL OBS7 OBS1)
               (R1 MUL -1.7440876432835979d0 R1))))

  ;; Learner 15
  (make-learner
   :action 3
   :program '(PROGRAM P202427_S
              ((R1 ADD OBS2 OBS2) (R1 ADD R1 OBS5))))

  ;; Learner 16
  (make-learner
   :action 1
   :program '(PROGRAM P206332_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 17
  (make-learner
   :action 0
   :program '(PROGRAM P215259_S
              ((R6 ADD R4 OBS7) (R1 ADD OBS6 OBS5)
               (R8 MUL R1 -6.01485245919763d0)
               (R5 MUL R8 OBS3) (R7 SUB R6 R8)
               (R1 SUB R7 R5))))

  ;; Learner 18
  (make-learner
   :action 0
   :program '(PROGRAM P197513_S
              ((R1 ADD OBS2 OBS2))))

  ;; Learner 19
  (make-learner
   :action 2
   :program '(PROGRAM P210529_S
              ((R1 SUB OBS6 3.85050291856895d0))))

  ;; Learner 20
  (make-learner
   :action 3
   :program '(PROGRAM P193021_S
              ((R1 ADD OBS6 -0.514061188457467d0))))

  ;; Learner 21
  (make-learner
   :action 1
   :program '(PROGRAM P191569_S
              ((R4 ADD OBS1 R2)
               (R1 MUL -2.622371740966042d0 R4))))

  ;; Learner 22
  (make-learner
   :action 2
   :program '(PROGRAM P187977_S
              ((R1 SUB R6 R8))))

  ;; Learner 23
  (make-learner
   :action 2
   :program '(PROGRAM P181192_S
              ((R8 ADD OBS6 R5) (R1 ADD OBS2 OBS2)
               (R1 ADD R1 R8))))

  ;; Learner 24
  (make-learner
   :action 3
   :program '(PROGRAM P217556_S
              ((R5 SUB OBS2 2.849840195371555d0)
               (R7 MUL -7.905800400499212d0 R5)
               (R1 MUL R7 OBS6))))

  ;; Learner 25
  (make-learner
   :action 2
   :program '(PROGRAM P203947_S
              ((R1 ADD OBS6 OBS1))))

  ;; Learner 26
  (make-learner
   :action 2
   :program '(PROGRAM P162163_S
              ((R7 MUL R2 OBS8) (R7 SUB OBS6 R7)
               (R1 ADD R7 -0.7844911441527148d0)
               (R4 SUB R1 -5.438348560370116d0)
               (R3 SUB OBS3 R4) (R1 MUL R3 OBS4)))))))
