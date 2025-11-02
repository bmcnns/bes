(defparameter *minimal-agent* '(TPG
 (LEARNERS (LEARNER L802989 (PROGRAM P529301 ((R1 SUB -5.113125 OBS2))) 2)
           (LEARNER L815686
            (PROGRAM P539302
             ((R6 LOG OBS1) (R8 MUL OBS1 OBS2)
              (R3 DIV 4.406220402106052d0 OBS2) (R1 ADD R3 R8)
              (R8 DIV 0.023549126044756896d0 R1)
              (R4 ADD -8.788028142386631d0 R8) (R3 ADD OBS1 R6)
              (R1 SUB R3 R4)))
            (GOTO T2077905))
           (LEARNER L663649 (PROGRAM P438894 ((R1 ADD OBS1 OBS2))) 1)
           (LEARNER L664465 (PROGRAM P437314 ((R7 COS OBS2) (R1 COS R7)))
            (GOTO T6065))
           (LEARNER L542 (PROGRAM P542 ((R1 SIN OBS2))) 2)
           (LEARNER L545 (PROGRAM P545 ((R1 DIV OBS2 7.4001465))) 0)
           (LEARNER L4459 (PROGRAM P4269 ((R1 SIN OBS1))) 1))
 (TEAMS (TEAM T2551702 L802989 L815686) (TEAM T2077905 L663649 L664465)
  (TEAM T6065 L542 L545 L4459))))

(defun remove-hitch-hikers (tpg team-id env-name)
  (let ((learner-ids (mapcar #'learner-id (learners tpg)))
        (team-ids (mapcar #'team-id (teams tpg)))
        (execution-paths '())
        (seeds (loop repeat 100
                     collect (random 1000000000))))
    (loop for seed in seeds
          do (multiple-value-bind (_ path)
                 (bes-gym:rollout* tpg team-id env-name seed)
               (declare (ignore _))
               (push (reduce #'append path) execution-paths)))
    (let ((hitchhikers (set-difference
                        (union learner-ids team-ids)
                        (remove-duplicates (reduce #'append execution-paths)))))
      (remove-learners tpg hitchhikers))))

(defun change-learner-action (tpg learner-id action)
  (let* ((learners (learners tpg))
         (teams (teams tpg))
         (learner (find-learner-by-id tpg learner-id))
         (modified-learner `(LEARNER ,learner-id ,(learner-program learner) ,action)))
    `(TPG
      (LEARNERS
       ,@(cons modified-learner (remove-if (lambda (learner)
                                             (equal (learner-id learner) learner-id)) learners)))
      (TEAMS
       ,@teams))))

(defun resolve-terminal-team (tpg team-id)
  "Follow chain of single-learner teams until you reach a team with any atomic learner.
   Return the terminal team-id, or NIL if none found."
  (loop
    with current-id = team-id
    do
      (let* ((team (find-team-by-id tpg current-id))
             (learner-ids (team-learners team)))
        ;; stop if any learner is atomic
        (when (some (lambda (lid)
                      (let ((learner (find-learner-by-id tpg lid)))
                        (and learner (atomic-p learner))))
                    learner-ids)
          (return current-id))
        ;; if it's not a chain, break
        (unless (= (length learner-ids) 1)
          (return nil))
        ;; otherwise, follow the only learner's reference
        (let* ((learner (find-learner-by-id tpg (first learner-ids)))
               (next-team-id (get-reference (learner-action learner))))
          (setf current-id next-team-id)))))

(defun break-chain (tpg team-id)
  "For each non-atomic learner in TEAM-ID, replace its action if it starts a chain."
  (let* ((team (find-team-by-id tpg team-id))
         (learner-ids (team-learners team)))
    (reduce (lambda (updated-tpg learner-id)
              (let ((learner (find-learner-by-id updated-tpg learner-id)))
                (if (atomic-p learner)
                    updated-tpg
                    (let* ((target-team-id (get-reference (learner-action learner)))
                           (terminal-id (resolve-terminal-team updated-tpg target-team-id)))
                      (if terminal-id
                          (change-learner-action updated-tpg learner-id `(GOTO ,terminal-id))
                          updated-tpg)))))
            learner-ids
            :initial-value tpg)))

(defun postprocess (tpg team-id env-name)
  (break-chain (remove-hitch-hikers (prune-tpg (rebuild-tpg tpg (list team-id))) team-id env-name) team-id))
