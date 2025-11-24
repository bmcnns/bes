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
  "Follow a chain of single-learner teams starting from TEAM-ID.
   Return the ID of the first team that contains any atomic learner.
   Return NIL if no such team exists or if a cycle/broken link is detected."
  (loop
    with visited = '()
    with current-id = team-id
    do
       (when (member current-id visited :test #'equal)
         (return nil)) ;; prevent infinite loops
       (push current-id visited)

       (let ((team (find-team-by-id tpg current-id)))
         (unless team (return nil))

         (let ((learner-ids (team-learners team)))
           ;; stop if any learner is atomic
           (when (some (lambda (lid)
                         (let ((learner (find-learner-by-id tpg lid)))
                           (and learner (atomic-p learner))))
                       learner-ids)
             (return current-id))

           ;; if not a single-learner team, we can't follow the chain
           (unless (= (length learner-ids) 1)
             (return nil))

           ;; follow the reference if it exists
           (let* ((learner (find-learner-by-id tpg (first learner-ids)))
                  (action (learner-action learner)))
             (if (reference-p action)
                 (setf current-id (get-reference action))
                 (return nil)))))))

(defun break-chain (tpg team-id)
  "For each non-atomic learner in TEAM-ID, update its action to point directly
   to the terminal team (the first in the chain with any atomic learner)."
  (let* ((team (find-team-by-id tpg team-id))
         (learner-ids (team-learners team)))
    (reduce
     (lambda (updated-tpg learner-id)
       (let ((learner (find-learner-by-id updated-tpg learner-id)))
         (cond
           ((or (null learner) (atomic-p learner))
            updated-tpg)
           (t
            (let* ((target-id (get-reference (learner-action learner)))
                   (terminal-id (resolve-terminal-team updated-tpg target-id)))
              (if (and terminal-id (not (equal terminal-id target-id)))
                  (change-learner-action updated-tpg learner-id `(GOTO ,terminal-id))
                  updated-tpg))))))
     learner-ids
     :initial-value tpg)))

(defun break-all-chains (tpg)
  "Apply break-chain to every team in the TPG."
  (reduce #'break-chain
          (mapcar #'team-id (teams tpg))
          :initial-value tpg))

(defun postprocess (tpg team-id env-name)
  (clear-cache)
  (break-all-chains (remove-hitch-hikers (prune-tpg (rebuild-tpg tpg (list team-id))) team-id env-name)))

(defun safe-postprocess (tpg team-id env-name)
  (clear-cache)
  (break-all-chains (prune-tpg (rebuild-tpg tpg (list team-id)))))
