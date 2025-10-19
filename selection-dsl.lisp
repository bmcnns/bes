(defun rebuild-tpg (tpg survivor-ids)
  "Given TPG and a list of ROOT TEAM IDs to keep,
   return a TPG with only the ROOT TEAMs specified
   and their INNER TEAMs and LEARNERs."
  (let ((team-table (build-team-table tpg))
        (learner-table (build-learner-table tpg)))
    (labels
        ((collect-teams (team-ids &optional (acc (make-hash-table :test 'equal)))
           ;; recursively collect reachable teams
           (dolist (id team-ids)
             (unless (gethash id acc)
               (let* ((team (find-team-by-id tpg id :team-table team-table))
                      (internal-refs (team-references tpg id)))
                 (setf (gethash id acc) team)
                 (collect-teams internal-refs acc))))
           acc)
         (collect-learners (teams)
           ;; gather all learner IDs reference by the given teams
           (remove-duplicates
            (loop for team being the hash-values of teams append (team-learners team)))))
      (let* ((teams (collect-teams survivor-ids))
             (learners (collect-learners teams)))
        `(TPG (LEARNERS ,@(mapcar (lambda (id)
                                    (find-learner-by-id tpg id :learner-table learner-table))
                                  learners))
              (TEAMS ,@(loop for team being the hash-values of teams collect team)))))))

(defun select (selection-fn model scores)
  (rebuild-tpg model (funcall selection-fn scores)))

(defun add-learners-to-tpg (tpg learners)
  `(TPG
    (LEARNERS ,@(append (learners tpg) learners))
    (TEAMS ,@(teams tpg))))

(defun add-teams-to-tpg (tpg teams)
  `(TPG
    (LEARNERS ,@(learners tpg))
    (TEAMS ,@(append (teams tpg) teams))))

