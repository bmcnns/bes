(in-package :bes)

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
  "Perform selection with lucky-break protection (flywheel style)."
  (let* ((selected-ids (funcall selection-fn scores))
         (new-teams '()))
    (loop for team in (teams model)
          for id = (team-id team)
          do (cond
               ;; Keep if selected normally
               ((member id selected-ids :test #'equal)
                (push team new-teams))))

    (rebuild-tpg model (mapcar #'team-id new-teams))))

(defun select-ids (model ids)
  "Perform selection by ids."
  (let* ((new-teams '()))
    (loop for team in (teams model)
          for id = (team-id team)
          do (cond
               ;; Keep if selected normally
               ((member id ids :test #'equal)
                (push team new-teams))))

    (rebuild-tpg model (mapcar #'team-id new-teams))))


(defun add-learners-to-tpg (tpg learners)
  (if learners
    `(TPG
      (LEARNERS ,@(append (learners tpg) learners))
      (TEAMS ,@(teams tpg)))
    tpg))

(defun add-teams-to-tpg (tpg teams)
  (if teams
      `(TPG
        (LEARNERS ,@(learners tpg))
        (TEAMS ,@(append (teams tpg) teams)))
      tpg))

