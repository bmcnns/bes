(defun mutate-team-add-learner (team-or-id)
  "Add a random non-member learner to TEAM-OR-ID. Returns TEAM."
  (let* ((team (resolve-team team-or-id))
         (pool (available-learner-ids-for-team team)))
    (when (null pool)
      (error "No available learners to add to team ~A." (team-id team)))
    (multiple-value-bind (t2 addedp)
        (add-learner-to-team team (random-choice pool))
      (declare (ignore addedp))
      t2)))

(defun mutate-team-remove-learner (team-or-id)
  "Remove a random non-member learner from TEAM-OR-ID. Returns TEAM."
  (let* ((team (resolve-team team-or-id))
         (pool (team-learner-ids team))
         (ls (team-learners team)))
    (when (> (length pool) 1)
      (multiple-value-bind (t2 addedp)
          (remove-learner-from-team team-or-id (random-choice pool))
        (declare (ignore addedp))
        t2))))

(defun mutate-learner-swap-action (learner-or-id)
  "Change the action of LEARNER-OR-ID to a different action. Returns LEARNER."
  (let* ((lrn (resolve-learner learner-or-id))
         (pool (available-actions-for-learner lrn)))
    (when (>= (length pool) 1)
      (multiple-value-bind (lrn2 changedp)
          (change-learner-action lrn (random-choice pool))
        (declare (ignore changedp))
        lrn2))))

