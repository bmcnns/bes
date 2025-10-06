(in-package :bes)

(defun execute (model dataset)
  (cond ((tpg-p model) (eval-tpg model dataset))
        ((linear-gp-p model) (eval-linear-gp model dataset))
        (t (error "Was expecting a LGP or a TPG. Got ~A instead." model))))

(defun first-objective (score)
  (cdaadr score))

(defun select-top-R (fitness-scores R &key (key #'first-objective) (pred #'<))
  "Given a set of SCORES of (INDIVIDUAL-ID ((OBJ1 .. OBJ1-SCORE)..(OBJN .. OBJN-SCORE)))
   Return the top R PERCENTAGE of INDIVIDUALs using SCORES sorted by KEY.
   By default, the KEY is OBJ1-SCORE and the LOWEST VALUES are selected."
  (let ((n (floor (* R (length fitness-scores)))))
    (when (and (> R 0) (equal n 0))
      (incf n))
    (mapcar #'car (subseq (sort fitness-scores pred :key key) 0 n))))

(defun selection (model scores fn args)
  (cond ((tpg-p model)
         (let* ((team-table (build-team-table model))
                (survivor-ids (funcall fn scores args))
                (survivors (loop for id in survivor-ids
                                 collect (find-team-by-id model id :team-table team-table)))
                (model-after-selection `(TPG (LEARNERS ,@(learners model))
                                             (TEAMS ,@survivors)))
                (internal-team-ids-after-selection (internal-team-ids model-after-selection))
                (team-ids (mapcar #'team-id (teams model-after-selection)))
                (missing-internal-team-ids (set-difference internal-team-ids-after-selection team-ids)))
           ;; add missing internal teams BEFORE pruning learners
           (when missing-internal-team-ids
             (let ((missing-teams (loop for id in missing-internal-team-ids
                                        collect (find-team-by-id model id :team-table team-table))))
               (setf model-after-selection
                     `(TPG (LEARNERS ,@(learners model))
                           (TEAMS ,@(append missing-teams (teams model-after-selection)))))))
           ;; now safely remove any truly dangling learners
           (remove-dangling-learners model-after-selection)))
        ((linear-gp-p model)
         (let* ((program-table (build-program-table model))
                (survivor-ids (funcall fn scores args))
                (survivors (loop for id in survivor-ids
                                 collect (find-program-by-id model id :program-table program-table))))
           `(LINEAR-GP ,@survivors)))))

(defun fill-N-offspring (model parents N)
  (cond ((tpg-p model)
         (let ((new-learners '())
               (new-teams '()))
           (loop while (< (length new-teams) N)
                 do (let ((parent (random-choice parents)))
                      (multiple-value-bind (new-team new-learner) (mutate-team model parent)
                        (when new-learner
                          (push new-learner new-learners))
                        (unless (equal (team-id new-team) (team-id parent))
                          (push new-team new-teams)))))
           `(TPG (LEARNERS ,@(append (learners model) (nreverse new-learners)))
                 (TEAMS ,@(append (teams model) (nreverse new-teams))))))
        
        ((linear-gp-p model)
         (let ((new-programs '()))
           (loop while (< (length new-programs) N)
                 do (let ((parent (random-choice parents)))
                      (push (mutate-program parent) new-programs)))
           `(LINEAR-GP ,@(append (programs model) (nreverse new-programs)))))))
                                       
(defun breeder (model dataset &optional (R 0.20))
  (let* ((population-size (experiment-population-size *experiment*))
         (scores (execute model dataset))
         (model-after-selection (selection model scores #'select-top-R R))
         (parents (cond ((tpg-p model)
                         (intersection (root-teams model) (root-teams model-after-selection)) :test #'equal)
                        ((linear-gp-p model)
                         (programs model-after-selection))))
         (gap (cond ((tpg-p model)
                     (- population-size (length (teams model-after-selection))))
                    ((linear-gp-p model)
                     (- population-size (length (programs model-after-selection)))))))
    (fill-N-offspring model-after-selection parents gap)))
  
(defun evolve (strategy dataset &key (budget 100) (mode 'lgp))
  (clear-cache)
  (let ((model (case mode
                 (lgp (make-linear-gp))
                 (tpg (make-tpg))
                 (otherwise (error "Mode is not supported. ~A~%" mode)))))
    (loop repeat budget
          do (progn
                (setf model (funcall strategy model dataset))))
    model))

