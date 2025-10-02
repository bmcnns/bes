(defun execute (model dataset)
  (cond ((tpg-p model) (eval-tpg model dataset))
        ((linear-gp-p model) (eval-linear-gp model dataset))
        (t (error "Was expecting a LGP or a TPG. Got ~A instead." model))))

;; known bug here that if there are duplicates in the tournament, then
;; the whole population won't be replenished.
(defun selection (model fitness-scores method offspring-size)
  (let ((offspring-ids (loop repeat offspring-size
                             collect (funcall method fitness-scores))))
    (cond ((tpg-p model) (remove-dangling-learners `(TPG
                           (LEARNERS ,@(learners model))
                           (TEAMS ,@(remove-if-not (lambda (team) (member (team-id team) offspring-ids)) (teams model))))))
          ((linear-gp-p model) `(LINEAR-GP
                                 ,@(remove-if-not (lambda (individual) (member (program-id individual) offspring-ids)) (programs model))))
          (t (error "Not a TPG")))
    ))

(defun replacement (model)
  (cond ((tpg-p model) 
         (let* ((mutated-tpg (mutate-tpg model))
                (new-learners (set-difference (learners mutated-tpg) (learners model) :test 'equal))
                (new-teams (set-difference (teams mutated-tpg) (teams model) :test 'equal)))
           `(TPG (LEARNERS ,@(append (learners model) new-learners)) (TEAMS ,@(append (teams model) new-teams)))))
        ((linear-gp-p model)
         (let* ((mutated-lgp (mutate-linear-gp model))
                (new-programs (set-difference (programs mutated-lgp) (programs model))))
           `(LINEAR-GP ,@(append new-programs (programs model)))))))

(defun run-strategy (strategy dataset &key (generations 1) (mode 'lgp))
  (let ((population (case mode
                      ('lgp (make-linear-gp))
                      ('tpg (make-tpg))))
        (population-size (experiment-population-size *experiment*)))
    (loop repeat generations
          for generation from 1
          do (let ((fitness-scores (execute population dataset)))
               (let ((parent-pool (selection population fitness-scores #'tournament-selection (/ population-size 2))))
                 (setf population (replacement parent-pool)))))
    population))

                             
               
  
            
