(defun execute (candidate-solution observation)
  (cond ((program-p candidate-solution)
         (phenotype (program-instructions candidate-solution) observation))
        ((team-p candidate-solution)
         (team-phenotype candidate-solution observation))))

(defun evolve-tpg (dataset)
  (let ((population-size (experiment-population-size *experiment*))
        (tournament-size (experiment-tournament-size *experiment*)))
    
    ;; clear populations
    (setf *teams* (make-hash-table :test 'equal))
    (setf *learners* (make-hash-table :test 'equal))
    (setf *fitness* (make-hash-table :test 'equal))
    
    ;; initialize population
    (loop repeat population-size
          do (generate-learner)
             (generate-team))

    ;; generation loop here
    (let ((ranked-population '()))
      ;; evaluate population on dataset
      (let ((team-ids (loop for k being the hash-keys of *teams* collect k)))
        (loop for team-id in team-ids
              do (progn
                   (format t "~%Evaluating Team ~A~%" team-id)
                   (let ((fitness 0.0))
                     (loop for exemplar in (observations dataset)
                           for action in (actions dataset)
                           do (progn
                                (let* ((team (resolve-team team-id))
                                       (prediction (execute team exemplar)))
                                  (format t "Predicted ~A Actually ~A~%" prediction action)
                                  (incf fitness (cadr (fitness team action prediction))))))
                     (format t "Fitness: ~A~%" fitness)
                     (setf (gethash team-id *fitness*) fitness))))))

    ;; replace the population
    (loop repeat population-size
          collect (let* ((tournament (loop repeat tournament-size
                                           collect (random-choice (loop for k being the hash-keys of *teams* collect k))))
                         (winner-idx (argmin tournament (lambda (tid) (gethash tid *fitness*))))
                         (winner-id (elt tournament winner-idx)))
                    (format t "~%Tournament consists of: ~%")
                    (loop for tid in tournament
                          do (format t "~A: ~A~%" tid (gethash tid *fitness*)))
                    (format t "Winner: ~A" winner-id)
                    winner-id))))
