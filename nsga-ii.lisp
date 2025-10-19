(in-package :bes)

(defun nsga-ii-better-p (a b)
  (let ((ra (cdr (assoc 'RANK (cadr a))))
        (rb (cdr (assoc 'RANK (cadr b))))
        (ca (cdr (assoc 'CROWDING-DISTANCE (cadr a))))
        (cb (cdr (assoc 'CROWDING-DISTANCE (cadr b)))))
    (cond ((< ra rb) t)
          ((> ra rb) nil)
          ((> ca cb) t)
          ((< ca cb) nil)
          (t (zerop (random 2))))))

(defun nsga-ii-sort (fitness-scores)
  "Sort FITNESS-SCORES such that lower rank is prioritized,
   ties in rank are broken by crowding distance (maximum preferred).
   Ties beyond this are broken randomly."
  (sort (copy-list fitness-scores) #'nsga-ii-better-p))

(defun nsga-ii-selection (fitness-scores &key (tournament-size 2))
  "Perform a lexicographic tournament over FITNESS-SCORES.
Each element is (ID ((OBJ1 . VAL1) (OBJ2 . VAL2) ...)).
Lower objective values are preferred lexicographically."
  (let* ((candidates (loop repeat tournament-size
                           collect (random-choice fitness-scores)))
         (winner (first candidates)))
    (dolist (competitor (rest candidates))
      (when (nsga-ii-better-p competitor winner)
        (setf winner competitor)))
    (car winner))) ; return ID only

(defun truncate-nsga-ii (fitness-scores N)
  (mapcar #'car (subseq (nsga-ii-sort fitness-scores) 0 N)))
                                         
(defun scores->rank+crowding-distance (scores)
  "Given a set of SCORES, transform the objectives to their
   respective Pareto ranks and crowding distances."
  (let ((fronts (non-dominated-sorting scores))
        (rank+crowd '()))
    ;; assign rank and crowding
    (loop for i from 1 for front in fronts do
          (let ((distances (crowding-distances scores front)))
            (dolist (pair distances)
              (let ((id (car pair))
                    (cd (cdr pair)))
                (push (cons id (list (list (cons 'RANK i) (cons 'CROWDING-DISTANCE cd)))) rank+crowd)))))
    (setf rank+crowd (sort rank+crowd #'nsga-ii-better-p))
    rank+crowd))

(defun nsga-ii (model dataset log-fn)
  "Canonical NSGA-II strategy 
   Works for both TPG and Linear-GP models."
  (let* ((parent-scores (execute model dataset))
         (pop-size (experiment-population-size *experiment*))
         (rank+crowding-distance (scores->rank+crowding-distance parent-scores))
         (parent-ids (loop repeat pop-size
                           collect (nsga-ii-selection rank+crowding-distance :tournament-size 2)))
         (team-table (build-team-table model))
         (learner-table (build-learner-table model))
         (parents (mapcar (lambda (id) (find-team-by-id model id :team-table team-table)) parent-ids))
         (new-learners '())
         (new-teams '()))
    (block make-offspring
      (loop while (< (length (remove-duplicates new-teams)) pop-size)
            do
               (loop for parent in parents
                     do (multiple-value-bind (new-team new-learner)
                            (mutate-team model parent)
                          (unless (gethash (team-id new-team) team-table)
                            (push new-team new-teams))
                          (when (and new-learner
                                     (not (gethash (learner-id new-learner) learner-table)))
                            (push new-learner new-learners)))
                        ;; Break out if outer condition met
                        (when (>= (length new-teams) pop-size)
                          (return-from make-offspring)))))
    (let* ((combined-tpg (add-teams-to-tpg
                          (add-learners-to-tpg model new-learners)
                          new-teams))
           (combined-scores (execute combined-tpg dataset))
           (combined-rank+cd (scores->rank+crowding-distance combined-scores))
           (fittest-ids (mapcar #'car (subseq combined-rank+cd 0 pop-size)))
           (fittest-scores (remove-if-not (lambda (id) (member id fittest-ids)) combined-scores :key #'car))
           (final-tpg (rebuild-tpg combined-tpg fittest-ids)))
      (funcall log-fn fittest-scores)
      ;; (format t "combined scores: ~A~%" combined-scores)
      ;; (format t "Len combined scores: ~A~%" (length combined-scores))
      ;; (format t "combined rank+cd: ~A~%" combined-rank+cd)
      ;; (format t "~A~%" combined-rank+cd)
      (format t "len teams fittest ~A~%" (length (teams final-tpg)))
      (format t "len root teams fittest ~A~%" (length (root-teams final-tpg)))
      final-tpg)))

