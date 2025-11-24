(defun scores-matrix (tpg ds)
  "Calculates the accuracy of TPG's root teams on a data subset DS.
   Returns a 2D array where each row is a root team and each column is a datapoint."
  (let* ((root-teams (root-team-ids tpg))
         (score-matrix (make-array (list (length root-teams) (length ds)))))
    (loop for root-team in root-teams
          for i from 0
          do (loop for trajectory in ds
                   for observation = (observation trajectory)
                   for action = (action trajectory)
                   for reward = (reward trajectory)
                   for j from 0
                   do (setf (aref score-matrix i j)
                            (if (equal action (execute-team tpg root-team observation))
                                1
                                0))))
    score-matrix))

(defun distinction-matrix (scores ds)
  "A (I^2-I)*N matrix that says whether individual I
   is better than individual J at datapoint N"
  (let* ((individuals (array-dimension scores 0))
         (datapoints (array-dimension scores 1))
         (rewards (rewards ds))
         (distinction-matrix (make-array (list datapoints (- (* individuals individuals) individuals)))))

    ;; Flatten (i, j) into a single index in [0, n^2 - n)
    (flet ((pair-index (i j n) (if (< j i)
                           (+ (* i (1- n)) j)
                           (+ (* i (1- n)) (- j 1)))))
      (loop for individual_i below individuals
            do (loop for individual_j below individuals
                     unless (= individual_i individual_j)
                       do (loop for datapoint below datapoints
                                do (setf (aref distinction-matrix datapoint (pair-index individual_i individual_j individuals))
                                         (if (> (aref scores individual_i datapoint) (aref scores individual_j datapoint))
                                             1
                                             0))))))
      distinction-matrix))

(defun fitness-sharing (distinction-matrix)
  "Column-wise normalize DISTINCTION-MATRIX by the per-column
   values from CALCULATE-FITNESS-SHARING."
  (flet ((calculate-fitness-sharing (distinctions) 
                                        ; Returns a vector of values indicating how many individuals got each datapoint correct.
                                        ; Assumes rows are individuals, columns are datapoints."
           (let* ((num-distinctions (array-dimension distinctions 0)) ; rows
                  (num-datapoints (array-dimension distinctions 1)) ; cols
                  (difficulty (make-array num-datapoints)))
             (loop for j from 0 below num-datapoints
                   do (setf (aref difficulty j)
                            (loop for i from 0 below num-distinctions
                                  sum (aref distinctions i j))))
             difficulty)))
    (let* ((datapoints   (array-dimension distinction-matrix 0)) ; rows (D)
           (distinctions (array-dimension distinction-matrix 1)) ; cols (M)
           (share        (calculate-fitness-sharing distinction-matrix))
           (normalized   (make-array (list datapoints distinctions)
                                     :initial-element 0)))
      (loop for m below distinctions do
        (let ((s (aref share m)))
          (loop for d below datapoints do
            (setf (aref normalized d m)
                  (if (zerop s)
                      0
                      (/ (aref distinction-matrix d m) s))))))
      normalized)))

(defun rank-rows-by-sum (matrix)
  "Given a matrix, rank points (rows) according to the sum of their rows.
   A lower rank implies a higher sum."
  (flet ((rank-values (values)
           (let* ((sorted (stable-sort (copy-list values) #'>))
                  (ranks (loop for v in values
                               collect (1+ (position v sorted :test #'=)))))
             ranks)))
    (let* ((datapoints (array-dimension matrix 0))
           (distinctions (array-dimension matrix 1))
           (sums (loop for datapoint below datapoints
                       collect (loop for distinction below distinctions
                                     sum (aref matrix datapoint distinction)))))
      (rank-values sums))))
                   
(defun rank-teams (scores)
  (let ((individuals (array-dimension scores 0))
        (datapoints (array-dimension scores 1)))
    (loop for individual below individuals
          collect (loop for datapoint below datapoints
                        sum (/ (aref scores individual datapoint) datapoints)))))

(defun select-teams (ranks gap)
  (let* ((n (ceiling (* gap (length ranks))))
         (indexed (loop for team in ranks
                      for i from 0
                        collect (cons i team)))
         (sorted (sort (copy-list indexed) #'> :key #'cdr))
         (highest-indices (mapcar #'car (subseq sorted 0 n))))
    highest-indices))

(defun select-datapoints (ranks gap)
  (let* ((n (ceiling (* gap (length ranks))))
         (indexed (loop for x in ranks
                        for i from 0
                        collect (cons i x)))
         (sorted (sort (copy-list indexed) #'< :key #'cdr))
         (lowest-indices (mapcar #'car (subseq sorted 0 n))))
    lowest-indices))


(defun sample-from-dataset-until-full (subset subset-size dataset)
  (loop while (< (length subset) subset-size)
        do (setf subset (append subset (list (random-choice dataset)))))
  subset)


(defun coevolve (dataset &key
                           (teams-gap 0.50)
                           (datapoints-gap 0.80)
                           (budget 1000)
                           (subset-size 1000))
  (clear-cache)
  ;; initial data subset, initial tpg
  (let ((tpg (make-tpg))
        (ds (sample-from-dataset-until-full '() subset-size dataset)))
    (loop repeat budget
          for generation from 1
          ;; calculate score matrix on data subset
          do (let* ((scores (scores-matrix tpg ds))
                    ;; calculate distinctions
                    (distinctions (distinction-matrix scores ds))
                    ;; normalize by fitness sharing
                    (normalized (fitness-sharing distinctions))
                    ;; rank datapoints by distinctions
                    (ranked-ds (rank-rows-by-sum normalized))
                    (selected-datapoints (loop for idx in (select-datapoints ranked-ds datapoints-gap)
                                               collect (elt ds idx)))
                    (root-teams (root-team-ids tpg))
                    (ranked-teams (rank-teams scores))
                    (selected-teams (loop for idx in (select-teams ranked-teams teams-gap)
                                          collect (elt root-teams idx))))
               ;; update tpg
               (let* ((tpg-after-selection (select-ids tpg selected-teams))
                      (parents (intersection (root-teams tpg) (root-teams tpg-after-selection))))
                 (setf tpg (fill-N-offspring tpg-after-selection parents (- (experiment-population-size *experiment*) (length (teams tpg-after-selection))))))
               ;; update the data subset
               (setf ds (sample-from-dataset-until-full selected-datapoints subset-size dataset))
               (format t "Validation accuracy: ~F~%" (best-reward (eval-tpg tpg *validation-set*))))
          do (format t "~A root teams ~A teams ~A~%" generation (length (root-teams tpg)) (length (teams tpg))))
    tpg))
  
