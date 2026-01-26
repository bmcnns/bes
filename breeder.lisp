(in-package :bes)

(defparameter *TOURNAMENT-SIZE* 3 "Size of the random pool for selection.")

(defun standard-deviation (numbers)
  "Calculates the population standard deviation of a list of numbers."
  (let* ((n (length numbers))
         (mean (/ (reduce #'+ numbers) n)))
    (if (< n 2)
        0.0d0
        (sqrt (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) numbers))
                 n)))))


(defun get-tournament-winner (population score-map)
  "Selects N random teams and returns the one with the lowest score (minimization)."
  (let* ((tournament-size (experiment-tournament-size *experiment*))
         (best-candidate nil)
         (best-score 1.0d9)) ;; Initialize high for minimization
    
    (loop repeat tournament-size do
          (let* ((candidate (random-choice population))
                 (score (gethash candidate score-map)))
            ;; If this candidate's score is lower than our current best, update
            (when (< score best-score)
              (setf best-score score)
              (setf best-candidate candidate))))
    
    best-candidate))

(defun tournament-breeder (tpg dataset generation save-file log-file)
  (let* ((population (tpg-teams tpg))
         (pop-size (length population))
         ;; We use a lock to ensure only one thread writes to the hash table at a time
         (score-map (make-hash-table :test #'eq))
         (map-lock (bt:make-lock "score-map-lock"))
         
         (num-threads (experiment-num-threads *experiment*))
         (num-immigrants (floor (* pop-size 0.10)))
         (num-breeders (- pop-size 1 num-immigrants)))

    ;; --- STEP 1: MULTITHREADED EVALUATION ---
    ;; We distribute the population across threads.
    ;; Each thread calculates the score and then safely pushes it to the map.
    (multi-thread population team num-threads
      (let ((score (eval-team team dataset)))
        (bt:with-lock-held (map-lock)
          (setf (gethash team score-map) score))))

    ;; --- STEP 2: STATISTICS & ELITISM (Single Threaded) ---
    (let* ((scores (loop for team in population collect (gethash team score-map)))
           (best-score (apply #'min scores))
           (best-team (find-if (lambda (team) (= (gethash team score-map) best-score)) 
                               population))
           (std-dev (standard-deviation scores))
           (new-population (list best-team)))

      (with-open-file (stream log-file
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (format stream "~A ~,5F~%" generation best-score))
      
      (format t "~&Gen ~3D | Pop: ~3D | Best: ~8,5F | Std: ~8,5F | New: ~D" 
              generation pop-size best-score std-dev num-immigrants)
      (finish-output)

      (if (zerop (mod generation 50))
          (save-tpg save-file tpg))
      
      ;; --- STEP 3: IMMIGRATION ---
      (loop repeat num-immigrants do
            (push (make-team) new-population))

      ;; --- STEP 4: BREEDING ---
      (loop repeat num-breeders do
            (let* ((parent (get-tournament-winner population score-map))
                   (offspring (mutate-team parent tpg)))
              (push offspring new-population)))

      ;; --- STEP 5: UPDATE TPG ---
      (setf (tpg-teams tpg) new-population)
      
      tpg)))
