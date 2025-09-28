(in-package :bes)

;;; Single-objective Optimization

(defun tournament-selection (ranked-population tournament-size)
  "Select an individual from RANKED-POPULATION using tournament selection.
   The tournament size is specified by *EXPERIMENT*."
  (let* ((tournament (loop repeat tournament-size
                           collect (random-choice ranked-population))))
    (argmin tournament #'cadr)))

(defun single-objective-selection (ranked-population)
  "Return a list of individuals from RANKED-POPULATION selected by tournament selection.
   The number of selected individuals matches the target population size in *EXPERIMENT*."
  (let ((desired-population-size (experiment-population-size *experiment*)))
    (loop repeat desired-population-size
          collect (tournament-selection ranked-population (experiment-tournament-size *experiment*)))))

;;; Multi-objective Optimization

(defun dominates-p (ranked-individual-a ranked-individual-b)
  "Return T if RANKED-INDIVIDUAL-A dominates RANKED-INDIVIDUAL-B.
   Each individual is of the form (genotype . scores) where scores is a
   list of objective values. Assumes all objectives are to be minimized."
  (let* ((better-or-equal t)
         (strictly-better nil)
         (individual-a-scores (cdr ranked-individual-a))
         (individual-b-scores (cdr ranked-individual-b)))
    (loop for a in individual-a-scores
          for b in individual-b-scores do
            (cond
              ((< a b) (setf strictly-better t))
              ((> a b) (setf better-or-equal nil))))
    (and better-or-equal strictly-better)))

(defun pareto-front (population)
  "Return the Pareto front (non-dominated set) from POPULATION.
   Each individual is of the form (label . scores)."
  (remove-if
   (lambda (p)
     (some (lambda (q) (dominates-p q p))
           population))
   population))

;; unfortunately the same as below
(defun non-dominated-sorting (ranked-population)
  "Perform non-dominated sorting on RANKED-POPULATION.
   Returns a list of fronts, where each front is a list of non-dominated individuals."
  (let ((remaining (copy-list ranked-population))
        (fronts '()))
    (loop while remaining do
      (let ((front (pareto-front remaining)))
        (push front fronts)
        (setf remaining (set-difference remaining front :test #'equal))))
    (nreverse fronts)))

;; you did not write this and therefore you do not understand it
(defun crowding-distances (front)
  "Compute crowding distance for each individual in FRONT.
   Returns an alist mapping genotypes to crowding distances.
   Used to preserve diversity in NSGA-II selection."
  (let* ((objectives (loop for i from 1 below (length (first front)) collect i)) ; skip label
         (distances (mapcar (lambda (pt) (cons (first pt) 0.0)) front)))
    (dolist (obj-index objectives distances)
      (let* ((sorted (sort (copy-list front) #'< :key (lambda (pt) (nth obj-index pt))))
             (f-min (nth obj-index (first sorted)))
             (f-max (nth obj-index (car (last sorted))))
             (range (max (- f-max f-min) 1e-9)))
        (setf distances
              (mapcar
               (lambda (entry)
                 (destructuring-bind (label . current-distance) entry
                   (cond
                     ((or (equal label (first (first sorted)))
                          (equal label (first (car (last sorted)))))
                      (cons label most-positive-fixnum))
                     (t
                      (let* ((idx (position label sorted :key #'first :test #'equal))
                             (prev (nth (- idx 1) sorted))
                             (next (nth (+ idx 1) sorted))
                             (prev-val (nth obj-index prev))
                             (next-val (nth obj-index next))
                             (partial (/ (- next-val prev-val) range)))
                        (cons label (+ current-distance partial)))))))
               distances))))))

;; same as above
(defun sort-by-crowding-distance (front)
  "Return a new list of individuals from FRONT sorted by crowding distance in descending order.
   Used in NSGA-II to break ties within a Pareto front."
  (let* ((distances (crowding-distances front)))
    (sort (copy-list front)
          #'>
          :key (lambda (ind)
                 (let ((genotype (first ind)))
                   (cdr (assoc genotype distances :test #'equal)))))))


