(in-package :bes)

                                        ; Fitness metrics

(defmacro minimize (fn)
  "Syntactic sugar: no transformation needed since NSGA-II assumes minimization"
  `(lambda (&rest args)
     (apply ,fn args)))

(defmacro maximize (fn)
  "Negate the objective since NSGA-II assumes minimization"
  `(lambda (&rest args)
     (let ((result (apply ,fn args)))
       (if (listp result)
           (mapcar #'- result)
           (- result)))))
                                        ; Single-objective Optimization

(defun tournament-selection (ranked-population experiment)
  (let* ((tournament-size (experiment-tournament-size experiment))
         (tournament (loop repeat tournament-size
                           collect (random-choice ranked-population))))
    (car (argmax tournament #'cdr))))

(defun single-objective-selection (ranked-population experiment)
  (let ((desired-population-size (experiment-population-size experiment)))
    (loop repeat desired-population-size
          collect (tournament-selection ranked-population experiment))))

                                        ; Multi-objective Optimization

(defun dominates-p (ranked-individual-a ranked-individual-b)
  "Returns T if individual A dominates individual B."
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
  "Return the non-dominated individuals in the population"
  (remove-if
   (lambda (p)
     (some (lambda (q) (dominates-p q p))
           population))
   population))

(defun non-dominated-sorting (ranked-population)
  (let ((remaining (copy-list ranked-population))
        (fronts '()))
    (loop while remaining do
      (let ((front (pareto-front remaining)))
        (push front fronts)
        (setf remaining (set-difference remaining front :test #'equal))))
    (nreverse fronts)))

(defun crowding-distances (front)
  "Returns an alist of (label . distance) for a Pareto front."
  (let* ((objectives (loop for i from 1 below (length (first front)) collect i)) ; skip label
         ;; Initialize distances: ((label . 0.0) ...)
         (distances (mapcar (lambda (pt) (cons (first pt) 0.0)) front)))
    ;; Loop over each objective dimension
    (dolist (obj-index objectives distances)
      (let* ((sorted (sort (copy-list front) #'< :key (lambda (pt) (nth obj-index pt))))
             (f-min (nth obj-index (first sorted)))
             (f-max (nth obj-index (car (last sorted))))
             (range (max (- f-max f-min) 1e-9)))
        ;; Update distances
        (setf distances
              (mapcar
               (lambda (entry)
                 (destructuring-bind (label . current-distance) entry
                   (cond
                     ;; Boundaries get ∞
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

(defun sort-by-crowding-distance (front)
  "Sorts a Pareto front by descending crowding distance"
  (let* ((distances (crowding-distances front)))
    (sort (copy-list front)
          #'>
          :key (lambda (ind)
                 (let ((genotype (first ind)))
                   (cdr (assoc genotype distances :test #'equal)))))))


