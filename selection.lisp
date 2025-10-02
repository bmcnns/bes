(in-package :bes)

;;; Single-objective Optimization

(defun tournament-selection (fitness-scores)
  "Select an individual from RANKED-POPULATION using tournament selection.
   The tournament size is specified by *EXPERIMENT*."
   ; with replacement
  (let* ((tournament-size (experiment-tournament-size *experiment*))
         (tournament (loop repeat tournament-size
                           collect (random-choice fitness-scores)))
         (objective-scores (mapcar #'cdaadr tournament))
         (ids (mapcar #'car tournament))
         (winner-idx (argmin objective-scores))
         (winner (elt ids winner-idx)))
    (format t "~A~%" winner)))


;; unfortunately the same as below

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


