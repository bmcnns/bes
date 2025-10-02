(in-package :bes)

(defun pareto-front (fitness-scores)
  "Return the Pareto front (non-dominated set) from POPULATION.
   Each individual is of the form (label . scores)."
  (let ((ids (mapcar #'car fitness-scores)))
    (remove-if
     (lambda (p)
       (some (lambda (q) (dominates-p fitness-scores q p))
             ids))
     ids)))

(defun dominates-p (fitness-scores individual-a-id individual-b-id)
  "Return T if INDIVIDUAL-A dominates INDIVIDUAL-B.
   FITNESS-SCORES is a list of (ID ((OBJ1 . OBJ1-SCORE)...(OBJN . OBJN-SCORE)))
   INDIVIDUAL-A and INDIVIDUAL-B are IDs of the individual in FITNESS-SCORES."
  (let ((individual-a (find-if (lambda (score) (equal (car score) individual-a-id)) fitness-scores))
        (individual-b (find-if (lambda (score) (equal (car score) individual-b-id)) fitness-scores)))
    (if (and individual-a individual-b)
        (let ((individual-a-scores (mapcar #'cdr (cadr individual-a)))
              (individual-b-scores (mapcar #'cdr (cadr individual-b))))
          (let ((better-or-equal t)
                (strictly-better nil))
            (loop for a in individual-a-scores
                  for b in individual-b-scores
                  do
                     (cond ((< a b) (setf strictly-better t))
                           ((> a b) (setf better-or-equal nil))))
            (and better-or-equal strictly-better)))
        (error "Individual not found. Either ~A or ~A~%" individual-a-id individual-b-id))))

(defun non-dominated-sorting (fitness-scores)
  "Return a list of fronts, each a list of IDs, from FITNESS-SCORES."
  (let ((remaining (copy-list fitness-scores))
        (fronts '()))
    (loop while remaining do
      (let* ((front-ids (pareto-front remaining)));; => list of IDs
        ;; record this front of IDs
        (push front-ids fronts)
        ;; drop those IDs from remaining
        (setf remaining
              (remove-if (lambda (item)
                           (member (car item) front-ids :test #'equal))
                         remaining))))
    (nreverse fronts)))

(defun obj-val (pt obj)
  "PT = (ID ((OBJ . VAL) ...)); return VAL for OBJ."
  (cdr (assoc obj (second pt))))

(defun crowding-distances (fitness-scores pareto-front)
  "Sorts the PARETO FRONT according to their sum of differences
   in OBJECTIVES in FITNESS-SCORES."
  (let* ((relevant-scores (remove-if-not
                           (lambda (score) (member (car score) pareto-front))
                           fitness-scores))
         (n (length relevant-scores))
         (objectives (mapcar #'car (second (car relevant-scores))))
         (dist (make-hash-table :test 'equal)))
    ;; init every ID to 0.0
    (dolist (pt relevant-scores)
      (setf (gethash (car pt) dist) 0.0))

    ;; edge case: N <= 2 → all ∞
    (when (<= n 2)
      (dolist (pt relevant-scores)
        (setf (gethash (car pt) dist) most-positive-fixnum))
      (return-from crowding-distances
        (mapcar (lambda (id) (cons id (gethash id dist)))
                pareto-front)))

    ;; --- per-objective pass ---
    (dolist (obj objectives)
      (let* ((sorted (sort (copy-list relevant-scores) #'<
                           :key (lambda (pt) (obj-val pt obj))))
             (fmin   (obj-val (first sorted) obj))
             (fmax   (obj-val (car (last sorted)) obj))
             (range  (max (- fmax fmin) 1e-9))
             (first-id (caar sorted))
             (last-id  (caar (last sorted))))
        ;; boundaries → ∞ for this objective
        (setf (gethash first-id dist) most-positive-fixnum)
        (setf (gethash last-id  dist) most-positive-fixnum)
        ;; interior → add normalized neighbor gap (skip if already ∞)
        (loop for i from 1 below (1- n) do
              (let* ((mid   (nth i sorted))
                     (id    (car mid))
                     (curr  (gethash id dist))
                     (prev  (nth (1- i) sorted))
                     (next  (nth (1+ i) sorted)))
                (unless (eql curr most-positive-fixnum)
                  (incf (gethash id dist)
                        (/ (- (obj-val next obj)
                              (obj-val prev obj))
                           range)))))))

    (nreverse (mapcar (lambda (id) (cons id (gethash id dist)))
            pareto-front))))
    
    
