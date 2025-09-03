(in-package :bes)

                                        ; Fitness Metrics

(defun mean-squared-error (y-predicted y-truth)
  "Compute the Mean Squared Error (MSE) between Y-PREDICTED and Y-TRUTH.
   Handles both flat and batched inputs:
   - If Y-PREDICTED is a flat list of numbers, computes standard MSE.
   - If Y-PREDICTED is a list of lists (batches), computes the total MSE across all batches."
  (if (numberp (first y-predicted))
      ;; Flat case
      (let* ((squared-errors (mapcar (lambda (y1 y2) (expt (- y1 y2) 2))
                                     y-predicted y-truth))
             (sum-squared-errors (reduce #'+ squared-errors)))
        (/ sum-squared-errors (length squared-errors)))
      ;; Batched case
      (/ (reduce #'+ (mapcar #'mean-squared-error y-predicted y-truth)) (length y-predicted))))

(defun complexity (genotype)
  "Return the complexity of GENOTYPE, defined here as its length."
  (length genotype))

(defparameter *objectives*
  `((:complexity . ,#'(lambda (genotype truth prediction)
                        (declare (ignore truth prediction))
                        (complexity genotype)))
    (:mean-squared-error . ,#'(lambda (genotype truth prediction population)
                                (declare (ignore genotype population))
                                (mean-squared-error truth prediction)))
    (:support . ,#'(lambda (genotype truth prediction)
                     (declare (ignore truth prediction ))
                     (support genotype)))
    (:confidence . ,#'(lambda (genotype truth prediction)
                     (declare (ignore truth prediction))
                     (confidence genotype))))

  "The available objectives to optimize in a fitness function.
   This maps between a defined EXPERIMENT and the dynamic fitness
   function being generated to lazy evaluate objectives")

                                        ; Fitness Calculation

;; this seems like a key candidate for data-driven programming
(defun make-fitness-function (objectives)
  "Construct a fitness function from a list of OBJECTIVES.
   Each objective is a keyword that maps to a fitness function in *OBJECTIVES*.
   The resulting function accepts (genotype truth prediction) and returns a cons:
   (genotype . list-of-objective-values)
   NOTE: This is lazy evaluation so only the objectives provided will be computed."
  (lambda (genotype truth prediction)
     (cons genotype
           (mapcar
              (lambda (objective)
                (let ((fn (cdr (assoc objective *objectives*))))
                  (unless fn
                    (error "Unknown objective: ~A" objective))
                  (funcall fn genotype truth prediction)))
              objectives))))

(defun fitness (experiment genotype truth prediction)
  "Evaluate the FITNESS of a GENOTYPE using the objectives defined in EXPERIMENT.
   Returns (genotype . (obj1 [obj2]...)"
  (let ((objectives (experiment-objectives experiment)))
    (funcall (make-fitness-function objectives) genotype truth prediction)))

(defun best-points (genotype)
  "Return residuals for datapoints where GENOTYPE is best."
  (loop for (residual . winner) being the hash-values of *point-winners*
        when (equalp genotype winner)
        collect residual))

(defun support (genotype)
  "Return count of datapoints where GENOTYPE is best."
  (- (length (best-points genotype)))) ; negate for minimization

(defun confidence (genotype)
  "Sum of squared errors for the genotype's best points."
  (let ((residuals (best-points genotype)))
    (reduce #'+ (mapcar (lambda (r) (* r r)) residuals))))

(defparameter *point-winners* (make-hash-table)) ; datapoint-index → (residual . genotype)

;; (setf vs getf vs hash-table)
(defun assign-errors-to-datapoints (experiment observations truth population)
  "Evaluates unique genotypes, stores best genotype per datapoint."
  (clrhash *point-winners*)
  (with-population population (experiment-num-threads experiment)
    (let ((predictions (phenotype individual experiment observations)))
      (loop for prediction in predictions
            for actual in truth
            for j from 0
            do (let ((residual (reduce #'+ (mapcar (lambda (p a) (abs (- p a))) prediction actual))))
                 (multiple-value-bind (current-entry exists) (gethash j *point-winners*)
                   (when (or (not exists) (< residual (car current-entry)))
                     (setf (gethash j *point-winners*) (cons residual individual)))))))))

;; arguably this should be a separate module.
(defun export-piecewise-fit (experiment observations truth gen)
  "Export a CSV with columns: x0,x1,...,y0,y1,...,p0,p1,...,gen
   One row per datapoint. No residuals or individual IDs."
  (with-open-file (out "residuals.csv"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    ;; Dynamically determine header from first sample
    (let* ((first-obs (first observations))
           (first-truth (first truth))
           (x-cols (length first-obs))
           (y-cols (length first-truth)))
      (if (= gen 1)
          ;; Write header
          (format out "~{x~A~^,~},~{y~A~^,~},~{p~A~^,~},gen~%"
                  (loop for i below x-cols collect i)
                  (loop for i below y-cols collect i)
                  (loop for i below y-cols collect i))))
    ;; Write rows
    (loop for obs in observations
          for ytrue in truth
          for j from 0
          for entry = (gethash j *point-winners*)
          for individual = (cdr entry)
          for predicted = (first (phenotype individual experiment (list obs)))
          do (format out "~{~A~^,~},~{~A~^,~},~{~A~^,~},~A~%"
                     obs ytrue predicted gen))))
