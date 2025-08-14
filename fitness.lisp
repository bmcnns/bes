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
  `((:complexity . ,#'(lambda (genotype truth prediction population support-matrix residual-matrix)
                        (declare (ignore truth prediction population support-matrix residual-matrix))
                        (complexity genotype)))
    (:mean-squared-error . ,#'(lambda (genotype truth prediction population support-matrix residual-matrix)
                                (declare (ignore genotype population support-matrix residual-matrix))
                                (mean-squared-error truth prediction)))
    (:support . ,#'(lambda (genotype truth prediction population support-matrix residual-matrix)
                     (declare (ignore truth prediction residual-matrix))
                     (support genotype population support-matrix)))
    (:confidence . ,#'(lambda (genotype truth prediction population support-matrix residual-matrix)
                     (declare (ignore truth prediction support-matrix))
                     (confidence genotype population residual-matrix))))

  "The available objectives to optimize in a fitness function.
   This maps between a defined EXPERIMENT and the dynamic fitness
   function being generated to lazy evaluate objectives")

                                        ; Fitness Calculation

(defun make-fitness-function (objectives)
  "Construct a fitness function from a list of OBJECTIVES.
   Each objective is a keyword that maps to a fitness function in *OBJECTIVES*.
   The resulting function accepts (genotype truth prediction) and returns a cons:
   (genotype . list-of-objective-values)
   NOTE: This is lazy evaluation so only the objectives provided will be computed."
  (lambda (genotype truth prediction population support-matrix residual-matrix)
     (cons genotype
           (mapcar
              (lambda (objective)
                (let ((fn (cdr (assoc objective *objectives*))))
                  (unless fn
                    (error "Unknown objective: ~A" objective))
                  (funcall fn genotype truth prediction population support-matrix residual-matrix)))
              objectives))))

(defun fitness (experiment genotype truth prediction population support-matrix residual-matrix)
  "Evaluate the FITNESS of a GENOTYPE using the objectives defined in EXPERIMENT.
   Returns (genotype . (obj1 [obj2]...)"
  (let ((objectives (experiment-objectives experiment)))
    (funcall (make-fitness-function objectives) genotype truth prediction population support-matrix residual-matrix)))

;; Discrete variant
;;
;; (defun best-points (genotype)
;;   "Return residuals for datapoints where GENOTYPE is best."
;;   (loop for (residual . winner) being the hash-values of *point-winners*
;;         when (equalp genotype winner)
;;         collect residual))

;; (defun support (genotype)
;;   "Return count of datapoints where GENOTYPE is best."
;;   (- (length (best-points genotype)))) ; negate for minimization

;; (defun confidence (genotype)
;;   "Sum of squared errors for the genotype's best points."
;;   (let ((residuals (best-points genotype)))
;;     (reduce #'+ (mapcar (lambda (r) (* r r)) residuals))))

;; (defparameter *point-winners* (make-hash-table)) ; datapoint-index → (residual . genotype)

;; (defun assign-errors-to-datapoints (experiment observations truth population)
;;   "Evaluates unique genotypes, stores best genotype per datapoint."
;;   (clrhash *point-winners*)
;;   (with-population population (experiment-num-threads experiment)
;;     (let ((predictions (phenotype individual experiment observations)))
;;       (loop for prediction in predictions
;;             for actual in truth
;;             for j from 0
;;             do (let ((residual (reduce #'+ (mapcar (lambda (p a) (abs (- p a))) prediction actual))))
;;                  (multiple-value-bind (current-entry exists) (gethash j *point-winners*)
;;                    (when (or (not exists) (< residual (car current-entry)))
;;                      (setf (gethash j *point-winners*) (cons residual individual)))))))))
;; (defun export-piecewise-fit (experiment observations truth output-file)
;;   "Export a CSV with columns: x0,x1,...,y0,y1,...,p0,p1,...
;;    One row per datapoint. No residuals or individual IDs."
;;   (with-open-file (out output-file
;;                        :direction :output
;;                        :if-exists :supersede
;;                        :if-does-not-exist :create)
;;     ;; Dynamically determine header from first sample
;;     (let* ((first-obs (first observations))
;;            (first-truth (first truth))
;;            (x-cols (length first-obs))
;;            (y-cols (length first-truth)))
;;       ;; Write header
;;       (format out "~{x~A,~}~{y~A,~}~{p~A,~}~%" ; <- all comma-separated
;;               (loop for i below x-cols collect i)
;;               (loop for i below y-cols collect i)
;;               (loop for i below y-cols collect i)))
;;     ;; Write rows
;;     (loop for obs in observations
;;           for ytrue in truth
;;           for j from 0
;;           for entry = (gethash j *point-winners*)
;;           for individual = (cdr entry)
;;           for predicted = (first (phenotype individual experiment (list obs)))
;;           do (format out "~{~A,~}~{~A,~}~{~A,~}~%" ; <- fixed here
;;                      obs ytrue predicted))))

(defun sum-absolute-error (truth prediction)
  (reduce #'+ (mapcar (lambda (x y) (abs (- x y)))
                      truth
                      prediction)))

(defun calculate-residual-matrix (experiment observations truth population)
  (let* ((num-data-points (length observations))
        (num-individuals (length population))
        (residuals (make-array (list num-individuals num-data-points) :initial-element 0.0)))
                                        ; for each individual, we have predictions across all the dataset
    (loop for individual in population
          for i from 0
          do (loop for residual in (mapcar #'sum-absolute-error truth (phenotype individual experiment observations))
                   for j from 0
                   do (setf (aref residuals i j) residual)))
    residuals))

(defun invert-residual-matrix (residual-matrix)
  (let* ((rows (array-dimension residual-matrix 0))
        (cols (array-dimension residual-matrix 1))
        (result (make-array (list rows cols))))
    (loop for i from 0 below rows do
      (loop for j from 0 below cols do
        (setf (aref result i j) (/ 1.0 (+ (aref residual-matrix i j) 0.0001)))))
    result))

(defun normalize-inverse-residual-matrix (inverted-residual-matrix)
  (let* ((rows (array-dimension inverted-residual-matrix 0))
         (cols (array-dimension inverted-residual-matrix 1))
         (sums (make-array cols :initial-element 0.0))
         (result (make-array (list rows cols))))
    (loop for i from 0 below cols
          do (loop for j from 0 below rows
                   do (incf (aref sums i) (aref inverted-residual-matrix j i))))
    (loop for i from 0 below rows
          do (loop for j from 0 below cols
                   do (setf (aref result i j) (/ (aref inverted-residual-matrix i j) (aref sums j)))))
    result))

(defun calculate-support-matrix (residual-matrix)
  (normalize-inverse-residual-matrix (invert-residual-matrix residual-matrix)))

(defun support (genotype population support-matrix)
  (let ((cols (array-dimension support-matrix 1))
        (idx (position genotype population)))
    (- (loop for j from 0 below cols
        sum (aref support-matrix idx j)))))

(defun confidence (genotype population residual-matrix)
  "Sum of errors for the datapoints where GENOTYPE is the best fit.
   Returns most-positive-double-float if support is 0."
  (let* ((genotype-idx (position genotype population))
         (num-data-points (array-dimension residual-matrix 1))
         (best-fit-residuals '())) ; A list to hold residuals for best-fit points

    ; Find the best-fit individual for each data point and collect residuals if it's our genotype
    (loop for j from 0 below num-data-points do
      (let ((min-residual most-positive-double-float)
            (best-individual-idx -1))
        (loop for i from 0 below (array-dimension residual-matrix 0) do
          (let ((residual (aref residual-matrix i j)))
            (when (< residual min-residual)
              (setf min-residual residual
                    best-individual-idx i))))
        ; If this genotype is the best for this point, add its residual to our list
        (when (= genotype-idx best-individual-idx)
          (push (aref residual-matrix genotype-idx j) best-fit-residuals))))

    ; If the list of best-fit residuals is empty, support is 0.
    ; In this case, we return a very high confidence value for minimization.
    (if (null best-fit-residuals)
        100000
        (reduce #'+ best-fit-residuals))))

(defun find-best-individual-indices (residual-matrix)
  (let* ((num-data-points (array-dimension residual-matrix 1))
         (best-indices (make-array num-data-points)))
    (loop for j from 0 below num-data-points do
          (let ((min-residual most-positive-double-float)
                (min-index -1))
            (loop for i from 0 below (array-dimension residual-matrix 0) do
                  (let ((residual (aref residual-matrix i j)))
                    (when (< residual min-residual)
                      (setf min-residual residual
                            min-index i))))
            (setf (aref best-indices j) min-index)))
    best-indices))

(defun export-piecewise-fit (experiment observations truth population generation output-file)
  "Export a CSV with columns: x0,x1,...,y0,y1,...,p0,p1,...,gen
One row per datapoint. Each prediction is from the best individual for that point."
  (let* ((residual-matrix (calculate-residual-matrix experiment observations truth population))
         (best-individual-indices (find-best-individual-indices residual-matrix))
         (x-cols (length (first observations)))
         (y-cols (length (first truth))))
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (if (= generation 1)
          (progn 
            ;; Write header
            (format out "~{~A,~}~{~A,~}~{~A,~}gen~%"
                    (loop for i below x-cols collect (format nil "x~A" i))
                    (loop for i below y-cols collect (format nil "y~A" i))
                    (loop for i below y-cols collect (format nil "p~A" i)))))
      ;; Write rows
      (loop for obs in observations
            for ytrue in truth
            for j from 0
            for best-individual-index = (aref best-individual-indices j)
            for best-individual = (nth best-individual-index population)
            for predicted = (first (phenotype best-individual experiment (list obs)))
            do (format out "~{~A,~}~{~A,~}~{~A,~}~A~%"
                       obs ytrue predicted generation)))))
