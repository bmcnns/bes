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
      (reduce #'+ (mapcar #'mean-squared-error y-predicted y-truth))))

(defun complexity (genotype)
  "Return the complexity of GENOTYPE, defined here as its length."
  (length genotype))

(defparameter *objectives*
  `((:complexity . ,#'(lambda (genotype truth prediction)
                     (declare (ignore truth prediction))
                     (complexity genotype)))
    (:mean-squared-error . ,#'(lambda (genotype truth prediction)
                             (declare (ignore genotype))
                                (mean-squared-error truth prediction))))
  "The available objectives to optimize in a fitness function.
   This maps between a defined EXPERIMENT and the dynamic fitness
   function being generated to lazy evaluate objectives")

                                        ; Fitness Wrappers 

(defun minimize (fn)
  "Wrap FN in a lambda that forwards arguments unchanged.
   This is syntactic sugar for NSGA-II which assumes minimization."
  (lambda (&rest args)
    (apply fn args)))

(defun maximize (fn)
  "Wrap FN in a lambda that negates its output.
   This transforms a maximization objective into a minimization one for NSGA-II."
  (lambda (&rest args)
    (let ((result (apply fn args)))
      (if (listp result)
          (mapcar #'- result)
          (- result)))))

                                        ; Fitness Calculation

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
