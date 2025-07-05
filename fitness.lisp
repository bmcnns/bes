(in-package :bes)

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

(defun fitness (genotype truth prediction)
  "Return the fitness of GENOTYPE as a cons of GENOTYPE and its objectives:
   (complexity, mean-squared-error).
   TRUTH AND PREDICTION are used to compute Mean Squared Error (MSE)."
  (cons genotype
        (list (funcall (minimize #'complexity) genotype)
              (funcall (minimize #'mean-squared-error) truth prediction))))




