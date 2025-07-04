
(defun mean-squared-error (y-predicted y-truth)
  (if (numberp (first y-predicted))
      ;; Flat case
      (let* ((squared-errors (mapcar (lambda (y1 y2) (expt (- y1 y2) 2))
                                     y-predicted y-truth))
             (sum-squared-errors (reduce #'+ squared-errors)))
        (/ sum-squared-errors (length squared-errors)))
      ;; Batched case
      (reduce #'+ (mapcar #'mean-squared-error y-predicted y-truth))))

(defun complexity (genotype)
  (length genotype))

(defun fitness (genotype truth prediction)
  (cons genotype
        (list (funcall (minimize #'complexity) genotype)
              (funcall (minimize #'mean-squared-error) truth prediction))))




