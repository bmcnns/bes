(defun mean-squared-error (y-predicted y-truth)
  (if (numberp (first y-predicted))
      ;; Flat case
      (let* ((squared-errors (mapcar (lambda (y1 y2) (expt (- y1 y2) 2))
                                     y-predicted y-truth))
             (sum-squared-errors (reduce #'+ squared-errors)))
        (/ sum-squared-errors (length squared-errors)))
      ;; Batched case
      (mapcar #'mean-squared-error y-predicted y-truth)))

(defun negative-mean-squared-error (y-predicted y-truth)
  (if (numberp (first y-predicted))
      ;; Flat case
      (let* ((squared-errors (mapcar (lambda (y1 y2) (expt (- y1 y2) 2))
                                     y-predicted y-truth))
             (sum-squared-errors (reduce #'+ squared-errors)))
        (- (/ sum-squared-errors (length squared-errors))))
      ;; Batched case
      (mapcar #'negative-mean-squared-error y-predicted y-truth)))


(defun fitness (genotype metric truth prediction)
  (cons genotype (reduce #'+ (funcall metric truth prediction))))

