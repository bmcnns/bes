(in-package :bes)

(defun make-counter ()
  (let ((count 0))
    (lambda ()
      (incf count))))

(defun random-choice (seq)
  "Returns a random element from the sequence SEQ."
  (elt seq (random (length seq))))

(defun random-range (low high)
  "Returns a random number from [LOW, HIGH].
   Preserves the datatype."
  (if (or (floatp high) (floatp low))
      (+ low (* (random 1.0) (- high low)))
      (+ low (random (1+ (- high low))))))

(defun coin-flip (p)
  "Returns T with likelihood p."
  (if (< (random-range 0.0 1.0) p)
      t
      nil))
