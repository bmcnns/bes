(in-package :bes)

(defun random-choice (seq)
  "Returns a random element from the sequence SEQ."
  (elt seq (random (length seq))))

(defun random-range (low high)
  "Returns a random number from [LOW, HIGH].
   Preserves the datatype."
  (if (or (floatp high) (floatp low))
      (+ low (* (random 1.0) (- high low)))
      (+ low (random (1+ (- high low))))))
  
