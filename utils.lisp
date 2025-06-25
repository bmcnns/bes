(defun random-choice (list)
  (nth (random (length list)) list))

(defun random-range (x y)
  (+ x (random (+ 1 (- y x)))))

(defun pretty-print (individual)
  (loop for instr in individual
        do (let* ((dest (first instr))
                  (opcode (nth 1 instr))
                  (arity (lookup-arity opcode)))
             (if (= arity 1)
                 (let ((x (nth 2 instr)))
                   (format t "~A = ~A ~A~%" dest opcode x))
                 (let ((x (nth 2 instr))
                       (y (nth 3 instr)))
                   (format t "~A = ~A ~A ~A ~%" dest x opcode y))))))

(defun string-prefix-p (prefix string)
  (let ((plen (length prefix)))
    (and (<= plen (length string))
         (string= prefix (subseq string 0 plen)))))

(defun weighted-coin-flip (p)
  "Return T with probability P, NIL with probability 1 - P."
  (< (random 1.0) p))

;; TODO: replace weighted-coin-flip with bernoulli naming
(defun bernoulli (p)
  "Return T with probability P, NIL with probability 1 - P."
  (< (random 1.0) p))


(defun weighted-random-choice (choices weights)
  (let ((windows (loop for weight in weights
                       for sum = weight then (+ sum weight)
                       collect (list (- sum weight) sum)))
        (cursor (random 1)))
    (loop for window in windows
          do (destructuring-bind (start end) window
               (when (and (>= cursor start) (< cursor end))
                 (return window))))))

(defun weighted-random-choice (choices weights)
  (let* ((total (reduce #'+ weights))
         (r (random total)))
    (loop for choice in choices
          for weight in weights
          for sum = weight then (+ sum weight)
          when (> sum r)
            return choice)))


(defun test-weighted-random-choice ()
  (let* ((choices '(bryce cat dog))
         (weights '(0.25 0.5 0.25))
         (counts (make-hash-table)))
    (loop repeat 100000
          do (incf (gethash (weighted-random-choice choices weights) counts 0)))
    (loop for choice in choices
          do (format t "~A: ~D~%" choice (gethash choice counts 0)))))

(defun normal (mean std)
  "Generate a single Gaussian random number with given MEAN and STD using Box-Muller."
  (let* ((u1 (random 1.0))
         (u2 (random 1.0))
         (z0 (* (sqrt (* -2 (log u1)))
                (cos (* 2 pi u2)))))
    (+ mean (* std z0))))

