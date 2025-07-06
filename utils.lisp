(in-package :bes)

(defun random-choice (list)
  "Randomly select and return an element from LIST."
  (nth (random (length list)) list))

(defun random-range (x y)
  "Return a random number in the inclusive range [X, Y].
   If either X or Y is a float, a random float is returned;
   otherwise, a random integer."
  (+ x (random (+ 1 (- y x)))))

(defun pretty-print (genotype)
  "Print a human-readable version of the GENOTYPE (a list of instructions)
   formatted in a style resembling RISC assembly."
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
  "Return T if STRING starts with PREFIX, NIL otherwise."
  (let ((plen (length prefix)))
    (and (<= plen (length string))
         (string= prefix (subseq string 0 plen)))))

(defun weighted-coin-flip (p)
  "Return T with probability P, NIL otherwise."
  (< (random 1.0) p))

(defun bernoulli (p)
  "Return T with probability P, NIL otherwise.
   Simulates a Bernoulli trial with success probability P."
  (< (random 1.0) p))

(defun weighted-random-choice (choices weights)
  "Select and return one element from CHOICES with probability proportional to WEIGHTS.
   CHOICES is a list of elements, WEIGHTS is a list of corresponding numeric weights."
  (let* ((total (reduce #'+ weights))
         (r (random total)))
    (loop for choice in choices
          for weight in weights
          for sum = weight then (+ sum weight)
          when (> sum r)
            return choice)))


(defun test-weighted-random-choice ()
  "Run a Monte Carlo test to count how often each choice is selected using
   WEIGHTED-RANDOM-CHOICE. Prints the count of each outcome after 100,000 trials."
  (let* ((choices '(bryce cat dog))
         (weights '(0.25 0.5 0.25))
         (counts (make-hash-table)))
    (loop repeat 100000
          do (incf (gethash (weighted-random-choice choices weights) counts 0)))
    (loop for choice in choices
          do (format t "~A: ~D~%" choice (gethash choice counts 0)))))

(defun normal (mean std)
  "Return a single float from a Gaussian distribution with given
   MEAN and STD using the Box-Muller transform."
  (let* ((u1 (random 1.0))
         (u2 (random 1.0))
         (z0 (* (sqrt (* -2 (log u1)))
                (cos (* 2 pi u2)))))
    (+ mean (* std z0))))

(defun zeros (n)
  "Return a float array of length N, initialized with zeros."
  (make-array n :element-type 'single-float :initial-element 0.0))

(defun list->vector (lst)
  "Convert a list of numbers LST into a single-float vector (array)."
  (make-array (length lst)
              :element-type 'single-float
              :initial-contents (mapcar #'(lambda (x) (coerce x 'single-float)) lst)))

(defun argmax (seq fn)
  "Return the element in SEQ for which FN returns the highest value."
  (let ((best (first seq))
        (best-val (funcall fn (first seq))))
    (dolist (item (rest seq) best)
      (let ((val (funcall fn item)))
        (when (> val best-val)
          (setf best item
                best-val val))))))

(defun column (matrix index)
  "Return the column at INDEX from a MATRIX (a list of lists)."
  (mapcar (lambda (row) (nth index row)) matrix))
