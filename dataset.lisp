(in-package :bes)

(defmacro defdataset (name)
  "Define a global variable containing a dataset loaded from 'datasets/<name>'.
   The dataset file should be a serialized Lisp object readable by 'read'.
   The NAME symbol will be bound as a global variable.

   Example:
   (defdataset *Minimal-Hopper-Expert-v5*) => loads 'datasets/Minimal-Hopper-Expert-v5'
   and binds to *Minimal-Hopper-Expert-v5*"
  (let* ((dataset-name (string-trim "*" (symbol-name name)))
         (file-name (concatenate 'string "datasets/" dataset-name)))
    `(defparameter ,name
       (with-open-file (in ,file-name :direction :input)
         (read in)))))

(defun batch (dataset start end)
  "Return a subsequence of DATASET from START to END (exclusive).
   This is syntactic sugar for `(subseq data start end)"
  (subseq dataset start end))

(defun sample (dataset experiment)
  "Sample a batch of transitions from DATASET based on the EXPERIMENT's batch size.
   If the dataset has fewer elements than the batch size, return the full dataset.
   Otherwise, return a contiguous random batch of size 'experiment-batch-size'."
  (let ((batch-size (experiment-batch-size experiment)))
    (if (>= batch-size (length dataset))
        dataset
        (progn
          (let* ((start (random (- (length dataset) batch-size)))
                 (end (+ start batch-size)))
            (batch dataset start end))))))

(defun observations (transitions)
  "Return a list of observations from TRANSITIONS.
   Each element of TRANSITIONS is expected to be a list where the first element is the observations."
  (mapcar #'car transitions))

(defun actions (transitions)
  "Return a list of actions from TRANSITIONS.
   Each element of TRANSITIONS is expected to be a list where the second element is the action taken."
  (mapcar #'cadr transitions))

(defun rewards (transitions)
  "Return a list of rewards from TRANSITIONS.
   Each element of TRANSITIONS is expected to be a list where the third element is
   the scalar reward received after the action taken."
  (mapcar #'caddr transitions))

(defun terminations (transitions)
  "Return a list of termination flags from TRANSITIONS.
   Each element of TRANSITIONS is expected to be a list where the fourth element is
   T/NIL indicating if the episode was terminated."
  (mapcar #'cadddr transitions))

(defun truncations (transitions)
  "Return a list of truncation flags from TRANSITIONS.
   Each element of TRANSITIONS is expected to be a list where the fifth element is
   T/NIL indicating if the episode was truncated."
  (mapcar (lambda (x) (car (cddddr x))) transitions))
