                                        ; Macros for data loading
(defmacro defdataset (name)
  (let* ((dataset-name (string-trim "*" (symbol-name name)))
         (file-name (concatenate 'string "datasets/" dataset-name)))
    `(defparameter ,name
       (with-open-file (in ,file-name :direction :input)
         (read in)))))

(defun batch (dataset start end)
  (subseq dataset start end))

(defun sample (dataset experiment)
  (let* ((batch-size (experiment-batch-size experiment))
         (start (random (- (length dataset) batch-size)))
         (end (+ start batch-size)))
    (batch dataset start end)))
    
(defun observations (transitions)
  (mapcar #'car transitions))

(defun actions (transitions)
  (mapcar #'cadr transitions))

(defun rewards (transitions)
  (mapcar #'caddr transitions))

(defun terminations (transitions)
  (mapcar #'cadddr transitions))

(defun truncations (transitions)
  (mapcar (lambda (x) (car (cddddr x))) transitions))
