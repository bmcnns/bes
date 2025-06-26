(defun parse-symbol (sym)
  (cond
    ((symbolp sym)
     (let ((name (symbol-name sym)))
       (cond
         ((string-prefix-p "R" name)
          `(aref registers ,(1- (parse-integer (subseq name 1)))))
         ((string-prefix-p "OBS" name)
          `(aref observations ,(1- (parse-integer (subseq name 3)))))
         (t sym))))
    (t sym)))

(defun translate-instruction (instr)
  (destructuring-bind (dest op &rest args) instr
      `(setf ,(parse-symbol dest) (,(lookup-fn op) ,@(mapcar #'parse-symbol args)))))

(defun phenotype (genotype)
  (compile nil
    `(lambda (registers observations)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type (simple-array single-float (*)) registers observations))
       ,@(mapcar #'translate-instruction genotype)
      registers)))

(defun evaluate (pheno observations experiment)
  (let* ((num-registers (length (experiment-registers experiment)))
         (registers (zeros num-registers))
         (obs-array (list->vector observations)))
    (funcall pheno registers obs-array)))
