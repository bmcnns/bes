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
      `(setf ,dest (,(lookup-fn op) ,@args))))

(defun phenotype (genotype)
  `(lambda (registers observations)
     ,@(mapcar #'translate-instruction genotype)
     registers))
