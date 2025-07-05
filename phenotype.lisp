(in-package :bes)

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

(defun convert-to-phenotype (genotype experiment &key show-all-registers)
  (let* ((output-registers (experiment-output-registers experiment))
         (return-expr (if show-all-registers
                        `(coerce registers 'list)
                        `(list ,@(mapcar #'parse-symbol output-registers))))
         (fn
           (compile nil
                    `(lambda (registers observations)
                       (declare (optimize (speed 3) (safety 0) (debug 0))
                                (type (simple-array single-float (*)) registers observations)
                                (ignorable observations))
                       ,@(mapcar #'translate-instruction genotype)
                       ,return-expr))))
    (lambda (obs)
      (let* ((num-registers (length (experiment-registers experiment)))
             (registers (zeros num-registers)))
        (if (and (listp obs) (listp (first obs))) ; batched
            (mapcar (lambda (o)
                      (funcall fn (zeros num-registers) (list->vector o)))
                    obs)
            (funcall fn registers (list->vector obs)))))))

(defun phenotype (genotype experiment observations &key show-all-registers)
  (funcall (convert-to-phenotype genotype experiment :show-all-registers show-all-registers) observations))
