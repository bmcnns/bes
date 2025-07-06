(in-package :bes)

(defun parse-symbol (sym)
  "Convert SYM to its corresponding register or observation variable.
   - If SYM starts with 'R', returns (aref registers INDEX)
   - If SYM starts with 'OBS', returns (aref observations INDEX)
   - Otherwise, returns SYM unchanged (e.g., numeric constants)."
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
  "Convert a single linear GP instruction to a Common Lisp form that
   performs the corresponding operation. Translates destination, operator and arguments
   via `parse-symbol` and `lookup-fn`."
  (destructuring-bind (dest op &rest args) instr
      `(setf ,(parse-symbol dest) (,(lookup-fn op) ,@(mapcar #'parse-symbol args)))))

(defun convert-to-phenotype (genotype experiment &key show-all-registers)
  "Compile a GENOTYPE (list of instructions) into an executable phenotype function.
   The resulting function takes OBSERVATIONS and returns the output register(s).

   GENOTYPE: List of instructions of the form (DEST OPCODE ARG1 [ARG2])
   EXPERIMENT: Defines possible registers, observation variables, output registers, etc.
   SHOW-ALL-REGISTERS (optional): If true, returns all registers;
   otherwise, return the output register(s) defined in EXPERIMENT.

   Supports both batched and single observation input."
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
  "Convenience function to evaluate a GENOTYPE on OBSERVATIONS using EXPERIMENT.
   Internally compiles the genotype into a function using CONVERT-TO-PHENOTYPE and
   immediately calls it.

   OBSERVATIONS may be:
   - A single input (list)
   - A batch of inputs (list of lists)

   SHOW-ALL-REGISTERS (optional): If true, returns all registers;
   otherwise, return the output register(s) defined in EXPERIMENT."
  (funcall (convert-to-phenotype genotype experiment :show-all-registers show-all-registers) observations))
