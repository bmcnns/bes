(in-package :bes)

(defparameter *program-id-generator* (make-unique-id-generator "P"))

(defvar *program-cache* (make-hash-table :test 'equal))
(defvar *program-cache-lock* (bt:make-lock "program-cache-lock"))

(defun random-argument ()
  "Return a random argument (either observation or register) based on the observation probability in *EXPERIMENT*."
  (if (< (random 1.0) (experiment-observation-probability *experiment*))
      (random-observation)
      (random-register)))

(defun random-constant ()
  "Return a random constant sampled uniformly from the constant range in *EXPERIMENT*."
  (let* ((constant-range (experiment-constant-range *experiment*)))
    (destructuring-bind (lower-bound upper-bound) constant-range
      (random-range lower-bound upper-bound))))

(defun random-register ()
  "Return a randomly selected register from *EXPERIMENT*."
  (random-choice (experiment-registers *experiment*)))

(defun random-observation ()
  "Return a randomly selected observation variable from *EXPERIMENT*."
  (random-choice (experiment-observations *experiment*)))

(defun random-opcode ()
  "Return a randomly selected opcode from the instruction set in *EXPERIMENT*."
  (random-choice (experiment-instruction-set *experiment*)))

(defun random-instruction ()
  "Return a syntactically valid instruction sampled from the *EXPERIMENT* configuration.
   For binary opcodes, exactly one of the inputs may be a constant, based on the constant-probability parameter.
   For unary opcodes, the input must either be a register or an observation variable."
  (let* ((instruction (random-opcode))
         (arity (lookup-arity instruction))
         (constant-probability (experiment-constant-probability *experiment*)))
    (cond
      ((= arity 1)
       (list (random-register) instruction (random-argument)))
      ((= arity 2)
       (if (< (random 1.0) constant-probability)
           (let ((constant-position (random 2)))
             (if (= 0 constant-position)
                 (list (random-register) instruction (random-constant) (random-argument))
                 (list (random-register) instruction (random-argument) (random-constant))))
           (list (random-register) instruction (random-argument) (random-argument))))
      (t (error "Unexpected arity")))))

(defun make-program ()
  (let* ((min-length (experiment-minimum-program-length *experiment*))
         (max-length (experiment-maximum-program-length *experiment*))
         (instructions
           (loop repeat (random-range min-length max-length)
                 collect (random-instruction))))
    `(PROGRAM ,(funcall *program-id-generator*) ,instructions)))

(defun program-p (form)
  (and
   (listp form)
   (equal (car form) 'PROGRAM)
   (symbolp (cadr form))
   (listp (caddr form))
   (listp (caaddr form))))

(defun program-id (program)
  (unless (program-p program)
    (error "PROGRAM-ID expects a PROGRAM. Got ~A instead.~%" program))
  (cadr program))

(defun program-instructions (program)
  (unless (program-p program)
    (error "PROGRAM-INSTRUCTIONS expects a PROGRAM. Got ~A instead.~%" program))
  (caddr program))

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

(defun compile-program (genotype &key show-all-registers)
  "Compile a GENOTYPE (list of instructions) into an executable phenotype function.
   The resulting function takes OBSERVATIONS and returns the output register(s).

   GENOTYPE: List of instructions of the form (DEST OPCODE ARG1 [ARG2])
   EXPERIMENT: Defines possible registers, observation variables, output registers, etc.
   SHOW-ALL-REGISTERS (optional): If true, returns all registers;
   otherwise, return the output register(s) defined in *EXPERIMENT*.

   Supports both batched and single observation input."
  (let* ((output-registers (experiment-output-registers *experiment*))
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
      (let* ((num-registers (length (experiment-registers *experiment*)))
             (registers (zeros num-registers)))
        (if (and (listp obs) (listp (first obs))) ; batched
            (mapcar (lambda (o)
                      (funcall fn (zeros num-registers) (list->vector o)))
                    obs)
            (funcall fn registers (list->vector obs)))))))

(defun clamp-registers (result &optional (min -1) (max 1))
  "Clamps all values in RESULT to be between MIN and MAX.
RESULT can be a list of registers or a batch of list of registers."
  (if (listp (first result))
      ;; If the first element is a list, assume it's a batch of inputs
      (mapcar (lambda (registers)
                (mapcar (lambda (value)
                          (clamp value min max))
                        registers))
              result)
      ;; Otherwise, it's a single input
      (mapcar (lambda (value)
                (clamp value min max))
              result)))


(defun get-cached-program (program-id)
  (gethash program-id *program-cache*))

(defun set-cached-program (program-id compiled-program)
  (bt:with-lock-held (*program-cache-lock*)
    (setf (gethash program-id *program-cache*) compiled-program)))
    

(defun execute-program (program observations &key show-all-registers)
  "Convenience function to evaluate a GENOTYPE on OBSERVATIONS.
   Internally compiles the genotype into a function using CONVERT-TO-PHENOTYPE and
   immediately calls it.

   OBSERVATIONS may be:
   - A single input (list)
   - A batch of inputs (list of lists)

   SHOW-ALL-REGISTERS (optional): If true, returns all registers;
   otherwise, return the output register(s) defined in EXPERIMENT."
  (unless (program-p program)
    (error "Tried to execute a program but the thing you're trying to execute~%is not a program. ~A" program))
  (let ((compiled-program (or (get-cached-program (program-id program))
                              (set-cached-program (program-id program)
                                    (compile-program (program-instructions program) :show-all-registers show-all-registers)))))
    (clamp-registers (funcall compiled-program observations))))

(defun eval-program (program dataset)
  (let* ((predictions (execute-program program (observations dataset))))
    (fitness program (actions dataset) predictions)))

