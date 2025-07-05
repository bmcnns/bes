(in-package :bes)

(defun random-input (experiment)
  "Return a random argument (either observation or register) based on the observation probability in EXPERIMENT."
  (if (< (random 1.0) (experiment-observation-probability experiment))
      (random-observation experiment)
      (random-register experiment)))

(defun random-constant (experiment)
  "Return a random constant sampled uniformly from the constant range in EXPERIMENT."
  (let* ((constant-range (experiment-constant-range experiment)))
    (destructuring-bind (lower-bound upper-bound) constant-range
      (random-range lower-bound upper-bound))))

(defun random-register (experiment)
  "Return a randomly selected register from EXPERIMENT."
  (random-choice (experiment-registers experiment)))

(defun random-observation (experiment)
  "Return a randomly selected observation variable from EXPERIMENT."
  (random-choice (experiment-observations experiment)))

(defun random-opcode (experiment)
  "Return a randomly selected opcode from the instruction set in EXPERIMENT."
  (random-choice (experiment-instruction-set experiment)))

(defun random-instruction (experiment)
  "Return a syntactically valid instruction sampled from the EXPERIMENT configuration.
   For binary operators, exactly one of the inputs may be a constant, based on the constant-probability parameter.
   For unary operators, the input must either be a register or an observation variable."
  (let* ((instruction (random-opcode experiment))
         (arity (lookup-arity instruction))
         (constant-probability (experiment-constant-probability experiment)))
    (cond
      ((= arity 1)
       (list (random-register experiment) instruction (random-input experiment)))
      ((= arity 2)
       (if (< (random 1.0) constant-probability)
           (let ((constant-position (random 2)))
             (if (= 0 constant-position)
                 (list (random-register experiment) instruction (random-constant experiment) (random-input experiment))
                 (list (random-register experiment) instruction (random-input experiment) (random-constant experiment))))
           (list (random-register experiment) instruction (random-input experiment) (random-input experiment))))
      (t (error "Unexpected arity")))))

(defun new-individual (experiment)
  "Generate a new genotype as a list of instructions,
   with program length uniformly sampled between the min and max lengths in EXPERIMENT."
  (let ((min-length (experiment-minimum-program-length experiment))
        (max-length (experiment-maximum-program-length experiment)))
    (loop repeat (random-range min-length max-length)
          collect (random-instruction experiment))))

(defun get-dest (instruction)
  "Return the destination register from INSTRUCTION"
  (first instruction))

(defun get-operator (instruction)
  "Return the opcode used in INSTRUCTION"
  (second instruction))

(defun get-register (instruction &optional i)
  "Return a register argument from INSTRUCTION
   If I is provided, return the Ith argument. Otherwise return a random argument."
  (destructuring-bind (dest op &rest args) instruction
    (if i
        (nth i args)
        (random-choice args))))

(defun has-constant (instruction)
  "Return T if INSTRUCTION contains a constant; otherwise return NIL."
  (destructuring-bind (dest op &rest args) instruction
    (some #'numberp args)))

(defun get-constant (instruction)
  "Return the constant argument from INSTRUCTION, or NIL if none is present."
  (destructuring-bind (dest op &rest args) instruction
    (find-if #'numberp args)))

(defun get-instruction-with-constant (genotype)
  "Return a randomly selected instruction from GENOTYPE that contains a constant.
   Return NIL if no such instruction exists."
  (let ((instructions (remove-if-not #'has-constant genotype)))
    (if instructions
        (random-choice instructions))))
