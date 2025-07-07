(in-package :bes)

(defun random-argument (experiment)
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

(defun new-instruction (experiment)
  "Return a syntactically valid instruction sampled from the EXPERIMENT configuration.
   For binary opcodes, exactly one of the inputs may be a constant, based on the constant-probability parameter.
   For unary opcodes, the input must either be a register or an observation variable."
  (let* ((instruction (random-opcode experiment))
         (arity (lookup-arity instruction))
         (constant-probability (experiment-constant-probability experiment)))
    (cond
      ((= arity 1)
       (list (random-register experiment) instruction (random-argument experiment)))
      ((= arity 2)
       (if (< (random 1.0) constant-probability)
           (let ((constant-position (random 2)))
             (if (= 0 constant-position)
                 (list (random-register experiment) instruction (random-constant experiment) (random-argument experiment))
                 (list (random-register experiment) instruction (random-argument experiment) (random-constant experiment))))
           (list (random-register experiment) instruction (random-argument experiment) (random-argument experiment))))
      (t (error "Unexpected arity")))))

(defun new-genotype (experiment)
  "Generate a new genotype as a list of instructions,
   with program length uniformly sampled between the min and max lengths in EXPERIMENT."
  (let ((min-length (experiment-minimum-program-length experiment))
        (max-length (experiment-maximum-program-length experiment)))
    (loop repeat (random-range min-length max-length)
          collect (new-instruction experiment))))
