(in-package :bes)

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
