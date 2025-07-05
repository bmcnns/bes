(in-package :bes)

(defun random-input (experiment)
  (if (< (random 1.0) (experiment-observation-probability experiment))
      (random-observation experiment)
      (random-register experiment)))

(defun random-constant (experiment)
  (let* ((constant-range (experiment-constant-range experiment)))
    (destructuring-bind (lower-bound upper-bound) constant-range
      (random-range lower-bound upper-bound))))
      
(defun random-register (experiment)
  (random-choice (experiment-registers experiment)))

(defun random-observation (experiment)
  (random-choice (experiment-observations experiment)))

(defun random-opcode (experiment)
  (random-choice (experiment-instruction-set experiment)))

(defun random-instruction (experiment)
  (let* ((instruction (random-opcode experiment))
         (arity (lookup-arity instruction))
         (constant-probability (experiment-constant-probability experiment)))
    (cond
      ((= arity 1)
       (list (random-register experiment) instruction (random-input experiment)))
      ((= arity 2)
                                        ; We need to enforce that only one of two inputs can be a constant
       (if (< (random 1.0) constant-probability)
           (let ((constant-position (random 2)))
             (if (= 0 constant-position)
                 (list (random-register experiment) instruction (random-constant experiment) (random-input experiment))
                 (list (random-register experiment) instruction (random-input experiment) (random-constant experiment))))
           (list (random-register experiment) instruction (random-input experiment) (random-input experiment))))
       (t (error "Unexpected arity")))))

(defun new-individual (experiment)
  (let ((min-length (experiment-minimum-program-length experiment))
        (max-length (experiment-maximum-program-length experiment)))
    (loop repeat (random-range min-length max-length)
          collect (random-instruction experiment))))

(defun get-dest (instruction)
  (first instruction))

(defun get-operator (instruction)
  (second instruction))

(defun get-register (instruction &optional i)
  (destructuring-bind (dest op &rest args) instruction
    (if i
        (nth i args)
        (random-choice args))))

(defun has-constant (instruction)
  (destructuring-bind (dest op &rest args) instruction
    (some #'numberp args)))

(defun get-constant (instruction)
  (destructuring-bind (dest op &rest args) instruction
    (find-if #'numberp args)))

(defun get-instruction-with-constant (genotype)
  (let ((instructions (remove-if-not #'has-constant genotype)))
    (if instructions
        (random-choice instructions))))
