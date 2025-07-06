(in-package :bes)

(defun analytic-quotient (a b)
  "Return the quotient A / sqrt(1 + B²).
   Used as a numerically stable alternative to division that avoids division by zero."
  (/ a (sqrt (+ 1 (* b b)))))

(defun protected-log (x)
  "Return log(X), but safely return 0.0 if X is zero."
  (if (= x 0.0)
      0.0
      (log x)))

(defun protected-exp (x)
  "Return exp(X), clamped to avoid overflow.
   X is clamped between -100 and 55 before exponentiation."
  (exp (clamp x -100 55)))
      
(def-safe-operator safe-add + 2)
(def-safe-operator safe-mul * 2)
(def-safe-operator safe-div analytic-quotient 2)
(def-safe-operator safe-sub - 2)
(def-safe-operator safe-sin sin 1)
(def-safe-operator safe-cos cos 1)
(def-safe-operator safe-log protected-log 1)
(def-safe-operator safe-exp protected-exp 1)

(defparameter *instruction-library*
  '((ADD . (:arity 2 :fn safe-add))
    (MUL . (:arity 2 :fn safe-mul))
    (DIV . (:arity 2 :fn safe-div))
    (SUB . (:arity 2 :fn safe-sub))
    (SIN . (:arity 1 :fn safe-sin))
    (COS . (:arity 1 :fn safe-cos))
    (LOG . (:arity 1 :fn safe-log))
    (EXP . (:arity 1 :fn safe-exp))))

(defun lookup-instruction (name)
  "Look up instruction metadata by name in *INSTRUCTION-LIBRARY*.
   Returns an assoc pair of the form (NAME . PROPS), or signals
   an error if not found."
  (or (assoc name *instruction-library*)
      (error "Unknown instruction ~A" name)))

(defun lookup-arity (instruction)
  "Return the arity (number of arguments) of INSTRUCTION."
  (getf (cdr (lookup-instruction instruction)) :arity))

(defun lookup-fn (instruction)
  "Return the function associated with INSTRUCTION."
  (getf (cdr (lookup-instruction instruction)) :fn))

(defun make-instruction-set (&rest opcodes)
  "Return a list of instruction names for the given OPCODES."
  (loop for opcode in opcodes
        collect (car (lookup-instruction opcode))))

(defun instructions-with-arity (n)
  "Return a list of opcodes that have arity N."
  (loop for (name . props) in *instruction-library*
        when (= (getf props :arity) n)
        collect name))
