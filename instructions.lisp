(in-package :bes)

;;; instructions.lisp
;;; ------------------
;;;
;;; This file defines the available instruction set used by programs in BES.
;;; It provides safe mathematical operations for execution within
;;; evolved programs, guarding against numerical issues such as
;;; overflow, division by zero, and domain errors.

(defparameter *fp-max* 1e10
  "Maximum allowable floating-point value for clamping.")

(defparameter *fp-min* -1e10
  "Minimum allowable floating-point value for clamping.")

(defmacro safe-wrapper (fn)
  "Return a lambda that applies FN to its arguments, then clamps the result.
   Use to wrap arbitrary math functions safely."
  `(lambda (&rest args)
     (clamp (apply ,fn args))))

(defmacro def-safe-operator (name fn arity)
  "Define a new function NAME with ARITY arguments that applies FN and clamps the result.
   Prevents floating-point overflows or invalid math behaviour during program execution."
  (let ((params (loop for i from 1 to arity collect (gensym "ARG"))))
    `(defun ,name ,params
       (clamp (,fn ,@params)))))

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
