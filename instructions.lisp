(defun analytic-quotient (a b)
  (/ a (sqrt (+ 1 (* b b)))))

(defparameter *instruction-library*
  '((ADD . (:arity 2 :fn +))
    (MUL . (:arity 2 :fn *))
    (DIV . (:arity 2 :fn analytic-quotient))
    (SUB . (:arity 2 :fn -))
    (SIN . (:arity 1 :fn sin))
    (COS . (:arity 1 :fn cos))
    (LOG . (:arity 1 :fn log))
    (EXP . (:arity 1 :fn exp))))

(defun lookup-instruction (name)
  (or (assoc name *instruction-library*)
      (error "Unknown instruction ~A" name)))

(defun lookup-arity (name)
    (getf (cdr (lookup-instruction name)) :arity))

(defun lookup-fn (name)
    (getf (cdr (lookup-instruction name)) :fn))

(defun make-instruction-set (&rest names)
  (loop for name in names
        collect (car (lookup-instruction name))))

(defun instructions-with-arity (n)
  (loop for (name . props) in *instruction-library*
        when (= (getf props :arity) n)
        collect name))
