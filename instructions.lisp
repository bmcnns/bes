(defun analytic-quotient (a b)
  (/ a (sqrt (+ 1 (* b b)))))

(defun protected-log (x)
  (if (= x 0.0)
      0.0
      (log x)))

(defun protected-exp (x)
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
