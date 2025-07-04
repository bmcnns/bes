                                        ; Fitness metrics

(defmacro minimize (fn)
  "Syntactic sugar: no transformation needed since NSGA-II assumes minimization"
  `(lambda (&rest args)
     (apply ,fn args)))

(defmacro maximize (fn)
  "Negate the objective since NSGA-II assumes minimization"
  `(lambda (&rest args)
     (let ((result (apply ,fn args)))
       (if (listp result)
           (mapcar #'- result)
           (- result)))))
                                        ; Single-objective Optimization

(defun tournament-selection (ranked-population experiment)
  (let* ((tournament-size (experiment-tournament-size experiment))
         (tournament (loop repeat tournament-size
                           collect (random-choice ranked-population))))
    (car (argmax tournament #'cdr))))
(defun single-objective-selection (ranked-population experiment)
  (let ((desired-population-size (experiment-population-size experiment)))
    (loop repeat desired-population-size
          collect (tournament-selection ranked-population experiment))))
