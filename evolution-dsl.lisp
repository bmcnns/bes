(in-package :bes)

(defun execute (model dataset)
  (cond ((tpg-p model) (eval-tpg model dataset))
        ((linear-gp-p model) (eval-linear-gp model dataset))
        (t (error "Was expecting a LGP or a TPG. Got ~A instead." model))))

(defun first-objective (score)
  (cdaadr score))

(defun evolve (strategy dataset &key (budget 100) (mode 'lgp))
  (let ((model (case mode
                 (lgp (make-linear-gp))
                 (tpg (make-tpg))
                 (otherwise (error "Mode is not supported. ~A~%" mode))))
        (logger (make-aggregate-score-logger "~/experiments/acrobot/scores.dat")))
    (loop for generation from 1 to budget
          do (progn (clear-cache)
                    (setf model (funcall strategy model (sample dataset)  (lambda (scores)
                                                                            (funcall logger generation scores))))
                    (format t "Generation ~A complete.~%" generation)
                    (when (zerop (mod generation 10))
                      (with-output-to-string (s)
                        (room)))))
             (values model)))

