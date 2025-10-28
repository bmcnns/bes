(in-package :bes)

(defun make-execute-on-dataset-fn (dataset)
  (lambda (model) 
    (cond ((tpg-p model) (eval-tpg model (sample dataset)))
          ((linear-gp-p model) (eval-linear-gp model (sample dataset)))
          (t (error "Was expecting a LGP or a TPG. Got ~A instead." model)))))

(defun execute (model dataset)
  (let ((fn (make-execute-on-dataset-fn dataset)))
    (funcall fn model)))

(defun first-objective (score)
  (cdaadr score))

(defun evolve (strategy eval-fn &key
                                  (budget 100)
                                  (mode 'lgp)
                                  (log-file "scores.dat")
                                  (save-file nil)
                                  (initial-model nil))
  (let ((model
          (if initial-model
              initial-model
              (case mode
                (lgp (make-linear-gp))
                (tpg (make-tpg))
                (otherwise (error "Mode is not supported. ~A~%" mode)))))
        (logger (make-aggregate-score-logger log-file)))
    (unwind-protect
         (progn 
           (loop for generation from 1 to budget
                 do (progn (when (zerop (mod generation 10))
                             (clear-cache))
                           (setf model (funcall strategy eval-fn model (lambda (scores)
                                                                         (funcall logger generation scores))))
                           (format t "Generation ~A complete.~%" generation)
                           (when (zerop (mod generation 10))
                             (with-output-to-string (s)
                               (room)))))
           (values model))
      (when save-file
        (save-model save-file model)))))


;; an evaluation function is anything that takes a model and returns
;; scores

