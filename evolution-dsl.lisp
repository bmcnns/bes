(in-package :bes)

(defun evolve (dataset &key (budget 100)
                         (log-file "scores.dat"))
  (let ((tpg (make-tpg)))
    (unwind-protect
         (progn
           (loop for generation from 1 to budget
                 do (progn
                      (setf tpg (tournament-breeder tpg dataset generation))
                      (when (zerop (mod generation 10))
                        (with-output-to-string (s)
                          (room)))))
           tpg))))
