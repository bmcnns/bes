(in-package :bes)

;;; Single-objective Optimization

(defun tournament-selection (fitness-scores)
  "Select an individual from RANKED-POPULATION using tournament selection.
   The tournament size is specified by *EXPERIMENT*."
   ; with replacement
  (let* ((tournament-size (experiment-tournament-size *experiment*))
         (tournament (loop repeat tournament-size
                           collect (random-choice fitness-scores)))
         (objective-scores (mapcar #'cdaadr tournament))
         (ids (mapcar #'car tournament))
         (winner-idx (argmin objective-scores))
         (winner (elt ids winner-idx)))
    winner))

