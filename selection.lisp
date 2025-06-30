(defun tournament-selection (ranked-population experiment)
  (let* ((tournament-size (experiment-tournament-size experiment))
         (tournament (loop repeat tournament-size
                           collect (random-choice ranked-population))))
    (car (argmax tournament #'cdr))))
  
(defun select (ranked-population experiment)
  (let ((desired-population-size (experiment-population-size experiment)))
    (loop repeat desired-population-size
          collect (tournament-selection ranked-population experiment))))
