(defun tournament-selection (ranked-population experiment)
  (let* ((tournament-size (experiment-tournament-size experiment))
         (tournament (loop repeat tournament-size
                           collect (random-choice ranked-population))))
    (cons tournament (argmax tournament #'cdr))))
  
