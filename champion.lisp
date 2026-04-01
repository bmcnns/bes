(defun select-champion (tpg dataset)
  (car (first (sort (loop for team in (tpg-teams tpg)
                          collect (cons team (eval-team team dataset)))
                    #'< :key #'cdr))))

(defun hundred-rewards (champion)
  (bes-gym:rollout-many champion "LunarLander-v3" (loop repeat 100 
                                                        collect (random 100000))))

(defun mean (seq)
  (/ (reduce #'+ seq) (length seq)))

