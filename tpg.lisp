(in-package :bes)

(defstruct tpg
  (root-teams (loop repeat (experiment-population-size *experiment*)
               collect (make-team))))

(defun eval-tpg (tpg dataset)
  (let* ((num-threads (experiment-num-threads *experiment*))
         (root-teams (tpg-root-teams tpg)))
    (multi-thread root-teams team num-threads
      (eval-team team dataset))))

(defun get-team (team-id tpg)
  (find team-id (tpg-root-teams tpg) :key #'team-id))

(defun get-all-reachable-teams (tpg)
  (tpg-root-teams tpg))
