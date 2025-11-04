(defparameter *agents* (loop for subdir in (uiop:subdirectories "~/experiments/acrobot/")
                             collect (load-tpg (format nil "~Aagent.tpg" subdir))))

(defparameter *experiment* *acrobot-v1*)
(defparameter *rollout-fn* (bes-gym:make-rollout-fn "Acrobot-v1"))

(defparameter *champions*
  (loop for agent in *agents*
        for i from 0
        do (progn
             (clear-cache)
             (format t "Selecting champion ~A~%" i))
        collect (select-champion agent *rollout-fn*)))

(defparameter *lean-mean-champions*
  (loop for champion in *champions*
        for agent in *agents*
        do (clear-cache)
        collect (postprocess agent champion "Acrobot-v1")))

(loop for champion in *lean-mean-champions*
      for agent in *agents*
      for champion-id in *champions* 
      do (clear-cache)
      do (format t "~A: ~A~%" champion-id (team-complexity champion champion-id)))

(loop for champion in *lean-mean-champions*
      for agent in *agents*
      for champion-id in *champions*
      for i from 1
      do (progn
           (clear-cache)
           (format t "~A: ~A~%" champion-id (mean-reward (bes-gym:rollout-many champion champion-id "Acrobot-v1" *seeds* :evaluations-file "~/experiments/acrobot/evaluations.dat" :trial i)))))
