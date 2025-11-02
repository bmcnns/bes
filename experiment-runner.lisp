;; cartpole-v1, mountaincar-v0, acrobot-v1, lunarlander-v3
;; pre-train on minimal treatments, complete treatments

(in-package :bes)

(defparameter *configurations*
  `(
    ("acrobot" ,*minimal-acrobot-expert-v1* ,*acrobot-expert-v1* ,*acrobot-v1* "Acrobot-v1")
    ("lunar-lander" ,*minimal-lunarlander-expert-v3* ,*lunarlander-expert-v3* ,*lunarlander-v3* "LunarLander-v3")
    ("mountain-car" ,*minimal-mountaincar-expert-v0* ,*mountaincar-expert-v0* ,*mountaincar-v0* "MountainCar-v0")
    ("cartpole" ,*minimal-cartpole-expert-v1* ,*cartpole-expert-v1* ,*cartpole-v1* "CartPole-v1")))

(defvar *eval-fn* nil)

(defun run-experiments (trials)
  (loop for i from 2 to trials
        do (loop for (experiment-name minimal-dataset complete-dataset experiment env-name) in *configurations*
                 do (progn
                      (format t "~A [~A/~A]~%" experiment-name i trials)
                      (setf *eval-fn* (make-execute-on-dataset-fn minimal-dataset))
                      (setf *experiment* experiment)
                      (evolve #'breeder *eval-fn*
                              :budget 1000
                              :mode 'tpg
                              :log-file (format nil "/Users/brycemacinnis/new-experiments/~A/minimal-scores-~A.dat"
                                                experiment-name i)
                              :save-file (format nil "/Users/brycemacinnis/new-experiments/~A/minimal-~A-~A.tpg"
                                                 experiment-name experiment-name i))
                      (setf *eval-fn* (make-execute-on-dataset-fn complete-dataset))
                      (evolve #'breeder *eval-fn*
                              :budget 1000
                              :mode 'tpg
                              :log-file (format nil "/Users/brycemacinnis/new-experiments/~A/complete-scores-~A.dat"
                                                experiment-name i)
                              :save-file (format nil "/Users/brycemacinnis/new-experiments/~A/complete-~A-~A.tpg"
                                                 experiment-name experiment-name i))))))

(defun run-online-experiments ()
  (loop for (experiment-name minimal-dataset complete-dataset experiment env-name) in *configurations*
        do (progn
             (format t "Starting ~A~%" experiment-name)
             (setf *eval-fn* (bes-gym:make-rollout-fn env-name))
             (setf *experiment* experiment)
             (evolve #'breeder *eval-fn*
                     :budget 1000
                     :mode 'tpg
                     :log-file (format nil "/Users/brycemacinnis/experiments/~A/online-scores.dat"
                                       experiment-name)
                     :save-file (format nil "/Users/brycemacinnis/experiments/~A/online-~A.tpg"
                                        experiment-name experiment-name)))))


(defun gather-tradeoffs (agent dataset environment-name file-name)
  (let* ((seeds '(3262143 74793174 23538169 39567349 78629257 6506179 90149411 98943448 96369547
                  9533583 69573632 20700837 63175032 84231255 3191636 23824646 30217768 3088549
                  58432542 24430835 82775897 48613750 70663292 38406420 43934389 88282019
                  46197707 41829623 76649282 57229310 71059706 33862166 9799450 60682591
                  62108716 99009683 22257719 80937210 89878971 44974186 25649761 75046821
                  81639931 85176762 49435514 70143129 2142748 16537871 92726950 65815791
                  70701032 7588129 40325155 6398057 28447043 2029873 3073110 19035986 16825016
                  45628376 75460713 35030680 52645157 1071630 83862917 37387358 35576461
                  97073240 89462303 62468447 83344440 74027324 15365946 80477258 86518343
                  12015422 5564004 77866831 5298189 63293370 48553881 2025773 21281141 15270977
                  53825291 80307489 70217896 55693881 33565108 29767531 87021583 70809576
                  14447331 76002520 47281185 6842761 89792210 65663655 95744809 60263103))
         (team-table (build-team-table agent))
         (learner-table (build-learner-table agent))
         (root-teams (root-teams agent :team-table team-table))
         (rewards (make-hash-table :test 'equal))
         (complexities (make-hash-table :test 'equal))
         (accuracies (make-hash-table :test 'equal)))
    (clear-cache)
    (format t "~A: ~A" environment-name (length root-teams))
    (loop for individual in root-teams
          for individual-id = (team-id individual)
          for i from 1
          do (progn
               (format t "~A," i)
               (finish-output)
               (setf (gethash individual-id complexities)
                   (team-complexity agent individual-id
                                    :learner-table learner-table
                                    :team-table team-table))
               (setf (gethash individual-id rewards)
                     (mean (mapcar #'cdaadr (loop for seed in seeds
                                                  collect
                                                  (bes-gym:rollout agent individual-id environment-name seed)))))
               (setf (gethash individual-id accuracies)
                     (cdaadr (eval-team (find-team-by-id agent
                                                         individual-id
                                                         :team-table team-table)
                                        agent
                                        dataset
                                        :learner-table learner-table
                                        :team-table team-table)))))
    (with-open-file (stream file-name
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "ID COMPLEXITY AVG_REWARD ACCURACY~%")
      (loop for individual in root-teams
            for individual-id = (team-id individual)
            do (let ((complexity (gethash individual-id complexities))
                     (reward (gethash individual-id rewards))
                     (accuracy (gethash individual-id accuracies)))
                 (format stream "~A ~A ~A ~A~%" individual-id complexity reward accuracy))))))

             
(defun run-tradeoff-collection ()
  (let ((minimal-mountain-car (load-model "~/experiments/mountain-car/minimal-mountain-car.tpg"))
        (complete-mountain-car (load-model "~/experiments/mountain-car/complete-mountain-car.tpg"))
        (online-mountain-car (load-model "~/experiments/mountain-car/online-mountain-car.tpg"))
;
        (minimal-acrobot (load-model "~/experiments/acrobot/minimal-acrobot-agent.tpg"))
        (complete-acrobot (load-model "~/experiments/acrobot/complete-acrobot-agent.tpg"))
        (online-acrobot (load-model "~/experiments/acrobot/online-acrobot.tpg"))
;
        (minimal-cartpole (load-model "~/experiments/cartpole/minimal-cartpole-agent.tpg"))
        (complete-cartpole (load-model "~/experiments/cartpole/complete-cartpole.tpg"))
        (online-cartpole (load-model "~/experiments/cartpole/online-cartpole.tpg"))
;
        (minimal-lunar-lander (load-model "~/experiments/lunar-lander/minimal-lunar-lander.tpg"))
        (complete-lunar-lander (load-model "~/experiments/lunar-lander/complete-lunar-lander.tpg"))
        (online-lunar-lander (load-model "~/experiments/lunar-lander/online-lunar-lander.tpg")))
    
    (gather-tradeoffs minimal-mountain-car *minimal-mountaincar-expert-v0*
                      "MountainCar-v0" "~/experiments/mountain-car/minimal-tradeoffs.dat")
    (gather-tradeoffs complete-mountain-car (batch *mountaincar-expert-v0* 0 1000)
                      "MountainCar-v0" "~/experiments/mountain-car/complete-tradeoffs.dat")
    (gather-tradeoffs online-mountain-car *minimal-mountaincar-expert-v0*
                      "MountainCar-v0" "~/experiments/mountain-car/online-tradeoffs.dat")

    (gather-tradeoffs minimal-acrobot *minimal-acrobot-expert-v1*
                      "Acrobot-v1" "~/experiments/acrobot/minimal-tradeoffs.dat")
    (gather-tradeoffs complete-acrobot (batch *acrobot-expert-v1* 0 1000)
                      "Acrobot-v1" "~/experiments/acrobot/complete-tradeoffs.dat")
    (gather-tradeoffs online-acrobot *minimal-acrobot-expert-v1*
                      "Acrobot-v1" "~/experiments/acrobot/online-tradeoffs.dat")

    (gather-tradeoffs minimal-cartpole *minimal-cartpole-expert-v1*
                      "CartPole-v1" "~/experiments/cartpole/minimal-tradeoffs.dat")
    (gather-tradeoffs complete-cartpole (batch *cartpole-expert-v1* 0 1000)
                      "CartPole-v1" "~/experiments/cartpole/complete-tradeoffs.dat")
    (gather-tradeoffs online-cartpole *minimal-cartpole-expert-v1*
                      "CartPole-v1" "~/experiments/cartpole/online-tradeoffs.dat")

    (gather-tradeoffs minimal-lunar-lander *minimal-lunarlander-expert-v3*
                      "LunarLander-v3" "~/experiments/lunar-lander/minimal-tradeoffs.dat")
    (gather-tradeoffs complete-lunar-lander (batch *lunarlander-expert-v3* 0 1000)
                      "LunarLander-v3" "~/experiments/lunar-lander/complete-tradeoffs.dat")
    (gather-tradeoffs online-lunar-lander *minimal-lunarlander-expert-v3*
                      "LunarLander-v3" "~/experiments/lunar-lander/online-tradeoffs.dat")))

    
    

    
         

    
