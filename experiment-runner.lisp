;; cartpole-v1, mountaincar-v0, acrobot-v1, lunarlander-v3
;; pre-train on minimal treatments, complete treatments

(in-package :bes)

(defparameter *configurations*
  `(("lunar-lander" ,*minimal-lunarlander-expert-v3* ,*lunarlander-expert-v3* ,*lunarlander-v3* "LunarLander-v3")))
    ;; ("mountain-car" ,*minimal-mountaincar-expert-v0* ,*mountaincar-expert-v0* ,*mountaincar-v0* "MountainCar-v0")))
    ;; ("cartpole" ,*minimal-cartpole-expert-v1* ,*cartpole-expert-v1* ,*cartpole-v1* "CartPole-v1")))
    ;("acrobot" ,*minimal-acrobot-expert-v1* ,*acrobot-expert-v1* ,*acrobot-v1* "Acrobot-v1")))

(defvar *eval-fn* nil)

(defun run-experiments ()
  (loop for (experiment-name minimal-dataset complete-dataset experiment env-name) in *configurations*
        do (progn
             (setf *eval-fn* (make-execute-on-dataset-fn minimal-dataset))
             (setf *experiment* experiment)
             (evolve #'breeder *eval-fn*
                     :budget 1000
                     :mode 'tpg
                     :log-file (format nil "/Users/brycemacinnis/experiments/~A/minimal-scores.dat"
                                       experiment-name)
                     :save-file (format nil "/Users/brycemacinnis/experiments/~A/minimal-~A.tpg"
                                        experiment-name experiment-name))
             (setf *eval-fn* (make-execute-on-dataset-fn complete-dataset))
             (evolve #'breeder *eval-fn*
                     :budget 1000
                     :mode 'tpg
                     :log-file (format nil "/Users/brycemacinnis/experiments/~A/complete-scores.dat"
                                       experiment-name)
                     :save-file (format nil "/Users/brycemacinnis/experiments/~A/complete-~A.tpg"
                                        experiment-name experiment-name)))))

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
