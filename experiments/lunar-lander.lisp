(ql:quickload :bes)
(in-package :bes)

(setf *random-state* (make-random-state t))

(defdataset *Balanced-Moderate-LunarLander-v3*
  :path "~/.datasets/Balanced-Moderate-LunarLander-v3")

(defvar *dataset* *balanced-moderate-lunarlander-v3*)
(setf *dataset* *balanced-moderate-lunarlander-v3*)

(defexperiment *LunarLander-v3*
  :batch-size 952
  :instruction-set (ADD SUB MUL)
  :registers (R from 1 to 8) 
  :observations (OBS from 1 to 8)
  :output-registers (R from 1 to 3)
  :constant-range '(-10.0d0 10.0d0)
  :objectives (accuracy)
  :tournament-size 4
  :num-threads 8
  :population-size 3600
  :generations 10
  :minimum-program-length 3
  :maximum-program-length 24
  :observation-probability 0.5
  :constant-probability 0.5
  :mutate-instruction-probability 0.2
  :mutate-register-probability 0.5
  :mutate-operation-probability 0.25
  :mutate-constant-probability 0.25
  :add-instruction-probability 1.0
  :remove-instruction-probability 1.0
  :swap-instruction-probability 1.0
  :tune-constants-probability 0.3
  :constant-mutation-std 1.0
  :maximum-instruction-count 24
  :actions '(0 1 2 3)
  :initial-minimum-number-of-learners 2
  :initial-maximum-number-of-learners 20
  :minimum-number-of-learners 2
  :maximum-number-of-learners 30
  :mutate-learner-probability 0.3
  :add-learner-probability 0.3
  :remove-learner-probability 0.3
  :mutate-learner-program-probability 0.25
  :mutate-learner-action-probability 0.25
  :learner-atomic-action-probability 1.0
  :mutate-team-probability 1.0)

(defparameter *experiment* *LunarLander-v3*)
(defparameter *results-folder* (format nil "/Users/brycemacinnis/experiments/lunar-lander/~A/" (substitute #\- #\: (timestamp))))

(ensure-directories-exist *results-folder*) 

(evolve #'breeder *eval-fn*
        :mode 'tpg
        :budget 150
        :log-file (concatenate 'string *results-folder* "scores.dat")
        :save-file (concatenate 'string *results-folder* "agent.tpg"))

