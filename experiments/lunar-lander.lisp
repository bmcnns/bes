(ql:quickload :bes)
(in-package :bes)

(setf *random-state* (make-random-state t))

(defdataset *LunarLander-Expert-v3*
  :path "~/.datasets/LunarLander-Expert-v3")

(setf *dataset* (batch *LunarLander-Expert-v3* 0 25000))

(defexperiment *LunarLander-v3*
  :batch-size 1000
  :instruction-set (ADD SUB MUL DIV SIN COS LOG EXP)
  :registers (R from 1 to 12) 
  :observations (OBS from 1 to 8)
  :output-registers (R from 1 to 3)
  :constant-range '(-10.0 10.0)
  :objectives (accuracy)
  :tournament-size 4
  :num-threads 8
  :population-size 1000
  :generations 10
  :minimum-program-length 1
  :maximum-program-length 100
  :observation-probability 0.5
  :constant-probability 0.5
  :mutate-instruction-probability 1.0
  :mutate-register-probability 0.5
  :mutate-operation-probability 0.25
  :mutate-constant-probability 0.25
  :add-instruction-probability 1.0
  :remove-instruction-probability 1.0
  :swap-instruction-probability 1.0
  :constant-mutation-std 1.0
  :maximum-instruction-count 256
  :actions '(0 1 2 3)
  :initial-minimum-number-of-learners 2
  :initial-maximum-number-of-learners 5
  :minimum-number-of-learners 2
  :maximum-number-of-learners 10
  :mutate-learner-probability 0.3
  :add-learner-probability 0.7
  :remove-learner-probability 0.7
  :mutate-learner-program-vs-action-probability 0.66
  :learner-atomic-action-probability 0.5
  :mutate-team-probability 1.0)

(defparameter *experiment* *LunarLander-v3*)
(defparameter *eval-fn* (make-execute-on-dataset-fn *dataset*))
(defparameter *results-folder* (format nil "/Users/brycemacinnis/experiments/lunar-lander/~A/" (substitute #\- #\: (timestamp))))

(ensure-directories-exist *results-folder*) 

(evolve #'breeder *eval-fn*
        :mode 'tpg
        :budget 150
        :log-file (concatenate 'string *results-folder* "scores.dat")
        :save-file (concatenate 'string *results-folder* "agent.tpg"))

