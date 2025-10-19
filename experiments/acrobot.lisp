(in-package :bes)

(defdataset *Acrobot-Expert-v1*
  :path "~/.datasets/Acrobot-Expert-v1")

(defdataset *Minimal-Acrobot-Expert-v1*
  :path "~/.datasets/Minimal-Acrobot-Expert-v1")

(defexperiment *Acrobot-v1*
  :batch-size 200
  :instruction-set (ADD SUB MUL DIV SIN COS LOG EXP)
  :registers (R from 1 to 12) 
  :observations (OBS from 1 to 6)
  :output-registers (R from 1 to 3)
  :constant-range '(-10.0 10.0)
  :objectives (accuracy)
  :tournament-size 4
  :num-threads 1
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
  :actions '(0 1 2)
  ;; tpg parameters
  :initial-minimum-number-of-learners 2
  :initial-maximum-number-of-learners 5
  :minimum-number-of-learners 22
  :maximum-number-of-learners 10
  ;; todo -- replace ryan's constants with stephen's
  ;;; team mutation probabilities
  :mutate-learner-probability 0.3
  :add-learner-probability 0.7
  :remove-learner-probability 0.7
  ;;; learner mutation probabilities
  :mutate-learner-program-vs-action-probability 0.66
  :learner-atomic-action-probability 0.5
  :mutate-team-probability 1.0)

(defvar *experiment* nil)
(setf *experiment* *Acrobot-v1*)
