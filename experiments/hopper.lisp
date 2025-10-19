(in-package :bes)

(defdataset *Minimal-Hopper-Expert-v5*
  :path "~/.datasets/Minimal-Hopper-Expert-v5")

(defexperiment *Hopper-v5*
  :batch-size 1000
  :instruction-set (ADD SUB MUL DIV SIN COS LOG EXP)
  :registers (R from 1 to 11) 
  :observations (OBS from 1 to 11)
  :output-registers (R from 1 to 3)
  :constant-range '(-10.0 10.0)
  :objectives (mean-squared-error)
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
  :actions '((-0.996943 0.9911264 1.0)
             (-0.25040507 -0.9932743 1.0)
             (-0.342475 -0.99995047 0.999132)
             (-0.23646641 -0.99991244 0.99670434)
             (0.91290236 0.926286 0.9992367))
  ;; tpg parameters
  :initial-minimum-number-of-learners 2
  :initial-maximum-number-of-learners 5
  :minimum-number-of-learners 2
  :maximum-number-of-learners 5
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
(setf *experiment* *Hopper-v5*)

