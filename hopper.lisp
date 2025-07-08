(in-package :bes)

;;; hopper.lisp
;;; ----------
;;; This file defines the dataset and hyperparameter configuration for
;;; the Minimal-Hopper-Expert-v5 offline reinforcement learning benchmark.
;;;
;;; It loads the pre-saved dataset and uses 'defexperiment' to define a
;;; reusable EXPERIMENT struct containing hyperparameters for symbolic regression
;;; via linear genetic programming.
;;;
;;; The experiment targets two objectives: minimizing prediction error (MSE)
;;; and minimizing program complexity.

(defdataset *Minimal-Hopper-Expert-v5*)

(defexperiment *Hopper-v5*
  :batch-size 1000
  :instruction-set (ADD SUB MUL DIV SIN COS LOG EXP)
  :registers (R from 1 to 11) 
  :observations (OBS from 1 to 11)
  :output-registers (R from 1 to 3)
  :constant-range '(-10.0 10.0)
  :objectives (mean-squared-error complexity)
  :tournament-size 4
  :num-threads 8
  :population-size 1000
  :generations 10
  :minimum-program-length 8
  :maximum-program-length 128
  :observation-probability 0.5
  :constant-probability 0.5
  :mutate-instruction-probability 1.0
  :mutate-register-probability 0.5
  :mutate-operation-probability 0.25
  :mutate-constant-probability 0.25
  :add-instruction-probability 1.0
  :delete-instruction-probability 1.0
  :swap-instruction-probability 1.0
  :constant-mutation-std 1.0
  :maximum-instruction-count 256)
