(defstruct experiment
  dataset
  batch-size
  instruction-set
  registers
  observations
  output-registers
  fitness
  constant-range
  ; search
  population-size
  generations
  ; initialization
  maximum-program-length
  minimum-program-length
  observation-probability
  constant-probability
  ;; mutation
  mutate-instruction-probability
  mutate-register-probability
  mutate-operation-probability
  mutate-constant-probability
  add-instruction-probability
  delete-instruction-probability
  swap-instruction-probability
  constant-mutation-std
  maximum-instruction-count)

(defmacro defexperiment (name &body properties)
  `(defparameter ,name
     (make-experiment ,@properties)))

(defdataset *Hopper-Expert-v5*)

(defexperiment Hopper-v5
   :batch-size 1000
   :instruction-set (make-instruction-set 'ADD 'SUB 'MUL 'DIV 'SIN 'COS 'LOG 'EXP)
   :registers (symbols R from 1 to 11) 
   :observations (symbols OBS from 1 to 11)
   :output-registers (symbols R from 1 to 3)
   :fitness 'mean-squared-error
   :constant-range '(-10.0 10.0)
   ; search
   :population-size 1000
   :generations 10
   ; initialization 
   :minimum-program-length 2
   :maximum-program-length 6
   :observation-probability 0.2
   :constant-probability 0.2
   ;; mutation
   :mutate-instruction-probability 1.0
   :mutate-register-probability 0.5
   :mutate-operation-probability 0.25
   :mutate-constant-probability 0.25
   :add-instruction-probability 0.5
   :delete-instruction-probability 0.5
   :swap-instruction-probability 0.2
   :constant-mutation-std 0.5
   :maximum-instruction-count 256)
