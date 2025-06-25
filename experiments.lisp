(defstruct experiment
  instruction-set
  registers
  observations
  output-registers
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

(defparameter Hopper-v5
  (make-experiment
   :instruction-set (make-instruction-set 'ADD 'SUB 'MUL 'DIV 'SIN 'COS 'LOG 'EXP)
   :registers '(R1 R2 R3 R4 R5 R6 R7 R8 R9 R10) 
   :observations '(OBS1 OBS2 OBS3 OBS4 OBS5 OBS6)
   :output-registers '(R1 R2 R3)
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
   :maximum-instruction-count 256))

