(in-package :bes)

(defstruct experiment
  "An EXPERIMENT struct stores the full set of hyperparameters for evolutionary search.

   Fields include: batch size, instruction set, number of registers, population settings,
   initialization and mutation probabilities, and performance configuration (e.g., threads).

   This struct is intended to be used with `defexperiment` for defining reusable configurations."
  batch-size
  instruction-set
  registers
  observations
  output-registers
  objectives
  constant-range
  ;; selection
  tournament-size
  ;; performance 
  num-threads
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
  "Define a global variable NAME bound to an EXPERIMENT initialized with keyword arguments.
   Each keyword in PROPERTIES should correspond to a slot in the EXPERIMENT struct.

   Example:
     (defexperiment *Hopper-v5*
        :batch-size 1000
        :population-size 1000
        :generations 1000
        :registers 8
        :add-instruction-probability 1.0)
        ...)"
  `(defparameter ,name
     (make-experiment ,@properties)))
