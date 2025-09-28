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
  ;; search
  population-size
  generations
  ;; initialization
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
  remove-instruction-probability
  swap-instruction-probability
  constant-mutation-std
  maximum-instruction-count
  ;; tpg parameters
  actions
  initial-minimum-number-of-learners
  initial-maximum-number-of-learners
  minimum-number-of-learners
  maximum-number-of-learners
  ;; team mutation probabilities
  mutate-learner-probability
  add-learner-probability
  remove-learner-probability
  ;; learner mutation probabilities
  mutate-learner-program-vs-action-probability
  learner-atomic-action-probability
  mutate-team-probability)

(defmacro defexperiment (name &rest options)
  "Define a global variable NAME bound to an EXPERIMENT initialized with keyword arguments.
   Each keyword in PROPERTIES should correspond to a slot in the EXPERIMENT struct.

   Example:
     (defexperiment *Hopper-v5*
        :batch-size 1000
        :population-size 1000
        :generations 1000
        :registers (R from 1 to 11)
        :observations (OBS from 1 to 11)
        :add-instruction-probability 1.0
        :objectives (mean-squared-error complexity)
        ...)"
  (flet ((expand-symbol-range (form)
           (destructuring-bind (prefix from start to end) form
             (declare (ignore from to))
             `(symbols ,prefix from ,start to ,end))))
    (let ((expanded-options
            (loop for (key val) on options by #'cddr
                  append
                  (cond
                    ((eq key :instruction-set)
                     `(:instruction-set (make-instruction-set ,@(mapcar (lambda (x) `',x) val))))
                    ((and (member key '(:registers :observations :output-registers))
                          (listp val) (eq (second val) 'from))
                     (list key (expand-symbol-range val)))
                    ((eq key :objectives)
                     `(:objectives (list ,@(mapcar (lambda (x) `(quote ,(intern (string x) :keyword))) val))))
                    (t (list key val))))))
  `(defparameter ,name
     (make-experiment ,@expanded-options)))))


