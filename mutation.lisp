(in-package :bes)

;;; mutation.lisp
;;; ------------
;;; This file defines the mutation operations for a genotype
;;; including both micro-mutations (e.g., mutations at the instruction level), and
;;; macro-mutations (e.g., mutations at the genotype level).
;;;
;;; The design is inspired by Stephen Kelly's Tangled Program Graph implementation.
;;; Each mutation is applied independently according to experiment-defined probabilities.

(defun mutate-dest (dest)
  "Return a randomly chosen destination register that is *not* DEST.
   Ensures mutations actually change the genotype."
  (let* ((registers (experiment-registers *experiment*)))
    (random-choice (remove dest registers))))

(defun mutate-opcode (opcode)
  "Return a randomly selected opcode with the same arity as OPCODE,
   excluding OPCODE itself."
  (let* ((arity (lookup-arity opcode))
         (compatible-opcodes (instructions-with-arity arity)))
    (random-choice (remove opcode compatible-opcodes))))

(defun mutate-argument (argument)
  "Randomly mutate ARGUMENT to either:
   - A new constant (based on *EXPERIMENT*'s constant probability), or
   - A new observation variable or register, excluding the original ARGUMENT."
  (let ((constant-probability (experiment-constant-probability *experiment*))
        (constant-range (experiment-constant-range *experiment*)))
    
    (if (bernoulli constant-probability)
        (random-range (first constant-range) (first (last constant-range)))
        (random-choice (remove argument `(,@(experiment-observations *experiment*) ,@(experiment-registers *experiment*)))))))

(defun swap-instructions (genotype)
  "Randomly swap two instructions in the GENOTYPE."
  (let* ((i (random (length genotype)))
        (j (loop for r = (random (length genotype))
                 until (/= r i)
                 finally (return r))))
    (loop for instr in genotype
          for idx from 0
          collect (cond
                    ((= idx i) (nth j genotype))
                    ((= idx j) (nth i genotype))
                    (t instr)))))

(defun add-instruction (genotype)
  "Insert a new randomly generated instruction at a random position'
   in GENOTYPE. The instruction is generated based on *EXPERIMENT*'s configuration.
   There is *no* check that the genotype will remain under the maximum length.
   Instead, this check is performed externally in 'maybe-add-instruction'."
  (let* ((i (random (1+ (length genotype))))
         (new-instr (random-instruction)))
    (loop for idx from 0
          for instr in genotype
          appending (if (= idx i)
                        (list new-instr instr)
                        (list instr))
          finally (when (= i (length genotype))
                    (return (append genotype (list new-instr)))))))

(defun remove-instruction (genotype)
  "Remove a random instruction from GENOTYPE.
   There is *no* check that the genotype has more than one instruction.
   This check is performed externally in 'maybe-remove-instruction."
  (let* ((i (random (length genotype))))
    (loop for instr in genotype
          for idx from 0
          when (/= idx i)
            collect instr)))

(defun mutate-constant (genotype)
  "Select a random constant in GENOTYPE and perturb it using Gaussian noise
   with standard deviation from *EXPERIMENT*. If no constants exists,
   return the original GENOTYPE unchanged."
  (let* ((constant-locations (loop for instr in genotype
                                   for instr-idx from 0
                                   append (loop for arg in (cddr instr)
                                                for arg-idx from 2
                                                when (numberp arg)
                                                  collect (cons instr-idx arg-idx))))
         (target (when constant-locations (random-choice constant-locations))))
    (if (null target)
        genotype
        (let* ((std (experiment-constant-mutation-std *experiment*))
               (instr-idx (car target))
               (arg-idx (cdr target))
               (old-instr (nth instr-idx genotype))
               (old-const (nth arg-idx old-instr))
               (new-const (normal old-const std))
               (new-instr
                 (loop for x in old-instr
                       for i from 0
                       collect (if (= i arg-idx) new-const x))))
          (loop for instr in genotype
                for i from 0
                collect (if (= i instr-idx) new-instr instr))))))

(defun mutate-instruction (genotype)
  "Mutate a randomly chosen instruction in GENOTYPE.
   The mutation may target:
   - the destination register
   - the opcode
   - or one of the arguments (register, observation, or constant)."
  (let* ((choice-of-mutation (random-choice '(destination opcode args)))
         (target (random (length genotype)))
         (old-instruction (nth target genotype))
         (new-instruction
           (cond ((equal choice-of-mutation 'destination)
                  (loop for x in old-instruction
                        for i from 0
                        collect (if (= i 0) (mutate-dest x) x)))

                 ((equal choice-of-mutation 'opcode)
                  (loop for x in old-instruction
                        for i from 0
                        collect (if (= i 1) (mutate-opcode x) x)))

                 ((equal choice-of-mutation 'args)
                  (let* ((arg-index (random-range 2 (length old-instruction)))
                         (old-arg (nth arg-index old-instruction))
                         (new-arg (mutate-argument old-arg)))
                    (loop for x in old-instruction
                          for i from 0
                          collect (if (= i arg-index) new-arg x))))

                 (t (error "Unknown form of mutation ~A~%" choice-of-mutation))))
         (new-genotype (loop for instruction in genotype
                             for i from 0
                             collect (if (= i target) new-instruction instruction))))
    ;; this is necessary because sometimes the arguments
    ;; are both mutated and the left argument becomes the right
    ;; and the right argument becomes the left
    ;; and it doesn't appear to be a mutation
    (if (equal new-genotype genotype)
        (mutate-instruction genotype)
        new-genotype)))

(defun maybe-add-instruction (genotype)
  "Add a new instruction to GENOTYPE with a probability defined by *EXPERIMENT*.
   Only adds if the GENOTYPE's instruction count is below the *EXPERIMENT*'s maximum."
  (let ((add-instruction-probability (experiment-add-instruction-probability *experiment*))
        (maximum-instruction-count (experiment-maximum-instruction-count *experiment*)))
    (if (and (bernoulli add-instruction-probability) (< (length genotype) maximum-instruction-count))
        (add-instruction genotype)
        genotype)))

(defun maybe-remove-instruction (genotype)
  "Remove a random instruction from GENOTYPE with a probability defined by *EXPERIMENT*.
   Only removes if more than one instruction is present."
  (let ((remove-instruction-probability (experiment-remove-instruction-probability *experiment*)))
    (if (and (bernoulli remove-instruction-probability) (> (length genotype) 1))
        (remove-instruction genotype)
        genotype)))

(defun maybe-swap-instructions (genotype)
  "Swap two instructions in GENOTYPE with a probability defined by *EXPERIMENT*.
   Only applies if the genotype has more than one instruction."
  (let ((swap-instruction-probability (experiment-swap-instruction-probability *experiment*)))
    (if (and (bernoulli swap-instruction-probability) (> (length genotype) 1))
        (swap-instructions genotype)
        genotype)))

(defun maybe-mutate-instruction (genotype)
  "Apply an instruction mutation (destination, opcode, or argument)
   with a probability defined by *EXPERIMENT*."
  (let ((mutate-instruction-probability (experiment-mutate-instruction-probability *experiment*)))
    (if (bernoulli mutate-instruction-probability)
        (mutate-instruction genotype)
        genotype)))

(defun maybe-mutate-constant (genotype)
  "Mutate a constant in GENOTYPE using Gaussian noise, with probability defined by *EXPERIMENT*.
   Only applies if the GENOTYPE contains a constant."
  (let ((mutate-constant-probability (experiment-mutate-constant-probability *experiment*)))
    (if (bernoulli mutate-constant-probability)
        (mutate-constant genotype)
        genotype)))
    
(defun mutate (genotype)
  "Apply a series of stochastic mutations to GENOTYPE according to the *EXPERIMENT*'s settings.
   Mutation steps include:
    - maybe adding an instruction
    - maybe removing an instruction
    - maybe swapping two instructions
    - maybe mutating an instruction
    - maybe mutating a constant
    Uses the `->` macro to thread GENOTYPE through each stage."
  (-> genotype
      (maybe-add-instruction)
      (maybe-remove-instruction)
      (maybe-swap-instructions)
      (maybe-mutate-instruction)
      (maybe-mutate-constant)))

