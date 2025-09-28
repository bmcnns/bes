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
    
(defun mutate-program (program)
  "Apply a series of stochastic mutations to GENOTYPE according to the *EXPERIMENT*'s settings.
   Mutation steps include:
    - maybe adding an instruction
    - maybe removing an instruction
    - maybe swapping two instructions
    - maybe mutating an instruction
    - maybe mutating a constant
    Uses the `->` macro to thread GENOTYPE through each stage."
  (destructuring-bind (_ id instructions) program
    ;; syntactic sugar to have PROGRAM as part of the representation
    (declare (ignore _))
    ;; we generate a new id since PROGRAMs are immutable
    (declare (ignore id))
    ;; mutated programs
    (let ((new-id (funcall *program-id-generator*))
          (mutated (-> instructions
                       (maybe-add-instruction)
                       (maybe-remove-instruction)
                       (maybe-swap-instructions)
                       (maybe-mutate-instruction)
                       (maybe-mutate-constant))))
      `(PROGRAM ,new-id ,mutated))))

(defun mutate-learner-action-to-atomic (tpg learner)
  (declare (ignore tpg))
  (let* ((new-learner-id (funcall *learner-id-generator*))
         (all-actions (experiment-actions *experiment*))
         (available-actions (remove (learner-action learner) all-actions)))
    (if available-actions
        `(LEARNER ,new-learner-id ,(learner-program learner) ,(random-choice available-actions))
        learner)))

(defun safe-gotos-from-team (tpg from-id &optional (adj (build-adjacency tpg)))
  "All team-ids that won't create a cycle if FROM-ID points to them."
  (remove-if (lambda (tid) (would-create-cycle-p tpg from-id tid adj))
             (mapcar #'team-id (teams tpg))))

(defun mutate-learner-action-to-reference (tpg learner team)
  (let* ((new-learner-id (funcall *learner-id-generator*))
         (team-ids (safe-gotos-from-team tpg (team-id team))))
    (if team-ids
        `(LEARNER ,new-learner-id ,(learner-program learner) (GOTO ,(random-choice team-ids)))
        learner)))

(defun mutate-learner-action (tpg learner team)
  (let ((atomic-action-probability (experiment-learner-atomic-action-probability *experiment*)))
    (if (bernoulli atomic-action-probability)
        (mutate-learner-action-to-atomic tpg learner)
        (mutate-learner-action-to-reference tpg learner team))))

(defun mutate-learner-program (learner)
  (let ((new-learner-id (funcall *learner-id-generator*))
        (program (learner-program learner))
        (action (learner-action learner)))
    `(LEARNER ,new-learner-id ,(mutate-program program) ,action)))

(defun mutate-learner (tpg team &key (attempts 3) (return-original-learner-id nil))
  (let* ((new-team-id (funcall *team-id-generator*))
         (learner-ids (team-learners team))
         (learner-id (random-choice learner-ids))
         (learner (find-learner-by-id tpg learner-id))
         (mutate-learner-program-vs-action-probability
           (experiment-mutate-learner-program-vs-action-probability *experiment*))
         (mutated-learner
           (if (bernoulli mutate-learner-program-vs-action-probability)
               (mutate-learner-program learner)
               (mutate-learner-action tpg learner team)))
         (mutated-learner-id (learner-id mutated-learner))
         (mutated-team `(TEAM ,new-team-id ,@(append (remove learner-id learner-ids) (list mutated-learner-id))))
         (mutated-tpg `(TPG (LEARNERS ,@(append (list mutated-learner) (learners tpg))) (TEAMS ,@(append (list mutated-team) (teams tpg))))))
    (if (> attempts 0)
        (if (>= (team-count-unique-atomic-actions mutated-tpg mutated-team) 2)
            (if return-original-learner-id
                (values mutated-team mutated-learner learner-id)
                (values mutated-team mutated-learner))
            (mutate-learner tpg team :attempts (1- attempts) :return-original-learner-id return-original-learner-id))
        (values team nil))))

(defun add-learner (tpg team)
  (let* ((new-team-id (funcall *team-id-generator*))
        (all-learners (learners tpg))
        (all-learners-that-wont-introduce-cycle
          (remove-if (lambda (learner) (if (reference-p (learner-action learner))
                                           (would-create-cycle-p tpg (team-id team) (get-reference (learner-action learner)))
                                           nil)) all-learners))
        (maximum-learner-count (experiment-maximum-number-of-learners *experiment*)))
    (if (< (1- (length all-learners)) maximum-learner-count)
        (let* ((all-learner-ids (mapcar #'learner-id all-learners-that-wont-introduce-cycle))
               (learners (team-learners team))
               (available-learner-ids (set-difference all-learner-ids (team-learners team))))
          (if available-learner-ids
              (let ((new-learner (random-choice available-learner-ids)))
                `(TEAM ,new-team-id ,@(append learners (list new-learner))))
              team))
        team)))

(defun remove-learner (tpg team)
  ;; does not remove learner if it would leave the team
  ;; without at least two unique atomic learners
  (let* ((learners (team-learners team))
         (original-team-id (team-id team))
         (permutations
           (loop for learner in learners
                 collect `(TEAM ,original-team-id ,@(remove learner learners))))
         (candidates (remove-if-not (lambda (permutation) (>= (team-count-unique-atomic-actions tpg permutation) 2))
                                    permutations)))
    (if candidates
        (let ((new-team-id (funcall *team-id-generator*)))
          `(TEAM ,new-team-id ,@(team-learners (random-choice candidates))))
        team)))

(Defun maybe-add-learner (tpg team)
  (let ((add-learner-probability (experiment-add-learner-probability *experiment*)))
    (if (bernoulli add-learner-probability)
        (add-learner tpg team)
        team)))

(defun maybe-remove-learner (tpg team)
  (let ((remove-learner-probability (experiment-remove-learner-probability *experiment*)))
    (if (bernoulli remove-learner-probability)
        (remove-learner tpg team)
        team)))

(defun maybe-mutate-learner (tpg team)
  (let ((mutate-learner-probability (experiment-mutate-learner-probability *experiment*)))
    (if (bernoulli mutate-learner-probability)
        (mutate-learner tpg team)
        (values team nil))))

(defun mutate-team (tpg team)
  (maybe-mutate-learner tpg (maybe-remove-learner tpg (maybe-add-learner tpg team))))

(defun maybe-mutate-team (tpg team)
  ;; ask malcolm if teams are always mutated -- or whether some teams are unmutated.
  ;; or check stephen's code base first.
  (let ((mutate-team-probability (experiment-mutate-team-probability *experiment*)))
    (if (bernoulli mutate-team-probability)
        (mutate-team tpg team)
        (values team nil))))

(defun mutate-tpg (tpg)
  (let* ((learners (learners tpg))
         (teams    (teams tpg))
         (teams-to-add '())
         (learners-to-add '()))
    (dolist (team teams)
      (multiple-value-bind (modified-team maybe-new-learner)
          (maybe-mutate-team tpg team)
        (when maybe-new-learner
          (push maybe-new-learner learners-to-add))
        (unless (member modified-team teams :test #'equal)
          (push modified-team teams-to-add))))
    `(TPG
      (LEARNERS ,@(append learners (nreverse learners-to-add)))
      (TEAMS    ,@(append teams    (nreverse teams-to-add))))))

(defun mutate-linear-gp (lgp)
  (let ((programs (mapcar #'mutate-program (programs lgp))))
    `(LINEAR-GP ,@programs)))
