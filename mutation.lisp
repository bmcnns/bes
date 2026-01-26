(in-package :bes)

;;; ---------------------------------------------------------------------------
;;; 1. INSTRUCTION-LEVEL (MICRO) MUTATIONS
;;; ---------------------------------------------------------------------------

(defun random-gaussian (&optional (mean 0.0) (stddev 1.0))
  "Generates a random number from a normal distribution using Box-Muller."
  (let ((u1 (random 1.0))
        (u2 (random 1.0)))
    ;; Handle edge case where u1 is 0 to avoid log(0)
    (loop while (zerop u1) do (setf u1 (random 1.0)))
    (+ mean (* stddev (sqrt (* -2.0 (log u1))) (cos (* 2.0 pi u2))))))

(defun mutate-dest (dest)
  "Return a randomly chosen destination register that is *not* DEST."
  (let* ((registers (experiment-registers *experiment*)))
    (random-choice (remove dest registers))))

(defun mutate-opcode (opcode)
  "Return a randomly selected opcode with the same arity as OPCODE, excluding itself."
  (let* ((arity (lookup-arity opcode))
         (compatible-opcodes (instructions-with-arity arity)))
    (random-choice (remove opcode compatible-opcodes))))

(defun mutate-argument (argument)
  "Mutates an argument. 
   If targeting a Constant:
     - If old arg was Constant: 70% Gaussian noise, 30% Reset (new random).
     - If old arg was Register: Always Reset (new random).
   If targeting a Register:
     - Standard switch logic."
  (let* ((constant-probability (experiment-constant-probability *experiment*))
         (constant-range (experiment-constant-range *experiment*))
         (min-val (first constant-range))
         (max-val (first (last constant-range)))
         ;; Calculate dynamic sigma (e.g., 10% of the total range) for noise
         (sigma (* 0.10 (- max-val min-val))))

    (if (coin-flip constant-probability)
        ;; CASE: We want a Constant
        (if (and (numberp argument)      ;; Check if it currently IS a constant
                 (coin-flip 0.70))       ;; 70% chance to add noise
            
            ;; Option A: Gaussian Noise
            ;; We clamp the result to stay within valid range (optional but safer)
            (let ((noise (random-gaussian 0.0 sigma)))
              (max min-val (min max-val (+ argument noise))))

            ;; Option B: Reset (30% chance or if prev arg was not constant)
            (random-range min-val max-val))

        ;; CASE: We want a Register/Observation
        (random-choice (remove argument `(,@(experiment-observations *experiment*) 
                                          ,@(experiment-registers *experiment*)))))))

(defun mutate-individual-instruction (instr)
  "The core logic for changing a single instruction's parts."
  (let ((choice (random-choice '(destination opcode args))))
    (cond ((eq choice 'destination)
           (cons (mutate-dest (first instr)) (rest instr)))
          ((eq choice 'opcode)
           (list* (first instr) (mutate-opcode (second instr)) (cddr instr)))
          ((and (eq choice 'args) (> (length instr) 2))
           (let* ((arg-index (random-range 2 (length instr)))
                  (new-arg (mutate-argument (nth arg-index instr))))
             (loop for x in instr for i from 0
                   collect (if (= i arg-index) new-arg x))))
          (t (cons (mutate-dest (first instr)) (rest instr))))))

(defun mutate-instructions (genotype)
  "Each instruction has an independent chance to be modified.
   Returns the new genotype (which might be identical to the old one)."
  (let ((p (experiment-mutate-instruction-probability *experiment*)))
    (mapcar (lambda (instr)
              (if (coin-flip p)
                  (mutate-individual-instruction instr)
                  instr))
            genotype)))

;;; ---------------------------------------------------------------------------
;;; 2. PROGRAM-LEVEL MUTATIONS (MACRO)
;;; ---------------------------------------------------------------------------

(defun swap-instructions (genotype)
  (let* ((len (length genotype))
         (i (random len))
         (j (loop for r = (random len) until (/= r i) finally (return r)))
         (new-genotype (copy-list genotype)))
    (rotatef (nth i new-genotype) (nth j new-genotype))
    new-genotype))

(defun add-instruction (genotype)
  (let* ((i (random (1+ (length genotype))))
         (new-instr (random-instruction)))
    (append (subseq genotype 0 i) (list new-instr) (subseq genotype i))))

(defun remove-instruction (genotype)
  (let ((i (random (length genotype))))
    (append (subseq genotype 0 i) (subseq genotype (1+ i)))))

(defun mutate-program (program)
  "Applies stochastic structural changes AND instruction tweaks.
   GUARANTEE: The output program will ALWAYS be different from the input."
  (destructuring-bind (_ id instructions) program
    (declare (ignore _ id))
    (let ((new-id (funcall *program-id-generator*))
          (mutated (copy-list instructions))
          (changed-p nil)) ;; Track if we have made a change yet

      ;; 1. Structural: Maybe Add
      (when (and (coin-flip (experiment-add-instruction-probability *experiment*))
                 (< (length mutated) (experiment-maximum-instruction-count *experiment*)))
        (setf mutated (add-instruction mutated))
        (setf changed-p t))

      ;; 2. Structural: Maybe Remove
      (when (and (coin-flip (experiment-remove-instruction-probability *experiment*))
                 (> (length mutated) (experiment-minimum-program-length *experiment*)))
        (setf mutated (remove-instruction mutated))
        (setf changed-p t))

      ;; 3. Structural: Maybe Swap
      (when (and (coin-flip (experiment-swap-instruction-probability *experiment*))
                 (> (length mutated) 1))
        (setf mutated (swap-instructions mutated))
        (setf changed-p t))

      ;; 4. Micro: Mutate Instructions
      ;; We check if the result is different to set changed-p
      (let ((micro-mutated (mutate-instructions mutated)))
        (unless (equal micro-mutated mutated)
          (setf mutated micro-mutated)
          (setf changed-p t)))

      ;; 5. THE IMPACT GUARANTEE
      ;; If the dice rolls all failed (changed-p is nil), FORCE a mutation.
      (unless changed-p
        (let ((target-idx (random (length mutated))))
          (setf (nth target-idx mutated) 
                (mutate-individual-instruction (nth target-idx mutated)))))

      `(PROGRAM ,new-id ,mutated))))

;;; ---------------------------------------------------------------------------
;;; 3. LEARNER & ACTION MUTATIONS
;;; ---------------------------------------------------------------------------

(defun leads-to-p (start-team target-team)
  "Returns T if there is a path from START-TEAM to TARGET-TEAM."
  (let ((visited (make-hash-table :test #'eq)))
    (labels ((dfs (current)
               (cond
                 ((eq current target-team) t) ;; Found the target!
                 ((gethash current visited) nil) ;; Already checked this node
                 (t 
                  (setf (gethash current visited) t)
                  ;; Search all children (actions that are Teams)
                  (some (lambda (learner)
                          (let ((act (learner-action learner)))
                            (and (typep act 'team) ;; Check if action is a Team
                                 (dfs act))))      ;; Recurse
                        (team-learners current))))))
      (dfs start-team))))


(defparameter *p-link* 0.5 "Probability of generating a Team pointer action.")

(defun mutate-action (current-action tpg current-team)
  "Mutates action to either Atomic or Team Pointer. Checks for cycles."
  (declare (ignore current-action))
  
  (if (coin-flip *p-link*)
      ;; CASE 1: Atomic Action (0, 1, 2...)
      (random-choice (experiment-actions *experiment*))
      (random-choice (experiment-actions *experiment*))))
      
      ;; ;; CASE 2: Team Pointer
      ;; (let ((all-teams (tpg-teams tpg))
      ;;       (candidate nil)
      ;;       (attempts 0))
        
      ;;   ;; Try to find a valid team to point to (Max 10 attempts to avoid hanging)
      ;;   (loop while (and (< attempts 10) (null candidate)) do
      ;;     (let ((rand-team (random-choice all-teams)))
      ;;       (cond
      ;;         ;; Rule 1: Don't point to yourself
      ;;         ((eq rand-team current-team) nil)
              
      ;;         ;; Rule 2: Don't create a cycle
      ;;         ;; If rand-team already leads to me, I can't point to it.
      ;;         ((leads-to-p rand-team current-team) nil)
              
      ;;         ;; Valid!
      ;;         (t (setf candidate rand-team))))
      ;;     (incf attempts))
        
      ;;   ;; Return candidate if found, otherwise fallback to Atomic
      ;;   (or candidate (random-choice (experiment-actions *experiment*))))))


(defun mutate-learner (learner tpg team)
  "Creates a new learner by mutating either the program or the action."
  ;; Note: mutate-program now GUARANTEES a change, so we don't need extra checks here.
  (if (coin-flip (experiment-mutate-learner-program-probability *experiment*))
      (make-learner :id (funcall *learner-id-generator*)
                    :program (mutate-program (learner-program learner))
                    :action (learner-action learner))
      (make-learner :id (funcall *learner-id-generator*)
                    :program (learner-program learner)
                    :action (mutate-action (learner-action learner) tpg team))))

;;; ---------------------------------------------------------------------------
;;; 4. TEAM-LEVEL (MACRO) MUTATIONS
;;; ---------------------------------------------------------------------------

(defun add-learner (team tpg)
  "Adds a learner from another team to this team."
  (let* ((max-learners (experiment-maximum-number-of-learners *experiment*))
         (current-learners (team-learners team))
         (all-reachable (get-all-reachable-teams tpg))
         (potential (set-difference (remove-duplicates 
                                     (loop for tm in all-reachable append (team-learners tm)))
                                    current-learners)))
    (if (and (< (length current-learners) max-learners) potential)
        (let ((candidate (random-choice potential)))
          (%make-team :id (funcall *team-id-generator*)
                      :learners (cons candidate current-learners)))
        team)))

(defun remove-learner (team)
  "Removes a random learner if above minimum size."
  (let ((learners (team-learners team))
        (min (experiment-minimum-number-of-learners *experiment*)))
    (if (> (length learners) min)
        (%make-team :id (funcall *team-id-generator*)
                    :learners (remove (random-choice learners) learners))
        team)))

(defun mutate-team (team tpg)
  "The top-level breeder call. 
   GUARANTEE: This will always return a NEW team with at least one modification."
  (let ((new-team team)
        (mutation-occurred-p nil))

    ;; 1. Try Add Learner
    (when (coin-flip (experiment-add-learner-probability *experiment*))
      (let ((added (add-learner new-team tpg)))
        (unless (eq added new-team)
          (setf new-team added mutation-occurred-p t))))

    ;; 2. Try Remove Learner
    (when (coin-flip (experiment-remove-learner-probability *experiment*))
      (let ((removed (remove-learner new-team)))
        (unless (eq removed new-team)
          (setf new-team removed mutation-occurred-p t))))

    ;; 3. Try Mutate Learner
    (when (coin-flip (experiment-mutate-learner-probability *experiment*))
      (let* ((learners (team-learners new-team))
             (old-l (random-choice learners))
             (new-l (mutate-learner old-l tpg team)))
        (unless (eq old-l new-l)
          (setf new-team (%make-team :id (funcall *team-id-generator*)
                                     :learners (substitute new-l old-l learners)))
          (setf mutation-occurred-p t))))

    ;; 4. IMPACT GUARANTEE
    ;; If probabilities were low and nothing happened, force a learner mutation.
    (unless mutation-occurred-p
      (let* ((learners (team-learners new-team))
             (old-l (random-choice learners))
             (new-l (mutate-learner old-l tpg team)))
        ;; We know mutate-learner GUARANTEES a new ID, so this is safe.
        (setf new-team (%make-team :id (funcall *team-id-generator*)
                                   :learners (substitute new-l old-l learners)))))
    new-team))
