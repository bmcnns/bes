(in-package :bes)

;; <tpg>      ::= (TPG (LEARNERS <learner>+) (TEAMS <team>+))
;; <learner>  ::= (LEARNER <lid> <program> <action>)
;; <program>  ::= (<instr>+)
;; <instr>    ::= (R<idx> <op> <src>)         ; e.g., (R1 SIN OBS3)
;; <action>   ::= <atomic-symbol> | (GOTO <team-id>) | <program>
;; <team>     ::= (TEAM <tid> <lid>+)

(defun initialize-tpg ()
  "Returns a randomly created TPG."
  '(TPG
    (LEARNERS
     (LEARNER L1 (PROGRAM P1 ((R1 SIN OBS1))) TRIANGLE)
     (LEARNER L2 (PROGRAM P2 ((R1 SIN OBS2))) (GOTO T3))
     (LEARNER L3 (PROGRAM P3 ((R1 SIN OBS3))) SQUARE)
     (LEARNER L4 (PROGRAM P4 ((R1 SIN OBS4))) CIRCLE)
     (LEARNER L5 (PROGRAM P5 ((R1 SIN OBS5))) RHOMBUS)
     (LEARNER L6 (PROGRAM P6 ((R1 SIN OBS6))) TRIANGLE)
     (LEARNER L7 (PROGRAM P7 ((R1 SIN OBS7))) SQUARE))
    (TEAMS
     (TEAM T1 L3 L4 L5)
     (TEAM T2 L2 L4 L6)
     (TEAM T3 L1 L3))))

(defun teams (tpg)
  "Returns the TEAMs of a TANGLED PROGRAM GRAPH"
  (unless (tpg-p tpg)
    (error "Expecting a TPG. Got ~A instead.~%" tpg))
  (cdaddr tpg))

(defun learners (tpg)
  "Returns the LEARNERs of a TANGLED PROGRAM GRAPH"
  (unless (tpg-p tpg)
    (error "Expecting a TPG. Got ~A instead.~%" tpg))
  (cdadr tpg))

(defun programs (lgp)
  "Returns the PROGRAMs of a LINEAR GP"
  (unless (linear-gp-p lgp)
    (error "Expecting a Linear GP. Got ~A instead.~%" lgp))
  (cdr lgp))

(defun program-p (form)
  (and
   (listp form)
   (equal (car form) 'PROGRAM)
   (symbolp (cadr form))
   (listp (caddr form))
   (listp (caaddr form))))

(defun learner-p (form)
  (and
   (listp form)
   (equal (car form) 'LEARNER)
   (program-p (caddr form))
   (and
    (cadddr form)
    (or
     (program-p (cadddr form))
     (listp (cadddr form))
     (symbolp (cadddr form))
     (numberp (cadddr form))))))

(defun team-p (form)
  (and
   (listp form)
   (equal (car form) 'TEAM)
   (every #'symbolp (cddr form))))
   
(defun tpg-p (form)
  (and
   (listp form)
   (equal (car form) 'TPG)
   (equal (caadr form) 'LEARNERS)
   (> (length (cdadr form)) 0)
   (equal (caaddr form) 'TEAMS)
   (every #'team-p (cdaddr form))
   (every (lambda (team) (> (length (team-learners team)) 0)) (cdaddr form)) 
   (> (length (cdaddr form)) 0)
   (every #'learner-p (cdadr form))))

(defun mutate-program (program)
  "Mutates a program."
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

(defun program-id (program)
  (unless (program-p program)
    (error "PROGRAM-ID expects a PROGRAM. Got ~A instead.~%" program))
  (cadr program))

(defun program-instructions (program)
  (unless (program-p program)
    (error "PROGRAM-INSTRUCTIONS expects a PROGRAM. Got ~A instead.~%" program))
  (caddr program))

(defun team-id (team)
  (unless (team-p team)
    (error "TEAM-ID expects a TEAM. Got ~A instead.~%" team))
  (cadr team))

(defun team-learners (team)
  (unless (team-p team)
    (error "TEAM-LEARNERS expects a TEAM. Got ~A instead.~%" team))
  (cddr team))

(defun learner-id (learner)
  (unless (learner-p learner)
    (error "LEARNER-ID expects a LEARNER. Got ~A instead.~%" learner))
  (cadr learner))

(defun learner-program (learner)
  (unless (learner-p learner)
    (error "LEARNER-PROGRAM expects a LEARNER. Got ~A instead.~%" learner))
  (caddr learner))

(defun learner-action (learner)
  (unless (learner-p learner)
    (error "LEARNER-ACTION expects a LEARNER. Got ~A instead.~%" learner))
  (cadddr learner))

(defun team-count-unique-atomic-actions (tpg team)
  (let* ((learner-ids (team-learners team))
         (learners (mapcar (lambda (lid) (find-learner-by-id tpg lid)) learner-ids))
         (atomic-learners (remove-if-not #'atomic-p learners))
         (unique-atomic-actions (remove-duplicates (mapcar #'learner-action atomic-learners) :test #'equal)))
    (length unique-atomic-actions)))

(defun mutate-learner-action-to-atomic (tpg learner)
  (declare (ignore tpg))
  (let* ((new-learner-id (funcall *learner-id-generator*))
         (all-actions (experiment-actions *experiment*))
         (available-actions (remove (learner-action learner) all-actions)))
    (if available-actions
        `(LEARNER ,new-learner-id ,(learner-program learner) ,(random-choice available-actions))
        learner)))

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
  
(defun find-learner-by-id (tpg learner-id)
  (let ((learners (learners tpg)))
    (or (find learner-id learners :key #'learner-id)
        (error "Learner not found."))))

(defun find-team-by-id (tpg team-id)
  (let ((teams (teams tpg)))
    (or (find team-id teams :key #'team-id)
        (error "Team not found."))))

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
          
(defun execute-program (program observations)
  (let ((instructions (caddr program)))
    (if (program-p program)
        (phenotype instructions observations)
        (error "Tried to execute a program but the thing you're trying to execute~%is not a program. ~A" program))))

(defun reference-p (form)
  "A reference is a list (GOTO <team-id>) where <team-id> is a symbol or string."
  (and (consp form) (eq (first form) 'GOTO) (second form)))

(defun atomic-p (learner)
  "A learner is atomic if its action is not a reference to another team."
  (not (reference-p (learner-action learner))))

(defun get-bid (learner observation)
  (let ((program (learner-program learner)))
    (elt (execute-program program observation) 0)))

(defun get-reference (reference)
  (if (reference-p reference)
      (cadr reference)
      (error "Tried to get a reference of something that is not a reference. ~A~%" reference)))

(defun follow-reference (tpg reference observation &key (visited nil))
  (cond ((member reference visited) (error "Cycle detected during team execution."))
        ((team-p reference) (execute-team tpg (team-id reference) observation :visited visited))
        ((learner-p reference) (let* ((goto-team-id (get-reference (learner-action reference)))
                                      (goto-team (find-team-by-id tpg goto-team-id)))
                                 (push goto-team-id visited)
                                 (follow-reference tpg goto-team observation :visited visited)))))

(defun suggest-action (learner observation)
  (let ((action (learner-action learner)))
    (if (program-p action)
        (execute-program action observation)
        action)))

(defun execute-team (tpg team-id observation &key (visited nil))
  (let* ((team (find-team-by-id tpg team-id))
         (learner-ids (team-learners team))
         (learners (mapcar (lambda (lid) (find-learner-by-id tpg lid)) learner-ids))
         (bids (mapcar (lambda (learner) (get-bid learner observation)) learners)))
    (let ((highest-bidder (elt learners (argmax bids))))
      (if (atomic-p highest-bidder)
          (suggest-action highest-bidder observation)
          (follow-reference tpg highest-bidder observation :visited visited)))))

(defun remove-dangling-learners (tpg)
  (let* ((teams (teams tpg))
         (learners (learners tpg))
         (active-learner-ids (remove-duplicates (mappend #'team-learners teams))))
    `(TPG (LEARNERS ,@(remove-if-not (lambda (l) (member (learner-id l) active-learner-ids)) learners))
          (TEAMS ,@teams))))

(defun team-neighbours (tpg team)
  "List of target team-ids this TEAM references via its learners' actions."
  (let* ((learners (mapcar (lambda (learner-id) (find-learner-by-id tpg learner-id))
                           (team-learners team)))
         (non-atomic-learners (remove-if-not (lambda (learner) (reference-p (learner-action learner))) learners))
         (references (mapcar #'learner-action non-atomic-learners))
         (gotos (mapcar #'get-reference references)))
    gotos))
    
(defun build-adjacency (tpg)
  "Return hash: team-id -> list target-team-id ... )."
  (let ((adj (make-hash-table :test 'equal))
        (teams (teams tpg)))
    (dolist (team teams)
      (let ((team-id (team-id team))
            (neighbours (team-neighbours tpg team)))
        (setf (gethash team-id adj) neighbours)))
    adj))

(defun reachable-p (tpg from-id to-id &optional (adj (build-adjacency tpg)))
  "Is there a path from FROM-ID to TO-ID in adjacency ADJ?"
  (let ((seen (make-hash-table :test 'equal))
        (stack (list from-id)))
    (loop while stack
          for v = (pop stack) do
            (when (equal v to-id) (return-from reachable-p t))
            (unless (gethash v seen)
              (setf (gethash v seen) t)
              (dolist (w (gethash v adj)) (push w stack))))
    nil))

(defun would-create-cycle-p (tpg from-id to-id &optional (adj (build-adjacency tpg)))
  "Adding edge FROM-ID -> TO-ID creates a cycle iff TO-ID can already reach FROM-ID,
   or TO-ID = FROM-ID (self-loop)."
  (or (equal from-id to-id)
      (reachable-p tpg to-id from-id adj)))

(defun safe-gotos-from-team (tpg from-id &optional (adj (build-adjacency tpg)))
  "All team-ids that won't create a cycle if FROM-ID points to them."
  (remove-if (lambda (tid) (would-create-cycle-p tpg from-id tid adj))
             (mapcar #'team-id (teams tpg))))

(defun make-learner ()
  (let ((actions (experiment-actions *experiment*)))
    `(LEARNER ,(funcall *learner-id-generator*) ,(make-program) ,(random-choice actions))))

(defun make-team (&key (attempts 5))
  (let* ((min (experiment-initial-minimum-number-of-learners *experiment*))
         (max (experiment-initial-maximum-number-of-learners *experiment*))
         (learners (loop repeat (random-range min max)
                         collect (make-learner)))
         (atomic-learners (remove-if-not #'atomic-p learners))
         (unique-atomic-actions (remove-duplicates (mapcar #'learner-action atomic-learners) :test #'equal))
         (team `(TEAM ,(funcall *team-id-generator*)
                      ,@(mapcar #'learner-id learners))))
    (cond
      ((>= (length unique-atomic-actions) 2)
       (values team learners))
      ((> attempts 0)
       (make-team :attempts (1- attempts)))
      (t
       (error "Ran out of attempts to make a team. Not enough distinct atomic actions to ensure >= 2 per team.")))))

(defun make-tpg ()
  (let ((teams '())
        (learners '())
        (population-size (experiment-population-size *experiment*)))
    (loop repeat population-size
          do (multiple-value-bind (new-team new-learners) (make-team)
               (push new-team teams)
               (setf learners (append new-learners learners))))
    `(TPG (LEARNERS ,@learners) (TEAMS ,@teams))))

(defun eval-team (team tpg dataset)
  (let* ((team-id (team-id team))
         (observations (observations dataset))
         (actions (actions dataset))
         (predictions (mapcar (lambda (obs) (execute-team tpg team-id obs)) observations)))
    (fitness (cons tpg team) actions predictions)))

(defun eval-tpg (tpg dataset)
  (let ((teams (teams tpg))
        (num-threads (experiment-num-threads *experiment*)))
    (with-population teams num-threads
      (eval-team individual tpg dataset))))

(defun tournament-selection (fitness-scores)
  ; with replacement
  (let* ((tournament-size (experiment-tournament-size *experiment*))
         (tournament (loop repeat tournament-size
                           collect (random-choice fitness-scores)))
         (objective-scores (mapcar #'cdaadr tournament))
         (ids (mapcar #'car tournament))
         (winner-idx (argmin objective-scores))
         (winner (elt ids winner-idx)))
  (format t "~A~%" winner)))

(defun make-linear-gp ()
  (let* ((population-size (experiment-population-size *experiment*))
         (programs (loop repeat population-size
                         collect (make-program))))
    `(LINEAR-GP ,@programs)))

(defun linear-gp-p (form)
  (and (equal (car form) 'LINEAR-GP)
       (every #'program-p (cdr form))))

(defun eval-program (program dataset)
  (let* ((observations (observations dataset))
         (actions (actions dataset))
         (predictions (mapcar (lambda (obs) (execute-program program obs)) observations)))
    (fitness program actions predictions)))

(defun eval-linear-gp (lgp dataset)
  (let ((programs (programs lgp))
        (num-threads (experiment-num-threads *experiment*)))
    (with-population programs num-threads
      (eval-program individual dataset))))

(defun mutate-linear-gp (lgp)
  (let ((programs (mapcar #'mutate-program (programs lgp))))
    `(LINEAR-GP ,@programs)))
