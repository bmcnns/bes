(in-package :bes)

(defvar *teams* (make-hash-table :test 'equal))
(defvar *learners* (make-hash-table :test 'equal))

(defun make-unique-id-generator (prefix)
  "Returns a closure that generates a unique ID
   Note: Not thread-safe."
  (let ((counter 0))
    (lambda ()
      (format nil "~A~D" (string-upcase prefix) (incf counter)))))

(defparameter *team-id-generator* (make-unique-id-generator "TEAM-"))
(defparameter *learner-id-generator* (make-unique-id-generator "LEARNER-"))

(defstruct team
  (id (funcall *team-id-generator*))
  learners)

(defstruct learner
  (id (funcall *learner-id-generator*))
  (program nil)
  (action nil))

(defstruct program
  (genotype nil))

(defun action (learner)
  "Returns the action of the LEARNER"
  (learner-action learner))

(defun root-team-p (team)
  "T if no other TEAM points to this TEAM."
  nil)

(defun atomic-p (learner)
  "Returns T if the LEARNER is suggesting an action.
   Returns NIL if the LEARNER references another team."
  (not (reference-p (action learner))))

(defun register-zero (registers)
  "Returns the first register of a sequence of registers."
  (elt registers 0))

(defun get-bid (learner-id experiment observation)
  "Returns the first register of the LEARNER's program
   output after execution. This register is used as the bid
   of the learner on suggesting its action to the team."
  ;redundant lookup
  (cons (lookup-learner learner-id) (register-zero (phenotype (program-genotype (learner-program (lookup-learner learner-id))) experiment observation))))
               
(defun lookup-learner (learner-id)
  (if (gethash learner-id *learners*)
      (gethash learner-id *learners*)
      (error "Learner not found.")))

(defun lookup-team (team-id)
  (if (gethash team-id *teams*)
      (gethash team-id *teams*)
      (error "Team not found.")))

(defun suggest-action (learner)
  (if (atomic-p learner)
      (learner-action learner)
      (error "Action suggested for non-atomic learner.")))

(defun strip-goto-from-reference (reference)
  (if (reference-p reference)
      (second reference)
      (error "Tried to strip GOTO from a non-reference.")))
  
(defun get-reference (learner)
  (if (atomic-p learner)
      (error "Next learner attempted for atomic learner.")
      (lookup-team (strip-goto-from-reference (action learner)))))

(defun add-learner (learner)
  (setf (gethash (learner-id learner) *learners*) learner))

(defun add-team (team)
  (setf (gethash (team-id team) *teams*) team))

(defun reference-p (x)
  "A reference is a list (GOTO <team-id>) where <team-id> is a symbol or string."
  (and (consp x) (eq (first x) 'GOTO) (second x)))

(defun team-phenotype (team observation &key (visited nil))
  (eval (genotype (resolve-team team observation :visited visited)))

(defun get-learner-from-bid (bid)
  (car bid))

(defun select-learner (bids)
  "Selects the learner with the highest bid.
   Expects BIDS which are cons pairs of (LEARNER . BID)"
  (get-learner-from-bid (first (sort bids #'> :key #'cdr))))

;; <tpg>      ::= (TPG (LEARNERS <learner>*) (TEAMS <team>*))
;; <learner>  ::= (LEARNER <lid> <program> <action>)
;; <program>  ::= (<instr>+)
;; <instr>    ::= (R<idx> <op> <src>)         ; e.g., (R1 SIN OBS3)
;; <action>   ::= <atomic-symbol> | (GOTO <team-id>)
;; <team>     ::= (TEAM <tid> <lid>*)

(defparameter *tpg*
  '(TPG
    (LEARNERS
     (LEARNER L1 ((R1 SIN OBS1)) TRIANGLE)
     (LEARNER L2 ((R1 SIN OBS2)) (GOTO TEAM-3))
     (LEARNER L3 ((R1 SIN OBS3)) SQUARE)
     (LEARNER L4 ((R1 SIN OBS4)) CIRCLE)
     (LEARNER L5 ((R1 SIN OBS5)) RHOMBUS)
     (LEARNER L6 ((R1 SIN OBS6)) TRIANGLE))
    (TEAMS
     (TEAM TEAM-1 L3 L4 L5)
     (TEAM TEAM-2 L2 L4 L6)
     (TEAM TEAM-3 L1 L3))))

(defun %as-id-string (x)
  (etypecase x
    (string x)
    (symbol (string x))))

(defun parse-tangled-program-graph (tpg)
  (destructuring-bind (learners-section teams-section) (rest tpg)
    (declare (ignore TPG))
    (values learners-section teams-section)))

(defun build-learners (learners-section)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry (rest learners-section))
      (destructuring-bind (LEARNER lid program action) entry
        (declare (ignore LEARNER))
        (let* ((prog (make-program :genotype program))
               (act (if (and (consp action) (eq (first action) 'GOTO))
                        (list 'GOTO (%as-id-string (second action)))
                        action))
               (lrn (make-learner :id (%as-id-string lid)
                                  :program prog
                                  :action act)))
          (setf (gethash (%as-id-string lid) table) lrn)
          (add-learner lrn))))
    table))

(defun build-teams (teams-section learner-table)
  (dolist (entry (rest teams-section))
    (destructuring-bind (TEAM tid &rest lids) entry
      (declare (ignore TEAM))
      (let ((learner-objs (mapcar (lambda (lid)
                                    (or (gethash (%as-id-string lid) learner-table)
                                        (error "Unknown learner ~A" lid)))
                                  lids)))
        (let ((tm (make-team :id (%as-id-string tid)
                             :learners learner-objs)))
          (add-team tm))))))
                                  
(defun materialize-tpg (tpg)
  (setf *learners* (make-hash-table :test 'equal))
  (setf *teams* (make-hash-table :test 'equal))
  (multiple-value-bind (learners-section teams-section) (parse-tangled-program-graph tpg)
    (let ((learner-table (build-learners learners-section)))
      (build-teams teams-section learner-table)
      (validate-acyclic-or-error)
      t)))

(defvar *root-team-ids* '()
  "Cache of team IDs that are not referenced by any other teams (GOTO <team-id>).")

(defun team-ids ()
  "All team IDs currently in *teams*."
  (loop for k being the hash-keys of *teams* collect k))

(defun referenced-team-ids ()
  "Team IDs that appear as the target of a (GOTO <team-id>) action in any learner."
  (loop for lrn being the hash-values of *learners*
        for act = (learner-action lrn)
        when (and (consp act) (eq (first act) 'GOTO))
          collect (%as-id-string (second act))))

(defun compute-root-team-ids ()
  "Compute (but do not cache) the current set of root teams."
  (let* ((all (team-ids))
         (refs (remove-duplicates (referenced-team-ids) :test #'equal)))
    (set-difference all refs :test #'equal)))

(defun refresh-root-teams ()
  "Recompute and cache the root team IDs into *root-team-ids*."
  (setf *root-team-ids* (compute-root-team-ids)))

(defun follow-learner (reference observation &key (visited nil))
  (cond ((member reference visited) (error "Cycle detected."))
        ((team-p reference) (team-phenotype reference observation :visited visited))
         ((learner-p reference) (progn
                                  (push reference visited)
                                  (follow-learner (get-reference reference) observation :visited visited)))))
    
(defun genotype (team observation &key (visited nil))
  `(LET ((HIGHEST-BIDDER (SELECT-LEARNER
                          (LIST ,@(loop for learner in (team-learners team)
                                  collect `(GET-BID ,(learner-id learner) *Hopper-v5* ',observation))))))
     (IF (ATOMIC-P HIGHEST-BIDDER)
         (SUGGEST-ACTION HIGHEST-BIDDER)
         (FOLLOW-LEARNER HIGHEST-BIDDER ',observation :visited ',visited))))

(defun team-learner-ids (team)
  "Return the list of learner IDs currently in TEAM."
  (mapcar #'learner-id (team-learners team)))

(defun team-has-learner-id-p (team learner-id)
  "T iff TEAM already contains a learner with LEARNER-ID."
  (find learner-id (team-learner-ids team) :test #'equal))

(defun resolve-team (team-or-id)
  "Accept a TEAM object, symbol, or string; return the TEAM object."
  (etypecase team-or-id
    (team team-or-id)
    (string (lookup-team team-or-id))
    (symbol (lookup-team (string team-or-id)))))

(defun resolve-learner (learner-or-id)
  "Accept a LEARNER object, symbol, or string; return the LEARNER object."
  (etypecase learner-or-id
    (learner learner-or-id)
    (string (lookup-learner learner-or-id))
    (symbol (lookup-learner (string learner-or-id)))))

(defun remove-team (team-or-id)
  "Remove TEAM-OR-ID from *TEAMS*.
   Returns *TEAMS*."
  (let* ((team (resolve-team team-or-id))
         (tid (team-id team)))
    (remhash tid *teams*))
  *teams*)

(defun remove-learner (learner-or-id)
  "Remove LEARNER-OR-ID from *LEARNERS*.
   Returns *LEARNERS*."
  (let* ((lrn (resolve-learner learner-or-id))
         (lid (learner-id lrn)))
    (remhash lid *learners*))
  *learners*)

(defun add-learner-to-team (team-or-id learner-or-id)
  "Add LEARNER-OR-ID to TEAM-OR-ID if it does not introduce a cycle.
   Returns two values: TEAM and ADDED-P."
  (let* ((team (resolve-team team-or-id))
         (lrn (resolve-learner learner-or-id))
         (lid (learner-id lrn)))
    (when (team-has-learner-id-p team lid)
      (return-from add-learner-to-team (values team nil))) ; no-op

    ;; If learner action is a reference, adding create edge TEAM -> TARGET
    (let ((target (reference-target-id (learner-action lrn))))
      (when target
        (let* ((adj (build-adjacency)))
          ;; Simulate the new edge in our head: adding TEAM->TARGET
          ;; creates a cycle iff TARGET already reaches TEAM.
          (when (reachable-p target (team-id team) adj)
            (error "Refusing to add ~A to ~A: would create cycle (~A -> ... -> ~A -> ~A)."
                   lid (team-id team) target (team-id team) target)))))
    
    ;; order doesn't matter; cons for O(1)
    (push lrn (team-learners team))
    (setf (gethash (team-id team) *teams*) team)
    (refresh-root-teams)
    (values team t)))

(defun remove-learner-from-team (team-or-id learner-or-id)
  "Remove LEARNER from TEAM. Refuses to make TEAM empty.
   Returns two values: TEAM and REMOVED-P."
  (let* ((team (resolve-team team-or-id))
         (lrn (resolve-learner learner-or-id))
         (lid (learner-id lrn))
         (ls (team-learners team)))
    (unless (team-has-learner-id-p team lid)
      (error "Learner ~A is not a member of team ~A." lid (team-id team)))
    (when (= (length ls) 1)
      (error "Refusing to make team ~A empty." (team-id team)))
    (setf (team-learners team)
          (remove-if (lambda (x) (equal (learner-id x) lid)) ls :count 1))
    (setf (gethash (team-id team) *teams*) team)
    (refresh-root-teams)
    (values team t)))

(defun change-learner-action (learner-or-id action)
  "Change the LEARNER's action to ACTION if it does not create any cycles.
   Returns two values: LEARNER and CHANGED-P."
  (let* ((lrn (resolve-learner learner-or-id))
         (lid (learner-id lrn)))
    (when (null action)
      (error "Refusing to make learner ~A's action NIL." lid))
    (when (equal action (learner-action lrn))
      (return-from change-learner-action (values lrn nil))) ; no-op

    (let ((target (reference-target-id action)))
      (when target
        (let* ((adj (build-adjacency))
               (hosts (teams-containing-learner lid)))
          (dolist (host hosts)
            ;; Changing the learner creates edge HOST -> TARGET
            ;; illegal iff TARGET reaches HOST.
            (when (reachable-p target (team-id host) adj)
              (error "Refusing to set action of ~A to (GOTO ~A): host team ~A would enter a cycle."
                     lid target (team-id host)))))))
      
    (setf (learner-action lrn) action)
    (setf (gethash (learner-id lrn) *learners*) lrn)
    (refresh-root-teams)
    (values lrn t)))

(defun all-learner-ids ()
  (loop for k being the hash-keys of *learners* collect k))

(defun all-team-ids ()
  (mapcar #'%normalize-team-id (loop for k being the hash-keys of *teams* collect k)))

(defun all-actions ()
  '(SQUARE CIRCLE TRIANGLE RHOMBUS))

(defun available-actions-for-learner (learner)
  (let ((action (learner-action learner)))
    (remove action (all-actions))))

(defun available-learner-ids-for-team (team)
  (set-difference (all-learner-ids) (team-learner-ids team) :test #'equal))

(defun mutate-team-add-learner (team-or-id)
  "Add a random non-member learner to TEAM-OR-ID. Returns TEAM."
  (let* ((team (resolve-team team-or-id))
         (pool (available-learner-ids-for-team team)))
    (when (null pool)
      (error "No available learners to add to team ~A." (team-id team)))
    (multiple-value-bind (t2 addedp)
        (add-learner-to-team team (random-choice pool))
      (declare (ignore addedp))
      t2)))

(defun mutate-team-remove-learner (team-or-id)
  "Remove a random non-member learner from TEAM-OR-ID. Returns TEAM."
  (let* ((team (resolve-team team-or-id))
         (pool (team-learner-ids team))
         (ls (team-learners team)))
    (when (> (length pool) 1)
      (multiple-value-bind (t2 addedp)
          (remove-learner-from-team team-or-id (random-choice pool))
        (declare (ignore addedp))
        t2))))

(defun mutate-learner-swap-action (learner-or-id)
  "Change the action of LEARNER-OR-ID to a different action. Returns LEARNER."
  (let* ((lrn (resolve-learner learner-or-id))
         (pool (available-actions-for-learner lrn)))
    (when (>= (length pool) 1)
      (multiple-value-bind (lrn2 changedp)
          (change-learner-action lrn (random-choice pool))
        (declare (ignore changedp))
        lrn2))))

(defun %normalize-team-id (x)
  (etypecase x
    (string x)
    (symbol (string x))))

(defun reference-target-id (ref)
  "Return the target team-id STRING from a (GOTO ...) form, or return NIL."
  (when (reference-p ref)
    (%normalize-team-id (second ref))))

(defun team-out-neighbours (team)
  "List of target team-ids this TEAM references via its learners' actions."
  (remove nil
          (loop for lr in (team-learners team)
                for ref = (%normalize-team-id (reference-target-id (learner-action lr)))
                collect ref)))

(defun build-adjacency ()
  "Return hash: team-id -> (list target-team-id ...). Ignores dangling refs."
  (let ((adj (make-hash-table :test 'equal)))
    (dolist (tid (all-team-ids))
      (let* ((tm (lookup-team tid))
             (targets (team-out-neighbours tm)))
        (setf (gethash tid adj) targets)))
    adj))

(defun reachable-p (from-id to-id &optional (adj (build-adjacency)))
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

(defun find-any-cycle (&optional (adj (build-adjacency)))
  "Return a list representing a cycle path (v0 v1 ... v0), or NIL if acyclic."
  (let ((color (make-hash-table :test 'equal)) ; nil/white, :gray, :black
        (parent (make-hash-table :test 'equal))
        (cycle nil))
    (labels ((visit (u)
               (setf (gethash u color) :gray)
               (dolist (v (gethash u adj))
                 (cond
                   ((null (gethash v color))
                    (setf (gethash v parent) u)
                    (when (visit v) (return-from visit t)))
                   ((eq (gethash v color) :gray)
                    ;; Found a back-edge u -> v; reconstruct cycle u...v->u
                    (let ((path (list u)))
                      (loop for x = u then (gethash x parent)
                            until (equal x v)
                            do (push (gethash x parent) path))
                      (setf cycle
                            (let ((p (nreverse path)))
                              (append p (list (first p)))))
                      (return-from visit t)))))
               (setf (gethash u color) :black)
               nil))
      (dolist (u (all-team-ids))
        (unless (gethash u color)
          (when (visit u) (return-from find-any-cycle cycle))))
      nil)))

(defun validate-acyclic-or-error ()
  "Signal a descriptive error if a cycle exists anywhere."
  (let ((c (nth-value 0 (find-any-cycle))))
    (when c
      (let* ((*print-circle* nil)) ; ensure no #n=/#n#
        (error (format nil "Cycle detected through teams: ~{~A~^ -> ~}" c))))))

(defun teams-containing-learner (learner-id)
  "Return list of TEAM objects that list LEARNER-ID among their learners."
  (loop for tid in (all-team-ids)
        for tm = (lookup-team tid)
        when (find learner-id (team-learners tm) :key #'learner-id :test 'equal)
          collect tm))


(defun compute-unused-learners ()
  (loop for lid in (all-learner-ids)
        for lrn = (lookup-learner lid)
        when (null (teams-containing-learner lid))
          collect lrn))

(defun clean-up-learners ()
  (mapcar #'remove-learner (compute-unused-learners)))
