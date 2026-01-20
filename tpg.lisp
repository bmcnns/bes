(in-package :bes)

;; <tpg>      ::= (TPG (LEARNERS <learner>+) (TEAMS <team>+))
;; <learner>  ::= (LEARNER <lid> <program> <action>)
;; <program>  ::= (<instr>+)
;; <instr>    ::= (R<idx> <op> <src>)         ; e.g., (R1 SIN OBS3)
;; <action>   ::= <atomic-symbol> | (GOTO <team-id>) | <program>
;; <team>     ::= (TEAM <tid> <lid>+)

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

(defun find-learner-by-id (tpg learner-id &key (learner-table nil))
  (let ((learners (learners tpg)))
    (if learner-table
        (gethash learner-id learner-table)
        (or
         (find learner-id learners :key #'learner-id)
         (error "Learner not found.")))))

(defun find-team-by-id (tpg team-id &key (team-table nil))
  (let ((teams (teams tpg)))
    (if team-table
        (gethash team-id team-table)
        (or (find team-id teams :key #'team-id)
            (error "Team not found.")))))

(defun remove-dangling-learners (tpg)
  (let* ((teams (teams tpg))
         (learners (learners tpg))
         (active-learner-ids (remove-duplicates (mappend #'team-learners teams))))
    `(TPG (LEARNERS ,@(remove-if-not (lambda (l) (member (learner-id l) active-learner-ids)) learners))
          (TEAMS ,@teams))))

(defun team-neighbours (tpg team &key (learner-table (build-learner-table tpg)))
  "List of target team-ids this TEAM references via its learners' actions."
  (unless team
    "NIL team provided. ~A~%" team)
  (let* ((learners (mapcar (lambda (learner-id) (or (gethash learner-id learner-table)
                                                    (error "Learner not found ~A on ~A team~%" learner-id team)))
                           (team-learners team)))
         (non-atomic-learners (remove-if-not (lambda (learner)
                                               (reference-p (learner-action learner)))
                                              learners))
         (references (mapcar #'learner-action non-atomic-learners))
         (gotos (mapcar #'get-reference references)))
    gotos))

(defun make-tpg ()
  (let ((teams '())
        (learners '())
        (population-size (experiment-population-size *experiment*)))
    (loop repeat population-size
          do (multiple-value-bind (new-team new-learners) (make-team)
               (push new-team teams)
               (setf learners (append new-learners learners))))
    `(TPG (LEARNERS ,@learners) (TEAMS ,@teams))))

(defun build-learner-table (tpg)
  (let ((learner-table (make-hash-table :test 'equal)))
    (dolist (learner (learners tpg))
      (setf (gethash (learner-id learner) learner-table) learner))
    learner-table))

(defun build-team-table (tpg)
  (let ((team-table (make-hash-table :test 'equal)))
    (dolist (team (teams tpg))
      (setf (gethash (team-id team) team-table) team))
    team-table))

(defun eval-tpg (tpg dataset)
  (let* ((num-threads (experiment-num-threads *experiment*))
         (learner-table (build-learner-table tpg))
         (team-table (build-team-table tpg))
         (root-teams (root-teams tpg :team-table team-table)))
    (multi-thread root-teams team num-threads
      (eval-team team tpg dataset :learner-table learner-table :team-table team-table))))

(defun root-team-ids (tpg)
  (let* ((team-ids (mapcar #'team-id (teams tpg)))
         (learners (learners tpg))
         (non-atomic-learners (remove-if #'atomic-p learners))
         (non-atomic-actions (mapcar #'learner-action non-atomic-learners))
         (references (mapcar #'get-reference non-atomic-actions)))
    (set-difference team-ids references)))
  
(defun root-teams (tpg &key (team-table (build-team-table tpg)))
  (let ((root-team-ids (root-team-ids tpg))
        (root-teams '()))
    (dolist (id root-team-ids)
      (when (or (gethash id team-table)
                (error "Team ID not found while searching root teams. ~A in ~A~%" id tpg))
        (push (gethash id team-table) root-teams)))
    root-teams))
  
(defun internal-team-ids (tpg)
  (let* ((team-ids (mapcar #'team-id (teams tpg)))
         (learners (learners tpg))
         (non-atomic-learners (remove-if #'atomic-p learners))
         (non-atomic-actions (mapcar #'learner-action non-atomic-learners))
         (references (mapcar #'get-reference non-atomic-actions)))
    references))

(defun internal-teams (tpg &key (team-table (build-team-table tpg)))
  (let ((internal-team-ids (internal-team-ids tpg))
        (internal-teams '()))
    (dolist (id internal-team-ids)
      (when (or (gethash id team-table)
                (error "Team ID not found while searching internal teams. ~A in ~A~%" id tpg))
        (push (gethash id team-table) internal-teams)))
    internal-teams))


(defun select-champion (tpg rollout-fn)
  (let* ((scores (funcall rollout-fn tpg))
         (min-score (reduce #'min (mapcar #'cdaadr scores)))
         (best-teams (mapcar #'car (remove-if-not (lambda (score)
                                                        (equal score min-score))
                                                      scores :key #'cdaadr))))
    (sort best-teams #'< :key (lambda (team-id) (team-complexity tpg team-id)))
    (first best-teams)))

(defun remove-learners (tpg learner-ids)
  ;; remove learner ids from all teams
  (let* ((teams (teams tpg))
         (modified-teams (loop for team in teams
                               collect `(TEAM ,(team-id team)
                                              ,@(remove-if (lambda (learner) (member learner learner-ids))
                                                          (team-learners team))))))
    (remove-dangling-learners `(TPG (LEARNERS ,@(learners tpg))
                                    (TEAMS ,@modified-teams)))))

(defun prune-tpg (tpg)
  `(TPG
    (LEARNERS
     ,@(mapcar #'clean-learner (learners tpg)))
    (TEAMS
     ,@(teams tpg))))
