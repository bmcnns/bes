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

(defun find-learner-by-id (tpg learner-id)
  (let ((learners (learners tpg)))
    (or (find learner-id learners :key #'learner-id)
        (error "Learner not found."))))

(defun find-team-by-id (tpg team-id)
  (let ((teams (teams tpg)))
    (or (find team-id teams :key #'team-id)
        (error "Team not found."))))

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

(defun make-tpg ()
  (let ((teams '())
        (learners '())
        (population-size (experiment-population-size *experiment*)))
    (loop repeat population-size
          do (multiple-value-bind (new-team new-learners) (make-team)
               (push new-team teams)
               (setf learners (append new-learners learners))))
    `(TPG (LEARNERS ,@learners) (TEAMS ,@teams))))

(defun eval-tpg (tpg dataset)
  (let ((teams (teams tpg))
        (num-threads (experiment-num-threads *experiment*)))
    (with-population teams num-threads
      (eval-team individual tpg dataset))))
