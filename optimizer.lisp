(in-package :bes)

(defun fitness-using-constants (dataset learners team tpg x)
  (clear-cache)
  (let* ((substitutions (loop for new-learner in (replace-doubles learners x)
                              for learner in learners
                              collect (cons learner new-learner)))
         (sol (sublis substitutions tpg)))
    (cdaadr (eval-team team sol dataset))))

(defun tune-constants-in-team (tpg team-id dataset)
  "Use CMA-ES to search for the optimal constant values within the genotype."
  (let* ((team (find-team-by-id tpg team-id))
         (learner-ids (team-learners team))
         (learners (loop for learner-id in learner-ids
                         collect (find-learner-by-id tpg learner-id)))
         (num-constants (count-tunable-constants learners)))
    (format t "~A~%" num-constants)
    (when (> num-constants 0)
      (let ((eval-fn (lambda (x) (funcall #'fitness-using-constants dataset learners team tpg x))))
        (let* ((best-sol (cma-es:run eval-fn num-constants :generations 500))
               (substitutions (loop for new-learner in (replace-doubles learners best-sol)
                                    for learner in learners
                                    collect (cons learner new-learner))))
          substitutions)))))

(defun replace-doubles (seq constants)
  (replace-tree-walk (lambda (node) (sb-int:double-float-p node)) seq constants))

(defun replace-tree-walk (predicate seq constants)
  (let ((remaining-values constants))
    (labels ((walker (node)
               (cond ((funcall predicate node)
                      (if remaining-values
                          (pop remaining-values)
                          (error "Not enough values to replace all instances of target-symbol.")))
                     ((consp node)
                      (cons (walker (car node))
                            (walker (cdr node))))
                     (t
                      node))))
      (walker seq))))

(defun count-tunable-constants (learners)
  "Given a sequence of LEARNERs, count how many constants there are."
  (cond ((and (atom learners) (or (equal learners 'C)
                                  (sb-int:single-float-p learners)
                                  (sb-int:double-float-p learners))) 1)
        ((atom learners) 0)
        (t (reduce #'+ (mapcar (lambda (x) (count-tunable-constants x)) learners)))))
         
(defun tune-constants (tpg dataset)
  (let ((current-tpg tpg))
    (loop for team-id in (mapcar #'team-id (teams tpg))
          do (setf current-tpg (sublis (tune-constants-in-team current-tpg team-id dataset) current-tpg)))
    current-tpg))

