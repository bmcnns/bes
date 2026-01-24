(in-package :bes)

(defun fitness-using-constants (dataset learners team tpg x)
  (clear-cache)
  (let* ((substitutions (loop for new-learner in (replace-doubles learners x)
                              for learner in learners
                              collect (cons learner new-learner)))
         (sol (sublis substitutions tpg)))
    (cdaadr (eval-team team sol dataset))))

(defun fitness-using-constants (dataset learners team tpg x)
  (clear-cache)
  ;; Map the new parameter vector 'x' onto the learners of this team
  (let ((overridden-learners (replace-doubles learners x)))
    ;; We assume eval-team can take an optional 'overrides' list or alist
    ;; (cdaadr (eval-team team tpg dataset :learner-overrides overridden-learners))
    
    ;; If you can't modify eval-team easily, use a local 'mock' team:
    (let ((local-team `(TEAM ,(team-id team) ,@(mapcar #'learner-id overridden-learners))))
      ;; Inject the new learners into a temporary learner-table or context
      (cdaadr (eval-team local-team tpg dataset)))))

(defun tune-constants-in-team (tpg team-id dataset &key generations pop-size (xstd0 0.1d0))
  "Use CMA-ES to search for the optimal constant values within the genotype."
  (let* ((team (find-team-by-id tpg team-id))
         (learner-ids (team-learners team))
         (learners (loop for learner-id in learner-ids
                         collect (find-learner-by-id tpg learner-id)))
         (num-constants (count-tunable-constants learners)))
    (format t "~A~%" num-constants)
    (when (> num-constants 0)
      (let ((eval-fn (lambda (x) (funcall #'fitness-using-constants dataset learners team tpg x)))
            (constants (get-previous-constants learners)))
        (let* ((best-sol (cma-es:run eval-fn num-constants
                                     :xinit0 constants
                                     :xstd0 xstd0
                                     :generations generations
                                     :pop-size pop-size))
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

(defun get-previous-constants (learners)
  "Given a sequence of LEARNERs, return the original constants in a vector."
  (let* ((num-constants (count-tunable-constants learners))
         (constants (make-array num-constants :element-type 'double-float :initial-element 0.0d0)))
    (labels ((collect-constants (x)
               (cond ((and (atom x) (or (equal x 'C)
                                        (sb-int:single-float-p x)
                                        (sb-int:double-float-p x))) (list x))
                     ((atom x) nil)
                     (t (reduce #'append (mapcar #'collect-constants x))))))
      (let ((vals (collect-constants learners)))
        (loop for i from 0 below (length constants)
              do (setf (aref constants i) (elt vals i)))))
    constants))

(defun tune-constants (tpg dataset &key (generations 20) (pop-size 40) (sigma 0.1d0))
  (let ((current-tpg tpg))
    (loop for team-id in (mapcar #'team-id (teams tpg))
          if (bernoulli (experiment-tune-constants-probability *experiment*))
          do (setf current-tpg (sublis (tune-constants-in-team current-tpg team-id dataset
                                                               :xstd0 sigma
                                                               :generations generations
                                                               :pop-size pop-size) current-tpg)))
    current-tpg))

