(in-package :bes)

                                        ; Fitness Metrics

(defun mean-squared-error (y-predicted y-truth)
  "Compute the Mean Squared Error (MSE) between Y-PREDICTED and Y-TRUTH.
   Handles both flat and batched inputs
   - If Y-PREDICTED is a flat list of numbers, computes standard MSE.
   - If Y-PREDICTED is a list of lists (batches), computes the total MSE across all batches."
  (if (numberp (first y-predicted))
      ;; Flat case
      (let* ((squared-errors (mapcar (lambda (y1 y2) (expt (- y1 y2) 2))
                                     y-predicted y-truth))
             (sum-squared-errors (reduce #'+ squared-errors)))
        (/ sum-squared-errors (length squared-errors)))
      ;; Batched case
      (/ (reduce #'+ (mapcar #'mean-squared-error y-predicted y-truth)) (length y-predicted))))

;; TODO: Add correct complexity measure for teams.
(defun complexity (genotype tpg)
  "Return the complexity of GENOTYPE, defined here as its length."
  (if (program-p genotype)
      (return-from complexity (length (program-instructions genotype))))
  (if (and tpg (team-p genotype))
      (let* ((learner-ids (team-learners genotype))
             (learners (mapcar (lambda (lid) (find-learner-by-id tpg lid)) learner-ids))
             (learner-lengths (apply #'+ (mapcar #'length (mapcar #'learner-program learners)))))
        learner-lengths)))

(defparameter *objectives*
  `((:complexity . ,#'(lambda (genotype tpg truth prediction)
                        (declare (ignore truth prediction))
                        `(COMPLEXITY . ,(complexity genotype tpg))))
    (:mean-squared-error . ,#'(lambda (genotype tpg truth prediction)
                                (declare (ignore genotype tpg))
                                `(LOSS . ,(mean-squared-error truth prediction))))
    (:support . ,#'(lambda (genotype tpg truth prediction)
                     (declare (ignore tpg truth prediction ))
                     (support genotype)))
    (:confidence . ,#'(lambda (genotype tpg truth prediction)
                     (declare (ignore tpg truth prediction))
                     (confidence genotype))))

  "The available objectives to optimize in a fitness function.
   This maps between a defined *EXPERIMENT* and the dynamic fitness
   function being generated to lazy evaluate objectives")

                                        ; Fitness Calculation

;; this seems like a key candidate for data-driven programming
(defun make-fitness-function (objectives)
  "Construct a fitness function from a list of OBJECTIVES.
   Each objective is a keyword that maps to a fitness function in *OBJECTIVES*.
   The resulting function accepts (genotype truth prediction) and returns a cons:
   (genotype . list-of-objective-values)
   NOTE: This is lazy evaluation so only the objectives provided will be computed."
  (lambda (genotype truth prediction)
    (let* ((is-tpg (and (listp genotype) (tpg-p (car genotype)) (team-p (cdr genotype))))
           (id (cond ((program-p genotype) (program-id genotype))
                     (is-tpg (team-id (cdr genotype)))
                     (t (error "Tried calculating fitness of something other than a program or a team. ~A~%" genotype)))))
      (cons id
            (list (mapcar
                   (lambda (objective)
                     (let ((fn (cdr (assoc objective *objectives*))))
                       (unless fn
                         (error "Unknown objective: ~A" objective))
                       (if is-tpg
                           (funcall fn (cdr genotype) (car genotype) truth prediction)
                           (funcall fn genotype nil truth prediction))))
                   objectives))))))

(defun fitness (genotype truth prediction)
  "Evaluate the FITNESS of a GENOTYPE using the objectives defined in *EXPERIMENT*.
   Returns (genotype . (obj1 [obj2]...)"
  (let ((objectives (experiment-objectives *experiment*)))
    (funcall (make-fitness-function objectives) genotype truth prediction)))
