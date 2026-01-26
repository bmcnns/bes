(defun replace-constants! (team constants)
  "Walks in-place, reading from a vector of constants using an index."
  (let ((idx 0)) ; Simple integer counter
    (labels ((walker (node)
               (cond 
                 ((consp node)
                  (let ((head (car node)))
                    (cond 
                      ((sb-int:double-float-p head)
                       ;; Direct vector access - Zero Consing
                       (setf (car node) (aref constants idx)) 
                       (incf idx))
                      ((consp head)
                       (walker head))))
                  (walker (cdr node)))
                 (t nil))))
      (dolist (learner (team-learners team))
        (walker (learner-program learner)))
      team)))

(defun get-constants (team)
  "Returns a list of all double-float constants found in a team's programs."
  (let ((found nil))
    (labels ((walker (node)
               (cond ((sb-int:double-float-p node) 
                      (push node found))
                     ((consp node)
                      (walker (car node))
                      (walker (cdr node))))))
      (dolist (learner (team-learners team))
        (walker (learner-program learner)))
      (nreverse found))))                    

(defun to-c-double-array (list)
  (let ((len (length list)))
    (make-array len 
                :element-type 'double-float 
                :initial-contents (mapcar (lambda (x) (coerce x 'double-float)) list))))

(defun tune-team! (team dataset &key generations pop-size (sigma 0.1d0))
  (let ((constants (get-constants team)))
    (if constants
        (let ((eval-fn (lambda (x) (progn
                                        (replace-constants! team x)
                                        (eval-team team dataset)))))
          (let* ((best-sol (cma-es:run eval-fn (length constants)
                                       :xinit0 (to-c-double-array constants)
                                       :xstd0 sigma
                                       :generations generations
                                       :pop-size pop-size)))
            (replace-constants! team best-sol))))))
