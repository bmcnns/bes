(in-package :bes)

(defun shuffle (list)
  "Return a shuffled copy of LIST using Fisher–Yates algorithm."
  (let* ((vec (coerce list 'vector))
         (n (length vec)))
    (loop for i from (1- n) downto 1
          do (rotatef (aref vec i)
                      (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun choose (list)
  (let ((elt (random-choice list)))
    (values elt (remove elt list))))

(defun observation (step)
  (car (observations (list step))))

(defun action (step)
  (car (actions (list step))))

(defun lexicase-selection (tpg pool remaining-cases)
  (cond ((equal (length pool) 1)
         (first pool))
        ((null remaining-cases)
         (random-choice pool))
        (t
         (multiple-value-bind (next-case remaining-cases) (choose remaining-cases)
           (let ((new-pool (remove-if-not
                        (lambda (root-team)
                          (equal (execute-team tpg (team-id root-team) (observation next-case))
                                 (action next-case)))
                        pool)))
             (if (null new-pool)
                 (lexicase-selection tpg pool remaining-cases)
                 (lexicase-selection tpg new-pool remaining-cases)))))))

(defun lexicase (model dataset log-fn)
  (let* ((pop-size (experiment-population-size *experiment*))
         (parents (loop for i from 1 to pop-size
                        collect (lexicase-selection model (root-teams model) (shuffle dataset)))))
    parents))

           
           
