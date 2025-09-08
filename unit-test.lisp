(defvar *tests* (make-hash-table :test 'equal)
  "A global registry of all unit tests."

(defstruct test-summary
  (passes 0)
  (fails 0))

(defvar *test-summary* (make-test-summary))

(defmacro deftest (name &body body)
  "Define a test NAME.
   If the first form in BODY is a string,
   it is used as the test's docstring."
  (let* ((has-doc (and body (stringp (first body))))
         (doc (when has-doc (first body)))
         (forms (if has-doc (rest body) body)))
    `(progn
       ,(when doc
          `(setf (documentation ',name 'function) ,doc))
       (setf (gethash ',name *tests*) (lambda () ,@forms))
       (loop for test-name being the hash-keys of *tests*
             collect test-name))))
  
(defmacro assert-true (&body forms)
  "Returns T if the EXPR evaluates to T."
  `(when ,(if (= (length forms) 1)
               (first forms)
               `(progn ,@forms))
     t))

(defmacro assert-false (&body forms)
  "Returns T if the EXPR evaluates to NIL."
  `(unless ,(if (= (length forms) 1)
               (first forms)
               `(progn ,@forms))
     t))

(defun assert-equal (form1 form2)
  (equal form1 form2))

(defmacro assert-error (&body forms)
  "Returns T if an error while evaluating FORMS."
  `(handler-case 
       (progn ,@forms nil)
    (error (c)
      (declare (ignore c))
      t)))

(defun pass-test ()
  (format t "PASSED~%")
  (incf (test-summary-passes *test-summary*)))

(defun fail-test ()
  (format t "FAILED~%")
  (incf (test-summary-fails *test-summary*)))

(defun run-tests ()
  "Outputs whether each test in *TESTS* passed or failed."
  (setf *test-summary* (make-test-summary))
  (let ((alphabetically-sorted-tests
          (sort (loop for k being the hash-keys of *tests* collect k) #'string<)))
    (dolist (test-name alphabetically-sorted-tests)
      (let ((test-function (gethash test-name *tests*)))
        (format t "==== ~A ====~%" test-name)
        (if (funcall test-function)
            (pass-test)
            (fail-test))))
    (let* ((passes (test-summary-passes *test-summary*))
           (failures (test-summary-fails *test-summary*))
           (total (+ passes failures)))
      (terpri)
      (format t "~,2F% -- ~A passed ~A failed." (* (/ passes total) 100) passes failures))
      (terpri)))

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
