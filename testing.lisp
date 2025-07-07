                                        ; Our minimalist unit-testing library
(defvar *test-name* nil
  "*TEST-NAME* is updated by `deftest` to track nested test call chains.")


(defmacro deftest (name parameters &body body)
  "Define a test function named NAME with PARAMETERS. Within the test,
   CHECK and CHECK-FREQUENCY macros can be used. *TEST-NAME* is updated
   to track nested test call chains."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Evaluate each form and report the result as a test case.
   Uses REPORT-RESULT to log pass/fail status for each form."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Evaluate each form and report the result as a test case.
   Uses REPORT-RESULT to log pass/fail status for each form.
   Similar to AND but there is no short-circuiting."
  (let ((result (gensym "RESULT")))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Print the result of a single test case."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-frequency ((n target threshold) &body body)
  "Evaluate BODY N times and compute how often it returns true.
   Passes if the observed proportion is within THRESHOLD of TARGET.
   Useful for statistical checks, e.g., for measuring stochastic distributions."
  (let ((occurrences (gensym "OCCURRENCES"))
        (trial (gensym "TRIAL"))
        (proportion (gensym "PROPORTION"))
        (result (gensym "RESULT")))
    `(let ((,occurrences 0))
       (dotimes (,trial ,n)
         (when (progn ,@body)
           (incf ,occurrences)))
       (let* ((,proportion (/ ,occurrences ,n))
             (,result (<= (abs (- ,proportion ,target)) ,threshold)))
         (format t "~:[FAIL~;pass~]... ~A [~A/~A] (~,2f) ~A~%" ,result *test-name* ,occurrences ,n ,proportion ',body)
         ,result))))
