(in-package :bes)

(defmacro -> (x &rest forms)
  "Thread X through FORMS. Each form receives X as its first argument."
  (reduce (lambda (acc form)
            (if (consp form)
                `,(cons (car form) (cons acc (cdr form))) ; (f acc arg1 arg2...)
                `(,form ,acc))) ; if just a symbol: (f acc)
          forms
          :initial-value x))

(defmacro symbols (prefix from start to end)
  "Return a list of symbols formed by appending integers START through END
   to the given PREFIX. E.g., (symbols R from 1 to 3) => (R1 R2 R3)."
  `(list ,@(loop for i from start to end
                 collect `(intern ,(format nil "~A~D" prefix i)))))


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

(defmacro with-population (population threads &body forms)
  "Execute FORMS on each individual in POPULATION in parallel.
   THREADS specifies how many threads to use."
  (let ((pop-var (gensym "POP")))
    `(let ((,pop-var ,population))
       (if (> ,threads 1)
           (progn
             (unless (find-package 'lparallel)
               (ql:quickload :lparallel))
             (setf lparallel:*kernel* (lparallel:make-kernel ,threads))
             (unwind-protect
                  (progn
                    ,@(loop for form in forms
                            collect
                            `(setf ,pop-var
                                   (lparallel:pmap 'list (lambda (individual) ,form) ,pop-var))))
               (lparallel:end-kernel :wait t)))
           (progn
             ,@(loop for form in forms
                     collect
                     `(setf ,pop-var
                            (mapcar (lambda (individual) ,form) ,pop-var)))))
       ,pop-var)))


(defmacro wall-clock-time (&body body)
  "Executes BODY and prints the wall-clock time (in seconds)."
  `(let ((start-time (get-internal-real-time)))
     (multiple-value-prog1
         (progn ,@body)
       (let ((elapsed (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
         (format t "~&Wall clock time: ~,4F seconds~%" elapsed)))))

(defmacro defpopulation (name size experiment)
  "Define a global population NAME as a list of SIZE genotypes.
   Each genotype is created using NEW-INDIVIDUAL with EXPERIMENT
   provided for initialization parameters."
  `(defparameter ,name (loop repeat ,size
                             collect (new-individual ,experiment))))

(defmacro no-result (&body body)
  "Evaluate BODY and return NIL. Useful when the result will
   take very long to return."
  `(progn ,@body nil))


(defparameter *fp-max* 1e10
  "Maximum allowable floating-point value for clamping.")

(defparameter *fp-min* -1e10
  "Minimum allowable floating-point value for clamping.")

(defun clamp (x &optional (min *fp-min*) (max *fp-max*))
  "Clamp X to the range [MIN, MAX]. If X is not a real number, return 0.0"
  (cond
    ((not (realp x)) 0.0)
    ((> x max) max)
    ((< x min) min)
    (t x)))

(defmacro safe-wrapper (fn)
  "Return a lambda that applies FN to its arguments, then clamps the result.
   Use to wrap arbitrary math functions safely."
  `(lambda (&rest args)
     (clamp (apply ,fn args))))

(defmacro def-safe-operator (name fn arity)
  "Define a new function NAME with ARITY arguments that applies FN and clamps the result.
   Prevents floating-point overflows or invalid math behaviour during program execution."
  (let ((params (loop for i from 1 to arity collect (gensym "ARG"))))
    `(defun ,name ,params
       (clamp (,fn ,@params)))))
