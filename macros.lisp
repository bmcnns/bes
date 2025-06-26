(defmacro -> (x &rest forms)
  "Thread X through FORMS. Each form receives X as its first argument."
  (reduce (lambda (acc form)
            (if (consp form)
                `,(cons (car form) (cons acc (cdr form))) ; (f acc arg1 arg2...)
                `(,form ,acc))) ; if just a symbol: (f acc)
          forms
          :initial-value x))

(defmacro symbols (prefix from start to end)
  `(list ,@(loop for i from start to end
                 collect `(intern ,(format nil "~A~D" prefix i)))))


                                        ; Our minimalist unit-testing library
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (let ((result (gensym "RESULT")))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-frequency ((n target threshold) &body body)
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


(load "mutation.tests.lisp")
