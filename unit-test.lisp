(in-package :bes)

(defparameter *reset*  (format nil "~C[0m"  #\Esc))
(defparameter *green* (format nil "~C[32m" #\Esc))
(defparameter *red* (format nil "~C[31m" #\Esc))
(defparameter *orange* (format nil "~C[38;5;208m" #\Esc))

(defvar *tests* (make-hash-table :test 'equal)
  "A global registry of all unit tests.")

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

(defmacro assert-true (expr)
  "Returns T if the EXPR evaluates to T."
  `(if ,expr
       t
       (format t "~A~2T> ~A is FALSE not TRUE.~A~%" *orange* ',expr *reset*)))

(defmacro assert-false (expr)
  "Returns T if the EXPR evaluates to NIL."
  `(if ,expr
       (format t "~A~2T> ~A is TRUE not FALSE.~A~%" *orange* ',expr *reset*)
       t)) 

(defmacro assert-equal (form1 form2)
  `(if (equal ,form1 ,form2)
      t
      (format t "~A~2T> ~A is not equal to ~A.~A~%" *orange* ',form1 ',form2 *reset*)))

(defmacro assert-not-equal (form1 form2)
  `(if (not (equal ,form1 ,form2))
      t
      (format t "~A~2T> ~A is equal to ~A.~A~%" *orange* ',form1 ',form2 *reset*)))

(defmacro assert-frequency (trials frequency epsilon &body forms)
  "Run a Monte-Carlo for TRIALS times.
   Returns T if the FORMS returns T with probability within EPSILON of FREQUENCY.
   Passes if the observed probabiity is within THRESHOLD of FREQUENCY."
  `(let ((positives 0))
     (dotimes (trial ,trials)
       (when (progn ,@forms)
         (incf positives)))
     (let* ((probability (/ positives ,trials))
            (result (<= (abs (- probability ,frequency)) ,epsilon)))
       (if result
           t
           (let ((*print-right-margin* 45)) ; wrap at ~45 chars
             (format t "~A~<~:;~2T> Probability of ~A~% ~4Tbeing T was expected to be between ~,3F and ~,3F.~%~4TIt was ~,3F instead.~>~A~%"
                     *orange*
                     ',forms
                     (- ,frequency ,epsilon)
                     (+ ,frequency ,epsilon)
                     probability
                     *reset*))))))

(defmacro assert-error (&rest args)
  "Return T if evaluating FORMS signals an ERROR.
If :EXPECTS is provided, require that the error message contains it."
  (let* ((pos     (position :expects args))
         (expects (when pos (nth (1+ pos) args)))
         ;; Remove the key/value pair :EXPECTS <value> from ARGS
         (forms   (if pos
                      (append (subseq args 0 pos)
                              (subseq args (+ pos 2)))
                      args))
         ;; For nicer reporting when no error occurs
         (printed (if (= (length forms) 1)
                      (first forms)
                      (cons 'progn forms))))
    `(block assert-error
       (handler-case
           (progn
             ,@forms
             (format t "~A~2T> Expected an error, but none occurred when evaluating ~%~4T~S~A~%"
                     *orange* ',printed *reset*)
             nil)
         (error (c)
           ,(if expects
                `(if (search (string-upcase (princ-to-string ,expects))
                             (string-upcase (princ-to-string c)))
                     t
                     (progn
                       (format t "~A~2T> Error ~S does not contain ~S.~A~%"
                               *orange* (princ-to-string c) ,expects *reset*)
                       (return-from assert-error nil))))
           t)))))

(defmacro assert-always (trials &body forms)
  "Returns T if FORMS is T throughout ALL TRIALS."
  `(assert-frequency ,trials 1.0 0.0 ,@forms))

(defun pass-test ()
  (format t "~APASSED~A~%" *green* *reset*)
  (incf (test-summary-passes *test-summary*)))

(defun fail-test ()
  (format t "~AFAILED~A~%" *red* *reset*)
  (incf (test-summary-fails *test-summary*)))

(defun safely-run-test-code (test-function)
  (handler-case 
      (funcall test-function)
    (error (c)
      (format t "~AError occured while executing test. Error: ~A~A~%" *orange* (princ-to-string c) *reset*))))
  
(defun run-tests (&key (dangerous-execution nil))
  "Outputs whether each test in *TESTS* passed or failed."
  (setf *test-summary* (make-test-summary))
  (let ((alphabetically-sorted-tests
          (sort (loop for k being the hash-keys of *tests* collect k) #'string<)))
    (dolist (test-name alphabetically-sorted-tests)
      (run-test test-name :dangerous-execution dangerous-execution))
    (let* ((passes (test-summary-passes *test-summary*))
           (failures (test-summary-fails *test-summary*))
           (total (+ passes failures)))
      (terpri)
      (format t "~,2F% -- ~A passed ~A failed." (* (/ passes total) 100) passes failures))
    (terpri)))

(defmacro check (&rest forms)
  "Returns T if all the forms are T.
   If a form is NIL then remaining forms are still evaluated."
  (cond
    ((null forms) t)
    ((null (cdr forms)) (car forms))
    (t
     (let ((ok (gensym)))
       `(let ((,ok t))
          ,@(mapcar (lambda (f)
                      `(unless ,f (setf ,ok nil)))
                    forms)
          (if ,ok ,(car (last forms)) nil))))))

(defun run-test (test-name &key (dangerous-execution nil))
  "Runs the test with TEST-NAME."
  (let ((test-function (gethash test-name *tests*)))
    (if (null test-function)
        (error "The test ~A is not found.~%" test-name))
    (format t "==== ~A ====~%" test-name)
    (if dangerous-execution
        (funcall test-function)
        (if (safely-run-test-code test-function)
            (pass-test)
            (fail-test)))
    (terpri)))
