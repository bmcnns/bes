(defun write-log-header (experiment)
  "Print a header row to *standard-output* showing column names for objectives
   (min/avg/max). Used at the start of logging for human-readable console output."
  (let ((objectives (experiment-objectives experiment)))
    (format t "~&~A~15T" "Generation")
    (loop for objective in objectives
          for col = (format nil "~A (min/avg/max)" objective)
          do (format t "~A~45T" col))
    (terpri)
    (format t "~A~15T" "-----------")
    (loop for _ in objectives
          do (format t "--------------------------~45T"))
    (terpri)))

(defun write-log (ranked-population generation experiment)
  "Print a log entry to *standard-output* showing summary statistics (min, avg, max)
   for each objective in RANKED-POPULATION at the given GENERATION."
  (let ((objectives (experiment-objectives experiment))
        (metrics (apply #'mapcar #'list (mapcar #'cdr ranked-population))))
    (format t "~&~D~15T" generation)
    (loop for objective in objectives
          for fitness in metrics
          for avg = (/ (reduce #'+ fitness) (length fitness))
          for min = (reduce #'min fitness)
          for max = (reduce #'max fitness)
          do (format t "~,3f / ~,3f / ~,3f~45T" min avg max))
    (terpri)))

(defun write-results-file-header (experiment)
  (let ((objectives (experiment-objectives experiment)))
    (with-open-file (stream "results.csv"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "generation")
      (dolist (objective objectives)
        (format stream ",MIN_~A" objective)
        (format stream ",AVG_~A" objective)
        (format stream ",MAX_~A" objective))
      (terpri stream))))

(defun write-to-results-file (ranked-population experiment generation)
  (let* ((objectives (experiment-objectives experiment))
         (metrics (apply #'mapcar #'list (mapcar #'cdr ranked-population))))
    (with-open-file (stream "results.csv"
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "~A" generation)
      (loop for values in metrics
            do (let ((min (reduce #'min values))
                     (avg (/ (reduce #'+ values) (length values)))
                     (max (reduce #'max values)))
                 (format stream ",~F,~F,~F" min avg max)))
      (terpri stream))))

(defun report (ranked-population experiment generation)
  (if (= generation 1)
      (progn
        (write-log-header experiment)
        (write-results-file-header experiment)))
  (write-log ranked-population generation experiment)
  (write-to-results-file ranked-population experiment generation))
