(in-package :bes)


(defun make-score-logger (file-name)
  "Return a closure that logs SCORES to FILE-NAME."
  (let ((output (open file-name
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create))
        (header-written nil))
    (lambda (generation scores)
      ;; If the file is empty, write the header.
      (unless header-written
        (let ((objectives (mapcar #'car (cadar scores))))
          (format output "# generation id ~{~A~^ ~}~%" objectives))
        (setf header-written t))
      (dolist (score scores)
        (destructuring-bind (id . a-list) score
          (format output "~A ~A" generation id)
          (dolist (pair (car a-list))
            (format output " ~,6F" (cdr pair)))
          (terpri output)))
      (finish-output output))))

(defun timestamp ()
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun make-aggregate-score-logger (file-name)
  "Append one line per GENERATION: for each objective => min mean max."
  (let ((output (open file-name
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create))
        (header-written nil))
    (lambda (generation scores)
      ;; header from the alist of the first score
      (unless header-written
        (let* ((objectives (mapcar #'car (cadar scores)))
               (header (loop for obj in objectives append
                             (list (format nil "~A_min" obj)
                                   (format nil "~A_mean" obj)
                                   (format nil "~A_max" obj)))))
          (format output "ts generation ~{~A~^ ~}~%" header)
          (setf header-written t)))

      (let ((objectives (mapcar #'car (cadar scores))))
        (format output "~A " (timestamp))
        (format output "~A " generation)
        (dolist (obj objectives)
          (let* ((vals (mapcar (lambda (s)
                                 ;; s = (id . (ALIST)), so the ALIST is (cadar s)
                                 (cdr (assoc obj (cadar (list s))))) ; safe, same as (cdr (assoc obj (car (cdr s))))
                               scores))
                 (mn  (reduce #'min vals))
                 (mx  (reduce #'max vals))
                 (avg (/ (reduce #'+ vals) (length vals))))
            (format output "~,6F ~,6F ~,6F" mn avg mx))))
      (terpri output)
      (finish-output output))))

(defun make-aggregate-score-and-data-logger (file-name)
  "Append one line per GENERATION: for each objective => min mean max."
  (let ((output (open file-name
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create))
        (header-written nil))
    (lambda (generation scores num-datapoints-so-far)
      ;; header from the alist of the first score
      (unless header-written
        (let* ((objectives (mapcar #'car (cadar scores)))
               (header (loop for obj in objectives append
                             (list (format nil "~A_min" obj)
                                   (format nil "~A_mean" obj)
                                   (format nil "~A_max" obj)))))
          (format output "ts generation datapoints ~{~A~^ ~}~%" header)
          (setf header-written t)))

      (let ((objectives (mapcar #'car (cadar scores))))
        (format output "~A " (timestamp))
        (format output "~A " generation)
        (format output "~A " num-datapoints-so-far)
        (dolist (obj objectives)
          (let* ((vals (mapcar (lambda (s)
                                 ;; s = (id . (ALIST)), so the ALIST is (cadar s)
                                 (cdr (assoc obj (cadar (list s))))) ; safe, same as (cdr (assoc obj (car (cdr s))))
                               scores))
                 (mn  (reduce #'min vals))
                 (mx  (reduce #'max vals))
                 (avg (/ (reduce #'+ vals) (length vals))))
            (format output "~,6F ~,6F ~,6F" mn avg mx))))
      (terpri output)
      (finish-output output))))
