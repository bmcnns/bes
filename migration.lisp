(defun make-migration-buffer ()
  "Encapsulate the migration buffer in a closure."
  (let ((queue '())
	(lock (bt:make-lock "migrant-buffer-lock")))
    (list
     ;; Thread-safe push function
     (lambda (migrant)
       (bt:with-lock-held (lock)
	 (setf queue (nconc queue (list migrant)))))
     ;; Thread-safe pop function
     (lambda ()
       (bt:with-lock-held (lock)
	 (when queue
	   (let ((migrant (car queue)))
	     (setf queue (cdr queue))
	     migrant)))))))

(defparameter *migration-buffer* (make-migration-buffer)
  "Closure for pushing and popping migrants from a buffer.")

(defun pop-migrant ()
  "Pop a migrant from the migration buffer."
  (funcall (second *migration-buffer*)))

(defun push-migrant (migrant)
  "A migrant has been received. Push it to the migration buffer."
  (funcall (first *migration-buffer*) migrant))

