(in-package :bes)

(defstruct (dataset (:constructor %make-dataset))
  ;; Array of Double-Float Arrays
  (observations (make-array 0) :type (simple-array (simple-array double-float (*)) (*)))

  ;; Array of Actions
  (actions (make-array 0) :type simple-vector)

  ;; Array of Rewards
  (rewards (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))

  ;; Array of Terminations
  (terminations (make-array 0 :element-type 'bit) :type simple-vector)

  ;; Array of Truncations
  (truncations (make-array 0 :element-type 'bit) :type simple-vector)

  ;; Metadata
  (size 0 :type fixnum))

(defun convert-list-to-dataset (raw-list)
  "Converts a raw list of transitions ((obs act rew term trunc) ..) into a dataset struct."
  (declare (optimize (speed 3) (safety 1))
           (type list raw-list))
  (let* ((count (length raw-list))
         (obs-arr (make-array count))
         (act-arr (make-array count))
         (rew-arr (make-array count :element-type 'double-float))
         (term-arr (make-array count))
         (trunc-arr (make-array count)))

    (loop for transition in raw-list
          for i fixnum from 0
          do (destructuring-bind (obs act rew term trunc) transition
               (declare (type list obs))
               ;; 1. Store observation
               (setf (aref obs-arr i)
                     (make-array (length obs)
                                 :element-type 'double-float
                                 :initial-contents (mapcar (lambda (x) (coerce x 'double-float)) obs)))

               ;; 2. Store action
               (setf (aref act-arr i) act)

               ;; 3. Store reward
               (setf (aref rew-arr i) (coerce rew 'double-float))

               ;; 4. Store flags
               (setf (aref term-arr i) term)
               (setf (aref trunc-arr i) trunc)))

    (%make-dataset :observations obs-arr
                   :actions act-arr
                   :rewards rew-arr
                   :terminations term-arr
                   :truncations trunc-arr
                   :size count)))
               

;; Simple Accessors
(defun observations (ds) (dataset-observations ds))
(defun actions (ds) (dataset-actions ds))
(defun rewards (ds) (dataset-rewards ds))
(defun terminations (ds) (dataset-terminations ds))
(defun truncations (ds) (dataset-truncations ds))

(defun batch (ds start end)
  "Returns a NEW dataset struct containing the subset."
  (declare (type dataset ds)
           (type fixnum start end))
  (%make-dataset
   :observations (subseq (dataset-observations ds) start end)
   :actions (subseq (dataset-actions ds) start end)
   :rewards (subseq (dataset-rewards ds) start end)
   :terminations (subseq (dataset-terminations ds) start end)
   :truncations (subseq (dataset-truncations ds) start end)
   :size (- end start)))

(defun sample (ds &key (n (experiment-batch-size *experiment*)))
  (let ((len (dataset-size ds)))
    (if (>= n len)
        ds
        (let ((start (random (- len n))))
          (batch ds start (+ start n))))))
                     
  
(defmacro defdataset (name &key path)
  "Define a global variable containing a dataset loaded from 'datasets/<name>'.
   Auto-converts the raw list into a high-performance Structure of Arrays."
  (let* ((dataset-name (string-trim "*" (symbol-name name)))
         (file-name (if path
                        path
                        (concatenate 'string "datasets/" dataset-name))))
    `(defparameter ,name
       (with-open-file (in ,file-name :direction :input)
         (format t "Loading and converting dataset: ~A...~%" ,file-name)
         ;; RAW LIST -> FAST STRUCT
         ;; We read the file, then immediately convert it.
         (convert-list-to-dataset (read in))))))
