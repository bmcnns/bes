(in-package :bes)

(defparameter *telemetry-ip* "129.173.22.24"
  "IP address of the emacs client receiving telemetry.")

(defparameter *heartbeat-interval* 30
  "The amount of time to wait between sending heartbeats (in seconds).")

(defparameter *islands*
  '(("129.173.22.24" . 0) ;; ds-login2
    ("10.100.202.42" . 1) ;; ds-cmlm-02
    ("10.100.202.43" . 2) ;; ds-cmlm-03
    ("10.100.202.44" . 3) ;; ds-cmlm-04
    ("10.100.202.46" . 4) ;; ds-cmlm-06
    ("10.100.202.47" . 5) ;; ds-cmlm-07
    ("10.100.202.48" . 6) ;; ds-cmlm-08
    ("10.100.202.49" . 7) ;; ds-cmlm-09
    ("10.100.202.51" . 8) ;; ds-cmlm-11
    ("10.100.202.52" . 9) ;; ds-cmlm-12
    ("10.100.202.54" . 10) ;; ds-cmlm-14
    ("10.100.202.55" . 11) ;; ds-cmlm-15
    ("10.100.202.56" . 12) ;; ds-cmlm-16
    ("10.100.202.57" . 13) ;; ds-cmlm-17
    ("10.100.202.58" . 14) ;; ds-cmlm-18
    ("10.100.202.59" . 15)) ;; ds-cmlm-19
  "The mapping between IP address and their island ID.")

(defparameter *topology*
  '((1 . (2 6))
    (2 . (1 3 7))
    (3 . (2 4 8))
    (4 . (3 5 9))
    (5 . (4 10))
    (6 . (1 7 11))
    (7 . (2 6 8 12))
    (8 . (3 7 9 13))
    (9 . (4 8 10 14))
    (10 . (5 9 15))
    (11 . (6 12))
    (12 . (7 11 13))
    (13 . (12 8 14))
    (14 . (9 13 15))
    (15 . (10 14)))
  "Currently this represents a grid topology with up-to 4 adjacent neighbours.
   Another topology of interest might be the torus where all islands have 4 adjacent
  neighbours.")

(defun notify-telemetry (msg)
  "Fire and forget a message over UDP to the telemetry client."
  (let ((socket (usocket:socket-connect *telemetry-ip* 8080 :protocol :datagram)))
    (unwind-protect
	 (usocket:socket-send socket msg (length msg))
      (usocket:socket-close socket))))
			 
(defun emit-fitness-scores (island-id fitness generation)
  "Sends the island's fitness score to the telemetry client."
  (let ((payload (prin1-to-string
		  `(:type :fitness
		    :fitness ,fitness
		    :from ,island-id
		    :generation ,generation
		    :ts ,(get-universal-time)))))
    (notify-telemetry payload)))

(defun get-cpu-usage (&optional (interval 1))
  "Returns a single float representing the total CPU utilization (0.0 to 100.0)."
  (flet ((get-raw-cpu ()
	   (with-open-file (s "/proc/stat")
	     (let* ((line (read-line s))
		    (parts (remove-if (lambda (x) (string= x ""))
				      (uiop:split-string line)))
		    (stats (mapcar #'parse-integer (rest parts))))
	       (values (reduce #'+ stats) (fourth stats))))))
    (multiple-value-bind (total1 idle1) (get-raw-cpu)
      (sleep interval)
      (multiple-value-bind (total2 idle2) (get-raw-cpu)
	(let ((total-delta (- total2 total1))
	      (idle-delta (- idle2 idle1)))
	  (if (zerop total-delta)
	      0.0
	      (* 100.0 (- 1 (/ idle-delta total-delta)))))))))
  
(defun get-memory-usage ()
  "Returns the percentage of RAM currently in use (0.0 to 100.0)."
  (with-open-file (s "/proc/meminfo")
    (let (total free buffers cached)
      (loop for line = (read-line s nil)
	    while (and line (or (not total) (not free) (not buffers) (not cached)))
	    do (let* ((parts (remove-if (lambda (x) (string= x ""))
					(uiop:split-string line)))
		      (key (car parts))
		      (val (parse-integer (second parts))))
		 (cond
		   ((string= key "MemTotal:") (setf total val))
		   ((string= key "MemFree:") (setf free val))
		   ((string= key "Buffers:") (setf buffers val))
		   ((string= key "Cached:") (setf cached val)))))
      (if (and total free buffers cached)
	  (let ((used (- total (+ free buffers cached))))
	    (* 100.0 (/ used (float total))))
	  0.0))))

(defun emit-heartbeat ()
  "On a regular interval *heartbeat-interval*, send a heartbeat message
   to the telemetry client and additionally send CPU and memory usage."
  (let* ((ip-address (get-local-ip))
	 (island-id (lookup-island-id-by-ip ip-address))
	 (payload (prin1-to-string
		  `(:type :heartbeat
		    :from ,island-id
		    :ts ,(get-universal-time)
		    :cpu ,(get-cpu-usage)
		    :memory ,(get-memory-usage)))))
    (notify-telemetry payload)))
			  
(defun get-local-ip ()
  "By getting our local IP, we know who we are and who our adjacent neighbours are."
  (let ((socket (usocket:socket-connect "8.8.8.8" 53 :protocol :datagram)))
    (unwind-protect
	 (format nil "~{~A~^.~}" (coerce (usocket:get-local-address socket) 'list))
      (usocket:socket-close socket))))

(defun lookup-island-id-by-ip (ipaddr)
  "Look up the ID of the island by an IP address."
  (cdr (assoc ipaddr *islands* :test #'string=)))
	  
(defun lookup-island-ip-by-id (id)
  "Look up the IP address of the island by its ID."
  (car (rassoc id *islands* :test #'equal)))

(defun emit-error (message)
  "An error has occurred, notify the telemetry client."
  (let* ((ip (get-local-ip))
	 (island-id (lookup-island-id-by-ip ip))
	 (payload (prin1-to-string
		   `(:type :error
		     :from ,island-id
		     :ts ,(get-universal-time)
		     :msg ,message))))
    (notify-telemetry payload)))

(defun emit-message (message)
  "Sends a message to the telemetry client log."
  (let* ((ip (get-local-ip))
	 (island-id (lookup-island-id-by-ip ip))
	 (payload (prin1-to-string
		   `(:type :message
		     :from ,island-id
		     :ts ,(get-universal-time)
		     :msg ,message))))
    (notify-telemetry payload)))

(defun send-migrant (from-id to-id team)
  "Sends a TEAM from island FROM-ID to island TO-ID through a TCP socket.
   Notifies the telemetry client that a migrant was sent over UDP."
  (let ((receiver-ip-address (lookup-island-ip-by-id to-id)))
    ;; Send the migrant to the receiving island over TCP.
    (usocket:with-client-socket (socket stream receiver-ip-address 8080)
      (prin1 `(:type :migrant
	       :from ,from-id
	       :ts ,(get-universal-time)
	       :team ,team)
	     stream)
      (finish-output stream))
    ;; Notify the telemetry client that a migrant was sent over UDP.
    (emit-message (format nil "Migrant was sent to island ~A.~%" to-id))))

(defun get-neighbour-ids (island-id)
  "Returns the island IDs that this island ID is connected to."
  (cdr (assoc island-id *topology* :test #'equal)))

(defun random-choice (seq)
  "Returns a random element from the sequence SEQ."
  (elt seq (random (length seq))))

(defun handle-migrant-received (msg)
  "When a migrant is received through TCP, push it to the migration buffer.
   Then send a migrant received message to the telemetry client."
  (let ((from (getf msg :from))
	(team (getf msg :team)))
    (emit-message (format nil "Migrant was received from island ~A.~%" from))
    (push-migrant team)))

(defun set-global-parameters (num-observations num-actions
			      p-add p-del p-mut p-act p-swap
			      gap init-program-size max-program-size
			      p-add-instr p-del-instr p-swap-instrs
			      p-mut-constant)
  "Set the hyperparameters according to the TCP request."
  (setf *num-observations* num-observations)
  (setf *num-actions* num-actions)
  (setf *p-add* p-add)
  (setf *p-del* p-del)
  (setf *p-mut* p-mut)
  (setf *p-act* p-act)
  (setf *p-swap* p-swap)
  (setf *gap* gap)
  (setf *init-program-size* init-program-size)
  (setf *max-program-size* max-program-size)
  (setf *p-add-instr* p-add-instr)
  (setf *p-del-instr* p-del-instr)
  (setf *p-swap-instrs* p-swap-instrs)
  (setf *p-mut-constant* p-mut-constant))

(defun valid-search-parameters-p (mode gym-environment-name dataset-name
				  num-observations num-actions
				  population-size init-number-of-learners
				  max-number-of-learners p-add
				  p-del p-mut p-act p-swap
				  gap init-program-size max-program-size
				  p-add-instr p-del-instr p-swap-instrs
				  p-mut-constant seed)
  "Returns T if the search parameters are valid. NIL otherwise."
       ;; 1. Check that mode is either :online or :offline
  (and (or (eq mode :online)
	   (eq mode :offline))
       ;; 2. Check that if mode is online then a gym environment is set and a dataset is not provided.
       (case mode
	 (:online (and gym-environment-name
		       (not (eq gym-environment-name :none))
		       (eq dataset-name :none)))
	 (:offline (and dataset-name
			(not (eq dataset-name :none))
			(eq gym-environment-name :none))))))

(defun who-am-i ()
  "Returns the island ID of the currently running server."
  (let ((ip-address (get-local-ip)))
    (lookup-island-id-by-ip ip-address)))

(defun handle-start-search (msg)
  "When a search is started through TCP, validate that the search parameters
   are valid and then begin the search. This will also send a search started
   message to the telemetry client."
  (let ((mode (getf msg :mode))
	(gym-environment-name (getf msg :gym-environment-name))
	(dataset-name (getf msg :dataset-name))
	(num-observations (getf msg :num-observations))
	(num-actions (getf msg :num-actions))
	(population-size (getf msg :population-size))
	(init-number-of-learners (getf msg :init-number-of-learners))
	(max-number-of-learners (getf msg :max-number-of-learners))
	(p-add (getf msg :p-add))
	(p-del (getf msg :p-del))
	(p-mut (getf msg :p-mut))
	(p-act (getf msg :p-act))
	(p-swap (getf msg :p-swap))
	(gap (getf msg :gap))
	(init-program-size (getf msg :init-program-size))
	(max-program-size (getf msg :max-program-size))
	(p-add-instr (getf msg :p-add-instr))
	(p-del-instr (getf msg :p-del-instr))
	(p-swap-instrs (getf msg :p-swap-instrs))
	(p-mut-constant (getf msg :p-mut-constant 0.5))
	(seed (getf msg :seed)))
    (format t "~S~%" msg)
    (format t "num-actions: ~A~%" num-actions)
    (if (valid-search-parameters-p mode gym-environment-name dataset-name
				   num-observations num-actions population-size
				   init-number-of-learners max-number-of-learners
				   p-add p-del p-mut p-act p-swap gap
				   init-program-size max-program-size
				   p-add-instr p-del-instr p-swap-instrs
				   p-mut-constant seed)
	(progn
	  (set-global-parameters
	                         num-observations num-actions
	                         p-add p-del p-mut p-act
				 p-swap gap init-program-size
				 max-program-size p-add-instr
				 p-del-instr p-swap-instrs
				 p-mut-constant)
	  (emit-message (format nil "Search started on island ~A~%" (who-am-i)))
	  (run-search mode gym-environment-name dataset-name seed))
	(emit-error "The search parameters provided are invalid."))))
	  
				   
(defun start-server ()
  "The main entry point to starting the island server.
   This will send UDP packets to the emacs client for telemetry.
   This will send and receive TCP packets to other islands for migration.
   This will receive TCP packets from the emacs client to start evolution."
  (let* ((ip-address (get-local-ip))
	 (island-id (lookup-island-id-by-ip ip-address))
	 (neighbour-ids (get-neighbour-ids island-id)))
    (format t "Server started on ~A:8080.~%" ip-address)
    (format t "This is island ~A.~%" island-id)
    (when neighbour-ids
      (format t "My neighbours are: ~{~A~^, ~}~%" neighbour-ids) 
      (format t "Their IPs are: ~{~A~^, ~}~%" (mapcar #'lookup-island-ip-by-id neighbour-ids)))

    ;; Start the telemetry socket over UDP
    (bt:make-thread
     (lambda ()
       (loop for generation from 0
	     do (emit-fitness-scores island-id (random 42.0) generation)
	     ;; do (when (and neighbour-ids (> generation 10))
	     ;; 	  (send-migrant island-id (random-choice neighbour-ids) 'TEAM-XX))
	     do (sleep (+ 1 (random 4))))))

    ;; Start the heartbeat thread over UDP
    (bt:make-thread
     (lambda ()
       (loop
	 do (emit-heartbeat)
	 do (sleep *heartbeat-interval*))))

    ;; Start listening for commands over TCP
    (bt:make-thread
     (lambda ()
       (usocket:with-server-socket (server (usocket:socket-listen "0.0.0.0" 8080))
	 (loop
	   (usocket:with-connected-socket (client (usocket:socket-accept server))
	     (let* ((stream (usocket:socket-stream client))
		    (msg (read stream nil :eof)))
	       (unless (eq msg :eof)
		 ;; Dispatch according to the request received
		 (case (getf msg :type)
		   (:migrant (handle-migrant-received msg))
		   (:start-search (handle-start-search msg))))))))))))
