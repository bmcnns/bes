(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defparameter *telemetry-ip* "129.173.22.24"
  "IP address of the emacs client receiving telemetry.")

(defparameter *islands*
  '(("10.100.202.42" . 1) ;; ds-cmlm-02
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
			 
(defun emit-fitness-scores (island-id fitness)
  "Sends the island's fitness score to the telemetry client."
  (let ((payload (prin1-to-string
		  `(:id ,island-id :fitness ,fitness :ts ,(get-universal-time)))))
    (unwind-protect
	 (notify-telemetry payload))))
  

(defun start-island-server ()
  "The main entry point to starting the island server.
   This will send UDP packets to the emacs client for telemetry.
   This will send and receive TCP packets to other islands for migration.
   This will receive TCP packets from the emacs client to start evolution."
  (let* ((ip-address (get-local-ip))
	 (island-id (lookup-island-id-by-ip ip-address)))
    (format t "Server started on ~A:8080.~%" ip-address)
    (format t "This is island ~A.~%" island-id))
  
  ;; Start the telemetry socket
  (bt:make-thread
   (lambda ()
     (loop
       do (emit-fitness-scores 1 (random 42.0))
       do (sleep (+ 1 (random 4)))))))
	  
(defun lookup-island-id-by-ip (ipaddr)
  "Look up the ID of the island by an IP address."
  (car (assoc ipaddr *islands* :test #'string=)))

(defun lookup-island-ip-by-id (id)
  "Look up the IP address of the island by its ID."
  (car (rassoc id *islands* :test #'equal)))

(defun get-local-ip ()
  "By getting our local IP, we know who we are and who our adjacent neighbours are."
  (let ((socket (usocket:socket-connect "8.8.8.8" 53 :protocol :datagram)))
    (unwind-protect
	 (format nil "~{~A~^.~}" (coerce (usocket:get-local-address socket) 'list))
      (usocket:socket-close socket))))

(defun get-neighbours (island-id)
  "Returns the island IDs that this island ID is connected to."
  (cdr (assoc island-id *topology* :test #'equal)))
  






  
