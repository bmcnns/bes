;; -*- lexical-binding: t; -*-
(require 'transient)

(defvar *islands*
  '(("127.0.0.1" . 0) ;; ds-login2
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

(defun plist-to-cl-sexp (plist)
  "Format a plist as a Common Lisp readable s-expression with keywords."
  (concat "("
          (mapconcat
           (lambda (pair)
             (let ((key (car pair))
                   (val (cdr pair)))
               (format ":%s %s"
                       (substring (symbol-name key) 1)  ; strip the leading ':'
                       (cond
                        ((stringp val) (format "\"%s\"" val))
                        ((symbolp val) (format ":%s" (symbol-name val)))
                        ((numberp val) (format "%s" val))
                        (t (format "%S" val))))))
           (cl-loop for (k v . _rest) on plist by #'cddr
                    collect (cons k v))
           " ")
          ")"))

(transient-define-suffix start-search ()
  (interactive)
  (let* ((args (transient-args 'start-search-menu))
	 (island-id (string-to-number (transient-arg-value "--island=" args)))
	 (ip-address (lookup-ip-by-island-id island-id))
	 (payload (make-payload-from-transient-args args)))
    (message "[LOCAL] Sending start-search request to island %s at IP address %s." island-id ip-address)
    (message "%s" payload)
    (let ((proc (make-network-process
		 :name "start-search-tcp-client"
		 :host ip-address
		 :service 8080
		 :family 'ipv4
		 :nowait nil
		 :sentinel (lambda (proc event)
			     (message "TCP Event: %s" event)))))
      (process-send-string proc (concat (plist-to-cl-sexp payload) "\n"))
      (process-send-eof proc)
      proc)))

(defun lookup-ip-by-island-id (id)
  "Given an island ID, look up the island's IP address."
  (car (rassoc id *islands*)))

(defun string-to-number-or-inf (string)
  "Returns a string as a number. If 'inf' is provided then :inf is returned."
  (if (string= string "inf")
      :inf
    (string-to-number string)))

(defun make-payload-from-transient-args (args)
  "Given a list of transient args, construct the TCP payload for starting searches."
  (let ((mode (pcase (transient-arg-value "--mode=" args)
		("online" :online)
		("offline" :offline)
		(other (error "Invalid mode: %S" other))))
	(gym-environment-name  (pcase (transient-arg-value "*env=" args)
				 ("none" :none)
				 (other other)))
	(dataset-name (pcase (transient-arg-value "*dataset=" args)
			("none" :none)
			(other other)))
	(num-observations (string-to-number
			   (transient-arg-value "*num-observations=" args)))
	(num-actions (string-to-number
		      (transient-arg-value "*num-actions=" args)))
        (population-size (string-to-number
	 		  (transient-arg-value "*population-size=" args)))
        (init-number-of-learners (string-to-number
	 			  (transient-arg-value "*init-number-of-learners=" args)))
        (max-number-of-learners (string-to-number-or-inf
	 			 (transient-arg-value "*max-number-of-learners=" args)))
	(p-add (string-to-number
		(transient-arg-value "*p-add=" args)))
	(p-del (string-to-number
	 	(transient-arg-value "*p-del=" args)))
	(p-mut (string-to-number
		(transient-arg-value "*p-mut=" args)))
	(p-act (string-to-number
		(transient-arg-value "*p-act=" args)))
	(p-swap (string-to-number
	 	 (transient-arg-value "*p-swap=" args)))
	(gap (string-to-number
	      (transient-arg-value "*gap=" args)))
        (init-program-size (string-to-number
	 		    (transient-arg-value "*init-program-size=" args)))
	(max-program-size (string-to-number-or-inf
	 		   (transient-arg-value "*max-program-size=" args)))
	(p-add-instr (string-to-number
		      (transient-arg-value "*p-add-instr=" args)))
	(p-del-instr (string-to-number
		      (transient-arg-value "*p-del-instr=" args)))
	(p-swap-instrs (string-to-number
			(transient-arg-value "*p-swap-instrs=" args)))
	(p-mut-constant (string-to-number
			 (transient-arg-value "*p-mut-constant=" args)))
	(seed (let ((seed-arg (transient-arg-value "*seed=" args)))
		(cond
		 ((string= seed-arg "random")
		  :random)
		 ((stringp seed-arg)
		  (string-to-number seed-arg))))))
    `(:type :start-search
      :mode ,mode
      :gym-environment-name ,gym-environment-name
      :dataset-name ,dataset-name
      :num-observations ,num-observations
      :num-actions ,num-actions
      :population-size ,population-size
      :init-number-of-learners ,init-number-of-learners
      :max-number-of-learners ,max-number-of-learners
      :p-add ,p-add
      :p-del ,p-del
      :p-mut ,p-mut
      :p-act ,p-act
      :p-swap ,p-swap
      :gap ,gap
      :init-program-size ,init-program-size
      :max-program-size ,max-program-size
      :p-add-instr ,p-add-instr
      :p-del-instr ,p-del-instr
      :p-swap-instrs ,p-swap-instrs
      :p-mut-constant ,p-mut-constant
      :seed ,seed)))

(transient-define-suffix start-search ()
  (interactive)
  (let* ((args (transient-args 'start-search-menu))
	 (island-id (string-to-number (transient-arg-value "--island=" args)))
	 (ip-address (lookup-ip-by-island-id island-id))
	 (payload (make-payload-from-transient-args args)))
    (message "[LOCAL] Sending start-search request to island %s at IP address %s." island-id ip-address)
    (message "%s" payload)
    (let ((proc (make-network-process
		 :name "start-search-tcp-client"
		 :host ip-address
		 :service 8080
		 :family 'ipv4
		 :nowait nil
		 :sentinel (lambda (proc event)
			     (message "TCP Event: %s" event)))))
      (process-send-string proc (format "%S\n" payload))
      (process-send-eof proc)
      proc)))

(transient-define-prefix start-search-menu ()
  "Menu for configuring TPG hyperparameters before starting a run."
  :refresh-suffixes t
  ;; These are the default values for the entire prefix
  :value '("*p-add=0.2" 
           "*p-del=0.1"
           "*p-mut=0.5" 
           "*p-act=0.2"
	   "*p-swap=0.1"
           "*init-program-size=100"
           "*max-program-size=1000"
           "*gap=0.5"
           "*p-add-instr=0.9"
           "*p-del-instr=0.5"
           "*p-swap-instrs=1.0"
	   "*p-mut-constant=0.5"
	   "*env=none"
	   "*dataset=none"
	   "*population-size=160"
	   "*init-number-of-learners=3"
	   "*max-number-of-learners=inf"
	   "*seed=random")
  ["Island"
    ("-I" "Island" "--island="
    :choices ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"))]
  ["Evaluation"
   ("-M" "Evaluation Mode" "--mode="
    :choices ("online" "offline"))
   ("-G" "Gymnasium Environment Name" "*env="
    :choices ("none" "Hopper-v5" "Walker2d-v5" "HalfCheetah-v5"))
   ("-F" "Dataset Name" "*dataset=")]
  ["Key Settings"
   ("-Z" "Number of Observations" "*num-observations=")
   ("-X" "Number of Actions" "*num-actions=")
   ("-P" "Population Size" "*population-size=")
   ("-g" "Gap" "*gap=")
   ("-s" "Seed" "*seed=")]
  [:description "Team Constraints"
   ("-l" "Initial Number of Learners" "*init-number-of-learners=")
   ("-L" "Maximum Number of Learners" "*max-number-of-learners=")]
  [:description "Team Mutation Probabilities"
   ("-a" "Add Learner (p)"      "*p-add=")
   ("-d" "Delete Learner (p)"   "*p-del=")
   ("-m" "Mutate Program (p)"   "*p-mut=")
   ("-c" "Change Action (p)"    "*p-act=")
   ("-w" "Swap Learner Actions (p)" "*p-swap=")]

  [:description "Program Constraints"
   ("-i" "Initial Size"         "*init-program-size=")
   ("-x" "Maximum Size"         "*max-program-size=")]

  [:description "Program Mutation Probabilities"
   ("-A" "Add Instruction (p)"  "*p-add-instr=")
   ("-D" "Delete Instruction (p)" "*p-del-instr=")
   ("-S" "Swap Instructions (p)" "*p-swap-instrs=")
   ("-C" "Mutate Constant (p)" "*p-mut-constant=")]

  ["Actions"
   ("S" "START Search" start-search)
   ("q" "Back to Main" bes-menu)])

(transient-define-prefix bes-menu ()
  "Control Center for BES."
  ["Dashboard Controls"
   ("r" "Start Auto-Refresh" bes-start-auto-refresh)
   ("s" "Stop Auto-Refresh" bes-stop-auto-refresh)
   ("g" "Force Refresh" revert-buffer)]

  ["Runs"
   ("S" "Configure & START" start-search-menu)]

  ["Navigation"
   ("q" "Quit Menu" transient-quit-one)])

(defvar bes-data (make-hash-table :test 'equal))

(defvar bes-refresh-timer nil
  "Timer for auto-refreshing the BES dashboard.")

(defun bes-start-auto-refresh ()
  "Start a timer to refresh the *bes* buffer every 2 seconds."
  (interactive)
  ;; Cancel existing timer if it exists
  (when bes-refresh-timer
    (cancel-timer bes-refresh-timer))

  (setq bes-refresh-timer
	(run-at-time 0 1
		     (lambda ()
		       (let ((buf (get-buffer "*bes*")))
			 (when (buffer-live-p buf)
			   (with-current-buffer buf
			     (revert-buffer t t))))))))

(defun bes-stop-auto-refresh ()
  "Stop the BES refresh timer."
  (interactive)
  (cancel-timer bes-refresh-timer)
  (setq bes-refresh-timer nil))

(define-derived-mode bes-mode tabulated-list-mode "BES-Islands"
  "Major mode for displaying Island fitness data."
  (setq tabulated-list-format [("Island" 15 t) 
                               ("Fitness" 15 t)
			       ("Generation" 15 t)
			       ("CPU" 15 t)
			       ("Mem" 15 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'bes--refresh-data nil t)
  (tabulated-list-init-header))

(defun bes--refresh-data ()
  "Translate hash table data into tabulated-list format."
  (interactive)
  (let (entries)
    (maphash (lambda (id data)
	       (let ((fitness (plist-get data :fitness))
		     (gen (plist-get data :generation))
		     (cpu (plist-get data :cpu))
		     (memory (plist-get data :memory)))
		 (push (list id (vector
				 (format "%s" id)
				 (format "%s" (or fitness "-"))
				 (format "%s" (or gen "-"))
				 (format "%.1f" (or cpu 0.0))
				 (format "%.1f" (or memory 0.0))))
		       entries)))
	     bes-data)
    (setq tabulated-list-entries entries)))

(defun bes-telemetry-handler (_proc string)
  "Receive telemetry information from the island nodes."
  (ignore-errors
    (let* ((msg (read string))
	   (type (plist-get msg :TYPE))
	   (from-id (plist-get msg :FROM))
	   ;; Get existing data for this island, or an empty list if new.
	   (current-data (gethash from-id bes-data '())))
      (cond
       ((eq type :FITNESS)
	(let ((fitness (plist-get msg :FITNESS))
	      (generation (plist-get msg :GENERATION)))
	  (puthash from-id
		   (plist-put (plist-put current-data :fitness fitness) :generation generation)
		   bes-data)))

       ((eq type :MESSAGE)
	(let ((message (plist-get msg :MSG)))
	  (bes--log-event (format "[ISLAND %s] Message: %s"
				  from-id message))))
       ((eq type :HEARTBEAT)
	(let ((cpu (plist-get msg :CPU))
	      (memory (plist-get msg :MEMORY)))
	  (puthash from-id
		   (plist-put (plist-put current-data :cpu cpu) :memory memory)
		   bes-data)
	  (bes--log-event (format "[ISLAND %s] Heartbeat received."
				  from-id cpu memory))))
       ((eq type :ERROR)
	(let ((error-message (plist-get msg :MSG)))
	  (bes--log-event (propertize (format "ISLAND %s ENCOUNTERED AN ERROR: %s"
					      from-id error-message)
				      'font-lock-face 'error))))))))
	   
(defun bes--log-event (message)
  "Appends MESSAGE to the *bes-log* buffer and caps it at 1000 lines."
  (let ((log-buf (get-buffer-create "*bes-log*"))
        (max-lines 1000))
    (with-current-buffer log-buf
      (let ((inhibit-read-only t))
        (save-excursion
          ;; 1. Move to the end and insert the new log entry
          (goto-char (point-max))
          (insert (format "[%s] %s\n"
                          (format-time-string "%H:%M:%S")
                          message))
          
          ;; 2. Check line count and trim if necessary
          (let ((line-count (count-lines (point-min) (point-max))))
            (when (> line-count max-lines)
              (goto-char (point-min))
              ;; Calculate how many lines to delete to get back to max-lines
              (forward-line (- line-count max-lines))
              (delete-region (point-min) (point)))))))))

(defun bes ()
  "Start BES dashboard and log."
  (interactive)
  ;; Ensure server is running
  (unless (process-live-p (get-process "bes"))
    (make-network-process
     :name "bes" :family 'ipv4 :service 8080 :type 'datagram
     :host "0.0.0.0" :server t :filter #'bes-telemetry-handler))
  
  ;; Setup Dashboard
  (let ((db-buf (get-buffer-create "*bes*"))
        (log-buf (get-buffer-create "*bes-log*")))
    
    (with-current-buffer db-buf
      (bes-mode)
      (revert-buffer))

    ;; Display logic: Dashboard on top, Log on bottom
    (delete-other-windows)
    (switch-to-buffer db-buf)
    (set-window-buffer (split-window-vertically -30) log-buf)))

;; Bind the transient menu to C-c C-c
(define-key bes-mode-map (kbd "C-c C-c") 'bes-menu)

