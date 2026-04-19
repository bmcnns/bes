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
  (setq tabulated-list-format [("Island" 25 t) 
                               ("Fitness" 20 t)
			       ("Generation" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Island" . t))
  (add-hook 'tabulated-list-revert-hook #'bes--refresh-data nil t)
  (tabulated-list-init-header))

(defun bes--refresh-data ()
  "Translate hash table data into tabulated-list format."
  (let (entries)
    (maphash (lambda (id row)
               (let ((fitness (car row))
                     (generation (cadr row)))
                 (push (list id (vector (format "%s" id)
                                        (format "%s" fitness)
                                        (format "%s" generation)))
                       entries)))
             bes-data)
    (setq tabulated-list-entries entries)))

(defun bes-telemetry-handler (_proc string)
  "Receive telemetry information from the island nodes."
  (ignore-errors
    (let* ((msg (read string))
	   (type (plist-get msg :TYPE)))
      (cond
       ((eq type :FITNESS)
	(let ((fitness (plist-get msg :FITNESS))
	      (island-id (plist-get msg :FROM))
	      (generation (plist-get msg :GENERATION)))
	  (puthash island-id (list fitness generation) bes-data)))
	      
       ((eq type :MIGRANT)
	(let ((from-island (plist-get msg :FROM))
	      (to-island (plist-get msg :TO))
	      (status (plist-get msg :STATUS))
	      (timestamp (plist-get msg :TS)))
	  (if (eq status :SENT)
	      (bes--log-event (format "Migrant sent from island %s to island %s."
				      from-island to-island))
	    (bes--log-event (format "Migrant received on island %s from island %s."
				    to-island from-island)))))))))
	   
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
