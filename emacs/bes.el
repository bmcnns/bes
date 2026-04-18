(defvar bes-data (make-hash-table :test 'equal))

(define-derived-mode bes-mode tabulated-list-mode "BES-Islands"
  "Major mode for displaying Island fitness data."
  (setq tabulated-list-format [("Island" 25 t) 
                               ("Fitness" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Fitness" . t))
  (add-hook 'tabulated-list-revert-hook #'bes--refresh-data nil t)
  (tabulated-list-init-header))

(defun bes--refresh-data ()
  "Translates hash table data into tabulated-list format."
  (setq tabulated-list-entries
        (let (entries)
          (maphash (lambda (id fitness)
                     (push (list id (vector (format "%s" id) 
                                            (format "%s" fitness)))
                           entries))
                   bes-data)
          entries)))

(defun bes-telemetry-handler (_proc string)
  "Receive telemetry information from the island nodes."
  (ignore-errors
    (let* ((msg (read string))
	   (fitness (plist-get msg :FITNESS))
	   (migrant (plist-get msg :MIGRANT)))
      (cond
       (fitness
	(let ((island (plist-get msg :FROM)))
	  (puthash island fitness bes-data)))
       (migrant
	(let ((from (plist-get msg :FROM))
	      (to (plist-get msg :TO))
	      (status (plist-get msg :STATUS)))
	  (message "Migration from %s to %s: %s." from to status)))))))

(defun bes ()
  "Start BES dashboard. Use 'g' to refresh, 's' to sort."
  (interactive)
  (unless (process-live-p (get-process "bes"))
    (make-network-process
     :name "bes" :family 'ipv4 :service 8080 :type 'datagram
     :host "0.0.0.0" :server t :filter #'bes-telemetry-handler))
  
  (with-current-buffer (get-buffer-create "*bes*")
    (bes-mode)
    (revert-buffer) ; Calls bes--refresh-data via hook
    (display-buffer (current-buffer))))
