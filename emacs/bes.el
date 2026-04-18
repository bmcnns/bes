(require 'vtable)

(defvar bes-data (make-hash-table :test 'equal))

(defun bes-telemetry-handler (_proc string)
  "Receives (:id :fitness :ts (timestamp)) from the islands."
  (let ((msg (read string)))
    (puthash (plist-get msg :ID)
	     (plist-get msg :FITNESS) bes-data)
    (with-current-buffer (get-buffer-create "*bes*")
      (let ((inhibit-read-only t)) (erase-buffer)
	   (make-vtable
	    :columns '((:name "Island" :width 20) (:name "Fitness" :width 20))
	    :objects (hash-table-keys bes-data)
	    :getter (lambda (id col _)
		      (if (= col 0) (format "%s" id)
			(format "%s" (gethash id bes-data)))))))))

(defun bes ()
  (interactive)
  (unless (process-live-p (get-process "bes"))
    (make-network-process
     :name "bes"
     :buffer "*bes*"
     :family 'ipv4
     :service 8080
     :type 'datagram
     :host "0.0.0.0"
     :server t
     :filter #'bes-telemetry-handler))
  (switch-to-buffer (get-buffer-create "*bes*")))
