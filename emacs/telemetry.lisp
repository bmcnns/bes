(ql:quickload :usocket)

(defparameter *control-tower-ip* "129.173.242.67"
  "The IP of the computer running the emacs control tower.")

(defun send-fitness (id fit)
  "Sends the island's fitness to the control tower."
  (let ((socket (usocket:socket-connect *control-tower-ip* 8080 :protocol :datagram))
	(payload (prin1-to-string `(:id ,id :fitness ,fit :ts ,(get-universal-time)))))
    (unwind-protect
	(usocket:socket-send socket payload (length payload))
      (usocket:socket-close socket))))

