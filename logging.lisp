(defun log-champion (champion log-file)
  (with-open-file (stream log-file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (loop repeat 100
          do (format stream "offline ~A~%" (- (bes-gym:rollout champion "LunarLander-v3" (random 100000)))))))
