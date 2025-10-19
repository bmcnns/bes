(deftest check-program-complexity-measures-length
  (let ((program-1 '(PROGRAM P1 ((R1 SIN OBS1))))
        (program-2 '(PROGRAM P2 ((R1 SIN OBS1)
                                 (R1 SIN OBS2)
                                 (R1 SIN OBS3)
                                 (R1 SIN OBS4)))))
    (check
     (assert-equal (program-complexity program-1) 1)
     (assert-equal (program-complexity program-2) 4))))
                                 
                 
