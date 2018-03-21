(defun horner (&rest numbers)
  (compute-poly (car numbers) (reverse (cdr numbers))))


(defun compute-poly (x consts)
  (cond ((null (cdr consts)) (car consts))
        (t  (+ (car consts) (* x (compute-poly x (cdr consts)))) )))

