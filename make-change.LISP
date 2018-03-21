(defun make-change (val &optional (coins '(25 10 5 1)))
  
  (let ((factor (if (= 0 val)
                    0
                  (truncate (float (/ val (car coins)))))))

      (cond ((null (cdr coins)) (format t "mod:~a val:~a coins:~a~%" factor val coins) factor)
            ((and (= 0 factor) (= 0 val)) (values factor (make-change val (cdr coins))))
            (t (format t "mod:~a val:~a coins:~a~%" factor val coins) (values factor (make-change (mod val (* (car coins) factor)) (cdr coins)))))))
      




(defun make-change (amt &optional (coins '(25 10 5 1)))
  (values-list (find-change amt coins)))


(defun find-change (amt coins)
  (let ((factor (if (< amt (car coins))
                    0
                  (floor amt (car coins)))))
    (cond ((null (cdr coins)) (list factor))
          (t (cons factor (find-change 
                           (- amt (* (car coins) factor)) (cdr coins)))))))
      






