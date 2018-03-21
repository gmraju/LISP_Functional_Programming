(defun max-min (vect &key (start 0) (end (length vect)))  
  (if (eql start end)
      (values nil nil)
    (let ((cur-val (svref vect start)))    
      (if (eql start (1- end)) 
          (values cur-val cur-val)
        (calc-max-min 
         cur-val
         (multiple-value-bind (max min) 
             (max-min vect :start (1+ start) :end end) 
           (list max min)))))))


(defun calc-max-min (val maxmin)
  (values 
   (max val (first maxmin)) 
   (min val (second maxmin))))


#|
(defun calc-max-min (val maxmin)
  (let ((max (first maxmin)) (min (second maxmin)))
    (cond ((> val max) (values val min))
          ((< val min) (values max val))
          (t (values max min)))))
|#