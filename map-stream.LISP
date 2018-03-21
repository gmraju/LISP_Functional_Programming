(defun map-stream (func stream)
  (let ((eof (list 'eof)))
    (do ((exp (read stream nil eof)
              (read stream nil eof)))
        ((eql exp eof) nil)
      (funcall func exp))))
    

(defun map-file (func pathname)
  (with-open-file (stream pathname :direction :input)
    (map-stream func stream))




(defun map-file (func pathname)
  (with-open-file (stream pathname :direction :input)
    (do* ((eof (list 'eof))
          (exp (read stream nil eof) 
               (read stream nil eof)))
         ((eql exp eof) nil)
      (funcall func exp))))

