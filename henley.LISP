(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (read-stream s (make-see))
    (hash-table-count *words*)))



(defun read-stream (stream fn)
  (let ((buffer (make-string maxword))
        (pos 0))
    (do ((c (read-char stream nil :eof) 
            (read-char stream nil :eof)))
        ((and (eql c :eof) (zerop pos)) t)  
      (if (and (not (eql c :eof)) 
               (or (alpha-char-p c) (char= c #\')))
          (progn
            (setf (aref buffer pos) c)
            (incf pos))
        (progn
          (unless (zerop pos)
            (funcall 
             fn (intern (string-downcase 
                         (subseq buffer 0 pos))))
            (setf pos 0))
          (let ((p (punc c)))
            (if p (funcall fn p))))))))




(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) ))



(defun make-see ()
  (let ((prev `|.|))
    #'(lambda (symb)   
        (let ((pair (assoc symb (gethash prev *words*))))
          (if (null pair)
              (push (cons symb 1) (gethash prev *words*))
            (incf (cdr pair))))
        (setf prev symb))))



(defun generate-text (n)
  (do ((next (random-next '|.|) (random-next next))
       (num n (1- num)))
      ((or (format t "~a " next) (zerop num)) (terpri))))
    


(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))


(defun make-valid-text-p ()
  (let ((prev `|.|))
    #'(lambda (word) 
        (unless (assoc word (gethash prev *words*))          
          (throw 'not-valid nil))
        (setf prev word))))

 
(defun henley-p (str)
  (catch 'not-valid
    (with-input-from-string (st str)
      (read-stream st (make-valid-text-p)))))

                            
                         





#|

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))


(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))
|#





#|

(defun make-check-valid-word ()
  (let ((prev `|.|))
    #'(lambda (word) 
        (format t "~a~%" (assoc word (gethash prev *words*)))
        (cond ((assoc word (gethash prev *words*)) (format t "found~%"))
              (t (format t "~a not found~%" word))) 
        (set prev word))))


(defun make-check-valid-word ()
  (let ((prev `|.|))
    #'(lambda (word) 
        (let ((following (gethash prev *words*)))
          (do ((poi following (cdr poi)))
              ((or (eql (car following) word) (null poi))
               (cond ((null poi) 
                      (format t "asdasdas~%"))
                     (t (set prev word) (format t "aaaaaaaa ")))))))))
|#