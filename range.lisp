(in-package :cs325-user)

(defun map-range (f start end)
  (let ((incdec (if (> start end) -1 1)))
    (do ((i start (+ i incdec))
         (item nil (cons (funcall f i) item))) 
        ((= i end) (reverse item)))))


(defun every-range (f start end)
  (let ((incdec (if (> start end) -1 1)))
    (do ((i start (+ i incdec)))
       ((or (= i end) (null (funcall f i))) 
        (or (= start end) (= i end))))))


(defun find-range (f start end)
  (let ((incdec (if (> start end) -1 1)))
    (do ((i start (+ incdec i)))
        ((or (= i end) (funcall f i)) 
         (if (= i end) nil i)))))