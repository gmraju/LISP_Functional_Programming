(in-package :cs325-user)


(defun greater (num1 num2)
  (if (> num1 num2)
      num1
    num2))


(defun has-list-p (lst)
  (cond ((null lst) nil)
        ((listp (car lst)) t)
        (t (has-list-p (cdr lst)))))


(defun print-dots (num)
  (when (> num 0) 
    (format t ".")
    (print-dots (1- num))))


 (defun print-dots (num)
   (do ((i 1 (1+ i )))
       ((> i num))
     (format t ".")))


(defun get-a-count (lst)
  (do ((item lst (cdr item))
       (count 0 (cond ((eql 'a (car item)) (1+ count))
                      (t count))))  
      ((null item) count)))



(defun get-a-count (lst)
  (cond ((null lst) 0)
        ((eql 'a (car lst)) (1+ (get-a-count(cdr lst))))
        (t (get-a-count(cdr lst)))))


(defun summit (lst)
  (apply #'+ (remove nil lst)))


(defun summit (lst)
  (cond ((null lst) 0)
        ((null (car lst)) (summit (cdr lst)))
        (t (+ (car lst) (summit (cdr lst))))))




