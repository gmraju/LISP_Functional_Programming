(in-package :cs325-user)

(defun show-list (exp)
  (cond ((atom exp) (format t "~a" exp))      
        (t (format t "[")
           (show-list (car exp))
           (do ((item (cdr exp) (cdr item)))
               ((atom item) (dot-print item))
             (format t " ")
             (show-list (car item)))
           (format t "]"))))


(defun dot-print (item)
  (unless (null item)
    (format t " . ~a" item)))