(defun shortest-path (start end net)
  (reverse (ids 
            (list (list start))
            (lambda (n) (eql n end))
            (lambda (n) (cdr (assoc n net))))))


(defun ids (paths pred gen)
  (do ((n 1 (1+ n))
       (found '(nil) 
              (dls (car paths) nil pred gen n)))
      ((or (null found) (funcall pred (car found))) found)))


(defun dls (adj-nodes init-path pred gen n)
  (cond ((null adj-nodes) nil)
        ((= n 0) (list nil)) 
        (t (do ((node adj-nodes (cdr node))
                (cur-path init-path 
                          (find-paths node init-path pred gen n)))
               ((or (null node) (funcall pred (car cur-path))) 
                (if (null cur-path) nil cur-path))))))


(defun find-paths (node path pred gen n)
  (cond 
   ((funcall pred (car node)) 
    (cons (car node) path))                    
   (t (dls 
       (get-legal-states node path gen) 
       (cons (car node) path) pred gen (1- n)))))


(defun get-legal-states (node path gen)
  (mapcan #'(lambda (n) 
              (if (member n path) 
                  nil 
                (list n))) 
          (funcall gen (car node))))




