(in-package :cs325-user)


(defun shortest-path (start end net)
  (reverse (bfs 
            (list (list start)) 
            (lambda (n) (eql n end))
            (lambda (n) (cdr (assoc n net))))))



(defun bfs (paths pred gen)
  (if (empty-queue-p paths) 
      nil
    (let* ((cur-path (car paths))
           (goal 
            (find-if 
             pred 
             (funcall gen (car cur-path)))))
      (if goal
          (cons goal cur-path)
        (expand-queue
         paths cur-path pred gen)))))
      


(defun expand-queue (paths cur-path pred gen)
  (bfs
   (append (cdr paths)
           (new-paths cur-path (car cur-path) gen))
   pred gen))


(defun new-paths (cur-path state gen)
  (mapcan 
   #'(lambda (x) 
       (if (member x cur-path) 
           nil
         (list (cons x cur-path)))) 
   (funcall gen state)))
