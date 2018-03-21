(in-package :cs325-user)

(defun longest-path (start end net)
  (reverse 
   (dfs (list start) end nil net)))


(defun longer (path1 path2)
  (cond ((> (length path1) (length path2)) path1)
        (t path2)))

(defun find-new-path (node init-path best-path end net)
  (cond 
   ((and (eql (car node) end) (consp best-path)) 
    (cons (car node) init-path))
   ((member (car node) init-path) best-path)                    
   (t (dfs (cdr (assoc (car node) net)) end (cons (car node) init-path) net))))


(defun dfs (adj-nodes end init-path net)
  (if (null adj-nodes) 
      nil    
    (do ((node adj-nodes (cdr node))
         (best-path init-path 
                    (longer 
                     (find-new-path node init-path best-path end net) 
                     best-path)))
        ((null node) (if (eql (car best-path) end) best-path nil)))))
 
        
            
           
 #|



(defun longer (path1 path2)
  (cond ((> (length path1) (length path2)) path1)
        (t path2)))


(defun npath (list path)
  (append path (list (car list))))


(defun nlist (list net)
  (cdr (assoc (car list) net)))


(defun dfs (list end path net)
  (cond ((null list)  nil)
        ((and (consp path) (eql (car list) end))
         (compare (new-path list path)
                  (dfs (cdr list) end path net)))            
        ((member (car list) path) 
         (dfs (cdr list) end path net))
        (t (compare 
            (dfs (nlist list net) end (npath list path) net)  
            (dfs (cdr list) end path net)))))

|#                 


#|

(defun dfs (list end net)
 (exit conditions)
 [list is null]
 [

 (dfs (list (car list)) end net)
 [check if return is not null]
 [if so, append (car list) to path if it is not a repeat]

 (dfs (cdr list) end net)


(defun dfs (list end path net)
  (cond ((null list) (format t "lol" ) nil)
        ((equal (car list) end) (format t "reached~%") (append path (list(car list))))
        ((member (car list) path) (format t "here") (dfs (cdr list) end path net))
        (t (format t "~%new path ~a  " (append path (list (car list)))) (format t "Next list ~a  " (cdr (assoc (car list) net))) (format t "list ~a~%" list)
           (compare (dfs (cdr (assoc (car list) net)) end (append path (list (car list))) net) 
                    (dfs (cdr list) end (append path (list (car list))) net)))))



(defun dfs (list end path net)
  (format t "~%~% list- ~a  path- ~a" list path)
  (cond ((null list) (format t "lol" ) nil)
         ((and (not (null path))  
              (equal (car list) end)) (format t "reached and ~a~%"(append path (list (car list)))) (compare (dfs (cdr list) end path net) (append path (list (car list)))))
         ((member (car list) path) (format t "here~%") (dfs (cdr list) end path net))
         (t (format t "~%new path1 ~a  new path 2 ~a " (append path (list (car list))) (cdr list)) (format t "Next list ~a  " (cdr (assoc (car list) net))) (format t "current list ~a~%" list)
            (compare  
             (dfs (cdr (assoc (car list) net)) end (append path (list (car list))) net) 
             (dfs (cdr list) end (append path (list (car list))) net)))))

|#


#|



(defun dfs (list end path net)
  (cond ((null list) nil)
        ((equal (car list) end) (append path (list(car list))))
        ((member (car list) path) (dfs (cdr list) end path net))
        (t (compare 
            (dfs (cdr (assoc (car list) net)) end (append path (list (car list))) net) 
            (dfs (cdr list) end path net)))))

(defun compare (path1 path2)
  (cond ((> (length path1) (length path2)) path1)
        (t path2)))

(defun longest-path (start end net)
  (dfs (cdr (assoc start net)) end (list start) net))
|#



#| FINAL

(defun longest-path (start end net)
   (or (dfs (list start) end nil net) (and (equal start end) (list start))))

(defun compare (path1 path2)
  (format t "~%~a ~a~%~%" path1 path2)
  (cond ((> (length path1) (length path2)) path1)
        (t path2)))

(defun dfs (list end path net)
  (format t "~%~% list- ~a  path- ~a" list path)
  (cond ((null list) (format t "lol" ) nil)
         ((and (not (null path))  
              (equal (car list) end)) (format t "reached and ~a~%"(append path (list (car list)))) (compare (dfs (cdr list) end path net) (append path (list (car list)))))
         ((member (car list) path) (format t "here~%") (dfs (cdr list) end path net))
         (t (format t "~%new path1 ~a  new path 2 ~a " (append path (list (car list))) (cdr list)) (format t "Next list ~a  " (cdr (assoc (car list) net))) (format t "current list ~a~%" list)
            (compare  
             (dfs (cdr (assoc (car list) net)) end (append path (list (car list))) net) 
             (dfs (cdr list) end path net)))))
|#