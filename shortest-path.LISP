(in-package :cs325-user)

;;; Abort version

(defun shortest-path (start end net)
  (catch 'short-path
   (bfs end (list (list start)) net)))


(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (expand-queue queue path (car path) end net))))


(defun expand-queue (queue path node end net)
  (bfs end
       (append (cdr queue)
               (new-paths path node net end))
       net))


(defun new-paths (path node net end)
  (mapcan
   #'(lambda (n)
       (cond ((eql n end)
              (throw 'short-path (reverse (cons n path))))
             ((member n path) nil)
             (t (list (cons n path)))))
   (cdr (assoc node net))))






;;; Non-abort version

(defun shortest-path (start end net)
   (bfs end (list (list start)) net))


(defun bfs (end queue net)
  (if (empty-queue-p queue) 
      nil
    (let ((path (car queue)))
      (expand-queue queue path (car path) end net))))


(defun expand-queue (queue path node end net)
  (if (member end (cdr (assoc node net)))
      (reverse (cons end path))
    (bfs end
         (append (cdr queue)
                 (new-paths path node net end))
         net)))


(defun new-paths (path node net end)
  (mapcan 
   #'(lambda (n)
       (if (member n path) 
           nil
         (list (cons n path))))
   (cdr (assoc node net))))






