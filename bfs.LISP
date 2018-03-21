(defun shortest-path (start end net)
  (catch 'short-path
    (bfs end (list (list start)) net)))


(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
            (throw 'short-path (reverse path))
          (bfs end
               (adjoin (cdr queue)
                       (new-paths path node net))
               net))))))


(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))