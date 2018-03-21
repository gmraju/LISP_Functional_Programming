(defun reduce-tree (fn tree &optional (init-val nil))
  (cond ((null tree) init-val) 
        ((atom tree) (funcall fn init-val tree))
        (t (let ((cur-val (reduce-tree fn (car tree) init-val)))
             (reduce-tree fn (cdr tree) cur-val)))))
           



(defun reduce-tree (fn tree &optional (init-val nil))
  (cond ((null tree) init-val) 
        ((atom tree) (funcall fn init-val tree))
        (t (reduce-tree fn (cdr tree) (reduce-tree fn (car tree) init-val)))))