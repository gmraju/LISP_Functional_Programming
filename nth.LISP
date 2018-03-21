(defmacro nth-expr (n &rest exp-lst)
  (let (
  `(nth ,n ,@exp-lst))



(defmacro nth-expr (n &rest exp-lst)
  


(let ((n 2))
(nth-expr n (/ 1 0) ( + 1 2 ) (/ 1 0)))




(defmacro nth-expr (n &body exp-lst)
  (let ((pos (gensym))
        (exp (gensym)))
    (let ((pos (1- ,n))
           (exp exp-lst)))
       (nth ,pos ,exp)))


(defmacro nth-expr (n &body exp-lst)
  `(nth 1 ',exp-lst))



(defmacro nth-expr (n


(defmacro n-of (n exp)
  (let ((out (gensym))
        (c n))
    `(do ((count ,c (1- ,count))
          (,out ,nil) (or (format t "~A ~a" ,exp ,count) (cons ,exp ,out)))
         ((zerop ,count) (reverse ,out)))))