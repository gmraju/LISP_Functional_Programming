(defun bst-elements (bst)
  (cond ((null bst) nil)
        (t (append 
            (bst-elements (node-r bst))
            (list (node-elt bst))
            (bst-elements (node-l bst))))))


(defun endcons (a v)
   (if (null v) (cons a nil) (cons (car v) (endcons a (cdr v)))))

(defun tree-traverse (bst)
  (if (null bst) 
      nil 
    (cons (remove nil (tree-traverse (node-l bst))
          (cons (node-elt bst) (tree-traverse (node-r bst))))))

(defun get-single-list (branch)
  (do ((elts branch (cdr elts))
       (complete-list nil (cond ((null (car elts)) complete-list) 
                                ((numberp (car elts)) (cons (car elts) complete-list))
                                (t (cons (get-single-list (car elts)) ))))
      ((null elts) complete-path)))


(defun bst-elements (bst)
  (tree-traverse bst))

         
(defun bst-elements (bst)
  (let ((lst nil))
    (bst-traverse (lambda (x) (setf lst (cons x lst))) bst)
    lst))
       

;;;;;

;;;;using find  --not done
(defun make-ascending-lst (bst current-node)
  (cond ((equal (node-elt current-node) (node-elt (setf next-node (make-ascending-lst bst (bst-find (node-elt current-node) bst #'<)))))
         (cons (node-elt current-node) nil))
        (t (cons (node-elt current-node) (node-elt next-node)))))


;;;;delete and max using loop

(defun bst-elements (bst)
  (reverse (make-ascending-lst bst)))

(defun make-ascending-lst (bst)
  (do ((new-bst bst (bst-remove (node-elt (bst-max new-bst)) new-bst #'<))
       (ascending-lst nil (cons (node-elt (bst-max new-bst)) ascending-lst)))
      ((null new-bst) ascending-lst)))
    


;;;;append
(defun bst-elements (bst)
  (cond ((null bst) nil)
        (t (append 
            (bst-elements (node-r bst))
            (list (node-elt bst))
            (bst-elements (node-l bst))))))




;;;delete and max using recursion
(defun bst-elements (bst)
  (reverse (make-ascending-lst bst nil)))

;version1 - not working
(defun make-ascending-lst (bst path)
  (cond ((null bst) path)
        (t 
         (push (node-elt (bst-max bst)) (car path))
         (make-ascending-lst 
          (bst-remove (node-elt (bst-max bst)) bst #'<) path))))

;version2
(defun make-ascending-lst (bst path)
  (cond ((null bst) path)
        (t 
         (setf path (cons (node-elt (bst-max bst)) path))
         (make-ascending-lst 
          (bst-remove (node-elt (bst-max bst)) bst #'<) path))))





(defun bst-elements (bst)
  (bst-traverse #'cons-elements bst)

(defun cons-elements (elt)
  (setf (


;(format t "~%Node-elt CN: ~a  make-asc-lst: ~a ~%" (node-elt current-node) (make-ascending-lst bst (bst-find (node-elt current-node) bst #'<)))