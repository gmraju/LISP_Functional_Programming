(in-package #:exmatch)

(defun ?not (x y lsts)
  (cond ((match-p (car x) y lsts) nil)
        (t lsts)))


(defun ?or (x y lsts)
  (cond ((null lsts) nil)
        ((



(define-test match-or
  (assert-equal '(nil) (match-p '(?or a b) 'a))
  (assert-equal '(nil) (match-p '(?or a b) 'b))
  (assert-false (match-p '(?or a b) 'c))
  
  (assert-equality set-equal '(((?x . a)) ((?y . a))) (match-p '(?or ?x ?y) 'a))
  (assert-equality set-equal '(((?y . b) (?x . a)) ((?z . a) (?y . b) (?x . a))) 
                (match-p '(?x ?y (?or ?x ?y ?z)) '(a b a)))
  
  (assert-equal '(nil) (match-p '(?or (?? > 5) (?? < 0)) 10))
  (assert-equal '(nil) (match-p '(?or (?? > 5) (?? < 0)) -3))
  (assert-false (match-p '(?or (?? > 5) (?? < 0)) 3))
  
  (assert-equal '(((?x . 1))) (match-p '(?and (?or (?? < -10) (?? > 0)) ?x) 1))
  )



(define-test match-and
  (assert-equal '(nil) (match-p '(?and) nil))
  (assert-equal '(((?x . 12)))  (match-p '(?and (?? numberp) ?x) 12))
  (assert-false  (match-p '(?and (?? > 15) ?x) 12))
  
  (assert-equality set-equal '(((?x . 24)) ((?x . 30)))  (match-p '(?* (?and (?? > 20) ?x) ?*) '(24 11 3 30)))













  (assert-false (match-p '(?not (a b c)) '(a b c)))
  (assert-equal '(nil) (match-p '(?not (a b c)) 12))
  (assert-false (match-p '(?not ?x) 'a))
  (assert-equal '(((?x . a))) (match-p '(?x (?not ?x)) '(a b)))
  (assert-equal '(nil) (match-p '(?not (?? > 5)) 1))
  (assert-false (match-p '(?not (?? > 5)) 8))
  
  (assert-equal '(((?x . 1))) (match-p '(?and (?not > 5) ?x) 1))
  (assert-false (match-p '(?and (?not (?? > 5)) ?x) 8))