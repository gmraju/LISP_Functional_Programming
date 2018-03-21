(in-package :cs325-user)

;;; From Paul Graham's ANSI Common Lisp
;;; Figure 7.2, Page 129
;;;
;;; Requires fixed version of buf.lisp

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
     (with-open-file (out file2 :direction :output
                                :if-exists :supersede)
       (stream-subst old new in out)))


(defun stream-subst (old new in out &key (wildcard #\+))       
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((zerop len) (princ old out))
            ((or (equal c element) 
                 (equal element wildcard) 
                 (equal element :wild)
             (elt-match-p c old pos wildcard))
             (incf pos)
             (cond ((= pos len)    
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         
                    (buf-insert c buf))))
            ((zerop pos)                   
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))


(defun elt-match-p (c old pos wildcard)
  (let ((element (elt old pos))) 
    (if (characterp element) 
        (or (eql c element) 
            (eql element wildcard))
      (ecase element
        (:digit (digit-char-p c))
        (:alpha (alpha-char-p c))
        (:alphanumeric (alphanumericp c))
        (:wild t)))))

#|
  (cond ((eql element :digit) (digit-char-p c))
        ((eql element :alpha) (alpha-char-p c))
        ((eql element :alphanumeric) (alphanumericp c))
        (t (or (eql c element) 
               (eql element wildcard) 
               (eql element :wild))))))

(defun checker (elt(chec c)
  (case elt
    (c (format t "lol"))))
         
|#

