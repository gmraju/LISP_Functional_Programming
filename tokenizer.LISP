(defclass tokenizer ()
  ((delimiter :accessor d)
   (str :accessor s)
   (index :accessor in)
   (prev :accessor pre)))

(defmethod next-token-p ((object tokenizer))
  (if (<= (in tok) (length (s tok)))
      t
    nil))





(defmethod next-token ((object tokenizer))
  (cond ((char= #\SPACE (d object)) 
         (next-token-space-delim object))
        (t (next-token-other-delim object))))


(defmethod next-token-other-delim ((object tokenizer))
  (let ((starter (in object)))
    (with-input-from-string (st (s object)
                                :start (in object))
      (do ((pointer (in object) (1+ pointer))
           (c (read-char st nil :eof)
              (read-char st nil :eof)))
          ((or 
            (and (eql c :eof) (setf (in object) (1+ pointer))) 
            (and (char= (d object) c)
                 (setf (in object) (1+ pointer))))  
           (subseq (s object) starter pointer))))))
        

(cond ((or (eql c :eof) (char= (d object) c)) (setf (in object) (1+ pointer)))



(defmethod next-token-space-delim ((object tokenizer))
  (let ((starter (in object)))
    (with-input-from-string (st (s object)
                                :start (in object))
      (do ((pointer (in object) (1+ pointer))
           (c (read-char st nil :eof)
              (read-char st nil :eof)))
          ((cond ((char= #\SPACE (d object)) 
                  (next-token-space-delim object starter pointer))
                 (t (next-token-other-delim object pointer)))
           (subseq (s object) starter pointer))))))



(defmethod next-token-space-delim ((object tokenizer) starter pointer)
  (cond ((eql c :eof) (setf (in object) (1+ pointer)))
        ((and (char= c (d object)) (eql starter pointer)) (1+ starter) nil)
        ((char= c (d object)) t)))








(defmethod next-token-space-delim ((object tokenizer))
  (let ((word nil))
  (with-input-from-string (st (s object)
                              :index ind
                              :start (in object))
    (peek-char st)
    (format t "~a " ind) (setf word (read st nil :eof)))
  (setf (in object) ind)
  (string-downcase word)))



(defun make-tokenizer (str &optional (del #\SPACE))
  (setf tok (make-instance 'tokenizer))
  (setf (d tok) del)
  (setf (s tok) str)
  (setf (in tok) 0)
  tok)









(defmethod next-token-other-delimiter ((object tokenizer))
  (with-input-from-string (st (s object)
                              :start (in object))
    (do ((c (read-char st nil :eof) (read-char st nil :eof))
         (pointer (in object) (cond ((and (char= (d object) c) (null prev)) pointer)
                                    (t (1+ pointer)))))
        ((cond ((eql c :eof) t) 
               ((and (char= (d object) c) (null (pre object))) (setf (in object) (1+ pointer)))
               ((char= (d object) (pre object)) (setf (in object) (1+ pointer)))
               (t ()))
         (setf (pre object) (subseq (s object) (in object) pointer))))))


(defmethod lol ((object tokenizer))
  (let ((prev nil))
    (format t "~a " prev)
    (setf prev 1)
    (format t "~a" prev)))



(defmethod next-token ((object tokenizer))
  (with-input-from-string (st (s object) 
                              :start (in-object))
    (do ((cur (read-char st nil :eof) (read-char st nil :eof))
         (prev nil c)
         (starter (in object) 
                  (cond ((char= (d object) cur)
                         (or (null prev) (char= (d object) prev))  
