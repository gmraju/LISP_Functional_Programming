(in-package :cs325-user)

(defun has-number-p (exp)
  (unless (null exp)
    (or (numberp exp) (and (consp exp) (some #'has-number-p exp)))))