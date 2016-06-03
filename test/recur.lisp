(defun f (x)
  (if (= 0 x)
      0
      (recur 0)))

(f 1)
