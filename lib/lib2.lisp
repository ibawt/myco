(defun map* (f coll out)
  (if (empty? coll)
      out
      (recur f (rest coll)
             (append (f (first coll)) out))))
