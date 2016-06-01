(defun map* (f coll out)
  (if (empty? coll)
      out
      (recur f (rest coll)
             (cons (f (first coll)) out))))
