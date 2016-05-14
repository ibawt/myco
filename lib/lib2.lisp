(defun reduce2 (f coll out)
  (print coll)
  (if (empty? coll)
      out
      (recur f (rest x)
             (cons (f (first x)) out))))
