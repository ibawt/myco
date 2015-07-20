(defmacro foo (a) `(list ~@a))
(foo '(1 2))
