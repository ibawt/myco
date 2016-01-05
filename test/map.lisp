(assert (= '(2 2) (map (fn (x) (+ x 1)) '(1 1))))
(assert (= '() (map (fn (x) x) '())))
