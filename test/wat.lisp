(def fib (fn (n) (fibiter 1 0 n))) (def fibiter (fn (a b count) (if (= count 0) b (fibiter (+ a b) a (- count 1))))) (fib 35)
