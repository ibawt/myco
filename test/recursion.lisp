(def fib (fn (n)
             (fibiter 1 0 n)))

(def fibiter (fn (a b c)
                  (if (= c 0)
                      b
                      (fibiter (+ a b) a (- c 1)))))
(fib 45)
