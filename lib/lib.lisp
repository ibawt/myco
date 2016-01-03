(defmacro defun (name args & body)
  `(def ~name (fn ~args ~@body)))

(defmacro inc (x)
  `(+ 1 ~x))

(defmacro let (bindings & body)
  `(let* (~bindings)
     (do ~@body)))

(def *gen-sym-counter* 0)

(defun gen-sym ()
  (set! *gen-sym-counter* (inc *gen-sym-counter*))
  *gen-sym-counter*)

(defmacro deftest (name & body)
  `(defun ~name ()
     (print "running test:" '~name)
     ~@body))
