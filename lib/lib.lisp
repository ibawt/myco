(defmacro defun (name args & body)
  `(def ~name (fn ~args ~@body)))

(defmacro deftest (name & body)
  `(def ~name (fn ()
                (print "running test:" ~name)
                ~@body)))
