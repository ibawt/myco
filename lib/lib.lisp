(defmacro defun (name args & body)
  `(def ~name (fn ~args ~@body)))

(defun foo (f) (+ 1 f))
