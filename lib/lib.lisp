(defmacro defun (name args &rest body)
  `(def ~name (fn ~args ~@body)))

(defun foo (f) (+ 1 f))
