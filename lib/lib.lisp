(defmacro defun (name args &rest body)
  `(def ~name (fn ~args) ~@body)))
