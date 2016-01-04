(defmacro let (bindings & body)
  `(let* ~bindings
     (do ~@body)))

(defmacro defun (name args & body)
  `(def ~name (fn ~args ~@body)))

(defmacro inc (x)
  `(+ 1 ~x))

(defmacro assert (condition msg)
  `(if ~condition
       true
       (do
        (print "FAIL: " '~condition)
        (error))))

(defmacro unless (x & body)
  `(if (not ~x)
       (do ~@body)))

(defmacro when (x & body)
  `(if ~x (do ~@body)))

(def *gen-sym-counter* 0)

(defun gen-sym ()
  (set! *gen-sym-counter* (inc *gen-sym-counter*))
  *gen-sym-counter*)

(defmacro deftest (name & body)
  `(defun ~name ()
     (print "running test:" '~name)
     ~@body))
