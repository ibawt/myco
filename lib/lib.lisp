(defmacro let (bindings & body)
  `(let* ~bindings
     (do ~@body)))

(defmacro defun (name args & body)
  `(def ~name (fn ~args ~@body)))

(defmacro inc (x)
  `(+ 1 ~x))

(defmacro assert (condition msg)
  `(if ~condition
     (do
       true)
     (do
       (print "FAIL: " '~msg)
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

(defmacro defsuite (name & body)
  `(defun ~name ()
     (let ((passes 0)
           (fails 0))
      (print "Running Test Suite: " '~name)
       ~@body)))

(defmacro deftest (name & body)
  `(defun ~name ()
     (let ((assert-passes 0)
           (assert-fails 0))
       (print "running test:" '~name)
       ~@body)))

(defun second (x)
 (first (rest x)))

(defmacro cond (& xs)
  (if (> (count xs) 0)
      (list 'if (first (first xs))
            (second (first xs))
            (cons 'cond (rest xs)))))

(defmacro empty? (x)
  `(= 0 (count ~x)))

(defun map (f coll out)
  (if (empty? coll)
      out
      (recur f (rest coll)
             (append (f (first coll)) out))))
