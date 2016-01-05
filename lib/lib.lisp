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

;; (conjVj
;;   ((test) (body))
;;   ((test2 (body2))))

;; (if test
;;     (body)
;;     (if test2
;;         body2))
(defun second (x)
  (first (first x)))


(defmacro cond (& xs)
  (print "xs: " xs)
  (if (> (count xs) 0)
      (list 'if (first (first xs))
            (second (first xs))
            (cons 'cond (rest xs)))))

;; (defmacro cond (& body)
;;   (let ((f (fn (args)
;;                (print "f = " )
;;                (print "start of args" args)
;;                (unless (= '() args)
;;                  (let ((ca (first args)))
;;                    (print "in the unless " ca)
;;                    `(if ~(first ca)
;;                         ~(second ca)
;;                         ~@(f (rest args))))))))
;;     (f body)))
;; -
  ;; (let ((f (fn (out c)
  ;;              (if (= () c)
  ;;                  out
  ;;                  (do
  ;;                   (let ((ce (first c))
  ;;                         (rest (rest c)))
  ;;                     (f )))))))
  ;;   (f body)))
