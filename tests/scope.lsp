(defun test1() ;; Test variable scope
  (setq x 55)
  ((lambda(x) (setq x (+ x 1)) (return)) 10)
  x ;; should return 55
)

(defun test2() ;; Test variable scope with let)
(let ((x 22)) 
  (print x)
  ((lambda(x) (setq x (+ x 1)) (return)) 10)
  x ;; should return 22
)
)

(defun test3()
  (setf a 6 b 7)
  (+ a (+ 1 b))
)

(defun test4()
  (let ((a)(b))
     (setf a 6 b 7)
     (return (+ 1 (+ a b )))
  )
)