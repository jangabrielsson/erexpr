

(loop for x from 12 to 11 do
    (print x)
)

(
  (lambda (x stop step) 
    (loop
      (breakif (> x stop))
      (print i)
      (setq x (+ x step))
    )
  )
  12 stop step
)

(defvar nums `(9 5 2 3))
(dolist (item nums)
  (print item)
)

(
  (lambda (variableName list)
    (loop
      (breakif (empty list))
      (setq variableName (first list))
      (setq list (rest list))
      expressions
    )
  )
  nil list
)

(do ((a 0 (+ a 1))
     (b 10 (- b 1)))
    ((> a 10) (print "done"))
  (print a)
  (print b)
)

(
  (lambda (a b)
    (loop
      (if test then (break))
      expressions
      (setq a stepa)
      (setq b stepb
    )
  )
  5 10
)

(
  (lambda (a b)
    expression
  )
  5 10 v
)