(defun fact (n)   ;; Test factorial, eats stack
  (if (= n 1)
      1
      (* n (fact (- n 1)))
  )
)

(defun fact22 (n acc) ;; Test tail-recursive factorial, constant stack
  (if (= n 1)
      acc
      (fact22 (- n 1) (* n acc))
  )
)
(defun fact2 (n) (fact22 n 1)) ;; Test tail-recursive factorial, constant stack
