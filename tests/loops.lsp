;(defun test1(n)
;  (dotimes (i n) (print i))
;)

(defun test2(start stop step)
  (loop for i from start to stop by step do (print i))
  (return 42)
)


(defun test3(list)
  (dolist (e list) (print e))
)

(defun test4(list)
  (setf a 1)
  (loop (inc+ a 1) (print a) (if (> a 10) (return a)))
)

(defun test5(list)
  (setf a 1)
  (loop (inc+ a 1) (print a) (breakif (> a 10)))
)

(defun test6(list)
  (loop forin k v in (ipairs list) do (print k v))
  (return 42)
)

(defun test7(lis)
  (loop forin k v in (pairs list) do (print k v))
  (return 42)
)