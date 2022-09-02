(defun fib (n)
    (if 
        (lt n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))
(println (fib 10))

(defun fib-non-recursive (n)
    (fib-iter 1 0 0 1 n))
(defun fib-iter (a b p q count)
    (if (eq count 0) b
        (fib-iter (+ (* b q ) (* a q ))
                            (+ (* b p) ( * a q))
                            p
                            q
                            (- count 1))))
(println (fib-non-recursive 100))

(defun instanceof (a b) (eq (typeof a) (typeof b)))
(defun typeif (a b) (eq (typeof a) b))

(println (typeif '(1 2 3) 'CONS))

(defun recursion (x) (cond
                        ((typeif x 'CONS) 
                            (progn 
                                (recursion (car x))
                                (cond ((neq (cdr x) null) (recursion (cdr x))))))
                        (true (println x))))

(recursion '(defun recursion (x) (cond
                        ((typeif x 'CONS) 
                            (progn 
                                (recursion (car x))
                                (cond ((neq (cdr x) null) (recursion (cdr x))))))
                        (true (println x)))))
