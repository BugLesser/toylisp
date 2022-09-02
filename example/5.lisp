(defun normal (x)
    (cond
        ((typeif x 'CONS) x)
        (true (cons (normal (car x)) (normal (cdr x))))
    )
)

(println (normal '(defun normal (x)
                    (cond
                        ((typeif x 'CONS) x)
                        (true (cons (normal (car x)) (normal (cdr x))))
                    )
                )
        ))