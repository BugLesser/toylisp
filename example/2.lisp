; Realizing recursive factorial by Y combinator

(setq Y
    (lambda (f) 
        ((lambda (g) (g g))
            (lambda (g) (f (lambda (x) ((g g) x)))))))

(defun fc (g) 
    (lambda (x) 
        (cond ((lte x 0) 1) ((gt x 0) (* x (g (- x 1)))))))

(println ((Y fc) 10))
