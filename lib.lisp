(defmacro if (test then else) 
    (list 'cond (list test then) (list true else))
)

(defmacro not (x) (list 'if x false true))

(defmacro and (a b) 
    (list 'if a (list 'if b true false) false)
)

(defmacro or (a b)
    (list 'if a true (list 'if b true false))
)

(defun instanceof (a b) (eq (typeof a) (typeof b)))

(defun typeif (a b) (eq (typeof a) b))

(defmacro inc (x) (list 'setq x (list '+ x 1)))

(defmacro dec (x) (list 'setq x (list '- x 1)))

(defun length (l) 
    (cond
        ((eq null l) 0)
        (true (+ 1 (length (cdr l))))
    )
)

(defmacro atom (x) 
    (list 'or 
        (list 'neq 'CONS (list 'typeof x)) 
        (list 'eq 0 (list 'length x)) 
    )
)

(defmacro swap (a b) 
    (list 
        'progn 
        (list 'setq 'tmp a) 
        (list 'setq a b) 
        (list 'setq b 'tmp)
    )
)

(defun intersect (s t)
    (cond 
        ((or (eq null s) (eq null t)) null)
        ((eq (car s) (car t)) (cons (car s) (intersect (cdr s) (cdr t))))
        ((lt (car s) (car t)) (intersect (cdr s) t))
        ((gt (car s) (car t)) (intersect s (cdr t)))
    )
)

(defun union (s t)
    (cond ((eq null s) t)
            ((eq null t) s)
            ((eq (car s) (car t)) (cons (car s) (union (cdr s) (cdr t))))
            ((lt (car s) (car t)) (cons (car s) (union (cdr s) t)))
            ((gt (car s) (car t)) (cons (car t) (union s (cdr t))))
    )
)

(defmacro caar (x)
    (list 'car (list 'car x))
)

(defmacro cddr (x)
    (list 'cdr (list 'cdr x))
)

(defun empty (x)
    (eq null x)
)

(defun map (proc items)
    (if (empty items)
        null
        (cons (proc (car items))
            (map proc (cdr items)))
    )
)

(defun foreach (proc items)
    (cond
        ((empty items) null)
        (true 
            (progn 
                (proc (car items)) 
                (foreach proc (cdr items))))
    )
)

(defun filter (proc items)
    (progn 
        (setq result null)
        (cond
            ((empty items) null)
            (true 
                (progn
                    (setq ret (proc (car items)))
                    (cond (ret (setq result (cons (car items) result))))
            ))
        )
        result
    )
)

