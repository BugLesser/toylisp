; macro tests

; (Nil! val) => (setq val null)
(defmacro Nil! (x) (list 'setq x null))
(setq val 666)
(println val) ; 666
(Nil! val)
(println val) ; null
(println (macroexpand '(Nil! val)))


; (F! 10) => ((lambda (x) (println x)) 10)
(defmacro F! (x) (list (list 'lambda (list 'y) (list 'println 'y)) x))
(F! 10)
(println (macroexpand '(F! 10)))


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

(println (and false true))
(println (or false true))