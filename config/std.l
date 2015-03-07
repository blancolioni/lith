(define t 't)
(define + (#alu '+))

(define defmacro (macro (name args body) `(define ,name (macro ,args ,body))))
(defmacro defun (name args body) `(define ,name (lambda ,args ,body)))
(defmacro if (test true false) `(cond (,test ,true) ('t ,false)))
(defmacro and (x y) (if x y x))
(defmacro or (x y) (if x x y))
(defun not (x) (if x nil 't))

(define t 't)

(defun + (x y) (#alu '+ x y))
(defun - (x y) (#alu '- x y))
(defun * (x y) (#alu '* x y))
(defun / (x y) (#alu '/ x y))
(defun mod (x y) (#alu 'mod x y))

(defun evenp (x) (eq (mod x 2) 0))
(defun oddp (x) (eq (mod x 2) 1))

(defun map (f xs) (cond (xs (cons (f (car xs)) (map f (cdr xs))))
                        ((quote t) nil)))
(defun filter (p xs) (cond ((and xs (p (car xs))) (cons (car xs) (filter p (cdr xs))))
                           (xs (filter p (cdr xs)))
                           ('t nil)))
                           
(defun fac (n) (cond ((eq n 0) 1)
                     ('t (* n (fac (- n 1))))))

(defun getf (lst sym)
  (cond ((eq lst nil) nil)
        ((eq (car lst) sym) (car (cdr lst)))
        ('t (getf (cdr (cdr lst)) sym))))

(defmacro push (x xs)
   (setq xs (cons x xs)))