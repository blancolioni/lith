(define nil '())
(define list (lambda x x))

(define defmacro (macro (name args body) `(define ,name (macro ,args ,body))))

(define __exit nil)
(define (exit) (set! __exit '#t))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(define (filter f xs) (cond ((null? xs) '())
                            ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
                            (else (filter f (cdr xs)))))
                            
(define (append xs ys)
  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))

(define let (macro (bindings expr)
  (cons (list 'lambda (map car bindings) expr) (map cadr bindings))))

(define (let*2let bindings expr)
  (if (null? bindings) expr
    (list 'let (list (car bindings)) (let*2let (cdr bindings) expr))))

(define let* (macro (bindings expr) (let*2let bindings expr)))
  
(define (if2cond clauses)
  (if (null? clauses) nil
    (if (eq? (caar clauses) 'else) (cadar clauses)
      (list 'if (caar clauses) (cadar clauses) (if2cond (cdr clauses))))))

(define cond (macro clauses (if2cond clauses)))

(define and (macro (x y) `(if ,x ,y ,x)))
(define or (macro (x y) `(if ,x ,x ,y)))
(define (not x) (if x '#f '#t))

(define alu (macro (op) `(define ,op (lambda x (#alu (quote ,op) x)))))
                    ;(defun + x (#alu '+ x))
(alu +)
(alu -)
(alu *)
(alu /)
(alu mod)
(alu <)
(alu >)
(alu <=)
(alu >=)
(alu =)
(alu remainder)

(define (square x) (* x x))

(define (even? x) (eq? (mod x 2) 0))
(define (odd? x) (eq? (mod x 2) 1))

(define (string? x) (and (pair? x) (eq? (car x) '#string)))

(define (write-list xs)
  (write-string "(")
  (write-rest-of-list xs))

(define (write-rest-of-list xs)
  (cond ((null? xs) (write-string ")"))
        ((pair? xs) (begin (display (car xs))
                           (if (null? (cdr xs)) (write-string ")")
                                                (begin (write-string " ")
                                                       (write-rest-of-list (cdr xs))))))
        (else (begin (write-string ". ")
                     (display xs)
                     (write-string ")")))))
          
(define (display x)
  (cond ((string? x) (write-string x))
    ((pair? x) (write-list x))
    (else (write-string (symbol->string x)))))

(define (wchars xs) (if (null? xs) nil
              (begin (write-char (car xs))
                 (wchars (cdr xs)))))

(define (write-string x)
  (wchars (cdr x)))
  
(define (newline) (begin (write-char 13) (write-char 10)))
(define (write x) (begin (display x) (newline)))

(define (do-string-append xs)
  (if (null? xs) ""
    (if (null? (cdr xs)) (car xs)
      (append (car xs) (cdr (do-string-append (cdr xs)))))))

(define string-append (lambda xs (do-string-append xs)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
