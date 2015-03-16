(define and (macro (x y) `(if ,x ,y ,x)))
(define or (macro (x y) `(if ,x ,x ,y)))
(define (not x) (if x #f #t))

(define (+1 x) (+ x 1))
(define (-1 x) (+ x 1))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f)))

(define (eqv? x y) (eq? x y))    ; so wrong

(define (boolean? x) (or (eq? x #f) (eq? x #t)))

(define (number? x) (integer? x))  ; the only numbers we have :(

(define (complex? x) #f)
(define (real? x) #f)
(define (rational? x) #f)

(define (exact? x) (integer? x))   ; not entirely correct, but we don't have floating point yet, so it will do
(define (inexact? x) #f)           ; see above

(define (exact-integer? x) (and (exact? x) (integer? x)))

(define (finite? x) (number? x))
(define (infinite? x) #f)
(define (nan? x) #f)

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

(define (list? xs)
   (or (null? xs) (and (pair? xs) (list? (cdr xs)))))

(define (make-list . xs) (do-make-list (car xs) (if (null? (cdr xs)) #f (cadr xs))))
(define (do-make-list n fill) (if (eq? n 0) '() (cons fill (do-make-list (- n 1) fill))))
(define (length xs) (if (null? xs) 0 (+ 1 (length (cdr xs)))))

(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(define (filter f xs) (cond ((null? xs) '())
                            ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
                            (else (filter f (cdr xs)))))
                            
(define (append . lists) (do-append lists))
(define (do-append lists)
  (cond ((null? lists) '())
        ((null? (cdr lists)) (car lists))
        (else (append-2 (car lists) (do-append (cdr lists))))))
(define (append-2 xs ys)
  (if (null? xs) ys (cons (car xs) (append-2 (cdr xs) ys))))

(define (reverse xs) (reverse-acc xs '()))
(define (reverse-acc xs acc) (if (null? xs) acc (reverse-acc (cdr xs) (cons (car xs) acc))))
(define (list-tail x k) (if (zero? k) x (list-tail (cdr x) (- k 1))))
(define (list-ref x k) (car (list-tail x k)))
(define (memq obj xs)
   (cond ((null? xs) #f)
         ((eq? (car xs) obj) xs)
         (else (memq obj (cdr xs)))))

(define (memv obj xs) (member obj xs eqv?))

(define (assq obj alist) (assoc obj alist eq?))
(define (assv obj alist) (assoc obj alist eqv?))

(define let (macro (bindings expr)
  (cons (list 'lambda (map car bindings) expr) (map cadr bindings))))

(define (member . args)
   (let ((obj (car args))
         (xs (cadr args))
         (compare (if (null? (cddr args)) equal? (caddr args))))
        (member-3 obj xs compare)))

(define (member-3 obj xs compare)
   (cond ((null? xs) #f)
         ((compare (car xs) obj) xs)
         (else (member-3 obj (cdr xs) compare))))
         
(define (assoc . args)
   (let ((obj (car args))
         (alist (cadr args))
         (compare (if (null? (cddr args)) equal? (caddr args))))
        (assoc-3 obj alist compare)))

(define (assoc-3 obj alist compare)
   (cond ((null? alist) #f)
         ((compare (caar alist) obj) (car alist))
         (else (assoc-3 obj (cdr alist) compare))))

(define (let*2let bindings expr)
  (if (null? bindings) expr
    (list 'let (list (car bindings)) (let*2let (cdr bindings) expr))))

(define let* (macro (bindings expr) (let*2let bindings expr)))
  
(define (if2cond clauses)
  (if (null? clauses) nil
    (if (eq? (caar clauses) 'else) (cadar clauses)
      (list 'if (caar clauses) (cadar clauses) (if2cond (cdr clauses))))))

(define cond (macro clauses (if2cond clauses)))

(define alu (macro (op) `(define ,op (lambda x (#alu (quote ,op) x)))))
(define alu2 (macro (op) `(define ,op (lambda (x y) (#alu (quote ,op) (list x y))))))

(alu +)
(alu -)
(alu *)
(alu mod)
(alu <)
(alu >)
(alu <=)
(alu >=)
(alu =)
(alu2 floor/)

(define (floor-quotient x y) (car (floor/ x y)))
(define (floor-remainder x y) (cadr (floor/ x y)))

(define (gcd x y)
  (if (eq? y 0)
      x
      (gcd y (floor-remainder x y))))

(define (lcm x y) (floor-quotient (* x y) (gcd x y)))

(define (square x) (* x x))

(define (even? x) (eq? (mod x 2) 0))
(define (odd? x) (eq? (mod x 2) 1))
(define (zero? x) (eq? x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define max (lambda x (minmax (car x) (cdr x) >)))
(define min (lambda x (minmax (car x) (cdr x) <)))

(define (minmax curr rest compare)
   (if (null? rest) curr
       (minmax (if (compare (car rest) curr) (car rest) curr) (cdr rest))))

(define (string? x) (and (pair? x) (eq? (car x) '#string)))

(define (write-list xs)
  (begin
    (write-string "(")
    (write-rest-of-list xs)))

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
  
(define (newline) (write-char 10))
(define (write x) (begin (display x) (newline)))

(define (do-string-append xs)
  (if (null? xs) ""
    (if (null? (cdr xs)) (car xs)
      (append (car xs) (cdr (do-string-append (cdr xs)))))))

(define string-append (lambda xs (do-string-append xs)))
