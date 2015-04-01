(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define define-syntax (macro (name rules)
    ((lambda (xs)
       (list 'define name (list 'macro xs (list 'apply-syntax xs rules)))) (gensym)))) ; (list 'macro 'xs (list 'apply-syntax 'xs rules)))))

(define-syntax and
  (syntax-rules () 
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...) (if test1 (and test2 ...) #f))))
               
(define-syntax or
  (syntax-rules () 
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...) (if test1 #t (or test2 ...)))))
               
(define (not x) (if x #f #t))

(define (boolean? x) (or (eq? x #t) (eq? x #f)))
(define (boolean=? x y) (and (boolean? x) (boolean? y) (eq? x y)))

; (define (set-car! xs x) (list-set! xs 0 (cons x (cdr xs))))
(define (list-set! xs k x) (if (zero? k) (set-car! xs x)
                               (list-set! (cdr xs) (- k 1) x)))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((lith-external? x) (lith-external-equal x y))
        ((lith-external? y) #f)
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f)))

(define (eqv? x y)
  (cond ((eq? x y) #t)
        ((and (number? x) (number? y)) (equal? x y))))   ; works because equal numbers have the same list representation

(define (number? x) (or (integer? x) (real? x)))  ; the only numbers we have :(

(define (complex? x) #f)
(define (rational? x) (and (pair? x) (eq? (car x) '#rational)))

(define (exact? x) (or (integer? x) (rational? x)))
(define (inexact? x) (or (real? x) (complex? x)))

(define (exact-integer? x) (and (exact? x) (integer? x)))
(define (exact-integer-sqrt k)
   (let ((r (isqrt-try k 0)))
        (list r (- k (square r)))))
        
(define (isqrt-try k n) (if (<= (square n) k) (isqrt-try k (+ n 1)) (- n 1)))
        
(define (finite? x) (number? x))
(define (infinite? x) #f)
(define (nan? x) #f)

(define (list? xs)
   (or (null? xs) (lith-and (pair? xs) (list? (cdr xs)))))

(define (make-list . xs)
    (define (do-make-list n fill) (if (eq? n 0) '() (cons fill (do-make-list (- n 1) fill))))
  (do-make-list (car xs) (if (null? (cdr xs)) #f (cadr xs))))
  
(define (length xs) (if (null? xs) 0 (+ 1 (length (cdr xs)))))

(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(define (filter f xs) (cond ((null? xs) '())
                            ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
                            (else (filter f (cdr xs)))))
                            
(define (append . lists) 
  (define (do-append lists)
    (cond ((null? lists) '())
          ((null? (cdr lists)) (car lists))
          (else (append-2 (car lists) (do-append (cdr lists))))))
  (define (append-2 xs ys)
    (if (null? xs) ys (cons (car xs) (append-2 (cdr xs) ys))))
  (do-append lists))

(define (reverse xs) 
  (define (reverse-acc xs acc) (if (null? xs) acc (reverse-acc (cdr xs) (cons (car xs) acc))))
  (reverse-acc xs '()))

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

(define let* (macro (bindings . exprs)
                    (if (null? bindings) (append (list 'let '()) exprs)
                        (list 'let (list (car bindings)) (append (list 'let* (cdr bindings)) exprs)))))
  
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

(define (list-copy xs) (if (pair? xs) (cons (car xs) (list-copy (cdr xs))) xs))

(define (if2cond clauses)
  (if (null? clauses) nil
    (if (eq? (caar clauses) 'else) (car (cdar clauses))
      (list 'if (caar clauses) (car (cdar clauses)) (if2cond (cdr clauses))))))

(define cond (macro clauses (if2cond clauses)))

(define (lith-merge-numbers x y)
   (cond ((or (inexact? x) (inexact? y)) (list (inexact x) (inexact y)))
         ((and (integer? x) (integer? y)) (list x y))
         ((and (integer? x) (rational? y)) (list (make-rat x) y))
         ((and (rational? x) (integer? y)) (list x (make-rat y)))
         ((and (rational? x) (rational? y)) (list x y))))

;(define fold-alu (macro (op identity) `((lambda (xs) (display xs)) ,identity)))
(define fold-alu (macro (op app identity)
   `(define ,op 
       (lambda xs (if (null? xs) 
                      ,identity
                      (if (null? (cdr xs)) 
                          (fold-op ,identity (quote ,op) ,app (list ,identity (car xs)))
                          (fold-op ,identity (quote ,op) ,app xs)))))))

(define (fold-op identity op app xs)
  (let* ((x-1 (car xs))
         (x-2 (cadr xs))
         (merge (lith-merge-numbers x-1 x-2))
         (x (car merge))
         (y (cadr merge))
         (z (app op x y)))
         (if (null? (cddr xs))
             z
             (fold-op identity op app (cons z (cddr xs))))))
           
(define (apply-op op x y) 
   (cond ((inexact? x) (#alu op (list x y)))
         ((integer? x) (#alu op (list x y)))
         ((rational? x) (apply-rat op x y))))
         
(define (apply-/ op x y) 
   (cond ((inexact? x) (#alu op (list x y)))
         ((integer? x) (make-rat x y))
         ((rational? x) (div-rat x y))))
         
(define (apply-rat op x y)
   (cond ((eq? op '+) (add-rat x y))
         ((eq? op '-) (sub-rat x y))
         ((eq? op '*) (mul-rat x y))
         ((eq? op '/) (div-rat x y))
         (else (error "bad operation"))))
         
(define alu (macro (op) `(define ,op (lambda x (#alu (quote ,op) x)))))
(define alu2 (macro (op) `(define ,op (lambda (x y) (#alu (quote ,op) (list x y))))))
(define char-alu (macro (op) 
  `(define ,(string->symbol (string-append "char" (symbol->string op) "?")) (lambda x (#alu (quote ,op) (map char->integer x))))))

(fold-alu + apply-op 0)
(fold-alu - apply-op 0)
(fold-alu * apply-op 1)
(fold-alu / apply-/ 1)

;(alu +)
;(alu -)
;(alu *)
(alu mod)
(alu <)
(alu >)
(alu <=)
(alu >=)
(alu =)
(alu2 floor/)

(define (plus-2 x y)
   (cond ((or (inexact? x) (inexact? y)) (+ x y))
         ((and (integer? x) (integer? y)) (+ x y))
         (else (add-rat (make-rat x) (make-rat y)))))

(define (make-rat . rs)
   (cond ((null? rs) (list '#rational 1 1))
         ((null? (cdr rs)) (list '#rational (car rs) 1))
         (else (rat-simplify (list '#rational (car rs) (cadr rs))))))
         
(define (rat-simplify r)
   (let ((n (numerator r)) (d (denominator r)))
     (let ((g (gcd n d)))
       (if (eqv? g d) (floor-quotient n g)
         (list '#rational (floor-quotient n g) (floor-quotient d g))))))
   
(define (print-rat r) (display (numerator r)) (write-char #\/) (display (denominator r)))

(define (add-rat x y)
   (make-rat 
     (+ 
       (* (numerator x) (denominator y))
       (* (numerator y) (denominator x)))
     (* (denominator x) (denominator y))))

(define (sub-rat x y)
   (make-rat 
     (-
       (* (numerator x) (denominator y))
       (* (numerator y) (denominator x)))
     (* (denominator x) (denominator y))))

(define (mul-rat x y)
   (make-rat (* (numerator x) (numerator y)) (* (denominator x) (denominator y))))
   
(define (div-rat x y)
   (make-rat (* (numerator x) (denominator y)) (* (denominator x) (numerator y))))
   
(define (numerator r) (if (integer? r) r (cadr r)))
(define (denominator r) (if (integer? r) 1 (car (cddr r))))

(define (floor-quotient x y) (car (floor/ x y)))
(define (floor-remainder x y) (cadr (floor/ x y)))

(define (gcd . xs) 
  (define (gcd-2 x y)       
    (if (eq? y 0)
        x
        (gcd-2 y (floor-remainder x y))))
  (define (run-gcd xs acc)
    (if (null? xs) acc
        (run-gcd (cdr xs) (gcd-2 (car xs) acc))))
 (run-gcd xs 0))

(define (lcm . xs) 
  (define (lcm-2 x y) (floor-quotient (* x y) (gcd x y)))
  (define (run-lcm xs acc)
    (if (null? xs) acc
        (run-lcm (cdr xs) (lcm-2 (car xs) acc))))
  (run-lcm xs 1))
      

(define (square x) (* x x))

(define (even? x) (eq? (mod x 2) 0))
(define (odd? x) (eq? (mod x 2) 1))
(define (zero? x) (eq? x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (abs x) (if (negative? x) (- x) x))

(define max (lambda x (minmax (car x) (cdr x) >)))
(define min (lambda x (minmax (car x) (cdr x) <)))

(define (minmax curr rest compare)
   (if (null? rest) curr
       (minmax (if (compare (car rest) curr) (car rest) curr) (cdr rest) compare)))

(define (number->string . z-and-radix)
   (let ((z (car z-and-radix))
         (radix (if (null? (cdr z-and-radix)) 10 (cadr z-and-radix))))
        (if (zero? z) "0"
         (number->string-with-radix z radix))))
         
(define (number->string-with-radix z radix)
  (if (zero? z) ""
      (let ((q-r (floor/ z radix)))
         (string-append (number->string-with-radix (car q-r) radix)
                        (string (radix-digit (cadr q-r) radix))))))
(define (radix-digit z radix)
   (if (< z 10) (integer->char (+ z 48)) (integer->char (+ (char->integer #\A) (- z 10)))))
   
(define (string? x) (and (pair? x) (eq? (car x) '#string)))
(define (string . chars) (cons '#string chars))
(define (make-string . args)
   (cons '#string (make-list (car args) (if (null? (cdr args)) #\x00 (cadr args)))))
(define (string-length s) (length (cdr s)))
(define (string-ref s k) (list-ref (cdr s) k))
(define (string-set! s k char) (list-set! (cdr s) k char))

(define (string-compare op strings)
   (define (go xs)
     (cond ((null? xs) #t)
           ((null? (cdr xs)) #t)
           (else (and (op (cdar xs) (cdr (cadr xs))) (go (cdr xs))))))
   (go strings))

(define (string=? . strings) (string-compare equal? strings))

(define (string-lt strings)
  (define (<2 s1 s2) 
     (cond ((null? s2) #f)
           ((null? s1) #t)
           ((char<? (car s1) (car s2)) #t)
           ((char>? (car s1) (car s2)) #f)
           (else (<2 (cdr s1) (cdr s2)))))
   (string-compare <2 strings))
   
(define (string-gt strings)
  (define (>2 s1 s2) 
     (cond ((null? s1) #f)
           ((null? s2) #t)
           ((char>? (car s1) (car s2)) #t)
           ((char<? (car s1) (car s2)) #f)
           (else (>2 (cdr s1) (cdr s2)))))
   (string-compare >2 strings))

(define (string<? . strings) (string-lt strings))
(define (string>? . strings) (string-gt strings))
(define (string>=? . strings) (not (string-lt strings)))
(define (string<=? . strings) (not (string-gt strings)))

(define (lith-string-copy args)   
  (define (take s count)
     (cond ((null? s) s)
           ((not (positive? count)) nil)
           (else (cons (car s) (take (cdr s) (- count 1))))))
  (define (go s start count)
     (if (positive? start) (if (null? s) s (go (cdr s) (- start 1) count)) (if count (take s count) s)))
  (let ((s (cdr (car args)))
        (start (if (null? (cdr args)) 0 (cadr args)))
        (end (if (or (null? (cdr args)) (null? (cddr args))) #f (car (cddr args)))))
       (cons '#string (go s start (if end (+ 1 (- end start)) #f)))))
       
(define (string-copy . args) (lith-string-copy args))
(define substring string-copy)

(define (string->list . args) (cdr (lith-string-copy args)))

(define (list->string list) (cons '#string list))

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
        ((rational? x) (print-rat x))
        ((pair? x) (write-list x))
        (else (write-string (symbol->string x)))))

(define (write-string x)
   (define (wchars xs) (if (null? xs) #no-value 
                           (begin (write-char (car xs))
                                  (wchars (cdr xs)))))
   (wchars (cdr x)))
  
(define (newline) (write-char #\newline))
(define (write x) (begin (display x) (newline)))

(define (string-append . xs)
  (define (go xs)
    (if (null? xs) ""
      (if (null? (cdr xs)) (car xs)
          (append (car xs) (cdr (go (cdr xs)))))))
  (go xs))

(char-alu <=)
(char-alu >=)
(char-alu <)
(char-alu >)
(char-alu =)

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))
         
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))
         
(define (call-with-port port proc)
  (begin (proc port)
         (close-port port)))

(define (input-port? port) (port-attribute? port 'input))
(define (output-port? port) (port-attribute? port 'output))
(define (textual-port? port) (port-attribute? port 'textual))
(define (binary-port? port) (port-attribute? port 'binary))
(define (port? port) (port-attribute? port 'port))

(define (global-error-handler obj) (write (string-append "unhandled exception: " (display-error-object obj))))
(define (display-error-object err)
   (if (and (pair? err) (eq? (car err) '#error-object)) (cadr err) (if (symbol? err) (symbol->string err) "unknown")))
   
(define (raise obj) (lith-raise-exception obj))
(define (error message . xs) (raise (list '#error-object message xs)))
