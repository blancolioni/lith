(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define define-syntax (macro (name rules) 
    (list 'define name (list 'macro 'xs (list 'apply-syntax 'xs rules))))) ; (list 'macro 'xs (list 'apply-syntax 'xs rules)))))
    
; `(define ,name (macro xs (apply-syntax (xs ,rules))))))

(define-syntax lith-and
  (syntax-rules () ((lith-and) #t) ((lith-and test) test) ((lith-and test1 test2 ...) (if test1 (lith-and test2 ...) #f))))
               
; (define (match-syntax-rules call rules)
  ; (let ((keywords (cadr rules))
        ; (patterns (cddr rules)))
      ; (match-patterns call keywords patterns)))
      
; (define (match-patterns call keywords patterns)
  ; (if (null? patterns) (error "no matching patterns")
      ; (let ((env (match-pat call keywords (caar patterns) '())))
           ; (if env (apply-pat keywords (car (cdar patterns)) env)
               ; (match-patterns call keywords (cdr patterns))))))
               
; (define (match-pat call keywords pat env)
  ; (if (and (null? call) (null? pat)) 
      ; env
      ; (if (null? pat) 
          ; #f
          ; (if (eq? (car pat) '...) 
              ; (cons (list '&rest call) env)
              ; (if (null? call)
                  ; #f
                  ; (match-pat (cdr call) keywords (cdr pat) (cons (list (car pat) (car call)) env)))))))
               
; (define (apply-pat keywords body env)
   ; (if (pair? body) (cons (apply-pat (car body)) (apply-pat (cdr body)))
       ; (if (eq? body '...) (apply-pat-rest env)
           ; (if (symbol? body)
               ; (apply-pat-sub body env)
               ; body))))
           
; (define (apply-pat-sub sym env)
   ; (if (null? env) sym
       ; (if (eq? (caar env) sym) (car (cdar env))
           ; (apply-pat-sub sym (cdr env)))))
           

(define and (macro (x y) `(if ,x ,y ,x)))
(define or (macro (x y) `(if ,x ,x ,y)))
(define (not x) (if x #f #t))

; (define (set-car! xs x) (list-set! xs 0 (cons x (cdr xs))))
(define (list-set! xs k x) (if (zero? k) (set-car! xs x)
                               (list-set! (cdr xs) (- k 1) x)))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f)))

(define (eqv? x y)
  (cond ((eq? x y) #t)
        ((and (number? x) (number? y)) (equal? x y))))   ; works because equal numbers have the same list representation

(define (boolean? x) (or (eq? x #f) (eq? x #t)))

(define (number? x) (or (integer? x) (real? x)))  ; the only numbers we have :(

(define (complex? x) #f)
(define (rational? x) #f)

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

(define (list-copy xs) (if (pair? xs) (cons (car xs) (list-copy (cdr xs))) xs))

(define (let*2let bindings expr)
  (if (null? bindings) expr
    (list 'let (list (car bindings)) (let*2let (cdr bindings) expr))))

(define let* (macro (bindings expr) (let*2let bindings expr)))
  
(define (if2cond clauses)
  (if (null? clauses) nil
    (if (eq? (caar clauses) 'else) (car (cdar clauses))
      (list 'if (caar clauses) (car (cdar clauses)) (if2cond (cdr clauses))))))

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

(define (gcd . xs) (run-gcd xs 0))
(define (run-gcd xs acc)
  (if (null? xs) acc
       (run-gcd (cdr xs) (gcd-2 (car xs) acc))))
(define (gcd-2 x y)       
  (if (eq? y 0)
      x
      (gcd-2 y (floor-remainder x y))))

(define (lcm . xs) (run-lcm xs 1))
(define (run-lcm xs acc)
  (if (null? xs) acc
      (run-lcm (cdr xs) (lcm-2 (car xs) acc))))
      
(define (lcm-2 x y) (floor-quotient (* x y) (gcd x y)))

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
       (minmax (if (compare (car rest) curr) (car rest) curr) (cdr rest))))

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
(define string=? equal?)

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
  
(define (newline) (write-char #\newline))
(define (write x) (begin (display x) (newline)))

(define (do-string-append xs)
  (if (null? xs) ""
    (if (null? (cdr xs)) (car xs)
      (append (car xs) (cdr (do-string-append (cdr xs)))))))

(define string-append (lambda xs (do-string-append xs)))

(define (call-with-port port proc)
  (begin (proc port)
         (close-port port)))

(define (input-port? port) (port-attribute? port 'input))
(define (output-port? port) (port-attribute? port 'output))
(define (textual-port? port) (port-attribute? port 'textual))
(define (binary-port? port) (port-attribute? port 'binary))
(define (port? port) (port-attribute? port 'port))

(define (raise obj) (lith-raise-exception obj))
