(define *total-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)
(define *show-all-tests* #f)

(define (reset-counts)
  (set! *total-count* 0)
  (set! *pass-count* 0)
  (set! *fail-count* 0))
         
(define test (macro (expr) 
  `(begin (if *show-all-tests* (begin (display (quote ,expr)) (newline)) #f)
          (if ,expr (set! *pass-count* (+ 1 *pass-count*)) 
             (begin (display (quote ,expr)) (write-string ":\t") (display ,expr) (newline)
                    (set! *fail-count* (+ 1 *fail-count*)))))))

(define (unit-test-all)
  (reset-counts)
  (unit-test-symbol)
  (unit-test-boolean)
  (unit-test-numbers)
  (unit-test-large-numbers)
  (unit-test-inexact)
  (unit-test-characters)
  (unit-test-strings)
  (unit-test-exceptions)
  (unit-test-functions)
  (unit-test-vectors)
  (set! *total-count* (+ *pass-count* *fail-count*))
  (display *pass-count*) (write-string "/") (display *total-count*) (write-string " tests passed\n"))
  
(define (unit-test-symbol)
    (test (symbol? 'foo))
    (test (symbol? (car '(a b))))
    (test (not (symbol? "bar")))
    (test (symbol? 'nil))
    (test (not (symbol? '())))
    (test (not (symbol? #f))))

(define (unit-test-boolean)
    (test (boolean? #t))
    (test (boolean? #f))
    (test (not (boolean? '())))
    (test (not (boolean? 1)))
    (test (boolean=? #t #t))
    (test (boolean=? #f #f))
    (test (not (boolean=? #t 77))))
    
(define (unit-test-numbers)
      (test (eqv? (max 3 4) 4))
      (test (eqv? (min 3 4) 3))
      (test (eqv? (- 3 4) (- 1)))
      (test (eqv? (- 3 4 5) (- 6)))
      (test (eqv? (- 3 4) -1))
      (test (eqv? (- 3 4 5) -6))
      (test (equal? (floor/ 5 2) '(2 1)))
      (test (eqv? (abs (- 7)) 7))
      (test (eqv? (gcd 32 36) 4))
      (test (eqv? (gcd) 0))
      (test (eqv? (lcm 32 36) 288))
      (test (eqv? (lcm) 1))
      (test (eqv? (square 42) 1764))
      (test (eqv? (square (- 42)) 1764))
      (test (eqv? 65535 #xFFFF))
      (test (eqv? #x12345678790ABcdEF 20988295470375489007))
      (test (eqv? #b10101010100101010101 #xAA955))
      (test (eqv? -21342134324 (- 21342134324)))
      (test (equal? (exact-integer-sqrt 4) '(2 0)))
      (test (equal? (exact-integer-sqrt 5) '(2 1)))
      (test (equal? (exact-integer-sqrt 0) '(0 0)))
      (test (eqv? (* 11237612837621873621873 0) 0))
      )

(define (unit-test-large-numbers)
      (test (exact? (fac 100)))
      (test (eqv? 123123123123123 123123123123123))
      (test (eqv? (fac 100) (* 100 (fac 99))))
      (test (eqv? (floor-quotient 123123123123123 10000000) 12312312))
      (test (equal? (number->string 1234567890987654321) "1234567890987654321"))
      )

(define (unit-test-inexact)
      (test (inexact? 1.5))
      (test (inexact? 10.0e3))
      (test (<= 1 (* 1.5 0.9)))
      (test (eqv? (max 1.1 2.2 3.3 1.2) 3.3))
      (test (eqv? -1.4 (- 1.4)))
      )

(define (unit-test-strings)
      (test (string? "hello"))
      (test (not (string? 'hello)))
      (test (string=? (make-string 5 #\X) "XXXXX"))
      (test (string=?))
      (test (string=? "asdasd"))
      (test (string=? "hello" "hello"))
      (test (string=? "hello" "hello" "hello" "hello"))
      (test (not (string=? "hello" "hello" "hello" "hellox")))
      (test (string=? "hello" (string-copy "hello")))
      (test (string=? "hello" (string-copy "hello" 0)))
      (test (string=? "hello" (string-copy "erm, hello" 5)))
      (test (string=? "hello" (string-copy "hello world" 0 4)))
      (test (string=? "hello" (substring "erm, hello world" 5 9)))
   )
   
(define (unit-test-characters)
      (test (eqv? (char->integer #\alarm) 7))
      (test (eqv? (char->integer #\backspace) 8))
      (test (eqv? (char->integer #\delete) 127))
      (test (eqv? (char->integer #\escape) 27))
      (test (eqv? (char->integer #\newline) 10))
      (test (eqv? (char->integer #\null) 0))
      (test (eqv? (char->integer #\return) 13))
      (test (eqv? (char->integer #\space) 32))
      (test (eqv? (char->integer #\tab) 9))
      (test (eqv? (char->integer #\ ) 32))
      (test (eqv? (char->integer #\0) 48))
      (test (char<=? #\A #\B #\C #\C))
      (test (not (char>=? #\A #\B #\C)))
      (test (char<? #\A #\B #\C))
      (test (not (char>? #\A #\B #\C)))
      (test (char=? #\C #\C #\C))
      )
      
(define (unit-test-exceptions)
      (test (eqv? (dynamic-wind 1 2 3) 2))
   )
   
(define (unit-test-functions)
      (test (eqv? ((lambda (x) (+ x 1) (+ x 2)) 1) 3))
      (test (eqv? (multi-expr-body 4) 6))
   )

(define (unit-test-vectors)
      (test (vector? (vector 1 2 3)))
      (test (not (vector? '(1 2 3))))
      (test (vector? #(1 2 3)))
      (test (eqv? (vector-length #(1 2 3)) 3))
      (test (eqv? (vector-ref #(1 2 3) 1) 2))
   )

(define (multi-expr-body x) (+ x 1) (+ x 2))
   
(define (fac n) (if (zero? n) 1 (* n (fac (- n 1)))))
