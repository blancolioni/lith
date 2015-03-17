(define *total-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)
(define *show-all-tests* #f)

(define (reset-counts)
  (begin (set! *total-count* 0)
         (set! *pass-count* 0)
         (set! *fail-count* 0)))
         
(define test (macro (expr) 
  `(begin (if *show-all-tests* (begin (display (quote ,expr)) (newline)) #f)
          (if ,expr (set! *pass-count* (+ 1 *pass-count*)) 
             (begin (display (quote ,expr)) (write-string ":\t") (display ,expr) (newline)
                    (set! *fail-count* (+ 1 *fail-count*)))))))

(define (unit-test-all)
  (begin (reset-counts)
         (unit-test-symbol)
         (unit-test-numbers)
         (unit-test-large-numbers)
         (set! *total-count* (+ *pass-count* *fail-count*))
         (display *fail-count*) (write-string "/") (display *total-count*) (write-string " tests failed\n")))
  
(define (unit-test-symbol)
  (begin
    (test (symbol? 'foo))
    (test (symbol? (car '(a b))))
    (test (not (symbol? "bar")))
    (test (symbol? 'nil))
    (test (not (symbol? '())))
    (test (not (symbol? #f)))))

(define (unit-test-numbers)
   (begin
      (test (eqv? (max 3 4) 4))
      (test (eqv? (min 3 4) 3))
      (test (eqv? (- 3 4) (- 1)))
      (test (eqv? (- 3 4 5) (- 6)))
      (test (equal? (floor/ 5 2) '(2 1)))
      (test (eqv? (abs (- 7)) 7))
      (test (eqv? (gcd 32 36) 4))
      (test (eqv? (gcd) 0))
      (test (eqv? (lcm 32 36) 288))
      (test (eqv? (lcm) 1))
      (test (eqv? (square 42) 1764))
      (test (eqv? (square (- 42)) 1764))
      ))

(define (unit-test-large-numbers)
   (begin
      (test (eqv? (floor-quotient 123123123123123 10000000) 12312312))
      ))

      