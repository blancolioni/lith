(define test (macro (expr) `(begin (display (quote ,expr)) (write-string ":\t") (display ,expr) (newline))))
(define (unit-test-all)
  (begin (unit-test-symbol)))
  
(define (unit-test-symbol)
  (begin
    (test (symbol? 'foo))
    (test (symbol? (car '(a b))))
    (test (not (symbol? "bar")))
    (test (symbol? 'nil))
    (test (not (symbol? '())))
    (test (not (symbol? #f)))))
