(import (scheme base))

(define __exit #f)
(define (exit) (set! __exit #t))