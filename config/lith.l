(lith-define nil '())
(lith-define list (lambda x x))
(lith-define define 
   (macro (name-and-args body)
      (if (pair? name-and-args)
          (list 'lith-define (car name-and-args) (list 'lambda (cdr name-and-args) body))
          (list 'lith-define name-and-args ((lambda (x) x) (if pair? body (cons 'begin body) body))))))

(lith-define defmacro (macro (name args body) `(define ,name (macro ,args ,body))))

(lith-define trace (macro (expr) `(begin (set! *trace-eval* #t) ,expr (set! *trace-eval* #f))))