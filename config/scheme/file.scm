(define (call-with-output-file identity proc)
  (let ((port (open-output-file identity)))
       (call-with-port port proc)))

(define (call-with-input-file identity proc)
  (let ((port (open-input-file identity)))
       (call-with-port port proc)))
