(defmacro let (bindings expr)
  ((lambda (map car ,bindings) expr) (map cadr ,bindings)))

(defun my-last (xs) (cond ((null xs) nil)
                          ((null (cdr xs)) (car xs))
                          (t (my-last (cdr xs)))))
                          
(defun my-but-last (xs) (cond ((null xs) nil)
                              ((null (cdr xs)) nil)
                              ((null (cddr xs)) xs)
                              (t (my-but-last (cdr xs)))))
                              
(defun element-at (xs n) (cond ((< 1 n) nil)
                               ((null xs) nil)
                               ((eq n 1) (car xs))
                               (t (element-at (cdr xs) (- n 1)))))

(defun my-length (xs) (cond ((null xs) 0)
                            (t (+ 1 (my-length (cdr xs))))))
                            
(defun my-reverse (xs) 
    (let (r (lambda (ys acc) (cond ((null ys) acc)
                                   (t (r (cdr ys) (cons (car ys) acc))))))
         (r xs '())))

(defun is-palindrome (xs) (equal xs (my-reverse xs)))

(defun my-flatten (xs) (cond ((null xs) nil)
                             ((atom xs) xs)
                             (t (append (my-flatten (car xs)) (my-flatten (cdr xs))))))

(defun compress (xs) (cond ((null xs) nil)
                           ((null (cdr xs)) xs)
                           ((eq (car xs) (cadr xs)) (compress (cdr xs)))
                           (t (cons (car xs) (compress (cdr xs))))))
                           

(defun take-while (p xs) 
    (let (tw (lambda (ys acc)
                  (cond ((null ys) (list acc))
                        ((p (car ys)) (tw (cdr ys) (cons (car ys) acc)))
                        (t (list acc ys)))))
         (tw xs '())))

(defun pack (xs) (cond ((null xs) nil)
                       (t (let (tw (take-while (lambda (x) (eq x (car xs))) xs)
                                   (cons (car tw) (pack (cdr tw))))))))

(defun encode (xs) (map (lambda (xs) (list (length xs) (car xs))) (pack xs)))

(defun encode-modified (xs) (map (lambda (xs) (if (eq (car xs) 1) (cadr xs) xs)) (encode xs)))

(defun decode (xs)
  (my-flatten (map (lambda (x) (cond ((atom x) x)
				     (t (repeat (car x) (cadr x))))
		     xs))))

