(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(define *db* nil)

(defun add-record (cd) (push cd *db*))
(defun select-by-artist (artist)
  (filter
   (lambda (cd) (eq (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (filter selector-fn *db*))
  
(defun where (&key title artist rating (ripped nil ripped-p))
    (lambda (cd)
      (and
       (if title    (eq (getf cd :title)  title)  t)
       (if artist   (eq (getf cd :artist) artist) t)
       (if rating   (eq (getf cd :rating) rating) t)
       (if ripped-p (eq (getf cd :ripped) ripped) t))))
       
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

(defun where1 (&key title artist rating (ripped nil ripped-p))
    (lambda (cd) (if title title t)))
    
(defun try (x) (eq x "Title"))
