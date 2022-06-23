(define-module (calculusrex list)
  :export (in?
	   range
	   transpose
	   zip
	   reduce
	   remove-element
	   multirember
	   path-join
	   frep
	   all-satisfying?
	   all-different?
	   flatten))

;; (load "/home/feral/.guile")

(use-modules (ice-9 regex)
	     (calculusrex functools))

(define (in? x xs)
  (cond ((null? xs) #f)
	((equal? x (car xs)) #t)
	(else
	 (in? x (cdr xs)))))

(define (path-join strings)
  (string-join strings "/"))

(define range
  (λ xs
    (cond ((= (length xs) 1)
	   (let recur ((a 0) (b (car xs)))
	     (if (>= a b) '()
		 (cons a (recur (1+ a) b)))))
	  ((= (length xs) 2)
	   (let recur ((a (car xs)) (b (cadr xs)))
	     (if (>= a b) '()
		 (cons a (recur (1+ a) b))))))))

(define (transpose xss)
  (if (null? (car xss))
      '()
      (cons (map car xss)
	    (transpose (map cdr xss)))))

(transpose (let recur ((n 4))
	     (if (< n 1) '()
		 (cons (range 6) (recur (1- n))))))

(define (reduce f xs)
  (if (null? (cdr xs))
      (car xs)
      (f (car xs)
	 (reduce f (cdr xs)))))

(define (zip xs ys)
  (if (and (null? xs)
	   (null? ys))
      '()
      (cons (cons (car xs) (car ys))
	    (zip (cdr xs) (cdr ys)))))

(define (remove-element elem xs)
  (cond ((null? xs) '())
	((equal? (car xs) elem)
	 (remove-element elem (cdr xs)))
	(else
	 (cons (car xs)
	       (remove-element elem (cdr xs))))))


(define (frep pattern elements)
  (if (null? elements)
      '()
      (let ((elem (car elements)))
	(let ((string
	       (cond ((string? elem) elem)
		     ((symbol? elem)
		      (symbol->string elem))
		     ((number? elem)
		      (number->string elem)))))
	  (if (string-match pattern string)
	      (cons string
		    (frep pattern (cdr elements)))
	      (frep pattern (cdr elements)))))))


(define (all-satisfying? predicate)
  (λ (xs)
    (fold (λ (a b) (and a b))
	  (map predicate xs))))

(define (all-different? xs)
  (cond ((null? xs) #t)
	((in? (car xs) (cdr xs)) #f)
	(else
	 (all-different? (cdr xs)))))

(define (flatten xss)
  (apply append xss))
