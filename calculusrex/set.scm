(define-module (calculusrex set)
  :export (set
	   makeset
	   set-difference))

(use-modules (calculusrex list))

(define (set xs)
  (if (null? xs) '()
      (cons (car xs)
	    (set
	     (remove-element (car xs)
			     (cdr xs))))))

;; (define (set- xs)
;;   (cond ((null? xs) '())
;; 	((member (car xs) (cdr xs))
;; 	 (set- (cdr xs)))
;; 	(else
;; 	 (cons (car xs)
;; 	       (set- (cdr xs))))))

;; (define (set-- xs)
;;   (cond ((null? xs) '())
;; 	((member (car xs) (cdr xs))
;; 	 (cons (car xs)
;; 	       (set--
;; 		(remove-element (car xs)
;; 				(cdr xs)))))
;; 	(else
;; 	 (cons (car xs)
;; 	       (set-- (cdr xs))))))

(define (makeset lat)
  (cond ((null? lat) '())
	(else (cons (car lat)
		    (makeset (multirember (car lat)
					  (cdr lat)))))))


(define (set-difference xs ys)
  (cond ((null? xs) ys)
	((in (car xs) ys)
	 (set-difference (cdr xs)
			 (remove-element (car xs)
					 ys)))
	(else
	 (cons (car xs)
	       (set-difference (cdr xs) ys)))))
