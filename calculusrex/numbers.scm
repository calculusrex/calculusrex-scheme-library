(define-module (calculusrex numbers)
  :export (all-equal?
	   all-equal?--))




(define all-equal?
  (λ (ns)
    (let ((a (car ns)))
      (let recur ((ns (cdr ns)))
	(if (null? ns) #t
	    (or (eqv? (car ns) a)
		(recur (cdr ns))))))))
