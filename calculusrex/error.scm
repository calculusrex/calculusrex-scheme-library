(define-module (calculusrex error)
  :export (screaming-error))



(define (screaming-error text)
  (let* ((signature "- CALCULUSREX -")
	 (dashline (list->string
		    (let recur ((n 16))
		      (if (zero? n) '()
			  (cons #\- (recur (1- n)))))))
	 (scream
	  (string-join
	   (list "\n" signature "\n"
		 dashline "\n"
		 " - error - " text "\n"
		 dashline "\n"
		 signature)
	   "")))
    (error scream)))
