(define-module (calculusrex string)
  :export (string-strip))


(define* (string-strip strng #:optional #:key
		       (chars '(#\space #\newline #\tab))
		       (additional-chars '()))
  (let ((chars-to-strip (append chars additional-chars))
	(lngth (string-length strng)))
    (let ((start-index
	   (let recur ((n 0))
	     (cond ((or (>= n lngth)
			(not (member (string-ref strng n)
				     chars-to-strip)))
		    n)
		   (else (recur (1+ n))))))
	  (stop-index
	   (let recur ((n (1- lngth)))
	     (cond ((or (< n 0)
			(not (member (string-ref strng n)
				     chars-to-strip)))
		    n)
		   (else (recur (1- n)))))))
      (if (> start-index stop-index)
	  ""
	  (substring strng start-index (1+ stop-index))))))

;; (string-strip "\" boom \"" #:additional-chars '(#\"))
;; (string-strip "\"b\"" #:additional-chars '(#\"))
;; (string-strip "\"  \"" #:additional-chars '(#\"))


