;; (define-module (calculusrex csv)
;;   :export (read-csv
;; 	   ref
;; 	   df-header
;; 	   df-rows
;; 	   row-at-index
;; 	   df-columns
;; 	   column-at-key
;; 	   value-at-key
;; 	   ))
;; ;; 	   row-at-value))

;; (load "/home/feral/.guile")

(use-modules (calculusrex string)
	     (calculusrex list)
	     ;; (ice-9 rdelim)
	     )

;; (define* (read-csv fname
;; 		   #:optional
;; 		   (sep #\,)
;; 		   (strip-string-quotation #t)
;; 		   (header-present #t))
;;   (call-with-input-file fname
;;     (λ (file-port)
;;       (let ((split-line
;; 	     (λ (cs)
;; 	       (let ((elements (string-split cs (λ (c) (eq? c sep)))))
;; 		 (if strip-string-quotation
;; 		     (map (λ (element)
;; 			    (string-strip
;; 			     element #:additional-chars '(#\")))
;; 			  elements)
;; 		     elements)))))
;; 	(let* ((header
;; 		(if header-present
;; 		    (split-line
;; 		     (read-line file-port))
;; 		    '()))
;; 	       (columns (transpose
;; 			 (cons (map string->symbol header)
;; 			       (let recur ()
;; 				 (if (eof-object? (peek-char file-port))
;; 				     '()
;; 				     (cons (split-line (read-line file-port))
;; 					   (recur))))))))
;; 	  `((header . ,(map string->symbol header))
;; 	    (columns . ,columns)))))))


(define (whitespace? char)
  (char-set-contains? char-set:whitespace
		      char))

(define (whitespace-predicate sep delim)
  (let ((char-set:sep-delim
	 (list->char-set (list sep delim))))
    (λ (port)
      (let ((char (peek-char port)))
	(and (not (char-set-contains? char-set:sep-delim
				      char))
	     (char-set-contains? char-set:whitespace
				 char))))))

(define (whitespace-consumer imminent-whitespace?)
  (λ (port)
    (let recur ()
      (if (imminent-whitespace? port)
	  (begin
	    (read-char port)
	    (recur))
	  port))))

(define (consume-whitespace port)
  (let recur ()
    (if (whitespace? (peek-char port))
	(begin
	  (read-char port)
	  (recur))
	port)))

(define (new-value-predicate sep)
  (λ (port)
    (eqv? sep (peek-chr port))))

(define (double-quote? char)
  (eqv? char #\"))

(define (alphabetic? char)
  (char-set-contains? char-set:letter
		      char))

(define (digit? char)
  (char-set-contains? char-set:digit
		      char))

(define (parse-string port)
  (read-char port) ;; to get rid of the doublequote char
  (let ((string-chars
	 (let recur ()
	   (let ((char (peek-char port)))
	     (if (double-quote? char)
		 (begin
		   (read-char port)
		   '())
		 (cons (read-char port)
		       (recur)))))))
    (list->string string-chars)))

(define (symbol-parser sep)
  (λ (port)
    (let ((string-chars
	   (let recur ()
	     (if (eof-object? (peek-char port))
		 '()
		 (let ((char (peek-char port)))
		   (if (or (eqv? char sep)
			   (whitespace? char))
		       '()
		       (cons (read-char port)
			     (recur))))))))
      (string->symbol
       (list->string
	string-chars)))))

(define (number-parser sep)
  (λ (port)
    (let ((number-chars
	   (let recur ()
	     (if (eof-object? (peek-char port))
		 '()
		 (let ((char (peek-char port)))
		   (if (or (eqv? char sep)
			   (whitespace? char))
		       '()
		       (cons (read-char port)
			     (recur))))))))
      (string->number
       (list->string number-chars)))))

(define (value-parser sep delim)
  (let ((whitespace?
	 (λ (char)
	   (and (not (eqv? char delim))
		(char-set-contains? char-set:whitespace
				    char)))))
    (let ((consume-whitespace
	   (λ (port)
	     (let recur ()
	       (cond ((eof-object? (peek-char port))
		      port)
		     ((whitespace? (peek-char port))
		      (begin
			(read-char port)
			(recur)))
		      (else port)))))
	  (consume-sep
	   (λ (port)
	     (let recur ()
	       (if (eof-object? (peek-char port))
		   port
		   (let ((char (peek-char port)))
		     (if (or (eqv? char sep)
			     (whitespace? char))
			 (begin
			   (read-char port)
			   (recur))
			 port)))))))
      (λ (port)
	(consume-sep port)
	(let ((init-char (peek-char port)))
	  (let ((value
		 (let ((parse
			(cond
			 ((double-quote? init-char)
			  parse-string)
			 ((alphabetic? init-char)
			  (symbol-parser sep))
			 ((digit? init-char) (number-parser
					      sep)))))
		   (parse port))))
	    (consume-whitespace port)
	    value))))))


;; (define (row-parser sep delim)
;;   (let ((parse-value (value-parser sep delim))
;; 	(imminent-delim?
;; 	 (λ (port)
;; 	   (eqv? delim (peek-char port)))))
;;     (λ (port)
;;       (let recur ((collector
;; 		   (λ (vals val-n) (cons vals val-n))))
;; 	(if (imminent-delim? port)
;; 	    (collector '() 0)
;; 	    (begin
;; 	      (display (peek-char port))
;; 	      (recur
;; 	       (λ (vals val-n)
;; 		 (collector
;; 		  (cons (parse-value port)
;; 			vals)
;; 		  (1+ val-n))))))))))


(define (row-parser sep delim)
  (let ((parse-value (value-parser sep delim))
	(imminent-delim?
	 (λ (port)
	   (eqv? delim (peek-char port)))))
    (λ (port)
      (let recur ()
	(cond ((eof-object? (peek-char port))
	       '())
	      ((imminent-delim? port)
	       (begin
		 (read-char port)
		 '()))
	      (else
	       (cons (parse-value port)
		     (recur))))))))


(define (row-parser-- sep delim)
  (let ((parse-value (value-parser sep delim))
	(imminent-delim?
	 (λ (port)
	   (eqv? delim (peek-char port))))
	(end-of-the-line '('() 0)))
    (λ (port)
      (let recur ((collector (λ (values n-values)
			       (cons values n-values))))
	(cond ((eof-object? (peek-char port))
	       (apply collector end-of-the-line))
	      ((imminent-delim? port)
	       (begin
		 (read-char port)
		 (apply collector end-of-the-line)))
	      (else
	       (recur
		(λ (values n-values)
		  (collector
		   (cons (parse-value port)
			 values)
		   (1+ n-values))))))))))

(define (csv-parser sep delim)
  (let ((parse-row (row-parser sep delim)))
    (λ (port)
      (let recur ()
	(if (eof-object? (peek-char port))
	    '()
	    (cons (parse-row port)
		  (recur)))))))
 
;; testing ----------------

(define test-port
  (open-input-file
   "test-csv.csv"))

(define parse-row (row-parser #\, #\newline))
;; (display (parse-row test-port))


;; (define (predicates-x-parsers sep delim
;; 				       strip-string-quotation)
;;   (let ((imminent-whitespace? (whitespace-predicate sep delim))
;; 	(new-value? (new-value-predicate sep))
;; 	(new-row? (new-row-predicate delim)))
;;     `((,imminent-whitespace? . consume-whitespace)
;;       (,new-value? . (new-value-parser sep))
;;       (,new-row? . (new-row-parser delim))
;;       (,imminent-string? . ,(string-parser
;; 			     strip-string-quotation))
;;       (,imminent-number? . ,parse-number)
;;       (,imminent-symbol? . ,parse-symbol))))

;; (define* (read-csv fname
;; 		   #:optional
;; 		   (sep #\,)
;; 		   (delim #\newline)
;; 		   (strip-string-quotation #t)
;; 		   (header-present #t))
;;   (let ((proc
;; 	 (λ (port)
;; 	   (let ((imminent-delim?
;; 		  (λ (port)
;; 		    (eqv? delim
;; 			  (peek-char)))))
;; 	     (let recur ()
;; 	       (if (imminent-delim? port)


	   
;;     (call-with-input-file fname proc)




;; (define (df-header df)
;;   (assq-ref df 'header))

;; (define (df-rows df)
;;   (cdr (transpose (assq-ref df 'columns))))

;; (define (row-at-index df id)
;;   (list-ref (df-rows df) id))

;; (define (df-columns df)
;;   (assq-ref df 'columns))

;; (define (column-at-key df key)
;;   (assq-ref (df-columns df) key))

;; (define (value-at-key df row column-key)
;;   (let recur ((columns (assq-ref df 'header))
;; 	      (row row))
;;     (let ((key (car columns))
;; 	  (value (car row)))
;;       (cond ((null? columns) #f)
;; 	    ((eq? key column-key)
;; 	     value)
;; 	    (else (recur (cdr columns)
;; 			 (cdr row)))))))

;; ;; (define (row-at-value df column value)
;; ;;   (let recur ((rows (transpose (assq-ref df 'rows))))
;; ;;     (cond ((null? rows) #f)
;; ;; 	  ((eq? 




;; ;; TESTING ------------------------------------------------

;; (define df
;;   (let* ((folder--date-saga "/home/feral/engineering/autoconta/transfer_date_mircea")
;; 	 (fnames--date-saga
;; 	  `((articole
;; 	     . ,(string-join
;; 		 `(,folder--date-saga "intrari_articole_ardeleanu__20_06_2022.csv")
;; 		 "/"))
;; 	    (facturi
;; 	     . ,(string-join
;; 		 `(,folder--date-saga "intrari_facturi_ardeleanu__20_06_2022.csv")
;; 		 "/")))))
;;     (read-csv (assq-ref fnames--date-saga 'articole))))


;; ;; (car (df-rows df))
;; ;; (assq-ref (df-columns df) 'pret_vanz)
;; ;; (row-at-index df 0)
;; ;; (df-header df)
;; ;; (column-at-key df 'pret_vanz)
;; ;; (value-at-key df
;; ;; 	      (row-at-index df 0)
;; ;; 	      'pret_vanz)
	      

;; ;; csv-lines

;; ;; (car (car (assq-ref csv-lines 'rows)))
;; ;; (assq-ref (assq-ref csv-lines 'rows) 'tip)

