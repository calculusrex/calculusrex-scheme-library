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
	     (calculusrex numbers)
	     (calculusrex error)
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
    (eqv? sep (peek-char port))))

(define (double-quote? char)
  (eqv? char #\"))

(define (alphabetic? char)
  (char-set-contains? char-set:letter
		      char))

(define (digit? char)
  (char-set-contains? char-set:digit
		      char))

(define (imminent-number? char)
  (or (in? char (string->list "./eE-+i%"))
      (digit? char)))

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
			 ((imminent-number? init-char)
			  (number-parser
			   sep))
			 (else
			  (symbol-parser sep)))))
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


;; (define (row-parser sep delim)
;;   (let ((parse-value (value-parser sep delim))
;; 	(imminent-delim?
;; 	 (λ (port)
;; 	   (eqv? delim (peek-char port)))))
;;     (λ (port)
;;       (let recur ()
;; 	(cond ((eof-object? (peek-char port))
;; 	       '())
;; 	      ((imminent-delim? port)
;; 	       (begin
;; 		 (read-char port)
;; 		 '()))
;; 	      (else
;; 	       (cons (let ((value (parse-value port)))
;; 		       (display value) (display " ")
;; 		       value) ;; DEBUG
;; 		     (recur))))))))



(define (row-parser- sep delim)
  (let ((parse-value (value-parser sep delim))
	(imminent-delim?
	 (λ (port)
	   (eqv? delim (peek-char port))))
	(collector
	 (λ (values) values)))
    (λ (port)
      (let recur ((collector collector))
	(cond ((eof-object? (peek-char port))
	       (collector '()))
	      ((imminent-delim? port)
	       (begin
		 (read-char port)
		 (collector '())))
	      (else
	       (let ((value (parse-value port)))
		 (recur
		  (λ (values)
		    (collector 
		     (cons value
			   values)))))))))))


(define (row-parser-- sep delim)
  (let ((parse-value (value-parser sep delim))
	(imminent-delim?
	 (λ (port)
	   (eqv? delim (peek-char port))))
	(collector
	 (λ (values n-values) (cons values n-values))))
    (λ (port)
      (let recur ((collector collector))
	(cond ((eof-object? (peek-char port))
	       (collector '() 0))
	      ((imminent-delim? port)
	       (begin
		 (read-char port)
		 (collector '() 0)))
	      (else
	       (let ((value (parse-value port)))
		 (recur
		  (λ (values n-values)
		    (collector
		     (cons value values)
		     (1+ n-values)))))))))))


(define (csv-parser sep delim)
  (let ((parse-row (row-parser sep delim)))
    (λ (port)
      (let recur ()
	(if (eof-object? (peek-char port))
	    '()
	    (cons (parse-row port)
		  (recur)))))))


;; (define (csv-parser sep delim)
;;   (let ((parse-row (row-parser sep delim)))
;;     (λ (port)
;;       (let recur ()
;; 	(if (eof-object? (peek-char port))
;; 	    '()
;; 	    (cons (let ((values (parse-row port)))
;; 		    (display values) (newline)
;; 		    values) ; DEBUG
;; 		  (recur)))))))
 

(define (csv-parser-- sep delim)
  (let ((parse-row (row-parser-- sep delim))
	(collector
	 (λ (rows row-lengths)		; columns too maybe
	   `((rows . ,(cdr rows))
	     (row-lengths . ,row-lengths)))))
    (λ (port)
      (let recur ((collector collector))
	(if (eof-object? (peek-char port))
	    (collector '() '())
	    (let ((row-data (parse-row port)))
	      (let ((values (car row-data))
		    (n-values (cdr row-data)))
		(recur
		 (λ (rows row-lengths)
		   (collector
		    (cons values rows)	; rows
		    (cons n-values row-lengths)))))))))))


(define (read-csv fname sep delim)
  (let ((parse-csv (csv-parser sep delim)))
    (let ((proc
	   (λ (port)
	     (parse-csv port))))
      (call-with-input-file fname proc))))
	       

;; (define (read-csv-- fname sep delim)
;;   (let ((parse-csv (csv-parser-- sep delim)))
;;     (let ((proc
;; 	   (λ (port)
;; 	     (let ((csv-data (parse-csv port)))
;; 	       (let ((rows
;; 		      (assq-ref csv-data 'rows))
;; 		     (row-lengths
;; 		      (assq-ref csv-data 'row-lengths)))
;; 		 (if (not (all-equal? row-lengths))
;; 		     (error "rows of unequal size")
;; 		     (let ((header (car rows))
;; 			   (columns (transpose rows)))
;; 		       `((header . ,header)
;; 			 (columns . ,columns)
;; 			 (rows . ,rows)))))))))
;;       (call-with-input-file fname proc))))

(define (read-csv-- fname sep delim)
  (let ((parse-csv (csv-parser-- sep delim)))
    (let ((proc
	   (λ (port)
	     (let ((csv-data (parse-csv port)))
	       (let ((rows
		      (assq-ref csv-data 'rows))
		     (row-lengths
		      (assq-ref csv-data 'row-lengths)))
		 (if (not (all-equal? row-lengths))
		     (error "rows of unequal size")
		     (let ((header (car rows))
			   (columns (transpose rows)))
		       `((header . ,header)
			 (columns . ,columns)
			 (rows . ,rows)))))))))
      (call-with-input-file fname proc))))


;; testing ----------------

;; (define csv-fnames
;;   (let ((cwd (getcwd))
;; 	(folder "date-test-saga/ardeleanu--20-06-2022")
;; 	(articole "intrari_articole_ardeleanu__20_06_2022.csv")
;; 	(facturi "intrari_facturi_ardeleanu__20_06_2022.csv"))
;;     '((articole . ,(string-join (list folder articole) "/"))
;;       (facturi . ,(string-join (list folder facturi) "/")))))


(define csv-fname
  "date-test-saga/ardeleanu--20-06-2022/intrari_articole_ardeleanu__20_06_2022.csv")

;; (define csv-fname "test-csv.csv")

(define test-port
  (open-input-file
   csv-fname))

(define csv-data
  (read-csv-- csv-fname #\, #\newline))

;; (define parse-row (row-parser #\, #\newline))
;; ;; (display (parse-row test-port))


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

