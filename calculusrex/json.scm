(define-module (calculusrex json)
  :export (parse-json
	   read-json-file
	   parse-json-string
	   format-json
	   write-json-file))


(use-modules (ice-9 textual-ports))

(load "list.scm")

;; -------------------------------------------------------------
;; - PARSING

(define peek lookahead-char)
(define consume read-char) 

(define (in? x xs)
  (if (null? xs)
      #f
      (if (eqv? (car xs) x)
	  #t
	  (in? x (cdr xs)))))

(define (appl f x)
  (apply f (list x)))

(define (construct-datum port imminent-key?)
  (cons port imminent-key?))

(define (datum->port datum)
  (car datum))

(define (datum->key? datum)
  (cdr datum))

(define (datum->char datum)
  (let ((port (datum->port datum)))
    (peek port)))

(define (whitespace? char)
  (char-set-any (λ (c) (eqv? char c))
		char-set:whitespace))

(define (comma? char)
  (eqv? char #\,))

(define (consume-whitespace datum)
  (let ((port (datum->port datum))
	(char (datum->char datum)))
    (if (not (in? char '(#\space #\tab #\newline)))
	datum
	((lambda (a b) b) (consume port)
	 (consume-whitespace datum)))))

(define (trash-char datum)
  (let ((port (datum->port datum)))
    ((lambda (a b) b) (consume port) datum)))

'(array
  (bracketed-sepparated "[" "]" ",") -> #(value))

'(object
  (bracketed-sepparated "{" "}" ",") -> #((key . value))) / #(key)

;; the parse-value function receives the port with the distingiuishing character up front.
;; based on that first character it chooses what parsing-function to use for decoding.

  
(define (parse-string datum)
  (let* ((datum (trash-char datum))
	 (port (datum->port datum)))
    (letrec ((loop (lambda ()
		     (let inner-loop ((char (consume port)))
		       (if (eqv? char #\")
			   '()
			   (cons char
				 (inner-loop (consume port))))))))
      (list->string (loop)))))

(define (parse-symbol datum)
  (let ((port (datum->port datum)))
    (list->symbol
     (let recur ()
       (cond ((let ((c (peek port)))
		(or (comma? c)
		    (whitespace? c)))
	      '())
	     (else
	      (cons (consume port)
		    (recur))))))))

(define (parse-key datum)
  (string->symbol
   (parse-string datum)))

(define (parse-number datum)
  (let ((port (datum->port datum))
	(number-characters (string->list "1234567890./eE-+i%")))
    (letrec ((loop (lambda ()
		     (if (not (in? (peek port)
				   number-characters))
			 '()
			 (cons (consume port)
			       (loop))))))
      (string->number (list->string (loop))))))

(define (maybe-parse-value datum stop-characters)
  (let* ((datum (consume-whitespace datum))
	 (char (datum->char datum)))
    (if (in? char stop-characters)
	'nothing
	(parse-value datum))))
    

;; ;; OUTPUTS VECTOR NOT LIST
;; (define (parse-array datum)
;;   (letrec ((loop (lambda (datum)
;; 		   (let* ((datum (consume-whitespace datum))
;; 			  (val (maybe-parse-value datum '(#\, #\])))
;; 			  (char (datum->char (consume-whitespace datum))))
;; 		     (if (eqv? char #\])
;; 			 (cons val '())
;; 			 (if (eqv? char #\,)
;; 			     (cons val
;; 				   (loop (trash-char datum)))
;; 			     (cons val
;; 				   (loop datum))))))))
;;     (let* ((array (list->vector (loop (trash-char datum))))
;; 	   (closing-bracket (consume (datum->port datum))))
;;       array)))

(define (parse-array datum)
  (letrec ((loop (lambda (datum)
		   (let* ((datum (consume-whitespace datum))
			  (val (maybe-parse-value datum '(#\, #\])))
			  (char (datum->char (consume-whitespace datum))))
		     (if (eqv? char #\])
			 (cons val '())
			 (if (eqv? char #\,)
			     (cons val
				   (loop (trash-char datum)))
			     (cons val
				   (loop datum))))))))
    (let* ((array (loop (trash-char datum)))
	   (closing-bracket (consume (datum->port datum))))
      array)))

(define (maybe-parse-kvpair datum stop-characters)
  (let* ((datum (consume-whitespace datum))
	 (char (datum->char datum)))
    (if (in? char stop-characters)
	'nothing
	(let* ((key (let ((val (parse-value datum)))
		      (if (string? val)
			  (string->symbol val)
			  val)))
	       (char (datum->char (consume-whitespace datum))))
	  (if (eqv? char #\:)
	      (let ((value (parse-value (trash-char datum))))
		(cons key value))
	      (list key))))))
	      

;; ;; OUTPUTS VECTOR OF KEY-VELUE PAIRS, NOT AN ALIST
;; (define (parse-object datum)
;;   (letrec
;;       ((loop
;; 	(lambda (datum)
;; 	  (let* ((datum (consume-whitespace datum))
;; 		 (kvpair (maybe-parse-kvpair datum '(#\, #\})))
;; 		 (char (datum->char (consume-whitespace datum))))
;; 	    (if (eqv? char #\})
;; 		(cons kvpair '())
;; 		(if (eqv? char #\,)
;; 		    (cons kvpair
;; 			  (loop (trash-char datum)))
;; 		    (cons kvpair
;; 			  (loop datum))))))))
;;     (let* ((object (list->vector (loop (trash-char datum))))
;; 	   (closing-bracket (consume (datum->port datum))))
;;       object)))


(define (parse-object datum)
  (letrec
      ((loop
	(lambda (datum)
	  (let* ((datum (consume-whitespace datum))
		 (kvpair (maybe-parse-kvpair datum '(#\, #\})))
		 (char (datum->char (consume-whitespace datum))))
	    (if (eqv? char #\})
		(cons kvpair '())
		(if (eqv? char #\,)
		    (cons kvpair
			  (loop (trash-char datum)))
		    (cons kvpair
			  (loop datum))))))))
    (let* ((object (loop (trash-char datum)))
	   (closing-bracket (consume (datum->port datum))))
      object)))


(define (imminent-array? datum)
  (in? (datum->char datum)
       (list #\[)))

(define (imminent-object? datum)
  (in? (datum->char datum)
       (list #\{)))

(define (imminent-number? datum)
  (in? (datum->char datum)
       (string->list "1234567890")))

(define (imminent-key? datum)
  (and (in? (datum->char datum)
	    (list #\"))
       (datum->key? datum)))

(define (imminent-string? datum)
  (in? (datum->char datum)
       (list #\")))

(define (imminent-symbol? datum)
  (let ((char (datum->char datum)))
    (char-set-any (λ (c) (eqv? c char))
		  (char-set-union char-set:lower-case
				  char-set:upper-case))))

(define predicates (list imminent-array?
			 imminent-object?
			 imminent-number?
			 imminent-key?
			 imminent-symbol?
			 imminent-string?))

(define predicates-x-parsing-functions
  (list (cons imminent-array? parse-array)
	(cons imminent-object? parse-object)
	(cons imminent-number? parse-number)
	(cons imminent-key? parse-key)
	(cons imminent-symbol? parse-symbol)
	(cons imminent-string? parse-string)))


(define (parse-value datum)
  (let loop ((pxf predicates-x-parsing-functions)
	     (datum (consume-whitespace datum)))
    (if (null? pxf) (error "malformed or unsuported data feed")
	(let ((pair (car pxf)))
	  (let ((predicate (car pair))
		(pfunction (cdr pair)))
	    (if (predicate datum)
	    	(pfunction datum)
	    	(loop (cdr pxf)
		      datum)))))))
	      
 
(define (parse-json port)
  (let ((datum (construct-datum port #f)))
    (parse-value datum)))

(define (read-json-file fname)
  (parse-json (open-file fname "r")))

;; EVERYTHING WORKS, DAMN


(define (parse-json-string string)
  (let ((p (open-input-string string)))
    (parse-json p)))



;; ----------------------------------------------------------
;; - FORMATTING

(define (pairs-in-vector? data)
  (reduce (lambda (a b) (or a b))
	  (map pair?
	       (map (lambda (index) (vector-ref data index))
		    (range 0 (vector-length data))))
	  #f))

(define (json-array? data)
  (and (vector? data)
       (not (pairs-in-vector? data))))

(define (json-object? data)
  (and (vector? data)
       (pairs-in-vector? data)))

(define (format-pair data)
  (if (null? (cdr data))
      (format-value (car data))
      (string-join (list (format-value (car data))
			 (format-value (cdr data)))
		   " : ")))

(define (format-array data)
  (string-join
   (list "["
	 (string-join (map format-value (vector->list data)) ", ")
	 "]")
   ""))

(define (format-object data)
  (string-join
   (list "{"
	 (string-join (map format-value (vector->list data)) ", ")
	 "}")
   ""))

(define predicates-x-formatting-functions
  (list (cons number?      number->string)
	(cons symbol?      (lambda (sym)
			     (string-join (list "\""
						(symbol->string sym)
						"\"")
					  "")))
	(cons string?      (lambda (string)
			     (string-join (list "\""
						string
						"\"")
					  "")))
	(cons pair?        format-pair)
	(cons json-array?  format-array)
	(cons json-object? format-object)))

(define (format-value data)
  (let loop ((pxf predicates-x-formatting-functions))
    (if (null? pxf) (error "malformed or unsuported data structure")
	(let ((pair (car pxf)))
	  (let ((predicate (car pair))
		(pfunction (cdr pair)))
	    (if (predicate data)
	    	(pfunction data)
	    	(loop (cdr pxf))))))))

(define (write-json-file filename data)
  (let ((port (open-file filename "w"))
	(string (format-value data)))
    (put-string port string)
    (close port)))

(define format-json format-value)

;; -----------------------------------------------------------
;; - TESTING


;; (define tree (read-json-file "json_sample_2.json"))

;; (define p (open-file "json_sample_2.json" "r"))
;; (define string (get-string-all p))
;; (define tree (parse-json-string string))
