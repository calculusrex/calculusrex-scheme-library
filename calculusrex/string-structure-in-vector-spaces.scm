(use-modules (calculusrex string)
	     (calculusrex list)
	     (calculusrex set)
	     (calculusrex csv)
	     (calculusrex json)
	     (ice-9 rdelim))

;; (load "/home/feral/.guile")


(define folder--date-saga "/home/feral/engineering/calculusrex-scheme-library/calculusrex/date-test-saga/ardeleanu--20-06-2022")
(define folder--date-saga
  (string-join (list (getcwd)
		     "date-test-saga"
		     "ardeleanu--20-06-2022")
	       "/"))


(define fnames--date-saga
  '((articole . "intrari_articole_ardeleanu__20_06_2022.csv")
    (facturi . "intrari_facturi_ardeleanu__20_06_2022.csv")))

;; date_saga = {}
;; for key in date_saga__fnames:
;;     date_saga[key] = pd.read_csv(
;;         f'{date_saga__folder}/{date_saga__fnames[key]}')

(define articole--date-saga
  (read-csv (string-join (list folder--date-saga
			       (assq-ref fnames--date-saga 'articole))
			 "/")))

(define facturi--date-saga
  (read-csv (string-join (list folder--date-saga
			       (assq-ref fnames--date-saga 'facturi))
			 "/")))


;; (assq-ref articole--date-saga 'header)
;; (assq-ref (assq-ref articole--date-saga 'rows) 'pret_vanz)
		   
;;	    #:sep #\, #:strip-string-quotation #t  #:header-present #t))


;; (read-json-file

(define set-denumiri-articole
  (set (assq-ref (df-columns articole--date-saga)
		 'denumire)))

(define set-denumiri-articole--from-python
  (assq-ref (parse-json (open-file
			 (path-join
			  `(,(getcwd)
			    "date-test-saga"
			    "set_denumiri_date_saga.json")) "r"))
	    'set))

(define diff
  (set-difference set-denumiri-articole
		  set-denumiri-articole--from-python))
