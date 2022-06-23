(use-modules (calculusrex string)
	     (calculusrex list)
	     (calculusrex set)
	     (calculusrex csv)
	     (calculusrex json)
	     (calculusrex alist)
	     (ice-9 rdelim))

(define read-csv read-csv--)

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

(define date-saga
  `((articole
     . ,(read-csv (string-join (list folder--date-saga
				     (ref fnames--date-saga
					  'articole))
			       "/")
		  #\, #\newline))
    (facturi
     . ,(read-csv (string-join (list folder--date-saga
				     (ref fnames--date-saga
					  'facturi))
			       "/")
		  #\, #\newline))))


(define set-denumiri-articole
  (set (ref date-saga 'articole 'columns "denumire")))


(define set-denumiri-articole--from-python
  (assq-ref (parse-json (open-file
			 (path-join
			  `(,(getcwd)
			    "date-test-saga"
			    "set_denumiri_date_saga.json")) "r"))
	    'set))


(define diff
  (set-diff set-denumiri-articole
	    set-denumiri-articole--from-python))


(define checksum-pret-vanzare-articole-python 104764.6292)
(define checksum-total-facturi-python 506081.47000000003)
