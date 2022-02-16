;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  utils.scm
;;  written by Joshua S. Allen
;;  Tulane University
;;  Department of Electrical Engineering and Computer Science
;;
;;  Masters Thesis
;;  Distributed Processor Allocation
;;  Advisor: Dr. Mark Benard
;;  May 1998
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flatten-1
    (lambda (ls flat)
      (if (null? ls)
	  flat
	  (flatten-1 (cdr ls)
	       (append flat (car ls))))))


; filter a list, returning a new list of all items that pass the predicate

;(define filter
;  (lambda (pred ls)
;    (if (null? ls)
;        '()
;        (if (pred (car ls)) 
;            (cons (car ls) (filter pred (cdr ls)))
;            (filter pred (cdr ls))))))

; Delete the first occurence of item in the list ls.

(define delete-1st
  (lambda (item ls)
    (if (null? ls) 
	'()
	(if (equal? item (car ls))
	    (cdr ls)
	    (cons (car ls) (delete-1st item (cdr ls)))))))
	

; Delete all occurences of an item in a list

(define delete-all
  (lambda (item ls)
    (filter (lambda (item2) 
	      (if 
	       (not (equal? item item2))
	       #t
	       #f))
	    ls)))



; When called, return a random number between 0 and 99.

(define real-random100 (make-maxrandom 100))


; Return a random number between 1 and 100.

(define random100
  (lambda ()
    (+ (real-random100) 1)))
  


; Return a random machine id from the machines available.  

(define random-machine
  (lambda ()
    (let 
	((index
	  (+ (modulo (random100) (length (get-processor-list))) 1)))
      ;; call get-processor-list (the encaped version of processor list to
      ;;  ensure a consistent list
      (let
	  ((id ((nth index (get-processor-list)) 'id)))
	(if (eq? id 0)			; to prevent server from being used
	    (random-machine)
	    id)))))



; return the nth item of a list

(define nth
  (lambda (n ls)
    (letrec 
	((nth-helper
	  (lambda (n ls count)
	    (cond
	     ((null? ls) #f)
	     ((equal? n count) (car ls))
	     (else
	      (nth-helper n (cdr ls) (+ count 1)))))))
      (nth-helper n ls 1))))





; Remove the nth item of a list, returning a list without that nth item.
;  NO ERROR CHECKING

(define remove-nth
  (lambda (n ls)
    (letrec
	((helper
	  (lambda (cnt ls-head ls-tail)
	    (if (eq? cnt n)
		(append ls-head (cdr ls-tail))
		(helper (+ cnt 1) 
			(append ls-head (list (car ls-tail))) 
			(cdr ls-tail))))))
      (helper 1 '() ls))))






; return the last member of a list

(define last-member
  (lambda (ls)
    (list-ref ls (- (length ls) 1))))


(define (memq? x l)
  (let loop ((l l))
    (cond ((null? l)       #f)
	  ((eq? x (car l)) #t)
	  (else            (loop (cdr l))))))

(define (remove-dups-helper list)
  (do ((list list (cdr list))
       (res  '()  (if (memq? (car list) res)
                      res
                      (cons (car list) res))))
      ((null-list? list)
       res)))

(define remove-duplicates
  (lambda (ls)
    (reverse (remove-dups-helper ls))))

(define (null-list? x)
  (cond ((null? x) #t)
	((pair? x) #f)
	(else
	 (error "null-list? got a non-list" x))))


 (define table-map
    (lambda (proc tbl)
      (let ((result '()))
	(table-walk
	 (lambda (k d)
	   (set! result (cons (proc k d) result)))
	 tbl)
	result)))
