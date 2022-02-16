;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  debug.scm
;;  written by Joshua S. Allen
;;  Tulane University
;;  Department of Electrical Engineering and Computer Science
;;
;;  Masters Thesis
;;  Advisor: Dr. Mark Benard
;;  May 1998
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Debugging Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define show-stats
  (lambda ()
    (for-each show-status (processor-list 'self))))


(define show-status-primitive
    (lambda (p)
;;      (let ((p (find-processor (p 'id) (processor-list 'self))))
	(host-display (p 'id))
	(host-display ": ")
	(host-display (if (no-mail? (p 'id))
			  "mailbox empty"
			  "has message(s)"))
	(host-newline)
	(host-display "jobs: ")
	(host-display (table-map
		       (lambda (k d)
			 k)
		       (p 'running-jobs)))
	(host-newline)
	(host-display "load: ")
	(host-display (p 'load))
	(host-newline)
	(host-display "free memory: ")
	(host-display (p 'memory))
	(host-newline)
	(host-display "number of remote jobs: ")
	(host-display (p 'remote-jobs))
	(host-newline)
	(host-display tab)
	(host-display "usage table")
	(host-newline)
	(table-walk (lambda (id load)
		      (for-each 
		       host-display (list tab id " " load #\newline)))
		    (p 'usage-table))))



; Returns the processor given its id

(define find-processor
  (lambda (id p-list)
    (cond
     ((null? p-list) (error "processor not in list" id (processor-list 'self)))
     ((= id ((car p-list) 'id)) 
      (car p-list))
     (else
      (find-processor id (cdr p-list))))))
