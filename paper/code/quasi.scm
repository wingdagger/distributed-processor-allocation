;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  quasi.scm
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




;; get-thread-load should be defined such that it runs in the appropriate
;; address space.  Perhaps a proxy should be setup for it.

(define update-load
  (lambda (p)
    (let ((load #f))
      (display "Updating load on processor ")
      (display (p 'id))
      (newline)
      (case *environment*
	((QUASI-SIMULATION)
	 ;; return the load over the last minute only
	 (set! load (vector-ref ((p 'scheduler) 'LOAD) 0)))
	((FULL-IMPLEMENTATION)
	 (set! load (vector-ref (get-os-load) 0)))
	(else
	 (error "unknown environment in update-load" *environment*)))
      (p 'set-load! load)
      (if (eq? *algorithm* 'RANDOM-DPA)
	  (update-local-load p)))
    (sleep 5000)
    (if (p 'alive?)
	(update-load p))))

