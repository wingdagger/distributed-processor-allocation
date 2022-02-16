;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  processes.scm
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



;; need to do a 
;;  ,config ,load /shared/ernst/mover/lib/modules/unix.mod
;;  ,open processes



;; Returns a list of load averages over the last minute, 5 minutes, and 15
;; minutes. 

;; Relies on "get_load.sh" which runs w and pipes the results to awk, which
;; processes them slightly. 

;; The calling procedure needs to ensure this is run on the correct aspace!


(define get-os-load
  (lambda ()
    (let*
	((loads (run-process 
		 (string-append *home-dir* "/bin/get_load.sh")))
	 (load-1 (string->number (substring loads 0 4)))
	 (load-5 (string->number (substring loads 5 9)))
	 (load-15 (string->number (substring loads 10 14))))
      (vector load-1 load-5 load-15))))



;; Returns a list of lists
;;   '('(total-physical-mem free-physical-mem) '(total-swap free-swap))

;; This information can be obtained from top, but since top takes so long to
;; run, there should be a better alternative

(define get-os-memory
  (lambda ()
    '()))
