(define run-tests
  (lambda (num-jobs job-load-and-duration processor-to-run-on
		    interval-between-jobs mem)
    (if (> num-jobs 0)
	(begin
	  (let
	      ((ld-dur (job-load-and-duration))
	       (processor (processor-to-run-on))
	       (interval (interval-between-jobs)))
	    (host-display "load ")
	    (host-display (car ld-dur))
	    (host-newline)
	    (host-display "duration: ")
	    (host-display (cdr ld-dur))
	    (host-newline)
	    (host-display "processor: ")
	    (host-display processor)
	    (host-newline)
	    (host-display "interval: ")
	    (host-display interval)
	    (host-newline)
	    (host-newline)
	    (force-output (current-output-port))
	    
	    (create-process processor 
			    (simulated-process (cdr ld-dur)) 
			    (car ld-dur)
			    mem)
;	    (relinquish-timeslice)
	    (sleep2 interval)
	    (run-tests (- num-jobs 1) 
		       job-load-and-duration
		       processor-to-run-on
		       interval-between-jobs
		       mem)))
	(begin
	  (host-display "All processes have been started")
	  (host-newline)))))


;; Test Case 1

(define test1
;  (lambda ()
;    (spawn
     (lambda ()
       (run-tests 15
		  (constant-load 0.1 50000)
		  (constant-processor 1)
		  (constant-interval 2500)
		  2)))


(define test2
;  (lambda ()
;    (spawn
     (lambda ()
       (run-tests 15
		  (constant-load 0.1 50000)
		  (var-processor)
		  (constant-interval 2500)
		  2)))



(define test3
  (lambda ()
;    (spawn
;     (lambda ()
       (run-tests 15
		  (var-load 0.0 50000 1000)
		  (var-processor)
		  (var-interval 2500 1000)
		  2)))

(define test4
;  (lambda ()
;    (spawn
     (lambda ()
       (run-tests 150
		  (constant-load 0.1 4000)
		  (constant-processor 1)
		  (constant-interval 50)
		  0.1)))


(define test5
;  (lambda ()
;    (spawn
     (lambda ()
       (run-tests 150
		  (constant-load 0.1 4000)
		  (var-processor)
		  (constant-interval 50)
		  0.1)))



(define test6
  (lambda ()
;    (spawn
;     (lambda ()
       (run-tests 150
		  (var-load 0.0 4000 100)
		  (var-processor)
		  (var-interval 50 10)
		  0.1)))



;; For some reason real-sleep does not seem to work well, so let's define
;; our own.

(define sleep2
  (lambda (interval)
    (let
	((end (+ (real-time) interval)))
      (let loop ()
	(if
	 (< (real-time) end)
	 (loop))))))
	 

