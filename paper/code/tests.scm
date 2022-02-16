;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  tests.scm
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

;; Procedure for returning a constant load and duration

(define constant-load
  (lambda (load dur)
    (lambda ()
      (cons load dur))))

; Returns a variable load and duration given a base load and base duration.
; Also the amount of variation on the duration can be controlled by
; dur-variation. 

(define var-load
  (lambda (load dur dur-variation)
    (lambda ()
      (cons
       (+ load (/ (random100) 100))
       (+ dur (* (random100) dur-variation))))))
      
; Return the same processor everytime

(define constant-processor
  (lambda (processor)
    (lambda ()
      processor)))

; Return a random processor-id from the processor list

(define var-processor
  (lambda ()
    (lambda ()
      (random-machine))))

; Return a constant interval

(define constant-interval
  (lambda (interval)
    (lambda ()
      interval)))

; Return a varaible intervial, the variation of which can be controlled by
; variation

(define var-interval
  (lambda (interval variation)
    (lambda ()
      (+ interval (* (/ (random100) 100) variation)))))


(define variable-load
  var-interval)


;; 25000 iterations last ~10 secs.

(define cpu-intensive-process
  (lambda (num-iterations)
    (let loop ((cnt 0))
      (if (not (eq? cnt num-iterations))
	  (begin
	    (* 10000 10000)
	    (loop (+ cnt 1)))))))



(define highly-interactive-process
  (lambda (num-iterations)
;;    (host-display "running highly-interactive process")
;;    (host-newline)
;;    (let
;;	((foo '()))
    (let loop ((cnt 0))
      (if (not (eq? cnt num-iterations))
	  (begin
	    ;; sleep for at least 3 seconds + some random amt of time
	    (sleep (+ 3000 (int (* (random100) 10))))
	    ;; interact
	    (let loop ((cnt 0))
	      (if (not (eq? cnt 100))
		  (begin
		    (cons 'a '())
		    (loop (+ cnt 1)))))
	    (loop (+ cnt 1)))))))
;;    (host-display "highly-interactive-process complete")
;;    (host-newline)))



;; not for migration

(define disk-intensive-process
  (lambda (num-iterations)
    (let ((output-file (open-output-file "/tmp/disk-intensive-output")))
      (let loop ((cnt 0))
	(if (not (eq? cnt num-iterations))
	    (begin
	      (display "a" output-file)
	      (loop (+ cnt 1)))))
      (close-output-port output-file))))



(define real-process
  (lambda (prog num-iterations)
    (lambda ()
      (let* ((process (string-append 
		       *home-dir*
		       "/thesis/implementation/test_processes/"
		       prog))
	     (arg (number->string num-iterations)))
	;; run-process waits until the process is done before returning
	(run-process process arg)))))

; Returns a procedure that sleeps for the desired number of milliseconds.

(define simulated-process
  (lambda (run-time)
    (lambda ()
      (host-display "starting")
      (host-newline)
      (sleep run-time)
      (host-display "finished")
      (host-newline))))


;; Define standard processes for testing

(define cip (lambda () (cpu-intensive-process 5000000)))

(define hip (lambda () (highly-interactive-process 6)))

(define dip (lambda () (disk-intensive-process 250000)))

(define rcip (real-process "cpu" 150000000))

(define rhip (real-process "interactive" 6))

(define rdip (real-process "disk" 3000000))



;; Run cpu intensive processes.  Pick a random machine on which to create
;; the process each time a process is created.  Create the processes at an
;; interval of at least 0.5 sec apart but up to 10.0 seconds apart.  Run a
;; total of "num-processes" processes.


(define testa
  (lambda ()
    (spawn
     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval (var-interval 500 9500)))
	 (cond
	  ((equal? *environment* 'SIMULATION)
	   (set! load 1.0)
	   (set! mem 0.5)
	   (set! process (simulated-process 35000)))
	  ((equal? *environment* 'QUASI-SIMULATION)
	   (set! process cip))
	  ((equal? *environment* 'FULL-IMPLEMENTATION)
	   (set! process rcip)))
	 
	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 (create-process (random-machine) process load mem)
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 
	 (host-display "all processes created")
	 (host-newline))))))
;	 (sleep 400000)			; 5 minutes
;	 (show-stats)
;	 (shutdown-ds #t))))

  
  
;; Run highly-interactive processes.  Pick a random machine on which to
;; create the process each time a process is created.  Create the processes
;; at an interval of at least 0.5 sec apart but up to 10.0 seconds apart.
;; Run a total of "num-processes" processes.


(define testb
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval (var-interval 500 9500)))
	 (cond
	  ((equal? *environment* 'SIMULATION)
	   (set! load 0.01)
	   (set! mem 0.5)
	   (set! process (simulated-process 10000)))
	  ((equal? *environment* 'QUASI-SIMULATION)
	   (set! process hip))
	  ((equal? *environment* 'FULL-IMPLEMENTATION)
	   (set! process rhip)))
	 
	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 (create-process (random-machine) process load mem)
;		 (sleep 250)
;		 (show-stats)
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 75000)			; 
	 (show-stats)
	 (shutdown-ds #t))))


;; Run disk-intensive processes.  Pick a random machine on which to create
;; the process each time a process is created.  Create the processes at an
;; interval of at least 0.5 sec apart but up to 10.0 seconds apart.  Run a
;; total of "num-processes" processes.


(define testc
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval (var-interval 500 9500)))
	 (cond
	  ((equal? *environment* 'SIMULATION)
	   (set! load 0.6)
	   (set! mem 0.5)
	   (set! process (simulated-process 18000)))
	  ((equal? *environment* 'QUASI-SIMULATION)
	   (set! process dip))
	  ((equal? *environment* 'FULL-IMPLEMENTATION)
	   (set! process rdip)))
	 
	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 (create-process (random-machine) process load mem)
;		 (sleep 250)
;		 (show-stats)
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 400000)			; 5 minutes
	 (show-stats)
	 (shutdown-ds #t))))


;; Run a mix of processes.  Pick a random machine on which to create
;; the process each time a process is created.  Create the processes at an
;; interval of at least 0.5 sec apart but up to 10.0 seconds apart.  Run a
;; total of "num-processes" processes.  Processes in the simulated
;; environment can have a variable load and a variable duration.


(define testd
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 50)
	    (creation-interval (var-interval 500 9500))
	    (variable-load (var-interval 0.01 0.99))
	    (duration (var-interval 1000 34000)))

	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 
		 (cond
		  ((equal? *environment* 'SIMULATION)
		   (set! load (variable-load))
		   (set! mem 0.5)
		   (set! process (simulated-process (duration))))
		  ((or
		    (equal? *environment* 'QUASI-SIMULATION)
		    (equal? *environment* 'FULL-IMPLEMENTATION))
		   (set! process (random-process)))
		  (else
		   (error "unknown *environment* in testd")))
		 
		 (create-process (random-machine) process load mem)
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 400000)			; 5 minutes
	 (show-stats)
	 (shutdown-ds #t))))

(define test-preemption testd)


(define random-process
  (lambda ()
    (let*
	((random3 (make-maxrandom 3))
	 (rand (random3)))
	 
      (cond
       ((equal? *environment* 'QUASI-SIMULATION)
	(cond
	 ((eq? rand 0)
	  cip)
	 ((eq? rand 1)
	  dip)
	 ((eq? rand 2)
	  hip)
	 (else
	  (error "random number > 2 in random-process"))))
       ((equal? *environment* 'FULL-IMPLEMENTATION)
	(cond
	 ((eq? rand 0)
	  rcip)
	 ((eq? rand 1)
	  rdip)
	 ((eq? rand 2)
	  rhip)
	 (else 
	  (error "random number > 2 in random-process"))))
       (else
	(error "unknown *environment* in random-process"))))))
	
	 
	       


;; Other variables to vary: 
;;  -- Total processes introduced into system. Scalability
;;  -- Number of processors in system
;;  -- Constant location where processes are introduced
;;  -- Time distribution of process creation
;;  -- Preemptive vs. non-premptive load balancing (Test separately)
;;  -- Length of processes (num interations)
;;  ++ Mix of processes introduced into system




(define test-longer-creation-interval
  (lambda ()
;;    (spawn
;;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval 15000)
	    (variable-load (var-interval 0.01 0.99))
	    (duration (var-interval 1000 34000)))

	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 
		 (cond
		  ((equal? *environment* 'SIMULATION)
		   (set! load (variable-load))
		   (set! mem 0.5)
		   (set! process (simulated-process (duration))))
		  ((or
		    (equal? *environment* 'QUASI-SIMULATION)
		    (equal? *environment* 'FULL-IMPLEMENTATION))
		   (set! process (random-process)))
		  (else
		   (error "unknown *environment* in testd")))
		 
		 (create-process (random-machine) process load mem)
		 (sleep creation-interval)
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 350000)			; 5 min
	 (show-stats)
	 (shutdown-ds #t))))


(define test-same-location
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval 15000)
	    (variable-load (var-interval 0.01 0.99))
	    (duration (var-interval 1000 34000)))

	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 
		 (cond
		  ((equal? *environment* 'SIMULATION)
		   (set! load (variable-load))
		   (set! mem 0.5)
		   (set! process (simulated-process (duration))))
		  ((or
		    (equal? *environment* 'QUASI-SIMULATION)
		    (equal? *environment* 'FULL-IMPLEMENTATION))
		   (set! process (random-process)))
		  (else
		   (error "unknown *environment* in testd")))
		 
		 ;; create all processes on processsor 1
		 (create-process 1 process load mem)
		 (sleep creation-interval)
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 350000)			; 5 min
	 (show-stats)
	 (shutdown-ds #t))))


(define test-scalability
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 50)
	    (creation-interval (var-interval 500 14500))
	    (variable-load (var-interval 0.01 0.99))
	    (duration (var-interval 1000 34000)))

	 (host-display "test start time: ")
	 (host-display (real-time))
	 (host-newline)
		      
	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 
		 (cond
		  ((equal? *environment* 'SIMULATION)
		   (set! load (variable-load))
		   (set! mem 0.5)
		   (set! process (simulated-process (duration))))
		  ((or
		    (equal? *environment* 'QUASI-SIMULATION)
		    (equal? *environment* 'FULL-IMPLEMENTATION))
		   (set! process cip))
		  (else
		   (error "unknown *environment* in testd")))
		 
		 (create-process (random-machine) process load mem)
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (host-display "time: ")
	 (host-display (real-time))
	 (host-newline)
	 (sleep 500000)			; 5 min
	 (show-stats)
	 (shutdown-ds #t))))


(define test-fault-tolerance
  (lambda ()
;    (spawn
;     (lambda ()
       (let
	   ((load 0)
	    (mem 0)
	    (process #f)
	    (num-processes 30)
	    (creation-interval (var-interval 500 9500))
	    (variable-load (var-interval 0.01 0.99))
	    (duration (var-interval 1000 34000)))

	 (let loop ((cnt 0))
	   (if (< cnt num-processes)
	       (begin
		 
		 (cond
		  ((equal? *environment* 'SIMULATION)
		   (set! load (variable-load))
		   (set! mem 0.5)
		   (set! process (simulated-process (duration))))
		  ((or
		    (equal? *environment* 'QUASI-SIMULATION)
		    (equal? *environment* 'FULL-IMPLEMENTATION))
		   (set! process (random-process)))
		  (else
		   (error "unknown *environment* in testd")))
		 
		 (create-process (random-machine) process load mem)
		 
		 (if (eq? cnt 7)
		     (begin
		       (add-processor-to-ds (create-processor))))
		 
		 (if (eq? cnt 19)
		     (remove-processor-from-ds (car (get-processor-list))))
		 
		 (sleep (creation-interval))
		 (loop (+ cnt 1)))))
	 (host-display "all processes created")
	 (host-newline)
	 (sleep 350000)			; 5 minutes
	 (show-stats)
	 (shutdown-ds #t))))














