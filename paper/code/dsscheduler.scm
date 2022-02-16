;; returns a list of the ready-Q and the scheduler
;; call run-ds-scheduler with the result of this function to start the
;; scheduler and the load average procedure which runs every n
;; seconds.

(define new-ds-scheduler
  (lambda optional-args
    (let ((quantum (if (null? optional-args)
		       100
		       (car optional-args)))
	  (continue? (if (or (null? optional-args)
			     (null? (cdr optional-args)))
			 (lambda () (wait) #t)
			 (lambda () (wait)
				 ((cadr optional-args)))))
	  (ready-Q (make-queue)))	; no initial threads
      (list ready-Q
	    (lambda ()
	      (run-threads
	       (round-robin-event-handler
		ready-Q
		quantum				     ; quantum
		(list)				     ; dynamic env
		(list 0)			     ; thread counter
		(lambda (type data) #f)		     ; event handler
		(lambda (thread token . args) #f)    ; upcall handler
		continue?)))))))	             ; continue?



;; usage:
;; (define s (spawn (new-scheduler)))
;; (spawn-on-scheduler s thunk)


;; Starts the scheduler running and the load average update thread which
;; runs every n seconds.  Returns a procudure which can give the load or
;; scheduler, spawn new threads on this scheduler, or kill the scheduler.

(define run-ds-scheduler
  (lambda (ds-scheduler)
    (let* ((ready-Q (car ds-scheduler))
	   (sched-proc (cadr ds-scheduler))
	   (load (vector 0 0 0))
	   (sched (spawn sched-proc "scheduler")))
      (spawn-on-scheduler sched (lambda () (calc-load load ready-Q)))
      (encap
       (lambda args
	 (let ((msg (car args)))
	   (case msg
	     ((SPAWN)
	      ;;	     (apply spawn (cdr args)))
	      (apply spawn-on-scheduler sched (cdr args)))
	     ((LOAD)
	      load)
	     ((READY-Q)
	      ready-Q)
	     ((SCHED)
	      sched)
	     ((KILL)
	      (kill-thread! sched))
	     (else
	      (error "Invalid message to ds-scheduler" msg)))))))))



;; Load average is calculated over past 1, 5, and 15 minutes using an
;; exponential decay alogrithm.  This gives a weighted average of the
;; current load with previous loads (of 1, 5, and 15 minutes).  The current
;; load is defined to be the length of the ready queue.  The weight of the
;; current load in calculating the load average over the past 1 minute is (1
;; - exp (- interval 60)), whereas the weight of the previous load is exp (-
;; interval 60), where interval is the frequency (in sec) at which the load
;; is updated.


(define calc-load
  ;; load is a vector of three elements, containing the load average over
  ;; the last 1, 5, and 15 minutes respectively.
  ;; ready-Q is the ready-Q of the scheduler.
  (lambda (load ready-Q)
    (let*
	((interval 5000)		; 5 sec
	 (exp-1 (exp (* -1 (/ (/ interval 1000) 60))))
	 (exp-5 (exp (* -1 (/ (/ interval 1000) 300))))
	 (exp-15 (exp (* -1 (/ (/ interval 1000) 900)))))
      (letrec 
	  ((calculate
	    (lambda (which exp-n num-tasks)
	      (vector-set! load which
			   (+ (* (vector-ref load which) exp-n)
			      (* num-tasks (- 1 exp-n))))))
	   
	   (update-load
	    (lambda ()
	      (let
		  ((num-tasks (queue-length ready-Q)))
		(calculate 0 exp-1 num-tasks)
		(calculate 1 exp-5 num-tasks)
		(calculate 2 exp-15 num-tasks)
		(sleep interval)	; recalc load every interval
		(update-load)))))
	(update-load)))))



