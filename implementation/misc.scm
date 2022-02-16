;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  misc.scm
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





; Shutdown the distributed system.  Also, convert scheme-readable logfile
; into a human readable logfile.

(define shutdown-ds
  (lambda (plot)

    ; after shutting down all processors, a shutdown message needs to be
    ;  sent to the daemon.  Actually this can be done instead of having a
    ;  field in the processor record

    (let 
	((n (length (processor-list 'self))))
      (for-each shutdown-processor (processor-list 'self))
      
      (close-output-port logfile)
      (let
	  ((in-port 
	    (open-input-file 
	     "/tmp/ds-logfile.ds"))
	   (out-port 
	    (open-output-file
	     "/tmp/user-logfile.ds")))
	(host-display "Writing user log")
	(host-newline)
	(init-user-log out-port)
	(convert-log in-port out-port)
	(finish-user-log out-port)
	(close-output-port out-port)
	(if plot
	    (begin
	      (host-display "Making gnuplot data. . .Be patient")
	      (host-newline)
	      (make-stat-data n)))))))



; procedure that continously runs receives-requests-primitive. i.e., always
; monitors for and dispatches incoming messages

(define receive-requests
  (lambda (p)
    (receive-requests-primitive p)
    (if
     (p 'alive?)
     (receive-requests p))))
    


;; Create a processor

(define create-processor
  (let ((count 0))
    (lambda ()
      (let
	  ((make-processor
	    (if (eq? *environment* 'SIMULATION)
		make-processor
		dist-make-processor))
	   (p #f))
	(set! count (+ count 1))
	(cond
	 ((equal? *algorithm* 'TOKEN)
	  (set! p (make-processor count 
				  make-receive-requests-token)))
	 ;; wrap a lambda around receive-requests so that it is
	 ;; compatible with the TOKEN algorithm
	 ((equal? *algorithm* 'RANDOM-DPA)
	  (set! p (make-processor count 
				  (lambda () receive-requests))))
	 ((equal? *algorithm* 'CENTRALIZED)
	  (set! p (make-processor count 
				  (lambda () client-receive-requests))))
	 (else
	  (error "Unknown algorithm in create-processor")))
	p))))
  


(define migrate-processes
  (lambda (p)
    (if (and (> (p 'load) *process-migration-load-threshold*)
	     (> (- (p 'load) (lowest-load (p 'usage-table)))
		*process-migration-load-diff-threshold*))
	;; migrate a processes
	(begin
	  (let*
	      ;; select the thread to migrate  -- process rec
	      ((thread-to-migrate 
		(select-thread-to-migrate (p 'running-jobs))))
	    (host-display "migrating thread ")
	    (host-display (thread-to-migrate 'pid))
	    (host-newline)
	    ;; suspend the thread and get its continuation
	    (thread-to-migrate 
	     'set-thread!
	     (thread->continuation (thread-to-migrate 'thread)))
	    ;; set the wait variable for the process 
	    (placeholder-set! (thread-to-migrate 'wait-var) 'MIGRATED)
	    ;; update the process's info
	    (thread-to-migrate 'inc-migration-cnt)
	    (thread-to-migrate 'set-last-owner! (p 'id))
			       
	    ;; migrate the process
	    (let 
		((msg (make-message 
		       'CREATE 
		       (list (thread-to-migrate 'pid) 
			     (thread-to-migrate 'thread) 
			     (thread-to-migrate 'load)
			     (thread-to-migrate 'mem))
		       '())))
	      (set-message-process-rec! msg thread-to-migrate)
	      (set-message-parent! msg (p 'id))
	      (mail-to (p 'id) msg))

	    ;; log the migration
	    (let
		((l (make-log-entry 'PROCESS-MIGRATION)))
	      (l 'set-processor-id! (p 'id))
	      (l 'set-process-id! (thread-to-migrate 'pid))
	      (log-it (create-log-entry-list l))))))
	    
    (sleep *process-migration-interval*)
    (if (p 'alive?)
	(migrate-processes p))))


;; Select a thread to migrate (selection policy)
;; Choose the first thread we come to with the lowest migration count

(define select-thread-to-migrate
  (lambda (process-tbl)
    (let ((thread-to-migrate #f))
      (table-walk
       (lambda (k d)
	 ;; Choose the first thread we come to with the 
	 ;; lowest migration count
	 (if
	  (or
	   (not thread-to-migrate)
	   (> (thread-to-migrate 'migration-cnt)
	      (d 'migration-cnt)))
	  (set! thread-to-migrate d)))
       process-tbl)
      thread-to-migrate)))


;; Return the lowest load in the usage table

(define lowest-load
  (lambda (usage-table)
    (let ((least-load #f))
      (table-walk
       (lambda (k d)
	 (if
	  (or
	   (not least-load)
	   (> least-load (car d)))
	  (begin
	    (set! least-load (car d)))))
       usage-table)
      least-load)))
	    
	
    
;; For the random-dpa algorithm, this procedure will update the owner of the
;;  local load after a new process has run for at least 5 seconds.

(define update-owner-of-local-load
  (lambda (p owner)
    ;; sleep for 5 secs so that load is updated at least once
    (sleep 5000)
    (let*
	((machine-info (list (list (p 'id) 
				   (p 'load)
				   (p 'memory))))
	 (msg (make-message 'UPDATE '() machine-info)))
      
      (mail-to owner msg))))



; Runs a process.  Changes the load information before and after the process
; runs and adds the jobs to the job list

(define run-ds-process
  (lambda (pid p-rec p)
    ;; for simulation, adjust load and mem with new process
    (if (eq? *environment* 'SIMULATION)
	(begin
	  (p 'set-load! (+ (p-rec 'load) (p 'load)))
	  (p 'set-memory! (- (p 'memory) (p-rec 'mem)))
	  (if (equal? *algorithm* 'RANDOM-DPA)
	      (update-local-load p))))
    (let*
	((wait-var (make-placeholder))
	 (t (spawn 
	     (lambda ()
	       ;; actually run the procedure
	       ((p-rec 'thread))
	       (if (not (placeholder-has-value? wait-var))
		   (placeholder-set! wait-var 'DONE)))
	     pid)))
	       
      (p-rec 'set-thread! t)
      (p-rec 'set-wait-var! wait-var)
      ;; add the thread to the process table
      (table-set! (p 'running-jobs) pid p-rec)
      

      ;; if RANDOM-DPA algorithm, then update the process owner of the new
      ;; load after 5 secs.  This will allow the local load to be
      ;; updated at least once.
      
      (if (eq? *algorithm* 'RANDOM-DPA)
	  (spawn 
	   (lambda () 
	     (update-owner-of-local-load p (p-rec 'originator)))))
      
      
      ;; wait for the process to finish
      (let 
	  ((exit-code
	    (placeholder-value wait-var)))
	
	;; remove the thread from the process table
	(table-set! (p 'running-jobs) pid #f)
	(if (eq? *environment* 'SIMULATION)
	    (begin
	      (p 'set-memory! (+ (p 'memory) (p-rec 'mem)))
	      (p 'set-load! (- (p 'load) (p-rec 'load)))
	      (if (equal? *algorithm* 'RANDOM-DPA)
		  (update-local-load p))))
	exit-code))))



; Create a process and give it to the local processor.  The process should
; have a running time (perhaps more info as well).  It should update the
; local-load in both the variable and the usage-table.  Then it should call
; forward-tobest-processor.  When the process finishes it should automatically
; readjust the local-load as well.  This could be done via an alarm or it
; could be programmed into an actuall procedure which could simulate a
; process, running the correct amount of time and then readjusting the local
; load.

; Should the local load be updated only when processes enter and exit or
; should a thread run continuously and monitor the real load average? Both?
; Neither? 

(define create-process
  (let ((pid 1000))
    (lambda (id proc load mem)
      (set! pid (+ pid 1))
      (let 
	  ((msg (make-message 'CREATE (list pid proc load mem) '()))
	   (p-rec (make-process-rec proc #f id pid)))
	(p-rec 'set-load! load)
	(p-rec 'set-mem! mem)
	(set-message-process-rec! msg p-rec)
	(set-message-parent! msg id)
	(mail-to id msg))
      (let
	  ((l (make-log-entry 'PROCESS-CREATE)))
	(l 'set-processor-id! id)
	(l 'set-process-id! pid)
	(log-it (create-log-entry-list l))))))




; remove machine id from all usage tables throughout the distributed
; system. 

; Processor also needs to be removed from the processor list!!

(define shutdown-processor
  (lambda (p)
    (p 'set-alive! #f)
    (let
	((msg (make-message 'SHUTDOWN '() '())))
      (mail-to (p 'id) msg)
      (sleep 4000)
      ;; probably don't need to do this, becuase not everyone knows
      (processor-list 'remove p))))


; Given a list containing '(pid proc load), return the list '(pid load)

(define get-pid-and-load
  (lambda (ls)
    (list (car ls) (caddr ls))))


;;  Add a processor to the system

(define add-processor-to-ds
  (lambda (p)
    (processor-list 'add p)
    ;; add processor mailbox to global processor list
    (add-mailbox! p)
    ;; log start of processor
    (log-processor-start p)
    ;; start processor's network daemon on the correct scheduler
    (host-display "starting network daemon for ")
    (host-display (p 'id))
    ((p 'scheduler) 'spawn (lambda () ((p 'daemon) p)) "network daemon")
    (host-newline)
    
    ;; Add this processor to everyone's processor list
    
    (if (equal? *algorithm* 'CENTRALIZED)
	(begin
	  (mail-to 0 (make-message 'ADD-PROCESSOR '() p)))
	(begin
	  (group-broadcast
	   (map
	    (lambda (pr)
	      (pr 'id))
	    (processor-list 'self))
	   (make-message 'ADD-PROCESSOR '() p))))
    
    ;; Add other procesor to new processor's list
    (for-each 
     (lambda (pr)
       ((p 'plist) '+ pr))
     (processor-list 'self))

    ;; Add other processors to new processor's usage-table
    (if (not (equal? *algorithm* 'CENTRALIZED))
	(begin
	  (for-each
	   (lambda (pr)
	     (p 'usage-table-set!
		(pr 'id)
		(list 0			; assume init load of zero
		      (pr 'memory)
		      (real-time))))
	   (processor-list 'self))))

    ;; Initialize process-locations table for server
    
    (if (equal? *algorithm* 'CENTRALIZED)
	(begin
	  (table-set! (server 'process-locations)
		      (p 'id)
		      (cons '() '()))))

    ;; If not simulation then update-load every n seconds
    (if
     (not (eq? *environment* 'SIMULATION))
     ((p 'scheduler) 'spawn (lambda () (update-load p))))

    ;; run the process-migration algorithm
    
    (if *migrate-processes*
	((p 'scheduler) 'spawn (lambda () (migrate-processes p))))))




;; Remove a processor from the system.  Gracefully shutdown after all mail
;; has been processed.

(define remove-processor-from-ds
  (lambda (p)
    (if (no-mail? (p 'id))
	(begin
	  (shutdown-processor p)
	  (if (equal? *algorithm* 'CENTRALIZED)
	      (begin
		(mail-to 0 (make-message 'REMOVE-PROCESSOR '() p)))
	      (begin
		(group-broadcast 
		 (map 
		  (lambda (pr)
		    (pr 'id))
		  (processor-list 'self))
		 (make-message 'REMOVE-PROCESSOR '() p)))))
	(begin
	  (sleep 500)
	  (remove-processor-from-ds p)))))


      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Fault Tolerance
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	  
;; After n seconds, make sure a process has been accepted by another
;; machine.  

(define timeout-on-process
  (lambda (processor process)
    (let 
	((process-id (car (get-pid-and-load process))))
      (sleep *timeout-wait-period*)
      
      ;; ensure processor has not died while sleeping
      (if 
       (processor 'alive?)
       (begin
	 ;; check to see if process has been accepted by a processor
	 (if 
	  (eq? (table-ref (processor 'process-timeouts) process-id)
	       'CREATE)
	  (begin
	    (host-display "process timeout on id ")
	    (host-display process-id)
	    (host-display " : processor - ")
	    (host-display (processor 'id))
	    (host-newline)

	    ;; make sure processors in processor list are still alive by
	    ;; broadcasting a message to all processors in group
	    

	    ;; assume processor is down until reply is received
	    (table-walk
	     (lambda (k d)
	       (table-set! (processor 'processor-timeouts) k #f))
	     (processor 'processor-timeouts))
	       

	    ;; send request to all processors
	    (let
		((alive-msg (make-message 'ALIVE? '() '())))
	      (set-message-parent! alive-msg (processor 'id))
	      (group-broadcast-pr
	       (filter
		(lambda (pr)
		  (not (equal? (processor 'id) (pr 'id))))
		((processor 'plist) 'self))
	       alive-msg))
	    
	    ;; wait for reply from processors
	    (for-each
	     (lambda (pr)
	       (spawn (lambda ()
			(timeout-on-processor processor pr))))
	     ((processor 'plist) 'self)))))))))
	    
;; resend process creation request
;	    (let 
;		((msg (make-message 'CREATE (list pid proc load mem) '())))
;	      (set-message-parent! msg (processor 'id))
;	      (mail-to (processor 'id) msg)))))))))


(define timeout-on-processor
  (lambda (p wait-on-processor)
    ;; Sleep for a time period, giving processor enough time to reply
    (sleep *processor-timeout*)

    ;; make sure processor is still alive
    
    (if 
     (p 'alive?)
     (begin
       
       ;; if processor has not replied within given time, remove it from
       ;; the processor list, assuming it is no longer available.
       (if 
	(not 
	 (table-ref (p 'processor-timeouts) (wait-on-processor 'id)))
	(begin
	  ((p 'plist) 'remove wait-on-processor)
	  
	  ;; if we are running the random-dpa algorithm then remove
	  ;; the processor from our usage table also
	  (if (equal? *algorithm* 'RANDOM-DPA)
	      (table-set! (p 'usage-table) (wait-on-processor 'id) #f))))))))
    

