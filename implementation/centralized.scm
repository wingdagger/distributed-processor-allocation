;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  centralized.scm
;;  written by Joshua S. Allen
;;  Tulane University
;;  Department of Electrical Engineering and Computer Science
;;
;;  Masters Thesis
;;  Advisor: Dr. Mark Benard
;;  May 1998
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *algorithm* 'CENTRALIZED)
(define *usage-table-update-interval* 5000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  New Centralized Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define server-id
  (lambda ()
    (server 'id)))



(define server #f)


(define make-server
  (lambda ()
    (let
	((make-processor
	  (if (eq? *environment* 'SIMULATION)
	      make-processor
	      dist-make-processor)))
      (let ((srv (make-processor 0 (lambda () server-receive-requests))))
	(add-mailbox! srv)
	(spawn (lambda () ((srv 'daemon) srv)))
	(spawn (lambda () (usage-table-updates srv)))
	(log-processor-start srv)
	srv))))


(define *process-table* #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Modified procedures  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; OK - i think

(define forward-to-best-processor
  (lambda (p msg)
    (let*
	((usage-table (p 'usage-table))
	 (process-info (message-process-info msg))
	 (mem (cadddr process-info))
	 (best-machine (get-lowest-load usage-table mem))
	 (requestor (message-parent msg))
	 (process-id (car (get-pid-and-load process-info))))

      ;; set the number of remote jobs for the requesting processor
      (if (not (equal? requestor best-machine))
	  ((find-processor requestor (processor-list 'self)) 
	   'set-remote-jobs! 
	   (+ ((find-processor requestor (processor-list 'self)) 'remote-jobs)
	      1)))
      
      (set-message-parent! msg 0)
      ((message-process-rec msg) 'set-originator! 0)
      ((message-process-rec msg) 'set-last-owner! 0)
      
      (forward-request p msg best-machine)
      
      (add-process-to-server-info p process-id best-machine requestor))))
      
      ;; Update usage table info
;      (let
;	  ((new-load (+ (caddr (message-process-info msg))
;			(car (table-ref usage-table best-machine))))
;	   (new-mem (- mem (cadr (table-ref usage-table best-machine)))))
;	(table-set! usage-table best-machine 
;		    (list new-load new-mem (real-time)))))))




(define add-process-to-server-info
  (lambda (p process-id compute-srv requestor)
    ;; add process to process table
    (table-set! *process-table* process-id 
		(cons requestor compute-srv))
    
    ;; add process to compute server's info
    (table-set! (p 'process-locations)
		compute-srv
		(cons
		 (car (table-ref (p 'process-locations) compute-srv))
		 (cons 
		  process-id
		  (cdr (table-ref (p 'process-locations) compute-srv)))))
    
    ;; add proces to requesting server's info
    
    (table-set! (p 'process-locations)
		requestor
		(cons
		 (cons 
		  process-id
		  (car (table-ref (p 'process-locations) requestor)))
		 (cdr (table-ref (p 'process-locations) requestor))))))
      



; OK - i think

(define client-receive-requests-primitive
  (lambda (p)
    (let*
	((msg (get-mail (p 'id)))
	 (id (message-id msg))
	 (type (message-type msg))
	 (process-info (message-process-info msg))
	 (machine-info (message-machine-info msg)))

      (set-message-hop-count! msg (+ (message-hop-count msg) 1))

      (let
	  ((l (make-log-entry 'MSG)))
	(l 'set-msg-id! id)
	(l 'set-hop-count! (message-hop-count msg))
	(l 'set-msg-size! (message-size msg))
	(l 'set-processor-id! (p 'id))
	(l 'set-msg-type! type)
	(log-it (create-log-entry-list l)))
      
      (cond
       
       
       ((equal? type 'REQUEST)
	(spawn (lambda () (accept-process p msg))))

       
       ((equal? type 'CREATE)
	(mail-to (server-id) msg))
       

       ((equal? type 'LOG)
	(let 
	    ((l (make-log-entry 'LOG)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))


       ;; Another processor has requested to see if you are alive.
       
       ((equal? type 'ALIVE?)
	(let
	    ((alive-msg (make-message 'ALIVE! '() '())))
	  (set-message-parent! alive-msg (p 'id))
	  (mail-to (message-parent msg) alive-msg)))

       
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry 'SHUTDOWN)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))))))




; OK - i think

(define server-receive-requests-primitive
  (lambda (p)
    (let*
	((msg (get-mail (p 'id)))
	 (id (message-id msg))
	 (type (message-type msg))
	 (process-info (message-process-info msg))
	 (machine-info (message-machine-info msg)))

      (set-message-hop-count! msg (+ (message-hop-count msg) 1))

      (let
	  ((l (make-log-entry 'MSG)))
	(l 'set-msg-id! id)
	(l 'set-hop-count! (message-hop-count msg))
	(l 'set-msg-size! (message-size msg))
	(l 'set-processor-id! (p 'id))
	(l 'set-msg-type! type)
	(log-it (create-log-entry-list l)))
      
      (cond
       
       ((equal? type 'CREATE)
	;; set a timeout to ensure job is accepted
	
	(let*
	    ((process (message-process-info msg))
	     (process-id (car (get-pid-and-load process))))
	  (table-set! (p 'process-timeouts) process-id 'CREATE)
	  (spawn (lambda () (timeout-on-process p process))))

	(forward-to-best-processor p msg))

       
       
       ((equal? type 'ACCEPT)
	;; ensure that process-timeout knows the process has been accepted
	(let
	    ((process-id (car (get-pid-and-load process-info))))
	  (table-set! (p 'process-timeouts) process-id 'ACCEPT)))
	
	;; this need not be done for up/down
	;;	(update-table (cdr machine-info) update-algorithm p))
       
       
       ((equal? type 'DONE)
	(update-server-info p process-info))

       ;;	(update-table (cdr machine-info) update-algorithm p))
       
       
       ;; Another processor has is claimng to be alive
       
       ((equal? type 'ALIVE!)
	(table-set! (p 'processor-timeouts) 
		     (message-parent msg)
		     #t))
       
       
       ;; A processor is no longer available for remote computation, so
       ; remove it from the processor-list
       
       ((equal? type 'REMOVE-PROCESSOR)
	((p 'plist) 'remove machine-info)
	(table-set! (p 'usage-table)
		    (machine-info 'id)
		    #f))

       ;; A new processor is now available for remote computation.
       
       ((equal? type 'ADD-PROCESSOR)
	((p 'plist) 'add machine-info)
	(table-set! (p 'usage-table) 
		    (machine-info 'id)
		    (list (machine-info 'load)
			  (machine-info 'memory)
			  (real-time))))
      

       
       ((equal? type 'SERVER-LOG)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))

       
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))))))




;OK


(define server-receive-requests
  (lambda (p)
    (server-receive-requests-primitive p)
    (if
     (p 'alive?)
     (server-receive-requests p))))



; OK

(define client-receive-requests
  (lambda (p)
    (client-receive-requests-primitive p)
    (if
     (p 'alive?)
     (client-receive-requests p))))



; OK



(define update-server-info
  (lambda (p process-info)
  ;; Now that process is complete, remove it from the run queue kept
  ;; locally 
    (let*
	((process-id (car (get-pid-and-load process-info)))
	 (requestor-machine 
	  (car (table-ref *process-table* process-id)))
	 (compute-srv
	  (cdr (table-ref *process-table* process-id))))
      
      ;; remove the process id from the runner's list
      
      (table-set!
       (p 'process-locations)
       compute-srv
       (cons
	(car (table-ref (p 'process-locations) compute-srv))
	(delete-1st 
	 process-id
	 (cdr (table-ref (p 'process-locations) compute-srv)))))
      
      ;; remove the process id from the requestor's list
      
      (table-set!
       (p 'process-locations)
       requestor-machine
       (cons
	(delete-1st
	 process-id
	 (car (table-ref (p 'process-locations) requestor-machine)))
	(cdr (table-ref (p 'process-locations) requestor-machine))))
      
      ;;  remove the process id from the process table
      (table-set!
       *process-table* process-id #f)

      ;; update the number of remote jobs on the requesting machine
      (if (not (equal? requestor-machine compute-srv))
	  ((find-processor requestor-machine (processor-list 'self)) 
	   'set-remote-jobs! 
	   (- ((find-processor requestor-machine 
			       (processor-list 'self)) 'remote-jobs)
	      1))))))





;; usage table updates every *usage-table-update-interval* seconds

(define usage-table-updates
  (lambda (p)
    (let
	((usage-table (p 'usage-table)))
      (table-walk
       (lambda (k d)
	 (let*
	     ((num-requested-processes (length (car d)))
	      (num-local-processes (length (cdr d)))
	      ; this is the original way I updated the usage table, now I
	      ;  want to not decrease for remote process, but only increase
	      ;  for local processes run for someone else
;	      (new-usage (- num-local-processes num-requested-processes)))
	      (new-usage num-local-processes))
	   (if (and (equal? num-local-processes 0)
		    (> (car (table-ref usage-table k)) 0))
	       (set! new-usage -1))
	   (table-set! usage-table k
		       (cons
			(+ (car (table-ref usage-table k))
			   new-usage)
			(cdr (table-ref usage-table k))))))
       (p 'process-locations)))
    (sleep *usage-table-update-interval*)
    (usage-table-updates p)))
	 




;      
      

(define start-ds
  (let ((first-time #t))
    (lambda (n migrate-procs?)
      (if first-time
	  (begin
	    (set! *process-table* (make-table))
	    (set-migrate-processes! migrate-procs?)
	    (set-system-scheduler! 
		  (run-ds-scheduler (new-ds-scheduler)))
	    (set! server (make-server))
	    ;; This is a hack to keep the scheduler from freezing!  I don't know why
	    ;;    the scheduler is freezing up
	    (spawn (lambda () 
		     (let loop ()
		       (resume-thread (*system-scheduler* 'SCHED))
		       (sleep 5000)
		       (loop))))
	    (set! first-time #f)))
      (cond 
       ((zero? n) 
	(init-one-usage-table (server 'usage-table) 
			      (processor-list 'self))
	(for-each host-display '("Usage table initialized" #\newline))
      (processor-list 'add server)
      (spawn (lambda () (always-log-all-processors)))
      (for-each host-display '("Logging started" #\newline)))
       (else
	(add-processor-to-ds (create-processor))
	(start-ds (- n 1) migrate-procs?))))))
  



