;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  random-dpa.scm
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
;;  !!To Do!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Notes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Global Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How often should a process randomly pick a machine to which to send a
; request?   The number should be between 1 and 100 (a 100% scale)

(define *parameter-p* 15)
(define *algorithm* 'RANDOM-DPA)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  User level procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Initialize the distributed system by creating records and mailboxes for
; each processor.  Also, creates a table consisting of processor id's and
; their mailboxes.  "n" is the number of processors to create for the
; system. 

(define start-ds
  (let ((first-time #t))
    (lambda (n migrate-procs?)
      (cond 
       ((zero? n) 
;;	(init-all-usage-tables (processor-list 'self))
;;	(for-each host-display '("Initialized usage tables" #\newline))
	(spawn (lambda () (always-log-all-processors)))
	(for-each host-display '("Started logging" #\newline)))
       
       (else
	(if first-time
	    (begin
	      (display "Starting Distributed System. . .")
	      (newline)
	      (set-migrate-processes! migrate-procs?)
	      (if (eq? *environment* 'SIMULATION)
		  (begin
		    (set-system-scheduler! 
		     (run-ds-scheduler (new-ds-scheduler)))
		    ;; This is a hack to keep the scheduler from freezing!  
		    ;; I don't know why the scheduler is freezing up
		    (spawn (lambda () 
			     (let loop ()
			       (resume-thread (*system-scheduler* 'SCHED))
			       (sleep 5000)
			       (loop))))))
	      (set! first-time #f)))
	(display "Adding processor to DS")
	(newline)
	(add-processor-to-ds (create-processor))
	(start-ds (- n 1) migrate-procs?))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Table-related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Update the local usage table with most current load average information
; available.  Should take an update-algorithm as a procedure.

; The input data "msg" is a list containing sublists.  The sublists contain
; a machine-id and the most current-load info. for that machine.

; After all load averages have been updated, the local load average should
; be restored in case the local load average in the msg is stale.

(define update-table
  (lambda (msg proc p)
    (cond
     ((null? msg) 
      (update-local-load p))
     (else
      (let 
	  ((result (proc (car msg) (p 'usage-table))))
	(p 'usage-table-set! (caar msg) result))
      (update-table (cdr msg) proc p)))))



;; NEEDS TO TAKE USAGE TABLE AS AN ARGUMENT AND UPDATE THE USAGE TABLE
;; STORED IN THE RECORD STRUCTURE FOR THE PROCESSOR

; Update algorithm used to update the usage table.  A naive approach is
; simple to replace the old value with the new value.  A better approach
; might be to use a weighted average of the old and new data, perhaps
; incorporating the age of the old data.

; The input data "ls" is a list containing only the machine id, load
; average and memory info for a single machine (ex. '(1 2.15 32)).

(define update-algorithm2
  (lambda (ls usage-table)
    (list (cadr ls) (caddr ls) (real-time))))

;; Currently return only the most recently value for the load average as the
;; update rule.




;; Use a weighted average of old load and new load, where the weight depends
;; upon the age of the old load data


(define update-algorithm
  (lambda (ls usage-table)
    (if (table-ref usage-table (car ls)) ; if processor data exists in usage
					 ; table 
	(begin
	  (let*
	      ((current-time (real-time))
	       (machine-id (car ls))
	       (new-load (cadr ls))
	       (old-load (car (table-ref usage-table machine-id)))
	       (new-mem (caddr ls))
	       (old-mem (cadr (table-ref usage-table machine-id)))
	       (old-time (caddr (table-ref usage-table machine-id)))
	       (weight (/ (- current-time old-time) 1000))
	       (update-load 
		(float 
		 (/ (+ old-load (* new-load weight)) (+ weight 1))))
	       (update-mem 
		(float 
		 (/ (+ old-mem (* new-mem weight)) (+ weight 1)))))
	    (list update-load update-mem current-time)))
	#f)))





; Update the load average on the local machine

(define update-local-load
  (lambda (p)
    (p 'usage-table-set! (p 'id) 
       (list (p 'load) (p 'memory) (real-time)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Decision procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Try to find an available processor to which to offload a process.  This is
; usually done by finding the least used processor from the usage-table.
; However, it is occasionally done at random, depending upon *parameter-p*.


(define forward-to-best-processor
  (lambda (p process)
    (let*
	((process-info (message-process-info process))
	 (load (caddr process-info))
	 (mem (cadddr process-info))
	 (best-processor 0))
      (if (> *parameter-p* (random100))
	  (begin
	    (set! best-processor (random-machine)))
	  (begin
	    (set! best-processor (get-lowest-load (p 'usage-table) mem))))
      (forward-request p process best-processor))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Communication Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This procedure should be spawned as a thread and run continuously.  It
; monitors for incoming messages and dispatches these messages to the
; appropriate functions. (i.e., update-table, accept-or-forward-process)

;; Currently there is a problem in this procedure where mailbox-read is
;; performed.  It never returns.  Hence, more than likely the problem is
;; that the mailbox is updated on the mail server, but the remote processor
;; does not see the change.  mailbox-read should be using encap.!!

(define receive-requests-primitive
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
	(update-table machine-info update-algorithm p)
	(accept-or-forward-process p msg))

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
	  (table-set! (p 'process-timeouts) process-id 'ACCEPT))

	(if 
	 (not (= (p 'id) (message-parent msg)))
	 (p 'set-remote-jobs! (+ (p 'remote-jobs) 1)))
	(update-table machine-info update-algorithm p))


       
       ;; This is for updating the usage table once the remote process has
       ;; been executing for at least 5 seconds.
       
       ((equal? type 'UPDATE)
	(update-table machine-info update-algorithm p))
       
       
       ;; This notifies the owner of the process that it has completed
       ;; remote execution
       
       ((equal? type 'DONE)
	(if 
	 (not (= (p 'id) (message-parent msg)))
	 (p 'set-remote-jobs! (- (p 'remote-jobs) 1))))

       
       ((equal? type 'LOG)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))
       
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
      

       ;; Another processor has requested to see if you are alive.
       
       ((equal? type 'ALIVE?)
	(let
	    ((alive-msg (make-message 'ALIVE! '() '())))
	  (set-message-parent! alive-msg (p 'id))
	  (mail-to (message-parent msg) alive-msg)))

       ;; Another processor has is claimng to be alive
       
       ((equal? type 'ALIVE!)
	(table-set! (p 'processor-timeouts) 
		    (message-parent msg)
		    #t))

       
       
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))
       
       (else
	(error "Unknown message type in receive requests primitive"))))))


