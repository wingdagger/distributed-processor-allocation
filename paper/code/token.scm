;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  token.scm
;;  written by Joshua S. Allen
;;  Tulane University
;;  Department of Electrical Engineering and Computer Science
;;
;;  Masters Thesis
;;  Distributed Processor Allocation 
;;    using a token-based algorithm
;;  Adviser: Dr. Mark Benard
;;  May 1998
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; It might help to have the person who sends the token monitor to make sure
;; the next processor receives it.  Otherwise, the token easily gets lost
;; when a machine dies.  Need to monitor for machine death that occurs
;; without proper shutdown!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Notes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The design of this system is separated into several entities: Processors
; and Processes, Communications, and Logging, each of which can be divided
; into its constituent parts.



;  Processors and Processes:
;        Amount of Free Memory available to processor
;        Load Average of a Processor
;        Run time of a process
;        Amount of memory required by a process
;        Load average of a process
;        Source of process creation


;  Communications:
;        Ring-procedures
;        Token-procedures
;        Network daemons
;        Messsages:
;              Token
;              Process creation
;              etc.


;  Logging:
;        Scheme-readable format
;        Human-readable format
;        Performance Measurements (including plotting of data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Global Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *algorithm* 'TOKEN)


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
  (lambda (n migrate-procs?)
    (set-system-scheduler! (run-ds-scheduler (new-ds-scheduler)))
    (set-migrate-processes! migrate-procs?)
    (init-ring n)			;local
    (host-display "Ring initialized")
    (host-newline)
    (elect-coordinator processor-list)
    (spawn (lambda () (always-log-all-processors))) ;local
    (for-each host-display '("Continuous logging started" #\\newline))
    (generate-new-token)		;local
    (for-each host-display '("Token generated" #\\newline))
    ;; This is a hack to keep the scheduler from freezing!  I don't know why
    ;;    the scheduler is freezing up
    (spawn (lambda () 
	     (let loop ()
	       (resume-thread (*system-scheduler* 'SCHED))
	       (sleep 5000)
	       (loop))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Table-related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; The input data "ls" is a list containing only the machine id, load
; average, and free memory for a single machine (ex. '(1 2.15 6.7)).  The
; return value should look like this: '(2.15 6.7 45000).

(define update-algorithm
  (lambda (ls usage-table)
    (append (cdr ls) (list (real-time)))))




; Update the load average on the local machine

;; Here is where load average can be adjusted according to the number of
;; remote running processes.  If no remote processes, then load average
;; still can be adjusted.

(define update-local-load
  (lambda (p usage-table)
    ;(let ((loads (get-os-load))
    ;      (mems (get-os-mem)))
    ;   (set-processor-load! p (car loads))
    ;   (set-processor-mem! p (car mems))
    (table-set! usage-table
		(p 'id)
		(list (p 'load) 
		      (p 'memory) 
		      (real-time)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Decision procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Try to find an available processor to which to offload a process.  This is
; done by finding from the usage-table the least used processor which has
; enough memory to run the process.

; This procedure should forward the process request to the best processor
; whenver it receives the token.  When the token is received, a wait
; variable should be unblocked so that the best processor can be chosen
; based upon the usage table in the token.  Once the processes are sent to
; processors, the condition variable can be blocked again, and the token
; sent on the next processor in the logical ring.


(define forward-to-best-processor
  (lambda (p process usage-table)
    (let*
	((process-info (message-process-info process))
	 (load (caddr process-info))
	 (mem (cadddr process-info))
	 (best-processor (get-lowest-load usage-table mem)))
      (forward-request p process best-processor)
      (if 
       (not (= (p 'id) best-processor))
       (p 'set-remote-jobs! (+ (p 'remote-jobs) 1)))
      (let*
	  ((new-load
		  (+ load (car (table-ref usage-table best-processor))))
	   (new-mem
		  (- (cadr (table-ref usage-table best-processor)) mem)))
	(table-set! usage-table best-processor 
		    (list new-load new-mem (real-time)))))))





;  Whenever a process creation request comes into the system, the request
;  should be enqueued until such time that this processor receives the
;  token.  


(define accept-creation-requests
  (lambda (p msg)

    ;; enqueue the job until token is received
    (let
	((q (p 'waiting-jobs)))
      (enqueue! q msg))
    
    ;; set a timeout to ensure job is accepted

    (let*
	((process (message-process-info msg))
	 (process-id (car (get-pid-and-load process))))
      (table-set! (p 'process-timeouts) process-id 'CREATE)
      (spawn (lambda () (timeout-on-process p process))))))





; When a processor receives the token, the processor can offload all of its
; waiting processes.


(define offload-processes
  (lambda (p usage-table)
    (cond 
     ((queue-empty? (p 'waiting-jobs)) '())
     (else
      (let
	  ((process (dequeue! (p 'waiting-jobs))))
	(forward-to-best-processor p process usage-table))
      (offload-processes p usage-table)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Communication Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; probably need to edit this procedure a bit, perhaps including broadcast
; and ring management message headers

; This procedure should be spawned as a thread and run continuously.  It
; monitors for incoming messages and dispatches these messages to the
; appropriate functions. (i.e.,  accept-or-forward-process)

(define receive-requests-primitive
  (lambda (p coordinator)
    (let*
	((msg (get-mail (p 'id)))
	 (id (message-id msg))
	 (type (message-type msg))
	 (process-info (message-process-info msg))
	 (machine-info (message-machine-info msg))
	 (token-info (message-token-info msg)))
      
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
       
       ; request to run a process
       ;  processor should accept the process and run it, and ack to the
       ;  requestor. 
       
       ((equal? type 'REQUEST)
	(spawn (lambda () (accept-process p msg))))
       
       ; a process creation request.  Request should be enqueued until token
       ; is received.  Then it should be offloaded to the best
       ; processor. 
       
       ((equal? type 'CREATE)
	(accept-creation-requests p msg))
       
       ; acknowledgment that a processor has accepted a process and will run
       ; it 
       
       ((equal? type 'ACCEPT)
	(let
	    ((process-id (car (get-pid-and-load process-info))))
	  (table-set! (p 'process-timeouts) process-id 'ACCEPT)))
       
       ; log request for statistical purposes
       
       ((equal? type 'LOG)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))
       
       
       ; the token has been received by this processor
       
       ((equal? type 'TOKEN)
	(p 'set-usage-table! token-info)
	(let
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l)))

	;; this is to make token compatible with other algorithms
	(p 'set-usage-table! (make-integer-table))

	(receive-token p msg))
       
       ;; token management message.  If coordinator broadcast message is
       ;; has not been received within the last n seconds, then a new
       ;  coordinator must be elected.
       
       ((equal? type 'COORDINATOR)
	(vector-set! coordinator 0 (real-time))
;;	(host-display (p 'id))
;;	(host-display " received COORDINATOR message")
;;	(host-newline)
	)
       
       ; A process has completed execution.  If it was a remote process,
       ; then adjust the number of remote-jobs accordingly
	 
       ((equal? type 'DONE)
	(if 
	 (not (= (p 'id) (message-parent msg)))
	 (p 'set-remote-jobs! (- (p 'remote-jobs) 1))))
       
       ;; A processor is no longer available for remote computation, so
       ; remove it from the processor-list
       
       ((equal? type 'REMOVE-PROCESSOR)
	((p 'plist) 'remove machine-info))
       
       ;; A new processor is now available for remote computation.
       
       ((equal? type 'ADD-PROCESSOR)
	((p 'plist) 'add machine-info))
       

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

       
       ; this processor should shutdown and no longer monitor the network
       ; for messages.			
	 
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))))))



; probably need to edit this procedure a bit, perhaps including broadcast
; and ring management message headers

; This procedure should be spawned as a thread and run continuously.  It
; monitors for incoming messages and dispatches these messages to the
; appropriate functions. (i.e.,  accept-or-forward-process)

(define coordinator-receive-requests-primitive
  (lambda (p last-token-time)
    (let*
	((msg (get-mail (p 'id)))
	 (id (message-id msg))
	 (type (message-type msg))
	 (process-info (message-process-info msg))
	 (machine-info (message-machine-info msg))
	 (token-info (message-token-info msg)))
      
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
       
       ; request to run a process
       ;  processor should accept the process and run it, and ack to the
       ;  requestor. 
       
       ((equal? type 'REQUEST)
	(spawn (lambda () (accept-process p msg))))
       
       ; a process creation request.  Request should be enqueued until token
       ; is received.  Then it should be offloaded to the best
       ; processor. 
       
       ((equal? type 'CREATE)
	(accept-creation-requests p msg))
       
       ; acknowledgment that a processor has accepted a process and will run
       ; it 
       
       ((equal? type 'ACCEPT)
	;; ensure that process-timeout knows the process has been accepted
	(let
	    ((process-id (car (get-pid-and-load process-info))))
	  (table-set! (p 'process-timeouts) process-id 'ACCEPT)))
	
	 
       ; log request for statistical purposes
       
       ((equal? type 'LOG)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))
       
       
       ; the token has been received by this processor
       
       ((equal? type 'TOKEN)
	(vector-set! last-token-time 0 (real-time))
	(p 'set-usage-table! token-info)
	(let
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l)))
	
	(p 'set-usage-table! (make-integer-table))
	(receive-token p msg))
       
       ; A process has completed execution.  If it was a remote process,
       ; then adjust the number of remote-jobs accordingly
	 
       ((equal? type 'DONE)
	(if 
	 (not (= (p 'id) (message-parent msg)))
	 (p 'set-remote-jobs! (- (p 'remote-jobs) 1))))
       
       ;; A processor is no longer available for remote computation, so
       ; remove it from the processor-list
       
       ((equal? type 'REMOVE-PROCESSOR)
	((p 'plist) 'remove machine-info))
       
       ;; A new processor is now available for remote computation.
       
       ((equal? type 'ADD-PROCESSOR)
	((p 'plist) 'add machine-info))
       
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
       
       
       ;; Another processor is checking to make sure the coordinator is
       ;;  alive 
       ((equal? type 'COORDINATOR-ALIVE?)
	(let
	    ((alive-msg (make-message 'COORDINATOR '() '())))
	  (set-message-parent! alive-msg (p 'id))
	  (mail-to (message-parent msg) alive-msg)))
		 
      
       ; this processor should shutdown and no longer monitor the network
       ; for messages.			
	 
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))
       
       (else
	(error "Unknown message received by cooridinator-receive-messages"
	       msg))))))


; procedure that continously runs receives-requests-primitive. i.e., always
; monitors for and dispatches incoming messages

(define make-receive-requests-token
  (lambda ()
    (let
	((coordinator (vector (real-time)))
	 (last-token-time (vector (real-time)))
	 (last-role 'none))
    (letrec
	((receive-requests-token
	  (lambda (p)
	    (cond
	     
	     ;; Processor is coordinator
	     ((equal? (p 'alive?) 'COORDINATOR)
	      (if (not (eq? (p 'alive?) last-role))
		  (begin
		    (host-display (p 'id))
		    (host-display " spawning coordinator threads")
		    (host-newline)
		    (spawn (lambda () (watch-for-lost-token last-token-time p)))
		    (spawn (lambda () (coordinator-broadcast p)))
		    ))
	      (set! last-role 'COORDINATOR)
	      (vector-set! last-token-time 0 (real-time))
	      (coordinator-receive-requests-primitive p last-token-time))

	     ;; Processor is regular machine
	     ((p 'alive?)
	      (if (not (eq? (p 'alive?) last-role))
		  (begin
		    (host-display (p 'id))
		    (host-display " spawning workstation threads")
		    (host-newline)
		    (spawn (lambda () (watch-for-dead-coordinator coordinator p)))
		    ))
	      (set! last-role #t)
	      (vector-set! coordinator 0 (real-time))
	      (receive-requests-primitive p coordinator)))


	    ;; If processor is still alive, then continue to run receive-requests
	    (if (p 'alive?)
		(begin
		  (receive-requests-token p))))))
      receive-requests-token))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Procedures for Processes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Token procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define copy-usage-table
  (lambda (old-tbl)
    (let 
	((new-tbl (make-integer-table)))
      (table-walk
       (lambda (k d)
	 (table-set! new-tbl k d))
       old-tbl)
      new-tbl)))


;  When a token is received by a processor, that processor should do the
;  following: (1) Update the token's usage table with its current load.  (2)
;  Offload any new processes.  (3) Forward the token to the next processor
;  in the logical ring.

(define receive-token
  (lambda (p old-token)
    (let*
	((old-usage-table (message-token-info old-token))
	 (new-usage-table (copy-usage-table old-usage-table))
	 (new-token (make-token new-usage-table)))
      
      ;; this processor list should be the local one NOT the global one
      ;; this may create inconsistencies in the order of processor lists.
      
      ;; instead we might want to use an encap or proxy-value procedure to
      ;;  get the global definition of processor list.  That way everyone
      ;;  sees a consistent order.  WE HAVE TO MAINTAIN A CONSISTENT ORDER
      ;;  in order to have a LOGICAL RING
;      (remove-dead-machines ((p 'plist) 'self) new-usage-table)
;      (remove-dead-machines (processor-list 'self) usage-table)
;      (add-new-machines ((p 'plist) 'self) new-usage-table)
;      (add-new-machines (processor-list 'self) usage-table)
      (update-local-load p new-usage-table)
      (offload-processes p new-usage-table)
      
      ;; sleep for better performance during testing
      ;; the token takes up a lot or resources!!
      (sleep 300)
      (update-local-load p new-usage-table)

      (forward-token p new-token))))
	

;; If any machines have failed since last time token was received, then
;; remove those machines from the usage table.

(define remove-dead-machines
  (lambda (p-list usage-table)
    (table-walk
     (lambda (k d)
       (if (not (member 
		 k 
		 (map 
		  (lambda (p)
		    (p 'id))
		  p-list)))
	   (table-set! usage-table k #f)))
     usage-table)))
		    

;; Add newly available machines to the usage table

(define add-new-machines
  (lambda (p-list usage-table)
    (for-each
     (lambda (pr)
       (if (not (table-ref usage-table (pr 'id)))
	   (table-set! usage-table (pr 'id) (list 0 32 (real-time)))))
     p-list)))



;  The coordinator should generate a new token at startup and any time a
;  token is determined to be lost.  At startup, the token should have
;  "initial" values that should be correct for all processors.  If a
;  subsequent token is being generated, one of two things must occur: (1)
;  The token can contain the most recent data held by the coordinator.  (2)
;  The coordinator must request updated information from each processor in
;  the ring in order to construct the new token. (3) Give each processor a
;  default value.


(define generate-new-token
  (lambda ()
    (let*
	((token (make-token (init-one-usage-table 
			     (make-integer-table) 
			     (get-processor-list)))))
      (mail-to ((car (get-processor-list)) 'id) token))
    (host-display "New token generated")
    (host-newline)))


;  This process should be run continuously by the coordinator in order to
;  determine whether the token has been lost.  If it has, then the
;  coordinator should generate a new one.


(define watch-for-lost-token
  (lambda (last-token-time p)
    (if (> (real-time) 
	   (+ *token-threshold*
	      (vector-ref last-token-time 0)))
	(generate-new-token))
    (sleep *token-threshold*)
    (if (p 'alive?)
	(watch-for-lost-token last-token-time p))))



; The process should be run by the coordinator to determine whether a
; duplicate token exists.  If one does, then the coordinator should simply
; remove the token from the network.


(define watch-for-duplicate-token
  (lambda ()
    '()))



; When a processor finishes using the token, it should forward it to the
; next processor in the logical ring.

(define forward-token
  (lambda (p token)
;    (host-display "processor ")
;    (host-display (p 'id))
;    (host-display "'s plist: ")
;    (host-display ((p 'plist) 'self))
;    (host-newline)
    (let
	; MIGHT want to change this back!!
	;	((next-member (next-processor p ((p 'plist) 'self))))
	((next-member (next-processor p (get-processor-list))))
      (mail-to (next-member 'id) token))))




; This will pick the next member in the logical ring given the current
; member and the ring.   The ring is determined by have the first member of
; the ring follow the last member of the ring.

;  This should be caled with processor-list with its 2nd argument

(define next-processor
  (lambda (p p-list)
    (cond
     ((null? p-list) (error "processor not in list" p (processor-list 'self)))
     ((= (p 'id) ((car p-list) 'id)) 
      (if (null? (cdr p-list))
	  (car (get-processor-list))
	  (cadr p-list)))
     (else
      (next-processor p (cdr p-list))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Ring Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Ring Initialization Takes the number of processors, creates them, and
;;  constructs a ring with these members


(define init-ring
  (lambda (n)
    (cond
     ((equal? n 0) 
      (processor-list 'self))
     (else
      (add-member-to-ring (create-processor))
      (init-ring (- n 1))))))




;; Add a member to the ring

(define add-member-to-ring add-processor-to-ds)
	

;; Remove a member from the ring

(define remove-member-from-ring remove-processor-from-ds)


;; Elect coordinator.  The coordinator must be elected at startup and
;; whenever the coordinator fails.  The coordinator is responsible for
;; genereating the token and for monitoring for lost or duplicate tokens. 

;; First machine in processor list is chosen to be the coordinator!

(define elect-coordinator
  (lambda (plist)
    (let ((contender (car (plist 'self))))
      (if (contender 'alive?)
	  (begin
	    (contender 'set-alive! 'COORDINATOR)
	    (host-display "Coordinator elected ")
	    (host-display (contender 'id))
	    (host-newline))))))
    




;;  the process should be run by all processors in order to determine if a
;;  coordinator has failed.  If the coordinator has failed, then a new
;;  coordinator must be elected.

(define watch-for-dead-coordinator
  (let ((timeout #f))
    (lambda (coordinator p)
      (if (> (real-time) 
	     (+ *coordinator-threshold*
		(vector-ref coordinator 0)))
	  (if timeout
	      (begin
		(elect-coordinator (p 'plist))
		(set! timeout #f))
	      (begin
		(host-display (p 'id))
		(host-display " Sending COORDINATOR-ALIVE? message")
		(host-newline)
		(let ((msg (make-message 'COORDINATOR-ALIVE? '() '())))
		  (set-message-parent! msg (p 'id))
		  (group-broadcast-pr ((p 'plist) 'self) msg))
		(set! timeout #t))))
      (sleep *coordinator-threshold*)
      (if (equal? (p 'alive?) #t)
	  (watch-for-dead-coordinator coordinator p)))))
  
  


;; Send a broadcast message to all machines every (/ *coordinator-threshold*
;; 2) seconds so that all machines will know that the coordinator is alive

(define coordinator-broadcast
  (lambda (p)
    (let 
	((msg (make-message 'COORDINATOR '() '())))
      (set-message-parent! msg (p 'id))
      (group-broadcast-pr 
       (filter
	(lambda (pr)
	  (not (equal? (pr 'id) (p 'id))))
	((p 'plist) 'self))
       msg)
      ;;      (host-display (p 'id))
      ;;      (host-display " just broadcasted")
      ;;      (host-newline)
      (sleep (/ *coordinator-threshold* 2))
      (if (equal? (p 'alive?) 'COORDINATOR)
	  (coordinator-broadcast p)))))
