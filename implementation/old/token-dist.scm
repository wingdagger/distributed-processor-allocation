;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  token-dist.scm
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
;;  !!To Do!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Autoload the following files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For mailboxes

(load "/home/stu93/allen/lib/scm/mailbox.scm")

;; For random numbers

(load "/home/stu93/allen/lib/scm/maxrandom.scm")

;; Load distributed procedures

(load "~/thesis/implementation/remote.scm")


;; load performance measurements procedures

(load "~/thesis/implementation/stats.scm")


;; Utilities

(load "~/thesis/implementation/utils.scm")



;; Load test cases

(load "~/thesis/implementation/tests.scm")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Global Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; List of all processors currently in the system.

(define processor-list (make-processor-list))

(define make-processor-list
  (lambda ()
    (encap
     (let ((list '()))
       (lambda (msg . args)
	 (case msg
	   ((self) list)
	   ((add +) (set! list (cons (car args) list)))
	   ((remove -) (set! list (delete-1st (car args) list)))))))))
	   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Processor object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Record

;; This is the record type for a processor.  Each processor is defined by a
;; processor record.


(define-record-type processor         ;; record name
  :processor                          ;; "type" indicator (not important)
  ;; processor maker function
  (really-make-processor id daemon mailbox usage-table load memory
  running-jobs waiting-jobs remote-jobs list alive?) 
  processor?                          ;; type predicate
  (id          processor-id)         ;; <slot> <accessor> <set!>
  (daemon      processor-daemon)
  (mailbox     processor-mailbox)
  ; This is the usage-table for the local machine.  It starts out empty, but
  ; entries are added as "create-processor" is called.  Entries are updated
  ; whenever a message containing load info. is received from other machines. 
  (usage-table processor-usage-table    set-processor-usage-table!)
  (load        processor-load        set-processor-load!)
  (memory      processor-memory      set-processor-memory!)
  (running-jobs        processor-running-jobs   set-processor-running-jobs!)
  (waiting-jobs        processor-waiting-jobs   set-processor-waiting-jobs!)
  (remote-jobs         processor-remote-jobs    set-processor-remote-jobs!)
  (plist        processor-plist        set-processor-plist!)
  (alive?      processor-alive?      set-processor-alive!))



;; Constructor (Initial values)


(define make-processor-primitive
  (lambda (id)
    (really-make-processor
     id					; processor id
     receive-requests			; network daemon
     (make-mailbox)			; mailbox
     (make-integer-table)		; usage-table
     0.0				; load average
     32					; amount of free memory
     '()				; list of running jobs
     (make-queue)                       ; list of waiting job requests
     0					; number of remote jobs
     '()				; processor list (for next processor)
     #t)))				; processor alive?





;;  Object

(define make-processor
  (lambda (id)
    (remote-apply
     (uid->aspace id)
     (lambda ()
       (encap
	(let ((p (make-processor-primitive id)))
	  (lambda (msg . args)
	    (case msg
	      ((set-usage-table!) (set-processor-usage-table! p (car args)))
	      ((set-load!) (set-processor-load! p (car args)))
	      ((set-memory!) (set-processor-memory! p (car args)))
	      ((set-running-jobs!) (set-processor-running-jobs! p (car args)))
	      ((set-waiting-jobs!) (set-processor-waiting-jobs! p (car args)))
	      ((set-remote-jobs!) (set-processor-remote-jobs! p (car args)))
	      ((set-plist!) (set-processor-plist! p (car args)))
	      ((set-alive!) (set-processor-alive! p (car args)))
	      ((id) (processor-id p))
	      ((daemon) (processor-daemon p))
	      ((mailbox) (processor-mailbox p))
	      ((usage-table) (processor-usage-table p))
	      ((load) (processor-load p))
	      ((memory) (processor-memory p))
	      ((running-jobs) (processor-running-jobs p))
	      ((waiting-jobs) (processor-waiting-jobs p))
	      ((remote-jobs) (processor-remote-jobs p))
	      ((plist) (processor-plist p))
	      ((alive?) (processor-alive? p))
	      ((processor) p)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Message object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is the record type for a message.  Each message is defined by a
;; message record.


(define-record-type message           ;; record name
  :message                            ;; "type" indicator (not important)
  ;; message maker function
  (really-make-message  id type process-info parent machine-info token-info) 
  message?                ;; type predicate
  (id           message-id)         ;; <slot> <accessor> <set!>
  (type         message-type           set-message-type!)
  (process-info message-process-info)
  (parent       message-parent         set-message-parent!)
  (machine-info message-machine-info   set-message-machine-info!)
  (token-info   message-token-info     set-message-token-info!))



(define make-message-primitive
  (let ((id 0))
  (lambda (type process-info machine-info token-info)
    (set! id (+ id 1))
    (really-make-message
     id
     type
     process-info
     0					; parent
     machine-info
     token-info))))


(define make-message
  (lambda (type process-info machine-info)
    (make-message-primitive
     type
     process-info
     machine-info
     '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Token object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define make-token
  (lambda (token-info)
    (make-message-primitive
     'TOKEN
     '()
     '()
     token-info)))




(define add-plist
  (lambda (p)
    (p 'set-plist! (processor-list 'self))))


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
  (lambda (n)
    (init-ring n)			;local
    (for-each add-mailbox! (processor-list 'self)) ;local
    (for-each add-plist (processor-list 'self))  ; local
    (host-display "mailboxes created!")
    (host-newline)
    (for-each log-processor-start (reverse (processor-list 'self))) ;local
    (for-each host-display '("Logged processor start" #\newline))
    (start-all-daemons)			;dist
    (for-each host-display '("Processors started" #\newline))
    (spawn (lambda () (always-log-all-processors))) ;local
    (for-each host-display '("Continuous logging started" #\newline))
    (generate-new-token)		;local
    (for-each host-display '("Token generated" #\newline))))


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
	      (make-gnuplot-data n)))))))





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
	  ((msg (make-message 'CREATE (list pid proc load mem) '())))
	(set-message-parent! msg id)
	(mail-to id msg)))))
      


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






; Create all procedures on same machine

(define run-procs-cent
  (lambda (cnt)
    (letrec ((loop
	   (lambda (cnt)
	     (if 
	      (not (= 0 cnt))
	      (begin
		(create-process 1 (make-process 50000) 0.3 2.5)
		(sleep 10)
		(loop (- cnt 1)))))))
      (loop cnt))))



;; Create same procedure on different machines

(define run-procs-dist
  (lambda (cnt)
    (letrec ((loop
	   (lambda (cnt i)
	     (if 
	      (< 0 cnt)
	      (begin
		(create-process (+ 1 (modulo i (length (processor-list 'self))))
				(make-process 50000) 0.1 2.5)
		(loop (- cnt 1) (+ i 1)))))))
      (spawn (lambda () 
	       (loop cnt 1)
	       (sleep 6000)
	       (loop cnt 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Table-related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Return the machine id which currently has the lowest load, according to
; the usage table, but only if it has enough free memory to run the process.

(define get-lowest-load
  (lambda (usage-table mem)
    (let 
	((lowest-load #f)
	 (lowest-processor #f))
    (table-walk 

     ;; procedure to send to table-walk in order to find the machine with
     ;; the lowest load.  Sets the variable lowest-processor to the machine
     ;; id with the lowest load.

     (lambda (key data)
       (if
	(<= mem (cadr data))		; check memory requirments
	(if
	 (or
	  (not lowest-load)
	  (> lowest-load (car data)))
	 (begin
	   (set! lowest-processor key)
	   (set! lowest-load (car data))))))
     usage-table)
    lowest-processor)))



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
    ;    (get-os-load)
    ;    (get-os-mem)
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





; Determine whether or not to accept a process from another processor based
; upon local load information and the load information in the request
; message. 

(define accept-or-forward-process
  (lambda (p msg)
    (let*
	((machine-info (message-machine-info msg))
	 (req-load (cadar machine-info))
	 (process-info (message-process-info msg))
	 (mem (cadddr process-info)))
      (cond
       ((<= (p 'load) req-load)
	(spawn (lambda () (accept-process p msg))))
       (else
	(forward-request p msg 
			 (get-lowest-load (p 'usage-table) mem)))))))





; Accepted a process.  Now send the request message back to machine A (who
; originated the request), and request the process be migrated to this
; machine.  accept-process sends the projected load of the system to the
; originating machine.  run-ds-process actually updates the system load.

(define accept-process
  (lambda (p msg)
    (let*
	((process-info (message-process-info msg))
	 (parent (message-parent msg))
	 (pid (car process-info))
	 (proc (cadr process-info))
	 (process-load (caddr process-info))
	 (mem (cadddr process-info))
	 (machine-id (caar (message-machine-info msg))))
	
      (set-message-type! msg 'ACCEPT)
      (set-message-machine-info! 
       msg 
       (append (message-machine-info msg) 
	       (list (list (p 'id) 
			   (+ (p 'load) process-load)
			   (- (p 'memory) mem)))))
      (mail-to machine-id msg)
      (let
	  ((l (make-log-entry 'RUNNING)))
	(l 'set-processor! p)
	(l 'set-process-id! (car (get-pid-and-load process-info)))
	(l 'set-process-load! (cadr (get-pid-and-load process-info)))
	(log-it (create-log-entry-list l)))
      (run-process pid proc process-load mem p parent)
      (let
	  ((l (make-log-entry 'FINISHED)))
	(l 'set-processor! p)
	(l 'set-process-id! (car (get-pid-and-load process-info)))
	(l 'set-process-load! (cadr (get-pid-and-load process-info)))
	(log-it (create-log-entry-list l))))))



;  Whenever a process creation request comes into the system, the request
;  should be enqueued until such time that this processor receives the
;  token.  


(define accept-creation-requests
  (lambda (p msg)
    (let
	((q (p 'waiting-jobs)))
      (enqueue! q msg))))




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


; Forward a processor request message to the machine with "machine-id",
; appending to the request message the local-machine-id and the
; local-load-average. 

(define forward-request
  (lambda (p msg dest-id)
    (set-message-type! msg 'REQUEST)
    (set-message-machine-info! 
     msg 
     (append (message-machine-info msg) 
	     (list (list (p 'id) 
			 (p 'load)
			 (p 'memory)))))
    (mail-to dest-id msg)))





;  probably need to edit this procedure a bit, perhaps including broadcast
;  and ring management message headers

; This procedure should be spawned as a thread and run continuously.  It
; monitors for incoming messages and dispatches these messages to the
; appropriate functions. (i.e.,  accept-or-forward-process)

(define receive-requests-primitive
  (lambda (p)
    (let*
	((msg (get-mail (p 'id)))
	 (id (message-id msg))
	 (type (message-type msg))
	 (process-info (message-process-info msg))
	 (machine-info (message-machine-info msg))
	 (token-info (message-token-info msg)))
      
      (set-message-hop-count! msg (+ (message-hop-count msg) 1))
      
;      (for-each host-display `("processor " 
;                               ,(p 'id) 
;                               " got a msg of type"  
;                               ,type))
      
      (cond

       ; request to run a process
       ;  processor should accept the process and run it, and ack to the
       ;  requestor. 
       
       ((equal? type 'REQUEST)
	(let
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (l 'set-process-id! (car (get-pid-and-load process-info)))
	  (l 'set-process-load! (cadr (get-pid-and-load process-info)))
	  (log-it (create-log-entry-list l)))
	(spawn (lambda () (accept-process p msg))))

       ; a process creation request.  Request should be enqueued until token
       ; is received.  Then it should be offloaded to the best
       ; processor. 
       
       ((equal? type 'CREATE)
	(let
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (l 'set-process-id! (car (get-pid-and-load process-info)))
	  (l 'set-process-load! (cadr (get-pid-and-load process-info)))
	  (log-it (create-log-entry-list l)))
	(accept-creation-requests p msg))
       
       ; acknowledgment that a processor has accepted a process and will run
       ; it 
       
       ((equal? type 'ACCEPT)
	(let
	    ((l (make-log-entry 'ACCEPT)))
	  (l 'set-processor! p)
	  (l 'set-process-id! (car (get-pid-and-load process-info)))
	  (l 'set-process-load! (cadr (get-pid-and-load process-info)))
	  (l 'set-hop-count! (message-hop-count msg))
	  (log-it (create-log-entry-list l))))

       
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
	(p 'set-usage-table! (make-integer-table))
	(receive-token p msg))

       
       ((equal? type 'DONE)
	(if 
	 (not (= (p 'id) (message-parent msg)))
	 (p 'set-remote-jobs! (- (p 'remote-jobs) 1))))
       
       ; this processor should shutdown and no longer monitor the network
       ; for messages.			
       
       ((equal? type 'SHUTDOWN)
	(let 
	    ((l (make-log-entry type)))
	  (l 'set-processor! p)
	  (log-it (create-log-entry-list l))))))))

    



; procedure that continously runs receives-requests-primitive. i.e., always
; monitors for and dispatches incoming messages

(define receive-requests
  (lambda (p)
    (receive-requests-primitive p)
    (if
     (p 'alive?)
     (receive-requests p)
     '())))				; is there something else to return
					; besides the empty list??





; Given a list containing '(pid proc load), return the list '(pid load)

(define get-pid-and-load
  (lambda (ls)
    (list (car ls) (caddr ls))))



; Mail a msg to the machine with the given id.

(define mail-to
  (encap
   (lambda (id msg)
     (let
	 ((mbx (table-ref *mailboxes* id)))
       (if mbx
	   (mailbox-write mbx msg)
	   (error "mailbox does not exist" id msg))))))



(define get-mail
  (encap
   (lambda (id)
     (let
	 ((mbx (table-ref *mailboxes* id)))
       (if mbx
	   (mailbox-read mbx)
	   (error "mailbox does not exist" id))))))



(define put-mail
  (encap
   (lambda (id)
     (let
	 ((mbx (table-ref *mailboxes* id)))
       (if mbx
	   (mailbox-read mbx)
	   (error "mailbox does not exist" id))))))




(define no-mail?
  (encap
   (lambda (id)
     (let
	 ((mbx (table-ref *mailboxes* id)))
       (if mbx
	   (mailbox-empty? mbx)
	   (error "mailbox does not exist" id))))))



; Create a table of mailboxes where processor-id is the index into the
; table. 

(define *mailboxes* (make-integer-table))


; (for-each add-mailbox! processor-list)

; Add a mailbox to *mailboxes* table, keyed by a processor-id

(define add-mailbox!
  (lambda (p)
    (table-set! *mailboxes* (p 'id) (p 'mailbox))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initialization procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Simulate a processor in the distributed system.  Must determine processor
; id and propogate this id to every machine in the network, so that each
; machine knows which other machines exist in the network.

(define create-processor
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (rogue count make-processor count))))




; Spawn a daemon procedure which monitors for incoming messages.

(define start-daemon
  (lambda (p)
					; rogue?
    (remote-run! (uid->aspace (p 'id))
		 (lambda ()
		   (spawn (lambda () ((p 'daemon) p)))))))




; Start all the processor daemons

(define start-all-daemons
  (lambda ()
    (for-each start-daemon (processor-list 'self))))



; Initialize all usage tables in the processor list "p-list"

(define init-all-usage-tables
  (lambda (p-list)
    '()))



; Initialize a single usage-table.  n is the number of machines in the
; distributed system.  Better than n would be a list of all processor id's. 

; This procedure is kind of a hack.  Instead of assuming all processors have
; an initial load of 0 and initial free memory of 16, it should use actual
; values from the processor.  This would require looking at each processor
; to construct the usage tables.  probably the best plan of attack is to
; have each processor broadcast its load and free memory at startup.  Right
; now, this procedure assumes a static, predetermined environment.


(define init-one-usage-table
  (lambda (usage-table p-list)
    (cond
     ((null? p-list) usage-table)
     (else
      (table-set! usage-table 
		  ((car p-list) 'id)
		  (list 0 16 (real-time)))
      (init-one-usage-table usage-table (cdr p-list))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Procedures for Processes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Returns a procedure that sleeps for the desired number of milliseconds.

(define make-process
  (lambda (run-time)
    (lambda ()
      (host-display "starting")
      (host-newline)
      (sleep run-time)
      (host-display "finished")
      (host-newline))))
	  


; Runs a process.  Changes the load information before and after the process
; runs and adds the jobs to the job list

(define run-ds-process
  (lambda (pid proc load mem p parent)
    (p 'set-load! (+ load (p 'load)))
    (p 'set-memory! (- (p 'memory) mem))
;    (update-local-load p (processor-usage-table p))
    (p 'set-running-jobs! (cons pid (p 'running-jobs)))
    (proc)
    (p 'set-running-jobs! (delete-1st pid (p 'running-jobs)))
    (p 'set-memory! (+ (p 'memory) mem))
    (p 'set-load! (- (p 'load) load))
;    (update-local-load p (processor-usage-table p))
    (let 
	((msg (make-message 'DONE '() '())))
      (set-message-parent! msg (p 'id))
      (mail-to parent msg))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Token procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;  When a token is received by a processor, that processor should do the
;  following: (1) Update the token's usage table with its current load.  (2)
;  Offload any new processes.  (3) Forward the token to the next processor
;  in the logical ring.

(define receive-token
  (lambda (p msg)
    (let*
	((usage-table (message-token-info msg)))
      (update-local-load p usage-table)
      (offload-processes p usage-table)
      (forward-token p msg))))
	


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
			     (make-integer-table) (processor-list 'self)))))
      (mail-to ((car (processor-list 'self)) 'id) token))))


;  This process should be run continuously by the coordinator in order to
;  determine whether the token has been lost.  If it has, then the
;  coordinator should generate a new one.


(define watch-for-lost-token
  (lambda ()
    '()))


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
    (let
	((next-member (next-processor p (processor-list 'self)))) ; (p 'plist)
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
	  (car (processor-list 'self))	; (p 'plist)
	  (cadr p-list)))
     (else
      (next-processor p (cdr p-list))))))


; Returns the processor given its id

(define find-processor
  (lambda (id p-list)
    (cond
     ((null? p-list) (error "processor not in list" id (processor-list 'self)))
     ((= id ((car p-list) 'id)) 
      (car p-list))
     (else
      (find-processor id (cdr p-list))))))


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
      (processor-list 'add (create-processor))
      (init-ring (- n 1))))))




;; Add a member to the ring

(define add-member-to-ring
  (lambda (p)
    (processor-list 'add p)))



;; Remove a member from the ring

(define remove-member-from-ring
  (lambda (p)
    (processor-list 'remove p)))



;; Elect coordinator.  The coordinator must be elected at startup and
;; whenever the coordinator fails.  The coordinator is responsible for
;; genereating the token and for monitoring for lost or duplicate tokens. 

(define elect-coordinator
  (lambda ()
    '()))




;;  the process should be run by all processors in order to determine if a
;;  coordinator has failed.  If the coordinator has failed, then a new
;;  coordinator must be elected.

(define watch-for-dead-coordinator
  (lambda ()
    '()))




;; Broadcast message to group
;;  What type of group communication is this going to be?  Does there need
;;  to be some type of lock on mailboxes such that messages are received in
;;  the order they are sent?   I think that simple FIFO broadcast semantics
;;  should be sufficient, although for unforseen reasons an Atomic FIFO
;;  broadcast could be necessary, in which case some type of lock (mutex)
;;  will need to be placed on all the mailboxes while a group broadcast is
;;  taking place.  In other words, this should appear in a critical section
;;  (i.e., this thread should not be preempted.  it should run until
;;  finished).  can probably use a "with-lock"

(define group-broadcast
  (lambda (group msg)
    '()))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Debugging Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define show-stats
  (lambda ()
    (for-each show-status (processor-list 'self))))


(define show-status
    (lambda (p)
      (rogue (p 'id)
	     (lambda ()
	       (let ((p (find-processor (p 'id) (processor-list 'self))))
		 (host-display (p 'id))
		 (host-display ": ")
		 (host-display (if (no-mail? (p 'id))
				   "mailbox empty"
				   "has message(s)"))
		 (host-newline)
		 (host-display "jobs: ")
		 (host-display (p 'running-jobs))
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
			     (p 'usage-table)))))))



;; Given a scheme form, write it to the logfile

(define *logfile-lock* (make-lock))

(define log-it
  (encap
   (lambda (ls)
     (spawn
      (lambda ()
	(obtain-lock *logfile-lock*)
	(write ls logfile)
	(release-lock *logfile-lock*))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Distributed Implementation Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define host-display
  (encap
   display))

(define host-newline
  (encap
   newline))

