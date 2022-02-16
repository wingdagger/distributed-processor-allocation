;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  usage-table.scm
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
;;       (if
;;	(<= mem (cadr data))		; check memory requirments
	(if
	 (or
	  (not lowest-load)
	  (> lowest-load (car data)))
	 (begin
	   (set! lowest-processor key)
	   (set! lowest-load (car data)))))
     usage-table)
    lowest-processor)))



; Determine whether or not to accept a process from another processor based
; upon local load information and the load information in the request
; message. 

(define accept-or-forward-process
  (lambda (p msg)
    (let*
	((machine-info (message-machine-info msg))
	 (req-load (cadar machine-info))
	 (process-info (message-process-info msg))
	 (mem (cadddr process-info))
	 (hop-count (message-hop-count msg)))
      (cond
       ;; if the local load is less the requestor's load OR is if the hop
       ;;  count has exceeded the number of processors in the system, then
       ;;  accept the process and run it
       ((or 
	 (<= (p 'load) req-load)
	 (> hop-count (+ (length (get-processor-list)) 1)))
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
	 (pid (car process-info))
	 (p-rec (message-process-rec msg))
	 (process-load (caddr process-info))
	 (mem (cadddr process-info))
	 (hop-count (message-hop-count msg)))

      ;; Tell originating machine that the process is accepted
      (let
	  ((accept-rply (make-message 'ACCEPT process-info '()))
	   (machine-info #f))
	
	(set-message-parent! accept-rply (p 'id))
	(if (eq? *environment* 'SIMULATION)
	    (set! machine-info
		  (append (message-machine-info msg) 
			  (list (list (p 'id) 
				      (+ (p 'load) process-load)
				      (- (p 'memory) mem)))))
	    (set! machine-info
		  (append (message-machine-info msg)
			  (list (list (p 'id)
				      (p 'load)
				      (p 'memory))))))
	(set-message-machine-info! accept-rply machine-info)
	(mail-to (p-rec 'last-owner) accept-rply))
      
      ;; LOG process start
      (let
	  ((l (make-log-entry 'PROCESS-RUNNING)))
	(l 'set-processor-id! (p 'id))
	(l 'set-process-id! (car (get-pid-and-load process-info)))
	(l 'set-hop-count! hop-count)
	(log-it (create-log-entry-list l)))
      
      ;; RUN the process
      (let
	  ((exit-code
	    (run-ds-process pid p-rec p)))
	
	(cond
	 ((equal? exit-code 'DONE)
	  ;; LOG process completion
	  (let
	      ((l (make-log-entry 'PROCESS-FINISHED)))
	    (l 'set-processor-id! (p 'id))
	    (l 'set-process-id! (car (get-pid-and-load process-info)))
	    (l 'set-hop-count! hop-count)
	    (log-it (create-log-entry-list l)))
	  
	  ;; Let originating machine know that process is completed
	  (let 
	      ((finished-msg (make-message 'DONE process-info '())))
	    (set-message-parent! finished-msg (p 'id))
	    (set-message-machine-info! 
	     finished-msg
	     (append (message-machine-info msg) 
		     (list (list (p 'id) 
				 (p 'load)
				 (p 'memory)))))
	    (mail-to (p-rec 'originator) finished-msg))))))))
      



; Forward a processor request message to the machine with "machine-id",
; appending to the request message the local-machine-id and the
; local-load-average. 

(define forward-request
  (lambda (p msg dest-id)

;    (host-display (p 'id))
;    (host-display " not accepting process")
;    (host-newline)
;    (host-display (table->list (p 'usage-table)))
;    (host-newline)
;    (host-display "destination: ")
;    (host-display dest-id)
;    (host-newline)
    
    (set-message-type! msg 'REQUEST)
    (set-message-machine-info! 
     msg 
     (append (message-machine-info msg) 
	     (list (list (p 'id) 
			 (p 'load)
			 (p 'memory)))))
    (mail-to dest-id msg)))


; Initialize all usage tables

(define init-all-usage-tables
  (lambda (p-list)
    (cond
     ((null? p-list) '())
     (else
      (init-one-usage-table ((car p-list) 'usage-table)
			    (processor-list 'self))
      (init-all-usage-tables (cdr p-list))))))


; This procedure is kind of a hack.  Instead of assuming all processors have
; an initial load of 0 and initial free memory of 32, it should use actual
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
		  (list 0 32 (real-time)))
      (init-one-usage-table usage-table (cdr p-list))))))

