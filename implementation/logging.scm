;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  logging.scm
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
;;  Scheme-readable logging procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Machine readable logfile

(define logfile 
  (open-output-file 
   "/tmp/ds-logfile.ds"))



;; Changed logging such that it varies depending on type of log info being
;; recorded.  (create-log-entry) should be a case statement.  Currently, the
;; different types of logs are
;;
;; LOG, SERVER-LOG, MSG, PROCESS-CREATE, PROCESS-RUNNING, PROCESS-FINISHED,
;; PROCESSOR, SHUTDOWN, TOKEN
;;
;; 
;; Perhaps, LOG should be renamed to LOAD.
;;

;; LOG, SERVER-LOG: shows the load and memory usage of a processor.  Each
;; processor generates one of these every n seconds.  SERVER-LOG is the
;; same, except that it is generated by the centralized server in the
;; centralized algorithm

;; MSG: generated whenever a message is received by a processor.  Contains
;; the hop-count, size, message-id, message type, and processor id of the
;; processor receiving the message.  This will be used to generate data
;; about then number and size of messages generated by the system in order
;; to calculate overhead.

;; PROCESS-CREATE, PROCESS-RUNNING, PROCESS-FINISHED: generated during the
;; lifetime of a process in order to measure the time between process
;; creation, acceptance and completion.  Logs the processor-id of where the
;; process is concepted or where it runs and the process id of the process.
;; Also logged is the hop count of the messsage which sent the process
;; request. 

;; PROCESSOR:  Generated whenever a processor is initialized in the DS.
;; Records only the time and processor id

;; SHUTDOWN:  Generated whenver a processor shuts down.  Logs the time and
;; processor id

;; TOKEN:  Shows the usage table of the token.


;; returns a list to be written to the logfile.  Has the following
;; format depending on the type of log entry

(define create-log-entry-list
  (lambda (l)
    (case (l 'type)
      
      ;; This is where a processor logs itself every n seconds
      
      ((LOG SERVER-LOG)
       (list (real-time) (l 'type) (processor->list (l 'processor))))

      ;; A message has been received by a machine.  Log its size, id, type,
      ;; hop-count, and which processor received it
      
      ((MSG)
       (list (real-time) (l 'type) (l 'processor-id) (l 'msg-id)
	     (l 'msg-size) (l 'hop-count) (l 'msg-type)))

      ;; A process has been accepted and is running or a process has
      ;; completed execution.  Or a process has been created
      
      ((PROCESS-CREATE PROCESS-RUNNING PROCESS-FINISHED PROCESS-MIGRATION)
       (list (real-time) (l 'type) (l 'processor-id)
	     (l 'process-id) (l 'hop-count)))

      ;; Log processor creation or shutdown
      
      ((PROCESSOR SHUTDOWN)
       (list (real-time) (l 'type) (l 'processor-id)))

      ;; A token has been received by a machine
      
      ((TOKEN)
       (list (real-time) (l 'type) (processor->list (l 'processor))))

      (else
       (host-display "Unknown log entry type ")
       (host-display (l 'type))
       (host-newline)
       (list (real-time) (l 'type) (processor->list (l 'processor)) 
	     (list (l 'process-id) (l 'process-load)))))))



(define list->log-entry
  (lambda (ls)
    (let 
	((log-entry (make-log-entry (cadr ls))))
      (log-entry 'set-creation-time! (list-ref ls 0))
      (case (cadr ls)			; type
	
	;; This is where a processor logs itself every n seconds
	
	((LOG SERVER-LOG)
	 (log-entry 'set-processor! (list->processor (list-ref ls 2))))
	
	;; A message has been received by a machine.  Log its size, id, type,
	;; hop-count, and which processor received it
	
	((MSG)
	 (log-entry 'set-processor-id! (list-ref ls 2))
	 (log-entry 'set-msg-id! (list-ref ls 3))
	 (log-entry 'set-msg-size! (list-ref ls 4))
	 (log-entry 'set-hop-count! (list-ref ls 5))
	 (log-entry 'set-msg-type! (list-ref ls 6)))
	
	;; A process has been accepted and is running or a process has
	;; completed execution.  Or a process has been created
	
	((PROCESS-CREATE PROCESS-RUNNING PROCESS-FINISHED)
	 (log-entry 'set-processor-id! (list-ref ls 2))
	 (log-entry 'set-process-id! (list-ref ls 3))
	 (log-entry 'set-hop-count! (list-ref ls 4)))
	
	;; Log processor creation or shutdown
	
	((PROCESSOR SHUTDOWN)
	 (log-entry 'set-processor-id! (list-ref ls 2)))

	;; A token has been received by a machine
	
	((TOKEN)
	 (log-entry 'set-processor! (list->processor (list-ref ls 2))))
	
	(else
	 (host-display "Unknown log entry type ")
	 (host-display (ls 'type))
	 (host-newline)))
      log-entry)))

  



;; turns processor information into a list

(define processor->list
  (lambda (p)
    (list (p 'id) (p 'load) 
	  (table-map
	   (lambda (k d)
	     k)
	   (p 'running-jobs))
	  (table->list (p 'usage-table)))))



;; turns a list of processor information into a processor record

(define list->processor
  (lambda (ls)
    (let
	((p (create-processor)))
      (p 'set-id! (list-ref ls 0))
      (p 'set-load! (list-ref ls 1))
      (p 'set-running-jobs! (list-ref ls 2))
      (p 'set-usage-table! (list->usage-table (list-ref ls 3)))
      p)))



;; usage-table will be a list made by table-walk.

(define table->list
  (lambda (table) 
    (let ((ls '()))
      (table-walk
	(lambda (k d)
          (set! ls (cons (list k d) ls)))
        table)
      ls)))


;; turns a list of usage table info into a table

(define list->usage-table
  (lambda (ls)
    (let 
	((tbl (make-table)))
      (for-each 
       (lambda (item)
	 (table-set! tbl (car item) (cadr item)))
       ls)
      tbl)))




;; Given a scheme form, write it to the logfile

(define *logfile-lock* (make-lock))


(define log-processor-start
  (lambda (p)
    (let 
	((l (make-log-entry 'PROCESSOR)))
      (l 'set-processor! p)
    (log-it (create-log-entry-list l)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Human readable logfile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define tab

(define tab (ascii->char 9))


; Start log

(define init-user-log
  (lambda (out-port)
    (host-display "Time" out-port)
    (host-display tab out-port)
    (host-display "Header" out-port)
    (host-display tab out-port)
    (host-display tab out-port)
    (host-display "Machine" out-port)
    (host-display tab out-port)
    (host-display "M-load" out-port)
    (host-display tab out-port)
    (host-display "Jobs" out-port)
    (host-display tab out-port)
    (host-display "Data" out-port)
    (host-newline out-port)
    (host-display tab out-port)
    (host-display "Usage Table" out-port)
    (host-newline out-port)
    
    (host-display "=======" out-port)
    (host-display tab out-port)
    (host-display "=======" out-port)
    (host-display tab out-port)
    (host-display tab out-port)
    (host-display "=======" out-port)
    (host-display tab out-port)
    (host-display "=======" out-port)
    (host-display tab out-port)
    (host-display "=======" out-port)
    (host-display tab out-port)
    (host-display "=======" out-port)
    (host-newline out-port)
    
    
    (host-display (real-time) out-port)
    (host-display tab out-port)
    (host-display " " out-port)
    (host-display tab out-port)
    (host-display " " out-port)
    (host-display tab out-port)
    (host-display "distributed system started" out-port)
    (host-newline out-port)))



; Log shutdown of the system and close the out-port

(define finish-user-log
  (lambda (out-port)
    (host-display (real-time) out-port)
    (host-display tab out-port)
    (host-display " " out-port)
    (host-display tab out-port)
    (host-display " " out-port)
    (host-display tab out-port)
    (host-display "distributed system shutting down" out-port)
    (host-newline out-port)))
  


; Convert a single scheme list to a human readable format, writing it to the
; log.

; '(540174 create (2 0. () ((2 0) (1 0))) (1001 2.3))

;; Needs to be changed to incorporate different log entry types


(define pretty-print
  (lambda (entry out-port)
    (let 
	((type (cadr entry)))
      (case type
	((LOG)
	 (host-display (car entry) out-port)	; time
	 (host-display tab out-port)
	 (host-display (cadr entry) out-port)	; message header (command)
	 (host-display tab out-port)
	 (let
	     ((p (caddr entry))
	      (data (cdddr entry)))
	   (host-display (car p) out-port)	; processor-id
	   (host-display tab out-port)		 
	   (host-display (cadr p) out-port)	; processor-load 
	   (host-display tab out-port)		
	   (host-display (caddr p) out-port)	; running jobs
	   (host-display tab out-port)
	   (host-display data out-port)	; process-id (pid) and load
	   (host-newline out-port)
	   (list->ascii-table (cadddr p) out-port) ; usage-table
	   (host-newline out-port)))
	(else
	 (host-display (car entry) out-port)	; time
	 (host-display tab out-port)
	 (host-display (cadr entry) out-port)	; message header (command)
	 (host-display tab out-port)
	 (host-newline out-port))))))
	 
      


; Convert a list to a table, writing it to outport

(define list->ascii-table
  (lambda (ls outport)
    (cond
     ((null? ls) '())
     (else
      (host-display tab outport)
      (host-display (car ls) outport)
      (host-newline outport)
      (list->ascii-table (cdr ls) outport)))))




; Convert scheme-readable log to a human-readable form

(define convert-log
  (lambda (in-port out-port)
    (let 
	((item (read in-port)))
      (cond
       ((eof-object? item) 
	(close-input-port in-port))
       (else
	(pretty-print item out-port)
	(convert-log in-port out-port))))))


; Send a request to all processors to update the log

(define log-all-processors
  (lambda ()
    (let*
	((msg (make-message 'LOG '() '()))
	 (log-all-processors-helper
	  (lambda (p)
	    (mail-to (p 'id) msg))))
      (for-each log-all-processors-helper (processor-list 'self)))))


; Continuously calls log-all-procesors, sleeping for 2 seconds in between
; each call
	    
(define always-log-all-processors
  (lambda ()
    (log-all-processors)
    (sleep 6000)
    (always-log-all-processors)))


(define log-it-primitive
  (lambda (ls)
    (spawn
     (lambda ()
       (obtain-lock *logfile-lock*)
       (write ls logfile)
       (release-lock *logfile-lock*)))))
