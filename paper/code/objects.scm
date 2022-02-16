;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  objects.scm
;;  written by Joshua S. Allen
;;  Tulane University
;;  Department of Electrical Engineering and Computer Science
;;
;;  Masters Thesis
;;  Advisor:  Dr. Mark Benard
;;  May 1998
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; When distributing the algorithms, we need to use a global time across the
;; system, therefore my-real-time might be redefined as encap so that it
;; runs only on one machine, therefore imposing a global time ordering.
;; However, this is not very effecient and a there might be a better way.

(define my-real-time
  (lambda ()
    (real-time)))

; List of all processors currently in the system.

(define processor-list (make-processor-list))
;;(define processor-list #f)


(define make-processor-list-primitive
  (lambda ()
    (let ((list '()))
      (lambda (msg . args)
	(case msg
	  ((self) list)
	  ((add +) (set! list (cons (car args) list)))
	  ((remove -) (set! list (delete-1st (car args) list))))))))
	   
(define get-processor-list
  (encap
   (lambda ()
     (processor-list 'self))))



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
  running-jobs waiting-jobs remote-jobs plist alive? process-locations
  process-timeouts processor-timeouts scheduler)
  processor?                          ;; type predicate
  (id          processor-id  set-processor-id!)  ;; <slot> <accessor> <set!>
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
  (alive?      processor-alive?      set-processor-alive!)
  (process-locations processor-process-locations 
		     set-processor-process-locations!)
  (process-timeouts  processor-process-timeouts
		     set-processor-process-timeouts!)
  (processor-timeouts processor-processor-timeouts
		      set-processor-processor-timeouts!)
  (scheduler   processor-scheduler   set-processor-scheduler!))




;; Constructor (Initial values)


(define make-processor-primitive
  (lambda (id daemon sched)
    (for-each display (list "Begin of make-processor-primitive for id: " id #\\newline))
    (really-make-processor
     id					; processor id
     daemon			; network daemon
     (make-mailbox)			; mailbox
     (make-integer-table)		; usage-table
     0.0				; load average
     32					; amount of free memory
     (make-table)			; running jobs -- process table
     (make-queue)                       ; list of waiting job requests
     0					; number of remote jobs
     (make-processor-list)		; processor list (for next processor)
     #t					; processor alive?
     (make-table)			; process locations
     (make-table)			; process-timeouts
     (make-table)			; processor-timeouts
     sched)))






;;  Object

(define make-processor
  (lambda (id daemon)
    (let ((sched #f))
      (if (eq? *environment* 'SIMULATION)
	  (set! sched *system-scheduler*)
	  (set! sched (run-ds-scheduler (new-ds-scheduler))))
      ;; dameon returns a daemon procedure, so call it
      (let ((p (make-processor-primitive id (daemon) sched)))
	(lambda (msg . args)
	  (case msg
	    ((set-id!) (set-processor-id! p (car args)))
	    ((set-usage-table!) (set-processor-usage-table! p (car args)))
	    ((set-load!) (set-processor-load! p (car args)))
	    ((set-memory!) (set-processor-memory! p (car args)))
	    ((set-running-jobs!) (set-processor-running-jobs! p (car args)))
	    ((set-waiting-jobs!) (set-processor-waiting-jobs! p (car args)))
	    ((set-remote-jobs!) (set-processor-remote-jobs! p (car args)))
	    ((set-plist!) (set-processor-plist! p (car args)))
	    ((set-alive!) (set-processor-alive! p (car args)))
	    ((set-process-locations!)
	     (set-processor-process-locations! p (car args)))
	    ((set-process-timeouts!)
	     (set-processor-process-timeouts! p (car args)))
	    ((set-processor-timeouts!)
	     (set-processor-processor-timeouts! p (car args)))
	    ((set-scheduler!)
	     (set-processor-scheduler! p (car args)))
	    ((usage-table-set!)
	     (table-set! (processor-usage-table p) (car args) (cadr args)))
	    
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
	    ((process-locations) (processor-process-locations p))
	    ((process-timeouts) (processor-process-timeouts p))
	    ((processor-timeouts) (processor-processor-timeouts p))
	    ((scheduler) (processor-scheduler p))
	    ((processor) p)
	    ((usage-table-ref)
	     (table-ref (processor-usage-table p) (car args)))
	    (else
	     (error "Invalid message to processor-object" msg))))))))


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
  (really-make-message  id type process-info process-rec parent 
			machine-info token-info hop-count creation-time)
  message?                ;; type predicate
  (id            message-id)         ;; <slot> <accessor> <set!>
  (type          message-type           set-message-type!)
  (process-info  message-process-info)
  (process-rec   message-process-rec  set-message-process-rec!)
  (parent        message-parent         set-message-parent!)
  (machine-info  message-machine-info   set-message-machine-info!)
  (token-info    message-token-info     set-message-token-info!)
  (hop-count     message-hop-count      set-message-hop-count!)
  (creation-time message-creation-time))



(define make-message-primitive
  (let ((id 0))
  (lambda (type process-info machine-info token-info)
    (set! id (+ id 1))
    (really-make-message
     id
     type
     process-info
     #f					; process rec
     0					; parent
     machine-info
     token-info
     0					; hop count
     (my-real-time)))))


(define make-message
  (lambda (type process-info machine-info)
    (make-message-primitive
     type
     process-info
     machine-info
     '())))


(define message-size
  (lambda (msg)
    256))

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
;;  Log object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type log-entry           ;; record name
  :log-entry                            ;; "type" indicator (not important)
  ;; message maker function
  (really-make-log-entry type processor process-id processor-id msg-id
			 msg-type msg-size hop-count creation-time time2)
  log-entry?                ;; type predicate
  (type         log-entry-type   set-log-entry-type!)
  (processor    log-entry-processor  set-log-entry-processor!)
  (process-id     log-entry-process-id       set-log-entry-process-id!)
  (processor-id   log-entry-processor-id     set-log-entry-processor-id!)
  (msg-id   log-entry-msg-id     set-log-entry-msg-id!)
  (msg-type       log-entry-msg-type     set-log-entry-msg-type!)
  (msg-size       log-entry-msg-size     set-log-entry-msg-size!)
  (hop-count    log-entry-hop-count      set-log-entry-hop-count!)
  (creation-time  log-entry-creation-time set-log-entry-creation-time!)
  (time2        log-entry-time2          set-log-entry-time2!))



(define make-log-entry-primitive
  (lambda (type)
    (really-make-log-entry type '() 0 0 0 0 0 0 0 0)))


(define make-log-entry
  (lambda (type)
    (let ((p (make-log-entry-primitive type)))
      (lambda (msg . args)
	(case msg
	  ((set-type!) (set-log-entry-type! p (car args)))
	  ((set-processor!) (set-log-entry-processor! p (car args)))
	  ((set-process-id!) (set-log-entry-process-id! p (car args)))
	  ((set-processor-id!) (set-log-entry-processor-id! p (car args)))
	  ((set-msg-id!) (set-log-entry-msg-id! p (car args)))
	  ((set-msg-type!) (set-log-entry-msg-type! p (car args)))
	  ((set-msg-size!) (set-log-entry-msg-size! p (car args)))
	  ((set-hop-count!) (set-log-entry-hop-count! p (car args)))
	  ((set-creation-time!) (set-log-entry-creation-time! p (car args)))
	  ((set-time2!) (set-log-entry-time2! p (car args)))
	  ((type) (log-entry-type p))
	  ((processor) (log-entry-processor p))
	  ((process-id) (log-entry-process-id p))
	  ((processor-id) (log-entry-processor-id p))
	  ((msg-id) (log-entry-msg-id p))
	  ((msg-type) (log-entry-msg-type p))
	  ((msg-size) (log-entry-msg-size p))
	  ((hop-count) (log-entry-hop-count p))
	  ((creation-time) (log-entry-creation-time p))
	  ((time2) (log-entry-time2 p))
	  (else
	   (error "Unknown method in log-entry object " msg)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Process Record
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type process-rec           ;; record name
  :process-rec                            ;; "type" indicator (not important)
  ;; message maker function
  (really-make-process-rec thunk wait-var migration-cnt originator
			   last-owner load mem pid)
  process-rec?                                 ;; type predicate
  (thunk       process-rec-thread   set-process-rec-thread!)
  (wait-var     process-rec-wait-var  set-process-rec-wait-var!)
  (migration-cnt process-rec-migration-cnt  set-process-rec-migration-cnt!)
  (originator   process-rec-originator  set-process-rec-originator!)
  (last-owner  process-rec-last-owner  set-process-rec-last-owner!)
  (load        process-rec-load  set-process-rec-load!)
  (mem         process-rec-mem   set-process-rec-mem!)
  (pid         process-rec-pid   set-process-rec-pid!))


(define make-process-rec-primitive
  (lambda (thunk wait-var originator pid)
    (really-make-process-rec thunk wait-var 0 originator originator
			     0 0 pid)))


(define make-process-rec
  (lambda (thunk wait-var originator pid)
    (let ((p (make-process-rec-primitive thunk wait-var originator pid)))
      (lambda (msg . args)
	(case msg
	  ((set-thread!) (set-process-rec-thread! p (car args)))
	  ((set-wait-var!) (set-process-rec-wait-var! p (car args)))
	  ((set-migration-cnt!)
	   (set-process-rec-migration-cnt! p (car args)))
	  ((set-originator!) (set-process-rec-originator! p (car args)))
	  ((set-last-owner!) (set-process-rec-last-owner! p (car args)))
	  ((set-load!) (set-process-rec-load! p (car args)))
	  ((set-mem!) (set-process-rec-mem! p (car args)))
	  ((set-pid!) (set-process-rec-pid! p (car args)))
	  ((inc-migration-cnt)
	   (set-process-rec-migration-cnt! 
	    p (+ (process-rec-migration-cnt p) 1)))
	  ((thread) (process-rec-thread p))
	  ((wait-var) (process-rec-wait-var p))
	  ((migration-cnt) (process-rec-migration-cnt p))
	  ((originator) (process-rec-originator p))
	  ((last-owner) (process-rec-last-owner p))
	  ((load) (process-rec-load p))
	  ((mem) (process-rec-mem p))
	  ((pid) (process-rec-pid p))
	  (else
	   (error "Unknown method in process-rec object " msg)))))))
