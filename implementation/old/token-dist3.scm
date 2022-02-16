;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  token-dist3.scm
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





;; Load distributed procedures

(load "~/thesis/implementation/remote.scm")





(define make-processor-list
  (lambda ()
    (encap
     (let ((list '()))
       (lambda (msg . args)
	 (case msg
	   ((self) list)
	   ((add +) (set! list (cons (car args) list)))
	   ((remove -) (set! list (delete-1st (car args) list)))))))))


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

