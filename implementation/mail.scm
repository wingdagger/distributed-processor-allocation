;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  mail.scm
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

(load "~/lib/scm/mailbox.scm")

; Mail a msg to the machine with the given id.

(define mail-to-primitive
  (lambda (id msg)
    (if (not id)
	(error "Invalid id given to mail-to-primitive"))
    (let
	((mbx (table-ref *mailboxes* id)))
      (if mbx
	  (mailbox-write mbx msg)
	  (error "mailbox does not exist" id msg)))))



(define get-mail-primitive
  (lambda (id)
    (let
	((mbx (table-ref *mailboxes* id)))
      (if mbx
	  (mailbox-read mbx)
	  (error "mailbox does not exist" id)))))



(define no-mail?-primitive
  (lambda (id)
    (let
	((mbx (table-ref *mailboxes* id)))
      (if mbx
	  (mailbox-empty? mbx)
	  (error "mailbox does not exist" id)))))



; Create a table of mailboxes where processor-id is the index into the
; table. 

(define *mailboxes* (make-integer-table))


; (for-each add-mailbox! processor-list)

; Add a mailbox to *mailboxes* table, keyed by a processor-id

(define add-mailbox!
  (lambda (p)
    (table-set! *mailboxes* (p 'id) (p 'mailbox))))

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

(define group-broadcast-primitive
  (lambda (group msg)
    (for-each 
     (lambda (id)
       (mail-to id msg))
     group)))


(define group-broadcast-pr-primitive
  (lambda (group-pr msg)
    (let
	((group
	  (map 
	   (lambda (pr)
	     (pr 'id))
	   group-pr)))
      
      (for-each 
       (lambda (id)
	 (mail-to id msg))
       group))))
  
