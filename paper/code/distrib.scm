;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  distrib.scm
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


(define make-processor-list
  (if (eq? *environment* 'SIMULATION)
      make-processor-list-primitive
      (encap
       make-processor-list-primitive)))


(define dist-make-processor
  (lambda (id daemon)
    (remote-apply
     (uid->aspace id)
     (lambda ()
       (encap
	(make-processor id daemon))))))



(define mail-to
  (if (eq? *environment* 'SIMULATION)
      mail-to-primitive
      (encap
       mail-to-primitive)))

(define get-mail
  (if (eq? *environment* 'SIMULATION)
      get-mail-primitive
      (encap
       get-mail-primitive)))

(define no-mail?
  (if (eq? *environment* 'SIMULATION)
      no-mail?-primitive
      (encap
       no-mail?-primitive)))

(define log-it
  (if (eq? *environment* 'SIMULATION)
      log-it-primitive
      (encap
       log-it-primitive)))


(define host-display
  (if (eq? *environment* 'SIMULATION)
      display
      (encap
       display)))
  
(define host-newline
  (if (eq? *environment* 'SIMULATION)
      newline
      (encap
       newline)))

(define show-status
  (if (eq? *environment* 'SIMULATION)
      show-status-primitive
      (lambda (p)
	(rogue (p 'id) show-status-primitive p))))
  
(define group-broadcast
  (if (eq? *environment* 'SIMULATION)
      group-broadcast-primitive
      (encap 
       group-broadcast-primitive)))


(define group-broadcast-pr
  (if (eq? *environment* 'SIMULATION)
      group-broadcast-pr-primitive
      (encap 
       group-broadcast-pr-primitive)))

(define real-time
  (if (eq? *environment* 'SIMULATION)
      real-time
      (encap
       real-time)))