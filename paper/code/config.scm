;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  config.scm
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

;; What environment do you want to use? simulation, quasi, or full
;; implementation? 

;;(define *environment* 'SIMULATION)
(define *environment* 'QUASI-SIMULATION)
;;(define *environment* 'FULL-IMPLEMENTATION)

;; If a processor's load is > this threshold, the process will migrate one
;; of its processes to a more lightly loaded processor
(define *process-migration-load-threshold* 4)

;; In order for a processor to migrate a process the difference between its
;; load and the minimum loaded processor must be at least this
(define *process-migration-load-diff-threshold* 3)

;; How often the process migration procedure runs.
(define *process-migration-interval* 30000)

;; Amount of time a processor waits to receive an ACCEPT message for an
;; offloaded process before invoking fault tolerance procedures
(define *timeout-wait-period* 720000)

;; Amount of time a processor waits to receive an ALIVE reply from another
;; processor before invoking fault tolerance procedures
(define *processor-timeout* 720000)

;; Should process migration occur?
(define *migrate-processes* #f)

;; How often should the coordinator check to see if it has seen the token?
(define *token-threshold* 720000)

;; How often should machines in the system check to see if the coordinator
;; is alive?
(define *coordinator-threshold* 720000)

;; This scheduler is used for everybody in the simulation environment
(define *system-scheduler* #f)

;; This is the home directory of me

(define *home-dir* (getenv "HOME"))

(define set-system-scheduler!
  (lambda (arg)
    (set! *system-scheduler* arg)))

(define set-migrate-processes!
  (lambda (arg)
    (set! *migrate-processes* arg)))
