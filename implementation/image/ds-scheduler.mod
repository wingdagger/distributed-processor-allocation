(define-interface ds-scheduler-interface
  (export  new-ds-scheduler
	   run-ds-scheduler
	   ))
  

(define-structure ds-scheduler
  ds-scheduler-interface
  (open scheme
	custom-threads
	remote-prims
	signals
	handle
	conditions
	scheduler
	linked-queues)
  (files ds-scheduler))
