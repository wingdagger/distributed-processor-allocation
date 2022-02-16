; -*- Mode: scheme; -*-


(define-interface ds-interface
  (export  show-stats
	   add-processor-to-ds
	   remove-processor-from-ds
	   create-process
	   set-system-scheduler!
	   set-migrate-processes!
	   ))
  

(define-structure ds
  ds-interface
  (open scheme
	custom-threads
	remote-prims
	signals
	locks
	custom-placeholders
	define-record-types
	ascii
	handle
	sort
	time
	utils
	processes
	features
	aspaces
	comrades
	debugging
	conditions
	scheduler
	linked-queues
	getenv
	randomness
	table
	ds-scheduler)
  (files config
	 distrib
	 mailbox
	 utils
	 mail
	 objects
	 stats
	 logging
	 debug
	 tests
	 misc
	 usage-table
	 quasi
	 processes))
