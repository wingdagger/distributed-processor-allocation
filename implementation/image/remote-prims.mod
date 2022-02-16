(define-interface remote-prims-interface
  (export  encap
	   rogue
	   encap-result-on-id
	   ))
  

(define-structure remote-prims
  remote-prims-interface
  (open scheme
	custom-threads
	proxies
	aspaces
	signals
	handle
	conditions
	scheduler
	linked-queues)
  (files remote-prims))
