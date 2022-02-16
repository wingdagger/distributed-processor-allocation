;; -*- Mode: Scheme; -*- 
;;
;; gather.scm           Gather up comrades "automatically".
;;
;; James S. Jennings
;; Dept. of Computer Science
;; Tulane University
;;

;; ,build (lambda (ignore) (become-comrade)) /tmp/remote-ds.image

;; Shell script: praline[1]  start ernst moseley elvis
;    #!/bin/bash
;    echo -n "" > /tmp/comrades
;    for m in "$@"
;    do 
;     rsh $m uname -n >> /tmp/comrades
;     rsh $m /opt/bin/remote-mover >> /tmp/comrades &sleep 5
;    done
;

(letrec ((read-host-and-port-numbers-from-file
	  (lambda (file)		; logfile
	    (with-input-from-file file
	      (lambda ()
		(let* ((read-host read)	; host name from 'start' script
		       (read-6		; ignore the message:
			(lambda ()	; "Waiting for connection on port"
			  (let loop ((i 0))
			    (if (= 5 i)
				(read)	; keep the port number
				(begin (read) (loop (+ i 1))))))))
		  (let loop ((answer '()))
		    (let ((host (read-host)))
		      (if (eof-object? host)
			  answer
			  (loop (cons 
				 (cons 
				  (symbol->string host)
				  (read-6)) answer))))))))))
	 
	 ;; 
	 ;; Interface:
	 ;;

	 (connect
	  (lambda ()
	    (let ((rads (read-host-and-port-numbers-from-file *file*)))
	      (let loop ((rest rads)
			 (id 1))
		(if (not (null? rest))
		    (begin
		      (for-each display (list id ":" (car rest) #\\newline))
		      (loop (cdr rest) (+ id 1)))))
	      (commissar rads))))

	 ;;
	 ;; Customization variables:
	 ;;

	 (*file* "/tmp/comrades"))
  
  (newline)
  (connect))

 
