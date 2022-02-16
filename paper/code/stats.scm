;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  stats.scm
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
;;  Performance Measurements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; to do:
; filter and sort the data to make the following list:
;  ( (data for proc 1 sorted by time)     <--- sublist for proc 1
;    (data for proc 2 sorted by time)
;    ...
;  )
;
; then can make usage tables out of this structure by assuming that the
; times for the nth entry in each sublist above are roughly the same.
; it's these usage tables that can be processed to produce, e.g.:
;   ( (time1 max-difference-in-usages) (time2 max-diff...) )
; 
; then can average the max-diff's over time to get a single scalar
; performance measure (although a graph of just the max-diff's might be more
; illuminating).
;
; a good set of scalars which summarize the graph might be:
;  average difference
;  maximum difference
;  minimum difference
;  median difference (less sensitive to extremes)
;  std deviation



; write to a gnuplot file the "time and load" data for processor proc
; data is (flatten-1
;           (make-all-usage-tables-from-file (open-input-file "ds-logfile") 3)
;           '())

(define *data-file-prefix* "/tmp/data")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Processor statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (write-usage-data proc data)
  (with-output-to-file (string-append *data-file-prefix*
				      (number->string proc)
				      ".ds")
    (lambda ()
      (for-each (lambda (e)
		  (host-display (caddr e))
		  (host-display " ")
		  (host-display (cadr e))
		  (host-newline))
		(sort-list
		 (filter (lambda (e) (= proc (car e))) data)
		 (lambda (a b)		; sort by time
		   (< (caddr a) (caddr b))))))))


(define write-all-usage-data-files
    (lambda (num data)
      (let loop ((n num))
	(if (zero? n)
	    #t
	    (begin
	      (host-display "Writing ")
	      (host-display (string-append *data-file-prefix*
				      (number->string n)
				      ".ds"))
	      (write-usage-data n data)
	      (host-newline)
	      (loop (- n 1)))))))



; Calculate the maximum difference in jobs between two processors
; '(((3 3.2 380) (3 2.2 1380)) ((2 4.4 401) (2 2.4 1401)) ((1 2. 411) (1
; 3. 1400))


(define calculate-max-unfairness
  (lambda (usage-table)
    (let
	((min #f)
	 (max #f))
      (letrec
	  ((calculate-unfairness-helper
	    (lambda (usage-table)
	      (cond
	       ((null? usage-table) (- max min))
	       (else
		(let
		    ((data (cadar usage-table)))
		  (if (or
		       (not min)
		       (< data min))
		      (set! min data))
		  (if (or
		       (not max)
		       (> data max))
		      (set! max data))
		  (calculate-unfairness-helper (cdr usage-table))))))))
	(calculate-unfairness-helper usage-table)))))



; Calculate average load for a single point in time

(define calculate-average
  (lambda (usage-table)
    (let
	((sum 0)
	 (count 0))
      (letrec
	  ((helper
	    (lambda (usage-table)
	      (cond
	       ((null? usage-table) (/ sum count))
	       (else
		(set! sum (+ sum (cadar usage-table)))
		(set! count (+ count 1))
		(helper (cdr usage-table)))))))
	(helper usage-table)))))
				    


; Calculate standard deviation for a single point in time

(define calculate-stddev
  (lambda (usage-table)
    (let 
	((avg (calculate-average usage-table))
	 (sum 0)
	 (count 0))
      (letrec
	  ((helper
	    (lambda (usage-table)
	      (cond
	       ((null? usage-table) (/ sum count))
	       (else
		(set! sum (+ sum (abs (- avg (cadar usage-table)))))
		(set! count (+ count 1))
		(helper (cdr usage-table)))))))
	(helper usage-table)))))



; Calculate standard deviation over time.  Returns a list of the following
; format: '((time stddev) (time stddev) ... )

(define make-list-of-stddev-over-time
  (lambda (usage-table-list)
    (let
	((stddev-list '()))
      (letrec
	  ((helper
	    (lambda (usage-table-list)
	      (cond
	       ((null? usage-table-list) stddev-list)
	       (else
		(let
		    ((this-table (car usage-table-list)))
		  (set! stddev-list
			(append stddev-list
				(list (list
				       (caddar this-table)  ; time
				       (calculate-stddev
					this-table)))))) ; stddev
		(helper (cdr usage-table-list)))))))
	(helper usage-table-list)))))


(define make-list-of-avg-unfairness-over-time
  (lambda (usage-table-list)
    (let
	((avg-unfair-list '()))
      (letrec
	  ((helper
	    (lambda (usage-table-list)
	      (cond
	       ((null? usage-table-list) avg-unfair-list)
	       (else
		(let
		    ((this-table (car usage-table-list)))
		  (set! avg-unfair-list
			(append avg-unfair-list
				(list (list
				       (caddar this-table)  ; time
				       (calculate-average
					this-table)))))) ; average
		(helper (cdr usage-table-list)))))))
	(helper usage-table-list)))))



; usage-table-list: '(((id load time) (id load time) ... ) ...)
; where each sublist is a usage table, constructed in order of processor-id,
; and the whole list is sorted by time.

(define make-list-of-unfairness-over-time
  (lambda (usage-table-list)
    (let
	((unfairness-list '()))
      (letrec
	  ((helper
	    (lambda (usage-table-list)
	      (cond
	       ((null? usage-table-list) unfairness-list)
	       (else
		(let
		    ((this-table (car usage-table-list)))
		  (set! unfairness-list
			(append unfairness-list
				(list (list
				       (caddar this-table)  ; time
				       (calculate-max-unfairness 
					this-table)))))) ; diff
		(helper (cdr usage-table-list)))))))
	(helper usage-table-list)))))





; unfairness-list: '((time diff) ... )


(define write-max-unfairness-data
  (lambda (unfairness-list)
    (let 
	((outport (open-output-file "/tmp/max-unfair-data.ds")))
      (letrec
	  ((helper
	    (lambda (unfairness-list)
	      (cond
	       ((null? unfairness-list) (host-newline outport)
					(close-output-port outport))
	       (else
		(host-display (caar unfairness-list) outport) ;time
		(host-display " " outport)
		(host-display (cadar unfairness-list) outport) ;unfairness
		(host-newline outport)
		(helper (cdr unfairness-list)))))))
	(host-display "Writing /tmp/max-unfair-data.ds")
	(host-newline)
	(helper unfairness-list)))))


(define write-avg-unfairness-data
  (lambda (unfairness-list)
    (let 
	((outport (open-output-file "/tmp/avg-load-data.ds")))
      (letrec
	  ((helper
	    (lambda (unfairness-list)
	      (cond
	       ((null? unfairness-list) (host-newline outport)
					(close-output-port outport))
	       (else
		(host-display (caar unfairness-list) outport) ;time
		(host-display " " outport)
		(host-display (cadar unfairness-list) outport) ;avg. unfairness
		(host-newline outport)
		(helper (cdr unfairness-list)))))))
	(host-display "Writing /tmp/avg-load-data.ds")
	(host-newline)
	(helper unfairness-list)))))



;; Write standard deviation data to file

(define write-stddev-data
  (lambda (stddev-list)
    (let 
	((outport (open-output-file "/tmp/stddev-data.ds")))
      (letrec
	  ((helper
	    (lambda (stddev-list)
	      (cond
	       ((null? stddev-list) (host-newline outport)
					(close-output-port outport))
	       (else
		(host-display (caar stddev-list) outport) ;time
		(host-display " " outport)
		(host-display (cadar stddev-list) outport) ;stddev
		(host-newline outport)
		(helper (cdr stddev-list)))))))
	(host-display "Writing /tmp/stddev-data.ds")
	(host-newline)
	(helper stddev-list)))))


  
  
; write statistcis to files.  These files will be used by gnuplot to plot
; each processor's load average over time and to plot the unfairness over
; time. 


(define make-load-gnuplot-data
  (lambda (load-data num)
    (let*
	((sorted-by-machine (sort-by-machine (reorder-data num load-data)))
	 (same-length (make-lists-same-length sorted-by-machine))
	 (usage-table-list (reverse (make-usage-tables 
				     (reverse same-length)))))
      (write-all-usage-data-files num load-data)
      (write-max-unfairness-data (make-list-of-unfairness-over-time 
				  usage-table-list))
      (write-avg-unfairness-data (make-list-of-avg-unfairness-over-time
				  usage-table-list))
      (write-stddev-data (make-list-of-stddev-over-time
			  usage-table-list)))))
	 





;; find the shortest list, and make all other lists this same length
;; THIS SHOULD BE CALLED AFTER REORDER_DATA

(define make-lists-same-length
  (lambda (all-lists)
    (let
	((shortest-ls-cnt (cnt-lists all-lists)))
      (letrec
	  ((list-shortener
	    (lambda (ls)
	      (if (> (length ls) shortest-ls-cnt)
		  (list-shortener (remove-nth (length ls) ls))
		  ls))))
	(letrec
	    ((shorten-all-lists
	      (lambda (old-ls new-ls)
		(cond
		 ((null? old-ls) new-ls)
		 (else
		  (shorten-all-lists (cdr old-ls)
				     (append 
				      new-ls
				      (list 
				       (list-shortener (car old-ls))))))))))
	   (shorten-all-lists all-lists '()))))))
		 

;;  Return the length of the shortest sublists within a list
;;


(define cnt-lists
  (let ((cnt #f))
    (lambda (all-lists)
      (cond
       ((null? all-lists) cnt)
       ((or (not cnt)
	    (< (length (car all-lists)) cnt))
	(set! cnt (length (car all-lists)))
	(cnt-lists (cdr all-lists)))
       (else
	(cnt-lists (cdr all-lists)))))))
       
	  
	      







;;   take the load-data and sort it into sublists, where each sublist
;;   contains data for only one machine.  This sublist should be sorted by
;;   time. 


(define reorder-data
  (lambda (num-processors data)
    (let loop ((proc 1)
	       (results '()))
      (if (> proc num-processors)
	  results
	  (loop (+ proc 1)
		(cons (filter (lambda (entry)
				(= proc (car entry)))
			      data)
		      results))))))


(define sort-by-machine
  (lambda (reordered-data)
    (map 
     (lambda (entries)
       (sort-list entries (lambda (a b)                ; sort by time
			    (< (caddr a) (caddr b)))))
     reordered-data)))




;;   constructs usage table by taking the nth element of each sublist,
;;   assuming that this data was written at approxiamately the same time,
;;   and returns this list to the calling procedure


(define make-usage-tables
    (lambda (sorted-data)
      (let loop ((data sorted-data)
                 (results '()))
        (if (null? (car data))          ; assumes all lists same length!
            results
            (loop (map cdr data)
                  (cons (map car data) results))))))

 






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Message Statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Write all msg statistics to files for plotting

(define plot-msg-stats
  (lambda (msg-data)

    ;; Counts for each type of message
    (let 
	((msg-cnt-file (open-output-file "/tmp/msg-type-counts.ds"))
	 (msg-cnt-tbl (make-table)))
      
      (for-each 
       (lambda (item)
	 (count-msg-type item msg-cnt-tbl))
       msg-data)
      
      (host-display "Writing message counts to /tmp/msg-type-counts.ds")
      (host-newline)
      (write-stats msg-cnt-tbl msg-cnt-file)
      (close-output-port msg-cnt-file))
    
    ;; Sizes for each type of message
    (let 
	((msg-size-file (open-output-file "/tmp/msg-type-sizes.ds"))
	 (msg-size-tbl (make-table)))
      
      (for-each
       (lambda (item)
	 (count-msg-sizes item msg-size-tbl))
       msg-data)
      
      (host-display "Writing message sizes to /tmp/msg-type-sizes.ds")
      (host-newline)
      (write-stats msg-size-tbl msg-size-file)
      (close-output-port msg-size-file))))




;; Counts for each type of message

(define count-msg-sizes
  (lambda (item msg-tbl)
    (if (table-ref msg-tbl (item 'msg-type))
	(table-set! 
	 msg-tbl 
	 (item 'msg-type)
	 (+ (table-ref msg-tbl (item 'msg-type)) 
	    (item 'msg-size)))
	(table-set!
	 msg-tbl
	 (item 'msg-type)
	 (item 'msg-size)))))




;; Counts for each type of message

(define count-msg-type
  (lambda (item msg-tbl)
    (if (table-ref msg-tbl (item 'msg-type))
	(table-set! 
	 msg-tbl 
	 (item 'msg-type)
	 (+ (table-ref msg-tbl (item 'msg-type)) 1))
	(table-set!
	 msg-tbl
	 (item 'msg-type)
	 1))))



;; Write message stats to file

(define write-stats
  (lambda (table outport) 
    (table-walk
     (lambda (k d)
       (host-display k outport)
       (host-display tab outport)
       (host-display d outport)
       (host-newline outport))
     table)))

  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Process statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define plot-process-stats
  (lambda (process-data)

    ;; hop counts
    
    (let
	((hop-count-file (open-output-file "/tmp/process-hop-counts.ds"))
	 (hop-tbl (make-table)))
      (for-each
       (lambda (item)
	 (count-request-hops item hop-tbl))
       ;; count hops only when process starts running, so filter out
       ;; everything else			
       (filter
	(lambda (item)
	  (equal? 'process-running (item 'type)))
	process-data))

      (host-display "Writing hop count stats to /tmp/process-hop-counts.ds")
      (host-newline)
      (write-stats hop-tbl hop-count-file)
      (close-output-port hop-count-file))
    
    (let*
	((process-data-file (open-output-file "/tmp/process-data-stats.ds"))
	 (data-tbl (make-table))
	 (p-ids (remove-duplicates
		 (map 
		 (lambda (item)
		   (item 'process-id))
		 process-data)))
	 (processes-grp 
	  (map
	   (lambda (p-id)
	     (filter
	      (lambda (process)
		(equal? p-id (process 'process-id)))
	      process-data))
	   p-ids)))
      
      (for-each
       (lambda (item)
	 (make-process-stat-tbl item data-tbl))
       processes-grp)
      
      (host-display "Writing process stats to /tmp/process-data-stats.ds")
      (host-newline)
      (write-process-stats data-tbl process-data-file)
      (close-output-port process-data-file))))


(define make-process-stat-tbl
  (lambda (item data-tbl)
    (table-set! data-tbl
		((car item) 'process-id)
		(list 
		 ((car item) 'creation-time) ; process-create time
		 ((cadr item) 'creation-time) ; process-running time
		 ((caddr item) 'creation-time))))) ; process-finished time



(define write-process-stats
  (lambda (data-tbl outport)
    (table-walk
     (lambda (k d)
       (host-display k outport)
       (host-display tab outport)
       (for-each
	(lambda (item)
	  (host-display item outport)
	  (host-display tab outport))
	d)
       (host-newline outport))
     data-tbl)))
    
	 
      
	 
      
	 


;; Counts for each type of message

(define count-request-hops
  (lambda (item hop-tbl)
    (if (table-ref hop-tbl (item 'hop-count))
	(table-set! 
	 hop-tbl 
	 (item 'hop-count)
	 (+ (table-ref hop-tbl (item 'hop-count))
	    1))
	(table-set!
	 hop-tbl
	 (item 'hop-count)
	 1))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main Stat Procedures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Reads the logfile to construct load information of each processor over
;; time. 

;; check to make sure processor-id is not 0.

;; Needs to be modified to return a list of data like:
;;   '(load-data hop-count-data process-time-data other-data)
;;
;; The calling procedure needs to disect into constituent parts


(define get-data-from-file
  (lambda (in-port)
    (let
	((load-data '())
	 (msg-data '())
	 (process-data '()))
      (letrec
	  ((helper
	    (lambda ()
	      (let
		  ((item (read in-port)))
		(cond
		 ((eof-object? item) 
		  (close-input-port in-port)
		  (list load-data msg-data process-data))
		 (else
		  (case (cadr item)
		    ((LOG)
		     (if (not 
			  (equal? 0 (caaddr item))) ;processor-id
			 (set! load-data 
			       (cons (get-usage-info item) load-data))))
		    ((MSG)
		     (set! msg-data 
			   (cons (get-usage-info item) msg-data)))
		    ((PROCESS-CREATE PROCESS-RUNNING PROCESS-FINISHED)
		     (set! process-data
			   (cons (get-usage-info item) process-data))))
		  (helper)))))))
	(helper)))))



;  Given a single read from the logfile, return a list containing the
;  machine-id and the load-information.

;; This should be altered to handle all types of log messages.  Also, it
;; should store the results in log-entry data structures, not in a list.  In
;; order to handle the log-entry data structures for LOG types, the
;; corresponding load-data procedures need to be changed to handle the data
;; structure instead of lists.


(define get-usage-info
  (lambda (item)
    (let
	((type (cadr item)))
      (case type
	((LOG)
	 (let*
	     ((time (car item))
	      (processor-info (caddr item))
	      (processor-id (car processor-info))
	      (load (cadr processor-info)))
	   (list processor-id load time)))
	((MSG PROCESS-CREATE PROCESS-RUNNING PROCESS-FINISHED)
	 (list->log-entry item))))))
      





;; Write all statistical information to files
    

(define make-stat-data
  (lambda (num)
    ;; if centralized algorithm, then we have one less machine because one
    ;;  of the machines is the coordinator
    (if (eq? *algorithm* 'CENTRALIZED)
	(set! num (- num 1)))
    (let*
	((input-file
	  (open-input-file 
	   "/tmp/ds-logfile.ds"))
	 (log-data (get-data-from-file input-file))
	 (load-data (car log-data))
	 (msg-data (cadr log-data))
	 (process-data (caddr log-data)))
      
      ;; Write the load data to files, including the load average of
      ;; processors over time, max-unfairness, avg, and stddev.
      (make-load-gnuplot-data load-data num)
      
      ;; Write all message statistics, inlcuding counts and sizes
      (plot-msg-stats msg-data)
      
      
      (plot-process-stats process-data))))
    



    
    
    
