;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  remote.scm
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



;; Coded by Dr. James Jennings


;; for convenience when not inside a with-team (or even within...)

(define rogue                           ; short for rogue-apply
  (lambda (uid proc . args)
    (remote-apply
     (uid->aspace uid) 
     (lambda ()
       (apply safe-apply proc args)))))


(define safe-apply
  (lambda (proc . args)
    (call-with-current-continuation
     (lambda (k)
       (with-handler (lambda (c punt)
                       (if (error? c)
                           (k c)
                           (punt)))
                     (lambda ()
                       (apply proc args)))))))

;; and then: (exception? (safe-apply foo args)
;;       or: (error? (safe-apply foo args)


;; encapsulation procedure (from Kali paper)
;; this encap no longer works
;(define encap 
;  (lambda (proc)
;    (let ((proxy (make-proxy proc)))
;      (lambda args
;        (remote-apply (proxy-owner proxy)
;                      (lambda ()
;                        (apply (proxy-local-ref proxy) args)))))))


;; an encapsulation procedure which avoids calling remote-apply when
;; possible because the remote-apply in Kali 0.47 spawns a thread every
;; time. 

(define encap
  (lambda (proc)
    (let ((proxy (make-proxy proc)))
      (lambda args
        (if (eq? (local-aspace) (proxy-owner proxy))
            (apply (proxy-local-ref proxy) args)
            (remote-apply (proxy-owner proxy)
                          (lambda ()
                            (apply (proxy-local-ref proxy) args))))))))



;; run proc on id, and encap the result there.  useful when proc is a
;; make-foo-proc that needs to close over local data.

(define encap-result-on-id
  (lambda (id proc)
    (remote-apply (uid->aspace id) (lambda () (encap (proc))))))

