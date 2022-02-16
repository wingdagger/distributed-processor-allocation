;;  ,exec ,load build-scm

(config '(load "/shared/ernst/mover/lib/modules/custom-threads.mod"))
(config '(load "/shared/ernst/mover/lib/modules/utils.mod"))
(config '(load "/shared/ernst/mover/lib/modules/randomness.mod"))
(config '(load "/home/allen/thesis/implementation/image/ds-scheduler.mod"))
(config '(load "/home/allen/thesis/implementation/image/remote-prims.mod"))
(config '(load "/shared/ernst/mover/lib/modules/unix.mod"))

(user '(load-package custom-placeholders)) 
(user '(load-package random))
(user '(load-package floatnums))
(user '(load-package custom-threads))
(user '(load-package remote-prims))
(user '(load-package ds-scheduler))
(user '(load-package utils))
(user '(load-package randomness))
(user '(load-package processes))
(user '(load-package getenv))


(user '(open handle conditions custom-threads remote-prims
	     ds-scheduler locks queues
	     custom-placeholders tables comrades aspaces random 
	     time sort signals utils randomness processes getenv
	     debugging floatnums define-record-types ascii features))

;; This is now done by the shell script 'build':
;;(user '(dump "/shared/ernst/old/ds/ds.image" "Distrbuted System v2.02"))

