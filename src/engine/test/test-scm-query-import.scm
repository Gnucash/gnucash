;; test-scm-query-import.scm
;; load the engine and test the import of some old-style scheme queries

(use-modules (gnucash gnc-module))

(define query-list
  (list
   '((terms (((pd-account pr-account #t acct-match-any ("39d55759e0561fe7c7a5b6a99deb17a0"))))) (primary-sort by-standard) (secondary-sort by-none) (tertiary-sort by-none) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -1))
   ))

(define (run-test)
  (gnc:module-system-init)
  (gnc:module-load "gnucash/engine" 0)

  (let* ((session (gnc:session-new))
         (book (gnc:session-get-book session))
	 (failures #f))
    
    (for-each
     (lambda (query-scm)
	     (let* ((q (gnc:scm->query query-scm))
		    (q2 (gnc:query->scm q)))
	       (if (or (null? q) (not q))
		   (begin
		     (set! failures #t)
		     (display query-scm)
		     (display "\n")
		     (display q2)
		     (display "\n")))))
     query-list)

    (not failures)))
