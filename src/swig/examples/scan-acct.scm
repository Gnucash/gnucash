
(define (list-accts filename)
  (let ((db (xaccReadAccountGroup filename)))
    (do ((total (xaccGetNumAccounts db))
         (i 0 (+ i 1)))
        ((= i total))
      (let ((acct (xaccGetAccountFromID db i)))
        (write (xaccAccountGetName acct))))))
