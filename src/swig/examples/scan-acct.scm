
(define (list-accts filename)
  (let ((db (xaccReadAccountGroup filename)))
    (do ((total (xaccGroupGetNumAccounts db))
         (i 0 (+ i 1)))
        ((= i total))
      (let ((acct (xaccGroupGetAccount db i)))
        (write (xaccAccountGetName acct))))))
