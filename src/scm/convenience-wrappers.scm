

(define (gnc:query-dialog message default-answer
                          yes-button? ok-button? no-button? cancel-button?)
  ;; Show yes/no/cancel dialog box with given message.
  ;;
  ;; display message, and wait for a yes, no, or cancel, depending on
  ;; the arguments.  Each of the *-button? arguments indicates whether
  ;; or not the dialog should contain a button of that type.
  ;; default-answer may be set to 'yes, 'ok, 'no or 'cancel.  If you
  ;; allow both yes and OK buttons, and set 'yes or 'ok as the default
  ;; answer, which button is the default is undefined, but the result
  ;; is the same either way, and why would be doing that anyhow?
  ;;
  ;; This function returns #t for yes (or OK), #f for no, or 'cancel.
  ;;
  ;; NOTE: This function does not return until the dialog is closed.")
  
  (let* ((default (case default-answer
                    ((yes) 1)
                    ((ok) 1)
                    ((no) 0)
                    ((cancel) -1)))
         (result
          (gnc:_query-dialog-lowlev_
           message default yes-button? ok-button? no-button? cancel-button?)))
    (case result
      ((1) #t)
      ((0) #f)
      ((-1) 'cancel))))

(define (gnc:message-dialog message)
  (let ((result (gnc:query-dialog message 'ok #f #t #f #f)))
    #t))
