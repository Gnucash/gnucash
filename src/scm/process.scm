;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process.scm - manage sub-processes.
;;; Copyright 2001 Rob Browning <rlb@cs.utexas.edu>
;;; 
;;; This program is free software; you can redistribute it and/or    
;;; modify it under the terms of the GNU General Public License as   
;;; published by the Free Software Foundation; either version 2 of   
;;; the License, or (at your option) any later version.              
;;;                                                                  
;;; This program is distributed in the hope that it will be useful,  
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;;; GNU General Public License for more details.                     
;;;                                                                  
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "process.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run the program specified by path with the given args as a
;;; sub-proces.  If envt is not #f, then use it as the sub-process
;;; environment (as per execle in the guile info pages).  Note that
;;; you must specify the path explicitly.
;;;
;;; Returns #f on failure, or
;;; (pid child-output-pipe child-input-pipe child-standard-error-pipe)
;;; on success.  Right now the standard-error pipe is always #f.
;;;
;;; For example:
;;;
;;;   (run-sub-process "/bin/date" "--rfc-822")
;;;

(define (run-sub-process envt path . args)
  (let ((parent-to-child-pipe (false-if-exception (pipe)))
        (child-to-parent-pipe (false-if-exception (pipe))))
    (if (not (and parent-to-child-pipe
                  child-to-parent-pipe))
        #f
        (let* ((parent-read-pipe (car child-to-parent-pipe))
               (parent-write-pipe (cdr parent-to-child-pipe))
               (child-read-pipe (car parent-to-child-pipe))
               (child-write-pipe (cdr child-to-parent-pipe))
               (pid (false-if-exception (primitive-fork))))
          
          (if (not (zero? pid))
              ;; we're the parent
              (begin
                (close-input-port child-read-pipe)
                (close-output-port child-write-pipe)
                (list pid parent-read-pipe parent-write-pipe #f))
              ;; else we're the child
              (begin
                ;; set standard-input and standard-output at the fd
                ;; level -- which is really all that matters since
                ;; we're about to exec...
                (close-input-port parent-read-pipe)
                (close-output-port parent-write-pipe)
                (dup->fdes child-read-pipe 0)
                (dup->fdes child-write-pipe 1)
                ;; now launch the child process.
                (or (false-if-exception
                     (if envt
                         (apply execle path envt args)
                         (apply execl path args)))
                    (exit 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Random test code.
;;;

; (define (get-1-quote exchange . items)
;   (let ((cmd (apply list 'fetch exchange items))
;         (quoter (run-sub-process #f
;                                  "./scmio-finance-quote"
;                                  "./scmio-finance-quote")))
;     (and quoter
;          (write cmd (caddr quoter))
;          (newline (caddr quoter))
;          (force-output (caddr quoter))
;          (let ((result (read (cadr quoter))))
;            (close-input-port (cadr quoter))
;            (close-output-port (caddr quoter))
;            result))))

; (define (parrot)
;   (let loop ((input (false-if-exception (read))))
;     (cond
;      ((eof-object? input) (quit 0))
;      ((not input) (quit 0))
;      (else (write input)
;            (force-output)
;            (loop (read))))))

; (define (launch-parrot envt path args)
;   ;; Returns (pid child-input-port child-output-port child-error-port)
;   ;; Right now the error port is broken...
  
;   (let* ((pid #f)
;          (sockets (false-if-exception (socketpair AF_UNIX SOCK_STREAM 0))))
    
;     (if sockets
;         (set! pid (false-if-exception (primitive-fork))))
    
;     (cond
;      ((not pid) #f)

;      ((= pid 0)
;       ;; We're the child.

;       ;; set standard-input and standard-output, swapping input and
;       ;; output sockets from parent...
;       (display 'foo) (newline) (flush-all-ports)
;       ;;(redirect-port (car sockets) (current-input-port))
;       (set-current-input-port (cdr sockets))
;       (display 'bar) (newline) (flush-all-ports)
;       ;;(redirect-port (cdr sockets) (current-output-port))
;       (set-current-output-port (cdr sockets))

;       (parrot))
     
;      ; (or (false-if-exception
;      ;      (if envt
;      ;          (apply execle path envt args)
;      ;          (apply execl path args)))
;      ;     (exit 1)))

;      (else
;       ;; we're the parent
;       ;;          child-input-port child-output-port child-error-port
;       (list pid (car sockets) #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This code was part of an attempt to just return one
;;; read-write-port for the child, but I had some trouble getting it
;;; to work.  I think either (1) this was misguided from the start
;;; since you can't hook up the plumbing this way, or (2) I was
;;; forgetting some flushing or something somewhere that kept it from
;;; working.  At one point, I knew which of these two options was
;;; true, but I can't recall what I concluded now, so I'll leave the
;;; code here in case we want to resurrect it...

; (define (run-sub-process envt path . args)
;    (let ((pid #f)
;          (sockets (false-if-exception (socketpair AF_UNIX SOCK_STREAM 0))))
    
;      (if sockets
;          (set! pid (false-if-exception (primitive-fork))))
    
;      (cond
;       ((or (not sockets) (not pid)) #f)
     
;       ((= pid 0)
;        ;; We're the child: set standard-input and standard-output to be
;        ;; the socket that's connected to the parent.
;        (set-current-input-port (cdr sockets))
;        (set-current-output-port (cdr sockets))
;        (dup->fdes (cdr sockets) 0)
;        (dup->fdes (cdr sockets) 1)
      
;        ;; now launch the child process.
;        (or (false-if-exception
;             (if envt
;                 (apply execle path envt args)
;                 (apply execl path args)))
;            (exit 1)))

;       (else
;        ;; we're the parent
;        (list pid (car sockets) #f)))))
