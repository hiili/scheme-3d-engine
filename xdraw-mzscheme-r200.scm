;;;
;;; xdraw-mzscheme.scm -- Interface to xdraw for MzScheme
;;;
;;; This file is placed in the public domain by the author,
;;; Riku Saikkonen <Riku.Saikkonen@hut.fi>.
;;;
;;; $Id: xdraw-mzscheme.scm,v 1.45 2000/11/28 20:16:29 rjs Exp $
;;;
;;; See README.Scheme for documentation.
;;;
;;; These non-R5RS features of MzScheme are used: error, process*,
;;; flush-output, sleep, semaphores, exceptions, find-executable-path
;;;
;;; ---
;;; 
;;; This file is originally from the xdraw package by Riku Saikkonen, available
;;; at http://users.tkk.fi/u/rsaikkon/software/xdraw.html. It has been modified
;;; to work with PLT Scheme v200. -Paul Wagner

;; FORMAT-XDRAW-COMMAND: Formats a command into a string to send to
;; xdraw
(define (format-xdraw-command . command)
  (define (flatten l)
    (apply append l))
  (define (string->xdraw s)
    (define (char->escaped-char-list c)
      (cond ((eq? c #\newline) (list #\\ #\n))
	    ((eq? c #\\) (list #\\ #\\))
	    ((eq? c #\") (list #\\ #\"))
	    (else (list c))))
    (list->string (flatten (append (list (list #\"))
				   (map char->escaped-char-list
					(string->list s))
				   (list (list #\"))))))
  (define (symbol->xdraw sym)
    (symbol->string sym))
  (define (number->xdraw x)
    (number->string (inexact->exact (round x))))
  (define (list->xdraw x)
    (string-append "( " (list-contents->xdraw x) " )"))
  (define (any->xdraw x)
    (cond ((number? x) (number->xdraw x))
	  ((symbol? x) (symbol->xdraw x))
	  ((string? x) (string->xdraw x))
	  ((pair? x) (list->xdraw x))	  
	  (else (error "XDRAW: Invalid element in command:" x))))
  (define (list-contents->xdraw l)
    (define (insert-spaces l)		; insert " " between elements
      (cond ((null? l) '())
	    ((null? (cdr l)) l)
	    (else (cons (car l) (cons " " (insert-spaces (cdr l)))))))
    (apply string-append (insert-spaces (map any->xdraw l))))
  (string-append (list-contents->xdraw command) (string #\newline)))

;; START-XDRAW: Starts a copy of xdraw, returning an xdraw object.
;; filename = path and file name for the xdraw executable
;; cmdline-args = strings to give as command-line arguments
;;
;; Notes/bugs:
;;  * send, send-with-result and get-event may block (but shouldn't
;;    block in practice)
;;  * wait-for-errors might not wait long enough for xdraw to react
;;  * if there is an error that aborts the evaluator, the semaphore is
;;    not released, so further messages to xdraw will block
(define (start-xdraw filename . cmdline-args)
  (define (make-queue)			; a simple FIFO queue
    (let ((front-ptr '())
          (rear-ptr '())
          (len 0))
      (define (insert! item)
        (let ((old-rear rear-ptr))
          (set! rear-ptr (cons item '()))
          (if (empty?)
              (set! front-ptr rear-ptr)
              (set-cdr! old-rear rear-ptr)))
        (set! len (+ len 1)))
      (define (remove!)
        (if (empty?)
            (error "XDRAW: QUEUE: Tried to remove from empty queue"))
        (let ((item (car front-ptr)))
          (set! front-ptr (cdr front-ptr))
          (set! len (- len 1))
          (if (empty?)
              (set! rear-ptr '()))      ; for garbage collection
          item))
      (define (peek)
        (if (empty?)
            (error "XDRAW: QUEUE: Tried to peek from empty queue"))
        (car front-ptr))
      (define (flush!)
        (let ((elems front-ptr))
          (set! front-ptr '())
          (set! rear-ptr '())
          (set! len 0)
          elems))
      (define (empty?)
	(null? front-ptr))
      (define (queue-dispatch m . args)
	(cond ((eq? m 'insert!) (apply insert! args))
              ((eq? m 'peek) (peek))
	      ((eq? m 'remove!) (remove!))
	      ((eq? m 'empty?) (empty?))
              ((eq? m 'length) len)
              ((eq? m 'flush!) (flush!))
	      (else (error "XDRAW: QUEUE: Invalid message received" m))))
      queue-dispatch))
  (let ((synchronizer (make-semaphore 1))
	(display-commands #f)
	(error-handler display)
	(wait-for-errors #f)
        (command-buffer-maxsize 0)
        (command-buffer (make-queue))
	(cleaned-up #f)
	(event-queue (make-queue)))
    (let-values (((proc-handle stdout-port stdin-port stderr-port) (apply subprocess #f #f #f filename cmdline-args)))
      (define (read-all port)		; read all there is to a string
	(define (read-chars)
	  (if (char-ready? port)
	      (let ((c (read-char port)))
		(if (eof-object? c)
		    (begin (clean-up!)
                           '())
                    (cons c (read-chars))))
	      '()))
	(if cleaned-up
	    ""
	    (list->string (read-chars))))
      (define (display-cmd cmdstring)
	(if display-commands
	    (begin
	      (display "xdraw sent: ")
	      (display cmdstring))
	    'ok))
      (define (display-data data)
	(if display-commands
	    (begin
	      (display "xdraw got: ")
	      (display data)
	      (newline))
	    'ok))
      (define (get-stderr)
	(read-all stderr-port))
      (define (report-error from errmsg)
	(if (and error-handler (> (string-length errmsg) 0))
	    (error-handler (string-append "xdraw error from "
					  from
					  errmsg
					  (string #\newline)))
	    'ignored))
      (define (check-cleaned-up cont-thunk)
	(if cleaned-up
            (begin (if error-handler
                       (error-handler (string-append
                                       "xdraw error: xdraw has exited!"
                                       (string #\newline))))
                   '())
            (cont-thunk)))
      (define (handle-errors last-command)
	(if wait-for-errors
	    (begin
	      (sleep 0.2)		; wait a while for xdraw
	      (report-error last-command (get-stderr)))
	    'ok))
      (define (handle-previous-errors)
	(report-error (string-append "a previous command"
                                     (string #\newline))
                      (get-stderr)))
      (define (send-string str)
	(handle-previous-errors)
	(display-cmd str)
	(check-cleaned-up
	 (lambda ()
	   (display str stdin-port)
	   (flush-output stdin-port)
	   (handle-errors str)
	   'sent)))
      (define (buffered-send-string str)
        (command-buffer 'insert! str)
        (if (>= (command-buffer 'length) command-buffer-maxsize)
            (flush-command-buffer))
        'ok)
      (define (flush-command-buffer)
        (if (command-buffer 'empty?)
            'nothing-done
            (send-string (apply string-append (command-buffer 'flush!)))))
      (define (send-cmd . cmd)
	(cond ((null? cmd)
	       'done)
	      ((string? (car cmd))	; a list of preformatted strings
	       (buffered-send-string (car cmd))
               (apply send-cmd (cdr cmd)))
	      (else			; a list containing the command
	       (buffered-send-string (apply format-xdraw-command cmd)))))
      (define (process-stdout want-return wait-for-input)
	(define (warn msg value)
	  (if error-handler
	      (begin
		(display "xdraw warning: ")
		(display msg)
		(display value)
		(newline))
	      'ok))
	(define (skip-whitespace-char-ready? port)
	  (and (char-ready? port)
	       (if (memv (peek-char port) '(#\newline #\space))
		   (begin
		     (read-char port)
		     (skip-whitespace-char-ready? port))
		   #t)))
	(if (and (not cleaned-up)
		 (or wait-for-input
		     (skip-whitespace-char-ready? stdout-port)))
	    (let ((val (read stdout-port)))
	      (cond ((eof-object? val)
		     (clean-up!)
		     '())
		    ((and (pair? val) (eq? (car val) 'event))
		     (event-queue 'insert! val)
		     (process-stdout want-return (and wait-for-input
						      want-return)))
		    ((and (pair? val) (eq? (car val) 'return))
		     (if want-return
			 val
			 (begin
			   (warn "ignored return value: " val)
			   (process-stdout want-return wait-for-input))))
		    (else
		     (warn "got an invalid value: " val)
		     (process-stdout want-return wait-for-input))))
	    '()))
      (define (send-with-result . cmd)
	(check-cleaned-up
	 (lambda ()
           (flush-command-buffer)
	   (process-stdout #f #f)	; ignore old stdout, if any
	   (apply send-cmd cmd)
           (flush-command-buffer)
	   (process-stdout #t #t))))	; return new stdout (parsed)
      (define (get-event)
	(check-cleaned-up
	 (lambda ()
	   (process-stdout #f #f)
	   (if (event-queue 'empty?)
	       '()
	       (event-queue 'remove!)))))
      (define (peek-event)
        (check-cleaned-up
         (lambda ()
           (process-stdout #f #f)
           (if (event-queue 'empty?)
               '()
	       (event-queue 'peek)))))
      (define (wait-for-event)
        (define (loop)
          (if cleaned-up
              '()
              (if (event-queue 'empty?)
                  (begin
                    (flush-command-buffer)
                    (process-stdout #f #t)
                    (loop))
                  (event-queue 'remove!))))
        (check-cleaned-up loop))
      (define (error-handling new)
	(cond ((eq? new 'display)
	       (set! error-handler display))
	      ((eq? new 'error)
	       (set! error-handler error))
	      ((not new)
	       (set! error-handler #f))
	      (else
	       (error "XDRAW: Invalid argument to error-handling:" new))))
      (define (clean-up!)
	(close-output-port stdin-port)
	(close-input-port stdout-port)
	(close-input-port stderr-port)
	(set! cleaned-up #t)
	'done)
      (define (exit-xdraw)
	(if cleaned-up
	    'already-cleaned-up
	    (begin (set! wait-for-errors #f) ; don't want this now...
                   (if (alive?)
                       (send-cmd (format-xdraw-command 'Exit)))
                   (clean-up!))))
      (define (alive?)
	(and (not cleaned-up)
	     (eq? (subprocess-status proc-handle) 'running)))
      (define (cleanup-if-dead)
	(if (or cleaned-up (alive?))
	    'ok
	    (clean-up!)))
      (let ((dispatch-table		; (symbol procedure)
	     `((send ,send-cmd)
	       (send-with-result ,send-with-result)
	       (get-event ,get-event)
	       (peek-event ,peek-event)
	       (wait-for-event ,wait-for-event)
               (flush-command-buffer ,flush-command-buffer)
               (whatami ,(lambda ()
			   'xdraw-object))
	       (alive? ,alive?)
	       (display-commands ,(lambda (new)
				    (set! display-commands new)))
	       (error-handling ,error-handling)
	       (wait-for-errors ,(lambda (new)
				   (set! wait-for-errors new)))
               (buffer-commands ,(lambda (newlen)
                                   (flush-command-buffer)
                                   (set! command-buffer-maxsize newlen)))
	       (exit ,exit-xdraw)
	       (cleanup-if-dead ,cleanup-if-dead))))
	(define (real-dispatch m . args) ; dispatch based on table
	  (let ((table-entry (assq m dispatch-table)))
	    (if (not table-entry)
		(error "XDRAW: object got an invalid message" m))
	    (apply (cadr table-entry) args)))
	(define (serialize thunk)
	  (let ((result #f))
	    (semaphore-wait synchronizer)
	    (with-handlers
	     ((exn? (lambda (e)
		      (semaphore-post synchronizer)
		      (raise e))))
	     (set! result (thunk)))
	    (semaphore-post synchronizer)
	    result))
	(define (xdraw-dispatch . args)
	  (serialize (lambda () (apply real-dispatch args))))
	xdraw-dispatch))))

;;; Simple interface

;; set! this to the location of the xdraw binary, if necessary. For
;; example, (set! *xdraw-path* "/usr/local/bin/xdraw").
;; (find-executable-path is an MzScheme primitive which looks for the
;; named command in the PATH.)
(define *xdraw-path* (find-executable-path "xdraw" "xdraw"))

;; An internal list of the objects created by new-xdraw
(define *xdraw-objects* '())

;; NEW-XDRAW: Starts a new copy of xdraw, cleaning up old, dead ones.
;; Returns the new xdraw object (inside a small wrapper object).
(define (new-xdraw . cmdline-args)
  (define (add-to-xdraw-objects! obj)
    (set! *xdraw-objects*
	  (cons obj *xdraw-objects*)))
  (define (remove-from-xdraw-objects! obj)
    (define (remove o l)
      (cond ((null? l) '())
	    ((eq? (car l) o) (remove o (cdr l)))
	    (else (cons (car l) (remove o (cdr l))))))
    (set! *xdraw-objects* (remove obj *xdraw-objects*)))
  (cleanup-xdraws!)
  (let ((the-obj (apply start-xdraw (cons *xdraw-path* cmdline-args))))
    (define (new-xdraw-dispatch . args)
      (if (and (= (length args) 1) (eq? (car args) 'exit))
	  (begin
	    (remove-from-xdraw-objects! the-obj)
	    (the-obj 'exit))
	  (apply the-obj args)))
    (add-to-xdraw-objects! the-obj)
    new-xdraw-dispatch))

;; CLEANUP-XDRAWS!: Cleans up dead xdraw processes that were started
;; with new-xdraw. This is called automatically whenever new-xdraw is
;; called.
(define (cleanup-xdraws!)
  (define (clean left)			; keep only live objects
    (if (null? left)
	'()
	(let ((this-obj (car left))
	      (rest (cdr left)))
	  (this-obj 'cleanup-if-dead)
	  (if (this-obj 'alive?)
	      (cons this-obj (clean rest))
	      (clean rest)))))
  (set! *xdraw-objects* (clean *xdraw-objects*))
  'done)

;; EXIT-ALL-XDRAWS!: Exits all xdraw processes started with new-xdraw
(define (exit-all-xdraws!)
  (for-each (lambda (obj) (obj 'exit)) *xdraw-objects*)
  (set! *xdraw-objects* '())
  'done)
