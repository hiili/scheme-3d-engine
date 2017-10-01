;;; ticker.scm


;;; Propagoi tick! -signaalia kaikille sit� pyyt�neille olioille pyynt�j�rjestyksess�.
(define (new-ticker)
  (let ((target-objects '()))
    
    ; Metodit
    (define (tick!)
      (for-each (lambda (object)
                  ((object 'tick!)))
                target-objects))
    (define (request-ticking! object)
      (set! target-objects
            (append target-objects
                    (list object))))
    (define (stop-ticking-me! object)
      (set! target-objects
            (remove-from-list target-objects object)))
    
    ; Dispatch
    (define (dispatch method)
      (cond ((eq? method 'tick!) tick!)
            ((eq? method 'request-ticking!) request-ticking!)
            ((eq? method 'stop-ticking-me!) stop-ticking-me!)
            (else (error "Unknown method! ticker." method))))
    dispatch))
