;;; player.scm


;;; Pelaaja-olio.
;;;   Olio sis‰lt‰‰:
;;;     -sijainnin labyrintissa locator-olion muodossa
;;;     -katsesuunnan
;;;     -k‰skyt liikkumiseen
;;;      (Seinien l‰pi kulkeminen estetty t‰‰ll‰)
(define (new-player locator maze-interface ticker)
  (let ((dir 0)                      ; k‰velysuunta
        (vel-turning 0)              ; k‰‰ntymisnopeus
        (player-light-magnitude (new-smooth-changing-value ticker
                                                           player-light-switching-steps
                                                           (lambda (t)
                                                             (expt t player-light-changing-speed-curve-exp))
                                                           0))
        (cell-visit-counters (generate-vector (cadr (maze-interface 'size))
                                              (lambda ()
                                                (make-vector (car (maze-interface 'size))
                                                             0))))
        (cell-visit-marker-lights (generate-vector (cadr (maze-interface 'size))
                                                   (lambda ()
                                                     (make-vector (car (maze-interface 'size))
                                                                  false)))))
    
    ;;; ==================================================
    ;;;    Apufunktiot                                    
    ;;; ==================================================
    ; T‰m‰ funktio k‰sittelee locatorista tulevia cell-visit -viestej‰.
    (define (cell-visit-message-handler visited-cell-x.y)
      ; Lis‰‰ ensivierailun ilmaisevan merkkivalon
      (define (add-first-visit-light cell-x.y)
        (vector-set! (vector-ref cell-visit-marker-lights
                                 (cdr cell-x.y))
                     (car cell-x.y)
                     (((maze-interface 'dynamic-lights-manager) 'new-dynamic-light!)
                      (new-locator maze-interface
                                   ticker
                                   (cell-index-x.y->loc-vector cell-x.y
                                                               visit-marker-lights-initial-altitude)
                                   (vector 0 0 0)
                                   visit-marker-light-kinetic-attributes)
                      ticker
                      visit-marker-lights-affection-distance
                      visit-marker-lights-switching-steps
                      first-visit-marker-light-color
                      false
                      true)))
      (define (add-second-visit-light cell-x.y)
        (let ((first-visit-light (vector-ref (vector-ref cell-visit-marker-lights
                                                         (cdr cell-x.y))
                                             (car cell-x.y))))
          ((first-visit-light 'die!))
          (((maze-interface 'dynamic-lights-manager) 'new-dynamic-light!)
           (new-locator maze-interface
                        ticker
                        (cell-index-x.y->loc-vector cell-x.y
                                                    visit-marker-lights-initial-altitude)
                        (vector 0 0 0)
                        visit-marker-light-kinetic-attributes)
           ticker
           visit-marker-lights-affection-distance
           visit-marker-lights-switching-steps
           second-visit-marker-light-color
           false
           true)))
      ; cell-visit-message-handler -runko:
      ; Exitiss‰?
      (if (equal? visited-cell-x.y
                  (maze-interface 'exit-loc))
          (set! solved? true))
      ; Onko huoneessa k‰yty aikaisemmin 0, 1 vai useamman kerran? Merkit‰‰n muutos valaistuksella.
      (case (vector-ref (vector-ref cell-visit-counters
                                    (cdr visited-cell-x.y))
                        (car visited-cell-x.y))
        ((0) (add-first-visit-light visited-cell-x.y))
        ((1) (add-second-visit-light visited-cell-x.y)))
      ; Kasvatetaan laskuria yhdell‰
      (vector-set! (vector-ref cell-visit-counters
                               (cdr visited-cell-x.y))
                   (car visited-cell-x.y)
                   (+ (vector-ref (vector-ref cell-visit-counters
                                    (cdr visited-cell-x.y))
                        (car visited-cell-x.y))
                      1)))
    
    
    ;;; ==================================================
    ;;;    Metodit                                        
    ;;; ==================================================
    (define (set-dir! new-dir) (set! dir (mod new-dir 2pi)))
    (define (get-dir) dir)
    
    ; Liikutaan normaalisti, about 3d-r‰tkytysten tyyliin
    ;   (toteutettu k‰skytt‰m‰ll‰ sis‰ist‰ locator-oliota
    (define (move-normal cmd)
      (cond
       ; Pys‰ytysk‰sky?
       ((eq? cmd 'stop-left-right) ((locator 'stop-in-2d-dir!) (mod (+ dir pi/2) 2pi)))
       ((eq? cmd 'stop-forward-backward) ((locator 'stop-in-2d-dir!) dir))
       ((eq? cmd 'stop-turning) (set! vel-turning 0))
       ; Vasemmalle/oikealle?
       ((eq? cmd 'step-left) ((locator 'accelerate-in-2d-dir!) (mod (+ dir pi/2) 2pi)
                                                               move-acceleration-delta))
       ((eq? cmd 'step-right) ((locator 'accelerate-in-2d-dir!) (mod (- dir pi/2) 2pi)
                                                                move-acceleration-delta))
       ; Eteen/taakse?
       ((eq? cmd 'step-forward) ((locator 'accelerate-in-2d-dir!) dir
                                                                  move-acceleration-delta))
       ((eq? cmd 'step-backward) ((locator 'accelerate-in-2d-dir!) (mod (+ dir pi) 2pi)
                                                                   move-acceleration-delta))
       ; K‰‰nnˆs?
       ((eq? cmd 'turn-left) (set! vel-turning (if (negative? vel-turning)
                                                   turn-acceleration-delta
                                                   (+ vel-turning
                                                      turn-acceleration-delta))))
       ((eq? cmd 'turn-right) (set! vel-turning (if (positive? vel-turning)
                                                   (- turn-acceleration-delta)
                                                   (- vel-turning
                                                      turn-acceleration-delta))))))
    
    ; Poukkoilumoodi
    (define (move-true-inertia cmd)
      (move-normal cmd))
    
    (define (tick!)
      ; k‰‰ntyminen
      (set-dir! (+ dir vel-turning))
      ; jarrutetaan hieman
      (set! vel-turning (* move-deceleration-factor
                           vel-turning)))

    (define (switch-player-light!)
      ((player-light-magnitude 'set-target-value!) (- 1 ((player-light-magnitude 'get-target-value)))))
    (define (player-light-off!)
      ((player-light-magnitude 'set-target-value!) 0))
    (define (get-player-light-magnitude)
      ((player-light-magnitude 'get-mapped-value)))
    (define (throw-torch! light-color)
      (((maze-interface 'dynamic-lights-manager) 'new-dynamic-light!)
       (new-locator maze-interface
                    ticker
                    ((locator 'get-loc))   ; location
                    (add ((locator 'get-vel))
                         (vector (* (cos (get-dir))
                                    torch-throw-speed)
                                 (* (sin (get-dir))
                                    torch-throw-speed)
                                 torch-vertical-velocity))   ; velocity
                    torch-kinetic-attributes)
       ticker
       torch-light-affection-distance
       torch-light-switching-steps
       light-color
       true
       false))
    
    ;;; ==================================================
    ;;;    Dispatch                                       
    ;;; ==================================================
    (define (dispatch method)
      (cond ((eq? method 'tick!) tick!)
            ((eq? method 'locator) locator)
            ((eq? method 'set-dir!) set-dir!)
            ((eq? method 'get-dir) get-dir)
            ((eq? method 'switch-player-light!) switch-player-light!)
            ((eq? method 'player-light-off!) player-light-off!)
            ((eq? method 'get-player-light-magnitude) get-player-light-magnitude)
            ((eq? method 'throw-torch!) throw-torch!)
            ((eq? method 'move) (if true-inertia?
                                    move-true-inertia
                                    move-normal))
            (else (error "Unknown method! camera." method))))
    
    ((ticker 'request-ticking!) dispatch)
    ((locator 'request-cell-visit-messages) cell-visit-message-handler)
    ((player-light-magnitude 'set-target-value!) 1)   ; Sytytet‰‰n pelaajavalo
    dispatch))
