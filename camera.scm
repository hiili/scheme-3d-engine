;;; camera.scm


;;; Kamera -olio.
(define (new-camera followed-player vis-interface ticker)
  
  ; Apufunktioita
  (define (h-phase-0.5-altitude)
    (- (* 2
          h-transition-view-distance)
       1))
  (define (h-phase-1.0-altitude maze-width maze-height)
    (let ((max-distance (max (if (odd? maze-width)
                                 maze-width
                                 (+ maze-width 1))
                             (if (odd? maze-height)
                                 maze-height
                                 (+ maze-height 1)))))
      (+ (* zoom-factor
            max-distance)
         1)))
  (define (calculate-maze-center-loc maze-width maze-height)
    (vector (/ (if (odd? maze-width)
                   maze-width
                   (+ maze-width 1))
               2)
            (/ (if (odd? maze-height)
                   maze-height
                   (+ maze-height 1))
               2)
            0))
  
  ;;; h-phase -olio. Kuvaa 2d<->3d -moodien v‰lisen siirtym‰n vaihetta.
  ;;; 0.0 = 3d, 0.5 = bezier-liukuman ja pystysuoran liukuman liitoskohta, 1.0 = 2d
  ;;;   Olion arvo on v‰lill‰ 0.0 - 1.0. V‰lill‰ 0.0 - 0.5 (bezier phase) muutosnopeus vaihtelee
  ;;;   tilanteen mukaan, mutta v‰lill‰ 0.5 - 1.0 (linear phase) muutosnopeus on vakio.
  (define (new-h-phase ticker player maze-width maze-height)
    (define (bezier-phase-steps player maze-width maze-height)
      (/ (vector-geom-length (sub (add (calculate-maze-center-loc maze-width maze-height)
                                       (vector 0 0 (h-phase-0.5-altitude)))
                                  (((player 'locator) 'get-loc))))
         2d<->3d-transition-average-step-length))
    (define (linear-phase-steps maze-width maze-height)
      (/ (abs (- (h-phase-1.0-altitude maze-width maze-height)
                 (h-phase-0.5-altitude)))
         2d<->3d-transition-average-step-length))
    (let ((bezier-phase-slider (new-smooth-changing-value ticker
                                                          (* 2 (bezier-phase-steps player
                                                                                   maze-width
                                                                                   maze-height))
                                                          (lambda (t) t)
                                                          (cadr (assq initial-viewmode
                                                                      '((3d 0)
                                                                        (2d 0.5))))))
          (linear-phase-slider (new-smooth-changing-value ticker
                                                          (* 2 (linear-phase-steps maze-width
                                                                                   maze-height))
                                                          (lambda (t)
                                                            (+ (/ (dynamic-bezier (* 2 (- t 0.5))
                                                                                  0 0.33 1 1)
                                                                  2)
                                                               0.5))
                                                          (cadr (assq initial-viewmode
                                                                      '((3d 0.5)
                                                                        (2d 1))))))
          (active-phase (cadr (assq initial-viewmode
                                    '((3d bezier)
                                      (2d linear)))))
          (target-mode initial-viewmode))
      
      ;;; Metodit
      ; Tarkistaa ollaanko bezier- ja linear -vaiheiden vaihdoskohdassa.
      (define (tick!)
        ; sliderit ovat jo saaneet t‰m‰n tikityksen.. (ne alustettiin ensin)
        (cond
         ; Ollaan siirtym‰ss‰ bezier->linear?
         ((and (eq? target-mode '2d)
               (eq? active-phase 'bezier)
               (= ((bezier-phase-slider 'get-value)) 0.5))
          (set! active-phase 'linear)
          ; bezier-slideri pys‰htyy 0.5:een, k‰ynnistet‰‰n linear -slideri
          ((linear-phase-slider 'set-target-value!) 1))
         ; Ollaan siirtym‰ss‰ linear->bezier?
         ((and (eq? target-mode '3d)
               (eq? active-phase 'linear)
               (= ((linear-phase-slider 'get-mapped-value)) 0.5))
          (set! active-phase 'bezier)
          ((bezier-phase-slider 'set-target-value!) 0))))
      (define (manual-tick!)
        ((bezier-phase-slider 'tick!))
        ((linear-phase-slider 'tick!))
        (tick!))
      (define (set-state! new-h new-target-mode)
        (set! target-mode new-target-mode)
        ((bezier-phase-slider 'set-value!) (min new-h 0.5))
        ((bezier-phase-slider 'set-target-value!) (cadr (assq target-mode
                                                              '((3d 0)
                                                                (2d 0.5)))))
        ((linear-phase-slider 'set-value!) (max new-h 0.5))
        ((linear-phase-slider 'set-target-value!) (cadr (assq target-mode
                                                              '((3d 0.5)
                                                                (2d 1)))))
        (set! active-phase (if (< new-h 0.5)
                               'bezier
                               'linear)))
      (define (get-value)
        (if (eq? active-phase 'bezier)
            ((bezier-phase-slider 'get-mapped-value))
            ((linear-phase-slider 'get-mapped-value))))
      (define (get-linear-phase-steps)
        (linear-phase-steps maze-width maze-height))
      (define (switch-viewmode!)
        (set! target-mode
              (if (eq? target-mode '3d)
                  '2d
                  '3d))
        (if (eq? active-phase 'bezier)
            ((bezier-phase-slider 'set-target-value!) (if (eq? target-mode '3d)
                                                          0
                                                          0.5))
            ((linear-phase-slider 'set-target-value!) (if (eq? target-mode '3d)
                                                          0.5
                                                          1)))
        ((bezier-phase-slider 'set-0-to-1-steps!) (* 2 (bezier-phase-steps player
                                                                           maze-width
                                                                           maze-height))))
      (define (set-target-viewmode! new-viewmode)
        (if (not (eq? target-mode new-viewmode))
            (switch-viewmode!)))
      (define (get-state-snapshot)
        (vector ((bezier-phase-slider 'get-value))
                ((bezier-phase-slider 'get-0-to-1-steps))
                ((linear-phase-slider 'get-value))
                active-phase
                target-mode))
      (define (revert-state-to-snapshot! snapshot)
        ((bezier-phase-slider 'set-value!) (vector-ref snapshot 0))
        ((bezier-phase-slider 'set-0-to-1-steps!) (vector-ref snapshot 1))
        ((linear-phase-slider 'set-value!) (vector-ref snapshot 2))
        (set! active-phase (vector-ref snapshot 3))
        (set! target-mode (vector-ref snapshot 4))
        ((bezier-phase-slider 'set-target-value!) (if (eq? target-mode '3d)
                                                      0
                                                      0.5))
        ((linear-phase-slider 'set-target-value!) (if (eq? target-mode '3d)
                                                      0.5
                                                      1)))
      
      ;;; Dispatch
      (define (dispatch method)
        (cond ((eq? method 'tick!) tick!)
              ((eq? method 'manual-tick!) manual-tick!)
              ((eq? method 'set-state!) set-state!)
              ((eq? method 'get-value) get-value)
              ((eq? method 'get-linear-phase-steps) get-linear-phase-steps)
              ((eq? method 'switch-viewmode!) switch-viewmode!)
              ((eq? method 'set-target-viewmode!) set-target-viewmode!)
              ((eq? method 'get-state-snapshot) get-state-snapshot)
              ((eq? method 'revert-state-to-snapshot!) revert-state-to-snapshot!)
              (else (error "Unknown method! camera.h-phase." method))))
      ((ticker 'request-ticking!) dispatch)
      dispatch))
  
  
  ;;; new-camera -runko:
  (let ((h-phase (new-h-phase ticker
                              followed-player
                              (car (vis-interface 'size))
                              (cadr (vis-interface 'size))))
        (window-size-x.y (cons 0 0)) ; x-ikkunan koko
        (color-cycle-phase 0)        ; v‰rikierron vaihe
        (3d-vectors false))          ; n‰kym‰n eteen, oikealle ja ylˆs -suuntavektorit
    
    
    ;;; ==================================================
    ;;;    Apufunktiot                                    
    ;;; ==================================================
    ; sijainti, eteen, oikealle ja ylˆs -vektoreiden laskenta h-phase huomioiden.
    ; h-phase:
    ;   0.0 - 0.5   Kamera liukuu pelaajan sijainnista labyrintin keskelle
    ;               katsomaan suoraan alas. Korkeus siten, ett‰ labyrintti‰ n‰kyy
    ;               h-transition-view-distancen verran joka suuntaan keskipisteest‰.
    ;               (= h-phase-0.5-altitude)
    ;               Katseen suunta:
    ;                 Pelaajan kulkusuunta(h-phase == 0.0) - pohjoiseen&alas(h-phase == 0.5)
    ;   0.5 - 1.0   Kamera liukuu em. pisteest‰ suoraan ylˆs, kunnes koko labyrintti n‰kyy.
    ;               (= h-phase-1.0-altitude)
    ;               Katseen suunta pohjoiseen&alas (eli katse pohjoiseen ja sitten nokka alas)
    (define (make-3d-vectors)
      ; h-phase=0.0 - h-phase=0.5   -siirtym‰:
      ;   Katseen suunta:
      ;     Siirtym‰n kuvaajak‰yr‰n derivaatta on alussa ja lopussa 0
      ;     alku- ja loppunyk‰ysten v‰ltt‰miseksi:
      ;       f(0) = 0, f(1) = 1, f'(0)=f'(1)=0
      ;        -> f: -2t^3 + 3t^2
      (define (view-dir-transition-curve-func t)
        (+ (* -2
              (expt t 3))
           (* 3
              (expt t 2))))
      ;   Down-pitch:
      ;     Muuttuu jyrk‰sti alussa -> kamera k‰‰ntyy nopeasti katsomaan alas
      (define (down-pitch-transition-curve-func t)
        (view-dir-transition-curve-func (expt t 0.5)))
      ; h-phase == 0.0   -> down-pitch = 0
      ; h-phase >= 0.5   -> down-pitch = pi/2
      (define (generate-down-pitch-from-h-phase h-phase)
        (if (< h-phase 0.5)
            (* (down-pitch-transition-curve-func (* 2 h-phase))
               pi/2)
            pi/2))
      
      (let ((loc-vec (make-vector 3))
            (ahead-vec (make-vector 3))
            (right-vec (make-vector 3))
            (up-vec (make-vector 3))
            (down-pitch (generate-down-pitch-from-h-phase ((h-phase 'get-value))))
            (dir ((followed-player 'get-dir))))
        
        ; 3d-vectors -laskenta --------------------------------------
        (define (calculate)
          ;;; ==============================================================
          ;;;    Apufunktiot
          ;;; ==============================================================
          ; h-phase == 0.0   -> result = dir
          ; h-phase >= 0.5   -> result = pi/2
          (define (generate-dir-with-h-turn dir h-phase)
            (let ((t (if (< h-phase 0.5)
                         (* 2 h-phase)
                         1)))
              (mod (+ (* (- 1 (view-dir-transition-curve-func t))
                         (if (> dir (* 1.5 pi))
                             (- dir 2pi)
                             dir))
                      (* (view-dir-transition-curve-func t)
                         pi/2))
                   2pi)))
          ; loc-vec:
          ;   h-phase = 0.0 - 0.5:
          ;     m‰‰ritell‰‰n bezier-k‰yr‰ll‰ seuraavien tukipisteiden avulla:
          ;       x0: (player-loc)
          ;       x1: (player-loc      + < 0  0  (0.5 * h-phase-0.5-altitude) > )
          ;       x2: (maze-center-loc + < 0  0  (0.5 * h-phase-0.5-altitude) > )
          ;       x3: (maze-center-loc + < 0  0  h-phase-0.5-altitude > )
          ;     Sijainti x0 -p‰‰ss‰, kun h-phase == 0
          ;              x3 -p‰‰ss‰, kun h-phase == 0.5 
          ;   h-phase = 0.5 - 1.0:
          ;     interpoloidaan v‰lilt‰:
          ;       x0: (maze-center-loc + < 0  0  h-phase-0.5-altitude > )
          ;       x1: (maze-center-loc + < 0  0  h-phase-1.0-altitude > )
          (define (calculate-loc-vec player-loc maze-center-loc maze-width maze-height h-phase)
            (define (calculate-with-bezier t)
              (let ((x0 player-loc)
                    (x1 (add player-loc
                             (vector 0 0 (* 0.5 (h-phase-0.5-altitude)))))
                    (x2 (add maze-center-loc
                             (vector 0 0 (* 0.5 (h-phase-0.5-altitude)))))
                    (x3 (add maze-center-loc
                             (vector 0 0 (h-phase-0.5-altitude)))))
                (dynamic-bezier t x0 x1 x2 x3)))
            (define (calculate-with-interpolation t)
              (let ((x0 (add maze-center-loc
                             (vector 0 0 (h-phase-0.5-altitude))))
                    (x1 (add maze-center-loc
                             (vector 0 0 (h-phase-1.0-altitude maze-width maze-height)))))
                (add (mul (- 1 t)
                          x0)
                     (mul t
                          x1))))
            (if (< h-phase 0.5)
                (calculate-with-bezier (* 2 h-phase))
                (calculate-with-interpolation (* 2
                                                 (- h-phase 0.5)))))
            
          ;;; ==============================================================
          ;;;    calculate -runko
          ;;; ==============================================================
          (let ((dir-with-h-turn (generate-dir-with-h-turn dir
                                                           (get-h-phase))))
            ; loc-vec
            (let ((maze-width (car (vis-interface 'size)))
                  (maze-height (cadr (vis-interface 'size))))
              (let ((player-loc (((followed-player 'locator) 'get-loc)))
                    (maze-center-loc (calculate-maze-center-loc maze-width maze-height)))
                (set! loc-vec (calculate-loc-vec player-loc
                                                 maze-center-loc
                                                 maze-width
                                                 maze-height
                                                 (get-h-phase)))))
            ;;; ahead, right ja up -vektorit
            ; ahead-vec
            (vector-set! ahead-vec 0 (* (cos down-pitch)
                                        (cos dir-with-h-turn)))   ; x
            (vector-set! ahead-vec 1 (* (cos down-pitch)
                                        (sin dir-with-h-turn)))   ; y
            (vector-set! ahead-vec 2 (- (sin down-pitch)))   ; z
            ; right-vec
            (vector-set! right-vec 0 (sin dir-with-h-turn))   ; x
            (vector-set! right-vec 1 (- (cos dir-with-h-turn)))   ; y
            (vector-set! right-vec 2 0)   ; z
            ; up-vec
            (vector-set! up-vec 0 (* (sin down-pitch)
                                     (cos dir-with-h-turn)))   ; x
            (vector-set! up-vec 1 (* (sin down-pitch)
                                     (sin dir-with-h-turn)))
            (vector-set! up-vec 2 (cos down-pitch))))
        
        ; 3d-vectors -dispatch ----------------------------------------
        (define (dispatch request)
          (cond ((eq? request 'loc-vec) loc-vec)
                ((eq? request 'ahead-vec) ahead-vec)
                ((eq? request 'right-vec) right-vec)
                ((eq? request 'up-vec) up-vec)
                ((eq? request 'down-pitch) down-pitch)
                (else (error "Unknown vector request! camera.3d-vectors." request))))
        (calculate)
        ;(display (list 'cam: 'ahead: ahead-vec)) (newline)
        dispatch))
    
    
    
    
    
    ;;; ==================================================
    ;;;    Metodit                                        
    ;;; ==================================================
    (define (follow-player! new-player) (set! followed-player new-player))
    (define (tick!)
;      (cond ((= h-dir 1)
;             ; Kuvakulma vaihtumassa 3d -> 2d
;             (begin
;               (set! h-phase (+ h-phase h-phase-step))
;               (if (> h-phase 1)
;                   (begin
;                     (set! h-dir 0)
;                     (set! h-phase 1)))))
;            ; Kuvakulma vaihtumassa 2d -> 3d
;            ((= h-dir -1)
;             (begin
;               (set! h-phase (- h-phase h-phase-step))
;               (if (< h-phase 0)
;                   (begin
;                     (set! h-dir 0)
;                     (set! h-phase 0))))))
      ; Kasvatetaan color-cycle-phasea yhdell‰
      (begin
        (set! color-cycle-phase (+ color-cycle-phase 1))
        (if (= color-cycle-phase color-cycle-steps)
            (set! color-cycle-phase 0)))
      ; Lasketaan 3d-vektorit uudelleen
      (set! 3d-vectors (make-3d-vectors)))
    (define (manual-tick!)
      ((h-phase 'manual-tick!))
      (tick!))
    (define (switch-viewmode!)
      ((h-phase 'switch-viewmode!)))
;      (if (= h-dir 0)
;          (set! h-dir (if (< h-phase 0.5)
;                          1
;                          -1))
;          (set! h-dir (- h-dir))))
    (define (set-window-size-x.y! new-size)
      (set! window-size-x.y new-size)
      (set! 3d-vectors false))
    (define (set-color-cycle-phase! new-phase) (set! color-cycle-phase new-phase))
    (define (set-h-phase-state! . args)
      (apply (h-phase 'set-state!) args)
      (set! 3d-vectors false))
    (define (get-h-phase) ((h-phase 'get-value)))
    (define (above-walls?) (> (z-component (3d-vectors-handler 'loc-vec)) 1.0))
    (define (get-window-size-x.y) window-size-x.y)
    (define (get-color-cycle-phase) color-cycle-phase)
    (define (3d-vectors-handler vec-name)
      (if (not 3d-vectors)
          (set! 3d-vectors (make-3d-vectors)))
      (3d-vectors vec-name))
    (define (get-loc-as-cell-index-x.y) (loc-vector->cell-index-x.y (3d-vectors-handler 'loc-vec)))
    (define (get-loc-as-x.y-pair) (loc-vector->x.y-pair (3d-vectors-handler 'loc-vec)))
    (define (get-state-snapshot)
      (vector ((h-phase 'get-state-snapshot))
              color-cycle-phase))
    (define (revert-state-to-snapshot! snapshot)
      ((h-phase 'revert-state-to-snapshot!) (vector-ref snapshot 0))
      (set! color-cycle-phase (vector-ref snapshot 1))
      (set! 3d-vectors false))
    
    
    ;;; ==================================================
    ;;;    Dispatch                                       
    ;;; ==================================================
    (define (dispatch method)
      (cond ((eq? method 'tick!) tick!)
            ((eq? method 'manual-tick!) manual-tick!)
            ((eq? method 'follow-player!) follow-player!)
            ((eq? method 'switch-viewmode!) switch-viewmode!)
            ((eq? method 'set-h-phase-state!) set-h-phase-state!)
            ((eq? method 'get-state-snapshot) get-state-snapshot)
            ((eq? method 'revert-state-to-snapshot!) revert-state-to-snapshot!)
            ((eq? method 'set-window-size-x.y!) set-window-size-x.y!)
            ((eq? method 'set-color-cycle-phase!) set-color-cycle-phase!)
            ((eq? method 'get-h-phase) get-h-phase)
            ((eq? method 'get-linear-phase-steps) (h-phase 'get-linear-phase-steps))
            ((eq? method 'above-walls?) above-walls?)
            ((eq? method 'get-window-size-x.y) get-window-size-x.y)
            ((eq? method '3d-vectors) 3d-vectors-handler)
            ((eq? method 'get-loc-as-cell-index-x.y) get-loc-as-cell-index-x.y)
            ((eq? method 'get-loc-as-x.y-pair) get-loc-as-x.y-pair)
            ((eq? method 'get-color-cycle-phase) get-color-cycle-phase)
            (else (error "Unknown method! camera." method))))
    
    ((ticker 'request-ticking!) dispatch)
    dispatch))
