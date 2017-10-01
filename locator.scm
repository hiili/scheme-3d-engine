;;; locator.scm


(define (make-kinetic-attributes deceleration-factor
                                 deceleration-delta
                                 bounce-deceleration-factor
                                 bounce-deceleration-delta
                                 gravity-acceleration-delta
                                 max-acceleration-delta)
  (vector deceleration-factor
          deceleration-delta
          bounce-deceleration-factor
          bounce-deceleration-delta
          gravity-acceleration-delta
          max-acceleration-delta))
(define (kin-attr-get-deceleration-factor ka)
  (vector-ref ka 0))
(define (kin-attr-get-deceleration-delta ka)
  (vector-ref ka 1))
(define (kin-attr-get-bounce-deceleration-factor ka)
  (vector-ref ka 2))
(define (kin-attr-get-bounce-deceleration-delta ka)
  (vector-ref ka 3))
(define (kin-attr-get-gravity-acceleration-delta ka)
  (vector-ref ka 4))
(define (kin-attr-get-max-acceleration-delta ka)
  (vector-ref ka 5))


;;; Määrittelee konkreettisen olinpaikan labyrintissä.
;;; Voidaan määritellä myös kineettiseksi olioksi.
;;; Sisältää:
;;;   -sijaintikoordinaatit
;;;   -liikenopeuden
;;;   -kinetiikka-attribuutit
(define (new-locator vis-interface ticker loc-vec vel-vec kinetic-attributes)
  (let ((cell-visit-messages-target false))
    
    ;;; =======================================================
    ;;;    Apufunktiot
    ;;; =======================================================
    (define (2d-dir->3d-unit-vec dir)
      (vector (cos dir)
              (sin dir)
              0))
    (define (get-loc-as-cell-index-x)
      (inexact->exact (floor (x-component loc-vec))))
    (define (get-loc-as-cell-index-y)
      (inexact->exact (floor (y-component loc-vec))))
    (define (get-loc-inside-cell-x)
      (- (x-component loc-vec)
         (get-loc-as-cell-index-x)))
    (define (get-loc-inside-cell-y)
      (- (y-component loc-vec)
         (get-loc-as-cell-index-y)))
    
    
    ; Yrittää siirtyä uusiin koordinaatteihin loc-vec -muuttujaa muuttaen ja seinät huomioiden.
    ; Jos seinä estää siirtymisen, palautetaan tieto seinän
    ; suunnasta: 'horizontal/'vertical ja mennään suht lähelle seinää, 
    ; muuten palautetaan false.
    (define (try-to-move-to! dest-loc)
      
      ; Siirrytään (muutetaan loc-muuttujaa) annettuihin koordinaatteihin
      ; tai lähimpään seinien ulkopuolella olevaan kohtaan.
      (define (move-to-nearest-loc-not-inside-wall! dest-loc)
        ;(display (list 'move-to-nearest: dest-loc)) (newline)
        (let ((dest-loc-cell-index-x (inexact->exact (floor (x-component dest-loc))))
              (dest-loc-cell-index-y (inexact->exact (floor (y-component dest-loc))))
              (result false))
          (let ((dest-loc-inside-cell-x (- (x-component dest-loc) dest-loc-cell-index-x))
                (dest-loc-inside-cell-y (- (y-component dest-loc) dest-loc-cell-index-y)))
            ; Visit-messageja pyydetty? Lähetetään sellainen jos kohdehuone on eri kuin nykyinen.
            (if (and cell-visit-messages-target
                     (not (and (= (get-loc-as-cell-index-x)
                                  dest-loc-cell-index-x)
                               (= (get-loc-as-cell-index-y)
                                  dest-loc-cell-index-y))))
                (cell-visit-messages-target (cons dest-loc-cell-index-x
                                                  dest-loc-cell-index-y)))
            ; Länsiseinän sisällä?
            (if (and (< dest-loc-inside-cell-x wall-thickness/2)
                     (vis-interface 'wall
                                    (+ dest-loc-cell-index-x 1)
                                    (+ dest-loc-cell-index-y 1)
                                    'w))
                (begin
                  (set! result 'vertical)
                  (set-x-component! dest-loc (+ dest-loc-cell-index-x wall-thickness/2))))
            ; Itäseinän sisällä?
            (if (and (> dest-loc-inside-cell-x (- 1 wall-thickness/2))
                     (vis-interface 'wall
                                    (+ dest-loc-cell-index-x 1)
                                    (+ dest-loc-cell-index-y 1)
                                    'e))
                (begin
                  (set! result 'vertical)
                  (set-x-component! dest-loc (+ dest-loc-cell-index-x (- 1 wall-thickness/2)))))
            ; Eteläseinän sisällä?
            (if (and (< dest-loc-inside-cell-y wall-thickness/2)
                     (vis-interface 'wall
                                    (+ dest-loc-cell-index-x 1)
                                    (+ dest-loc-cell-index-y 1)
                                    's))
                (begin
                  (set! result (if (eq? result 'vertical)
                                   'both
                                   'horizontal))
                  (set-y-component! dest-loc (+ dest-loc-cell-index-y wall-thickness/2))))
            ; Pohjoisseinän sisällä?
            (if (and (> dest-loc-inside-cell-y (- 1 wall-thickness/2))
                     (vis-interface 'wall
                                    (+ dest-loc-cell-index-x 1)
                                    (+ dest-loc-cell-index-y 1)
                                    'n))
                (begin
                  (set! result (if (or (eq? result 'vertical)
                                       (eq? result 'both))
                                   'both
                                   'horizontal))
                  (set-y-component! dest-loc (+ dest-loc-cell-index-y (- 1 wall-thickness/2))))))
          ;(display (list 'move-to-nearest: 'final-dest-loc dest-loc)) (newline)
        (set! loc-vec dest-loc)
        result))
    
      
      ; Liikutaan 1 askel, delta-x & delta-y oltava < 1!!!
      (define (move-one-step! dest-loc delta-x delta-y)
        
        ; Yritetään rekursiivisesti edetä hieman lyhyempi matka
        (define (move-closer-to-wall! delta-x delta-y)
          (if (or (> (abs delta-x) wall-thickness/2)
                  (> (abs delta-y) wall-thickness/2))
              (begin
                ;(display (list 'move-closer 'delta-x: delta-x 'delta-y: delta-y)) (newline)
                (move-one-step! (add loc-vec
                                     (vector (* 0.66 delta-x) (* 0.66 delta-y) 0))
                                (* 0.66 delta-x)
                                (* 0.66 delta-y)))))
        
        ; Siirtymän alku- ja loppupisteet ovat samassa huoneessa
        (define (move-inside-cell! dest-loc)
          ; Pysytään vain irti seinistä
          ;(display (list 'move-inside-cell)) (newline)
          (move-to-nearest-loc-not-inside-wall! dest-loc))
        
        ; alku- ja loppupisteet päällekkäisissä/vierekkäisissä huoneissa
        (define (move-horiz/vert! dest-loc dest-cell-dir delta-x delta-y)
          ;(display (list 'move-horiz-vert: 'dest-loc: dest-loc 'delta-x: delta-x 'delta-y delta-y)) (newline)
          (if (vis-interface 'wall
                             (+ (get-loc-as-cell-index-x) 1)
                             (+ (get-loc-as-cell-index-y) 1)
                             dest-cell-dir)
              ; Seinä edessä, liikutaan lähemmäksi ja palautetaan seinän asento
              (begin
                ;(display 'wall-ahead..) (newline)
                (move-closer-to-wall! delta-x delta-y)
                ; Palautetaan seinän suunta
                (if (or (eq? dest-cell-dir 'n)
                        (eq? dest-cell-dir 's))
                    'horizontal
                    'vertical))
              ; Ei seinää, liikutaan&pysytään irti seinistä
              (begin
                (move-to-nearest-loc-not-inside-wall! dest-loc)
                false)))
        
        ; alku- ja loppupisteet kulmittaisissa huoneissa
        (define (move-diagonally! dest-loc delta-x delta-y)
          
          (define (y-index-changes-first? delta-x delta-y)
            (let ((distance-from-wall-x (if (positive? delta-x)
                                            (- 1
                                               (get-loc-inside-cell-x))
                                            (get-loc-inside-cell-x)))
                  (distance-from-wall-y (if (positive? delta-y)
                                            (- 1
                                               (get-loc-inside-cell-y))
                                            (get-loc-inside-cell-y))))
              (> (/ (abs delta-y)
                    (abs delta-x))
                 (/ distance-from-wall-y
                    distance-from-wall-x))))
          
          (let ((y-index-changes-first? (y-index-changes-first? delta-x delta-y)))
            ;(display (list 'move-diagonally 'dest-loc: dest-loc)) (newline)
            ; Onko ensimmäinen ylitettävä seinä olemassa?
            (if (vis-interface 'wall
                               (+ (get-loc-as-cell-index-x) 1)
                               (+ (get-loc-as-cell-index-y) 1)
                               (if y-index-changes-first?
                                   (if (negative? delta-y)
                                       's
                                     'n)
                                   (if (negative? delta-x)
                                       'w
                                       'e)))
                ; Eka ylitettävä seinä olemassa,
                ; mennään lähemmäksi ja palautetaan seinän asento
                (begin
                  (move-closer-to-wall! delta-x delta-y)
                  (if y-index-changes-first?
                      'horizontal
                      'vertical))
                ; Ekaa ylitettävää seinää ei ole,
                ; onko toinen ylitettävä seinä olemassa?
                (if (vis-interface 'wall
                                   (+ (get-loc-as-cell-index-x)
                                      1
                                      (if y-index-changes-first?
                                          0
                                          (if (positive? delta-x)
                                              1
                                              -1)))
                                   (+ (get-loc-as-cell-index-y)
                                      1
                                      (if y-index-changes-first?
                                          (if (positive? delta-y)
                                              1
                                              -1)
                                          0))
                                   (if y-index-changes-first?
                                       (if (negative? delta-x)
                                           'w
                                           'e)
                                       (if (negative? delta-y)
                                           's
                                           'n)))
                    ; Toka ylitettävä seinä on olemassa,
                    ; mennään lähemmäksi ja palutetaan seinän asento
                    (begin
                      (move-closer-to-wall! delta-x delta-y)
                      (if y-index-changes-first?
                          'vertical
                          'horizontal))
                    ; Kumpaakaan seinää ei ole, reitti selvä!
                    (begin
                      (move-to-nearest-loc-not-inside-wall! dest-loc)
                      false)))))
        
        
        ; Move-one-step -runko -------------------------------------------------
        (let ((cell-index-delta-x (inexact->exact (- (floor (x-component dest-loc))
                                                     (floor (x-component loc-vec)))))
              (cell-index-delta-y (inexact->exact (- (floor (y-component dest-loc))
                                                     (floor (y-component loc-vec))))))
          ;(display (list 'move-one-step: 'dest-loc: dest-loc 'delta-x: delta-x 'delta-y: delta-y)) (newline)
          (cond
           ; Kohdehuone edelleen sama?
           ((= cell-index-delta-x cell-index-delta-y 0)
            (move-inside-cell! dest-loc))
           ; Vinottainen siirtymä?
           ((not (or (= cell-index-delta-x 0)
                     (= cell-index-delta-y 0)))
            (move-diagonally! dest-loc delta-x delta-y))
           ; Vain vaaka/pystysuuntainen siirtymä?
           ((or (= cell-index-delta-x 0)
                (= cell-index-delta-y 0))
            (move-horiz/vert! dest-loc
                              (cond ((positive? cell-index-delta-x) 'e)
                                    ((positive? cell-index-delta-y) 'n)
                                    ((negative? cell-index-delta-x) 'w)
                                    (else 's))
                              delta-x
                              delta-y)))))
      
      ; Moneenko askeleeseen matka on jaettava?
      (define (needed-steps delta-x delta-y)
        (inexact->exact (ceiling (/ (max (abs delta-x)
                                         (abs delta-y))
                                    0.95))))
      
      ; Paloitellaan homma <steps> askeleeseen
      (define (move-in-n-steps! dest-loc delta-x delta-y steps)
        ;(display 'move-in-n-steps!) (newline)
        (let ((result (move-one-step! (add loc-vec
                                           (mul (/ 1 steps)
                                                (sub dest-loc loc-vec)))
                                      (/ delta-x steps)
                                      (/ delta-y steps))))
          ; Tuliko seinä vastaan?
          (if result
              result
              ; ei tullut, liikutaan loppuosa
              (try-to-move-to! (add loc-vec
                                    (mul (/ (- steps 1)
                                            steps)
                                         (sub dest-loc loc-vec)))))))
      
      ; try-to-move-to! -runko -------------------------------------------------------
      ;(display 'try-to-move-to!) (newline)
      ; Yritetäänkö mennä maan alle? Estetään.
      (if (< (z-component dest-loc) floor-altitude)
          (vector-set! dest-loc 2 floor-altitude))
      ; Onko liike paloiteltava osiin?
      ; (siirtymä oltava korkeintaan kulmittainen viereiseen huoneeseen!)
      (let ((delta-x (x-component (sub dest-loc
                                       loc-vec)))
            (delta-y (y-component (sub dest-loc
                                       loc-vec))))
        (let ((needed-steps (needed-steps delta-x delta-y)))
          (if (> needed-steps 1)
              (move-in-n-steps! dest-loc delta-x delta-y needed-steps)
              (move-one-step! dest-loc delta-x delta-y)))))
    
    
    
    (define (decelerate-with-delta velocity-vec delta)
      (let ((vel-vec-length (vector-geom-length velocity-vec)))
        (if (> vel-vec-length delta)
            (mul velocity-vec
                 (/ (- vel-vec-length
                       delta)
                    vel-vec-length))
            (vector 0 0 0))))
    
    
    ;;; =======================================================
    ;;;    Metodit
    ;;; =======================================================
    (define (stop-in-2d-dir! dir)
      (let* ((vel-magnitude-in-requested-dir (inner-product (2d-dir->3d-unit-vec dir)
                                                            vel-vec))
             (requested-dir-component (mul vel-magnitude-in-requested-dir
                                           (2d-dir->3d-unit-vec dir))))
        (set! vel-vec
              (sub vel-vec
                   (if (> vel-magnitude-in-requested-dir
                          (kin-attr-get-max-acceleration-delta kinetic-attributes))
                       (mul requested-dir-component
                            (/ (kin-attr-get-max-acceleration-delta kinetic-attributes)
                               vel-magnitude-in-requested-dir))
                       requested-dir-component)))))
    (define (accelerate-in-2d-dir! dir amount)
      (if (> amount (kin-attr-get-max-acceleration-delta kinetic-attributes))
          (set! amount (kin-attr-get-max-acceleration-delta kinetic-attributes)))
      ; Yritetään pysähtyä ensin mikäli ollaan ennestään menossa vastakkaiseen suuntaan.
      (if (negative? (* amount
                        (inner-product (2d-dir->3d-unit-vec dir)
                                       vel-vec)))
          (stop-in-2d-dir! dir))
      (set! vel-vec
            (add vel-vec
                 (mul amount
                      (2d-dir->3d-unit-vec dir)))))
    (define (tick!)
      ; Liikutaan
      (let ((wall-collision? (try-to-move-to! (add loc-vec
                                                   vel-vec))))
        ; Jarrutus
        (set! vel-vec (mul (kin-attr-get-deceleration-factor kinetic-attributes)
                           (decelerate-with-delta vel-vec
                                                  (kin-attr-get-deceleration-delta kinetic-attributes))))
        ; Painovoimakiihdytys/lattiasta kimpoaminen
        (if (> (z-component loc-vec) (+ floor-altitude gravity-acceleration-delta))
            ; Putoaa
            (set! vel-vec (add vel-vec
                               (vector 0 0 (- gravity-acceleration-delta))))
            (if (> (z-component loc-vec) floor-altitude)
                ; Melkein maan tasalla
                (begin
                  (vector-set! loc-vec 2 floor-altitude))
                  ;(vector-set! vel-vec 2 0))
                ; Maan tasalla
                (if (not (= (z-component vel-vec) 0))
                    (set! vel-vec (mul (kin-attr-get-bounce-deceleration-factor kinetic-attributes)
                                       (decelerate-with-delta (vector-map-n mul
                                                                            (vector 1 1 -1)
                                                                            vel-vec)
                                                              (kin-attr-get-bounce-deceleration-delta kinetic-attributes)))))))
        ; Törmäys?
        (cond ((or (eq? wall-collision? 'horizontal)
                   (eq? wall-collision? 'both))
               (set! vel-vec
                     (vector-map-n mul
                                   (vector 1 -1 1)
                                   vel-vec)))
              ((or (eq? wall-collision? 'vertical)
                   (eq? wall-collision? 'both))
               (set! vel-vec
                     (vector-map-n mul
                                   (vector -1 1 1)
                                   vel-vec))))
        (if wall-collision?
            ; Jarrutus ja ylöspäin -kimpoaminen (vain jos ollaan maan tasalla)
            (set! vel-vec
                  (add (mul (kin-attr-get-bounce-deceleration-factor kinetic-attributes)
                            (decelerate-with-delta vel-vec
                                                   (kin-attr-get-bounce-deceleration-delta kinetic-attributes)))
                       (vector 0
                               0
                               (if (= (z-component loc-vec) floor-altitude)
                                   (* bounce-vertical-acceleration-delta
                                      (kin-attr-get-bounce-deceleration-factor kinetic-attributes)
                                      (vector-geom-length vel-vec))
                                   0)))))))
    
    (define (set-loc! new-loc)
      (set! loc-vec new-loc))
    (define (get-loc)
      loc-vec)
    (define (get-vel)
      vel-vec)
    ; Tämän kutsun jälkeen uuteen huoneeseen siirryttäessä annettua funktiota kutsutaan muodossa:
    ;   (target-func cell-x.y)
    (define (request-cell-visit-messages new-target-func)
      (set! cell-visit-messages-target new-target-func))
    (define (get-loc-as-x.y-pair)
      (cons (x-component loc-vec)
            (y-component loc-vec)))
    (define (get-loc-as-cell-index-x.y)
      (cons (get-loc-as-cell-index-x)
            (get-loc-as-cell-index-y)))
    (define (set-kinetic-attributes! new-attrs)
      (set! kinetic-attributes new-attrs))
    
    
    ;;; =======================================================
    ;;;    Dispatch
    ;;; =======================================================
    (define (dispatch method)
      (cond ((eq? method 'tick!) tick!)
            ((eq? method 'stop-in-2d-dir!) stop-in-2d-dir!)
            ((eq? method 'accelerate-in-2d-dir!) accelerate-in-2d-dir!)
            ((eq? method 'set-loc!) set-loc!)
            ((eq? method 'set-kinetic-attributes!) set-kinetic-attributes!)
            ((eq? method 'get-loc) get-loc)
            ((eq? method 'get-vel) get-vel)
            ((eq? method 'request-cell-visit-messages) request-cell-visit-messages)
            ((eq? method 'get-loc-as-x.y-pair) get-loc-as-x.y-pair)
            ((eq? method 'get-loc-as-cell-index-x.y) get-loc-as-cell-index-x.y)
            (else (error "Unknown method! locator." method))))
    ((ticker 'request-ticking!) dispatch)
    dispatch))
