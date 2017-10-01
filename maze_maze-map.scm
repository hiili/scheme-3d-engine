;;; maze_maze-map.scm


; category = 'solve-route-light / 'dynamic-light
; affection-distance = säteilyetäisyys
; lights = color-cycle-steps -pituinen vektori valoarvoja
(define (make-light-source-type category affection-distance lights)
  (vector category
          affection-distance
          lights))
(define (light-source-type->category lst)
  (vector-ref lst 0))
(define (light-source-type->affection-distance lst)
  (vector-ref lst 1))
(define (light-source-type->lights lst)
  (vector-ref lst 2))


;;; maze-map -olio, sis. konkreettisen vektoritaulukon sekä käyttöä
;;; helpottavia access-metodeja.
;;; Huom! Luo yhden ylim. rivin&sarakkeen pohjoiseen ja itään sivuseiniä varten!
;;; Sisältää myös alku- ja loppuhuoneen koordinaatit
(define (new-maze-map width height initial-walls-state)
  (load "maze_maze-map_cell.scm")
  (let ((y-vec (make-vector (+ height 1)))
        (start-loc (cons 0 0))
        (exit-loc (cons (- width 1) (- height 1)))
        (active-light-sources '()))
    
    ; Apufunktiot ------------------------------------------------
    ; Täyttää vektoritaulukon cell-olioilla
    (define (init)
      (do ((y 0 (+ y 1)))
          ((> y height))
        (vector-set! y-vec y (make-vector (+ width 1))))
      (for-each-x-y (lambda (x y)
                      (vector-set! (vector-ref y-vec y)
                                   x
                                   (new-cell initial-walls-state
                                             initial-walls-state)))))
    
    (define (get-cell-ref x y)
      (vector-ref (vector-ref y-vec y) x))
    
    ; Käy läpi jokaisen solun (myös ylimääräisen rivin ja sarakkeen!)
    (define (for-each-x-y func)
      (do ((y 0 (+ y 1)))
          ((> y height))
        (do ((x 0 (+ x 1)))
            ((> x width))
          (func x y))))
    
    (define (for-each-cell func)
      (for-each-x-y (lambda (x y) (func (get-cell-ref x y)))))

    ; Ensimmäisen alkion oltava id! (remove-light-source olettaa..)
    (define (make-light-source id affected-cells)
      (cons id
            affected-cells))
    (define (light-source-id ls)
      (car ls))
    (define (light-source-affected-cells ls)
      (cdr ls))
    
    ; Metodit ----------------------------------------------------
    (define (get-size-list)
      (list width height))
    
    (define (set-loc! id loc)
      (cond ((eq? id 'start) (set! start-loc loc))
            ((eq? id 'exit) (set! exit-loc loc))
            (else (error "Invalid location id! maze.maze-map.set-loc!" id))))
    
    (define (get-loc id)
      (cond ((eq? id 'start) start-loc)
            ((eq? id 'exit) exit-loc)
            (else (error "Invalid location id! maze.maze-map.get-loc" id))))
    
    ; Asettaa _vierekkäisten_ (x1,y1) ja (x2,y2) -välisen seinän tilan.
    (define (set-wall-between! x1 y1 x2 y2 wall-state)
      (let ((the-cell (get-cell-ref (max x1 x2) (max y1 y2)))
            (wall-dir (if (= x1 x2) 's 'w)))
        ((the-cell 'set-wall!) wall-dir wall-state)))
    ; Kysyy vierekkäisten solujen välisen seinän tilan
    (define (wall-between? x1 y1 x2 y2)
      (let ((the-cell (get-cell-ref (max x1 x2) (max y1 y2)))
            (wall-dir (if (= x1 x2) 's 'w)))
        ((the-cell 'wall-at?) wall-dir)))
    
    (define (set-wall-at-to! x y dir wall-state)
      (let ((the-cell (get-cell-ref (if (eq? dir 'e) (+ x 1) x)
                                    (if (eq? dir 'n) (+ y 1) y)))
            (the-dir (cond ((eq? dir 'e) 'w)
                           ((eq? dir 'n) 's)
                           (else dir))))
        ((the-cell 'set-wall!) the-dir wall-state)))
    (define (wall-at-to? x y dir)
      (let ((the-cell (get-cell-ref (if (eq? dir 'e) (+ x 1) x)
                                    (if (eq? dir 'n) (+ y 1) y)))
            (the-dir (cond ((eq? dir 'e) 'w)
                           ((eq? dir 'n) 's)
                           (else dir))))
        ((the-cell 'wall-at?) the-dir)))
    
    (define (load-from-current!)
      (set-loc! 'start (read))
      (set-loc! 'exit (read))
      (for-each-cell (lambda (cell)
                       ((cell 'load-from-current!)))))
    
    (define (save-to-current)
      (newline)
      (write (get-loc 'start))
      (write (get-loc 'exit))
      (newline)
      (for-each-x-y (lambda (x y)
                      (if (= x 0) (newline))
                      (((get-cell-ref x y) 'save-to-current))))
      (newline))
    
    ;(define (set-cell-solve-route-lightning! loc-x loc-y lightning)
    ;  (((get-cell-ref loc-x loc-y) 'set-solve-route-lightning!) lightning))
    ;(define (set-cell-dynamic-lights-lightning! loc-x loc-y lightning)
    ;  (((get-cell-ref loc-x loc-y) 'set-dynamic-lights-lightning!) lightning))
    ; Palauttaa cell-lightning -rakenteen
    (define (get-cell-solve-route-lightning loc-x loc-y color-cycle-phase)
      (((get-cell-ref loc-x loc-y) 'get-solve-route-lightning) color-cycle-phase))
    ; Palauttaa cell-lightning -rakenteen
    (define (get-cell-dynamic-lights-lightning loc-x loc-y color-cycle-phase)
      (((get-cell-ref loc-x loc-y) 'get-dynamic-lights-lightning) color-cycle-phase))
    
    (define (erase-solve-route-lights!)
      (for-each-cell (lambda (cell)
                       ((cell 'reset-solve-route-lightning!)))))

    ; Lisää valonlähteen labyrinttiin. Valoa ei pysty liikuttelemaan, ainoastaan poistamaan.
    ; Id = mikä tahansa uniikki tunniste
    ; type = 'solve-route-light tai 'dynamic-light
    ; Location = valonlähteen paikkavektori
    ; Lights = color-cycle-steps -pituinen vektori, joka sisältää valoarvoja ko. phasessa.
    (define (add-light-source! light-source-type location id)
      ; Yrittää edetä annettuun suuntaan. Huomioi seinät.
      (define (try-to-advance-to-dir light-source-type start-loc id
                                     current-cell-x.y current-distance walk-dir)
        (if (wall-at-to? (car current-cell-x.y)
                         (cdr current-cell-x.y)
                         walk-dir)
            '()   ; seinä edessä..
            (walk-thru-and-enlight-cells! light-source-type
                                          start-loc
                                          id
                                          (one-step-to-enws-dir current-cell-x.y
                                                                walk-dir)
                                          (+ current-distance 1)
                                          walk-dir)))
      ; Kulkee labyrintissä kaikkiin mahd. suuntiin, korkeintaan affection-distance -matkan päähän
      ; Palauttaa x.y-parilistana kaikkien käytyjen huoneiden koordinaatit
      ; Valo himmenee askelten määrän myötä, käännökset lasketaan yhdeksi askeleeksi.
      (define (walk-thru-and-enlight-cells! light-source-type start-loc id
                                            current-cell-x.y current-distance current-walk-dir)
        ; Valaistaan tämä huone
        (((get-cell-ref (car current-cell-x.y)
                        (cdr current-cell-x.y)) 'add-light!)
         id
         (light-source-type->category light-source-type)
         ; Valot eri cycle-phaseissa:
         (if (= current-distance 0)   ; Aloitushuoneessa vielä?
             (light-source-type->lights light-source-type)   ; Valo sellaisenaan
             (vector-map (lambda (light)   ; Ei olla, himmennetään valoa
                           (mul (/ 1
                                   (expt (+ (/ current-distance
                                               (if (eq? light-source-type 'solve-route-light)
                                                   solve-route-lights-effective-distance-factor
                                                   dynamic-lights-effective-distance-factor))
                                            1)
                                         2))
                                light))
                         (light-source-type->lights light-source-type)))
         ; Suunta:
         (if (= current-distance 0)   ; Aloitushuoneessa vielä?
             (+ (* -2 (z-component start-loc))   ; Valaisee kaikkiin suuntiin, asetetaan suunnaksi
                2)                               ; tieto lattian kirkkauskertoimesta
             (make-unit-vector
              (sub (cell-index-x.y->loc-vector current-cell-x.y
                                               0.0)   ; Ei olla, lasketaan suunta
                   start-loc))))
        ; Paluuarvo ja rekursiot:   (kaameeta katottavaa!)
        (cons current-cell-x.y
              ; Jatketaan seuraaviin huoneisiin, mikäli ei olla vielä liian pitkällä
              (if (>= current-distance
                      (light-source-type->affection-distance light-source-type))
                  '()
                  ; Kootaan lista kaikkien suuntien paluuarvoista
                  (apply append
                         (map (lambda (dir.new-distance)
                                (try-to-advance-to-dir light-source-type start-loc id
                                                       current-cell-x.y
                                                       (cdr dir.new-distance)
                                                       (car dir.new-distance)))
                              ; Suunnat ja uudet etäisyydet kyseiseen suuntaan lähdettäessä
                              (append (list (cons current-walk-dir   ; Eteenpäin
                                                  (if (= current-distance 0)   ; Aloitushuoneessa? Alussa mennään itään..
                                                      (- 1.001 (mod (x-component start-loc) 1))   ; ei näin..
                                                      (+ current-distance 1)))
                                            (cons (turn-left current-walk-dir)   ; Vasemmalle
                                                  (if (= current-distance 0)   ; Aloitushuoneessa?
                                                      (- 1.001 (mod (y-component start-loc) 1))   ; pohj. (ei tod näin!)
                                                      (+ current-distance 2)))
                                            (cons (turn-right current-walk-dir)   ; Oikealle
                                                  (if (= current-distance 0)   ; ( --||-- )
                                                      (+ 0.001 (mod (y-component start-loc) 1))   ; etelä
                                                      (+ current-distance 2))))
                                      (if (= current-distance 0)   ; Taakse, jos ollaan vasta ensimmäisessä huoneessa
                                          (list (cons (turn-back current-walk-dir)
                                                      (+ 0.001 (mod (x-component start-loc) 1))))
                                          '())))))))
      ; add-light-source! -runko. Lisätään koko homman tulos active-light-sourceseihin
      (set! active-light-sources
            (cons (make-light-source id
                                     (walk-thru-and-enlight-cells! light-source-type
                                                                   location
                                                                   id
                                                                   (loc-vector->cell-index-x.y location)
                                                                   0
                                                                   'e))
                  active-light-sources)))
    
    ; Poistaa valonlähteen labyrintistä tunnisteen perusteella.
    (define (remove-light-source! id category)
      (let ((affected-cells (light-source-affected-cells (assq id active-light-sources))))
        (for-each (lambda (cell-x.y-pair)
                    (((get-cell-ref (car cell-x.y-pair)
                                    (cdr cell-x.y-pair)) 'remove-light!) id category))
                  affected-cells)
        (set! active-light-sources
              (remove-from-list-if (lambda (ls)
                                     (eq? (light-source-id ls)
                                          id))
                                   active-light-sources))))
    
    ; Dispatch --------------------------------------------------
    (define (dispatch method)
      (cond ((eq? method 'get-size-list) get-size-list)
            ((eq? method 'set-loc!) set-loc!)
            ((eq? method 'get-loc) get-loc)
            ((eq? method 'set-wall-between!) set-wall-between!)
            ((eq? method 'wall-between?) wall-between?)
            ((eq? method 'set-wall-at-to!) set-wall-at-to!)
            ((eq? method 'wall-at-to?) wall-at-to?)
            ;((eq? method 'set-cell-solve-route-lightning!) set-cell-solve-route-lightning!)
            ;((eq? method 'set-cell-dynamic-lights-lightning!) set-cell-dynamic-lights-lightning!)
            ((eq? method 'erase-solve-route-lights!) erase-solve-route-lights!)
            ((eq? method 'get-cell-solve-route-lightning) get-cell-solve-route-lightning)
            ((eq? method 'get-cell-dynamic-lights-lightning) get-cell-dynamic-lights-lightning)
            ((eq? method 'add-light-source!) add-light-source!)
            ((eq? method 'remove-light-source!) remove-light-source!)
            ((eq? method 'load-from-current!) load-from-current!)
            ((eq? method 'save-to-current) save-to-current)
            (else (error "Unknown method! maze.maze-map." method))))
    
    (init)
    dispatch))
