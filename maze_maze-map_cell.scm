;;; maze_maze-map_cell.scm


;;; maze.maze-map.cell -olio.
;;; sis. tiedot etelä- ja länsiseinien olemassaolosta sekä huoneen valaistuksesta
(define (new-cell wall-at-south? wall-at-west?)
  ; Valoja ei tallenneta save-metodissa.
  ; *-lighting sisältö:
  ;   Jos kyseistä valoa ei ole:
  ;     false
  ;   Jos kyseinen valo on olemassa, mutta sitä ei ole vielä laskettu:
  ;     'delayed
  ;   Muuten:
  ;     Molemmat muuttujat sisältävät color-cycle-steps -pituisen vektorin, joka sis:
  ;       cell-lightning -rakenteita.
  ;         (= 5 -pituisen vektorin (0=lattia, 1-4=ilmansuunnat), joka sis:
  ;            valoarvon ko. ilmansuuntaan/lattialle.)
  ; *-lightning-components sisältö:
  ;   Lista kaikkien huonetta valaisevien valonlähteiden valon kirkkauksista eri color-cycle -vaiheissa,
  ;   ja suunnasta tässä huoneessa. (eli lista color-cycle-steps -pituisia vektoreita, jotka sis valoarvon)
  (let ((solve-route-lightning false)
        (solve-route-lightning-components '())
        (dynamic-lights-lightning false)
        (dynamic-lights-lightning-components '()))
    
    ; Apufunktiot ---------------------------------------------
    ; light-component, sis:
    ;   -valonlähteen tunnisteen
    ;   -color-cycle-steps -pituisen vektorin valoarvoja
    ;   -valon suuntavektorin
    (define (make-light-component id lights dir-vec)
      (vector id
              lights
              dir-vec))
    (define (light-component-id c)
      (vector-ref c 0))
    (define (light-component-lights c)
      (vector-ref c 1))
    (define (light-component-dir-vec c)
      (vector-ref c 2))

    ; Kokoaa komponenteista kokonaisvalaistuksen.
    ; Paluuarvo:
    ;   Sama kuin *-lightning -muuttujien sisältö, ks yllä.
    (define (compile-lightning components)
      ; Valon kirkkaus halutussa suunnassa. Jos light-dir = <number>, on kirkkaus sivuille 1.0 ja
      ;                                                               alaspäin ko. kertoimen verran
      ;   Kirkkaus valon suuntaan: 1.0
      ;            90' sivulle:    light-ambient-diffusion
      ;            vastasuuntaan:  light-ambient-diffusion + reflected-light-magnitude-factor
      ; requested-dir oltava jonkin kantavektorin suuntainen yksikkövektori!
      ;   (lattiavalaistussähläys rajoittaa..)
      (define (light-magnitude-in-dir requested-dir light-dir)
        (if (vector? light-dir)
            ; Suuntavektori
            (linear-interpolate (inner-product requested-dir
                                               light-dir)   ; = t
                                (cons -1 (+ light-ambient-diffusion
                                            reflected-light-magnitude-factor))   ; 1. ohjauspiste
                                (cons 0 light-ambient-diffusion)   ; 2. ohjauspiste
                                (cons 1 1.0))   ; 3. ohjauspiste
            ; Skalaari = lattian kirkkauskerroin
            (if (= (z-component requested-dir) 0)
                ; Seinien kirkkaus
                1.0
                ; Lattian kirkkaus
                light-dir)))
      (define (compile-light-in-dir lights dir-vecs requested-dir)
        (apply add
               (map mul
                    lights
                    (map (lambda (light-dir-vec)
                           (light-magnitude-in-dir requested-dir
                                                   light-dir-vec))
                         dir-vecs))))
      (define (compile-phase-n-lightning components color-cycle-phase)
        (let ((component-lights-in-this-phase (map (lambda (component)
                                                     (vector-ref (light-component-lights component)
                                                                 color-cycle-phase))
                                                   components))
              (component-dir-vecs (map (lambda (component)
                                         (light-component-dir-vec component))
                                       components)))
          (vector
           ; Lattia
           (compile-light-in-dir component-lights-in-this-phase
                                 component-dir-vecs
                                 (vector 0 0 -1))
           ; east
           (compile-light-in-dir component-lights-in-this-phase
                                 component-dir-vecs
                                 (vector 1 0 0))
           ; north
           (compile-light-in-dir component-lights-in-this-phase
                                 component-dir-vecs
                                 (vector 0 1 0))
           ; west
           (compile-light-in-dir component-lights-in-this-phase
                                 component-dir-vecs
                                 (vector -1 0 0))
           ; south
           (compile-light-in-dir component-lights-in-this-phase
                                 component-dir-vecs
                                 (vector 0 -1 0)))))
      
      (if (null? components)
          (make-vector color-cycle-steps
                       dark-cell-lightning)
          (generate-vector-with-i color-cycle-steps
                                  (lambda (color-cycle-phase)
                                    (compile-phase-n-lightning components
                                                               color-cycle-phase)))))
    
    ; Metodit -------------------------------------------------
    (define (set-wall! dir value)
      (cond ((eq? dir 's) (set! wall-at-south? value))
            ((eq? dir 'w) (set! wall-at-west? value))
            (else (error "Unknown direction! maze.cell.set-wall! " dir))))
    (define (wall-at? dir)
      (cond ((eq? dir 's) wall-at-south?)
            ((eq? dir 'w) wall-at-west?)
            (else (error "Unknown direction! maze.cell.wall-at? " dir))))
    
    (define (reset-solve-route-lightning!)
      (set! solve-route-lightning false)
      (set! solve-route-lightning-components '()))
    ; Palauttaa cell-lightning -rakenteen
    (define (get-solve-route-lightning color-cycle-phase)
      (if solve-route-lightning
          (begin
            (if (eq? solve-route-lightning 'delayed)
                (set! solve-route-lightning
                      (compile-lightning solve-route-lightning-components)))
            (vector-ref solve-route-lightning color-cycle-phase))
          dark-cell-lightning))
    (define (reset-dynamic-lights-lightning!)
      (set! dynamic-lights-lightning false)
      (set! dynamic-lights-lightning-components '()))
    ; Palauttaa cell-lightning -rakenteen
    (define (get-dynamic-lights-lightning color-cycle-phase)
      (if dynamic-lights-lightning
          (begin
            (if (eq? dynamic-lights-lightning 'delayed)
                (set! dynamic-lights-lightning
                      (compile-lightning dynamic-lights-lightning-components)))
            ;(display (list 'cell-get-dyn: dynamic-lights-lightning)) (newline)
            (vector-ref dynamic-lights-lightning color-cycle-phase))
          dark-cell-lightning))
    
    
    ; Lisää valonlähteen vaikutuksen tähän huoneeseen.
    ; light-source-id = mikä tahansa yksilöllinen tunniste, esim valonlähteen osoitin.
    ; light-source-type = 'solve-route-light tai 'dynamic-light
    ; lights = color-cycle-steps -pituinen vektori joka sis valoarvoja ko. phasessa.
    ; dir-vec = valon yksikkö(!)suuntavektori, tai <number> jos valo säteilee joka suuntaan.
    ;           tällöin ko. luku on lattian kirkkauskerroin.
    (define (add-light! light-source-id light-source-type lights dir-vec)
      (cond ((eq? light-source-type 'solve-route-light)
             (set! solve-route-lightning-components
                   (cons (make-light-component light-source-id
                                               lights
                                               dir-vec)
                         solve-route-lightning-components))
             (set! solve-route-lightning 'delayed))
            ((eq? light-source-type 'dynamic-light)
             (set! dynamic-lights-lightning-components
                   (cons (make-light-component light-source-id
                                               lights
                                               dir-vec)
                         dynamic-lights-lightning-components))
             (set! dynamic-lights-lightning 'delayed))
            (else (error "Unknown light type! maze.maze-map.cell.add-light!." light-source-type))))
    ; Poistaa valonlähteen vaikutuksen tästä huoneesta
    (define (remove-light! light-source-id light-source-type)
      (cond ((eq? light-source-type 'solve-route-light)
             (set! solve-route-lightning-components
                   (remove-from-list-if (lambda (component)
                                          (eq? (light-component-id component)
                                               light-source-id))
                                        solve-route-lightning-components))
             (set! solve-route-lightning 'delayed))
            ((eq? light-source-type 'dynamic-light)
             (set! dynamic-lights-lightning-components
                   (remove-from-list-if (lambda (component)
                                          (eq? (light-component-id component)
                                               light-source-id))
                                        dynamic-lights-lightning-components))
             (set! dynamic-lights-lightning 'delayed))
            (else (error "Unknown light type! maze.maze-map.cell.remove-light!." light-source-type))))
    
    (define (load-from-current!)
      (set! wall-at-south? (read))
      (set! wall-at-west? (read)))
    (define (save-to-current)
      (write wall-at-south?)
      (write wall-at-west?))
    
    ; Dispatch ------------------------------------------------
    (define (dispatch method)
      (cond ((eq? method 'set-wall!) set-wall!)
            ((eq? method 'wall-at?) wall-at?)
            ((eq? method 'reset-solve-route-lightning!) reset-solve-route-lightning!)
            ((eq? method 'get-solve-route-lightning) get-solve-route-lightning)
            ((eq? method 'reset-dynamic-lights-lightning!) reset-dynamic-lights-lightning!)
            ((eq? method 'get-dynamic-lights-lightning) get-dynamic-lights-lightning)
            ((eq? method 'add-light!) add-light!)
            ((eq? method 'remove-light!) remove-light!)
            ((eq? method 'load-from-current!) load-from-current!)
            ((eq? method 'save-to-current) save-to-current)
            (else (error "Unknow method! maze.cell." method))))
    dispatch))
