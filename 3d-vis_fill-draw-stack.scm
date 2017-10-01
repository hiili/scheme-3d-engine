;;; 3d-vis_fill-draw-stack.scm

  
; draw-mode = 'all / 'static-elements / 'dynamic-elements
(define (fill-draw-stack draw-stack camera player maze-interface draw-mode)
  
  ;;; ==================================================
  ;;;    fill-draw-stack -apufunktiot                   
  ;;; ==================================================
  (define (loc-x.y-out-of-bounds? loc-x.y)
    ;(display (list 'loc-out-of-bounds? loc-x.y)) (newline)
    (let ((maze-size-list (maze-interface 'size)))
      (not (and (< -1 (car loc-x.y) (car maze-size-list))
                (< -1 (cdr loc-x.y) (cadr maze-size-list))))))
  
  ; Kutsuu funktiota func kaikille enws-dir -suunnassa oleville huoneille
  ; kunnes tulee seinä vastaan tai func palauttaa 'abort
  (define (for-each-cell-ahead start-cell-x.y enws-dir max-distance func)
    (if (and (not (loc-x.y-out-of-bounds? start-cell-x.y))
             (> max-distance 0))
        (begin
          (if (eq? (func start-cell-x.y) 'abort)
              'abort
              (for-each-cell-ahead (one-step-to-enws-dir start-cell-x.y
                                                         enws-dir)
                                   enws-dir
                                   (- max-distance 1)
                                   func)))))
  
  ; Lasketaan piirtoetäisyys:
  ;   not camera-above-walls? : +inf.0
  ;   0 < h-phase < 0.5       : h-transition-view-distance
  ;   h-phase >= 0.5 - 1      : h-transition-view-distance - laidoille
  (define (calculate-draw-distance h-phase camera-above-walls? maze-width maze-height)
    (cond ((not camera-above-walls?) +inf.0)
          ((< h-phase 0.5) h-transition-view-distance)
          (else
           (let ((t (* 2 (- h-phase 0.5))))
             (+ (* (- 1 t)
                   h-transition-view-distance)
                (* t
                   (+ (/ (max maze-width maze-height)
                         2)
                      1)))))))
  
  ; Pelaaja-valonlähteen kirkkauskerroin etäisyyden neliön funktiona.
  ; Jos h-phase > 0   -->   valo himmenee ja sammuu (h-phase = 0.5) :ssä
  (define (player-light-magnitude distance-pow2 h-phase)
    (let ((distance-pow2-scaled (/ distance-pow2 player-light-effective-distance-factor)))
      (if (< h-phase 0.5)
          (* ((player 'get-player-light-magnitude))   ; pelaajavalon alkuperäinen kirkkaus
             (- 1 (* 2 h-phase))   ; h-phase -feidaus
             (/ 1.0   ; etäisyysfeidaus
                (max distance-pow2-scaled 1.0)))
          0)))
  
  ; Laskee huoneen valaistuksen, joka koostuu seuraavista:
  ;   -ratkaisureitin valo, mikäli olemassa.
  ;   -liikkuvien valojen valo, mikäli h-phase < 0.5
  ;    (eli liikkuvat valot sammutetaan 2d-moodiin siirryttäessä)
  ;   -pelaajan valo
  ;   -cell-ambient-lightning
  (define (calculate-cell-lightning loc-x.y player-light h-phase)
    (let ((solve-route-lightning (maze-interface 'get-cell-solve-route-lightning
                                                 (car loc-x.y)
                                                 (cdr loc-x.y)
                                                 ((camera 'get-color-cycle-phase))))
          (dynamic-lights-lightning (maze-interface 'get-cell-dynamic-lights-lightning
                                                    (car loc-x.y)
                                                    (cdr loc-x.y)
                                                    ((camera 'get-color-cycle-phase))))
          (ambient-lightning (if (maze-interface 'solve-route-marked?)
                                 cell-ambient-lightning-when-solve-route-marked
                                 cell-ambient-lightning-when-no-solve-route)))
      (vector-map-n (lambda (l1 l2 l3)
                      (add player-light
                           l1
                           l2
                           l3))
                    solve-route-lightning
                    (cond ((= h-phase 0) dynamic-lights-lightning)
                          ((< h-phase 0.5) (vector-map (lambda (light)
                                                         (mul (- 1 (* 2 h-phase))
                                                              light))
                                                       dynamic-lights-lightning))
                          (else dark-cell-lightning))
                    ambient-lightning)))
  
  (define (calculate-player-light loc-x.y player-x.y h-phase)
    (let ((distance-from-player-pow2 (x.y-pair-geom-length-pow2 (sub (add loc-x.y (cons 0.5 0.5))
                                                                     player-x.y))))
      (mul player-light-color
           (player-light-magnitude distance-from-player-pow2 h-phase))))
  
  ; Tunkee yhden seinän pinoon
  (define (push-wall loc-x.y enws-dir cell-lightning)
    (let ((corner-1-x.y (if (or (eq? enws-dir 's)
                                (eq? enws-dir 'w))
                            loc-x.y   ; sw -kulma
                            (cons (+ (car loc-x.y) 1)
                                  (+ (cdr loc-x.y) 1))))   ; ne -kulma
          (corner-2-x.y (if (or (eq? enws-dir 's)
                                (eq? enws-dir 'e))
                            (cons (+ (car loc-x.y) 1)
                                  (cdr loc-x.y))   ; se -kulma
                            (cons (car loc-x.y)
                                  (+ (cdr loc-x.y) 1))))   ; nw -kulma
          (current-dir-lightning (cell-lightning->cell-fenws-dir-lightning cell-lightning
                                                                           enws-dir)))
      ;(display (list 'wall loc-x.y enws-dir corner-1-x.y corner-2-x.y)) (newline)
      ((draw-stack 'push-maze-polygon!)
       (make-polygon (color->color-string (apply-light-to-color wall-color
                                                                current-dir-lightning))
                     (list (vector (car corner-1-x.y)
                                   (cdr corner-1-x.y)
                                   0)
                           (vector (car corner-1-x.y)
                                   (cdr corner-1-x.y)
                                   1)
                           (vector (car corner-2-x.y)
                                   (cdr corner-2-x.y)
                                   1)
                           (vector (car corner-2-x.y)
                                   (cdr corner-2-x.y)
                                   0))
                     'has-integer-maze-coords
                     'is-wall))))
  (define (push-floor cell-lightning loc-x loc-y)
    ((draw-stack 'push-maze-polygon!)
     (make-polygon (color->color-string (apply-light-to-color floor-color
                                                              (cell-lightning->cell-fenws-dir-lightning
                                                               cell-lightning
                                                               'f)))   ; 'f = floor lightning
                   (list (vector loc-x
                                 loc-y
                                 0)
                         (vector loc-x
                                 (+ loc-y 1)
                                 0)
                         (vector (+ loc-x 1)
                                 (+ loc-y 1)
                                 0)
                         (vector (+ loc-x 1)
                                 loc-y
                                 0))
                   'has-integer-maze-coords)))
  
  ; Laittaa pinoon huoneen takaseinän ja dynaamiset valot (jos h-phase < 0.5)
  ; valinnaisesti myös sivuseinän/-seinät.
  ;   (side-walls? == 'none | 'both-side-walls | 'left-side-wall | 'right-side-wall)
  (define (push-cell-contents loc-x.y player-x.y enws-dir side-walls? color-cycle-phase)
    ;(display (list 'push-cell: 'loc: loc-x.y)) (newline)
    (let* ((loc-x (car loc-x.y))
           (loc-y (cdr loc-x.y))
           (h-phase ((camera 'get-h-phase)))
           (cell-lightning (calculate-cell-lightning loc-x.y
                                                     (calculate-player-light loc-x.y
                                                                             player-x.y
                                                                             h-phase)
                                                     h-phase)))
      
      ; Näkyvät dynaamiset valot, jos h-phase < 0.5
      ; Ei-näkyvät dynaamiset valot h-phasen mukaan skaalattuna, jos 0 < h-phase < 0.5
      ;   (h-phase = 0: koko 0x, h-phase = 0.5: koko 1x)
      (if (< h-phase 0.5)
          (let ((dynamic-lights-in-cell (((maze-interface 'dynamic-lights-manager) 'get-dynamic-lights-in-cell-list)
                                         loc-x.y)))
            (for-each (lambda (dynamic-light)
                        (if ((dynamic-light 'visible-in-3d-mode?))
                            (push-dynamic-light-image dynamic-light
                                                      1
                                                      '3d
                                                      cell-lightning)))
                      dynamic-lights-in-cell)))
      ; Lattia
      (if (< h-phase 0.5)
          (push-floor cell-lightning loc-x loc-y))
      ; Takaseinä
      (if (maze-interface 'wall (+ loc-x 1) (+ loc-y 1) enws-dir)
          (push-wall loc-x.y
                     enws-dir
                     cell-lightning))
      ; Sivuseinät
      (if (and (or (eq? side-walls? 'both-side-walls)
                   (eq? side-walls? 'left-side-wall))
               (maze-interface 'wall (+ loc-x 1) (+ loc-y 1) (turn-left enws-dir)))
          (push-wall loc-x.y
                     (turn-left enws-dir)
                     cell-lightning))
      (if (and (or (eq? side-walls? 'both-side-walls)
                   (eq? side-walls? 'right-side-wall))
               (maze-interface 'wall (+ loc-x 1) (+ loc-y 1) (turn-right enws-dir)))
          (push-wall loc-x.y
                     (turn-right enws-dir)
                     cell-lightning))
      ; Lattia
      (if (>= h-phase 0.5)
          (push-floor cell-lightning loc-x loc-y))))
  
  ; Tunkee pinoon rivillisen huoneita. Tarkkailee pinon kertomaa seinien peittävyystilannetta
  ; ja keskeyttää, mikäli.. joo..
  (define (push-row-of-cells current-main-cell-x.y player-x.y primary-walk-enws-dir
                             draw-distance side-walls? color-cycle-phase)
    ;(display (list 'push-row: 'loc: current-main-cell-x.y)) (newline)
    ; Tämä huone pinoon
    (push-cell-contents current-main-cell-x.y
                        player-x.y
                        primary-walk-enws-dir
                        (if side-walls? 'both-side-walls 'none)
                        color-cycle-phase)
    ; Kävellään vasemmalle laidalle asti
    (for-each-cell-ahead (one-step-to-enws-dir current-main-cell-x.y
                                               (turn-left primary-walk-enws-dir))
                         (turn-left primary-walk-enws-dir)
                         draw-distance
                         (lambda (current-cell-x.y)
                           (push-cell-contents current-cell-x.y
                                               player-x.y
                                               primary-walk-enws-dir
                                               (if side-walls? 'left-side-wall 'none)
                                               color-cycle-phase)
                           (if ((draw-stack 'last-pushed-polygon) 'were-leftmost-visible?)
                               'abort)))
    ; Kävellään oikealle laidalle asti
    (for-each-cell-ahead (one-step-to-enws-dir current-main-cell-x.y
                                               (turn-right primary-walk-enws-dir))
                         (turn-right primary-walk-enws-dir)
                         draw-distance
                         (lambda (current-cell-x.y)
                           (push-cell-contents current-cell-x.y
                                               player-x.y
                                               primary-walk-enws-dir
                                               (if side-walls? 'right-side-wall 'none)
                                               color-cycle-phase)
                           (if ((draw-stack 'last-pushed-polygon) 'were-rightmost-visible?)
                               'abort))))
  
  ; Kävellään kaikkien edessä/takana (ahead/behind?) olevien huoneiden läpi
  ; peittävyysjärjestyksessä (lähimmät ensin) ja laitetaan
  ; niissä olevat seinät ja näkyvät dynaamiset valot piirtopinoon.
  (define (push-all-cells ahead/behind?)
    (let ((primary-walk-enws-dir (nearest-enws-dir ((player 'get-dir))
                                                   ahead/behind?))
          (camera-cell-x.y ((camera 'get-loc-as-cell-index-x.y)))
          (player-x.y (((player 'locator) 'get-loc-as-x.y-pair)))
          (color-cycle-phase ((camera 'get-color-cycle-phase)))
          (current-draw-distance (calculate-draw-distance ((camera 'get-h-phase))
                                                          ((camera 'above-walls?))
                                                          (car (maze-interface 'size))
                                                          (cadr (maze-interface 'size)))))
      ;(display (list 'push-all-cells: 'dir: primary-walk-enws-dir)) (newline)
      ; Asetetaan piirtopinoon tieto kameran näkökentässä olevasta sektorista
      ((draw-stack 'set-visible-sector!) primary-walk-enws-dir
                                         ((camera '3d-vectors) 'ahead-vec))
      ; Kävellään kaikkien edessä olevien huoneiden läpi laidalle asti
      ; (Jos ollaan menossa taaksepäin, ei pinota ekan rivin sivuseiniä,
      ;  ovat jo pinossa eteenpäin kävelyn jäljiltä)
      (if (eq? ahead/behind? 'behind)
          (push-row-of-cells camera-cell-x.y
                             player-x.y
                             primary-walk-enws-dir
                             current-draw-distance
                             false
                             color-cycle-phase))
      (for-each-cell-ahead (if (eq? ahead/behind? 'ahead)
                               camera-cell-x.y
                               (one-step-to-enws-dir camera-cell-x.y
                                                     primary-walk-enws-dir))
                           primary-walk-enws-dir
                           current-draw-distance
                           (lambda (current-main-cell-x.y)
                             (push-row-of-cells current-main-cell-x.y
                                                player-x.y
                                                primary-walk-enws-dir
                                                current-draw-distance
                                                true
                                                color-cycle-phase)
                             (if ((draw-stack 'all-obscured-at?) 'ahead)
                                 'abort)))))

  
  ; Interpoloi kolmion pintanormaalin suuntaisen valoarvon annetun solun ilmansuuntavalaistuksista
  ; ja tunkee kolmion piirtopinoon. Kulmapisteiden tulee kiertää kolmio myötäpäivään valaistavan
  ; pinnan puolelta katsottuna.
  (define (push-enlighted-triangle base-color cell-lightning corner-1 corner-2 corner-3)
    (let* ((surface-normal-vec (make-unit-vector (cross-product (sub corner-3 corner-1)
                                                                (sub corner-2 corner-1))))
           (surface-lightning (add (mul (abs (/ (asin (x-component surface-normal-vec)) pi/2))
                                        (cell-lightning->cell-fenws-dir-lightning cell-lightning
                                                                                  (if (positive? (x-component surface-normal-vec))
                                                                                      'w
                                                                                      'e)))
                                   (mul (abs (/ (asin (y-component surface-normal-vec)) pi/2))
                                        (cell-lightning->cell-fenws-dir-lightning cell-lightning
                                                                                  (if (positive? (y-component surface-normal-vec))
                                                                                      's
                                                                                      'n)))
                                   (if (negative? (z-component surface-normal-vec))
                                       (mul (/ (asin (- (z-component surface-normal-vec))) pi/2)
                                            (cell-lightning->cell-fenws-dir-lightning cell-lightning
                                                                                      'f))
                                       (vector 0 0 0)))))
      ((draw-stack 'push-maze-polygon!)
       (make-polygon (color->color-string (apply-light-to-color surface-lightning
                                                                (vector 1 1 1)))
                                                                ;(mul 0.5
                                                                ;     (add base-color
                                                                ;          (vector 1 1 1)))))
                     (list corner-1 corner-2 corner-3)))))
  
  ; Laittaa pinoon pelaajakuvan = neljästä palasta koostuva salmiakki
  (define (push-player-image player)
    (let ((cell-lightning (calculate-cell-lightning (((player 'locator) 'get-loc-as-cell-index-x.y))
                                                    (vector 0 0 0)
                                                    0))
          (player-loc (((player 'locator) 'get-loc)))
          (ahead-vec (vector (cos ((player 'get-dir)))
                             (sin ((player 'get-dir)))
                             0))
          (right-vec (vector (sin ((player 'get-dir)))
                             (- (cos ((player 'get-dir))))
                             0))
          (up-vec    (vector 0 0 1)))
      (let ((nose-vec         (add player-loc
                                   (mul 0.3 ahead-vec)))
            (right-corner-vec (add player-loc
                                   (mul 0.15 right-vec)))
            (left-corner-vec  (sub player-loc
                                   (mul 0.15 right-vec)))
            (tail-vec         (sub player-loc
                                   (mul 0.10 ahead-vec)))
            (top-vec          (add player-loc
                                   (mul 0.10 up-vec))))
        ; Etu-vasen kylki
        (push-enlighted-triangle player-color cell-lightning
                                 top-vec left-corner-vec nose-vec)
        ; Taka-vasen kylki
        (push-enlighted-triangle player-color cell-lightning
                                 top-vec tail-vec left-corner-vec)
        ; Taka-oikea kylki
        (push-enlighted-triangle player-color cell-lightning
                                 top-vec right-corner-vec tail-vec)
        ; Etu-oikea kylki
        (push-enlighted-triangle player-color cell-lightning
                                 top-vec nose-vec right-corner-vec))))
  
  ; Draw-mode = '3d / '2d.
  ;   (3d: kuva on 3d-timantti, 2d: kuvasta puuttuu pohja, joka ei kuitenkaan näkyisi ylhäältä)
  (define (push-dynamic-light-image dynamic-light size-scale-factor draw-mode cell-lightning)
    (if (> size-scale-factor 0)
        (let ((light-color ((dynamic-light 'get-light-color)))
              (loc-vec (((dynamic-light 'locator) 'get-loc)))
              (cell-x.y (((dynamic-light 'locator) 'get-loc-as-cell-index-x.y))))
          (if (eq? draw-mode '2d)
              ((draw-stack 'push-maze-polygon!) (make-polygon (color->color-string light-color)
                                                              (list loc-vec)))
              (let ((east-corner (add loc-vec
                                      (vector 0.15 0 0)))
                    (north-corner (add loc-vec
                                       (vector 0 0.15 0)))
                    (west-corner (add loc-vec
                                      (vector -0.15 0 0)))
                    (south-corner (add loc-vec
                                       (vector 0 -0.15 0)))
                    (top-corner (add loc-vec
                                     (vector 0 0 0.2)))
                    (bottom-corner (add loc-vec
                                        (vector 0 0 -0.2))))
                (push-enlighted-triangle light-color cell-lightning
                                         top-corner north-corner east-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         top-corner west-corner north-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         top-corner south-corner west-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         top-corner east-corner south-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         bottom-corner east-corner north-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         bottom-corner north-corner west-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         bottom-corner west-corner south-corner)
                (push-enlighted-triangle light-color cell-lightning
                                         bottom-corner south-corner east-corner))))))
  
  (define (push-all-dynamic-lights-as-images dynamic-lights-list filter-mode)
    ;(newline) (display (list 'push-all: filter-mode ':))
    (case filter-mode
      ((skip-bg-merged-lights)
       (for-each (lambda (dynamic-light)
                   (if (not ((dynamic-light 'bg-merged?)))
                       (push-dynamic-light-image dynamic-light
                                                 1
                                                 '2d
                                                 false)))
                 dynamic-lights-list))
      ((only-bg-mergeable-lights)
       (for-each (lambda (dynamic-light)
                   (if ((dynamic-light 'bg-mergeable?))
                       (begin
                         ;(display "x")
                         (push-dynamic-light-image dynamic-light
                                                   1
                                                   '2d
                                                   false)
                         ((dynamic-light 'set-as-bg-merged!)))))
                 dynamic-lights-list))))
  
  
  ;;; ==================================================
  ;;;    fill-draw-stack -runko                         
  ;;; ==================================================
  ; dynamic-elements:
  (if (or (eq? draw-mode 'all)
          (eq? draw-mode 'dynamic-elements))
      (begin
        ; Pelaaja:
        (if ((camera 'above-walls?))
            (push-player-image player))
        ; Dynaamiset valot:
        (if (eq? draw-mode 'dynamic-elements)
            (push-all-dynamic-lights-as-images
             (((maze-interface 'dynamic-lights-manager) 'get-dynamic-lights-list))
             'skip-bg-merged-lights))))
  ; static-elements:
  (if (or (eq? draw-mode 'all)
          (eq? draw-mode 'static-elements))
      (begin
        (if (eq? draw-mode 'static-elements)
            (push-all-dynamic-lights-as-images
             (((maze-interface 'dynamic-lights-manager) 'get-dynamic-lights-list))
             'only-bg-mergeable-lights))
        (push-all-cells 'ahead)
        (if (> ((camera 'get-h-phase)) 0)
            (push-all-cells 'behind)))))
