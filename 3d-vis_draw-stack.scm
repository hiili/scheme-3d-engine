;;; 3d-vis_draw-stack.scm


;;; Monikulmio.
;;;   Sis‰lt‰‰:
;;;     -v‰ri
;;;     -k‰rkivektorit
;;;     -unique-maze-corner-indexes = mik‰li has-integer-maze-coords = true,
;;;                                   -> lista k‰rkivektoreita vastaavien labyrintin kulmien indekseist‰
;;;                                   (draw-stack.push-maze-polygon generoi tarvittaessa automaatisesti)
;;;     -flagit:
;;;       'has-integer-maze-coords = vektorit ovat kokonaislukukoordinaatteja labyrintin koon rajoissa
;;;       'is-wall                 = kyseess‰ on sein‰
(define (make-polygon color-string vectors . flags)
  (vector color-string
          vectors
          (if (memq 'is-wall flags) true false)
          (if (memq 'has-integer-maze-coords flags) true false)
          false))   ; unique-maze-corner-indexes
(define (polygon-get-color-string p)
  (vector-ref p 0))
(define (polygon-get-vectors p)
  (vector-ref p 1))
(define (polygon-set-vectors! p vecs)
  (vector-set! p 1 vecs))
(define (polygon-is-wall? p)
  (vector-ref p 2))
(define (polygon-has-integer-maze-coords? p)
  (vector-ref p 3))
(define (polygon-set-unique-maze-corner-indexes! p indexes)
  (vector-set! p 4 indexes))
(define (polygon-get-unique-maze-corner-indexes p)
  (vector-ref p 4))


;;; Sein‰ n‰kyviss‰? Ker‰‰ samalla tietokantaa seinien peittoalueista.
;;; Annettujen vektorien oltava suhteellisia kameran sijaintiin n‰hden,
;;; kameran edess‰(!) ja kameran on oltava seinien yl‰laidan alapuolella.
(define (new-visibility-manager)
  (define make-sector cons)
  (define sector-left-edge car)
  (define sector-right-edge cdr)
  (define (inside-sector? sector k)
    (<= (sector-left-edge sector) k (sector-right-edge sector)))
  (let (; visible-sectors:
        ;   sis‰lt‰‰ suuruusj‰rjestyksess‰ (eli vasemmalta oikealle) listan pareja,
        ;   jotka kuvaavat peitt‰m‰ttˆmien suuntien alku- ja loppukulmakertoimia:
        ;     k = left-right-offset / z-distance
        (visible-sectors false)
        ; x.y-pair->relative-k:
        ;   muuntaa x.y -koordinaattiparin primary-walk-enws-dir -suuntaan
        ;   suhteutetuksi kulmakertoimeksi:
        ;     left-right-offset / z-distance
        (x.y-pair->relative-k false)
        ; Sis‰lt‰‰ viimeksi liitettyn monikulmion peitt‰m‰n sektorin.
        (last-merged-vectors-sector false))
    
    ;;; Apufunktiot
    (define (make-func-x.y-pair->relative-k primary-walk-enws-dir)
      (define (process-lr-offset-and-z-distance left-right-offset z-distance)
        (if (> z-distance 0)
            (/ left-right-offset
               z-distance)
            (* left-right-offset
               +inf.0)))
      (cond ((eq? primary-walk-enws-dir 'e)
             (lambda (x.y-pair)
               (process-lr-offset-and-z-distance (- (cdr x.y-pair))
                                                 (car x.y-pair))))
            ((eq? primary-walk-enws-dir 'n)
             (lambda (x.y-pair)
               (process-lr-offset-and-z-distance (car x.y-pair)
                                                 (cdr x.y-pair))))
            ((eq? primary-walk-enws-dir 'w)
             (lambda (x.y-pair)
               (process-lr-offset-and-z-distance (cdr x.y-pair)
                                                 (- (car x.y-pair)))))
            (else   ; 's
             (lambda (x.y-pair)
               (process-lr-offset-and-z-distance (- (car x.y-pair))
                                                 (- (cdr x.y-pair)))))))
    ; x.y-pair->relative-k:n oltava jo m‰‰ritelty t‰t‰ kutsuttaessa
    (define (calculate-camera-visible-sector ahead-vec)
      (let ((ahead-vec-mapped-dir-k (x.y-pair->relative-k (loc-vector->x.y-pair ahead-vec))))
        (let ((sector-left-edge-k (tan (- (atan ahead-vec-mapped-dir-k)
                                          0.463647609000806)))   ; 0.4636..=(atan 0.5), <- fiksattu field of view
              (sector-right-edge-k (tan (+ (atan ahead-vec-mapped-dir-k)
                                           0.463647609000806))))   ; (atan 0.5)
          ;(display (list 'cam-visible-sector: sector-left-edge-k sector-right-edge-k)) (newline)
          (make-sector sector-left-edge-k
                       sector-right-edge-k))))
    ; Palauttaa kysytyn sektorin n‰kyvyystilanteen:
    ;   'visible, 'partially-visible tai 'obscured
    (define (sector-visible? left-k right-k)
      (define (sector-visible?-recurse left-k right-k rest-visible-sectors)
        (if (null? rest-visible-sectors)
            'obscured
            (let ((first-visible-sector (car rest-visible-sectors)))
              (cond
               ; obscured?
               ((<= right-k (sector-left-edge first-visible-sector)) 'obscured)
               ; visible?
               ((and (inside-sector? first-visible-sector left-k)
                     (inside-sector? first-visible-sector right-k)) 'visible)
               ; partially visible?
               ((and (<= left-k (sector-right-edge first-visible-sector))
                     (>= right-k (sector-left-edge first-visible-sector))) 'partially-visible)
               ; kokonaan t‰m‰n sektorin oikealla puolella?
               ((>= left-k (sector-right-edge first-visible-sector))
                (sector-visible?-recurse left-k right-k (cdr rest-visible-sectors)))
               (else (error "sector-visible? kusee.."))))))
      (sector-visible?-recurse left-k right-k visible-sectors))
    ; Poistaa annetun sektorin n‰kyvist‰ sektoreista
    (define (erase-sector-from-visibles! left-k right-k)
      ; Edet‰‰n vasemmalta oikealle ja kootaan uudelleen listaa
      (define (reconstruct-visible-sectors! rest-sectors left-k right-k)
        (if (null? rest-sectors)
            '()
            (let ((current-sector (car rest-sectors)))
              (cond
               ; sektori kokonaan nykyisen oikealla puolella? Siirryt‰‰n eteenp‰in.
               ((>= left-k (sector-right-edge current-sector))
                (cons current-sector
                      (reconstruct-visible-sectors! (cdr rest-sectors)
                                                    left-k
                                                    right-k)))
               ; sektori jo kokonaan nykyisen vasemmalla puolella? Katkaistaan rekursio.
               ((<= right-k (sector-left-edge current-sector))
                rest-sectors)
               ; menee kokonaan nykyisen yli? Poistetaan sektori kokonaan ja siirryt‰‰n eteenp‰in.
               ((and (<= left-k (sector-left-edge current-sector))
                     (>= right-k (sector-right-edge current-sector)))
                (reconstruct-visible-sectors! (cdr rest-sectors)
                                              left-k
                                              right-k))
               ; alkaa vasemmalta ja p‰‰ttyy t‰h‰n sektoriin? leikataan sektoria ja katkaistaan rekursio.
               ((and (<= left-k (sector-left-edge current-sector))
                     (inside-sector? current-sector right-k))
                (cons (make-sector right-k
                                   (sector-right-edge current-sector))
                      (cdr rest-sectors)))
               ; alkaa t‰st‰ sektorista ja jatkuu oikealle? leikataan sektoria ja siirryt‰‰n eteenp‰in.
               ((and (inside-sector? current-sector left-k)
                     (>= right-k (sector-right-edge current-sector)))
                (cons (make-sector (sector-left-edge current-sector)
                                   left-k)
                      (reconstruct-visible-sectors! (cdr rest-sectors)
                                                    left-k
                                                    right-k)))
               ; kokonaan nykyisen sis‰ll‰? Jaetaan current-sektori kahtia ja katkaistaan rekursio.
               ((and (inside-sector? current-sector left-k)
                     (inside-sector? current-sector right-k))
                (cons (make-sector (sector-left-edge current-sector)
                                   left-k)
                      (cons (make-sector right-k
                                         (sector-right-edge current-sector))
                            (cdr rest-sectors))))
               (else (error "erase-sector-from-visibles! kusee.."))))))
      ;(display (list 'erase-from-vis: 'sect.: left-k right-k)) (newline)
      ;(display (list 'erase-from-vis: 'start: visible-sectors)) (newline)
      (set! visible-sectors (reconstruct-visible-sectors! visible-sectors
                                                          left-k
                                                          right-k))
      ;(display (list 'erase-from-vis: 'end..: visible-sectors)) (newline)
      )
    
    ;;; Metodit
    ; Alustaa n‰kyvyystietokannan kameran n‰kˆkent‰ll‰ ja katseen ilmansuunnalla
    (define (set-visible-sector! primary-walk-enws-dir ahead-vec)
      (set! x.y-pair->relative-k (make-func-x.y-pair->relative-k primary-walk-enws-dir))
      (set! visible-sectors (list (calculate-camera-visible-sector ahead-vec))))
    ; Palauttaa truen jos monikulmio on (edes osittain) n‰kyviss‰.
    ; Lis‰ksi p‰ivitt‰‰ peitt‰vyystietokantaa mik‰li on kyse sein‰st‰.
    ; P‰ivitt‰‰ myˆs last-merged-vectors-sector -muuttujan.
    (define (check-and-merge-vectors! vectors is-wall?)
      (if (not x.y-pair->relative-k)
          true
          (let ((k-values (map (lambda (vector)
                                 (x.y-pair->relative-k (loc-vector->x.y-pair vector)))
                               vectors)))
            (let ((left-k (apply min k-values))
                  (right-k (apply max k-values)))
              (set! last-merged-vectors-sector (make-sector left-k
                                                            right-k))
              (if (eq? (sector-visible? left-k
                                        right-k)
                       'obscured)
                  false
                  (begin
                    (if is-wall?
                        (erase-sector-from-visibles! left-k
                                                     right-k))
                    true))))))
    ; Onko tietty ilmansuunta jo kokonaan peitetty?
    (define (all-obscured-at? dir-name)
      (cond ((eq? dir-name 'ahead) (null? visible-sectors))
            ((eq? dir-name 'left) (eq? (sector-visible? -inf.0 0)
                                       'obscured))
            ((eq? dir-name 'right) (eq? (sector-visible? 0 +inf.0)
                                        'obscured))))
    ; Palauttaa tietoa viimeksi liitetyst‰ monikulmiosta
    (define (last-merged-vectors method)
      (if last-merged-vectors-sector
          (cond ((eq? method 'were-leftmost-visible?)
                 (eq? (sector-visible? -inf.0
                                       (sector-right-edge last-merged-vectors-sector))
                      'obscured))
                ((eq? method 'were-rightmost-visible?)
                 (eq? (sector-visible? (sector-left-edge last-merged-vectors-sector)
                                       +inf.0)
                      'obscured)))
          false))
    
    ;;; Dispatch
    (define (dispatch method)
      (cond ((eq? method 'set-visible-sector!) set-visible-sector!)
            ((eq? method 'check-and-merge-vectors!) check-and-merge-vectors!)
            ((eq? method 'all-obscured-at?) all-obscured-at?)
            ((eq? method 'last-merged-vectors) last-merged-vectors)
            (else (error "Unknown method! draw-stack&visibility-manager." method))))
    dispatch))


;;; Pino-olio. T‰nne ladotaan ensin kaikki piirrett‰v‰t sein‰t
;;; ja lattiat (monikulmioita) maze-koordinaateilla, jolloin ne tallentuvat
;;; pinoon (jos ovat n‰kyviss‰) suhteellisina kameraan.
(define (new-draw-stack camera maze-interface)
  (let* ((maze-width (car (maze-interface 'size)))
         (maze-height (cadr (maze-interface 'size)))
         ; Sis‰lt‰‰ polygon -rakenteita listana
         (the-stack '())
         (visibility-manager (new-visibility-manager)))
    
    ;;; ==================================================
    ;;;    Apufunktiot                                    
    ;;; ==================================================
    (define (absolute->relative-maze-coords camera vectors)
      (let ((loc-vec ((camera '3d-vectors) 'loc-vec)))
        (map (lambda (vec)
               (sub vec loc-vec))
             vectors)))
    
    ; Generoi monikulmion k‰rkivektoreita vastaavat labyrintti-indeksit, katso make-polygon
    (define (generate-unique-maze-corner-indexes polygon maze-width maze-height)
      (map (lambda (vec)
             ; index = vec[2] * ((maze-width + 1) * (maze-height + 1)) +
             ;         vec[1] * (maze-width + 1) +
             ;         vec[0]
             (+ (* (vector-ref vec 2)
                   (+ maze-width 1)
                   (+ maze-height 1))
                (* (vector-ref vec 1)
                   (+ maze-width 1))
                (vector-ref vec 0)))
           (polygon-get-vectors polygon)))
      
    ; Luo listan totuusarvoja, jotka kertovat onko vastaava vektori katoamistason edess‰
    ; (Vektorien oltava suhteellisia kameraan, lis‰ksi
    ;  projection-vanishing-plane-guard-distance-factor otetaan huomioon,
    ;  eli sis‰tulon oltava v‰hint‰‰n em vakion verran)
    (define (make-in-front-of-camera?-list camera vectors)
      (map (lambda (vector)
             (> (inner-product ((camera '3d-vectors) 'ahead-vec)
                               vector)
                projection-vanishing-plane-guard-distance-factor))
           vectors))
    
    ; Palauttaa vektorin, joka sijaitsee katoamistasolla (guard-factor huomioiden)
    ; kahden annetun vektorin v‰liss‰. K‰ytt‰‰ lis‰ksi kamera -oliota.
    (define (interpolate-vectors-to-vanishing-plane vector-1 vector-2 vanishing-plane-guard-factor)
      ;(display (list 'interp-to-plane: vector-1 vector-2)) (newline)
      (let* ((ahead-vec ((camera '3d-vectors) 'ahead-vec))
             ;      vanishing-plane-guard-factor - ( ahead-vec | vector-2 )
             ; t = ---------------------------------------------------------
             ;          ( ahead-vec | ( vector-1 - vector-2 ) )
             (t (/ (- vanishing-plane-guard-factor
                      (inner-product ahead-vec
                                     vector-2))
                   (inner-product ahead-vec
                                  (sub vector-1
                                       vector-2)))))
        ; result-vec = (1-t) * vector-2 + t * vector-1
        (add (mul (- 1 t)
                  vector-2)
             (mul t
                  vector-1))))
    
    ; Leikkaa monikulmiota kuvaavista k‰rkivektoreista katoamistason takana olevan osuuden pois
    (define (crop-non-visible-part-from-vectors vectors in-front-of-camera?-list)
      ; Kun piste on tason takana:
      ;   -Jos pisteen molemmat naapurit ovat myˆs katoamistason takana,
      ;    voidaan piste heitt‰‰ pois.
      ;   -Jos molemmat naapurit ovat edess‰, jaetaan piste kahtia ja asetetaan
      ;    molemmat katoamistasolle alkup. pisteen ja ko. naapurin v‰liin
      ;   -Jos toinen naapuri on edess‰, siirret‰‰n piste katoamistasolle alkup. pisteen ja
      ;    ko. naapurin v‰liin.
      (define (process-vectors first-vec first-vec-in-front-of? previous-vec previous-vec-in-front-of?
                               rest-vecs rest-vecs-in-front-of?-list)
        (if (null? rest-vecs)
            '()
            ; Hoidellaan loput pisteet ensin..
            (let ((rest-processed-vectors (process-vectors first-vec
                                                           first-vec-in-front-of?
                                                           (car rest-vecs)
                                                           (car rest-vecs-in-front-of?-list)
                                                           (cdr rest-vecs)
                                                           (cdr rest-vecs-in-front-of?-list))))
              ; Piste tason edess‰? S‰ilytet‰‰n sellaisenaan
              (if (car rest-vecs-in-front-of?-list)
                  (cons (car rest-vecs)
                        rest-processed-vectors)
                  ; Ei ole, mit‰s tehd‰‰n?
                  (let ((current-vec (car rest-vecs))
                        (current-vec-in-front-of? (car rest-vecs-in-front-of?-list))
                        (next-vec (if (null? (cdr rest-vecs))
                                      first-vec
                                      (cadr rest-vecs)))
                        (next-vec-in-front-of? (if (null? (cdr rest-vecs-in-front-of?-list))
                                                   first-vec-in-front-of?
                                                   (cadr rest-vecs-in-front-of?-list))))
                    (cond
                     ; Myˆs molemmat naapurit tason takana? Heitet‰‰n piste pois
                     ((and (not previous-vec-in-front-of?)
                           (not next-vec-in-front-of?))
                      rest-processed-vectors)
                     ; Molemmat naapurit tason edess‰? Jaetaan kahtia..
                     ((and previous-vec-in-front-of?
                           next-vec-in-front-of?)
                      ; Interpolaatio edellisen suuntaan
                      (cons (interpolate-vectors-to-vanishing-plane previous-vec
                                                                    current-vec
                                                                    projection-vanishing-plane-guard-distance-factor)
                            ; Interpolaatio seuraavan suuntaan
                            (cons (interpolate-vectors-to-vanishing-plane current-vec
                                                                          next-vec
                                                                          projection-vanishing-plane-guard-distance-factor)
                                  ; Ja loput
                                  rest-processed-vectors)))
                     ; Eli vain toinen naapuri tason edess‰? Interpoloidaan sen suuntaan
                     (else
                      (cons (interpolate-vectors-to-vanishing-plane current-vec
                                                                    (if previous-vec-in-front-of?
                                                                        previous-vec
                                                                        next-vec)
                                                                    projection-vanishing-plane-guard-distance-factor)
                            rest-processed-vectors))))))))
      
      (process-vectors (car vectors)
                       (car in-front-of-camera?-list)
                       (list-ref vectors
                                 (- (length vectors) 1))
                       (list-ref in-front-of-camera?-list
                                 (- (length in-front-of-camera?-list) 1))
                       vectors
                       in-front-of-camera?-list))
    
    
    ;;; ==================================================
    ;;;    Metodit                                        
    ;;; ==================================================
    ; Laitetaan pinoon jos:
    ;   (ainakin osittain kameran edess‰) JA ( (kamera ylh‰‰ll‰) TAI
    ;                                          (kohde n‰kyviss‰) )
    ; Leikataan takana oleva osa pois jos vain osittain edess‰.
    (define (push-maze-polygon! polygon)
      ;(display 'push-maze-polygon!) (newline)
      (let ((relative-vectors (absolute->relative-maze-coords camera
                                                              (polygon-get-vectors polygon)))
            (cropped? false))
        ; Mitk‰ vektorit ovat kameran edess‰ ja mitk‰ takana?
        (let ((in-front-of-camera?-list (make-in-front-of-camera?-list camera relative-vectors)))
          ; Ainakin osa on edess‰?
          (if (apply-or in-front-of-camera?-list)
              (begin
                ; Kaikki eiv‰t ole edess‰? Leikataan takana oleva osuus pois
                (if (not (apply-and in-front-of-camera?-list))
                    (begin
                      (set! relative-vectors (crop-non-visible-part-from-vectors relative-vectors
                                                                                 in-front-of-camera?-list))
                      (set! cropped? true)))
                ; ( (kamera ylh‰‰ll‰) TAI (kohde n‰kyviss‰) )?
                (if (or ((camera 'above-walls?))
                        ((visibility-manager 'check-and-merge-vectors!) relative-vectors
                                                                        (polygon-is-wall? polygon)))
                    (begin
                      ; Voidaanko generoida labyrintti-indeksit monikulmiolle?
                      (if (and (polygon-has-integer-maze-coords? polygon)
                               (not cropped?))
                          (polygon-set-unique-maze-corner-indexes! polygon
                                                                   (generate-unique-maze-corner-indexes polygon
                                                                                                        maze-width
                                                                                                        maze-height)))
                      ; Talletetaan relatiiviset vektorit
                      (polygon-set-vectors! polygon relative-vectors)
                      ; Laitetaan pinoon
                      (set! the-stack
                            (cons polygon
                                  the-stack)))))))))
    
    (define (last-pushed-polygon method)
      ((visibility-manager 'last-merged-vectors) method))
    
    ; Palauttaa suoran referenssin sis‰iseen pinolistaan,
    ; jonka k‰sittely‰ varten on tossa lopussa parit funktiot.
    (define (get-direct-ref)
      the-stack)
    (define (get-maze-corner-index-count)
      (* 2
         (+ maze-width 1)
         (+ maze-height 1)))
    
    ;  Dispatch --------------------------------------------
    (define (dispatch method)
      (cond ((eq? method 'push-maze-polygon!) push-maze-polygon!)
            ;((eq? method 'set-visible-sector!) (visibility-manager 'set-visible-sector!))
            ;((eq? method 'all-obscured-at?) (visibility-manager 'all-obscured-at?))
            ((eq? method 'last-pushed-polygon) last-pushed-polygon)
            ((eq? method 'get-maze-corner-index-count) get-maze-corner-index-count)
            ((eq? method 'get-direct-ref) get-direct-ref)
            (else
             (visibility-manager method))))
    
    dispatch))
