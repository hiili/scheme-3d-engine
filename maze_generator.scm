;;; maze-generator.scm


;;; maze.generator -funktio
;;;   palauttaa generoidun sokkelon, joka sis‰lt‰‰ start- ja exitpointit
(define (generator width height)
  (let ((maze-map (new-maze-map width height true))
        (remaining-cells (* width height)))
    
    (define (unvisited-cell? x y)
      (and ((maze-map 'wall-at-to?) x y 'e)
           ((maze-map 'wall-at-to?) x y 'n)
           ((maze-map 'wall-at-to?) x y 'w)
           ((maze-map 'wall-at-to?) x y 's)))
    
    (define (cut-from-list the-list index)
      (define (list-without-n the-list index curr-pos)
        (if (= index curr-pos)
            (cdr the-list)
            (cons (car the-list)
                  (list-without-n (cdr the-list) index (+ curr-pos 1)))))
      (list-without-n the-list index 0))
    
    ; Palauttaa x.y-parina satunnaisen naapurin jossa ei ole k‰yty,
    ; falsen jos kaikissa on k‰yty
    (define (gen-random-unvisited-neighbor-x.y x y)
      (define (try-randomly-from-these options)
        (if (null? options)
            false
            (let* ((choice (random (length options)))
                   (chosen-x.y (one-step-to-enws-dir (cons x y) (list-ref options choice))))
              (if (and (< -1 (car chosen-x.y) width)
                       (< -1 (cdr chosen-x.y) height)
                       (unvisited-cell? (car chosen-x.y) (cdr chosen-x.y)))
                  chosen-x.y
                  (try-randomly-from-these (cut-from-list options choice))))))
      (try-randomly-from-these '(e n w s)))
    
    ; V‰hennet‰‰n huonelaskuria ja
    ; l‰hetet‰‰n rekursiot t‰st‰ huoneesta kaikkiin mahdollisiin
    ; suuntiin satunnaisessa j‰rjestyksess‰
    (define (continue-recursion-at x y)
      
      ; L‰hetet‰‰n rekursioita satunnaisiin suuntiin (ja kaadetaan
      ; seini‰ matkalla) niin kauan kuin suuntia riitt‰‰.
      (define (recurse-into-random-dirs-from x y)
        (if (> remaining-cells 0)
            (let ((next-cell-x.y (gen-random-unvisited-neighbor-x.y x y)))
              (if next-cell-x.y
                  (begin
                    ; Sein‰ alas
                    ((maze-map 'set-wall-between!) x y
                                                   (car next-cell-x.y) (cdr next-cell-x.y)
                                                   false)
                    ; L‰hetet‰‰n rekursio
                    (continue-recursion-at (car next-cell-x.y)
                                           (cdr next-cell-x.y))
                    ; Tarkistetaan viel‰ loput suunnat
                    (recurse-into-random-dirs-from x y))))))
      
      (set! remaining-cells (- remaining-cells 1))
      (recurse-into-random-dirs-from x y))
            
    
    ; Generoidaan labyrintti
    (continue-recursion-at (random width)
                           (random height))
    ; Asetetaan aloitus- ja lopetuspisteet,
    ; lopetus aina vastakkaiselle sein‰lle
    (let ((start-wall (list-ref '(e n w s) (random 4))))
      ((maze-map 'set-loc!) 'start
                            (cons (cond ((eq? start-wall 'w) 0)
                                        ((eq? start-wall 'e) (- width 1))
                                        (else (random width)))
                                  (cond ((eq? start-wall 's) 0)
                                        ((eq? start-wall 'n) (- height 1))
                                        (else (random height)))))
      ((maze-map 'set-loc!) 'exit
                            (cons (cond ((eq? start-wall 'e) 0)
                                        ((eq? start-wall 'w) (- width 1))
                                        (else (random width)))
                                  (cond ((eq? start-wall 'n) 0)
                                        ((eq? start-wall 's) (- height 1))
                                        (else (random height))))))
    maze-map))
