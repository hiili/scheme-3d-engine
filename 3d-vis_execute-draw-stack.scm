;;; 3d-vis_execute-draw-stack.scm


;;;   Piirt‰‰ draw-stackin sis‰llˆn annetun kameran mukaan projisoituna
(define (execute-draw-stack draw-stack xo camera)
  (let ((window-width (car ((camera 'get-window-size-x.y))))
        (window-height (cdr ((camera 'get-window-size-x.y))))
        ; sis valmiiksilaskettuja ikkunakoordinaatteja
        (window-coord-cache (generate-vector ((draw-stack 'get-maze-corner-index-count))
                                             (lambda ()
                                               (list 'dummy))))
        ; the projektiomatriisi!
        (the-projection-matrix (make-projection-matrix-for-camera camera)))
    
    
    ;;; ==================================================
    ;;;    Apufunktiot                                    
    ;;; ==================================================
    ; Projisoi ja piirt‰‰ annetun monikulmion
    (define (project-and-draw-polygon polygon)
      
      ; project-and-draw-polygon -apufunktiot ---------------------------
      ; Projisoi yksitt‰isen vektorin ikkunakoordinaateiksi
      (define (project-maze-vector->window-coords-list vector maze-corner-index)
        (let* ((cache-ref (if maze-corner-index
                              (vector-ref window-coord-cache
                                          maze-corner-index)
                              '(dummy)))
               (window-coords-list (cdr cache-ref)))
          ; Lˆytyykˆ valmiina cachesta?
          (if (not (null? window-coords-list))
              window-coords-list
              ; Ei lˆytynyt, lasketaan uudet koordinaatit
              (begin
                (set! window-coords-list
                      (let ((window-coords-vec (vector-homogenic->normal
                                                (mul the-projection-matrix
                                                     (vector-normal->homogenic vector)))))
                        (let ((x-coord (vector-ref window-coords-vec 0))
                              (y-coord (vector-ref window-coords-vec 1)))
                          ; Ei anneta x-drawille liian isoja koordinaatteja
                          (list (cond ((< -30000 x-coord 30000) x-coord)
                                      ((< x-coord -30000) -30000)
                                      (else 30000))
                                (cond ((< -30000 y-coord 30000) y-coord)
                                      ((< y-coord -30000) -30000)
                                      (else 30000))))))
                (if maze-corner-index
                    (set-cdr! cache-ref window-coords-list))
                window-coords-list))))
      
      ; Projisoi vektorilistan ikkunakoordinaateiksi.
      (define (project-to-window-coords vectors indexes)
        (if (null? vectors)
            '()
            (cons (project-maze-vector->window-coords-list (car vectors)
                                                           (if indexes
                                                               (car indexes)
                                                               false))
                  (project-to-window-coords (cdr vectors)
                                            (if indexes
                                                (cdr indexes)
                                                false)))))
      ; Convex FillPolygon kusee viivamaisissa monikulmioissa..
      (define (check-and-convert-if-line window-polygon-list)
        (cond ((< (- (apply max (map car window-polygon-list))
                     (apply min (map car window-polygon-list)))
                  1.5)   ; vertical
               (list (apply min (map car window-polygon-list))
                     (apply min (map cadr window-polygon-list))
                     (apply min (map car window-polygon-list))
                     (apply max (map cadr window-polygon-list))))
              ((< (- (apply max (map cadr window-polygon-list))
                     (apply min (map cadr window-polygon-list)))
                  1.5)   ; horizontal
               (list (apply min (map car window-polygon-list))
                     (apply min (map cadr window-polygon-list))
                     (apply max (map car window-polygon-list))
                     (apply min (map cadr window-polygon-list))))
              (else false)))
      
      ; project-and-draw-polygon -runko --------------------------------
      (let ((window-polygon-list (project-to-window-coords
                                  (polygon-get-vectors polygon)
                                  (polygon-get-unique-maze-corner-indexes polygon))))
        (xo 'send 'SetForeground (polygon-get-color-string polygon))
        ; Piste, viiva vai monikulmio?
        (if (null? (cdr window-polygon-list))
            ; Piste
            (xo 'send
                'FillRectangle
                (- (caar window-polygon-list) 3)
                (- (cadar window-polygon-list) 3)
                7
                7)
            ; Viiva/monikulmio? (Jos h-phase < 0.5, ei tarkisteta viivavaihtoehtoa)
            (if (>= ((camera 'get-h-phase)) 0.5)
                (let ((line-conversion (check-and-convert-if-line window-polygon-list)))
                  (if line-conversion
                      (apply xo
                             (append (list 'send
                                           'DrawLine)
                                     line-conversion))
                      (xo 'send
                          'FillPolygon
                          window-polygon-list
                          'Convex
                          'Origin)))
                (xo 'send
                    'FillPolygon
                    window-polygon-list
                    'Convex
                    'Origin)))))
    
    
    ; Piirt‰‰ draw-stackista jokaisen monikulmion
    (define (draw-loop direct-stack)
      ;(display (list 'direct-stack direct-stack)) (newline)
      (if (not (null? direct-stack))
          (begin
            (project-and-draw-polygon (car direct-stack))
            (draw-loop (cdr direct-stack)))))
    
    ;(display 'execute-draw-stack) (newline)
    (draw-loop ((draw-stack 'get-direct-ref)))))




;;; Luo annetun kameran n‰kym‰‰n muuntavan projektiomatriisin.
;;; (matriisi sis‰lt‰‰ skaalauksen ikkunan kokoon n‰hden)
(define (make-projection-matrix-for-camera camera)
  ; Luo ikkunan kokoa vastaavan skaalausmatriisin.
  ;   ( x-scale   0      0  )
  ;   (    0   y-scale   0  )
  ;   (    0      0      1  )
  (define (make-scale-matrix camera)
    (let ((x-scale (* zoom-factor (car ((camera 'get-window-size-x.y)))))
          (y-scale (* zoom-factor (cdr ((camera 'get-window-size-x.y)))))
          (scale-matrix (make-matrix 3 3)))
      (matrix-set-cell! scale-matrix 0 0 x-scale)
      (matrix-set-cell! scale-matrix 1 1 y-scale)
      (matrix-set-cell! scale-matrix 2 2 1.0)
      scale-matrix))

  ; field-of-view fiksattu 53.13 asteeksi.
  ; ikkunan keskipisteen koordinaatti = ahead-vec
  ; projektiokeskus c = (0 0 0)
  ; ex = -right-vec   (eli k‰‰nt‰‰ y:n ymp‰ri,
  ;                    ikkunan alasp‰in kasvavaa y:t‰ varten)
  ; ey = up-vec
  ; r0 = ikkunan vas. yl‰kulma = ahead-vec + 0.5ex + 0.5ey
  ; tason normaali n = -(ahead-vec)
  (define (make-unscaling-projection-matrix-from-cam-3d-vecs cam-3d-vecs)
    (let* ((c (vector 0 0 0))
           (ex (sub 0 (cam-3d-vecs 'right-vec)))
           (ey (cam-3d-vecs 'up-vec))
           (r0 (add (add (cam-3d-vecs 'ahead-vec)
                         (mul 0.5 ex))
                    (mul 0.5 ey)))
           (n (sub 0 (cam-3d-vecs 'ahead-vec))))
      ;(display (list 'make-matrix: 'c c 'ex ex 'ey ey 'r0 r0 'n n)) (newline)
      (make-projection-matrix c r0 ex ey n)))
  
  (mul (make-scale-matrix camera)
       (make-unscaling-projection-matrix-from-cam-3d-vecs (camera '3d-vectors))))
