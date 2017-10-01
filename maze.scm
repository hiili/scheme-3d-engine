;;; maze.scm


;;; maze-olio
;;;   Sisältää:
;;;     -labyrintin kartan oliona (maze-map)
;;;     -metodeja, jotka käsittelevät labyrinttiä kokonaisuutena
(define (new-maze)
  (load "lights.scm")
  (load "maze_maze-map.scm")
  (load "maze_generator.scm")
  (load "maze_maze-solver.scm")
  
  (let ((maze-map '())
        (dynamic-lights-manager false)
        (solve-route-marked? false))
    
    ; Apufunktiot ------------------------------------------------
    (define (mark-exit!)
      ((maze-map 'add-light-source!) (make-light-source-type 'solve-route-light
                                                             exit-light-affection-distance
                                                             (generate-randomly-flickering-lights exit-light))
                                     (cell-index-x.y->loc-vector ((maze-map 'get-loc) 'exit)
                                                                 exit-light-altitude)
                                     'exit))
    (define (enlight-route! route)
      (define (enlight-route-with-cycling-light rest-route current-phase flash-lights)
        (if (not (null? rest-route))
            (begin
              ; Valaistaan ensimmäinen huone
              ((maze-map 'add-light-source!) (make-light-source-type 'solve-route-light
                                                                     solve-route-lights-affection-distance
                                                                     (rotate-vector flash-lights
                                                                                    current-phase))
                                             (cell-index-x.y->loc-vector (car rest-route)
                                                                         solve-route-lights-altitude)
                                             'dummy)
              ; Loput huoneet
              (enlight-route-with-cycling-light (cdr rest-route)
                                                (modulo (+ current-phase 1)
                                                        color-cycle-steps)
                                                flash-lights))))
      (enlight-route-with-cycling-light route
                                        0
                                        (generate-flashing-lights solve-route-light)))
    
    ; Metodit ----------------------------------------------------
    (define (generate! width height)
      (set! maze-map (generator width height))
      (set! dynamic-lights-manager (new-dynamic-lights-manager maze-map))
      (mark-exit!))
    
    (define (load-from-current!)
      (let ((size-list (read)))
        (begin
          (set! maze-map
                (new-maze-map (car size-list)
                              (cadr size-list)
                              false))
          ((maze-map 'load-from-current!)))))
    
    (define (save-to-current)
      (if (null? maze-map) (error "Tried to save an empty maze! - maze.save-to-current")
          (begin
            (write ((maze-map 'get-size-list)))
            (newline)
            ((maze-map 'save-to-current)))))
    
    ; Tämä rajapinta annetaan 3d-visualizerille
    (define (vis-interface method . args)
      
      (define size (maze-map 'get-size-list))
      
      (define (exit-loc) ((maze-map 'get-loc) 'exit))
      
      (define (wall x y dir)
        ((maze-map 'wall-at-to?) (- x 1) (- y 1) dir))
      
      (define (mark-solve-route! start-loc-x.y)
        (let ((solve-route (maze-solver maze-map
                                        start-loc-x.y
                                        ((maze-map 'get-loc) 'exit))))
          ;(display (list 'solve-route: solve-route)) (newline)
          (set! solve-route-marked? true)
          (enlight-route! solve-route)))
      (define (erase-solve-route!)
        ((maze-map 'erase-solve-route-lights!))
        (set! solve-route-marked? false)
        (mark-exit!))
      ;(define (get-dynamic-lights-list)
      ;  '())
      
      ; vis-interface dispatch
      (cond ((eq? method 'size) (size))
            ((eq? method 'wall) (apply wall args))
            ((eq? method 'exit-loc) (exit-loc))
            ((eq? method 'mark-solve-route!) (apply mark-solve-route! args))
            ((eq? method 'erase-solve-route!) (apply erase-solve-route! args))
            ((eq? method 'solve-route-marked?) solve-route-marked?)
            ((eq? method 'get-cell-solve-route-lightning) (apply (maze-map 'get-cell-solve-route-lightning)
                                                                 args))
            ((eq? method 'get-cell-dynamic-lights-lightning) (apply (maze-map 'get-cell-dynamic-lights-lightning)
                                                                    args))
            ((eq? method 'dynamic-lights-manager) dynamic-lights-manager)
            ;((eq? method 'get-dynamic-lights-list) (apply get-dynamic-lights-list args))
            (else (error "Unknown method! maze.vis-interface." method))))
    
    
    ; Dispatch ---------------------------------------------------
    (define (dispatch method)
      (cond ((eq? method 'generate!) generate!)
            ((eq? method 'load-from-current!) load-from-current!)
            ((eq? method 'save-to-current) save-to-current)
            ((eq? method 'vis-interface) vis-interface)
            ((eq? method 'maze-map) maze-map)
            (else (error "Unknown method! maze." method))))
    dispatch))
