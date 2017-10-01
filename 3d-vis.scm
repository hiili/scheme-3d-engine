;;; 3d-vis.scm


(load "3d-vis_draw-stack.scm")
(load "3d-vis_fill-draw-stack.scm")
(load "3d-vis_execute-draw-stack.scm")

;;; 3d-vis -funktio, piirt�� n�kym�n.
;;;   -Luodaan uusi piirtopino-olio (lifo-pino)
;;;   -L�tkit��n seini� ja lattiamerkint�j� pinoon
;;;   -Piirret��n pinon sis�lt�
;;; draw-mode = 'all / 'static-elements / 'dynamic-elements
(define (3d-vis xo camera player maze-interface draw-mode)
  (let ((draw-stack (new-draw-stack camera maze-interface)))
    
    ;;; Piirret��n tausta (maa ja taivas)
    ;   horisontti: (y-size / 2) * (1 - raise-amount)
    (define (draw-background)
      (let ((horizon-y (let ((raise-amount (/ ((camera '3d-vectors) 'down-pitch)
                                              (/ field-of-view
                                                 2))))
                         (if (> raise-amount 1)
                             0
                             (* (- 1 raise-amount)
                                (/ (cdr ((camera 'get-window-size-x.y)))
                                   2))))))
        ; Taivas
        (xo 'send 'SetForeground bg-color-string)
        (xo 'send
            'FillRectangle
            0
            0
            (car ((camera 'get-window-size-x.y)))
            horizon-y)
        ; Maa
        (xo 'send 'SetForeground floor-color-string)
        (xo 'send
            'FillRectangle
            0
            horizon-y
            (car ((camera 'get-window-size-x.y)))
            (cdr ((camera 'get-window-size-x.y))))))
    
    ;;; Piirret��n 1-v�rinen tausta (bg-color)
    (define (draw-1-color-background)
      (xo 'send 'SetForeground bg-color-string)
      (xo 'send
          'FillRectangle
          0
          0
          (car ((camera 'get-window-size-x.y)))
          (cdr ((camera 'get-window-size-x.y)))))
    
    ;;; Piirret��n n�kym�
    (define (draw-scene draw-mode)
      ; Odotetaan, kunnes xdraw on piirt�nyt edellisen kuvan valmiiksi
      ;   (muuten kuvan p�ivitys ja n�pp�insy�te menee ep�synkkaan isolla ikkunalla)
      (define (wait-until-previous-frame-is-finished)
        (xo 'send-with-result 'getxdrawversion))
      
      ;(display "fill-draw-stack:        ")
      ;(time
      (fill-draw-stack draw-stack camera player maze-interface draw-mode)
      ;)
      ;(display "execute-draw-stack:        ")
      ;(time
      ;(wait-until-previous-frame-is-finished)   ; eip� tunnu auttavan :(
      (execute-draw-stack draw-stack xo camera)
      ;)
      )
    
    
    ;;; 3d-vis -runko:
    (if (or (eq? draw-mode 'all)
            (eq? draw-mode 'static-elements))
        (draw-1-color-background))
    (draw-scene draw-mode)))
