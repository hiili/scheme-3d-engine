;;; user-interface.scm
;;;   Hoitaa kommunikoinnin k�ytt�j�n kanssa,
;;;   eli hoitaa ikkunaa ja n�pp�imist��


;;; Avaa ikkunan, k�sittelee inputtia
;;; silmukassa ja py�ritt�� koko roskaa, lopulta
;;; sulkee ikkunan ja palaa.
(define (user-interface maze-interface player camera ticker)
  (load "realtime-and-cached-image-mixer.scm")
  
  (let* ((xo (new-xdraw "-geometry" initial-window-size-string))
         (image-mixer (new-realtime-and-cached-image-mixer xo
                                                           camera
                                                           player
                                                           maze-interface))
         ; Luodaan parilista, joka sis�lt�� jokaisen k�ytetyn n�pp�imen
         ; tilan listana muodossa ( <key-cmd-id> . <key-cmd-state> )
         (keys (map (lambda (key-id)
                      (cons key-id false))
                    (map cadr key-bindings))))
    
    
    ;;; ==================================================
    ;;;    user-interface -apufunktiot                    
    ;;; ==================================================
    (define (event-id e) (cadr e))
    (define (event-params e) (cddr e))
    (define (resize-event-width e) (caddr e))
    (define (resize-event-height e) (cadddr e))
    (define (key-event-keyname e) (cadddr (cddr e)))
    ; Muuntaa x-drawin palauttaman napin nimen vastaavan sidonnan nimeksi
    (define (keyname->key-command name)
      (let ((binding (assoc name key-bindings)))
        (if binding
            (cadr binding)
            false)))
    ; Onko n�pp�inkomento m��ritelty stickyksi, eli.. joo, katso handle-key-event :)
    (define (sticky-key-cmd? key-cmd)
      (eq? 'sticky (cadr (assq key-cmd (map cdr key-bindings)))))
    ; Asettaa n�pp�inkomennon tilan, eli tiedon onko vastaava nappi pohjassa
    (define (key-cmd-set! key-cmd state)
      (set-cdr! (assq key-cmd keys)
                state))
    ; Onko sidontaa vastaava nappi pohjassa?
    (define (key-cmd-active? key-cmd)
      (cdr (assq key-cmd keys)))
    
    
    ;;; ==================================================
    ;;;    user-interfacen p��silmukka (input-loop)        
    ;;; ==================================================
    ;;;  -Tutkitaan odottavat eventit   (handle-events)
    ;;;  -Toimitaan n�pp�imist�n tilanteen mukaan   (check-keys)
    ;;;  -Piirret��n n�kym�   (draw-scene)
    ;;;  -Toistetaan, mik�li k�ytt�j� ei ole sulkemassa ohjelmaa
    (define (input-loop done?)
      
      ;;; ==================================================
      ;;;    input-loop -apufunktiot                        
      ;;; ==================================================
      
      ; K�sitell��n x-ikkunassa odottavat eventit
      (define (handle-events)
        ; handle-events -apufunktiot -------------------------------------
        ; P�ivitet��n tieto ikkunan koosta
        (define (handle-resize-event event)
          ((camera 'set-window-size-x.y!) (cons (resize-event-width event)
                                                (resize-event-height event)))
          ((image-mixer 'update-cached-images!)))
        ; P�ivitet��n eventti� vastaavan n�pp�imen tila keys-listassa
        (define (handle-key-event event press-event?)
          (let ((key-cmd (keyname->key-command (key-event-keyname event))))
            (if key-cmd
                ; Vain sticky-n�pp�imet nostetaan jo t�ss�
                ; (ennenkuin niiden mukainen toiminto on v�ltt�m�tt� suoritettu)
                (if (or (sticky-key-cmd? key-cmd)
                        press-event?)
                    (key-cmd-set! key-cmd press-event?)))))
        ; handle-events -runko -------------------------------------------
        ; Kutsutaan handle-*-event -funktioita kunnes eventtijono on tyhj�
        (let ((event (xo 'get-event)))
          (if (not (null? event))
              (let ((id (event-id event)))
                (begin
                  (cond ((eq? id 'Resize) (handle-resize-event event))
                        ((eq? id 'KeyPress) (handle-key-event event true))
                        ((eq? id 'KeyRelease) (handle-key-event event false)))
                  (handle-events))))))
      
      ; Toimitaan&liikutaan painettuna olevien n�pp�inten mukaan
      ; (l�hinn� k�skytet��n pelaajaoliota/kameraa)
      ; Nostetaan non-sticky -n�pp�imet yl�s k�sittelyn j�lkeen
      (define (check-keys)
        ; check-keys -apufunktiot ----------------------------------------
        (define (take-actions)
          ; Vasemmalle/oikealle -nappi painettuna?
          (if (and (key-cmd-active? 'step-left)
                   (key-cmd-active? 'step-right))
              ((player 'move) 'stop-left-right)
              (if (key-cmd-active? 'step-left)
                  ((player 'move) 'step-left)
                  (if (key-cmd-active? 'step-right)
                      ((player 'move) 'step-right))))
          ; Eteen/taakse -nappi painettuna?
          (if (and (key-cmd-active? 'step-forward)
                   (key-cmd-active? 'step-backward))
              ((player 'move) 'stop-forward-backward)
              (if (key-cmd-active? 'step-forward)
                  ((player 'move) 'step-forward)
                  (if (key-cmd-active? 'step-backward)
                      ((player 'move) 'step-backward))))
          ; K��nn�s vasemmalle/oikealle -nappi painettuna?
          (if (and (key-cmd-active? 'turn-left)
                   (key-cmd-active? 'turn-right))
              ((player 'move) 'stop-turning)
              (if (key-cmd-active? 'turn-left)
                  ((player 'move) 'turn-left)
                  (if (key-cmd-active? 'turn-right)
                      ((player 'move) 'turn-right))))
          ; True inertia?
          (if (key-cmd-active? 'toggle-true-inertia)
              (set! true-inertia? (not true-inertia?))
              (((player 'locator) 'set-kinetic-attributes!) (if true-inertia?
                                                                player-true-inertia-kinetic-attributes
                                                                player-normal-kinetic-attributes)))
          ; Soihdun heitto?
          (if (key-cmd-active? 'throw-torch-1)
              ((player 'throw-torch!) torch-1-light-color))
          (if (key-cmd-active? 'throw-torch-2)
              ((player 'throw-torch!) torch-2-light-color))
          (if (key-cmd-active? 'throw-torch-3)
              ((player 'throw-torch!) torch-3-light-color))
          (if (key-cmd-active? 'throw-torch-4)
              ((player 'throw-torch!) torch-4-light-color))
          ; Ratkaisureitin n�ytt�/piilotus?
          (if (key-cmd-active? 'toggle-solve-route)
              (begin
                (if (maze-interface 'solve-route-marked?)
                    (maze-interface 'erase-solve-route!)
                    (begin
                      (maze-interface 'mark-solve-route!
                                      (((player 'locator) 'get-loc-as-cell-index-x.y)))
                      ((player 'player-light-off!))))
                ((image-mixer 'update-cached-images!))))
          ; Taustojen p�ivitys?
          (if (key-cmd-active? 'update-backgrounds)
              ((image-mixer 'update-cached-images!)))
          ; Pelaajavalon sytytys/sammutus?
          (if (and (key-cmd-active? 'switch-player-light)
                   (not (maze-interface 'solve-route-marked?)))
              ((player 'switch-player-light!)))
          ; N�kym�n vaihto?
          (if (key-cmd-active? 'switch-viewmode)
              ((camera 'switch-viewmode!)))
          ; Exit?
          (if (or (key-cmd-active? 'exit)
                  (not (xo 'alive?)))
              (set! done? true)))
        ; Nostetaan non-sticky -napit yl�s
        (define (release-non-sticky-keys)
          (for-each (lambda (x)
                      (if (and (cdr x)
                               (not (sticky-key-cmd? (car x))))
                          (set-cdr! x false)))
                    keys))
        
        ; check-keys -runko ------------------------------------------------
        (take-actions)
        (release-non-sticky-keys))
      
      (define (tick!)
        ((ticker 'tick!)))
        ;((player 'move) 'move!)
        ;((camera 'move!))
        ;((light-source-manager 'move) 'move!))
      
      ;;; ==================================================
      ;;;    input-loop -runko                              
      ;;; ==================================================
      ;(let ((start-time (current-milliseconds)))
      (let ((elapsed-time
             (measure-time (lambda ()
                             (handle-events)
                             (check-keys)
                             ;(display 'tick:)
                             ;(time
                             (tick!)
                             ;)
                             ;(display 'image-mixer:)
                             ;(time
                             ((image-mixer 'update-window!))
                             ;)
                             ))))
        (if (< elapsed-time
               (/ 1000 max-fps))
            (sleep (/ (- (/ 1000 max-fps)
                         elapsed-time)
                      1000))))
      
      (if (not done?)
          (input-loop false)))
    
    
    ;;; ==================================================
    ;;;    user-interface -runko                          
    ;;; ==================================================
    ;
    ; Ikkunan ja kameran s��t��
    (xo 'send 'Reset)
    (xo 'send 'SetWindowDrawing 'off 'off)
    (xo 'send 'ReportResizeEvents 'on)
    (xo 'send 'ReportKeyEvents 'on 'on)
    (sleep 1)
    (do ((event '(true) (xo 'get-event)))
        ((null? event)))
    (let ((GetWindowSize-result (xo 'send-with-result 'GetWindowSize)))
      ((camera 'set-window-size-x.y!) (cons (cadr GetWindowSize-result)
                                            (caddr GetWindowSize-result))))
    ((camera 'follow-player!) player)
    (xo 'buffer-commands 500)
    ;(xo 'display-commands #t)
    ((image-mixer 'update-cached-images!))
    
    ; Homma k�yntiin
    ;(with-input-from-file "kartta.text" (maze 'load-from-current!))
    (input-loop false)
    
    ; Suljetaan ikkuna ja palataan
    (xo 'exit)))
