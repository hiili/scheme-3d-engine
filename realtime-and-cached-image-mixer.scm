;;; realtime-and-cached-image-mixer.scm


;;; Hoitaa valmiiksipiirrettyjen taustojen ja liikkuvien kohteiden yhdistelyn.
;;; Metodilla 'update-window! olio päivittää koko ruudun sisällön käyttäen
;;; valmiita taustoja ja/tai 3d-vis -funktion piirtämiä liikkuvia kohteita.
;;; Taustat piirretään myös 3d-vis -funktiota käyttäen.
(define (new-realtime-and-cached-image-mixer xo camera player maze-interface)
  (let ((cached-images-valid? false)
        (pixmaps-size-x.y false)
        ; lista, joka sis: (list h-phase pixmap-name) -alkioita,
        ; h-phasen mukaan kasvavassa järjestyksessä.
        (linear-transition-pixmap-names '())
        (recent-all-draw-times '(0 0 0)))
    
    ;;; Apufunktiot
    (define (add-time-to-recent-draw-times new-time)
      (set! recent-all-draw-times
            (list new-time
                  (car recent-all-draw-times)
                  (cadr recent-all-draw-times))))
    ; Laskee viivekertoimen, jolla häivytetään h-phase = 0.5 -kohdassa tulevaa piirtonopeuseroa.
    (define (calculate-current-delay-factor h-phase)
      (dynamic-bezier (* 2 (- h-phase 0.5))
                      1 1 0 0 0))
    
    ; Päivitetään tulos näkyviin
    (define (refresh-window!)
      (xo 'send
          'RedrawWindowArea
          0
          0
          (car ((camera 'get-window-size-x.y)))
          (cdr ((camera 'get-window-size-x.y))))
      (xo 'flush-command-buffer))
    
    ; Tuhotaan vanhat ja piirretään uudet taustat. Ilmoitetaan asiasta myös näytöllä.
    (define (redraw-cached-images!)
      (let ((window-width (car ((camera 'get-window-size-x.y))))
            (window-height (cdr ((camera 'get-window-size-x.y))))
            (camera-state-snapshot ((camera 'get-state-snapshot)))
            (linear-phase-steps ((camera 'get-linear-phase-steps))))
        
        ; Piirretään tumma tausta valmiiksi
        (define (init-dark-bg)
          (xo 'send 'CreatePixmap "dark-bg" window-width window-height)
          (xo 'send 'SetDrawTargetToPixmap "dark-bg")
          (set! global-brightness-factor 0.5)
          (3d-vis xo camera player maze-interface 'all)
          (set! global-brightness-factor 1)
          (xo 'send 'SetDrawTargetToWindow))
        (define (free-dark-bg)
          (xo 'send 'FreePixmap "dark-bg"))
        ; Piirtää tumman taustakuvan päälle vaiheesta kertovan tekstin
        (define (draw-phase-info-text op1-phase op1-max-phase op2-phase op2-max-phase)
          (define (generate-info-string)
            (string-append "ESIPIIRRETÄÄN TAUSTOJA:   VAIHE 1: "
                           (number->string op1-phase)
                           "/"
                           (number->string op1-max-phase)
                           "   VAIHE 2: "
                           (number->string op2-phase)
                           "/"
                           (number->string op2-max-phase)))
          (define (draw-text)
            (let ((info-string (generate-info-string)))
              ; Musta varjostus
              (xo 'send 'SetForeground "black")
              (xo 'send
                  'DrawJustifiedString
                  (+ (/ window-width 2) 2)
                  (+ (/ window-height 2) 2)
                  'Center 'Baseline 'Normal
                  info-string)
              ; Valkoinen teksti
              (xo 'send 'SetForeground "white")
              (xo 'send
                  'DrawJustifiedString
                  (/ window-width 2)
                  (/ window-height 2)
                  'Center 'Baseline 'Normal
                  info-string)))
          ; draw-phase-info-text -runko
          (xo 'send 'SetDrawTargetToWindow)
          (xo 'send 'CopyAreaFromPixmap
              "dark-bg"
              0 0
              window-width window-height
              0 0)
          (draw-text)
          (refresh-window!))
        
        ; Palauttaa listan, joka sis: (list h-phase pixmap-name) -alkioita.
        (define (draw-rest-linear-transition-images! step-counter)
          (let* ((h-phase ((camera 'get-h-phase)))
                 (pixmap-name (string-append "linear-phase:"
                                             (number->string h-phase))))
            (draw-phase-info-text step-counter
                                  (inexact->exact (ceiling linear-phase-steps))
                                  0
                                  color-cycle-steps)
            (xo 'send
                'CreatePixmap
                pixmap-name
                window-width
                window-height)
            (xo 'send 'SetDrawTargetToPixmap pixmap-name)
            (3d-vis xo camera player maze-interface 'static-elements)
            ((camera 'manual-tick!))
            ; Paluuarvo ja iteraatio
            (cons (list h-phase pixmap-name)
                  (if (= ((camera 'get-h-phase)) 1)
                      '()
                      (draw-rest-linear-transition-images! (+ step-counter 1))))))
        (define (draw-rest-2d-images! stop-phase)
          (let ((pixmap-name (string-append "2d-phase:"
                                            (number->string ((camera 'get-color-cycle-phase))))))
            (draw-phase-info-text (inexact->exact (ceiling linear-phase-steps))
                                  (inexact->exact (ceiling linear-phase-steps))
                                  (modulo (- ((camera 'get-color-cycle-phase))
                                             stop-phase)
                                          color-cycle-steps)
                                  color-cycle-steps)
            (xo 'send
                'CreatePixmap
                pixmap-name
                window-width
                window-height)
            (xo 'send 'SetDrawTargetToPixmap pixmap-name)
            (3d-vis xo camera player maze-interface 'static-elements)
            ((camera 'manual-tick!))
            (if (not (= ((camera 'get-color-cycle-phase))
                        stop-phase))
                (draw-rest-2d-images! stop-phase))))
        
        ;;; Runko:
        ; Vapautetaan entiset taustat ja valmistellaan tumma tausta
        (xo 'send 'FreePixmaps)
        (init-dark-bg)
        ; Valmistellaan kameraa
        ((camera 'set-h-phase-state!) 0.5 '2d)
        ((camera 'set-color-cycle-phase!) 0)
        ; Piirretään linear-transition -vaiheen kuvat
        (set! linear-transition-pixmap-names
              (draw-rest-linear-transition-images! 0))
        ; Piirretään 2d -asennon kaikki color-cycle -vaiheet
        (draw-rest-2d-images! ((camera 'get-color-cycle-phase)))
        ; Loppusiivous
        ((camera 'revert-state-to-snapshot!) camera-state-snapshot)
        (xo 'send 'SetDrawTargetToWindow)
        (set! cached-images-valid? true)
        (set! pixmaps-size-x.y (cons window-width
                                     window-height))
        (free-dark-bg)))

    (define (copy-cached-image-to-current h-phase color-cycle-phase)
      ; Etsii linear-transition-pixmap-names -listasta h-phasea lähinnä olevan kuvan
      (define (find-nearest-linear-phase-image-name rest-names h-phase)
        (define (h-phase&name->h-phase x) (car x))
        (define (h-phase&name->name x) (cadr x))
        (cond
         ; Viimeinen kuva?
         ((null? (cdr rest-names))
          (h-phase&name->name (car rest-names)))
         ; Ensimmäinen kuva lähempänä kuin seuraava? Palautetaan ensimmäinen.
         ((< (abs (- h-phase
                     (h-phase&name->h-phase (car rest-names))))
             (abs (- h-phase
                     (h-phase&name->h-phase (cadr rest-names)))))
          (h-phase&name->name (car rest-names)))
         ; Palautetaan joku seuraavista
         (else (find-nearest-linear-phase-image-name (cdr rest-names)
                                                     h-phase))))
      ; copy-cached-image-to-current -runko
      (xo 'send
          'CopyAreaFromPixmap
          (if (< h-phase 1)
              ; linear-transition meneillään
              (find-nearest-linear-phase-image-name linear-transition-pixmap-names
                                                    h-phase)
              ; 2d-moodissa
              (string-append "2d-phase:"
                             (number->string color-cycle-phase)))
          0 0
          (car pixmaps-size-x.y)
          (cdr pixmaps-size-x.y)
          0 0))
    
    (define (huono-loppuhehkutus)
      (define (loop)
        (3d-vis xo camera player maze-interface 'all)
        (refresh-window!)
        (set! global-brightness-factor
              (- global-brightness-factor 0.0075))
        (set! projection-vanishing-plane-guard-distance-factor
              (+ projection-vanishing-plane-guard-distance-factor
                 0.03))
        (if (positive? global-brightness-factor)
            (loop)
            (begin
              (xo 'exit)
              (error ""))))
      ((camera 'set-h-phase-state!) 0 '3d)
      (let ((player-loc-x (car (((player 'locator) 'get-loc-as-cell-index-x.y))))
            (player-loc-y (cdr (((player 'locator) 'get-loc-as-cell-index-x.y))))
            (maze-center-x (/ (car (maze-interface 'size)) 2))
            (maze-center-y (/ (cadr (maze-interface 'size)) 2)))
        ((player 'set-dir!) (cond ((and (< player-loc-x maze-center-x)
                                        (< player-loc-y maze-center-y))
                                   ; Vas alakulma
                                   (* pi/2 0.5))
                                  ((and (< player-loc-x maze-center-x)
                                        (> player-loc-y maze-center-y))
                                   ; Vas yläkulma
                                   (* pi/2 3.5))
                                  ((and (> player-loc-x maze-center-x)
                                        (> player-loc-y maze-center-y))
                                   ; Oik yläkulma
                                   (* pi/2 2.5))
                                  ((and (> player-loc-x maze-center-x)
                                        (< player-loc-y maze-center-y))
                                   ; Oik alakulma
                                   (* pi/2 1.5)))))
      ((camera 'manual-tick!))
      (loop))
    
    ;;; Metodit
    (define (update-window!)
      (if solved? (huono-loppuhehkutus))
      (let ((h-phase ((camera 'get-h-phase))))
        (if (< h-phase 0.5)
            ; Piirretään kaikki livenä
            (add-time-to-recent-draw-times (measure-time 3d-vis xo camera player maze-interface 'all))
            ; Osa kuvasta tulee valmiiksipiirretyistä taustoista. Homma on paljon nopeampi kuin
            ; koko kuvan piirto uudestaan, joten välillä 0.5 <= h-phase < 1 lisätään piirtoon viive
            ; nopeuseron häivyttämiseksi.
            (begin
              (if (not cached-images-valid?)
                  (redraw-cached-images!))
              (let* ((draw-time
                      (measure-time (lambda ()
                                      (copy-cached-image-to-current h-phase
                                                                    ((camera 'get-color-cycle-phase)))
                                      (3d-vis xo camera player maze-interface 'dynamic-elements))))
                     (required-delay (* (- (apply average recent-all-draw-times)
                                           draw-time)
                                        (calculate-current-delay-factor h-phase))))
                (if (> required-delay 0)
                    (sleep (/ required-delay 1000))))))
        (refresh-window!)))
    (define (update-cached-images!)
      ;(redraw-cached-images!))
      (set! cached-images-valid? false))
    
    ;;; Dispatch
    (define (dispatch method)
      (cond ((eq? method 'update-window!) update-window!)
            ((eq? method 'update-cached-images!) update-cached-images!)
            (else (error "Unknown method! realtime-and-cached-image-mixer." method))))
    dispatch))

