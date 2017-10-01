;;; main.scm


;;; Laaja harjoitustyˆ: Labyrintti ja 3d-visualisointi
;;;   Paul Wagner
;;;
;;; Lyhyt selostus ohjelman toiminnasta lˆytyy readme.text -tiedostosta.


;; Yhteensopivuuskorjauksia mzscheme v200:aa varten
(load "xdraw-mzscheme-r200.scm")
(define true #t)
(define false #f)


(load "scheme-extensions.scm")
(load "math.scm")
(load "lights.scm")
(load "locator.scm")


; N‰pp‰insidonnat
(define key-bindings
  '(("a"       step-left           sticky)
    ("d"       step-right          sticky)
    ("w"       step-forward        sticky)
    ("s"       step-backward       sticky)
    ("q"       turn-left           sticky)
    ("e"       turn-right          sticky)
    ("i"       toggle-true-inertia non-sticky)
    ("1"       throw-torch-1       non-sticky)
    ("2"       throw-torch-2       non-sticky)
    ("3"       throw-torch-3       non-sticky)
    ("4"       throw-torch-4       non-sticky)
    ("l"       toggle-solve-route  non-sticky)
    ("p"       update-backgrounds  non-sticky)
    ("v"       switch-player-light non-sticky)
    ("space"   switch-viewmode     non-sticky)
    ("Escape"  exit                non-sticky)))
; Kuva
(define zoom-factor 1.0)   ; Ei toimi, ‰l‰ muuta! ; 2.0 -> fov = 60'
(define max-fps 30)   ; (12fps on turvallinen) ettei x puuroudu sein‰‰ tuijottaessa..
(define initial-viewmode '2d)   ; '3d / '2d
;(define initial-window-size-string "1024x576+0+0")
(define initial-window-size-string "1280x720+0+0")
;(define initial-window-size-string "1920x1080+0+0")
;(define initial-window-size-string "1260x708+0+140")   ; Niksulan O2 -koneissa about keskell‰ n‰yttˆ‰.
                                                        ; T‰t‰ suuremmilla ikkunoilla kuvan p‰ivitykseen
                                                        ; alkaa kasautua viivett‰, ellei fps <= 12.
;(define initial-window-size-string "1270x987+0+0")   ; Niksula O2 -koneissa about fullscreen,
                                                      ; edellytt‰‰ ett‰ fps <= 12! (muuten xdraw puuroutuu..)
; Soihdut
(define torch-1-light-color (vector 0.1 0.8 0.1))
(define torch-2-light-color (vector 0.8 0.1 0.1))
(define torch-3-light-color (vector 0.1 0.8 0.8))
(define torch-4-light-color (vector -0.15 -0.15 -0.15))
(define torch-throw-speed 0.12)
(define torch-vertical-velocity 0.1)
(define torch-light-affection-distance 5)
(define dynamic-lights-effective-distance-factor 18.0)
(define torch-light-switching-steps 3)
(define torch-kinetic-attributes
  (make-kinetic-attributes 0.99   ; move-deceleration-factor
                           0.001  ;                  -delta
                           0.8    ; bounce-deceleration-factor
                           0.0  ;                    -delta
                           0.0    ; gravity-acceleration-delta
                           +inf.0))   ; max-acceleration-delta
; V‰rit ja valot
(define color-cycle-steps 42)   ; T‰t‰ pienennet‰‰n yli 10x10 -labyrinteiss‰
(define color-max-intensity 1.0)
(define color-mode 'normal)   ; Kysyt‰‰n k‰ytt‰j‰lt‰ mainissa ja muutetaan
(define color-quantization-levels 256)   ; Asetetaan mainissa uudelleen v‰ritilan valinnasta riippuen
(define bg-color (vector 0.0 0.0 0.0))
(define wall-color (vector 1.0 1.0 1.0))
(define floor-color (vector 0.0 0.1 0.6))
;(define player-color (vector 0.0 0.4 1.0))
(define player-color (vector 1 1 1))
(define player-light-color (vector 0.5 0.5 0.4))
(define player-light-effective-distance-factor 3.0)
(define player-light-switching-steps 5)
(define player-light-changing-speed-curve-exp 1)
(define cell-ambient-lightning-when-no-solve-route
  (vector (mul 0.3 (vector 0.2 0.2 0.2))   ; lattia
          (mul 0.3 (vector 0.15 0.15 0.15))   ; it‰sein‰
          (mul 0.3 (vector 0.05 0.05 0.05))   ; pohjois-
          (mul 0.3 (vector 0.1 0.1 0.1))   ; l‰nsi-
          (mul 0.3 (vector 0.2 0.2 0.2)))) ; etel‰-
(define cell-ambient-lightning-when-solve-route-marked
  (vector-map (lambda (light)
                (mul 0.75 light))
              cell-ambient-lightning-when-no-solve-route))
(define lights-flicker-amount 0.2)
(define reflected-light-magnitude-factor 0)
(define light-ambient-diffusion 0.33)
(define lights-switching-steps 10)
(define exit-light (vector 0.0 1.0 0.2))
(define exit-light-altitude 0.5)
(define exit-light-affection-distance 5)
(define solve-route-light (vector 0.0 0.6 0.2))
(define solve-route-lights-altitude 0.4)
(define solve-route-lights-affection-distance 3)
(define solve-route-lights-effective-distance-factor 3.0)
(define visit-marker-lights-affection-distance 1)
(define visit-marker-lights-initial-altitude 0.2)
(define visit-marker-lights-switching-steps 1)
(define first-visit-marker-light-color (vector 0 0.05 0))
(define second-visit-marker-light-color (vector 0 0 0.05))
; Siirtym‰t
;(define h-transition-view-distance 8)
(define h-transition-view-distance 7)   ; ei ala heti tahmata soihtuja heitelless‰
; Liikkuminen
(define walking-altitude 0.3)
(define move-acceleration-delta 0.02)
(define turn-acceleration-delta 0.025)
(define move-deceleration-factor 0.8)   ; turn deceleration factor, actually
(define bounce-vertical-acceleration-delta 0.2)
(define initial-true-inertia? false)
(define player-normal-kinetic-attributes
  (make-kinetic-attributes 0.9   ; move-deceleration-factor
                           0.001   ; -delta
                           0.95   ; bounce-deceleration-factor
                           0.0  ; -delta
                           0.0   ; gravity-acceleration-delta
                           +inf.0))   ; max-acceleration-delta
(define player-true-inertia-kinetic-attributes
  (make-kinetic-attributes 0.99  ;player-true-inertia-deceleration-factor
                           0.0   ;player-true-inertia-deceleration-delta
                           0.8   ;player-true-inertia-bounce-deceleration-factor
                           0.01  ; -delta
                           0.0   ;gravity-acceleration-delta
                           move-acceleration-delta))   ; max-acceleration-delta
; Yleist‰
(define wall-thickness/2 0.25)
(define gravity-acceleration-delta 0.01)   ; globaali painovoima, kinetic-attributes
                                            ; -painovoimakiihtyvyys ei ole k‰ytˆss‰
(define floor-altitude 0.3)
(define projection-vanishing-plane-guard-distance-factor 0.1)


;-----------------------------------------------------
(define global-brightness-factor 1)
(define true-inertia? initial-true-inertia?)
(define 2d<->3d-transition-average-step-length false)   ; Lasketaan kun labyrintin koko on tiedossa
(define solved? false)

(define bg-color-string (color->color-string bg-color))
(define wall-color-string (color->color-string wall-color))
(define floor-color-string (color->color-string floor-color))
(define player-color-string (color->color-string player-color))

(load "ticker.scm")
(load "enws-dirs.scm")
(load "maze.scm")
(load "camera.scm")
(load "player.scm")
(load "3d-vis.scm")
(load "user-interface.scm")


(define visit-marker-light-kinetic-attributes
  (make-kinetic-attributes 0 0 0 0 0 0))


(define (display-startup-message!)
  ; N‰pp‰inkomennot:
  (display "Voimassa olevat n‰pp‰insidonnat:") (newline)
  (for-each (lambda (binding)
              (display (string-append "    "
                                      (car binding)
                                      " = "
                                      (symbol->string (cadr binding))))
              (newline))
            key-bindings)
  (newline)
  ; Loppul‰p‰t:
  (display "Hieman infoa ohjelman k‰ytˆst‰:") (newline)
  (display "  Ratkaisureitin n‰yttˆ/piilotus ja ikkunan koon muuttaminen aiheuttavat") (newline)
  (display "  2d-moodiin ment‰ess‰ / 2d-moodissa oltaessa taustojen uudelleenlaskennan,") (newline)
  (display "  mik‰ voi kest‰‰ kohtalaisen kauan!") (newline)
  (newline)
  (display "  Mik‰li 2d-moodi alkaa tahmata pahasti, kannattaa taustat p‰ivitt‰‰") (newline)
  (display "  uudelleen. (update-backgrounds -nappi)") (newline)
  (newline)
  (display "  Kaikki ylim‰‰r‰iset ohjelmat kannattaa sulkea taustalta pois, muuten xdraw") (newline)
  (display "  saattaa alkaa itkem‰‰n v‰rien loppumisesta. Jos v‰rit silti loppuvat (sein‰t") (newline)
  (display "  alkavat v‰l‰htelem‰‰n mustana), auttaa main.scm -tiedostosta lˆytyv‰n") (newline)
  (display "  color-quantization-levels -vakion arvon pienent‰minen.") (newline)
  (newline)
  (display "(Yli 30x30 -labyrinttien taustojen piirto kest‰‰ aika tolkuttoman kauan, en suosittele..)") (newline))


(define (main)
  (let ((maze-width 0)
        (maze-height 0)
        (maze (new-maze))
        (ticker (new-ticker)))
    
    ; Kerrotaan k‰ytt‰j‰lle sit‰ sun t‰t‰
    (display-startup-message!)
    
    ; Kysyt‰‰n v‰rimoodi
    ;   ( 'normal / 'blue-gray / 'cyan-gray / 'green-blue-gray / 'grayscale )
    (display "V‰ritilan valinta:") (newline)
    (display "    a) normal   (t‰ydet v‰rit)") (newline)
    (display "    b) cyan-gray   (vanhoja koneita varten: v‰h‰n v‰rej‰, sopii soihtujen heittelyyn)") (newline)
    (display "    c) green-blue-gray   (vanhoja koneita varten: enemm‰n v‰rej‰, karkea porrastus)") (newline)
    (display "  Anna haluamasi v‰ritilan kirjain: ")
    (set! color-mode
      (let ((input (read)))
        (case input
          ((a) 'normal)
          ((b) 'cyan-gray)
          ((c) 'green-blue-gray)
          (else (error "Virheellinen valinta!")))))
    ;; V‰rien kvantisointia ei ilmeisesti tarvita en‰‰ uudemmilla X-toteutuksilla.
    ;; (set! color-quantization-levels
    ;;   (case color-mode
    ;;     ((normal) 10)
    ;;     ((green-blue-gray) 14)   ; 16
    ;;     ((blue-gray) 26)   ; 24 on suht turvallinen
    ;;     ((cyan-gray) 26)   ; 24 on suht turvallinen
    ;;     ((grayscale) 32)))
    
    ; Kysyt‰‰n labyrintin mitat
    (display "Anna labyrintin leveys: ")
    (let ((input (read)))
      (if (not (integer? input))
          (error "Virheellinen syˆte: " input)
          (set! maze-width (inexact->exact input))))
    (display "Anna labyrintin korkeus: ")
    (let ((input (read)))
      (if (not (integer? input))
          (error "Virheellinen syˆte: " input)
          (set! maze-height (inexact->exact input))))
    
    ; Tarkistetaan mittojen j‰rkevyys ja k‰ynnistet‰‰n systeemi
    (if (or (< maze-width 2)
            (< maze-height 2))
        (error "Labyrintin leveyden ja korkeuden tulee olla >= 2!")
        (begin
          ; Mitat ok, initialisoidaan olioita
          (set! 2d<->3d-transition-average-step-length
                (max (/ (expt (* maze-width maze-height)
                              1)
                        10000)
                     0.3))
          ;; T‰t‰ tuskin tarvitaan en‰‰ uudemmilla X-toteutuksilla.
          ;; (if (> (* maze-width maze-height) 100)
          ;;     (set! color-cycle-steps
          ;;           (max (inexact->exact (round (* color-cycle-steps
          ;;                                          (/ (expt 100 0.25)
          ;;                                             (expt (* maze-width maze-height)
          ;;                                                   0.25)))))
          ;;                6)))
          ((maze 'generate!) maze-width maze-height)
          (let* ((player (new-player (new-locator (maze 'vis-interface)
                                                  ticker
                                                  (vector 0 0 0)   ; location
                                                  (vector 0 0 0)   ; velocity
                                                  (if initial-true-inertia?
                                                      player-true-inertia-kinetic-attributes
                                                      player-normal-kinetic-attributes))
                                     (maze 'vis-interface)
                                     ticker))
                 (camera (new-camera player
                                     (maze 'vis-interface)
                                     ticker)))
            (let ((start-loc-x.y (((maze 'maze-map) 'get-loc) 'start)))
              ; Siirret‰‰n pelaaja alkupisteeseen
              (((player 'locator) 'set-loc!) (vector (+ (car start-loc-x.y) 0.5)
                                                     (+ (cdr start-loc-x.y) 0.5)
                                                     walking-altitude))
              ; K‰‰nnet‰‰n katsomaan sellaiseen suuntaan jossa ei ole sein‰‰ edess‰
              ((player 'set-dir!) (cadr (assq false
                                              (map (lambda (enws-dir&rad-dir)
                                                     (list (((maze 'maze-map) 'wall-at-to?) (car start-loc-x.y)
                                                                                            (cdr start-loc-x.y)
                                                                                            (car enws-dir&rad-dir))
                                                           (cadr enws-dir&rad-dir)))
                                                   (list (list 'e 0)
                                                         (list 'n pi/2)
                                                         (list 'w pi)
                                                         (list 's 3/2pi)))))))
            ; Ja homma k‰yntiin
            (user-interface (maze 'vis-interface) player camera ticker))))))


(define run main)
