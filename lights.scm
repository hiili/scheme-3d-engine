;;; lights.scm


; Pime‰‰ huonetta kuvaava cell-lightning -rakenne
(define dark-cell-lightning
  (make-vector 5 (vector 0 0 0)))


; Valaisee v‰ri‰ valolla
(define (apply-light-to-color color light)
  (vector-map-2 * color light))


(define (color->color-string original-color)
  (define (quantize-and-crop-component color-component)
    (cond ((<= 0.0 color-component color-max-intensity)
           (/ (round (* color-quantization-levels color-component))
              color-quantization-levels))
          ((>= color-component color-max-intensity)
           color-max-intensity)
          (else
           0.0)))
  (let ((color (mul global-brightness-factor original-color)))
    (case color-mode
      ((normal)
       (string-append "rgbi:"
                      (number->string (quantize-and-crop-component (vector-ref color 0)))
                      "/"
                      (number->string (quantize-and-crop-component (vector-ref color 1)))
                      "/"
                      (number->string (quantize-and-crop-component (vector-ref color 2)))))
      ((blue-gray)
       (let ((red&green-string (number->string (quantize-and-crop-component (min (average (vector-ref color 0)
                                                                                          (vector-ref color 1))
                                                                                 (vector-ref color 2))))))
         (string-append "rgbi:"
                        red&green-string
                        "/"
                        red&green-string
                        "/"
                        (number->string (quantize-and-crop-component (vector-ref color 2))))))
      ((cyan-gray)
       (let ((green&blue-string (number->string (quantize-and-crop-component (average (vector-ref color 1)
                                                                                      (vector-ref color 2))))))
         (string-append "rgbi:"
                        (number->string (quantize-and-crop-component (min (vector-ref color 0)
                                                                          (average (vector-ref color 1)
                                                                                   (vector-ref color 2)))))
                        "/"
                        green&blue-string
                        "/"
                        green&blue-string)))
      ((green-blue-gray)
       (string-append "rgbi:"
                      (number->string (quantize-and-crop-component (min (vector-ref color 0)
                                                                        (vector-ref color 1)
                                                                        (vector-ref color 2))))
                      "/"
                      (number->string (quantize-and-crop-component (vector-ref color 1)))
                      "/"
                      (number->string (quantize-and-crop-component (vector-ref color 2)))))
      ((grayscale)
       (let ((gray-string (number->string (quantize-and-crop-component (average (vector-ref color 0)
                                                                                (vector-ref color 1)
                                                                                (vector-ref color 2))))))
         (string-append "rgbi:"
                        gray-string
                        "/"
                        gray-string
                        "/"
                        gray-string))))))


; Poimii huoneen valaistustiedosta yksitt‰isen valaistuksen tiettyyn suuntaan.
; dir: f = floor, enws = ilmansuunnat
(define (cell-lightning->cell-fenws-dir-lightning lights dir)
  (vector-ref lights
              (cadr (assq dir
                          '((f 0) (e 1) (n 2) (w 3) (s 4))))))


; Generoi yksitt‰isest‰ valoarvosta hieman v‰p‰tt‰v‰n color-cycle-steps -pituisen sekvenssin
(define (generate-randomly-flickering-lights light)
  (generate-vector color-cycle-steps
                   (lambda ()
                     (mul light
                          ; 1 +- (0 .. lights-flicker-amount/2):
                          (+ (- 1 (/ lights-flicker-amount 2))
                             ;   0 <= random-value < lights-flicker-amount:
                             (/ (random (inexact->exact (round (* 1000
                                                                  lights-flicker-amount))))
                                1000))))))
(define (generate-flashing-lights light)
  (let ((light-magnitudes (generate-vector-with-i color-cycle-steps
                                                  (lambda (i)
                                                    (linear-interpolate (/ i
                                                                           color-cycle-steps)
                                                                        (cons 0.0 1.0)
                                                                        (cons 0.5 0.0)
                                                                        (cons 1.0 0.0))))))
                                                    ;(dynamic-bezier (/ i
                                                    ;                   color-cycle-steps)
                                                    ;                0.4
                                                    ;                0.2
                                                    ;                0.0
                                                    ;                0.4)))))
    (vector-map (lambda (magnitude)
                  (mul magnitude
                       light))
                light-magnitudes)))


;;; Luo, alustaa ja aktivoi uuden liikkuvan valonl‰hteen
(define (new-dynamic-light dynamic-lights-manager locator ticker
                           affection-distance switching-steps light-color visible-in-3d-mode? bg-mergeable?)
  (let ((previous-loc ((locator 'get-loc)))
        (visit-handlers-prev-cell ((locator 'get-loc-as-cell-index-x.y)))
        (dying? false)
        (state-changed? true)
        (flicker-lights false)
        (prev-light-brightness 0)
        (light-brightness (new-smooth-changing-value ticker
                                                     switching-steps
                                                     (lambda (t) t)
                                                     0)))
    
    ; Apufunktiot
    (define (init)
      ((ticker 'request-ticking!) dispatch)
      ((light-brightness 'set-target-value!) 1)
      ((locator 'request-cell-visit-messages) cell-visit-message-handler)
      ((dynamic-lights-manager 'update-me!) dispatch true))
    (define (tick!)
      ; Onko henki l‰htenyt?
      (if (and dying?
               (= ((light-brightness 'get-mapped-value)) 0))
          (begin
            ((dynamic-lights-manager 'erase-me!) dispatch)
            ((ticker 'stop-ticking-me!) dispatch)
            ((locator 'request-cell-visit-messages) false)
            (set! dying? 'dead!))
          ; Hengiss‰ viel‰
          (begin
            ; Ollaanko liikuttu?
            (if (not (equal? previous-loc
                             ((locator 'get-loc))))
                (set! state-changed? true))
            ; Kirkkaus muuttunut? P‰ivitet‰‰n flicker-lights
            (if (not (= prev-light-brightness
                        ((light-brightness 'get-mapped-value))))
                (begin
                  (set! state-changed? true)
                  (set! flicker-lights false)
                  (set! prev-light-brightness ((light-brightness 'get-mapped-value)))))
            ; Onko tila muuttunut? P‰ivitet‰‰n tilanne dynamic-lights-managerille
            (if state-changed?
                (begin
                  ((dynamic-lights-manager 'update-me!) dispatch false)
                  (set! previous-loc ((locator 'get-loc)))
                  (set! state-changed? false))))))
    (define (cell-visit-message-handler visited-cell-x.y)
      ((dynamic-lights-manager 'im-in-a-new-cell!) dispatch
                                                   visit-handlers-prev-cell
                                                   visited-cell-x.y)
      (set! visit-handlers-prev-cell
            visited-cell-x.y))
    (define (make-flicker-lights)
      (generate-randomly-flickering-lights (mul ((light-brightness 'get-mapped-value))
                                                light-color)))
    
    ; Metodit
    (define (set-light-color! new-light-color)
      (set! light-color new-light-color)
      (set! flicker-lights false)
      (set! state-changed? true))
    (define (get-light-color)
      light-color)
    (define (get-affection-distance)
      affection-distance)
    (define (get-info-visible-in-3d-mode?)
      visible-in-3d-mode?)
    (define (get-lights)
      (if (not flicker-lights)
          (set! flicker-lights
                (make-flicker-lights)))
      flicker-lights)
    (define (die!)
      ((light-brightness 'set-target-value!) 0)
      (set! dying? true))
    (define (query-bg-mergeable?)
      bg-mergeable?)
    (define (set-as-bg-merged!)
      (set! bg-mergeable? 'merged!))
    (define (bg-merged?)
      (eq? bg-mergeable? 'merged!))
    
    ; Dispatch
    (define (dispatch method)
      (if (eq? dying? 'dead!)
          false
          (cond ((eq? method 'tick!) tick!)
                ((eq? method 'set-light-color!) set-light-color!)
                ((eq? method 'get-light-color) get-light-color)
                ((eq? method 'locator) locator)
                ((eq? method 'light-brightness) light-brightness)
                ((eq? method 'get-affection-distance) get-affection-distance)
                ((eq? method 'visible-in-3d-mode?) get-info-visible-in-3d-mode?)
                ((eq? method 'get-lights) get-lights)
                ((eq? method 'die!) die!)
                ((eq? method 'bg-mergeable?) query-bg-mergeable?)
                ((eq? method 'set-as-bg-merged!) set-as-bg-merged!)
                ((eq? method 'bg-merged?) bg-merged?)
                (else (error "Unknown method! dynamic-light." method)))))
    (init)
    dispatch))


;;; dynamic-lights-manager:
;;;   Hoitaa labyrintin liikkuvia valoja.
(define (new-dynamic-lights-manager maze-map)
  (let ((all-dynamic-lights '())
        (dynamic-lights-in-cells-vector (generate-vector (cadr ((maze-map 'get-size-list)))
                                                         (lambda ()
                                                           (generate-vector (car ((maze-map 'get-size-list)))
                                                                            (lambda ()
                                                                              '()))))))
    
    ;;; Apufunktiot
    (define (add-dynamic-light-ref-to-cell cell-x.y ref)
      (vector-set! (vector-ref dynamic-lights-in-cells-vector
                               (cdr cell-x.y))
                   (car cell-x.y)
                   (cons ref
                         (vector-ref (vector-ref dynamic-lights-in-cells-vector
                                                 (cdr cell-x.y))
                                     (car cell-x.y)))))
    (define (remove-dynamic-light-ref-from-cell cell-x.y ref)
      (vector-set! (vector-ref dynamic-lights-in-cells-vector
                               (cdr cell-x.y))
                   (car cell-x.y)
                   (remove-from-list (vector-ref (vector-ref dynamic-lights-in-cells-vector
                                                             (cdr cell-x.y))
                                                 (car cell-x.y))
                                     ref)))
    
    ;;; Metodit
    ; Luo uuden dynaamisen valonl‰hteen. Valonl‰hdett‰ voi liikutella ja sill‰ on kineettiset ominaisuudet.
    ; Valonl‰hde aktivoituu v‰littˆm‰sti.
    (define (make-and-register-dynamic-light! . args)
      (apply new-dynamic-light (cons dispatch
                                     args)))
    (define (update-me! dynamic-light first-time?)
      ; Jos eka kerta, lis‰t‰‰n kaikkien valojen listaan ja ko. huoneen listaan
      (if first-time?
          (begin
            (set! all-dynamic-lights
                  (cons dynamic-light
                        all-dynamic-lights))
            (add-dynamic-light-ref-to-cell (((dynamic-light 'locator) 'get-loc-as-cell-index-x.y))
                                           dynamic-light)))
      ; Poistetaan
      (if (not first-time?)
          ((maze-map 'remove-light-source!) dynamic-light 'dynamic-light))
      ; Ja laitetaan takaisin
      ((maze-map 'add-light-source!) (make-light-source-type 'dynamic-light
                                                             ((dynamic-light 'get-affection-distance))
                                                             ((dynamic-light 'get-lights)))
                                     (((dynamic-light 'locator) 'get-loc))
                                     dynamic-light))
    (define (im-in-a-new-cell! dynamic-light prev-cell new-cell)
      ;(display (list 'im-in-new-cell!: prev-cell new-cell)) (newline)
      (if prev-cell
          (remove-dynamic-light-ref-from-cell prev-cell
                                              dynamic-light))
      (add-dynamic-light-ref-to-cell new-cell
                                     dynamic-light))
    (define (erase-me! dynamic-light)
      ((maze-map 'remove-light-source!) dynamic-light 'dynamic-light)
      (remove-dynamic-light-ref-from-cell (((dynamic-light 'locator) 'get-loc-as-cell-index-x.y))
                                          dynamic-light)
      (set! all-dynamic-lights
            (remove-from-list all-dynamic-lights
                              dynamic-light)))
    (define (get-dynamic-lights-list)
      all-dynamic-lights)
    (define (get-dynamic-lights-in-cell-list cell-x.y)
;      (display (list 'get-dyns-in-cell: cell-x.y)) (newline)
;      (display (list 'result:
;                     (vector-ref (vector-ref dynamic-lights-in-cells-vector
;                                             (cdr cell-x.y))
;                                 (car cell-x.y))))
;      (newline)
      (vector-ref (vector-ref dynamic-lights-in-cells-vector
                              (cdr cell-x.y))
                  (car cell-x.y)))
    
    ;;; Dispatch
    (define (dispatch method)
      (cond ((eq? method 'new-dynamic-light!) make-and-register-dynamic-light!)
            ((eq? method 'update-me!) update-me!)
            ((eq? method 'erase-me!) erase-me!)
            ((eq? method 'im-in-a-new-cell!) im-in-a-new-cell!)
            ((eq? method 'get-dynamic-lights-list) get-dynamic-lights-list)
            ((eq? method 'get-dynamic-lights-in-cell-list) get-dynamic-lights-in-cell-list)
            (else (error "Unknown method! dynamic-lights-manager." method))))
    dispatch))
