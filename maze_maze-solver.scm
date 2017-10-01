;;; maze_maze-solver.scm


;;; Generoi reitin kahden annetun sijainnin v‰lille.
;;; Paluuarvo:
;;;   lista ratkaisureitin huoneiden koordinaateista x.y-pareina
(define (maze-solver maze-map start-loc-x.y end-loc-x.y)
  ; L‰hett‰‰ rekursioita eteenp‰in annetusta huoneesta.
  ; Mik‰li jokin rekursio lˆyt‰‰ ratkaisun, palautetaan t‰m‰n huoneen koordinaatit
  ; liitettyn‰ ko. rekrusion palauttamaan reittilistaan.
  ; Mik‰li annettu huone on kohdehuone, palautetaan t‰m‰n huoneen koordinaatit.
  (define (advance current-loc-x.y end-x end-y current-walk-dir)
    (if (and (= (car current-loc-x.y) end-x)
             (= (cdr current-loc-x.y) end-y))
        ; Ollaan tavoitehuoneessa
        (list current-loc-x.y)
        ; Ei olla, l‰hetet‰‰n rekursiot
        (let ((result (do ((dirs (remove-from-list '(e n w s)
                                                   (turn-back current-walk-dir))
                                 (cdr dirs))
                           (result false))
                          ; Lopetusehto
                          ((or result
                               (null? dirs)) result)
                        (set! result
                              (try-to-advance-to-dir current-loc-x.y
                                                     end-x
                                                     end-y
                                                     (car dirs))))))
          ; Lˆytyikˆ raktaisureitti?
          (if result
              (cons current-loc-x.y
                    result)
              false))))
  ; Yritt‰‰ l‰hte‰ annettuun suuntaan etsim‰‰n ratkaisua. Huomioi sein‰t.
  (define (try-to-advance-to-dir current-loc-x.y end-x end-y dir)
    (if ((maze-map 'wall-at-to?) (car current-loc-x.y)
                                 (cdr current-loc-x.y)
                                 dir)
        false
        (advance (one-step-to-enws-dir current-loc-x.y
                                       dir)
                 end-x
                 end-y
                 dir)))
  
  ; Ollaanko jo valmiiksi ratkaisuhuoneessa?
  (if (and (= (car start-loc-x.y) (car end-loc-x.y))
           (= (cdr start-loc-x.y) (cdr end-loc-x.y)))
      ; Palautetaan pelk‰st‰‰n t‰m‰n huoneen sijainti
      (list start-loc-x.y)
      ; Ei olla, yritet‰‰n l‰hett‰‰ rekursiot jokaiseen ilmansuuntaan
      (cons start-loc-x.y
            (do ((dirs '(e n w s) (cdr dirs))
                 (result false))
                (result result)
              (set! result
                    (try-to-advance-to-dir start-loc-x.y
                                           (car end-loc-x.y)
                                           (cdr end-loc-x.y)
                                           (car dirs)))))))
