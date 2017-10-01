; enws-dirs.scm


; Pyöristää astesuunnan lähimmäksi ilmansuunnaksi
; (tai sen vastasuunnaksi, mikäli 2. argumentti = 'behind)
(define (nearest-enws-dir dir ahead/behind?)
  (let ((the-dir (mod (if (eq? ahead/behind? 'ahead)
                          dir
                          (+ dir pi))
                      2pi)))
    (cond ((or (< the-dir (* 0.25 pi))
               (> the-dir (* 1.75 pi)))
           'e)
          ((< the-dir (* 0.75 pi))
           'n)
          ((< the-dir (* 1.25 pi))
           'w)
          (else 's))))

(define (turn-left enws-dir)
  (cond ((eq? enws-dir 'e) 'n)
        ((eq? enws-dir 'n) 'w)
        ((eq? enws-dir 'w) 's)
        (else 'e)))

(define (turn-right enws-dir)
  (cond ((eq? enws-dir 'e) 's)
        ((eq? enws-dir 'n) 'e)
        ((eq? enws-dir 'w) 'n)
        (else 'w)))

(define (turn-back enws-dir)
  (cond ((eq? enws-dir 'e) 'w)
        ((eq? enws-dir 'n) 's)
        ((eq? enws-dir 'w) 'e)
        (else 'n)))

; Siirtyy loc-x.y -paikasta yhden askeleen annettuun
; ilmansuuntaan ja palauttaa uuden sijainnin x.y -parina
(define (one-step-to-enws-dir loc-x.y enws-dir)
  (cons (+ (car loc-x.y)
           (cond ((eq? enws-dir 'w) -1)
                 ((eq? enws-dir 'e) 1)
                 (else 0)))
        (+ (cdr loc-x.y)
           (cond ((eq? enws-dir 's) -1)
                 ((eq? enws-dir 'n) 1)
                 (else 0)))))
