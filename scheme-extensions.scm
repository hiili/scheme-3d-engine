;;; scheme-extensions.scm


(define no-op false)


; Palauttaa procin käyttämän ajan millisekuntteina
(define (measure-time proc . args)
  (let ((start-time (current-milliseconds)))
    (apply proc args)
    (- (current-milliseconds)
       start-time)))


;;; listaoperaatioita vektoreille
(define (vector-apply func vec)
  (apply func (vector->list vec)))
; func: (func <vektorin i:s elementti> i)
(define (vector-for-each-with-i func vec)
  (let ((length (vector-length vec)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (func (vector-ref vec i) i))))
(define (vector-for-each func vec)
  (vector-for-each-with-i (lambda (i) (func)) vec))
(define (vector-for-each-2 func vec1 vec2)
  (let ((length (vector-length vec1)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (func (vector-ref vec1 i)
            (vector-ref vec2 i)))))
; Generoi length-pituisen vektorin ja alustaa jokaisen alkion
; funcin paluuarvolla. Func saa argumentikseen senhetkisen indeksin.
(define (generate-vector-with-i length func)
  (let ((vec (make-vector length)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (vector-set! vec i (func i)))
    vec))
; Sama kuin make-vector, paitsi alkioihin tulee yksilöllinen sisältö,
; eikä osoittimia yhteen yhteiseen kohteeseen.
(define (generate-vector length func)
  (generate-vector-with-i length (lambda (i) (func))))
(define (vector-map func vec)
  (let ((new-vec (make-vector (vector-length vec))))
    (vector-map-to func vec new-vec)
    new-vec))
(define (vector-map-n func . vecs)
  (define (nth-elements vecs n)
    (map (lambda (vec)
           (vector-ref vec n))
         vecs))
  (let* ((length (vector-length (car vecs)))
         (new-vec (make-vector length)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (vector-set! new-vec
                   i
                   (apply func (nth-elements vecs i))))
    new-vec))
; Sama kuin vector-map, paitsi tulos asetetaan annettuun
; dest-vec -vektoriin, jonka on oltava vähintään yhtä pitkä kuin vec.
(define (vector-map-to func vec dest-vec)
  (let ((length (vector-length vec)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (vector-set! dest-vec
                   i
                   (func (vector-ref vec i))))))
(define (vector-map-2 func vec1 vec2)
  (let* ((length (vector-length vec1))
         (new-vec (make-vector length)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (vector-set! new-vec
                   i
                   (func (vector-ref vec1 i)
                         (vector-ref vec2 i))))
    new-vec))
; Palauttaa kopion vektorista.
; Huom! tekee vain 1. tason kopioinnin, ei deep-copya!
(define (copy-vector vec)
  (vector-map (lambda (x) x) vec))
; Siirtää vektorin sisältöä annetun muuttujan verran,
; kierrättäen sisältöä toiseen päähän. Positiivinen siirtymä = siirto oikealle
(define (rotate-vector vec amount)
  (let ((vec-length (vector-length vec)))
    (generate-vector-with-i vec-length
                            (lambda (i)
                              (vector-ref vec
                                          (modulo (- i amount)
                                                  vec-length))))))


;;; and ja or -laajennuksia
(define (apply-and args)
  (if (null? args)
      true
      (and (car args)
           (apply-and (cdr args)))))
(define (apply-or args)
  (if (null? args)
      false
      (or (car args)
          (apply-or (cdr args)))))


;;; Listaoperaatioita
; Palauttaa listan, josta on poistettu ensimmäinen(!) element -alkio.
(define (remove-from-list the-list element)
  (if (null? the-list)
      '()
      (if (eq? (car the-list)
               element)
          (cdr the-list)
          (cons (car the-list)
                (remove-from-list (cdr the-list)
                                  element)))))
; Palauttaa listan, josta on poistettu ensimmäinen(!) func -ehdon toteuttava alkio
(define (remove-from-list-if func the-list)
  (if (null? the-list)
      '()
      (if (func (car the-list))
          (cdr the-list)
          (cons (car the-list)
                (remove-from-list-if func (cdr the-list))))))
