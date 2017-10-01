;;; math.scm


;;; Perusjuttuja
(define pi (* 2 (acos 0)))
(define 2pi (* 2 pi))
(define pi/2 (/ pi 2))
(define 3/2pi (* 3/2 pi))
(define (mod a n)   ; Modulo reaaliluvuille
  (- a
     (* (floor (/ a n))
        n)))
(define (factorial n)
  (if (< n 2)
      1
      (* n
         (factorial (- n 1)))))
(define (average . args)
  (/ (apply + args)
     (length args)))


;;; x.y -koordinaattiparit
(define (x.y-pair? x)
  (and (pair? x)
       (number? (car x))
       (number? (cdr x))))
(define (x.y-pair-geom-length-pow2 x.y)
  (+ (* (car x.y) (car x.y))
     (* (cdr x.y) (cdr x.y))))
(define (loc-vector->cell-index-x.y loc-vec)
  (cons (inexact->exact (floor (x-component loc-vec)))
        (inexact->exact (floor (y-component loc-vec)))))
(define (cell-index-x.y->loc-vector cell-index-x.y height)
  (vector (+ (car cell-index-x.y) 0.5)
          (+ (cdr cell-index-x.y) 0.5)
          height))
(define (loc-vector->x.y-pair loc-vec)
  (cons (x-component loc-vec)
        (y-component loc-vec)))
(define (enws-dir->x.y-pair enws-dir)
  (cond ((eq? enws-dir 'e) (cons 1 0))
        ((eq? enws-dir 'n) (cons 0 1))
        ((eq? enws-dir 'w) (cons -1 0))
        (else (cons 0 -1))))


;;; värit
;;; Lineaarinen interpolointi
; t.value-parien on oltava t:n mukaan kasvavassa suuruusjärjestyksessä.
(define (linear-interpolate t . t.value-pairs)
  ; Etsii määräävät arvot
  (define (find-controlling-t.value-pairs t rest-t.value-pairs)
    ; Onko t tämän ja seuraavan parin t-arvon välillä?
    (if (and (>= t (car (car rest-t.value-pairs)))
             (<= t (car (cadr rest-t.value-pairs))))
        rest-t.value-pairs
        (find-controlling-t.value-pairs t (cdr rest-t.value-pairs))))
  (let ((controlling-t.value-pairs (find-controlling-t.value-pairs t t.value-pairs)))
    (let ((start-t (car (car controlling-t.value-pairs)))
          (start-value (cdr (car controlling-t.value-pairs)))
          (end-t (car (cadr controlling-t.value-pairs)))
          (end-value (cdr (cadr controlling-t.value-pairs))))
      (let ((start-value-weight (/ (- end-t t)
                                   (- end-t start-t))))
        (add (mul start-value-weight
                  start-value)
             (mul (- 1 start-value-weight)
                  end-value))))))


;;; bezier-käyrä
; t = 0.0..1.0
(define (dynamic-bezier t . control-vectors)
  (define (bernstein-weight n k t)
    (* (/ (factorial n)
          (* (factorial k)
             (factorial (- n k))))
       (if (= k 0)
           1.0
           (expt t k))
       (if (= k n)
           1.0
           (expt (- 1 t)
                 (- n k)))))
  (define (make-bernstein-weight-list n t)
    (define (recurse n k t)
      (if (> k n)
          '()
          (cons (bernstein-weight n k t)
                (recurse n (+ k 1) t))))
    (recurse n 0 t))
  (let* ((n (- (length control-vectors) 1))
         (bernstein-weights (make-bernstein-weight-list n t)))
    (apply add
           (map mul
                bernstein-weights
                control-vectors))))


;;; Hitaasti muuttuvat lukuarvot
; Olion arvo muuttuu pehmeästi ticker -olion antaman kellotuksen
; tahdissa haluttua kohdearvoa kohti. Haluttaessa voidaan kysyä arvo annetun funktion
; läpi muunnettuna.
(define (new-smooth-changing-value ticker 0-to-1-steps mapping-func value)
  (let ((target-value value)
        (step-length (/ 1 0-to-1-steps)))
    
    ; Metodit -----------------------------------------------
    (define (tick!)
      (cond ((> target-value value)
             (set! value (+ value step-length))
             (if (>= value target-value)
                 (set! value target-value)))
            ((< target-value value)
             (set! value (- value step-length))
             (if (<= value target-value)
                 (set! value target-value)))))
    (define (set-target-value! new-target)
      (set! target-value new-target))
    (define (get-target-value)
      target-value)
    (define (set-0-to-1-steps! steps)
      (set! 0-to-1-steps steps)
      (set! step-length (/ 1 steps)))
    (define (get-0-to-1-steps)
      0-to-1-steps)
    (define (set-value! new-value)
      (set! value new-value)
      (set! target-value new-value))
    (define (get-value)
      value)
    (define (get-mapped-value)
      (mapping-func value))
    
    ; Dispatch ----------------------------------------------
    (define (dispatch method)
      (cond ((eq? method 'tick!) tick!)
            ((eq? method 'set-target-value!) set-target-value!)
            ((eq? method 'get-target-value) get-target-value)
            ((eq? method 'set-0-to-1-steps!) set-0-to-1-steps!)
            ((eq? method 'get-0-to-1-steps) get-0-to-1-steps)
            ((eq? method 'set-value!) set-value!)
            ((eq? method 'get-value) get-value)
            ((eq? method 'get-mapped-value) get-mapped-value)
            (else (error "Unknown method! smooth-changing-value." method))))

    ((ticker 'request-ticking!) dispatch)
    dispatch))

;;; Matriisit ja vektorit
; Vektori:
;   Schemen vakiovektori, elementteinä skalaareita.
(define (math-vector? x)
  (and (vector? x)
       (number? (vector-ref x 0))))
(define (inner-product v1 v2)
  (vector-apply + (vector-map-2 * v1 v2)))
(define (cross-product v1 v2)
  (vector (- (* (vector-ref v1 1)
                (vector-ref v2 2))
             (* (vector-ref v1 2)
                (vector-ref v2 1)))
          (- (* (vector-ref v1 2)
                (vector-ref v2 0))
             (* (vector-ref v1 0)
                (vector-ref v2 2)))
          (- (* (vector-ref v1 0)
                (vector-ref v2 1))
             (* (vector-ref v1 1)
                (vector-ref v2 0)))))
(define (make-unit-vector vec)
  (mul (/ 1 (vector-geom-length vec))
       vec))
; Determinantti 3x3 -matriiseille
(define (det m)
  (inner-product (matrix-get-row-ref m 0)
                 (cross-product (matrix-get-row-ref m 1)
                                (matrix-get-row-ref m 2))))
(define (vector-geom-length-pow2 vec)
  (vector-apply +
                (vector-map (lambda (x)
                              (* x x))
                            vec)))
(define (vector-geom-length vec)
  (sqrt (vector-geom-length-pow2 vec)))
(define (vector-normal->homogenic vec)
  (let ((new-vec (make-vector (+ (vector-length vec) 1) 1)))
    (vector-map-to (lambda (x) x) vec new-vec)
    new-vec))
(define (vector-homogenic->normal vec)
  (let* ((new-vec-length (- (vector-length vec) 1))
         (new-vec (make-vector new-vec-length))
         (divisor (vector-ref vec new-vec-length)))
    (do ((i 0 (+ i 1)))
        ((= i new-vec-length))
      (vector-set! new-vec i (/ (vector-ref vec i)
                                divisor)))
    new-vec))
(define (x-component vec)
  (vector-ref vec 0))
(define (y-component vec)
  (vector-ref vec 1))
(define (z-component vec)
  (vector-ref vec 2))
(define (set-x-component! vec val)
  (vector-set! vec 0 val))
(define (set-y-component! vec val)
  (vector-set! vec 1 val))
(define (set-z-component! vec val)
  (vector-set! vec 2 val))


; Matriisi:
;   Schemen vakiovektori, elementteinä vektoreita (rivit).
(define (make-matrix rows cols)
  (let ((y-vec (make-vector rows)))
    (do ((y 0 (+ y 1)))
        ((= y rows))
      (vector-set! y-vec y (make-vector cols 0)))
    y-vec))
(define (matrix? x)
  (and (vector? x)
       (vector? (vector-ref x 0))
       (number? (vector-ref (vector-ref x 0) 0))))
(define (matrix-rows x)
  (vector-length x))
(define (matrix-cols x)
  (vector-length (vector-ref x 0)))
; Apufunktioita
(define (matrix-set-row! matrix row vec)
  (vector-set! matrix row (copy-vector vec)))
(define (matrix-get-row-ref matrix row)
  (vector-ref matrix row))
(define (matrix-get-row matrix row)
  (copy-vector (matrix-get-row-ref matrix row)))
(define (matrix-set-col! matrix col vec)
  (vector-for-each-with-i (lambda (row i)
                            (vector-set! row
                                         col
                                         (vector-ref vec i)))
                          matrix))
(define (matrix-get-col matrix col)
  (vector-map (lambda (row)
                (vector-ref row col))
              matrix))
(define (matrix-set-cell! matrix row col val)
  (vector-set! (vector-ref matrix row) col val))
(define (matrix-get-cell matrix row col)
  (vector-ref (vector-ref matrix row) col))
(define (transpose matrix)
  (let* ((result-rows (matrix-cols matrix))
         (result-rows-vec (make-vector result-rows)))
    (do ((row 0 (+ row 1)))
        ((= row result-rows))
      (vector-set! result-rows-vec
                   row
                   (matrix-get-col matrix row)))
    result-rows-vec))


;;; Projektiomatriisi
; c  = projektiokeskus
; r0 = piste projektiotasolla
; ex = tason x-virittäjä
; ey = tason y-virittäjä
; n  = tason normaali
(define (make-projection-matrix c r0 ex ey n)
  (let ((cross-product-for-x (cross-product ey (sub c r0)))
        (cross-product-for-y (cross-product ex (sub c r0)))
        (matrix-row-0 (make-vector 4))
        (matrix-row-1 (make-vector 4))
        (matrix-row-2 (make-vector 4)))
    ; rivi 0
    (vector-map-to +
                   cross-product-for-x
                   matrix-row-0)
    (vector-set! matrix-row-0
                 3
                 (- (inner-product cross-product-for-x c)))
    ; rivi 1
    (vector-map-to -
                   cross-product-for-y
                   matrix-row-1)
    (vector-set! matrix-row-1
                 3
                 (inner-product cross-product-for-y c))
    ; rivi 2
    (vector-map-to -
                   n
                   matrix-row-2)
    (vector-set! matrix-row-2
                 3
                 (inner-product n c))
    (vector matrix-row-0
            matrix-row-1
            matrix-row-2)))


; **** Dataohjatut(?) laskutoimitukset
(define (add-2 lhs rhs)
  (cond
   ; x.y-pari + x.y-pari
   ((and (x.y-pair? lhs)
         (x.y-pair? rhs))
    (cons (+ (car lhs) (car rhs))
          (+ (cdr lhs) (cdr rhs))))
   ; skalaari + skalaari
   ;((and (number? lhs)
   ;      (number? rhs))
   ; (+ lhs rhs))
   ; skalaari + vektori
   ((and (number? lhs)
         (math-vector? rhs))
    (vector-map (lambda (x)
                  (+ lhs x))
                rhs))
   ; vektori + skalaari
   ((and (math-vector? lhs)
         (number? rhs))
    (add rhs lhs))
   ; vektori + vektori
   ((and (math-vector? lhs)
         (math-vector? rhs))
    (vector-map-2 + lhs rhs))
   ; matriisi + matriisi
   ((and (matrix? lhs)
         (matrix? rhs))
    (vector-map-2 (lambda (v1 v2)
                    (add v1 v2))
                  lhs
                  rhs))
   (else (error "Math: argument type(s) unknown!" (list 'add-2 lhs rhs)))))
(define (add . args)
  ; Skalaarien yhteenlasku?
  (if (and (not (null? args))
           (number? (car args)))
      (apply + args)
      ; Ei ole..
      (case (length args)
        ((2) (add-2 (car args) (cadr args)))
        ((1) (car args))
        ((0) void)
        (else (add-2 (car args) (apply add (cdr args)))))))
(define (sub lhs rhs)
  (cond
   ; * - skalaari
   ((number? rhs)
    (add lhs (- rhs)))
   ; x.y-pari + x.y-pari
   ((and (x.y-pair? lhs)
         (x.y-pair? rhs))
    (cons (- (car lhs) (car rhs))
          (- (cdr lhs) (cdr rhs))))
   ;skalaari - vektori
   ((and (number? lhs)
         (math-vector? rhs))
    (vector-map (lambda (x)
                  (- lhs x))
                rhs))
   ; vektori - vektori
   ((and (math-vector? lhs)
         (math-vector? rhs))
    (vector-map-2 - lhs rhs))
   (else (error "Math: argument type(s) unknown!" (list 'sub lhs rhs)))))
(define (mul lhs rhs)
  (cond
   ; skalaari * skalaari
   ((and (number? lhs)
         (number? rhs))
    (* lhs rhs))
   ; skalaari * vektori
   ((and (number? lhs)
         (math-vector? rhs))
    (vector-map (lambda (x)
                  (* lhs x))
                rhs))
   ; vektori * skalaari
   ((and (math-vector? lhs)
         (number? rhs))
    (mul rhs lhs))
   ; skalaari * matriisi
   ((and (number? lhs)
         (matrix? rhs))
    (vector-map (lambda (row)
                  (mul lhs row))
                rhs))
   ; matriisi * skalaari
   ((and (matrix? lhs)
         (number? rhs))
    (mul rhs lhs))
   ; matriisi * vektori
   ((and (matrix? lhs)
         (math-vector? rhs))
    (vector-map (lambda (row)
                  (inner-product rhs row))
                lhs))
   ; matriisi * matriisi
   ((and (matrix? lhs)
         (matrix? rhs))
    (let ((rhs-t (transpose rhs)))
      (vector-map (lambda (lhs-row)
                    (vector-map (lambda (rhs-col)
                                  (inner-product lhs-row
                                                 rhs-col))
                                rhs-t))
                  lhs)))
   (else (error "Math: argument type(s) unknown!" (list 'mul lhs rhs)))))
