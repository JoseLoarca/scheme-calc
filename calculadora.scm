(define search-first (lambda (x y) (let z ([a (string->list x)] [b 0]) (if (null? a) #f (if (equal? (car a) y) b (z (cdr a) (+ b 1))))))) 
(define search-last (lambda (x y) (let z ([a (string->list x)] [b 0] [c -1]) (if (null? a) (if (= -1 c) #f c) (if (equal? (car a) y) (z (cdr a) (+ b 1) (+ b 0)) (z (cdr a) (+ b 1) (+ c 0)))))))
(define str (read-line))
(define par1 0)
(define par2 0)
(define x 0)
(define y 0)
(define resp 0)
(define (suma x y)
  (+ x y)
)
(define (resta x y)
  (- x y)
)
(define (mult x y)
  (* x y)
)
(define (div x y)
  (/ x y)
)
(define (verificar s)
  (if (and (search-first s #\() (search-first s #\)))
      (begin
        (set! par1 (search-first s #\())
        (set! par2 (search-first s #\)))
        (cond
          [(string=? "+" (string (string-ref s (+ par1 1)))) (begin 
                                                               (set! x (string->number (substring s (+ (search-first s #\ ) 1) (search-last s #\ ))))
                                                               (set! y (string->number (substring s (+ (search-last s #\ ) 1) (search-first s #\)))))
                                                               (set! resp (suma x y))
                                                               (display resp)
                                                               )]
          [(string=? "-" (string (string-ref s (+ par1 1)))) (begin 
                                                               (set! x (string->number (substring s (+ (search-first s #\ ) 1) (search-last s #\ ))))
                                                               (set! y (string->number (substring s (+ (search-last s #\ ) 1) (search-first s #\)))))
                                                               (set! resp (resta x y))
                                                               (display resp)
                                                               )]
          [(string=? "*" (string (string-ref s (+ par1 1)))) (begin 
                                                               (set! x (string->number (substring s (+ (search-first s #\ ) 1) (search-last s #\ ))))
                                                               (set! y (string->number (substring s (+ (search-last s #\ ) 1) (search-first s #\)))))
                                                               (set! resp (mult x y))
                                                               (display resp)
                                                               )]
          [(string=? "/" (string (string-ref s (+ par1 1)))) (begin 
                                                               (set! x (string->number (substring s (+ (search-first s #\ ) 1) (search-last s #\ ))))
                                                               (set! y (string->number (substring s (+ (search-last s #\ ) 1) (search-first s #\)))))
                                                               (set! resp (div x y))
                                                               (display resp)
                                                               )]
          [else (display "El string ingresado no está en el formato correcto")]
          )
        )
      (begin 
        (if (string=? "quit" s)
            (exit #t)
            (display "El string ingresado no está en el formato correcto")
            )
        )
      )
)
(verificar str)