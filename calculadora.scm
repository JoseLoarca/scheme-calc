(require racket/string)

(define search-first (lambda (x y) (let z ([a (string->list x)] [b 0]) (if (null? a) #f (if (equal? (car a) y) b (z (cdr a) (+ b 1))))))) 
(define search-last (lambda (x y) (let z ([a (string->list x)] [b 0] [c -1]) (if (null? a) (if (= -1 c) #f c) (if (equal? (car a) y) (z (cdr a) (+ b 1) (+ b 0)) (z (cdr a) (+ b 1) (+ c 0)))))))
(define search-all (lambda (x y) (let z ([a (string->list x)] [b 0] [r (list)]) (if (null? a) r (if (equal? (car a) y) (z (cdr a) (+ b 1) (append r (list b))) (z (cdr a) (+ b 1) (append r (list))))))))

(define str "texto")

(define (factorial n)
  (define contador 1)
  (define fact 1)
  (define (cicloFact)
    (if (<= contador n)
        (begin
          (set! fact (* fact contador))
          (set! contador (+ contador 1))
          (cicloFact)
          )
        )
    )
  (cicloFact)
    
  fact
)  

(define (opera cadena)
  (set! cadena (string-replace cadena "(" ""))
  (set! cadena (string-replace cadena ")" ""))
  (define operador "")
  
  (define numero1 0)
  (define numero2 0)

  (if (>= (length (search-all cadena #\ )) 1) (set! operador (substring cadena 0 (search-first cadena #\ ))))
  
  (if (= (length (search-all cadena #\ )) 2)
      (begin
        (set! numero1 (substring cadena (+ 1 (search-first cadena #\ )) (search-last cadena #\ )))
        (set! numero2 (substring cadena (+ 1 (search-last cadena #\ )) (string-length cadena)))
       )
   )

  (if (= (length (search-all cadena #\ )) 1)
      (begin
        (set! numero1 (substring cadena (+ 1 (search-first cadena #\ )) (string-length cadena)))
       )
  )
  (define resultado 0)
  
  (if (string=? operador "+") (set! resultado (+ (string->number numero1) (string->number numero2))))
  (if (string=? operador "-") (set! resultado (- (string->number numero1) (string->number numero2))))
  (if (string=? operador "*") (set! resultado (* (string->number numero1) (string->number numero2))))
  (if (string=? operador "/") (set! resultado (/ (string->number numero1) (string->number numero2))))
  (if (string=? operador "sqroot") (set! resultado (sqrt (string->number numero1))))
  (if (string=? operador "sqr") (set! resultado (expt (string->number numero1) 2)))
  (if (string=? operador "sen") (set! resultado (sin (string->number numero1))))
  (if (string=? operador "cos") (set! resultado (cos (string->number numero1))))
  (if (string=? operador "tan") (set! resultado (tan (string->number numero1))))
  (if (string=? operador "div") (set! resultado (quotient (string->number numero1) (string->number numero2))))
  (if (string=? operador "%") (set! resultado (remainder (string->number numero1) (string->number numero2))))
  (if (string=? operador "fact!") (set! resultado (factorial (string->number numero1))))
  (if (string=? operador "") (set! resultado (string->number cadena)))

  resultado
)

(define respuesta 0)

(define (recursivo cadena)
  (define subcadena "")
  (define abre (length (search-all cadena #\()))
  (define cierra (length (search-all cadena #\))))
  (define subrespuesta 0)

  (define (recorre)
    (if (and (> abre 1) (> cierra 1))
        (begin
          (set! subcadena (substring cadena (search-last cadena #\() (+ 1 (search-first cadena #\)))))
          (set! subrespuesta (opera subcadena))
          (set! cadena (string-replace cadena subcadena (number->string subrespuesta)))
          
          (set! abre (length (search-all cadena #\()))
          (set! cierra (length (search-all cadena #\))))
          (recorre)
        )
    )
  )
  (recorre)

  (display (opera cadena))
)

(define (ciclo)
  (display "Calculadora >> ")
  (set! str (read-line))
  (set! respuesta 0)
  (if (not (string=? str "quit"))
      (begin
        (recursivo str)
        (newline)
        (ciclo)
      )
  )
)

(define (main)
  ;(display (string-replace "(* 5 (+ 5 10))" "(+ 5 10)" "15"))
  (ciclo)
  (display "Saliendo ...\n")
  (display "Gracias por usar nuestra calculadora")
)
(main)
