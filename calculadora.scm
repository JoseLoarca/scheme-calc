(require racket/string)

(define search-first (lambda (x y) (let z ([a (string->list x)] [b 0]) (if (null? a) #f (if (equal? (car a) y) b (z (cdr a) (+ b 1))))))) 
(define search-last (lambda (x y) (let z ([a (string->list x)] [b 0] [c -1]) (if (null? a) (if (= -1 c) #f c) (if (equal? (car a) y) (z (cdr a) (+ b 1) (+ b 0)) (z (cdr a) (+ b 1) (+ c 0)))))))
(define search-all (lambda (x y) (let z ([a (string->list x)] [b 0] [r (list)]) (if (null? a) r (if (equal? (car a) y) (z (cdr a) (+ b 1) (append r (list b))) (z (cdr a) (+ b 1) (append r (list))))))))

(define str "texto")

;Método para calcular el factorial de un número.
;Parámetros de entrada: n (int)
;Parámetros de salida: factorial de n (int)
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
;Convierte un ángulo a radianes (para calcular seno, conseno o tangente)
;Parámetros de entrada: angulo (int)
;Parámetros de salida: radianes (int)
(define (aRadianes angulo)
   (* angulo (/ pi 180))
)

;Función para realizar las operaciones.
;Parámetros de entrada: cadena (string)
;Parámetros de salida: resultado (int|string)
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
  (if (string=? operador "/")
      (begin
        (if (= (string->number numero2) 0)
            (begin
              (set! resultado "ERROR! Division entre cero")
            )
            (begin
              (set! resultado (/ (string->number numero1) (string->number numero2)))
            )
        )
      )
  )
  (if (string=? operador "sqroot")
      (begin
        (if (< (string->number numero1) 0)
            (begin
              (set! resultado "ERROR! Raiz cuadrada negativa")
            )
            (begin
              (set! resultado (sqrt (string->number numero1)))
            )
         )
      )
  )
  (if (string=? operador "sqr") (set! resultado (expt (string->number numero1) 2)))
  (if (string=? operador "sen") (set! resultado (sin (string->number numero1))))
  (if (string=? operador "cos") (set! resultado (cos (string->number numero1))))
  (if (string=? operador "tan") (set! resultado (tan (string->number numero1))))
  (if (string=? operador "div") (set! resultado (quotient (string->number numero1) (string->number numero2))))
  (if (string=? operador "%") (set! resultado (remainder (string->number numero1) (string->number numero2))))
  (if (string=? operador "fact!") (set! resultado (factorial (string->number numero1))))
  (if (string=? operador "") (set! resultado (string->number cadena)))
  (if (boolean? resultado)
      (begin
        (if (resultado)
            (begin
              (set! resultado resultado)
            )
            (begin
              (set! resultado "ERROR! Expresion no valida")
            )
        )
      )
   )
            
  resultado
)

(define respuesta 0)

;Función para realizar operaciones compuestas.
;Parámetros de entrada: cadena (string)
(define (recursivo cadena)
  (define subcadena "")
  (define abre (length (search-all cadena #\()))
  (define cierra (length (search-all cadena #\))))
  (define subrespuesta 0)
  (define ultimo 0)
  (define cadena2 "")
  (define cadena3 "")
  
  (define (recorre)
    (if (and (> abre 1) (> cierra 1))
        (begin
          (set! ultimo (search-last cadena #\())
          (set! cadena2 (substring cadena ultimo (string-length cadena)))
          (set! cadena3 (string-replace cadena cadena2 ""))
          (set! subcadena (substring cadena ultimo (+ 1 (search-first cadena2 #\)) (string-length cadena3))))
          
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

;Función para verificar si un string es válido.
;Parámetros de entrada: str (string)
;Parámetros de salida: valido (boolean)
(define (isValido str)
  (define valido #t)

  (define longitud (string-length str))
  (define abre 0)
  (define cierra 0)
  (define espacios 0)

  (if (< longitud 0)
      (begin(* 5 3  )(* 5 3  )
        (set! valido #f)
      )
      (begin
        (set! abre (length (search-all str #\()))
        (set! cierra (length (search-all str #\))))
        (set! espacios (length (search-all str #\ )))
        
        (if (= abre 0) (set! valido #f))
        (if (= cierra 0) (set! valido #f))

        (if (number? (string->number str)) (set! valido #t))
        (if (espaciosJuntos str) (set! valido #f))
      )
  )

  valido
)

;Función para verificar si un string tiene varios espacios (#\ ) juntos.
;Parámetros de entrada: cadena (string)
;Parámetros de salida: hayJuntos (boolean)
(define (espaciosJuntos cadena)
  (define contador 0)
  (define longitud (string-length cadena))
  (define hayJuntos #f)
  (define caracterAnterior #\-)

  (define (recorre)
    (if (< contador longitud)
        (begin
          (if (string=? (string caracterAnterior) (string #\ ))
              (begin
                (if (string=? (string (string-ref cadena contador)) (string #\ )) (set! hayJuntos #t))
              )
          )

          (set! caracterAnterior (string-ref cadena contador))
          (set! contador (+ contador 1))
          (recorre)
         )
    )
  )
  (recorre)

  hayJuntos
)

;Función para verificar que la expresión ingresada sea válida (expresión normal, compuesta o "quit"),
;también se encarga de evitar que la calculadora "se cierre" después de realizar una operación.
(define (ciclo)
  (display "Calculadora >> ")
  (set! str (read-line))
  (set! respuesta 0)
  (if (not (string=? str "quit"))
      (begin
        (if (isValido str)
            (begin
              (display "respuesta >> ")
              (recursivo str)
            )
            (begin
              (display "ERROR! Expresion no valida")
            )
         )
        (newline)
        (ciclo)
      )
  )
)

;Programa principal
(define (main)
  (display "**** Ciencias de la Computacion - 2017 - Sección AN ****\n")
  (display "**** Proyecto Calculadora ****\n")
  (display "**** Rolando Pineda 17004816 ****\n")
  (display "**** José Loarca 17001087 ****\n\n")

  (ciclo)

  (display "Saliendo ...\n")
  (display "Gracias por usar nuestra calculadora")
)

;Ejecución programa principal
(main)
