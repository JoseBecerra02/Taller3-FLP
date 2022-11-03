#lang eopl

; Sergio Escudero Tabares - 2040801
; Jose Miguel Becerra Casierra - 2043246
; Natalia Andrea Marín Hernandez - 2041622
; Esteban Andres Hernandez - 2042817
; Juan Esteban Brand Tovar - 2043291

;;Definicion de interfaces 
(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successor(lambda (n) (+ n 1)))
(define predecessor (lambda  (n) (- n 1)))
;;Codigo cliente
;;Suma dos numeros dados
(define suma
  (lambda (x y)
         (if(is-zero? x)
            y
            (successor(suma(predecessor x) y))
          )
  )
 )
;;Multiplica dos numeros dados
(define multiplicar
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma y(multiplicar(predecessor x)y)))
    )
)
;;Resta dos numeros dados
(define resta
  (lambda (x y)
         (if(is-zero? y)
            x
            (predecessor(resta x  (predecessor y)))
          )
  )
 )
;;Divide dos numeros dados
(define division
  (lambda (n ite)
    (if(and (or (eqv? (multiplicar 16 ite) n) (< (multiplicar 16 ite) n)) (> (multiplicar 16 (suma ite 1)) n) )
       ite
       (division n (suma ite 1))
    )
   )
 )
;;Devuelve el residuo de una division
(define residuo
  (lambda (n)
    (resta n (multiplicar (division n  0) 16))
   )
 )
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicar n (factorial (predecessor n)))
     )
  )
 )
;;Hace divisiones recursivas del cociente con el divisor(16)  y devuelve una lista de residuos
(define aux-bignum
  (lambda (x y)
    (if (or (is-zero? y) (< x 16))
        (cons y (cons x '()))
        (cons y (aux-bignum (car (cons (division x 0) (cons (residuo x) '()))) (cadr (cons (division x 0) (cons (residuo x) '())))))
     )
   )
 )
;;bignum
;;Proposito: procedimiento que convierte un numero entero en una lista de enteros en notacion hexadecimal 
;;n -> l
;;<n>::= <int>
;;<l>::=lista(<valor>)
;;<valor>::= <int> <valor> | empty
(define bignum
  (lambda (n)
    (aux-bignum (car (cons (division n 0) (cons (residuo n) '()))) (cadr (cons (division n 0) (cons (residuo n) '())))) 
   )
 )
;;Pruebas
(bignum 17)
(bignum 4419)
(bignum 999)
(bignum 844)