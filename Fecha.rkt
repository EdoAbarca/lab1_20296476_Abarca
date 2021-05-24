#lang racket
;Entregar funciones
(provide Fecha)
(provide FechaDefecto)
(provide getDia)
(provide getMes)
(provide getAnio)
(provide Fecha?)

;TDA Fecha
;Composicion: (Dia x Mes x Anio) (int x int x int)

;Constructores
(define (Fecha Dia Mes Anio)(list Dia Mes Anio))
(define (FechaDefecto)(list 21 05 2021))

;Selectores
(define (getDia Fecha)(car Fecha))
(define (getMes Fecha)(cadr Fecha))
(define (getAnio Fecha)(caddr Fecha))

;Pertenencia
(define (Fecha? Fecha)
  (if (not (list? Fecha))
      #f
      (cond
        ;Largo TDA fecha debe ser siempre 3
        [(not (= (length Fecha) 3)) #f]
        ;Dominio por defecto de cada parametro TDA fecha
        [(or (> (getDia Fecha) 31) (< (getDia Fecha) 1)) #f]
        [(or (> (getMes Fecha) 12) (< (getMes Fecha) 1)) #f]
        [(or (> (getAnio Fecha) 2021) (< (getAnio Fecha) 1970)) #f]
        ;A la fecha de creacion de este laboratorio
        [(not(and (or (> (getDia Fecha) 22) (= (getDia Fecha) 22))
              (or (> (getMes Fecha) 5) (= (getMes Fecha) 5))
              (or (> (getAnio Fecha) 2021) (= (getAnio Fecha) 2021)))) #f]
        ;Respecto al mes escogido (meses con 28, 30 y 31 dias)
        ;Febrero tiene 28/29 dias, dependiendo del año (por defecto, Anio%4 == 0 implica anio bisiesto, 29 dias)
        [(not (and (= (getMes Fecha) 2)  (< (getDia Fecha) 30) (= (remainder (getAnio Fecha) 4) 0)))#f]
        [(not (and (= (getMes Fecha) 2)  (< (getDia Fecha) 29) (not (= (remainder (getAnio Fecha) 4) 0))))#f]
        ;Los siguientes meses tienen 31 dias
        [(not (and (or (= (getMes Fecha) 1)
                       (= (getMes Fecha) 3)
                       (= (getMes Fecha) 5)
                       (= (getMes Fecha) 7)
                       (= (getMes Fecha) 8)
                       (= (getMes Fecha) 10)
                       (= (getMes Fecha) 12)) (< getDia 32)))#f]
        ;Los siguientes meses tienen 30 dias
        [(not (and (or (= (getMes Fecha) 4) (= (getMes Fecha) 6)(= (getMes Fecha) 9)(= (getMes Fecha)11)) (< getDia 31)))#f]
        ;Pruebas pasadas, el dato ingresado corresponde a TDA fecha
        [else #t])))

;NECESITA DEBUG, COMPROBAR POR STRINGS