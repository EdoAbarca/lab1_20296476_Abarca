#lang racket
;Entregar funciones
(provide (all-defined-out))

;TDA Fecha
;Composicion: (Dia x Mes x Anio) (int x int x int)

;Constructores
(define (Fecha Dia Mes Anio)(list Dia Mes Anio))
(define (FechaDefecto)(list 27 05 2021)) ;A la entrega de este laboratorio

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
        ;A la fecha de entrega de este laboratorio
        [(and (or (> (getDia Fecha) 27) (= (getDia Fecha) 27))
              (or (> (getMes Fecha) 5) (= (getMes Fecha) 5))) #f]
        ;Respecto al mes escogido (meses con 28, 30 y 31 dias)
        ;Febrero tiene 28/29 dias, dependiendo del año (por defecto, Anio%(remainder)4 == 0 implica anio bisiesto, 29 dias)
        [(and (= (getMes Fecha) 2) (= (remainder (getAnio Fecha) 4) 0)) (not (< (getDia Fecha) 30)) #f]
        [(and (= (getMes Fecha) 2) (not (= (remainder (getAnio Fecha) 4) 0)) (not (< (getDia Fecha) 29))) #f]
        ;Los siguientes meses tienen 31 dias
        [(and (or (= (getMes Fecha) 1)
                       (= (getMes Fecha) 3)
                       (= (getMes Fecha) 5)
                       (= (getMes Fecha) 7)
                       (= (getMes Fecha) 8)
                       (= (getMes Fecha) 10)
                       (= (getMes Fecha) 12)) (not (< (getDia Fecha) 32))) #f]
        ;Los siguientes meses tienen 30 dias
        [(and (or (= (getMes Fecha) 4)
                  (= (getMes Fecha) 6)
                  (= (getMes Fecha) 9)
                  (= (getMes Fecha) 11)) (not (< (getDia Fecha) 31))) #f]
        ;Pruebas pasadas, el dato ingresado corresponde a TDA fecha
        [else #t])))

;SI SE DESEA COMPROBAR LA VERACIDAD DE ESTA FUNCION, REEMPLAZAR #f POR NUMEROS Y REVISAR CADA CASO