#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide ListaReacciones?)

;TDA Reaccion
;Composicion: (int x int x Fecha x string x string x string) -> (IdPublicacion x IdReaccion x FechaReaccion x CuentaReaccion x TipoReaccion x ContenidoReaccion) (Sujeto a cambios)

;Constructor
(define (CrearReaccion idP idR CuentaR FechaR TipoR ContenidoR) (list idP idR CuentaR FechaR TipoR ContenidoR))

;Selectores
(define (getIdP Reaccion)(car Reaccion))
(define (getIdR Reaccion)(cadr Reaccion))
(define (getFechaR Reaccion)(caddr Reaccion))
(define (getCuentaR Reaccion)(cadddr Reaccion))
(define (getTipoR Reaccion)(car (cddddr Reaccion)))
(define (getContenidoR Reaccion)(car (cdr (cddddr Reaccion))))

;Pertenencia
(define (Reaccion? Reaccion)
  (if (not (list? Reaccion))
      #f
      (cond
        [(not (= (length Reaccion) 6)) #f] ;TDA Reaccion tiene 6 elementos
        [(not (integer? (getIdP Reaccion))) #f] ;ID publicacion es un entero
        [(not (integer? (getIdR Reaccion))) #f] ;ID reaccion es un entero
        [(not (Fecha? (getFechaR Reaccion))) #f] ;Fecha reaccion es TDA Fecha
        [(not (string? (getCuentaR Reaccion))) #f] ;Cuenta que realiza reaccion se identifica con el nombre
        [(not (string? (getTipoR Reaccion))) #f] ;Tipo de reaccion es un string
        [(not (string? (getContenidoR Reaccion))) #f] ;Contenido de la reaccion es un string
        [else #t]))) ;Pruebas cumplidas, el elemento ingresado es un TDA Reaccion

;Modificadores

;Otros

;TDA lista reacciones
;Composicion: (Reaccion x Reaccion x ... x Reaccion)

;Constructor
(define (ListaReacciones)(list null))

;Selectores

;Pertenencia
(define (ListaReacciones? ListaReacciones)
  (if (not (list? ListaReacciones)) ;Lista de listas
      #f
      (if (= (length ListaReacciones) 0) ;Si se revisaron todos los elementos/la lista esta vacia, es TDA ListaReacciones
          #t
          (if (Reaccion? (car ListaReacciones)) ;Si el elemento actual es TDA reaccion
              (ListaReacciones? (cdr ListaReacciones)) ;Se revisa el siguiente elemento
              #f)))) ;La lista ingresada no es TDA ListaReacciones

;Modificadores

;Otros