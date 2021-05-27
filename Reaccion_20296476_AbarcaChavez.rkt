#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA Reaccion
;Composicion: (int x int x Fecha x string x string x string) -> (IdPublicacionReaccionada x IdReaccion x FechaReaccion x CuentaReaccion x TipoReaccion x ContenidoReaccion) (Sujeto a cambios)

;Constructor
(define (CrearReaccion idPR idR CuentaR FechaR TipoR ContenidoR) (list idPR idR CuentaR FechaR TipoR ContenidoR))

;Selectores
(define (getIdPR Reaccion)(car Reaccion))
(define (getIdR Reaccion)(cadr Reaccion))
(define (getFechaR Reaccion)(caddr Reaccion))
(define (getCuentaR Reaccion)(cadddr Reaccion))
(define (getTipoR Reaccion)(car (cddddr Reaccion)))
(define (getContenidoR Reaccion)(car (cdr (cddddr Reaccion))))

;Pertenencia
(define (Reaccion? Reaccion) 
  (if (not (list? Reaccion)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length Reaccion) 6)) #f] ;TDA Reaccion tiene 6 elementos
        [(not (integer? (getIdPR Reaccion))) #f] ;ID publicacion es un entero
        [(not (integer? (getIdR Reaccion))) #f] ;ID reaccion es un entero
        [(not (Fecha? (getFechaR Reaccion))) #f] ;Fecha reaccion es TDA Fecha
        [(not (string? (getCuentaR Reaccion))) #f] ;Cuenta que realiza reaccion se identifica con el nombre
        [(not (string? (getTipoR Reaccion))) #f] ;Tipo de reaccion es un string
        ;Tipo de reaccion debe cumplir un dominio
        [(not (or (eqv? (getTipoR Reaccion) "Like") (eqv? (getTipoR Reaccion) "Comentario"))) #f]
        [(not (string? (getContenidoR Reaccion))) #f] ;Contenido de la reaccion es un string
        [else #t]))) ;Pruebas cumplidas, el elemento ingresado es un TDA Reaccion

;Modificadores

;Otros

