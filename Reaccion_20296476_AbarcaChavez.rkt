#lang racket
;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")

;TDA Reaccion
;Composicion: (IdR x IdER x FechaRegistroR x AutorR x TipoR x ContenidoR)
;            -> (int x int x TDA Fecha x string x string x string)

;Constructores
(define (CrearReaccion IdR IdER FechaR AutorR TipoR ContenidoR) (list IdR IdER FechaR AutorR TipoR ContenidoR))

;Selectores
(define (getIdR Reaccion)       (car Reaccion))
(define (getIdER Reaccion)      (cadr Reaccion))
(define (getFechaR Reaccion)    (caddr Reaccion))
(define (getAutorR Reaccion)    (cadddr Reaccion))
(define (getTipoR Reaccion)     (car (cddddr Reaccion)))
(define (getContenidoR Reaccion)(cadr (cddddr Reaccion)))

;Pertenencia
(define (Reaccion? Reaccion) 
  (if (not (list? Reaccion)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length Reaccion) 6)) #f] ;TDA Reaccion tiene 7 elementos
        [(not (integer? (getIdR Reaccion))) #f] ;ID publicacion es un entero
        [(not (integer? (getIdER Reaccion))) #f] ;ID elemento reaccionado es un entero
        [(not (Fecha? (getFechaR Reaccion))) #f] ;Fecha reaccion es TDA Fecha
        [(not (string? (getAutorR Reaccion))) #f] ;Cuenta que realiza reaccion se identifica con el nombre
        [(not (string? (getTipoR Reaccion))) #f] ;Tipo de reaccion es un string
        [(not (string? (getContenidoR Reaccion)))#f] ;Contenido de la reaccion es un string
        [else #t]))) ;Pruebas cumplidas, el elemento ingresado es un TDA Reaccion

;Modificadores
;Sin modificadores.

;Otros
;Sin operaciones adicionales