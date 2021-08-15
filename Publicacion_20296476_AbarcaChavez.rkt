#lang racket
;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")

;TDA Publicacion
;Composicion: (IdP x IdPOriginal x FechaRegistroP x TipoP x ContenidoP x AutorP x DestinosP x ComparteP x RecibeP)
;            -> (int x int x TDA Fecha x string x string x string x string x list x string)

;Constructor
(define (CrearPublicacion IdP IdPOriginal FechaRegistroP ContenidoP AutorP DestinosP FechaComparteP ComparteP RecibeP)
  (list IdP IdPOriginal FechaRegistroP "Texto" ContenidoP AutorP DestinosP FechaComparteP ComparteP RecibeP))

;Segundo constructor, adaptado a la hora de compartir una publicacion

;Selectores
(define (getIdP Publicacion)            (car Publicacion))
(define (getIdOriginalP Publicacion)    (cadr Publicacion))
(define (getFechaRegistroP Publicacion) (caddr Publicacion))
(define (getTipoP Publicacion)          (cadddr Publicacion))
(define (getContenidoP Publicacion)     (car (cddddr Publicacion)))
(define (getAutorP Publicacion)         (cadr (cddddr Publicacion)))
(define (getDestinosP Publicacion)      (caddr (cddddr Publicacion)))
(define (getFechaComparteP Publicacion) (cadddr (cddddr Publicacion)))
(define (getComparteP Publicacion)      (car (cddddr (cddddr Publicacion))))
(define (getRecibeP Publicacion)        (cadr (cddddr (cddddr Publicacion))))

;Pertenencia
(define (Publicacion? Publicacion)
  (if (not (list? Publicacion)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length Publicacion) 10)) #f] ;TDA Publicacion tiene 9 elementos
        [(not (integer? (getIdP Publicacion))) #f] ;ID entero
        [(not (integer? (getIdOriginalP Publicacion))) #f] ;ID entero publicacion original
        [(not (Fecha? (getFechaRegistroP Publicacion))) #f] ;TDA fecha
        [(not (string? (getTipoP Publicacion))) #f] ;Tipo de publicacion debe ser string
        [(not (string? (getContenidoP Publicacion))) #f] ;Contenido de la publicacion debe ser string
        [(not (string? (getAutorP Publicacion))) #f] ;Usuario es string
        [(not (list? (getDestinosP Publicacion))) #f] ;Usuario destinado es string
        [(not (or (null? (getFechaComparteP Publicacion)) (Fecha? (getFechaComparteP Publicacion)))) #f]
        ;Persona que comparte la publicacion debe ser un string
        [(not (string? (getComparteP Publicacion))) #f]
        ;Usuario que recibe la publicacion debe ser un string
        [(not (string? (getRecibeP Publicacion))) #f]
        ;Pruebas pasadas, el dato ingresado es TDA publicacion
        [else #t])))

;Modificadores
;Sin modificadores.

;Otros
;Sin operaciones adicionales