#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA Publicacion
;Composicion: (int x Fecha x string x string x string x string) -> (ID x FechaPublicacion x CuentaOrigenPublicacion x TipoDePublicacion x ContenidoPublicacion x CuentaDestinoPublicacion)

;Constructor
(define (CrearPublicacion id fecha origen tipo contenido comparte destino)
  (list id fecha origen tipo contenido comparte destino))

;Selectores
(define (getIdP Publicacion)(car Publicacion))
(define (getFechaP Publicacion)(cadr Publicacion))
(define (getCuentaOrigenP Publicacion)(caddr Publicacion))
(define (getTipoP Publicacion)(cadddr Publicacion))
(define (getContenidoP Publicacion)(car (cddddr Publicacion)))
(define (getCuentaQueComparteP Publicacion)(car (cdr (cddddr Publicacion))))
(define (getCuentaDestinoP Publicacion)(car (cddr (cddddr Publicacion))))

;Pertenencia
(define (Publicacion? Publicacion)
  (if (not (list? Publicacion)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length Publicacion) 7)) #f] ;TDA Publicacion tiene 6 elementos
        [(not (integer? (getIdP Publicacion))) #f] ;ID entero
        [(not (Fecha? (getFechaP Publicacion))) #f] ;TDA fecha
        [(not (string? (getCuentaOrigenP Publicacion))) #f] ;Usuario es string
        ;Tipo de publicacion debe ser string
        [(not (string? (getTipoP Publicacion))) #f]
        ;Tipo de publicacion debe estar dentro de las categorias mostradas a continuacion
        [(not (or
               (eqv? (getTipoP Publicacion) "foto")
               (eqv? (getTipoP Publicacion) "video")
               (eqv? (getTipoP Publicacion) "url")
               (eqv? (getTipoP Publicacion) "texto")
               (eqv? (getTipoP Publicacion) "audio"))) #f]
        ;Contenido de la publicacion debe ser string
        [(not (string? (getContenidoP Publicacion))) #f]
        ;Persona que comparte la publicacion debe ser un string
        [(not (string? (getCuentaQueComparteP Publicacion))) #f]
        ;Usuario que recibe la publicacion debe ser un string
        [(not (string? (getCuentaDestinoP Publicacion))) #f]
        ;Pruebas pasadas, el dato ingresado es TDA publicacion
        [else #t])))

;Modificadores

;Otros

