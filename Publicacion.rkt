#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;TDA Publicacion
;Composicion: (int x date x string x string x string) -> (ID x FechaPublicacion x usuarioPublicacion x TipoDePublicacion x ContenidoPublicacion) (Sujeto a cambios)

;Constructor
(define (crearPublicacion id fecha usuario tipo contenido)(list id fecha usuario tipo contenido))

;Selectores
(define (getIdP Publicacion)(car Publicacion))
(define (getFechaP Publicacion)(cadr Publicacion))
(define (getUsuarioP Publicacion)(caddr Publicacion))
(define (getTipoP Publicacion)(cadddr Publicacion))
(define (getContenidoP Publicacion)(car (cddddr Publicacion)))

;Pertenencia
(define (Publicacion? Publicacion)
  (if (or (null? Publicacion) (not (list? Publicacion)))
      #f
      (if (and
           (integer? (getIdP Publicacion))
           (Fecha? (getFechaP Publicacion))
           (string? (getUsuarioP Publicacion)) ;Posible modificacion, se debe comprobar que el usuario existe
           (and (string? (getTipoP Publicacion)) (or
                                                (eqv? (getTipoP Publicacion) "foto")
                                                (eqv? (getTipoP Publicacion) "video")
                                                (eqv? (getTipoP Publicacion) "url")
                                                (eqv? (getTipoP Publicacion) "texto")
                                                (eqv? (getTipoP Publicacion) "nomeacuerdo")))
           (string? (getContenidoP Publicacion)))
          #t
          #f)))

;Modificadores