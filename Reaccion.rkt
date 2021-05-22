#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;TDA Reaccion
;Composicion: (int x int x Fecha x string x string) -> (IdPublicacion x IdReaccion x FechaReaccion x TipoReaccion x ContenidoReaccion) (Sujeto a cambios)

;Constructor
(define (crearReaccion idP idR FechaR TipoR ContenidoR) (list idP idR FechaR TipoR ContenidoR))

;Selectores
(define (getIdP Reaccion)(car Reaccion))
(define (getIdR Reaccion)(cadr Reaccion))
(define (getFechaR Reaccion)(caddr Reaccion))
(define (getTipoR Reaccion)(cadddr Reaccion))
(define (getContenidoR Reaccion)(car (cddddr Reaccion)))

;Pertenencia
(define (Reaccion? Reaccion)
  (if (not (list? Reaccion))
      #f
      (if (and
           (integer? (getIdP Reaccion))
           (integer? (getIdR Reaccion))
           (Fecha? (getFechaR Reaccion))
           (string? (getTipoR Reaccion))
           (string? (getContenidoR Reaccion)))
          #t
          #f)))

;Modificadores