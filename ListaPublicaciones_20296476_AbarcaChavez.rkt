#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA ListaPublicaciones
;Composicion: (Publicacion x Publicacion x ... x Publicacion)

;Constructor
(define (CrearListaPublicaciones)(list null))

;Selectores
;(define (getPublicacionXID ID ListaPublicaciones)())

;Pertenencia
(define (ListaPublicaciones? ListaPublicaciones)
  (if (not (list? ListaPublicaciones)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (if (= (length ListaPublicaciones) 0) ;Lista recorrida (o vacia) sirve como TDA ListaPublicaciones
          #t
          (if (Publicacion? (car ListaPublicaciones)) ;Elemento seleccionado debe ser TDA Publicacion
              (ListaPublicaciones? (cdr ListaPublicaciones)) ;Se revisa siguiente elemento
              #f)))) ;NO es TDA Publicacion, el parametro no vale como TDA ListaPublicaciones

;Modificadores

;Otros