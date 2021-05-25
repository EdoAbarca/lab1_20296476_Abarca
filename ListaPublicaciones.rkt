#lang racket
;Pedir archivos externos
(require "Fecha.rkt")
(require "Publicacion.rkt")

;Entregar funciones
(provide CrearListaPublicaciones)
(provide ListaPublicaciones?)

;TDA ListaPublicaciones
;Composicion: (Publicacion x Publicacion x ... x Publicacion)

;Constructor
(define (CrearListaPublicaciones)(list null))

;Selectores
;(define (getPublicacionXID ID ListaPublicaciones)())

;Pertenencia
(define (ListaPublicaciones? ListaPublicaciones)
  (if (not (list? ListaPublicaciones)) ;TDA ListaPublicaciones deben ser listas
      #f
      (if (= (length ListaPublicaciones) 0) ;Se llegó al final de la lista, sea vacia o se haya recorrido, es TDA lista publicaciones
          #t
          (if (Publicacion? (car ListaPublicaciones)) ;Si el elemento actual corresponde a un TDA publicacion
              (ListaPublicaciones? (cdr ListaPublicaciones)) ;Se revisa el siguiente elemento de la lista
              #f))));Si no, la lista no cumple con la composición especificada, no es TDA lista publicaciones

;Modificadores

;Otros