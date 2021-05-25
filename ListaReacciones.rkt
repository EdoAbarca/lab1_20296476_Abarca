#lang racket
;Pedir archivos externos
(require "Fecha.rkt")
(require "Reaccion.rkt")

;Entregar funciones
(provide CrearListaReacciones)
(provide ListaReacciones?)

;TDA ListaReacciones
;Composicion: (Reaccion x Reaccion x ... x Reaccion)

;Constructor
(define (CrearListaReacciones)(list null))

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