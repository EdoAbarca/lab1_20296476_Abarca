#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA ListaReacciones
;Composicion: (Reaccion x Reaccion x ... x Reaccion)

;Constructor
(define (CrearListaReacciones)(list null))

;Selectores

;Pertenencia
(define (ListaReacciones? ListaReacciones)
  (if (not (list? ListaReacciones)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (if (= (length ListaReacciones) 0) ;Si se revisaron todos los elementos/la lista esta vacia, es TDA ListaReacciones
          #t
          (if (Reaccion? (car ListaReacciones)) ;Elemento seleccionado debe ser TDA Reaccion
              (ListaReacciones? (cdr ListaReacciones)) ;Se revisa el siguiente elemento
              #f)))) ;NO es TDA Reaccion, el parametro no vale como TDA ListaReacciones

;Modificadores

;Otros