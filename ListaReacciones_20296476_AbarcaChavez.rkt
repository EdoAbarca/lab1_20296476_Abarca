#lang racket
;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")

;TDA Publicacion
;Composicion: (Reaccion1 x Reaccion2 x ... x ReaccionN)
;            -> (TDA Reaccion x TDA Reaccion x ... x TDA Reaccion)

;Constructor
(define (CrearListaReacciones)(list null))

;Selectores
(define (getReaccionXIdR IdR ListaReacciones)
  (if (not (integer? IdR))
      null
      (if (= (length ListaReacciones) 0) ;No existe reaccion con ese id
          null
          (if (eqv? (getIdR (car ListaReacciones)) IdR) ;Si se encontro reaccion
              (car ListaReacciones) ;Se retorna
              (getReaccionXIdR IdR (cdr ListaReacciones)))))) ;Se busca siguiente elemento

;Pertenencia
(define (ListaReacciones? ListaReacciones)
      (if (= (length ListaReacciones) 0) ;Si se revisaron todos los elementos/la lista esta vacia, es TDA ListaReacciones
          #t
          (if (Reaccion? (car ListaReacciones)) ;Elemento seleccionado debe ser TDA Reaccion
              (ListaReacciones? (cdr ListaReacciones)) ;Se revisa el siguiente elemento
              #f))) ;NO es TDA Reaccion, el parametro no vale como TDA ListaReacciones

;Modificadores
;Agregar reaccion (debe ser creada antes de ingresarse a esta funcion)
(define (AgregarReaccion ListaReacciones Reaccion)
  (if (= (length ListaReacciones) 0)
      (cons Reaccion null)
      (cons (car ListaReacciones) (AgregarReaccion (cdr ListaReacciones) Reaccion))))

;Otros
;Sin operaciones adicionales