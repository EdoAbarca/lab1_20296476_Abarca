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
;Modificador para funcion "post", que se encargara de ingresar en la lista publicaciones todas las publicaciones nacidas al llamar a esta funcion
;Dominio: (ListaPublicaciones x integer x Fecha x string x list)
;Recorrido: ListaPublicaciones
(define (AgregarNuevasPublicaciones ListaPublicaciones Largo Fecha CuentaLogueada Contenido CuentasDestino)
  (if (and (ListaPublicaciones? ListaPublicaciones) (integer? Largo) (string? CuentaLogueada) (Fecha? Fecha) (string?) (list? CuentasDestino))
      (cond
        [(not (= (length ListaPublicaciones) 0)) (cons (car ListaPublicaciones) (AgregarNuevasPublicaciones (cdr ListaPublicaciones) Largo Fecha CuentaLogueada Contenido CuentasDestino))]
        [else (cond
                [(not (= (length CuentasDestino) 0)) (cons (CrearPublicacion (+ Largo 1) Fecha CuentaLogueada (string-append Contenido "[COMPARTIDO]") (car CuentasDestino)) (AgregarNuevasPublicaciones ListaPublicaciones Largo Fecha CuentaLogueada (cdr CuentasDestino)))]
                [else (cons (CrearPublicacion (+ Largo 1)) null)])])
      ListaPublicaciones))

;Otros