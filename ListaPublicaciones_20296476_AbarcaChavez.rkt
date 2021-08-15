#lang racket
;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")

;TDA ListaPublicaciones
;Composicion: (Publicacion1 x Publicacion2 x ... x PublicacionN)
;            -> (TDA Publicacion x TDA Publicacion x ... x TDA Publicacion)

;Constructor
(define (CrearListaPublicaciones)(list null))

;Selectores
(define (getPublicacionXId IdP ListaPublicaciones)
      (if (= (length ListaPublicaciones) 0) ;No existe publicacion con ese id
          null
          (if (equal? (getIdP (car ListaPublicaciones)) IdP) ;Si se encontro publicacion
              (car ListaPublicaciones) ;Se retorna
              (getPublicacionXId IdP (cdr ListaPublicaciones))))) ;Se busca siguiente elemento

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
;Agregar publicacion (debe ser creada antes de ingresarse a esta funcion)
(define (AgregarPublicacion ListaPublicaciones Publicacion)
  (if (= (length ListaPublicaciones) 0)
      (cons Publicacion null)
      (cons (car ListaPublicaciones) (AgregarPublicacion (cdr ListaPublicaciones) Publicacion))))

;Agregar conjunto de publicaciones a compartir
(define (CompartirPublicacionesADestinos ListaPublicaciones PublicacionCompartida CuentaLogueada postID FechaComparte ListaDestinos CorrReacciones)
  (cond
    [(= (length ListaDestinos) 0) ListaPublicaciones]
    [else (CompartirPublicacionesADestinos (AgregarPublicacion ListaPublicaciones (CrearPublicacion (+ (+ (length ListaPublicaciones) 1) CorrReacciones) postID (getFechaRegistroP PublicacionCompartida) (getContenidoP PublicacionCompartida) (getAutorP PublicacionCompartida) (getDestinosP PublicacionCompartida) FechaComparte CuentaLogueada (car ListaDestinos))) PublicacionCompartida CuentaLogueada postID FechaComparte (cdr ListaDestinos) CorrReacciones)]))


;Otros
;Sin operaciones adicionales
