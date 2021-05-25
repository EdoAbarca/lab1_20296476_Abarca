#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide ListaPublicaciones?)

;TDA Publicacion
;Composicion: (int x Fecha x string x string x string x list) -> (ID x FechaPublicacion x usuarioPublicacion x TipoDePublicacion x ContenidoPublicacion x UsuariosCompartidos) (Sujeto a cambios)

;Constructor
(define (crearPublicacion id fecha usuario tipo contenido compartidos)
  (list id fecha usuario tipo contenido compartidos))

;Selectores
(define (getIdP Publicacion)(car Publicacion))
(define (getFechaP Publicacion)(cadr Publicacion))
(define (getUsuarioP Publicacion)(caddr Publicacion))
(define (getTipoP Publicacion)(cadddr Publicacion))
(define (getContenidoP Publicacion)(car (cddddr Publicacion)))
(define (getCompartidosP Publicacion)(car (cdr (cddddr Publicacion))))

;Pertenencia
(define (Publicacion? Publicacion)
  (if (not (list? Publicacion)) ;TDA se basa en lista, debe ser si o si una
      #f
      (cond
        [(not (= (length Publicacion) 6)) #f] ;TDA de 5 elementos
        [(not (integer? (getIdP Publicacion))) #f] ;ID entero
        [(not (Fecha? (getFechaP Publicacion))) #f] ;TDA fecha
        [(not (string? (getUsuarioP Publicacion))) #f] ;Usuario es string
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
        ;Usuarios compartidos es una lista de strings (o null si esta vacia, pero siempre lista)
        [(not (list? (getCompartidosP Publicacion))) #f]
        ;Pruebas pasadas, el dato ingresado es TDA publicacion
        [else #t])))

;Modificadores

;Otros

;TDA lista publicaciones
;Composicion: (Publicacion x Publicacion x ... x Publicacion)

;Constructor
(define (crearListaPublicaciones)(list null))

;Selectores
;(define (getPublicacionXID ID ListaPublicaciones)())

;Pertenencia
(define (ListaPublicaciones? ListaPublicaciones)
  (if (not (list? ListaPublicaciones)) ;Ambos TDA's deben ser listas
      #f
      (if (= (length ListaPublicaciones) 0) ;Se llegó al final de la lista, sea vacia o se haya recorrido, es TDA lista publicaciones
          #t
          (if (Publicacion? (car ListaPublicaciones)) ;Si el elemento actual corresponde a un TDA publicacion
              (ListaPublicaciones? (cdr ListaPublicaciones)) ;Se revisa el siguiente elemento de la lista
              #f))));Si no, la lista no cumple con la composición especificada, no es TDA lista publicaciones

;Modificadores

;Otros