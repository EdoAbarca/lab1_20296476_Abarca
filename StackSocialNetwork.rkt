#lang racket
;Pedir archivos externos
(require "Cuenta.rkt")
(require "Usuario.rkt")
(require "Publicacion.rkt")
(require "Reaccion.rkt")
(require "Fecha.rkt")

;Entregar funciones
(provide StackVacio)
(provide StackRedSocial?)

;TDA stack social network
;Composicion: (UsuarioLogueado x ListaUsuarios x ListaCuentas x ListaPublicaciones x ListaReacciones)

;Constructor
;(define (StackRedSocial)(list null (crearListaUsuarios) (crearListaCuentas) (crearListaPublicaciones) (crearListaReacciones)))
(define (StackVacio) (list null null null null null))
;Selectores

(define (getUsuarioLogueado stack)(car stack))
(define (getListaUsuarios stack)(cadr stack))
(define (getListaCuentas stack)(caddr stack))
(define (getListaPublicaciones stack)(cadddr stack))
(define (getListaReacciones stack)(car (cddddr stack)))

;Pertenencia
(define (StackRedSocial? stack)
  (cond
    [(not (list? stack))#f] ;Todos los TDA se basan en listas
    [(not (= (length stack) 5))#f] ;Este TDA tiene 5 elementos
    [(not (list? (getUsuarioLogueado stack))) #f] ;Usuario logueado, contiene una lista con un string o una lista vacia (null)
    [(not (ListaUsuarios? (getListaUsuarios stack))) #f] ;2do elemento -> Lista TDA usuarios
    [(not (ListaCuentas? (getListaCuentas stack))) #f] ;3er elemento -> Lista TDA cuentas
    [(not (ListaPublicaciones? (getListaPublicaciones stack))) #f] ;4to elemento -> Lista TDA publicaciones
    [(not (ListaReacciones? (getListaReacciones stack))) #f] ;5to elemento -> Lista TDA reacciones
    [else #t]))

;Modificadores
;Modificador global, se encargara de actualizar cada elemento del stack
(define (ActualizarStack UsuarioLogueado ListaUsuarios ListaCuentas ListaPublicaciones ListaReacciones)
  (list UsuarioLogueado ListaUsuarios ListaCuentas ListaPublicaciones ListaReacciones))

;Otros
;Funcion que se encarga de convertir el stack de la red social a un string (para efectos de encriptamiento, desencriptamiento y socialnetwork -> string)
;(define (stack->string stack)(string-append (cuentas->string stack usuariologueado) (publicaciones->string stack usuariologueado) (reacciones->string stack usuariologueado))))
;Funciones adicionales de conversion de stack a string para cada substack del stack principal (basar en usuario logueado, para funcion socialnetwork -> string)


;AQUI SE ACTUALIZARA LA INFORMACION A OCURRIR EN LA RED SOCIAL, ESTARÁ EL ESPACIO PARA LOGUEO ACTUAL Y LOS TDA NECESARIOS