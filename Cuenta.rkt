#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide CrearCuenta)
(provide getUsuarioCuenta)
(provide getSeguidoresCuenta)
(provide getSiguiendoCuenta)
(provide Cuenta?)


;TDA Cuenta
;Composicion: (NombreUsuario x ListaSeguidos x ListaSeguidosVuelta) (string x list x list)

;Constructor
(define (CrearCuenta usuario seguidores siguiendo)(list usuario seguidores siguiendo))

;Selectores
(define (getUsuarioCuenta Cuenta)(car Cuenta))
(define (getSeguidoresCuenta Cuenta)(cadr Cuenta))
(define (getSiguiendoCuenta Cuenta)(caddr Cuenta))

;Pertenencia
(define (Cuenta? Cuenta)
  (if (not (list? Cuenta))
      #f
      (cond
        [(not (= (length Cuenta) 3)) #f]
        [(not (string? (getUsuarioCuenta Cuenta))) #f]
        [(not (list? (getSeguidoresCuenta Cuenta))) #f] ;Verificar de lista usuarios
        [(not (list? (getSiguiendoCuenta Cuenta))) #f] ;Verificar de lista usuarios
        [else #t])))

;Modificadores
;Modificar lista de seguidos, agregando el nuevo usuario
;(define (agregarSeguimiento CuentaASeguir CuentaActual))



;Otros
;Verificar si usuario ingresado esta en lista seguidores
;Verificar si usuario ingresado esta en lista seguidos

