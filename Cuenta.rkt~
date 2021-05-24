#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;TDA Cuenta
;Composicion: (NombreUsuario x TipoUsuario x ListaSeguidos x ListaSeguidosVuelta) (string x string x list x list)

;Constructor
(define (CrearCuenta usuario tipo seguidores siguiendo)(list usuario tipo seguidores siguiendo))

;Selectores
(define (getUsuarioCuenta Cuenta)(car Cuenta))
(define (getTipoCuenta Cuenta)(cadr Cuenta))
(define (getSeguidoresCuenta Cuenta)(caddr Cuenta))
(define (getSiguiendoCuenta Cuenta)(cadddr Cuenta))

;Pertenencia
(define (Cuenta? Cuenta)
  (if (or (null? Cuenta) (not (list? Cuenta)))
      #f
      (cond
        [(not (= (length Cuenta) 4)) #f]
        [(not (string? (getUsuarioCuenta Cuenta))) #f]
        [(not (string? (getTipoCuenta Cuenta))) #f]
        [(not (list? (getSeguidoresCuenta Cuenta))) #f]
        [(not (list? (getSiguiendoCuenta Cuenta))) #f]
        [else #t])))

;Modificadores
;Agregar seguido
;(define (agregarSeguimiento usuarioASeguir CuentaActual))

;Otros

;TDA ListaCuenta
;Composicion: (Cuenta x Cuenta x Cuenta x ... x Cuenta)

;Constructor
(define (crearListaCuentas)(list null))

;Selectores
(define (getCuentaXNick nombre ListaCuentas)
  (if (not (and (string? nombre) (ListaCuentas? ListaCuentas)))
      null
      (if (eqv? (getUsuarioCuenta (car ListaCuentas)) nombre)
          (car ListaCuentas)
          (getCuentaXNick nombre ListaCuentas))))

;Pertenencia
(define (ListaCuentas? ListaCuentas)
  (if (not (list? ListaCuentas))
      #f
      (if (= (length ListaCuentas) 0)
          #t
          (if (Cuenta? (car ListaCuentas))
              (ListaCuentas? (cdr ListaCuentas))
              #f))))

;Modificadores

;Otros