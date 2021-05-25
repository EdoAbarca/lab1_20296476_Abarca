#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide ListaCuentas?)

;TDA Cuenta
;Composicion: (NombreUsuario x ListaSeguidos x ListaSeguidosVuelta) (string x list x list)

;Constructor
(define (CrearCuenta usuario seguidores siguiendo)(list usuario seguidores siguiendo))

;Selectores
(define (getUsuarioCuenta Cuenta)(car Cuenta))
(define (getSeguidoresCuenta Cuenta)(cadr Cuenta))
(define (getSiguiendoCuenta Cuenta)(caddr Cuenta))

;Pertenencia
;Pertenencia para cada lista seguidos y seguidores
(define (ContactosValidos ListaCuentasOr ListaCuentas Contactos UsuarioActual)
  (if (and (ListaCuentas? ListaCuentasOr) (ListaCuentas? ListaCuentas) (list? Contactos))
      (cond
        [(= (length ListaCuentas) 0) #f]
        [(eqv? (car Contactos) UsuarioActual) #f]
        [(eqv? (car Contactos) (getUsuarioCuenta (car ListaCuentas))) (ContactosValidos ListaCuentasOr ListaCuentasOr (cdr Contactos))]
        [(= (length Contactos) 0) #t]
        [else (ContactosValidos ListaCuentasOr (cdr ListaCuentas) Contactos)])
      #f))

;Funcion principal de pertenencia
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
;Al registrar un nuevo usuario, se debe ingresar una nueva cuenta
(define (RegistrarCuenta Cuenta ListaCuentas)
  (if (= (length ListaCuentas) 0)
      (cons Cuenta null)
      (cons (car ListaCuentas) (RegistrarCuenta Cuenta (cdr ListaCuentas)))))

;Otros