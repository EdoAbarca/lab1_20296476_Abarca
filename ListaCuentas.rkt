#lang racket
;Pedir archivos externos
(require "Fecha.rkt")
(require "Cuenta.rkt")

;Entregar funciones
(provide CrearListaCuentas)
(provide ListaCuentas?)
;(provide ContactosValidos)
(provide RegistrarCuenta)

;TDA ListaCuentas
;Composicion: (Cuenta x Cuenta x Cuenta x ... x Cuenta)

;Constructor
(define (CrearListaCuentas)(list null))

;Pertenencia
(define (ListaCuentas? ListaCuentas)
  (if (not (list? ListaCuentas))
      #f
      (if (= (length ListaCuentas) 0)
          #t
          (if (Cuenta? (car ListaCuentas))
              (ListaCuentas? (cdr ListaCuentas))
              #f))))

;Selectores
(define (getCuentaXNick nombre ListaCuentas)
  (if (not (and (string? nombre) (ListaCuentas? ListaCuentas)))
      null
      (if (eqv? (getUsuarioCuenta (car ListaCuentas)) nombre)
          (car ListaCuentas)
          (getCuentaXNick nombre ListaCuentas))))

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

;Modificadores
;Al registrar un nuevo usuario, se debe registrar una cuenta asociada a este usuario
(define (RegistrarCuenta Cuenta ListaCuentas)
  (if (= (length ListaCuentas) 0)
      (cons Cuenta null)
      (cons (car ListaCuentas) (RegistrarCuenta Cuenta (cdr ListaCuentas)))))

;Otros
