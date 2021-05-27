#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Cuenta_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA ListaCuentas
;Composicion: (Cuenta x Cuenta x Cuenta x ... x Cuenta)

;Constructor
(define (CrearListaCuentas)(list null))

;Pertenencia
;Recursion: De cola
(define (ListaCuentas? ListaCuentas)
  (if (not (list? ListaCuentas)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (if (= (length ListaCuentas) 0) ;Lista recorrida (o vacia) sirve como TDA ListaCuentas
          #t
          (if (Cuenta? (car ListaCuentas)) ;Elemento seleccionado debe ser TDA Cuenta
              (ListaCuentas? (cdr ListaCuentas)) ;Se revisa siguiente elemento
              #f)))) ;NO es TDA Cuenta, el parametro no vale como TDA ListaCuenta

;Pertenencia a usar para seguidores y siguiendo
;(define (RevisarContactosDeCadaCuenta ListaCuentasOr ListaCuentas UsuarioActual Seguidos Seguidores)


;Selectores
(define (getCuentaXUsuario Usuario ListaCuentas)
  (if (not (and (string? Usuario) (ListaCuentas? ListaCuentas)))
      null
      (if (eqv? (getUsuarioCuenta (car ListaCuentas)) Usuario)
          (car ListaCuentas)
          (getCuentaXUsuario (cdr ListaCuentas) Usuario))))


;Modificadores
;Al registrar un nuevo usuario, se debe registrar una cuenta asociada a este usuario (Para register)
(define (RegistrarCuenta Cuenta ListaCuentas)
  (if (= (length ListaCuentas) 0)
      (cons Cuenta null)
      (cons (car ListaCuentas) (RegistrarCuenta Cuenta (cdr ListaCuentas)))))

;Otros
