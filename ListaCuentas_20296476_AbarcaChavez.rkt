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
;Selector comparando por el usuario
;Dominio: string x ListaCuentas
;Recorrido: TDA Cuenta/null
(define (getCuentaXUsuario UsuarioCuenta ListaCuentas)
  (if (not (and (string? UsuarioCuenta) (ListaCuentas? ListaCuentas))) ;Datos erroneos
      null
      (if (= (length ListaCuentas) 0) ;No existe cuenta con ese usuario
          null
          (if (eqv? (getUsuarioCuenta (car ListaCuentas)) UsuarioCuenta) ;Si se encontro cuenta
              (car ListaCuentas) ;Se retorna
              (getCuentaXUsuario (cdr ListaCuentas) UsuarioCuenta))))) ;Se busca siguiente elemento


;Modificadores
;Al registrar un nuevo usuario, se debe registrar una cuenta asociada a este usuario (Para register)
(define (RegistrarCuenta ListaCuentas Cuenta)
  (if (= (length ListaCuentas) 0) ;Final lista
      (cons Cuenta null) ;Se guarda nueva cuenta
      (cons (car ListaCuentas) (RegistrarCuenta (cdr ListaCuentas) Cuenta)))) ;Se va generando la nueva lista...

;Otros
;Filtrar cuentas que no esten en contactos de usuario
;Dominio: list x ListaCuentas x string
;Recorrido: list
(define (FiltrarCompartidos ListaComp ListaCuentas CuentaLogueada)
  (if (= (length ListaComp) 0)
      null
      (if (EstaEnContactos (car ListaComp) (getSeguidosCuenta (getCuentaXUsuario ListaCuentas CuentaLogueada)) (getSeguidoresCuenta (getCuentaXUsuario ListaCuentas CuentaLogueada)))
          (cons (car ListaComp) (FiltrarCompartidos (cdr ListaComp) ListaCuentas CuentaLogueada))
          (FiltrarCompartidos (cdr ListaComp) ListaCuentas CuentaLogueada))))
