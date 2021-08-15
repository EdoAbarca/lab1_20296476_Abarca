#lang racket
;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")

;TDA CuentaUsuario
;Composicion: (UsuarioCuenta x ContraseniaCuenta x FechaRegistroC x ListaSeguidos x ListaSeguidores)
;            -> (string x string x TDA Fecha x list x list)

;Constructores
(define (CrearCuentaUsuario UsuarioCuenta ContraseniaCuenta FechaRegistroC)
  (list UsuarioCuenta ContraseniaCuenta FechaRegistroC null null))

;Selectores
(define (getUsuarioCuenta CuentaUsuario) (car CuentaUsuario))
(define (getContraseniaCuenta CuentaUsuario) (cadr CuentaUsuario))
(define (getFechaRegistroC CuentaUsuario) (caddr CuentaUsuario))
(define (getSeguidoresCuenta CuentaUsuario) (cadddr CuentaUsuario))
(define (getSeguidosCuenta CuentaUsuario) (car (cddddr CuentaUsuario)))

;Pertenencia
(define (CuentaUsuario? CuentaUsuario)
  (if (not (list? CuentaUsuario)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length CuentaUsuario) 5)) #f] ;TDA Cuenta tiene 4 elementos
        [(not (string? (getUsuarioCuenta CuentaUsuario))) #f] ;Nombre usuario es string
        [(not (string? (getContraseniaCuenta CuentaUsuario))) #f]
        [(not (Fecha? (getFechaRegistroC CuentaUsuario))) #f] ;Fecha registro es TDA Fecha
        [(not (list? (getSeguidoresCuenta CuentaUsuario))) #f] ;Seguidores cuenta es una lista
        [(not (list? (getSeguidosCuenta CuentaUsuario))) #f] ;Seguidos cuenta es una lista
        [else #t]))) ;Pruebas pasadas, elemento en parametro es TDA Cuenta


;Modificadores
;Modificar lista de seguidos, agregando el nuevo usuario (la existencia del usuario debe estar verificada)
(define (AgregarSeguimiento CuentaASeguir Fecha ListaSeguidosAct)
  (cond
    [(= (length ListaSeguidosAct) 0) (cons (list CuentaASeguir Fecha) null)]
    [else (cons (car ListaSeguidosAct) (AgregarSeguimiento CuentaASeguir Fecha (cdr ListaSeguidosAct)))]))

;Modificar lista seguidores, agregando al usuario seguido la cuenta la cual se efectuo el seguimiento (la existencia del usuario debe estar verificada)
(define (AgregarSeguidor CuentaSeguida Fecha ListaSeguidoresAct)
  (cond
    [(= (length ListaSeguidoresAct) 0) (cons (list CuentaSeguida Fecha) null)]
    [else (cons (car ListaSeguidoresAct) (AgregarSeguidor CuentaSeguida Fecha (cdr ListaSeguidoresAct)))]))


;Modificar TDA CuentaUsuario, actualizando ambos casos anteriormente presentados:
;Actualizar seguidos
(define (ActualizarSeguidosCuenta CuentaOr ListaSeguidosAct)
  (list (getUsuarioCuenta CuentaOr) (getContraseniaCuenta CuentaOr) (getFechaRegistroC CuentaOr) (getSeguidoresCuenta CuentaOr) ListaSeguidosAct))

;Actualizar seguidores
(define (ActualizarSeguidoresCuenta CuentaOr ListaSeguidoresAct)
  (list (getUsuarioCuenta CuentaOr) (getContraseniaCuenta CuentaOr) (getFechaRegistroC CuentaOr) ListaSeguidoresAct (getSeguidosCuenta CuentaOr)))

;Otros
;Funcion para verificar si usuario ingresado esta en contactos (i.e., lista seguidos)
(define (EstaEnContactos CuentaApuntada ListaSeguidosLog)
  (cond
    [(null? ListaSeguidosLog) #f]
    [(= (length ListaSeguidosLog) 0) #f]
    [(eqv? CuentaApuntada (caar ListaSeguidosLog)) #t]
    [else (EstaEnContactos CuentaApuntada (cdr ListaSeguidosLog))]))

;Funcion para validar si los destinos ingresados pertenecen a los contactos de la cuenta logueada
(define (ValidarDestinos CuentaLogueada ListaDestinos)
  (if (= (length ListaDestinos) 0)
      #t
      (if (EstaEnContactos (car ListaDestinos) (getSeguidosCuenta CuentaLogueada))
          (ValidarDestinos CuentaLogueada (cdr ListaDestinos))
          #f)))

;Pasar cuenta a string

;RECORDATORIO: EN ESTA VARIANTE HAY QUE REGISTRAR LA FECHA DE SEGUIMIENTO Y EL NOMBRE DE LA CUENTA SEGUIDA, EVALUAR LA CREACION DE UN TDA