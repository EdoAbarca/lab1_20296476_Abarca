#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA Cuenta
;Composicion: (UsuarioCuenta x FechaRegistro x ListaSeguidores x ListaSeguidos) (string x Fecha x list x list)
;Composicion ListaSeguidores/ListaSeguidos: (string x Fecha)

;Constructor
(define (CrearCuenta usuario fecha seguidores siguiendo)(list usuario fecha seguidores siguiendo))

;Selectores
(define (getUsuarioCuenta Cuenta)(car Cuenta))
(define (getFechaRegistroC Cuenta) (cadr Cuenta))
(define (getSeguidoresCuenta Cuenta)(caddr Cuenta))
(define (getSeguidosCuenta Cuenta)(cadddr Cuenta))

;Pertenencia
;Pertenencia para las listas de seguidos y seguidores (solo si cumple firma, veracidad de cuentas se comprueba en TDA ListaCuentas)
;Dominio: list
;Recorrido: bool
;Recursion: De cola
(define (ListaContactos? ListaContactos)
  (cond
    [(= (length ListaContactos) 0) #t]
    [(not (string? (car ListaContactos))) #f]
    [(not (Fecha? (cadr ListaContactos))) #f]
    [else (ListaContactos? (cdr ListaContactos))]))

;Pertenencia principal
(define (Cuenta? Cuenta)
  (if (not (list? Cuenta)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length Cuenta) 4)) #f] ;TDA Cuenta tiene 4 elementos
        [(not (string? (getUsuarioCuenta Cuenta))) #f] ;Nombre usuario es string
        [(not (Fecha? (getFechaRegistroC Cuenta))) #f] ;Fecha registro es TDA Fecha
        [(not (list? (getSeguidoresCuenta Cuenta))) #f] ;Seguidores cuenta es una lista
        [(not (ListaContactos? (getSeguidoresCuenta Cuenta)))#f] ;Se verifica veracidad en lista usuarios
        [(not (list? (getSeguidosCuenta Cuenta))) #f] ;S cuenta es una lista
        [(not (ListaContactos? (getSeguidosCuenta Cuenta)))#f] ;Se verifica veracidad de lista usuarios
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


;Modificar TDA Cuenta, actualizando ambos casos anteriormente presentados
(define (ActualizarSeguidosCuenta CuentaOr ListaSeguidosAct)
  (CrearCuenta (getUsuarioCuenta CuentaOr) (getFechaRegistroC CuentaOr) (getSeguidoresCuenta CuentaOr) ListaSeguidosAct))

(define (ActualizarSeguidoresCuenta CuentaOr ListaSeguidoresAct)
  (CrearCuenta (getUsuarioCuenta CuentaOr) (getFechaRegistroC CuentaOr) ListaSeguidoresAct (getSeguidosCuenta CuentaOr)))

;Otros
;Verificar si usuario ingresado esta en lista seguidores
;Verificar si usuario ingresado esta en lista seguidos

