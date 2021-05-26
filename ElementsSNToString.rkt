#lang racket
;Pedir archivos externos
(require "Fecha.rkt")
(require "Usuario.rkt")
(require "Cuenta.rkt")
(require "Publicacion.rkt")
(require "Reaccion.rkt")
(require "ListaUsuarios.rkt")
(require "ListaCuentas.rkt")
(require "ListaPublicaciones.rkt")
(require "ListaReacciones.rkt")

;FUNCIONES DE CONVERSION DE LISTAS A STRING
;ESCENARIO IDEAL: MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR NOMBRE, FECHA REGISTRO, CONTACTOS, ACTIVIDAD DEL USUARIO Y REACCIONES A ESTA ACTIVIDAD, PARA LUEGO AVANZAR AL SIGUIENTE
;ESCENARIO TEMPORAL: MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR DATOS USUARIOS -> PUBLICACIONES -> REACCIONES DE LA PUBLICACION APUNTADA (ID)

(define (Fecha->string Fecha)
  (string-append "Fecha registro: " (number->string (getDia Fecha)) "/" (number->string (getMes Fecha)) "/" (number->string (getAnio Fecha)) "\n"))

(define (Contactos->string Contactos)
  (cond
    [(= (length Contactos) 0) "\n"]
    [else (string-append (car Contactos) "\n" (Contactos->string (cdr Contactos)))]))

(define (ReaccionesPublicacion->string SocialNetwork ListaReacciones IdPublicacion)
  (cond
    [(= (length ListaReacciones) 0) "\n"]
    [(= (getIdPR (car ListaReacciones)) IdPublicacion) (string-append (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion))]
    [else (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion)]))

(define (Publicaciones->string SocialNetwork ListaPublicaciones NombreCuenta)
  (cond
    [(= (length ListaPublicaciones) 0) "\n"]
    [(eqv? (getUsuarioP (car ListaPublicaciones)) NombreCuenta) (string-append "AgregarContenido"(Publicaciones->string (cdr ListaPublicaciones) NombreCuenta))]
    [else (Publicaciones->string (cdr ListaPublicaciones) NombreCuenta)]))

(define (Cuentas->string SocialNetwork ListaCuentas CuentaLogueada)
  (cond
    [(= (length ListaCuentas) 0) "\n"]
    [(null? CuentaLogueada) (string-append "Usuario: " "\nSeguidores:\n" "\nSiguendo:\n" )]
    [else (cond
            [(eqv? (getUsuarioCuenta (car ListaCuentas)) CuentaLogueada) (string-append "Registrado como: " )]
            [else (Cuentas->string SocialNetwork (cdr ListaCuentas) CuentaLogueada)])]))

;PROBLEMA ACTUAL:
;AL MOMENTO DE CONSTRUIR EL TDA SOCIALNETWORK, NO TENGO STRING LISTO PARA PASAR POR PARAMETRO -> IDEA DE MOMENTO: GENERAR EL TDA TAL CUAL Y LUEGO USARLO COMO PARAMETRO PARA UNA FUNCION QUE 
