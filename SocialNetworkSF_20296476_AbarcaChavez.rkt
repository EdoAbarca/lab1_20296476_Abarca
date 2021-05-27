#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Usuario_20296476_AbarcaChavez.rkt")
(require "Cuenta_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")
(require "ListaUsuarios_20296476_AbarcaChavez.rkt")
(require "ListaCuentas_20296476_AbarcaChavez.rkt")
(require "ListaPublicaciones_20296476_AbarcaChavez.rkt")
(require "ListaReacciones_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA SocialNetworkSF (SocialNetwork Sin Firma, haciendo alusión a los elementos del TDA que NO pertenecen a la firma del constructor)
;Composicion: (string x ListaUsuarios x ListaCuentas x ListaPublicaciones x ListaReacciones)

;Constructor
(define (SNSFvacio) (list null null null null null))
;(define (SNSFcustom) (list null null null null null))

;Selectores
(define (getCuentaLogueada SNSF) (car SNSF))
(define (getListaUsuarios SNSF) (cadr SNSF))
(define (getListaCuentas SNSF) (caddr SNSF))
(define (getListaPublicaciones SNSF) (cadddr SNSF))
(define (getListaReacciones SNSF) (car (cddddr SNSF)))

;Pertenencia
(define (SNSF? SNSF)
  (if (not (list? SNSF)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length SNSF) 5)) #f] ;TDA SNSF tiene 5 elementos
        [(not (or (string? (getCuentaLogueada SNSF)) (null? (getCuentaLogueada SNSF)))) #f] ;Usuario debe ser null (sin log) o string (con log)
        [(not (ListaUsuarios? (getListaUsuarios SNSF))) #f] ;TDA ListaUsuarios
        [(not (ListaCuentas? (getListaCuentas SNSF))) #f] ;TDA ListaCuentas
        [(not (ListaPublicaciones? (getListaPublicaciones SNSF))) #f] ;TDA ListaPublicaciones
        [(not (ListaReacciones? (getListaReacciones SNSF))) #f] ;TDA ListaReacciones
        [else #t]))) ;Pruebas pasadas, el elemento ingresado por parametro es TDA SNSF

;Modificadores
;Modificador para actualizar el TDA SNSF en base a las listas involucradas

(define (RetornoFinal SocialNetwork EncryptInfo DecryptInfo)
  (list
   (getCuentaLogueada SocialNetwork)
   (getListaUsuarios SocialNetwork)
   (getListaCuentas SocialNetwork)
   (getListaPublicaciones SocialNetwork)
   (getListaReacciones SocialNetwork)))

(define (ConcretarActualizacionSocialNetwork SocialNetwork) null)
;  (RetornoFinal SocialNetwork () ())) ;Ingresar funcion que transforma el stack a string

;Luego del procesamiento de cada funcion, se debe actualizar el TDA SocialNetwork
;Se actualiza TDA con la lista a reemplazar segun codigo:
; - 1: Se inicia sesion
; - 2: Se actualiza TDA ListaUsuarios
; - 3: Se actualiza TDA ListaCuentas
; - 4: Se actualiza TDA ListaPublicaciones
; - 5: Se actualiza TDA ListaReacciones
(define (ActualizarSNSF SNSF ElementoSNSF Codigo)
  (cond
    [(not (integer? Codigo)) SNSF]
    [(= Codigo 1) (ConcretarActualizacionSocialNetwork (list
                                                         ElementoSNSF
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 2) (ConcretarActualizacionSocialNetwork (list
                                                         null ;Cerrar sesion
                                                         ElementoSNSF
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 3) (ConcretarActualizacionSocialNetwork (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         ElementoSNSF
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 4) (ConcretarActualizacionSocialNetwork (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         ElementoSNSF
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 5) (ConcretarActualizacionSocialNetwork (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         ElementoSNSF))]
    [else SNSF]))
;No se creo constructor personalizado para esta instancia para no pasar a llevar la firma exigida en el enunciado

;Otros

;SITUACION: NO RESULTO MOVER ESTAS FUNCIONES A TDA SOCIALNETWORK, POR LO QUE SE TUVO QUE CREAR ESTE TDA PARA PODER HACER EFECTIVA LA CONVERSION DE LOS DATOS A STRING

;//////////////////////////////////////////// FUNCIONES DE CONVERSION DE LISTAS A STRING ///////////////////////////////////////////////////////////////////

;ESCENARIO IDEAL: MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR NOMBRE, FECHA REGISTRO, CONTACTOS, ACTIVIDAD DEL USUARIO Y REACCIONES A ESTA ACTIVIDAD, PARA LUEGO AVANZAR AL SIGUIENTE
;ESCENARIO TEMPORAL (DEFINITIVO PROBABLEMENTE): MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR DATOS USUARIOS -> PUBLICACIONES -> REACCIONES DE LA PUBLICACION APUNTADA (ID)

;Funcion de conversion de TDA Fecha a string
;Dominio: TDA Fecha
;Recorrido: string
(define (Fecha->string Fecha)
  (string-append "Fecha registro: " (number->string (getDia Fecha)) "/" (number->string (getMes Fecha)) "/" (number->string (getAnio Fecha)) "\n"))

;Funcion de conversion de lista contactos (seguidores/seguidos cuenta) a string
;Dominio: list
;Recorrido: string
;Recursion: Natural
(define (Contactos->string Contactos)
  (cond
    [(= (length Contactos) 0) "\n"]
    [else (string-append (car Contactos) "\n" (Contactos->string (cdr Contactos)))]))

;Funcion de conversion de TDA ListaReacciones a string si IdPublicacion de TDA Reaccion apuntado coincide con IdPublicacion por parametro
;Dominio: (SocialNetwork x ListaReacciones x integer)
;Recorrido: string
;Recursion: Natural
(define (ReaccionesPublicacion->string SocialNetwork ListaReacciones IdPublicacion)
  (cond
    [(= (length ListaReacciones) 0) "\n"]
    [(= (getIdPR (car ListaReacciones)) IdPublicacion) (string-append (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion))]
    [else (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion)]))

;Funcion de conversion de TDA ListaPublicaciones a string si NombreCuenta de TDA Publicacion apuntado coincide con NombreCuenta por parametro
;Dominio: (SocialNetwork x ListaPublicaciones x string)
;Recorrido: string
;Recursion: Natural
(define (Publicaciones->string SocialNetwork ListaPublicaciones NombreCuenta)
  (cond
    [(= (length ListaPublicaciones) 0) "\n"]
    [(eqv? (getUsuarioP (car ListaPublicaciones)) NombreCuenta) (string-append "AgregarContenido" (ReaccionesPublicacion->string SocialNetwork) (Publicaciones->string (cdr ListaPublicaciones) NombreCuenta))]
    [else (Publicaciones->string (cdr ListaPublicaciones) NombreCuenta)]))

;Funcion de conversion de TDA ListaCuentas a string considerando 2 casos:
; - Sin usuario logueado (CuentaLogueada -> null) -> se transforma toda la lista
; - Con usuario logueado (CuentaLogueada -> not(null)) -> se transforma solo la cuenta involucrada (Para SocialNetwork -> string)
;Dominio: (SocialNetwork x ListaCuentas x string/null)
;Recorrido: string
(define (Cuentas->string SocialNetwork ListaCuentas CuentaLogueada)
  (cond
    [(= (length ListaCuentas) 0) "\n"]
    [(null? CuentaLogueada) (string-append "Usuario: " "\nSeguidores:\n" "\nSiguendo:\n" )]
    [else (cond
            [(eqv? (getUsuarioCuenta (car ListaCuentas)) CuentaLogueada) (string-append "Registrado como: " CuentaLogueada "\nDatos:\n\nFecha registro:" (Fecha->string ()) "\nSeguidores:\n" "\nSiguendo:\n" "\nPublicaciones:\n")]
            [else (Cuentas->string SocialNetwork (cdr ListaCuentas) CuentaLogueada)])]))

;Funcion de conversion parcial de TDA socialnetwork a string (elementos fuera de la firma del constructor)
;Dominio: SocialNetwork
;Recorrido: string
(define (NoFirmaSN->string SocialNetwork)
  (if (SocialNetwork? SocialNetwork)
      (string-append "Muestra contenido fuera de la firma:\n\n" (Cuentas->string SocialNetwork (getListaCuentas SocialNetwork) (getCuentaLogueada SocialNetwork)) "Publicaciones y reacciones:\n\n" Publicaciones->string )
      ("Error: Elemento ingresado no es TDA SocialNetwork.\n")))

;////////////////////////////////// FIN FUNCIONES DE CONVERSION, AHORA SI SE MUESTRA TDA SOCIALNETWORK //////////////////////////////////////