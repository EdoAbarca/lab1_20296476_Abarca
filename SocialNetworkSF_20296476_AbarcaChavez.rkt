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
;(define (SNSFcustom) (list null (CrearListaUsuarios) (CrearListaCuentas) (CrearListaPublicaciones) (CrearListaReacciones)))

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
    [(= Codigo 1) ( (list
                                                         ElementoSNSF
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 2) ( (list
                                                         null ;Cerrar sesion
                                                         ElementoSNSF
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 3) ( (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         ElementoSNSF
                                                         (getListaPublicaciones SNSF)
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 4) ( (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         ElementoSNSF
                                                         (getListaReacciones SNSF)))]
    [(= Codigo 5)  (list
                                                         null ;Cerrar sesion
                                                         (getListaUsuarios SNSF)
                                                         (getListaCuentas SNSF)
                                                         (getListaPublicaciones SNSF)
                                                         ElementoSNSF)]
    [else SNSF]))

;Actualizador mas sencillo, pero mas supceptible a efectos colaterales
;Dominio: (string/null x CuentaLogueada x ListaUsuarios x ListaCuentas x ListaPublicaciones x ListaReacciones)
;Recorrido: SNSF
(define (ActualizarSNSFv2 CuentaLogueada ListaUsuarios ListaCuentas ListaPublicaciones ListaReacciones)
  (if (SNSF? (list CuentaLogueada ListaUsuarios ListaCuentas ListaPublicaciones ListaReacciones))
      (list CuentaLogueada ListaUsuarios ListaCuentas ListaPublicaciones ListaReacciones)
      (list null null null null null)))

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
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
    [else (string-append (car Contactos) ", desde " (Fecha->string (cadr Contactos)) "\n" (Contactos->string (cdr Contactos)))]))

;Funcion de conversion de TDA ListaReacciones a string si IdPublicacion de TDA Reaccion apuntado coincide con IdPublicacion por parametro
;Dominio: (SocialNetwork x ListaReacciones x integer)
;Recorrido: string
;Recursion: Natural
(define (ReaccionesPublicacion->string SNSF ListaReacciones IdPublicacion)
  (cond
    [(= (length ListaReacciones) 0) "\n"]
    [(= (getIdPR (car ListaReacciones)) IdPublicacion)
     (string-append
      "\nID Publicacion: " (getIdPR ListaReacciones)
      "\nID Publicacion: " (getIdR ListaReacciones)
      "\nFecha reaccion: " (getFechaR ListaReacciones)
      "\nCuenta reaccion: " (getCuentaR ListaReacciones)
      "\nTipo reaccion: " (getTipoR ListaReacciones)
      "\nContenido reaccion: " (getContenidoR ListaReacciones)
      (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion))]
    [else (ReaccionesPublicacion->string (cdr ListaReacciones) IdPublicacion)]))


;Funcion de conversion de TDA ListaPublicaciones a string si NombreCuenta de TDA Publicacion apuntado coincide con NombreCuenta por parametro
;Dominio: (SocialNetwork x ListaPublicaciones x string)
;Recorrido: string
;Recursion: Natural
(define (Publicaciones->string SNSF ListaPublicaciones UsuarioCuenta)
  (cond
    [(= (length ListaPublicaciones) 0) "\n"]
    [(eqv? (getCuentaOrigenP (car ListaPublicaciones)) UsuarioCuenta) (string-append
                                                                  "\nID: " (getIdP (car ListaPublicaciones))
                                                                  "\nCuenta perteneciente: " (getCuentaOrigenP (car ListaPublicaciones))
                                                                  "\nTipo de publicacion: " (getTipoP (car ListaPublicaciones))
                                                                  "\nContenido publicacion: " (getContenidoP (car ListaPublicaciones))
                                                                  "\nCuenta que compartio esta publicacion: "(getCuentaQueComparteP (car ListaPublicaciones))
                                                                  "\nCuenta destino: " (getCuentaDestinoP (car ListaPublicaciones))
                                                                  "\n\nRESPUESTAS A ESTA PUBLICACION:\n\n"
                                                                  (ReaccionesPublicacion->string SNSF (getListaReacciones SNSF) (getIdP (car ListaPublicaciones)))
                                                                  (Publicaciones->string (cdr ListaPublicaciones) UsuarioCuenta))]
    [else (Publicaciones->string (cdr ListaPublicaciones) UsuarioCuenta)]))

;Funcion de conversion de TDA ListaCuentas a string considerando 2 casos:
; - Sin usuario logueado (CuentaLogueada -> null) -> se transforma toda la lista
; - Con usuario logueado (CuentaLogueada -> not(null)) -> se transforma solo la cuenta involucrada (Para SocialNetwork -> string)
;Dominio: (SocialNetwork x ListaCuentas x string/null)
;Recorrido: string
(define (Cuentas->string SNSF ListaCuentas CuentaLogueada)
  (cond
    [(= (length ListaCuentas) 0) "\n"] ;Lista recorrida o vacia, salto de linea como condicion de borde
    [(null? CuentaLogueada) ;No hay sesion iniciada, se debe mostrar todo lo que hay en el(los) stacks
     (string-append
      "Usuario: "
      (getUsuarioCuenta (car ListaCuentas))
      "\nFecha de registro: "
      (Fecha->string (getFechaRegistroC (car ListaCuentas)))
      "\n\nSeguidores:\n\n"
      (Contactos->string (getSeguidoresCuenta (car ListaCuentas)))
      "\nSiguendo:\n\n"
      (Contactos->string (getSeguidosCuenta (car ListaCuentas)))
      "\nPublicaciones:\n\n"
      (Publicaciones->string SNSF (getListaPublicaciones SNSF) (getUsuarioCuenta (car ListaCuentas)))
      (Cuentas->string SNSF (cdr ListaCuentas) CuentaLogueada))]
    [else (cond
            [(eqv? (getUsuarioCuenta (car ListaCuentas)) CuentaLogueada)
             (string-append
              "Registrado como: "
              CuentaLogueada
              "\nDatos:\n\nFecha registro:"
              (Fecha->string (getFechaRegistroC (car ListaCuentas)))
              "\nSeguidores:\n\n"
              (Contactos->string (getSeguidoresCuenta (car ListaCuentas)))
              "\nSiguendo:\n"
              (Contactos->string (getSeguidosCuenta (car ListaCuentas)))
              "\nPublicaciones:\n\n"
              (Publicaciones->string SNSF (getListaPublicaciones SNSF) CuentaLogueada))]
            [else (Cuentas->string SNSF (cdr ListaCuentas) CuentaLogueada)])]))

;Funcion de conversion parcial de TDA socialnetwork a string (elementos fuera de la firma del constructor)
;Dominio: SocialNetwork
;Recorrido: string
(define (NoFirmaSN->string SNSF)
  (if (SNSF? SNSF)
      (string-append "Muestra contenido fuera de la firma:\n\n" (Cuentas->string SNSF (getListaCuentas SNSF) (getCuentaLogueada SNSF)))
      ("Error: Elemento ingresado no es TDA SNSF, no es posible transformar la informacion almacenada.\n")))

;////////////////////////////////////////////// FIN FUNCIONES DE CONVERSION //////////////////////////////////////////////////