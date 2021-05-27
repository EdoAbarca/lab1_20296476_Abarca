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

;Entregar funciones
;(provide EmptyIG)
;(provide getNombreRed)
;(provide getFechaRed)
;(provide getEncriptado)
;(provide getDecriptado)
;(provide getCuentaLogueada)
;(provide getListaUsuarios)
;(provide getListaCuentas)
;(provide getListaPublicaciones)
;(provide getListaReacciones)
;(provide SocialNetwork?)

;SITUACION: POR NECESIDAD DEL TDA SOCIAL NETWORK Y EVITAR EL ERROR CYCLYNG PATH, SE TUVIERON QUE MOVER LAS FUNCIONES DE CONVERSION A ESTA PARTE DEL PROGRAMA

;//////////////////////////////////////////// FUNCIONES DE CONVERSION DE LISTAS A STRING ///////////////////////////////////////////////////////////////////

;ESCENARIO IDEAL: MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR NOMBRE, FECHA REGISTRO, CONTACTOS, ACTIVIDAD DEL USUARIO Y REACCIONES A ESTA ACTIVIDAD, PARA LUEGO AVANZAR AL SIGUIENTE
;ESCENARIO TEMPORAL (DEFINITIVO PROBABLEMENTE): MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR DATOS USUARIOS -> PUBLICACIONES -> REACCIONES DE LA PUBLICACION APUNTADA (ID)

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
    [(eqv? (getUsuarioP (car ListaPublicaciones)) NombreCuenta) (string-append "AgregarContenido" (ReaccionesPublicacion->string SocialNetwork) (Publicaciones->string (cdr ListaPublicaciones) NombreCuenta))]
    [else (Publicaciones->string (cdr ListaPublicaciones) NombreCuenta)]))

(define (Cuentas->string SocialNetwork ListaCuentas CuentaLogueada)
  (cond
    [(= (length ListaCuentas) 0) "\n"]
    [(null? CuentaLogueada) (string-append "Usuario: " "\nSeguidores:\n" "\nSiguendo:\n" )]
    [else (cond
            [(eqv? (getUsuarioCuenta (car ListaCuentas)) CuentaLogueada) (string-append "Registrado como: " )]
            [else (Cuentas->string SocialNetwork (cdr ListaCuentas) CuentaLogueada)])]))


(define (NoFirmaSN->string SocialNetwork)
  (if (SocialNetwork? SocialNetwork)
      (string-append "Cuentas:\n\n" (Cuentas->string SocialNetwork ListaCuentas CuentaLogueada) "Publicaciones y reacciones:\n\n" Publicaciones->string )
      ("Error: Elemento ingresado no es TDA SocialNetwork.\n")))

;////////////////////////////////// FIN FUNCIONES DE CONVERSION, AHORA SI SE MUESTRA TDA SOCIALNETWORK //////////////////////////////////////

;TDA SocialNetwork
;Composicion: (String X Fecha X EncryptFunction X DecryptFunction x CuentaLogueada x ListaUsuarios x ListaCuentas x ListaPublicaciones x ListaReacciones)

;Constructor
(define (EmptyIG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt null null null null null)) ;Agregar dentro de encrypt y decrypt el texto respectivo (a realizars)
;(define (IG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt null (CrearListaUsuarios) (CrearListaCuentas) (CrearListaPublicaciones) (CrearListaReacciones)))

;Selectores
(define (getNombreRed socialnetwork) (car socialnetwork))
(define (getFechaRed socialnetwork) (cadr socialnetwork))
(define (getEncriptado socialnetwork) (caddr socialnetwork))
(define (getDecriptado socialnetwork) (cadddr socialnetwork))
(define (getCuentaLogueada socialnetwork) (car (cddddr socialnetwork)))
(define (getListaUsuarios socialnetwork) (car (cdr (cddddr socialnetwork))))
(define (getListaCuentas socialnetwork) (car (cddr (cddddr socialnetwork))))
(define (getListaPublicaciones socialnetwork) (car (cdddr (cddddr socialnetwork))))
(define (getListaReacciones socialnetwork) (car (cddddr (cddddr socialnetwork))))

;Pertenencia
(define (SocialNetwork? socialnetwork)
  (if (not (list? socialnetwork))
      #f
      (cond
        [(not (= (length socialnetwork) 9)) #f]
        [(not (string? (getNombreRed socialnetwork))) #f]
        [(not (or (eqv? (getNombreRed socialnetwork) "Instagram")
                  (eqv? (getNombreRed socialnetwork) "Facebook")
                  (eqv? (getNombreRed socialnetwork) "Twitter"))) #f]
        [(not (Fecha? (getFechaRed socialnetwork))) #f]
        [(not (procedure? (getEncriptado socialnetwork))) #f]
        [(not (procedure? (getDecriptado socialnetwork))) #f]
        [(not (or (null? (getCuentaLogueada socialnetwork)) (string? (getCuentaLogueada socialnetwork)))) #f]
        [(not (ListaUsuarios? (getListaUsuarios socialnetwork))) #f]
        [(not (ListaCuentas? (getListaCuentas socialnetwork))) #f]
        [(not (ListaPublicaciones? (getListaPublicaciones socialnetwork))) #f]
        [(not (ListaReacciones? (getListaReacciones socialnetwork))) #f]
        [else #t])))

;Modificadores

(define (RetornoFinal SocialNetwork EncryptInfo DecryptInfo)
  (list
   (getNombreRed SocialNetwork)
   (getFechaRed SocialNetwork)
   (encryptFn EncryptInfo)
   (encryptFn DecryptInfo)
   (getCuentaLogueada SocialNetwork)
   (getListaUsuarios SocialNetwork)
   (getListaCuentas SocialNetwork)
   (getListaPublicaciones SocialNetwork)
   (getListaReacciones SocialNetwork)))

(define (ConcretarActualizacionSocialNetwork SocialNetwork) null)
;  (RetornoFinal SocialNetwork () ())) ;Ingresar funcion que transforma el stack a string

;Luego del procesamiento de cada funcion, se debe actualizar el TDA SocialNetwork
;Se actualiza TDA con la lista a reemplazar segun codigo:
; - 5: Se inicia sesion
; - 6: Se actualiza TDA ListaUsuarios
; - 7: Se actualiza TDA ListaCuentas
; - 8: Se actualiza TDA ListaPublicaciones
; - 9: Se actualiza TDA ListaReacciones
(define (ActualizarSocialNetwork SocialNetwork ElementoSN Codigo)
  (cond
    [(not (integer? Codigo)) SocialNetwork]
    [(= Codigo 5) (ConcretarActualizacionSocialNetwork (list
                                                         (getNombreRed SocialNetwork)
                                                         (getFechaRed SocialNetwork)
                                                         (getEncriptado SocialNetwork)
                                                         (getDecriptado SocialNetwork)
                                                         ElementoSN
                                                         (getListaUsuarios SocialNetwork)
                                                         (getListaCuentas SocialNetwork)
                                                         (getListaPublicaciones SocialNetwork)
                                                         (getListaReacciones SocialNetwork)))]
    [(= Codigo 6) (ConcretarActualizacionSocialNetwork (list
                                                         (getNombreRed SocialNetwork)
                                                         (getFechaRed SocialNetwork)
                                                         (getEncriptado SocialNetwork)
                                                         (getDecriptado SocialNetwork)
                                                         null
                                                         ElementoSN
                                                         (getListaCuentas SocialNetwork)
                                                         (getListaPublicaciones SocialNetwork)
                                                         (getListaReacciones SocialNetwork)))]
    [(= Codigo 7) (ConcretarActualizacionSocialNetwork (list
                                                         (getNombreRed SocialNetwork)
                                                         (getFechaRed SocialNetwork)
                                                         (getEncriptado SocialNetwork)
                                                         (getDecriptado SocialNetwork)
                                                         null
                                                         (getListaUsuarios SocialNetwork)
                                                         ElementoSN
                                                         (getListaPublicaciones SocialNetwork)
                                                         (getListaReacciones SocialNetwork)))]
    [(= Codigo 8) (ConcretarActualizacionSocialNetwork (list
                                                         (getNombreRed SocialNetwork)
                                                         (getFechaRed SocialNetwork)
                                                         (getEncriptado SocialNetwork)
                                                         (getDecriptado SocialNetwork)
                                                         null
                                                         (getListaUsuarios SocialNetwork)
                                                         (getListaCuentas SocialNetwork)
                                                         ElementoSN
                                                         (getListaReacciones SocialNetwork)))]
    [(= Codigo 9) (ConcretarActualizacionSocialNetwork (list
                                                         (getNombreRed SocialNetwork)
                                                         (getFechaRed SocialNetwork)
                                                         (getEncriptado SocialNetwork)
                                                         (getDecriptado SocialNetwork)
                                                         null
                                                         (getListaUsuarios SocialNetwork)
                                                         (getListaCuentas SocialNetwork)
                                                         (getListaPublicaciones SocialNetwork)
                                                         ElementoSN))]
    [else SocialNetwork]))
;No se creo constructor personalizado para esta instancia para no pasar a llevar la firma exigida en el enunciado

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;
;Funcion que transforma el stack de social network a un string, esto para poder encriptar y desencriptar el contenido
;(define (stack->string stack)(string-append (cuentas->string stack usuariologueado) (publicaciones->string stack usuariologueado) (reacciones->string stack usuariologueado)))

;DILEMA RESPECTO A ESTE TDA (A RESOLVER ANTES DE LA FECHA DE ENTREGA -> 27-05-2021):
; - SE LIMITA A 4 PARAMETROS DE ENTRADA
; - NO SE LOGRO DETERMINAR VIA DE ACTUALIZACION POR EL STRING DIRECTAMENTE -> SE NECESITA STACK
; - SE ALTERA FIRMA EXIGIDA PARA CONSTRUCTOR SOCIALNETWORK SI SE INGRESAN LISTAS DE DATOS POR PARAMETRO -> SE TUVO QUE LLAMAR A LOS CONSTRUCTORES EN EL RETORNO
; - AUN PUDIENDO ACTUALIZAR LA INFORMACION POR STACK, AUN OCURRE UN PROBLEMA LATENTE:
;      *¿COMO CUBRO LO EXIGIDO POR LAS FUNCIONES ENCRYPT Y DECRYPT ANTES (Y DESPUES) DE OBTENER EL STACK?

;RESPUESTA PRESENTADA POR PROFESOR ROBERTO EN TDA SOCIALNETWORK EXPLICA COMO SE DEBE TRATAR LAS FUNCIONES ENCRYPT Y DECRYPT
(define (ideaEn Fn)(Fn "Text"))

;ESTRATEGIA FINAL PARA LIDIAR CON TEMA DE ENCRIPTACION TEXTO:
; 1) CREAR/ACTUALIZAR TDA SOCIALNETWORK
; 2) ESTA ACTUALIZACION SE INGRESARA EN UNA NUEVA FUNCION QUE TOME COMO UNICO PARAMETRO EL TDA CON LOS DATOS YA ACTUALIZADOS
; 3) ESTA NUEVA FUNCION SE ENCARGARA DE LLAMAR A LA FUNCION FINAL QUE TENDRA 3 PARAMETROS:
;      A) EL TDA SOCIAL NETWORK (LOS CAMBIOS EN LOS DATOS DE LA RED SOCIAL YA ESTAN REALIZADOS)
;      B) PARAMETRO DE TEXTO DONDE SE LLAMARA A LA FUNCION DE TRANSFORMACION DE TDA A TEXTO ENCRIPTADO (ESPACIO PARA ENCRYPT)
;      C) PARAMETRO DE TEXTO DONDE SE LLAMARA A LA FUNCION DE TRANSFORMACION DE TDA A TEXTO ENCRIPTADO (ESPACIO PARA DECRYPT)
; 4) EN ESTA FUNCION FINAL SE RETORNARA EL TDA SOCIALNETWORK, DONDE LOS PARAMETROS DE TEXTO IRAN EN LOS ESPACIOS DE FUNCIONES DE ENCRIPTACION DE TEXTO (NUNCA SE EVALUAN LAS FUNCIONES, RECIEN SE HACE EN ESTE PUNTO)