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

;TDA SocialNetwork
;Composicion: (String X Fecha X EncryptFunction X DecryptFunction x CuentaLogueada x ListaUsuarios x ListaCuentas x ListaPublicaciones x ListaReacciones)

;Constructor
(define (EmptyIG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt (SNSFvacio))) ;Agregar dentro de encrypt y decrypt el texto respectivo (a realizars)
;(define (IG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt null (CrearListaUsuarios) (CrearListaCuentas) (CrearListaPublicaciones) (CrearListaReacciones)))

;Selectores
(define (getNombreRed socialnetwork) (car socialnetwork))
(define (getFechaRed socialnetwork) (cadr socialnetwork))
(define (getEncriptado socialnetwork) (caddr socialnetwork))
(define (getDecriptado socialnetwork) (cadddr socialnetwork))


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