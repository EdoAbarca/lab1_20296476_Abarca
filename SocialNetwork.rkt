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
        [(not (string? (getEncriptado socialnetwork))) #f]
        [(not (string? (getDecriptado socialnetwork))) #f]
        [(not (or (list? (getCuentaLogueada socialnetwork)) (string? (getCuentaLogueada socialnetwork)))) #f]
        [(not (ListaUsuarios? (getListaUsuarios socialnetwork))) #f]
        [(not (ListaCuentas? (getListaCuentas socialnetwork))) #f]
        [(not (ListaPublicaciones? (getListaPublicaciones socialnetwork))) #f]
        [(not (ListaReacciones? (getListaReacciones socialnetwork))) #f]
        [else #t])))

;Modificadores
;Luego del procesamiento de cada funcion, se deben actualizar los textos de encriptado y desencriptado
(define (ActualizarSocialNetwork SocialNetwork ElementoSN Codigo)
  ())

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;
;Funcion que transforma el stack de social network a un string, esto para poder encriptar y desencriptar el contenido
;(define (stack->string stack)(string-append (cuentas->string stack usuariologueado) (publicaciones->string stack usuariologueado) (reacciones->string stack usuariologueado)))

;DILEMA RESPECTO A ESTE TDA (A RESOLVER ANTES DE LA FECHA DE ENTREGA -> 27-05-2021):
; - SE LIMITA A 4 PARAMETROS DE ENTRADA
; - NO SE LOGRO DETERMINAR VIA DE ACTUALIZACION POR EL STRING DIRECTAMENTE -> SE NECESITA STACK
; - SE ALTERA TDA EXIGIDO PARA SOCIALNETWORK SI SE INGRESA STACK POR PARAMETRO -> SE TUVO QUE LLAMAR AL CONSTRUCTOR EN EL RETORNO
; - AUN PUDIENDO ACTUALIZAR LA INFORMACION POR STACK, AUN OCURRE UN PROBLEMA LATENTE:
;      *¿COMO CUBRO LO EXIGIDO POR LAS FUNCIONES ENCRYPT Y DECRYPT ANTES (Y DESPUES) DE OBTENER EL STACK?

;RESPUESTA PRESENTADA POR PROFESOR ROBERTO EN TDA SOCIALNETWORK EXPLICA COMO SE DEBE TRATAR LAS FUNCIONES ENCRYPT Y DECRYPT
(define (ideaEn Fn)(Fn "Text"))