#lang racket
(require "StackSocialNetwork.rkt")
(require "Fecha.rkt")

;TDA SocialNetwork
;Composicion: (String X Date X EncryptFunction X DecryptFunction x StackRedSocial) (A implementar, leer enunciado nuevamente)

;Constructor
(define (EmptyIG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt (StackVacio)))
;(define (IG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt (StackRedSocial)))

;Selectores
(define (getNombreRed socialnetwork) (car socialnetwork))
(define (getFechaRed socialnetwork) (cadr socialnetwork))
(define (getEncriptado socialnetwork) (caddr socialnetwork))
(define (getDecriptado socialnetwork) (cadddr socialnetwork))
(define (getStack socialnetwork)(car (cddddr socialnetwork)))

;Pertenencia
(define (SocialNetwork? socialnetwork)
  (if (not (list? socialnetwork))
      #f
      (cond
        [(not (= (length socialnetwork) 5)) #f]
        [(not (string? (getNombreRed socialnetwork))) #f]
        [(not (Fecha? (getFechaRed socialnetwork))) #f]
        [(not (string? (getEncriptado socialnetwork))) #f]
        [(not (string? (getDecriptado socialnetwork))) #f]
        [(not (StackRedSocial? (getStack socialnetwork))) #f]
        [else #t])))

;Modificadores
;Luego del procesamiento de cada funcion, se deben actualizar los ultimos 3 parametros del tda socialnetwork

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
;Funcion que transforma el stack de social network a un string, esto para poder encriptar y desencriptar el contenido
;(define (stack->string stack)(string-append (cuentas->string stack usuariologueado) (publicaciones->string stack usuariologueado) (reacciones->string stack usuariologueado)))

;DILEMA RESPECTO A ESTE TDA (A RESOLVER ANTES DE LA FECHA DE ENTREGA -> 27-05-2021):
; - SE LIMITA A 4 PARAMETROS DE ENTRADA
; - NO SE LOGRO DETERMINAR VIA DE ACTUALIZACION POR EL STRING DIRECTAMENTE -> SE NECESITA STACK
; - SE ALTERA TDA EXIGIDO PARA SOCIALNETWORK SI SE INGRESA STACK POR PARAMETRO -> SE TUVO QUE LLAMAR AL CONSTRUCTOR EN EL RETORNO
; - AUN PUDIENDO ACTUALIZAR LA INFORMACION POR STACK, AUN OCURRE UN PROBLEMA LATENTE:
;      *¿COMO CUBRO LO EXIGIDO POR LAS FUNCIONES ENCRYPT Y DECRYPT ANTES (Y DESPUES) DE OBTENER EL STACK?

;RESPUESTA PRESENTADA POR EL PROFESOR ROBERTO EN TDA SOCIALNETWORK EXPLICA COMO SE DEBE TRATAR LAS FUNCIONES ENCRYPT Y DECRYPT
(define (ideaEn Fn)(Fn "Text"))