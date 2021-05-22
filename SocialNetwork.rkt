#lang racket
(require "StackSocialNetwork.rkt")

;TDA SocialNetwork
;Composicion: (String X Date X EncryptFunction X DecryptFunction x StackRedSocial) (A implementar, leer enunciado nuevamente)

;Constructor
;(define (EmptyIG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt (StackVacio)))
;(define (IG nombre fecha encrypt decrypt) (list nombre fecha encrypt decrypt (StackRedSocial)))

;Selectores

;Pertenencia

;Modificadores

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;DILEMA RESPECTO A ESTE TDA (A RESOLVER ANTES DE LA FECHA DE ENTREGA -> 27-05-2021):
; - SE LIMITA A 4 PARAMETROS
; - NO SE LOGRO DETERMINAR VIA DE ACTUALIZACION POR EL STRING DIRECTAMENTE -> SE NECESITA STACK
; - SE ALTERA TDA EXIGIDO PARA SOCIALNETWORK SI SE INGRESA STACK POR PARAMETRO -> SE TUVO QUE LLAMAR AL CONSTRUCTOR EN EL RETORNO
; - AUN PUDIENDO ACTUALIZAR LA INFORMACION POR STACK, AUN OCURRE UN PROBLEMA LATENTE:
;      *¿COMO CUBRO LO EXIGIDO POR LAS FUNCIONES ENCRYPT Y DECRYPT ANTES DE OBTENER EL STACK?