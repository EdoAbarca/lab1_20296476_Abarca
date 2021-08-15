#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "CuentaUsuario_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")
(require "ListaCuentas_20296476_AbarcaChavez.rkt")
(require "ListaPublicaciones_20296476_AbarcaChavez.rkt")
(require "ListaReacciones_20296476_AbarcaChavez.rkt")
(require "ContenidoSN_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA SocialNetwork
;Composicion: (NombreSN FechaRegistroSN EncryptFunction DecryptFunction ContenidoSN)
;              -> (String X Fecha X EncryptFn X DecryptFn x ContenidoSN)

;Constructor
(define (SocialNetwork Nombre Fecha Encrypt Decrypt) (list Nombre Fecha (Encrypt (ContenidoSN->string (ContenidoSNVacio))) (Decrypt (ContenidoSN->string (ContenidoSNVacio))) (ContenidoSNVacio)))

;Selectores
(define (getNombreSN SocialNetwork) (car SocialNetwork))
(define (getFechaSN SocialNetwork) (cadr SocialNetwork))
(define (getEncriptadoSN SocialNetwork) (caddr SocialNetwork))
(define (getDecriptadoSN SocialNetwork) (cadddr SocialNetwork))
(define (getContenidoSN SocialNetwork) (car (cddddr SocialNetwork)))


;Pertenencia
(define (SocialNetwork? SocialNetwork)
  (if (not (list? SocialNetwork)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length SocialNetwork) 5)) #f] ;TDA SocialNetwork tiene 5 elementos
        [(not (string? (getNombreSN SocialNetwork))) #f] ;Nombre red social debe ser un string y debe estar entre las 3 opciones a continuacion
        [(not (Fecha? (getFechaSN SocialNetwork))) #f] ;Fecha de registro de red social debe ser TDA Fecha
        [(not (string? (getEncriptadoSN SocialNetwork))) #f] ;Contenido encriptado debe ser un string (esta procesado antes de este punto)
        [(not (string? (getDecriptadoSN SocialNetwork))) #f] ;Contenido encriptado (a decriptar) debe ser un string (esta procesado antes de este punto)
        [(not (ContenidoSN? (getContenidoSN SocialNetwork)))#f] ;TDA SNSF, representa la materializacion del contenido a encriptar
        [else #t]))) ;Pruebas pasadas, elemento ingresado por  parametro es TDA SocialNetwork

;Modificadores
;Modificador para actualizar el TDA SocialNetwork (Esto despues de actualizar el TDA SNSF)
(define (ActualizarSocialNetwork SocialNetwork ContenidoSNActualizado)
  (list (getNombreSN SocialNetwork) (getFechaSN SocialNetwork) (encryptFn (ContenidoSN->string ContenidoSNActualizado)) (encryptFn (ContenidoSN->string ContenidoSNActualizado)) ContenidoSNActualizado))

;Otros
;Funcion que une la informacion encriptada con la de la red social, a la espera del llamado de la funcion principal en menu
(define (PrevSocialNetwork->string socialnetwork)
  (if (SocialNetwork? socialnetwork)
      (string-append "\nNombre red social: " (getNombreSN socialnetwork) "\nFecha registro red social: " (Fecha->string (getFechaSN socialnetwork)) (ContenidoSN->string (getContenidoSN socialnetwork)))
      (encryptFn "Error: Dato ingresado no es TDA SocialNetwork.\n")))

