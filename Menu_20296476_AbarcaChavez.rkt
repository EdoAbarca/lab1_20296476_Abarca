#lang racket
;Laboratorio 1 Paradigmas de Programacion
;Archivo de requerimientos funcionales pedidos por enunciado
;Objetivo: Simular la interaccion de un usuario en una red social, implementando la variante usando el paradigma funcional, en el lenguaje Scheme (Racket)
;Nombre alumno: Eduardo Abarca
;RUT: 20.296.476-1
;Seccion: C-3
;Profesor: Daniel Gacitua
;Entrega: Original (27-05-2021)

;PEDIR ARCHIVOS EXTERNOS
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "Usuario_20296476_AbarcaChavez.rkt")
(require "Cuenta_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")
(require "ListaUsuarios_20296476_AbarcaChavez.rkt")
(require "ListaCuentas_20296476_AbarcaChavez.rkt")
(require "ListaPublicaciones_20296476_AbarcaChavez.rkt")
(require "ListaReacciones_20296476_AbarcaChavez.rkt")
(require "SocialNetwork_20296476_AbarcaChavez.rkt")

;///////////////////////////// REQUERIMIENTOS FUNCIONALES EXIGIDOS //////////////////////////////

;/////// MINIMOS:

;Funcion que registra en la red social un nuevo usuario
;Dominio: (SocialNetwork x Fecha x string x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (register socialnetwork date username password)
  (if (and (SocialNetwork? socialnetwork) (string? username) (string? password))
      (if (EstaRegistrado (getListaUsuarios socialnetwork) username password)
          socialnetwork
          null) ;Se actualiza TDA SocialNetwork
      socialnetwork))

;Ejemplos de uso:

;Funcion que inicia sesion en la red social con un usuario ya registrado
;Dominio: (SocialNetwork x string x string x procedure)
;Recorrido: procedure
;Recorrido final: SocialNetwork
;Recursion: Natural

(define (login socialnetwork username password operation)
  (cond
    [(not (SocialNetwork? socialnetwork)) (operation socialnetwork)]
    [(not (and (string? username) (string? password) (procedure? operation))) (operation socialnetwork)]
    [(not (CredencialesSonCorrectas (getListaUsuarios (getStack socialnetwork)) username password)) (operation socialnetwork)]
    [else #t])) ;Se retorna operation, pero se debe actualizar TDA socialnetwork con el usuario logueado

;Funcion que, con sesion iniciada, realiza un post en la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x '(Contenido x '(string x string x ... x string)))
;Recorrido: SocialNetwork
;Recursion: Natural
(define (post socialnetwork)
  (lambda (date)
    (lambda (content . users)
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) ()) ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
          () ;Se revisa si es posible
          ()))))

;Funcion que, con sesion iniciada, sigue a otra cuenta de la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x NombreUsuario)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (follow socialnetwork)
  (lambda (date)
    (lambda (user)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (string? (getUsuarioLogueado socialnetwork)) (Fecha? date) (string? user))
          () ;Se verifica si el usuario ingresado existe
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion


;SocialNetwork->string (si es que logro hacer Share, #difiiiiiishil)
;(define (SocialNetwork->string SocialNetwork)(encryptFn (getDecriptado SocialNetwork))

;/////// OPTATIVOS: