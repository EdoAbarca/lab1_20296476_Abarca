#lang racket
;Laboratorio 1 Paradigmas de Programacion
;Archivo de requerimientos funcionales pedidos por enunciado
;Objetivo: Simular la interaccion de un usuario en una red social usando el paradigma funcional (Racket Scheme)
;Nombre alumno: Eduardo Abarca
;RUT: 20.296.476-1
;Seccion: C-3

;PEDIR ARCHIVOS EXTERNOS

;///////////////////////////// REQUERIMIENTOS FUNCIONALES EXIGIDOS //////////////////////////////

;///////////////////////////// REQUERIMIENTOS MINIMOS //////////////////////////////

;Funcion que registra en la red social a un nuevo usuario
;Dominio: (SocialNetwork x Date x string x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (register socialnetwork date username password)
  (if (EstaRegistrado (getListaUsuarios (getStack socialnetwork)) user pass)
      socialnetwork
      null)) ;Se actualiza tda social network

;Funcion que inicia sesion en la red social con un usuario ya registrado
;Dominio: (SocialNetwork x string x string x procedure)
;Recorrido: procedure
;Recorrido final: SocialNetwork
;Recursion: Natural

(define (login socialnetwork username password operation)
  (cond
    [(not (SocialNetwork? socialnetwork)) (operation socialnetwork)]
    [(not (CredencialesSonCorrectas (getListaUsuarios (getStack socialnetwork)) username password)) (operation socialnetwork)]
    [(not (and (string? username) (string? password) (procedure? operation))) (operation socialnetwork)]
    [else #t])) ;Se retorna operation, pero se debe actualizar TDA socialnetwork con el usuario logueado

;Funcion que realiza un post mientras haya un usuario con sesion iniciada
;Dominio: (SocialNetwork)
;Recorrido: (SocialNetwork)
;Recursion: 

;///////////////////////////// REQUERIMIENTOS OPTATIVOS //////////////////////////////