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
(require "SocialNetworkSF_20296476_AbarcaChavez.rkt")
(require "SocialNetwork_20296476_AbarcaChavez.rkt")

;/////////////////////////////////////////////////////// REQUERIMIENTOS FUNCIONALES EXIGIDOS ///////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////// REGISTER ////////////////////////////////////////////////////////////////////////////////

;Funcion que registra en la red social un nuevo usuario
;Dominio: (SocialNetwork x Fecha x string x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (register socialnetwork date username password)
  (if (and (SocialNetwork? socialnetwork) (Fecha? date) (string? username) (string? password)) ;Se verifican datos de entrada
      (if (EstaRegistrado (getListaUsuarios (getSNSF socialnetwork)) username password) ;Si los datos ya estan registrados, no es posible el registro
          socialnetwork
          (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarSNSF null
                                                   (RegistrarUsuario (getListaUsuarios (getSNSF socialnetwork)) (CrearUsuario username password))
                                                   (RegistrarCuenta (getListaCuentas (getSNSF socialnetwork)) (CrearCuenta username date null null))
                                                   (getListaPublicaciones (getSNSF socialnetwork))
                                                   (getListaReacciones (getSNSF socialnetwork))))) ;Se actualiza TDA SocialNetwork
      socialnetwork))

;Ejemplos de uso:
;(register (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass")
;
;

;///////////////////////////////////////////////////////////// LOGIN ////////////////////////////////////////////////////////////////////////////////

;Funcion que inicia sesion en la red social con un usuario ya registrado
;Dominio: (SocialNetwork x string x string x procedure)
;Recorrido: procedure
;Recorrido final: SocialNetwork
;Recursion: De cola

(define (login socialnetwork username password operation)
  (cond
    [(not (SocialNetwork? socialnetwork)) (operation socialnetwork)] ;Se necesita TDA SocialNetwork para iniciar sesion
    [(not (and (string? username) (string? password) (procedure? operation))) (operation socialnetwork)] ;Parametros deben tener ser de un tipo definido
    [(not (CredencialesSonCorrectas (getListaUsuarios (getSNSF socialnetwork)) username password)) (operation socialnetwork)] ;Se debe verificar las credenciales ingresadas (aca se usa recursion de cola)
    [else (operation (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarSNSF username
                                                   (getListaUsuarios (getSNSF socialnetwork)) 
                                                   (getListaCuentas (getSNSF socialnetwork))
                                                   (getListaPublicaciones (getSNSF socialnetwork))
                                                   (getListaReacciones (getSNSF socialnetwork)))))])) ;Se retorna operation, pero se debe actualizar TDA socialnetwork con el usuario logueado

;Ejemplos de uso:
;(login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" register) ;error
;(login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" login) ;error
;(login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" nooperation) ;error

;///////////////////////////////////////////////////////////// POST ////////////////////////////////////////////////////////////////////////////////

;Funcion que, con sesion iniciada, realiza un post en la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x string x list)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (post socialnetwork) 
  (lambda (date)
    (lambda (content . users)
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (string? content))
          (cond ;De haber usuarios a compartir, estos deben estar en la lista de contactos
            [(null? (getCuentaLogueada (getSNSF socialnetwork))) socialnetwork] ;Debe existir usuario con sesion iniciada
            [(not (list? users)) socialnetwork] ;Error en ingreso de cuentas a compartir -> debe ser lista
            [(null? users) (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarSNSF null
                                                   (getListaUsuarios (getSNSF socialnetwork))
                                                   (getListaCuentas (getSNSF socialnetwork))
                                                   (AgregarNuevasPublicaciones (getListaPublicaciones (getSNSF socialnetwork)) (length (getListaPublicaciones (getSNSF socialnetwork))) (getCuentaLogueada (getSNSF socialnetwork)) date content null))
                                                   (getListaReacciones (getSNSF socialnetwork)))] ;Se realiza unico registro de publicacion
            [else (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarSNSF null
                                                   (getListaUsuarios (getSNSF socialnetwork))
                                                   (getListaCuentas (getSNSF socialnetwork))
                                                   (AgregarNuevasPublicaciones (getListaPublicaciones (getSNSF socialnetwork)) (length (getListaPublicaciones (getSNSF socialnetwork))) (getCuentaLogueada (getSNSF socialnetwork)) date content (FiltrarCompartidos users (getListaCuentas (getSNSF socialnetwork)) (getCuentaLogueada (getSNSF socialnetwork))))
                                                   (getListaReacciones (getSNSF socialnetwork))))]) ;Se realiza multiple registro de publicacion (abarcando los perfiles de cada usuario alcanzado (se debe verificar que existen))
          socialnetwork))))

;Ejemplos de uso:
;(((login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" post)(Fecha 22 5 2021)) "Contenido")
;(((login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" post)(Fecha 22 5 2021)) "Contenido" "user1")
;(((login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" post)(Fecha 22 5 2021)) "Contenido" "user1" "user2")

;///////////////////////////////////////////////////////////// FOLLOW ////////////////////////////////////////////////////////////////////////////////

;Funcion que, con sesion iniciada, sigue a otra cuenta de la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (follow socialnetwork)
  (lambda (date)
    (lambda (user)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (string? (getCuentaLogueada (getSNSF socialnetwork)) (Fecha? date) (string? user)))
          (cond
            [(null? (getCuentaLogueada (getSNSF socialnetwork))) socialnetwork]
            [(null? (getCuentaXUsuario (getListaCuentas (getSNSF socialnetwork)) user)) socialnetwork] ;Verificar existencia de usuario
            [] ;Se debe verificar que no se siga a este usuario
            [else ()]) ;Se realiza seguimiento, se debe actualizar ambas listas
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:
;((login (EmptyFB "Facebook" (Fecha 22 5 2021) encryptFn encryptFn) (Fecha 22 5 2021) "user" "pass" post)(Fecha 22 5 2021)) "user1")
;
;

;///////////////////////////////////////////////////////////// SHARE ////////////////////////////////////////////////////////////////////////////////

;Funcion que, con sesion iniciada, comparte una publicacion ya realizada a otro usuario ;F
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x NombreUsuario)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (share socialnetwork)
  (lambda (date)
    (lambda (postID . listUser)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (integer? postID))
          (cond
            [(null? (getCuentaLogueada (getSNSF socialnetwork))) socialnetwork]
            []
            []
            []) ;Se verifica si el usuario ingresado existe
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:
;
;
;

;/////////////////////////////////////////////// SocialNetwork->string //////////////////////////////////////////////////////////////////////////

;SocialNetwork->string (si es que logro hacer Share, #difiiiiiishil)
;(define (SocialNetwork->string SocialNetwork)(encryptFn (getDecriptado SocialNetwork))

;/////////////////////////////////////////////////////// REQUERIMIENTOS FUNCIONALES OPTATIVOS ///////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////// LIKE //////////////////////////////////////////////////////////////////////////////////
(define (like socialnetwork) ;F
  (lambda (date)
    (lambda (postID)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (integer? postID))
          (cond
            [(null? (getCuentaLogueada (getSNSF socialnetwork))) socialnetwork]
            []
            []
            []) ;Se verifica si el usuario ingresado existe
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion
;//////////////////////////////////////////////////////////// COMMENT ////////////////////////////////////////////////////////////////////////////////
(define (comment socialnetwork) ;F
  (lambda (date)
    (lambda (postID)
      (lambda (comment)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (integer? postID))
          (cond
            [(null? (getCuentaLogueada (getSNSF socialnetwork))) socialnetwork]
            []
            []
            []) ;Se verifica si el usuario ingresado existe
          socialnetwork)))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion