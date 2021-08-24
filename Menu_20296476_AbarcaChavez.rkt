#lang racket
;Laboratorio 1 Paradigmas de Programacion
;Archivo de requerimientos funcionales exigidos por enunciado
;Objetivo: Simular la interaccion de un usuario en una red social, implementando la variante bajo el Paradigma Funcional
;Nombre alumno: Eduardo Abarca
;RUT: 20.296.476-1
;Seccion: C-3
;Profesor: Daniel Gacitua
;Entrega: Comodin (24/08/2021) :)

;PEDIR ARCHIVOS EXTERNOS
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "CuentaUsuario_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")
(require "ListaCuentas_20296476_AbarcaChavez.rkt")
(require "ListaPublicaciones_20296476_AbarcaChavez.rkt")
(require "ListaReacciones_20296476_AbarcaChavez.rkt")
(require "ContenidoSN_20296476_AbarcaChavez.rkt")
(require "SocialNetwork_20296476_AbarcaChavez.rkt")

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;//////////////////////////////////////////////////////////// REQUERIMIENTOS FUNCIONALES EXIGIDOS ////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////////////////// REGISTER //////////////////////////////////////////////////////////////////////////

;Requerimiento funcional register, funcion que registra una nueva cuenta en la red social
;Dominio: (SocialNetwork x Fecha x string x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (register socialnetwork date username password)
  (if (and (SocialNetwork? socialnetwork) (Fecha? date) (string? username) (string? password)) ;Se verifican datos de entrada
      (if (EstaRegistrado (getListaCuentas (getContenidoSN socialnetwork)) username password) ;Si los datos ya estan registrados, no es posible el registro
          socialnetwork
          (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarContenidoSN null
                                                   (RegistrarCuenta (getListaCuentas (getContenidoSN socialnetwork)) (CrearCuentaUsuario username password date))
                                                   (getListaPublicaciones (getContenidoSN socialnetwork))
                                                   (getListaReacciones (getContenidoSN socialnetwork))))) ;Se actualiza TDA SocialNetwork
      socialnetwork))

;Ejemplos de uso:

;1) Ejemplo de registro simple (primero se crea funcion para llamar al constructor de SocialNetwork con valores por defecto a usar a futuro, algo asi como un correlativo)
(define (IBVacio)(SocialNetwork "Instabook" (Fecha 16 8 2021) encryptFn encryptFn))
(define (IB1)(register (IBVacio) (Fecha 16 8 2021) "a" "b"))

;2) Error: Usuario ingresado ya esta registrado
(define (IB1Error) (register (IB1) (Fecha 16 8 2021) "a" "b"))

;3) Registro de 2 usuarios, continuando el registro presentado en 1)
(define (IB2) (register (IB1) (Fecha 16 8 2021) "c" "d"))
(define (IB3) (register (IB2) (Fecha 16 8 2021) "e" "f"))

;////////////////////////////////////////////////////////////////////////// LOGIN ////////////////////////////////////////////////////////////////////////////

;Requerimiento funcional login, funcion que inicia sesion en la red social con un usuario ya registrado
;Dominio: (SocialNetwork x string x string x procedure)
;Recorrido: procedure
;Recorrido final: SocialNetwork
;Recursion: De cola

(define (login socialnetwork username password operation)
  (cond
    [(not (SocialNetwork? socialnetwork)) (operation socialnetwork)] ;Se necesita TDA SocialNetwork para iniciar sesion
    [(not (and (string? username) (string? password) (procedure? operation))) (operation socialnetwork)] ;Parametros deben tener ser de un tipo definido
    [(not (CredencialesCorrectas (getListaCuentas (getContenidoSN socialnetwork)) username password)) (operation socialnetwork)] ;Se debe verificar las credenciales ingresadas (aca se usa recursion de cola)
    [else (operation (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarContenidoSN username
                                                   (getListaCuentas (getContenidoSN socialnetwork))
                                                   (getListaPublicaciones (getContenidoSN socialnetwork))
                                                   (getListaReacciones (getContenidoSN socialnetwork)))))])) ;Se retorna operation, pero se debe actualizar TDA socialnetwork con el usuario logueado

;Ejemplos de uso:
;No se logro adaptar login para presentar ejemplos. Estos quedaran pendientes, a mostrar en siguientes requerimientos funcionales.

;/////////////////////////////////////////////////////////////////////////// POST ////////////////////////////////////////////////////////////////////////////

;Requerimiento funcional post, funcion que, con sesion iniciada, realiza un post en la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x string x list)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (post socialnetwork) 
  (lambda (date)
    (lambda (content . users)
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (string? content))
          (cond ;De haber usuarios a compartir, estos deben estar en la lista de contactos
            [(null? (getCuentaLogueada (getContenidoSN socialnetwork))) socialnetwork] ;Debe existir usuario con sesion iniciada
            [(not (list? users)) socialnetwork] ;Error en ingreso de cuentas a compartir -> debe ser lista
            [(not (ValidarDestinos (getCuentaXUsuario (getCuentaLogueada (getContenidoSN socialnetwork)) (getListaCuentas (getContenidoSN socialnetwork))) users)) socialnetwork]
            [else (ActualizarSocialNetwork socialnetwork ;Es posible registro, se actualiza el TDA SocialNetwork
                                   (ActualizarContenidoSN null
                                                   (getListaCuentas (getContenidoSN socialnetwork))
                                                   (AgregarPublicacion (getListaPublicaciones (getContenidoSN socialnetwork))
                                                                       (CrearPublicacion
                                                                        (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                                        0
                                                                        date
                                                                        content
                                                                        (getCuentaLogueada (getContenidoSN socialnetwork))
                                                                        users
                                                                        null
                                                                        ""
                                                                        ""))
                                                   (getListaReacciones (getContenidoSN socialnetwork))))]) ;Se realiza multiple registro de publicacion (abarcando los perfiles de cada usuario alcanzado (se debe verificar que existen))
          socialnetwork))))

;Ejemplos de uso:
;1) Post sin destinos (Ejemplo 1 pendiente de login)
(define (IB4) (((login (IB3) "a" "b" post)(Fecha 16 8 2021))"Post de prueba"))

;2) Error: No hay login
(define (IB4err1) (((post (IB4)) (Fecha 16 8 2021)) "Post error"))

;3) Error: Destinos no son validos
(define (IB4err2) (((login (IB4) "a" "b" post) (Fecha 16 8 2021))"Post de prueba" "c" "e"))

;4) Post con destinos validos (Pendiente, a mostrar en requerimiento funcional follow)

;////////////////////////////////////////////////////////////////////////// FOLLOW ///////////////////////////////////////////////////////////////////////////

;Requerimiento funcional follow, funcion que, con sesion iniciada, sigue a otra cuenta en la red social
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (follow socialnetwork)
  (lambda (date)
    (lambda (user)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (string? user))
          (cond
            [(null? (getCuentaLogueada (getContenidoSN socialnetwork))) socialnetwork] ;No hay sesion iniciada
            [(null? (getCuentaXUsuario user (getListaCuentas (getContenidoSN socialnetwork)))) socialnetwork] ;Usuario apuntado no existe
            [(EstaEnContactos user (getSeguidosCuenta (getCuentaXUsuario (getCuentaLogueada (getContenidoSN socialnetwork)) (getListaCuentas (getContenidoSN socialnetwork))))) socialnetwork] ;Usuario apuntado ya esta seguido
            [(eqv? (getCuentaLogueada (getContenidoSN socialnetwork)) user) socialnetwork] ;Se debe verificar que el usuario ingresado no sea de cuenta logueada
            [else (ActualizarSocialNetwork socialnetwork ;Es posible operacion, se agrega seguidor a cuenta logueada y se agrega seguidor a cuenta apuntada
                                   (ActualizarContenidoSN null
                                                   (ActualizarCuenta
                                                    (ActualizarCuenta
                                                     (getListaCuentas (getContenidoSN socialnetwork))
                                                     (ActualizarSeguidosCuenta
                                                      (getCuentaXUsuario (getCuentaLogueada (getContenidoSN socialnetwork))(getListaCuentas (getContenidoSN socialnetwork)))
                                                      (AgregarSeguimiento user date (getSeguidosCuenta (getCuentaXUsuario (getCuentaLogueada (getContenidoSN socialnetwork)) (getListaCuentas (getContenidoSN socialnetwork)))))))
                                                    (ActualizarSeguidoresCuenta
                                                     (getCuentaXUsuario user (getListaCuentas (getContenidoSN socialnetwork)))
                                                     (AgregarSeguidor (getCuentaLogueada (getContenidoSN socialnetwork)) date (getSeguidoresCuenta (getCuentaXUsuario user (getListaCuentas (getContenidoSN socialnetwork)))))))
                                                   (getListaPublicaciones (getContenidoSN socialnetwork))
                                                   (getListaReacciones (getContenidoSN socialnetwork))))])
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:
;1) Ejemplo simple de seguimiento (Ejemplo 2 pendiente de login)
(define (IB5) (((login (IB4) "c" "d" follow) (Fecha 16 8 2021)) "a"))

;2) Error: No hay sesion iniciada
(define (IB5err1) (((follow (IB5)) (Fecha 16 8 2021)) "c"))

;3) Error: Usuario a seguir no existe
(define (IB5err2) (((login (IB5) "c" "d" follow) (Fecha 16 8 2021)) "TrabajoTerminado"))

;4) Error: Usuario a seguir es usuario logueado
(define (IB5err3) (((login (IB5) "c" "d" follow) (Fecha 16 8 2021)) "c"))

;5) Error: Usuario a seguir ya esta seguido
(define (IB5err4) (((login (IB5) "c" "d" follow) (Fecha 16 8 2021)) "a"))

;6) Ejemplo pendiente en requerimiento anterior: Crear post con destinatarios (se necesita de este requerimiento para poder realizar tal post)
(define (IB6) (((login (IB5) "c" "d" post) (Fecha 16 8 2021)) "Post de prueba con destinatario" "a"))

;////////////////////////////////////////////////////////////////////////// SHARE ////////////////////////////////////////////////////////////////////////////

;Requerimiento funcional share, funcion que, con sesion iniciada, comparte una publicacion ya realizada a otro usuario
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
            [(null? (getCuentaLogueada (getContenidoSN socialnetwork))) socialnetwork] ;No hay login
            [(null? (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork)))) socialnetwork] ;Post con ese ID no existe
            [(null? listUser) (ActualizarSocialNetwork socialnetwork (ActualizarContenidoSN null
                                                     (getListaCuentas (getContenidoSN socialnetwork))
                                                     (AgregarPublicacion (getListaPublicaciones (getContenidoSN socialnetwork))
                                                                       (CrearPublicacion
                                                                        (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                                        postID
                                                                        (getFechaRegistroP (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork))))
                                                                        (getContenidoP(getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork))))
                                                                        (getAutorP(getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork))))
                                                                        (getDestinosP (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork))))
                                                                         date
                                                                        (getCuentaLogueada (getContenidoSN socialnetwork))
                                                                        (getCuentaLogueada (getContenidoSN socialnetwork))))
                                                     (getListaReacciones (getContenidoSN socialnetwork))))] ;No hay destinos, se comparte publicacion en espacio propio de cuenta logueada
            [(not (ValidarDestinos (getCuentaXUsuario (getCuentaLogueada (getContenidoSN socialnetwork)) (getListaCuentas (getContenidoSN socialnetwork))) listUser)) socialnetwork] ;Los destinos no son validos
            [else (ActualizarSocialNetwork socialnetwork (ActualizarContenidoSN null
                                                     (getListaCuentas (getContenidoSN socialnetwork))
                                                     (CompartirPublicacionesADestinos (getListaPublicaciones (getContenidoSN socialnetwork)) (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork))) (getCuentaLogueada (getContenidoSN socialnetwork)) postID date listUser (length (getListaReacciones (getContenidoSN socialnetwork))))
                                                     (getListaReacciones (getContenidoSN socialnetwork))))]) ;Pruebas pasadas, se comparte la publicacion a todos los usuarios de la lista
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:
;Ejemplo simple de share, sin destinatarios (i.e., a espacio propio) (Ejemplo 3 pendiente de login)
(define (IB7) (((login (IB6) "e" "f" share) (Fecha 16 8 2021)) 1))

;Error: Destinatario(s) no son validos
(define (IB7err1) (((login (IB7) "a" "b" share) (Fecha 16 8 2021)) 1 "e"))

;Error: ID de publicacion no existe
(define (IB7err2) (((login (IB7) "a" "b" share) (Fecha 16 8 2021)) -1))

;Ejemplo de share a multiples destinos
(define (IB8) (((login (IB7) "a" "b" follow) (Fecha 16 8 2021)) "c"))
(define (IB9) (((login (IB8) "a" "b" follow) (Fecha 16 8 2021)) "e"))
(define (IB10) (((login (IB9) "a" "b" share) (Fecha 16 8 2021)) 1 "c" "e"))

;////////////////////////////////////////////////////////////////// SOCIALNETWORK->STRING ////////////////////////////////////////////////////////////////////

;Requerimiento funcional socialnetwork->string, funcion que convierte toda la informacion de la red social a un string legible
;Dominio: SocialNetwork
;Recorrido: String
;Recursion: Natural
(define (socialnetwork->string socialnetwork)
  (if (SocialNetwork? socialnetwork)
      (PrevSocialNetwork->string socialnetwork)
      "Dato ingresado no es TDA SocialNetwork.\n"))

;Ejemplos de uso:
;1) Ejemplo de muestra red social sin login
(define (IB10->string)(socialnetwork->string (IB10)))

;Ejemplo de muestra red social con login (Ejemplo 4 pendiente de login)
(define (IB10Logged->string) (login (IB10) "a" "b" socialnetwork->string))

;Error: Elemento ingresado no es TDA SocialNetwork
(define (IB10->stringErr) (socialnetwork->string null))

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////// REQUERIMIENTOS FUNCIONALES OPTATIVOS ////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////////////////// COMMENT ///////////////////////////////////////////////////////////////////////////

;Requerimiento funcional comment, funcion que, con sesion iniciada, realiza un comentario en la red social, ya sea en una publicacion o a otro comentario
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x integer x string)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (comment socialnetwork)
  (lambda (date)
    (lambda (postID)
      (lambda (comment)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (integer? postID) (string? comment))
          (cond
            [(null? (getCuentaLogueada (getContenidoSN socialnetwork))) socialnetwork] ;No hay login
            [(and (null? (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork)))) (null? (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork))))) socialnetwork] ;Se obtiene null de ambos getters, TDA con ese ID no existe
            [(not (null? (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork))))) ;Si el ID ingresado corresponde a una reaccion
             (cond
               [(eqv? (getTipoR (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork)))) "Like") socialnetwork] ;No se puede comentar un like
                [else (ActualizarSocialNetwork socialnetwork ;Si es un comentario, se comenta como respuesta
                                           (ActualizarContenidoSN
                                            null
                                            (getListaCuentas (getContenidoSN socialnetwork))
                                            (getListaPublicaciones (getContenidoSN socialnetwork))
                                            (AgregarReaccion (getListaReacciones (getContenidoSN socialnetwork))
                                                             (CrearReaccion
                                                              (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                              postID
                                                              date
                                                              (getCuentaLogueada (getContenidoSN socialnetwork))
                                                              "Comentario"
                                                              comment))))])]
            [else (ActualizarSocialNetwork socialnetwork ;Caso contrario, es una publicacion la apuntada
                                           (ActualizarContenidoSN
                                            null
                                            (getListaCuentas (getContenidoSN socialnetwork))
                                            (getListaPublicaciones (getContenidoSN socialnetwork))
                                            (AgregarReaccion (getListaReacciones (getContenidoSN socialnetwork))
                                                             (CrearReaccion
                                                              (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                              postID
                                                              date
                                                              (getCuentaLogueada (getContenidoSN socialnetwork))
                                                              "Comentario"
                                                              comment))))]) ;Pruebas pasadas, se crea el comentario
          socialnetwork)))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:

;1) Ejemplo simple de comentario a una publicacion
(define (IB11) ((((login (IB10) "a" "b" comment) (Fecha 16 8 2021)) 1) "Comentario de prueba"))

;2) Error: Publicacion/Reaccion con ese ID no existe
(define (IB11err1) ((((login (IB10) "a" "b" comment) (Fecha 16 8 2021)) -1) "No pasa nada aqui :3"))

;3) Error: Reaccion encontrada es un like (Pendiente, a revisar en siguente requerimiento)

;4) Ejemplo de comentario hacia otro comentario
(define (IB12) ((((login (IB11) "a" "b" comment) (Fecha 16 8 2021)) 6) "Comentario de prueba hacia otro comentario"))

;Caso no abordado: Autor Publicacion/Comentario apuntado debe estar en contactos.

;/////////////////////////////////////////////////////////////////////////// LIKE ////////////////////////////////////////////////////////////////////////////

;Requerimiento funcional like, funcion que, con sesion iniciada, realiza un like en la red social, ya sea en una publicacion o un comentario
;Dominio: (SocialNetwork)
;Dominio por currificacion: (Fecha x integer)
;Recorrido: SocialNetwork
;Recursion: Natural
(define (like socialnetwork)
  (lambda (date)
    (lambda (postID)
      ;Revisar si tenemos TDA socialnetwork, hay usuario logueado y los datos ingresados por lambda son correctos
      (if (and (SocialNetwork? socialnetwork) (Fecha? date) (integer? postID))
          (cond
            [(null? (getCuentaLogueada (getContenidoSN socialnetwork))) socialnetwork] ;No hay cuenta logueada, no se puede realizar like
            [(and (null? (getPublicacionXId postID (getListaPublicaciones (getContenidoSN socialnetwork)))) (null? (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork))))) socialnetwork] ;Se obtiene null de ambos getters, TDA con ese ID no existe
            [(not (null? (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork))))) ;Si el ID apunta a una reaccion
             (cond
               [(eqv? (getTipoR (getReaccionXIdR postID (getListaReacciones (getContenidoSN socialnetwork)))) "Like") socialnetwork] ;No se puede dar like a otro like
               [else (ActualizarSocialNetwork socialnetwork ;Se agrega like a comentario
                                           (ActualizarContenidoSN
                                            null
                                            (getListaCuentas (getContenidoSN socialnetwork))
                                            (getListaPublicaciones (getContenidoSN socialnetwork))
                                            (AgregarReaccion (getListaReacciones (getContenidoSN socialnetwork))
                                                             (CrearReaccion
                                                              (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                              postID
                                                              date
                                                              (getCuentaLogueada (getContenidoSN socialnetwork))
                                                              "Like"
                                                              ""))))])]
            [else (ActualizarSocialNetwork socialnetwork ;Se agrega like a publicacion
                                           (ActualizarContenidoSN
                                            null
                                            (getListaCuentas (getContenidoSN socialnetwork))
                                            (getListaPublicaciones (getContenidoSN socialnetwork))
                                            (AgregarReaccion (getListaReacciones (getContenidoSN socialnetwork))
                                                             (CrearReaccion
                                                              (+ (length (getListaPublicaciones (getContenidoSN socialnetwork))) (length (getListaReacciones (getContenidoSN socialnetwork))) 1)
                                                              postID
                                                              date
                                                              (getCuentaLogueada (getContenidoSN socialnetwork))
                                                              "Like"
                                                              ""))))]) ;Pruebas pasadas, se procede a crear like
          socialnetwork))));Datos ingresados erroneos, se retorna socialnetwork sin actualizacion

;Ejemplos de uso:

;1) Ejemplo simple de like a una publicacion
(define (IB13) (((login (IB12) "a" "b" like) (Fecha 16 8 2021)) 1))

;2) Error: Publicacion/Reaccion con ese ID no existe
(define (IB13err1) (((login (IB13) "a" "b" like) (Fecha 16 8 2021)) -1))

;3) Error: Reaccion encontrada es un like (Pendiente de requerimiento anterior)
(define (IB13err2) ((((login (IB10) "a" "b" comment) (Fecha 16 8 2021)) 8) "Comentando un like, las cosas que uno tiene que hacer..."))

;4) Error: Reaccion encontrada es un like (Esta vez desde este requerimiento)
(define (IB13err3) (((login (IB13) "a" "b" like) (Fecha 16 8 2021)) 8))

;5) Ejemplo de like a un comentario
(define (IB14) (((login (IB13) "a" "b" like) (Fecha 16 8 2021)) 6))

;Caso no abordado: Like en publicacion/comentario no puede repetirse por parte de un mismo usuario (i.e., dar like mas de una vez a una misma publicacion/comentario)

;/////////////////////////////////////////////////////////////////////////// FIN //////////////////////////////////////////////////////////////////////////////

;ASPECTOS A ARREGLAR:
; - POST Y SHARE: DESTINOS NO VALIDOS SE DEBEN FILTRAR DE LA LISTA, NO MATAR LA OPERACION POR UN DESTINO NO VALIDO
; - COMMENT: SOLO SE PUEDE COMENTAR PUBLICACIONES/COMENTARIOS DE CONTACTOS
; - LIKE: SOLO SE PUEDE DAR LIKE A PUBLICACION O COMENTARIO 1 VEZ, NO SE PUEDE REPETIR