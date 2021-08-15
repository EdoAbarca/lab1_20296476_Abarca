#lang racket
;Pedir archivos externos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "CuentaUsuario_20296476_AbarcaChavez.rkt")
(require "Publicacion_20296476_AbarcaChavez.rkt")
(require "Reaccion_20296476_AbarcaChavez.rkt")
(require "ListaCuentas_20296476_AbarcaChavez.rkt")
(require "ListaPublicaciones_20296476_AbarcaChavez.rkt")
(require "ListaReacciones_20296476_AbarcaChavez.rkt")

;Entregar funciones
(provide (all-defined-out))

;TDA ContenidoSN
;Composicion: (string x ListaCuentas x ListaPublicaciones x ListaReacciones)

;Constructores
(define (ContenidoSNVacio) (list null null null null))

;Selectores
(define (getCuentaLogueada ContenidoSN) (car ContenidoSN))
(define (getListaCuentas ContenidoSN) (cadr ContenidoSN))
(define (getListaPublicaciones ContenidoSN) (caddr ContenidoSN))
(define (getListaReacciones ContenidoSN) (cadddr ContenidoSN))

;Pertenencia
(define (ContenidoSN? ContenidoSN)
  (if (not (list? ContenidoSN)) ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
      #f
      (cond
        [(not (= (length ContenidoSN) 4)) #f] ;TDA ContenidoSN tiene 4 elementos
        [(not (or (string? (getCuentaLogueada ContenidoSN)) (null? (getCuentaLogueada ContenidoSN)))) #f] ;Usuario debe ser null (sin log) o string (con log)
        [(not (ListaCuentas? (getListaCuentas ContenidoSN))) #f] ;TDA ListaCuentas
        [(not (ListaPublicaciones? (getListaPublicaciones ContenidoSN))) #f] ;TDA ListaPublicaciones
        [(not (ListaReacciones? (getListaReacciones ContenidoSN))) #f] ;TDA ListaReacciones
        [else #t]))) ;Pruebas pasadas, el elemento ingresado por parametro es TDA SNSF

;Modificadores
(define (ActualizarContenidoSN CuentaLogueada ListaCuentas ListaPublicaciones ListaReacciones)
  (list CuentaLogueada ListaCuentas ListaPublicaciones ListaReacciones))

;Evaluar agregar idea anterior: Agregar modificador por codigo (probar luego de dejar programa funcional)

;Otros
;Funcion de encriptacion
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
;SITUACION: NO RESULTO MOVER ESTAS FUNCIONES A TDA SOCIALNETWORK, POR LO QUE SE TUVO QUE CREAR ESTE TDA PARA PODER HACER EFECTIVA LA CONVERSION DE LOS DATOS A STRING

;//////////////////////////////////////////// FUNCIONES DE CONVERSION DE LISTAS A STRING ///////////////////////////////////////////////////////////////////

;ESCENARIO IDEAL: MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR NOMBRE, FECHA REGISTRO, CONTACTOS, ACTIVIDAD DEL USUARIO Y REACCIONES A ESTA ACTIVIDAD, PARA LUEGO AVANZAR AL SIGUIENTE
;ESCENARIO TEMPORAL (DEFINITIVO PROBABLEMENTE): MIENTRAS NO SE MUESTREN TODOS LOS USUARIOS -> MOSTRAR DATOS USUARIOS -> PUBLICACIONES -> REACCIONES DE LA PUBLICACION APUNTADA (ID)

;Funcion de conversion de TDA Fecha a string
;Dominio: TDA Fecha
;Recorrido: string
(define (Fecha->string Fecha)
  (string-append "Fecha registro: " (number->string (getDia Fecha)) "/" (number->string (getMes Fecha)) "/" (number->string (getAnio Fecha)) "\n"))

;Funcion de conversion de lista contactos (seguidores/seguidos cuenta) a string
;Dominio: list
;Recorrido: string
;Recursion: Natural
(define (Contactos->string Contactos)
  (cond
    [(= (length Contactos) 0) "\n"]
    [else (string-append (car (car Contactos)) ", desde " (Fecha->string (cadr (car Contactos))) "\n" (Contactos->string (cdr Contactos)))]))

;Funcion de conversion de TDA ListaReacciones a string si IdPublicacion de TDA Reaccion apuntado coincide con IdPublicacion por parametro
;Dominio: (SocialNetwork x ListaReacciones x integer)
;Recorrido: string
;Recursion: Natural
(define (ReaccionesPublicacion->string ContenidoSN ListaReacciones IdPublicacion)
  (cond
    [(= (length ListaReacciones) 0) "\n"]
    [(= (getIdER (car ListaReacciones)) IdPublicacion)
     (string-append
      "\nID Reaccion " (number->string (getIdR (car ListaReacciones)))
      "\nID Elemento reaccionado: " (number->string (getIdER (car ListaReacciones)))
      "\nFecha reaccion: " (Fecha->string (getFechaR (car ListaReacciones)))
      "\nAutor reaccion: " (getAutorR (car ListaReacciones))
      "\nTipo reaccion: " (getTipoR (car ListaReacciones))
      "\nContenido reaccion: " (getContenidoR (car ListaReacciones))
      (ReaccionesPublicacion->string ContenidoSN (cdr ListaReacciones) IdPublicacion))]
    [else (ReaccionesPublicacion->string ContenidoSN (cdr ListaReacciones) IdPublicacion)]))


;Funcion de conversion de TDA ListaPublicaciones a string si NombreCuenta de TDA Publicacion apuntado coincide con NombreCuenta por parametro
;Dominio: (SocialNetwork x ListaPublicaciones x string)
;Recorrido: string
;Recursion: Natural
(define (Publicaciones->string ContenidoSN ListaPublicaciones UsuarioCuenta)
  (cond
    [(= (length ListaPublicaciones) 0) "\n"]
    [(eqv? (getAutorP (car ListaPublicaciones)) UsuarioCuenta) (string-append
                                                                  "\nID: " (number->string (getIdP (car ListaPublicaciones))) ;Pasar a string
                                                                  "\nCuenta que creo publicacion: " (getAutorP (car ListaPublicaciones))
                                                                  "\nTipo de publicacion: " (getTipoP (car ListaPublicaciones))
                                                                  "\nContenido publicacion: " (getContenidoP (car ListaPublicaciones))
                                                                  "\nCuenta que compartio esta publicacion: "(getComparteP (car ListaPublicaciones))
                                                                  "\nCuenta destino: " (getRecibeP (car ListaPublicaciones))
                                                                  "\n\nREACCIONES A ESTA PUBLICACION:\n\n"
                                                                  (ReaccionesPublicacion->string ContenidoSN (getListaReacciones ContenidoSN) (getIdP (car ListaPublicaciones)))
                                                                  (Publicaciones->string ContenidoSN (cdr ListaPublicaciones) UsuarioCuenta))]
    [else (Publicaciones->string ContenidoSN (cdr ListaPublicaciones) UsuarioCuenta)]))

;Funcion de conversion de TDA ListaCuentas a string considerando 2 casos:
; - Sin usuario logueado (CuentaLogueada -> null) -> se transforma toda la lista
; - Con usuario logueado (CuentaLogueada -> not(null)) -> se transforma solo la cuenta involucrada (Para SocialNetwork -> string)
;Dominio: (SocialNetwork x ListaCuentas x string/null)
;Recorrido: string
(define (Cuentas->string ContenidoSN ListaCuentas CuentaLogueada)
  (cond
    [(= (length ListaCuentas) 0) "\n"] ;Lista recorrida o vacia, salto de linea como condicion de borde
    [(null? CuentaLogueada) ;No hay sesion iniciada, se debe mostrar todo lo que hay en el(los) stacks
     (string-append
      "Usuario: "
      (getUsuarioCuenta (car ListaCuentas))
      "\nFecha de registro: "
      (Fecha->string (getFechaRegistroC (car ListaCuentas)))
      "\n\nSeguidores:\n\n"
      (Contactos->string (getSeguidoresCuenta (car ListaCuentas)))
      "\nSiguendo:\n\n"
      (Contactos->string (getSeguidosCuenta (car ListaCuentas)))
      "\nPublicaciones:\n\n"
      (Publicaciones->string ContenidoSN (getListaPublicaciones ContenidoSN) (getUsuarioCuenta (car ListaCuentas)))
      (Cuentas->string ContenidoSN (cdr ListaCuentas) CuentaLogueada))]
    [else (cond
            [(eqv? (getUsuarioCuenta (car ListaCuentas)) CuentaLogueada)
             (string-append
              "Registrado como: "
              CuentaLogueada
              "\nDatos:\n\nFecha registro:"
              (Fecha->string (getFechaRegistroC (car ListaCuentas)))
              "\nSeguidores:\n\n"
              (Contactos->string (getSeguidoresCuenta (car ListaCuentas)))
              "\nSiguendo:\n"
              (Contactos->string (getSeguidosCuenta (car ListaCuentas)))
              "\nPublicaciones:\n\n"
              (Publicaciones->string ContenidoSN (getListaPublicaciones ContenidoSN) CuentaLogueada))]
            [else (Cuentas->string ContenidoSN (cdr ListaCuentas) CuentaLogueada)])]))

;Funcion de conversion parcial de TDA socialnetwork a string (elementos fuera de la firma del constructor)
;Dominio: SocialNetwork
;Recorrido: string
(define (ContenidoSN->string ContenidoSN)
  (if (ContenidoSN? ContenidoSN)
      (string-append "Informacion del contenido almacenado en la red social:\n\n" (Cuentas->string ContenidoSN (getListaCuentas ContenidoSN) (getCuentaLogueada ContenidoSN)))
      ("Error: Elemento ingresado no es TDA ContenidoSN, no es posible transformar la informacion almacenada.\n")))
