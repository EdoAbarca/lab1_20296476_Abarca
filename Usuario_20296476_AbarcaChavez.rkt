#lang racket
;Entregar funciones
(provide (all-defined-out))

;TDA Usuario
;Composicion: (string x string) -> (usuarioRegistro x contraseniaRegistro)

;Constructor
(define (CrearUsuario usuario pass)(list usuario pass))

;Selectores
(define (getNombreUsuario Usuario) (car Usuario))
(define (getPassUsuario Usuario) (cadr Usuario))

;Pertenencia
(define (Usuario? Usuario)
  (cond
    [(not (list? Usuario)) #f] ;Los TDAs se basan en listas, el parametro de entrada DEBE ser una lista
    [(not (= (length Usuario) 2)) #f] ;TDA se compone de 2 elementos
    [(not (string? (getNombreUsuario Usuario)))#f] ;Nombre de usuario debe ser string
    [(not (string? (getPassUsuario Usuario))) #f] ;Nombre de usuario debe ser string
    [else #t])) ;Pruebas pasadas, elemento ingresado es TDA usuario

;Modificadores

;Otros

;DETALLES RESPECTO A ESTE TDA:
;- ACTUALMENTE EXISTE UNA CONFUSION RESPECTO A ESTE TDA Y EL TDA CUENTA
;- DEBIDO A LO ANTERIOR, SE ASUMIRÁ LO SIGUIENTE:
;    *ESTE TDA FUNCIONARA COMO UN CONTENEDOR DE CREDENCIALES QUE UNICAMENTE SERVIRA PARA PERMITIR AL USUARIO INGRESAR A SU CUENTA EN LA RED SOCIAL
;    *POR EL CONTRARIO, TDA CUENTA FUNCIONARA COMO LA CARA VISIBLE DE LO QUE SERIA UNA CUENTA DE RED SOCIAL, CON SEGUIDORES, SEGUIDOS Y PUBLICACIONES (APARTE)
; ESTE TDA NO SE MOSTRARA POR PANTALLA EN SOCIALNETWORK -> STRING NI SE ENCRIPTARA