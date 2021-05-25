#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide CrearUsuario)
(provide getNombreUsuario)
(provide getPassUsuario)
(provide getFechaRegistroU)
(provide Usuario?)

;TDA Usuario
;Composicion: (string x string x Fecha) -> (usuarioRegistro x contraseniaRegistro x FechaRegistro) (Sujeto a cambios)

;Constructor
(define (CrearUsuario usuario pass fechaRegistro)(list usuario pass fechaRegistro))

;Selectores
(define (getNombreUsuario Usuario) (car Usuario))
(define (getPassUsuario Usuario) (cadr Usuario))
(define (getFechaRegistroU Usuario) (caddr Usuario))

;Pertenencia
(define (Usuario? Usuario)
  (cond
    [(not (list? Usuario)) #f]
    [(not (= (length Usuario) 3)) #f]
    [(not (string? (getNombreUsuario Usuario)))#f]
    [(not (string? (getPassUsuario Usuario))) #f]
    [(not (Fecha? (getFechaRegistroU Usuario))) #f]
    [else #t]))

;Modificadores

;Otros

;DETALLES RESPECTO A ESTE TDA:
;- ACTUALMENTE EXISTE UNA CONFUSION RESPECTO A ESTE TDA Y EL TDA CUENTA
;- POR AHORA, RESPECTO A LO ANTERIOR, SE ASUMIRÁ LO SIGUIENTE:
;    *ESTE TDA FUNCIONARA COMO UN CONTENEDOR DE CREDENCIALES QUE UNICAMENTE SERVIRA PARA PERMITIR AL USUARIO INGRESAR A SU CUENTA EN LA RED SOCIAL
;    *POR EL CONTRARIO, TDA CUENTA FUNCIONARA COMO LA CARA VISIBLE DE LO QUE SERIA UNA CUENTA DE RED SOCIAL, CON SEGUIDORES, SEGUIDOS Y PUBLICACIONES (APARTE)
; ESTE TDA NO SE MOSTRARA POR PANTALLA EN SOCIALNETWORK -> STRING


