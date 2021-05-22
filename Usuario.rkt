#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;TDA Usuario
;Composicion: (string x string x Fecha x string) -> (usuarioRegistro x contraRegistro x FechaRegistro x nombreCuenta) (Sujeto a cambios)

;Constructor
(define (crearUsuario usuario contra fechaRegistro nombreCuenta)(list usuario contra fechaRegistro nombreCuenta))

;Selectores
(define (getNombreUsuario Usuario) (car Usuario))
(define (getContraUsuario Usuario) (cadr Usuario))
(define (getFechaRegistroU Usuario) (caddr Usuario))
(define (getNombreCuentaU Usuario) (cadddr Usuario))

;Pertenencia
(define (Usuario? Usuario)
  (cond
    [(not (list? Usuario)) #f]
    [(not (= (length Usuario) 4)) #f]
    [(string? (getNombreUsuario Usuario))#f]
    [(string? (getContraUsuario Usuario))#f]
    [(Fecha? (getFechaRegistroU Usuario))#f]
    [(string? (getNombreCuentaU Usuario))#f] ;De alguna manera, se debe comprobar que este usuario no este ocupado
    [else #t]))

;Modificadores

;Otros

;DETALLES RESPECTO A ESTE TDA:
;- ACTUALMENTE EXISTE UNA CONFUSION RESPECTO A ESTE TDA Y EL TDA CUENTA
;- POR AHORA, RESPECTO A LO ANTERIOR, SE ASUMIRÁ LO SIGUIENTE:
;    *ESTE TDA FUNCIONARA COMO UN CONTENEDOR DE CREDENCIALES QUE UNICAMENTE SERVIRA PARA PERMITIR AL USUARIO INGRESAR A SU CUENTA EN LA RED SOCIAL
;    *POR EL CONTRARIO, TDA CUENTA FUNCIONARA COMO LA CARA VISIBLE DE LO QUE SERIA UN USUARIO DE RED SOCIAL
 
;TDA ListaUsuarios
;Composicion: (Usuario x Usuario x ... x Usuario)

;Constructor
(define (crearListaUsuarios)(list null))

;Pertenencia