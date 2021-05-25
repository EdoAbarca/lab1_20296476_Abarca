#lang racket
;Pedir archivos externos
(require "Fecha.rkt")

;Entregar funciones
(provide ListaUsuarios?)

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
;    *POR EL CONTRARIO, TDA CUENTA FUNCIONARA COMO LA CARA VISIBLE DE LO QUE SERIA UNA CUENTA DE RED SOCIAL, CON SEGUIDORES, SEGUIDOS Y PUBLICAICONES (APARTE)
; ESTE TDA NO SE MOSTRARA POR PANTALLA EN SOCIALNETWORK -> STRING

;TDA ListaUsuarios
;Composicion: (Usuario x Usuario x ... x Usuario)

;Constructor
(define (crearListaUsuarios) (list null))

;Selectores

;Pertenencia
(define (ListaUsuarios? ListaUsuarios)
  (if (not (list? ListaUsuarios))
      #f
      (if (= (length ListaUsuarios) 0)
          #t
          (if (Usuario? (car ListaUsuarios))
              (ListaUsuarios? (cdr ListaUsuarios))
              #f))))

;Modificadores
;Modificador para agregar un nuevo usuario en caso de que este disponible
;Datos se verifican antes de llegar a esta funcion
(define (EfectuarRegistro ListaUsuarios NuevoUsuario)
  (if (= (length ListaUsuarios) 0)
      (cons NuevoUsuario null)
      (cons (car ListaUsuarios) (EfectuarRegistro (cdr ListaUsuarios) NuevoUsuario))))


;Otros
;Verificador que revisa si el usuario a registrar esta en uso (para register)
(define (EstaRegistrado ListaUsuarios usuario pass)
  (cond
    ;Si los datos ingresados no cumplen con dominio, no tiene sentido decir que es posible registrar el usuario
    [(not (and (ListaUsuarios? ListaUsuarios) (string? usuario) (string? pass))) #t]
    ;Lista recorrida completamente, el usuario ingresado esta disponible para ser registrado
    [(= (length ListaUsuarios) 0) #f]
    ;Si el usuario actual es igual al ingresado, no es posible realizar el registro
    [(or (eqv? (getNombreUsuario (car ListaUsuarios)) usuario) (eqv? (getPassUsuario (car ListaUsuarios)) pass)) #t]
    ;No se cumple ninguno de los puntos anteriores, se revisa el siguiente usuario registrado
    [else (EstaRegistrado (cdr ListaUsuarios) usuario pass)]))

;Verificador que revisa si las credenciales ingresadas para iniciar sesion son correctas (login)
(define (CredencialesSonCorrectas ListaUsuarios usuario pass)
  (cond
    ;Si los datos ingresados no cumplen con dominio, no tiene sentido decir que es posible verificar el usuario a iniciar sesion
    [(not (and (ListaUsuarios? ListaUsuarios) (string? usuario) (string? pass))) #f]
    ;Lista recorrida completamente, no existe un usuario con tales credenciales
    [(= (length ListaUsuarios) 0) #f]
    ;Si el usuario actual coincide con las credenciales ingresadas, se procede a iniciar sesion
    [(and (eqv? (getNombreUsuario (car ListaUsuarios)) usuario) (eqv? (getPassUsuario (car ListaUsuarios)) pass)) #t]
    ;No se cumple ninguno de los puntos anteriores, se revisa el siguiente usuario registrado
    [else (CredencialesSonCorrectas (cdr ListaUsuarios) usuario pass)]))
