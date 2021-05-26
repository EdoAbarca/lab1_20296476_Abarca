#lang racket
;Pedir archivos externos
(require "Fecha.rkt")
(require "Usuario.rkt")

;Entregar funciones
(provide CrearListaUsuarios)
(provide ListaUsuarios?)
(provide EfectuarRegistro)
(provide EstaRegistrado)
(provide CredencialesSonCorrectas)

;TDA ListaUsuarios (EVALUAR MOVER FECHA REGISTRO A CUENTA, PUES USUARIO SOLO CORRESPONDE A CREDENCIALES)
;Composicion: (Usuario x Usuario x ... x Usuario)

;Constructor
(define (CrearListaUsuarios) (list null))

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
;Modificador para registrar un nuevo usuario en caso de que este disponible
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

