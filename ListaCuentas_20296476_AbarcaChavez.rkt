#lang racket

;Entregar todas las funciones implementadas en este archivo
(provide (all-defined-out))

;Pedir archivos
(require "Fecha_20296476_AbarcaChavez.rkt")
(require "CuentaUsuario_20296476_AbarcaChavez.rkt")

;TDA ListaCuentas
;Composicion: (Cuenta1 x Cuenta2 x ... x CuentaN)
;            -> (TDA CuentaUsuario x TDA CuentaUsuario x ... x TDA CuentaUsuario)

;Constructor
(define (CrearListaCuentas)(list null))

;Selectores
;Selector de TDA CuentaUsuario en TDA ListaUsuarios, usando como elemento de comparacion el username
;Dominio: string x ListaCuentas
;Recorrido: TDA Cuenta/null
(define (getCuentaXUsuario UsuarioCuenta ListaCuentas)
  (if (not (and (string? UsuarioCuenta) (ListaCuentas? ListaCuentas))) ;Datos erroneos
      null
      (if (= (length ListaCuentas) 0) ;No existe cuenta con ese usuario
          null
          (if (eqv? (getUsuarioCuenta (car ListaCuentas)) UsuarioCuenta) ;Si se encontro cuenta
              (car ListaCuentas) ;Se retorna
              (getCuentaXUsuario UsuarioCuenta (cdr ListaCuentas)))))) ;Se busca siguiente elemento

;Pertenencia
(define (ListaCuentas? ListaCuentas)

      (if (= (length ListaCuentas) 0) ;Lista recorrida (o vacia) sirve como TDA ListaCuentas
          #t
          (if (CuentaUsuario? (car ListaCuentas)) ;Elemento seleccionado debe ser TDA Cuenta
              (ListaCuentas? (cdr ListaCuentas)) ;Se revisa siguiente elemento
              #f))) ;NO es TDA Cuenta, el parametro no vale como TDA ListaCuenta


;Modificadores
;Modificador que agrega una nueva cuenta a la lista de cuentas
(define (RegistrarCuenta ListaCuentas CuentaUsuario)
  (if (= (length ListaCuentas) 0) ;Final lista
      (cons CuentaUsuario null) ;Se guarda nueva cuenta
      (cons (car ListaCuentas) (RegistrarCuenta (cdr ListaCuentas) CuentaUsuario)))) ;Se va generando la nueva lista...

;Modificador para actualizar cuenta en lista cuentas
(define (ActualizarCuenta ListaCuentas CuentaUsuarioAct)
  (if (= (length ListaCuentas) 0)
      null
      (if(eqv? (getUsuarioCuenta (car ListaCuentas)) (getUsuarioCuenta CuentaUsuarioAct))
         (cons CuentaUsuarioAct (ActualizarCuenta (cdr ListaCuentas) CuentaUsuarioAct))
         (cons (car ListaCuentas) (ActualizarCuenta (cdr ListaCuentas) CuentaUsuarioAct)))))

;Otros
;Verificador que revisa si el usuario a registrar esta en uso (para register)
(define (EstaRegistrado ListaCuentas User Pass)
  (cond
    ;Si los datos ingresados no cumplen con dominio, no tiene sentido decir que es posible registrar el usuario
    [(not (and (ListaCuentas? ListaCuentas) (string? User) (string? Pass))) #t]
    ;Lista recorrida completamente, el usuario ingresado esta disponible para ser registrado
    [(= (length ListaCuentas) 0) #f]
    ;Si el usuario actual es igual al ingresado, no es posible realizar el registro
    [(or (eqv? (getUsuarioCuenta (car ListaCuentas)) User) (eqv? (getContraseniaCuenta (car ListaCuentas)) Pass)) #t]
    ;No se cumple ninguno de los puntos anteriores, se revisa el siguiente usuario registrado
    [else (EstaRegistrado (cdr ListaCuentas) User Pass)]))


;Verificador que revisa si las credenciales ingresadas para iniciar sesion son correctas (login)
(define (CredencialesCorrectas ListaCuentas User Pass)
  (cond
    ;Si los datos ingresados no cumplen con dominio, no tiene sentido decir que es posible verificar el usuario a iniciar sesion
    [(not (and (ListaCuentas? ListaCuentas) (string? User) (string? Pass))) #f]
    ;Lista recorrida completamente, no existe un usuario con tales credenciales
    [(= (length ListaCuentas) 0) #f]
    ;Si el usuario actual coincide con las credenciales ingresadas, se procede a iniciar sesion
    [(and (eqv? (getUsuarioCuenta (car ListaCuentas)) User) (eqv? (getContraseniaCuenta (car ListaCuentas)) Pass)) #t]
    ;No se cumple ninguno de los puntos anteriores, se revisa el siguiente usuario registrado
    [else (CredencialesCorrectas (cdr ListaCuentas) User Pass)]))