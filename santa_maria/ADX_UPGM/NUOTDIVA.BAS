!-----------------------------------------------------------------------------
! Modulo: NUOTDIVA.BAS
! Fecha      : 23 de junio de 2022
! Autor      : Alejandro Farre P.
! Descripcion: Modulo para la devolucion de IVA a tercera edad y discapacitados
!------------------------------------------------------------------------------
%ENVIRON T

!variables del modulo
integer*1 estado,        \indicador estado de funcionalidad 0 -> desactivado
          empresas,      \indicador beneficio para empresas 0 -> desactivado
          esbeneficiario,\indicador si es beneficiario
          cambiarCliente,\indicador cambiar cliente 
          pedirPIN,      \indicador para que se pida el PIN al cliente
          depurar,       \indicador para depurar el programa
          autorespuesta, \indicador para autorespuesta a las consultas
          calculado      !indicador de valor de devolucion de IVA ya calculado 0 -> aun no calculado

integer*4 valorDev,      \valor de iva a devolver
          cadenaPago,    \valor de iva a devolver anterior
          totalIVA       !total IVA en transaccion original al hacer NC

string    cliente,       \Identificacion del cliente
          claveSRI,      \Clave de acceso del comprobante
          encabezado,    \Encabezado de la mensajeria
          codigo,        \Codigo de acceso otorgado al beneficiario
          tarifaIVA,     \tarifa del IVA
          baseIVA,       \base IVA
          valorIVA,      \valor del IVA
          devolIVA,      \valor de devolucion de IVA retornado por el SRI
          montoResp$,    \
          lf$,           \
          separador,     \separador de campos
          mediosPago$    \medios de pagos habilitados

integer*4 pipeTimeout

Integer*4 Global GR.IVA.DEVOL%  ! Devolucion IVA

!------------------------------------------------------------------------
! Variables globales de otros modulos
!------------------------------------------------------------------------
%INCLUDE JAVAGUIV.J86
integer*2 global SL.END, TS.RECOVERY, SL.IT.INDICAT2

!------------------------------------------------------------------------
! Rutinas de otros modulos
!------------------------------------------------------------------------
%INCLUDE EAMTSWKG.J86                  ! working storage
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86

! ASSEMBLER SUBROUTNE
%INCLUDE EAMASMCT.J86
%INCLUDE JAVAGUIC.J86

!para grabar en el journal electronico
FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
   STRING QXL.TSUPEC21, PRTLINE
FEND

!------------------------------------------------------------------------
!Genera el encabezado
!------------------------------------------------------------------------
function generaEncabezado
   string generaEncabezado
   generaEncabezado = "P" + separador + \ canal de comunicacion S=socket P=Pipe
                      "61" + separador + \ requerimiento 61=mensaje por pipe a vector
                      "0000" + separador + \ canal de respuesta
                      TS.TERMINAL$ + separador + \ terminal de origen
                      "0" + separador + \ tipo de conexion 0=temporal 1=permanente
                      "20" + date$ + time$ + separador ! fecha y hora actual
end function

!------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------
function recibeRespuesta
   string recibeRespuesta, a$
   INTEGER*1 I

   recibeRespuesta = ""
   a$ = recibeMensaje( "servicio DEVOL IVA" ) 
   if buscaCampo(a$, separador, 7) = "1" then begin
      call muestraVisor(1, "Error en servicio", buscaCampo(a$, separador, 8) )
      !call traceNUO("recibeRespuesta respuesta=[" + a$ + "]" )
      wait; 5000
      goto salir
   endif

   recibeRespuesta = a$
   a$ = ""

   salir:
   call restauraVisor(1)

end function

!------------------------------------------------------------------------
!Consulta beneficiario
! retorna:
!  0 -> no es beneficiario
! -1 -> no se puede determinar
!  1 -> es beneficiario
!------------------------------------------------------------------------
function consultaBeneficiario
   integer*1 consultaBeneficiario
   string m$, t$, r$

   consultaBeneficiario = -1
   m$ = encabezado +      \
        "1" + separador + \ consulta beneficiario
        cliente           ! RUC del beneficiario

   t$ = "consultaBeneficiario"

   if autorespuesta then begin
      t$ = t$ + " auto respuesta"
      !call traceNUO( t$ + " mensaje=[" + m$ + "]" )
      r$ = encabezado + "0" + separador + "1"
      goto auto
   endif

!call traceNUO( t$ + " mensaje=[" + m$ + "]" )

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !Envia el mensaje
   if not enviaMensaje(m$) then goto salir

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons Devol IVA" ) then goto salir

   !lee respuesta
   r$ = ""
   r$ = recibeRespuesta
   if r$ = "" then goto salir

!call traceNUO( t$ + " respuesta=[" + r$ + "]" )

   auto:
   if buscaCampo(r$, separador, 8) = "0" then \
      consultaBeneficiario = 0 \
   else \
      consultaBeneficiario = 1

   salir:
   r$ = ""
   m$ = ""

end function

!------------------------------------------------------------------------
!Obtiene el valor de devolucion de IVA
! retorna:
!  0 -> no tiene devolucion de IVA
! -1 -> no se puede determinar
!  x -> el valor de devolucion de iva
!------------------------------------------------------------------------
function valorDevolucionIVA
   integer*4 valorDevolucionIVA
   string m$, t$, v$, r$
   integer*4 v

   valorDevolucionIVA = 0
   montoResp$ = ""
   m$ = encabezado +                                \
        "2" + separador +                           \ devolucion de IVA
        MSMTFACTrucEstablecimiento$ + separador +   \ RUC del emisor electronico
        claveSRI + separador +                      \ Clave de acceso del comprobante
        cliente + separador +                       \ Identificacion del cliente
        codigo + separador +                        \ Codigo de acceso otorgado al beneficiario
        baseIVA + separador +                       \ Subtotal del comprobante con IVA gravada
        tarifaIVA + separador +                     \ tarifa del IVA
        valorIVA                                    ! valor del IVA

   t$ = "valorDevolucionIVA "

   call QXL.TSUPEC21( t$ + valorIVA )

!call traceNUO( t$ + "mensaje=[" + m$ + "]" )

   if autorespuesta then begin
      t$ = t$ + "auto respuesta "
      v$ = translate$(valorIVA, ".", ",")
      v$ = str$(round(1.0 * val(v$), 2, 0))
      r$ = encabezado + "0" + separador + v$ + separador + "Exito"
      goto auto
   endif

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia el mensaje
   if not enviaMensaje(m$) then begin
      valorDevolucionIVA = -1
      goto salir
   endif

   !espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons Devol IVA" ) then begin
      valorDevolucionIVA = -1
      goto salir
   endif

   !lee respuesta
   r$ = ""
   r$ = recibeRespuesta
   if r$ = "" then begin
      valorDevolucionIVA = -1
      goto salir
   endif

!call traceNUO( t$ + "respuesta=[" + r$ + "]" )

   auto:
   montoResp$ = buscaCampo(r$, separador, 9)
   devolIVA = buscaCampo(r$, separador, 8)
   call QXL.TSUPEC21( t$ + devolIVA )
   !v$ = translate$(devolIVA, ".", ",")
   !v = round(100.0 * val(v$), 0, 0)
   v = round(100.0 * val(devolIVA), 0, 0)
   valorDevolucionIVA = v

   salir:
   r$ = ""
   m$ = ""

end function

!------------------------------------------------------------------------
!Anulacion devolucion de IVA
!------------------------------------------------------------------------
function anulaDevolucionIVA
   integer*1 anulaDevolucionIVA
   string m$, t$, r$

   anulaDevolucionIVA = 0
   m$ = encabezado +                                 \
        "3" + separador +                            \ anulacion devolucion de IVA
        MSMTFACTrucEstablecimiento$ + separador +    \ RUC del emisor electronico
        claveSRI + separador +                       \ Clave de acceso del comprobante
        cliente + separador +                        \ Identificacion del cliente
        codigo + separador +                         \ Codigo de acceso otorgado al beneficiario
        baseIVA + separador +                        \ Subtotal (base imponible)
        tarifaIVA + separador +                      \ tarifa del IVA
        valorIVA + separador +                       \ valor del IVA
        devolIVA                                     ! valor devuelto por el SRI

   t$ = "anulaDevolucionIVA"

   if autorespuesta then begin
      t$ = t$ + " auto respuesta"
      !call traceNUO( t$ + " mensaje=[" + m$ + "]" )
      r$ = encabezado + "0" + separador + "1" + separador + "ANULACION EXITOSA"
      goto auto
   endif

!call traceNUO( t$ + " mensaje=[" + m$ + "]" )

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia el mensaje
   if not enviaMensaje(m$) then goto salir

   !espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons Devol IVA" ) then goto salir

   !lee respuesta
   r$ = ""
   r$ = recibeRespuesta
   if r$ = "" then goto salir

!call traceNUO( t$ + " respuesta=[" + r$ + "]" )

   auto:
   if buscaCampo(r$, separador, 8) = "0" then begin
      !call traceNUO( t$ + " respuesta=[" + r$ + "]" )
      goto salir
   endif
   anulaDevolucionIVA = -1

   salir:
   r$ = ""
   m$ = ""

end function

!-------------------------------------------------
!Retorna el valor de IVA a devolver
!-------------------------------------------------
function NUOTDIVA.monto public
   integer*4 NUOTDIVA.monto
   NUOTDIVA.monto = valorDev
end function

!-------------------------------------------------
!inicia una transaccion
!incorporar a user 02
!-------------------------------------------------
sub inicializaTrx 
   esbeneficiario = 0
   pedirPIN = -1
   calculado = 0
   valorDev = 0
   cadenaPago = 0
   totalIVA = 0
   cliente = ""
   cambiarCliente = 0
   claveSRI = ""
   codigo = ""
   baseIVA= ""
   valorIVA = ""
   devolIVA = ""
   encabezado = ""
   montoResp$ = ""
end sub

!------------------------------------------------------------------------
!Lee parametros del archivo c:/ADX_UDT1/PARGENER.DAT
!incorporar a USER 7
!------------------------------------------------------------------------
sub leeParametros
   string    r
   integer*1 k
   string    v$
   integer*4 inicioBloque

   separador = "'|'"
   lf$ = chr$(10)
   pipeTimeout = 5000
   estado = 0
   calculado = 0
   empresas = 0
   depurar = 0
   autorespuesta = 0
   mediosPago$ = ""

   call imprimeCJ("******* MODULO DEVOLUCION IVA ********")

   inicioBloque = abreParametros( "R::$ARGENER", "[DEVOLUCION IVA]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE DEVOL IVA", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO 23 Jun 2022" )
      call cierraParametros( 80 )
      exit sub
   endif

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin

      if esNumero( v$ ) then k = val(v$) else k = 0
      if (k and 1) = 1 then estado = -1
      if (k and 2) = 2 then depurar = -1
      if (k and 4) = 4 then autorespuesta = -1

      if estado then begin
         !tiempo de espera respuesta SRI
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         if esNumero( v$ ) then PipeTimeout = val(v$)

         !devolucion de IVA para empresas
         v$ = leeParametro$( "EMPRESAS", 80, inicioBloque )
         if esNumero( v$ ) then k = val( v$ ) else k = 0
         if k = 1 then empresas = -1

         !medios de pagos habilitados
         v$ = leeParametro$( "MEDIOS_PAGO", 80, inicioBloque )
         if v$ <> "" then \
            mediosPago$ = buscaValores$( v$ )
      endif

   endif

   call cierraParametros( 80 )

   if estado then r = "ON " else r = "OFF"
   call imprimeCJ( "ESTADO " + r + " 23 Jun 2022" )
   if depurar then begin
      call imprimeCJ( "TIEMPO DE ESPERA" + right$( string$( 10, " " ) + str$( pipeTimeout ), 10 ) )
      if autorespuesta then call imprimeCJ("ACTIVADO INDICADOR AUTORESPUESTA")
   endif

   tarifaIVA = MSMTFACTimpuestos$(1)

end sub

!------------------------------------------------------------------------
!Anula el calculo de IVA a devolver
!------------------------------------------------------------------------
sub anulaCalculo

   !Si habia hecho el calculo, lo anula
   if calculado then begin
      TS.TENDERED(0) = TS.TENDERED(0) - valorDev
      !TS.BALDUE(0) = TS.BALDUE(0) + valorDev

      !Anula devolucion de IVA
      call anulaDevolucionIVA

      valorDev = 0
      calculado = 0
   endif

end sub

!-------------------------------------------------
!Consulta si es beneficiario de devolucion de IVA
!al ingresar Cedula/RUC
!-------------------------------------------------
sub NUOTDIVA.consulta(s$) public
   string s$
   integer*1 k

   if not estado then exit sub

   !si es consumidor final
   if left$(s$, 10) = "9999999999" then begin
      call anulaCalculo
      cliente = ""
      esbeneficiario = 0
      cambiarCliente = 0
      exit sub
   endif

   !si es empresa y no esta habilitado para empresas
   if len(s$) = 13 and right$(s$, 3) = "001" then begin
      if not empresas then begin
         cliente = ""
         esbeneficiario = 0
         cambiarCliente = 0
         exit sub
      endif
   endif

   !Para no consultar un mismo cliente
   if not cambiarCliente then begin
      if s$ = cliente and esbeneficiario then exit sub
   endif
   cambiarCliente = 0
   cliente = s$

   call anulaCalculo

   !genera encabezado de la mensajeria
   encabezado = generaEncabezado

   !consulta si es beneficiario de devolucion de IVA
   k = consultaBeneficiario
   if k = 1 then begin
      esbeneficiario = -1
      pedirPIN = -1
      TS.BAL.TAKEN = 0
   endif else begin
      cambiarCliente = -1
      esbeneficiario = 0
      pedirPIN = 0
   endif

end sub

!-------------------------------------------------
!Acciones e restriccion de la en User 14
!-------------------------------------------------
sub validaReglas 

   if valorDev > 0 then begin
      if TS.IO.MOTORKEY = 80  and TS.IO.KEYS(10) <> 79  then begin
         dim TS.IO.DATA$(10)
         dim TS.IO.KEYS(10)   
         call muestraBorrar( "NO SE PERMITE ITEM","DEVOLUCION IVA APLICA" )
         TS.IO.MOTORKEY = 0
      endif

      !Si es anulacion total
      if TS.IO.KEYS(1) = 70 then begin
         if calculado then TS.BALDUE(0) = TS.BALDUE(0) + valorDev
         call anulaCalculo
      endif
      exit sub
   endif

end sub

!-------------------------------------------------
!Calcula valor de IVA a devolver UE30
!-------------------------------------------------
sub procesaCalculo
   string    s$
   integer*1 f
   
!   valorDev = 24

   GR.IVA.DEVOL% = valorDev  ! Add 15Abr2025

!agrega el valor de IVA a devolver en el total de pagos
   TS.TENDERED(0) = TS.TENDERED(0) + valorDev
   TS.BALDUE(0) = TS.BALDUE(0) - valorDev

!-- Para proceso de pruebas   
!   calculado = -1
!   Exit Sub 


   !s$ = "DEVOLUCION IVA VALOR IVA=" + valorIVA
   !call QXL.TSUPEC21( s$ )
   !call traceNUO( s$ )

   !Calcula clave SRI
   claveSRI = claveAccesoSRI$(1 , MSMTFACTnumeroFactura + 1, MSMTFACTserieTerminal$, MSMTFACTrucEstablecimiento$, MSMTFACTtipoAmbienteSRI$, MSMTFACTsecuenciaSRI$ , MSMTFACTtipoEmisionSRI$)

   !Solicita ingreso de PIN
   ingresoPIN:
   if pedirPIN then begin
      f = -1
      while f

         if not (jGuiSubState = 80504) then begin
            jGuiSubState = 80511
         endif
         call javaEvent(terminalSubStateMsg)

         s$ = ingresoDatos$( "INGRESE PIN SRI", "PARA DEVOLUCION IVA" )

         if left$( s$, 1 ) = "A" then begin
            cambiarCliente = -1
            s$ = "DEVOLUCION IVA NO INGRESO PIN"
            call QXL.TSUPEC21( s$ )
            !call traceNUO( s$ )
            exit sub
         endif

         if left$( s$, 1 ) = "P" and len( s$ ) > 5 then begin
            codigo = right$( s$, len( s$ ) - 2 )
            f = 0
         endif

         if jGuiSubState = 80511 then begin
            jGuiSubState = 80504 
         endif else begin
            jGuiSubState = 0
         endif
      wend
      pedirPIN = 0
   endif

   !consulta valor de devolucion de IVA
   valorDev = valorDevolucionIVA

   if valorDev = -1 then begin
      valorDev = 0
      pedirPIN = -1
      if len(montoResp$) = 0 then begin
         s$ = "DEVOLUCION IVA PIN NO CORRESPONDE"
         call QXL.TSUPEC21( s$ )
         !call traceNUO( s$ )
         call muestraBorrar( "DEVOLUCION IVA", "CAMBIE PIN O CLIENTE" )
      endif else begin
         s$ = "DEVOLUCION IVA RECHAZADA"
         call QXL.TSUPEC21( s$ )
         call QXL.TSUPEC21( montoResp$ )
         !call traceNUO( s$ + " " + montoResp$ )
         call muestraBorrar( montoResp$, "" )
      endif
      exit sub
   endif

   !si el valor de devolucion es cero
   if valorDev = 0 then begin
      cambiarCliente = -1
      if left$(MontoResp$, 5) <> "Exito" then begin
         pedirPIN = -1
         if len(montoResp$) = 0 then begin
            call muestraBorrar( "PIN NO CORRESPONDE", "CAMBIE PIN O CLIENTE")
            call QXL.TSUPEC21( "DEVOLUCION IVA PIN NO CORRESPONDE" )
         endif else begin
            call muestraBorrar( montoResp$, "")
            call QXL.TSUPEC21( "DEVOLUCION IVA " + montoResp$ )
         endif
         !goto ingresoPIN
      endif
      exit sub
   endif

   !s$ = right$(string$(20, " ") + formateaMonto( valorDev ), 20)
   !call VISOR.AND.BORRAR("VALOR DEVOLUCION IVA" + s$)
   call QXL.TSUPEC21( "DEVOLUCION IVA VALOR DEV=" + formateaMonto( valorDev ) )

   !agrega el valor de IVA a devolver en el total de pagos
   TS.TENDERED(0) = TS.TENDERED(0) + valorDev
   TS.BALDUE(0) = TS.BALDUE(0) - valorDev
   
   GR.IVA.DEVOL% = valorDev  ! Add 15Abr2025
   
   calculado = -1

end sub

!-------------------------------------------------
!Controla medios de pago
!al ingresar un pago (UE14)
!-------------------------------------------------
sub validaPagos
   integer*2 k

   !No es beneficiario
   if not esbeneficiario then exit sub

   !Valor de devolucion de IVA es cero
   if valorDev = 0 then exit sub

   if not TS.INTRX then exit sub
   
   ! Se presiono tecla PLU
   if TS.IO.MOTORKEY = 80 then exit sub

   !No es un medio de pago permitido
   k = matchb( chr$( TS.IO.MOTORKEY ), mediosPago$, 1 )
   if k = 0 then begin
      call muestraBorrar( "PAGO NO PERMITIDO EN", "DEVOLUCION DE IVA" )
      dim TS.IO.DATA$(10)
      dim TS.IO.KEYS(10)
      TS.IO.MOTORKEY = 73
      exit sub
   endif

   !No se ha hecho TOTAL
   if NOT TS.BAL.TAKEN then begin
      TS.GUIDANCE = 1020
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   !Impide se usen los medios de pago tarjeta (quitar 94 y 180 de entre los pagos permitidos)
   !if TS.IO.MOTORKEY = 94 or TS.IO.MOTORKEY = 180 then begin
   !   call muestraBorrar( "FORMA PAGO INVALIDA", "USE OTRA FORMA PAGO" )
   !   dim TS.IO.DATA$(10)
   !   dim TS.IO.KEYS(10)
   !   TS.IO.MOTORKEY = 73
   !   exit sub
   !endif

end sub

!-------------------------------------------------
!Genera linea con valor de IVA a devolver
!Al imprimir el comprobante
!-------------------------------------------------
function NUOTDIVA.devolver public
   string NUOTDIVA.devolver
   integer*4 v, t2

   if valorDev = 0 then begin
      NUOTDIVA.devolver = ""
      exit function
   endif

   NUOTDIVA.devolver = string$( 10, " " ) + "(-) Devolucion Iva" + \
      right$( string$( 10, " " ) + formateaMonto(valorDev), 10 )

end function

!------------------------------------------------------------------------
!Elimina la condicion de beneficiario
!------------------------------------------------------------------------
sub NUOTDIVAanulaBeneficiario public
   esbeneficiario = 0
   cambiarCliente = -1
   pedirPIN = 0
end sub

!------------------------------------------------------
! Rutina debe llamarse en user 02
!------------------------------------------------------
sub NUOTDIVA02 public
    call inicializaTrx
end sub

!------------------------------------------------------
! Rutina debe llamarse en user 07
!------------------------------------------------------
sub NUOTDIVA07 public
   call leeParametros
end sub

!------------------------------------------------------
! Rutina debe llamarse en user 14
!------------------------------------------------------
sub NUOTDIVA14 public

   !Impedir que ingrese por teclado la forma de pago devolucion de IVA (2 / monto BONO) 
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 96 ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "2" ) and \
      ( TS.IO.KEYS(7)  = 96 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   if not estado then exit sub

   call validaReglas
   call validaPagos

end sub

!------------------------------------------------------
! Ingresa string de pago tipo 6 variedad 2 en el TSL
! llamado desde UE20 cuando imprime la linea de tienda
!------------------------------------------------------
sub NUOTDIVA20 public
   string c$

   !store data print
   if NOT TS.TRAINING and TS.INTRX and TS.LINETYPE = 6 AND TS.LINEDATA = 1 then begin

      if cadenaPago = valorDev then exit sub
      if valorDev = 0 then exit sub

      c$ = chr$(5) + ":" +               \ string de pago
         chr$(62H) + ":" +               \ tipo 6 variedad 2
         pack$(str$(valorDev)) + ":::" + \ monto
         pack$("00")
      call agregaCadenaTSL( c$ )

      cadenaPago = valorDev
      !TS.GUIDANCE = 1020
      !TS.IO.MOTORKEY = 0

   endif

end sub

!------------------------------------------------------
! En el calculo del saldo a pagar
!------------------------------------------------------
sub NUOTDIVA30 public
   string    s$
   integer*4 t0, t1, t2

   !if not estado then exit Sub PILAS QUITAR COMENTARIO

   !No es beneficiario
   !if not esbeneficiario then exit Sub PILAS QUITAR COMENTARIO

   !No es una transaccion de venta
   if NOT TS.INTRX then exit sub

   !tecla total
   if TS.IO.MOTORKEY = 81 then begin

!call traceNUO("NUOTDIVA30 valorDev=" + str$( valorDev) )

      !Si es anulacion total
      if TS.IO.KEYS(1) = 70 then begin
         if calculado then TS.BALDUE(0) = TS.BALDUE(0) + valorDev
         call anulaCalculo
         exit sub
      endif

      if valorDev > 0 then exit sub

      !Obtiene tarifas con descuento
      call tarifasConDescuento( t0, t1, t2 )

      !si valor de IVA es cero
      if t2 = 0 then exit sub

      call anulaCalculo

      baseIVA = elider$( formateaMonto( t1 ) )
      valorIVA = elider$( formateaMonto( t2 ) )

      call procesaCalculo

   endif

end sub

