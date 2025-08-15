!-----------------------------------------------------------------------------
! Modulo: NUOTCNB.BAS Cliente no Bancario
! Fecha : 29 de septiembre de 2020
! Autor : Alejandro Farre P.
!
! History
! Fecha : 30 de Marzo de 2021 se incorpora funcionalidad de AVANCE con TARJETA
! Autor : Carlos Calderon
! Fecha : 27 de Mayo de 2021 se incorpora funcionalidad de REVERSO DEPOSITO
! Autor : Carlos Calderon
! Fecha : 10 de Junio de 2021 se incorpora funcionalidad a TCx.Sky
! Autor : Carlos Calderon
! Fecha : 19 de Julio de 2021 se corrige funcionalidad anulacion de deposito
! Autor : Alejandro Farre P.
! Autor : Carlos Calderon
! Fecha : 01 de Junio de 2022 se corrige impresion de centavos en los comprobantes 
!------------------------------------------------------------------------------
%ENVIRON T

!variables del modulo
string    fechaModificacion
integer*1 tipoCNB,                    \ tipo de proceso 1 Deposito, 2 Retiro, 3 Consulta, 4 Avance
          cnb.modoAvance,             \ modalidad de avance 1=corriente, 2=diferido
          cnb.simulaAvance,           \ etapas del avance 0=Diferido 1=simula
          tipoReimpresion,            \ tipo de proceso reimpresion 1 Grabar, 2 Leer
          operation,                  \ 62: Mensajeria CNB 63: Reimpresion
          estado,                     \ indicador estado de funcionalidad 0 -> desactivado
          depurar,                    \ indicador para depurar el programa
          anula.deposito.activo,      \ indicador de funcionalidad ANULACION DE DEPOSITO
                                      \ 0=NO HABILITADO
                                      \ 1=HABILITADO
                                      \ 3=HABILITADO CON TRACE
                                      \ 7=HABILITADO CON TRACE Y DUMMY
          dummy                       ! indicador de autorespuesta dummy

string    nombreProceso(1),           \ Nombre de los procesos
          lf$,                        \ salto de linea chr$(10)
          separador,                  \ separador de campos agente "'|'"
          cnb.doc.anu.deposito$       ! numero de documento para la anulacion de DEPOSITO

integer*4 montoMinimo(1),             \ Monto minimo de deposito y retiro
          montoMaximo(1)              ! Monto maximo de deposito y retiro

!variables para las tramas de comunicacion
string    cnb.header,                 \ encabezado
          cnb.estado,                 \ estado valores: iniciar, reglaSeguridad, finalizar
          cnb.clientIdType,           \ tipo de cliente, 0001=cedula, 0002=pasaporte, 0003=RUC
          cnb.clientId,               \ identificador de cliente
          cnb.accountId,              \ numero de cuenta
          cnbCommissionTrx$,          \ monto de comision
          cnb.otp,                    \ codigo OTP
          cnb.referential,            \ siempre con valor 0
          cnb.entryMode,              \ 012
          cnb.officeId,               \ numero de tienda
          cnb.terminal,               \ numero de terminal sin ceros a la izquierda
          cnb.terminalUser,           \ codigo de operador sin ceros a la izquierda
          cnb.sequential,             \ numero secuencial
          cnbAmount$,                 \ monto de la operacion
          cnb.currency,               \ USD
          cnb.processId,              \ ID del proceso
          cnb.officeName,             \ nombre de la oficina
          cnb.control,                \
          cnb.accountingDate,         \ fecha contable
          cnb.timeStamp,              \
          cnb.maskAccountId,          \ numero de cuenta enmascarada
          cnb.accountType,            \ tipo de cuenta
          cnb.accountOwnerName,       \ nombre del dueno de la cuenta
          cnb.available,              \ balance disponible
          cnb.countable,              \ balance contable
          cnb.sequentialRev,          \ secuence reverse
          cnb.error,                  \ error message
          cnb.message,                \
          cnb.datosTrx,               \ local(4), caja(3), trx(4), fecha(6) y hora(4)
          cnb.receipt,                \ numero de recibo
                                      \
          cnb.listaDiferido,          \ lista de meses diferidos
          cnb.diferido,               \ meses diferido
                                      \
          cnb.listMaskedAccount,      \ Lista numero de cuenta enmascarado AVANCE
          cnb.fechaExpira,            \ CCVV fecha vencimiento tarjeta
          cnb.listaHashAccount,       \ Lista hash de la cuenta enmascarada AVANCE
          cnb.hashAccount,            \ hash de cuenta
          cnb.descriptionTarjeta,     \ Descripcion de la tarjeta
                                      \
          cnb.servFeeAmount,          \ serviceFee Amount                   Por definir
          cnb.servFeeTax,             \ serviceFee Tax                      Por definir
          cnb.servFeeTotal,           \ serviceFee Total                    Por definir
                                      \
          cnb.factorDetalleFactor,    \ factorDetails Factor
          cnb.factorDetalleInterest,  \ factorDetails Tax                   Por definir
          cnb.factorDetalleFinalAmount,  \ factorDetails Total                 Por definir
          cnb.factorDetalleQuota,     \ factorDetails quota                 Por definir
                                      \
          cnb.cvv2,                   \ Valor cvv2 tarjeta credito
          cnb.maskedCreditCard,       \ Numero enmascarado tarjeta de credito
          cnb.authorizationNumber,    \ codigo de autorizacion
          cnb.authorizationCod,       \ Por definir
          cnb.promoDescripcion,       \ Numero autorizacion para impresion
                                      \
          cnb.creditCardBrand,        \ Nombre de la tarjeta de credito
          cnb.sesion,                 \ Sesion de la transaccion
          cnb.hashTransaction,        \ Hash de la transaccion
          cnb.lot,                    \ Valor de numero Lote para impresion
          cnb.approval,               \ Valor de numero de aprobacion para impresion
          cnb.document,               \ Numero documento para impresion
                                      \
          cnb.officeLocation,         \ Ubicacion(ciudad) de la sucursal
                                      \
          cnb.inquiry,                \ comision por ANULACION DE DEPOSITO
          cnb.procesID,               \
          cnb.documentoDeposito,      \ Documento del deposito reversado.
          cnb.descripcionTrx          ! Descripcion de la transaccion

!variables para manejo de comunicaciones por PIPE
integer*4 pipeTimeout    !Tiempo de espera

!variables globales de otros modulos
string    global TS.OPER.NAME$

%INCLUDE NUOTVARI.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86
%INCLUDE JAVAGUIV.J86

%INCLUDE NUOTRUTI.J86
%INCLUDE BASROUT.J86
%INCLUDE JAVAGUIC.J86

! open cash drawer
SUB TSCSEC06 EXTERNAL
END SUB

!-------------------------------------------------------------------------------
! Imprime en lineas de 42 caracteres
!-------------------------------------------------------------------------------
sub imprime42( b$ )
   string b$, s$, e$
   integer*2 i, n, k

   k = 42
   e$ = string$( k, " " )
   n = len(b$) + 1
   i = 1
   while i < n
      s$ = left$( mid$( b$, i, k ) + e$, k )
      write #34; s$ + lf$
      i = i + k
   wend
end sub

!------------------------------------------------------------------
! graba log "R::Q:/" o "R::C:/DQ/" + "TRC." + TS.TERMINAL$
!------------------------------------------------------------------
sub logCNB( dato$ )
   string dato$
   if depurar then begin
      call traceNUO( dato$ )
   endif
end sub

!-----------------------------------------------------------------------------
! Lee archivo que contiene el cuerpo del mensaje
!-----------------------------------------------------------------------------
function leeBody
   INTEGER*1 A%
   integer*2 indice
   integer*4 tamano
   string leeBody, body$, archivo$, correlativo$, f$

   leeBody = ""
   close 47
   archivo$ = "R::C:/CNB/"+"CNBRS" + TS.TERMINAL$ + ".DAT"
   body$ = ""
   correlativo$ = ""

   TS.ER.RETURN = -1
   open archivo$ as 47
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "No se puede abrir", "Archivo de respuesta" )
      exit function
   endif

   tamano = size("R::C:/CNB/"+"CNBRS" + TS.TERMINAL$ + ".DAT")

   !informacion del archivo debe contener caracter de fin de linea
   f$ = "C"+str$(tamano)
   read form f$;#47;body$
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "No se puede leer", "Archivo de respuesta" )
      delete 47
      exit function
   endif

   if right$(body$,2) = CRLF$ then body$ = left$(body$,len(body$)-2)

   if len( body$ ) = 0 then begin
      call muestraBorrar( "Archivo de respuesta", "sin informacion..." )
      delete 47
      exit function
   endif

   leeBody = body$
   if dummy then begin
      close 47
      archivo$ = "R::C:/CNB/BODYCORE.DAT"
      open archivo$ direct recl 5 as 47
      read form "C5";#47,1;correlativo$
      if TS.ER.RETURN <> -1 then begin
         call muestraBorrar( "No se puede leer", "Archivo bodycore" )
         exit function
      endif

      indice = val(left$(correlativo$,3)) + 1
      A% = OSSHELL("REN  R::C:/CNB/CNBRS" + TS.TERMINAL$ + ".DAT R::C:/CNB/CNBRS" + TS.TERMINAL$ + "." + right$("000"+str$(indice),3))
      correlativo$ = right$("000"+str$(indice),3) + CRLF$
      write form "C5";#47,1;correlativo$
      close 47
   endif \
   else delete 47
end function

!------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------
function recibeRespuesta
   string recibeRespuesta, a$, f$
   integer*2 x

   a$ = recibeMensaje( "del banco" )

   if operation = 62 or tipoReimpresion = 2 then begin
      f$ = leeBody
      a$ = a$ + f$
   endif

!call logCNB("recibeRespuesta: "+a$)

   if f$ = "" then begin
      call restauraVisor(1)
      exit function
   endif

   if buscaCampo(a$, separador, 7) <> "0" then begin
      call restauraVisor(1)
      if tipoReimpresion = 2 then begin
        f$ = buscaCampo( a$, separador, 8 ) + string$( 40, " " )
         call muestraBorrar( left$( f$, 20 ), mid$( f$, 21, 20 ) )
      endif \
      else call muestraBorrar( "Error del servicio", "de comunicacion" )
      exit function
   endif
   recibeRespuesta = a$

   call restauraVisor(1)

end function

!------------------------------------------------------------------------
!Lee parametros del archivo PARGENER
!incorporar en la USER 7
!------------------------------------------------------------------------
sub NUOTCNB07 public
   string    r
   integer*1 k
   string    v$
   integer*4 inicioBloque

   fechaModificacion = " 21 Ene 2022"
   DIM nombreProceso(7)
   nombreProceso(1) = "DEPOSITO"
   nombreProceso(2) = "RETIRO"
   nombreProceso(3) = "CONSULTA"
   nombreProceso(4) = "AVANCE"
   nombreProceso(7) = "REVERSO DEPOSITO"

   DIM montoMinimo(4)
   DIM montoMaximo(4)
   for k = 1 to 4
      montoMinimo(k) = 500    !USD   5.00
      montoMaximo(k) = 50000  !USD 500.00
   next

   separador = "'|'"
   lf$ = chr$(10)
   pipeTimeout = 5000
   estado = 0
   depurar = 0
   dummy = 0

   call imprimeCJ( "***** MODULO CLIENTE NO BANCARIO *****" )

   inicioBloque = abreParametros( "R::$ARGENER", "[CLIENTE NO BANCARIO]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE CNB", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO" + fechaModificacion )
      call cierraParametros( 80 )
      exit sub
   endif

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin
      if esNumero( v$ ) then k = val( v$ ) else k = 0
      if ( k and 1 ) = 1 then estado = -1
      if ( k and 2 ) = 2 then depurar = -1
      if ( k and 4 ) = 4 then dummy = -1

      if estado then begin
         !tiempo de espera respuesta banco
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         if esNumero( v$ ) then pipeTimeout = val( v$ )

         !monto minimo y maximo deposito
         v$ = leeParametro$( "DEPOSITO_MINIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMinimo(1) = val( v$ )
         v$ = leeParametro$( "DEPOSITO_MAXIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMaximo(1) = val( v$ )

         !monto minimo y maximo retiro
         v$ = leeParametro$( "RETIRO_MINIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMinimo(2) = val( v$ )
         v$ = leeParametro$( "RETIRO_MAXIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMaximo(2) = val( v$ )

         !montos minimo y maximo AVANCE
         v$ = leeParametro$( "AVANCE_MINIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMinimo(4) = val( v$ )
         v$ = leeParametro$( "AVANCE_MAXIMO", 80, inicioBloque )
         if esNumero( v$ ) then montoMaximo(4) = val( v$ )

         !Indicador anulacion de deposito
         v$ = leeParametro$( "ANULACION_DEPOSITO_ACTIVO", 80, inicioBloque )
         if esNumero( v$ ) then anula.deposito.activo = val( v$ )
      endif
   endif

   call cierraParametros( 80 )

   if estado then r = "ON " else r = "OFF"
   call imprimeCJ( "ESTADO " + r + fechaModificacion )
   if depurar then begin
      call imprimeCJ( "TIEMPO DE ESPERA" + right$( string$( 10, " " ) + str$( pipeTimeout ), 10 ) )
      call imprimeCJ("MINIMO DEPOSITO" + right$( string$( 12, " " ) + formateaMonto( montoMinimo(1) ), 12 ) )
      call imprimeCJ("MAXIMO DEPOSITO" + right$( string$( 12, " " ) + formateaMonto( montoMaximo(1) ), 12 ) )
      call imprimeCJ("MINIMO RETIRO" + right$( string$( 14, " " ) + formateaMonto( montoMinimo(2) ), 14 ) )
      call imprimeCJ("MAXIMO RETIRO" + right$( string$( 14, " " ) + formateaMonto( montoMaximo(2) ), 14 ) )
      call imprimeCJ("MINIMO AVANCE" + right$( string$( 14, " " ) + formateaMonto( montoMinimo(4) ), 14 ) )
      call imprimeCJ("MAXIMO AVANCE" + right$( string$( 14, " " ) + formateaMonto( montoMaximo(4) ), 14 ) )
      if anula.deposito.activo then call imprimeCJ("ANULACION DEPOSITO ON")
      call imprimeCJ( "ACTIVADA DEPURACION" )
      if dummy then call imprimeCJ( "ACTIVADA DUMMY" )
   endif

end sub

!------------------------------------------------------------------------
! Ingreso y validacion de identificacion del cliente
! retorna -1 si se cancela el ingreso
!          0 ingreso ok
!------------------------------------------------------------------------
function ingresoIdentificacion
   integer*1 ingresoIdentificacion, f, t
   string    s$, e$

   ingresoIdentificacion = -1
   cnb.clientIdType = ""
   cnb.clientId = ""
   t = 0
   f = -1
   while f

      if not (jGuiSubState = 80529) then begin  !JC
         jGuiSubState = 80510
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$( "INGRESE TIPO ID", "1 CEDULA 2 PASAPORTE" )
      if left$( s$, 1 ) = "A" then exit function
      if len(s$) <> 3 or match( mid$( s$, 3, 1), "12", 1 ) = 0 then begin
         call muestraBorrar( "DIGITE UNA OPCION", "VALIDA:(1-2)" )
      endif else begin
         e$ = left$(s$, 1)
         if e$ = "P" then begin
            e$ = mid$( s$, 3, 1 )
            t = val( e$ )
            cnb.clientIdType = right$( string$( 4, "0") + str$( t ), 4 )
            f = 0
         endif
      endif

      if jGuiSubState = 80510 then begin
         jGuiSubState = 80529 
      endif else begin
         jGuiSubState = 0
      endif
   wend

   if java.init = -1 then begin
      jGuiSubState = 0
   endif   

   f = -1
   while f

      if not (jGuiSubState = 80504) then begin  !JC
         jGuiSubState = 80511
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("INGRESE ID CLIENTE", "")
      e$ = left$(s$, 1)
      if e$ = "P" then begin
         e$ = right$( s$, len(s$) - 2 )
         if t = 1 then begin
            if esnumero( e$ ) then begin
               if verificaCedula( e$ ) then begin
                  cnb.clientId = e$
                  ingresoIdentificacion = 0
                  f = 0
               endif else begin
                  call muestraBorrar( "NO VALIDO COMO", "CEDULA" )
               endif
            endif
         endif else begin
            cnb.clientId = e$
            ingresoIdentificacion = 0
            f = 0
         endif
      endif else if e$ = "A" then exit function

      if jGuiSubState = 80511 then begin
         jGuiSubState = 80504 
      endif else begin
         jGuiSubState = 0
      endif
   wend

   if java.init then begin
      jGuiSubState = 0
   endif

end function

!------------------------------------------------------------------------
! Genera encabezado del mensaje
! Operation 62: Transacciones cnb
!           63: Reimpresion de comprobantes
!------------------------------------------------------------------------
sub generaEncabezadoCNB

cnb.header = "P"                                + separador + \ canal de comunicacion S=socket P=Pipe
             str$(operation)                    + separador + \ requerimiento mensaje por pipe para MSM
             "0000"                             + separador + \ canal de respuesta
             TS.TERMINAL$                       + separador + \ terminal de origen
             "0"                                + separador + \ tipo de conexion 0=temporal 1=permanente
             "20" + date$ + time$                             ! fecha y hora de inicio de la operacion

cnb.datosTrx = right$( "0000" + TS.STORE$, 4 )              + \
               right$( "000" + TS.TERMINAL$, 3 )            + \
               right$( "0000" + str$( SL.HD.TRANSNUM + 1 ), 4 ) + \
               "20" + date$ + left$( time$, 4 )               !

end sub

!------------------------------------------------------------------------
! Genera cuerpo del mensaje
!------------------------------------------------------------------------
function generaMensaje
   string generaMensaje, m$

   if operation = 62 then begin
      m$ = cnb.header                                              ! header comun
      if tipoCNB = 7 then begin
         m$ = m$ + separador + \
         str$(tipoCNB) + separador + \         !identificador de mensaje
         cnb.estado + separador + \            !estado
         cnb.officeId + separador + \          !officeId
         cnb.terminal + separador + \          !terminal
         cnb.terminalUser + separador + \      !terminalUser
         cnb.sequential + separador + \        !sequential
         cnb.entryMode + separador + \         !entryMode
         cnb.doc.anu.deposito$ + separador + \ !Secuencial de a transaccion que se quiere reversar
         "0601422900" + separador + \          !clientIdDep
         "0001" + separador + \                !clientIdTypeDep
         "2203978576" + separador + \          !accountIdDep
         "reversoDeposito"                     !
      endif else begin
         m$ = m$ + separador + \
         str$(tipoCNB) + separador + \               !identificador de mensaje
         cnb.estado + separador + \                  !estado
         cnb.clientIdType + separador + \            !tipo de cliente
         cnb.clientId + separador + \                !Id del cliente
         cnb.accountId + separador                   ! accountId
         if tipoCNB <> 3 then \
            m$ = m$ + eliminaCar$( cnbAmount$, "," ) + separador    ! amount sin separador de miles
         if tipoCNB = 1 then \
            m$ = m$ + cnb.currency                   + separador    ! currency
         m$ = m$ + cnbCommissionTrx$                 + separador    ! commissionTrx
         if tipoCNB <> 1 then \
            m$ = m$ + cnb.otp + separador            ! otp
         m$ = m$ + cnb.referential + separador + \   !referential
         cnb.entryMode + separador + \               !entryMode
         cnb.officeId + separador + \                !officeId
         cnb.terminal + separador + \                !terminal
         cnb.terminalUser + separador + \            !terminalUser
         cnb.sequential                              ! sequential
      endif
   endif else if operation = 63 then begin

      if tipoReimpresion = 1 then begin
         m$ = cnb.header                          + separador +  \ header comun
              str$(tipoReimpresion)               + separador +  \ identificador de mensaje
              cnb.control                         + separador +  \ llave de busqueda
              str$(tipoCNB)                       + separador +  \ tipo de comprobante
              cnb.maskAccountId                   + separador +  \ Numero de cuenta enmascarada
              cnb.accountOwnerName                + separador +  \ Nombre del dueno de la cuenta
              cnb.sequential                      + separador +  \ Documento                               !no se
              cnbAmount$                          + separador +  \ Monto
              cnbCommissionTrx$                   + separador +  \ Cargo
              cnb.currency                        + separador +  \ Moneda
              cnb.officeName                      + separador +  \ Nombre Oficina
              cnb.datosTrx                        + separador +  \ datos de la transaccion
              cnb.accountType                     + separador +  \ tipo de cuenta                         ! no se
              cnb.receipt                         + separador +  \ Recibo
              cnb.terminalUser                    + separador +  \ Cajero
              cnb.available                       + separador +  \ Saldo Disponible
              cnb.countable                                      ! Saldo Contable
      endif \
      else begin
         m$ = cnb.header                         + separador +   \ header comun
         str$(tipoReimpresion)                   + separador +   \ identificador de mensaje
         cnb.control                                             ! llave de busqueda
      endif

   endif
   generaMensaje = m$

end function

!------------------------------------------------------------------------
!Asigna valor de la respuesta a las variables cnb.error y cnb.message
!------------------------------------------------------------------------
sub leeMensajeCNB( m$ )
   string m$
   integer*1 k
   k = 25

   if tipoCNB = 2 then k = 26
   if tipoCNB = 3 then k = 27
   if tipoCNB = 4 then begin
      if cnb.simulaAvance = 0 then k =  9
      if cnb.simulaAvance = 1 then k = 32
      if cnb.simulaAvance = 2 or cnb.simulaAvance = 3 then k = 38
   endif

   cnb.error = buscaCampo( m$, separador, k )
   k = k + 1
   cnb.message = buscaCampo( m$, separador, k )

end sub

!------------------------------------------------------------------------
!Asigna valor de la respuesta a las variables cnb.xxx
!------------------------------------------------------------------------
sub leeRespuestaCNB( m$ )
   string    m$
   integer*1 i

!call logCNB("leeRespuestaCNB operacion [" + str$(operation)+ "]")

   if operation = 62 then begin

      cnb.sequential     = buscaCampo( m$, separador, 10 )
      cnb.officeName     = buscaCampo( m$, separador, 11 )
      cnb.control        = buscaCampo( m$, separador, 13 )
      cnb.accountingDate = buscaCampo( m$, separador, 14 )

      i = 15
      if tipoCNB <> 1 then begin
         cnb.timeStamp = buscaCampo( m$, separador, i )
         i = i + 1
      endif
      cnb.clientId = buscaCampo( m$, separador, i )
      i = i + 1
      cnb.clientIdType = buscaCampo( m$, separador, i )
      i = i + 1
      cnb.accountId = buscaCampo( m$, separador, i )
      i = i + 1
      cnb.maskAccountId = buscaCampo( m$, separador, i )
      i = i + 1
      cnb.accountType = buscaCampo( m$, separador, i )
      i = i + 1
      cnb.accountOwnerName = buscaCampo( m$, separador, i )
      i = i + 1
      if tipoCNB = 3 then begin
         cnb.available = buscaCampo( m$, separador, i )
         i = i + 1
         cnb.countable = buscaCampo( m$, separador, i )
         i = i + 1
      endif else begin
         cnbAmount$ = buscaCampo( m$, separador, i )
         i = i + 1
      endif
      if tipoCNB = 1 then begin
         cnbCommissionTrx$ = buscaCampo( m$, separador, i )
         i = i + 1
         cnb.currency = buscaCampo( m$, separador, i )
         i = i + 1
      endif else begin
         cnb.currency = buscaCampo( m$, separador, i )
         i = i + 1
         cnbCommissionTrx$ = buscaCampo( m$, separador, i )
         i = i + 1
      endif
      cnb.sequentialRev = buscaCampo( m$, separador, i )

   endif else if operation = 63 then begin

      cnb.error = buscaCampo( m$, separador, 7 )
      if cnb.error <> "0" then exit sub

      if tipoReimpresion = 1 then begin
         cnb.receipt          = buscaCampo( m$, separador, 8 )
      endif else begin
         cnb.control          = buscaCampo( m$, separador, 8 )
         tipoCNB              = val(buscaCampo( m$, separador, 9 ))
         cnb.maskAccountId    = buscaCampo( m$, separador, 10 )
         cnb.accountOwnerName = buscaCampo( m$, separador, 11 )
         cnb.sequential       = buscaCampo( m$, separador, 12 )
         cnbAmount$           = buscaCampo( m$, separador, 13 )
         cnbCommissionTrx$    = buscaCampo( m$, separador, 14 )
         cnb.currency         = buscaCampo( m$, separador, 15 )
         cnb.officeName       = buscaCampo( m$, separador, 16 )
         cnb.datosTrx         = buscaCampo( m$, separador, 17 )
         cnb.accountType      = buscaCampo( m$, separador, 18 )
         cnb.receipt          = buscaCampo( m$, separador, 19 )
         cnb.terminalUser     = buscaCampo( m$, separador, 20 )
         cnb.available        = buscaCampo( m$, separador, 21 )
         cnb.countable        = buscaCampo( m$, separador, 22 )
      endif
   endif

end sub

!------------------------------------------------------------------------
! Ingresa datos numero de control para re-impresion de un comprobante
!------------------------------------------------------------------------
function ingresoDatosReimpresion
   integer*1 ingresoDatosReimpresion, f
   string    s$
   ingresoDatosReimpresion = -1

   jGuiSubState = 80529
   call javaEvent(terminalSubStateMsg)

   s$ = ingresoDatos$( "INGRESE CONTROL", "" )
   if left$(s$, 1) = "P" then begin
      s$ = right$( s$, len( s$ ) - 2 )
      if esnumero(s$) then begin
         cnb.control = s$
         ingresoDatosReimpresion = 0
         if java.init then begin
            jGuiSubState = 0 
         endif
         exit function
      endif
   endif

end function

!------------------------------------------------------------------------
! Ingresa datos numero de documento para la anulacion de deposito
!------------------------------------------------------------------------

function ingresoDatosAnulacionDeposito
   integer*1 ingresoDatosAnulacionDeposito, f
   string    s$
   ingresoDatosAnulacionDeposito = -1

   jGuiSubState = 80511
    call javaEvent(terminalSubStateMsg)

   s$ = ingresoDatos$( "INGRESE # DOCUMENTO", "" )
   if left$(s$, 1) = "P" then begin
      s$ = right$( s$, len( s$ ) - 2 )
      if esnumero(s$) then begin
         cnb.doc.anu.deposito$ = s$
         ingresoDatosAnulacionDeposito = 0
         if java.init then begin
            jGuiSubState = 0
         endif
         exit function
      endif
   endif

end function

sub leeRespuestaAnulaDepositoCNB( m$ )
string    m$

   cnb.estado            = buscaCampo( m$, separador,  8 )  ! Estado transaccion.
   cnb.inquiry           = buscaCampo( m$, separador,  9 )  ! Comision de la transaccion.
   cnb.procesID          = buscaCampo( m$, separador, 10 )  ! Codigo de proceso.
   cnb.officeName        = buscaCampo( m$, separador, 11 )  ! Nombre de la sucursal.
   cnb.terminal          = buscaCampo( m$, separador, 12 )  ! Numero de la caja.
   cnb.accountingDate    = buscaCampo( m$, separador, 13 )  ! Fecha de proceso transaccion.
   cnb.clientId          = buscaCampo( m$, separador, 14 )  ! ID de cliente.
   cnb.clientIdType      = buscaCampo( m$, separador, 15 )  ! Tipo de ID de cliente
   cnb.accountId         = buscaCampo( m$, separador, 16 )  ! Cuenta del cliente.
   cnb.maskAccountId     = buscaCampo( m$, separador, 17 )  ! Numero de cuenta enmascarada.
   cnb.accountType       = buscaCampo( m$, separador, 18 )  ! Tipo de cuenta.
   cnb.accountOwnerName  = buscaCampo( m$, separador, 19 )  ! Nombre de beneficiario de cuenta
   cnbAmount$            = buscaCampo( m$, separador, 20 )  ! Monto
   cnb.currency          = buscaCampo( m$, separador, 21 )  ! Moneda
   cnbCommissionTrx$     = buscaCampo( m$, separador, 22 )  ! comision del DEPOSITO para imprimir en comprobante
   cnb.documentoDeposito = buscaCampo( m$, separador, 23 )  ! Documento del deposito reversado.
   cnb.descripcionTrx    = buscaCampo( m$, separador, 24 )  ! Descripcion de la transaccion
   cnb.error             = buscaCampo( m$, separador, 25 )  ! 1= Correcto, 0= Error.
   cnb.message           = buscaCampo( m$, separador, 26 )  ! Mensaje de error presentado.

end sub

!------------------------------------------------------------------------
! Consulta la anulacion de deposito
!------------------------------------------------------------------------
function consultaAnulacionDeposito
   integer*1 consultaAnulacionDeposito
   string    m$

   tipoReimpresion           = 1
   cnb.estado                = "iniciar"
   consultaAnulacionDeposito = -1
   call generaEncabezadoCNB

 !Genera mensaje

   m$ = generaMensaje

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB" ) then begin
      !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   call leeRespuestaAnulaDepositoCNB( m$ )

   if cnb.error <> "1" then begin
      !call logCNB("Sali Mal cnb.error["+cnb.error+"]")
      call muestraBorrar( "No se pudo recuperar", "anula deposito" )
      exit function
   endif

   consultaAnulacionDeposito = 0

end function

!------------------------------------------------------------------------
! Consulta un comprobante para reimprimir
!------------------------------------------------------------------------
function consultaComprobante
   integer*1 consultaComprobante
   string    m$

   operation = 63
   call generaEncabezadoCNB
   tipoReimpresion     = 2
   consultaComprobante = -1

   !Genera mensaje
   m$ = generaMensaje

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB" ) then begin
      !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   call leeRespuestaCNB( m$ )

   if cnb.error <> "0" then begin
      call muestraBorrar( "No se pudo recuperar", "el comprobante" )
      exit function
   endif

   consultaComprobante = 0

end function

!------------------------------------------------------------------------
! Graba el comprobante para la reimpresion
!------------------------------------------------------------------------
function grabaComprobante
   integer*1 grabaComprobante
   string    m$

   operation = 63
   call generaEncabezadoCNB
   tipoReimpresion  = 1
   grabaComprobante = -1

   !Genera mensaje
   m$ = generaMensaje

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB" ) then begin
      !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   call leeRespuestaCNB( m$ )

   if cnb.error <> "0" then \
      call muestraBorrar( "No se pudo grabar el", "comprobante" )

   grabaComprobante = 0

end function

!------------------------------------------------------------------------
!ingreso y reingreso de numero de cuenta
!retorna un dato ingresado por teclado o una cadena vacia
!------------------------------------------------------------------------
function ingresoCuenta$(tipo)
   string    ingresoCuenta$, e$, s$, d1$, d2$
   integer*1 tipo

   ingresoCuenta$ = ""

   if tipo = 1 then \
      d1$ = "INGRESE " \
   else \
      d1$ = "REINGRESE "

   if (tipoCNB = 1) then begin
      d1$ = d1$ + "NUMERO"
      d2$ = "DE CUENTA"
   endif else if (tipoCNB = 2) or (tipoCNB = 3) then begin
      d1$ = d1$ + "4 ULT DIG"
      d2$ ="DE TARJETA DEBITO"
   endif else if (tipoCNB = 4) then begin
      d1$ = d1$ + "4 ULT DIG"
      d2$ ="DE TARJETA CREDITO"
   endif

   ingresoCuenta$ = ingresoDatos$( d1$, d2$ )

end function

!------------------------------------------------------------------------
! Ingreso cuenta del cliente o ultimos 4 digitos
! retorna -1 si se cancela el ingreso
!          0 ingreso ok
!------------------------------------------------------------------------
function ingresoDatosCuenta
   integer*1 ingresoDatosCuenta, f
   string    s1$, s2$

   ingresoDatosCuenta = -1
   f = 1
   while f <= 3

      if not (jGuiSubState = 80527) then begin  !JC
         jGuiSubState = 80510
      endif

      call javaEvent(terminalSubStateMsg)

      s1$ = ingresoCuenta$( 1 )
      if s1$ = "A" then exit function

      if not (jGuiSubState = 80504) then begin  !JC
         jGuiSubState = 80511
      endif

      call javaEvent(terminalSubStateMsg)

      s2$ = ingresoCuenta$( 2 )
      if java.init then begin
         jGuiSubState = 0
      endif
      if s2$ = "A" then exit function
      if s1$ = s2$ then begin
         cnb.accountId = right$( s1$, len( s1$ ) - 2 )
         ingresoDatosCuenta = 0
         exit function
      endif
      call muestraBorrar( "ERROR EN INGRESO DE", "DATOS" )
      f = f + 1

      if jGuiSubState = 80510 then begin
         jGuiSubState = 80527 
      endif else begin
         jGuiSubState = 0
      endif
   wend
end function

!------------------------------------------------------------------------
! Ingresa monto
!------------------------------------------------------------------------
function ingresoMonto
   integer*1 ingresoMonto, f
   string    s$, p$
   integer*4 v

   ingresoMonto = -1

   f = 1
   while f <= 3

      if not (jGuiSubState = 80538) then begin  !JC
         jGuiSubState = 80542
      endif

      call javaEvent(terminalSubStateMsg)

      s$ = ingresoDatos$( "INGRESE VALOR DEL",  nombreProceso(tipoCNB) )
      if left$( s$, 1 ) = "A" then exit function
      s$ = right$( s$, len( s$ ) - 2 )
      if esnumero(s$) then begin
         if ( val(s$) >= montoMinimo(tipoCNB) ) and ( val(s$) <= montoMaximo(tipoCNB) ) then begin
            p$ = s$
            if not (jGuiSubState = 80546) then begin  !JC
               jGuiSubState = 80547
            endif

            call javaEvent(terminalSubStateMsg)
            s$ = ingresoDatos$( "CONFIRMAR MONTO", "" )
            if left$(s$, 1) = "A" then exit function
            s$ = right$( s$, len( s$ ) - 2 )
            if s$ = p$ then begin
               cnbAmount$ = elider$( formateaMonto( val( s$ ) ) )
               ingresoMonto = 0
               if java.init then begin
                  jGuiSubState = 0
               endif
               exit function
            endif
         endif
      endif
      call muestraBorrar( "MONTO INVALIDO", "" )
      f = f + 1

      if jGuiSubState = 80538 then begin
         jGuiSubState = 80542 
      endif else begin
         jGuiSubState = 0
      endif
   wend
end function

!-------------------------------------------------------------------------
! Construye mensaje diferido, AVANCE CNB
!-------------------------------------------------------------------------
function generaMensajeDiferido
   string m$, \
          generaMensajeDiferido

   m$ = cnb.header                                  + separador +  \    campos 1 al 6 header comun
        "4"                                         + separador +  \  7 consulta meses plazo
        cnb.officeId                                + separador +  \  8 numero de tienda
        cnb.terminal                                + separador +  \  9 numero de terminal sin ceros a la izquierda
        cnb.terminalUser                            + separador +  \ 10 numero de cajero   sin ceros a la izquierda
        cnb.sequential                              + separador +  \ 11 numero secuencial de la transaccion
        cnb.entryMode                                              ! 12 fijo "012"

   generaMensajeDiferido = m$

end function

!------------------------------------------------------------------------------
! Construye mensaje simulacion, AVANCE CNB
!------------------------------------------------------------------------------
function generaMensajeSimulaAvance
string generaMensajeSimulaAvance,                                  \
       m$                                                          !

   m$ = cnb.header                                  + separador +  \    header comun campos 1 al 6
        "5"                                         + separador +  \  7 5 = Simulacion de AVANCE
        cnb.estado                                  + separador +  \  8 estado iniciar
        cnb.clientId                                + separador +  \  9 Id cliente
        cnb.clientIdType                            + separador +  \ 10 tipo de cliente
        cnb.accountId                               + separador +  \ 11 accountId
        right$("00" + str$(cnb.modoAvance),3)       + separador +  \ 12 modalidad de avance 001 Corriente 002 Diferido
        eliminaCar$( cnbAmount$, "," )              + separador +  \ 13 amount sin separador de miles
        cnb.currency                                + separador +  \ 14 currency
        cnb.diferido                                + separador +  \ 15 meses de diferimiento
        cnb.officeId                                + separador +  \ 16 officeId
        cnb.terminal                                + separador +  \ 17 terminal
        cnb.terminalUser                            + separador +  \ 18 terminalUser
        cnb.sequential                              + separador +  \ 19 sequential
        cnb.entryMode                                              ! 20 mode 012

   generaMensajeSimulaAvance = m$

end function

!-------------------------------------------------------------------------
! Construye mensaje efectivizar, AVANCE CNB
!-------------------------------------------------------------------------
function generaMensajeEfectivizarAvance
   string generaMensajeEfectivizarAvance, \
          m$, CV2$

!call logCNB("generaMensajeEfectivizarAvance: cnb.simulaAvance=[" + str$(cnb.simulaAvance) + "] CVV2 [" + cnb.cvv2 + "]")

   if cnb.simulaAvance = 2 then begin
      cnb.estado       = "iniciar"
      CV2$             = "0"
   endif     \
   else begin
      cnb.estado = "reglaSeguridad"
      CV2$       = cnb.cvv2
   endif

   m$ = cnb.header                                  + separador +  \  1-6 header comun campo
        "6"                                         + separador +  \  7 6 = Efectivisar AVANCE
        cnb.estado                                  + separador +  \  8 Estado de envio (iniciar- reglaSeguridad)
        cnb.otp                                     + separador +  \  9 Codigo OTP enviado al cliente
        cnb.officeId                                + separador +  \ 10 Numero de sucursal
        cnb.terminal                                + separador +  \ 11 Numero de caja
        cnb.terminalUser                            + separador +  \ 12 Usuario del terminal donde se ejecuta la transaccion
        cnb.sequential                              + separador +  \ 13 Numero secuencial de la transaccion.
        cnb.entryMode                               + separador +  \ 14 fijo 012
        cnb.clientId                                + separador +  \ 15 Id cliente
        cnb.clientIdType                            + separador +  \ 16 tipo de cliente
        cnb.accountId                               + separador +  \ 17 ultimos digitos tarjeta de credito
        cnb.sesion                                  + separador +  \ 18 Sesion de la transaccion.
        cnb.accountOwnerName                        + separador +  \ 19 Nombre titular cuenta
        cnb.creditCardBrand                         + separador +  \ 20 Nombre titular cuenta
        cnb.maskAccountId                           + separador +  \ 21 numero enmascarado TC
        cnb.HashAccount                             + separador +  \ 22 Hash TC
                                                                   \    trasactionDetail
        cnb.hashTransaction                         + separador +  \ 23 Hash de la transaccion
        right$("00" + str$(cnb.modoAvance),3)       + separador +  \ 24 modalidad avance 001 Corriente 002 Diferido
        eliminaCar$( cnbAmount$, "," )              + separador +  \ 25 Monto solicitado
        cnb.currency                                + separador +  \ 26 currency
        cnb.diferido                                + separador +  \ 27 meses plazo
                                                                   \    detailsCashAdvanced
        cnb.servFeeAmount                           + separador +  \ 28 monto sin separador de miles  por definir
        cnb.servFeeTax                              + separador +  \ 29 tax                           por definir
        cnb.servFeeTotal                            + separador +  \ 30 total                         por definir
                                                                   \    factorDetails
        cnb.factorDetalleFactor                     + separador +  \ 31 factor                        por definir
        cnb.factorDetalleInterest                   + separador +  \ 32 interes                       por definir
        cnb.factorDetalleFinalAmount                + separador +  \ 33 monto final                   por definir
        cnb.factorDetalleQuota                      + separador +  \ 34 Por definir (el valor corresponde al devuelto en el campo fee)
                                                                   \
        cnb.accountType                             + separador +  \ 35 CC corriente, CA ahorros)
        CV2$                                        + separador +  \ 36 Valor cvv2 tarjeta credito
        cnb.fechaExpira                                            ! 37 CCVV fecha vencimiento tarjeta

   generaMensajeEfectivizarAvance = m$

end function

!------------------------------------------------------------------------
!
!------------------------------------------------------------------------
function generaMenuCuentas
integer*1 f,x,y,z
string generaMenuCuentas,               \
       paso1$,                          \
       paso2$,                          \
       paso3$                           !

!12345678901234567890123456789012345678901234567890
!2100xxxx01-2100xxxx02-2100xxxx03-2100xxxx04

generaMenuCuentas = ""
x = 1
y = 0
z = -1
paso1$ = ""
paso2$ = ""
paso3$ = ""
while z
   f = match("-",cnb.listMaskedAccount,x)
   y = y + 1
   if f = 0 then begin
      f = len(cnb.listMaskedAccount)
      if f > x then begin
         paso1$ = paso1$ + str$(y) + " = " + right$(cnb.listMaskedAccount,5) + " "
         paso2$ = paso2$ + str$(y)
         paso3$ = paso3$ + left$(mid$(cnb.listMaskedAccount,x,f-x)+string$(10," "),20)
      endif
      z = 0
   endif \
   else begin
      paso1$ = paso1$ + str$(y) + " = " + mid$(cnb.listMaskedAccount,f-5,5) + " "
      paso2$ = paso2$ + str$(y) + ","
      paso3$ = paso3$ + left$(mid$(cnb.listMaskedAccount,x,f-x)+string$(10," "),20)
      x = f + 1
   endif
wend

paso1$ = left$(paso1$ + string$(40," "),40) + separador + paso3$ + separador + paso2$

! contenido de salida funcion en paso1$
! PI  PF  Contenido              Ejemplo
!  1  20  linea 1 visor    "1 = XXX01 2 = XXX02 "
! 21  40  linea 2 visor    "3 = XXX03 4 = XXX04 "
! 41  60  cuenta 1         "2100xxxx01          "
! 61  80  cuenta 2         "2100xxxx02          "
! 81 100  cuenta 3         "2100xxxx03          "
!101 120  cuenta 4         "2100xxxx01          "
!120 xxx  opciones         "1,2,3,4"

generaMenuCuentas = paso1$

end function

!------------------------------------------------------------------------
!
!------------------------------------------------------------------------
function seleccionaOpcionMenu
   integer*1 seleccionaOpcionMenu,  \
      f,x                           !

   string s$,                              \
      xx$,                             \
      yy$,                             \
      msg1$,                           \linea 1 display
      msg2$,                           \linea 2 display
      msg3$,                           \cuentas
      msg4$,                           \punteros de cuentas
      paso$                            !

   seleccionaOpcionMenu = -1
   paso$ = ""

   if cnb.simulaAvance = 0 then begin
      msg1$ = "MESES DIFERIDo"
      msg2$ = cnb.listaDiferido
   endif else if cnb.simulaAvance = 1 then begin
      paso$ = generaMenuCuentas
      if paso$ = "" then exit function
      msg1$ = left$(paso$,20)
      msg2$ = mid$(paso$,21,20)
      msg3$ = buscaCampo( paso$, separador, 2 )
      msg4$ = buscaCampo( paso$, separador, 3 )
   endif

   f = -1
   while f

      if not (jGuiSubState = 80520) then begin  !JC
         jGuiSubState = 80527
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$( msg1$, msg2$ )
      if left$(s$,1) = "P" then begin
         s$ = right$( s$, len(s$) - 2 )
         xx$ = "," + s$ + ","
         yy$ = "," + msg4$ + ","

         x  = match(xx$,yy$,1)
         if x <> 0 then begin
            f = val(s$)
            cnb.maskAccountId = buscaCampo( cnb.listMaskedAccount, "-", f)                ! elider$(mid$(msg3$,(val(s$)*20-19),20))
            cnb.hashAccount   = buscaCampo( cnb.listaHashAccount,  "-", f)
            f = 0
         endif else begin
            call muestraBorrar( "DIGITE OPCION VALIDA", "[" + msg4$ + "]" )
         endif
      endif

      if jGuiSubState = 80527 then begin
         jGuiSubState = 80520 
      endif else begin
         jGuiSubState = 0
      endif
   wend

   call muestraBorrar( "SELECCIONA CUENTA", left$( cnb.maskAccountId + string$( 20 - len( cnb.maskAccountId), " " ), 20 ) )

   seleccionaOpcionMenu = 0
end function

!------------------------------------------------------------------------
!
!------------------------------------------------------------------------
sub validaFechaVencimiento(s$, x)
integer*1 x
string s$

if esnumero(s$) then begin
   if val(mid$(s$,1,2)) >= 1 and val(mid$(s$,1,2)) <=12 then begin
      if val(mid$(s$,3,2)) > val(mid$(cnb.datosTrx,14,2)) then begin
         x = 1
         exit sub
     endif \
     else if val(mid$(s$,3,2)) = val(mid$(cnb.datosTrx,14,2)) and val(mid$(s$,1,2)) >= val(mid$(cnb.datosTrx,16,2)) then begin
         x = 1
       exit sub
      endif
   endif
endif

end sub

!------------------------------------------------------------------------
!
!------------------------------------------------------------------------
function solicitaFechaVencimiento
   integer*1 solicitaFechaVencimiento,                     \
             f,                                            \
             x                                             !

   string    s$                                            !

   solicitaFechaVencimiento = -1
   cnb.fechaExpira          = ""                           ! fORMATO MMAA
   x = 0
   f = 1

   while f <= 3

      if not (jGuiSubState = 80524) then begin  !JC
         jGuiSubState = 80530
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("INGRESE FECHA EXPIRA","TARJ. CRED. MM/AA")
      if left$(s$,1) = "A" then exit function
      if left$(s$,1) = "P" then begin
         s$ = right$( s$, len(s$) - 2 )
         if len(s$) = 4 then begin
            call validaFechaVencimiento(s$, x)
            if x = 1 then begin
               cnb.fechaExpira = right$(s$,2) + left$(s$,2)  ! formato aamm
               f = 4
            endif \
         else begin
            f = f + 1
               call muestraBorrar( "FECHA INVALIDA MM/AA", "REINTENTE" )
            endif
       endif
      endif

      if jGuiSubState = 80530 then begin
         jGuiSubState = 80524 
      endif else begin
         jGuiSubState = 0
      endif
   wend
   if java.init then begin
      jGuiSubState = 0
   endif
   solicitaFechaVencimiento = 0
end function

!------------------------------------------------------------------------
!
!------------------------------------------------------------------------
function confirmaAvance
   integer*1 confirmaAvance,       \
      f, x
   string    s$                    !

   confirmaAvance = -1
   f = -1
   while f

      if not (jGuiSubState = 80605) then begin
         jGuiSubState = 80606
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("CONFIRMA AVANCE ?   ", "1. SI   2. NO       ")
      if left$(s$,1) = "A" then exit function
      if left$(s$,1) = "P" then begin
         s$ = right$( s$, len(s$) - 2 )
         if esnumero(s$) then begin
            x = match("," + s$ + ",",",1,2,",1)
            if x <> 1 then begin

               jGuiSubState = 80610
               call javaEvent(terminalSubStateMsg)
               s$ = ingresoDatos$("1 NUEVA SIMULACION  ", "2 CANCELA OPERACION ")
               if left$(s$,1) = "A" then exit function
               if left$(s$,1) = "P" then begin
                  s$ = right$( s$, len(s$) - 2 )
                  if esnumero(s$) then begin
                     x = match("," + s$ + ",",",1,2,",1)
                     if x = 1 then confirmaAvance = 2
                     exit function
                  endif
               endif
            endif
            f = 0
         endif
      endif

      if jGuiSubState = 80606 then begin
         jGuiSubState = 80605 
      endif else begin
         jGuiSubState = 0
      endif
   wend

   confirmaAvance = 0
end function

!------------------------------------------------------------------------
!Pide codigo CVV2
!retorna -1 si se cancela el ingreso
!         0 ingreso ok
!------------------------------------------------------------------------
function solicitaCVV2
   integer*1 solicitaCVV2, f
   string    m$, s$

   solicitaCVV2 = -1
   f = 1
   while f <= 3

      if not (jGuiSubState = 80520) then begin  !JC
         jGuiSubState = 80527
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$( "INGRESE CCVV DE LA","TARJETA")
      if left$( s$, 1 ) = "A" then exit function
      s$ = right$( s$, len( s$ ) - 2 )
      if esnumero(s$) and len(s$) = 3 then begin
         solicitaCVV2 = 0
         cnb.cvv2     = s$
         exit function
      endif else begin
         call muestraBorrar( "OPCION NO VALIDA    ","REINTENTE           " )
         goto finsiclo
      endif

      if jGuiSubState = 80527 then begin
         jGuiSubState = 80520 
      endif else begin
         jGuiSubState = 0
      endif

      finsiclo:
      f = f + 1
   wend

   if java.init then begin
      jGuiSubState = 0
   endif

end function

!------------------------------------------------------------------------
! Consulta Banco
! retorna -1 banco rechaza la transaccion
!          0 banco acepta la transaccion
!------------------------------------------------------------------------
function consultaBanco
   integer*1 consultaBanco, k
   string    m$, s$

   consultaBanco = -1

   !Genera mensaje para consultar a banco
   cnb.estado = "iniciar"

   if tipoCNB = 4 then begin
      m$ = generaMensajeSimulaAvance
   endif else begin
      m$ = generaMensaje
   endif

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB") then begin
      !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   recibe.respuesta:

   call leeMensajeCNB( m$ )

   if cnb.error <> "1" then begin
      s$ = cnb.message + string$(40," ")
      call muestraBorrar( left$(s$,20), mid$(s$,21,20) )
      exit function
   endif

   cnb.estado = buscaCampo( m$, separador, 8 )
   if tipoCNB = 4 then cnb.sequential = buscaCampo( m$, separador, 31 ) \
   else cnb.sequential = buscaCampo( m$, separador, 10 )

   k = 0
   if tipoCNB = 2 then k = 24
   if tipoCNB = 3 then k = 25
   if k <> 0 then cnbCommissionTrx$ = buscaCampo( m$, separador, k )

        if tipoCNB = 1 then k = 20 \
   else if tipoCNB = 4 then k = 12 \
   else k = 21

   s$ = buscaCampo( m$, separador, k )
   s$ = left$( s$ + string$( 20, " " ), 20 )

   !Deposito: Muestra nombre del titular de la cuenta y monto
   if tipoCNB = 1 then begin
      s$ = s$ + "$" + cnbAmount$
   endif

   !Retiro y AVANCE: Muestra nombre del titular de la cuenta, monto y comision
   if tipoCNB = 2 or tipoCNB=4 then begin
      s$ = s$ + "$" + cnbAmount$ + \
         " comm $" + cnb.servFeeTax
   endif

   !Consulta de Saldo: Muestra nombre del titular de la cuenta y comision
   if tipoCNB = 3 then begin
      s$ = s$ + "comm $" + cnbCommissionTrx$ + string$(20," ")
   endif

   call muestraBorrar( left$( s$, 20 ), mid$( s$, 21, 20 ) )

   ! AVANCE rescata campos respuesta simulacion
   if tipoCNB = 4 then begin

      cnb.maskedCreditCard         = buscaCampo( m$, separador, 11)  ! Numero enmascarado de la tarjeta de credito
      cnb.accountOwnerName         = buscaCampo( m$, separador, 12)  ! Nombre del cliente
      cnb.listMaskedAccount        = buscaCampo( m$, separador, 13)  ! Lista numero de cuenta enmascarado
      cnb.listaHashAccount         = buscaCampo( m$, separador, 14)  ! Lista hash de la cuenta
      cnb.hashTransaction          = buscaCampo( m$, separador, 15)  ! Hash de la transaccinon
      cnb.sesion                   = buscaCampo( m$, separador, 16)  ! Sesion de la transaccion.
      cnb.creditCardBrand          = buscaCampo( m$, separador, 17)  ! nombre de la tarjeta de credito

      cnb.descriptionTarjeta       = buscaCampo( m$, separador, 19)  ! Descripcion de la tarjeta
      cnb.authorizationNumber      = buscaCampo( m$, separador, 23)  ! Por definir

      cnb.servFeeAmount            = buscaCampo( m$, separador, 24)  ! Por definir
      cnb.servFeeTax               = buscaCampo( m$, separador, 25)  ! Por definir
      cnb.servFeeTotal             = buscaCampo( m$, separador, 26)  ! Por definir

      cnb.factorDetalleFactor      = buscaCampo( m$, separador, 27)  ! Por definir
      cnb.factorDetalleInterest    = buscaCampo( m$, separador, 28)  ! Por definir
      cnb.factorDetalleFinalAmount = buscaCampo( m$, separador, 29)  ! Por definir
      cnb.factorDetalleQuota       = buscaCampo( m$, separador, 30)  ! Por definir

      cnb.sequential               = buscaCampo( m$, separador, 31)  ! Numero secuencial de la transaccion.

      if seleccionaOpcionMenu then exit function
      k = confirmaAvance

      if k = 2 then begin
         consultaBanco    = 2
         cnb.simulaAvance = 0
         exit function
      endif
      if k = -1                   then exit function
     if solicitaCVV2             then exit function
      if solicitaFechaVencimiento then exit function
   endif else begin
      jGuiSubState = 80560
      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("CONFIRMA (ANULAR=NO)", nombreProceso(tipoCNB) )
      if left$( s$, 1 ) = "A" then exit function
   endif

   consultaBanco    = 0
   cnb.simulaAvance = 2

end function

!------------------------------------------------------------------------
! Confirma Banco
! retorna -1 banco rechaza la transaccion
!          0 banco acepta la transaccion
!------------------------------------------------------------------------
function confirmaBanco
   integer*1 confirmaBanco
   string    m$, s$

   confirmaBanco = -1

   call muestraVisor( 1, "PROCESANDO", "TRANSACCION" )

   if tipoCNB = 4 then begin
      m$ = generaMensajeEfectivizarAvance

   endif else begin
      !Genera mensaje
      m$ = generaMensaje
   endif

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB") then begin
      !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

   recibe.respuesta:

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   call leeMensajeCNB( m$ )

   if cnb.error <> "1" then begin
      s$ = cnb.message + string$(40," ")
      call muestraBorrar( left$( s$, 20 ), mid$( s$, 21, 20 ) )
      exit function
   endif

   cnb.estado = buscaCampo( m$, separador,  8 )

   ! AVANCE rescata campos respuesta efectivizacion 1
   if tipoCNB = 4 then begin

      cnb.accountType              = buscaCampo( m$, separador, 15 )  ! account type
      cnb.officeName               = buscaCampo( m$, separador, 17 )  ! Nombre sucursal
      cnb.officeLocation           = buscaCampo( m$, separador, 18 )  ! Ubicacion(ciudad) de la sucursal

      cnb.descriptionTarjeta       = buscaCampo( m$, separador, 19)  ! Descripcion de la tarjeta

      cnb.servFeeAmount            = buscaCampo( m$, separador, 23)  ! Por definir
      cnb.servFeeTax               = buscaCampo( m$, separador, 24)  ! Por definir
      cnb.servFeeTotal             = buscaCampo( m$, separador, 25)  ! Por definir

      cnb.factorDetalleFactor      = buscaCampo( m$, separador, 26)  ! Por definir
      cnb.factorDetalleInterest    = buscaCampo( m$, separador, 27)  ! Por definir
      cnb.factorDetalleFinalAmount = buscaCampo( m$, separador, 28)  ! Por definir
      cnb.factorDetalleQuota       = buscaCampo( m$, separador, 29)  ! Por definir

      cnb.authorizationNumber      = buscaCampo( m$, separador, 30 )
      cnb.promoDescripcion         = buscaCampo( m$, separador, 31 )  ! Descripcion legalidad avance politica banco pichincha
      cnb.lot                      = buscaCampo( m$, separador, 32 )  ! Valor de numero Lote para impresion
      cnb.approval                 = buscaCampo( m$, separador, 33 )  ! Numero documento para impresion
      cnb.document                 = buscaCampo( m$, separador, 34 )  ! Numero documento para impresion
      cnb.control                  = buscaCampo( m$, separador, 35 )  ! Numero control para impresion
      cnb.authorizationCod         = buscaCampo( m$, separador, 36 )  ! Numero autorizacion para impresion
      cnb.sequential               = buscaCampo( m$, separador, 37 )  ! Numero secuencial de la transaccion.
   endif else \
      cnb.sequential = buscaCampo( m$, separador, 10 )

   if tipoCNB = 1 then call leeRespuestaCNB( m$ )

   confirmaBanco    = 0
   cnb.simulaAvance = 3

end function

!--------------------------------------------------------------------------
! Imprime recibo de transacciones CNB utilizando variables cnb.xxx
!--------------------------------------------------------------------------
sub imprimeRecibo( cantidad )
   integer*1 cantidad, i, k, tcnb
   string p$, c$, s$, b$, n$
   real x, y

   !activa des-activa negrita
   b$  = chr$(1Bh) + chr$(47H) + chr$(1)
   n$  = chr$(1Bh) + chr$(47H) + chr$(0)

   !  Centrado
   write #34; CHR$(1BH) + CHR$(61H) + CHR$(01H)
   tclose 34
   for i = 1 to cantidad

      p$ = "BANCO PICHINCHA C.A."
      write #34; b$
      call imprimeCJ( p$ )

      if tipoCNB = 4 then begin
         p$ = "1790010937001"
         call imprimeCJ( p$ )
      endif

      p$ = nombreProceso(tipoCNB)
      if tipoCNB = 3 then p$ = p$ + " DE SALDOS"
      if tipoCNB = 4 then begin
         if cnb.modoAvance = 1 then \
            p$ = p$ + " Efectivo Corriente" \
         else \
            p$ = p$ + " Efectivo Diferido / Efectivo Xpress"
      endif
      write #34; n$
      call imprimeCJ( p$ )

      if tipoCNB = 4 then begin

         p$ = CHR$(1BH) + CHR$(57H) + CHR$(1) + \  doble ancho on
              CHR$(1BH) + CHR$(68H) + CHR$(1)   !  doble alto on
         write #34; p$
         call imprimeCJ( cnb.creditCardBrand )
         p$ = CHR$(1BH) + CHR$(57H) + CHR$(00) + \ doble ancho off
              CHR$(1BH) + CHR$(68H) + CHR$(00)   ! doble alto  off
         write #34; p$

         p$ = "Tarjeta...........: XXXXXXXXX" + cnb.accountId
         call imprimeCJ( p$ )

         p$ = "Cuenta acreditada.: " + cnb.maskAccountId
         call imprimeCJ( p$ )

         if cnb.modoAvance = 1 then \
            p$ = "Corriente con intereses" \
         else \
            p$ = "Diferido con intereses"

         call imprimeCJ( b$ + p$ )

         p$ = "Meses plazo.......: " + cnb.diferido
         call imprimeCJ( n$ + p$ )

         p$ = "Aprobacion........: " + cnb.approval
         call imprimeCJ( p$ )

         p$ = "V.Solicitado......: " + cnbAmount$
         call imprimeCJ( p$ )

         p$ = "Cargo por servicio: " + cnb.servFeeAmount
         call imprimeCJ( p$ )

         p$ = "Iva...............: " + cnb.servFeeTax
         call imprimeCJ( p$ )

         p$ = "Interes...........: " + cnb.factorDetalleInterest
         call imprimeCJ( p$ )

         p$ = "Total a pagar.....: " + cnb.factorDetalleFinalAmount
         call imprimeCJ( p$ )

         p$ = "Moneda............: " + cnb.currency
         call imprimeCJ( p$ )

         p$ = "Nombre............: " + cnb.accountOwnerName
         call imprimeCJ( p$ )

         p$ = "CNB...............: " + cnb.officeName
         call imprimeCJ( p$ )

         !                   1234  567      8901    23456789     0123
         !                   llll  ccc      tttt    aaaammdd     hhmm
         !cnb.datosTrx   local(4), caja(3), trx(4), fecha(6) y hora(4)

         p$ = "ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC"
         p$ = mid$(cnb.datosTrx,12,4)                     + "/"  + \ aaaa
              mid$(p$,val(mid$(cnb.datosTrx,16,2))*3-2,3) + "/"  + \ mmm
              mid$(cnb.datosTrx,18,2)                     + "  " + \ dd
              mid$(cnb.datosTrx,20,2) + ":" + right$(cnb.datosTrx,2)

         p$ = "Fecha.............: " + p$
         call imprimeCJ( p$ )

         p$ = "Ciudad............: " + cnb.officeLocation
         call imprimeCJ( p$ )

         p$ = "Control...........: " + cnb.control
         call imprimeCJ( p$ )

         if cnb.modoAvance = 2 then begin

            p$ = "Esta transacción genera una contribucion  " + \
                 "especial  a  SOLCA  cuyo  valor  se vera  " + \
                 "reflejado en su estado de cuenta          "   !
            call imprime42( p$ )

         endif

         p$ = "Suc. " + left$(cnb.datosTrx,4) + " - Caja: " + mid$(cnb.datosTrx,5,3)
         call imprimeCJ( p$ )

         p$ = "T:"+ mid$( cnb.datosTrx, 8, 4 )  +  \
              "  H: " + mid$(cnb.datosTrx,20,2) + ":" + right$(cnb.datosTrx,2)  + \
              "  F: " + mid$(cnb.datosTrx,18,2) + "/" + mid$(cnb.datosTrx,16,2) + "/" + mid$(cnb.datosTrx,12,4)
         call imprimeCJ( p$ )

         p$ = "Nombre Cajero:" + leeOperador$( cnb.terminalUser )
         call imprimeCJ( p$ )
         !if tipoCNB <> 4 then write #34; NUO.SaltaLineas$ + chr$( 5 ) + NUO.CortaPapel$

         cnb.promoDescripcion = "Debo       y        Pagare     al   emisor" + \
                                "incondicionalmente,y sin protesto el total" + \
                                "de este  pagare mas los intereses y cargos" + \
                                "por servicios, en caso de  mora  pagare la" + \
                                "tasa maxima autorizada para el emisor     " + \
                                "Declaro   que   el   producto   de    esta" + \
                                "transaccion    no    sera   utilizado   en" + \
                                "actividades de lavado de dinero  y activos" + \
                                "financiamiento  del   terrorismo  u  otros" + \
                                "delitos                                   " + \
                                "                                          " + \
                                "                                          " + \
                                "                                          " + \
                                "                                          " + \
                                left$("Nombre: " + cnb.accountOwnerName + string$(34," "),42)

         call imprime42( cnb.promoDescripcion )

         p$ = string$( 38, "-" )
         write #34; lf$ + lf$
         call imprimeCJ( p$ )

         p$ = "EL ESTABLECIMIENTO CERTIFICA QUE LA "
         call imprimeCJ( p$ )
         p$ = "FIRMA DEL CLIENTE ES AUTENTICA"
         call imprimeCJ( p$ )
         write #34; lf$

         if i = 1 then \
            p$ = "COPIA CLIENTE" \
         else \
            p$ = "ORIGINAL EMISOR"

         call imprimeCJ( "           " + p$ )
      endif else begin
         if tipoCNB <> 7 then begin
            p$ = "CUENTA"
            if right$( cnb.accountType, 1 ) = "1" then p$ = p$ + " CORRIENTE" \
            else if right$( cnb.accountType, 1 ) = "2" then p$ = p$ + " DE AHORROS" \
            else if match(right$(cnb.accountType,1),"12",1) = 0 then \
               p$ = p$ + " [" + cnb.accountType + "]"
            call imprimeCJ( p$ )
         endif
         write #34; lf$
         if tipoCNB <> 3 then begin
            p$ = "Cuenta Cliente:"
            if tipoCNB = 7 then begin
               p$ = p$ + left$(cnb.accountId,4) + string$(len(cnb.accountId)-6,"X") + right$(cnb.accountId,2)
            endif \
            else p$ = p$ + cnb.maskAccountId
            call imprimeCJ( p$ )
         endif
         call imprimeCJ( "Nombre....:" + cnb.accountOwnerName )
         if tipoCNB = 3 then call imprimeCJ( "Cuenta Cliente:" + cnb.maskAccountId )
         if tipoCNB <> 7 then call imprimeCJ( "Documento.:" + cnb.sequential ) \
         else call imprimeCJ( "Documento.:" + cnb.documentoDeposito )

         if tipoCNB <> 3 then call imprimeCJ( "Efectivo..:" + cnbAmount$ )
         if tipoCNB = 7 then y = 0 \ ! reverso de deposito no tiene comision
         else call imprimeCJ( "Cargo.....:" + cnbCommissionTrx$ )

         if tipoCNB <> 3 then begin
            x = val(cnbAmount$) * 100.0
            y = val(cnbCommissionTrx$) * 100.0
            call imprimeCJ( "Total.....:" + str$(( x + y ) / 100.0 ) )
         endif
         if tipoCNB <> 3 then call imprimeCJ( "Moneda....:" + cnb.currency )
         call imprimeCJ( "Oficina...:" + cnb.officeName )
         p$ = mid$( cnb.datosTrx, 12, 4 ) + "/" + mid$( cnb.datosTrx, 16, 2 ) + "/" + mid$( cnb.datosTrx, 18, 2 ) + \
            " " + mid$( cnb.datosTrx, 20, 2 )  + ":" + mid$( cnb.datosTrx, 22, 2 )
         call imprimeCJ( "Fecha.....:" + p$ )
         call imprimeCJ( "Control...:" + cnb.control )
         write #34; lf$
         if tipoCNB = 3 then begin
            call imprimeCJ( "Saldo Contable: " + cnb.countable )
            call imprimeCJ( "Saldo Disponible: " + cnb.available )
            write #34; lf$
            call imprimeCJ( "" )
         endif

         p$ = "Suc:" + left$( cnb.datosTrx, 4 ) + " - " + \
            "Caja:" + mid$( cnb.datosTrx, 5, 3 )
         call imprimeCJ( p$ )
         p$ = "T:" + mid$( cnb.datosTrx, 8, 4 ) + \
            " H:" + mid$( cnb.datosTrx, 20, 2 )  + ":" + mid$( cnb.datosTrx, 22, 2 ) + \
            " F:" + mid$( cnb.datosTrx, 18, 2 ) + "/" + mid$( cnb.datosTrx, 16, 2 ) + "/" + mid$( cnb.datosTrx, 12, 4 )
         call imprimeCJ( p$ )
         if tipoCNB = 7 then \
            tcnb = 5 \
         else \
            tcnb = tipoCNB
         call imprimeCJ( "Recibo: " + mid$( cnb.datosTrx, 2, 3 ) + "-" + \
            str$( tcnb ) + "-" + right$( string$( 9, "0" ) + cnb.receipt, 9 ) )
         p$ = leeOperador$( cnb.terminalUser )
         call imprimeCJ( "Cajero:" + p$ )
         write #34; lf$
         call imprimeCJ( "**GUARDE SU RECIBO**" )
         p$ = "Cliente"
         if tipoCNB = 7 then \
            if i = 2 then p$ = "Emisor" \
         else \
            if tipoReimpresion = 2 then p$ = "Copia"
         call imprimeCJ( p$ )
      endif
      write #34; NUO.SaltaLineas$ + chr$( 5 ) + NUO.CortaPapel$

   next i

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg)
   endif
end sub

!------------------------------------------------------------------------
! Menu para un segundo reintento confirmacion AVANCE
!------------------------------------------------------------------------
function reintentaAVANCE
integer*1 reintentaAVANCE,                                              \
          f
string    s$, e$

reintentaAVANCE = 0
f               = -1
while( f )

   jGuiSubState = 80615
   call javaEvent(terminalSubStateMsg)
   s$ = ingresoDatos$("DESEA CONTINUAR"," 1 = SI  / 2 = NO")
   if left$(s$, 1) = "P" then begin
      e$ = right$( s$, len(s$) - 2 )
        if esnumero( e$ ) then begin
           f = val( e$ )
           if f = 1 then begin
              f = 0
              reintentaAVANCE = -1
           endif \
           else if f <> 2 then begin
              call muestraBorrar( "Opcion no valida", "Reintente" )
           endif \
           else f = 0
        endif
   endif
wend

end function

!------------------------------------------------------------------------
! Ingresa y consulta valides del codigo OTP
!------------------------------------------------------------------------
function consultaOTP
   integer*1 consultaOTP,                                               \
             f,                                                         \
          cnbRetryAvance                                             ! Indicador para reintento confirmacion AVANVE
   string    m$,                                                        \
             s$                                                         !

   consultaOTP = -1
   cnbRetryAvance   = 0

otroOTP:

   f = 1
   while f <= 3

      if not (jGuiSubState = 80520) then begin  !JC
         jGuiSubState = 80527
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$( "INGRESE CODIGO OTP", "" )
      if left$( s$, 1 ) = "A" then exit function
      s$ = right$( s$, len( s$ ) - 2 )
      if esnumero(s$) then cnb.otp = s$ \
      else begin
         call muestraBorrar( "Opcion no valida    ","Reintente           " )
         goto finsiclo
      endif
      call muestraVisor( 1, "PROCESANDO", "TRANSACCION" )

      if tipoCNB = 4 then begin
         m$ = generaMensajeEfectivizarAvance

      endif else begin
         !Genera mensaje
         m$ = generaMensaje
      endif

      !para evitar que existan mensajes antiguos
      call limpiaPipe

      !envia mensaje
      if not enviaMensaje( m$ ) then exit function

      !Espera que lleguen datos
      if not esperaPipe( pipeTimeout, "Cons CNB" ) then begin
         !call muestraBorrar( "No se recibe", "respuesta Cons CNB" )
         exit function
      endif

      !lee respuesta
      m$ = recibeRespuesta

      !Si hubo error de comunicacion
      if m$ = "" then exit function

recibe.respuesta:

      call leeMensajeCNB( m$ )

      if cnb.error = "1" then begin
         !AVANCE rescata datos respuesta efectivizacion 2
         if tipoCNB = 4 then begin

            cnb.officeName               = buscaCampo( m$, separador, 17 )  ! Nombre sucursal
            cnb.officeLocation           = buscaCampo( m$, separador, 18 )  ! Ubicacion(ciudad) de la sucursal

            cnb.descriptionTarjeta       = buscaCampo( m$, separador, 19 )  ! Descripcion de la tarjeta

            cnb.servFeeAmount            = buscaCampo( m$, separador, 23 )  ! Por definir
            cnb.servFeeTax               = buscaCampo( m$, separador, 24 )  ! Por definir
            cnb.servFeeTotal             = buscaCampo( m$, separador, 25 )  ! Por definir

            cnb.factorDetalleFactor      = buscaCampo( m$, separador, 26 )  ! Por definir
            cnb.factorDetalleInterest    = buscaCampo( m$, separador, 27 )  ! Por definir
            cnb.factorDetalleFinalAmount = buscaCampo( m$, separador, 28 )  ! Por definir
            cnb.factorDetalleQuota       = buscaCampo( m$, separador, 29 )  ! Por definir

            cnb.authorizationNumber      = buscaCampo( m$, separador, 30 )  ! Por definir
            cnb.promoDescripcion         = buscaCampo( m$, separador, 31 )  ! Descripcion legalidad avance politica banco pichincha
            cnb.lot                      = buscaCampo( m$, separador, 32 )  ! Valor de numero Lote para impresion
            cnb.approval                 = buscaCampo( m$, separador, 33 )  ! Numero documento para impresion
            cnb.document                 = buscaCampo( m$, separador, 34 )  ! Numero documento para impresion
            cnb.control                  = buscaCampo( m$, separador, 35 )  ! Numero control para impresion
            cnb.authorizationCod         = buscaCampo( m$, separador, 36 )  ! Numero autorizacion para impresion
            cnb.sequential               = buscaCampo( m$, separador, 37 )  ! Numero secuencial de la transaccion.

         endif else \
            call leeRespuestaCNB( m$ )

         consultaOTP = 0
         if java.init then begin
            jGuiSubState = 0
         endif
         exit function
      endif else begin
        s$ = cnb.message + string$(40," ")
         call muestraBorrar( left$( s$, 20 ), mid$( s$, 21, 20 ) )
         if tipoCNB = 4 then begin
            if cnbRetryAvance = 0 then begin                                ! ante un rechazo se reintenta por segunda vez
               if reintentaAVANCE then begin
                  cnbRetryAvance = 1

                  if jGuiSubState = 80527 then begin
                     jGuiSubState = 80520 
                  endif else begin
                     jGuiSubState = 0
                  endif
                  goto otroOTP
               endif
            endif
            exit function
         endif
      endif

      if jGuiSubState = 80527 then begin
         jGuiSubState = 80520 
      endif else begin
         jGuiSubState = 0
      endif

      finsiclo:
      f = f + 1
   wend

end function

!------------------------------------------------------------------------
! Graba Transaccion
!------------------------------------------------------------------------
sub grabaTransaccion
   integer*1 tcnb
   string s$, v$, c$, t$, i$, f$, d$, a$, cs$
   integer*4 v

!call logCNB("grabaTransaccion")

   v$ = valorCadena$( cnbAmount$ )

   if tipoCNB = 4 then c$ = valorCadena$( cnb.servFeeAmount ) \
   else if tipoCNB = 7 then c$ = valorCadena$( cnb.Inquiry  ) \
   else c$ = valorCadena$( cnbCommissionTrx$ )

   !En AVANCE se usa fecha del dia como fecha contable.
   if tipoCNB = 4 then begin

      cnb.accountingDate = "20" + date$
      cnb.receipt        = right$("000000000" + cnb.sequential,9)

!para validar campo cnb.receipt debe quedar 001042000000001 segun definicion de Adriana Leon, correo 25-02-2021
!call logCNB("grabatrandaccion cnb.receipt [" + TS.STORE$ + "][" + TS.TERMINAL$ + "][" + cnb.sequential + "]")

   endif

   if tipoCNB = 7 then begin
      if cnb.maskAccountId = "null" then cnb.maskAccountId = left$(cnb.accountId,4) + string$(len(cnb.accountId)-6,"X") + right$(cnb.accountId,2)
      tcnb = 5
   endif else \
      tcnb = tipoCNB

   s$ = pack$("99")                     + ":" + \ cadena de usuario
        pack$("20201007")               + ":" + \ cadena de CNB
        pack$(cnb.accountingDate)       + ":" + \ Fecha contable
        pack$(cnb.officeId)             + ":" + \ Sucursal
        pack$(cnb.terminal)             + ":" + \ Caja
        pack$(cnb.terminalUser)         + ":"   ! Cajero

   s$ = s$ + pack$(str$(tcnb))          + ":" + \ Tipo transaccion
        pack$(cnb.receipt)              + ":" + \ Numero de recibo
        pack$(cnb.clientIdType)         + ":" + \ Tipo de identificacion
        cnb.clientId                    + ":" + \ identificador de cliente
        pack$( v$ )                     + ":" + \ Valor transaccion (dos decimales)
        pack$( c$ )                     + ":" + \ Valor comision (dos decimales)
        cnb.maskAccountId               + ":" + \ Cuenta encriptada cliente
        pack$(str$(val(cnb.control)))   + ":"   ! Dato de control enviado por el Banco

        if tipoCNB <> 7 then s$ = s$ + pack$(cnb.sequential) + ":"   ! Numero secuencial de la transaccion

        if tipoCNB <> 5 and tipoCNB <> 7 then s$ = s$ + pack$(cnb.otp) + ":" ! OTP

   if tipoCNB = 4 then begin                    ! AVANCE

      cs$ = valorCadena$(cnb.servFeeAmount)
      t$  = valorCadena$(cnb.servFeeTax)
      i$  = valorCadena$(cnb.factorDetalleInterest)
      f$  = valorCadena$(cnb.factorDetalleFinalAmount)
      d$  = valorCadena$(cnb.document)
      a$  = valorCadena$(cnb.authorizationNumber)

      s$ = s$                           + ":" + \
      pack$(cnb.accountId)              + ":" + \ Numero tarjeta enmascarada
      pack$(cs$)                        + ":" + \ comision por servicio
      pack$(t$)                         + ":" + \ Iva
      pack$(i$)                         + ":" + \ Interes
      pack$(f$)                         + ":" + \ Monto total a pagar
      pack$(d$)                         + ":" + \ Documento
      pack$(a$)                         + ":" + \ Codigo de autorizacion
      pack$(str$(cnb.modoAvance))       + ":" + \ modalidad avance 001 Corriente 002 Diferido
      pack$(cnb.diferido)               + ":" + \ Meses plazo
      pack$(cnb.approval)                       ! Valor de numero de aprobacion para impresion
   endif

   if tipoCNB = 7 then begin       ! REVERSO DEPOSITO
      s$ = s$ + pack$(cnb.documentoDeposito) + ":" + \ numero de secuencia para reverso
      pack$(str$(0))
   endif

   if tipoCNB = 1 then \
      call grabaDotacion( 1, v$, "12", s$ ) !se incrementa el valor de la variable SL.HD.TRANSNUM
   if tipoCNB = 2 or tipoCNB = 7 then \
      call grabaRetiro( 1, v$, "16", s$ )   !se incrementa el valor de la variable SL.HD.TRANSNUM
   if tipoCNB = 3 or tipoCNB = 4 then \
      call grabaTrx99( 1, s$ )              !se incrementa el valor de la variable SL.HD.TRANSNUM

end sub

!------------------------------------------------------------------------
! Ingreso modalidad de AVANCE 1 Corriente 2 Diferido
! retorna -1 si se cancela el ingreso
!          0 ingreso ok
!------------------------------------------------------------------------
function solicitaModalidadAvance
   integer*1 solicitaModalidadAvance,                  \
             f                                         !
   string    s$,                                       \
             e$                                        !

   solicitaModalidadAvance = -1
   cnb.diferido            = "1"

   f = -1
   while f
      cnb.modoAvance = 0

      if not (jGuiSubState = 80529) then begin  !JC
         jGuiSubState = 80515
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$(" 1 CORRIENTE"," 2 DIFERIDO")
      if s$ = "A" then begin
         f = 0
      endif else begin
         if len(s$) <> 3 or match( mid$( s$, 3, 1 ), "12", 1 ) = 0 then begin
            call muestraBorrar( "DIGITE UNA OPCION", "VALIDA:(1-2)" )
         endif else begin
            if left$(s$, 1) = "P" then begin
               cnb.modoAvance = val( mid$(s$, 3, 1) )  ! 1 CORRIENTE, 2 DIFERIDO
               solicitaModalidadAvance  = 0
               f = 0
            endif
         endif
      endif

      if jGuiSubState = 80515 then begin
         jGuiSubState = 80529 
      endif else begin
         jGuiSubState = 0
      endif
   wend

   if java.init then begin
      jGuiSubState = 0
   endif

   salir:
   call ignoraTeclas

end function

!------------------------------------------------------------------------
!retorna -1 si se cancela el ingreso
!         0 ingreso ok
!------------------------------------------------------------------------
function pideDiferidos
integer*1 f, x, pideDiferidos           !
string    m$, s$,                       \
          msg1$,                        \
          msg2$,                        \
          xx$,yy$

   pideDiferidos = -1

   call generaEncabezadoCNB
   m$ = generaMensajeDiferido

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje de
   if not enviaMensaje( m$ ) then exit function

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Cons CNB") then begin
      !call muestraBorrar( "No se recibe", "respuesta Consul CNB" )
      exit function
   endif

   !lee respuesta
   m$ = recibeRespuesta

recibe.respuesta:

   !Si hubo error de comunicacion
   if m$ = "" then exit function

   call leeMensajeCNB( m$ )                                                           ! se obtiene cnb.error y cnb.message

   if cnb.error <> "1" then begin
      s$ = cnb.message + string$(40," ")
      call muestraBorrar( left$( s$, 20 ), mid$( s$, 21, 20 ) )
      exit function
   endif

   cnb.listaDiferido = buscaCampo( m$, separador, 8 )
   msg1$ = " MESES DIFERIDO"
   msg2$ = " "+ cnb.listaDiferido

   if cnb.listaDiferido = "" then begin
      call muestraBorrar( "DATO MESES DIFERIDO", "INCONSISTENTE" )
      exit function
   endif

   ! ccy menu opciones diferido
   f = -1
   while f
      s$ = ingresoDatos$( msg1$, msg2$ )
      if left$( s$, 1 ) = "P" then begin
         s$ = right$( s$, len(s$) - 2 )
         xx$ = "-" + s$ + "-"
         yy$ = "-" + msg2$ + "-"
         x = match( xx$, yy$, 1 )
         if x <> 0 then begin
            cnb.diferido = s$
            goto salir
         endif
      endif
      call muestraBorrar( "DIGITE UNA OPCION", "VALIDA:(" + cnb.listaDiferido + ")" )
   wend

   salir:
   pideDiferidos = 0

end function

!------------------------------------------------------------------------
! FUNCIONALIDAD PARA TRANSACCIONES DE CLIENTE NO BANCARIO
!------------------------------------------------------------------------
sub procesaCNB
   integer*1 f

   if ingresoIdentificacion then exit sub
   if ingresoDatosCuenta then exit sub

   newmodal:

   if tipoCNB = 4 then begin
      if solicitaModalidadAvance then exit sub
      if cnb.modoAvance = 2 then begin
         if pideDiferidos then exit sub
      endif
      cnb.simulaAvance = 1
   endif
   if tipoCNB <> 3 then begin
      if ingresoMonto then exit sub
   endif

   call generaEncabezadoCNB
   f = consultaBanco
        if f = -1 then exit sub \
   else if f =  2 then goto newmodal                             ! aborta confirmacion de AVANCE
   if confirmaBanco then exit sub
   if tipoCNB <> 1 then begin
      cnb.simulaAvance = 3
      if consultaOTP  then exit sub
   endif

   if tipoCNB <> 4 then call grabaComprobante                    ! para la reimpresion
   call grabaTransaccion                                         ! para el TLOG

   imprime:
   if tipoCNB = 4 or tipoCNB = 5 then f = 2 \
   else f = 1
   call imprimeRecibo(f)
   if tipoCNB = 1 or tipoCNB = 2 then call TSCSEC06       ! TSHIEC01

end sub

!-------------------------------------------------------------------
! Detecta secuencia de tecla 65 Operador reimpresion Comprobante CNB
!-------------------------------------------------------------------
function NUOTCNBsecuenciaReimprecionComprobante public
   integer*1 NUOTCNBsecuenciaReimprecionComprobante

   NUOTCNBsecuenciaReimprecionComprobante = 0

   ! Acceso a la secuencia de teclas 65 Operador reimpresion de comprobantes CNB
   if ( TS.IO.STATE = 10 ) and (TS.IO.MOTORKEY = 61 ) and ( TS.IO.DATA$(5) = "65" ) then NUOTCNBsecuenciaReimprecionComprobante = -1

end function

!---------------------------------------------------------------
! Detecta secuencia de tecla 66 Operador Anulacion Depositos CNB
!---------------------------------------------------------------
function NUOTCNBsecuenciaReversoDeposito public
   integer*1 NUOTCNBsecuenciaReversoDeposito

   NUOTCNBsecuenciaReversoDeposito = 0

   ! Acceso a la secuencia de teclas 66 Operador reverso de deposito
   if ( TS.IO.STATE = 10 ) and (TS.IO.MOTORKEY = 61 ) and ( TS.IO.DATA$(5) = "66" ) then NUOTCNBsecuenciaReversoDeposito = -1

end function

!------------------------------------------------------------
! Detecta secuencia de tecla 176 acceso a las operaciones CNB
!------------------------------------------------------------
function NUOTCNBsecuenciaTeclaCNB public
   integer*1 NUOTCNBsecuenciaTeclaCNB

   NUOTCNBsecuenciaTeclaCNB = 0

   ! Acceso a funcionalidades CNB tecla 176
   if ( TS.INTRX = 0 ) and ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 176 ) and ( TS.IO.KEYS(7) = 176 ) then NUOTCNBsecuenciaTeclaCNB = -1

end function

!------------------------------------------------------------------------
!Captura tecla 176, muestra menu CNB y llama a la funcion seleccionada
!captura los procedimientos:
!   65 Conex reimpresion de comprobantes CNB
!   66 Conex reverso de deposito
!DEBE LLAMARSE EN USER EXIT 14
!------------------------------------------------------------------------
sub NUOTCNB14 PUBLIC
   INTEGER*1 f
   string    s$, e$

   !Impedir que ingrese por teclado las formas de pago CNB (2 / monto EFECTIVO, 6 / monto EFECTIVO)
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 91 ) and \
      ( TS.IO.KEYS(3) = 78 ) and \
      ( ( TS.IO.DATA$(3) = "2" ) OR ( TS.IO.DATA$(3) = "6" ) ) and \
      ( TS.IO.KEYS(7) = 91 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   if not estado then exit sub

   !procedimiento 65 para re-impresion de comprobante
   !procedimiento 66 para anulacion de deposito
   !tecla 176 para menu CNB
   f = 0

   !if NUOTCNBsecuenciaReimprecionComprobante then f = 1
   !if NUOTCNBsecuenciaAnulaDeposito          then f = 2
   !if not NUOTCNBsecuenciaTeclaCNB and ( f <> 0 ) then exit sub

   if TS.IO.MOTORKEY = 61 then begin
      if val( TS.IO.DATA$(5) ) = 65 then begin
         f = 1
      endif else if val( TS.IO.DATA$(5) ) = 66 then begin
         if not anula.deposito.activo then begin
            call muestraBorrar( "ANULACION DEPOSITO", "NO HABILITADO" )
            goto salir
         endif
         f = 2
      endif else exit sub
   endif else if TS.IO.MOTORKEY <> 176 then exit sub

   if requiereCambioCaja then exit sub
   if not estado then begin
      call muestraBorrar( "CLIENTE NO BANCARIO", "NO HABILITADO" )
      goto salir
   endif

   if TS.INTRX <> 0 then begin
      call muestraBorrar( "NO PERMITIDO DENTRO ","DE UNA VENTA        " )
      goto salir
   endif

   !Inicia variables
   operation         = 62
   tipoReimpresion   = 0
   cnb.estado        = "iniciar"
   cnb.accountId     = "0"
   cnbCommissionTrx$ = "0"
   cnbAmount$        = "0"
   cnb.otp           = "0"
   cnb.sequential    = "0"
   cnb.officeId      = str$( val( TS.STORE$ ) )
   cnb.terminal      = str$( val( TS.TERMINAL$ ) )
   cnb.terminalUser  = str$( val( unpack$( TS.OPER$ ) ) )
   cnb.referential   = "0"
   cnb.accountType   = "0"
   cnb.simulaAvance  = 0
   cnb.receipt       = "000000000"

   cnb.currency      = "USD"
   cnb.entryMode     = "012"

   if f = 1 then begin
      if ingresoDatosReimpresion then goto salir
      if consultaComprobante then goto salir
      call imprimeRecibo(1)
      goto salir
   endif else if f = 2 then begin
      tipoCNB = 7
      if ingresoDatosAnulacionDeposito then goto salir
      if consultaAnulacionDeposito then goto salir
      call grabaComprobante
      call grabaTransaccion
      call imprimeRecibo(2)
      call TSCSEC06
      goto salir
   endif

   f = -1
   while f
      tipoCNB = 0

      if not (jGuiSubState = 80504) then begin  !JC
         jGuiSubState = 80511
      endif

      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("1 DEPOSITO 2 RETIRO", "3 CONSULTA 4 AVANCE")
      if s$ = "A" then f = 0 \
      else begin
         if len(s$) <> 3 or match( mid$( s$, 3, 1 ), "1234", 1 ) = 0 then begin
            call muestraBorrar( "DIGITE UNA OPCION","VALIDA:(1-2-3-4)" )
         endif else begin
            if left$(s$, 1) = "P" then begin
               e$ = mid$( s$, 3, 1 )
               tipoCNB = val(e$)
               call procesaCNB
               f = 0
            endif
         endif
      endif

      if jGuiSubState = 80511 then begin
         jGuiSubState = 80504 
      endif else begin
         jGuiSubState = 0
      endif 
   wend

   salir:
   call ignoraTeclas

end sub
