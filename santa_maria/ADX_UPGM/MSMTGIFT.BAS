!-----------------------------------------------------------------------------
! Modulo: MSMTGIFT.BAS Modulo Tarjeta de Regalo
! Fecha : 06 de Agosto de 2021
! Autor : CARLOS CALDERON
!------------------------------------------------------------------------------
%ENVIRON T

integer*1 estado,                      \ !
          depurar,                     \ !
          dummy,                       \ !
          operation,                   \ ! 65: Mensajeria Credito Institucional
          TREaccion%,                  \ ! tipo de operacion tarjeta regalo
                                       \ ! TREaccion% indicador
                                       \ !  0=consulta 1=consumo 2=reverso 3=reintegro 4 cierre
          treRucMsmIntrx,              \ ! indicador si la trx esta seteada con RUC MSM
          TREproceso%,                 \ ! tipo de proceso 0=consulta 1=consumo 2=reverso 3=reintegro 4 cierre
          TREinTrx%,                   \ !  0=No existen pagos tarjeta regalo
                                       \   -1=Si existen pagos tarjeta regalo
          GItarjetaPremio%,            \ ! indicador -1= Tarjeta premio
                                       \              0= No es tarjeta premio
          treCantidadPagos%,           \ ! cantidad de pagos tarjera regalo en la transaccion
          GIanulaPago%,                \ ! indicador de anulacion de pago 0=NO -1=SI
          tre.cant.max.pagos,          \ ! cantidad paximo de pagos con tarjeta de regalo permitidos
          trePagoAprobado,             \ ! Flaw de pago aprobado
          treInyPago,                  \ ! flaw de inyeccion de pago
          borrarBorrar                   ! Indicador de control cuando se presiona BORRAR/BORRAR

integer*4 pipeTimeout                    ! Tiempo de espera

string    tre.header,                  \ ! encabezado
          tre.neto.pago$,              \ ! valor neto pago
          tre.iva.pago$,               \ ! valor iva del pago
          valorConsumo$,               \ ! valor consumido
          cupoDisponible$,             \ ! cupo disponible
          tre.Excl$,                   \ !
          treCadenaVenta$,             \ ! cadena venta tarjeta regalo
          treCadenasIntrx$(2),         \ ! matriz con mensajes de pagos con tarjeta regalo
          tre.tazaImpIva$,             \ ! valor impuesto IVA ej=12
          tre.recibo$,                 \ ! numero de recibo
          treMsrData$,                 \ ! numero de la tarjeta
          tre.Crd.Pago$,               \ ! Tipo y variedad de pago
          tre.Crd.Prefijo$,            \ ! Prefijo tarjeta
          treRutMSM$,                  \ ! Rut Santa maria
          separador,                   \ !
          fechaModificacion,           \ ! Fecha modificacion modulo
          lf$                            ! salto de linea chr$(10)

integer*1 global TS.RECOVERY

integer*2 global SL.END                  ! last index to be used in summary log

%INCLUDE EAMTOPTS.J86
%INCLUDE NUOTVARI.J86                    !
%INCLUDE EAMTSWKG.J86                    ! working storage
%INCLUDE EAMTRANS.j86                    !
%INCLUDE JAVAGUIV.J86

%INCLUDE NUOTRUTI.J86                    ! Rutinas generales
%INCLUDE MSMTFACT.J86
%INCLUDE MSMTCLIE.J86
%INCLUDE NUOTPROM.J86
%INCLUDE JAVAGUIC.J86

sub TSHIECET external
end sub

!para grabar en el journal electronico
FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
   STRING QXL.TSUPEC21, PRTLINE
FEND

!Disponibiliza si se trata de una promocion Aleatoria libreria NUOTPROM.BAS
sub promocionAleatoria( PrAl ) external
    integer*1 PrAl
end sub

function MSMTGIFTsecuenciaTeclaTarjetaRegalo public
   integer*1 MSMTGIFTsecuenciaTeclaTarjetaRegalo

   MSMTGIFTsecuenciaTeclaTarjetaRegalo = 0
   if (TS.IO.MOTORKEY = 97) and (TS.IO.KEYS(2) = 97) and (TS.IO.STATE = 10) then MSMTGIFTsecuenciaTeclaTarjetaRegalo = -1

end function

!Disponibiliza RUC de MSM
sub getRUCMSM( RUCMSM ) public
    string RUCMSM
    RUCMSM = treRutMSM$
end sub

!------------------------------------------------------------------
! graba log "C:/TRACE/" + TS.TERMINAL$ + ".TXT"
!------------------------------------------------------------------
sub traceTRE( dato$ )
   string dato$
   if depurar then begin
      call traceNUO( dato$ )
   endif
end sub

function agregaPuntoDecimal( m$ )
   string f$, m$, mm$ , agregaPuntoDecimal

   if len(m$) <= 3 then begin
      mm$ = right$("000" + m$,3)
   endif else begin
      mm$ = m$
   endif
   !f$ = left$(m$,len(m$)-2) + "." + right$(m$,2)
   f$ = left$(mm$,len(mm$)-2) + "." + right$(mm$,2)

   agregaPuntoDecimal = f$

end function

!------------------------------------------------------------------------
! Genera encabezado del mensaje
! Operation 65: Transacciones tarjeta de regalo
!------------------------------------------------------------------------
sub generaEncabezadoTarjReg

   tre.header = "P"                             + separador + \ canal de comunicacion S=socket P=Pipe
             str$(operation)                    + separador + \ requerimiento mensaje por pipe para MSM FIJO 65
             "0000"                             + separador + \ canal de respuesta
             TS.TERMINAL$                       + separador + \ terminal de origen
             "0"                                + separador + \ tipo de conexion 0=temporal 1=permanente
             "20" + date$ + time$                             ! fecha y hora de inicio de la operacion

end sub

function generaMensaje
   string generaMensaje, m$, f$, vc$

!call traceTRE("generaMensaje_1" ) 

   if TREaccion% = 0 then \
      vc$ = "0" \
   else \
      vc$ = agregaPuntoDecimal( valorConsumo$ )

!call traceTRE("generaMensaje_2" ) 

   f$ = right$( "000" + TS.STORE$, 3 ) + "-" + right$( "000" + TS.TERMINAL$, 3 ) + "-" + right$("000000000" + str$(MSMTFACTnumeroFactura + 1),9)

   m$ = tre.header                                       + separador + \ ! header comun
        treMsrData$                                      + separador + \ ! numero de tarjeta
        vc$                                              + separador + \ ! valor consumo
        right$( "0000" + TS.STORE$, 4 )                  + separador + \ ! tienda
        right$( "000" + TS.TERMINAL$, 3 )                + separador + \ ! numero de caja
        str$( val( unpack$( TS.OPER$ ) ) )               + separador + \ ! Id cajero
        "20" + date$ + time$                             + separador + \ ! fecha hora aaaammddhhmmss
        right$( "0000" + str$( SL.HD.TRANSNUM + 1 ), 4 ) + separador + \ ! numero transaccion
        str$(TREaccion%)                                 + separador + \ ! accion 0=consulta 1=consumo 2=reverso 3=reintegro 4 cierre
        "Factura"                                        + separador + \ ! tipo documento (Fijo)
        f$                                                               ! numero de factura

   generaMensaje = m$

end function

!-----------------------------------------------------------------------------
! Lee archivo que contiene el cuerpo del mensaje
!-----------------------------------------------------------------------------
function leeBody
   integer*1 A%
   integer*2 indice
   integer*4 tamano
   string leeBody, body$, archivo$, correlativo$, f$

   leeBody = ""
   close 47
   archivo$ = "R::C:/CNB/"+"TRTP" + TS.TERMINAL$ + ".DAT"

   body$ = ""
   correlativo$ = ""

   TS.ER.RETURN = -1
   open archivo$ as 47
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "NO SE PUEDE ABRIR", "ARCHIVO DE RESPUESTA" )
      exit function
   endif

   tamano = size("R::C:/CNB/"+"TRTP" + TS.TERMINAL$ + ".DAT")

   !informacion del archivo debe contener caracter de fin de linea
   f$ = "C" + str$(tamano)
   read form f$;#47;body$
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "NO SE PUEDE LEER", "ARCHIVO DE RESPUESTA" )
      delete 47
      exit function
   endif

   if right$(body$,2) = CRLF$ then body$ = left$(body$,len(body$)-2)

   if len( body$ ) = 0 then begin
      call muestraBorrar( "ARCHIVO DE RESPUESTA", "SIN INFORMACION..." )
      delete 47
      exit function
   endif

   leeBody = body$
   delete 47
end function

!------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------
function recibeRespuesta
   string recibeRespuesta, a$, f$

   recibeRespuesta = ""
   a$ = recibeMensaje( "del banco" )
   f$ = leeBody

   if f$ = "" then begin
      call restauraVisor(1)
      exit function
   endif

   a$ = a$ + f$
   f$ = buscaCampo(a$, separador, 7)
   if buscaCampo(a$, separador, 7) <> "01" then begin
      call restauraVisor(1)
      call muestraBorrar( "Error del servicio", "de comunicacion" )
      exit function
   endif

   recibeRespuesta = a$
   call restauraVisor(1)

end function

function existePagoPorAnular( opcion )
   integer*1 x, opcion,                  \
             existePagoPorAnular         !

   string    f$,                         \
             numTar$,                    \
             monTar$                     !

   existePagoPorAnular = 0
   if ( not treInTrx% ) or ( treCantidadPagos% = 0 ) then exit function
   for x = 1 to treCantidadPagos%
      f$ = treCadenasIntrx$(x,2)
      if f$ = "" then begin
         f$ = treCadenasIntrx$(x,1)
         numTar$ = buscaCampo(f$, separador, 7)        ! numero de tarjeta
         monTar$ = buscaCampo(f$, separador, 8)        ! monto tarjeta
         if opcion = 0 then begin
            if ( val(valorConsumo$) = ( val(monTar$) * 100.0 ) ) and ( treMsrData$ = numTar$ ) then begin
               existePagoPorAnular = x
               exit function
            endif
         endif \
         else if opcion = 1 then begin
            existePagoPorAnular = x
            exit function
         endif
      endif
   next x

end function

!------------------------------------------------------------------------
! reemplaza el campo enesimo en men$
! los campos estan separados por sep$
!------------------------------------------------------------------------
function cambiaCampo(men$, sep$, num, new$)
   string    cambiaCampo, men$, sep$, new$, m$
   integer*2 num

   integer*2 f, i, k, h

   cambiaCampo = ""
   h = len(sep$)
   men$ = men$ + sep$
   k = 1 - h
   f = 0
   while f < num
      i = k + h
      k = match(sep$, men$, i)
      if k = 0 then begin
         cambiaCampo = "[E]" + men$
         exit function
      endif
      f = f + 1
   wend

   m$ = left$(men$,i-1) + new$ + mid$(men$,i + ( k - i ),len(men$)-(i + 2 + (k -i )))
   cambiaCampo = m$
end function

function leeTarjetaRegalo$ public
   string leeTarjetaRegalo$, f$
   string s$

!call grabaVariablesIO( "1 leeTarjetaRegalo$ " )
   call respaldaVariablesIO

   leeTarjetaRegalo$ = ""
   jGuiSubState = 80501
   call javaEvent(terminalSubStateMsg) !JC

   f$ = leeTeclado$("INGRESE TARJ. REGALO","")

!call grabaVariablesIO( "f$=[" + f$ + "] " )

   s$ = left$( f$, 1 )

   if s$ = "I" then begin
      call muestraBorrar("PROCESO CANCELADO   ","POR USUARIO         ")
      goto salir
   endif

   if s$ = "P" then begin
      if mid$( f$, 2, 1 ) = "0" then begin
         call muestraBorrar("SE REQUIERE LLAVE","SUPERVISOR P/DIGITAR")
         goto salir
      endif
   endif

   if len(f$) > 2 then begin
      treMsrData$ = right$( f$, len( f$ ) - 2 )
   endif else begin
      treMsrData$ = ""
   endif

   ! Dato ingresado no tiene el largo correcto, 13 digitos
   if len(treMsrData$) <> 13 then begin
      call muestraBorrar("DATO CAPTURADO NO ES","TARJETA REGALO")
      goto salir
   endif

   ! Dato ingresado no es tarjeta de Regalo
   if left$(treMsrData$,3) <> tre.Crd.Prefijo$ then begin
      call muestraBorrar("DATO CAPTURADO NO ES","TARJETA DE REGALO")
      goto salir
   endif

   if str$(digitoEAN( left$(treMsrData$,12) ) ) <> right$( treMsrData$, 1 ) then begin
      call muestraBorrar("REVISE DIGITO DE LA","TARJETA REGALO")
      goto salir
   endif

   leeTarjetaRegalo$ = treMsrData$

   salir:
   call restauraVariablesIO
!call grabaVariablesIO( "2 leeTarjetaRegalo$ " )

end function

function consultaTarjetaRegalo
   integer*1 consultaTarjetaRegalo,                   \
             x,                                         \
             y                                          !

   string    f$,                                        \
             m$,                                        \
             r$,                                        \
             msg.env$                                   !

   msg.env$ = ""
   consultaTarjetaRegalo = 0

!call traceTRE("consultaTarjetaRegalo TREaccion%=[" + str$(TREaccion%) + "]" )   

   if TREaccion%  = 0 or TREaccion% = 2 then begin
      if leeTarjetaRegalo$ = "" then exit function
   endif

   if ( ( TREaccion% = 2 ) and ( TREproceso% = 1 ) and ( GIanulaPago% ) ) then begin
      x = existePagoPorAnular( 0 )
      if x = 0 then begin
         call muestraBorrar("NO EXISTE ESTE PAGO","EN LA TRANSACCION")
         call ignoraTeclas
         exit function
      endif
      msg.env$ = treCadenasIntrx$( x, 1 )
      m$ = cambiaCampo(msg.env$,separador,14,"2")
      treCadenasIntrx$( x, 2 ) = m$
   endif \
   else begin
      call generaEncabezadoTarjReg

      !Genera mensaje
      m$ = generaMensaje

      ! se inicializa matriz para almacenar mensaje de pago y anulaciones de pago
      if ( treCantidadPagos% = 0 ) and ( not treInTrx% ) then dim treCadenasIntrx$(tre.cant.max.pagos,2)

      ! se respalda mensaje de pago
      if ( ( TREaccion% = 1 ) and ( TREproceso% = 1 ) and ( not GIanulaPago% )  ) then msg.env$ = m$

   endif

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   ! envia mensaje
   if not enviaMensaje( m$ ) then exit function

   ! espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Tarj Regalo" ) then exit function

   !lee respuesta
   m$ = recibeRespuesta

!call traceTRE("consultaTarjetaRegalo respuesta [" + m$ + "]" )

   if m$ = "" then exit function

   f$ = buscaCampo( m$, separador, 7 )                                ! codigo retorno 00=respuesta con error 01=respuesta ok
   r$ = buscaCampo( m$, separador, 8 )                                ! mensaje retorno de rechazo

   if f$ <> "01" then begin
      call muestraBorrar(mid$(r$,1,20),mid$(r$,21,20))
      exit function
   endif

   ! Almacena mensaje de pago en matriz de mensajes treCadenasIntrx$
   if ( ( TREaccion% = 1 ) and ( TREproceso% = 1 ) and ( not GIanulaPago% ) ) then begin
      call traceTRE("consultaTarjetaRegalo Almacena pago m$=[" + m$ + "] treCantidadPagos%=[" + str$(treCantidadPagos%) + "]")
      treCantidadPagos%     = treCantidadPagos% + 1
      treCadenasIntrx$(0,0) = str$(treCantidadPagos%)
      treCadenasIntrx$( treCantidadPagos%, 1 ) = msg.env$
   endif

   if ( TREaccion% = 0 ) and ( not GIanulaPago%  ) then begin
      cupoDisponible$ = buscaCampo( m$, separador,  8 )            ! saldo disponible
      f$              = buscaCampo( m$, separador,  9 )            ! S = Tarjeta premio N = No tarjeta premio
      if f$ = "S" then GItarjetaPremio% = -1 \                     ! -1= Tarjeta premio 0 = No tarjeta premio
      else GItarjetaPremio% = 0
   endif

   !Se controla que se trate de una tarjeta de regalo, cuando se esta en una venta con promocion aleatoria
   call promocionAleatoria( x )
   if x and ( not GItarjetaPremio% ) then begin
      call muestraBorrar("NO PERMITIDO TARJETA","PREMIO EN ALEATORIA")
      call ignoraTeclas
      exit function
   endif

  if ( TREproceso% = 0 ) then begin
      call TSHIECET
      call muestraBorrar("SALDO DISPONIBLE","Usd $" + cupoDisponible$)
      cupoDisponible$ = ""
   endif

   consultaTarjetaRegalo = -1

end function

sub GIinyectaPagoTarjetaRegalo
   integer*1 x

   if GIanulaPago% then TS.IO.KEYS(1) = 70
   TS.IO.MOTORKEY = 96
   TS.IO.KEYS(3)  = 78

   if GItarjetaPremio% then TS.IO.DATA$(3) = str$(4) \
   else TS.IO.DATA$(3) = str$(3)
   TS.IO.KEYS(7)  = 96
   TS.IO.DATA$(7) = valorConsumo$
   TS.IO.DATA$(9) = treMSRDATA$
   TREinTrx%    = -1
   trePagoAprobado = -1
   treInyPago      = -1

   if GItarjetaPremio%                and \                     ! Es tarjeta premio
      ( treRucMsmIntrx = 0 ) and \                              ! No se ha ingresado RUT de MSM en la TRX
      ( val(valorConsumo$) = ( MSMTFACTtotal - NUOTPROMdescuento ) ) then begin         ! Valor pagado es parcial no cubre toda la deuda no se debe
      x = asignaCliente( 2, treRutMSM$ )                        ! vincular RUC de MSM
      if x then treRucMsmIntrx = -1
   endif

   !call traceTRE("GIinyectaPagoTarjetaRegalo valorConsumo$=[" + valorConsumo$ + "] treMSRDATA$=[" + treMSRDATA$ + "]")

end sub

!----------------------------------------------------------------------------
! si f$ viene sin punto decimal ej: 1234  agrega 2 decimales dejandolo 123400
! si f$ viene con punto decimal ej: 12.00 elimina el punto   dejandolo 1200
! si f$ viene con punto decimal ej: 12.1  elimina el punto agrega un dejandolo 1200
!---------------------------------------------------------------------
sub format.add.decimal( f$ )
   integer*1 p
   string f$,m$

   p = match(".",f$,1)

   if ( p > 0 ) then begin
      if ( p + 1 ) = len(f$) then m$ = f$ + "0" \
      else m$ = f$
      m$ = left$( m$, p - 1 ) + right$( m$, len(m$) - p)
   endif \
   else m$ = f$ + "00"
   f$ = m$

end sub

sub calcula.impuesto
   string f$,m$
   real   vi

   tre.neto.pago$ = ""
   tre.iva.pago$  = ""
   tre.Excl$      = "0"

   vi = (val(valorConsumo$) * val(tre.tazaImpIva$))/ 10000.00
   tre.iva.pago$  = str$(round(vi,2,0)*100)
   tre.neto.pago$ = str$( val(valorConsumo$) - val(tre.iva.pago$) )

  valorConsumo$  = str$(val(valorConsumo$) /100.0)
  tre.iva.pago$  = str$(val(tre.iva.pago$) /100.0)
  tre.neto.pago$ = str$(val(tre.neto.pago$)/100.0)

end sub

! Se envia mensaje operacion de cierre
function enviaMensajeCierre

  integer*1 enviaMensajeCierre,       \
            x                         !

  string    m$,                       \
            f$,                       \
            r$                        !

  enviaMensajeCierre = 0

  for x = 1 to treCantidadPagos%
      r$ = treCadenasIntrx$(x,2)
      if r$ = "" then begin
         m$ = treCadenasIntrx$(x,1)
         f$ = cambiaCampo(m$,separador,14,"4")
         r$ = "BONO " + buscaCampo( f$, separador,  7 ) + " Monto " + buscaCampo( m$, separador,  8 )

         !para evitar que existan mensajes antiguos
         call limpiaPipe

         !envia mensaje
         if enviaMensaje( f$ ) then begin
            ! espera que lleguen datos
            if esperaPipe( pipeTimeout, "Tarj Regalo" ) then begin
               !lee respuesta
               m$ = recibeRespuesta
               f$ = buscacampo(m$,separador,7)
               if f$ <> "01" then begin
                  f$ = buscaCampo( m$, separador,  8 )
                  if f$ <> "" then f$ = "RECHAZO EN CIERRE CRD"
               endif \
               else begin
                  if GItarjetaPremio% then f$ = "SE ENVIO CIERRE TARJETA PREMIO" \
                  else f$ = "SE ENVIO CIERRE TARJETA REGALO"
               endif
               call imprimeSJ( f$ )
               call imprimeSJ( r$ )
            endif
         endif
      endif
   next x
   fin:
   enviaMensajeCierre = -1

end function

SUB MSMTGIFT02 PUBLIC
   integer*1 f

   if not estado then exit sub

   if TREinTrx% then begin                                 ! si existen pagos tarjeta.regalo en la trx
      f = enviaMensajeCierre                               ! entonces se envian mensajes operacion de cierre
   endif

   GItarjetaPremio%  = 0
   TREinTrx%        = 0
   treCantidadPagos% = 0
   GIanulaPago%     = 0
   treRucMsmIntrx   = 0
   dim treCadenasIntrx$(0,0)
   trePagoAprobado   = 0 
   borrarBorrar = 0

END SUB

SUB MSMTGIFT04 PUBLIC
   call MSMTGIFT02
END SUB

SUB MSMTGIFT05 PUBLIC
   call MSMTGIFT02
END SUB

SUB MSMTGIFT07 PUBLIC

   integer*1 k
   integer*4 inicioBloque
   string    v$,r$

   fechaModificacion = " 03 May 2022"

   separador          = "'|'"                             ! separador de campos mensajeria
   lf$                = chr$(10)                          ! fin de linea
   pipeTimeout        = 5000                              ! time out con WS MSM
   estado             = 0                                 ! Proyecto desactivado
   depurar            = 0                                 ! flag para debuguear
   dummy              = 0                                 ! flag para operar con dummy
   operation          = 65                                ! operacion fijo
   TREinTrx%          = 0                                 ! flag
   trecadenaventa$  = ""                                ! cadena string 99:20 02 y 12
   tre.tazaImpIva$    = "0"
   treCantidadPagos%  = 0                                 ! cantidad de pagos en la trx
   tre.cant.max.pagos = 10                                ! cantidad maxima de pagos permitidos con tajeta regalo en trx
   treRutMSM$       = "1792060346001"                     ! Rut de Santa Maria
   r$= ""
   r$ = MSMTFACTrucEstablecimiento$
   if r$ <> "" then begin
      treRutMSM$ = r$
   endif

   call imprimeCJ( "***** MODULO TARJETA REGALO **********" )

   inicioBloque = abreParametros( "R::$ARGENER", "[TARJETA REGALO]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE TAR REGALO", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO  2 Dic 2022" )
      call cierraParametros( 80 )
      exit sub
   endif

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )

   if v$ <> "" then begin
      if esNumero( v$ ) then k = val( v$ ) else k = 0
      if ( k and 1 ) = 1 then estado  = -1
      if ( k and 2 ) = 2 then depurar = -1
      if ( k and 4 ) = 4 then dummy   = -1
      if estado then begin

         tre.tazaImpIva$ = MSMTFACTimpuestos$(1)

         !tiempo de espera respuesta banco
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         if esNumero( v$ ) then pipeTimeout = val( v$ )

         ! prefijo tarjeta de regalo
         v$ = leeParametro$( "CODIGO_PREFIJO", 80, inicioBloque )
         if esNumero( v$ ) then tre.Crd.Prefijo$ = v$

         !Tipo y variedad de pago Crd Institucional
         v$ = leeParametro$( "TIPO_VARIEDAD_PAGO", 80, inicioBloque )
         if esNumero( v$ ) then tre.Crd.Pago$ = v$

         !cantidad maxima de pagos con tarjeta de regalo
         v$ = leeParametro$( "MAX_CANTIDA_PAGO", 80, inicioBloque )
         if esNumero( v$ ) then tre.cant.max.pagos = val(v$)

         !rut de Santa maria
         v$ = leeParametro$( "RUT_STA_MARIA", 80, inicioBloque )
         if esNumero( v$ ) then treRutMSM$ = v$

      endif
   endif

   call cierraParametros( 80 )

   if depurar then begin
      call imprimeCJ( "TIEMPO DE ESPERA   " + right$( string$( 13, " " ) +  str$( pipeTimeout )        , 13 ) )
      call imprimeCJ( "CODIGO_PREFIJO     " + right$( string$( 13, " " ) +        tre.Crd.Prefijo$     , 13 ) )
      call imprimeCJ( "TIPO_VARIEDAD_PAGO " + right$( string$( 13, " " ) +        tre.Crd.Pago$        , 13 ) )
      call imprimeCJ( "MAX. CANT. PAGO    " + right$( string$( 13, " " ) +  str$( tre.cant.max.pagos ) , 13 ) )
      call imprimeCJ( "RUT SANTA MARIA    " + right$( string$( 13, " " ) +        treRutMSM$         , 13 ) )
   endif

   if estado then r$ = "ON " else r$ = "OFF"
   call imprimeCJ( "ESTADO " + r$ + fechaModificacion )

end sub

SUB MSMTGIFT14 PUBLIC
   integer*1 k
   string    f$, a$

   ! Se controla si se presiono la tecla Borrar/Borrar cuando se espera
   ! la llave de supervisor y esta activo un pago de tarjeta regalo
   if trePagoAprobado and ( TS.IO.MOTORKEY = 73 ) and ( borrarBorrar = 1 ) then begin
      borrarBorrar = 2
      exit sub
   endif

   !Impedir que ingrese por teclado la forma de pago tarjeta premio (4 / monto BONO) 
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 96 ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "4" ) and \
      ( TS.IO.KEYS(7) = 96 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   call promocionAleatoria( k )

   !Si hay una promocion aleatoria solo se puede pagar con tarjeta regalo
   if k then begin
      if (( TS.IO.MOTORKEY =  96 )   and \
          ( TS.IO.KEYS(7)  =  96 )   and \
          ( TS.IO.KEYS(3)  =  78 )   and \
          ( TS.IO.DATA$(3) = "3" )   and \
          ( TS.IO.DATA$(7) <> "" ) ) or  \
         (( TS.IO.KEYS(1)  = 70 )    and \
          ( TS.IO.KEYS(6)  = 81 )    and \
          ( TS.IO.STATE    = 10 )    and \
          ( TS.IO.MOTORKEY = 81 ) )  or  \
         (( TS.IO.STATE    = 10 )    and \
          ( TS.IO.MOTORKEY = 80 )    and \
          ( TS.IO.KEYS(10) = 79 )    and \
          ( TS.IO.DATA$(10) <> "")) then begin

      endif \
      else begin
         call muestraBorrar("SECUENCIA DE TECLA","NO PERMITIDA")
         goto salir
      endif
   endif

   ! Valida estado funcionalidad on u off
   if not estado then begin
      if (TS.IO.KEYS(2) = 97) and (TS.IO.MOTORKEY = 97) and (TS.IO.STATE = 10) then begin
         call muestraBorrar("TARJETA DE REGALO","INHABILITADO")
         call ignoraTeclas
      endif
      exit sub
   endif

   ! Permite ANULA TOTAL si la trx tiene un venta con promocion aleatoria
   if ( TS.IO.KEYS(1) = 70 ) and ( TS.IO.KEYS(6) = 81 ) and ( TS.IO.STATE = 10 ) and \
      ( TS.IO.MOTORKEY = 81 ) then begin
      if TREinTrx% then begin
         call muestraBorrar("NO PERMITIDO EXISTEN","PAGOS TARJETA REGALO")
         goto salir
      endif
   endif

   ! Impide SUSPENDER transaccion si existen pagos de tarjeta de regalo
   if ( TS.IO.KEYS(1) = 82 ) and ( TS.IO.KEYS(6) = 81 ) and ( TS.IO.STATE = 10 ) and \
      ( TS.IO.MOTORKEY = 81 ) and TREinTrx% then begin
      call muestraBorrar("NO PERMITE SUSPENCIO","CON PAGO TARJ.REGA")
      goto salir
   endif

   ! Consulta cupo disponible tarjeta de regalo [Tecla 97]
   !if (TS.IO.MOTORKEY = 97) and (TS.IO.KEYS(2) = 97) and (TS.IO.STATE = 10) then begin
   if MSMTGIFTsecuenciaTeclaTarjetaRegalo then begin
      TREaccion%  = 0
      TREproceso% = 0

!call traceTRE("MSMTGIFT14 97 Llama consultaTarjetaRegalo")

      call consultaTarjetaRegalo
      goto salir
   endif

   ! Ingreso de un pago o anulacion de pago con tarjeta de Regalo
   ! con secuencia
   ! 3 / monto tecla 96        --> para un pago
   ! ANULA 3 / monto tecla 96  --> para una anulacion de pago
   if ( TS.IO.MOTORKEY = 96 ) and ( TS.IO.KEYS(7) = 96  ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "3" ) and \
      ( TS.IO.STATE = 10 ) and TS.INTRX then begin

      if java.Init = -1 Then begin 

         if NOT TS.BAL.TAKEN then begin
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            exit sub
         endif

         jGuiSubState = 80538
         call javaEvent(terminalSubStateMsg)

         a$ = ingresoDatos$( "INGRESE MONTO " , "" )
         if left$( a$, 1 ) = "A" then goto salir
         if left$( a$, 1 ) <> "P" then goto salir
         a$ = right$( a$, len( a$ ) - 2 )
         TS.IO.DATA$(7) = a$	

      endif

      if TS.IO.DATA$(7) = "" then begin
         TS.GUIDANCE = 1003 !B003 SECUENCIA DE TECLAS INCORRECTA
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      ! valida que la trx este totalizada
      if not TS.BAL.TAKEN then begin
         TS.GUIDANCE = 1020
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      if TS.IO.KEYS(1) = 70 then GIanulaPago% = -1 \
      else GIanulaPago% = 0

      ! Control para impedir se ingresen mas de 10 pagos con Tarjeta de Regalo
      if treCantidadPagos% = tre.cant.max.pagos then begin
         call muestraBorrar("NO PERMITIDO MAS DE ","[" + str$(tre.cant.max.pagos) + "] PAGO TARJ.REGALO")
         GIanulaPago% = 0
         goto salir
      endif

      ! Se valida que el monto ingresado sea valido
      valorConsumo$ = TS.IO.DATA$(7)
      if not esnumero(valorConsumo$) then begin
         call muestraBorrar("MONTO INVALIDO      ","REINTENTE           ")
         GIanulaPago% = 0
         goto salir
      endif

      ! Se valida que el monto no se cero
      if val(valorConsumo$) = 0 then begin
         call muestraBorrar("MONTO INGRESADO     ","NO PUEDE CER CERO   ")
         GIanulaPago% = 0
         goto salir
      endif

      ! se valida que lo pagado no supere el valor de la trx
      if ( GIanulaPago% = 0 ) and (( ( MSMTFACTtotal - NUOTPROMdescuento ) - TS.TENDERED(0) ) < val(valorConsumo$)) then begin
         call muestraBorrar("MONTO A PAGAR","SUPERIOR A DEUDA")
         goto salir
      endif

      ! se valida que lo pagado sea igual al valor de la trx solo para promocion aleatoria
      if ( GIanulaPago% = 0 ) and (( ( MSMTFACTtotal - NUOTPROMdescuento ) - TS.TENDERED(0) ) <> val( valorConsumo$ )) and ( k = -1) then begin
         call muestraBorrar("MONTO A PAGAR DEBE","SER IGUAL A DEUDA")
         goto salir
      endif

      if GIanulaPago% then TREaccion%  = 2 \
      else TREaccion%  = 0
      TREproceso%      = 1

call traceTRE("MSMTGIFT14_2 Llama consultaTarjetaRegalo")

      if consultaTarjetaRegalo = 0 then begin
         GIanulaPago% = 0
         goto salir
      endif \
      else begin
         if GIanulaPago% then begin
            ! se inyecta la anulacion del pago
            call GIinyectaPagoTarjetaRegalo
            exit sub
         endif
      endif

      f$ = cupoDisponible$
      call format.add.decimal( f$ )
      cupoDisponible$ = f$

      !Se valida que se tenga cupo disponible
      if esnumero( cupoDisponible$ ) then begin
         if val(cupoDisponible$) < val(valorConsumo$) then begin
            call muestraBorrar("CUPO INSUFICIENTE   ","SALDO USD=" + cupoDisponible$)
            goto salir
         endif
      endif \
      else begin
         call muestraBorrar("CUPO INVALIDO","VALIDE EN SERV.CLIEN")
         goto salir
      endif

     !se envia requerimiento por consumo operacion = 1
      TREaccion%  = 1
      TREproceso% = 1

!call traceTRE("MSMTGIFT14_3 Llama consultaTarjetaRegalo")

      if consultaTarjetaRegalo = 0 then begin
         call muestraBorrar("ERROR CONSULTA[1][1]","AL ENVIAR CONSUMO")
         goto salir
      endif

      call GIinyectaPagoTarjetaRegalo

!call traceTRE("MSMTGIFT14 Se inyectara un pago = [" + valorConsumo$ + "]")

   endif
   exit sub

   salir:
   call ignoraTeclas
END SUB

SUB MSMTGIFT20 PUBLIC
   integer*1 x
   integer*2 ind
   integer*4 acumEfe
   string dato$, pago$, monto$, vuelto$

   if not estado then exit sub

   ! Es una linea de pago con tarjeta de regalo porque treCadenaVenta$ es distinto de vacio. 
   if ( TS.LINETYPE = 2 ) and ( TS.LINEDATA = 0 ) and len(treCadenaVenta$) <> 0 then begin
      call QXL.TSUPEC21( "BONO " + treMsrData$ )
   endif

   if (TS.LINETYPE = 6) AND (TS.LINEDATA = 1) then begin
      x = existePagoPorAnular( 1 )
      if ( x = 0 ) then exit sub
      acumEfe = 0
      for ind = 1 to SL.END
         dato$ = leeCadenaTSL$(ind)
         if left$(dato$, 1) = chr$(5) or left$(dato$, 1) = chr$(6) then begin  !5:Pagos 6:Anulaciones
            pago$ = getCampoTSL$(dato$,2)
            if pago$ = "11" then begin
               monto$ = getCampoTSL$(dato$,3)
               if left$(dato$, 1) = chr$(6) then monto$ = "-" + monto$
               acumEfe = acumEfe + val(monto$)
            endif
         endif \
         else if left$(dato$, 1) = chr$(9) then begin                    !9:Vuelto
            pago$ = getCampoTSL$(dato$,2)
            if pago$ = "11" then begin
               vuelto$ = getCampoTSL$(dato$,3)
               acumEfe = acumEfe - val(vuelto$)
            endif
         endif
      next ind
      if ( acumEfe > 0 ) and ( acumEfe < 501 ) then begin
         x = asignaCliente( 2, treRutMSM$ )                      ! vincular RUC de MSM
         if x then treRucMsmIntrx = -1
      endif
   endif

END SUB

SUB MSMTGIFT30 PUBLIC

   trePagoAprobado = 0
   borrarBorrar    = 0

END SUB

SUB MSMTGIFT32 PUBLIC
   integer*1 aux%
   string Gr.Crd.Trama$,vc$,vn$,vi$,ve$

   if not estado then exit sub

   if TREinTrx% then begin
   
!call traceTRE("MSMTGIFT32 valorConsumo$=[" + valorConsumo$ + "] treInyPago=[" + str$(treInyPago) + "]")

      if treInyPago then begin
         treInyPago = 0

      call calcula.impuesto
      if tre.recibo$ = "" then tre.recibo$ = "10009"

      vc$ = str$(val(valorConsumo$) *100)
      vn$ = str$(val(tre.neto.pago$)*100)
      vi$ = str$(val(tre.iva.pago$) *100)
      ve$ = str$(val(tre.Excl$)     *100)

         aux% = 2                                                           ! pago con tarjeta de regalo
         if GIanulaPago% then aux% = 12                                     ! anulacion con tarjeta de regalo

    trecadenaventa$ = pack$("99")                            + ":" + \ ! cadena de usuario
                        pack$("20")                            + ":" + \ ! sub tipo 20 tarjeta de regalo
                        pack$(str$(aux%))                      + ":" + \ ! pago o manulacion con tarjeta regalo
                        pack$(right$(tre.header,14))           + ":" + \ ! fecha hora aaaammddhhmmss
                        pack$(str$(MSMTFACTnumeroFactura + 1)) + ":" + \ ! folio factura
                        pack$(treMSRDATA$)                   + ":" + \ ! numero tarjeta
                        pack$(vc$)                             + ":" + \ ! valor del Pago
                        pack$(vn$)                             + ":" + \ ! valor sin IVA
                        pack$(vi$)                             + ":" + \ ! valor impuesto IVA 12%
                        pack$(ve$)                             + ":" + \ ! valor base exento
                        pack$(tre.recibo$)                     + "::::"  ! numero de recibo
      endif
   endif

END SUB

SUB MSMTGIFT39 PUBLIC
   if not estado then exit sub

   ! Si se tiene una transaccion bancaria aprobada se setea indicador
   ! de control que permite controlar la generacion de una reversa automatica
   ! para los casos en los cuales la operadora no ingresa la llave del supervisor
   if trePagoAprobado then borrarBorrar = 1

END SUB

SUB MSMTGIFT40 PUBLIC
   if not estado then exit sub

   ! Si se tiene una transaccion bancaria aprobada se setea indicador
   ! de control que permite controlar la generacion de una reversa automatica
   ! para los casos en los cuales la operadora no ingresa la llave del supervisor
   if trePagoAprobado then borrarBorrar = 1

END SUB

SUB MSMTGIFT51 PUBLIC
      if not estado then exit sub

   ! Si se presiono la tecla Borrar/Borrar y estaba activa una
   ! transaccion con tarjeta regalo se regenera el IOPROC
   if trePagoAprobado and ( borrarBorrar = 2 ) and \
      ( TS.IO.STATE = 10 ) and ( TS.IO.NEXTSTATE = 0 ) then begin

      ! Se limpia la variable de control
      borrarBorrar = 0
  
      ! Se arma el arreglo del IOPROC con el pago tarjeta de regalo
      dim TS.IO.KEYS(10), TS.IO.DATA$(10)
      TS.IO.STATE = 10
      TS.IO.DEVICE = 1
      TS.IO.KEYS(3) = 78
      if GItarjetaPremio% then TS.IO.DATA$(3) = str$(4) \
      else TS.IO.DATA$(3) = str$(3)
      TS.IO.KEYS(7)   = 96
      TS.IO.MOTORKEY  = 96
      TS.IO.DATA$(7)  = valorConsumo$
      TS.IO.DATA$(9)  = treMSRDATA$
      TS.IO.KEYS(9)  = 90
      TS.IO.INPUT.READY = -1
   endif

END SUB

SUB MSMTGIFT53 PUBLIC
   string m$,f$

   if not estado then exit sub

   ! No deben considerarse las cadenas 99 20 en la recuperacion, deben ser eliminadas
   m$ = SL.STR.ENTRY$
   f$ = getCampoTSL$(m$,1)
   if f$ = "99" then begin
      f$ = getCampoTSL$(m$,2)
      if f$ = "20" then begin
         f$ = getCampoTSL$(m$,3)
         if ( f$ = "02" ) or ( f$ = "12" ) then begin
            TS.TEMP1I2 = 0
            exit sub
         endif
      endif
   endif \
   else begin
      if f$ = "05" or f$ = "06" then begin
         f$ = getCampoTSL$(m$,2)
         if f$ = tre.Crd.Pago$ then begin
            TS.TEMP1I2 = 0
            exit sub
         endif
      endif
   endif

END SUB

SUB MSMTGIFT68(ENTRY$,ELEMENT) PUBLIC
   string ENTRY$
   integer*2 ELEMENT

   if ( not estado ) or ( len(trecadenaventa$) = 0 ) then exit sub

   ! elimina string de pago 05:63 tarjeta de regalo
   if TS.RECOVERY and ( left$(ENTRY$,3) = chr$(5) + ":" + chr$(63H) ) then begin
      call evitaCadenaTSL( ELEMENT, ENTRY$ )
      exit sub
   endif

   ! elimina string de pago 99:20:02 y anulacion de pago 99:20:12 tarjeta de regalo
   if TS.RECOVERY and ( ( left$(ENTRY$,5) = chr$(99H) + ":" + chr$(20H) + ":" +   chr$(2) ) or \
                        ( left$(ENTRY$,5) = chr$(99H) + ":" + chr$(20H) + ":" + chr$(12H) )) then begin
      call evitaCadenaTSL( ELEMENT, ENTRY$ )
      exit sub
   endif

   ! agrega cadena de pago o anulacion de pago tarjeta de regalo
   if left$(ENTRY$, 1) = chr$(5) or left$(ENTRY$, 1) = chr$(6) then begin
      call agregaCadenaTSL68( trecadenaventa$ )
      ELEMENT = ELEMENT + 1
      trecadenaventa$ = ""
      GIanulaPago% = 0
   endif

END SUB