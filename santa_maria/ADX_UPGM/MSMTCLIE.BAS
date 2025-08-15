!-----------------------------------------------------------------------------
! Modulo: MSMTCLIE Cliente y Fidelidad
! Fecha : 27 diciembre de 2020
! Autor : Carlos Calderon
!
! History
! Fecha : 28 diciembre de 2021 se incorpora funcionalidad consulta automatica
!          y acumulacion de puntos.
!         03 enero de 2022 se incorpora funcionalidad de consulta manual
!         06 enero de 2022 se incorpora funcionalidad redencion de puntos
!         27 enero de 2022 se incorpora control que impide el uso via 
!                             secuencia de teclas de las variedades 2 y 6
!                             del medio de pago EFECTIVO
!         05 Mayo de  2022 se incorpora la funcionalidad de anulacion de 
!                             redencion de puntos.
!         10 mayo de 2022  se incorpora el envio del mensaje de anulacion de 
!                             redencion cuando, el POS recupera la trx antes
!                             una caida.
!         30 Nov de 2022   se trasladan tosa las rutinas de cliente desde
!                             MSMTFACT.BAS A.FARRE 
!------------------------------------------------------------------------------
%ENVIRON T

!variables del modulo

integer*1 cliOperacion,               \ Operacion para fidelidad 69
          cliIndic,                  \ Indicador de cliente
                                      \ Bit 0 Indica si es empresa estatal    (0->Si  1->No)
                                      \ Bit 1 Indica si es cliente fidelizado (0->Si  1->No)
                                      \ Bit 2 Indica si es cliente empleado   (0->Si  1->No)
          estado,                     \ indicador estado de funcionalidad 0 -> desactivado
          depurar,                    \ indicador para depurar el programa
                                      \ 0=NO HABILITADO
                                      \ 1=HABILITADO
                                      \ 3=HABILITADO CON TRACE
                                      \ 7=HABILITADO CON TRACE Y DUMMY
          dummy,                      \ indicador de autorespuesta dummy
          cliMpRedencion,             \ Medio de pago SA para la redencion de puntos
          cliRedencionIntrx,          \ Indicador si existe una redencion en la TRX
          cliEsEmpleado,              \
          cliSentido                  ! 4=redencion de puntos  5=Anulacion redencion de puntos


integer*2 cliTeclaRedencion,          \ Monto minimo canje o redencion de puntos
          clienteTeclaMotora          ! Tecla motora cliente, momentaneamente sin uso

integer*4 cliMontoRedencion           ! Monto redencion de puntos

string    fechaModificacion,          \ Fecha modificacion del modulo
          lf$,                        \ Salto de linea chr$(10)
          separador,                  \ Separador de campos agente "'|'"
          bodyFile$,                  \ Archivo con cuerpo de respuesta
                                      \
          msjConsulPtos$,             \ Mensaje   tipo 1 consulta fidelidad
          respuestaConsulPtos$,       \ Respuesta tipo 1 consulta fidelidad
                                      \
          msjAcumulPtos$,             \ Mensaje   tipo 2 acumulacion de puntos
          respuestaAcumulPtos$,       \ Respuesta tipo 2 acumulacion de puntos
                                      \
          msjRedencionPtos$,          \ Mensaje   tipo 4 redencion de puntos
          respuestaRedencionPtos$,    \ Respuesta tipo 4 redencion de puntos
                                      \
          cliUSDDisponible$,          \ valor en USD disponible para redencion ej "12.90"
                                      \
          cliFidelNeto$,              \
          cliFidelTotal$,             \
          cliMPMonto$,                \
                                      \
          cliValPtos$,                \ Texto comprobante fidelizacion valor en puntos
          cliValUSD$,                 \ Texto comprobante fidelizacion valor en USD
          clinom$,                    \ Texto comprobante fidelizacion nombre del cliente
          cliDesc$,                   \
          cliMsg1$,                   \ Texto comprobante fidelizacion mensaje 1
          cliMsg2$,                   \ Texto comprobante fidelizacion mensaje 2
                                      \
          cliId$,                     \ Id de cliente ingresado
          cliIdFidelizado$,           \ Id de cliente ya fidelizado en la transaccion
          cliIdType$,                 \ Tipo de cliente, 0001=Cedula, 0002=RUC, 0003=Pasaporte
                                      \
          cliHeader$,                 \ encabezado
          cliDatosTrx$,               \ Para operacion 1=Consulta --> va el Id del cliente
          cliCadenaAnulaRedencion$    \ Cadena de pago 05 redencion de puntos para determinar
                                      ! si se debe enviar una anulacion de redencion ante una caida del POS

!variables para manejo de comunicaciones por PIPE
integer*4 pipeTimeout                 ! Tiempo de espera

!-------------------------------------------------------------------------------
! variables para almacenar informacion de clientes
!-------------------------------------------------------------------------------
string tipoCliente, idCliente, nomCliente, \
   dirCliente, telCliente, codCliente, \
   cadCliente, regCliente, pasCliente, corCliente

!Variables globales de otros modulos
integer*1 global \
          TS.RECOVERY                 ! in recovery flag

integer*2 global \
          SL.END                      ! last index to be used in summary log

!variables globales de otros modulos
string    global TS.OPER.NAME$

%INCLUDE NUOTVARI.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86
%INCLUDE JAVAGUIV.J86

%INCLUDE EAMASMCT.J86
%INCLUDE MSMTFACT.J86                 ! Rutinas Factura
%INCLUDE NUOTRUTI.J86
%INCLUDE JAVAGUIC.J86
%INCLUDE NUOTPROM.J86
%INCLUDE NUOTDIVA.J86
%INCLUDE NUOTELEC.J86

!------------------------------------------------------------------
!Setea variables de cliente para el modulo de retenciones MSMTRETE.BAS
! cliId$
!------------------------------------------------------------------
sub seteaClienteRetenciones( cliId$ ) external
   string cliId$                          ! id cliente
end sub

!------------------------------------------------------------------
!Setea variables de cliente para el modulo de recarga MSMTREC.BAS
! cliId$
!------------------------------------------------------------------
sub seteaClienteRecarga( cliId$, cliIndic ) external
   integer*1 cliIndic                     ! Indicadores
   string cliId$                          ! id cliente
end sub

sub seteaClienteInventario( cliId$, cliIndic ) external
   integer*1 cliIndic                     ! Indicadores
   string cliId$                          ! id cliente
end sub

function secuenciaAnulaTotal
   integer*1 secuenciaAnulaTotal

   secuenciaAnulaTotal = 0

   if ( TS.IO.KEYS(1) = 70 ) and ( TS.IO.KEYS(6) = 81 ) and ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 81 ) then secuenciaAnulaTotal = -1

end function

function secuenciaSuspendeTransaccion
   integer*1 secuenciaSuspendeTransaccion

   secuenciaSuspendeTransaccion = 0

   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 81 ) and ( TS.IO.KEYS(1) = 82 ) and ( TS.IO.KEYS(6) = 81 ) then secuenciaSuspendeTransaccion = -1

end function

function EsSecuenciaAnulaRedencionCLI
   integer*1 EsSecuenciaAnulaRedencionCLI

   EsSecuenciaAnulaRedencionCLI = 0

   if ( TS.IO.STATE = 10 )                   and \
      ( TS.IO.MOTORKEY = cliTeclaRedencion ) and \
      ( TS.IO.KEYS(1) = 70 )                 and \
      ( TS.IO.KEYS(2) = cliTeclaRedencion )  and \  ! TS.IO.KEYS(6)
      TS.INTRX then begin
         EsSecuenciaAnulaRedencionCLI = -1
         TS.IO.DATA$(2)= ""                          ! si viene un monto digitado en la secuencia se ignora
      endif 

end function

function EsSecuenciaConsultaPuntosCLI public
   integer*1 EsSecuenciaConsultaPuntosCLI

   EsSecuenciaConsultaPuntosCLI = 0

   if TS.IO.STATE    =  10 and \
      TS.IO.MOTORKEY = 190 and \
      TS.IO.KEYS(2)  = 190 and \
      TS.IO.KEYS(4)  = 100 and \
      TS.INTRX = 0 then EsSecuenciaConsultaPuntosCLI = -1

end function

function EsSecuenciaRedencionPuntosCLI
   integer*1 EsSecuenciaRedencionPuntosCLI

   EsSecuenciaRedencionPuntosCLI = 0

   if ( TS.IO.STATE = 10 )                   and \
      ( TS.IO.MOTORKEY = cliTeclaRedencion ) and \
      ( TS.IO.KEYS(2)  = cliTeclaRedencion ) and \
      TS.INTRX then begin
      TS.IO.DATA$(2) = ""
      EsSecuenciaRedencionPuntosCLI = -1
   endif

end function

!------------------------------------------------------------------
! graba log "R::Q:/" o "R::C:/DQ/" + "TRC." + TS.TERMINAL$
!------------------------------------------------------------------
sub traceCLI( dato$ )
   string dato$
   if depurar then begin
      call traceNUO("MSMTCLIE " + dato$ )
   endif
end sub

sub seteaClienteFidelidad(id$, indic ) public
   integer*1 indic
   string id$

   if id$ = "" then begin
      cliId$ = ""
      cliIndic = 0
      cliIdFidelizado$ = ""
   endif else begin
      cliId$     = right$(id$,10)          ! Identificador de cliente
      cliIndic   = indic                   ! Indicador
   endif

end sub

!-----------------------------------------------------------------------------
! Lee archivo que contiene el cuerpo del mensaje
!-----------------------------------------------------------------------------
function leeBody
   integer*4 largo.body
   string leeBody, a$, b$

on error goto errloc

   leeBody = ""
   a$ = bodyFile$
   b$ = ""

   TS.ER.RETURN = -1

   open a$ as 47
   largo.body = size( a$ )

   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "NO SE PUEDE ABRIR", "ARCHIVO DE RESPUESTA" )
      exit function
   endif

   !informacion del archivo debe contener caracter de fin de linea

   read #47; b$

   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "NO SE PUEDE LEER", "ARCHIVO DE RESPUESTA" )
      delete 47
      exit function
   endif

   if len( b$ ) = 0 then begin
      call muestraBorrar( "ARCHIVO DE RESPUESTA", "SIN INFORMACION.." )
      delete 47
      exit function
   endif

   leeBody = b$
   delete 47

   sale:
   exit function

   errloc:
   if errf% = 39 then resume
   call traceCLI("leeBody err =" + err +  hexanum$( errn ) ) 
   resume sale
end function

sub generaEncabezado
   cliHeader$ = "P"                                + separador + \ canal de comunicacion S=socket P=Pipe
                str$(cliOperacion)                 + separador + \ requerimiento mensaje por pipe para MSM
                "0000"                             + separador + \ canal de respuesta
                TS.TERMINAL$                       + separador + \ terminal de origen
                "0"                                + separador + \ tipo de conexion 0=temporal 1=permanente
                "20" + date$ + time$                             ! fecha y hora de inicio de la operacion
end sub

Function pagosMontosTrx$
    integer*1 sentido,posc
    integer*2 x, ind
    integer*4 mpMon(2)
    string cadena$, tipCadena$, mp$, monto$, aux$, pagosMontosTrx$

    dim mpMon(20,2)
    aux$ = ""
    pagosMontosTrx$ = ""
    for x = 1 to SL.END
       cadena$ = SL.STR$( x )
       tipCadena$ = getCampoTSL$(cadena$,1)
       sentido = 1
       if tipCadena$ = "05" or tipCadena$ = "06" then begin
          mp$    = getCampoTSL$(cadena$,2)
          monto$ = getCampoTSL$(cadena$,3)
          posc = match(mp$ + "|", aux$,1)
          if posc = 0 then begin
             aux$ = aux$ + mp$ + "|"
             ind = len(aux$)/3
             mpMon(ind,1) = val(mp$)
             mpMon(ind,2) = val(monto$)
          endif \
          else begin
             if tipCadena$ = "06" then sentido = -1
             ind =( posc + 2 ) / 3
             mpMon(ind,2) = mpMon(ind,2) - (val(monto$) * sentido )
          endif
       endif
    next x

    if aux$ = "" then exit function

    cadena$ = ""
    for x = 1 to len(aux$)/3
       if mpMon(x,2) <> 0 then begin
          cadena$ = cadena$ + str$(mpMon(x,1)) + \
                              right$("000000000" + str$(mpMon(x,2)/100),9) + \
                              right$("00000000"  + right$(str$(mpMon(x,2)),2),8)
       endif
    next x
    pagosMontosTrx$ = cadena$

end function

function generaMensajeConsultaPuntos
   integer*1 generaMensajeConsultaPuntos
   string    m$

   generaMensajeConsultaPuntos = 0
   msjConsulPtos$              = ""
   call generaEncabezado
   cliDatosTrx$ = cliId$
   msjConsulPtos$ = cliHeader$ + separador + "1" + separador + cliDatosTrx$
   generaMensajeConsultaPuntos = -1

end function

sub borraFileBody
   string a$

   on error goto errloc

   a$ = bodyFile$
   open a$ as 47
   delete 47

   sale:
   exit sub

   errloc:
   if errf% = 39 then resume
   call traceCLI( "borraFileBody " + err +  hexanum$( errn ) )
   resume sale
end sub

function enviaMensajeConsultaPuntos
   integer*1 enviaMensajeConsultaPuntos

   enviaMensajeConsultaPuntos = 0

   !para evitar que existan mensajes antiguos
   call limpiaPipe
   call borraFileBody

   !envia mensaje consulta puntos
   if not enviaMensaje( msjConsulPtos$ ) then begin
      call traceCLI( "NO SE PUDO ENVIAR MENSAJE CONSULTA PUNTOS " )
      call muestraBorrar( "NO SE PUDO ENVIAR", "MSG CONSULTA PUNTOS" )
      cliId$ = ""
      exit function
   endif

   enviaMensajeConsultaPuntos = -1

end function

function esperaRespueConsultaPuntos
   integer*1 esperaRespueConsultaPuntos

   esperaRespueConsultaPuntos = 0

   !Espera respuesta consulta puntos
   if not esperaPipe( pipeTimeout, "Cons FIL" ) then begin
      call muestraBorrar( "NO LLEGA RESPUESTA  ", "CONSULTA PTOS FIDELI" )
      cliId$ = ""
      exit function
   endif

   esperaRespueConsultaPuntos = -1

end function

function leeRespuestaConsultaPuntos
   integer*1 leeRespuestaConsultaPuntos
   string    cliComunic$,            \
             cliWs$,                 \
             f$                      !

   leeRespuestaConsultaPuntos = 0
   respuestaConsulPtos$ = ""

   respuestaConsulPtos$ = recibeMensaje( "DEL WS FIDEL" )
   if respuestaConsulPtos$ = "" then begin
      call muestraBorrar( "NO SE PUDO LEER LA", "RESPUESTA CONSUL PTO" )
      cliId$ = ""
      exit function
   endif

   cliComunic$ = buscaCampo( respuestaConsulPtos$, separador,  7 )

   if cliComunic$ <> "0" then begin
      call muestraBorrar( "ERROR D COMUNICACION", "CON WS FIDELIDAD MSM" )
      cliId$ = ""
      exit function
   endif

   cliWs$ = buscaCampo( respuestaConsulPtos$, separador,  8 )

   if cliWs$ <> "1" then begin
      call muestraBorrar( "SIN RESPUESTA EN ", "SERVIDOR")
      cliId$ = ""
      exit function
   endif

   f$ = leeBody
   respuestaConsulPtos$ = respuestaConsulPtos$ + separador + f$
   leeRespuestaConsultaPuntos = -1

end function

! Se extrae el valor en USD del texto USD disponibles obtenido en la consulta
! Ej texto = "SUS USD ACUMULADOS SON: 12.22" --> 1222
function extraeUSDDisponible
   integer*1 x, extraeUSDDisponible
   string dato$

   on error goto errLoc

   extraeUSDDisponible = 0

   x = match(":",clivalUSD$,1)
   if x = 0 then begin
      cliUSDDisponible$ = clivalUSD$
   endif else begin
      dato$ = right$( clivalUSD$, len(clivalUSD$) - x )
      dato$ = eliizq$( dato$ )
      cliUSDDisponible$ = dato$
   endif

   extraeUSDDisponible = -1

   salir:
   exit function

   errLoc:
   if errf% = 39 then resume
   call traceCLI( "extraeUSDDisponible ERROR " + err +  hexanum$( errn ) )
   resume salir

end function

function splitRespuestaConsultaPuntos( tipoConsulta )
   integer*1 splitRespuestaConsultaPuntos,  \
             tipoConsulta
   string    cliComunic$,         \
             cliWs$               !

   splitRespuestaConsultaPuntos = 0

   !call traceCLI("splitRespuestaConsultaPuntos=[" + respuestaConsulPtos$ + "]")

   cliValPtos$ = buscaCampo( respuestaConsulPtos$, separador,  9 )
   clivalUSD$  = buscaCampo( respuestaConsulPtos$, separador, 10 )
   clinom$     = buscaCampo( respuestaConsulPtos$, separador, 11 )
   cliDesc$    = buscaCampo( respuestaConsulPtos$, separador, 12 )
   cliMsg1$    = buscaCampo( respuestaConsulPtos$, separador, 13 )
   cliMsg2$    = buscaCampo( respuestaConsulPtos$, separador, 14 )

   if tipoConsulta = 1 then cliIdFidelizado$ = cliId$            !solo fideliza si tipoConsulta es = 1 automatica
   splitRespuestaConsultaPuntos = -1

end function

Function generaTrailer$( tipo )
   integer*1 tipo
   string m$, s$,             \
          generaTrailer$      !

   generaTrailer$ = ""
   s$ = ""
   m$ = date$ + right$(cliHeader$,6)
   m$ = mid$(m$,5,2) + "/" + mid$(m$,3,2) + "/" + left$(m$,2) + " " + \
        mid$(m$,7,2) + ":" + mid$(m$,9,2)                     + " " + \
        right$( "000" + TS.STORE$, 3 )                        + " " + \
        right$( "000" + TS.TERMINAL$, 3 )                     + " "
        
   if tipo = 1 then s$ = right$( "0000" + str$( SL.HD.TRANSNUM ), 4 ) + " "

   m$ = m$ + s$ + str$( val( unpack$( TS.OPER$ ) ) )
   generaTrailer$ = m$

end function

sub imprimeConsultaPuntosAut
   string    trailer$

   if cliIdFidelizado$ = "" then exit sub

   NUOcmdImpresion$ = NUO.AlineaIzq$ + NUO.NegritaA$
   call imprimeCJ("--- PUNTOS RENDIDORES ---")
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprimeCJ("")
   call imprimeCJ(clinom$)
   NUOcmdImpresion$ = NUO.NegritaA$
   if esnumero( cliValPtos$) then begin
      call imprimeCJ("SUS PUNTOS ACUMULADOS son: " + cliValPtos$)
      call imprimeCJ("SUS USD ACUMULADOS son: " + cliValUSD$)
   endif else begin
      call imprimeCJ(cliValPtos$)
      call imprimeCJ(cliValUSD$)
   endif
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprimeCJ("")
   call imprimeCJ(cliMsg1$)
   call imprimeCJ(cliMsg2$)
   call imprimeCJ(string$(38,"-"))
   trailer$ = generaTrailer$( 1 )
   call imprimeCJ(trailer$)
   NUOcmdImpresion$ = NUO.CortaPapel$
   call imprimeCJ("")

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg) 
   endif

end sub

function generaMensajeAcumPuntos
   integer*1 generaMensajeAcumPuntos

   generaMensajeAcumPuntos = 0

   if (TS.LINETYPE = 6) AND (TS.LINEDATA = 1) then begin
      if cliIdFidelizado$ <> "" then begin
         msjAcumulPtos$ = ""
         call generaEncabezado
         cliMPMonto$ = pagosMontosTrx$
         msjAcumulPtos$ = cliHeader$ + separador +          \
            "2" + separador +                               \
            cliDatosTrx$ + separador +                      \
            "FAC" + separador +                             \
            lineaDatosFactura$ + separador +                \
            formateaMonto(val(cliFidelNeto$)) + separador +  \
            formateaMonto(val(cliFidelTotal$)) + separador +  \
            cliMPMonto$                !
         generaMensajeAcumPuntos = -1
      endif else exit function
      generaMensajeAcumPuntos = -1
   endif

end function

function enviaMensajeAcumulPuntos
   integer*1 enviaMensajeAcumulPuntos

   enviaMensajeAcumulPuntos = 0

   !para evitar que existan mensajes antiguos
   call limpiaPipe
   call borraFileBody

   !envia mensaje acumulacion puntos
   if not enviaMensaje( msjAcumulPtos$ ) then begin
      call muestraBorrar( "NO SE PUDO ENVIAR", "MSG ACUMULA PUNTO" )
      exit function
   endif

   enviaMensajeAcumulPuntos = -1

end function

function esperaRespuestaAcumulPuntos
   integer*1 esperaRespuestaAcumulPuntos

   esperaRespuestaAcumulPuntos = 0

   !Espera respuesta acumulacion puntos
   if not esperaPipe( pipeTimeout, "Acumul FIL" ) then begin
      call muestraBorrar( "NO LLEGA RESPUESTA  ", "ACUMULA PTOS FIDELID" )
      exit function
   endif

   esperaRespuestaAcumulPuntos = -1

end function

function leeRespuestaAcumulaPuntos
   integer*1 leeRespuestaAcumulaPuntos

   leeRespuestaAcumulaPuntos = 0

   respuestaAcumulPtos$ = ""

   respuestaAcumulPtos$ = recibeMensaje( "DEL WS FIDEL" )
   if respuestaAcumulPtos$ = "" then begin
      call muestraBorrar( "NO SE PUDO LEER LA", "RESPUESTA ACUMUL PTO" )
      exit function
   endif

   leeRespuestaAcumulaPuntos = -1

end function

function splitRespuestaAcumulaPuntos
   integer*1 splitRespuestaAcumulaPuntos
   string    cliComunic$,         \
             cliWs$               !

   splitRespuestaAcumulaPuntos = 0

   cliComunic$ = buscaCampo( respuestaAcumulPtos$, separador,  7 )

   if cliComunic$ <> "0" then begin
      !call traceCLI("splitRespuestaAcumulaPuntos ERROR DE COMUNICACION=[" + cliComunic$ + "]")
      call muestraBorrar( "ERROR D COMUNICACION", "CON WS FIDELIDAD MSM" )
      exit function
   endif

   cliWs$ = buscaCampo( respuestaAcumulPtos$, separador,  8 )

   if cliWs$ <> "1" then begin
      call muestraBorrar( "ERROR [" + cliWs$ + "] EN WS MSM", "ACUMULA FIDELIDAD")
      exit function
   endif

   cliValPtos$ = buscaCampo( respuestaAcumulPtos$, separador,  9 )
   cliValUSD$  = buscaCampo( respuestaAcumulPtos$, separador, 10 )
   clinom$     = buscaCampo( respuestaAcumulPtos$, separador, 11 )
   cliDesc$    = buscaCampo( respuestaAcumulPtos$, separador, 12 )
   cliMsg1$    = buscaCampo( respuestaAcumulPtos$, separador, 13 )
   cliMsg2$    = buscaCampo( respuestaAcumulPtos$, separador, 14 )

    splitRespuestaAcumulaPuntos = -1

end function

sub imprimeTicketRedencion
    integer*1 x
    string    f$
    if not cliRedencionIntrx then exit sub
    for x = 1 to 2
        call imprimeCJ("          MEGA SANTA MARIA")
        call imprimeCJ(" ")
        call imprimeCJ("- REDENCION PUNTOS RENDIDORES -")
        call imprimeCJ(" ")
        call imprimeCJ("CELUDA/RUC : " + cliIdFidelizado$)
        call imprimeCJ("NOMBRE     : " + clinom$)
        call imprimeCJ(" ")
        f$ = formatea2Dec(cliMontoRedencion)
        call imprimeCJ("VALOR REDIMIDO : " + f$)
        call imprimeCJ(" ")
        call imprimeCJ(" ")
        call imprimeCJ("FIRMA:________________________________")
        call imprimeCJ("TELEFONO:_____________________________")
        call imprimeCJ(" ")
        call imprimeCJ("EL ESTABLECIMIENTO CERTIFICA QUE LA")
        call imprimeCJ("FIRMA DEL CLIENTE ES AUTENTICA")       
        if x = 1 then f$ = "          - ORIGINAL -" \
        else f$ = "          - COPIA -"
        NUOcmdImpresion$ = NUO.NegritaA$
        call imprimeCJ( f$ )
        NUOcmdImpresion$ = NUO.NegritaD$
        f$ = generaTrailer$( 1 )
        call imprimeCJ( f$ )
        call imprimeCJ(" ")
        call imprimeCJ( string$(38,"-") )
        NUOcmdImpresion$ = NUO.CortaPapel$
        call imprimeCJ("")
    next x
end sub

!------------------------------------------------------------------------------
!inicia las variables de cliente nuevo
!------------------------------------------------------------------------------
function iniciaNuevoCliente
   nomCliente = ""
   dirCliente = ""
   telCliente = ""
   codCliente = ""
end function

!------------------------------------------------------------------------------
!inicia las variables de cliente
!------------------------------------------------------------------------------
function iniciaCliente
   tipoCliente = "0"
   idCliente = ""
   call iniciaNuevoCliente
   cadCliente = ""
   regCliente = ""
   pasCliente = ""
   corCliente = ""
end function

!------------------------------------------------------------------------
! Graba registro de pasaporte en el archivo PASAPORT
! retorna el indice al registro grabado
!------------------------------------------------------------------------
function grabaPasaporte( dato$ )
   integer*4 grabaPasaporte
   string    dato$
   integer*4 p
   integer*1 hayError
   string    arc$

   on error goto orror

   arc$ = "R::PASAPORT"
   grabaPasaporte = 0
   hayError = 0

   !abre archivo de pasaportes
   open arc$ direct recl 16 as 85
   if hayError then exit function

   !Obtiene el tamaño del archivo
   p = size( arc$ ) / 16 + 1
   if hayError then goto salir

   !graba registro en archivo de pasaporte
   dato$ = left$( dato$ + string$( 16, " " ), 16 )
   write form "C16"; #85, p; dato$
   if hayError then goto salir

   grabaPasaporte = p

!call traceNUO( "grabaPasaporte dato=[" + dato$ + "] p=[" + str$( p ) + "]" )

   salir:
   close 85
   exit function

   orror:
   if errf% = 39 then resume
   hayError = -1
   call traceNUO( "grabaPasaporte ERROR " + err +  hexanum$( errn ) )
   if err = "ZO" then resume salir
   if err = "RN" then resume salir
   resume

end function

!------------------------------------------------------------------------
! lee registro de pasaporte desde el archivo PASAPORT
! retorna el pasaporte leido sin blancos a la derecha o una cadena vacia
!------------------------------------------------------------------------
function leePasaporte$( p )
   string    leePasaporte$, dato$
   integer*4 p
   integer*1 hayError
   string    arc$

   on error goto orror

   arc$ = "R::$ASAPORT"
   leePasaporte$ = ""
   hayError = 0

   !abre archivo de pasaportes
   open arc$ direct recl 16 as 85 LOCKED
   if hayError then exit function

   !lee registro en archivo de pasaporte
   read form "C16"; #85, p; dato$
   if hayError then goto salir
   leePasaporte$ = elider$( dato$ )

!call traceNUO( "leePasaporte$ dato=[" + dato$ + "] p=[" + str$( p ) + "]"  )

   salir:
   close 85
   exit function

   orror:
   if errf% = 39 then resume
   hayError = -1
   if errn = 1CH then resume
   call traceNUO( "leePasaporte$ ERROR " + err +  hexanum$( errn ) )
   if err = "RN" then resume salir
   resume

end function

!------------------------------------------------------------------------
! Graba registro de correo en el archivo CORREOS
! retorna el puntero al registro grabado
!------------------------------------------------------------------------
function grabaCorreo( dato$ )
   integer*4 grabaCorreo
   string    dato$
   integer*4 p
   integer*1 hayError
   string    arc$

   on error goto orror

   arc$ = "R::CORREOS"
   grabaCorreo = 0
   hayError = 0

   !abre archivo de correos
   open arc$ direct recl 48 as 86
   if hayError then exit function

   !Obtiene el tamaño del archivo
   p = size( arc$ ) / 48 + 1
   if hayError then goto salir

   !graba registro en archivo de correos
   dato$ = left$( dato$ + string$( 48, " " ), 48 )
   write form "C48"; #86, p; dato$
   if hayError then goto salir

   grabaCorreo = p

!call traceNUO( "grabaCorreo dato=[" + dato$ + "] p=[" + str$( p ) + "]" )

   salir:
   close 86
   exit function

   orror:
   if errf% = 39 then resume
   hayError = -1
   call traceNUO( "grabaCorreo ERROR " + err +  hexanum$( errn ) )
   if err = "ZO" then resume salir
   if err = "RN" then resume salir
   resume

end function

!------------------------------------------------------------------------
! lee registro de correo desde el archivo CORREOS
! retorna el correo leido sin blancos a la derecha o una cadena vacia
!------------------------------------------------------------------------
function leeCorreo$( p )
   string    leeCorreo$, dato$
   integer*4 p
   integer*1 hayError
   string    arc$

   on error goto orror

   arc$ = "R::$ORREOS"
   leeCorreo$ = ""
   hayError = 0

   !abre archivo de correos
   open arc$ direct recl 48 as 86
   if hayError then exit function

   !lee archivo de correos
   read form "C48"; #86, p; dato$
   if hayError then goto salir
   leeCorreo$ = elider$( dato$ )

!call traceNUO( "leeCorreo$ dato=[" + dato$ + "] p=[" + str$( p ) + "]"  )

   salir:
   close 86
   exit function

   orror:
   if errf% = 39 then resume
   hayError = -1
   if errn = 1CH then resume
   call traceNUO( "leeCorreo$ ERROR " + err +  hexanum$( errn ) )
   if err = "RN" then resume salir
   resume

end function

!------------------------------------------------------------------------
! Graba registro de cliente en el archivo CLIENTES
!------------------------------------------------------------------------
sub grabaCliente( dato$ )
   string    dato$
   integer*1 hayError
   string    arc$

!call traceNUO( "grabaCliente dato=[" + cadenahex$( dato$ ) + "] largo " + str$( len( dato$ ) ) )

   arc$ = "R::CLIENTES"
   hayError = 0

   !abre archivo de clientes
   open arc$ keyed recl 84 as 84
   if hayError then exit sub

   !graba archivo de clientes
   write form "C84"; #84; dato$
   if hayError then goto salir

   salir:
   close 84
   exit sub

   orror:
   hayError = -1
   call traceNUO( "grabaCliente ERROR " + err +  hexanum$( errn ) )
   if err = "RN" then resume salir
   resume

end sub

!------------------------------------------------------------------------
! Actualiza los archivos PASAPORTE CORREOS y CLIENTES
!------------------------------------------------------------------------
sub registraCliente public
   integer*4 email, pasap

   if len( regCliente ) < 84 then exit sub

   !graba registro de PASAPORTE
   pasap = 0
   if len( pasCliente ) > 0 then begin
      pasap = grabaPasaporte( pasCliente )
   endif

   !graba registro de CORREOS
   email = 0
   if len( corCliente ) > 0 then begin
      email = grabaCorreo( corCliente )
   endif

   !inserta indice de correo en posicion 74 de regCliente
   if email > 0 then begin
      call PUTN4( regCliente, 74, email )
   endif

   !inserta indice de pasaporte en posicion 78 de regCliente
   if pasap > 0 then begin
      call PUTN4( regCliente, 78, pasap )
   endif

   call grabaCliente( regCliente )

end sub

!------------------------------------------------------------------------
! Busca en el archivo CLIENTES la llave
! retorna
!  0 si no existe el cliente
! -1 si existe y carga el nombre en nomCliente, la direccion en,
!    dirCliente, el telefono en telCliente y el id en codCliente
!------------------------------------------------------------------------
function leeCliente( llave$ )
   integer*1 leeCliente
   string    llave$
   string    key$, nom$, dir$, tel$, pas$
   integer*1 band, codSRI
   integer*4 email, pasap
   integer*1 hayError
   string    arc$

   on error goto orror

   arc$ = "R::$LIENTES"
   leeCliente = 0
   hayError = 0
   key$ = pack$( llave$ )

   !abre archivo de clientes
   open arc$ keyed recl 84 as 84 nowrite nodel
   if hayError then exit function

   !lee archivo de clientes
   read form "C7, C30, C30, C5, 2I1 2I4 C2"; #84 key key$; \
      pas$, nom$, dir$, tel$, band, codSRI, email, pasap, pas$
   if hayError then goto salir

   !Descuento empleado
   if band and 4 then call NUOTPROMempleado( right$( llave$, 10 ) )

   !busca el pasaporte
   if left$( llave$, 1 ) = "3" then begin
      pas$ = leePasaporte$( pasap )
      if len( pas$ ) > 0 then idCliente = pas$
   endif

   nomCliente = eliIzq$( nom$ )
   dirCliente = eliIzq$( dir$ )
   telCliente = unpack$( tel$ )
   codCliente = str$( val( right$( llave$, 13 ) ) )
   call seteaClienteFidelidad(llave$, band )
   call seteaClienteRetenciones( llave$ )
   call seteaClienteRecarga( llave$, band )
   call seteaClienteInventario( llave$, band )

   leeCliente = -1

   salir:
   close 84
   exit function

   orror:
   if errf% = 39 then resume
   hayError = -1
   if errn = 1CH then resume
   call traceNUO( "leeCliente ERROR " + err +  hexanum$( errn ) )
   if err = "RN" then resume salir
   resume

end function

!------------------------------------------------------------------------
! Permite ingresar los datos de un nuevo cliente y grabarlo en el
! archivo CLIENTES, CORREOS y PASAPORT si corresponde
! retorna
!  0 se aborta el proceso
! -1 se ingresan datos de cliente
!------------------------------------------------------------------------
function clienteNuevo( tipo, dato$, llave$, m$ )
   integer*1 clienteNuevo, tipo
   string    dato$, llave$, m$
   string    key$, nom$, dir$, tel$, pas$
   integer*1 i

   clienteNuevo = 0

   !uso de la tecla ANULAR para abortar el ingreso de datos
   !BORRAR solo limpia los datos
   call usoTeclaAnular( -1 )

   !reingreso de id hasta 3 veces
   m$ = "RE" + m$

   i = 3
   while i > 0
      if tipo = 3 then begin
         if not (jGuiSubState = 80526) then begin
            jGuiSubState = 80508
         endif
         call javaEvent(terminalSubStateMsg)
         pas$ = ingresoTexto$( m$, "" )
      endif else begin
         if not (jGuiSubState = 80527) then begin
            jGuiSubState = 80501
         endif
         call javaEvent(terminalSubStateMsg)
         pas$ = ingresoDatos$( m$, "" )
      endif
      if left$( pas$, 1 ) = "A" then goto salir
      if left$( pas$, 1 ) = "P" then begin
         pas$ = right$( pas$, len( pas$ ) - 2 )
         if pas$ = dato$ then i = 0
      endif
      if tipo = 3 then begin
         if jGuiSubState = 80508 then begin
            jGuiSubState = 80526
         endif else begin
            jGuiSubState = 0
         endif
      endif else begin
         if jGuiSubState = 80501 then begin
            jGuiSubState = 80527
         endif else begin
            jGuiSubState = 0
         endif
      endif

      i = i - 1
   wend

   if i = 0 then goto salir

   !ingreso de nombre
   ingresoNombre:

   if not (jGuiSubState = 80525) then begin
      jGuiSubState = 80507
   endif
   call javaEvent(terminalSubStateMsg)

   pas$ = ingresoTexto$( "INGRESE NOMBRE", "" )
   if left$( pas$, 1 ) = "A" then goto salir
   if left$( pas$, 1 ) <> "P" then goto salir
   nom$ = right$( pas$, len( pas$ ) - 2 )

   if len( nom$ ) > 30 then begin
      call muestraOperador( "maximo 30", nom$ )
      if jGuiSubState = 80507 then begin
         jGuiSubState = 80525
      endif else begin
         jGuiSubState = 0
      endif
      goto ingresoNombre
   endif

   if len( nom$ ) = 0 then begin
      call muestraOperador( "DEBE INGRESAR EL", "NOMBRE" )
      if jGuiSubState = 80507 then begin
         jGuiSubState = 80525
      endif else begin
         jGuiSubState = 0
      endif
      goto ingresoNombre
   endif

   !ingreso de direccion
   ingresoDireccion:

   if not (jGuiSubState = 80526) then begin
      jGuiSubState = 80508
   endif
   call javaEvent(terminalSubStateMsg)

   pas$ = ingresoTexto$( "INGRESE DIRECCION", "" )
   if left$( pas$, 1 ) = "A" then goto salir
   if left$( pas$, 1 ) <> "P" then goto salir
   dir$ = right$( pas$, len( pas$ ) - 2 )

   if len( dir$ ) > 30 then begin
      call muestraOperador( "maximo 30", dir$ )
      if jGuiSubState = 80508 then begin
         jGuiSubState = 80526
      endif else begin
         jGuiSubState = 0
      endif
      goto ingresoDireccion
   endif

   if len( dir$ ) = 0 then begin
      call muestraOperador( "DEBE INGRESAR LA", "DIRECCION" )
      if jGuiSubState = 80508 then begin
         jGuiSubState = 80526
      endif else begin
         jGuiSubState = 0
      endif
      goto ingresoDireccion
   endif

   !ingreso de telefono
   ingresoTelefono:
   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80501
   endif
   call javaEvent(terminalSubStateMsg)
   pas$ = ingresoDatos$( "INGRESE TELEFONO", "" )
   if left$( pas$, 1 ) = "A" then goto salir
   if left$( pas$, 1 ) <> "P" then goto salir
   tel$ = right$( pas$, len( pas$ ) - 2 )
   if len( tel$ ) > 10 then begin
      call muestraOperador( "maximo 10", tel$ )
      if jGuiSubState = 80501 then begin
         jGuiSubState = 80527
      endif else begin
         jGuiSubState = 0
      endif
      goto ingresoTelefono
   endif
   goto ingresoEmail

   !ingreso de email
   errorEmail:
   call muestraOperador( "email debe tener @ .", "no al inicio o final" )
   if jGuiSubState = 80509 then begin
      jGuiSubState = 80528
   endif else begin
      jGuiSubState = 0
   endif
   ingresoEmail:
   if not (jGuiSubState = 80528) then begin
      jGuiSubState = 80509
   endif
   call javaEvent(terminalSubStateMsg)
   pas$ = ingresoTexto$( "INGRESE EMAIL", "" )
   if left$( pas$, 1 ) = "A" then goto salir
   if left$( pas$, 1 ) <> "P" then goto salir
   pas$ = right$( pas$, len( pas$ ) - 2 )
   i = matchb( ".", pas$, 1 )
   if i = 0 or i = 1 or i = len( pas$ ) then goto errorEmail
   i = matchb( "@", pas$, 1 )
   if i = 0 or i = 1 or i = len( pas$ ) then goto errorEmail

   !Almacena datos para registrar cliente
   corCliente = pas$
   if tipo = 3 then pasCliente = dato$ else pasCliente = ""
   key$ = pack$( llave$ )
   i = val( mid$( "546", tipo, 1 ) )
   regCliente = key$ + \
      left$( nom$ + string$( 30, " " ), 30 ) + \
      left$( dir$ + string$( 30, " " ), 30 ) + \
      pack$( right$( string$( 10, "0" ) + tel$, 10 ) ) + \
      chr$( 0 ) + chr$( i ) + string$( 10, chr$( 0 ) )

   !define cadena de datos de cliente nuevo
   cadCliente = chr$( 99H ) + ":" + \
      chr$( 12H ) + ":" + \
      left$( elider$( dir$ ), 30 ) + ":" + \
      left$( elider$( tel$ ), 10 ) + ":" + \
      left$( elider$( pas$ ), 48 ) + ":" + \
      llave$

   nomCliente = nom$
   dirCliente = dir$
   telCliente = tel$
   codCliente = str$( val( right$( dato$, 13 ) ) )
   clienteNuevo = -1

   salir:
   call usoTeclaAnular( 0 )

end function

!------------------------------------------------------------------------
!Obtiene una llave numerica a partir del pasaporte
!------------------------------------------------------------------------
function llavePasaporte$( dato$ )
   string llavePasaporte$, dato$, paso$, c$
   integer*2 i, c, d

   paso$ = ""
   i = len( dato$ )
   c$ = ""
   while i > 0
      c = asc( mid$( dato$, i, 1 ) )
      if c > 64 then begin
         if len( c$ ) > 0 then begin
            paso$ = "0" + c$ + paso$
            c$ = ""
         endif
         paso$ = str$( c ) + paso$
      endif else begin
         if len( c$ ) > 0 then begin
            paso$ = chr$( c ) + c$ + paso$
            c$ = ""
         endif else begin
            c$ = chr$( c )
         endif
      endif
      i = i - 1
   wend
   if len( c$ ) > 0 then begin
      paso$ = "0" + c$ + paso$
   endif

   llavePasaporte$ = right$( string$( 13, "0" ) + paso$, 13 )

end function

!------------------------------------------------------------------------------
! Retorna datos de cliente
! k = 1 codigo de cliente
! k = 2 nombre del cliente
! k = 3 direccion del cliente
! k = 4 telefono del cliente
! k = 5 idCliente
! k = 6 tipo del cliente o "7" si no se ha identificado
! k = 7 cadena de cliente o "" si no se ha identificado
! k = 8 idCliente o "9999999999999" si no se ha identificado
! k = 9 nombre del cliente o "CONSUMIDOR FINAL" si no se ha identificado
!------------------------------------------------------------------------------
function datosCliente$(k) public
   string datosCliente$
   integer*1 k

   on k goto codi, nomb, dire, tele, idCl, tipo, cadC, idCp, nomp

   sindatos:
   datosCliente$ = ""
   exit function

   codi:
   datosCliente$ = codCliente
   exit function

   nomb:
   datosCliente$ = nomCliente
   exit function

   dire:
   datosCliente$ = dirCliente
   exit function

   tele:
   datosCliente$ = telCliente
   exit function

   idCl:
   datosCliente$ = idCliente
   exit function

   tipo:
   if idCliente = "" then begin
      datosCliente$ = "7"
   endif else begin
      datosCliente$ = tipoCliente
   endif
   exit function

   cadC:
   if idCliente = "" then begin
      datosCliente$ = ""
   endif else begin
      datosCliente$ = cadCliente
   endif
   exit function

   idCp:
   if idCliente = "" then begin
      datosCliente$ = "9999999999999"
   endif else begin
      datosCliente$ = idCliente
   endif
   exit function

   nomp:
   if idCliente = "" then begin
      datosCliente$ = "CONSUMIDOR FINAL"
   endif else begin
      datosCliente$ = nomCliente
   endif
   exit function

end function

!------------------------------------------------------------------------------
! Retorna linea de 24 caracteres con datos de cliente o "_" si no existe
! k = 1 codigo de cliente
! k = 2 nombre del cliente
! k = 3 direccion del cliente
! k = 4 telefono del cliente
!------------------------------------------------------------------------------
function datosClienteCupon$(k) public
   string datosClienteCupon$
   integer*2 k
   string s$

   datosClienteCupon$ = string$(24, "_")

   !if idCliente = "" then exit function

   s$ = string$(24, " ")

   on k gosub idcl, nomb, dire, tele
   exit function

   idcl:
   datosClienteCupon$ = left$(idCliente + s$, 24)
   return

   nomb:
   datosClienteCupon$ = left$(nomCliente + s$, 24)
   return

   dire:
   datosClienteCupon$ = left$(dirCliente + s$, 24)
   return

   tele:
   datosClienteCupon$ = left$(telCliente + s$, 24)
   return

end function

!-------------------------------------------------------------------------
! Asigna cliente
! tipo -> tipo de documento 1=cedula, 2=RUC, 3=pasaporte
! num$ -> numero del documento
! retorna
!  0 -> se asigno el cliente
! -1 -> no se pudo asignar el cliente
!-------------------------------------------------------------------------
function asignaCliente( tipo, num$ ) public
   integer*1 asignaCliente, tipo
   string num$, llave$

   asignaCliente = -1

   if tipo = 1 then begin

      !validar cedula
      if not verificaCedula( num$ ) then exit function
      llave$ = "1000" + num$

   endif else if tipo = 2 then begin

      !validar que sean 13
      if len( num$ ) <> 13 then exit function

      llave$ = str$(tipo) + num$

   endif else if tipo = 3 then begin

      !calcular clave numerica
      llave$ = str$(tipo) + llavePasaporte$(num$)

   endif

   !limpia variable que permite descuento empleado
   call NUOTPROMempleado( "" )

   !buscar cliente en archivo CLIENTES.DAT
   if leeCliente( llave$ ) then begin
      idCliente = num$
      tipoCliente = str$(tipo)
      asignaCliente = 0
      call calcularDescuentos( -1 )
   endif

end function

function pideCedulaCliente
   integer*1 pideCedulaCliente
   string dato$

   pideCedulaCliente = 0
   jGuiSubState = 80503
   call javaEvent(terminalSubStateMsg)

   dato$ = ingresoDatos$( "CEDULA DEL AFILIADO","" )
   if left$( dato$, 1 ) = "A" then goto saliringreso
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )

   !validar cedula
   if not verificaCedula( dato$ ) then goto erroringreso

   cliId$ = dato$
   pideCedulaCliente = -1

   saliringreso:
   call ignoraTeclas
   exit function

   erroringreso:
   call muestraBorrar( "ERROR EN INGRESO", "CEDULA FIDELIDAD" )
   goto saliringreso

end function

function imprimeConsultaPuntosMan
   string f$

   call imprimeCJ(cliValPtos$)
   call imprimeCJ(clivalUSD$)
   call imprimeCJ(clinom$)
   call generaEncabezado
   f$ = generaTrailer$( 2 )
   call imprimeCJ( f$ )
   call imprimeCJ(string$(38," "))
   call imprimeCJ(string$(38," "))
   call imprimeCJ(string$(38," "))
   NUOcmdImpresion$ = NUO.CortaPapel$
   call imprimeCJ("")

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg) 
   endif

end function

function pideMontoRedencion
   integer*1 pideMontoRedencion
   string dato$, f$,ff$

   pideMontoRedencion = 0

   if TS.IO.KEYS(1) = 70 then begin
      pideMontoRedencion = -1
      exit function
   endif
   
   otrodato:

   cliMontoRedencion = 0

   jGuiSubState = 80537
   call javaEvent(terminalSubStateMsg)

   dato$ = ingresoDatos$( "CUPO UD " + cliUSDDisponible$,"VALOR A REDIMIR" )
   if left$( dato$, 1 ) = "A" then exit function
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )

   if not esNumero( dato$ ) then goto otrodato
   f$ = valorCadena$( cliUSDDisponible$ )
   ff$ = str$(val(cliUSDDisponible$) * 100.0 )

   if val( ff$ ) < val(dato$) then begin
      call muestraBorrar("MONTO SUPERA CUPO RE","DENCION")
      goto otrodato
   endif

   if TS.BALDUE(0) < val(dato$) then begin
      call muestraBorrar("MONTO SUPERA LA VTA.","")
      goto otrodato
   endif

   cliMontoRedencion = val(dato$)
   if cliMontoRedencion = 0 then goto otrodato
   pideMontoRedencion = -1

salir:
   exit function

erroringreso:
   call muestraBorrar( "ERROR EN INGRESO", "MONTO REDENCION" )
   goto salir

end function

function validaCondicionesRedencion
   integer*1 validaCondicionesRedencion

   validaCondicionesRedencion = 0

   ! impide la redencion si no existe un cliente fidelizado en la trx
   if cliIdFidelizado$ = "" then begin
      call muestraBorrar("CLIENTE NO HA SIDO","CAPTURADO/Borrar")
      goto salir
   endif

   ! fuerza que este totalizado antes de realizar la redencion de puntos
   if not TS.BAL.TAKEN then begin
      TS.GUIDANCE = 1020
      TS.IO.MOTORKEY = 0
      exit function
   endif

   ! controla que la redencion de puntos sea el primer medio de pago
   if ( TS.TENDERED(0) <> 0 ) and not cliRedencionIntrx then begin
      call muestraBorrar("REDENCION DEBE SER","EL PRIMER MEDIO PAGO")
      goto salir
   endif

   ! controla que exista una redencion en la transaccion para permitir anularla
   if not cliRedencionIntrx and ( TS.IO.KEYS(1) = 70 )then begin
      call muestraBorrar("NO EXISTE REDENCION","PARA ANULAR")
      goto salir
   endif

   ! Impide una segunda redencion en la TRX
   if cliRedencionIntrx and ( TS.IO.KEYS(1) <> 70 ) then begin
      call muestraBorrar("YA EXISTE REDENCION","EN LA TRX")
      goto salir
   endif

   if TS.IO.KEYS(1) <> 70 then begin

      ! valida extraccion de USD disponible de respuesta consulta para redencion
      if not extraeUSDDisponible then begin
         call muestraBorrar("ERROR USD DISPONIBLE","")
         goto salir
      endif

      ! Impide remir puntos si el cliente posee USD en cero
      if cliUSDDisponible$ = "0" then begin
         call muestraBorrar("CLIENTE SIN CUPO","PARA REDIMIR")
         goto salir
      endif
      cliSentido = 4              ! redencion de puntos
   endif else begin
      cliSentido = 5              ! anulacion de redencion de puntos
   endif

   validaCondicionesRedencion = -1
salir:
end function

function generaMensajeRedencionPuntos
   integer*1 generaMensajeRedencionPuntos
   string    f$, dato$

   generaMensajeRedencionPuntos = 0

   !cliSentido = 4                                 ! Redencion de puntos
   !if TS.IO.KEYS(1) = 70 then cliSentido = 5      ! Anulacion de redencion de puntos 

   msjRedencionPtos$ = ""
   call generaEncabezado
   dato$ =  right$( "000" + TS.STORE$, 3 ) + "-" + TS.TERMINAL$ + "-" + right$( string$( 9, "0" ) + str$( MSMTFACTnumeroFactura + 1 ), 9 )

   f$ = formateaMonto(cliMontoRedencion)
   msjRedencionPtos$ = cliHeader$              + separador + \
                       str$(cliSentido)        + separador + \
                       cliIdFidelizado$        + separador + \
                       dato$                   + separador + \
                       f$

   generaMensajeRedencionPuntos = -1

end function

function enviaMensajeRedencionPuntos
   integer*1 enviaMensajeRedencionPuntos

   enviaMensajeRedencionPuntos = 0

   !envia mensaje Redencion de puntos
   if not enviaMensaje( msjRedencionPtos$ ) then begin
      call muestraBorrar( "NO SE PUDO ENVIAR", "MSG REDENCION PUNTOS" )
      exit function
   endif

call traceCLI("enviaMensajeRedencionPuntos [" + msjRedencionPtos$ + "]")

   enviaMensajeRedencionPuntos = -1

end function

function esperaRespueRedencionPuntos
   integer*1 esperaRespueRedencionPuntos

   esperaRespueRedencionPuntos = 0

   !Espera respuesta redencion de puntos
   if not esperaPipe( pipeTimeout, "Reden PTO" ) then begin
      call muestraBorrar( "NO LLEGA RESPUESTA  ", "REDENCION DE PUNTOS" )
      exit function
   endif

   esperaRespueRedencionPuntos = -1

end function

function leeRespuestaRedencionPuntos
   integer*1 leeRespuestaRedencionPuntos
   string    cliComunic$,            \
             cliWs$,                 \
             f$                      !

   leeRespuestaRedencionPuntos = 0
   respuestaRedencionPtos$ = ""

   respuestaRedencionPtos$ = recibeMensaje( "DEL WS FIDEL" )
   if respuestaRedencionPtos$ = "" then begin
      call muestraBorrar( "NO SE PUDO LEER LA", "RESPUESTA REDENC PTO" )
      exit function
   endif

!call traceCLI("leeRespuestaRedencionPuntos [" + respuestaRedencionPtos$ + "]")

   cliComunic$ = buscaCampo( respuestaRedencionPtos$, separador,  7 )

   if cliComunic$ <> "0" then begin
      call muestraBorrar( "ERROR D COMUNICACION", "CON WS FIDELIDAD MSM" )
      exit function
   endif

   cliWs$ = buscaCampo( respuestaRedencionPtos$, separador,  8 )

   if cliWs$ <> "1" then begin
      call muestraBorrar( "SIN RESPUESTA EN ", "SERVIDOR")
      exit function
   endif

   f$ = leeBody
   respuestaRedencionPtos$ = respuestaRedencionPtos$ + separador + f$

   leeRespuestaRedencionPuntos = -1

end function

Function splitRespuestaRedencionPuntos
   integer*1 splitRespuestaRedencionPuntos

   splitRespuestaRedencionPuntos = 0

   !call traceCLI("splitRespuestaRedencionPuntos [" + respuestaRedencionPtos$ + "]")

   cliValPtos$ = buscaCampo( respuestaRedencionPtos$, separador,  9 )
   clivalUSD$  = buscaCampo( respuestaRedencionPtos$, separador, 10 )

   splitRespuestaRedencionPuntos = -1

end function

sub inyectaPagoRedencionPuntos

   TS.IO.MOTORKEY = 95
   TS.IO.KEYS(3)  = 78
   TS.IO.DATA$(3) = str$(2)
   TS.IO.KEYS(7)  = 95
   TS.IO.DATA$(7) = str$(cliMontoRedencion)
   TS.IO.DATA$(9) = cliIdFidelizado$
   if TS.IO.KEYS(1) = 70 then cliRedencionIntrx = 0 \
   else cliRedencionIntrx = -1

!call traceCLI("inyectaPagoRedencionPuntos TS.IO.KEYS(1)=[" + str$(TS.IO.KEYS(1)) + "]")
!call traceCLI("inyectaPagoRedencionPuntos cliRedencionIntrx=[" + str$(cliRedencionIntrx) + "]")

end sub

function secuenciaTeclaCliente public
   integer*1 secuenciaTeclaCliente

   secuenciaTeclaCliente = 0

   ! Ingreso Id de cliente mediante (tecla RUC)
   if TS.IO.STATE = 10 and TS.IO.MOTORKEY = 252 and TS.IO.KEYS(2) = 252 then begin
      secuenciaTeclaCliente = -1
   endif

end function

!------------------------------------------------------------------------
!Al finalizar una transaccion de venta
!incorporar en la USER 02
!------------------------------------------------------------------------
sub MSMTCLIE02 PUBLIC
   string cad$

   if not estado then exit sub
   call imprimeConsultaPuntosAut
   call imprimeTicketRedencion

   cliIdType$     = ""                          !
   cliId$         = ""                          !
   cliIndic       = 0                           !
   cliIdFidelizado$ = ""                        ! Id de cliente ya fidelizado en la transaccion

   clinom$        = ""                          !
   cliValPtos$    = ""                          !
   clivalUSD$     = ""                          !
   cliMsg1$       = ""                          !
   cliMsg2$       = ""                          !

   cliFidelNeto$  = ""                          !
   cliFidelTotal$ = ""                          !

   cliRedencionIntrx = 0                        !
   cliMontoRedencion = 0                        !
   cliUSDDisponible$ = ""                       !
   cliEsEmpleado     = 0                        ! Indicador -1=es empleado  0=no es Empleado
   cliSentido        = 0                        ! 4=Redencion de puntos  5=Anulacion redencion de puntos

   call iniciaCliente

end sub

!------------------------------------------------------------------------
!Durante lectura de Parametros
!incorporar en la USER 07
!------------------------------------------------------------------------
sub MSMTCLIE07 PUBLIC
   string    r
   integer*1 k
   string    v$
   integer*4 inicioBloque

   fechaModificacion = "  2 Dic 2022"

   separador = "'|'"
   lf$ = chr$(10)
   pipeTimeout = 5000
   estado = 0
   depurar = 0
   dummy = 0
   cliOperacion = 69
   bodyFile$ = "R::C:/CNB/FIDE" + TS.TERMINAL$ + ".DAT"
   cliIdFidelizado$ = ""
   cliCadenaAnulaRedencion$ = ""

   call imprimeCJ( "***** MODULO CLIENTE Y FIDELIDAD *****" )

   inicioBloque = abreParametros( "R::$ARGENER", "[CLIENTE FIDELIDAD]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE FIDELIDAD", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "FIDELIDAD NO CONFIGURADO" + fechaModificacion )
      call cierraParametros( 80 )
      exit sub
   endif

   !Proyecto activo 0=No  1=Si
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

         !numero de tecla motora para clientes
         v$ = leeParametro$( "TECLA_CLIENTE", 80, inicioBloque )
         if esNumero( v$ ) then clienteTeclaMotora = val( v$ )

         !medio de pago SA para la redencion de puntos
         v$ = leeParametro$( "MEDIO_PAGO_REDENCION", 80, inicioBloque )
         if esNumero( v$ ) then cliMpRedencion = val( v$ )

         !numero tecla para la redencion de puntos
         v$ = leeParametro$( "TECLA_REDENCION", 80, inicioBloque )
         if esNumero( v$ ) then cliTeclaRedencion = val( v$ )
      endif
   endif

   call cierraParametros( 80 )

   if estado then r = "ON " else r = "OFF"
   call imprimeCJ( "ESTADO " + r + fechaModificacion )
   if depurar then begin
      call imprimeCJ("TIEMPO DE ESPERA" + right$( string$( 10, " " ) + str$( pipeTimeout  ), 10 ) )
      call imprimeCJ("TECLA CLIENTE   " + right$( string$( 10, " " ) + str$( clienteTeclaMotora  ), 10 ) )
      call imprimeCJ("MEDIO PAGO CANJE" + right$( string$( 10, " " ) + str$( cliMpRedencion ), 10 ) )
      call imprimeCJ("TECLA REDENCION " + right$( string$( 10, " " ) + str$( cliTeclaRedencion ), 10 ) )
      call imprimeCJ( "ACTIVADA DEPURACION" )
      if dummy then call imprimeCJ( "ACTIVADA DUMMY" )
   endif

   !Inicia variables de cliente
   call iniciaCliente

end sub

!------------------------------------------------------------------------
!Captura tecla motora de cliente
!Durante lectura del IOPROCE
!incorporar en la USER 14
!------------------------------------------------------------------------
sub MSMTCLIE14 public
   string    dato$, llave$, s$, IdclienteOld$
   integer*1 tipo, f
!   integer*2 k
!   integer*4 FAbase0, FAbase1, FAbase2

   ! Ingreso Id de cliente (tecla RUC)
   if secuenciaTeclaCliente then begin

     !No se permite utilizar tecla RUC estando el POS en modo autonomo
     if TS.STANDALONE then begin
         call muestraBorrar("NO PERMITIDO RUC","EN MODO AUTONOMO")
         goto salir
      endif

      if NUOTDIVA.monto > 0 then begin
         call muestraBorrar( "NO PERMITIDO", "DEVOLUCION DE IVA" )
         goto salir
      endif

      if requiereCambioCaja then exit sub

      if TS.TENDERED(0) <> 0 then begin
         call muestraBorrar( "NO PERMITIDO DESPUES", "DE UN PAGO" )
         goto salir
      endif

      ! Activa el calculo de descuentos
      call calcularDescuentos( -1 )

      IdclienteOld$ = ""
      s$ = datosCliente$(5)
      if s$ <> "" then IdclienteOld$ = s$
      call iniciaCliente

      !limpia indicador de beneficiario devolucion de IVA
      call NUOTDIVAanulaBeneficiario

      call seteaClienteFidelidad("", 0 )

      !limpia variable que permite descuento empleado
      call NUOTPROMempleado( "" )

      !Solicita tipo de documento a ingresar (1, 2 o 3)
      if TS.IO.DATA$(2) = "" then begin

         if java.init = -1 then begin
            jGuiSubState = 80550
         call javaEvent(terminalSubStateMsg)
            dato$ = ingresoDatos$( "ELIJA UNA OPCION:","" )
         endif else begin
            dato$ = ingresoDatos$( "TIPO ID     1=CEDULA", "2=RUC    3=PASAPORTE" )
         endif
         if left$( dato$, 1 ) = "A" then begin
            if IdclienteOld$ <> "" then begin
               call seteaClienteRetenciones( "" )
               call ignoraTeclas
               !forzar un total
               TS.BAL.TAKEN = 0
               TS.GUIDANCE = 1020
               TS.IO.MOTORKEY = 0
               exit sub
            endif
            goto salir
         endif
         if left$( dato$, 1 ) <> "P" then goto erroringreso
         TS.IO.DATA$(2) = right$( dato$, len( dato$ ) - 2 )
         if not esNumero( TS.IO.DATA$(2) ) then goto salir
      endif

      if java.init = -1 then begin
         jGuiSubState = 80670
         call javaEvent(terminalSubStateMsg)
      endif

      s$ = "INGRESE "
      tipo = val( TS.IO.DATA$(2) )
      if tipo = 1 then begin

         s$ = s$ + "CEDULA"

         jGuiSubState = 80501
         call javaEvent(terminalSubStateMsg)

         dato$ = ingresoDatos$( s$, "" )
         if left$( dato$, 1 ) = "A" then goto salir
         if left$( dato$, 1 ) <> "P" then goto erroringreso
         dato$ = right$( dato$, len( dato$ ) - 2 )

         !validar cedula
         if not verificaCedula( dato$ ) then goto erroringreso

         llave$ = "1000" + dato$

      endif else if tipo = 2 then begin

         s$ = s$ + "RUC"
         jGuiSubState = 80501
         call javaEvent(terminalSubStateMsg)
         dato$ = ingresoDatos$( s$, "" )

         if left$( dato$, 1 ) = "A" then begin
            call seteaClienteRetenciones( "" )
            goto salir
         endif

         if left$( dato$, 1 ) <> "P" then goto erroringreso
         dato$ = right$( dato$, len( dato$ ) - 2 )

         !validar que sean 13
         if len( dato$ ) <> 13 then goto erroringreso
         if right$( dato$, 3 ) <> "001" then goto erroringreso

         llave$ = str$(tipo) + dato$

      endif else if tipo = 3 then begin

         s$ = s$ + "PASAPORTE"
         jGuiSubState = 80508
         call javaEvent(terminalSubStateMsg)
         dato$ = ingresoTexto$( s$, "" )
         if left$( dato$, 1 ) = "A" then goto salir
         if left$( dato$, 1 ) <> "P" then goto erroringreso
         dato$ = right$( dato$, len( dato$ ) - 2 )

         !calcular clave numerica
         llave$ = str$(tipo) + llavePasaporte$(dato$)

      endif else begin

         call muestraBorrar( "TIPO DE ID CLIENTE", "NO PERMITIDO" )

         goto salir

      endif

      !buscar cliente en archivo CLIENTES.DAT
      if leeCliente( llave$ ) then goto ingresado

      !Si quiere crear el cliente

      jGuiSubState = 80505
      call javaEvent(terminalSubStateMsg)
      f = pregunta( "CLIENTE NO EXISTE LO", "CREA PLU=SI BORRA=NO" )

      !Ingreso de datos de cliente nuevo
      if f then begin
         if not clienteNuevo( tipo, dato$, llave$, s$ ) then begin
            call muestraOperador( "CLIENTE NO", "INGRESADO" )
            wait; 5000
            goto salir
         endif
         if java.init then begin
            jGuiSubState = 0
         endif
      endif else goto salir

      ingresado:
      idCliente = dato$
      tipoCliente = str$( tipo )
      call muestraOperador( mid$( nomCliente, 1, 20), mid$( nomCliente, 21, 20 ) )
      wait; 3000
      call restauraDisplay

      if tipo = 1 then begin

         !Consulta si es beneficiario de devolucion de IVA
         call NUOTDIVA.consulta( idCliente )

         !Consulta de Informacion Electoral
         call NUOTELEC.consulta( idCliente )

      endif

      if JAVA.INIT then begin
         if TS.INTRX then begin
            !forzar un total
            call ignoraTeclas
            TS.BAL.TAKEN = 0
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            jGuiSubState = 80671
            call javaEvent(terminalSubStateMsg)
            exit sub
         endif else begin
            call ignoraTeclas
            exit sub
         endif
      endif else begin
         !forzar un total
         call ignoraTeclas
         TS.BAL.TAKEN = 0
         TS.GUIDANCE = 1020
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      erroringreso:
      call muestraBorrar( "ERROR EN INGRESO", "DE ID CLIENTE" )
      goto salir

   endif

   !Impedir que ingrese por teclado la forma de pago redencion puntos (2 / monto OTROS) 
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 95 ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "2" ) and \
      ( TS.IO.KEYS(7) = 95 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   ! si es ANULA TOTAL o SUSPENCION DE TRX se inhibe el cliente ingresado
   if secuenciaAnulaTotal then cliIdFidelizado$ = ""
   if secuenciaSuspendeTransaccion then cliIdFidelizado$ = ""

   if not estado then exit sub

   if EsSecuenciaRedencionPuntosCLI or \
      EsSecuenciaAnulaRedencionCLI then begin
      if java.init = -1 then begin
         if NOT TS.BAL.TAKEN then begin
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            exit sub
         endif
      endif
      if not validaCondicionesRedencion    then goto salir
      if not pideMontoRedencion            then goto salir
      if not generaMensajeRedencionPuntos  then goto salir
      if not enviaMensajeRedencionPuntos   then goto salir           ! En caso de error en envio de mensaje
      if not esperaRespueRedencionPuntos   then goto salir           ! En caso de error en espera de respuesta
      if not leeRespuestaRedencionPuntos   then goto salir           ! En caso de error en leer respuesta consulta
      if not splitRespuestaRedencionPuntos then goto salir           ! En caso de error split sin fidelizacion
      call inyectaPagoRedencionPuntos
      !cliRedencionIntrx = -1
      exit sub
   endif

   if EsSecuenciaConsultaPuntosCLI then begin
      if not pideCedulaCliente then exit sub
      if not generaMensajeConsultaPuntos     then goto salir
      if not enviaMensajeConsultaPuntos      then goto salir           ! En caso de error en envio de mensaje
      if not esperaRespueConsultaPuntos      then goto salir           ! En caso de error en espera de respuesta
      if not leeRespuestaConsultaPuntos      then goto salir           ! En caso de error en leer respuesta consulta
      if not splitRespuestaConsultaPuntos(2) then goto salir           ! En caso de error split sin fidelizacion
      if not imprimeConsultaPuntosMan        then goto salir           ! En caso de error imprime consulta manual fidelizacion
      goto salir
   endif

   if cliId$ = "" then exit sub
   if ( cliIndic and 2 ) = 0 then exit sub

   if cliIdFidelizado$ = cliId$           then exit sub             ! Para no consultar nuevamente un mismo cliente
   if not generaMensajeConsultaPuntos     then exit sub             ! En caso de error en la generacion del mensaje
   if not enviaMensajeConsultaPuntos      then exit sub             ! En caso de error en envio de mensaje
   if not esperaRespueConsultaPuntos      then exit sub             ! En caso de error en espera de respuesta
   if not leeRespuestaConsultaPuntos      then exit sub             ! En caso de error en leer respuesta consulta
   if not splitRespuestaConsultaPuntos(1) then exit sub             ! En caso de error split con fidelizacion
   cliId$ = ""
   exit sub
salir:
   call ignoraTeclas
   cliId$ = ""
end sub

!------------------------------------------------------------------------
!Durante la impresion CR
!incorporar en la USER 20
!------------------------------------------------------------------------
sub MSMTCLIE20 PUBLIC
   if not estado then exit sub
   if not generaMensajeAcumPuntos     then exit sub              ! En caso de error en la generacion del mensaje
   if not enviaMensajeAcumulPuntos    then exit sub              ! En caso de error en envio de mensaje

end sub

!------------------------------------------------------------------------
!Durante el proceso de calculo del saldo
!incorporar en la USER 30
!------------------------------------------------------------------------
sub MSMTCLIE30 PUBLIC
   integer*4 t2

   if not estado then exit sub

   if ( cliIdFidelizado$ <> "" ) and ( TS.BALDUE(0) <= 0 ) then begin
      t2 = baseTarifa(2)
      cliFidelNeto$  = str$( (TS.TENDERED(0) + TS.BALDUE(0))  - t2 )
      cliFidelTotal$ = str$( TS.TENDERED(0) + TS.BALDUE(0) )

!call traceCLI("MSMTCLIE30 TS.TENDERED(0) =[" + str$(TS.TENDERED(0)) + "] t2=["	+ str$(t2) + "]")
!call traceCLI("MSMTCLIE30 TS.BALDUE(0)   =[" + str$(TS.BALDUE(0))  + "]")
!call traceCLI("MSMTCLIE30 cliFidelNeto$  =[" + cliFidelNeto$  + "]")
!call traceCLI("MSMTCLIE30 cliFidelTotal$ =[" + cliFidelTotal$ + "]")

   endif

end sub

sub MSMTCLIE53 public

   if not estado then exit sub

   !Si existe un pago con redencion se envia la anulacion al server
   if cliCadenaAnulaRedencion$ <> "" then begin

!call traceCLI("MSMTCLIE53 HAY REDENCION QUE ENVIAR ANULAR")

      cliMontoRedencion = val(getCampoTSL$(cliCadenaAnulaRedencion$,3))
      cliSentido        = 5
      cliIdFidelizado$  = getCampoTSL$(cliCadenaAnulaRedencion$,5)

!call traceCLI("MSMTCLIE53 cliMontoRedencion=[" + str$(cliMontoRedencion) + "] cliIdFidelizado$=[" + cliIdFidelizado$ + "]")

      call generaMensajeRedencionPuntos

!call traceCLI("MSMTCLIE53 SE GENERO MENSAGE ANULACION DE REDENCION")
!call traceCLI("MSMTCLIE53 msjRedencionPtos$=[" + msjRedencionPtos$ + "]")

      call enviaMensajeRedencionPuntos

!call traceCLI("MSMTCLIE53 SE ENVIO MENSAGE ANULACION DE REDENCION")

      call esperaRespueRedencionPuntos

!call traceCLI("MSMTCLIE53 SE ESPERO RESPUESTA MENSAGE ANULACION DE REDENCION")

      call leeRespuestaRedencionPuntos

!call traceCLI("MSMTCLIE53 SE LEE RESPUESTA A MENSAGE ANULACION DE REDENCION")	  

      cliIdFidelizado$         = ""
      cliSentido               = 0
      cliMontoRedencion        = 0
      cliCadenaAnulaRedencion$ = ""
   endif

end sub

sub MSMTCLIE68(ENTRY$, ELEMENT) public
   integer*2 ELEMENT

   string ENTRY$,  \
          dato$,   \
          campo1$, \
          campo2$  !

   if not estado then exit sub
   dato$ = ENTRY$
   if TS.RECOVERY then begin
      campo1$ = getCampoTSL$(dato$,1)
      if ( campo1$ = "05" ) or ( campo1$ = "06" ) then begin

!call traceCLI("MSMTCLIE68 campo1$ =[" + campo1$ + "]")

         campo2$ = getCampoTSL$(dato$,2)
         if campo2$ = str$(cliMpRedencion) then begin
            if campo1$ = "05" then begin
               cliCadenaAnulaRedencion$ = ENTRY$
            endif else begin
               cliCadenaAnulaRedencion$ = ""
            endif

!call traceCLI("MSMTCLIE68 cliCadenaAnulaRedencion$ =[" + unpack$(cliCadenaAnulaRedencion$) + "]")

         endif
      endif
   endif

end sub
