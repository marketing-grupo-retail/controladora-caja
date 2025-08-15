!-----------------------------------------------------------------------------------------
! Modulo toma inventario en POS.
! Fecha: 14 de Febrero de 2022.
! Autor: Carlos Calderon - NUOCORP.
! History: 17-03-2022 Carlos Calderon se implementa funcionalida
!          22-04-2022 Carlos Calderon se implementa control que impiden uso de secuencias:
!                     77-64-65 Operador operaciones CNB
!-----------------------------------------------------------------------------------------
%ENVIRON T

integer*1 activo,                \ Indicador de proyecto activo
          depurar,               \ Indicador de depuracion
          Dummy,                 \ Indicador de autorespuesta activo
          INVtipoCadena99%,      \ Cadena de inventario en TSL
          INVtipoCadenaNoExiste99%, \ Cadena de inventario en TSL articulo no existe
          INVenModalidad%,       \ Indicador esta en modalidad inventario -1=Si 0=No
          INVnumSecuencia%,      \ Numero de operacion ?? conex para activar inventario
          INVindicCliente%,      \ Tipo de cliente ingresado
          INVexigeCantidad%,     \ Indicador exige tecla cantidad -1=si 0=No
          INVnewSeccion%,        \ Indicador para nueva seccion a inventariar
          INVlinknewSeccion%,    \
          INVEtrxAnulada%,       \
          INVitemNoEncontrado%   !

integer*2 INVcantiMaximaPluUNIT% ! Cantidad Maxima de unidades ingresadas para productos unitario
integer*4 INVcantiMaximaPluPESA% ! Cantidad Maxima de unidades ingresadas para productos pesables
                   
string    separador,             \
          fechaModificacion$,    \
          INVcadena99$,          \
          INVprefijoVisor$,      \
          INVidPicking$,         \ Cedula funcionario que realiza la toma del inventario
          INVidSeccion$,         \ Identificacion seccion a inventariar
          INVidCliente$,         \ Id cliente
          msg1$, msg2$           !

!-------------------------------------------------------------------------------
! arreglos para almacenar el resumen de productos ingresados en la transaccion
!-------------------------------------------------------------------------------
%INCLUDE RESPROD.J86

!------------------------------------------------------------------------------
! Variables globales de otros modulos
!------------------------------------------------------------------------------
%INCLUDE EAMTSWKG.J86                     ! supermarket appl wking storage
%INCLUDE EAMTRANS.J86                     ! global defines for tlog vars
%INCLUDE EAMITEMR.J86                     ! global defines for item record vars
integer*2 global SL.END

!------------------------------------------------------------------------------
! Rutinas publicas de otros modulos
!------------------------------------------------------------------------------
%INCLUDE JAVAGUIV.J86
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE NUOTCNB.J86
%INCLUDE MSMTCLIE.J86
%INCLUDE NUOTPROM.J86
%INCLUDE EAMASMCT.J86
%INCLUDE JAVAGUIC.J86

!------------------------------------------------------------------------
!Imprime detalle de articulos de inventario
!------------------------------------------------------------------------
sub resumenInventario( seccINV$, cedulaINV$, tipoResu% )
   integer*1 tipo%,    \
             tipoResu% !

   integer*2 f,            \
             artUNIT,      \
             artPESA       !

   integer*4 cant%,        \
             acumArtUNIT%, \
             acumArtPESA%  !

   string seccINV$,        \
          cedulaINV$,      \
          Codi$,           \
          nomb$,           \
          dato$            !

   on error goto errLoc

   !retorna la cantidad de articulos ingresados
   if ( seccINV$ = "" ) and ( cedulaINV$ = "" ) then begin
      seccINV$ = str$(resProdNum)
      exit sub
   endif

!call traceNUO( "MSMTFACT resumenInventario seccion =[" + seccINV$ + "] CE=[" + cedulaINV$ + "] Ind=[" + str$(resProdNum) + "] tipoResu%=["+str$(tipoResu%) + "]")

   NUOcmdImpresion$ = NUO.Centrado$
   NUOcmdImpresion$ = NUO.LetraJunta$
   call imprimeCJ( string$(38,"-") )
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ( "          ** INVENTARIO **           ")
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprimeCJ( string$(38,"-") )
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ("             SECCION : " + right$("               " + seccINV$,15 ) )
   call imprimeCJ("           CEDULA ID : " + right$("               " + cedulaINV$,15 ) )
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprimeCJ( string$(38,"-") )

   acumArtUNIT% = 0
   acumArtPESA% = 0
   artUNIT      = 0
   artPESA      = 0

   for f = 0 to resProdNum - 1
      codi$ = unpack$(resProdCod$(f)) ! codigo de producto PD 6
      nomb$ = resProdNom$(f)          ! nombre del producto ASC 18
      cant% = resProdCan%(f)          ! cantidad de productos (si se vende por peso 3 decimales)
      tipo% = resProdTip%(f)          ! tipo de producto 0=normal 1= pesable 2=canasta
      dato$ = right$("       " + formateaMiles(str$(cant%)),7) + \  7
              " "                                              + \  1
              right$("            " + str$(val(Codi$)),12) + \ 12
              " "                                              + \  1
              left$(Nomb$,17)                                    ! 17
      call imprimeCJ( dato$ )

      if ( tipo% = 0 ) or ( tipo% = 2 ) then begin
         artUNIT =  artUNIT + 1
         acumArtUNIT% = acumArtUNIT% + Cant%
      endif
      if ( tipo% = 1 ) then begin
         artPESA = artPESA + 1
         acumArtPESA% = acumArtPESA% + Cant%
      endif
   next f
   call imprimeCJ( string$(38," ") )
   if tipoResu% = 2 then begin
      NUOcmdImpresion$ = NUO.NegritaA$
      call imprimeCJ( "**** INVENTARIO ANULADO *****" )
      NUOcmdImpresion$ = NUO.NegritaD$
      call imprimeCJ( string$(38," ") )
   endif

   call imprimeCJ( "ITEMS UNITARIOS      " + right$(string$(13," ") + formateaMiles(str$(artUNIT)),13) )
   call imprimeCJ( "CANT. ITEMS UNITARIOS" + right$(string$(13," ") + formateaMiles(str$(acumArtUNIT%)),13) )
   call imprimeCJ( "ITEMS PESABLES       " + right$(string$(13," ") + formateaMiles(str$(artPESA)),13) )
   call imprimeCJ( "CANT. ITEMS PESABLES " + right$(string$(13," ") + formateaMiles(str$(acumArtPESA%)),13) )

   sale:
   exit sub

   errLoc:
   if errf% = 39 then resume
   call traceNUO( "MSMTFACT resumenInventario " + err + hexanum$( errn ) )
   resume sale
end sub

sub limpiaFactura external
end sub

function EsSecuenciaConsultaPuntosCLI external              ! Modulo de fidelifdad MSMTCLIE.BAS
   integer*1 EsSecuenciaConsultaPuntosCLI
end function

function MSMTGIFTsecuenciaTeclaTarjetaRegalo external
   integer*1 MSMTGIFTsecuenciaTeclaTarjetaRegalo
end function

function MSMTINSTsecuenciaTeclaCreditoInstitucional
   integer*1 MSMTINSTsecuenciaTeclaCreditoInstitucional
   
   MSMTINSTsecuenciaTeclaCreditoInstitucional = 0
   if (TS.IO.MOTORKEY = 98) and (TS.IO.KEYS(2) = 98) and (TS.IO.STATE = 10) then MSMTINSTsecuenciaTeclaCreditoInstitucional = -1
end function

function secuenciaTeclaOperadorNoPermitida
   integer*1 secuenciaTeclaOperadorNoPermitida

   secuenciaTeclaOperadorNoPermitida = 0
   if (TS.IO.KEYS(5) = 61) and (TS.IO.MOTORKEY = 61) and (TS.IO.STATE = 10) then begin
      if ( TS.IO.DATA$(5) = "60" ) or   \ Fin del dia del pinpad
         ( TS.IO.DATA$(5) = "61" ) or   \ Configuracion de pinpad
         ( TS.IO.DATA$(5) = "62" ) or   \ Actualizacion de pinpad
         ( TS.IO.DATA$(5) = "63" ) or   \
         ( TS.IO.DATA$(5) = "6"  ) or   \
         ( TS.IO.DATA$(5) = "5"  ) or   \
         ( TS.IO.DATA$(5) = "4"  ) or   \
         ( TS.IO.DATA$(5) = "3"  ) or   \
         ( TS.IO.DATA$(5) = "2"  ) or   \ 
         ( TS.IO.DATA$(5) = "0"  ) then secuenciaTeclaOperadorNoPermitida = -1
   endif

end function

function MSMTINSTsecuenciaReimpresionCreditoInstitucional
   integer*1 MSMTINSTsecuenciaReimpresionCreditoInstitucional
   
   MSMTINSTsecuenciaReimpresionCreditoInstitucional = 0
   if (TS.IO.KEYS(5) = 61) and (TS.IO.MOTORKEY = 61) and (TS.IO.STATE = 10) then begin
      if esnumero(TS.IO.DATA$(5)) then begin
         if (val(TS.IO.DATA$(5)) = 77) then MSMTINSTsecuenciaReimpresionCreditoInstitucional = -1
      endif
   endif
 
end function

function MSMTTARBsecuenciaReimpresionComprobanteTarjeta
   integer*1 MSMTTARBsecuenciaReimpresionComprobanteTarjeta
   
   MSMTTARBsecuenciaReimpresionComprobanteTarjeta = 0
   if (TS.IO.KEYS(5) = 61) and (TS.IO.MOTORKEY = 61) and (TS.IO.STATE = 10) then begin
      if esnumero(TS.IO.DATA$(5)) then begin
         if (val(TS.IO.DATA$(5)) = 64) then MSMTTARBsecuenciaReimpresionComprobanteTarjeta = -1
      endif
   endif
 
end function

!------------------------------------------------------------------------------
!Escribe log de auditoria
!m$ mensaje
!------------------------------------------------------------------------------
sub TraceINV( m$ )
   string m$
   if not depurar then exit sub
   call traceNuo(m$)
end sub

sub seteaClienteInventario( cliId$, indic ) public
   integer*1 indic                        ! indicadores 
   string cliId$                          ! id cliente

   INVidCliente$    = cliId$
   INVindicCliente% = indic

end sub

!------------------------------------------------------------------------------
! Graba informacion cadena de usuario 99 ?? en TLOG
! Si tipo = 0 es cadena encabezado de inventario
! Si tipo = 1 es cadena articulo no encontrado 
!------------------------------------------------------------------------------
sub generaCadenaInventario( tipo% )
   integer*1 tipo%
   string    dato$

   if tipo% = 0 then begin
      INVcadena99$ = pack$( "99" )              + ":" + \
         pack$( str$(INVtipoCadena99%) )    + ":" + \
         pack$(INVidSeccion$)            + ":" + \
         pack$(INVidPicking$)
   endif

   if tipo% = 1 then begin
      INVcadena99$ = pack$( "99" )                   + ":" + \
         pack$( str$(INVtipoCadenaNoExiste99%) ) + ":" + \
         pack$(TS.IO.DATA$( 2 ))         + ":" + \
         pack$(TS.IO.DATA$( 6 ))
   endif

end sub

function pideSupervisor
   integer*1 pideSupervisor
   
   pideSupervisor = 0
   if INVnewSeccion% then begin
      pideSupervisor = -1
      exit function
   endif   
!!! Por desarrollar pendiente   

   pideSupervisor = -1
end function

function pideSeccionINVE
   integer*1 pideSeccionINVE
   string    dato$,           \
             m$

   pideSeccionINVE = 0
   INVidSeccion$   = ""

   otraSeccion:
   
   if not (jGuiSubState = 80504) then begin
      jGuiSubState = 80511
   endif
   
   call javaEvent(terminalSubStateMsg)
   m$ =  "INGRESE SECCION     "
   dato$ = ingresoDatos$( m$, "" )
   if left$( dato$, 1 ) = "A" then begin
      if jGuiSubState = 80511 then begin
         jGuiSubState = 80504 
      endif else begin
         jGuiSubState = 0
      endif
      goto otraSeccion
   endif
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )
   if not esnumero(dato$) or ( len( dato$) <> 6 ) then begin
      call muestraBorrar( "SECCION INVALIDA", "6 DIGITOS   <Borrar>" )

      if jGuiSubState = 80511 then begin
         jGuiSubState = 80504 
      endif else begin
         jGuiSubState = 0
      endif
      goto otraSeccion
   endif
   INVidSeccion$ = dato$
   pideSeccionINVE = -1

   salir:
   exit function
   
   erroringreso:
   call TraceINV("ERROR EN pideSeccionINVE " + + err + hexanum$( errn )  )
   resume salir

end function

function pideIdPickingINVE
   integer*1 pideIdPickingINVE
   string    dato$,         \
             m$
   
   pideIdPickingINVE = 0
   INVidPicking$     = ""

otraCedula:

   if not (jGuiSubState = 80529) then begin
      jGuiSubState = 80515
   endif

   call javaEvent(terminalSubStateMsg)
   m$ =  "INGRESE CEDULA      "
   dato$ = ingresoDatos$( m$, "" )
   if left$( dato$, 1 ) = "A" then begin
      if jGuiSubState = 80515 then begin
         jGuiSubState = 80529 
      endif else begin
         jGuiSubState = 0
     endif

      goto otraCedula
   endif
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )

   !validar cedula ej --> 1715686372
   if not verificaCedula( dato$ ) then begin
      call muestraBorrar( "CEDULA INVALIDA", "REINTENTE   <Borrar>" )
      if jGuiSubState = 80515 then begin
         jGuiSubState = 80529 
      endif else begin
         jGuiSubState = 0
      endif
      goto otraCedula
   endif
   INVidPicking$ = dato$
   pideIdPickingINVE = -1
   
   salir:
   exit function
   
   erroringreso:
   call TraceINV("ERROR EN pideIdPickingINVE " + + err + hexanum$( errn )  )
   resume salir
end function

function validaCondicionesINVE
   integer*1 validaCondicionesINVE, opc
   
   validaCondicionesINVE = -1

   !Impide ingresar RUC de cliente en una transaccion de inventario
   if INVenModalidad% then begin
      opc = 0
      if secuenciaTeclaCliente                            then opc = -1
      if NUOTCNBsecuenciaTeclaCNB                         then opc = -1     ! BUGS 22-04-2022 CCY 11087
      if NUOTCNBsecuenciaReversoDeposito                  then opc = -1     ! BUGS 22-04-2022 CCY 11087
      if NUOTCNBsecuenciaReimprecionComprobante           then opc = -1     ! BUGS 22-04-2022 CCY 11087
      if EsSecuenciaConsultaPuntosCLI                     then opc = -1     ! BUGS 09-05-2022 CCY 
      if MSMTGIFTsecuenciaTeclaTarjetaRegalo              then opc = -1
      if MSMTINSTsecuenciaTeclaCreditoInstitucional       then opc = -1
      if MSMTINSTsecuenciaReimpresionCreditoInstitucional then opc = -1     ! BUGS 22-04-2022 CCY 11087
      if MSMTTARBsecuenciaReimpresionComprobanteTarjeta   then opc = -1     ! BUGS 22-04-2022 CCY 11087
      if secuenciaTeclaOperadorNoPermitida                then opc = -1 

      if opc then begin
         msg1$ = "NO PERMITIDO        "
         msg2$ = "EN INVENTARIO       "
         call muestraBorrar( msg1$, msg2$ )
         validaCondicionesINVE = 0
      endif
   endif

end function

function esSecuenciaOnOffINVE
   integer*1 esSecuenciaOnOffINVE

   esSecuenciaOnOffINVE = 0

   if (TS.IO.KEYS(5) = 61) and (TS.IO.MOTORKEY = 61) and (TS.IO.STATE = 10) and ( TS.IO.DATA$(5) <> "" ) then begin
      if esnumero(TS.IO.DATA$(5)) and ( val(TS.IO.DATA$(5)) = INVnumSecuencia% ) then esSecuenciaOnOffINVE = -1
   endif

end function

function haySeccionActivaINVE
   integer*1 haySeccionActivaINVE

   haySeccionActivaINVE = 0
   if INVidSeccion$ <> "" then haySeccionActivaINVE = -1

end function

sub inicializaSeccionINVE

    INVnewSeccion% = -1 
    INVidPicking$  = ""         ! Cedula funcionario que realiza toma del inventario
    INVidSeccion$  = ""         ! Seccion a inventariar
    INVidCliente$  = ""         !
    INVEtrxAnulada% = 0         !

end sub

function existeClienteIngresado
   integer*1 existeClienteIngresado

   existeClienteIngresado = 0
   
   !No permite iniciar funcionalidad de inventario si existe cliente ingresado
   if INVidCliente$ <> "" then begin
      existeClienteIngresado = -1
      msg1$ = "CLIENTE INGRESADO NO"
      msg2$ = "CUMPLE TRX  <Borrar>"
      call muestraBorrar( msg1$, msg2$ )
   endif
   
end function 

function cantidadMaximaSuperadaINVE
   integer*1 cantidadMaximaSuperadaINVE
   integer*4 maxPermitido%
   
   cantidadMaximaSuperadaINVE = 0
   
   maxPermitido% = INVcantiMaximaPluUNIT%
   if TS.IO.KEYS(6) = 72 then maxPermitido% = INVcantiMaximaPluPESA%

   if val( TS.IO.DATA$(6) ) >  maxPermitido% then begin
      msg1$ = "ERROR CANTIDAD MAX. "
      msg2$ = "SUPERADA [" + str$(INVcantiMaximaPluUNIT%) + "]"
      call muestraBorrar( msg1$, msg2$ )
      call ignoraTeclas
      cantidadMaximaSuperadaINVE = -1
   endif 

end function 

function secuenciaVentaArticuloINVE
   integer*1 secuenciaVentaArticuloINVE
   string    dato$
   
   secuenciaVentaArticuloINVE = 0

   if INVenModalidad% and ( INVidPicking$ <> "" ) and (INVidSeccion$ <> "" ) then begin
      if INVnewSeccion% then INVnewSeccion% = 0
      
      !Venta de articulo
      if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) and (TS.IO.DATA$(2) <> "" ) then begin
         
         !venta por cantidad
         if (( TS.IO.KEYS(6) = 75 ) or ( TS.IO.KEYS(6) = 72 )) and ( TS.IO.DATA$(6) <> "" ) then begin
            dato$ = TS.IO.DATA$(2)
            !venta por peso
            if esArticuloPesable( dato$ ) then TS.IO.KEYS(6) = 72
            if cantidadMaximaSuperadaINVE then
         endif \
         else begin
            !Venta por unidad
            if INVexigeCantidad% then begin
               msg1$ = "INVENTARIO REQUIERE "
               msg2$ = "TECLA CANTIDAD      "
               call muestraBorrar( msg1$, msg2$ )
               call ignoraTeclas
            endif
         endif
         secuenciaVentaArticuloINVE = -1
      endif
   endif   

end function

sub mensajeBienVenidaINVE

    msg1$ = "ACCEDIENDO A TOMA DE"
    msg2$ = "INVENTARIO          "
    call muestraVisor(1, msg1$, msg2$)
    wait;1000

end sub

function secuenciaPagoINVE
   integer*1 secuenciaPagoINVE

   secuenciaPagoINVE = 0
   if INVenModalidad% and ( INVidPicking$ <> "" ) and (INVidSeccion$ <> "" ) then begin
      if ( TS.IO.KEYS(7) > 90 ) and ( TS.IO.KEYS(7) < 97 ) then begin
         if TS.IO.KEYS(7) <> 91 then begin
            call muestraBorrar("SOLO PUEDE PAGAR CON","EFECTIVO !! ")
            call ignoraTeclas
         endif \
         else INVnewSeccion% = -1 
         secuenciaPagoINVE = -1
      endif
   endif
   
end function   

function secuenciaSuspencionINVE
   integer*1 secuenciaSuspencionINVE
   
   secuenciaSuspencionINVE = 0
   
   if INVenModalidad% and ( INVidPicking$ <> "" ) and (INVidSeccion$ <> "" ) then begin
      if (TS.IO.KEYS(1) = 82) then begin
         msg1$ = "INVENTARIO NO PUEDE "
         msg2$ = "SUSPENDERSE         "
         call muestraBorrar( msg1$, msg2$ )
         call ignoraTeclas
         secuenciaSuspencionINVE = -1
      endif
   endif

end function   

function hayArticuloINVE
   integer*1 hayArticuloINVE
   string ind$
   
   hayArticuloINVE = 0
   
   ind$ = ""
   call resumenInventario(ind$, "", 0)
   if val(ind$) = 0 then exit function \
   else hayArticuloINVE = -1

end function   

sub deshabilitaINVE
   string dato$
   
   jGuiSubState = 80571
   call javaEvent(terminalSubStateMsg) 
   jGuiSubState = 1008
   call javaEvent(terminalSubStateMsg)
   INVenModalidad% = 0
   INVidPicking$   = "" 
   INVidSeccion$   = ""
   msg1$ = "INVENTARIO INACTIVO "
   msg2$ = "<Borra>    CONTINUAR"
   call muestraBorrar( msg1$, msg2$ )
   dato$ = TS.SDESC$(44)
   TS.DISP1$ = left$(dato$,20)
   TS.DISP2$ = right$(dato$,20)
   call muestraVisor(1,TS.DISP1$,TS.DISP2$)
   wait;2000

end sub

function enModoINVE
   integer*1 enModoINVE
   
   enModoINVE = 0
   if INVenModalidad% and not INVlinknewSeccion% then enModoINVE = -1
end function  

sub linkNextINVE
    INVlinknewSeccion% = 0
    if INVnewSeccion% and INVenModalidad% and INVidSeccion$ = "" then begin
       if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) then begin
       call ignoraTeclas
       TS.IO.KEYS(5)  = 61 
       TS.IO.MOTORKEY = 61
       TS.IO.STATE    = 10
       TS.IO.DATA$(5) = str$(INVnumSecuencia%)       
       INVlinknewSeccion% = -1
       endif
    endif
end sub

function secuenciaAnulaTotalINVE
   integer*1 secuenciaAnulaTotalINVE

   secuenciaAnulaTotalINVE = 0

   if ( TS.IO.MOTORKEY = 81 ) and ( TS.IO.KEYS(1) = 70 ) and ( TS.IO.KEYS(6) = 81 ) and INVenModalidad% then begin
      if not hayArticuloINVE then begin
         msg1$ = "SECUENCIA INVALIDA "
         msg2$ = "                   "
         call muestraBorrar( msg1$, msg2$ )
         call ignoraTeclas
      endif
      secuenciaAnulaTotalINVE = -1
   endif

end function

function secuenciaDesconecionINVE
   integer*1 secuenciaDesconecionINVE

   secuenciaDesconecionINVE = 0
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 61 ) and ( TS.IO.KEYS(5) = 61 ) and ( TS.IO.DATA$(5) = "" ) then secuenciaDesconecionINVE = -1

end function   

function trxVentaIniciadaINVE
   integer*1 trxVentaIniciadaINVE
   
   trxVentaIniciadaINVE = 0
   
   if SL.END <> 0 then begin
      msg1$ = "NO PERMITIDO TRX YA "
      msg2$ = "INICIADA            "
      call muestraBorrar( msg1$, msg2$ )
      call ignoraTeclas
      trxVentaIniciadaINVE = -1
   endif

end function 

!------------------------------------------------------------------------------
! Al iniciar una transaccion de venta
!------------------------------------------------------------------------------
function MSMTINVE01 public
   integer*1 MSMTINVE01

   MSMTINVE01 = 0
   if not activo then          exit function
   if not INVenModalidad% then exit function
   MSMTINVE01 = -1
   if ( len( INVcadena99$ ) <> 0 ) then begin
      call agregaCadenaTSL( INVcadena99$ )
      INVcadena99$ = ""
      call noAplicaDescuento
   endif
   
end function

!------------------------------------------------------------------------------
! Al finalizar una transaccion de venta
!------------------------------------------------------------------------------
function MSMTINVE02 public
   
   if not activo          then exit function
   if not INVenModalidad% then begin
      INVidCliente$  = ""
   endif \
   else begin
      call inicializaSeccionINVE
      call limpiaFactura
   endif
   
end function

!------------------------------------------------------------------------------
!Lectura de parametros desde archivo c:/ADX_UDT1/PARGENER.DAT
!incorporar a USER 7
!------------------------------------------------------------------------------
sub MSMTINVE07 public
   integer*1 k
   integer*4 inicioBloque
   string v$                      !
   
   fechaModificacion$ = " 03 May 2022"
   activo  = 0
   depurar = 0
   dummy   = 0
   INVtipoCadena99%   = 99
   INVtipoCadenaNoExiste99% = 99
   INVnumSecuencia%   = 99
   INVenModalidad%    = 0
   INVprefijoVisor$   = "<INV>"
   INVcantiMaximaPluUNIT% = 1000
   INVcantiMaximaPluPESA% = 1000000
   INVidCliente$      = ""
   INVexigeCantidad%  = 0
   INVnewSeccion%     = 0
   
   call imprimeCJ("******* MODULO TOMA INVENTARIO *******")
   
   v$ = "INVENTARIO"
   inicioBloque = abreParametros( "R::$ARGENER", "[" + v$ + "]" , 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO " + fechaModificacion$ )
      call cierraParametros( 80 )
      exit sub
   endif
   
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin
      if esNumero( v$ ) then k = val(v$) else k = 0
      if (k and 1) then activo  = -1
      if (k and 2) then depurar = -1
      if (k and 4) then dummy   = -1
      
      !cadena del tipo 99 de inventario
      v$ = leeParametro$( "CADENA_TSL", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then INVtipoCadena99% = val( v$ )
         
      !cadena del tipo 99 de inventario para articulo no existe
      v$ = leeParametro$( "CADENA_TSL_NO_EXISTE", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then INVtipoCadenaNoExiste99% = val( v$ )

      !operacion conex para acceder a inventario
      v$ = leeParametro$( "OPERACION_CONEX", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then INVnumSecuencia% = val( v$ )   

      !cantidad maxima de unidades permitidas por articulo unitarios
      v$ = leeParametro$( "CANTIDAD_MAXIMA", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then INVcantiMaximaPluUNIT% = val( v$ ) 

      !cantidad maxima de unidades permitidas por articulo pesable
      v$ = leeParametro$( "CANTIDAD_MAXIMA_PESABLE", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then INVcantiMaximaPluPESA% = val( v$ ) 

      !incorpora cadena 99 si no se encuentra un item 
      v$ = leeParametro$( "INFORMA_ITEM_NO_ENCONTRADO", 80, inicioBloque )
      if UCASE$(v$) = "SI" then INVitemNoEncontrado% = -1

      !exige tecla cantidad 
      v$ = leeParametro$( "EXIGE_CANTIDAD", 80, inicioBloque )
      if UCASE$(v$) = "SI" then INVexigeCantidad% = -1

   endif

   call cierraParametros( 80 )

   if activo then v$ = "ON " else v$ = "OFF "
   call imprimeCJ( "ESTADO " + v$ + fechaModificacion$ )
   if depurar then begin
      call imprimeCJ( "CADENA TSL       " + right$( string$( 15, " " ) + str$( INVtipoCadena99% )     , 15 ) )
      call imprimeCJ( "CADENA TSL NO EXI" + right$( string$( 15, " " ) + str$( INVtipoCadenaNoExiste99% )     , 15 ) )
      call imprimeCJ( "SECUENCIA CONEX  " + right$( string$( 15, " " ) + str$( INVnumSecuencia% )     , 15 ) )
      call imprimeCJ( "CANT.MAXIMA UNITA" + right$( string$( 15, " " ) + str$( INVcantiMaximaPluUNIT% ), 15 ) )
      call imprimeCJ( "CANT.MAXIMA PESAB" + right$( string$( 15, " " ) + str$( iNVcantiMaximaPluPESA% ), 15 ) )
      if INVitemNoEncontrado% then v$ = "SI" \
      else v$ = "NO"
      call imprimeCJ( "ITEM NO ENCONTRAD" + right$( string$( 15, " " ) + v$ , 15 ) )
      if INVexigeCantidad% then v$ = "SI" \
      else v$ = "NO"
      call imprimeCJ( "EXIGE CANTIDAD   " + right$( string$( 15, " " ) + v$ , 15 ) )
      if Dummy then call imprimeCJ("ACTIVADO INDICADOR AUTORESPUESTA")
   endif
       
end sub

!------------------------------------------------------------------------------
! Despues de leer un registro de articulo
!------------------------------------------------------------------------------
function MSMTINVE08 public
   integer*1 MSMTINVE08
   
   MSMTINVE08 = 0
   if not activo          then exit function
   if not INVenModalidad% then exit function
   MSMTINVE08 = -1
    
end function

!------------------------------------------------------------------------------
! Despues de la lectura del Teclado/Scan/Lect Optico
!------------------------------------------------------------------------------
function MSMTINVE14 public
   integer*1 MSMTINVE14
   string ind$

   call linkNextINVE
   MSMTINVE14 = 0

   if not activo                then exit function
   if not validaCondicionesINVE then goto salir
   if esSecuenciaOnOffINVE then begin
      if trxVentaIniciadaINVE then goto salir
      if haySeccionActivaINVE then begin
         if not hayArticuloINVE then begin
            call deshabilitaINVE
            goto salir
         endif             
         msg1$ = "NO PERMITIDO SECCION"
         msg2$ = "[" + INVidSeccion$ + "] EN INVENTARIO"
         call muestraBorrar( msg1$, msg2$ )
         goto salir
      endif
      
      if enModoINVE then begin
         call deshabilitaINVE
         goto salir
      endif

      if existeClienteIngresado then goto salir

      if not INVenModalidad% then begin
         if not pideSupervisor then goto salir
      endif

      call mensajeBienVenidaINVE
      INVlinknewSeccion% = 0
      if not pideSeccionINVE then goto salir

      if not pideIdPickingINVE then begin
         INVidSeccion$ = ""
         goto salir
      endif

      jGuiSubState = 80570
      call javaEvent(terminalSubStateMsg)	

      msg1$ = "INVENTARIO ACTIVADO "
      msg2$ = "<Borra>    CONTINUAR"
      call muestraBorrar( msg1$, msg2$ )
      if not INVenModalidad% then INVenModalidad% = -1
      jGuiSubState = 80575
      call javaEvent(terminalSubStateMsg) 
      call generaCadenaInventario( 0 )
      goto salir
   endif 

   if secuenciaVentaArticuloINVE then begin
      MSMTINVE14 = -1
      exit function
   endif
   
   if secuenciaPagoINVE then begin
      MSMTINVE14    = -1
      exit function
   endif
   
   if secuenciaSuspencionINVE then begin
      MSMTINVE14    = -1
      exit function
   endif
   
   if secuenciaAnulaTotalINVE then begin
      MSMTINVE14    = -1
      INVEtrxAnulada% = -1
      exit function
   endif
   
   if secuenciaDesconecionINVE then begin
      if INVenModalidad% then begin
         msg1$ = "SECUENCIA INVALIDA  "
         msg2$ = "INVENTARIO ACTIVO   "
         call muestraBorrar( msg1$, msg2$ )
         call ignoraTeclas
      endif
      MSMTINVE14    = -1
      exit function
   endif

   exit function

   salir:
   call ignoraTeclas

end function

!-----------------------------------------------------------------------------
! Antes de imprimir a la estacion de recibo de caja
!-----------------------------------------------------------------------------
function MSMTINVE20 public
   integer*1 MSMTINVE20, tipoResumen
   
   MSMTINVE20  = 0
   if not activo          then exit function
   if not INVenModalidad% then exit function 
   if ( TS.LINETYPE = 6 ) and ( TS.LINEDATA = 1 ) and ( INVidSeccion$ <> "" ) then begin
      tipoResumen = 1
      if INVEtrxAnulada% then tipoResumen    = 2
   endif
   
   if tipoResumen <> 0 then begin 
      if INVidSeccion$ <> "" then begin
         call resumenInventario(INVidSeccion$ , INVidPicking$, tipoResumen )    ! rutina del modulo MSMTFACT.BAS
         INVidSeccion$ = ""
         INVEtrxAnulada% = 0
         tipoResumen = 0
      endif
      MSMTINVE20 = -1
   endif
   
   if (TS.LINETYPE = 18) AND (TS.LINEDATA = 0) then begin
      call limpiaImpresion
      MSMTINVE20 = -1
   endif
   
   if INVenModalidad% and INVidSeccion$ <> "" then begin
      call TraceINV("MSMTINVE20 TS.LINETYPE=[" + str$(TS.LINETYPE) + "] TS.LINEDATA =[" + str$( TS.LINEDATA ) + "]")
   endif
   
end function

!-----------------------------------------------------------------------------
! Antes de imprimir una linea en el visor
!-----------------------------------------------------------------------------
function MSMTINVE23 public
   integer*1 MSMTINVE23, claveErronea, ingreso
   string    dato$

   MSMTINVE23 = 0
   if not activo          then exit function
   if not INVenModalidad% then begin
      if left$(TS.DISP1$,5) = "<INV>" then begin
         dato$ = TS.SDESC$(44)
         TS.DISP1$ = left$(dato$,20)
         TS.DISP2$ = right$(dato$,20)
      endif
      exit function
   endif

   if ( TS.IO.STATE = 10 ) then begin
      if ( INVidSeccion$ <> "" ) then begin

         !Incorpora cadena si no se encuentra articulo 
         if ( TS.LINETYPE = 12 ) and ( TS.LINEDATA = 0 ) then begin
            if ( TS.IO.DATA$(2) <> "" ) and ( TS.IO.DATA$(6) <> "" ) then begin
               if INVitemNoEncontrado% then begin
                  call generaCadenaInventario( 1 )
                  call agregaCadenaTSL( INVcadena99$ )
               endif
            endif
         endif

         !mensaje inicio
         if ( TS.LINETYPE = 9 ) and ( TS.LINEDATA = 0 ) or \
            ( TS.LINETYPE = 6 ) and ( TS.LINEDATA = 13 ) then begin
            if ingreso = 1 then begin
               jGuiSubState = 1008
               call javaEvent(terminalSubStateMsg)
            endif
            TS.DISP1$ = INVprefijoVisor$
            TS.DISP2$ = "INGRESAR ARTICULOS   "
         endif
          
         !mensaje ingreso articulo
         if ( TS.LINETYPE = 1 ) and ( TS.LINEDATA = 0 ) then begin
            TS.DISP2$ = TS.DISP1$
            TS.DISP1$ = INVprefijoVisor$ + " " + TS.IO.DATA$(6) + " * " + TS.IO.DATA$(2)
         endif
          
         !mensaje total
         if ( TS.LINETYPE = 3 ) and ( TS.LINEDATA = 0 ) then begin
            TS.DISP1$ = INVprefijoVisor$ + " EFE=FIN INVENT"
            TS.DISP2$ = "INGRESAR ARTICULOS   "
         endif
         
         !mensaje pago
         if ( TS.LINETYPE = 2 ) and ( TS.LINEDATA = 2 ) then begin
            TS.DISP1$ = INVprefijoVisor$ + " FIN INVENTARIO"
            TS.DISP2$ = "SECCION " + INVidSeccion$
         endif

         !---------------------------------
         !Mensaje original
         !TS.DISP1$=[TRANSACCION ANULADA ]
         !TS.DISP2$=[                .00 ]
         !---------------------------------
         if ( TS.LINETYPE = 5 ) and ( TS.LINEDATA = 8 ) then begin
            TS.DISP2$ = TS.DISP1$
            TS.DISP1$ = INVprefijoVisor$ + "SECCION=" + INVidSeccion$ + ""            
         endif

      endif else begin

         if ( TS.LINETYPE = 9 ) and ( TS.LINEDATA = 0 ) or \
            ( TS.LINETYPE = 6 ) and ( TS.LINEDATA = 13 ) then begin

            ingreso = 1
            jGuiSubState = 80580

            call javaEvent(terminalSubStateMsg)
            TS.DISP1$ = INVprefijoVisor$ + " PLU INGRESA   "
            TS.DISP2$ = "NUEVA SECCION       "
         endif
      
         if ( TS.LINETYPE = 6 ) and ( TS.LINEDATA = 12 ) then begin
            TS.DISP2$ = TS.DISP1$
            TS.DISP1$ = INVprefijoVisor$
          endif
      
      endif 
      MSMTINVE23 = -1 
   endif
   
   !terminal asegurado
   if ( TS.IO.STATE = 5 ) then begin
      if ( INVidSeccion$ = "" ) then begin
         
         !desasegurar terminal con clave erronea 
         if ( TS.LINETYPE = 8 ) and ( TS.LINEDATA = 77 ) then begin
            claveErronea = -1
            MSMTINVE23   = -1 
         endif

         if ( TS.LINETYPE = 9 ) and ( TS.LINEDATA = 0 ) then begin
            if claveErronea then begin
               TS.DISP1$ = INVprefijoVisor$
               TS.DISP2$ = TS.SDESC$(43)
               claveErronea = 0
            endif \
            else begin
               TS.DISP1$ = INVprefijoVisor$ + " PLU INGRESA   "
               TS.DISP2$ = "NUEVA SECCION       "
               MSMTINVE23 = -1 
            endif
         endif

      endif   
   endif
   
end function

!------------------------------------------------------------------------------
! En el cumputational price
!------------------------------------------------------------------------------
sub MSMTINVE43 public
   if not activo          then exit sub
   if not INVenModalidad% then exit sub
   if INVidSeccion$ = ""  then exit sub
   SL.IE.SALEPRIC = 0
   SL.IT.XPRICE   = 0
end sub

!------------------------------------------------------------------------------
! En la recuperacion de transaccion
!------------------------------------------------------------------------------
sub MSMTINVE53 public
   if not activo then exit sub
end sub

!------------------------------------------------------------------------------
! Antes de actualizar la matriz de transacciones
!------------------------------------------------------------------------------
function MSMTINVE68(ENTRY$, ELEMENT) public
   integer*1 MSMTINVE68
   string ENTRY$
   integer*2 ELEMENT           !

   MSMTINVE68 = 0
   if not activo          then exit function
   if not INVenModalidad% then exit function
   MSMTINVE68 = -1
   
end function
