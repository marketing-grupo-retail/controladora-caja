!------------------------------------------------------------------------------
! Modulo: MSMTRETE.BAS Modulo RETENCIONES
! Fecha : 26 de Enero de 2022
! Autor : Carlos Calderon
! History : 09-02-2022 Carlos Calderon
!                      Se incorpora propiedad para  manejar multiples retenciones
!------------------------------------------------------------------------------
%ENVIRON T

integer*1 \
   estado,                         \ Estado funcionalidad -1= Habilitado 0=Deshabilitado
   depurar,                        \ Indicador de debug -1= Habilitado 0=Deshabilitado
   dummy,                          \ Indicador de dummy -1= Habilitado 0=Deshabilitado
   medioPagoTef,                   \ Medio de pago SMA para tarjeta de credito
   REtipoRet%,                     \ Tipo de retencio 1 2 o 3 
   REpideAnular%,                  \ Indicador se solicita anular retencion -1=si 0=n0
   REesPagoRetencion%,             \ Indicador es un pago retencion -1=si 0=n0
   flagrete                        ! Flag si hay retencion

integer*2 \
   REteclaMotora,                  \ Tecla motora para Retencion
   teclaMotoraTef                  ! Tecla motora Tef
             
integer*4 \
   REmontoAfectoRetencion,         \ Monto base afecto a retencion
   REmontoRetencion%,              \ Monto retencion
   REtipoRetencion(2)              ! Matriz con tipos de retenciones y su porcentaje
   
string \
   fechaModificacion,                 \ Fecha ultima modificacion del modulo
   REidCliente$,                      \ Id del cliente
   REnumDocumento$,                   \ Numero de documento retencion
   REenTransaccion$(2),               \ Matriz con retenciones en la transaccion
   REpagosSAretencion$,               \ Medios de pagos SMA para las retenciones
   REpagosNoPermitidos$               ! Medio de pagos no permitidos con retenciones

integer*2 global \
   SL.END

%INCLUDE EAMTSWKG.J86
%INCLUDE NUOTVARI.J86
%INCLUDE JAVAGUIV.J86

!***** RUTINAS EXTERNAS *******
%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86
%INCLUDE MSMTCLIE.J86
%INCLUDE JAVAGUIC.J86

SUB TSHIEC04 EXTERNAL
END SUB

!------------------------------------------------------------------------------
!Escribe log de auditoria
!m$ mensaje
!------------------------------------------------------------------------------
sub TraceRET( m$ )
   string m$
   if not depurar then exit sub
   call traceNuo(m$)
end sub

!Retorna medio de pago SMA del pago tarjera de credito Tef
function getteclaMotoraTef external
   integer*2 getteclaMotoraTef
end function

!retorna el medio de pago SMA del pago tarjera de credito tef
function get.MP.Tef external
   integer*2 get.MP.Tef
end function

function REesSecuenciaPagoTarjeta
   integer*1 REesSecuenciaPagoTarjeta

   REesSecuenciaPagoTarjeta = 0
   
   if ( TS.IO.MOTORKEY = teclaMotoraTef  ) and \
      ( TS.IO.KEYS(7)  = teclaMotoraTef  ) and \
      ( TS.IO.STATE    = 10  ) and \
      ( TS.INTRX ) then begin
      REesSecuenciaPagoTarjeta = -1
   endif

   if ( TS.IO.MOTORKEY = 94 ) and \
      ( TS.IO.KEYS(7)  = 94 ) and \
      ( TS.IO.STATE    = 10 ) and \
      ( TS.IO.KEYS(3)  = 78 ) and \
      ( TS.IO.DATA$(3) = "2" ) or ( TS.IO.DATA$(3) = "3" ) and \
      ( TS.IO.DATA$(7) <> "" ) and \
      ( TS.INTRX ) then begin
      REesSecuenciaPagoTarjeta = -1
   endif 

end function

function RetencionIntrxRET
   integer*1 RetencionIntrxRET,     \
             x

   RetencionIntrxRET = 0
   
   for x = 1 to 3
       if REenTransaccion$(x,1) <> "" then begin
          RetencionIntrxRET = x
!call TraceRET("RetencionIntrxRET x=[" + str$(x) + "]" )
          x = 3
       endif
   next x

end function

function existeRetencionRET( opc% )
   integer*1 existeRetencionRET,     \
             opc%

!call TraceRET("existeRetencionRET")
   existeRetencionRET = 0
   if REenTransaccion$(opc%,1) <> "" then existeRetencionRET = -1

end function

sub debugRetencionesExistentes
   integer*1 x
   string cad1$,cad2$,cad3$
   for x = 1 to 3
       cad1$ = REenTransaccion$(x,1)
       cad2$ = REenTransaccion$(x,2)
       cad3$ = REenTransaccion$(x,3)
!call TraceRET("debugRetencionesExistentes x=[" + str$(x) + "] cad1$=["+cad1$+"] cad2$=["+cad2$+"] cad3$=["+cad3$+"]" )
   next x
end sub

function REesTeclaRetencion
   integer*1 REesTeclaRetencion
             
   REesTeclaRetencion = 0
   REpideAnular%      = 0
   if ( TS.IO.MOTORKEY = REteclaMotora ) and \
      ( TS.IO.KEYS(2)  = REteclaMotora ) and \
      ( TS.IO.STATE    = 10 )            and \
      ( TS.INTRX ) then begin
      if TS.IO.KEYS(1) = 70 then REpideAnular% = -1
      if NOT TS.BAL.TAKEN then begin
         TS.GUIDANCE = 1020
         TS.IO.MOTORKEY = 0
         exit function
      endif
      REesTeclaRetencion = -1
      call debugRetencionesExistentes
!call TraceRET("REesTeclaRetencion REpideAnular%=[" + str$(REpideAnular%) + "]" )
   endif

end function  

sub seteaClienteRetenciones( cliId$ ) public
    string cliId$                          ! id cliente

    REidCliente$   = cliId$
   
end sub

function buscaPagosActivosTef
   integer*1 buscaPagosActivosTef, \
             f,                    \
             sentido
             
   integer*4 montoTef
   string cad$, m$
   buscaPagosActivosTef = 0
   
   montoTef = 0
   
   for f = 1 to SL.END
       cad$ = leeCadenaTSL$( f )
       m$ = getCampoTSL$(cad$,1)
       if m$ = "05" or m$ = "06" then begin
          sentido = 1
          if m$ = "06" then sentido = -1
          m$ = getCampoTSL$(cad$,2)
          if m$ = str$(medioPagoTef) then begin
             m$ = getCampoTSL$(cad$,3)
             montoTef = montoTef + ( val( f$ ) * sentido )         
          endif
       endif
   next f
   if montoTef <> 0 then buscaPagosActivosTef = -1 

end function

!----------------------------------------------------------------------
!Busca en arreglo del TSL medios de pagos no permitidos con retenciones
!----------------------------------------------------------------------
function buscaPagosNoPermitidos
   integer*1 buscaPagosNoPermitidos, \
             f,                    \
             sentido

   integer*4 monto
   string cad$, m$
   buscaPagosNoPermitidos = 0

   monto = 0

   for f = 1 to SL.END
       cad$ = leeCadenaTSL$( f )
       m$ = getCampoTSL$(cad$,1)
       if m$ = "05" or m$ = "06" then begin
          sentido = 1
          if m$ = "06" then sentido = -1
          m$ = getCampoTSL$(cad$,2)
          if match( m$ + ";",REpagosNoPermitidos$,1) <> 0 then begin
             m$ = getCampoTSL$(cad$,3)
             monto = monto + ( val( m$ ) * sentido )
          endif
       endif
   next f
   if monto <> 0 then buscaPagosNoPermitidos = -1 

end function

function REcondicionesRetencion
   integer*1 x,                          \
             REcondicionesRetencion      !

!call TraceRET("REcondicionesRetencion_IN" )

   REcondicionesRetencion = 0
   if ( TS.IO.PREV.KEYS(2) = 80 ) and ( TS.IO.PREV.DATA$(2) <> "" ) then begin  
      call muestraBorrar( "DEBE TOTALIZAR", "PARA CONTINUAR" )
      exit function
   endif
      
   !x = buscaPagosActivosTef
   if buscaPagosNoPermitidos then begin
      !if x <> 0 then begin
      call muestraBorrar( "RETENCION NO PERMITI", "EXISTEN PAGOS NO AUT" )
      exit function
   endif
   
   if REidCliente$ = "" then begin
      call muestraBorrar( "DEBE CAPTURAR DATO", "DEL CLIENTE" )
      exit function
   endif
   
   if left$(REidCliente$,1) <> "2" then begin
      call muestraBorrar( "NO APLICA RETENCION", "A PERSONAS" )
      exit function
   endif

   if REpideAnular% then begin
      if RetencionIntrxRET = 0 then begin
         call muestraBorrar( "NO EXISTEN RETENCION", "POR ANULAR" )
         exit function
      endif
   endif

   if RetencionIntrxRET <> 0 and secuenciaTeclaCliente then begin
       call muestraBorrar( "NO PERMITIDO CLIENTE", "RETENCION ACTIVA" )
       exit function
   endif   

!call TraceRET("REcondicionesRetencion_OU" )

   REcondicionesRetencion = -1

end function   

function REpideTipoRetencion
   integer*1 REpideTipoRetencion, \
             x
   string    dato$

   REpideTipoRetencion = 0
   REtipoRet%          = 0

!call TraceRET("REpideTipoRetencion REpideAnular%=[" + str$(REpideAnular%) + "]")

   otrodato:

   if not (jGuiSubState = 80529) then begin
      jGuiSubState = 80511
   endif
   call javaEvent(terminalSubStateMsg)
   dato$ = ingresoDatos$( "1=RTEFFE 2=IVA", "3=EXP Intro/Bor" )
   if left$( dato$, 1 ) = "A" then goto salir
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )
   if ( len(dato$) <> 1 ) or ( match(dato$,"123",1) = 0 ) then begin
      if jGuiSubState = 80511 then begin
         jGuiSubState = 80529 
      endif else begin
         jGuiSubState = 0 
      endif
      goto otrodato
   endif
   if REpideAnular% then begin
      if not existeRetencionRET( val( dato$ ) ) then begin
         call muestraBorrar( "NO EXISTEN RETENCION", "TIPO [" + dato$ + "] PARA ANULAR" )
         exit function
      endif
   endif \   
   else begin
      if existeRetencionRET( val( dato$ ) ) then begin
         call muestraBorrar( "YA EXISTE RETENCION", "TIPO [" + dato$ + "] EN TRX" )
         exit function
      endif
   endif 
   REtipoRet% = val( dato$ )
   
!call TraceRET("REpideTipoRetencion REtipoRe=[" + str$(REtipoRet%) + "]")
   salta:
   REpideTipoRetencion = -1
   salir:
   exit function
   erroringreso:
   call muestraBorrar( "ERROR EN INGRESO", "TIPO RETENCION" )
   goto salir
   
end function

function REpideNumeroDocumento
   integer*1 REpideNumeroDocumento, \
             opc                    !
   string dato$

!call TraceRET("REpideNumeroDocumento_IN")
   
   REpideNumeroDocumento = 0
   REnumDocumento$       = ""
   opc = 1

otrodato:

   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80510
   endif

   call javaEvent(terminalSubStateMsg)
   dato$ = ingresoDatos$( "NUMERO DE DOCUMENTO", "ORIGEN Intro/Bor" )
   if left$( dato$, 1 ) = "A" then goto salir
   if left$( dato$, 1 ) <> "P" then goto erroringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )
   if ( len(dato$) <> 15 ) then begin
      call muestraBorrar( "NUMERO DOCUMENTO", "NO VALIDO" )
      if opc = 3 then exit function
      opc = opc + 1

      if jGuiSubState = 80510 then begin
         jGuiSubState = 80527 
      endif else begin
         jGuiSubState = 0
      endif
      goto otrodato
   endif

   !valida numero documento en anulacion   
   if REpideAnular% then begin
      if REenTransaccion$(REtipoRet%,2) <> dato$ then begin
         call muestraBorrar( "NRO. DOCTO NO VALIDO", "-->[" + REenTransaccion$(REtipoRet%,2) + "]" )
         if opc = 3 then exit function
         opc = opc + 1
         goto otrodato
      endif
   endif

   REnumDocumento$ = dato$
   
!call TraceRET("REpideNumeroDocumento REnumDocumento$=[" + REnumDocumento$ + "]")   
   
   salta:
   REpideNumeroDocumento = -1

salir:
   exit function

erroringreso:
   call muestraBorrar( "ERROR EN INGRESO", "DOCUMENTO" )
   resume salir
end function

function REcalculaRetencion
   integer*1 REcalculaRetencion
   integer*4 REmontoImpuesto,         \
             REbase0,                 \
             REbase1,                 \
             REbase2                  !
   
   REcalculaRetencion = 0

!call TraceRET("REcalculaRetencion REpideAnular%=[" + str$(REpideAnular%) + "]")
   
   if REpideAnular% then goto salta
   
   REbase0 = 0
   REbase1 = 0
   REbase2 = 0
   
   call tarifasConDescuento(REbase0, REbase1, REbase2)
   
   REmontoAfectoRetencion = 0
   
        if REtipoRet% = 1 then REmontoAfectoRetencion = REbase0 +  REbase1 \
   else REmontoAfectoRetencion = REbase2
   
   if REmontoAfectoRetencion = 0 then begin
      call muestraBorrar( "ERROR MONTO AFECTO A", "RETENCION = CERO" )
      exit function
   endif

   REmontoRetencion% = ( 1.0 * REmontoAfectoRetencion * ( REtipoRetencion(REtipoRet%,2)/100.0 ) ) / 100.0

!call TraceRET("REcalculaRetencion REmontoRetencion%=[" + str$(REmontoRetencion%) + "]")

   salta:
   REcalculaRetencion = -1
   
end function

function REinyectaPagoRetencion
   integer*1 REinyectaPagoRetencion

!call TraceRET("REinyectaPagoRetencion REtipoRet%=[" + str$(REtipoRet%) + "] REpideAnular%=[" + str$(REpideAnular%) + "]") 

   REinyectaPagoRetencion = 0 

   if REpideAnular% then begin
      TS.IO.KEYS(1) = 70
      call muestraOperador( "ANULANDO PAGO","RETENCION [" + str$(REtipoRet%) + "]")
      TS.IO.DATA$(7) = str$(REmontoRetencion%)
   endif
      
   TS.IO.MOTORKEY = 91
   TS.IO.KEYS(3)  = 78
   TS.IO.DATA$(3) = str$( REtipoRet% + 2 )
   TS.IO.KEYS(7)  = 91
   
   if REpideAnular% then begin
      TS.IO.KEYS(1) = 70
      call muestraOperador( "ANULANDO PAGO","RETENCION [" + str$(REtipoRet%) + "]")
      TS.IO.DATA$(7) = REenTransaccion$(REtipoRet%,3)
      TS.IO.DATA$(9) = REenTransaccion$(REtipoRet%,2) + right$("00" + str$(REtipoRet%),2)
   endif \
   else begin 
      TS.IO.DATA$(7) = str$(REmontoRetencion%)
      TS.IO.DATA$(9) = right$("0000000000000000" + REnumDocumento$,16) + right$("00" + str$(REtipoRet%),2) !        
   endif

   REinyectaPagoRetencion = -1
   REesPagoretencion% = -1
   
end function

sub acumularetencionRE
    string tip$,doc$,mon$

    tip$ = ""
    doc$ = ""
    mon$ = ""

!call TraceRET("acumularetencionRE REpideAnular%=[" + str$(REpideAnular%) + "] REtipoRet%=[" + str$(REtipoRet%) + "]")

    if not REpideAnular% then begin    
       tip$ = str$(REtipoRet%)
       doc$ = REnumDocumento$
       mon$ = str$(REmontoRetencion%)
    endif

    REenTransaccion$(REtipoRet%,1) = tip$
    REenTransaccion$(REtipoRet%,2) = doc$
    REenTransaccion$(REtipoRet%,3) = mon$
    REpideAnular% = 0

!call TraceRET("acumularetencionRE tip$=[" + tip$ + "] doc$=[" + doc$ + "] mon$=[" + mon$ + "]")

end sub

sub MSMTRETE02 public
   
   if not estado then exit sub
   REidCliente$     = ""
   REtipoRet%       = 0
   REmontoRetencion% = 0
   dim REenTransaccion$(3,3)
   REpideAnular%    = 0

   if flagrete = -1 then begin
   if java.init = -1 then begin
      jGuiSubState = 80602
      call javaEvent(terminalSubStateMsg)
   endif
   endif
   flagrete = 0

end sub

!------------------------------------------------------------------------------
!Lectura de parametros desde archivo c:/ADX_UDT1/PARGENER.DAT
!incorporar a USER 7
!------------------------------------------------------------------------------
sub MSMTRETE07 public
   integer*1 k
   integer*4 inicioBloque
   string    v$,m$

   fechaModificacion  = " 22 Abr 2022"
   estado             = 0                        ! Indicador de estado funcionalidad
   depurar            = 0                        ! Indicador de Debug
   dummy              = 0                        ! Indicador de Dummy
   REteclaMotora      = 0                        ! Tecla motora para Retencion
   m$                 = ""
   flagrete          = 0
   dim REenTransaccion$(3,3)                      ! Se dimenciona matrix con retenciones para la trx    
   
   call imprimeCJ( "***** MODULO RETENCIONES   ***********" )

   inicioBloque = abreParametros( "R::$ARGENER", "[RETENCIONES]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE RETENCION", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO" + fechaModificacion )
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
         
         dim REtipoRetencion(3,2)
         teclaMotoraTef = getteclaMotoraTef
         medioPagoTef   = get.MP.Tef 
         
         !tecla motora para retenciones
         v$ = leeParametro$( "TECLA_MOTORA_RETENCION", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then REteclaMotora = val( v$ )
         endif

         !Pago supermarket para retencion de la fuente
         v$ = leeParametro$( "TIPO_VARIEDAD_PAGO_RTEFTE", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then begin
               REtipoRetencion(1,1) = val( v$ )
               m$ = v$ + "|"
            endif   
         endif
         
         !% para retencion de la fuente
         v$ = leeParametro$( "PTG_COBRO_RTEFTE", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then REtipoRetencion(1,2) = val( v$ )
         endif
         
         !Pago supermarket para retencion de IVA
         v$ = leeParametro$( "TIPO_VARIEDAD_PAGO_RTEIVA", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then begin
               REtipoRetencion(2,1) = val( v$ )
               m$ = m$ + v$ + "|"
            endif
         endif
         
         !% para retencion de IVA
         v$ = leeParametro$( "PTG_COBRO_RTEIVA", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then REtipoRetencion(2,2) = val( v$ )
         endif

         !Pago supermarket para retencion EXP
         v$ = leeParametro$( "TIPO_VARIEDAD_PAGO_RTEEXP", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then begin
               REtipoRetencion(3,1) = val( v$ )
               m$ = m$ + v$ + "|"
            endif   
         endif
         
         REpagosSAretencion$ = m$
         
         !% para retencion de EXP
         v$ = leeParametro$( "PTG_COBRO_RTEEXP", 80, inicioBloque )
         if v$ <> "" then begin
            if esNumero( v$ ) then REtipoRetencion(3,2) = val( v$ )
         endif

         !medios de pagos no permitidos con retencion
         v$ = leeParametro$( "PAGOS_NO_PERMITIDOS", 80, inicioBloque )
         if v$ <> "" then REpagosNoPermitidos$ = v$ + ";"

      endif

      call cierraParametros( 80 )

      if estado then v$ = "ON " else v$ = "OFF"
      call imprimeCJ( "ESTADO " + v$ + fechaModificacion )
      if depurar then begin
         call imprimeCJ( "TECLA MOTORA RETENC " + right$( string$( 10, " " ) + str$( REteclaMotora ), 10 ) )
         call imprimeCJ( "MEDIO PAGO_RET FTE  " + right$( string$( 10, " " ) + str$( REtipoRetencion(1,1) ), 10 ) )
         call imprimeCJ( "% RET FUENTE        " + right$( string$( 10, " " ) + str$( REtipoRetencion(1,2) ), 10 ) )
         call imprimeCJ( "MEDIO PAGO_RET IVA  " + right$( string$( 10, " " ) + str$( REtipoRetencion(2,1) ), 10 ) )
         call imprimeCJ( "% RET IVA           " + right$( string$( 10, " " ) + str$( REtipoRetencion(2,2) ), 10 ) )
         call imprimeCJ( "MEDIO PAGO_RET EXP  " + right$( string$( 10, " " ) + str$( REtipoRetencion(3,1) ), 10 ) )
         call imprimeCJ( "% RET EXP           " + right$( string$( 10, " " ) + str$( REtipoRetencion(3,2) ), 10 ) )
         call imprimeCJ( "PAGOS NO PERMITIDOS " + left$(REpagosNoPermitidos$, 18 ) )
         call imprimeCJ( "PAGOS NO PERMITIDOS " + right$(REpagosNoPermitidos$, 18 ) )
         call imprimeCJ( "ACTIVADO DEPURACION RETENCION" )
         if dummy then call imprimeCJ( "ACTIVADA DUMMY" )
      endif
   endif

end sub

!-----------------------------------------------------------
! detecta la secuencia de teclas de pago con NC 
!-----------------------------------------------------------
function esSecuenciaTeclaPagoNC
   integer*1 esSecuenciaTeclaPagoNC
   integer*4 t0, t1, t2
   string monto$

   esSecuenciaTeclaPagoNC = 0

   if TS.IO.STATE = 10 then begin
      if ( TS.IO.MOTORKEY = 95  ) and ( TS.IO.KEYS(7)  = 95  ) then begin
         esSecuenciaTeclaPagoNC = -1
      endif
   endif

end function

sub MSMTRETE14 public

   !Impedir que ingrese por teclado las formas de pago retencion (3,4,5 / monto EFECTIVO) 
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 91 ) and \
      ( TS.IO.KEYS(3) = 78 ) and \
      ( ( TS.IO.DATA$(3) = "3" ) OR ( TS.IO.DATA$(3) = "4" ) OR ( TS.IO.DATA$(3) = "5" ) ) and \
      ( TS.IO.KEYS(7) = 91 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
   endif

   if not estado then exit sub

   if RetencionIntrxRET <> 0 and secuenciaTeclaCliente then begin
      call muestraBorrar( "NO PERMITIDO CLIENTE", "RETENCION ACTIVA" )
      call ignoraTeclas
      exit sub
   endif

   if ( RetencionIntrxRET <> 0 ) and esSecuenciaTeclaPagoNC then begin
      call muestraBorrar( "NO PERMITIDO PAGO NC", "RETENCION ACTIVA" )
      call ignoraTeclas
      exit sub
   endif

   if java.init = -1 then begin
      if RetencionIntrxRET <> 0 then begin
         flagrete= -1
         jGuiSubState = 80601
         call javaEvent(terminalSubStateMsg)
      endif else begin 
         if flagrete= -1 then begin
            jGuiSubState = 80602
            call javaEvent(terminalSubStateMsg)
         endif
      endif
   endif
   if REesSecuenciaPagoTarjeta then begin
      if RetencionIntrxRET = 0 then exit sub \
      else begin
         call muestraBorrar( "NO PERMITIDO TARJETA", "CON RETENCIONES" )
         goto salir
      endif
   endif \
   else begin
      if not REesTeclaRetencion        then exit sub
      if not REcondicionesRetencion    then goto salir
      if not REpideTipoRetencion       then goto salir
      if not REpideNumeroDocumento     then goto salir
      if not REcalculaRetencion        then goto salir
      if not REinyectaPagoRetencion    then goto salir
      exit sub
   endif
salir:
call ignoraTeclas
end sub

sub MSMTRETE22 public
   integer*1 x
   string f$
   if not estado then exit sub

   while( TS.DEV.FLAG = 0 )
      call TSHIEC04
      if not TS.DEV.FLAG then begin
         call muestraVisor(1,"B049 INSERTE DOCU-", "MENTO A FRANQUEAR" )
      endif
   wend

   if ( TS.LINETYPE = 9 ) and ( TS.LINEDATA = REtipoRet% + 3 ) then begin
      if JAVA.INIT = -1 AND TS.IO.MOTORKEY = 81 and ( TS.IO.DATA$(3) = str$(REtipoRet% + 2 ) ) then begin
         TS.IO.MOTORKEY = 91
      endif
      if ( TS.IO.MOTORKEY = 91 ) and ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = str$(REtipoRet% + 2 ) ) then begin
         x = match("Base de Retencion:",TS.PRTBUF$,1)
         if x <> 0 then begin
            f$ = formatea2Dec(REmontoAfectoRetencion)
            f$ = "          Base de Retencion:" + right$("              " + f$,14) + "   "
            TS.PRTBUF$ = f$
         endif
      endif
   endif
   
end sub

sub MSMTRETE30 public

   on error goto errLoc

!call TraceRET("MSMTRETE30-1" )

   if not estado then exit sub

   if REesPagoRetencion% then begin
!call TraceRET("MSMTRETE30-3")
      call acumulaRetencionRE
      REesPagoRetencion% = 0
   endif   
   
sale:
   
   exit sub
   
errLoc:
if errf% = 39 then resume
call TraceRET("MSMTRETE30 Error " + err + hexanum$(errn) + " Ses: " + str$( errf% ))
resume sale
end sub

sub MSMTRETE53 public

   on error goto err.loc
   
   if not estado then exit sub

!call TraceRET("MSMTRETE53")

   ! Detectar cadena 05 y 06 pago y anulacion de pago
   if not EsCadenaPago( SL.STR.ENTRY$ )                       then exit sub
   if match(NUOuserData$(2) + "|", REpagosSAretencion$,1) = 0 then exit sub
   TS.TEMP1I2 = 0

sale:   
   exit sub
   
err.loc:
if errf% = 39 then resume
call TraceRET("MSMTRETE53 Error " + err + hexanum$(errn) + " Ses: " + str$( errf% ))
resume sale
end sub
