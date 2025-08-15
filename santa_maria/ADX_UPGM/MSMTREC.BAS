!------------------------------------------------------------------------------
! Modulo de Recargas Electronicas.
! Fecha: 20 de Septiembre de 2021.
! Autor: Alexander Baidal M. - NUOCORP.
! History: 14-02-2022 Carlos Calderon se implementa mensajeria para comunicacion on line       
!------------------------------------------------------------------------------
%ENVIRON T

integer*1 activo,                \ Indicador de proyecto activo
          depurar,               \ Indicador de depuracion
          Dummy,                 \ Indicador de autorespuesta activo
          RECindiceOperador%,    \ indice del operador de recarga en curso
          RECindicCliente%,      \ Indicadores del cliente ingresado
          RECoperation,          \ Operacion de Recarga 70
          REClargoNumero%,       \ cantidad de digitos del numero del celular
          ocultaErrorSMA,        \ Indica si se oculta el error nativo del SMA
                                 \  
          cantLinVouRecCLA%,     \ Tamano de arreglo con lineas de informacion del formato de voucher de recargas CLARO
          cantLinVouRecMOV%,     \ Tamano de arreglo con lineas de informacion del formato de voucher de recargas MOVISTAR
          cantLinVouRecDTV%,     \ Tamano de arreglo con lineas de informacion del formato de voucher de recargas DIRECTV
          cantLinVouRecTOU%      ! Tamano de arreglo con lineas de informacion del formato de voucher por Time Out
                    
string    RECmsg$,               \ Mensaje consumo de recarga del POS para el agente
          RECheader$,            \ Header del mensaje
          RECpatner$,            \ Secuencial unico generador por el POS
          RECaditional$,         \ Datos compania mas numero telefonico
          RECnumeroRecarga$,     \ Numero a recargar
          RECmontoRecarga$,      \ Monto a recargar
          RECproductoId$(1),     \ Arreglo con informacion de Producto Id
          RECoperadorId$(1),     \ Arreglo con informacion de operador Id
                                 \
          RECtxld$,              \ Numero de autorizacion
          RECmobileOper$,        \ Id transaccion operadora
          RECrespCode$,          \ Codigo repuesta 0=Autorizado <> 0 Error  2=Time Out
          RECcadena99$,          \ Cadena 99 55 recarga
          RECidCliente$,         \ Id del cliente
          fechaModificacion,     \ Fecha ultima modificacion al modulo
                                 \
          forVouRecCLA$(1),      \ Arreglo con informacion formato voucher recargas CLARO
          forVouRecMOV$(1),      \ Arreglo con informacion formato voucher recargas MOVISTAR
          forVouRecDTV$(1),      \ Arreglo con informacion formato voucher recargas DIRECTV
          forVouRecTOU$(1),      \ Arreglo con informacion formato voucher recargas Time Out
                                 \
          msgOperacion$(1),      \ Arreglo con informacion de mensajes operativos
          parVouRecarga$(1),     \ Arreglo con informacion de valores del voucher de recargas
          recargas$(2),          \ Arreglo con informacion de recargas realizadas
          msg1$,                 \ Mensaje a mostrar en linea 1 de visor
          msg2$,                 \ Mensaje a mostrar en linea 2 de visor
          RECoperadores$,        \ Operadores activos
          pagosRestringidos$,    \ Medios de pago habilitados para recargas
          lineaDatosTrx$,        \ Linea con fecha hora tienda caja transaccion operador
          separador
            
integer*1 cantOperadores%,       \ Tamano de arreglo de operadores 
          cantRecargas%,         \ Cantidad de recargas    
          cantParVouRecarga%,    \ Cantidad de parametros a imprimir en voucher
          cantCopiasVoucher%,    \ Cantidad de vouchers a imprimir
          cantMaxRecargas%,      \ Cantidad maxima de recargas
          operador               \ Codigo de operador 
  
! Parametros por operador          
integer*1 opeCodigo%(1),         \ Codigo de Operador 1:CLARO 2:MOVISTAR 3:DIRECTV
          opeCantMontoFijo%(1),  \ Cantidad de montos fijos habilitados
          opeDecimales%(1),      \ Indica si el monto a recargar acepta decimales
          opeUnicaRecarga%(1)    \ Indica si solo se permite una recarga por transaccion

integer*4 opeMontoMinimo%(1),    \ Monto minimo a recargar  
          opeMontoMaximo%(1),    \ Monto maximo a recargar
          pipeTimeout%           ! Tiempo de espera time out
          
string    opePLU$(1),            \ PLU correspondiente a la recarga
          opeMontoFijo$(1),      \ Lista de montos fijos permitidos  
          opeNombre$(1)          \ Nombre de operador  
          
! Variables globales del SMA          
integer *4 global IR.PRICE1
integer *2 global SL.END         ! last index to be used in summary log

!------------------------------------------------------------------------------
! Variables globales de otros modulos
!------------------------------------------------------------------------------
%INCLUDE EAMTSWKG.J86                     ! supermarket appl wking storage
%INCLUDE EAMTRANS.J86                     ! global defines for tlog vars
%INCLUDE EAMITEMR.J86                     ! global defines for item record vars

!------------------------------------------------------------------------------
! Rutinas publicas de otros modulos
!------------------------------------------------------------------------------
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86
%INCLUDE MSMTCLIE.J86
%INCLUDE NUOTPROM.J86
%INCLUDE EAMASMCT.J86

!------------------------------------------------------------------------------
!Escribe log de auditoria
!m$ mensaje
!------------------------------------------------------------------------------
sub TraceRecarga( m$ )
   string m$
   if not depurar then exit sub
   call traceNuo(m$)
end sub

sub seteaClienteRecarga( cliId$, indic ) public
   integer*1 indic                        ! indicadores 
   string cliId$                          ! id cliente

   RECidCliente$   = cliId$
   RECindicCliente% = indic

end sub

!------------------------------------------------------------------------------
!Lectura de parametros del archivo c:/ADX_UDT1/PARGENER.DAT
!------------------------------------------------------------------------------
function leeOperadoresRecargas
  
   integer *1 i, l, k, w, leeOperadoresRecargas 
   integer*4 inicioBloque
   string n$, v$
   
   cantOperadores% = 0
   leeOperadoresRecargas = 0
   
   v$ = "[OPERADORES RECARGAS]"
   inicioBloque = abreParametros( "R::$ARGENER", v$ , 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO OPERADOR RECARGAS", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "OPERADORES DE RECARGA NO CONFIGURADO " + fechaModificacion )
      goto salir
   endif
   
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      !call TraceRecarga("leeOperadoresRecargas #=[" + str$(w) + "]")
      cantOperadores% = w
      dim opeCodigo%( w )
      dim RECproductoId$(w)
      dim RECoperadorId$(w)
      dim opeCantMontoFijo%( w )
      dim opeDecimales%( w )
      dim opeMontoMinimo%( w ) 
      dim opeMontoMaximo%( w )
      dim opePLU$( w )
      dim opeMontoFijo$( w )
      dim opeNombre$( w )
      dim opeUnicaRecarga%( w )
      
      for i = 1 to cantOperadores%
         call rescataParametro( i, n$, v$ )
         opeNombre$(i) = n$  
         if v$ <> "" then begin
!call TraceRecarga("leeOperadoresRecargas[" + str$(i) + "]=[" + v$ + "]")
            opeCodigo%(i)         = val( buscaCampo( v$, "|" , 1 ) )
            RECproductoId$(i)     = buscaCampo( v$, "|" , 2 )
            RECoperadorId$(i)     = buscaCampo( v$, "|" , 3 )
            opePLU$(i)            = buscaCampo( v$, "|" , 4 )
            opeMontoMinimo%(i)    = val( buscaCampo( v$, "|" , 5 ) )
            opeMontoMaximo%(i)    = val( buscaCampo( v$, "|" , 6 ) )
            if val( buscaCampo( v$, "|" , 7 ) ) = 1 then opeDecimales%(i)    = -1 else opeDecimales%(i)    = 0
            if val( buscaCampo( v$, "|" , 8 ) ) = 1 then opeUnicaRecarga%(i) = -1 else opeUnicaRecarga%(i) = 0
            opeCantMontoFijo%(i)  = val( buscaCampo( v$, "|" , 9 ) )
            opeMontoFijo$(i)      = trim$( buscaCampo( v$, "|" , 10 ) )
         endif
      next i
   endif
   
   leeOperadoresRecargas = -1
   
   salir:  
   call cierraParametros( 80 )
     
end function

!------------------------------------------------------------------------------
! Lee formato de voucher 
! archivo R::ADX_UDT1:PARVOUCH.DAT
!------------------------------------------------------------------------------
function leeFormatoVoucher

   integer *1 i, k, w, leeFormatoVoucher 
   integer*4 inicioBloque
   string n$, v$
  
   cantLinVouRecCLA% = 0
   cantLinVouRecMOV% = 0
   cantLinVouRecDTV% = 0
   cantLinVouRecTOU% = 0
  
   leeFormatoVoucher = 0
   cantParVouRecarga% = 6

   v$ = "[RECARGA CLARO]"
   inicioBloque = abreParametros( "R::$ARVOUCH", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      goto salir
   endif

   w = cargaLineas( 80, inicioBloque )
   if w > 0 then begin
      cantLinVouRecCLA% = w
      dim forVouRecCLA$( cantLinVouRecCLA% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouRecCLA$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   endif
   
   v$ = "[RECARGA MOVISTAR]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      goto salir
   endif
   
   w = cargaLineas( 80, inicioBloque )
   if w > 0 then begin
      cantLinVouRecMOV% = w
      dim forVouRecMOV$( cantLinVouRecMOV% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouRecMOV$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   endif

   v$ = "[RECARGA DIRECTV]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      goto salir
   endif
   
   w = cargaLineas( 80, inicioBloque )
   if w > 0 then begin
      cantLinVouRecDTV% = w 
      dim forVouRecDTV$( cantLinVouRecDTV% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouRecDTV$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   endif
   
   v$ = "[RECARGA TIMEOUT]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      goto salir
   endif
   
   w = cargaLineas( 80, inicioBloque )
!call TraceRecarga("leeFormatoVoucher w[" + str$(w) + "]")   
   if w > 0 then begin
      cantLinVouRecTOU% = w 
      dim forVouRecTOU$( cantLinVouRecTOU% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouRecTOU$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   endif
   
   leeFormatoVoucher = -1
   
   salir:
   call cierraParametros( 80 )
end function

!------------------------------------------------------------------------------
! Lee mensajes de operacion de tarjetas bancarias
! archivo R::ADX_UDT1:PARMSGOP.DAT
!------------------------------------------------------------------------------
function leeMensajesOperacion
   integer *1 i, k, w, leeMensajesOperacion
   integer*4 inicioBloque
   string n$, v$
   
   leeMensajesOperacion = 0
   v$ = "[RECARGAS ELECTRONICAS]"
   inicioBloque = abreParametros( "R::$ARMSGOP", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARMSGOP" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      goto salir
   endif
   
   w = cargaLineas( 80, inicioBloque )
   
!call TraceRecarga("leeMensajesOperacion CANT.MSG=[" + str$(w) + "]")
   
   if w > 0 then begin
      dim msgOperacion$( w )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          msgOperacion$( k ) = mid$( v$, 2, len(v$)-2 )
!call TraceRecarga("leeMensajesOperacion msgOperacion$(" + str$(k) + ")=["+ msgOperacion$( k ) + "]")
          k = k + 1
      next i
   endif
   
   leeMensajesOperacion = -1
   
   salir:      
   call cierraParametros( 80 )
   
end function

!------------------------------------------------------------------------------
! Obtiene mensaje a mostrar en visor
!------------------------------------------------------------------------------
sub obtieneMsgOperacion( k% )
   integer*2 k%

 ! 1 "NO SE PERMITE MEZCLADE PLUS     <Borrar>"
 ! 2 "INGRESE VALOR       DE LA RECARGA       "
 ! 3 "INGRESE NUMERO      A RECARGAR          "
 ! 4 "OPERADOR DE RECARGA INACTIVO    <Borrar>"
 ! 5 "MONTO MINIMO        INVALIDO    <Borrar>"
 ! 6 "MONTO MAXIMO        INVALIDO    <Borrar>"
 ! 7 "MONTO INGRESADO     INVALIDO    <Borrar>"
 ! 8 "NO SE PERMITE ANULARRECARGAS    <Borrar>"
 ! 9 "MEDIO DE PAGO NO    PERMITIDO   <Borrar>"
 !10 "NO SE PERMITE MAS   RECARGAS    <Borrar>"
 !11 "NO SE PERMITE       CENTAVOS    <Borrar>"
 !12 "NUMERO INGRESADO    NO COINCIDE <Borrar>"
 !13 "NO SE PERMITE       CANTIDAD    <Borrar>"
 !14 "NO SE PERMITE TECLA ANULAR      <Borrar>"
 !15 "AUTORIZADOR FUERA   DE LINEA    <Borrar>"
 !16 "CLIENTE NO PERMITIDOEN RECARGA  <Borrar>"
                    
  msg1$ = left$ ( msgOperacion$( k% ) , 20 )
  msg2$ = right$( msgOperacion$( k% ) , 20 )
end sub

!------------------------------------------------------------------------------
! Indica si el articulo es de recarga
!------------------------------------------------------------------------------
function articuloRecarga( plu )
   integer*1 articuloRecarga,     \
             i
   string plu

   articuloRecarga = 0
   RECindiceOperador% = 0
    
   for i=1 to cantOperadores%
       if opePLU$(i) = plu then begin
          operador = opeCodigo%(i)
          RECindiceOperador% = i
          articuloRecarga = -1
          i = cantOperadores%
       endif 
   next i
end function

!------------------------------------------------------------------------------
! Muestra mensaje de error para impedir venta de un articulo
!------------------------------------------------------------------------------
sub restringeVenta( m1$, m2$ )
   string m1$,               \
          m2$
   if m1$ <> "" or m2$ <> "" then call muestraBorrar( m1$, m2$ )
   IR.INDICAT0 = 04H
   ocultaErrorSMA = -1
end sub

!------------------------------------------------------------------------------
! Captura numero a recargar
!------------------------------------------------------------------------------
function capturaNumero
   integer*1 capturaNumero, f
   string d$, t1$, t2$ 

   capturaNumero     = 0  
   RECnumeroRecarga$ = ""
   t1$ =""
   t2$ =""  
  
   call obtieneMsgOperacion( 3 )
   f=1
   while f <= 2
     d$ = ingresoDatos$( msg1$ , msg2$ )
     if left$( d$, 1 ) = "P" then begin
        if len(d$)-2 <> REClargoNumero% then begin
           call obtieneMsgOperacion( 17 )
           call muestraBorrar( msg1$, msg2$ )
           call obtieneMsgOperacion( 3 )
        endif \
        else begin
           if f = 1 then t1$ = right$( d$, len(d$) - 2 )
           if f = 2 then begin
              t2$ = right$( d$, len(d$) - 2 )
              if t1$ = t2$ then begin
                 RECnumeroRecarga$ = t2$
                 if RECnumeroRecarga$ <> "" then capturaNumero = -1
              endif else begin
                 call obtieneMsgOperacion( 12 )
                 call restringeVenta( msg1$, msg2$ )
              endif
           endif
        endif
     endif	
     f = f + 1
   wend
end function

!------------------------------------------------------------------------------
! Indica si el monto es entero
!------------------------------------------------------------------------------
function montoEntero( monto$ )
   integer *1  montoEntero
   Integer *4  montoPaso
   string      monto$, m$
   real        montoRecargaReal
   
   montoEntero = 0
   montoPaso = val( monto$ )
   montoRecargaReal = montoPaso / 100
   montoPaso  = int(montoRecargaReal)
   m$ = str$( montoPaso ) + string$( 2, "0" )
   if m$ = monto$ then montoEntero = -1
   
   !call TraceRecarga("montoEntero monto$=["+ monto$ + "]" + "montoEntero=["+ m$ + "]")
end function

!------------------------------------------------------------------------------
! Valida monto a recargar
!------------------------------------------------------------------------------
function validaMonto
   integer*1 validaMonto, \
             k
   string d$ 

   validaMonto = 0  
   k = 0
  
!call TraceRecarga("validaMonto opeMontoMinimo%=["+ str$(opeMontoMinimo%(operador)) + "]" + \
                                "opeMontoMaximo%=["+ str$(opeMontoMaximo%(operador)) + "]" + \
                                "opeCantMontoFijo%=["+ str$(opeCantMontoFijo%(operador)) + "]" + \
                                "opeMontoFijo$=["+ opeMontoFijo$(operador) + "]" + \
                                "opeDecimales%=["+ str$(opeDecimales%(operador)) + "]")             
  
   ! Validacion de monto minimo
   if opeMontoMinimo%( operador ) > 0 and val( RECmontoRecarga$ ) < opeMontoMinimo%( operador ) then begin
      call obtieneMsgOperacion( 5 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
  
   ! Validacion de monto maximo
   if opeMontoMaximo%( operador ) > 0 and val( RECmontoRecarga$ ) > opeMontoMaximo%( operador ) then begin
      call obtieneMsgOperacion( 6 )    
      call restringeVenta( msg1$, msg2$ )
      exit function       
   endif
  
   ! Validacion de monto fijo
   if opeCantMontoFijo%( operador ) > 0 then begin
      d$ = right$( string$( 5, "0" ) + RECmontoRecarga$ , 5 )
      k = match( d$ , opeMontoFijo$( operador ) , 1 ) 
      if k=0 then begin
         call obtieneMsgOperacion( 7 )    
         call restringeVenta( msg1$, msg2$ )
         exit function
      endif
   endif
  
   ! Validacion de decimales
   if not opeDecimales%( operador ) and not montoEntero( RECmontoRecarga$ ) then begin
      call obtieneMsgOperacion( 11 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
  
   validaMonto = -1
end function

!------------------------------------------------------------------------------
! Captura monto a recargar
!------------------------------------------------------------------------------
function capturaMonto
   string d$ 
   integer*1 capturaMonto

   capturaMonto = 0  
   RECmontoRecarga$ = ""
   call obtieneMsgOperacion( 2 )
   d$ = ingresoDatos$( msg1$ , msg2$ )
   if left$( d$, 1 ) = "P" then begin
      RECmontoRecarga$ = right$( d$, len(d$) - 2 )
      if RECmontoRecarga$ <> "" then begin
         if validaMonto then capturaMonto = -1
     endif
   endif	
end function

!------------------------------------------------------------------------------
!Genera el encabezado
!------------------------------------------------------------------------------
sub generaEncabezadoREC
    RECheader$ = ""
    RECheader$ = "P"          + separador + \ canal de comunicacion S=socket P=Pipe
         str$( RECoperation ) + separador + \ requerimiento 70
                 "0000"       + separador + \ canal de respuesta
                 TS.TERMINAL$ + separador + \ terminal de origen
                 "0"          + separador + \ tipo de conexion 0=temporal 1=permanente
                 "20" + date$ + time$       ! fecha y hora actual
end sub

!------------------------------------------------------------------------------
!Genera campo de mensajeria Patner o secuencial generado por el POS
!------------------------------------------------------------------------------
sub generaPatnerREC
    RECpatner$ = ""
    RECpatner$ = date$                                       + \ fecha aammdd
                 right$("000"  + TS.STORE$, 3 )              + \ tienda
                 right$("000"  + TS.TERMINAL$, 3)            + \ terminal o POS
                 right$("0000" + str$(SL.HD.TRANSNUM + 1),4)   ! transaccion SMA
end sub   

!------------------------------------------------------------------------------
!Genera campo de mensajeria Aditional data
!------------------------------------------------------------------------------
sub generaAditionalREC
    RECaditional$ = ""
    RECaditional$ = RECproductoId$( RECindiceOperador% )           + \ producto ID ej REC_MOV
                    right$("000000000000" + RECnumeroRecarga$, 12) + \ numero telefonico
                    RECoperadorId$( RECindiceOperador% )             ! operador ID ej MOV 
end sub

!------------------------------------------------------------------------------
! Genera autorespuesta de mensajeria
!------------------------------------------------------------------------------
function generaAutorespuestaREC$
   string v$,                       \
          generaAutorespuestaREC$   !
   
   generaAutorespuestaREC$ = "00118P'|'70'|'0000'|'042'|'0'|'20220202180507'|'00250000'|'00006548711111111111'|'0000008618'|'0010099002'|'0000006558'|'0"
   
   RECtxld$       = buscaCampo( v$, separador ,  9 )    ! 0000008618
   RECmobileOper$ = buscaCampo( v$, separador , 11 )    ! 0000006558
   RECrespCode$   = buscaCampo( v$, separador , 12 )    ! 0
end function

!------------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------------
function recibeRespuesta
   string recibeRespuesta, m$
 
   recibeRespuesta = ""
   m$ = ""
  
   m$ = recibeMensaje("servicio recargas..") 
   if buscaCampo(m$, separador, 12) <> "0" then begin
      call muestraVisor( 1, "Error en servicio", buscaCampo(m$, separador, 8) )
      call TraceRecarga("recibeRespuesta m=[" + m$ + "]")
      goto salir
   endif

   recibeRespuesta = m$

   salir:
   call restauraVisor(1)

end function

sub RECsplitRespuesta( v$, opc% )
   integer*1 opc%
   string v$
   
   RECtxld$       = " "
   RECmobileOper$ = " "
   RECrespCode$   = "2"         ! valor por defecto para time out
   
!call TraceRecarga( "RECsplitRespuesta v$=[" + v$ + "]" )
   
   if opc% then begin
      RECtxld$       = buscaCampo( v$, separador ,  9 )    ! 0000008618
      RECmobileOper$ = buscaCampo( v$, separador , 11 )    ! 0000006558
      RECrespCode$   = buscaCampo( v$, separador , 12 )    ! 0
   endif

end sub 

!------------------------------------------------------------------------------
! Envia mensajeria para comunicacion con agente
!------------------------------------------------------------------------------
function enviaMensajeria
   integer*1 enviaMensajeria, opc%
   string m$

   m$ = RECmsg$

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   ! Envia el mensaje
   if not Dummy then begin
      if not enviaMensaje( m$ ) then begin
         call muestraBorrar( "NO SE ENVIO MENSAJE", "AL AGENTE" )
         exit function
      endif
   endif

   opc% = -1
   if dummy and RECnumeroRecarga$ = "0983153946" then begin
      m$ = generaAutoRespuestaREC$
      goto salta
   endif

   ! Espera que lleguen datos
   if not esperaPipe( pipeTimeout%, "Recarga" ) then begin
      call TraceRecarga( "enviaMensajeria NO SE RECIBE RESPUESTA RECARGA" )
      opc% = 0
      goto sinRespuesta
   endif

   m$ = recibeRespuesta

!call TraceRecarga("enviaMensajeria m$=[" + m$ + "]")

salta:

   if m$ = "" then begin
      call TraceRecarga("enviaMensajeria RESPUESTA LARGO 0")
      exit function
   endif

   sinRespuesta:

   call RECsplitRespuesta( m$, opc% )

   salir:
   enviaMensajeria = -1

end function

!------------------------------------------------------------------------------
! Valida reglas de negocio
!------------------------------------------------------------------------------
function validaReglas
integer*1 k, validaReglas

   validaReglas = 0
   k = 0

   !Impide ingresar recarga porque ya existe un RUC ingresado
   if RECidCliente$ <> "" then begin
      call obtieneMsgOperacion( 18 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif

   ! Ingreso de cantidad no permitido
   if val(TS.IO.DATA$(6)) > 1 then begin
      call obtieneMsgOperacion( 13 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
   
   ! Tecla anulacion
   if TS.IO.KEYS(1) = 70 then begin
      call obtieneMsgOperacion( 14 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
   
   ! Si ya hay articulos vendidos
   if ( MSMTFACTtotal - NUOTPROMdescuento ) > 0 and opeUnicaRecarga%( operador ) then begin
      call obtieneMsgOperacion( 1 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
   
   ! Si operador no esta activo
   k = match( str$(operador) , RECoperadores$ , 1 ) 
   if k=0 then begin
      call obtieneMsgOperacion( 4 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
   
   ! Si supera cantidad maxima de recargas
   if cantRecargas% > cantMaxRecargas% then begin
      call obtieneMsgOperacion( 10 )    
      call restringeVenta( msg1$, msg2$ )
      exit function
   endif
   
   validaReglas = -1
   
end function

!------------------------------------------------------------------------------
! Graba informacion de cadena de usuario 99 55 en TLOG
!------------------------------------------------------------------------------
sub generaCadenaUsuario
   
   RECcadena99$ = pack$( "99" )              + ":" + \ 
                  pack$( "55" )              + ":" + \ 
                  pack$( str$(operador) )    + ":" + \ 
                  pack$( RECpatner$ )        + ":" + \      
                  pack$( RECtxld$ )          + ":" + \ 
                  pack$( RECmobileOper$ )    + ":" + \ 
                  pack$( RECnumeroRecarga$ ) + ":" + \ 
                  pack$( RECmontoRecarga$ )          !

end sub

!----------------------------------------------------------------------------------------------------------------
! Acumula recargas exitosas
!----------------------------------------------------------------------------------------------------------------
sub acumulaRecargas

   cantRecargas% = cantRecargas% + 1 
   recargas$( cantRecargas%, 0 ) = str$( operador )
   recargas$( cantRecargas%, 1 ) = RECpatner$
   recargas$( cantRecargas%, 2 ) = RECtxld$
   recargas$( cantRecargas%, 3 ) = RECmobileOper$
   recargas$( cantRecargas%, 4 ) = RECnumeroRecarga$
   recargas$( cantRecargas%, 5 ) = RECmontoRecarga$
   recargas$( cantRecargas%, 6 ) = RECrespCode$

!call TraceRecarga("acumulaRecargas=[" + str$(cantRecargas%) + "]" )
   
end sub

function generaMensajeREC
   string    generaMensajeREC,            \
             m$
   
   generaMensajeREC = ""
   call generaEncabezadoREC                                     ! RECheader$ encabezado
   call generaPatnerREC                                         ! RECpatner$ secuencial
   call generaAditionalREC                                      ! RECaditional$ producto numero operador
   m$ = RECheader$        + separador + \
        "1"               + separador + \                       !
        right$("000000" + RECmontoRecarga$,8)  + separador + \
        RECpatner$        + separador + \
        RECaditional$

!call TraceRecarga("generaMensajeREC m$=[" + m$ + "]")

   generaMensajeREC = m$

end function

!------------------------------------------------------------------------------
! Procesa transaccion de recarga celular
!------------------------------------------------------------------------------
sub procesaTransaccion
    
    call muestraOperador("PROCESANDO RECARGA", opeNombre$( operador ) + ", ESPERE..")
    wait; 1000
    call restauraDisplay
    
    RECmsg$ = generaMensajeREC
    
!call TraceRecarga("procesaTransaccion RECmsg$=[" + RECmsg$ + "]")
    
    if not enviaMensajeria then begin
       call restringeVenta( "", "" )
       exit sub
    endif 
   
    !Reemplaza precio 
    IR.PRICE1 = VAL( RECmontoRecarga$ ) 

    call acumulaRecargas
    call generaCadenaUsuario

end sub

!------------------------------------------------------------------------------
! Procesa operacion recarga celular
!------------------------------------------------------------------------------
sub procesaRecarga
integer*1 capturaDatos
 
    capturaDatos = 0
    
    if not articuloRecarga( SL.IT.ITEMCODE$ ) then begin
       if cantRecargas% > 0 then begin
          call obtieneMsgOperacion( 1 )    
          call restringeVenta( msg1$, msg2$ )
       endif
       exit sub
    endif

    if not validaReglas  then exit sub
    if not capturaNumero then goto salir
    if not capturaMonto  then goto salir  

    capturaDatos = -1
  
    salir:
    if not capturaDatos then begin 
       call restringeVenta( "", "" )
       exit sub
    endif

    call procesaTransaccion

end sub

!------------------------------------------------------------------------------
! Carga Valores Voucher
!------------------------------------------------------------------------------
sub cargaValoresVoucher( index% )
   integer*1 index%

   dim parVouRecarga$(cantParVouRecarga%)

    operador = val( recargas$( index%, 0 ) )
    parVouRecarga$(1) = recargas$( index%, 1 )
    parVouRecarga$(2) = recargas$( index%, 2 )
    parVouRecarga$(3) = recargas$( index%, 3 )
    parVouRecarga$(4) = recargas$( index%, 4 )
    parVouRecarga$(5) = formateaMonto( val( recargas$( index%, 5 ) ) )
    parVouRecarga$(6) = recargas$( index%, 6 )

end sub

!------------------------------------------------------------------------------
! Asigna los valores a imprimir en el voucher
!------------------------------------------------------------------------------
function reemplazaLineaVoucher( lv$ )
   integer*2 i, k, n
   string reemplazaLineaVoucher,  \
      lv$, s$, v$

   n = len( lv$ )
   i = match( "&" , lv$, 1 )
   while i > 0
      s$ = mid$( lv$, i + 1, 2 )
      if esNumero( s$ ) then begin
         k = val( s$ )
         if ( k > 0 ) and ( k < cantParVouRecarga% ) then begin
            v$ = parVouRecarga$( k )
         endif else begin
            v$ = ""
         endif
         if ( mid$( lv$, i + 3, 1 ) = "-" ) and ( v$ <> "0.00" ) then s$ = "-" else s$ = " "
         lv$ = left$( lv$, i - 1 ) + v$ + s$ + mid$( lv$, i + 4, n )
      endif else begin
         lv$ = left$( lv$, i - 1 ) + "    " + mid$( lv$, i + 4, n )
      endif
      i = match( "&" , lv$, i )
   wend

   reemplazaLineaVoucher = lv$

end function

!------------------------------------------------------------------------------
! Imprime vouchers de recargas
!------------------------------------------------------------------------------
sub imprimeVoucher
   integer*1 index%, i%, k%, cantLin%
   string lineaVoucher, dataLin$

!call TraceRecarga("imprimeVoucher cantRecargas%=[" + str$(cantRecargas%) + "]")
     
    for index% = 1 to cantRecargas%
        call cargaValoresVoucher( index% )
        for i% = 1 to cantCopiasVoucher%
        if operador = 1 then begin
           if parVouRecarga$(6) = "2" then cantLin% = cantLinVouRecTOU% \
           else cantLin% = cantLinVouRecCLA%
           for k% = 1 to cantLin%
               if parVouRecarga$(6) = "2" then dataLin$ = forVouRecTOU$( k% ) \
               else dataLin$ = forVouRecCLA$( k% )              
               lineaVoucher = reemplazaLineaVoucher( dataLin$ )
               call imprimeCJ( lineaVoucher ) 
           next k%
        endif \
        else if operador = 2 then begin
           if parVouRecarga$(6) = "2" then cantLin% = cantLinVouRecTOU% \
           else cantLin% = cantLinVouRecMOV%
           for k% = 1 to cantLin%
               if parVouRecarga$(6) = "2" then dataLin$ = forVouRecTOU$( k% ) \
               else dataLin$ = forVouRecMOV$( k% ) 
               lineaVoucher = reemplazaLineaVoucher( dataLin$ )
               call imprimeCJ( lineaVoucher ) 
           next k%
        endif \
        else if operador = 3 then begin
           if parVouRecarga$(6) = "2" then cantLin% = cantLinVouRecTOU% \
           else cantLin% = cantLinVouRecDTV%
           for k% = 1 to cantLin%
               if parVouRecarga$(6) = "2" then dataLin$ = forVouRecTOU$( k% ) \
               else dataLin$ = forVouRecDTV$( k% ) 
               lineaVoucher = reemplazaLineaVoucher( dataLin$ )
               call imprimeCJ( lineaVoucher ) 
           next k%
        endif
        call imprimeCJ( lineaDatosTrx$ )
        call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) ) 
        call imprime34( NUO.CortaPapel$ )
        next i%  
    next index%
        
end sub

!------------------------------------------------------------------------------
! Recupera Recarga
!------------------------------------------------------------------------------
sub recuperaRecarga
   string dato$
   
   dato$ = getCampoTSL$( SL.STR.ENTRY$, 1 )
   
   if dato$ = "99" then begin
      dato$ = getCampoTSL$( SL.STR.ENTRY$, 2 )
      if dato$ = "55" then begin
         operador          = val( getCampoTSL$( SL.STR.ENTRY$, 3 ) )
         RECpatner$        =      getCampoTSL$( SL.STR.ENTRY$, 4 )
         RECtxld$          =      getCampoTSL$( SL.STR.ENTRY$, 5 )
         RECmobileOper$    =      getCampoTSL$( SL.STR.ENTRY$, 6 )
         RECnumeroRecarga$ =      getCampoTSL$( SL.STR.ENTRY$, 7 )
         RECmontoRecarga$  =      getCampoTSL$( SL.STR.ENTRY$, 8 )
         if ( RECtxld$ = "0" ) and ( RECmobileOper$ = "0" ) then RECrespCode$ = "2" \
         else RECrespCode$ = "0"
         !call TraceRecarga("recuperaRecarga RECtxld$=[" + RECtxld$ + "] RECmobileOper$=[" + RECmobileOper$ + "] RECrespCode$=[" + RECrespCode$ + "]" )
         call acumulaRecargas
      endif
   endif   
   
end sub

!------------------------------------------------------------------------------
! Al finalizar una transacciOn de venta
!------------------------------------------------------------------------------
sub MSMTREC02 public
   if not activo then exit sub
   cantRecargas% = 0
   RECidCliente$ = ""
end sub

!------------------------------------------------------------------------------
!Lectura de parametros desde archivo c:/ADX_UDT1/PARGENER.DAT
!incorporar a USER 7
!------------------------------------------------------------------------------
sub MSMTREC07 public
   
   integer*1 k
   integer*4 inicioBloque
   string v$                      !
   
   fechaModificacion  = " 10 Feb 2022"
   activo  = 0
   depurar = 0
   dummy   = 0
   
   RECoperation       = 70
   separador          = "'|'"
   cantCopiasVoucher% = 1                        ! valor por defecto
   cantMaxRecargas%   = 1                        ! valor por defecto
   pipeTimeout%       = 5000                     ! valor por defecto
   REClargoNumero%    = 9                        ! valor por defecto
   
   call imprimeCJ("**** MODULO RECARGAS ELECTRONICAS ****")
   
   inicioBloque = abreParametros( "R::$ARGENER", "[RECARGAS ELECTRONICAS]" , 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO " + fechaModificacion )
      call cierraParametros( 80 )
      exit sub
   endif
   
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin
      if esNumero( v$ ) then k = val(v$) else k = 0
      if (k and 1) then activo  = -1
      if (k and 2) then depurar = -1
      if (k and 4) then dummy   = -1
      
      !tiempo de espera respuesta
      v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
      if v$ <> "" then \
         if esNumero( v$ ) then pipeTimeout% = val( v$ )
      
      !Operadores activos
      v$ = leeParametro$( "OPERADORES", 80, inicioBloque )
      if v$ <> "" then RECoperadores$ = trim$( v$ )

      !Cantidad de copias de vouchers
      v$ = leeParametro$( "NRO_COPIAS_VOUCHER", 80, inicioBloque )
      if esNumero( v$ ) then cantCopiasVoucher% = val(v$)
    
      !Pagos restringidos
      v$ = leeParametro$( "PAGOS_RESTRINGIDOS", 80, inicioBloque )
      pagosRestringidos$ = trim$( v$ )
        
      !Cantidad maxima de recargas 
      v$ = leeParametro$( "NRO_RECARGAS_MAXIMO", 80, inicioBloque )
      if esNumero( v$ ) then cantMaxRecargas% = val(v$)
      
      !largo numero telefonico
      v$ = leeParametro$( "LARGO_NUMERO", 80, inicioBloque )
      if esNumero( v$ ) then REClargoNumero% = val(v$)
      
   endif
  
   call cierraParametros( 80 )
   
   if not leeMensajesOperacion  then activo = 0
   if not leeFormatoVoucher     then activo = 0
   if not leeOperadoresRecargas then activo = 0

   if activo then v$ = "ON " else v$ = "OFF "
   call imprimeCJ( "ESTADO " + v$ + fechaModificacion )
   if depurar then begin
      call imprimeCJ( "TIMEOUT          " + right$( string$( 15, " " ) + str$( pipeTimeout% )     , 15 ) )
      call imprimeCJ( "OPERADORES       " + right$( string$( 15, " " ) + RECoperadores$           , 15 ) )
      call imprimeCJ( "COPIA VOUCHER    " + right$( string$( 15, " " ) + str$(cantCopiasVoucher%) , 15 ) )
      call imprimeCJ( "PAGO RESTRINGIDO " + right$( string$( 15, " " ) + pagosRestringidos$       , 15 ) )
      call imprimeCJ( "MAX.CANT RECARGA " + right$( string$( 15, " " ) + str$(cantMaxRecargas%)   , 15 ) )
      call imprimeCJ( "LARGO NUMERO     " + right$( string$( 15, " " ) + str$(REClargoNumero%)    , 15 ) )
      
      call imprimeCJ( "MSG OPERACION                 OK")
      call imprimeCJ( "FORMATO VOUCHR                OK")
      call imprimeCJ( "DATOS OPERADORES              OK")
      if Dummy then call imprimeCJ("ACTIVADO INDICADOR AUTORESPUESTA")
   endif

   dim recargas$( cantMaxRecargas%, cantParVouRecarga% )
       
end sub

!------------------------------------------------------------------------------
! Despues de leer un registro de articulo
!------------------------------------------------------------------------------
sub MSMTREC08 public
    if not activo then exit sub
    call procesaRecarga 
end sub

!------------------------------------------------------------------------------
! Despues de la lectura del Teclado/Scan/Lect Optico
!------------------------------------------------------------------------------
sub MSMTREC14 public
integer*2 k

     if not activo        then exit sub
     if cantRecargas% = 0 then exit sub
     
     !Impide ingresar RUC de cliente en una transaccion con recarga
     if secuenciaTeclaCliente then begin
        call obtieneMsgOperacion( 16 )    
        call muestraBorrar( msg1$, msg2$ )
        call ignoraTeclas
     endif
     
     ! Impide el anular total en una transaccion con recarga
     if TS.IO.KEYS(1) = 70 and TS.IO.MOTORKEY = 81 then Begin
        call obtieneMsgOperacion( 8 )    
        call muestraBorrar( msg1$, msg2$ )
        call ignoraTeclas
     endif
     
     ! Control medios de pago
     k = match( ";" + str$( TS.IO.MOTORKEY ) + ";", ";" + pagosRestringidos$ + ";" , 1 )
     if k > 0 then begin
        call obtieneMsgOperacion( 9 )    
        call muestraBorrar( msg1$, msg2$ )
        call ignoraTeclas
     endif
    
end sub

!-----------------------------------------------------------------------------
! Antes de imprimir a la estacion de recibo de caja
!-----------------------------------------------------------------------------
sub MSMTREC20 public
    if not activo then exit sub
    if (TS.LINETYPE = 6) AND (TS.LINEDATA = 1) then begin
       lineaDatosTrx$ = TS.PRTBUF$
       call imprimeVoucher
    endif
end sub

!------------------------------------------------------------------------------
! En el control de errores
!------------------------------------------------------------------------------
sub MSMTREC37 public
    if not activo then exit sub
    if ocultaErrorSMA then TS.USER.RETURN = -1
    ocultaErrorSMA = 0
end sub

!------------------------------------------------------------------------------
! En la recuperacion de transaccion
!------------------------------------------------------------------------------
sub MSMTREC53 public
    if not activo then exit sub
    call recuperaRecarga
end sub

!------------------------------------------------------------------------------
! Antes de actualizar la matriz de transacciones
!------------------------------------------------------------------------------
sub MSMTREC68(ENTRY$, ELEMENT) public
   string ENTRY$,              \
          cad1$                !
   integer*2 ELEMENT           !
   
   if ( not activo ) or ( len(RECcadena99$) = 0 ) then exit sub
   cad1$ = right$("000000000000" + getCampoTSL$(ENTRY$,2),12)
   if ( getCampoTSL$(ENTRY$,1) = "01" ) and ( articuloRecarga( cad1$ ) ) then begin      
      call agregaCadenaTSL68( RECcadena99$ )
      RECcadena99$ = ""
      ELEMENT = ELEMENT + 1
      call noAplicaDescuento         ! para que el sistema de promociones no aplique descuentos en esta trx
   endif
end sub
