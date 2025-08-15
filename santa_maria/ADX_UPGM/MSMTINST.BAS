!-----------------------------------------------------------------------------
! Modulo: MSMTINST.BAS Modulo Credito Institucional
! Fecha : 20 Enero 2022
! Autor : Carlos Calderon
! 2021-12-21 Carlos Calderon Se incorpora descuentos al MP
!------------------------------------------------------------------------------
! Mod 01/Abr/2023
! Se adiciona generacion mensaje en voucher para empleados
! se define nombre logico TFPAGAR2 apuntando al archivo adx_udt1:tfpagar2.dat
! Desarrollado por Grupo Retail - OVS
! -----------------------------------------------------------------------------
! Mod 20Nov2023
! Se cambia integración de la aplicación desmontando pipes e integrando con 
! sockets.
! desarrollado por Grupo Retail - CAC/OVS
!------------------------------------------------------------------------------
! Mod 31Jul2024
! Se controla en proceso de domiciliarios si el pago se realiza con las tarjetas 
! parametrizadas, si el dato es 9999 no realiza validación de la tarjeta
! desarrollado por Grupo Retail - CAC/OVS
!------------------------------------------------------------------------------
! Mod 24Jul2025
! Se define una variable global donde se reportan 
! las tarjetas de crd institucional usadas por los 
! domiciliarios para evitar su uso en operaciones 
! fuera de domicilio.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------

%ENVIRON T

integer*1 estado,                      \ !
          depurar,                     \ !
          dummy,                       \ !
          operation,                   \ ! 65: Mensajeria Credito Institucional
          cri.accion,                  \ ! tipo de operacion credito institucional
          cri.proceso,                 \ ! tipo de proceso 0=consulta 1=consumo 2=reverso 3=reintegro 4 cierre
          criInTrx%,                   \ ! Si existen pagos Credito Institucional
          criPago%,                    \ ! Si el pago es credito institucional -1=Si, 0=No
          cri.Crd.Pagare%,             \ ! Formato de pagare a utilizar
          cri.cant.pagoCI,             \ ! cantidad de pagos con credito institucional
          cri.anula.pago,              \ ! indicador de anulacion de pago 0=NO -1=SI
          cri.cant.max.pagos,          \ ! cantidad maximo de pagos con credito institucional permitidos
          cri.ints.gui

integer*4 pipeTimeout,                 \ ! Tiempo de espera
          cri.montos.pago(2)           \ ! vector con los monto ingresados como MP sin descuento columna 1
                                         !                                             descuento columna 2

string    cri.header$,                 \ ! encabezado
          cupoDisponible$,             \ ! cupo disponible
          cri.registro.pago$,          \ ! copia del ultimo pago con cred inst realizado
          cri.premio$,                 \ ! tarjeta premio S tarjeta Regalo N
          cri.empresa$,                \ ! empresa convenio ejemplo: Mega Santamaria
          cri.nombreCliente$,          \ ! nombre cliente
          cri.cedulaCliente$,          \ ! Id Beneficiario
          cri.cadena.venta$,           \ ! cadena venta credito institucional
          cri.cadenas.intrx$(2),       \ ! mensajes de pagos con credito institucional
          cri.recibo$,                 \ ! numero de recibo
          cri.Prefijo$,                \ ! Prefijo tarjeta
          cri.tazaImpIva$,             \ ! porcentaje de impuesto IVA
          separador,                   \ ! Separador de campo
          cri.Crd.Msg$(2),             \ ! Pagares para el sistema
          cri.Crd.Msg2$(1),            \ ! Pagares para el sistema adicional
          cri.Crd.ImpVoucher$(2),      \ ! Impresion del Voucher
          criMsrData$,                 \ ! contenido banda magnetica
          cri.Crd.Pago$,               \ ! Tipo y variedad de pago
          cri.tarjeta.dummy$,          \ ! numero de tarjeta para esta dummy
          lf$                            ! salto de linea chr$(10)

INTEGER*4 valorConsumo,                \ valor del pago ingresado menos el descuento al pago
         valorDescuento                ! valor de descuento al pago

integer*1 global TS.RECOVERY

integer*2 global SL.END                  ! last index to be used in summary log
String    Global Gr.RtaTrama$,          \!
                 Gr.Domi.LstaTarj$,     \! Lista tarjetas deliverys 24Jul2025 
                 Gr.Domi.Lista$(2)       ! Lista domiciliarios
Integer*1 Global Gr.Domi.Delivery%,     \! Delivery capturado
                 Gr.Domi.Intrx%          ! Delivery en proceso

%INCLUDE NUOTVARI.J86                    !
%INCLUDE EAMTSWKG.J86                    ! working storage
%INCLUDE EAMTRANS.j86                    !
%INCLUDE JAVAGUIV.J86

%INCLUDE NUOTRUTI.J86                    ! Rutinas generales
%INCLUDE MSMTFACT.J86                    ! Rutinas Factura
%INCLUDE NUOTPROM.J86                    ! Rutinas de promociones
%INCLUDE JAVAGUIC.J86

%INCLUDE RECATSSU.011          					 ! RUTINAS GENERICAS APLICACION     

Sub TSHIECET External
End Sub

!para grabar en el journal electronico
FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
   STRING QXL.TSUPEC21, PRTLINE
FEND

Sub CRIN.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::GRLOG:CI" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            Call VISORES4690(1,"ERROR EN CREACION","DE AUDITORIA ",1500,"L")
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56
End Sub 




Sub Audit.Institucional(Xmsg$)
String Xmsg$
    TS.TEMP1I4 = Len(Xmsg$)		            								  ! Toma longitud del registro
    TS.TEMP1$ = "C"+Str$(TS.TEMP1I4)+" C2"							  ! Arma estructura de grabacion
    Write form TS.TEMP1$;#56 ;Xmsg$, (CHR$(10) + CHR$(13))  ! Grabacion del registro

End Sub 

!------------------------------------------------------------------
! graba log "C:/TRACE/" + TS.TERMINAL$ + ".TXT"
!------------------------------------------------------------------
sub logINT( dato$ )
   string dato$, h$
   if depurar then begin
      h$ = time$
      Call traceNUO( h$ + " " + dato$ )
   endif
end sub

Function cri.Crd.MsgAdd
String cri.Crd.MsgAdd
Integer*2 Xi%
If Gr.RtaTrama$ = "02" Then Begin 
   For XI% = 1 To 20
     Call imprimeCJ(cri.Crd.Msg2$(Xi%))                                   
   Next XI%
EndIf   
cri.Crd.MsgAdd = ""

End Function 

Function  GrProcCrdi(Gmsg$,gfunc%)																					! Proceso requerimientos Credito Institucional
String    GrProcCrdi, GM$, Xrtrama$, Xtemp4$, Gmsg$, ghead$, Grfun$					!
String    Xsnd$, Xfin$ 
Integer*1 gfunc%
   Grfun$ = "11"																														! Default consulta
   If gfunc% = 0 Then Grfun$ = "11"																					! Consulta Credito
   If gfunc% = 1 Then Grfun$ = "12"																					! Consumo Credito
   If gfunc% = 2 Then Grfun$ = "13"																					! Reverso Credito 
   If gfunc% = 3 Then Grfun$ = "14"																					! Reintegro Credito
   If gfunc% = 4 Then Grfun$ = "15"																					! Cierre Credito
   Call TSHIECET
   If gfunc% <> 4 Then \
      Call muestraOperador("PROCESA SOLICITUD ","ESPERE POR FAVOR") Else   \!
      Call muestraOperador("ACTUALIZA CREDITO ","ESPERE POR FAVOR")
   Xsnd$ = DATE$ +":"+ Time$                                              	! Fecha y hora del requerimiento
   gm$ = Armar.Trama.Msg("42",Grfun$,Gmsg$,"00","0001","123456")  	  			! Armar trama MSG
   XRtrama$  = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", gm$) ! Ejecuta Requerimiento
   XTEMP4$ = Valida.Rta(Xrtrama$)																			      ! Valida rta entregada
   Xfin$ = DATE$ +":"+ Time$                                              	! Fecha y hora del requerimiento
   Call CRIN.AUDITORIA(gm$,Xrtrama$,Xsnd$, Xfin$)                           ! Rastreo movimiento
   If Xtemp4$ <> "00" Then Begin 																						! Si falla proceso
  	 Call muestraBorrar("FALLA COMUNICACION","CREDITO INSTITUC.")
  	 Xrtrama$ = ""
  	 GoTo FIN.PROC.CRI
   EndIf 
   XTEMP4$ = Mid$(XrTrama$,12,2)	       																    ! Valida rta entregada
   If XTemp4$ <> "00" Then Begin 																				    ! Rta No Satisfactoria
   	  Call muestraBorrar(Mid$(XrTrama$,14,20),Mid$(XrTrama$,34,20))    			! Presenta Msg Error
   	  Xrtrama$ = ""
   EndIf
   FIN.PROC.CRI:   
    GrProcCrdi = XRtrama$

End Function  

!-- Impresion de Datos Especificos en el voucher
!-- de venta empresarial, con los siguientes parametros
!-- &V  Valor de la venta
!-- &B  Valor Base Impuesto del 12%
!-- &E  Tarifa Excenta
!-- &I  Valor Impto del 12%
!-- &T  Total de la Venta
!-- &X  Nombre Empresa
!-- &Y  Nombre Beneficiario
!-- &Z  RUC / CI Beneficiario
!-- &N  Numero de la tarjeta
!-- &S  Mensaje adicional empleado 01Abr2023
! Coloca datos por parametro
function Crd.String( X.A$,ind )
   string Crd.String, \
          X.A$, \
          X.B$, \
          X.C$, \
          X.D$, \
          XM$
   integer*4 \
          X.A%, \
          X.B%
   integer*1 ind


   X.A% = MATCH("&", X.A$, 1)                             ! Busca dato a imprimir
   if X.A% > 0 then begin                                 ! Si lo encontro
      X.B% = len(X.A$)                                    ! Arma mensaje con el dato
      X.B$ = left$(X.A$, X.A% - 1)                        ! solicitado, entregando
      X.C$ = mid$(X.A$, X.A%, 2)                          ! 51 caracteres
      X.D$ = right$(X.A$, X.B% - (X.A% + 1))

      ! Valor del credito
      if UCASE$(X.C$) = "&V" then begin
         X.C$ = formateaMonto(Val(cri.Crd.ImpVoucher$(ind,2)))
      endif

      ! Base Impuesto
      if UCASE$(X.C$) = "&B" then begin
         X.C$ = formateaMonto(Val(cri.Crd.ImpVoucher$(ind,5)))
      endif

      ! Valor Base Exenta
      if UCASE$(X.C$) = "&E" then begin
         X.C$ = formateaMonto(Val(cri.Crd.ImpVoucher$(ind,6)))
      endif

      ! Valor del Impuesto
      if UCASE$(X.C$) = "&I" then begin
         X.C$ = formateaMonto(Val(cri.Crd.ImpVoucher$(ind,7)))
      endif

      ! Total de la Venta
      if UCASE$(X.C$) = "&T" then begin
         X.C$ = formateaMonto(Val(cri.Crd.ImpVoucher$(ind,2)))
      endif

      ! Nombre de la empresa
      if UCASE$(X.C$) = "&X" then begin
         X.C$ = cri.Crd.ImpVoucher$(ind,8)
      endif

      ! Nombre Beneficiario
      if UCASE$(X.C$) = "&Y" then begin
         X.C$ = cri.Crd.ImpVoucher$(ind,9)
      endif

      ! Ruc / CI Beneficiario
      if UCASE$(X.C$) = "&Z" then begin
         X.C$ = cri.Crd.ImpVoucher$(ind,10)
      endif

      ! Numero de la tarjeta
      if UCASE$(X.C$) = "&N" then begin
         X.C$ = cri.Crd.ImpVoucher$(ind,1)
      endif

      ! Numero del recibo
      if UCASE$(X.C$) = "&G" then begin
         X.C$ = cri.Crd.ImpVoucher$(ind,11)
      endif

      ! Mensaje adicional empleados
      if UCASE$(X.C$) = "&S" then begin
         X.C$ = cri.Crd.MsgAdd
      endif


      X.B$ = X.B$ + X.C$ + X.D$
      X.B$ = left$(X.B$ + String$(51, " "), 51)
      X.A$ = X.B$
   endif

   Crd.String = X.A$

end function

Sub IMPRIMA.PAGARE(XPos%,ind)
   string XMsg$
   integer*1 ind
   integer*4 YI%,  \
             Xpos%

   for YI% = 1 TO VAL(cri.Crd.Msg$(XPos%, 0))                     ! Total de lineas a imprimir
      XMSG$ = cri.Crd.Msg$(Xpos%, YI%)                            ! Toma linea de impresion
      XMSG$ = Crd.String(XMSG$,ind)                               ! Analiza linea impresion
      Call imprimeCJ(XMSG$)                                       ! Imprime Texto
   next YI%
End sub

function agrega.punto.decimal( m$ )
  string f$, m$, agrega.punto.decimal

  if len(m$) <= 3 then begin
     m$ = right$("000" + m$,3)
  endif
  f$ = left$(m$,len(m$)-2) + "." + right$(m$,2)
  agrega.punto.decimal = f$

end function

!------------------------------------------------------------------------
! Genera encabezado del mensaje
! Operation 65: Transacciones Credito Institucional
!------------------------------------------------------------------------
sub generaEncabezadoCRDINST

   cri.header$ = "P"                             + separador + \ canal de comunicacion S=socket P=Pipe
             str$(operation)                    + separador + \ requerimiento mensaje por pipe para MSM FIJO 65
             "0000"                             + separador + \ canal de respuesta
             TS.TERMINAL$                       + separador + \ terminal de origen
             "0"                                + separador + \ tipo de conexion 0=temporal 1=permanente
             "20" + date$ + time$                             ! fecha y hora de inicio de la operacion

end sub

function generaMensaje( val.pag$ )
   string generaMensaje, m$, f$, vc$, val.pag$

   if cri.accion = 0 then vc$ = "0" \
   else vc$ = agrega.punto.decimal( val.pag$ )

   f$ = right$( "000" + TS.STORE$, 3 ) + "-" + right$( "000" + TS.TERMINAL$, 3 ) + "-" + right$("000000000" + str$(MSMTFACTnumeroFactura + 1),9)

   m$ = cri.header$                                      + separador + \ ! header comun
        criMsrData$                                      + separador + \ ! numero de tarjeta
        vc$                                              + separador + \ ! valor consumo formateado con dos decimales
        right$( "0000" + TS.STORE$, 4 )                  + separador + \ ! tienda
        right$( "000" + TS.TERMINAL$, 3 )                + separador + \ ! numero de caja
        str$( val( unpack$( TS.OPER$ ) ) )               + separador + \ ! Id cajero
        "20" + date$ + time$                             + separador + \ ! fecha hora aaaammddhhmmss
        right$( "0000" + str$( SL.HD.TRANSNUM + 1 ), 4 ) + separador + \ ! numero transaccion
        str$(cri.accion)                                 + separador + \ ! accion 0=consulta 1=consumo 2=reverso 3=reintegro 4 cierre
        "Factura"                                        + separador + \ ! tipo documento
        f$                                                               ! numero de factura

   generaMensaje = m$

end function

!-----------------------------------------------------------------------------
! Lee archivo que contiene el cuerpo del mensaje
!-----------------------------------------------------------------------------
function leeBody( file.resp$ )

   integer*2 indice
   integer*4 tamano
   string leeBody, body$, archivo$, correlativo$, f$, file.resp$

   leeBody = ""
   close 47
   if file.resp$ = "" then archivo$ = "R::C:/CNB/"+"TRTP" + TS.TERMINAL$ + ".DAT" \
   else archivo$ = "R::" + file.resp$

   body$ = ""
   correlativo$ = ""

   TS.ER.RETURN = -1
   open archivo$ as 47
   if TS.ER.RETURN <> -1 then begin
      Call muestraBorrar( "NO SE PUEDE ABRIR", "ARCHIVO DE RESPUESTA" )
      Call tracenuo("ERR OE "+archivo$)
      exit function
   endif
   Call tracenuo("apertura ok:"+archivo$)
   
   tamano = size(archivo$)

!Call logINT("leeBody tamaño = [" + str$(tamano) + "]")

   !informacion del archivo debe contener caracter de fin de linea
   f$ = "C" + str$(tamano)
   read form f$;#47;body$
   if TS.ER.RETURN <> -1 then begin
      Call muestraBorrar( "NO SE PUEDE LEER", "ARCHIVO DE RESPUESTA" )
      delete 47
      exit function
   endif
   
   if right$(body$,2) = CRLF$ then body$ = left$(body$,len(body$)-2)
   Call tracenuo("Rta body:"+body$)
   
   if len( body$ ) = 0 then begin
      Call muestraBorrar( "ARCHIVO DE RESPUESTA", "SIN INFORMACION..." )
      delete 47
      exit function
   endif

!Call logINT("leeBody = [" + body$ + "]")

   leeBody = body$
   delete 47
end function

!------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------
function recibeRespuesta( opc )
   integer*1 opc
   string recibeRespuesta, a$, f$, name.file$,cod$,m$

   recibeRespuesta = ""
   name.file$      = ""

   f$ = ""
   a$ = recibeMensaje( "del banco" )

   if opc = 0 then begin

     cod$ = buscaCampo(a$, separador, 2)
     if cod$ = "63" then begin
        f$ = buscaCampo(a$, separador, 7)
        m$ = buscaCampo(a$, separador, 8)
        if f$ = "0"  then begin
           name.file$ = m$
       endif else begin
           m$ = left$(m$ + string$(40," "),40)
           Call muestraBorrar( left$(m$,20), mid$(m$,21,20) )
           exit function
       endif
     endif

!Call logINT("recibeRespuesta pido body")

     f$ = leeBody(name.file$)

!Call logINT("recibeRespuesta-2 a=[" + f$ + "]")

     if ( f$ = "" ) or (( buscacampo(f$,separador,1) = "1" ) and ( buscacampo(f$,separador,2) = "No se encontro comprobante")) then begin
        f$ = "NO SE ENCONTRO      COMPROBANTE         "
        Call muestraBorrar( left$(f$,20), mid$(f$,21,20) )
        Call restauraVisor(1)
        exit function
     endif

!Call logINT("recibeRespuesta-3 a=[" + f$ + "]")
   endif

   a$ = a$ + f$
   f$ = buscaCampo(a$, separador, 7)

!Call logINT("recibeRespuesta-4 = [" + a$ + "][" + f$ + "]" )

   if opc = 0 then begin
      if cod$ <> "63" then begin
         !if buscaCampo(a$, separador, 7) <> "01" then begin
         	
         If buscaCampo(a$, separador, 7) = "00" then begin
            f$ = buscaCampo(a$, separador, 8) + string$(40," ")
            Call muestraBorrar( left$(f$,20), mid$(f$,21,20) )
            exit function
         endif
      endif
   endif

   recibeRespuesta = a$

   Call restauraVisor(1)

!Call logINT("recibeRespuesta-5 = [" + a$ + "]")

end function

function existePagoPorAnular
   integer*1 x,                          \
             existePagoPorAnular      !

   string    f$,                         \
             nt$,                        \ Numero de tarjeta
             mo$                         ! monto tarjeta con descuento aplicado al MP

   existePagoPorAnular = 0
   if ( not criInTrx% ) or ( cri.cant.pagoCI = 0 ) then exit function
   for x = 1 to cri.cant.pagoCI
      f$ = cri.cadenas.intrx$(x,2)
      if f$ = "" then begin

         f$ = cri.cadenas.intrx$(x,1)
         nt$ = buscaCampo(f$, separador, 7)
         mo$ = buscaCampo(f$, separador, 8)

         if ( valorConsumo = cri.montos.pago(x,1) - cri.montos.pago(x,2) ) and ( criMsrData$ = nt$ ) then begin
            valorConsumo = val(mo$) * 100.0
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

function leeTarjetaMsr
   integer*1 leeTarjetaMsr

   leeTarjetaMsr = 0

   if java.init = -1 then begin
      if not(cri.ints.gui) then begin
         jGuiSubState = 80650
         Call javaEvent(terminalSubStateMsg)
      endif
      if cri.ints.gui then begin
         jGuiSubState = 80655
         Call javaEvent(terminalSubStateMsg)
         cri.ints.gui = 0
      endif
      jGuiSubState = 0
   endif

   Call muestraOperador("DESLICE LA TARJETA  ","CREDITO INSTITUCIONAL")
   criMsrData$ = leeMSR$(1)                                                         ! Captura la tarjeta
   if java.init = -1 then begin
      if left$(criMsrData$,3) = cri.Prefijo$ then begin
         criMsrData$ = left$(criMsrData$,13) 
      endif
   endif

   if criMsrData$ = ""  then exit function                                          ! Sale de la rutina

   !---Dummy-- para simular lectura de banda magnetica de tarjeta de Cred-Inst
   if dummy then criMsrData$ = cri.tarjeta.dummy$

   if left$(criMsrData$,3) <> cri.Prefijo$ then begin                            ! Dato ingresado no es un Crd Institucional
      Call muestraBorrar("DATO CAPTURADO NO ES","CRD INSTITUCIONAL   ")
      exit function
   endif

   leeTarjetaMsr = -1

end function

function CONSULTA.CRD.INSTITUCIONAL
   integer*1 CONSULTA.CRD.INSTITUCIONAL,                   \
             x,                                            \
             y                                             !

   string   f$,                                            \
            m$,                                            \
            r$,                                            \
            msg.env$

   msg.env$ = ""
   CONSULTA.CRD.INSTITUCIONAL = 0

   if ( ( cri.accion = 2 ) and ( cri.proceso = 1 ) and ( cri.anula.pago = -1 ) ) then begin
      x = existePagoPorAnular
      if x = 0 then begin
         Call muestraBorrar("NO EXISTE ESTE PAGO","EN LA TRANSACCION")
         Call ignoraTeclas
         exit function
      endif
      msg.env$ = cri.cadenas.intrx$( x, 1 )
      m$ = cambiaCampo(msg.env$,separador,14,"2")
      cri.cadenas.intrx$( x, 2 ) = m$
      cri.Crd.ImpVoucher$(x,13 ) = "Pago Anulado"
   endif else begin
      Call generaEncabezadoCRDINST

      m$ = generaMensaje( str$( valorConsumo ) )

      ! se inicializa matriz para almacenar mensaje de pago y anulaciones de pago
      if ( cri.cant.pagoCI = 0 ) and ( not criInTrx% ) then begin
         dim cri.cadenas.intrx$(cri.cant.max.pagos,2)
         dim cri.montos.pago(cri.cant.max.pagos,2)
      endif

      ! se respalda mensaje de pago
      if ( ( cri.accion = 1 ) and ( cri.proceso = 1 ) and ( cri.anula.pago = 0  )  ) then begin
         msg.env$ = m$
         cri.registro.pago$ = m$
         !Call logINT("CONSULTA.CRD.INSTITUCIONAL cri.registro.pago$=[" + cri.registro.pago$ + "]" )
      endif
   endif

   Call tracenuo("Accion:"+Str$(cri.accion)+" TRCISND:"+M$)   ! Trama envio crd institucional
   M$ = GrProcCrdi(m$,cri.accion)				 ! Consulta de saldo
   Call tracenuo("RTCI1:"+m$) ! Rta crd institucional
   
   if m$ = "" then exit function

    f$ = buscaCampo( m$, separador, 7 )                                ! codigo retorno 00=respuesta con error 01=respuesta ok
    r$ = buscaCampo( m$, separador, 8 )

   if f$ = "00" then begin
      Call muestraBorrar(mid$(r$,1,20),mid$(r$,21,20))
      exit function
   endif 

   ! Guarda mensaje de pago en matriz de mensajes
   if ( ( cri.accion = 1 ) and ( cri.proceso = 1 ) and ( cri.anula.pago = 0  ) ) then begin

      cri.cant.pagoCI         = cri.cant.pagoCI + 1
      cri.cadenas.intrx$(0,0) = str$(cri.cant.pagoCI)
      cri.cadenas.intrx$( cri.cant.pagoCI, 1 ) = msg.env$
      cri.montos.pago(cri.cant.pagoCI,1) = valorConsumo + valorDescuento
      cri.montos.pago(cri.cant.pagoCI,2) = valorDescuento

   endif

   if ( cri.accion = 0 ) and ( not cri.anula.pago  ) then begin
      cupoDisponible$    = buscaCampo( m$, separador,  8 )            ! saldo disponible
      cri.premio$        = buscaCampo( m$, separador,  9 )
      cri.empresa$       = buscaCampo( m$, separador, 10 )
      cri.nombreCliente$ = buscaCampo( m$, separador, 11 )
      cri.cedulaCliente$ = buscaCampo( m$, separador, 12 )
      Gr.RtaTrama$       = buscaCampo( m$, separador, 7 )             ! Tipo de empleado 

   endif

   if ( cri.proceso = 0 ) then begin                                      ! 0=consulta 1=consumo
   	  cupoDisponible$    = buscaCampo( m$, separador,  8 )            ! saldo disponible
      Call TSHIECET
      Call muestraBorrar("SALDO DISPONIBLE","Usd $" + cupoDisponible$)
      cupoDisponible$ = ""
   endif

   CONSULTA.CRD.INSTITUCIONAL = -1

end function

sub Duplicado.Crd.Institucional
   Integer*1 f,r, Xf%
   String cri.KeyComp$,    \
          m$,f$,           \
          X.Crd$,          \
          X.Vlr$,          \
          X.Base$,         \
          X.IVA$,          \
          X.Emp$,          \
          X.Ben$,          \
          X.Ide$,          \
          X.NVC$,          \
          X.Llave$         !
   String Xfile$, Xbuf$(1)
   f = -1 : r = 0 : Dim Xbuf$(10)
   Dim cri.Crd.ImpVoucher$(1,11)
   cri.KeyComp$ = ""
   If TS.INTRX Then Begin                                                 ! Dentro de una trx
      Call muestraBorrar("PROCESO NO VALIDO   ","DENTRO DE UNA COMPRA")
      Exit Sub
   EndIf 

   While f
      if not (jGuiSubState = 80510) then begin
         jGuiSubState = 80527
      endif
      Call javaEvent(terminalSubStateMsg) 								  
      cri.KeyComp$ = ingresoDatos$("NUMERO VOUCHER ?    ","                    ")
      If cri.KeyComp$ = "A" Then Exit Sub
      If Left$( cri.KeyComp$, 1 ) = "P" Then \ 
      	 cri.KeyComp$ = Right$( cri.KeyComp$, len( cri.KeyComp$ ) - 2 )
      If esnumero(cri.KeyComp$) Then Begin
         cri.KeyComp$ = Right$("000000000" + cri.KeyComp$,9)
         f = 0
      EndIf  Else Begin
         cri.KeyComp$ = ""
         Call muestraBorrar( "OPCION NO VALIDA    ","REINTENTE           " )
         r = r + 1
         If r = 3 Then Exit Sub
      EndIf 

      If jGuiSubState = 80527 Then Begin
         jGuiSubState = 80510 
      EndIf Else Begin
         jGuiSubState = 0 
      EndIf 
   Wend

   Call muestraVisor( 1, "PROCESANDO", "TRANSACCION" )
   TS.ER.RETURN = -1
   xfile$ = "R::ADX_UDT1:TI"+date$+".DAT"
   Open Xfile$ As 47
   If TS.ER.RETURN <> -1 Then Begin
  	 	  Call muestraBorrar( "ERROR ABIENDO  ARCH.", "TKTS CRED. INSTI.  " )
        Exit Sub 
   EndIf
   If End #94 Then UE.FIN.TKTCRI
   Xf% = 0
   While (1)																																! Recorre archivo tkts
    Read #47; Xbuf$(01),Xbuf$(02),Xbuf$(03),                               \! Toma datos tkts
              Xbuf$(04),Xbuf$(05),Xbuf$(06),                               \!
              Xbuf$(07),Xbuf$(08),Xbuf$(09)																	!
    If Val(Xbuf$(01)) = Val(cri.KeyComp$) Then Begin												! Tkt encontrado
      cri.Crd.ImpVoucher$(1,11) = Xbuf$(01)                                 ! Nro recibo    
      cri.Crd.ImpVoucher$(1, 2) = Xbuf$(02)                                 ! Vlr venta     
      cri.Crd.ImpVoucher$(1, 5) = Xbuf$(03)                                 ! Base impuesto 
      cri.Crd.ImpVoucher$(1, 6) = Xbuf$(04)                                 ! Base excento  
      cri.Crd.ImpVoucher$(1, 7) = Xbuf$(05)                                 ! Valr Impto    
      cri.Crd.ImpVoucher$(1, 8) = Xbuf$(06)                                 ! Name Empresa  
      cri.Crd.ImpVoucher$(1, 9) = Xbuf$(07)                                 ! Name Benefic  
      cri.Crd.ImpVoucher$(1,10) = Xbuf$(08)                                 ! ID Benefic
      cri.Crd.ImpVoucher$(1, 1) = Xbuf$(09)                                 ! Nro Tarjeta
      Xf% = -1																															! Encontrado
    	GoTo UE.FIN.TKTCRI
    EndIf																																		!
   Wend 																																		! fin barrido tkts
   UE.FIN.TKTCRI:
    Close 47
   If Xf% = 0 Then Begin																										! Tiket no encontrado
	 	  Call muestraBorrar( "NO SE ENCONTRO EL   ", "COMPROBANTE /Borrar" )		! Msg Alerta
      Exit Sub 
   EndIf 																																		!
   Call imprimeCJ("          **** COPIA CLIENTE **** ")											!
   Call IMPRIMA.PAGARE(cri.Crd.Pagare%,1)                        						! Imprime Formato
   Write #34; NUO.SaltaLineas$ + chr$( 5 ) + NUO.CortaPapel$     						! Corta Papel
   Call IMPRIMA.PAGARE(cri.Crd.Pagare%,1)                        						! Imprime Formato
   Write #34; NUO.SaltaLineas$ + chr$( 5 ) + NUO.CortaPapel$     						! Corta Papel
   If JAVA.INIT = -1 Then Begin
      jGuiSubState = 80600
      Call javaEvent(terminalSubStateMsg) 
   EndIf
End Sub

Sub InyectaPagoCrdInstitucional

   if cri.anula.pago then TS.IO.KEYS(1) = 70
   TS.IO.MOTORKEY = 95
   TS.IO.KEYS(3)  = 78
   TS.IO.DATA$(3) = str$(3)
   TS.IO.KEYS(7)  = 95
   TS.IO.DATA$(7) = str$( valorConsumo )
   TS.IO.DATA$(9) = criMsrData$
   if criInTrx% = 0 then begin                       ! Si criInTrx = 0 no existen pagos con credito institucional
      dim cri.Crd.ImpVoucher$(cri.cant.max.pagos,15)
      criInTrx% = -1
   endif
   criPago% = 2
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
      if ( p + 1 ) = len(f$) then begin
        m$ = f$ + "0"
      endif   \
      else m$ = f$
      m$ = left$( m$, p - 1 ) + right$( m$, len(m$) - p)
   endif \
   else m$ = f$ + "00"
   f$ = m$
end sub

function existen.pagos.CRI
   integer*1 existen.pagos.CRI, \ !
             x
   string    r$

   existen.pagos.CRI = -1
   for x = 1 to cri.cant.pagoCI
      r$ = cri.cadenas.intrx$(x,2)
      if r$ = "" then exit function
   next x
   existen.pagos.CRI = 0

end function

Sub Almacena.Cri.Tkt
Integer*1 x%
Integer*4 X.Len%
String    xfile$, xbuf$(1), Xdat$, X.Finr$, X.Lec$
  TS.ER.RETURN = -1
  xfile$ = "R::ADX_UDT1:TI"+date$+".DAT"
  Open xfile$ As 47 Append 
  If TS.ER.RETURN <> -1 Then Begin
     TS.ER.RETURN = -1
  	 Create xfile$ As 47
  	 If TS.ER.RETURN <> -1 Then Begin
  	 	  Call muestraBorrar( "ERROR CREACION ARCH.", "TKTS CRED. INSTI.  " )
        Exit Sub 
  	 EndIf
  EndIf
  Dim Xbuf$(10)
  For X% = 1 To cri.cant.pagoCI
      Xbuf$(01) = cri.Crd.ImpVoucher$(X%,11)   ! Nro recibo
      Xbuf$(02) = cri.Crd.ImpVoucher$(X%, 2)   ! Vlr venta
      Xbuf$(03) = cri.Crd.ImpVoucher$(X%, 5)   ! Base impuesto
      Xbuf$(04) = cri.Crd.ImpVoucher$(X%, 6)   ! Base excento
      Xbuf$(05) = cri.Crd.ImpVoucher$(X%, 7)   ! Valr Impto
      Xbuf$(06) = cri.Crd.ImpVoucher$(X%, 8)   ! Name Empresa
      Xbuf$(07) = cri.Crd.ImpVoucher$(X%, 9)   ! Name Benefic
      Xbuf$(08) = cri.Crd.ImpVoucher$(X%,10)   ! ID Benefic
      Xbuf$(09) = cri.Crd.ImpVoucher$(X%, 1)   ! Nro tarjeta

      Xdat$ = Xbuf$(01)+","+Xbuf$(02)+","+Xbuf$(03)+","+ \!
              Xbuf$(04)+","+Xbuf$(05)+","+Xbuf$(06)+","+ \!
              Xbuf$(07)+","+Xbuf$(08)+","+Xbuf$(09)
			X.Finr$ = Chr$(13) + Chr$(10)
			X.Len% = Len(xdat$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #47 ; xdat$, X.Finr$            					          ! Graba registro
  Next X%
  Close 47
End Sub 

! se envian mensaje operacion de cierre
Function envia.transaccion.cierre
   Integer*1 envia.transaccion.cierre, \
             x,                        \
             cen,                      \ cantidad de pagos enviados
             cpa                       ! cantidad de pagos aceptados

   String    m$,                       \
             f$,                       \
             r$                        !

   envia.transaccion.cierre = 0
   cen = 0
   cpa = 0

   For x = 1 to cri.cant.pagoCI
      r$ = cri.cadenas.intrx$(x,2)
      If r$ = "" Then begin
      	 Call Almacena.Cri.Tkt
         m$ = cri.cadenas.intrx$(x,1)
         f$ = cambiaCampo(m$,separador,14,"4")
         r$ = "Tarjeta " + buscaCampo( f$, separador,  7 ) + " Monto " + buscaCampo( m$, separador,  8 )
         m$ = GrProcCrdi(f$,4)																			! proceso cierre
         Call tracenuo("Accion: 4 TRCISND:"+f$)   									! Trama envio crd institucional
         M$ = GrProcCrdi(f$,4)				 															! Cierre trx 
         Call tracenuo("RTCI1:"+m$) 																! Rta crd institucional
         m$ = buscaCampo( m$, separador,  8 )
         Call muestraOperador("RESPUESTA PROCESO ",M$)         
         cen = cen  + 1
         cpa = cpa + 1
         f$ = "SE ENVIO CIERRE CRD "+M$
         Call imprimeSJ( f$ )
         Call imprimeSJ( r$ )
      EndIf
   Next x
   fin:
   envia.transaccion.cierre = -1
End function

SUB MSMTINST02 PUBLIC
   integer*1 f

   if not estado then exit sub

   if criInTrx% then begin
      f = envia.transaccion.cierre
!Call logINT("MSMTINST02 cierres enviados=[" + str$(f) + "]" )
   endif

   cri.nombreCliente$    = ""
   cri.premio$        = ""
   cri.empresa$       = ""
   cri.nombreCliente$ = ""
   cri.cedulaCliente$ = ""
   criInTrx%        = 0
   cri.cadena.venta$  = ""
   cri.registro.pago$ = ""
   cri.cant.pagoCI    = 0
   dim cri.cadenas.intrx$(0,0)
   dim cri.montos.pago(0,0)
   dim cri.Crd.ImpVoucher$(0,0)

END SUB

SUB MSMTINST07 PUBLIC

   integer*1 k
   integer*4 inicioBloque
   string    v$,r$, basura$

   separador          = "'|'"
   lf$                = chr$(10)
   pipeTimeout        = 5000
   estado             = 0                                  ! Proyecto desactivado
   depurar            = 0                                  ! flag para debuguear
   dummy              = 0                                  ! flag para operar con dummy
   operation          = 65
   criInTrx%          = 0                                  !
   cri.cadena.venta$  = ""
   cri.cant.pagoCI    = 0
   cri.tazaImpIva$    = "0"                                !
   cri.registro.pago$ = ""
   cri.cant.max.pagos = 10                                 ! cantidad maxima de pagos permitidos con tajeta regalo en trx
   cri.tarjeta.dummy$ = ""

   dim cri.Crd.ImpVoucher$(0,0)
   dim cri.montos.pago(0,0)
   dim cri.Crd.Msg$(15,50)                                 ! 15 Mensajes c/u 50 lineas
   Dim Cri.Crd.Msg2$(20)                                   ! 20 lineas adicionales de mensaje

   Call imprimeCJ( "***** MODULO CREDITO INSTITUCIONAL **********" )

   inicioBloque = abreParametros( "R::$ARGENER", "[CRD INSTITUCIONAL]", 80 )
   if inicioBloque = -1 then begin
      Call muestraBorrar( "NO EXISTE CRD INSTIT", "EN ARCHIVO PARGENER" )
      Call imprimeCJ( "NO CONFIGURADO 22 Nov 2021" )
      Call cierraParametros( 80 )
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
         cri.tazaImpIva$ = MSMTFACTimpuestos$(1)

         !tiempo de espera respuesta banco
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         if esNumero( v$ ) then pipeTimeout = val( v$ )

         !codigo de cadena
         v$ = leeParametro$( "CODIGO_CADENA", 80, inicioBloque )
         if esNumero( v$ ) then basura$ = v$

         !codigo de la aplicacion
         v$ = leeParametro$( "CODIGO_APLICACION", 80, inicioBloque )
         if esNumero( v$ ) then basura$ = v$

         !codigo de la aplicacion
         v$ = leeParametro$( "CODIGO_PREFIJO", 80, inicioBloque )
         if esNumero( v$ ) then cri.Prefijo$ = v$

         !Tipo y variedad de pago Crd Institucional
         v$ = leeParametro$( "TIPO_VARIEDAD_PAGO", 80, inicioBloque )
         if esNumero( v$ ) then cri.Crd.Pago$ = v$

         !Sesion temporal de trabajo
         v$ = leeParametro$( "SESION_TEMPORAL", 80, inicioBloque )
         if esNumero( v$ ) then basura$ = v$

         !Formato de Pagare a Utilizar
         v$ = leeParametro$( "NUMERO_FORMATO_PAGARE", 80, inicioBloque )
         if esNumero( v$ ) then cri.Crd.Pagare% = val( v$ )

         !cantidad maxima de pagos con tarjeta de regalo
         v$ = leeParametro$( "MAX_CANTIDA_PAGOCI", 80, inicioBloque )
         if esNumero( v$ ) then cri.cant.max.pagos = val(v$)

         !Numero de tarjeta para pruebas Dummy
         v$ = leeParametro$( "TARJETA_DUMMY", 80, inicioBloque )
         if esNumero( v$ ) then cri.tarjeta.dummy$ = v$

      endif
   endif

   Call cierraParametros( 80 )                                                           ! Cierra archivo

   if depurar then begin
      Call imprimeCJ( "TIEMPO DE ESPERA      " + right$( string$( 13, " " ) +  str$( pipeTimeout )      , 13 ) )
      Call imprimeCJ( "CODIGO_PREFIJO        " + right$( string$( 13, " " ) +       cri.Prefijo$        , 13 ) )
      Call imprimeCJ( "TIPO_VARIEDAD_PAGO    " + right$( string$( 13, " " ) +       cri.Crd.Pago$       , 13 ) )
      Call imprimeCJ( "NUMERO_FORMATO_PAGARE " + right$( string$( 13, " " ) + str$( cri.Crd.Pagare% )   , 13 ) )
      Call imprimeCJ( "MAX. CANT. PAGO       " + right$( string$( 13, " " ) + str$( cri.cant.max.pagos ), 13 ) )
      if dummy then Call imprimeCJ( "TARJETA DUMMY         " + right$( string$( 13, " " ) + cri.tarjeta.dummy$, 13 ) )
   endif

   if estado then r$ = "ON " else r$ = "OFF"
   Call imprimeCJ( "ESTADO " + r$ + " 20 Ene 2022" )

   ! Apertura archivo formato mensajes
   TS.ER.RETURN = -1
   open "R::$FPAGARE" as 80 NOWRITE NODEL
   if TS.ER.RETURN = -1 then begin
      K% = 0
      if end #80 then GR.CRD.MSGLINEA                                        ! Si es EOF
      while (1)
         read #80; A$                                                        ! Lectura registro hasta eof()
         if left$(A$,1) = "<" then begin                                     ! Carga los mensajes
            I% = match(">",A$,1)                                             ! que van a utilizar
            J% = val(mid$(A$,2,I%-2))                                        ! c/u de los operadores
            K% = 1                                                           ! telefonicos
         endif else begin
            cri.Crd.Msg$(J%, K%) = A$
            cri.Crd.Msg$(J%, 0)  = str$(K%)
            K% = K% + 1
         endif
      wend
   endif else begin
      Call muestraBorrar("ERROR PARAMETROS    ","TFPAGARE            ")      ! Msg Alerta
      Call muestraBorrar("CREDITO INSTITUCIONA","NO OPERATIVO        ")      ! Msg Alerta
      Call imprimeCJ("ERROR CARGA CRD. INSTITUCIONAL")                       ! Rastro en SJ
      Call imprimeCJ("MENSAJES VOUCHER  NO EXISTE ")                         ! Rastro en SJ
      estado  = 0                                                            ! Proyecto desactivado
      exit sub
   endif

   GR.CRD.MSGLINEA:
   close 80

   ! Apertura archivo formato mensajes adicionales
   TS.ER.RETURN = -1
   open "R::$FPAGAR2" as 80 NOWRITE NODEL
   if TS.ER.RETURN = -1 then begin
      K% = 1
      if end #80 then GR.CRD.MSGLINEA2                                       ! Si es EOF
      while (1)
         read #80; A$                                                        ! Lectura registro hasta eof()
         cri.Crd.Msg2$(K%) = A$
         K% = K% + 1
         If k% > 20 Then GoTo GR.CRD.MSGLINEA2
      wend
   endif else begin
      Call muestraBorrar("ERROR PARAMETROS    ","TFPAGAR2            ")      ! Msg Alerta
      Call muestraBorrar("CREDITO INSTITUCIONA","NO OPERATIVO        ")      ! Msg Alerta
      Call imprimeCJ("ERROR CARGA CRD. INSTITUCIONAL")                       ! Rastro en SJ
      Call imprimeCJ("MENSAJES VOUCHER2 NO EXISTE ")                         ! Rastro en SJ
      estado  = 0                                                            ! Proyecto desactivado
      exit sub
   endif

   GR.CRD.MSGLINEA2:
   close 80

end sub

SUB MSMTINST14 PUBLIC
   string    f$, a$
   integer*4 v
   Integer*2 Xi%

   ! Valida estado funcionalidad on u off
   if not estado then exit sub

   ! Imprime duplicado comprobante credito institucional [77 CONEX]
   if (TS.IO.KEYS(5) = 61) and (TS.IO.MOTORKEY = 61) and (TS.IO.STATE = 10) then begin
      if esnumero(TS.IO.DATA$(5)) then begin
         if (val(TS.IO.DATA$(5)) = 77) then begin
            Call Duplicado.Crd.Institucional
            goto salir
         endif
      endif
      exit sub
   endif

   ! Impide ANULA TOTAL si existen pagos de credito institucional
   if ( TS.IO.KEYS(1) = 70 ) and ( TS.IO.KEYS(6) = 81 ) and ( TS.IO.STATE = 10 ) and \
      ( TS.IO.MOTORKEY = 81 ) and existen.pagos.CRI then begin
      Call muestraBorrar("NO PERMITIDO EXISTEN","PAGO CRED.INSTITUCIO")
      goto salir
   endif

   !Impide SUSPENDER transaccion si existen pagos de credito institucional
   if ( TS.IO.KEYS(1) = 82 ) and ( TS.IO.KEYS(6) = 81 ) and ( TS.IO.STATE = 10 ) and \
      ( TS.IO.MOTORKEY = 81 ) and existen.pagos.CRI then begin
      Call muestraBorrar("NO PERMITE SUSPENCIO","CON PAGO CRED.INST")
      goto salir
   endif

   ! Consulta cupo disponible de credito institucional [Tecla motora 98]
   if (TS.IO.MOTORKEY = 98) and (TS.IO.KEYS(2) = 98) and (TS.IO.STATE = 10) then begin
      cri.accion  = 0
      cri.proceso = 0
      cri.ints.gui = -1
      if leeTarjetaMsr = 0 then begin
         goto salir
      endif
      Call CONSULTA.CRD.INSTITUCIONAL
      goto salir
   endif

   ! Ingreso de un pago Credito Institucional tecleando
   ! con secuencia
   ! 3 / monto tecla 95        --> para un pago
   ! ANULA 3 / monto tecla 95  --> para una anulacion de pago

   if ( TS.IO.MOTORKEY = 95  ) and ( TS.IO.KEYS(7) = 95  ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3)= "3" ) and \
      ( TS.IO.STATE = 10 ) and TS.INTRX then begin

      if java.Init = -1 Then begin

         if NOT TS.BAL.TAKEN then begin
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            exit sub
         endif

         jGuiSubState = 80538
         Call javaEvent(terminalSubStateMsg)

         a$ = ingresoDatos$( "INGRESE MONTO   " , "" )
         if left$( a$, 1 ) = "A" then goto salir
         if left$( a$, 1 ) <> "P" then goto salir
         a$ = right$( a$, len( a$ ) - 2 )
         TS.IO.DATA$(7) = a$	

         jGuiSubState = 0

      endif

      if TS.IO.DATA$(7) = "" then begin
         TS.GUIDANCE = 1003 !B003 SECUENCIA DE TECLAS INCORRECTA
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      ! valida para la trx este totalizada
      if not TS.BAL.TAKEN then begin
         TS.GUIDANCE = 1020
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      ! Control para impedir ingresar + de n pagos con Credito Institucional
      if cri.cant.pagoCI = cri.cant.max.pagos then begin
         Call muestraBorrar("NO PERMITIDO MAS DE ","[" + str$(cri.cant.max.pagos) + "] PAGO CRED.INST")
         goto salir
      endif

      ! Se valida que el monto ingresado sea valido
      if not esnumero( TS.IO.DATA$(7) ) then begin
         Call muestraBorrar("MONTO INVALIDO      ","REINTENTE           ")
         goto salir
      endif
      valorConsumo = val( TS.IO.DATA$(7) )

      ! Se valida que monto no sea cero
      if valorConsumo = 0 then begin
         Call muestraBorrar("MONTO INGRESADO     ","NO PUEDE SER CERO   ")
         goto salir
      endif

      if TS.IO.KEYS(1) = 70 then cri.anula.pago = -1 \
      else cri.anula.pago = 0

      ! se valida que lo pagado no supere el valor de la trx
      if ( cri.anula.pago = 0 ) and (( ( MSMTFACTtotal - NUOTPROMdescuento ) - TS.TENDERED(0) ) < valorConsumo) then begin
         Call muestraBorrar("MONTO A PAGAR","SUPERIOR A DEUDA")
         goto salir
      endif

      ! si sale de la lectura sin deslizar tarjeta
      cri.ints.gui = 0
      if leeTarjetaMsr = 0 then begin
         goto salir
      endif

!--- Control registro CRI con domiciliarios 31Jul2024

      If Gr.Domi.Intrx% = -1 Then Begin																			 ! Si en proceso de domiciliario
       If Gr.Domi.Lista$(Gr.Domi.Delivery%,2) = "9999" Then GoTo continuaCri ! Si no valida CRI
       If Gr.Domi.Delivery% <> 1 Then Begin																	 ! Si diferente a Rappi
       	If Left$(criMsrData$,12) = Left$(Gr.Domi.Lista$(Gr.Domi.Delivery%,2),12) Or \! Si tarjeta no valida
       	   Left$(criMsrData$,12) = Left$(Gr.Domi.Lista$(Gr.Domi.Delivery%,3),12) Then Begin
       	   	
       	EndIf Else Begin
       		Call muestraBorrar("TARJETA NO VALIDA   ","PAGO CRI DELIVERY") 
       		GoTo salir 
        EndIf
       EndIf																																 ! fin control domiciliario
      EndIf	Else Begin																											 ! Fin proceso domiciliario
        For Xi% = 1 To 3
       	If Left$(criMsrData$,12) = Left$(Gr.Domi.Lista$(Xi%,2),12) Or \! Si tarjeta no valida
       	   Left$(criMsrData$,12) = Left$(Gr.Domi.Lista$(Xi%,3),12) Then Begin
       		Call muestraBorrar("TARJETA NO VALIDA   ","SOLO PAGO DELIVERY") 
       		GoTo salir 
       	EndIf
      EndIf

!--- Fin control registro CRI domiciliarios
      continuaCri:
      !pago o anulacion de pago
      if cri.anula.pago then begin
         cri.accion = 2
         v = consultaPago( str$(valorConsumo), criMsrData$, cri.Crd.Pago$ )

      endif else begin
         cri.accion  = 0
         v = descuentoPago( str$(valorConsumo), criMsrData$, cri.Crd.Pago$ )

      endif
      valorDescuento = v
      if v > 0 then valorConsumo = valorConsumo - v

!Call logINT("MSMTINST14 valorConsumo Ajustado =[" + str$(valorConsumo) + "]" )

      cri.proceso = 1
      if CONSULTA.CRD.INSTITUCIONAL = 0 then begin
         cri.anula.pago = 0
         goto salir
      endif else begin
         if cri.anula.pago then begin
            ! Inyecta anulacion del pago credito institucional
            Call InyectaPagoCrdInstitucional
            exit sub
         endif
      endif

     f$ = cupoDisponible$

     Call format.add.decimal( f$ )

     cupoDisponible$ = f$

     !Se valida que se tenga cupo disponible
     if esnumero(cupoDisponible$) then begin
        if val(cupoDisponible$) < valorConsumo then begin
           Call muestraBorrar("CUPO INSUFICIENTE   ","SALDO USD=" + cupoDisponible$)
           goto salir
        endif
     endif else begin
        Call muestraBorrar("CUPO INVALIDO","VALIDE EN SERV.CLIEN")
        goto salir
     endif

     !se envia requerimiento por consumo operacion = 1
     cri.accion  = 1
     cri.proceso = 1

      if CONSULTA.CRD.INSTITUCIONAL = 0 then begin
         Call muestraBorrar("ERROR CONSULTA[1][1]","AL ENVIAR CONSUMO")
         goto salir
      endif Else begin

      Endif

      Call InyectaPagoCrdInstitucional

   endif
   exit sub

   salir:
   Call ignoraTeclas

END SUB

SUB MSMTINST20 PUBLIC
   integer*1 f,ind
   string    m$, trailer$

   if ( not estado ) or ( not criInTrx% ) or ( criPago% <> 1 ) then exit sub

   if ( TS.LINETYPE = 2 ) and ( TS.LINEDATA = 0 ) and ( criPago% = 1 ) then begin
      Call QXL.TSUPEC21( "CRD "+ criMsrData$ )
      criPago% = 0
   endif

END SUB

! calcula impuestos
sub criCalculaImpuestos( x, exc$, net$, iva$ )
   integer*1 x                                         ! indice
   real      r                                         ! factor de participacion del pago Cre.Inst sobre el total de pagos
   string    exc$,                                   \ !
             net$,                                   \ !
             iva$                                      !
   integer*4 t0, t1, t2

   exc$ = ""
   net$ = ""
   iva$ = ""

   valorConsumo = val(cri.Crd.ImpVoucher$( x, 2 ))

   !Calcula la proporcion de las tarifas y el descuento relacionado al pago
   if ( valorConsumo > 0 ) and ( valorConsumo < ( TS.TOTALS(0,0,0) - NUOTPROMdescuento ) ) then begin
      r = ( 1.0 * valorConsumo ) / ( TS.TOTALS(0,0,0) - NUOTPROMdescuento )
   endif else begin
      r = 1
   endif

   t0 = baseTarifa(0)
   t1 = baseTarifa(1)
   t2 = baseTarifa(2)

!Call traceNUO("criCalculaImpuestos t0=" + str$(t0) + " t1=" + str$(t1) + " t2=" + str$(t2) + " r=" + str$(r) )

   exc$ = str$( round( r * t0, 0, 0 ) )
   net$ = str$( round( r * t1, 0, 0 ) )
   iva$ = str$( round( r * t2, 0, 0 ) )

!Call traceNUO("criCalculaImpuestos exc$=" + exc$ + " net$=" + net$ + " iva$=" + iva$ + " r=" + str$(r) )

end sub

sub cri.graba.comprobante( x, exc$, net$, iva$ )
   integer*1 y, x
   string    f$, g$, m$, net$, iva$, exc$, k$

   g$ = ""
   k$ = ""
   cri.recibo$ = ""

   cri.registro.pago$ = cri.Crd.ImpVoucher$( x,  4 )
   cri.empresa$       = cri.Crd.ImpVoucher$( x,  8 )
   cri.nombreCliente$ = cri.Crd.ImpVoucher$( x,  9 )
   cri.cedulaCliente$ = cri.Crd.ImpVoucher$( x, 10 )

   for y = 1 to 8

      f$ = buscaCampo(cri.registro.pago$, separador, y)

      if y = 2 then begin 
      	 f$ = "63" : k$ = "63"
      EndIf
      if y = 6 then f$ = f$ + separador + "3"
      if y = 1 then \
         g$ = f$ \
      else \
         g$ = g$ + separador + f$
   next y

   g$ = g$ + separador + formateaMonto( val( net$ ) ) + \       ! valor base tarifa 12%
             separador + formateaMonto( val( iva$ ) ) + \       ! valor impuesto tarifa 12%
             separador + formateaMonto( val( exc$ ) ) + \       ! valor base tarifa 0%
             separador + cri.empresa$ + \                       ! empresa Afiliada
             separador + cri.nombreCliente$ + \                 ! nombre Beneficiario
             separador + cri.cedulaCliente$                     ! iD Beneficiario

! If K$ = "" Then Begin   
!   Call tracenuo("Accion g$:"+Str$(cri.accion)+" TRCISND:"+g$)   ! Trama envio crd institucional
!   M$ = GrProcCrdi(g$,cri.accion)				 ! Consulta de saldo
!   Call tracenuo("RtaCri g$:"+m$)
! EndIF 

!  cri.recibo$ = buscaCampo(m$,separador,7)

!--- Asigna numero de recibo GR-OVS 24Nov2023
   cri.recibo$ = Right$( "000" + TS.STORE$, 3 )    + \
                 Right$( "000" + TS.TERMINAL$, 3 ) + \
                 Right$( "000" + Str$( SL.HD.TRANSNUM + 1 ), 3 )

   cri.Crd.ImpVoucher$(x,11) = cri.recibo$

end sub

sub cri.graba.cadenaTSL( x, exc$, net$, iva$ )        ! genera y graba cadena 99 20
   integer*4 x
   string    cri.ope$,                              \ ! 01 pago 11 anulacion de pago
             cri.mon.pag$,                          \ ! monto del pago
             exc$,                                  \ !
             net$,                                  \ !
             iva$

   cri.ope$      = cri.Crd.ImpVoucher$( x,  3 )
   cri.header$   = cri.Crd.ImpVoucher$( x, 12 )
   criMsrData$ = cri.Crd.ImpVoucher$( x,  1 )
   cri.mon.pag$  = cri.Crd.ImpVoucher$( x,  2 )

   cri.cadena.venta$ = pack$("99")                              + ":" + \
                       pack$("20")                              + ":" + \
                       pack$(cri.ope$)                          + ":" + \         ! pago con cred.inst
                       pack$(right$(cri.header$,14))            + ":" + \         ! fecha hora aaaammddhhmmss
                       pack$(str$(MSMTFACTnumeroFactura + 1))   + ":" + \         ! folio factura
                       pack$(criMsrData$)                       + ":" + \         ! numero tarjeta
                       pack$(cri.mon.pag$)                      + ":" + \         ! valor del Pago
                       pack$(net$)                              + ":" + \         ! valor base tarifa 12%
                       pack$(iva$)                              + ":" + \         ! valor impuesto IVA 12%
                       pack$(exc$)                              + ":" + \         ! valor base tarifa 0%
                       pack$(cri.recibo$)                       + ":" + \         ! numero de recibo
                       cri.empresa$                             + ":" + \         ! empresa Afiliada
                       cri.nombreCliente$                       + ":" + \         ! nombre Beneficiario
                       pack$(cri.cedulaCliente$)                        !         ! iD Beneficiario

   Call agregaCadenaTSL( cri.cadena.venta$ )
   if cri.Crd.ImpVoucher$( x, 13 ) = "Pago Anulado" then begin
      cri.ope$ = "11"
      cri.cadena.venta$ = pack$("99")                           + ":" + \
                          pack$("20")                           + ":" + \
                          pack$(cri.ope$)                             + \         ! anula pago con cred.inst
                          right$(cri.cadena.venta$,len(cri.cadena.venta$) - 5)

Call agregaCadenaTSL( cri.cadena.venta$ )

   endif

end sub

sub cri.imprime.comprobante( x, exc$, net$, iva$ )
   integer*1 x                                        !

   string    exc$,                                  \ !
             net$,                                  \ !
             iva$,                                  \ !
             m$,                                    \ !
             trailer$                               \ !

!Call logINT("INGRESO A cri.imprime.comprobante=[" + str$(x) + "] TS.LINETYPE=["+ str$(TS.LINETYPE) + "] TS.LINEDATA=[" + str$(TS.LINEDATA) + "] TS.INTRX=[" + str$(TS.INTRX) + "]" )

   cri.header$ = cri.Crd.ImpVoucher$( x, 12 )

   cri.Crd.ImpVoucher$( x, 5 ) = net$
   cri.Crd.ImpVoucher$( x, 6 ) = exc$
   cri.Crd.ImpVoucher$( x, 7 ) = iva$

   m$ = date$ + right$(cri.header$,6)
   trailer$ = mid$(m$,5,2) + "/" + mid$(m$,3,2) + "/" + left$(m$,2) + " " + \
              mid$(m$,7,2) + ":" + mid$(m$,9,2)                     + " " + \
              right$( "000" + TS.STORE$, 3 )                        + " " + \
              right$( "000" + TS.TERMINAL$, 3 )                     + " " + \
              right$( "0000" + str$( SL.HD.TRANSNUM + 1 ), 4 )      + " " + \
              str$( val( unpack$( TS.OPER$ ) ) )

   if cri.Crd.ImpVoucher$(x , 13) <> "Pago Anulado" then begin
      Call imprimeCJ("          **** COPIA CLIENTE **** ")
      Call IMPRIMA.PAGARE(cri.Crd.Pagare%,x)                                 ! Imprime Formato
      Call imprimeCJ(trailer$)
      write #34; NUO.SaltaLineas$ + chr$( 4 ) + NUO.CortaPapel$                                             ! Corta Papel
      Call IMPRIMA.PAGARE(cri.Crd.Pagare%,x)
      Call imprimeCJ(trailer$)
      write #34; NUO.SaltaLineas$ + chr$( 8 ) + NUO.CortaPapel$
   endif

!Call logINT("SALIDA DE cri.imprime.comprobante" )

end sub

SUB MSMTINST30 PUBLIC
   integer*1 x
   string aux$, net$, iva$, exc$
   real   r

   if not estado then exit sub
   if criPago% = 2 then begin
      criPago% = criPago% - 1
      aux$ = "01"                   ! 01=Pago 11=anulacion de pago
      if not cri.anula.pago then begin

         cri.Crd.ImpVoucher$(0,0) = str$(cri.cant.pagoCI)
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,1) = criMsrData$
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,2) = str$(valorConsumo) ! monto del credito (sin el descuento al pago)
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,3) = aux$               ! 01 si es pago 11 si es anulacion de pago para cadena 99
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,4) = cri.registro.pago$ ! necesario para generar mensaje que se envia al
                                                                     ! agente, asi este graba el comprobante para su reimpresion
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,5)  = ""                ! vacio se calculara cuando TS.BALDUE(0) sea cero "0"
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,6)  = ""                ! vacio se calculara cuando TS.BALDUE(0) sea cero "0"
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,7)  = ""                ! vacio se calculara cuando TS.BALDUE(0) sea cero "0"
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,8)  = cri.empresa$
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,9)  = cri.nombreCliente$
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,10) = cri.cedulaCliente$
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,11) = ""                ! Vacio se obtendra cuando se ejecute cri.graba.comprobante
         cri.Crd.ImpVoucher$(cri.cant.pagoCI,12) = cri.header$       ! para la generacion de la cadena 99 20

      endif
   endif

   if ( cri.cant.pagoCI <> 0 ) and ( TS.BALDUE(0) <= 0 ) then begin

      for x = 1 to cri.cant.pagoCI
          Call criCalculaImpuestos( x, exc$, net$, iva$ )                   ! calcula impuestos
          Call cri.graba.comprobante( x, exc$, net$, iva$ )                 ! genera y envia comprobantes al agente
          Call cri.graba.cadenaTSL( x, exc$, net$, iva$ )                   ! genera y graba cadena al TSL Pago y anulacion de pago
          Call cri.imprime.comprobante( x, exc$, net$, iva$ )               ! imprime comprobantes
      next x

   endif

   ! Obligar a totalizar despues de una anulacion de pago con Cred.Inst
   if cri.anula.pago then TS.BAL.TAKEN = 0

END SUB

!----------------------------------------------------------------------------
! Devuelve el campo ubicado en la posicion 'campo' del string 'dato$'
!----------------------------------------------------------------------------
function sacaCampoTSL$(dato$, campo, opc) public
   integer*2 i, k, campo, opc
   string    dato$, sacaCampoTSL$, f$, t$

   t$ = dato$
   for i=1 to campo
      k = match(":", t$, 1)
      if(k > 0) then begin
         f$ = left$(t$,k-1)
         if opc = 0 then f$ = unpack$(f$)
         t$ = right$(t$, len(t$)-k)
      endif else begin
         f$ = t$
         if opc = 0 then f$ = unpack$(f$)
         t$ = ""
      endif
      sacaCampoTSL$ = f$
   next i
end function

SUB MSMTINST53 PUBLIC
   string m$,f$

   if not estado then exit sub

   ! No deben considerarse las cadenas 99 20 en la recuperacion, se deben eliminar
   m$ = SL.STR.ENTRY$
   f$ = getCampoTSL$(m$,1)
   if f$ = "99" then begin
      f$ = getCampoTSL$(m$,2)
      if f$ = "20" then begin
         f$ = getCampoTSL$(m$,3)
         if ( f$ = "01" ) or ( f$ = "11" ) then begin
            TS.TEMP1I2 = 0
            exit sub
         endif
      endif
   endif \
   else begin
      if f$ = "05" or f$ = "06" then begin
         f$ = getCampoTSL$(m$,2)
         if f$ = cri.Crd.Pago$ then begin
            TS.TEMP1I2 = 0
            exit sub
         endif
      endif
   endif

END SUB

SUB MSMTINST68(ENTRY$,ELEMENT) PUBLIC
   string ENTRY$
   integer*2 ELEMENT

   if ( not estado ) or ( len(cri.cadena.venta$) = 0 ) then exit sub

   ! elimina string de pago 05:53 tarjeta de regalo
   if TS.RECOVERY and ( left$(ENTRY$,3) = chr$(5) + ":" + chr$(53H) ) then begin
      Call evitaCadenaTSL( ELEMENT, ENTRY$ )
      exit sub
   endif

   ! elimina string de pago 99:20:01 y anulacion de pago 99:20:11 tarjeta de regalo
   if TS.RECOVERY and ( ( left$(ENTRY$,5) = chr$(99H) + ":" + chr$(20H) + ":" +   chr$(1) ) or \
                        ( left$(ENTRY$,5) = chr$(99H) + ":" + chr$(20H) + ":" + chr$(11H) )) then begin
      Call evitaCadenaTSL( ELEMENT, ENTRY$ )
      exit sub
   endif

END SUB
