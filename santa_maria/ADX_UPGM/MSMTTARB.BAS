!------------------------------------------------------------------------------
! Modulo Transferencia Electronica de Fondos (Tarjetas Bancarias).
! Fecha: 11 de Agosto de 2021.
! Autor: Alexander Baidal M. - NUOCORP.
!------------------------------------------------------------------------------
! Mod 20Nov2023
! Se cambia integraci�n de la aplicaci�n desmontando pipes e integrando con 
! sockets.
! desarrollado por Grupo Retail - CAC/OVS
!------------------------------------------------------------------------------

%ENVIRON T

integer*1 activo,            \ Indicador de proyecto activo
          depurar,           \ Indicador de depuracion
          autoRespuesta,     \ Indicador de autorespuesta activo
          anulaPagoParcial,  \ Indicador de anulacion de pago parcial
          anulaVoucher,      \ Indicador de procedimiento de anulacion de voucher
          reimpresionVoucher,\ Indicador de procedimiento de reimpresion de voucher
          indPagoAnulado,    \ Indice de posicion de pago anulado en arreglo de pagos
          pagoAprobado,      \ Indicador de pago aprobado
          borrarBorrar,      \ Indicador de control cuando se presiona BORRAR/BORRAR
          pagoManualActivo,  \ Indicador de pago manual activo
          pagoManualDebito,  \ Indicador de pago manual debito
          pagoManualCredito  ! Indicador de pago manual credito
          !noPrintVouRecuperaSuspencion ! Indicador de no debe imprimir voucher en recuperacion de suspencion

integer*2 teclaMotora%,          \ Tecla motora para pagos con pinpad
          variedadPago%,         \ Tipo de variedad de pago pinpad
          cantCopiasVoucher%,    \ Cantidad de copias de vouchers
          cantMaxPagos%,         \ Cantidad de pagos m�ximos por transaccion
          teclaConsultaTarifas%, \ Tecla motora para conulta de tarifas
          varPagManDebito%,      \ Tipo de variedad de pago manual debito
          varPagManCredito%      \ Tipo de variedad de pago manual credito

integer*4 descPago               ! monto de descuento al pago

string    midMedianet,       \ Merchant ID de operador Medianet
          tidMedianet,       \ Terminal ID de operador Medianet
          midDatafast,       \ Merchant ID de operador Datafast
          tidDatafast,       \ Terminal ID de operador Datafast
          autorizadorUnico,  \ Tipo de validacion bines o autorizador unico
          operadorDefault,   \ Operador por default,
          archAuditoria,     \ Nombre del archivo de auditoria
          archVoucher,       \ Nombre del archivo de vouchers
          regPagoAnulado,    \ Registro de pago anulado,
          lineaDatosTrx$,    \ Linea con fecha hora tienda caja transaccion operador
          separador


string msg1$,                \ Mensaje a mostrar en linea 1 de visor
       msg2$,                \ Mensaje a mostrar en linea 2 de visor
       forVouCompra$(1),     \ Arreglo con informacion del formato del voucher de compra
       forVouAnulacion$(1),  \ Arreglo con informacion del formato voucher de anulacion
       msgOperacion$(1),     \ Arreglo con informacion de mensajes operativos
       codDiferidos$(1),     \ Arreglo con autorizadores de diferidos
       parBines$(2),         \ Arreglo con parametros de bines
       resPagos$(2)          ! Arreglo con la informacion de respuesta de todos los pagos

integer*2 cantParBines%      ! Cantidad de registros archivo de bines

integer*1 \
       cantLinVouCompra%,   \ Tama�o de arreglo con lineas de informacion del voucher de compra
       cantLinVouAnulacion%,\ Tama�o de arreglo con lineas de informacion del voucher de anulacion
       cantPagos%           ! Cantidad de pagos Exitosos

!Variables para las tramas de envio de comunicacion (Request)
string request,            \ Trama de requerimiento
       reqEncabezado,      \ Encabezado del Mensaje
       reqDetalle,         \ Detalle del mensaje
       reqHora,            \ Hora de requerimiento
       reqMonto,           \ Monto de la transaccion
       reqBaseEx,          \ Base exenta a impuestos
       reqBaseAf,          \ Base afecta a impuestos
       reqImpuesto,        \ Monto del impuesto
       reqFinanciamiento,  \ Tipo de financiamiento 00: Corriente 01:Diferido 02: Debito
       reqCodAplicacion,   \ Codigo de aplicacion
       reqCodFuncion,      \ Codigo de funcion
       reqEstado,          \ Estado del requerimiento
       reqComercio,        \ Codigo de la cadena
       reqNoFactura,       \ Factura de venta
       reqTienda,          \ N�mero de Tienda
       reqTerminal,        \ N�mero de terminal
       reqCajero,          \ Id Cajero
       reqNoTramas,        \ N�mero de tramas
       reqNoTransaccion,   \ N�mero de transacci�n
       reqFecha,           \ Fecha de transaccion
       reqOperacion,        \ Autorizador bancario
       reqIdRed,           \ Id de la red
       reqCodDiferido,     \ Codigo de diferido
       reqCuotas,           \ Plazo del diferido
       reqMesGracia,       \ Meses de gracia
       reqImpServicio,     \ Impuesto de servicio
       reqPropina,         \ Valor de propina
       reqMontoFijo,       \ Monto Fijo
       reqSecuencia,       \ N�mero de secuencia
       reqAutorizacion,    \ N�mero de autorizaci�n
       reqMid,             \ Merchant ID
       reqTid,             \ Terminal ID
       reqNomDiferido,     \ Descripcion diferido
       reqTipoDiferido     \ Tipo de diferido


!Variables para las tramas de respuesta de comunicacion (Response)
string response,          \ Trama de respuesta
       resHora,           \ Hora de respuesta
       resCodigo$,        \ C�digo de resuesta
       resOpeCorriente,   \ ID operador transacciones corrientes
       resOpeDiferido,    \ ID operador transacciones diferidas
       resNroTarjeta,     \ N�mero de tarjeta
       resBin,            \ Bin de tarjeta
       resCodigo,         \ Rta Trx
       resCodPinpad,      \ Codigo rta ejecucion pinpad
       resIdRed,          \ Codigo identificador red
       resMsgPinpad,      \ Msg rta pin pad
       resSecuencia,      \ Secuencial trx
       resLote,           \ Nro Lote
       resFecha,          \ Fecha trx
       resAutorizacion,   \ Nro Autorizacion
       resTid,            \ Terminal ID
       resMid,            \ Merchat ID
       resCuotas,         \ Plazo del diferido
       resInteres,        \ Valor de los intereses
       resCodBanco,       \ Codigo banco adquirente
       resNomBanco,       \ Nombre banco adquirente
       resNomFranquicia,  \ Nombre Franquicia
       resModLectura,     \ Modo de lectura
       resNomCliente,     \ Nombre  Tarjeta habiente
       resIdApp,          \ ID APPL EMV solo chip
       resMonto,          \ Monto de la transaccion
       resBaseEx,         \ Base exenta a impuestos
       resBaseAf,         \ Base afecta a impuestos
       resImpuesto,       \ Monto del impuesto
       resCodDiferido,    \ Codigo de diferido
       resMesGracia,      \ Meses de plazo de diferido
       resNomDiferido,    \ Descripcion diferido
       resTipoDiferido,   \ Tipo de Interes con o sin intereses
       resAID,            \ AID-EMV
       resEMV,            \ Valor EMV solo chip
       resARQC,             \ ARQC solo chip
       resMsgPromocion,     \ Mensaje promocion
       resValPin,         \ Validacion del PIN
       resTVR,             \ TVR de la transaccion
       resTSI,            \ TSI de la transaccion
       resAnulado         ! Indica si el voucher fue anulado

! Variables de comunicacion pi: hacia JAVA
string pipeInName,          \ Nombre de pipe de lectura JAVA
       pipeOutName          \ Nombre de pipe para escritura JAVA

integer*4 \
       bufferSize%,         \ Numero de bytes a escribir
       pipeTimeout% ,       \ Tiempo maximo de espera para lectura de pipe creado por JAVA con la trama de respuesta
       pipeSleep%           \ Tiempo de espera para enviar nuevo mensaje por medio del pipe

integer*2 \
       pipeInSession%,      \ Sesion para apertura de pipe de lectura
       pipeOutSession%      \ Sesion para apertura de pipe de escritura
        
Integer*4 Global GR.IVA.DEVOL%                  ! Devolucion IVA

!------------------------------------------------------------------------------
! Variables globales de otros modulos
!------------------------------------------------------------------------------
%INCLUDE EAMTRANS.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE JAVAGUIV.J86

integer*1 Global                   \
   TS.RETV.IN.PROGRESS             \ retrieval in progress

!------------------------------------------------------------------------------
! Rutinas publicas de otros modulos
!------------------------------------------------------------------------------
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86
%INCLUDE NUOTPROM.J86
%INCLUDE NUOTJAVA.J86
%INCLUDE JAVAGUIC.J86

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

!------------------------------------------------------------------------------
! Tono de alerta
!------------------------------------------------------------------------------
Sub TSHIECET external
End Sub

!para grabar en el journal electronico
Function  QXL.TSUPEC21(PRTLINE) EXTERNAL
   STRING QXL.TSUPEC21, PRTLINE
FEND

!------------------------------------------------------------------------------
!Escribe log de auditoria
!t$ titulo
!m$ mensaje
!------------------------------------------------------------------------------
Sub traceTAR(t$, m$)
   string t$, m$
   if not depurar then Exit Sub
   if t$ <> "" then call traceNuo(t$)
   call traceNuo(m$)
End Sub

Sub TEFEC.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::GRLOG:PE" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

!------------------------------------------------------------------------------
! Graba log de auditoria de tramas de comunicacion
!------------------------------------------------------------------------------
Sub grabaAuditoria
   String formato, final, linea
   Integer*4 largo

   !Close 47
   TS.ER.RETURN = -1

   Open archAuditoria As 47 Append
   If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
      TS.ER.RETURN = -1
      Create archAuditoria As 47
      If TS.ER.RETURN <> -1 Then Begin
         call muestraOperador("ERROR EN CREACION","DE AUDITORIA ")
         wait; 1500
         call restauraDisplay
         Exit Sub
      EndIf 
   EndIf 

   final = chr$( 13 ) + chr$( 10 )
   linea = "[" + reqFecha + ":" + reqHora + "]" + "MSG:" + request
   largo = len( linea )
   formato = "C"+ str$( largo ) + " C2"
   write form formato; #47 ; linea, final
   linea = "["+ date$ +":"+ time$ +"]" + "RTA:" + response
   largo = len( linea )
   formato = "C"+ str$( largo ) + " C2"
   Write Form formato; #47 ; linea, final
   close 47

End Sub

!------------------------------------------------------------------------------
! Lee mensajes de operacion de tarjetas bancarias
! archivo R::ADX_UDT1:PARMSGOP.DAT
!------------------------------------------------------------------------------
Function  leeMensajesOperacion
   integer *1 i, k, w, leeMensajesOperacion
   integer*4 inicioBloque
   string n$, v$

   leeMensajesOperacion = -1
   close 80
   v$ = "[TARJETAS BANCARIAS]"
   inicioBloque = abreParametros( "R::ADX_UDT1:PARMSGOP.DAT", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARMSGOP" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      leeMensajesOperacion = 0
      Exit Function 
   EndIf

   w = cargaLineas( 80, inicioBloque )

   if w > 0 then begin
      dim msgOperacion$( w )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          msgOperacion$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   EndIf

   call cierraParametros( 80 )

End Function 

!------------------------------------------------------------------------------
! Lee formato de voucher
! archivo "R::ADX_UDT1:PARVOUCH.DAT"
!------------------------------------------------------------------------------
Function  leeFormatoVoucher
   integer *1 i, l, k, w, leeFormatoVoucher
   integer*4 inicioBloque
   string n$, v$

   cantLinVouCompra%= 0
   cantLinVouAnulacion%= 0
   leeFormatoVoucher = -1
   close 80

   v$ = "[TARJETAS BANCARIAS COMPRA]"
   inicioBloque = abreParametros( "R::ADX_UDT1:PARVOUCH.DAT", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      leeFormatoVoucher = 0
      Exit Function 
   EndIf

   w = cargaLineas( 80, inicioBloque )
   if w > 0 then begin
      cantLinVouCompra% = w
      dim forVouCompra$( cantLinVouCompra% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouCompra$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   EndIf

   v$ = "[TARJETAS BANCARIAS ANULACION]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARVOUCH" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      leeFormatoVoucher = 0
      Exit Function 
   EndIf

   w = cargaLineas( 80, inicioBloque )

   if w > 0 then begin
      cantLinVouAnulacion% = w
      dim forVouAnulacion$( cantLinVouAnulacion% )
      k = 1
      for i = 1 to w
          call rescataParametro( i, n$, v$ )
          forVouAnulacion$( k ) = mid$( v$, 2, len(v$)-2 )
          k = k + 1
      next i
   EndIf

   call cierraParametros( 80 )

End Function 

!------------------------------------------------------------------------------
! Lee archivo de bines
!------------------------------------------------------------------------------
Function  leeParametrosBines
   string binInicial, \
       binFinal, \
       red1, \
       red2
   integer*1 leeParametrosBines

   close 80
   dim parBines$( 1000, 3 )
   leeParametrosBines = -1
   TS.TS11WERR$ = ""
   open "R::ADX_IDT1:TEFBINES.DAT" as 80 nowrite nodel

   if TS.TS11WERR$ <> "" then begin
      call muestraBorrar( "ERROR APERTURA", "ARCHIVO BINES" )
      goto salir:
   EndIf

   cantParBines% = 0
   if End #80 then salir

   while (1)
     read # 80 ; binInicial, binFinal, red1, red2
     cantParBines% = cantParBines% + 1
     parBines$( cantParBines%, 0 ) = binInicial
     parBines$( cantParBines%, 1 ) = binFinal
     parBines$( cantParBines%, 2 ) = red1
     parBines$( cantParBines%, 3 ) = red2
   wend

   salir:
   call cierraParametros( 80 )

End Function 

!------------------------------------------------------------------------------
! Lee par�metros de diferidos
! archivo "R::ADX_IDT1:TF2PARAM.DAT"
!------------------------------------------------------------------------------
Function  leeParametrosDiferidos
   integer*1 leeParametrosDiferidos
   INTEGER*4 REC.NO, MAX.R.SEC, REC.LEN, BLK.NUM, R.S, R, KEY.LEN, X
   STRING    H$, PZERO$, KEY$, C01$, XP$
   DIM codDiferidos$(5)

    close 80
   leeParametrosDiferidos = 0                                       				! DIMENCIONA LISTA DIFERIDOS
   TS.ER.RETURN = -1
   OPEN "R::ADX_IDT1:TF2PARAM.DAT" DIRECT RECL 512 AS 80                    ! APERTURA CONTROL DIFERIDOS

   IF TS.ER.RETURN <> -1 THEN Exit Function                            			! FALLA APERTURA
   REC.NO = 1                                                       				! INITIALIZE COUNTER
   I% = 1                                                            				! INITIALIZE COUNTER
   READ FORM "T43 I4 I2 T55 I2 C456"; #80,REC.NO;                          \! READ FIRTS RECORD
        BLK.NUM, REC.LEN, KEY.LEN, H$
   PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                                   ! ARMED KEY CONTROL
   IF REC.LEN <> 0 THEN \
      MAX.R.SEC = 508/REC.LEN ELSE                                         \! LENGTH RECORD
      MAX.R.SEC = 0                                             						!
   FOR REC.NO = 2 TO BLK.NUM                                                ! CICLE TO READ ALL BLOCKS
     READ FORM "T5 C508"; #80, REC.NO;  H$                                  ! H$ CONTAINS BLOCK
     X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                            ! EXTRACT FIRST KEY
     WHILE KEY$  NE  PZERO$                                                 ! INSIDE SECTOR LOOP
       R.S = R.S + 1                                                        ! RECORDS ON THIS SECTOR
       R = R + 1
       C01$ = UNPACK$(MID$(H$,X+0,2))                                       ! CODIGO DIFERIDO Y AUTORIZADOR
       XP$  = MID$(C01$,2,1)                                                ! CODIGO AUTORIZADOR
       codDiferidos$(VAL(XP$)) = codDiferidos$(VAL(XP$)) +                 \! ARMA LISTA DIFERIDOS
                                 STR$(VAL(RIGHT$(C01$,2))) + ","

       !call traceTAR("leeParametrosDiferidos$", "XP$=["+ XP$ + "]" +      \!
       !                      "codDiferidos$=["+ codDiferidos$(VAL(XP$)) + "]")

       X = X + REC.LEN                                                      ! INDEX TO NEXT KEY
       KEY$ = MID$(H$,X,KEY.LEN)                                            ! PICK UP NEXT KEY
       IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                                ! IF EOF() RECORD OR FILE
     WEND
   NEXT REC.NO
   CLOSE 80
   leeParametrosDiferidos = -1

End Function 

!------------------------------------------------------------------------------
! Lee par�metros de terminal pinpad
! archivo "R::ADX_IDT1:TEFPARAM.###"  ### = N�mero de terminal
!------------------------------------------------------------------------------
Function  leeParametrosTerminal
   string    r
   integer*1 k, leeParametrosTerminal
   string    v$
   integer*4 inicioBloque

   midMedianet = ""
   tidMedianet = ""
   midDatafast = ""
   tidDatafast = ""
   autorizadorUnico = ""
   operadorDefault = ""

   leeParametrosTerminal = -1
   close 80

   v$ = "[TEF II ECUADOR]"
   inicioBloque = abreParametros( "R::ADX_IDT1:TEFPARAM."+Right$("000"+TS.TERMINAL$,3) , v$ , 80 )
   if inicioBloque = -1 then begin
      leeParametrosTerminal = 0
      call muestraBorrar( "NO " + v$, "EN ARCHIVO TEFPARAM" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      Exit Function 
   EndIf

   v$ = leeParametro$( "MID_MEDIANET", 80, inicioBloque )
   if v$ <> "" then begin
      midMedianet = trim$( v$ )
   EndIf

      v$ = leeParametro$( "TID_MEDIANET", 80, inicioBloque )
      if v$ <> "" then begin
         tidMedianet = trim$( v$ )
      EndIf

      v$ = leeParametro$( "MID_DATAFAST", 80, inicioBloque )
      if v$ <> "" then begin
         midDatafast = trim$( v$ )
      EndIf

      v$ = leeParametro$( "TID_DATAFAST", 80, inicioBloque )
      if v$ <> "" then begin
         tidDatafast = trim$( v$ )
      EndIf

      v$ = leeParametro$( "PROCESO_BINES_AUTHUNICO", 80, inicioBloque )
      if v$ <> "" then begin
         autorizadorUnico = trim$(v$)
      EndIf

      v$ = leeParametro$( "CODIGO_OPERADOR_UNICO", 80, inicioBloque )
      if v$ <> "" then begin
         operadorDefault = trim$(v$)
      EndIf

   call cierraParametros( 80 )

End Function 

!retorna contenido variable activo, que es el status del modulo tarjetas
Function  status.tarjetas.tef Public 
   integer*1 status.tarjetas.tef

   status.tarjetas.tef = 0

   status.tarjetas.tef = activo

End Function 

!retorna el medio de pago SMA del pago tarjera de credito tef
Function  get.MP.Tef public
   integer*2 get.MP.Tef

   get.MP.Tef = variedadPago%

End Function 

!retorna la tecla motora del pago tarjera de credito tef
Function  getTeclaMotoraTef public
   integer*2 getTeclaMotoraTef

   getTeclaMotoraTef = teclaMotora%

End Function 

!------------------------------------------------------------------------------
!Lectura de parametros del archivo c:/ADX_UDT1/PARGENER.DAT
!incorporar a USER 7
!------------------------------------------------------------------------------
Sub leeParametros
   string    r
   integer*1 k
   string    v$
   integer*4 inicioBloque

   activo = 0
   depurar = 0
   autoRespuesta = 0
   teclaMotora% = 256
   variedadPago% = 0
   cantCopiasVoucher% = 0
   pipeInName= "pi:INTEF"
   pipeOutName= "pi:OUTTEF"
   bufferSize%= 300
   pipeTimeout% = 5000
   pipeSleep% = 4000
   pipeInSession%= 59
   pipeOutSession%= 60
   pagoManualActivo = 0
   teclaConsultaTarifas% = 256
   varPagManDebito% = 0
   varPagManCredito% = 0
   cantMaxPagos% = 0

   close 80
   call imprimeCJ("***** MODULO TARJETAS BANCARIAS ******")
   v$ = "[TARJETAS BANCARIAS]"
   inicioBloque = abreParametros( "R::$ARGENER", v$ , 80 )
   if inicioBloque = -1 then begin
      !call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO  3 Dic 2022" )
      call cierraParametros( 80 )
      Exit Sub
   EndIf

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin

      if esNumero( v$ ) then k = val(v$) else k = 0
      if (k and 1) then activo = -1
      if (k and 2) then depurar = -1
      if (k and 4) then autoRespuesta = -1

      v$ = leeParametro$( "TECLA_MOTORA", 80, inicioBloque )
      if esNumero( v$ ) then teclaMotora% = val(v$)

      v$ = leeParametro$( "TIPO_VARIEDAD_PAGO", 80, inicioBloque )
      if esNumero( v$ ) then variedadPago% = val(v$)

      v$ = leeParametro$( "NRO_COPIAS_VOUCHER", 80, inicioBloque )
      if esNumero( v$ ) then cantCopiasVoucher% = val(v$)

     !Tiempo de espera respuesta motor
      v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
      if esNumero( v$ ) then pipeTimeout% = val(v$)

      !Pipe de lectura JAVA
      v$ = leeParametro$( "PIPEIN", 80, inicioBloque )
      if v$ <> "" then \
         pipeInName = trim$( v$ )

      !Pipe de escritura JAVA
      v$ = leeParametro$( "PIPEOUT", 80, inicioBloque )
      if v$ <> "" then \
         pipeOutName = trim$( v$ )

      !Tama�o del pipe
      v$ = leeParametro$( "BUFFER_SIZE", 80, inicioBloque )
      if esNumero( v$ ) then bufferSize% = val(v$)

      !Sesion para uso de pipe de lectura
      v$ = leeParametro$( "PIPEIN_SESION", 80, inicioBloque )
      if esNumero( v$ ) then pipeInSession% = val(v$)

      !Sesion para uso de pipe de escritura
      v$ = leeParametro$( "PIPEOUT_SESION", 80, inicioBloque )
      if esNumero( v$ ) then pipeOutSession% = val(v$)

      !Tiempo de espera para abrir nueva comunicacion PI:
      v$ = leeParametro$( "PIPE_OUT_CLOSE_SLEEP", 80, inicioBloque )
      if esNumero( v$ ) then pipeSleep% = val(v$)

      !Numeros de Pagos Maximo
      v$ = leeParametro$( "NRO_PAGOS_MAXIMO", 80, inicioBloque )
      if esNumero( v$ ) then cantMaxPagos% = val(v$)

      !Tecla consulta de tarifas
      v$ = leeParametro$( "TECLA_CONSULTA_TARIFAS", 80, inicioBloque )
      if esNumero( v$ ) then teclaConsultaTarifas% = val(v$)

      !Pago manual activo
      v$ = leeParametro$( "PAGO_MANUAL_ACTIVO", 80, inicioBloque )
      if esNumero( v$ ) then k = val(v$) else k = 0
      if (k and 1) then pagoManualActivo = -1

      !Tipo de variedad de pago manual debito
      v$ = leeParametro$( "TIPO_VARIEDAD_DEBITO", 80, inicioBloque )
      if esNumero( v$ ) then varPagManDebito% = val(v$)

      !Tipo de variedad de pago manual credito
      v$ = leeParametro$( "TIPO_VARIEDAD_CREDITO", 80, inicioBloque )
      if esNumero( v$ ) then varPagManCredito% = val(v$)

   EndIf

   call cierraParametros( 80 )

   if not leeMensajesOperacion then activo = 0
   if not leeFormatoVoucher then activo = 0
   if not leeParametrosBines then activo = 0
   if not leeParametrosDiferidos then activo = 0
   if not leeParametrosTerminal then activo = 0

   if activo then r = "ON " else r = "OFF"
   call imprimeCJ( "ESTADO " + r + "  20 Ene 2021" )
   if depurar then begin
      call imprimeCJ( "TIPO_VARIEDAD_PAGO " + right$( string$( 11, " " ) + str$( variedadPago% ), 11 ) )
      call imprimeCJ( "TIMEOUT " + right$( string$( 11, " " ) + str$( pipeTimeout% ), 11 ) )
      call imprimeCJ( "PIPEIN  " + right$( string$( 11, " " ) + pipeInName , 11 ) )
      call imprimeCJ( "PIPEOUT " + right$( string$( 11, " " ) + pipeOutName , 11 ) )
      call imprimeCJ( "TID DAT " + right$( string$( 11, " " ) + tidDatafast , 11 ) )
      call imprimeCJ( "TID MED " + right$( string$( 11, " " ) + tidMedianet , 11 ) )
      if pagoManualActivo then r = "ON " else r = "OFF"
      call imprimeCJ( "P.MANUAL" + right$( string$( 11, " " ) + r , 11 ) )
      call imprimeCJ("ACTIVADO INDICADOR DEPURACION")
      if autoRespuesta then call imprimeCJ("ACTIVADO INDICADOR AUTORESPUESTA")
   EndIf

End Sub

!------------------------------------------------------------------------------
! Simula respuestas de pinpad
! pedirDato%: Indica si se pide tipo de respuesta por teclado
! operacion 84: Lectura de tarjeta
!           88: Compra
! tipo%: Tipo de respuesta a simular
!            0: LECTURA OK 1:RECHAZADO 2:AUTORIZACION OK 3:TRANSACCION CANCELAD
!------------------------------------------------------------------------------
Function  generaAutorespuesta$(pedirDato%, operacion, tipo%)
String generaAutorespuesta$,\
       dato$, \
       m$

Integer*1 \
       pedirDato%, \
       operacion, \
       tipo%

  generaAutorespuesta$ = ""
  m$ = ""

  if operacion = 88 then begin
     if pedirDato% then begin

        jGuiSubState = 80527
        call javaEvent(terminalSubStateMsg) 

        dato$ = ingresoDatos$( "1 RECHAZADO 2 OK", "3 CANCELADA" )
        if left$(dato$, 1) = "P" then begin
           dato$ = right$( dato$, len( dato$ ) - 2 )
           tipo% = val(dato$)
        EndIf
     EndIf

     if tipo% = 1 then m$ = "43882372000690001RECHAZADO PRUEBAS                       28502400001012201920180412414023MS0010031000000101                                                  "
     if tipo% = 2 then m$ = "43882372000000001AUTORIZACION OK.                        28502400001012201920180412414023MS0010031000000101                                                  DISNEY                   03DONALD  PATO/PLUTO                      DinersClub   "
     if tipo% = 2 and reqFinanciamiento = "01" then m$ = "43882372000000002AUTORIZACION OK.                        95000400027518402620190102950004MS032001000000864899                                                MASTERCARD/MAES          03DARIO CALO ORTIZ/                       Debit MasterCard    "
     if tipo% = 2 and reqFinanciamiento = "01" and val( reqCodDiferido ) > 1 then m$ = "43882372000000002AUTORIZACION OK.                        95000400027518402620190102000000MS032001000000864899                                                MASTERCARD/MAES          03DARIO CALO ORTIZ/                       Debit MasterCard    "
     if tipo% = 3 then m$ = "438823720000020  TRANSACCION CANCELAD                                                                                                                                                                                             "

  EndIf

  if operacion = 84 then m$ = "43840752000000000505874XXXXXX7466         LECTURA OK                              "
  if operacion = 85 then m$ = "438505020000000  AUTORIZADO                              "
  if operacion = 89 then m$ = "43891712000A0000006280001      40                                   6DD4BF361CE34200                                                                                80800080006800"
  if operacion = 89 and (anulaPagoParcial or anulaVoucher) then m$ = "43891712000A0000006280001      80                                   FD6D2E0050ED371E                                                                                80800080006800"

  generaAutorespuesta$ = m$

  !call traceTAR("generaAutorespuesta$", "m$=["+ m$ + "]" + \
  !                                              "operacion=["+ str$( operacion ) + "]" + \
  !                                              "tipo%=["+ str$( tipo% ) + "]" + \
  !                                              "reqFinanciamiento=["+ reqFinanciamiento + "]" )


End Function 

!------------------------------------------------------------------------------
! Retorna el valor del campo n a imprimir en voucher
!------------------------------------------------------------------------------
Function  valorVouCompra$( n )
   string valorVouCompra$
   integer*1 n

   valorVouCompra$ = ""
   n = n + 1

   on n goto v00, v01, v02, v03, v04, v05, v06, v07, v08, v09, \
             v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, \
             v20, v21, v22, v23, v24
   Exit Function 

   v00:
   valorVouCompra$ = resMid
   Exit Function 

   v01:
   valorVouCompra$ = resTid
   Exit Function 

   v02:
   valorVouCompra$ = resLote
   Exit Function 

   v03:
   valorVouCompra$ = resModLectura
   Exit Function 

   v04:
   valorVouCompra$ = resNroTarjeta
   Exit Function 

   v05:
   valorVouCompra$ = trim$( resNomFranquicia )
   Exit Function 

   v06:
   valorVouCompra$ = resSecuencia
   Exit Function 

   v07:
   valorVouCompra$ = resAutorizacion
   Exit Function 

   v08:
   valorVouCompra$ = resCuotas
   Exit Function 

   v09:
   valorVouCompra$ = trim$( resNomDiferido )
   Exit Function 

   v10:
   valorVouCompra$ = formateaMonto( val( resInteres ) )
   Exit Function 

   v11:
   valorVouCompra$ = left$( resFecha, 4 )  + "/" + mid$( resFecha , 5, 2 ) + "/" + right$( resFecha , 2 ) + \
      + " " + left$( resHora, 2 )  + ":" + mid$( resHora, 3, 2 ) + ":" + right$( resHora , 2 )
   Exit Function 

   v12:
   if val( resIdRed ) = 1 then valorVouCompra$ = resIdRed + "-" + "DATAFAST"
   if val( resIdRed ) = 2 then valorVouCompra$ = resIdRed + "-" + "MEDIANET"
   if val( resIdRed ) = 3 then valorVouCompra$ = resIdRed + "-" + "AUSTRO  "
   Exit Function 

   v13:
   valorVouCompra$ = formateaMonto( val( resBaseEx ) )
   Exit Function 

   v14:
   valorVouCompra$ = str$(MSMTFACTimpuestoIVA%)
   Exit Function 

   v15:
   valorVouCompra$ = formateaMonto( val( resBaseAf ) )
   Exit Function 

   v16:
   valorVouCompra$ = formateaMonto( val( resImpuesto ) )
   Exit Function 

   v17:
   valorVouCompra$ = formateaMonto( val( resMonto ) )
   Exit Function 

   v18:
   valorVouCompra$ = formateaMonto( val( resMonto ) + val( resInteres ) )
   Exit Function 

   v19:
   valorVouCompra$ = trim$( resNomCliente )
   Exit Function 

   v20:
   valorVouCompra$ = resTVR
   Exit Function 

   v21:
   valorVouCompra$ = resTSI
   Exit Function 

   v22:
   
   Exit Function 
   
   v23:
   
   Exit Function 
   
   V24:
   valorVouCompra$ = formateaMonto( GR.IVA.DEVOL% )     ! Add Devolucion IVA 15Abr2025
   Exit Function 
   
End Function 

!------------------------------------------------------------------------------
! Asigna los valores a imprimir en el voucher
!------------------------------------------------------------------------------
Function  reemplazaLineaVoucher( lv$ )
   integer*2 i, k, n
   string reemplazaLineaVoucher,  \
      lv$, s$, v$

   n = len( lv$ )
   i = match( "&" , lv$, 1 )
   while i > 0
      s$ = mid$( lv$, i + 1, 2 )
      if esNumero( s$ ) then begin
         k = val( s$ )
         v$ = valorVouCompra$( k )
         if ( mid$( lv$, i + 3, 1 ) = "-" ) and ( v$ <> "0.00" ) then s$ = "-" else s$ = " "
         lv$ = left$( lv$, i - 1 ) + v$ + s$ + mid$( lv$, i + 4, n )
      EndIf else begin
         lv$ = left$( lv$, i - 1 ) + "    " + mid$( lv$, i + 4, n )
      EndIf
      i = match( "&" , lv$, i )
   wend

   reemplazaLineaVoucher = lv$

End Function 

!------------------------------------------------------------------------------
! Genera encabezado de mensaje para comunicaci�n con pinpad
!------------------------------------------------------------------------------
Function  generaEncabezado$( operacion )
integer*1 operacion
string generaEncabezado$, m$

   m$ = ""
   generaEncabezado$ = ""
   reqCodAplicacion = ""
   reqCodFuncion = ""
   reqEstado = ""
   reqComercio = ""
   reqNoFactura = ""
   reqTienda = ""
   reqTerminal = ""
   reqCajero = ""
   reqNoTransaccion = ""
   reqFecha = ""
   reqHora = ""

   reqCodAplicacion = right$( string$( 2, "0" ) + "43", 2)
   reqCodFuncion = right$( string$( 2, "0" ) + str$(operacion) , 2)
   reqEstado = "00"
   reqComercio = "0001"
   reqNoFactura = "123456"
   reqTienda = right$( string$( 4, "0" ) + TS.STORE$, 4)
   reqTerminal = right$( string$( 6, "0" ) + str$(SL.HD.TERMINAL), 6)
   reqCajero = unpack$(TS.OPER$)
   reqNoTransaccion = right$( string$( 6, "0" ) + str$(SL.HD.TRANSNUM + 1), 6)
   reqFecha = date$
   reqHora  = time$

   !43881800001 0001 000043 0000013050 123456 000017 210906 122138 00
   !43881800001 0001 000043 0000013050 123456 000005 210906 123910 00

   m$ = reqComercio +       \ Codigo de la cadena
        reqTienda +         \ Numero del almacen
        reqTerminal +       \ Numero de terminal
        reqCajero +         \ Numero de operador
        reqNoFactura +      \ Factura fiscal
        reqNoTransaccion +  \ Numero de transaccion
        reqFecha +          \ Fecha
        reqHora +           \ Hora de la operacion
        reqEstado           ! Estado del requerimiento

   !call traceTAR( "generaEncabezado$", "reqCodAplicacion=["+ reqCodAplicacion + "]" + \
   !                                            "reqCodFuncion=["+ reqCodFuncion + "]" + \
   !                                            "reqComercio=["+ reqComercio + "]" + \
   !                                            "reqTienda=["+ reqTienda + "]" + \
   !                                            "reqTerminal=["+ reqTerminal + "]" + \
   !                                            "reqCajero=["+ reqCajero + "]" + \
   !                                            "reqNoFactura=["+ reqNoFactura + "]" + \
   !                                            "reqNoTransaccion=["+ reqNoTransaccion + "]" + \
   !                                            "reqFecha=["+ reqFecha + "]" + \
   !                                            "reqHora=["+ reqHora + "]" + \
   !                                            "reqEstado=["+ reqEstado + "]")
   generaEncabezado$ = m$

End Function 

!------------------------------------------------------------------------------
! Calcula bases por impuestos
!------------------------------------------------------------------------------
Sub calculaBasesImpuesto public
   integer*4     \
      baseEx%,   \
      baseAf%,   \
      imp%,      \
      tp, mp

   real factor%

   Call tarifasAjustadas( baseEx%, baseAf%, imp% )

   tp = baseEx% + baseAf% + imp%
   mp = val(reqMonto) + GR.IVA.DEVOL% ! Mod 8May2025 adiciona monto iva devol

!call traceNUO( "total=[" + str$(tp) + "] reqMonto=[" + str$(mp) + "] baseAf=[" + str$(baseAf%) + "] baseEx=[" + str$(baseEx%) + "] Impuesto=[" + str$(imp%) + "]" )
!call traceNUO( "descPago=[" + str$(descPago) + "] TENDERED=[" + str$(TS.TENDERED(0)) + "]" )

   !Pagos Parciales
   if (tp > 0) and (mp > 0) and (mp < tp) then begin

      factor% = 1.0 * mp / tp
      baseEx% = round( factor% * baseEx%, 0, 0 )
      baseAf% = round( factor% * baseAf%, 0, 0 )
      imp% = round( factor% * imp%, 0, 0 )

   EndIf

   reqBaseEx = str$( baseEx% )
   reqBaseAf = str$( baseAf% )
   reqImpuesto = str$( imp% )
!call traceNUO( "total=[" + str$(tp) + "] reqMonto=[" + reqMonto + "] baseAf=[" + reqBaseAf + "] baseEx=[" + reqBaseEx + "] Impuesto=[" + reqImpuesto + "]")
End Sub

!------------------------------------------------------------------------------
! Inicializa variables de solicitud
!------------------------------------------------------------------------------
Sub inicializaVariablesRequest

   reqBaseEx = ""
   reqBaseAf = ""
   reqImpuesto = ""
   reqIdRed = ""
   reqMesGracia = ""
   reqImpServicio = ""
   reqPropina = ""
   reqMontoFijo = ""
   reqSecuencia = ""
   reqAutorizacion = ""
   reqMid = ""
   reqTid = ""

End Sub

!------------------------------------------------------------------------------
! Setea valores en variables del mensaje de solicitud
!------------------------------------------------------------------------------
Sub completaValores

   call inicializaVariablesRequest

   if reqOperacion = "01" then begin
      reqIdRed = resOpeDiferido
      reqTipoDiferido = ""
      if reqCodDiferido = "05" then reqMesGracia = "01"
   EndIf
   if reqOperacion = "02" then begin
      reqIdRed = resOpeDiferido
      reqTipoDiferido = ""
      if reqCodDiferido = "05" then reqMesGracia = "01"
   EndIf

   call calculaBasesImpuesto

   !if reqIdRed = "1" then begin
   !   reqTid = tidDatafast
   !   reqMid = midDatafast
   !EndIf else begin
      reqTid = tidMedianet
      reqMid = midMedianet
   !EndIf

End Sub

!------------------------------------------------------------------------------
! Genera detalle de mensaje para comunicaci�n con pinpad
!------------------------------------------------------------------------------
Function  generaDetalle$( operacion )
integer*1 operacion
string generaDetalle$, m$

   m$ = ""
   generaDetalle$ = ""

   if operacion = 85 then goto cierrePinpad
   if operacion = 88 then goto transaccion
   if operacion = 89 then m$ = "2"

   salir:

     generaDetalle$ = m$
     !call traceTAR( "generaDetalle$" , "operacion=["+ str$( operacion ) + "]" + \
     !                       "m$=["+ m$ + "]")

     Exit Function 

   transaccion:

   if not anulaPagoParcial and not anulaVoucher then call completaValores

   reqNoTramas = "1"
   reqOperacion = right$( string$( 2, "00" ) + reqOperacion , 2 )
   reqIdRed = right$( string$( 1, "0" ) + reqIdRed , 1 )
   reqCodDiferido = right$( string$( 2, "0" ) + reqCodDiferido , 2 )
   reqCuotas = right$( string$( 2, "0" ) + reqCuotas , 2 )
   reqMesGracia = right$( string$( 2, " " ) + reqMesGracia , 2 )
   reqTipoDiferido = right$( string$( 1, " " ) + reqTipoDiferido , 1 )
   
   reqMonto = right$( string$( 12, " " ) + reqMonto , 12 )
   reqBaseAf = right$( string$( 12, " " ) + Str$(Val(reqBaseAf)) , 12 )
   
   !reqBaseEx = right$( string$( 12, " " ) + reqBaseEx , 12 )
   
   reqBaseEx = str$(val(reqBaseEx))
   reqBaseEx = right$( string$( 12, " " ) + reqBaseEx , 12 )
   
   if GR.IVA.DEVOL% <> 0 then reqImpuesto = str$( val(reqImpuesto) -  GR.IVA.DEVOL%)
   	
   reqImpuesto = right$( string$( 12, " " ) + Str$(Val(reqImpuesto)) , 12 )
   reqImpServicio = right$( string$( 12, " " ) + reqImpServicio , 12 )
   reqPropina = right$( string$( 12, " " ) + reqPropina , 12 )
   reqMontoFijo = right$( string$( 12, " " ) + reqMontoFijo , 12 )
   reqSecuencia = right$( string$( 6, " " ) + reqSecuencia , 6 )
   reqAutorizacion = right$( string$( 6, " " ) + reqAutorizacion , 6 )
   
!   reqMid = right$( string$( 15, " " ) + reqMid , 15 )
!   reqTid = right$( string$( 8, " " ) + reqTid , 8 )

   reqMid = right$( string$( 15, " " ) + midMedianet,15)    ! Mid parametro medianet
   reqTid = right$( string$( 8, " " )  + tidMedianet, 8)    ! Tid parametro medianet
   

   !1 01 2 00 00             500         446           0          54                                                   000000819782MS001003
   !1 01 2 00 00 00          400           0         357          43                                                   000000819782MS001003

   m$ = reqNoTramas +      \ Nro de tramas          Asc 1.
        reqOperacion +     \ Tipo de operacion      Asc 2.
        reqIdRed +         \ ID de la red           Asc 1.
        reqCodDiferido +   \ Codigo del diferido    Asc 2.
        reqCuotas +        \ Plazo del diferido     Asc 2.
        reqMesGracia +     \ Meses de gracias       Asc 2.
        reqTipoDiferido +  \ Tipo diferido          Asc 1.
        reqMonto +         \ Valor de la trx        Asc 12
        reqBaseAf +        \ Monto Base con Tax     Asc 12
        reqBaseEx +        \ Monto Base Sin Tax     Asc 12
        reqImpuesto +      \ Monto del Tax          Asc 12
        reqImpServicio +   \ Impto Servicio         Asc 12.
        reqPropina +       \ Propina                Asc 12.
        reqMontoFijo +     \ Monto Fijo             Asc 12.
        reqSecuencia +     \ Numero de secuencia    Asc 06.
        reqAutorizacion +  \ Numero autorizacion    Asc 06.
        reqMid +           \ Mid Operador           Asc 15.
        reqTid             ! Tid Operador           Asc 8.

   !call traceTAR( "generaDetalle$" , "reqNoTramas=["+ reqNoTramas + "]" + \
   !                                          "reqOperacion=["+ reqOperacion + "]" + \
   !                                          "reqIdRed=["+ reqIdRed + "]" + \
   !                                          "reqCodDiferido=["+ reqCodDiferido + "]" + \
   !                                          "reqCuotas=["+ reqCuotas + "]" + \
   !                                          "reqMesGracia=["+ reqMesGracia + "]" + \
   !                                          "reqTipoDiferido=["+ reqTipoDiferido + "]" + \
   !                                          "reqMonto=["+ reqMonto + "]" + \
   !                                          "reqBaseEx=["+ reqBaseEx + "]" + \
   !                                          "reqBaseAf=["+ reqBaseAf + "]" + \
   !                                          "reqImpuesto=["+ reqImpuesto + "]" + \
   !                                          "reqImpServicio=["+ reqImpServicio + "]" + \
   !                                          "reqPropina=["+ reqPropina + "]" + \
   !                                          "reqMontoFijo=["+ reqMontoFijo + "]" + \
   !                                          "reqSecuencia=["+ reqSecuencia + "]" + \
   !                                          "reqAutorizacion=["+ reqAutorizacion + "]" + \
   !                                          "reqMid=["+ reqMid + "]"+ \
   !                                          "reqTid=["+ reqTid + "]")
   resume salir


   cierrePinpad:

      if operadorDefault = "1" then begin
         reqMid = midDatafast
         reqTid = tidDatafast
      EndIf

      if operadorDefault = "2" then begin
         reqMid = midMedianet
         reqTid = tidMedianet
      EndIf

      reqMid = right$( string$( 15, " " ) + reqMid , 15 )
      reqTid = right$( string$( 8, " " ) + reqTid , 8 )

      m$ = reqMid + reqTid + operadorDefault

   resume salir

End Function 

!------------------------------------------------------------------------------
! Genera mensaje de request para comunicaci�n con pinpad
!------------------------------------------------------------------------------
Function  generaRequest$( operacion ) public
string generaRequest$, \
       m$, l$
integer*1 operacion

   m$ = ""
   generaRequest$= ""
   reqEncabezado = ""
   reqDetalle = ""

   reqEncabezado = generaEncabezado$( operacion )
   reqDetalle = generaDetalle$( operacion )

   m$ = reqEncabezado + reqDetalle
   l$ = right$( "000" + str$( len( m$ ) ), 3)                      ! Longitud de la trama
   m$ = reqCodAplicacion + reqCodFuncion + l$ + m$                 ! Arma trama del mensaje

   !call traceTAR( "generaRequest$", "operacion=["+ str$( operacion ) + "]" + \
   !                                         "m$=["+ m$ + "]" + \
   !                                         "reqEncabezado=["+ reqEncabezado + "]" + \
   !                                         "reqDetalle=["+ reqDetalle + "]")
   generaRequest$ = m$

End Function 


Function  GrProcTef(Gmsg$,gfunc%)																						! Proceso requerimientos TEF
String    GrProcTef, GM$, Xrtrama$, Xtemp4$, Gmsg$, ghead$, Grfun$ 					!
String    Xsnd$, Xfin$ 
Integer*1 gfunc%
Integer*4 XI%, XJ%, Gk%																											!

!   Call TSHIECET
!   Call muestraOperador("PROCESANDO COMPRA ","ESPERE POR FAVOR")						!

   Grfun$ = Str$(gfunc%)																										! funcion a ejecuar
   Xsnd$ = DATE$ +":"+ Time$                                              	! Fecha y hora del requerimiento
   XRtrama$  = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", gmsg$) ! Ejecuta Requerimiento
   Xfin$ = DATE$ +":"+ Time$                                              	! Fecha y hora del requerimiento
   Call TEFEC.AUDITORIA(gmsg$,Xrtrama$, Xsnd$, Xfin$)                       ! Rastreo movimiento
   Xtemp4$ = Mid$(XRtrama$,14,2)																						! Rta procedimiento
   If Xtemp4$ <> "00" Then Begin 																						! Si falla proceso
    If Mid$(XRtrama$,3,2) = "89" Then Begin
     If Mid$(XRtrama$,8,2) = "20" Then Begin 
       Xtemp4$ = Mid$(XRtrama$,8,2)
       Xrtrama$ = Left$(XRtrama$+"                 ",17) + "PROCESO EXITOSO      ACCESO PINPAD /Borra"
       GoTo PASOTEF2
     EndIf
    EndIf
   
   	If Xtemp4$ = "  " Then Begin																						! Falla en comunicacion
   		 GM$ = "FALLA COMUNICACION   ACCESO PINPAD /Borra"
    EndIf Else Begin
    	 Gm$ = Mid$(XRtrama$,18,40)
    EndIf  
    Call muestraBorrar(Mid$(GM$,1,20),Mid$(GM$,21,20))										  ! Mensaje de Error 
    XRtrama$ = ""
   EndIf
   PASOTEF2:
   
   GrProcTef = XRtrama$																											! Retorna respuesta
   
!   XTEMP4$ = Valida.Rta(Xrtrama$)																			    ! Valida rta entregada
!   Xfin$ = DATE$ +":"+ Time$                                              	! Fecha y hora del requerimiento
!   Call TEFEC.AUDITORIA(gmsg$,Xrtrama$, Xsnd$, Xfin$)                      ! Rastreo movimiento
!   If Xtemp4$ <> "00" Then Begin 																					! Si falla proceso
!  	 Call muestraBorrar("FALLA COMUNICACION","ACCESO PINPAD    ")
!  	 XRtrama$ = ""
!   EndIf 
!   GrProcTef = XRtrama$

End Function  



!------------------------------------------------------------------------------
! Valida c�digo de respuesta
!------------------------------------------------------------------------------
Function  validaRespuesta
   string resCodigo$(1), resMensaje$, operacion
   integer*1 validaRespuesta, \
          result
   string codigoRta
!43891712000A0000000041010      80                                   310A7CB76E36B281                                      
!43891712000 
   codigoRta =""
   result = 0
   validaRespuesta = 0

   dim resCodigo$(20)
   operacion = mid$( response, 3, 2)

   resCodigo$(01) = mid$( response, 001, 02 )  ! Codigo De Aplicacion
   resCodigo$(02) = mid$( response, 003, 02 )  ! Codigo De Funcion
   resCodigo$(03) = mid$( response, 005, 03 )  ! Longitud Respuesta
   resCodigo$(04) = mid$( response, 008, 01 )  ! Rta Appl Mngr
   resCodigo$(05) = mid$( response, 008, 02 )  ! Rta De La Trx
   resCodigo$(06) = mid$( response, 012, 02 )  ! Estado Del Appl Mngr
   resCodigo$(07) = mid$( response, 014, 02 )  ! Pinpad

   if operacion = "84" then begin
      resCodigo$(08) = trim$(mid$( response, 43, 40 ))  ! Mensaje
   EndIf else begin
      resCodigo$(08) = trim$(mid$( response, 18, 40 ))   ! Mensaje
   EndIf

   if resCodigo$(05) = "20" then begin
      codigoRta = resCodigo$(06)
      If operacion = "89" Then Begin 
      	 codigoRta = "00"          ! Rta Exitosa GR 28Mzo2025
      	 resCodigo$(05) = "20"
      	 resCodigo$(06) = "00"
      	 resCodigo$(07) = "00"
      	 resCodigo$(08) = "PROCESO EXITOSO     /Borrar"
      EndIf
   EndIf else begin
      resMensaje$ = "ERROR DESCONOCIDO " +  resCodigo$(05)
      if right$( resCodigo$(05), 1 ) = "1" then resMensaje$ = "ERROR DESPUES DE    CONEXION            "
      if right$( resCodigo$(05), 1 ) = "2" then resMensaje$ = "CONEXION NO         ESTABLECIDA         "
      if right$( resCodigo$(05), 1 ) = "3" then resMensaje$ = "SIN RESPUESTA EN    SERVIDOR            "
      if right$( resCodigo$(05), 1 ) = "K" then resMensaje$ = "ABANDONO DE OPERADOR                    "
      if right$( resCodigo$(05), 1 ) = "T" then resMensaje$ = "TERMINAL NO         INICIALIZADA        "
      if right$( resCodigo$(05), 1 ) = "G" then resMensaje$ = "FUNCION NO DEFINIDA APPL                "
      if right$( resCodigo$(05), 1 ) = "O" then resMensaje$ = "MOVIMIENTO NO       ENCONTRADO          "
      if right$( resCodigo$(05), 1 ) = "C" then resMensaje$ = "TIME OUT OPERACION                      "
      codigoRta = "99"
   EndIf

   if codigoRta = "00" then begin
      resMensaje$ = resCodigo$(08)
      !! Lectura Tarjeta y Autorizaciones
      if  resCodigo$(06) = "00" and resCodigo$(07) = "00"  then  result = -1
   EndIf else begin
      if codigoRta <> "99" then resMensaje$ = resCodigo$(08)
   EndIf

   !! Confirmacion
   if operacion ="89" and resCodigo$(05) = "20" then result = -1

   validaRespuesta = result

   msg1$ = left$( resMensaje$, 20 )
   msg2$ = mid$ ( resMensaje$, 21, 20 )

   if result then begin
      if operacion  <> "89" then begin
         call TSHIECET
         call muestraOperador( msg1$ , msg2$ )
         wait; 1000
         call restauraDisplay
      EndIf
   EndIf else begin
      call muestraBorrar( msg1$, msg2$ )
   EndIf

   !call traceTAR( "validaRespuesta", "response=["+ response + "]" + \
   !                                          "operacion=["+ operacion + "]" + \
   !                                          "resCodigo$(1)=["+ resCodigo$(1) + "]" + \
   !                                          "resCodigo$(2)=["+ resCodigo$(2) + "]" + \
   !                                          "resCodigo$(3)=["+ resCodigo$(3) + "]" + \
   !                                          "resCodigo$(4)=["+ resCodigo$(4) + "]" + \
   !                                          "resCodigo$(5)=["+ resCodigo$(5) + "]" + \
   !                                          "resCodigo$(6)=["+ resCodigo$(6) + "]" + \
   !                                          "resCodigo$(7)=["+ resCodigo$(7) + "]" + \
   !                                          "resCodigo$(8)=["+ resCodigo$(8) + "]" + \
   !                                          "result=["+ str$(result) + "]" )

End Function 

!------------------------------------------------------------------------------
! Obtiene mensaje a mostrar en visor
!------------------------------------------------------------------------------
Sub obtieneMsgOperacion( k%, msg1$, msg2$ ) public
   integer*2 k%
   string    msg1$, msg2$

   ! 1 "NRO RECIBO A ANULAR NO EXISTE EN LA TRX "
   ! 2 "PAGO SUPERIOR AL    VALOR DE LA COMPRA  "
   ! 3 "COMPRA NO PUDO SER  PROCESADA CON ExitO "
   ! 4 "TARJETA 1.CORRIENTE 2.CREDITO DIFERIDO  "
   ! 5 "NRO RECIBO A ANULAR         Borrar=Salir"
   ! 6 "NRO RECIBO DUPLICAR         Borrar=Salir"
   ! 7 "ERROR OPEN DUPLICADOVOUCHER Borrar=Salir"
   ! 8 "VOUCHER DUPLICAR NO EXISTE  Borrar=Salir"
   ! 9 "DUPLICADO VOUCHER   TERMINA Borrar=Salir"
   !10 "VOUCHER A ANULAR NO EXISTE  Borrar=Salir"
   !11 "VLR CERO EN PAGO NO VALIDO  Borrar=Salir"
   !12 "ERROR AUTORIZACION          Borrar=Salir"
   !13 "VOUCHER YA FUE      ANULADO Borrar=Salir"
   !14 "ERROR AUTORIZACION          Borrar=Salir"

   msg1$ = left$ ( msgOperacion$(k%) , 20 )
   msg2$ = right$( msgOperacion$(k%) , 20 )

End Sub

!------------------------------------------------------------------------------
! Obtiene c�digos de operador corriente y diferido
!------------------------------------------------------------------------------
Sub obtieneCodigosOperador
   integer*1 i%, \
          binActivo%

   resOpeCorriente =""
   resOpeDiferido = ""
   binActivo% = 0

   if autorizadorUnico = "2" then begin
      resOpeCorriente = operadorDefault
      resOpeDiferido  = operadorDefault
      goto salir
   EndIf

   for i% = 1 to cantParBines%
       if val( resBin ) >= val( parBines$( i%,0 ) ) and \
          val( resBin ) <= val( parBines$( i%,1 ) ) then begin

          resOpeCorriente = parBines$( i%, 2 )
          resOpeDiferido  = parBines$( i%, 3 )
          binActivo% = -1
          i% = cantParBines%

       EndIf
   next i%

   if not binActivo% then begin
      resOpeCorriente = "2" !Operador default Medianet
      resOpeDiferido  = "2"
      call muestraBorrar("BIN "+ resBin +"NO PARAM. ", "PROCESA MEDIANET")
   EndIf


   salir:
   !call traceTAR( "obtieneCodigosOperador", "resBin=["+ resBin + "]" + \
   !                       "autorizadorUnico=["+ autorizadorUnico + "]" + \
   !                       "operadorDefault=["+ operadorDefault + "]" + \
   !                       "binActivo%=["+ str$( binActivo% ) + "]"  + \
   !                       "resOpeCorriente=["+ resOpeCorriente + "]"  + \
   !                       "resOpeDiferido=["+ resOpeDiferido + "]" )
   Exit Sub

End Sub

!------------------------------------------------------------------------------
! Lee respuesta de operacion lectura de tarjeta
!------------------------------------------------------------------------------
Sub leeRespuestaLecturaTarjeta

   resNroTarjeta = trim$( mid$( response, 18, 25 ) )
   resBin = left$( resNroTarjeta, 6 )

   call obtieneCodigosOperador

   !call traceTAR( "leeRespuestaLecturaTarjeta",  "resOpeCorriente=["+ resOpeCorriente + "]" + \
   !                       "resOpeDiferido=["+ resOpeDiferido + "]" + \
   !                       "resNroTarjeta=["+ resNroTarjeta + "]" + \
   !                       "resBin=["+ resBin + "]")

End Sub

!------------------------------------------------------------------------------
! Lee respuesta de operacion compra
!------------------------------------------------------------------------------
Function  leeRespuestaTransaccion
   integer*1 leeRespuestaTransaccion

   leeRespuestaTransaccion = 0

   resCodigo       = trim$( mid$( response , 12,  2 ) )
   resCodPinpad    = trim$( mid$( response , 14,  2 ) )
   resIdRed        = trim$( mid$( response , 16,  2 ) )
   resMsgPinpad    = trim$( mid$( response , 18, 40 ) )
   resSecuencia    = trim$( mid$( response , 58,  6 ) )
   resLote         = trim$( mid$( response , 64,  6 ) )
   resHora         = trim$( mid$( response , 70,  6 ) )
   resFecha        = trim$( mid$( response , 76,  8 ) )
   resAutorizacion = trim$( mid$( response , 84,  6 ) )
   resTid          = trim$( mid$( response , 90,  8 ) )
   resMid          = trim$( mid$( response , 98, 15 ) )
   resInteres      = str$ (val ( mid$( response ,113, 12 ) ) )
   resCodBanco     = trim$( mid$( response ,125,  3 ) )
   resNomBanco     = trim$( mid$( response ,128, 30 ) )
   resNomFranquicia= trim$( mid$( response ,158, 25 ) )
   resModLectura   = trim$( mid$( response ,183,  2 ) )
   resNomCliente   = trim$( mid$( response ,185, 40 ) )
   resIdApp        = trim$( mid$( response ,225, 20 ) )
   resMonto        = reqMonto
   resImpuesto     = reqImpuesto
   resBaseEx       = reqBaseEx
   resBaseAf       = reqBaseAf
   resCodDiferido  = reqCodDiferido
   resCuotas       = reqCuotas
   resNomDiferido  = reqNomDiferido
   resMesGracia    = reqMesGracia
   resTipoDiferido = reqTipoDiferido
   resAnulado      = "0"

   call QXL.TSUPEC21("RESPUESTA TRX: " + resSecuencia)
   call QXL.TSUPEC21("MID: " + resMid + " TID:" + resTid)

   if anulaPagoParcial or anulaVoucher then resAnulado = "1"

   !call traceTAR( "leeRespuestaTransaccion", "resCodigo=["+ resCodigo + "]" + \
   !                                                  "resCodPinpad=["+ resCodPinpad + "]" + \
   !                                                  "resIdRed=["+ resIdRed + "]"+ \
   !                                                  "resMsgPinpad=["+ resMsgPinpad + "]"+ \
   !                                                  "resSecuencia=["+ resSecuencia + "]"+ \
   !                                                  "resLote=["+ resLote + "]"+ \
   !                                                  "resHora=["+ resHora + "]"+ \
   !                                                  "resFecha=["+ resFecha + "]"+ \
   !                                                  "resAutorizacion=["+ resAutorizacion + "]"+ \
   !                                                  "resTid=["+ resTid + "]"+ \
   !                                                  "resMid=["+ resMid + "]"+ \
   !                                                  "resInteres=["+ resInteres + "]"+ \
   !                                                  "resCodBanco=["+ resCodBanco + "]"+ \
   !                                                  "resNomBanco=["+ resNomBanco + "]"+ \
   !                                                  "resNomFranquicia=["+ resNomFranquicia + "]"+ \
   !                                                  "resModLectura=["+ resModLectura + "]"+ \
   !                                                  "resNomCliente=["+ resNomCliente + "]"+ \
   !                                                  "resIdApp=["+ resIdApp + "]" + \
   !                                                  "resMonto=["+ resMonto + "]" + \
   !                                                  "resBaseEx=["+ resBaseEx + "]" + \
   !                                                  "resBaseAf=["+ resBaseAf + "]" + \
   !                                                  "resImpuesto=["+ resImpuesto + "]" + \
   !                                                  "resCodDiferido=["+ resCodDiferido + "]" + \
   !                                                  "resMesGracia=["+ resMesGracia + "]" + \
   !                                                  "resCuotas=["+ resCuotas + "]" + \
   !                                                  "resNomDiferido=["+ resNomDiferido + "]" + \
   !                                                  "resTipoDiferido=["+ resTipoDiferido + "]"  + \
   !                                                  "resAnulado=["+ resAnulado + "]")

   ! Error autorizacion
   if resAutorizacion = "000000" then begin
      call obtieneMsgOperacion( 13, msg1$, msg2$ )
      call muestraBorrar( msg1$ , msg2$ )
      Exit Function 
   EndIf

   leeRespuestaTransaccion = -1

End Function 

!------------------------------------------------------------------------------
! Lee respuesta EMV
!------------------------------------------------------------------------------
Sub leeRespuestaEMV

   resAID          = trim$( mid$(response,  12, 20 ) )
   resEMV          = trim$( mid$(response,  32, 22 ) )
   resARQC         = trim$( mid$(response,  69, 16 ) )
   resMsgPromocion = trim$( mid$(response,  85, 80 ) )
   resValPin       = trim$( mid$(response,  54, 15 ) )
   resTVR          = mid$(response, 165, 10 )
   resTSI          = mid$(response, 175,  4 )

   !call traceTAR( "leeRespuestaEMV",  "resAID=["+ resAID + "]" + \
   !                                           "resEMV=["+ resEMV + "]" + \
   !                                           "resARQC=["+ resARQC + "]"+ \
   !                                           "resMsgPromocion=["+ resMsgPromocion + "]"+ \
   !                                           "resValPin=["+ resValPin + "]"+ \
   !                                           "resTVR=["+ resTVR + "]"+ \
   !                                           "resTSI=["+ resTSI + "]")
End Sub

!------------------------------------------------------------------------------
! Inicializa variables de respuesta
!------------------------------------------------------------------------------
Sub inicializaVariablesResponse

   resCodigo = ""
   resCodPinpad = ""
   resIdRed = ""
   resMsgPinpad = ""
   resSecuencia = ""
   resLote = ""
   resHora = ""
   resFecha = ""
   resAutorizacion = ""
   resTid = ""
   resMid = ""
   resInteres = ""
   resCodBanco = ""
   resNomBanco = ""
   resNomFranquicia  = ""
   resModLectura = ""
   resNomCliente = ""
   resIdApp = ""
   resMonto = ""
   resBaseEx = ""
   resBaseAf = ""
   resImpuesto = ""
   resCodDiferido = ""
   resMesGracia = ""
   resNomDiferido = ""
   resTipoDiferido  = ""

   resAID = ""
   resEMV = ""
   resARQC = ""
   resMsgPromocion = ""
   resValPin = ""
   resTVR = ""
   resTSI = ""

   resAnulado = ""
End Sub

!------------------------------------------------------------------------------
! Lee respuesta del pinpad
!------------------------------------------------------------------------------
Function  leeRespuesta( operacion )
   integer*1 operacion, \
          leeRespuesta

   leeRespuesta = 0

   if response = "" then Exit Function 

   if not validaRespuesta then begin
      call cancelaPago
      Exit Function 
   EndIf

   if operacion = 84 then call leeRespuestaLecturaTarjeta

   if operacion = 88 then begin
      if not leeRespuestaTransaccion then Exit Function 
   EndIf

   if operacion = 89 then call leeRespuestaEMV

   leeRespuesta = -1

End Function 

!------------------------------------------------------------------------------
! Envia mensajer�a para comunicaci�n con aplicaci�n JAVA
! operacion 84: Lectura de tarjeta
!           88: Compra
!------------------------------------------------------------------------------
!Function  enviaMensajeria( operacion )
!   integer*1 operacion, \
!          enviaMensajeria
!   enviaMensajeria = 0
!   request = ""
!   response = ""
!   request = generaRequest$( operacion )
!
!   !response = invocaMetodo$("com.appl.applkernel","threader", request)
!
!   if not abreComunicacionPI( pipeInSession%, pipeInName , -1, bufferSize% ) then begin
!      call muestraBorrar("Error comunicacion",  "PIPE: "+ pipeInName)
!      Exit Function 
!   EndIf
!
!   if not enviaMensajePI( pipeInSession%, request ) then begin
!      call muestraBorrar("Error escritura",  "PIPE: "+ pipeInName)
!      Exit Function 
!   EndIf
!
!   if autoRespuesta then begin
!      response = generaAutorespuesta$( -1 , operacion, 0 )
!   EndIf else begin
!      response = recibeComunicacionPI( pipeOutSession%, pipeOutName,  bufferSize%, pipeTimeout% )
!      !Con este tiempo se asegura que el pipe fue cerrado por la apliacion JAVA
!      !y puede ser abierto para un nuevo mensaje
!      call muestraOperador("PROCESANDO RESPUESTA","ESPERE...")
!      wait; pipeSleep%
!      call restauraDisplay
!      if response = "" then call muestraBorrar("Sin respuesta",  "PIPE: "+ pipeInName)
!   EndIf
!   !if JAVA.INIT = -1 then begin
!   !   jGuiSubState = 80539
!   !   call javaEvent(terminalSubStateMsg)
!   !EndIf
!   call grabaAuditoria
!
!   if leeRespuesta( operacion ) then enviaMensajeria = -1
!
!End Function 

!------------------------------------------------------------------------------
! Envia mensajer�a para comunicaci�n con aplicaci�n JAVA
! operacion 84: Lectura de tarjeta
!           88: Compra
! Funcion modificada por Grupo Retail para cambio integracion por TCP/IP
! y no por pipes, desarrollado noviembre 15 2023
!------------------------------------------------------------------------------
Function  enviaMensajeria( operacion )																			! Proceso operacion TEF
   Integer*1 operacion, enviaMensajeria																			! definicion de variables
   enviaMensajeria = 0																											! init Function 
   request = ""																															!
   response = ""																														!
   request = generaRequest$( operacion )																		! genera trama requerimiento
   response = GrProcTef(request,operacion)																	! Ejecuta requerimiento a pinpad
   If autoRespuesta Then Begin																							! Si simulaci�n 
      response = generaAutorespuesta$( -1 , operacion, 0 )									! retorna rta simulacion 
   EndIf Else Begin																													! revisa respuesta proceso TEF
      If response = "" Then                                                \! falla en respuesta tef
      	 Call muestraBorrar("Sin respuesta",  "Autorizador TEF")						! Msg Alerta
   EndIf 																																		!
   Call grabaAuditoria																											! graba auditoria
   If leeRespuesta( operacion ) Then enviaMensajeria = -1										! valida respuesta
End Function 

!------------------------------------------------------------------------------
! Retorna informaci�n de pagos
!------------------------------------------------------------------------------
Sub obtienePago( i% )
   integer*1 i%

   resMid = resPagos$( i%, 0)
   resTid = resPagos$( i%, 1)
   resLote = resPagos$( i%, 2)
   resModLectura = resPagos$( i%, 3)
   resNroTarjeta = resPagos$( i%, 4)
   resNomFranquicia = resPagos$( i%, 5)
   resSecuencia = resPagos$( i%, 6)
   resAutorizacion = resPagos$( i%, 7)
   resCuotas = resPagos$( i%, 8)
   resInteres = resPagos$( i%, 9)
   resNomDiferido = resPagos$( i%, 10)
   resFecha = resPagos$( i%, 11)
   resHora = resPagos$( i%, 12)
   resIdRed = resPagos$( i%, 13)
   resBaseEx = resPagos$( i%, 14)
   resBaseAf = resPagos$( i%, 15)
   resImpuesto = resPagos$( i%, 16)
   resMonto = resPagos$( i%, 17)
   resNomCliente = resPagos$( i%, 18)
   resTVR = resPagos$( i%, 19)
   resTSI = resPagos$( i%, 20)
   resCodDiferido = resPagos$( i%, 21)
   resMesGracia = resPagos$( i%, 22)
   reqTipoDiferido = resPagos$( i%, 23)
   resAnulado = resPagos$( i%, 24)
   descPago = val( resPagos$( i%, 25) )

   !call traceTAR("obtienePago", "i%=["+ str$(i%) + "]" + \
   !   "resMid=["+ resMid + "]" + \
   !   "resTid=["+ resTid + "]" + \
   !   "resLote=["+ resLote + "]" + \
   !   "resModLectura=["+ resModLectura + "]" + \
   !   "resNroTarjeta=["+ resNroTarjeta + "]" + \
   !   "resNomFranquicia=["+ resNomFranquicia + "]" + \
   !   "resSecuencia=["+ resSecuencia + "]" + \
   !   "resAutorizacion=["+ resAutorizacion + "]" + \
   !   "resCuotas=["+ resCuotas + "]" + \
   !   "resInteres=["+ resInteres + "]" + \
   !   "resNomDiferido=["+ resNomDiferido + "]" + \
   !   "resFecha=["+ resFecha + "]" + \
   !   "resHora=["+ resHora + "]" + \
   !   "resIdRed=["+ resIdRed + "]" + \
   !   "resBaseEx=["+ resBaseEx + "]" + \
   !   "resBaseAf=["+ resBaseAf + "]" + \
   !   "resImpuesto=["+ resImpuesto + "]" + \
   !   "resMonto=["+ resMonto + "]" + \
   !   "descPago=["+ str$( descPago ) + "]" + \
   !   "resNomCliente=["+ resNomCliente + "]" + \
   !   "resTVR=["+ resTVR + "]" + \
   !   "resTSI=["+ resTSI + "]" + \
   !   "resAnulado=["+ resAnulado + "]")

End Sub

!------------------------------------------------------------------------------
! Asigna valores necesarios para el mensaje request
!------------------------------------------------------------------------------
Sub asignaVariablesRequest
   reqOperacion = "03"
   reqIdRed = resIdRed
   reqCodDiferido = resCodDiferido
   reqCuotas = resCuotas
   reqMesGracia = resMesGracia
   reqTipoDiferido = resTipoDiferido
   reqMonto = resMonto
   reqBaseAf = resBaseAf
   reqBaseEx = resBaseEx
   reqImpuesto = resImpuesto
   !reqImpServicio = resImpServicio
   !reqPropina = resPropina
   !reqMontoFijo = resMontoFijo
   reqSecuencia = resSecuencia
   reqAutorizacion = resAutorizacion
   reqMid = resMid
   reqTid = resTid
End Sub

!------------------------------------------------------------------------------
! Busca un pago realizado mediante la secuencia
!------------------------------------------------------------------------------
Function  buscaPagoParcial( secuencia )
   integer*1 i%, buscaPagoParcial
   string secuencia

   buscaPagoParcial = 0
   for i%= 1 to cantPagos%
      if resPagos$( i%, 6 ) = secuencia and \
         resPagos$( i%, 24 ) = "0" then begin
         call obtienePago( i% )
         call asignaVariablesRequest
         buscaPagoParcial = i%
      EndIf
   next i%
End Function 

!------------------------------------------------------------------------------
! Captura nro de recibo para anulacion
!------------------------------------------------------------------------------
Function  capturaNroRecibo
   integer*1 capturaNroRecibo
   string d$

   reqSecuencia =""
   d$ =""
   capturaNroRecibo = 0

   if not reimpresionVoucher then begin
      call obtieneMsgOperacion( 5, msg1$, msg2$)
   EndIf else begin
      call obtieneMsgOperacion( 6, msg1$, msg2$ )
   EndIf

   jGuiSubState = 80504
   call javaEvent(terminalSubStateMsg)	  
   d$ = ingresoDatos$( msg1$, msg2$ )

   if java.init then begin
      jGuiSubState = 0
   EndIf

   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if d$ <> "" then begin
         reqSecuencia = d$
         reqSecuencia = right$( "000000" + reqSecuencia , 6 )
         capturaNroRecibo = -1
         if java.init then begin
            jGuiSubState = 0
         EndIf
      EndIf
   EndIf

!   call traceTAR("capturaNroRecibo", "d$=["+ d$ + "]" + \
!                         "reqSecuencia=["+ reqSecuencia + "]")

End Function 

!------------------------------------------------------------------------------
! Interpreta registro de archivo de vouchers
!------------------------------------------------------------------------------
Sub leeRegistroVoucher( registro )
   string registro

   resSecuencia = buscaCampo( registro, separador,  1 )
   resAnulado = buscaCampo( registro, separador,  2 )
   resMid = buscaCampo( registro, separador,  3 )
   resTid = buscaCampo( registro, separador,  4 )
   resLote = buscaCampo( registro, separador,  5 )
   resModLectura = buscaCampo( registro, separador,  6 )
   resNroTarjeta = buscaCampo( registro, separador,  7 )
   resNomFranquicia = buscaCampo( registro, separador,  8 )
   resAutorizacion = buscaCampo( registro, separador,  9 )
   resCuotas = buscaCampo( registro, separador,  10 )
   resInteres = buscaCampo( registro, separador, 11 )
   resNomDiferido = buscaCampo( registro, separador, 12 )
   resFecha = buscaCampo( registro, separador, 13 )
   resHora = buscaCampo( registro, separador, 14 )
   resIdRed = buscaCampo( registro, separador, 15 )
   resBaseEx = buscaCampo( registro, separador, 16 )
   resBaseAf = buscaCampo( registro, separador, 17 )
   resImpuesto = buscaCampo( registro, separador, 18 )
   resMonto = buscaCampo( registro, separador, 19 )
   resNomCliente = buscaCampo( registro, separador, 20 )
   resTVR = buscaCampo( registro, separador, 21 )
   resTSI = buscaCampo( registro, separador, 22 )
   resCodDiferido = buscaCampo( registro, separador, 23 )
   resMesGracia = buscaCampo( registro, separador, 24 )

End Sub

!------------------------------------------------------------------------------
! Busca voucher en archivo de vouchers para anulacion de pagos
!------------------------------------------------------------------------------
Function  buscaVoucher( secuencia )
   integer*4 buscaVoucher, k%, i%
   string secuencia

   buscaVoucher = 0
   regPagoAnulado = ""
   close 93
   TS.ER.RETURN = -1
   open archVoucher as 93 nowrite nodel
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "ERROR APERTURA", archVoucher )
      Exit Function 
   EndIf

   if End #93 then salir

   k% = 1
   i% = 0
   while (k%)
      read #93; regPagoAnulado
      i% = i% + 1
      if left$( regPagoAnulado, 6 ) = secuencia then begin
         buscaVoucher = i%
         call leeRegistroVoucher( regPagoAnulado )
         if anulaVoucher then call asignaVariablesRequest
         k% = 0
      EndIf
   wend

   salir:
   Exit Function 
   close 93

End Function 

!------------------------------------------------------------------------------
!Actualiza registro de archivo de vouchers
!------------------------------------------------------------------------------
Sub actualizaVoucher
   integer*2 l%
   string nroRecibo, \
          anulado, \
          resto, \
          sep, \
          formato

   nroRecibo= ""
   anulado= ""
   resto= ""
   sep= ""
   formato= ""

   close 47
   l% = len( regPagoAnulado + chr$( 13 ) + chr$( 10 ) )
   TS.ER.RETURN = -1
   open archVoucher direct recl l% as 47

   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "ERROR APERTURA", archVoucher )
      Exit Sub
   EndIf

   if End #47 then salir

   formato = "C6 C1 C1 C1 C" + str$(l% - 9)

   !call traceTAR("actualizaVoucher", "regPagoAnulado=["+ regPagoAnulado + "]" + \
   !                                           "l%=["+ str$(l%) + "]" + \
   !                                           "formato=["+ formato + "]" + \
   !                                           "indPagoAnulado=["+ str$( indPagoAnulado ) + "]"  + \
   !                                           "resAnulado=["+ resAnulado + "]"   + \
   !                                           "reqSecuencia=["+ reqSecuencia + "]" )

   TS.ER.RETURN = -1

   read form formato; #47, indPagoAnulado; nroRecibo, sep, anulado, sep, resto

   !call traceTAR("leyendo registro",  "nroRecibo=["+ nroRecibo + "]"  + \
   !                                            "separador=["+ separador + "]"  + \
   !                                            "anulado=["+ anulado + "]"  + \
   !                                            "separador=["+ separador + "]"  + \
   !                                            "resto=["+ resto + "]" )
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "ERROR LECTURA", archVoucher )
      goto salir
   EndIf

   if reqSecuencia = nroRecibo and anulado = "0" then begin
      TS.ER.RETURN = -1
      write form formato; #47, indPagoAnulado; nroRecibo, sep, resAnulado, sep, resto
      !call traceTAR("escribiendo registro",  "nroRecibo=["+ nroRecibo + "]"  + \
      !                                                "separador=["+ separador + "]"  + \
      !                                                "resAnulado=["+ resAnulado + "]"  + \
      !                                                "separador=["+ separador + "]"  + \
      !                                                "resto=["+ resto + "]" )

      if TS.ER.RETURN <> -1 then begin
         call muestraBorrar( "ERROR ESCRITURA", archVoucher )
         goto salir
      EndIf

   EndIf

   salir:
   close 47

End Sub

!------------------------------------------------------------------------------
! Valida n�mero de recibo para anular
!------------------------------------------------------------------------------
Function  validaNroRecibo
   integer*1 validaNroRecibo


   indPagoAnulado = 0
   validaNroRecibo = 0

   if anulaPagoParcial then begin
      indPagoAnulado = buscaPagoParcial( reqSecuencia )
   EndIf else begin
      indPagoAnulado = buscaVoucher( reqSecuencia )
      if indPagoAnulado = 0 then begin
         call QXL.TSUPEC21("BUSCA VOUCHER sec:" + reqSecuencia + " NO EXISTE")
      EndIf else begin
         call QXL.TSUPEC21("BUSCA VOUCHER sec:" + reqSecuencia + " ENCONTRADO")
         call QXL.TSUPEC21("MID: " + resMid + " TID:" + resTid)
      EndIf
   EndIf

   if indPagoAnulado = 0 then begin
      if anulaPagoParcial   then call obtieneMsgOperacion(  1, msg1$, msg2$)
      if reimpresionVoucher then call obtieneMsgOperacion(  8, msg1$, msg2$ )
      if anulaVoucher       then call obtieneMsgOperacion( 10, msg1$, msg2$ )
      call muestraBorrar( msg1$ , msg2$ )
      Exit Function 
   EndIf

   if resAnulado = "1" and anulaVoucher then begin
      call obtieneMsgOperacion( 12, msg1$, msg2$ )
      call muestraBorrar( msg1$ , msg2$ )
      Exit Function 
   EndIf

   if indPagoAnulado > 0 then validaNroRecibo = -1

End Function 

!------------------------------------------------------------------------------
! Reglas de validacion
!------------------------------------------------------------------------------
Function  validaReglas( proceso )
   integer*1 validaReglas,  proceso

   validaReglas = -1

   if proceso < 3 then begin
      if not activo then begin
         call muestraBorrar("TARJETAS BANCARIAS", "INACTIVO    <Borrar>")
         validaReglas = 0
         call ignoraTeclas
         goto salir
      EndIf
   EndIf else begin
      if not pagoManualActivo then begin
         call muestraBorrar("VOUCHER MANUAL", "INACTIVO    <Borrar>")
         validaReglas = 0
         goto salir
      EndIf
   EndIf

   if proceso = 1 then goto procedimientos
   if proceso = 2 then goto teclaPago

   salir:
   !call traceTAR("validaReglas","proceso=["+ str$( proceso ) + "]" + \
   !                      "TS.TRX.STATUS=["+ str$(TS.TRX.STATUS) + "]"  + \
   !                      "TS.PROCEDURE=["+ str$(TS.PROCEDURE) + "]" + \
   !                      "TS.BAL.TAKEN=["+ str$(TS.BAL.TAKEN) + "]" + \
   !                      "TS.IO.KEYS(5)=["+ str$(TS.IO.KEYS(5)) + "]" + \
   !                      "TS.IO.STATE=["+ str$(TS.IO.STATE) + "]")

   Exit Function 

   procedimientos:

   if TS.INTRX Then Begin
      call muestraBorrar("PROCESO NO VALIDO", "EN UNA TRX. BORRAR")
      validaReglas = 0
      goto salir
   EndIf
   resume salir

   teclaPago:

   ! Para evitar Error SU por nro de operaciones
   if cantPagos% >= cantMaxPagos% Then Begin
      validaReglas = 0
      call muestraBorrar("NO SE PERMITEN MAS",  "OPERACIONES TEF")
      resume salir
   EndIf

   ! No se ha ingresado total en venta
   if ((TS.TRX.STATUS <> 1 and TS.PROCEDURE = -1) OR NOT TS.BAL.TAKEN) THEN BEGIN
      TS.IO.MOTORKEY = 0
      validaReglas = 0
      TS.GUIDANCE = 1020 ! Solicita total a la trx
      resume salir
   EndIf

   ! Si secuencias fuera venta
   if (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin
      TS.IO.MOTORKEY = 0
      validaReglas = 0
      TS.GUIDANCE = 1075 ! Revise Procedimiento
      resume salir
   EndIf
   resume salir

End Function 

!------------------------------------------------------------------------------
! Procedimiento de cierre de d�a de pinpad
!------------------------------------------------------------------------------
Sub cierrePinpad
   string d$

   if not validaReglas( 1 ) then goto salir

   confirmaCierre:
   call TSHIECET

   jGuiSubState = 80585
   call javaEvent(terminalSubStateMsg)
   d$ = ingresoDatos$( "CIERRE DIARIO PINPAD" , "1.Confirma Borr=Sale" )
   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then goto salir
      if val(d$) <>  1 then goto confirmaCierre

      call TSHIECET
      call muestraOperador("FINALIZANDO DIA ","PINPAD,   ESPERE...")
      wait; 1000
      call restauraDisplay

      if not enviaMensajeria( 85 ) then goto salir

   EndIf

   salir:
   call ignoraTeclas
   Exit Sub

End Sub

!------------------------------------------------------------------------------
! Procedimiento de actualizaci�n de pinpad
!------------------------------------------------------------------------------
Sub actualizaIpPinpad

   if not validaReglas( 1 ) then goto salir

   call TSHIECET
   call muestraOperador("ACTUALIZACION DE","PINPAD, ESPERE...")
   wait; 1000
   call restauraDisplay

   if not enviaMensajeria( 87 ) then goto salir

   salir:
   call ignoraTeclas
   Exit Sub

End Sub

!------------------------------------------------------------------------------
! Procedimiento de actualizaci�n de software de pinpad
!------------------------------------------------------------------------------
Sub actualizaSwPinpad

   if not validaReglas( 1 ) then goto salir

   call TSHIECET
   call muestraOperador("ACTUALIZACION DE SW","PINPAD, ESPERE...")                                                                                    !
   wait; 1000
   call restauraDisplay

   if not enviaMensajeria( 86 ) then goto salir

   salir:
   call ignoraTeclas
   Exit Sub
End Sub

!------------------------------------------------------------------------------
! Captura tarjeta por medio de lectura en pinpad
!------------------------------------------------------------------------------
Function  capturaTarjeta public
   integer*1 capturaTarjeta

   capturaTarjeta = 0
   resNroTarjeta = ""
   resBin = ""

   call muestraOperador("PASE LA TARJETA POR","EL PINPAD, ESPERE...")
   !wait; 1000
   !call restauraDisplay

   if not enviaMensajeria( 84 ) then Exit Function 

   capturaTarjeta = -1

End Function 

!------------------------------------------------------------------------------
! Acumula pagos realizados
!------------------------------------------------------------------------------
Sub acumulaPagos

   if resAnulado = "0" then begin
      cantPagos% = cantPagos% + 1

!call traceNUO("acumulaPagos cantPagos%=[" + str$(cantPagos%) + "]")

      resPagos$( cantPagos%, 0) = resMid
      resPagos$( cantPagos%, 1) = resTid
      resPagos$( cantPagos%, 2) = resLote
      resPagos$( cantPagos%, 3) = resModLectura
      resPagos$( cantPagos%, 4) = resNroTarjeta
      resPagos$( cantPagos%, 5) = resNomFranquicia
      resPagos$( cantPagos%, 6) = resSecuencia
      resPagos$( cantPagos%, 7) = resAutorizacion
      resPagos$( cantPagos%, 8) = resCuotas
      resPagos$( cantPagos%, 9) = resInteres
      resPagos$( cantPagos%, 10) = resNomDiferido
      resPagos$( cantPagos%, 11) = resFecha
      resPagos$( cantPagos%, 12) = resHora
      resPagos$( cantPagos%, 13) = resIdRed
      resPagos$( cantPagos%, 14) = resBaseEx
      resPagos$( cantPagos%, 15) = resBaseAf
      resPagos$( cantPagos%, 16) = resImpuesto
      resPagos$( cantPagos%, 17) = resMonto
      resPagos$( cantPagos%, 18) = resNomCliente
      resPagos$( cantPagos%, 19) = resTVR
      resPagos$( cantPagos%, 20) = resTSI
      resPagos$( cantPagos%, 21) = resCodDiferido
      resPagos$( cantPagos%, 22) = resMesGracia
      resPagos$( cantPagos%, 23) = resTipoDiferido
      resPagos$( cantPagos%, 24) = resAnulado
      resPagos$( cantPagos%, 25) = str$( descPago )

   EndIf else begin
      if indPagoAnulado > 0 then resPagos$( indPagoAnulado, 24) = resAnulado
   EndIf


   !call traceTAR("acumulaPagos", "cantPagos%=["+ str$( cantPagos% ) + "]" + \
   !                      "anulaPagoParcial=["+ str$( anulaPagoParcial ) + "]" + \
   !                      "resMid=["+ resMid + "]" + \
   !                      "resTid=["+ resTid + "]" + \
   !                      "resLote=["+ resLote + "]" + \
   !                      "resModLectura=["+ resModLectura + "]" + \
   !                      "resNroTarjeta=["+ resNroTarjeta + "]" + \
   !                      "resNomFranquicia=["+ resNomFranquicia + "]" + \
   !                      "resSecuencia=["+ resSecuencia + "]" + \
   !                      "resAutorizacion=["+ resAutorizacion + "]" + \
   !                      "resCuotas=["+ resCuotas + "]" + \
   !                      "resInteres=["+ resInteres + "]" + \
   !                      "resNomDiferido=["+ resNomDiferido + "]" + \
   !                      "resFecha=["+ resFecha + "]" + \
   !                      "resHora=["+ resHora + "]" + \
   !                      "resIdRed=["+ resIdRed + "]" + \
   !                      "resBaseEx=["+ resBaseEx + "]" + \
   !                      "resBaseAf=["+ resBaseAf + "]" + \
   !                      "resImpuesto=["+ resImpuesto + "]" + \
   !                      "resMonto=["+ resMonto + "]" + \
   !                      "descPago=["+ str$( descPago ) + "]" + \
   !                      "resNomCliente=["+ resNomCliente + "]" + \
   !                      "resTVR=["+ resTVR + "]" + \
   !                      "resTSI=["+ resTSI + "]" + \
   !                      "resAnulado=["+ resAnulado + "]")
   descPago = 0
End Sub

!------------------------------------------------------------------------------
! Graba cadena de usuario 99 54
!------------------------------------------------------------------------------
Sub grabaCadenaUsuario
   string dato$
   integer* 1 pagoManual

   pagoManual = 0
   if pagoManualDebito or pagoManualCredito then pagoManual = 1

   dato$ = pack$("99")  + ":" + \
           pack$("54")  + ":" + \
           resMid + ":" + \
           resTid + ":" + \
           pack$( resLote ) + ":" + \
           pack$( resModLectura ) + ":" + \
           resNroTarjeta + ":" + \
           resNomFranquicia + ":" + \
           pack$( resSecuencia ) + ":" + \
           pack$( resAutorizacion ) + ":" + \
           pack$( str$( val( resCuotas ) ) ) + ":" + \
           pack$( str$( val( resInteres ) ) ) + ":" + \
           resNomDiferido + ":" + \
           pack$( resFecha ) + ":" + \
           pack$( resHora ) + ":" + \
           pack$( resIdRed ) + ":" + \
           pack$( str$( val( resBaseEx ) ) ) + ":" + \
           pack$( str$( val( resBaseAf ) ) ) + ":" + \
           pack$( str$( val( resImpuesto ) ) ) + ":" + \
           pack$( str$( val( resMonto) ) ) + ":" + \
           resNomCliente + ":" + \
           resTVR + ":" + \
           resTSI + ":" + \
           pack$( resCodDiferido ) + ":" + \
           pack$( resMesGracia ) + ":" + \
           pack$( reqTipoDiferido ) + ":" + \
           pack$( resAnulado ) + ":" + \
           pack$( str$( pagoManual ) ) + ":" + \
           pack$( str$( descPago ) )

   if anulaVoucher then begin
      call grabaTrx99( 1, dato$ )
   EndIf else begin
      call agregaCadenaTSL( dato$ )
   EndIf


   !call traceTAR("grabaCadenaUsuario", "resMid=["+ resMid + "]" + \
   !                      "resTid=["+ resTid + "]" + \
   !                      "resLote=["+ resLote + "]" + \
   !                      "resModLectura=["+ resModLectura + "]" + \
   !                      "resNroTarjeta=["+ resNroTarjeta + "]" + \
   !                      "resNomFranquicia=["+ resNomFranquicia + "]" + \
   !                      "resSecuencia=["+ resSecuencia + "]" + \
   !                      "resAutorizacion=["+ resAutorizacion + "]" + \
   !                      "resCuotas=["+ resCuotas + "]" + \
   !                      "resInteres=["+ resInteres + "]" + \
   !                      "resNomDiferido=["+ resNomDiferido + "]" + \
   !                      "resFecha=["+ resFecha + "]" + \
   !                      "resHora=["+ resHora + "]" + \
   !                      "resIdRed=["+ resIdRed + "]" + \
   !                      "resBaseEx=["+ resBaseEx + "]" + \
   !                      "resBaseAf=["+ resBaseAf + "]" + \
   !                      "resImpuesto=["+ resImpuesto + "]" + \
   !                      "resMonto=["+ resMonto + "]" + \
   !                      "descPago=["+ str$( descPago ) + "]" + \
   !                      "resNomCliente=["+ resNomCliente + "]" + \
   !                      "resTVR=["+ resTVR + "]" + \
   !                      "resTSI=["+ resTSI + "]" + \
   !                      "resAnulado=["+ resAnulado + "]")

End Sub

!------------------------------------------------------------------------------
! Recupera pagos
!------------------------------------------------------------------------------
Sub recuperaPagos
   string entry$

   entry$ = getCampoTSL$( SL.STR.ENTRY$, 1 )
   if entry$ = "99" then begin
      entry$ = getCampoTSL$( SL.STR.ENTRY$, 2 )
      if entry$ = "54" then begin
         !No es pago manual
         if (getCampoTSL$( SL.STR.ENTRY$, 28 )) = "0" then begin
            resMid = getCampoASC$( SL.STR.ENTRY$, 3 )
            resTid = getCampoASC$( SL.STR.ENTRY$, 4 )
            resLote = getCampoTSL$( SL.STR.ENTRY$, 5 )
            resModLectura = getCampoTSL$( SL.STR.ENTRY$, 6 )
            resNroTarjeta = getCampoASC$( SL.STR.ENTRY$, 7 )
            resNomFranquicia = getCampoASC$( SL.STR.ENTRY$, 8 )
            resSecuencia = getCampoTSL$( SL.STR.ENTRY$, 9 )
            resAutorizacion = getCampoTSL$( SL.STR.ENTRY$, 10 )
            resCuotas = getCampoTSL$( SL.STR.ENTRY$, 11 )
            resInteres = getCampoTSL$( SL.STR.ENTRY$, 12 )
            resNomDiferido = getCampoASC$( SL.STR.ENTRY$, 13 )
            resFecha = getCampoTSL$( SL.STR.ENTRY$, 14 )
            resHora = getCampoTSL$( SL.STR.ENTRY$, 15 )
            resIdRed = getCampoTSL$( SL.STR.ENTRY$, 16 )
            resBaseEx = getCampoTSL$( SL.STR.ENTRY$, 17 )
            resBaseAf = getCampoTSL$( SL.STR.ENTRY$, 18 )
            resImpuesto = getCampoTSL$( SL.STR.ENTRY$, 19 )
            resMonto = getCampoTSL$( SL.STR.ENTRY$, 20 )
            resNomCliente = getCampoASC$( SL.STR.ENTRY$, 21 )
            resTVR = getCampoASC$( SL.STR.ENTRY$, 22 )
            resTSI = getCampoASC$( SL.STR.ENTRY$, 23 )
            resCodDiferido = getCampoTSL$( SL.STR.ENTRY$, 24 )
            resMesGracia = getCampoTSL$( SL.STR.ENTRY$, 25 )
            reqTipoDiferido = getCampoTSL$( SL.STR.ENTRY$, 26 )
            resAnulado = getCampoTSL$( SL.STR.ENTRY$, 27 )
            descPago = val( getCampoTSL$( SL.STR.ENTRY$, 29 ) )
            if resAnulado = "1" then indPagoAnulado = buscaPagoParcial( reqSecuencia )

            call QXL.TSUPEC21("RECUPERA PAGOS: " + resSecuencia)
            call QXL.TSUPEC21("MID: " + resMid + " TID:" + resTid)

            call acumulaPagos
            call inicializaVariablesResponse
         EndIf
      EndIf
   EndIf

End Sub

!------------------------------------------------------------------------------
! Imprime vouchers de transacciones
!------------------------------------------------------------------------------
Sub imprimeVoucher( tipoVoucher )
   string lineaVoucher, s$
   integer*1 tipoVoucher, \
          i%, \
          k%, \
          indexPago%

   !call traceTAR("imprimeVoucher", "tipoVoucher=["+ str$(tipoVoucher) + "]" + \
   !                                         "cantPagos%=["+ str$(cantPagos%) + "]" + \
   !                                         "cantCopiasVoucher%=["+ str$(cantCopiasVoucher%) + "]" + \
   !                                         "cantLinVouCompra%=["+ str$(cantLinVouCompra%) + "]" + \
   !                                         "cantLinVouAnulacion%=["+ str$(cantLinVouAnulacion%) + "]")

   if tipoVoucher = 1 then goto voucherCompra
   if tipoVoucher = 2 then goto detallePago
   if tipoVoucher = 3 then goto voucherAnulacion
   if tipoVoucher = 4 then goto reimpresionCompra

   salir:
   Exit Sub

   voucherCompra:
   !if cantPagos% < 1 or noPrintVouRecuperaSuspencion then resume salir

   if cantPagos% < 1 then resume salir
   for indexPago% = 1 to cantPagos%
      call obtienePago( indexPago% )
      NUOcmdImpresion$ = NUO.Centrado$
      for i% = 1 to cantCopiasVoucher%

         if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
         for k% = 1 to cantLinVouCompra%
            lineaVoucher = reemplazaLineaVoucher( forVouCompra$( k% ) )
            call imprimeCJ( lineaVoucher )
         next k%
         if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
         lineaVoucher = "          -- ORIGINAL --              "
         if i% > 1 then lineaVoucher = "           -- COPIA --                "
         call imprimeCJ( lineaVoucher )
         if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
         call imprimeCJ( lineaDatosTrx$ )
         lineaVoucher = "----------------------------------------"
         call imprimeCJ( lineaVoucher )
         call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) )
         call imprime34( NUO.CortaPapel$ )

         if resAnulado = "1" then begin

            if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
            for k% = 1 to cantLinVouAnulacion%
               lineaVoucher = reemplazaLineaVoucher( forVouAnulacion$( k% ) )
               call imprimeCJ( lineaVoucher )
            next k%
            if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
            lineaVoucher = "          -- ORIGINAL --              "
            if i% > 1 then lineaVoucher = "           -- COPIA --                "
            call imprimeCJ( lineaVoucher )
            if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
            call imprimeCJ( lineaDatosTrx$ )
            lineaVoucher = "----------------------------------------"
            call imprimeCJ( lineaVoucher )
            call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) )
            call imprime34( NUO.CortaPapel$ )
         EndIf

      next i%

   next indexPago%

   resume salir

   voucherAnulacion:
   for i% = 1 to cantCopiasVoucher%
      NUOcmdImpresion$ = NUO.Centrado$
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      for k% = 1 to cantLinVouAnulacion%
         lineaVoucher = reemplazaLineaVoucher( forVouAnulacion$( k% ) )
         call imprimeCJ( lineaVoucher )
      next k%
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      lineaVoucher = "          -- ORIGINAL --              "
      if i% > 1 then lineaVoucher = "           -- COPIA --                "
      call imprimeCJ( lineaVoucher )
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      lineaVoucher = left$( lineaDatosTrx$, 23 ) + \
         right$( "0000" + str$( SL.HD.TRANSNUM + 1 ), 4 ) + \
         mid$( lineaDatosTrx$, 28, 38 )
      call imprimeCJ( lineaVoucher )
      lineaVoucher = "----------------------------------------"
      call imprimeCJ( lineaVoucher )
      call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) )
      call imprime34( NUO.CortaPapel$ )
   next i%
   resume salir

   detallePago:
   if resAnulado = "0" then begin
      call imprimeCJ( "******** PAGO TRANSACCION TEF *******" )
   EndIf else begin
      call imprimeCJ( "***** ANULACION TRANSACCION TEF *****" )
   EndIf
   call imprimeCJ( "RECIBO:" + resSecuencia + " AUT:" + resAutorizacion )
   call imprimeCJ( "TARJETA:"+ resNroTarjeta )
   lineaVoucher = formateaMonto( val( resMonto ) )
   if resAnulado = "1" then lineaVoucher = lineaVoucher + "-"
   call imprimeCJ( "MONTO COMPRA: USD " + lineaVoucher )
   TS.TEMP1$ = "20"+ left$(DATE$,2) + "/" + mid$(DATE$,3,2) + "/" + right$(DATE$,2)
   TS.TEMP1$ = TS.TEMP1$ + "  " + left$(TIME$,2) + ":" + mid$(TIME$,3,2) + ":" + right$(TIME$,2)
   call imprimeCJ(TS.TEMP1$)
   call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) )
   call imprime34( NUO.CortaPapel$ )
   resume salir

   reimpresionCompra:
   for i% = 1 to cantCopiasVoucher%
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      for k% = 1 to cantLinVouCompra%
         lineaVoucher = reemplazaLineaVoucher( forVouCompra$( k% ) )
         call imprimeCJ( lineaVoucher )
      next k%
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      lineaVoucher = "          -- ORIGINAL --              "
      if i% > 1 then lineaVoucher = "           -- COPIA --                "
      call imprimeCJ( lineaVoucher )
      if autoRespuesta then call imprimeCJ("     PRUEBA RESPUESTA SIMULADA     ")
      call imprimeCJ( lineaDatosTrx$ )
      lineaVoucher = "----------------------------------------"
      call imprimeCJ( lineaVoucher )
      call imprime34( NUO.SaltaLineas$ +  chr$( 1 ) )
      call imprime34( NUO.CortaPapel$ )
   next i%
   resume salir

End Sub

!------------------------------------------------------------------------------
! Graba archivo de vouchers
!------------------------------------------------------------------------------
Sub grabaVoucher
   string formato, final, linea
   Integer*4 largo

   close 93
   TS.ER.RETURN = -1
   open archVoucher AS 93 append
   if TS.ER.RETURN <> -1 Then Begin    ! Si no existe

      TS.ER.RETURN = -1
      create archVoucher AS 93
      if TS.ER.RETURN <> -1 Then Begin
         call muestraOperador("ERROR EN CREACION","DE VOUCHER ")
         wait; 1500
         call restauraDisplay
         Exit Sub
      EndIf
   EndIf

   call QXL.TSUPEC21("GRABA VOUCHER: " + resSecuencia)
   call QXL.TSUPEC21("MID: " + resMid + " TID:" + resTid)

   final = chr$( 13 ) + chr$( 10 )
   linea = resSecuencia + separador + \
           resAnulado + separador + \
           right$( string$( 15, " " ) + resMid , 15 ) + separador + \
           right$( string$( 8, " " ) + resTid , 8 ) + separador + \
           resLote + separador + \
           resModLectura + separador + \
           resNroTarjeta + separador + \
           right$( string$( 20, " " ) + resNomFranquicia , 20 ) + separador + \
           resAutorizacion + separador + \
           resCuotas + separador + \
           right$( string$( 8, " " ) + str$(val(resInteres)) , 8 ) + separador + \
           right$( string$( 30, " " ) + resNomDiferido , 30 ) + separador + \
           resFecha + separador + \
           resHora + separador + \
           resIdRed + separador + \
           right$( string$( 8, " " ) + str$( val( resBaseEx) ) , 8 ) + separador + \
           right$( string$( 8, " " ) + str$( val( resBaseAf) ) , 8 ) + separador + \
           right$( string$( 8, " " ) + str$( val( resImpuesto) ) , 8 ) + separador + \
           right$( string$( 8, " " ) + str$( val( resMonto) ) , 8 ) + separador + \
           right$( string$( 30, " " ) + resNomCliente, 30 ) + separador + \
           resTVR  + separador + \
           resTSI  + separador + \
           resCodDiferido + separador + \
           resMesGracia + separador + \
           resTipoDiferido

   largo = len( linea )
   formato = "C"+ str$( largo ) + " C2"
   write form formato; #93 ; linea, final
   close 93
End Sub

!------------------------------------------------------------------------------
! Agrega variedad de pago
!------------------------------------------------------------------------------
Sub agregaVariedadPago( variedad )
   integer*2 variedad

   if anulaPagoParcial then TS.IO.KEYS(1) = 70

   TS.IO.DATA$(3) = right$( str$( variedad ), 1 )            ! Variedad de Pago
   TS.IO.KEYS(3)  = 78                                       ! Tecla Slash
   TS.IO.DATA$(7) = resMonto                                 ! Valor a entregar
   TS.IO.KEYS(7)  = 94                                       ! Tipo de Pago
   TS.IO.DATA$(9) = resSecuencia                             ! Numero Secuencia
   TS.IO.MOTORKEY = TS.IO.KEYS(7)

   if anulaVoucher then begin
      TS.BAL.TAKEN   = -1                                    ! Reporta ya total en la trx
      TS.TRX.STATUS  =  1
      TS.ER.RETURN   = -1
   EndIf

   !call traceTAR( "agregaVariedadPago","resMonto=["+ resMonto + "]" + \
   !                       "variedadPago%=["+ str$( variedad ) + "]" + \
   !                       "resAutorizacion=["+ resAutorizacion + "]" )

End Sub

!------------------------------------------------------------------------------
! Procesa pago con tarjetas bancarias
!------------------------------------------------------------------------------
Sub procesaTransaccion
   integer*4 v
   string    s$

   !Descuento al pago
   if anulaPagoParcial then begin
      s$ = str$( val( reqMonto ) + descPago )
      v = consultaPago( s$, resBin , str$(variedadPago%) )
   EndIf else begin
      descPago = descuentoPago( reqMonto, resBin , str$(variedadPago%) )

!call traceNUO("procesaTransaccion monto=" + reqMonto + " desc=" + str$(descPago) + " AMTTENDE=" + str$(SL.TE.AMTTENDE))

      if descPago > 0 then begin
         reqMonto = str$( val( reqMonto ) - descPago )
      EndIf
   EndIf

!call traceNUO( "procesaTransaccion reqMonto=[" + reqMonto + "] descPago=[" + str$( descPago ) + "]" )

   if autoRespuesta then begin
      call muestraOperador("PROCESANDO COMPRA","SIMULACION, ESPERE..")
      wait; 1000
   EndIf else begin
       call muestraOperador("PROCESANDO COMPRA","PINPAD, ESPERE..")
   EndIf
   
   !call restauraDisplay

   call inicializaVariablesResponse

   if not enviaMensajeria( 88 ) then Exit Sub
   if not enviaMensajeria( 89 ) then Exit Sub

   call acumulaPagos
   call agregaVariedadPago( variedadPago% )
   call grabaCadenaUsuario
   call imprimeVoucher( 2 )
   call grabaVoucher
   pagoAprobado = -1

End Sub

!------------------------------------------------------------------------------
! Procesa pago manual
!------------------------------------------------------------------------------
Sub procesaTransaccionManual

   call muestraOperador("PROCESANDO COMPRA","MANUAL, ESPERE..")
   wait; 1000
   call restauraDisplay

   !jGuiSubState = 80539
   !call javaEvent(terminalSubStateMsg)

   if pagoManualDebito  then call agregaVariedadPago( varPagManDebito% )
   if pagoManualCredito then call agregaVariedadPago( varPagManCredito% )

   resAnulado="0"
   call grabaCadenaUsuario

   pagoManualDebito  = 0
   pagoManualCredito = 0

End Sub

!------------------------------------------------------------------------------
! Reimprime voucher
!------------------------------------------------------------------------------
Sub reimprimeVoucher

   if not validaReglas( 1 ) then goto salir

   reimpresionVoucher = -1

   if not capturaNroRecibo then goto salir
   if not validaNroRecibo then goto salir

   if resAnulado="1" then begin
      call imprimeVoucher( 3 )
   EndIf else begin
      call imprimeVoucher( 4 )
   EndIf

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg) 
   EndIf

   salir:
   reimpresionVoucher = 0
   call ignoraTeclas
   Exit Sub

End Sub

!------------------------------------------------------------------------------
! Anula pago realizado
!------------------------------------------------------------------------------
Sub anulacionVoucher

   if not validaReglas( 1 ) then goto salir

   anulaVoucher = -1

   !TS.IO.DATA$(5) = "1"                                     ! Secuencia cambio a efectivo
   !TS.IO.KEYS(5)  = 61                                      !
   !TS.IO.MOTORKEY = 61                                      !

   if not capturaNroRecibo then goto salir
   if not validaNroRecibo then goto salir

   if autoRespuesta then begin
      call muestraOperador("PROCESANDO ANULACION","SIMULACION, ESPERE..")
   EndIf else begin
      call muestraOperador("PROCESANDO ANULACION","PINPAD, ESPERE..")
   EndIf

   wait; 1000
   call restauraDisplay

   call inicializaVariablesResponse

   if not enviaMensajeria( 88 ) then goto salir
   if not enviaMensajeria( 89 ) then goto salir

   call agregaVariedadPago( variedadPago% )
   call grabaCadenaUsuario
   call imprimeVoucher( 3 )
   call actualizaVoucher

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg) 
   EndIf

   salir:
   anulaVoucher = 0
   call ignoraTeclas
   Exit Sub
End Sub

!------------------------------------------------------------------------------
! Captura monto a pagar
!------------------------------------------------------------------------------
Function  capturaMonto
   integer*1 capturaMonto
   string dt$, mt$

   capturaMonto=0
   reqMonto = ""

   if java.init = -1 then begin 
      jGuiSubState = 80541
      call javaEvent(terminalSubStateMsg) 
      dt$ = ingresoDatos$( "ELIJA FORMA", " DE PAGO" )
      dt$ = right$( dt$, len( dt$ ) - 2 )
      if val(dt$) = 1  then begin
         jGuiSubState = 80538     !pide teclado para monto parcial
         call javaEvent(terminalSubStateMsg)

         mt$ = ingresoDatos$( "INGRESE MONTO " , "" )
         if left$( mt$, 1 ) = "A" then Exit Function 
         if left$( mt$, 1 ) <> "P" then Exit Function 
         mt$ = right$( mt$, len( mt$ ) - 2 )
         TS.IO.DATA$(7) = mt$
         reqMonto = TS.IO.DATA$(7)

         jGuiSubState = 0

      EndIf

      if val(dt$) = 2  then begin
         reqMonto = STR$(TS.BALDUE(0))	  
      EndIf
      if val(dt$) = 3  then Exit Function 
   EndIf else begin
      !Monto a pagar
      reqMonto = TS.IO.DATA$(7)
      if reqMonto = "" then begin
         reqMonto = STR$(TS.BALDUE(0))
      EndIf
   EndIf

   if val(reqMonto) <= 0 then begin
      call obtieneMsgOperacion( 11, msg1$, msg2$)
      call muestraBorrar( msg1$ , msg2$ )
      Exit Function 
   EndIf

   ! Si pago supera total compra
   if val(reqMonto) > ( TS.BALDUE(0) ) then begin
      call obtieneMsgOperacion( 2, msg1$, msg2$)
      call muestraBorrar( msg1$ , msg2$ )
      Exit Function 
   EndIf

   if reqMonto <> "" then capturaMonto = -1

   !call traceTAR( "capturaMonto" , "TS.IO.DATA$(7)=["+ TS.IO.DATA$(7) + "]" + \
   !                       "TS.BALDUE(0)=["+ STR$(TS.BALDUE(0)) + "]" + \
   !                       "reqMonto=["+ reqMonto + "]" )

End Function 

!------------------------------------------------------------------------------
! Lee datos de diferido archivo "R::ADX_IDT1:TF2PARAM.DAT"
!------------------------------------------------------------------------------
Function  leeDatosDiferidos
   integer *1 leeDatosDiferidos
   string clave$, \
      filler$

   leeDatosDiferidos = 0
   clave$ = ""
   filler$ = ""

   close 80
   TS.ER.RETURN = -1
   open "R::ADX_IDT1:TF2PARAM.DAT" keyed recl 43 as 80
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar("ERROR PARAMETROS", "DIFERIDOS")
      goto salir
   EndIf

   clave$ = pack$( right$( "00" + reqOperacion, 2 ) + right$("00"+ reqCodDiferido, 2) )

   TS.ER.RETURN = -1
   read form "C2 2C20 C1";#80 key clave$; filler$,  reqNomDiferido, reqCuotas, reqTipoDiferido
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar("CODIGO DIFERIDO", "NO  EXISTE")
      goto salir
   EndIf

   leeDatosDiferidos = -1

   salir:
   close 80

   !call traceTAR( "leeDatosDiferidos" ,  "reqOperacion=["+ reqOperacion + "]" + \
   !                       "reqCodDiferido=["+ reqCodDiferido + "]" + \
   !                       "reqNomDiferido=["+ reqNomDiferido + "]"  + \
   !                       "reqCuotas=["+  reqCuotas + "]" + \
   !                       "reqTipoDiferido=["+ reqTipoDiferido + "]")
  Exit Function 

End Function 

!------------------------------------------------------------------------------
! Captura c�digo de diferido
!------------------------------------------------------------------------------
Function  capturaCodDiferido
   integer *1 capturaCodDiferido
   string d$

   capturaCodDiferido = 0
   d$ = ""

    jGuiSubState = 80529
    call javaEvent(terminalSubStateMsg)

   d$ = ingresoDatos$( "CODIGO DIFERIDO...?", \
                       left$( codDiferidos$( val( reqOperacion ) ), 20 ) )

   if java.init then begin
      jGuiSubState = 0
   EndIf

   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then Exit Function 
      reqCodDiferido = d$
      capturaCodDiferido = -1
   EndIf

   !call traceTAR( "capturaCodDiferido" , "reqOperacion=["+ reqOperacion + "]" + \
   !                       "reqCodDiferido=["+ reqCodDiferido + "]" )

End Function 

!------------------------------------------------------------------------------
! Confirma c�digo de diferido
!------------------------------------------------------------------------------
Function  confirmaDiferido
   integer *1 confirmaDiferido
   string d$

   confirmaDiferido = 0
   d$ = ""

   if java.init = -1 then begin
      jGuiSubState = 80540
      call javaEvent(terminalSubStateMsg)
      d$ = ingresoDatos$( reqNomDiferido , "?" )
   EndIf else begin  
      d$ = ingresoDatos$( reqNomDiferido , "CORRECTO? 1.SI 0.NO" )
   EndIf

   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then Exit Function 
      if val( d$ ) = 1 then confirmaDiferido = -1
   EndIf

End Function 

!------------------------------------------------------------------------------
! Captura meses de diferido de archivo "R::ADX_IDT1:TF2PARAM.DAT"
!------------------------------------------------------------------------------
Function  capturaCuota
   integer*1 capturaCuota, k%
   string a$, b$, d$

   k% = 0
   capturaCuota = 0
   d$ = ""

   ingresaCuota:

   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80501
   EndIf
   call javaEvent(terminalSubStateMsg)  

   d$ = ingresoDatos$( "MESES A DIFERIR", reqCuotas )
   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then Exit Function 

      a$ = "," + reqCuotas
      b$ = "," + d$ + ","
      k% = match( b$ , a$ ,1)

      if k% <= 0 then begin
         if jGuiSubState = 80501 then begin
            jGuiSubState = 80527 
         EndIf else begin
            jGuiSubState = 0 
         EndIf
         goto ingresaCuota
      EndIf else begin
         reqCuotas = d$
         capturaCuota = -1
      EndIf

   EndIf

   if java.init then begin
      jGuiSubState = 0
   EndIf

   !call traceTAR( "capturaCuota" , "reqCuotas=["+ reqCuotas + "]")

End Function 

!------------------------------------------------------------------------------
! Captura tipo de Diferido
!------------------------------------------------------------------------------
Function  capturaDatosDiferido
   integer*1 capturaDatosDiferido

   capturaDatosDiferido = 0

   if not capturaCodDiferido then Exit Function 
   if not leeDatosDiferidos then Exit Function 
   if not confirmaDiferido then Exit Function 
   if not capturaCuota then Exit Function 

   capturaDatosDiferido = -1

End Function 

!------------------------------------------------------------------------------
! Captura tipo de financiamiento
!------------------------------------------------------------------------------
Function  capturaFinanciamiento
   integer*1 capturaFinanciamiento
   string d$

   capturaFinanciamiento = 0
   reqFinanciamiento =""

   reqCodDiferido = ""
   reqNomDiferido = ""
   reqTipoDiferido = ""
   reqCuotas = ""
   d$ =""

   call obtieneMsgOperacion( 4, msg1$, msg2$ )

   ingresaFinanciamiento:

   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80501
   EndIf
   call javaEvent(terminalSubStateMsg)
   d$ = ingresoDatos$( msg1$, msg2$ )

   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then Exit Function 

      if not ( val(d$) >= 1 and val(d$) <= 2 ) then begin
         if jGuiSubState = 80501 then begin
            jGuiSubState = 80527
         EndIf else begin
            jGuiSubState = 0
         EndIf
         goto ingresaFinanciamiento
      EndIf

      if java.init then begin
        jGuiSubState = 0
      EndIf

      if d$ = "1" then reqFinanciamiento = "00"
      if d$ = "2" then reqFinanciamiento = "01"
      if d$ = "3" then reqFinanciamiento = "02"

      if reqFinanciamiento = "00" then reqOperacion = "01" ! Operacion de compra cte
      if reqFinanciamiento = "01" then reqOperacion = "02" ! Operacion de diferido

      if reqOperacion = "02" then begin
         if not capturaDatosDiferido then begin
            call obtieneMsgOperacion( 3, msg1$, msg2$ )
            call muestraBorrar( msg1$ , msg2$ )
            Exit Function 
         EndIf
      EndIf

      capturaFinanciamiento = -1

      !call traceTAR( "capturaFinanciamiento" , "d$=["+ d$ + "]" + \
      !                       "reqFinanciamiento=["+ reqFinanciamiento + "]" )

   EndIf else begin
      call muestraBorrar("PROCEDIMIENTO","CANCELADO ..")
   EndIf

End Function 

!------------------------------------------------------------------------------
! Muestra resumen de tarifas para pagos manuales
!------------------------------------------------------------------------------
Sub procesaConsultaDatosManual
   integer*4 \
      baseEx%, \
      baseAf%, \
      impuesto%, \
      t0, t1, t2

   integer*1 calculoParcial

   real factor%

   baseEx%= 0
   baseAf%= 0
   impuesto%= 0
   factor% = 0
   calculoParcial = 0

   if not validaReglas( 3 ) then goto salir

   if not ( ( MSMTFACTtotal - NUOTPROMdescuento ) > 0 AND TS.INTRX ) then Exit Sub

   if TS.BALDUE( 0 ) > 0 then begin

      call tarifasConDescuento( t0, t1, t2 )
      baseEx%   = t0
      baseAf%   = t1
      impuesto% = t2

      if TS.BALDUE( 0 ) <> ( MSMTFACTtotal - NUOTPROMdescuento ) then calculoParcial = -1

      if calculoParcial then begin
         factor%   = val( str$ ( TS.BALDUE( 0 ) ) )  / ( MSMTFACTtotal - NUOTPROMdescuento )
         baseEx%   = round( 1.0   * baseEx%   * factor% , 0, 0 )
         baseAf%   = round( 1.0   * baseAf%   * factor% , 0, 0 )
         impuesto% = round( 1.0   * impuesto% * factor% , 0, 0 )
      EndIf

      msg1$ = formateaMonto( baseAf% )
      msg2$ = formateaMonto( impuesto% )
      msg1$ = "BASE IVA: $" + msg1$
      msg2$ = "IVA  " + str$( MSMTFACTimpuestoIVA% ) + "%: $" + msg2$
      msg1$ = left$( msg1$ + string$( 20, " " ), 20 )
      msg2$ = left$( msg2$ + string$( 20, " " ), 20 )
      call muestraBorrar( msg1$ , msg2$ )
      msg1$ = formateaMonto( baseEx% )
      msg1$ = "IVA  0%: $" + msg1$
      msg1$ = left$( msg1$ + string$( 20, " " ), 20 )
      call muestraBorrar( msg1$ , "" )

      !call traceTAR( "procesaConsultaDatosManual" , "total=["+ str$( ( MSMTFACTtotal - NUOTPROMdescuento ) ) + "]" + \
      !                       "TS.BALDUE( 0 )=["+ str$( TS.BALDUE( 0 ) ) + "]" + \
      !                       "calculoParcial=["+ str$( calculoParcial ) + "]" + \
      !                       "factor%=["+ str$( factor% ) + "]" + \
      !                       "baseEx%=["+ str$( baseEx% ) + "]" + \
      !                       "baseAf%=["+ str$( baseAf% ) + "]" + \
      !                       "impuesto%=["+ str$( impuesto% ) + "]")

   EndIf else begin

      ! Solicita total a la trx
      TS.BAL.TAKEN = 0
      TS.GUIDANCE = 1020
      TS.IO.MOTORKEY = 0
      Exit Sub

   EndIf

   salir:
   call ignoraTeclas
   Exit Sub

End Sub

!------------------------------------------------------------------------------
! Captura bin para pago manual
!------------------------------------------------------------------------------
Function  capturaBinManual
   integer *1 capturaBinManual, binActivo
   integer *2 i%
   capturaBinManual = 0
   binActivo = 0
   resNroTarjeta = ""

   ingresaBin:

   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80501
   EndIf
   call javaEvent(terminalSubStateMsg)
   resNroTarjeta = ingresoDatos$( "INGRESE NUMERO" , "DE BIN" )
   if left$( resNroTarjeta, 1 ) = "P" then begin
      resNroTarjeta = right$( resNroTarjeta, len(resNroTarjeta) - 2 )
      if not esNumero( resNroTarjeta ) then Exit Function 
      if len( resNroTarjeta ) <> 6 then begin
         call muestraBorrar("NUMERO DE BIN", "INVALIDO    <Borrar>")
         if jGuiSubState = 80501 then begin
            jGuiSubState = 80527 
         EndIf else begin
            jGuiSubState = 0 
         EndIf
         goto ingresaBin
      EndIf

      if java.init then begin
         jGuiSubState = 0
      EndIf

      if resNroTarjeta <> "" then begin

         for i% = 1 to cantParBines%
            if val( resNroTarjeta ) >= val( parBines$( i%,0 ) ) and \
               val( resNroTarjeta ) <= val( parBines$( i%,1 ) ) then begin
               binActivo = -1
               i% = cantParBines%
            EndIf
         next i%

         if binActivo then begin
            capturaBinManual = -1
         EndIf else begin
            call muestraBorrar("BIN NO AUTORIZADO", "            <Borrar>")
            Exit Function 
         EndIf

      EndIf

   EndIf

   if java.init then begin
      jGuiSubState = 0
   EndIf

End Function 

!------------------------------------------------------------------------------
! Captura autorizacion para pago manual
!------------------------------------------------------------------------------
Function  capturaAutorizacionManual
   integer *1 capturaAutorizacionManual

   capturaAutorizacionManual = 0
   resAutorizacion = ""

   ingresaAutorizacion:
   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80501
   EndIf
   call javaEvent(terminalSubStateMsg)
   resAutorizacion = ingresoDatos$( "INGRESE NUMERO" , "DE AUTORIZACION" )
   if left$( resAutorizacion, 1 ) = "P" then begin
      resAutorizacion = right$( resAutorizacion, len(resAutorizacion) - 2 )
      if not esNumero( resAutorizacion ) then Exit Function 
      if len( resAutorizacion ) <> 6 then begin                                                                        ! Si Error longitud nro lote
         call muestraBorrar("NRO DE AUTORIZACION", "INVALIDO    <Borrar>")
         if jGuiSubState = 80501 then begin
            jGuiSubState = 80527 
         EndIf else begin
            jGuiSubState = 0 
         EndIf
         goto ingresaAutorizacion                                                                                             ! Captura dato nuevmente
      EndIf
      if resAutorizacion <> "" then capturaAutorizacionManual = -1
   EndIf

End Function 

!------------------------------------------------------------------------------
! Captura lote para pago manual
!------------------------------------------------------------------------------
Function  capturaLoteManual
   integer *1 capturaLoteManual

   capturaLoteManual = 0
   resLote = ""

   ingresaLote:

   if not (jGuiSubState = 80519) then begin
      jGuiSubState = 80529
   EndIf
   call javaEvent(terminalSubStateMsg)
   resLote = ingresoDatos$( "INGRESEN NUMERO" , "DE LOTE" )
   if left$( resLote, 1 ) = "P" then begin
      resLote = right$( resLote, len(resLote) - 2 )
      if not esNumero( resLote ) then Exit Function 
      if len( resLote ) <> 6 then begin                                        ! Si Error longitud nro lote
         call muestraBorrar("NRO DE LOTE", "INVALIDO    <Borrar>")
         if jGuiSubState = 80529 then begin
            jGuiSubState = 80519
         EndIf else begin
            jGuiSubState = 0
         EndIf
         goto ingresaLote                                                      ! Captura dato nuevmente
      EndIf
      if resLote <> "" then capturaLoteManual = -1
   EndIf

   if java.init then begin
      jGuiSubState = 0
   EndIf

End Function 

!------------------------------------------------------------------------------
! Captura recibo para pago manual
!------------------------------------------------------------------------------
Function  capturaReciboManual
   integer *1 capturaReciboManual

   capturaReciboManual = 0
   resSecuencia = ""

   ingresaRecibo:
   if not (jGuiSubState = 80510) then begin
      jGuiSubState = 80529
   EndIf
   call javaEvent(terminalSubStateMsg)
   resSecuencia = ingresoDatos$( "INGRESE NUMERO" , "DE RECIBO" )
   if left$( resSecuencia, 1 ) = "P" then begin
      resSecuencia = right$( resSecuencia, len(resSecuencia) - 2 )
      if not esNumero( resSecuencia ) then Exit Function 
      if len( resSecuencia ) <> 6 then begin                                   ! Si Error longitud nro lote
         call muestraBorrar("NRO DE RECIBO", "INVALIDO    <Borrar>")
         if jGuiSubState = 80529 then begin
            jGuiSubState = 80510 
         EndIf else begin
            jGuiSubState = 0 
         EndIf
         goto ingresaRecibo                                                    ! Captura dato nuevmente
      EndIf
      if resSecuencia <> "" then capturaReciboManual = -1
   EndIf

   if java.init  then begin
      jGuiSubState = 0
   EndIf
End Function 

!------------------------------------------------------------------------------
! Captura cuota para pago manual
!------------------------------------------------------------------------------
Function  capturaCuotaManual
   integer *1 capturaCuotaManual

   capturaCuotaManual = 0
   resCuotas = ""

   ingresaCuota:
   if not (jGuiSubState = 80515) then begin
      jGuiSubState = 80519
   EndIf
   call javaEvent(terminalSubStateMsg)
   resCuotas = ingresoDatos$( "INGRESE NUMERO" , "DE CUOTAS" )
   if left$( resCuotas, 1 ) = "P" then begin
      resCuotas = right$( resCuotas, len(resCuotas) - 2 )
      if not esNumero( resCuotas ) then Exit Function 
      if val( resCuotas ) < 1 then begin                                       ! Si Error longitud nro lote
         call muestraBorrar("NRO DE CUOTAS", "INVALIDO    <Borrar>")
         if jGuiSubState = 80519 then begin
            jGuiSubState = 80515
         EndIf else begin
            jGuiSubState = 0
         EndIf
         goto ingresaCuota                                                     ! Captura dato nuevmente
      EndIf
      if resCuotas <> "" then capturaCuotaManual = -1
      resCuotas = right$( "00" + resCuotas, 2 )
   EndIf

End Function 

!------------------------------------------------------------------------------
! Captura tipo credito para pago manual
!------------------------------------------------------------------------------
Function  capturaTipoCreditoManual
   integer *1 capturaTipoCreditoManual
   string d$

   d$ = ""
   capturaTipoCreditoManual = 0
   call obtieneMsgOperacion( 4, msg1$, msg2$ )
   resCuotas = ""

   ingresaTipoCredito:

   if not (jGuiSubState = 80527) then begin
      jGuiSubState = 80510
   EndIf
   call javaEvent(terminalSubStateMsg)
   d$ = ingresoDatos$( msg1$, msg2$ )

   if left$( d$, 1 ) = "P" then begin
      d$ = right$( d$, len(d$) - 2 )
      if not esNumero( d$ ) then Exit Function 
      if not ( val(d$) >= 1 and val(d$) <= 2 ) then begin
         if jGuiSubState = 80510 then begin
            jGuiSubState = 80527 
         EndIf else begin
            jGuiSubState = 0 
         EndIf
         goto ingresaTipoCredito
      EndIf
      if val(d$) = 1 then begin
         resCuotas = ""
         capturaTipoCreditoManual = -1
      EndIf else begin
         if capturaCuotaManual then capturaTipoCreditoManual = -1
      EndIf
   EndIf

   if java.init then begin
      jGuiSubState = 0
   EndIf

End Function 

!------------------------------------------------------------------------------
! Procesa pagos manuales
!------------------------------------------------------------------------------
Sub procesaPagoManual
   integer *1 capturaDatos
   string tipoingreso$, datoingreso$

   capturaDatos = 0
   pagoManualDebito=0
   pagoManualCredito=0

   anulaPagoParcial=0
   if TS.IO.KEYS(1) = 70 then anulaPagoParcial = -1

   if not validaReglas( 3 ) then goto salir

   call inicializaVariablesResponse

   if java.Init = -1 Then begin 

      jGuiSubState = 80545
      call javaEvent(terminalSubStateMsg)

      tipoingreso$ = ingresoDatos$( "ELIJA EL TIPO " , "" )
      if left$( tipoingreso$, 1 ) = "A" then goto salir
      if left$( tipoingreso$, 1 ) <> "P" then goto salir
      tipoingreso$ = right$( tipoingreso$, len( tipoingreso$ ) - 2 )
      TS.IO.DATA$(3) = tipoingreso$

      jGuiSubState = 80538
      call javaEvent(terminalSubStateMsg)

      datoingreso$ = ingresoDatos$( "INGRESE MONTO " , "" )
      if left$( datoingreso$, 1 ) = "A" then goto salir
      if left$( datoingreso$, 1 ) <> "P" then goto salir
      datoingreso$ = right$( datoingreso$, len( datoingreso$ ) - 2 )
      TS.IO.DATA$(7) = datoingreso$
      resMonto = TS.IO.DATA$(7)
      if resMonto = "" then goto salir
   EndIf else begin
      resMonto = TS.IO.DATA$(7)
      if resMonto = "" then resMonto = STR$(TS.BALDUE(0))
   EndIf

   if not ( TS.IO.DATA$(3) = "2" or TS.IO.DATA$(3) = "3" ) then begin
      call muestraBorrar("FORMA DE PAGO NO" , "VALIDA      <Borrar>")
      goto salir
   EndIf else if TS.IO.DATA$(3) = right$( str$( varPagManDebito% ), 1 ) then begin
      pagoManualDebito = -1
      call muestraOperador( "TARJETA DEB. MANUAL", "MONTO: $" + formateaMonto( val( resMonto )))
   EndIf else if TS.IO.DATA$(3) = right$( str$( varPagManCredito% ), 1 ) then begin
      pagoManualCredito = -1
      call muestraOperador( "TARJETA CRD. MANUAL", "MONTO: $" + formateaMonto( val( resMonto ) ) )
   EndIf

   if not( pagoManualDebito or pagoManualCredito ) then Exit Sub
   wait; 2000
   call restauraDisplay

   if not capturaBinManual then goto salir
   if pagoManualCredito then begin
      if not capturaTipoCreditoManual then goto salir
   EndIf
   if not capturaLoteManual then goto salir
   if not capturaAutorizacionManual then goto salir
   if not capturaReciboManual then goto salir
   capturaDatos = -1

   if capturaDatos then goto proceso

   salir:
   call ignoraTeclas
   Exit Sub

   proceso:
   if capturaDatos then call procesaTransaccionManual

End Sub

!------------------------------------------------------------------------------
! Captura datos necesarios para procesar el pago
!------------------------------------------------------------------------------
Sub procesaOperacionTEF
   integer*1 capturaDatos

   capturaDatos = 0

   anulaPagoParcial = 0
   if TS.IO.KEYS(1) = 70 then anulaPagoParcial = -1

   if not validaReglas( 2 ) then Exit Sub

   if not anulaPagoParcial then begin
      if not capturaMonto then goto salir
   EndIf

!call traceNUO( "procesaOperacionTEF anulaPagoParcial=" + str$(anulaPagoParcial) + \
!   " regMonto=[" + reqMonto + "]" )

   if not capturaFinanciamiento then goto salir

   if anulaPagoParcial then begin
      if not capturaNroRecibo then goto salir
      if not validaNroRecibo  then goto salir
      capturaDatos = -1
   EndIf else begin
      if not capturaTarjeta then goto salir
      capturaDatos = -1
   EndIf

   salir:
   call ignoraTeclas

   if capturaDatos then call procesaTransaccion

End Sub

!------------------------------------------------------------------------------
! Al finalizar una transacci�n de venta
!------------------------------------------------------------------------------
Sub MSMTTARB02 public

   if not activo then Exit Sub

   !Indicador de pago aprobado
   pagoAprobado = 0

   !Indicador de control cuando se presiona BORRAR/BORRAR
   borrarBorrar = 0

   cantPagos% = 0
   call inicializaVariablesRequest
   call inicializaVariablesResponse
   descPago = 0
   !noPrintVouRecuperaSuspencion = 0

End Sub
!------------------------------------------------------------------------------
! Despues de apertura de todos los archivos
!------------------------------------------------------------------------------
Sub MSMTTARB07 public

   archVoucher   = "R::ADX_UDT1:VO" + left$( DATE$, 6 ) + "." + right$( "000" + str$( SL.HD.TERMINAL ), 3 )
   archAuditoria = "R::ADX_UDT1:TF" + left$( DATE$, 6 ) + "." + right$( "000" + str$( SL.HD.TERMINAL ), 3 )
   separador = "|"
   cantPagos% = 0
   call leeParametros
   dim resPagos$( cantMaxPagos% , 26 )

End Sub

!------------------------------------------------------------------------------
! Despues de la lectura del Teclado/Scan/Lect Optico
!------------------------------------------------------------------------------
Sub MSMTTARB14 public

   if not activo then Exit Sub

!call traceNUO( "MSMTTARB14 pagoAprobado=" + str$(pagoAprobado) + " borrarBorrar=" + str$(borrarBorrar) )

   ! Se controla si se presiono la tecla Borrar/Borrar cuando se espera
   ! la llave de supervisor y esta activo un pago de tarjeta bancaria
   if pagoAprobado and ( TS.IO.MOTORKEY = 73 ) and ( borrarBorrar = 1 ) then begin
      borrarBorrar = 2
      Exit Sub
   EndIf

   if TS.IO.MOTORKEY = teclaConsultaTarifas% then begin
      call procesaConsultaDatosManual
      Exit Sub
   EndIf

   if TS.IO.STATE = 10 and TS.IO.MOTORKEY = 94 then begin
      if java.init = -1 then begin
         if NOT TS.BAL.TAKEN then begin
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            Exit Sub
         EndIf
      EndIf

      call procesaPagoManual
      Exit Sub
   EndIf

   ! Si anulacion total
   if TS.IO.KEYS(1) = 70 and TS.IO.MOTORKEY = 81 then begin
      if ( ( MSMTFACTtotal - NUOTPROMdescuento ) <> TS.BALDUE(0) ) then begin       ! Si pagos capturados
         TS.GUIDANCE = 1069     ! Msg Error appl nativa
         TS.IO.MOTORKEY = 0
         Exit Sub
      EndIf
   EndIf

   if ( TS.IO.KEYS(5) = 61 ) and ( TS.IO.STATE = 10 ) then begin
      if ( val( TS.IO.DATA$( 5 ) ) = 60 ) then begin
         call cierrePinpad
         Exit Sub
      EndIf
!-- Se retiran procedimientos TEF 24 Nov 2023 - GR 
!      if ( val( TS.IO.DATA$( 5 ) ) = 61 ) then begin
!         call actualizaIpPinpad
!         Exit Sub
!      EndIf

!      if ( val( TS.IO.DATA$( 5 ) ) = 62 ) then begin
!         call actualizaSwPinpad
!         Exit Sub
!      EndIf

!      if ( val( TS.IO.DATA$( 5 ) ) = 63 ) then begin
!         call anulacionVoucher
!         Exit Sub
!      EndIf

      if ( val( TS.IO.DATA$( 5 ) ) = 64 ) then begin
         call reimprimeVoucher
         Exit Sub
      EndIf
   EndIf

   if TS.IO.MOTORKEY = teclaMotora% then begin
      if requiereCambioCaja then Exit Sub
      if TS.IO.DATA$(7) = "" then begin
         TS.IO.DATA$(7) = str$( TS.TOTALS(0,0,0) - TS.TENDERED(0) - NUOTPROMdescuento )
!call traceNUO( "TOTALS=" + str$(TS.TOTALS(0,0,0)) + " TENDERED=" + str$(TS.TENDERED(0)) + " desc=" + str$(NUOTPROMdescuento) )
      EndIf
      call procesaOperacionTEF
   EndIf

End Sub

!------------------------------------------------------------------------------
! Antes de imprimir a la estacion de recibo de caja
!------------------------------------------------------------------------------
Sub MSMTTARB20 public

   if not activo then Exit Sub

   !call traceTAR( "MSMTTARB20" , "TS.LINETYPE=["+ str$(TS.LINETYPE) + "]" + \
   !                        "TS.LINEDATA=["+ str$(TS.LINEDATA) + "]" + \
   !                        "TS.PRTBUF$=["+ TS.PRTBUF$ + "]")

   if ( TS.LINETYPE = 6 ) and ( TS.LINEDATA = 1 ) then begin
      lineaDatosTrx$ = TS.PRTBUF$
      call imprimeVoucher(1)
   EndIf

End Sub

!------------------------------------------------------------------------------
! Limpia variables para controlar Borrar/ Borrar
!------------------------------------------------------------------------------
Sub MSMTTARB30 public

   pagoAprobado = 0
   borrarBorrar = 0

End Sub

!------------------------------------------------------------------------------
! Control de limites de entrada de items y cancelacion de items
!------------------------------------------------------------------------------
Sub MSMTTARB39 public

   if not activo then Exit Sub

   ! Si se tiene una transaccion bancaria aprobada se setea indicador
   ! de control que permite controlar la generacion de una reversa automatica
   ! para los casos en los cuales la operadora no ingresa la llave del supervisor
   if pagoAprobado then borrarBorrar = 1

End Sub

!------------------------------------------------------------------------------
! Control de limites de formas de pago
!------------------------------------------------------------------------------
Sub MSMTTARB40 public

   if not activo then Exit Sub
!call traceNUO( "MSMTTARB40 pagoAprobado=" + str$(pagoAprobado) + " borrarBorrar=" + str$(borrarBorrar) )
!call traceNUO( "MSMTTARB40 monto=" + reqMonto + " desc=" + str$(descPago) + " AMTTENDE=" + str$(SL.TE.AMTTENDE))

   ! Si tenemos una transaccion bancaria aprobada se setea el indicador
   ! de control que permite controlar la generacion de una reversa automatica
   ! para los casos en los cuales la operadora no ingresa la llave del supervisor
   if pagoAprobado then borrarBorrar = 1

End Sub

!------------------------------------------------------------------------------
! Subrutina que controla BORRAR/BORRAR
!------------------------------------------------------------------------------
Sub MSMTTARB51 public

   if not activo then Exit Sub
!call traceNUO( "MSMTTARB51 pagoAprobado=" + str$(pagoAprobado) + " borrarBorrar=" + str$(borrarBorrar) )
   ! Si se presiono la tecla Borrar/Borrar y estaba activa una
   ! transaccion bancaria se regenera el IOPROC
   if pagoAprobado and ( borrarBorrar = 2 ) and \
      ( TS.IO.STATE = 10 ) and ( TS.IO.NEXTSTATE = 0 ) then begin

      ! Se limpia la variable de control
      borrarBorrar = 0

      ! Se arma el arreglo del IOPROC
      dim TS.IO.KEYS(10), TS.IO.DATA$(10)
      TS.IO.STATE = 10
      TS.IO.DEVICE = 1
      TS.IO.KEYS(3) = 78
      TS.IO.DATA$(3) = right$( str$( variedadPago% ), 1 )     ! Variedad de Pago
      TS.IO.KEYS(7)  = 94                                     ! Tipo de Pago
      TS.IO.DATA$(7) = resMonto                               ! Valor a entregar
      TS.IO.DATA$(9) = resSecuencia                           ! Numero Secuencia
      TS.IO.KEYS(9) = 90
      TS.IO.MOTORKEY = 94                                     ! Tipo de Pago
      TS.IO.INPUT.READY = -1
   EndIf

End Sub

!------------------------------------------------------------------------------
! En la recuperaci�n de transacci�n
!------------------------------------------------------------------------------
Sub MSMTTARB53 public

   if not activo then Exit Sub

   call recuperaPagos

End Sub

!------------------------------------------------------------------------------
! En la recuperaci�n de transacci�n
!------------------------------------------------------------------------------
Sub MSMTTARB68(ENTRY$, ELEMENT) public
   STRING ENTRY$, dato$
   INTEGER*2 ELEMENT

   if not activo then Exit Sub
   if TS.RETV.IN.PROGRESS then begin
      dato$ = ENTRY$
      !call traceTAR("MSMTTARB68 ENTRY$=["  + lineahex$(dato$) + "]"," " )
      if left$(dato$,3) = pack$("99") + ":" + pack$("54") then begin
         ENTRY$ = left$(dato$,2) + pack$("8854") + right$(dato$,len(dato$)-3)
         !call traceTAR("MSMTTARB68 SE CAMBIO ENTRY$=["  + lineahex$(ENTRY$) + "]", " " )
      EndIf
      !noPrintVouRecuperaSuspencion = -1
   EndIf

End Sub

!------------------------------------------------------------------------------
! Anula pago realizado
!------------------------------------------------------------------------------
Function  anulaPagoTEF public
   integer*1 anulaPagoTEF

   anulaPagoTEF = 0
   anulaVoucher = -1

   if not capturaNroRecibo then goto salir
   if not validaNroRecibo then goto salir

   if autoRespuesta then begin
      call muestraOperador("PROCESANDO ANULACION","SIMULACION, ESPERE..")
   EndIf else begin
      call muestraOperador("PROCESANDO ANULACION","PINPAD, ESPERE..")
   EndIf

   wait; 1000
   call restauraDisplay

   call inicializaVariablesResponse

   if not enviaMensajeria( 88 ) then goto salir
   if not enviaMensajeria( 89 ) then goto salir

   call grabaCadenaUsuario
   call imprimeVoucher( 3 )
   call actualizaVoucher

   anulaPagoTEF = -1

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg)
   EndIf

   salir:
   anulaVoucher = 0

End Function 

