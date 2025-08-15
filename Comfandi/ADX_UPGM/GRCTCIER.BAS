!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCTCIER.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Control totales cierre           *
!                Transacciones en linea           *
!**************************************************
! Version 1.0 30/Nov/2021
!--------------------------------------------------

String    Global  Men$, Gtmp$, A$, B$,																		 \! Variables aplicacion
                  Str.Log$,																								 \! Trx a procesar
                  Nro.Term$,                                               \! Nro Terminal
                  Nro.Trans$,                                              \! Nro transaccion
                  Fec.Hor$,                                                \! Fecha y hora trx
                  Tipo.Trx$,                                               \! Tipo de trx
                  Nro.String$,                                             \! Nro Strings
                  Nro.Cajero$,                                             \! Operador
                  Pwd.Cajero$,                                             \! Pwd cajero
                  Grossplus$,                                              \! Gross +
                  GrossMinus$,                                             \! Gross -
                  Nro.Strings$,                                            \! Nro de strings
                  Cod.Item$,																							 \! Item vendido
                  Price.Item$,																						 \! Precio de venta
                  Qty.Venta$,																							 \! Cantidad Vendida
                  Cabecera$,                                               \! Cabecera archivo
                  Grtienda$,                                               \! Nro de la tienda
                  Gr.File.Ruta$,																				   \! ruta encolamiento
                  Nro.fiscal$,                                             \! Nro. Factura fsical
                  TS.TEMP3$,                                               \! Temporal salida datos
                  ARCHIVO.PROC$,                                           \! Tlog a procesar
                  Yfile$                                                    ! Archivo salida
                  
Integer*4 Global  Puntero%,																								 \! Puntero aplicacion
                  PP, K%, J%,																							 \! De control
                  Item.Apnt%                                                ! Item Apuntador

Integer*1 Global  Estado.Ejec%,																						 \! Estado ejecucion aplicacion
                  GrVenta%,                                                \! Control si es una venta
                  Sgn.Vta%,                                                \! Signo venta Item
                  Xtv%,                                                    \! Tipo variedad Pago
                  Peso%,                                                   \! Item pesable 
                  FinTlog%,                                                \! Fin recorrido Tlog
                  GrError%                                                  ! Control Errores aplicacion
Integer*4 Global  TGrosp%, TGrosn%, Tdscto%, Tmov%, Titms%, INDICAT2,      \!
                  INDICAT3, Pagos(1), Total.Pagos%, INDICAT1, Tmisc%        !
String    Global  Precio$, Alto$, Bajo$, Signo$, DateClose$, Tplu$          !

%INCLUDE  BASROUT.J86

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  								! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub																																			! Fin Adxserve

Function BIGLEN(A$) EXTERNAL
  Integer*4 BIGLEN
  String A$
End Function

Function ENTRADA.LOG
 If Estado.Ejec% = -1 Then Call ADXSERVE(PP,26,1,MEN$) 										 \! Mensaje Background
  Else Print MEN$																														! Mensaje a pantalla
End Function 

Sub GETUNPK																																	! Desempaqueta dato String
  K% = Match(":",B$,J%) 																										! Busca separador de campo
  If (K%-J%) > 0 Then 																										 \! Si hay datos
     A$ = UnPack$(Mid$(B$,J%,K%-J%)) Else A$ = ""														! desempaqueta campo
  J% = K% + 1 																															! Apuntador proximo String
End Sub 																																		! Fin desempaqueta String

Function FORMAT.VALORES( FDAMT )
  Integer*1 Format.VALORES
  INTEGER*4 FDAMT
  INTEGER*2 THOU, MILL
  IF FDAMT < 0 THEN                                     \
    TS.TEMP3$ = STR$(-FDAMT) + "-"                      \
  ELSE                                                  \
    TS.TEMP3$ = STR$(FDAMT)  + " "
  THOU = 4                                              ! for thousands
  MILL = 8                                              ! for millions
  IF (LEN(TS.TEMP3$) > THOU) THEN                       \ thousands
      TS.TEMP3$ = LEFT$(TS.TEMP3$, LEN(TS.TEMP3$)-THOU)+\ ins $ sep
                "," + RIGHT$(TS.TEMP3$, THOU)
  IF (LEN(TS.TEMP3$) > MILL) THEN                       \ millions
      TS.TEMP3$ = LEFT$(TS.TEMP3$, LEN(TS.TEMP3$)-MILL)+\ ins $ sep
                "," + RIGHT$(TS.TEMP3$, MILL)

End Function 
!--- Formateo de valores

Sub LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91																						! Archivo control tienda
     Read # 91 ; X.Tmp$																											! Lectura registro
     GrTienda$ = Left$(X.Tmp$,4)						    														! Numero del almacen
     Close 91																																! Cierre archivo
End Sub 																																		!
!--- Fin de la funcion de lectura

Function LEA.REG.TRANS.SUM.LOG	    																				! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$,TERM$,SLOGNAME$,OSLOGNAME$,FECHA.CIER$,RESERVED$							!
Integer*4 ClosePNT																													!
If B$ = "" Then Begin 																											!
  Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL													! Busca Eamtran Activo 
  LLAVE$ = PACK$("9998")																										!
  READ Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$,                    \!
       SLOGNAME$,OSLOGNAME$,ClosePNT,FECHA.CIER$,RESERVED$  								!
  Close 37																																	!
  If OSLOGNAME$="        "   THEN BEGIN 																		!
     MEN$ = "No Existe EAMTRAN? Previo. Procesando el Actual ... "					!
     Call ENTRADA.LOG																												!
	   B$ = SLOGNAME$ 																												!
  EndIf ELSE B$ = OSLOGNAME$ 																							  ! CARGA EAMTRAN PREVIO
EndIf 																																			!
ARCHIVO.PROC$ = B$																													!
End Function 																																!

Function APERTURA.LOG.TRX     																							! Consulta log actual de trx
String XA$, Trans.log$
Trans.Log$ = Archivo.Proc$
Men$ = "Archivo TSL [" + Trans.Log$ + "]"																		! Mensaje Tlog a procesar
Call Entrada.Log																														! Msg de alerta
Open Trans.Log$ as 12 Buffsize 1024 Unlocked Nowrite                        ! Apertura log de transacciones
End Function 																																! Fin apertura.log.trx

Sub Graba.Control																											      ! Control consecutivos
String Grcontrol$																													  ! 
Grcontrol$ = right$("000000000" + str$(Puntero%), 8)												! Nro trx procesada
Grcontrol$ = Grcontrol$ + string$(30, "0")																  ! Arma registro
Write Form "C38 C2"; #13, 1; Grcontrol$, chr$(13) + chr$(10)							  ! Almacena registro
End Sub 																																		! Fin Graba.Control

Sub Lectura.Apuntador																												! Lectura apuntador
String Grcontrol$																														!
GrError% = -1																																! Control lectura
Read Form "C40"; #13, 1; Grcontrol$																					! Lectura apuntador
If GrError% = -1 Then 																										 \! Toma dato archivo
   Puntero% = Val(Left$(Grcontrol$, 8)) Else 															 \! Toma apuntador
   Puntero% = 0																															! Inicio archivo	
End Sub																																			! Fin lectura apuntador

Sub Control.Apuntador.Trx																										! Control apuntador registro tlog
GrError% = -1																																! Control de errores
Create Posfile "ADX_UDT1:GRTRXPOS.DAT" Direct 1 Recl 40 as 13          		 \! Crea archivo apuntador
       compound perupdate																								    !

If GrError% <> -1 Then Begin																								! Error de apertura
   Men$ = "Falla Creando Apuntador Tlog"																		! Msg alerta
   Call Entrada.Log																													!
   Wait ; 1200																															!
   Stop 
Endif																																				! Fin control.apuntador.trx
Puntero% = 0																																! Reset para barrido log trx
Men$ = "Creando Apuntador Tlog"																							! Msg alerta
Call Entrada.Log																														!
Wait ; 1200																																	!
Call Graba.Control																													! Almacena puntero

End Sub 																																		! Fin Control.Apuntador.Trx

Sub Grabacion.Cierre.Tienda																									! Grabación totales de tienda
String XKey$, xs1$, xs2$, xs3$, xs4$, xs5$
GrError% = -1
Open "TF:CTLDAY" KEYED RECL 32 AS 29                           							! Abre registro cierre diario
Xkey$ = Pack$(Grtienda$ + DateClose$)																				! Llave de control
GrError% = -1

xs1$ = Pack$(Right$("0000000000"+Str$(Tgrosp%),10))
xs2$ = Pack$(Right$("0000000000"+Str$(Tgrosn%-Tmisc%),10))
xs3$ = Pack$(Right$("0000000000"+Str$(Tdscto%),10))
xs4$ = Pack$(Right$("0000000000"+Str$(Tmov%),10))
xs5$ = Pack$(Right$("0000000000"+Str$(Titms%),10))

Write FORM "C7 5C5";#29;                        						   				     \! Grabacion registro 
      Xkey$, xs1$, xs2$, xs3$, xs4$, xs5$

If GrError% = -1 Then Begin																									! Grabado correctamente
  Call Format.VALORES(Tgrosp%-TGrosn%-Tdscto%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = "Total: "+DateClose$+" "+TS.TEMP3$
  Call Entrada.Log
EndIf Else Begin																														! Si falla proceso
  Men$ = "Fail Write: "+DateClose$+" "+TS.TEMP3$
  Call Entrada.Log
EndIf 																																			! 
Close 29
End Sub 																																		! fin grabacion totales de tienda

Sub S0																																			! Cabecera de trx
  J% = 3																																		! Apuntador para extraer datos
  Item.Apnt% = 0
  GrVenta% = 0																															! Control trx venta
  Nro.fiscal$ = ""
  Call GetUnpk																															! Toma Nro Terminal
  Nro.Term$ = A$
  Call GetUnpk																															! Toma Nro Trx
  Nro.Trans$ = A$
  Call GetUnpk																															! Fecha y hora movimiento
  Fec.Hor$ = A$
  Call GetUnpk																															! Tipo de transaccion
  Tipo.Trx$ = A$  
  Call GetUnpk																															! Nro de strings
  Nro.Strings$ = A$
  Call GetUnpk																															! Operador
  Nro.Cajero$ = A$
  Call GetUnpk																															! Password
  Pwd.Cajero$ = A$
  Call GetUnpk																															! Gross +
  Grossplus$ = A$
  Call GetUnpk																															! Gross -
  GrossMinus$ = A$
  If Tipo.Trx$ = "00" Then Begin 
  	 GrVenta% = -1              
     men$ = "Procesando "+Nro.term$+" - "+Nro.Trans$+" OFFSET "+STR$(PUNTERO%)
     Call Entrada.Log																												! Procesa trx 
     Tmov% = Tmov% + 1
  EndIf
  
End Sub 																																		! Fin cabecera trx

Sub S1 																																			! Venta de Item
 J% = 3
 Call GetUnpk
 Tplu$ = A$
 Call GetUnpk
 PRECIO$ = A$																																! VLR VENTA
 Call GetUnpk
 Call GetUnpk                                                               ! Cupon Family 3 y 3 Long 6
 Call GetUnpk                                                               ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
 INDICAT1 = VAL(A$)
 If INDICAT1 AND 4000H Then PESO% = 1 Else PESO% = 0
 Call GetUnpk                                        												! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
 INDICAT2 = VAL(A$)
 Call GetUnpk                                        												! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
 INDICAT3 = VAL(A$)
 ALTO$ = LEFT$(A$,1)
 BAJO$ = RIGHT$(A$,1)
 SIGNO$ = "+"
 
 If ALTO$ = "4" Then begin 
 	  SIGNO$ = "-"
 endif
 If ALTO$ = "5" Then begin
 	  SIGNO$ = "-"
    Tmisc% = Tmisc% + Val (precio$)
 endif
 If (INDICAT2 AND 0080H) Then SIGNO$ = "-"
 If (INDICAT2 AND 0040H) Then SIGNO$ = "-"



 If SIGNO$ = "+" Then Begin
    TGrosp% = Tgrosp% + Val (precio$)
    TItms% = TItms% + 1
 EndIf Else Begin
	   TGrosn% = Tgrosn% + Val (precio$)
	   TItms% = TItms% - 1
 EndIf
 
End Sub 																																		! Fin venta Item

Sub S2																																			! Extension de Item
 J% = 3
 Call GetUnpk                 																							!  NO USADO   MPGROUP                  
 Call GetUnpk                                                               !  NO USADO   DEALQUAN                 
 Call GetUnpk                                                               !  NO USADO   PRICE METH               
 Call GetUnpk                                                               !  NO USADO   SALEQUAN                 
 Call GetUnpk                                                               !  NO USADO   SALEPRIC                 
 Call GetUnpk                                                               !  CANTIDAD O PESO VEndIDO = QTYORWGT  

 If Peso% = 0 Then begin
 	 If Signo$ = "+" Then \ 
      TItms% = TItms% - 1 Else \
  	  TItms% = TItms% + 1
   If Signo$ = "+" Then \ 
      TItms% = TItms% + Val(A$) Else \
    	TItms% = TItms% - Val(A$)
 EndIf  
  
End Sub 																																		! Fin Extension Item

Sub S3																																			! Aplicacion descuentos
  J% = 3
  Call GetUnpk     ! GRUPO DSCTO
  Call GetUnpk     ! PTG DSCTO
  Call GetUnpk     ! VALOR DSCTO
  Tdscto% = Tdscto% + Val(A$)
End Sub 																																		! Fin descuentos 

Sub S4																																			! Anulación descuentos 
  J% = 3
  Call GetUnpk     ! GRUPO DSCTO
  Call GetUnpk     ! PTG DSCTO
  Call GetUnpk     ! VALOR DSCTO
  Tdscto% = Tdscto% - Val(A$)
End Sub 																																		! Fin descuentos

Sub S5																																			! Aplicacion forma de pago
  J% = 3 
  Call GetUnpk     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)  
  Call GetUnpk     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) + Val(A$)
End Sub 																																		! Fin pago

Sub S6																																			! Anulación Forma de Pago
  J% = 3 
  Call GetUnpk     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)  
  Call GetUnpk     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) - Val(A$)
End Sub 																																		! Fin pago

Sub S9																																			! Cambio en la trx 
  J% = 3 
  Call GetUnpk     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)  
  Call GetUnpk     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) - Val(A$)
End Sub 																																		! Fin cambio trx

Sub S21																																			! Proceso cierre de dia
String XA$, Trans.Log$
Integer*4 XI%
  J% = 3 
  Call GetUnpk     																													! Fecha y hora del cierre AAMMDDhhmm
  DateClose$ = A$
  Men$ = "PROCESANDO CIERRE DIARIO..."																			!
  Call Entrada.Log																													! Msg sistema
  Close 12																																	! Cierre Tlog
  Men$ = "Actualizando Totales de Tienda..."
  Call Entrada.Log																													! Msg sistema
  
  Call Format.VALORES(Tgrosp%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Vtas Brutas     : "+TS.TEMP3$
  Call Entrada.Log

  Call Format.VALORES(TGrosn%-Tmisc%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Devol/Items     : "+TS.TEMP3$
  Call Entrada.Log

  Call Format.VALORES(Tdscto%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Descuentos      : "+TS.TEMP3$
  Call Entrada.Log

  Call Format.Valores(Tmisc%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Miscelaneos     : "+TS.TEMP3$
  Call Entrada.Log


  Men$ = "                  --------------------------------"
  Call Entrada.Log

  Call Format.VALORES(Tgrosp%-TGrosn%-Tdscto%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Total Cuadre    : "+TS.TEMP3$
  Call Entrada.Log
  Men$ = " Total Transacciones :"+Str$(Tmov%)
  Call Entrada.Log
  Men$ = " Total Items : "+Str$(TItms%)
  Call Entrada.Log
  Men$ = " "
  Call Entrada.Log
  Men$ = " REPORTE FORMAS DE PAGO "
  Call Entrada.Log
  For XI% = 11 To 70
    If PAGOS(XI%) <> 0 Then BEGIN 
    	 Call Format.VALORES(PAGOS(Xi%))
    	 Total.Pagos% = Total.Pagos% + Pagos(xi%)
    	 TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
    	 Men$ =  "Tipo Variedad "+Right$("00"+Str$(XI%),2) + " Valor : "+TS.TEMP3$
    	 Call Entrada.Log
    ENDIF 
  Next XI%
  Call Format.VALORES(Total.Pagos%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  Men$ = " Total Pagos     : "+TS.TEMP3$
  Call Entrada.Log
  Call Grabacion.Cierre.Tienda
  Stop     
End Sub 

Sub S99																																			! User data
  J% = 3																																		! Apuntador para extraer datos
  Call GetUnpk																															! Codigo proyecto
End Sub 																																		! Fin user data


Sub Recorre.Tlog																														! Barre Tlog
Integer*4 Q%, P%, A%
  Dim PAGOS(100) 
  FinTlog% = -1
  While FinTlog% = -1																												! Ciclo Infinito
   Lectura.Tlog:
    Read #12; Line Str.Log$																									! Lectura Tlog
    Puntero% = PTRRTN																												! Actualiza posicion Tlog
    Call Graba.Control																											! Actualiza apuntador
    If Len(Str.Log$) < 12 Then GoTo Lectura.Tlog														! Si basura en Tlog
    Str.Log$ = Str.Log$ + ","																								!
    Q% = 1 																																	! Reset Variable
    While (Q% < Len(Str.Log$))																							! Validar si String completos
      P% = Match(",",Str.Log$,Q%) 																					! Encontrar delimitador coma (,)
      If (P% - Q%) < 3 Then Begin 																					! Revisar falta de strings
	      GoTo Lectura.Tlog		   																							! Lectura siguiente registro
	    EndIf																																	!
      B$ = Mid$(Str.Log$,Q%+1,(P%-Q%)-2) 																		! Analiza cadena de String
      B$ = B$ + ":" 																												! Suma un semi colon
      Q% = P% + 1 																													! Prepara siguiente informacion
      A% = Val(Unpack$(Left$(B$,1))) 																				! Determina tipo de String
      If A% =  0 Then Call  S0																							! Cabecera de trx
      If A% =  1 Then Call  S1																							! Venta de Item
      If A% =  2 Then Call  S2																							! Extension de Item
      If A% =  3 Then Call  S3																							! Descuentos
      If A% =  4 Then Call  S4																							! Anulacion descuentos
      If A% =  5 Then Call  S5																							! Pagos
      If A% =  6 Then Call  S6																							! Anulacion pagos
      If A% =  9 Then Call  S9																							! Cambio en la trx
      If A% = 21 Then Call  S21																						  ! Cierre de dia
      If A% = 99 Then Call  S99																						  ! User data
    Wend 																																		! Fin analisis tlog
  Wend 																																			! Fin del ciclo
End Sub 																																		! Fin recorre Tlog

!--- Bloque principal
On Error GoTo Err.Trx
Estado.Ejec% = 0																														! Modo ejecucion interactivo
Gtmp$ = COMMAND$                                             								! Dato S.O
If UCase$(Gtmp$) = "VERSION" Then Begin																			! Consulta Version
   !Print "Control Totales Cierre TrxOnline Ver 1.0 30/Nov/2021 09:45 am"	  !
   Print "Control Totales Cierre TrxOnline Ver 1.1 20/Ene/2021 12:10 pm"	  !
   Stop 
EndIf
If Gtmp$ = "BACKGRND" Then B$ = "" Else B$ = Gtmp$                          ! Log trx a procesar
If Gtmp$ = "BACKGRND" Then Estado.Ejec% = -1																! Ejecucion Background
Call Leer.Cabecera																													! Lectura archivo control	
Call Lea.Reg.Trans.Sum.Log																									! Seleccion Tlog	
Call Apertura.Log.Trx																												! Apertura Log de trx
Call Control.Apuntador.Trx																									! Apertura contador trx
Call Lectura.Apuntador 																											! Toma posicion Tlog
Point 12; Puntero%																													!	Toma Offset Tlog
MEN$ = "Procesando Log Transacciones..."
Call Entrada.Log
Call Recorre.Tlog																														! Recorrer Tlog
Stop 																																				! Fin de programa

Err.Trx:
GrError% = 0																																! Se reporta Error en Appl

If ERRF% = 12 AND (ERR = "EF") Then Begin																		! Fin Archivo Tlog
   FinTlog% = 0
   Resume 
EndIf																																			  !

If ERR = "EF" Then Resume 																									! Error lectura
If ERR = "IH" Then Resume 																									! Error basura
If ERRF% = 11 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Error: APERTURA ESTADO DE TERMINALES "														! Msg de Error
    Call Entrada.Log																												! Presentacion mensaje
    Stop 																																		! Fin programa
EndIf																																			  !
If ERRF% = 12 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Error: APERTURA LOG DE TRANSACCIONES "														! Msg de Error
    Call Entrada.Log																												! Presentacion mensaje
    Stop 																																		! Fin programa
EndIf																																			  !
If ERRF% = 13 AND (ERR = "OE" OR ERR = "FU") Then Begin											! Error apertura eamterms
    MEN$="Err: Falla Creacion Apuntador "       
    Call Entrada.Log																												! Presentacion mensaje
    WAIT ; 1000
    Resume 																																	! Retorna ejecucion programa
EndIf																																			  !
If ERRF% = 29 AND                              							   	           \! Validacion si existe 
  (ERR = "OE" OR ERR = "FU") THEN BEGIN            									        ! el archivo de control
    CREATE POSFILE "TF:CTLDAY" KEYED 7,,,10000 RECL 32                     \! si no existe lo crea.
           As 29 compound perupdate 		                                    ! 
    Resume                                         										      ! retorna ejecucion
ENDIF                                             										      ! del programa

MEN$ = "Error de Aplicacion "+ERR+" Sesion "+Str$(ERRF%) 
Call Entrada.Log																														! Presentacion mensaje

Stop 																																				! Fin de programa

