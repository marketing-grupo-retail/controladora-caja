!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INMVDNTX.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Movimiento Dia Sin IVA *
!**************************************************
! Modificaciones:
! Version 1.0 18/Jun/2020 - Grupo Retail - OVS
!--------------------------------------------------
! Mod 7 Jul 2020
! Se adiciona a la lista las formas de pago capturadas
! el codigo del dpto nuevo asignado y numeros de 
! autorizacion de TEF.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------
! Mod 11Nov2021
! Se reportan todos los items de la compra realizada
! dentro de los tiquetes beneficiados con descuento
! de iva.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------
! Mod 16Dic2021
! Se controla la grabación de la cantidad de no 
! generar caracteres raros (=) como dato negativo
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------

%ENVIRON C						   												! Ambiente de controlador

String    Global Ue.Salida$, Ue.Data2$, DATO.SO$
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$
String    Global Cabecera$, Item$  , NRO.FACTURA$, UE.TMP1$, Udata$(1), \
                 Price1$ , Price2$, Dpto$, Struc$, Compra$, Cliente$, Doc$, Track$, \
                 Ue.Plan$, Pagos$(2), Gfg$, NRO.CUENTA2$, Umed$, Udesc$, Sgn$, \
                 IR.USERDATA$, Proc$
Integer*1 Global ARC%, IndGrab%
Integer*4 Global PP, UE.LINEAS%, TOT.MOV%, Nreg%, X%, Xtot%, Xnpg%, Y%, Price1%

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! Maestro Articulos
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion


Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   INTEGER*4 RETC
   String INFILE, OUTFILE
   INTEGER*2 OPT0, OPT1
End Sub


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

Function INICIO1
  Call.ORDER% = 1                                 ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Function ENTRADA.LOG
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Locate 15,1: Print MEN$
End Function 

Function BARRA.ESTADO
  INTEGER*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+String$(N,CHR$(219))+String$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"
End Function 

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub Cabecera.Reporte
  UE.DATA2$ = "1"             + \! Caracter Fijo
              FECMOV$         + \! Fecha generacion archivo
              DM.ALMACEN$        ! Centro de Costo
  Call Write.Data
End Sub 
!---

Sub Grabacion.Interface
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      UE.LINEAS% = UE.LINEAS% + 1
      Call Write.Data
End Sub
!--- Fin grabacion de interface


Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 

Function TERMINE.PROG
String XTEMP1$, XTEMP3$
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
 Else Begin
  Locate 12,15: Print MEN$
 Endif
  MEN$ = "INTERFACE "+Ue.Salida$
  Call ENTRADA.LOG
  Call SALIDA.PROG  
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     FECMOV$     = MID$(X.TMP$,5,8)              ! Fecha de movimiento
     Close 91
End Function
!--- Fin de la funcion de lectura

Function INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
  NRO.ERROR      = 0
  BARRA%         = 1
  TOTAL.REG      = 0
  CLIENTE.FREC   = 0
  ACUMULADOR%    = 0
  TRANS.AUTONOMO = 0
  TRANS.NORMAL   = 0
  TOT.DEPTOS     = 0
  NETMSC         = 0
  TOT.TERM       = 0
  TERM.INI       = 0
  NUM.SEP$       = ","
  PRIMERA.VEZ    = 1
  UE.LINEAS%     = 0
  IndGrab%       = 0
  Ue.Plan$       = ";11;15;41;51;34;"
  Dim Udata$(500)
  Dim Pagos$(100,2)
  
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		    :CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		  :LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
End Function 

Function INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q                 = 1
NRO.StringS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero 
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 XX%
  XX% = 0
  If XX% = 0 Then Begin                                          ! Si copia OK
     OPEN B$ AS TLOG NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     B$ = ""
  EndIf Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     Stop
  EndIf
  Open "EAMITEMR" KEYED RECL 77 AS 4
  
End Function 

Function LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$
Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL	! Busca Eamtran Activo 
LLAVE$ = PACK$("9998")
READ Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,ClosePNT,FECHA.CIER$,RESERVED$  
Close 37
If DM.CODIGO$ = "1" THEN B$ = SLOGNAME$
If DM.CODIGO$ = "2" THEN BEGIN 
   If OSLOGNAME$="        "   THEN BEGIN 
  	  MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
      Call ENTRADA.LOG
      WAIT;800
	    B$ = SLOGNAME$ 
   EndIf ELSE B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
ENDIF
TOT.TAMANO = SIZE(B$)
ARCHIVO.PROC$ = B$
FEND

Function PANTALLA.PRINCIPAL
Call INICIADM                                               ! Inicializacion Variables Display Manager
CAMPO% = 0
AREA1% = 11: AREA2% = 4 : AREA3% = 21                                       ! Definicion area de trabajo archivo
Call LEER.CABECERA
DM.CODIGO$ = "2"
DATO.SO$ = COMMAND$                                             ! Dato S.O

If Ucase$(Dato.So$) = "VERSION" Then Begin
   !Print "Interface Movimiento Dia sin IVA Ver. 1.0 18/Jun/2020"
   !Print "Interface Movimiento Dia sin IVA Ver. 1.1 07/Jul/2020"
   !Print "Interface Movimiento Dia sin IVA Ver. 1.3 02/Oct/2021"
   !Print "Interface Movimiento Dia sin IVA Ver. 1.4 09/Dic/2021"
   Print "Interface Movimiento Dia sin IVA Ver. 1.5 16/Dic/2021"
   Stop 
EndIf

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "****      INTERFACE MOVIMIENTO DIA SIN IVA          ****"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software Diciembre 16 2021 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 16/Dic/2021 G.R...."
  Call Entrada.Log
EndIf

If LEN(B$) = 0 Then Begin                                       ! Dia previo
  Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO
Endif
If LEN(B$) = 1 Then Begin
  B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT"                         ! Tomo log capturado
  FECHA.CIER$ = DATE$                                          ! Asigno fecha dia
Endif
FEnd 

Function CUENTE.StringS	       	! Esta funcion cuenta primero el #String y lo
INTEGER*2 POS1%,CUENTE.StringS	! compara con el #reportado por SMA
String CARACTER
 POS1% = 1
 NRO.StringS.FOUND = 0
 CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
 WHILE (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.StringS.FOUND = NRO.StringS.FOUND + 1
 WEnd
 NRO.StringS.FOUND = NRO.StringS.FOUND - 1
 If (NRO.StringS.FOUND < NRO.REG) Then Begin
	MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND > NRO.REG) Then Begin
	MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND = NRO.REG) Then CUENTE.StringS = 1	\ Retorna 1 si es
 Else CUENTE.StringS = 0				! satisfactorio el resultado y 0
End Function						! si el resultado es fallido.

Sub GRABE.IDENTIfICADOR
 String C$, DINNER.POS$, DINNER.NEG$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0
 TOTAL.REG = TOTAL.REG + 1
 Locate 20,25
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
 Else Begin
  Locate 12,15: Print MEN$
 Endif

End Sub

Function ERRNSTR$(ERRNUM)
Integer*1 I
Integer*4 ERRNUM,WORK
String HEX$,ERRNSTR$,WORK$
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
    For I = 1 TO 8
      WORK   = ERRNUM AND 0000000FH         ! AND OFF ALL BUT LOW NYBBLE
      WORK$  = MID$(HEX$,WORK+1,1)+WORK$    ! ADD HEX VALUE TO OUTPUT String
      ERRNUM = SHIfT(ERRNUM,4)              ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$                          ! Return Error Code
End Function 

Function CALCULO.HORA
String A$, H$, M$, S$
A$ = TIME$							    !
H$ = LEFT$(A$,2)						! Tomo hora
M$ = MID$(A$,3,2)						! Tomo minutos
S$ = RIGHT$(A$,2)						! Tomo segundos
M$ = STR$(VAL(M$) + 10)			! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)			!
If VAL(M$) > 59 Then Begin 	!
   M$ = STR$(VAL(M$) - 60)	!
   M$ = RIGHT$("00"+M$,2)		!
   H$ = STR$(VAL(H$) + 1)		!
   If VAL(H$) > 23 Then H$ = "00"	 !
EndIf								               !
HORA.FINAL$ =  H$+M$+"00"					 ! Hora de Inicio 
End Function

!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call LEER.CABECERA
  ARC% = 0
  CALL INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$  
!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Q                 = 1
  NRO.STRINGS.FOUND = 0
  NRO.ARTICULOS     = 0
  NO.VENTA          = 0
  SW.MISC           = 0
  DEPTO%            = 0
  T%                = 0
  PROCESO$ = " PROCESO PRINCIPAL "
  READ #TLOG; LINE INAREA$
  IF END #TLOG THEN REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  IF LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
	  GOTO NXTRCD
  ENDIF
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))	! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 	! ENCONTRAR DELIMITADOR, COMA
    IF (P-Q) < 3 THEN \ 	! REVISAR FALTA DE STRING
    	BEGIN
	  P=0
	  MEN$ ="FALTA STRING ID TRANSACCION"
	  !CALL ENTRADA.LOG
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. String
	  NO.VENTA = -1
	  GoTo AGAIN		   	! VUELVA A LEER OTRO REGISTRO
	ENDIF
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 	! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
    B$ = B$+":" 			! SUMA UN SEMI-COLON
    Q = P + 1 				! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 	! DETERMINA TIPO DE STRING
    If A = 0 THEN GoSub S0:GoTo AGAIN	! SI ES CHECKOUT TRANSACCION
    If A = 99 THEN GoSub S99:GoTo AGAIN
    If (A < 0) OR (A > 21) THEN GOTO AGAIN
    If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) THEN \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
    If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
		   CANT.PESO$ = "000000000" 
       SW.ITEM = 0
    EndIf
    If (A > 3) AND (SW.ITEM = 1) THEN \	!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
          	SW.ITEM = 0
     EndIf
     
    On A GoSub S1,S2,S3,S3,S5,S6,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  Proc$ = "S0"
  J = 3
  If IndGrab% = -1 Then Begin
     CABECERA$ = "2"																							       + \! Tipo de registro
                 Doc$                                                    + \! Tipo de documento valor fijo "CC  "
                 Cliente$                                                + \! ID del cliente
                 Right$("0000"+DM.ALMACEN$,4)            								 + \! Almacen
                 Right$("0000"+TERMINAL$,4)              								 + \! Numero de terminal
                 Right$("0000"+NRO.TRANS$,4)             								 + \! Numero de transaccion
                 Right$("0000000000"+COD.OPERA$,10)      								 + \! Codigo del cajero
                 FECMOV$                            								     + \! Fecha de la operacion
                 HORA$                                                      ! Hora de la transaccion          
  	 For X% = 1 To Nreg%																										! Total Items
  	     Ue.Data2$ = CABECERA$ + Udata$(x%) + NRO.FACTURA$       						!
  	     Call Grabacion.Interface																						!
  	     IndGrab% = -2
  	 Next X%

    CABECERA$ = "3"																							         + \! Tipo de registro
                 Doc$                                                    + \! Tipo de documento valor fijo "CC  "
                 Cliente$                                                + \! ID del cliente
                 Right$("0000"+DM.ALMACEN$,4)            								 + \! Almacen
                 Right$("0000"+TERMINAL$,4)              								 + \! Numero de terminal
                 Right$("0000"+NRO.TRANS$,4)             								 + \! Numero de transaccion
                 Right$("0000000000"+COD.OPERA$,10)      								 + \! Codigo del cajero
                 FECMOV$                            								     + \! Fecha de la operacion
                 HORA$                                                      ! Hora de la transaccion          
   If IndGrab% = -2 Then Begin
    For Y% = 1 To Xnpg%																											! Total formas de pago
     If Val(Pagos$(Y%,1)) <> 0 Then Begin
     	If Val(Pagos$(Y%,1)) >= 0 Then \
     	   Doc$ = "+" Else Doc$ = "-"
      Gfg$ = Right$("00"+Pagos$(Y%,0),2) +                                 \! Arma informacion del pago
             Right$("      "+Pagos$(Y%,2),6) +														 \! forma de pago, nro autor. 
             Right$("0000000000"+Pagos$(Y%,1),10)+												 \! y monto
             Doc$																														! Signo operacion     
      Ue.Data2$ = CABECERA$ + NRO.FACTURA$ + Gfg$						                !
      Ue.Data2$ = Left$(Ue.Data2$+String$(140," "),140)
      Call Grabacion.Interface																						  !
     EndIf
  	Next Y%
   EndIf

  EndIf

  Nreg% = 0 : Xnpg% = 0
  TRACK$ = ""
  NRO.FACTURA$ = "NO DEFINIDO"
  Dim Udata$(500)
  Dim Pagos$(100,2)
  IndGrab% = 0
  GoSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GoSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:DI"+RIGHT$(FECMOV$,6)+"."+Right$(DM.ALMACEN$,3)
     Create Ue.Salida$ AS 51 
     Call Cabecera.Reporte
  EndIf
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  GoSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans = 17 Then GOTO NXTRCD
  GoSub GETUNPK
  NRO.REG=VAL(A$)  		        											! NUMERO DE StringS EN TODA LA TRANSACCION
  StringS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 											! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
		NO.VENTA = 1
  EndIf Else	\
	StringS.COMPLETOS = CUENTE.StringS								! FUNCION PARA CORROBORAR INTEGRIDAD DE StringS
  GoSub GETUNPK
  COD.OPERA$=A$  																		! NUMERO DEL OPERADOR
  GoSub GETUNPK 													  			  ! PASSWORD DEL OPERADOR
  GoSub GETUNPK																			! Venta positiva
  GROSS.POSITIVO$=A$
  GoSub GETUNPK																			! Venta negativa
  GROSS.NEGATIVO$=A$
  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	   TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD									! 
  EndIf
  TOT.MOV% = TOT.MOV% + 1
                    
Return

S1:
  J = 3
Return

S2:
 J = 3 		        ! EXTENSION DE ENTRADA
Return

S3:
  J = 3
Return

S5:
 Proc$ = "S5"
 J = 3
 If tipo.trans = 0 Then begin
  GOSUB GETUNPK 			                                                      !  FORMA O TIPO DE PAGO, TENDTYPE           
  FORMA.PAGO$=A$                                                            !  Tipo de pago                             
  GOSUB GETUNPK 			                                                      !  CANTIDAD DE PAGO,  AMTTENDE              
  VALOR.PAGO$ = A$                                                          !  Valor del pago                           
  GOSUB GETUNPK 			                                                      !  NO USADO   CANTIDAD TENDER FEE           
  GOSUB GETUNPK 			                                                      !  NUMERO DE LA CUENTA DEL CLIENTE CUST NUM 
  NRO.CUENTA2$ = A$
  NRO.CUENTA$ = NRO.CUENTA2$
  TS.TEMP3$ = ";"+FORMA.PAGO$+";"
  If Match(TS.TEMP3$,Ue.Plan$,1) > 0 Then Begin
  	 If TRACK$ <> "" Then Begin
  	 	  NRO.CUENTA$ = MID$(TRACK$,33,6)                                     ! Numero Autorizacion - String TEF 
  	 EndIf Else Begin                                                                                           
  	    NRO.CUENTA$ = NRO.CUENTA2$										                   	  ! Transaccion manual                 
  	 EndIf
  	 Xnpg% = Xnpg% + 1
  	 Pagos$(Xnpg%,0) = FORMA.PAGO$
  	 Pagos$(Xnpg%,1) = VALOR.PAGO$
  	 Pagos$(Xnpg%,2) = NRO.CUENTA$
  EndIf
 endif 
Return

S6:
 Proc$ = "S6"
 J = 3
 If tipo.trans = 0 Then begin
  GOSUB GETUNPK 			                                                      !  FORMA O TIPO DE PAGO, TENDTYPE           
  FORMA.PAGO$=A$                                                            !  Tipo de pago                             
  GOSUB GETUNPK 			                                                      !  CANTIDAD DE PAGO,  AMTTENDE              
  VALOR.PAGO$ = A$                                                          !  Valor del pago                           
  GOSUB GETUNPK 			                                                      !  NO USADO   CANTIDAD TENDER FEE           
  GOSUB GETUNPK 			                                                      !  NUMERO DE LA CUENTA DEL CLIENTE CUST NUM 
  NRO.CUENTA2$ = A$
  NRO.CUENTA$ = NRO.CUENTA2$
  TS.TEMP3$ = ";"+FORMA.PAGO$+";"
  If Match(TS.TEMP3$,Ue.Plan$,1) > 0 Then Begin
  	 If TRACK$ <> "" Then Begin
  	 	  NRO.CUENTA$ = MID$(TRACK$,33,6)                                     ! Numero Autorizacion - String TEF 
  	 EndIf Else Begin                                                                                           
  	    NRO.CUENTA$ = NRO.CUENTA2$										                   	  ! Transaccion manual                 
  	 EndIf
  	 X% = 0
  	 For Y% = 1 To Xnpg%
  	  If Pagos$(Y%,0) = FORMA.PAGO$ And Pagos$(Y%,2) = NRO.CUENTA$ Then Begin
  	  	 Pagos$(Y%,1) = Str$( Val(Pagos$(Y%,1)) - Val(VALOR.PAGO$) )
  	  EndIf
  	 Next Y% 
  EndIf
 EndIf
Return


S7:
  J = 3!TAX
RETURN

S9:
  J = 3
Return

S10:
  J = 3
RETURN

S11:
  J = 3!DTENTRY
Return

S12:
  J = 3!CHANGE PRICE
RETURN

S13:
  J = 3!LOAN-PICKUP
RETURN

S14:
  J = 3
RETURN

S15:
  J = 3
RETURN

S16:
RETURN

S20:
  J = 3
RETURN

S21:
  J = 3
Return

S99:
  Proc$ = "S99"
  J = 3
  GoSub GETUNPK   																														! Codigo UD
  If A$ = "42" Then Begin                                             				! Dia Sin iva lista items factura
  	 GoSub GETUNPK                                        										! Plu del producto
  	 Item$ = A$
  	 GoSub GETUNPK                                        										! Precio total venta
  	 Price1$ = Translate$(A$,"=","-")
  	 GoSub GETUNPK																														! Cantidad vendida
  	 cantidad$ = Translate$(A$,"=","-")
  	 GoSub GETUNPK                                        										! Tasa de impuesto
  	 GoSub GETUNPK                                        										! excento/excluido
  	 GoSub GETUNPK                                        										! Impocunsumo
  	 GoSub GETUNPK4                                        										! unitario pesado
  	 Umed$ = A$

!  	 GoSub GETUNPK4                                        										! descripcion Item
!     UDesc$ = A$  	 

  	 GoSub GETUNPK                                        										! seccion de venta
  	 Depto$ = A$
  	 GoSub GETUNPK                                        										! dscto aplicado
  	 GoSub GETUNPK                                        										! signo operacion
     Sgn$ = A$  	 

  	 Compra$   = Str$(Val(Price1$))
     If Sgn$ = "01" Then Compra$ = Str$( Val(Compra$) * -1)
     If Umed$ = "UD" Then Begin 
     	  If Val(cantidad$) > 0 Then Begin
     	     Price1% = ( Val(compra$) / Val(Cantidad$) ) 
     	     Price1$ = Str$(Price1%)
     	  Endif Else \
     		   Price1$ = "0"
     	  Price2$ = Price1$
     EndIf Else Begin
     	 If Val(cantidad$) > 0 Then Begin
     	    Price1% = ( (Val(compra$) / Val(Cantidad$))*1000 ) 
     	    Price1$ = Str$(Price1%)
     	 Endif Else \
     	    Price1$ = "0"
     	  Price2$ = Price1$
     EndIf

  	 
     Item$     = Right$("0000000"+Item$,7)
     Cantidad$ = Right$("0000"+Cantidad$,4)
     Depto$    = Right$("000"+Depto$,3)
     Price1$   = Right$("00000000000"+Price1$,11)
     Price2$   = Right$("00000000000"+Price2$,11)
     Compra$   = Right$("00000000000"+Compra$,11)

     BAN.PRG$ = "0"
     IR.ITEMCODE$ = Pack$(Right$("000000000000"+ITEM$,12))
     %INCLUDE EAMIRRD1.J86
     If BAN.PRG$ = "0" Then \
     	Struc$ = LEFT$(IR.USERDATA$,16) Else \ 
      Struc$ = "9999999999999999"
     Struc$ = Right$(Struc$,15)
     UE.DATA2$ = ITEM$                       + \! Codigo del Item Vendido   Asc 07
                 Struc$                      + \! Estructura comercial      Asc 15
                 Cantidad$                   + \! Cantidad Vendida          Asc 04
                 Price1$                     + \! Precio de venta comercial Asc 11
                 Price2$                     + \! Precio de venta No IVA    Asc 11
                 Compra$                     + \! Valor total compra No IVA Asc 11
                 Depto$                         ! Depto nuevo asignado      Asc 03
                             
 	   Nreg% = Nreg% + 1
     Udata$(Nreg%) = Ue.Data2$
     IndGrab% = -1
  	 
  EndIf
  If A$ = "2020061799" Then Begin                                             ! Dia Sin iva se adiciona 99 para no ingreso
     GoSub GETUNPK                                        										! Plu del producto
     Item$ = A$
     GoSub GETUNPK                                        										! Cantidad beneficiada
     cantidad$ = A$
     Cantidad$ = Translate$(A$,"=","-")
     GoSub GETUNPK                                        										! precio venta original
     Price1$ = A$
     Price1$ = Translate$(A$,"=","-")
     GoSub GETUNPK                                        										! precio venta unitario
     Price2$ = A$
     Price2$ = Translate$(A$,"=","-")
     GoSub GETUNPK                                        										! dpto reportado
     Dpto$ = A$
     GoSub GETUNPK                                        										! Estructura comercial
     Struc$ = A$
     Xtot%     = Val(Cantidad$) * Val(Price2$)
     Compra$   = Str$(Xtot%)
     Item$     = Right$("0000000"+Item$,7)
     Cantidad$ = Right$("0000"+Cantidad$,4)
     Depto$    = Right$("000"+Depto$,3)
     Price1$   = Right$("00000000000"+Price1$,11)
     Price2$   = Right$("00000000000"+Price2$,11)
     Struc$    = Right$("000000000000000"+Struc$,15)
     Compra$   = Right$("00000000000"+Compra$,11)
   If Struc$ <> "999999999999999" Then Begin
     UE.DATA2$ = ITEM$                       + \! Codigo del Item Vendido   Asc 07
                 Struc$                      + \! Estructura comercial      Asc 15
                 Cantidad$                   + \! Cantidad Vendida          Asc 04
                 Price1$                     + \! Precio de venta comercial Asc 11
                 Price2$                     + \! Precio de venta No IVA    Asc 11
                 Compra$                     + \! Valor total compra No IVA Asc 11
                 Dpto$                          ! Depto nuevo asignado      Asc 03
                 
 	   Nreg% = Nreg% + 1
     Udata$(Nreg%) = Ue.Data2$
   EndIf
  EndIf																																				!
  If A$ = "20040608" Then Begin         																			! Lealtad de clientes Online
  	 GoSub GETUNPK 																														! ID DEL CLIENTE
  	 Cliente$ = A$
  	 Cliente$ = Right$("00000000000000000"+Cliente$,17)
  	 Doc$ = "CC  "
  EndIf
  If A$ = "21" AND                                                           \! Str Cierre/Trans
     MID$(B$,5,2) = "TR" Then Begin                          									! Ident TransVta
     J = 12                                               										!
     GoSub GETUNPK                                        										! Desempaq sig UPD
     A$ = Str$(Val(A$))
     UE.TMP1$ = RIGHT$(STRING$(7," ")+A$,7)               										! Numero de la factura
     J = 11                                               										!
     GoSub GETUNPK                                        										! Desempaq sig UPD
     NRO.FACTURA$ = Mid$(B$,8,4)+UE.TMP1$              							  				! Prefijo de la numeracion
     NRO.FACTURA$ = Left$(NRO.FACTURA$+String$(20," "),20)                    ! Ajusta dato factura
  EndIf 
  If A$ = "51" Then begin 																									  ! UD de Tef 
  	 GOSUB GETUNPK4																													  ! String de TEF
     Track$ = A$
  EndIf					
  
Return
    
GETEXCPT:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If ((K-J)-1) > 0 Then \ ! If DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  Else \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BEGINNING OF NEXT FIELD
RETURN
     
!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************

!*********************************************************************
GETUNPK3:
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************
GETUNPK4:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = (MID$(B$,J,K-J)) ! FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN

!*********************************************************************
GETFLAG:
  FLAG = VAL(A$) ! CONVERT FLAG TO INTEGER
                 ! START BUILDING String TO CONTAIN INDIVIDUAL FLAGS
  If (FLAG AND 00000001H) Then A$ = "1" Else A$ = "0"
  FOR I = 1 TO 15
    FLAG = SHIfT(FLAG,1) ! SET UP NEXT BIT
    If (FLAG AND 00000001H) Then A$ = "1" + A$ Else A$ = "0" + A$
  NEXT I
RETURN

!*********************************************************************
!
REPORTE.FINAL:
        Call TERMINE.PROG
RETURN
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
If ERR = "SS" Then RESUME NXTRCD:
If ERR = "IH" Then RESUME 

If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 
If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call ENTRADA.LOG
    Stop 
EndIf 

If ERRF% = 19 AND (ERR = "OE" OR ERR = "FU") Then Begin
   BAN.PRG$ = "1"
   RESUME        
EndIf 

If ERRF% = 19 AND (ERR = "EF") Then Begin
   BAN.PRG$ = "1"
   RESUME        
EndIf 
If ERRF% = 50 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Numeracion Fiscal "
    Call ENTRADA.LOG
    Stop 
EndIf 

If ERRF% = 50 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf

If ERRF% = 55 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Convenios"
    Call ENTRADA.LOG
    Stop 
EndIf 

If ERRF% = 55 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf


If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ENTRADA.LOG
    Stop 
 EndIf
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ENTRADA.LOG
     Call SALIDA.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$ + " Proc:"+Proc$
Print MEN$
STOP
!*********************************************************************
