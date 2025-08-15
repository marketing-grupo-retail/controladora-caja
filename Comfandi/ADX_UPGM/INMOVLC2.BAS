!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INMOVLC2.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Lealtad de Clientes    *
!                reproceso movimientos lealtad    *
!**************************************************
! Mod 14Feb2011
! Se ajusta modulo para retirar ceros ajustados 
! a la izquierda en el valor y numero de puntos 
! acumulados. Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 07Jun2022
! Se ajusta interface para reporte de datos 
! a sistema Leal
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, XEAN$, Xred$, \
                 XTotales$, XTotRed$, U.File$
Integer*1 Global ARC%, INDT%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%
String    Global Ue.Ip$, Ue.User$, Ue.Passwd$, Ue.Ruta$

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

Sub CARGA.PARAMETROS																												! Parametros del producto
Integer*1 XLISTO%
String    XTmp$
  BAN.PRG$ = "0"
  Open "TF:CFGLCL" AS 38																										! Apertura parametros
  If End #38 Then SALIDA.PARAMETROS																					! Si EOF
  XLISTO% = 0																																! Ctrl Ciclo
  While (1)																																	!
     Read #38; Xtmp$		
     Read #38; Xtmp$			! Direccion IP
     UE.IP$ = Mid$(XTMP$,30,38)	
     Read #38; Xtmp$			! Usuario FTP
     UE.USER$ = Mid$(XTMP$,30,38)    
     Read #38; Xtmp$		        ! Password
     UE.PASSWD$ = Mid$(XTMP$,30,38)     
     Read #38; Xtmp$                    ! Ruta destino archivo
     UE.RUTA$ = Mid$(XTMP$,30,50)
     XLISTO% = -1
     GoTo SALIDA.PARAMETROS
  Wend 
  SALIDA.PARAMETROS:
    If Xlisto% = 0 Then Begin			    																			! Si parametros incorrectos
     MEN$ = "ERROR: Archivo Parametros Incorrecto "													!
     Call ENTRADA.LOG																												!
     Stop 																																	!
    EndIf 																																	!
    Close 38																																!
End Sub 

Sub Armar.File.Trx
String Lec$, Buffer$
Integer*4 X.Len%

Create "ADX_SDT1:ADXHSIGF.DAT" AS 11                      ! Borra basura existente 
Close 11                                                  !

Create "ADX_UDT1:ENVIO.TXT" AS 3
  
 buffer$ = "open "+UE.IP$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "prompt"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "bin"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "user "+UE.USER$+" "+UE.PASSWD$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "lcd .."+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "lcd adx_udt1"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "cd "+UE.RUTA$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 
 
 buffer$ = "put "+Right$(Ue.Salida$,12)+ " " +finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "close "+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "by"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                             ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  				   ! Grabacion del registro 

 Close 3
 Wait ; 1200
 Print "Transfiriendo Archivo "
 Call OSSHELL("TYPE ADX_UDT1:ENVIO.TXT | ftp > adx_udt1:rta.txt") 					! Ejecuta el FTP
 Wait ; 1200 																																! 

End Sub 
!--- 



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
  Close 51 : Wait ; 500
  Call Armar.File.Trx
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
  OLD.TRX$       = "XXX"
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
  Ctrl.Trx$      = ""
  VTAS.TOTALES%  = 0 
  UE.LINEAS%     = 0
  Cnt.Reg%       = 0   ! Contador de registros
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
Dim RECIBOS$(100,2)

CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
Cnt.Reg%       = 0   ! Contador de registros
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
     OPEN B$ AS TLOG NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     FECMOV$ = "20" + MID$(B$,12,6)              								! Fecha de movimiento
     B$ = ""
  EndIf Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     STOP
  EndIf
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
If Ucase$(Dato.So$) = "VERSION" Then BEGIN
   !Print "Interface Lealtad de Clientes Version 19/Jun/2018 4:08 pm"
   !Print "Interface Lealtad de Clientes Version 07/Jun/2022 11:08 am"
   !Print "Interface Lealtad de Clientes Version 15/Jul/2022 03:00 pm"
    Print "Interface Lealtad de Clientes Version 22/Dic/2022 04:00 pm"
   Stop 
EndIf
If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "**** INTERFACE PUNTAJE LEALTAD DE CLIENTES ONLINE   ****"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software Diciembre 22  2022 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 22/Dic/2022 G.R...."
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
  Call CARGA.PARAMETROS
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
	  NO.VENTA = -1
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. STRING
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
     
    On A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GOSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GOSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:LC"+RIGHT$(FECMOV$,6)+"."+Right$(DM.ALMACEN$,3)
     Create Ue.Salida$ AS 51 
     Call Cabecera.Reporte
  EndIf
  HORA$ = RIGHT$(A$,4)    	        								! CAPTURA HORA DE LA TRANSACCION
  GOSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans = 17 Then GOTO NXTRCD
  If TIPO.TRANS = 2 Then X.CAMBIO% = -1 
  GOSub GETUNPK
  NRO.REG=VAL(A$)  		        											! NUMERO DE StringS EN TODA LA TRANSACCION
  StringS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 											! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
		NO.VENTA = 1
  EndIf Else	\
	StringS.COMPLETOS = CUENTE.StringS								! FUNCION PARA CORROBORAR INTEGRIDAD DE StringS
  GOSub GETUNPK
  COD.OPERA$=A$  																		! NUMERO DEL OPERADOR
  GoSub GETUNPK 													  			  ! PASSWORD DEL OPERADOR
  GOSub GETUNPK																			! Venta positiva
  GROSS.POSITIVO$=A$
  GOSub GETUNPK																			! Venta negativa
  GROSS.NEGATIVO$=A$
  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD									! 
  EndIf
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
 J = 3 
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
  J = 3
  GoSub GETUNPK 																															! NUMERO DE PROYECTO 
  If A$ = "20040608" Then Begin         																			! Lealtad de clientes Online
  	 GoSub GETUNPK 																														! ID DEL CLIENTE
  	 XEAN$ = A$
  	 GoSub GETUNPK 																														! PUNTOS ACUMULADOS
     NRO.CUENTA$ = A$
     NRO.CUENTA$ = TRANSLATE$(NRO.CUENTA$,"=","-")
     NRO.CUENTA$ = Str$(Val(NRO.CUENTA$))                                     !
  	 GoSub GETUNPK 																														! PUNTOS REDIMIDOS
     Xred$ = A$
     XRed$ = TRANSLATE$(XRed$,"=","-")
     Xred$ = Str$(Val(Xred$))
  	 GoSub GETUNPK 																														! VALOR DE LA COMPRA
  	 VALOR.PAGO$ = A$
  	 VALOR.PAGO$ = TRANSLATE$(VALOR.PAGO$,"=","-")
  	 VALOR.PAGO$ = Str$(Val(VALOR.PAGO$))
  	 GoSub GETUNPK 																														! Puntos Totales inicio trx
  	 XTotales$ = A$
  	 XTotales$ = TRANSLATE$(XTotales$,"=","-")
  	 XTotales$ = Str$(Val(Xtotales$))
  	 GoSub GETUNPK 																														! Puntos totales redimidos inicio trx
  	 XTotRed$  = A$
  	 XTotRed$ = TRANSLATE$(XTotRed$,"=","-")
  	 XTotRed$ = Str$(Val(XtotRed$))

!     UE.DATA2$ = Right$("000"+DM.ALMACEN$,3)          + \ Codigo del Almacen
!                 ","                                  + \ Separador de campo
!                 Right$("000"+TERMINAL$,3)            + \ Numero de terminal
!                 ","                                  + \ Separador de campo
!                 Right$("000"+NRO.TRANS$,3)           + \ Numero de transaccion
!                 ","                                  + \ Separador de campo
!                 FECHA$                               + \ Fecha de la trx AAMMDD
!                 ","                                  + \ Separador de campo
!                 HORA$                                + \ Hora de la TRX HHMMSS
!                 ","                                  + \ Separador de campo
!                 COD.OPERA$                           + \ Codigo del cajero
!                 ","                                  + \ Separador de campo                 
!                 XEAN$                                + \ Id del cliente
!                 ","                                  + \ Separador de campo                 
!                 NRO.CUENTA$                          + \ Puntos acumulados en la trx
!                 ","                                  + \ Separador de campo                 
!                 XRED$                                + \ Puntos redimidos en la trx
!                 ","                                  + \ Separador de campo                 
!                 VALOR.PAGO$                          + \ Valor de la compra
!                 ","                                  + \ Separador de campo                 
!                 XTotales$                            + \ Ptos totales inicio de la trx
!                 ","                                  + \ Separador de campo                 
!                 XTotRed$                               ! Puntos Redimitos totales inicio de la trx

     Fecha$ = "20" + Left$(Fecha$,2)+"-"+Mid$(Fecha$,3,2) + \!
              "-"+Right$(Fecha$,2)
     Hora$  = Left$(Hora$,2)+":"+Mid$(Hora$,3,2) + \!
              ":00"

     UE.DATA2$ = Right$("000"+DM.ALMACEN$,3)          + \! Codigo del Almacen
                 ","                                  + \!
                 XEAN$                                + \! Id del cliente
                 ","                                  + \!
                 VALOR.PAGO$                          + \! Valor de la compra
                 ","                                  + \! Separador de campo
                 Right$("000"+TERMINAL$,3)            + \! Numero de terminal
                 ","                                  + \! Separador de campo
                 Right$("0000"+NRO.TRANS$,4)          + \! Numero de transaccion
                 ","                                  + \! Separador de campo
                 FECHA$ + " " + Hora$            	    + \! Fecha de la trx AAAA-MM-DD
                 ","                                  + \! Separador de campo
                 Fecha$ + " " + Hora$                 + \! FEcha de Apertura 
                 ","                                  + \! Separador de campo
                 Fecha$ + " " + Hora$                 + \! FEcha de cierre
                 ","                                  + \! Separador de campo
                 "1"                                  + \! Total Personas 
                 ","                                  + \! Separador de campo
                 "EFECTIVO"                           + \! Forma de Pago
                 ","                                  + \! Separador de campo
                 COD.OPERA$                           + \! Codigo de operador
                 ","                                  + \! Separador de campo
                 VALOR.PAGO$                          + \! Total compra 
                 ","                                  + \! Separador de campo
                 "0"                                  + \! Propina
                 ","                                  + \! Separador de campo
                 "0"                                  + \! Impuesto total
                 ","                                  + \! Separador de campo
                 "0"                                     ! Descuento Total
                                  
     Call Grabacion.Interface
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
If ERR = "IH" Then Resume
If ERR = "SS" Then RESUME NXTRCD:

If ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call ENTRADA.LOG
    Stop 
EndIf 


If ERRF% = 38 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: Parametros FTP No Se Logro Abrir"
    Call ENTRADA.LOG
    Stop 
EndIf 


If ERRF% = 19 AND (ERR = "OE" OR ERR = "FU") Then Begin
   BAN.PRG$ = "1"
   RESUME        
EndIf 

If ERRF% = 50 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Numeracion Fiscal "
    Call ENTRADA.LOG
    Stop 
EndIf 

If ERRF% = 55 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Convenios"
    Call ENTRADA.LOG
    Stop 
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
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
