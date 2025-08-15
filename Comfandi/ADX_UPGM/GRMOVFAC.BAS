!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : GRMOVFAC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generacion Copia Factura Venta   *
!                Comfandi                         *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%, PrsStat, EF.RC
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, NIT$, NITEMPRESA$, XEAN$
Integer*1 Global ARC%, INDT%, Terror%, Ind.Itm%, Lpos%, GrPago.Naut%, Gr.Trx.Np%, Gr.Itm.Tarifa%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%, Xcont%, UE.TABLAIVA(1)
String    Global Promo$, VTA.ITEM$(2), U.TER$, U.TRX$, Xitm$, Xgrp$, GPagos$, Gdscto$


%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! Maestro Articulos
%Include DMEXTR.J86    		                      ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include BASROUT.J86

!--- Definicion de rutinas de la aplicacion

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

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
      Write form X.Lec$; #44 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 

Function TERMINE.PROG
Integer*4 XI%
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Locate 12,15: Print MEN$
 
 If GrPago.Naut% = -1 Then Begin										! Si forma de pago no autorizada para anulaciones
 	  CITM% = -1 																			! No retorna proceso
 EndIf																							!
 
 If Gr.Trx.Np% = -1 Then Begin 
    CITM% = -2
 EndIf
 !Close TLOG
 
 Gr.Trx.Np% = 0
 GrPago.Naut% = 0
End Function
!--- Fin de la ejecucion del programa

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
  NitEmpresa$ = "815000070" ! TelePalmira
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		    :CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		  :LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
Q                 = 1
NRO.StringS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero 
Dim VTA.ITEM$(500,5)
CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
Cnt.Reg%       = 0   ! Contador de registros
TOT.LEIDO   = 0 
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
  	 Print "PROCESANDO LOG "+B$
     OPEN B$ AS TLOG NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     B$ = ""
  EndIf Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     STOP
  EndIf
End Function 

Function LEA.REG.TRANS.SUM.LOG	    																					! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$																																	!
Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL															! Busca Eamtran Activo 
LLAVE$ = PACK$("9998")																												! Llave de busqueda
READ Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, 									     \!
       SLOGNAME$,OSLOGNAME$,ClosePNT,FECHA.CIER$,RESERVED$  									!
Close 37																																			!
B$ = SLOGNAME$																																! Retorna log actual
TOT.TAMANO = SIZE(B$)																													! Tamaño del log
ARCHIVO.PROC$ = B$																														! Retorna log a procesar
End Function 

Function PANTALLA.PRINCIPAL																										!
Call INICIADM 				                    																		! Inicializacion Variables Display Manager
TERM$ = " "					            																							! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    																					!
Call DM.ERR(RET.ERR%,INITDM$)				    																			!
RET.ERR% = OPNDIS("C:\ADX_UPGM\RPMOVREC.PNT")	            										! Apertura de la For ma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call INICIO1 









Call INICIADM                                               									! Inicializacion Variables Display Manager
CAMPO% = 0																																		!
DM.CODIGO$ = "2"																															!
If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

B$ = "C:/LOGS/EAMTRANB.DAT"

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "****  GENERACION COPIA FACTURA - COMFANDI           ****"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software Septiembre 21 2011 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
  Print "Esperando Requerimiento...."
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = "Esperando Requerimiento...."
  Call Entrada.Log
EndIf
  GrPago.Naut% = 0 
  Gr.Trx.Np% = 0 
  Input "Numero de Terminal    :";U.ter$
  Input "Numero de transaccion :";U.Trx$
  U.TER$ = Right$("0000"+U.ter$,4)
  U.TRX$ = Right$("0000"+U.trx$,4)
  MEN$ = "Procesando "+U.TER$+" - "+U.TRX$
  Call Entrada.Log

If LEN(B$) = 0 Then Begin                                       ! Dia previo
  Call LEA.REG.TRANS.SUM.LOG                                    ! DEVUELVE ARCHIVO PREVIO
Endif
If LEN(B$) = 1 Then Begin
  B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT"                         ! Tomo log capturado
  FECHA.CIER$ = DATE$                                          ! Asigno fecha dia
Endif
End Function 


Function Cuente.StringS	       	! Esta funcion cuenta primero el #String y lo
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

Sub CARGA.TARIFAS.IVA
Integer*4 UE.PORCENTAJE, UE.LINEA
Dim UE.TABLAIVA(10)
Open "TABLAIVA" AS 44 NOWRITE NODEL
If END # 44 THEN  CONT.IVA
LEER.IVA:
   READ # 44 ; UE.PORCENTAJE
   UE.LINEA = UE.LINEA + 1
   UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
   GoTo LEER.IVA 
CONT.IVA:
  CLOSE 44
End Sub 

Sub TARIFA.IVA.ITEM
Integer*4 UE.INDI1%
String UE.BIN$
   UE.INDI1% = INDICAT1                              		      !
    IF (UE.INDI1% AND 01H) Then          			               \!
        UE.BIN$ = "1" ELSE UE.BIN$ = "0"                 		  !
     FOR I% = 1 TO 7                                      		!
       UE.INDI1% = SHIFT(UE.INDI1%,1)                   	    ! Inicializa sig. BIT
       IF (UE.INDI1% AND 01H) Then UE.BIN$="1"+UE.BIN$       \!
           ELSE UE.BIN$ ="0"+UE.BIN$                      		!
     NEXT I%                                               		!
     GR.ITM.TARIFA% = 8					 												      ! Tarifa Default
     If LEFT$(UE.BIN$,4) = "1000" Then                       \! Si hay Indic Plan A
        GR.ITM.TARIFA% = 1		                               \! indica Plan = A
     	Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0100" Then                       \! Si hay Indic Plan B
        GR.ITM.TARIFA% = 2		                               \! indica Plan = B
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0010" Then                       \! Si hay Indic Plan C
        GR.ITM.TARIFA% = 3		                               \! indica Plan = C
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0001" Then                       \! Si hay Indic Plan D
        GR.ITM.TARIFA% = 4		                               \! indica Plan = D
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "1100" Then                       \! Si hay Indic Plan AB
        GR.ITM.TARIFA% = 5		                               \! indica Plan = E
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0110" Then                       \! Si hay Indic Plan BC
        GR.ITM.TARIFA% = 6		                               \! indica Plan = F
      Else                                                   \! si no,
     If LEFT$(UE.BIN$,4) = "0011" Then                       \! Si hay Indic Plan CD
        GR.ITM.TARIFA% = 7		                               \! indica Plan = G
      Else                                                   \! si no,
     IF LEFT$(UE.BIN$,4) = "0000" Then                       \! 
        GR.ITM.TARIFA% = 8		                                ! indica Plan H
End Sub 

Sub IMPRIME.DOCUMENTO
String Gr.Ind.Iva$, Linebuf$
Integer*1 UE.TARIFA%
Integer*4 XI%

 If CITM% > 0 Then Begin 
    For XI% = 0 To (CITM% - 1)
      Ue.Tarifa% = Val(VTA.ITEM$(XI%,3))
      Gr.Ind.Iva$ = MID$("ABCDEFGH",UE.TARIFA%,1)
      Ue.Data2$ = VTA.ITEM$(XI%,0)          + "," +  \! Plu del articulo
                  "1234567890----XXXXXXZZZ" + "," +  \! Descripcion del articulo
                  VTA.ITEM$(XI%,1)          + "," +  \! Precio
                  VTA.ITEM$(XI%,2)          + "," +  \! Cantidad
                  Gr.Ind.Iva$                         ! Tarifa Iva

      Print Ue.Data2$
    Next XI%
 EndIf Else Begin
 	Print "No se Encontro Movimiento...."
 EndIf
End Sub


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  INIT.PROCESO:
  ARC% = 0
  Call INI.VAR.PROG	
  Call CARGA.TARIFAS.IVA
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC

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
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21, S99
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  Call GRABE.IDENTIfICADOR
  GOSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GOSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GOSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
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
  GoSub GETUNPK
  COD.ITEM$=RIGHT$(STRING$(12,"0")+A$,12) 					! CODIGO PLU O EAN
  GoSub GETUNPK
  PRECIO$ = RIGHT$(STRING$(10,"0")+A$,10)						! PRECIO DEL ARTICULO
  CANTIDAD$ = "00001"																! INICIALIZAMOS CANTIDAD
  GoSub GETUNPK
  DEPTO$ = RIGHT$("0000"+A$,4)	 										! DEPARTAMENTO DEL ARTICULO
  GoSub GETUNPK																			! Cupon Family 3 y 3 Long 6
  GoSub GETUNPK 																	  ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  IVA% = 0
  PESO = 0
  INDICAT1 = VAL(A$)
  IF INDICAT1 AND 4000H THEN PESO = 1 ELSE PESO = 0
  Call TARIFA.IVA.ITEM                                 ! Tarifa de IVA del producto
  GoSub GETUNPK                                        ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = VAL(A$) 
  GoSub GETUNPK                                        ! INDICAT3   QUE VENDIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM? 
  ALTO$ = LEFT$(A$,1)
  BAJO$ = RIGHT$(A$,1)
  ESCANEADO = 0	
  IF ALTO$ = "8" OR ALTO$ = "2" OR ALTO$ = "5" \
  	THEN SIGNO$ = "-" ELSE SIGNO$ = ""
  IF (INDICAT2 AND 0080H) AND (ALTO$ = "7") THEN SIGNO$ = ""    
  IF (INDICAT2 AND 0040H) AND (ALTO$ = "7") THEN SIGNO$ = ""    
  IF (INDICAT2 AND 0080H) AND (ALTO$ = "2") THEN SIGNO$ = ""    
  IF (INDICAT2 AND 0080H) AND (ALTO$ = "4") THEN SIGNO$ = "-"    
  IF (INDICAT2 AND 0040H) AND (ALTO$ = "4") THEN SIGNO$ = "-"    
  IF (INDICAT2 AND 0080H) AND (ALTO$ = "1") THEN SIGNO$ = "-"   ! PRUEBA DEPOSIT
  IF (INDICAT2 AND 0040H) AND (ALTO$ = "1") THEN SIGNO$ = "-"   ! PRUEBA DEPOSIT
  If SIGNO$ = "-" Then Begin 
     PRECIO$ = Str$(Val(PRECIO$) * -1)
     CANTIDAD$ = Str$(Val(Cantidad$) * -1)
  EndIf 
 If Val(TERMINAL$) = Val(U.TER$)  And    \!
    Val(NRO.TRANS$) = Val(U.TRX$) Then Begin
    Ind.Itm% = 0
    For Xcont% = 0 To CITM%
      If Val(VTA.ITEM$(Xcont%,0)) = Val(COD.ITEM$)	Then Begin
      	  Ind.Itm% = -1
      	  Lpos% = Xcont%
      Endif 
    Next Xcont%
    If Ind.Itm% = 0 Then Begin
    	 Lpos% = CITM%
       VTA.ITEM$(LPOS%,0) = COD.ITEM$																						! Codigo del producto
       VTA.ITEM$(LPOS%,1) = PRECIO$																							! Precio del producto
       VTA.ITEM$(LPOS%,2) = CANTIDAD$																						! Qty Vendida
       VTA.ITEM$(LPOS%,3) = Str$(Gr.Itm.Tarifa%)																! Tarifa Iva
       VTA.ITEM$(LPOS%,4) = "0"   			  																			! Grupo descuento
       CITM% = CITM% + 1
    Endif Else Begin
       VTA.ITEM$(LPOS%,1) = Str$(Val(PRECIO$) + Val(VTA.ITEM$(LPOS%,1)))  			! Precio del producto    	 
       VTA.ITEM$(LPOS%,2) = Str$(Val(CANTIDAD$) + Val(VTA.ITEM$(LPOS%,2)))      ! Aumenta Qty Vendida
    EndIf
 EndIf

Return

S2:
  J = 3 		        ! EXTENSION DE ENTRADA
  GoSub GETUNPK			!  NO USADO   MPGROUP= Val(A$)
  GoSub GETUNPK			!  NO USADO   DEALQUAN= Val(A$)
  GoSub GETUNPK			!  NO USADO   PRICE METH= Val(A$)
  GoSub GETUNPK			!  NO USADO   SALEQUAN= Val(A$)
  GoSub GETUNPK			!  NO USADO   SALEPRIC= Val(A$)
  GoSub GETUNPK			!  CANTIDAD O PESO VENDIDO = QTYORWGT
  If SIGNO$ = "-" Then Begin 
     A$ = Str$(Val(A$) * -1)
  EndIf 
   If Val(TERMINAL$) = Val(U.TER$)  And    \!
    Val(NRO.TRANS$) = Val(U.TRX$) Then Begin
    If Ind.Itm% = 0 Then Begin    	
       VTA.ITEM$(Lpos%,2) = A$																									! Cantidad Vendida
    EndIf Else Begin 
    	 VTA.ITEM$(Lpos%,2) = Str$(Val(VTA.ITEM$(Lpos%,2)) + (Val(A$)))						! Cantidad Vendida
    	 If SIGNO$ = "-" Then Begin
    	 	  VTA.ITEM$(Lpos%,2) = Str$(Val(VTA.ITEM$(Lpos%,2)) + 1)						! Cantidad Vendida
    	 EndIf Else VTA.ITEM$(Lpos%,2) = Str$(Val(VTA.ITEM$(Lpos%,2)) - 1)						! Cantidad Vendida
    EndIf
   EndIf
  GoSub GETUNPK			!  NO UTILIZADO  INDICAT1= RIGHT$(A$,8)  
Return

S3:
  J = 3
Return

S5:
 J = 3 
 GoSub GETUNPK 						!  FORMA O TIPO DE PAGO, TENDTYPE
 FORMA.PAGO$=A$      			!  Tipo de pago
 GoSub GETUNPK 						!  CANTIDAD DE PAGO,  AMTTENDE
 VALOR.PAGO$ = A$    			!  Valor del pago
Return

S7:
  J = 3!TAX
Return

S9:
  J = 3
Return

S10:
  J = 3
Return

S11:
  J = 3!DTENTRY
Return

S12:
  J = 3!CHANGE PRICE
Return

S13:
  J = 3!LOAN-PICKUP
Return

S14:
  J = 3
Return

S15:
  J = 3
Return

S16:
Return

S20:
  J = 3
Return

S21:
  J = 3
Return

S99:
  J = 3
  GoSub GETUNPK																																! Numero de proyecto
Return
     
GETEXCPT:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If ((K-J)-1) > 0 Then \ ! If DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  Else \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BEGINNING OF NEXT FIELD
Return
     
!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
Return
!*********************************************************************

REPORTE.FINAL:
        Call TERMINE.PROG
        Call IMPRIME.DOCUMENTO
        Stop 
Return
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
  Terror% = 0 
If ERR = "SS" Then RESUME NXTRCD:

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ENTRADA.LOG
    Stop 
EndIf

If ERRF% = 44 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Tarifas de IVA "
    Call ENTRADA.LOG
    Stop 
EndIf


Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
