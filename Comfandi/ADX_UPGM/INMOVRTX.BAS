!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INMOVITM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Movimiento de Items vendidos     *
!                remisiones Saleforce             *
!**************************************************
!

%ENVIRON C						   												! Ambiente de controlador
%Include POSPVARI.BAS				  	                ! Rutinas Comunes

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
String    Global Ue.Ip$, Ue.User$, Ue.Passwd$, Ue.Ruta$, Plu.Val$(1), Gr.Nit$, UE.TMP1$,NRO.FACTURA$
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, XEAN$, Xred$, \
                 XTotales$, XTotRed$, U.File$, Price$, Sgn$, IR.USERDATA$, ASC.IR.NDEPTO$, Nfile$, Xmen$, \
                 remision$, XnroRem$, Xdpto$, UE.TABLAIVA(1), GrImpo$, GrTax$, Grtipo$
Integer*1 Global ARC%, INDT%, Xfound%, Xrms%, Xfnd%, Peso%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%, Xdscto%, Nitm%, Xj%
Integer*4 Global PP, VTAS.TOTALES%, Cnt.Reg% , CITM%, TotCab%, Pvup%, Pvtot%, QtyVta%, Xpos%, \
                 UE.TARIFA%, UE.LINEA

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

Sub Auditoria.Proceso
Integer*4 X.Len%
String X.Lec$
      X.Len% = Len(Xmen$)						  					  										! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #77 ; Xmen$, Finr$           								! Graba registro
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
Integer*4 Xlen%
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
 Else Begin
  Locate 12,15: Print MEN$
 Endif
  Ue.Data2$ = DM.ALMACEN$            + \! Centro de costo
 	            "9999"                 + \! Numero de terminal
 	            "9999"                 + \! Numero de transaccion
              "20"+Date$             +	\! fecha generacion archivo
              "999999999999"         +	\! Registro de cierre
              Right$("000000000000000"+Str$(Cnt.Reg%),15)       +	\! Cantidad de registros
              Right$("000000000000000"+Str$(Vtas.Totales%),15) ! Total de movimiento
  Xlen% = Len(Ue.Data2$)
  Xlen% = 158 - Xlen%
  Ue.Data2$ = Ue.Data2$ + String$(Xlen%," ")
  !Call Grabacion.Interface
  
  MEN$ = "INTERFACE "+Ue.Salida$
  Call ENTRADA.LOG
  Xmen$ = "TERMINADO : "+UE.SALIDA$
  Call Auditoria.Proceso

  Close 77 : Close 51 
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


Sub Cabecera.Reporte
  UE.DATA2$ = "1"            + \ Caracter Fijo
              FECMOV$        + \ Fecha generacion archivo
              DM.ALMACEN$      ! Centro de Costo
  !Call Write.Data
End Sub 


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
  Open "ADX_UDT1:AUSISMED.TXT" As 77 Append 
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
Dim RECIBOS$(700,11)
CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Cnt.Reg%       = 0   ! Contador de registros
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
   	 Xmen$ = "PROCESANDO: "+B$
     Call Auditoria.Proceso
     OPEN B$ AS TLOG NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     Nfile$ = Mid$(b$,12,6)
     B$ = ""
  EndIf Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     Xmen$ = Men$
     Call Auditoria.Proceso
     Close 77 
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
   Print "INTERFACE MOV. ITEMS SALEFORCE Version 11/Abr/2021 12:30 am"
   Stop 
EndIf

Clears

!Call CARGA.LISTA.PLUS

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "*   INTERFACE MOVIMIENTO PRODUCTOS REMISION SALEFORCE  *"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software ABRIL      11 2021 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
  LOCATE 7, 4: Input "Numero de transacc: ";XnroRem$
  locate 8, 4: Input "Numero de Remision: ";Remision$
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 03/Oct/2017 G.R...."
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

Sub Tarifa.Imptos.Remision
String Gbuff$, GR.BIN$, GrLt$, GrAlpha$, Grname$
Integer*1 GR.INDI1%, XJ%
  GrAlpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  GR.INDI1% = INDICAT1                                                 			 !                         
  IF (GR.INDI1% AND 01H) THEN                                               \!                         
      GR.BIN$ = "1" ELSE GR.BIN$ = "0"                                       !                         
  FOR I% = 1 TO 7                                                            !                         
       GR.INDI1% = SHIFT(GR.INDI1%,1)                                        ! Inicializa sig. BIT     
       IF (GR.INDI1% AND 01H) THEN GR.BIN$="1"+GR.BIN$                      \!                         
           ELSE GR.BIN$ ="0"+GR.BIN$                                         !                         
  NEXT I%                                                                    !                         
  UE.TARIFA% = 8																														 ! TARIFA Default 
  If LEFT$(GR.BIN$,4) = "1000" THEN                                         \! Si hay Indic Plan A     
                      UE.TARIFA% = 1		                                    \! indica Plan = A         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0100" THEN                                        \! Si hay Indic Plan B     
                      UE.TARIFA% = 2		                                    \! indica Plan = B         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0010" THEN                                        \! Si hay Indic Plan C     
                      UE.TARIFA% = 3		                                    \! indica Plan = C         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0001" THEN                                        \! Si hay Indic Plan D     
                      UE.TARIFA% = 4		                                    \! indica Plan = D         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "1100" THEN                                        \! Si hay Indic Plan AB    
                      UE.TARIFA% = 5		                                    \! indica Plan = E         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0110" THEN                                        \! Si hay Indic Plan BC    
                      UE.TARIFA% = 6		                                    \! indica Plan = F         
   ELSE                                                                     \! si no,                  
   IF LEFT$(GR.BIN$,4) = "0011" THEN                                        \! Si hay Indic Plan CD    
                      UE.TARIFA% = 7		                                    \! indica Plan = G         
   ELSE                                                                     \! si no,                  
                      UE.TARIFA% = 8		                                     ! indica 0%    
   GrTipo$ = "0" : GrImpo$ = "0"
   GrTax$ = UE.TABLAIVA(UE.TARIFA%)																					 ! VALOR DEL IVA
   If UE.TARIFA% = 7 Then GRTIPO$ = "1"															  		   ! Si es excluido
   If UE.TARIFA% = 8 Then GRTIPO$ = "0"															  		   ! Si es excento
   If UE.TARIFA% = 4 Then GrImpo$ = "1"                              		     ! Si es impoconsumo

End Sub 

Sub Carga.Ivas
String UE.PORCENTAJE
Integer*1 GI%
UE.LINEA = 0
BAN.PRG$ = "0"
Dim UE.TABLAIVA(10)
Open "TABLAIVA" AS 30
IF BAN.PRG$ = "1" THEN BEGIN
   Print "ERROR APERTURA IVA"
   GOTO SALIDA.IVA
ENDIF 
GI% = 1
While GI% = 1
   BAN.PRG$ = "0"
   READ # 30 ;  UE.PORCENTAJE
   If BAN.PRG$ = "1" Then GoTo salida.iva
   UE.LINEA = UE.LINEA + 1    
   UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
Wend 
SALIDA.IVA:
  CLOSE 30
  Print "Tarifas Iva Cargadas "+Str$(UE.LINEA)
  For gi% = 1 To Ue.Linea
      Print Ue.Tablaiva(GI%)
Next GI%
  wait ; 2000
End Sub 



!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call LEER.CABECERA
  Call Carga.Ivas
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
	  NO.VENTA = -1
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. STRING
	  GoTo AGAIN		   	! VUELVA A LEER OTRO REGISTRO
	ENDIF
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 	! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
    B$ = B$+":" 			! SUMA UN SEMI-COLON
    Q = P + 1 				! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 	! DETERMINA TIPO DE STRING
    If A = 0  Then GoSub S0:  GoTo AGAIN	! SI ES CHECKOUT TRANSACCION
    If A = 99 Then GoSub S99: GoTo AGAIN
    If (A < 0) OR (A > 21) THEN GoTo AGAIN
    If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) THEN \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
    If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
		   CANT.PESO$ = "000000000" 
       SW.ITEM = 0
    EndIf
    If (A > 3) AND (SW.ITEM = 1) THEN \	!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
          	SW.ITEM = 0
     EndIf
     
    On A GoSub S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  
       If CITM% > 0 And Xrms% = -1 Then Begin								 						! Hay items para almacenar
     	  xj% = 0
     	  Print "total items remision "+str$(citm%)
     	  For Xj% = 1 To Citm%
     	   If Recibos$(xj%,10) = "0" Then \
     	      Pvup%   = Val(Recibos$(xj%,3)) / Val(Recibos$(xj%,2)) Else \! Ajusta precio de venta 
     	      Pvup%   = (Val(Recibos$(xj%,3)) / Val(Recibos$(xj%,2))) * 1000
     	    Pvtot%  = Val(Recibos$(xj%,3))       											   ! Ajusta precio total venta 
     	    QtyVta% = Val(Recibos$(xj%,2))         											 ! Ajusta cantidad vendida
 	        UE.DATA2$ = DM.ALMACEN$       				+											\! Centro de costo				
 	                    ";"                       +                     \! Separador
 	                    TERMINAL$         				+											\! Numero de terminal     
 	                    ";"                       +                     \! Separador
 	                    NRO.TRANS$        				+											\! Numero de transaccion  
 	                    ";"                       +                     \! Separador
                      "20"+Fecha$     					+											\! fecha movimiento       
                      ";"                       +                     \! Separador
 	                    Recibos$(Xj%,0) 					+											\! Plu                    
 	                    ";"                       +                     \! Separador
    	                Recibos$(xj%,1) 					+											\! Descripcion            
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,6) 					+											\! Departamento pos       
    	                ";"                       +                     \! Separador
    	                Right$("0000000000"+Str$(QtyVta%),10)     +	  	\! Cantidad               
    	                ";"                       +                     \! Separador
    	                Right$("0000000000"+Str$(Pvup%),10)       + 		\! Precio Unitario 
    	                ";"                       +                     \! Separador
    	                Right$("0000000000"+Str$(Pvtot%),10)      + 		\! Total Venta            
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,4) 					+											\! Signo operacion        
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,5) 					+											\! Estructura comercial   
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,7) 					+											\! Tarifa de iva       
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,8) 					+											\! Tipo iva cero       
    	                ";"                       +                     \! Separador
    	                Recibos$(xj%,9) 					+											\! Impoconsumo
    	                ";"                       +                     \! Separador
                      Left$(Remision$+"                  ",18)  +     \! Nro remision saleforce 
                      ";"                                              ! Separador

          Cnt.Reg% = Cnt.Reg% + 1
          If Recibos$(xj%,4) = "+" Then \
               VTAS.TOTALES% = VTAS.TOTALES% + Val(Recibos$(xj%,3)) \
          Else VTAS.TOTALES% = VTAS.TOTALES% - Val(Recibos$(xj%,3)) 
          	
          Call Grabacion.Interface
        Next Xj%
     endif 

  NRO.FACTURA$ = ""
  !Remision$ = ""
  Gr.Nit$ = ""
  CITM% = 0
  Xfound% = 0
  Xrms% = 0
  Dim RECIBOS$(700,11)
  GoSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  Xfnd% = 0
  If XnroRem$ = (TERMINAL$ + NRO.TRANS$) Then \
     Xfnd% = -1
  GoSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:REMISION.TXT"
     Print "Creando "+ue.salida$
     Create Ue.Salida$ AS 51 
     Open "EAMITEMR" KEYED RECL 77 AS AREA2%       	! Abre maestra de articulos
     Call Cabecera.Reporte
  EndIf
  HORA$ = RIGHT$(A$,4)    	        								! CAPTURA HORA DE LA TRANSACCION
  GoSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans <> 00 Then GOTO NXTRCD
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
  TotCab% = TotCab% + (Val(GROSS.POSITIVO$) - Val(GROSS.NEGATIVO$))
  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD									! 
  EndIf
Return

S1:
  J = 3
  Xfound%  = 0
  GoSub GETUNPK
  COD.ITEM$ = RIGHT$(STRING$(12,"0")+A$,12) 					! CODIGO PLU O EAN
  GoSub GETUNPK																				! VENTA TOTAL
  Price$ = a$
  GoSub GETUNPK                                       ! Departamento
  Xdpto$ = A$
  GoSub GETUNPK                                       ! Cupon Family 3 y 3 Long 6
  GoSub GETUNPK                                       ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  INDICAT1 = Val(A$)
  If INDICAT1 AND 4000H Then PESO% = 1 Else PESO% = 0
  
  
  GoSub GETUNPK                                       ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = Val(A$)
  GoSub GETUNPK                                       ! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
  INDICAT3 = Val(A$)
  Sgn$ = "+"
  If (INDICAT2 AND 0080H) Then Sgn$ = "-"
    	 BAN.PRG$ = "0" 
    	 Xfound% = 0
    	 IR.ITEMCODE$ = PACK$(COD.ITEM$)
    	 %INCLUDE EAMIRRD1.J86
    	 If BAN.PRG$ = "1" Then Begin 
    	 	  IR.ITEMNAME$   = "ITEM NO DEFINIDO  "
    	 	  ASC.IR.NDEPTO$ = "000000000000000"					! DEPARTAMENTO COMERCIAL
    	 EndIf Else Begin
    	 	  ASC.IR.NDEPTO$ = LEFT$(IR.USERDATA$,16)			! DEPARTAMENTO COMERCIAL
    	 	  ASC.IR.NDEPTO$ = Right$(ASC.IR.NDEPTO$,15)  !
    	 EndIf
    	 Call Tarifa.Imptos.Remision
    	 For Xj% = 1 To CITM%
    	   If Val(Recibos$(Xj%,0)) = Val(COD.ITEM$) Then BEGIN
    	   	  Xfound% = -1
    	   	  Xpos% = Xj%
    	   EndIf
    	 Next Xj%
       If Xfound% = 0 Then BEGIN 
            CITM% = CITM% + 1
    	      Recibos$(CITM%,0) = COD.ITEM$									  ! Plu
    	      Recibos$(CITM%,1) = IR.ITEMNAME$								! Nombre
    	      Recibos$(CITM%,2) = "000000000001"						  ! Cantidad
    	      Recibos$(CITM%,3) = Right$("000000000000"+Price$,12) ! Total Venta
    	      Recibos$(CITM%,4) = Sgn$												! Signo operacion
    	      Recibos$(CITM%,5) = ASC.IR.NDEPTO$							! Estructura comercial
    	      Recibos$(CITM%,6) = Xdpto$
    	      Recibos$(CITM%,7) = Grtax$ 
    	      Recibos$(CITM%,8) = Grtipo$ 
    	      Recibos$(CITM%,9) = Grimpo$ 
    	      Recibos$(CITM%,10) = Str$(Peso%)                ! 1.Pesable 0.Unitario
    	      Xpos% = CITM%
       EndIf Else Begin
       	   If Sgn$ = "+" Then \
              Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) + 1) Else   \! Suma cantidad vendida
              Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) - 1)         ! Suma cantidad vendida
           If Sgn$ = "+" Then \
              Recibos$(xpos%,3) = Str$(Val(Recibos$(xpos%,3)) + Val(Price$)) Else \! Suma cantidad vendida
          	  Recibos$(xpos%,3) = Str$(Val(Recibos$(xpos%,3)) - Val(Price$)) 
                   	
       EndIf
Return

S2:
  J = 3 		        								! EXTENSION DE ENTRADA
 		 GoSub GETUNPK                 !  MPGROUP
 		 GoSub GETUNPK                 !  DEALQUAN
 		 GoSub GETUNPK                 !  PRICE METH
 		 GoSub GETUNPK                 !  SALEQUAN
     GoSub GETUNPK                 !  SALEPRIC
 		 GoSub GETUNPK                 !  CANTIDAD O PESO VEndIDO
 		 If Sgn$ = "+" Then Begin
 		 	Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) - 1) 
      Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) + Val(a$)) 
     EndIf Else Begin
     	Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) + 1) 
      Recibos$(xpos%,2) = Str$(Val(Recibos$(xpos%,2)) - Val(a$)) 
     EndIf
Return

S3:
  J = 3
Return

S5:
 J = 3 
  GoSub GETUNPK 													  			  ! TIPO Y VARIEDAD DE PAGO
  If A$ = "12" Then Begin
  	If Xfnd% = -1 Then \
  	   Xrms% = -1
  	
  EndIf
Return

S7:
  J = 3 ! TAX
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
  GOSUB GETUNPK 																															!  NUMERO DE PROYECTO 
!  If A$ = "20110114" Then Begin																								! remisiones saleforce
!     GoSub GETUNPK4                                        										! Nro remision hijo
!     Remision$ = A$
!     Print "remision encontrada" + a$
!     If Val(A$) = Val(XnroRem$) Then Begin
!     	  Print "PARA PROCESAR ITEMS "
!        Xrms% = -1
!     EndIf
!  EndIf																																				!

  If A$ = "21" Then Begin																								! remisiones saleforce
  	If Xfnd% = -1 Then \
  	 Xrms% = -1
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
If ERR = "IH" Then Resume 
If ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 77 AND (ERR = "OE" OR ERR = "FU") Then Begin
    Create "ADX_UDT1:AUSISMED.TXT" As 77 
    Resume 
EndIf 

If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call ENTRADA.LOG
    Xmen$ = Men$
    Call Auditoria.Proceso
    Stop 
EndIf 

If ERRF% = 18 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: Archivo lista departamentos no existe"
    Call ENTRADA.LOG
    Xmen$ = Men$
    Call Auditoria.Proceso
    Stop 
EndIf 

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ENTRADA.LOG
    Xmen$ = Men$
    Call Auditoria.Proceso
    Stop 
 EndIf
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ENTRADA.LOG
     Xmen$ = Men$
     Call Auditoria.Proceso

     Call SALIDA.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
Xmen$ = Men$
Call Auditoria.Proceso
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
Xmen$ = Men$
Call Auditoria.Proceso
Stop
!*********************************************************************
