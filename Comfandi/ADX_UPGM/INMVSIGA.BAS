!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INMVSIGA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Recaudo SIGA           *
!**************************************************
! Modificacion
! < Versión 1.0 28 Ago 2019 >
! Mod 12Nov2019
! Se adiciona el envio de la interface de recaudo
! a un sitio ftp parametrizado por Comfandi
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 11/aGO/2020
! Se adiciona EAN del recaudo SIGA
! Solicitado por Luisa Yule - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!--------------------------------------------------

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Dato$, Ind.Mov$
String    Global Ue.Ip$, Ue.User$, Ue.Passwd$, Ue.Data2$, Ue.ruta$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, UE.LINEAS%
Integer*1 Global Ind.Trx%

%Include BKCNVVAR.011					                  ! Variables del programa
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

Function INICIO1
  Call.ORDER% = 11                                ! Llamado Primera Pantalla D.M
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


Sub CARGA.PARAMETROS.FTP 																									  ! Parametros del FTP
Integer*1 XLISTO%
String    XTmp$
  BAN.PRG$ = "1"
  Open "TF:SIGFTP" AS 38																				            ! Apertura parametros
  If BAN.PRG$ = "0" Then Begin 																						  ! Error en apertura
     MEN$ = "ERROR: Archivo Parametros FTP No Definido"											!
     Call ENTRADA.LOG																												!
     Stop 																																	!
  EndIf 																																		!
  If End #38 Then SALIDA.PARAMETROS																					! Si EOF
  XLISTO% = 0     																													! Ctrl Ciclo
  While (1)																																	!
     Read #38; Xtmp$		
     Read #38; Xtmp$																												! Dirsccion IP
     UE.IP$ = Mid$(XTMP$,30,38)	
     Read #38; Xtmp$																												! Usuario FTP
     UE.USER$ = Mid$(XTMP$,30,38)    
     Read #38; Xtmp$		        																						! Password
     UE.PASSWD$ = Mid$(XTMP$,30,38)     
     Read #38; Xtmp$		        																						! ruta archivos
     UE.RUTA$ = Mid$(XTMP$,30,38)     
     XLISTO% = -1
     GoTo SALIDA.PARAMETROS
  Wend 
  SALIDA.PARAMETROS:
    If Xlisto% = 0 Then Begin																						    ! Si parametros incorrectos
     MEN$ = "ERROR: Archivo Parametros Incorrecto "													!
     Call ENTRADA.LOG																												!
     Stop 																																	!
    EndIf 																																	!
    Close 38																																!
End Sub 

Function Valida.Comunicacion																								! Transmite credito social
Integer*1 Valida.Comunicacion																								!
Integer*4 TSA																																!
String    Xcon$, Xopen$, Xfile$, buffer$, Lec$

Create "ADX_SDT1:ADXHSIGF.DAT" AS 11                      ! Borra basura existente 
Close 11                                                  !

 Create "ADX_UDT1:SNDSIGA.TXT" AS 3
 Xfile$ = Ue.Salida$																			! Interface a transmitir
  
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

 buffer$ = "cd "+Ue.Ruta$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "send "+Xfile$+ " " +finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$               							! Grabacion del registro 

 buffer$ = "dir "+Xfile$+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "close "+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  

 buffer$ = "by"+finr$
 LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
 Write Form Lec$; #3 ;buffer$  				                    ! Grabacion del registro 

 Close 3
 Wait ; 1200
  
  Print "Trasfiriendo "+Xfile$
  Print "-------------"
  Call OSSHELL("TYPE ADX_UDT1:SNDSIGA.TXT | ftp > adx_udt1:rtasiga.txt") 					! Ejecuta el FTP
 Wait ; 1200 																																! 
 BAN.PRG$ = "1"																														! Init variable
 Valida.Comunicacion = 0     
 Open "ADX_UDT1:RTASIGA.TXT" AS 4																								! Abre respuesta
 If BAN.PRG$ = "1"  Then Begin 																						! Si apertura correcta
  If End #4 Then SALIDA.RTA:
  While (1)
    Read #4 ; Line BUFFER$
    TSA = 0 
    BUFFER$ = UCASE$(BUFFER$)
    TSA = MATCH("TRANSFER COMPLETED",BUFFER$,1)
    If TSA > 0 Then Begin 
      Valida.Comunicacion = -1  																						! Proceso OK
      GoTo SALIDA.RTA
    EndIf Else Valida.Comunicacion = 0    
  Wend 
  SALIDA.RTA:
    If TSA <= 0 Then Valida.Comunicacion = 0     													! Retorna falla 
    Delete 4 
 EndIf Else Begin 																													! Falla en la apertura
   Valida.Comunicacion = 0    																							! Retorna falla 
 EndIf 																																			!

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


Sub Grabacion.Interface
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      UE.LINEAS% = UE.LINEAS% + 1
      Call Write.Data
End Sub
!--- Fin grabacion de interface

Sub Graba.Registro.cero
   Ue.Data2$ = String$(24,"0")                      + \ Numero de referencia   Char 24
               "20"+FECHA$                          + \ Fecha de registro      Char 08
               "000000"                             + \ Hora del movimiento    Char 06
               "0000000000"                         + \ Valor del recaudo      Char 10
               "0000"                               + \ Codigo del almacen     Char 04
               "000000"                             + \ Codigo de la terminal  Char 06
               "0000000000"                         + \ Numero de transaccion  Char 10
               "770999812345"                       + \ Ean del Recaudo        Char 13
               String$(21," ")                        ! Relleno registro       Char 21
   Call Write.Data
End Sub 


Function TERMINE.PROG
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
    Else Begin
     Locate 12,15: Print MEN$
  EndIf
  MEN$ = "Interface Generada "+Ue.Salida$
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
    Else Begin
     Locate 22,1: Print MEN$
  EndIf
  
  If Cnt.Reg% = 0 Then \
  	 Call Graba.Registro.cero
  
  Close 51 : Wait ; 500
  Call Valida.Comunicacion
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  STOP
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     FECMOV$     = MID$(X.TMP$,5,8)              ! Fecha de movimiento
     DM.ALMACEN$ = "000"+Right$(DM.ALMACEN$,3)       ! Centro costo de log procesado
!-- Ajustado para procesos de reproceso     
!    DM.ALMACEN$ = "000"+Right$(DATO.SO$,3)       ! Centro costo de log procesado
!    FECMOV$ = "20" + Mid$(DATO.SO$,9,6)          ! Fecha movimiento

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
  CITM%          = 0
  NUM.SEP$       = ","
  PRIMERA.VEZ    = 1
  Ctrl.Trx$      = ""
  UE.LINEAS% = 0
  VTAS.TOTALES%  = 0 
  Dim RECIBOS$(110,5)
  Dim VTA.PAGO$(40,6)
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
Dim RECIBOS$(700,5)
Dim VTA.PAGO$(40,6)
CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
     OPEN B$ AS TLOG BUFFSIZE 16384 NOWRITE NODEL
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

Function LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$
 Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL	! Busca Eamtran Activo 
 LLAVE$ = PACK$("9998")
 Read Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,CLOSEPNT,FECHA.CIER$,RESERVED$  
 Close 37
 If DM.CODIGO$ = "2" Then Begin 
   If OSLOGNAME$="        " Then Begin 
  	    MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
        Call ADXSERVE(PP,26,1,MEN$)
        Wait;1800
	      B$ = SLOGNAME$ 
        FECHA.CIER$ = DATE$
   EndIf Else Begin 
         B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
         FECHA.CIER$ = UNPACK$(FECHA.CIER$)
         FECHA.CIER$ = LEFT$(FECHA.CIER$,6)	! Tomo primeros 6 caracteres aammdd
   EndIf
EndIf
TOT.TAMANO = SIZE(B$)
End Function 

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    ! Inicializacion Variables Display Manager
CAMPO% = 0
AREA1% = 11: AREA2% = 4				            ! Definicion area de trabajo archivo

DM.CODIGO$ = "2" 
DATO.SO$ = COMMAND$						! Dato S.O
Call LEER.CABECERA

If Ucase$(DATO.SO$) = "VERSION" Then BEGIN 
   !Print "Interface Recaudo SIGA Ver. 12/Nov/2019  11:00 am"
   !Print "Interface Recaudo SIGA Ver. 06/Ago/2020  04:30 pm"
   !Print "Interface Recaudo SIGA Ver. 14/Oct/2020  03:55 pm"
   Print "Interface Recaudo SIGA Ver. 29/Oct/2020  12:30 pm"
   Stop
ENDIF

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****       INTERFACE RECAUDOS SIGA                   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Octubre   29 2020  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
EndIf Else DATO.SO$ = "1"
If LEN(B$) = 0 Then Begin 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
EndIf
If LEN(B$) = 1 Then Begin 
   B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
   FECHA.CIER$ = DATE$						! Asigno fecha dia
EndIf
End Function 

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
	!MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND > NRO.REG) Then Begin
	!MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
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
 EndIf
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

Sub Cabecera.Interface
String Rbuffer$, X.Lec$
Integer*4 X.Len%

  UE.DATA2$ = "01"           + \ Caracter Fijo
              "8903032085"   + \ Nit recaudadora
              "20"+date$     + \ Fecha generacion archivo
              Left$(Time$,6) + \ Hora de Generacion del reporte
              String$(74," ")  ! Filler

  Rbuffer$ = ue.data2$
  X.Len% = Len(Rbuffer$)														  								! Toma longitud del registro
  X.Lec$ = "C"+Str$(X.len%)+" C2"										    							! Arma estructura de grabacion
  
  !Write form X.Lec$; #51 ; Rbuffer$, Finr$              							! Graba registro
  
End Sub
!--- Fin grabacion cabecera


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call CARGA.PARAMETROS.FTP
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
	  !MEN$ ="FALTA STRING ID TRANSACCION"
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
     
    On A GoSub S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21,S99
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GoSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  If OLD.TRX$ = "XXX" Then BEGIN 
     OLD.TRX$  = NRO.TRANS$
     OLD.CAJA$ = TERMINAL$ 
  EndIf   
  GoSub GETUNPK																			!
!  FECMOV$ = A$
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,6)     						! Toma fecha de movimiento del
     ARC% = 1
     Ue.Salida$ = "/ADX_UDT1/SG"+FECHA.CIER$+"."+Right$(DM.ALMACEN$,3)
     Create Ue.Salida$ AS 51 
     Call Cabecera.Interface
  EndIf
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  GoSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans = 17 Then GOTO NXTRCD
  If TIPO.TRANS = 2 Then X.CAMBIO% = -1 
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
Return

S1:
  J = 3
  Ind.Trx% = 0 
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
  J = 3
RETURN

S9:
  J = 3
Return

S10:
  J = 3
RETURN

S11:
Return

S12:
  J = 3
RETURN

S13:
  J = 3
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
Return

S21:
  J = 3
RETURN

S99:
  J = 3 
    GoSub GETUNPK																														! LLAVE DEL ARCHIVO
    If A$ = "20080520" Then Begin                 													! RECAUDO TERCEROS
     GoSub GETUNPK 																													! EAN DEL RECAUDO
     If A$ = "770999812345" Then BEGIN             													! RECAUDO SIGA
        GoSub GETUNPK 																											! TIPO DE PROCESO
        GoSub GETUNPK 																											! NUMERO DE DOCUMENTO
        NRO.CUENTA$ = Str$(Val(A$))
        NRO.CUENTA$ = Right$(String$(24,"0")+NRO.CUENTA$,24)
        GoSub GETUNPK 		        																					! VALOR DEL RECAUDO
        VALOR.PAGO$ = Right$("0000000000"+A$,10)  
        
        UE.DATA2$ = NRO.CUENTA$                       + \ Numero de referencia   Char 24
                 "20"+FECHA$                          + \ Fecha de registro      Char 08
                 HORA$                                + \ Hora del movimiento    Char 06
                 VALOR.PAGO$                          + \ Valor del recaudo      Char 10
                 Right$("0000"+DM.ALMACEN$,4)         + \ Codigo del almacen     Char 04
                 Right$("000000"+TERMINAL$,6)         + \ Codigo de la terminal  Char 06
                 Right$("0000000000"+NRO.TRANS$,10)   + \ Numero de transaccion  Char 10
                 "770999812345"                       + \ Ean del Recaudo        Char 13
                 String$(21," ")                        ! Relleno registro       Char 21
        
        Call Grabacion.Interface
        Cnt.Reg% = Cnt.Reg% + 1
     EndIf   
    EndIf 
    
Return

GETEXCPT:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If ((K-J)-1) > 0 Then \ ! If DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  Else \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BeginNING OF NEXT FIELD
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
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
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
BAN.PRG$ = "0"
If ERR = "SS" Then RESUME NXTRCD:
If ERR = "IH" Then RESUME 
If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   RESUME           
EndIf 

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
 EndIf

If ERR = "OE" Or ERR = "FU" Or ERR = "CU" Then RESUME 	
	
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
     Call TERMINE.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Locate 22,1: Print String$(78," ")
Locate 22,1: Print MEN$
Stop
!*********************************************************************
