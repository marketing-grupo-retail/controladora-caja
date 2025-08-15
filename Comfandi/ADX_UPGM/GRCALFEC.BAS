!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCALFEC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : CALCULO FECHA CIERRE CONTABLE    *
!                TIENDA                           *
!**************************************************
! Mod9Dic2008
! Se ajusta la acumulacion de las formas de pago 
! para que acumulen por tipo y variedad
! Ajustado por Grupo Retail - OVS
!--------------------------------------------------
! Mod13Dic2008
! Se ajusta el modulo en la rutina de recorrido del
! Log de trx para evitar que se quede dentro del ciclo
! de lectura del tlog.
! ajustado por Grupo Retail - OVS 
!----------------------------------------------------
! Mod17Mzo2009
! Se controla el manejo de transacciones duplicadas.
! ajustado por Grupo Retail - OVS 
!----------------------------------------------------
! Mod 06Dic2011
! Se cambia el rango de busqueda de fechas de trx
! aumentandolo hasta los movimientos generados a 
! las 2100 horas.
! Ajustado por Grupo Retail Ltda - OVS
!-----------------------------------------------------
! Mod 14Jun2019
! Se ajusta modulo para tomar las transacciones de no 
! venta para el calculo de la fecha de cierre, solicitado
! por Comfandi.
! Ajustado por Grupo Retail Ltda - OVS
!-----------------------------------------------------
! Mod 10Mzo2022
! Se ajusta calculo fecha para que al no encontrar movimientos
! se reporte la fecha del sistema
!--------------------------------------------------------------

%ENVIRON C						   												! Ambiente de controlador

%INCLUDE POSPVARI.BAS					                  ! Variables del programa

String GLOBAL UE.AUTORIZA$, DATO.SO$, SERIAL$, ARC$, LOG$, Vlr.Venta$, N.Cliente$, abc$
Integer*4 Global PP, TAM%, Len.REG%, Vlr.Venta%, Vlr.Iva%, CITM%, CPAG%, CDSC%, \
          NRO.REG%, X.Len%
Integer*1 Global ARC%, X.PARA%, X.CAMBIO%, X.Anul%, Xtarifa%
String    Global HORA.FINAL$, FILEOUT$, FECHA.ARCH$, Asc.Mod.DateTime$, Old.Depto$, X.lec$

%INCLUDE ADX_UPGM:DMEXTR.J86    		   ! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS		  	           ! Rutinas Comunes
%INCLUDE ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   STRING PARM2
END SUB

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   INTEGER*4 RETC
   STRING INFILE, OUTFILE
   INTEGER*2 OPT0, OPT1
End Sub 

Function ADXSTART(NOMBRE,PARAMETROS,MENSAJE) EXTERNAL 
 Integer*2 ADXSTART
 String    NOMBRE,PARAMETROS,MENSAJE
End Function

Function ADXERROR (TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
	INTEGER*2     TERM,MSGNUM
	INTEGER*1     SEVERITY,MSGGRP,EVENT
	STRING                UNIQUE$
End Function

Function GETN1(P1$,P2) EXTERNAL                       
     INTEGER*1 GETN1
     STRING P1$
     INTEGER*2 P2
End Function

Function INICIO1
  Call.ORDER% = 11                                ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
END FUNCTION
!--- Fin de la funcion de inicio

Function ENTRADA.LOG
  IF DATO.SO$ = "1" THEN Call ADXSERVE(PP,26,1,MEN$) \
   ELSE LOCATE 15,1: PRINT MEN$
  WAIT;1500
End Function 

Function BARRA.ESTADO
  INTEGER*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+STRING$(N,CHR$(219))+STRING$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"
FEND

Function TERMINE.PROG
Integer*4 A%
  Call BARRA.ESTADO
  PROCESO$ = "EN TERMINE.PROG "
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  IF DATO.SO$ = "1" THEN Call ADXSERVE(PP,26,1,MEN$) \
    ELSE BEGIN
     LOCATE 12,15: PRINT MEN$
  EndIf
  MEN$ = "Fecha de Cierre Calculada "+N.Cliente$
  IF DATO.SO$ = "1" THEN Call ADXSERVE(PP,26,1,MEN$) \
    ELSE BEGIN
     LOCATE 22,1: PRINT MEN$
  EndIf
  If Val(N.Cliente$) <= 0 Then N.Cliente$ = date$														! Sin fecha calculada toma fecha del sistema
  Create "TF:FECDIA" AS 77                         													! Archivo para transmitir
   X.Len% = Len(N.Cliente$)														  										! Toma longitud del registro
   X.Lec$ = "C"+Str$(X.len%)+" C2"										  										! Arma estructura de grabacion
   Write form X.Lec$; #77 ; N.Cliente$, Finr$      													! Graba registro
   Close 77 					   ! Cierra archivo

!  	Call ADXERROR(	0,           	\ 0 for controller
!						0,           							\ no msg displayed
!						0,           							\ no msg displayed
!						5,           							\ severity level
!						2,           							\ 2 = checkout support
!		        "CIERRE SISTEMA "+N.Cliente$)

     CALL ADXERROR( 0,                              \ 0 for controller
                   ASC("B"),                        \ msg displayed
                   007,                             \ msg number
                     3,                             \ severity level
                     2,                             \ 2 = checkout support
		               "CS"+N.Cliente$)



  Call SETF("0000000")				   !
  Call CLRSCR					   !
  RET.ERR%= CLSDIS				   !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   !
  Stop
END FUNCTION
!--- Fin de la ejecucion del programa


Function LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
      
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
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		:CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
End Function 

FUNCTION INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q                 = 1
NRO.STRINGS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero 
CITM%             = 1
CPAG%             = 1
CDSC%             = 1 
X.PARA%           = 0
X.CAMBIO%         = 0
IVA% = 0 
X.Anul% = 0 
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%, WORK2I2
   BAN.PRG$ = "0"
   OPEN B$ AS TLOG 
   If BAN.PRG$ <> "0" Then BEGIN 																							! Error apertura archivo
 	  Men$ = "Falla Open TLOG Asigna "+Date$
 	  Call Entrada.Log
  	Call ADXERROR(	0,           	\ 0 for controller
						0,           							\ no msg displayed
						0,           							\ no msg displayed
						5,           							\ severity level
						2,           							\ 2 = checkout support
		        "TLOG "+ERR )
		N.cliente$ = "0"
		TOT.TAMANO = 1
    Call Termine.Prog																												! Genera fecha de sistema 	
   EndIf
   ARCHIVO.PROC$ = B$
   TOT.TAMANO = SIZE(B$)
   If TOT.TAMANO <= 0 Then TOT.TAMANO = 1
   B$ = ""
End Function 

Function LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$
 BAN.PRG$ = "0"
 Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL														! Busca Eamtran Activo 
 If BAN.PRG$ <> "0" Then BEGIN 																								! Error apertura archivo
 	  Men$ = "Falla Open EAMCSCF1 Asigna "+Date$
 	  Call Entrada.Log
  	Call ADXERROR(	0,           	\ 0 for controller
						0,           							\ no msg displayed
						0,           							\ no msg displayed
						5,           							\ severity level
						2,           							\ 2 = checkout support
		        "CSCF1 "+ERR )
    Call Termine.Prog																												! Genera fecha de sistema 	
 EndIf
 LLAVE$ = PACK$("9998")
 Read Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,CLOSEPNT,FECHA.CIER$,RESERVED$  
 Close 37
 If DM.CODIGO$ = "2" Then Begin 
   If OSLOGNAME$="        " Then Begin 
  	    MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
        Call ADXSERVE(PP,26,1,MEN$)
        WAIT;1800
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
Call LEER.CABECERA
DM.CODIGO$ = "2" 
DATO.SO$ = COMMAND$						! Dato S.O

If Ucase$(DATO.SO$) = "VERSION" Then BEGIN 
   !Print "Calculo Fecha Movimiento Interfaces  Ver. 31/May/2017  09:10 am"
   !Print "Calculo Fecha Movimiento Interfaces  Ver. 14/Jun/2019  02:00 pm"
   Print "Calculo Fecha Movimiento Interfaces  Ver. 10/Mzo/2022  04:15 pm"
   Stop
ENDIF


If DATO.SO$ = "BACKGRND" THEN B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" THEN BEGIN 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   LOCATE 2, 4: PRINT CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   LOCATE 3, 4: PRINT CHR$(179)
   LOCATE 4, 4: PRINT CHR$(179)
   LOCATE 3,12: PRINT "****  CALCULO FECHA DE CIERRE MOVIMIENTO FISCAL     ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Junio    14 2019  G.R. ***"
   LOCATE 4, 7: PRINT CHR$(27)+"b7"
   LOCATE 4,75: PRINT CHR$(179)
   LOCATE 5, 4: PRINT CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
ENDIF ELSE DATO.SO$ = "1"
IF LEN(B$) = 0 THEN BEGIN 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
ENDIF
IF LEN(B$) = 1 THEN BEGIN 
   B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
   FECHA.CIER$ = DATE$						! Asigno fecha dia
ENDIF

End Function 

Function CUENTE.STRINGS	       	! Esta funcion cuenta primero el #string y lo
INTEGER*2 POS1%,CUENTE.STRINGS	! compara con el #reportado por SMA
STRING CARACTER
 PROCESO$ = "EN CUENTE.STRING "
 POS1% = 1
 NRO.STRINGS.FOUND = 0
 CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
 WHILE (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.STRINGS.FOUND = NRO.STRINGS.FOUND + 1
 WEND
 NRO.STRINGS.FOUND = NRO.STRINGS.FOUND - 1
 IF (NRO.STRINGS.FOUND < NRO.REG) THEN BEGIN
	MEN$ = "FALTA STRING'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
 ENDIF
 IF (NRO.STRINGS.FOUND > NRO.REG) THEN BEGIN
	MEN$ = "SOBRAN STRING'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
 ENDIF
 IF (NRO.STRINGS.FOUND = NRO.REG) THEN CUENTE.STRINGS = 1	\ Retorna 1 si es
 ELSE CUENTE.STRINGS = 0				! satisfactorio el resultado y 0
End Function						! si el resultado es fallido.

Sub GRABE.IDENTIFICADOR
 String C$, DINNER.POS$, DINNER.NEG$, Clave$, Xdummy$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0
 TOTAL.REG = TOTAL.REG + 1
 LOCATE 20,25
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" THEN Call ADXSERVE(PP,26,1,MEN$) \
  Else Begin
   Locate 12,15: PRINT MEN$
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
      WORK$  = MID$(HEX$,WORK+1,1)+WORK$    ! ADD HEX VALUE TO OUTPUT STRING
      ERRNUM = SHIFT(ERRNUM,4)              ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$                          ! Return Error Code
End Function 


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  On Error GoTo IO.ARCHIVOS
  Call INI.VAR.PROG		! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
  ARC% = 0
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  LOCATE 8,12: PRINT "Procesando .... "+ARCHIVO.PROC$
  N.Cliente$        = "0"																! Asigna Fecha del sistema

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  PROCESO$ = " PROCESO PRINCIPAL "
  Read #TLOG; LINE INAREA$
  IF END #TLOG THEN REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO + Len(INAREA$) + 2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  IF LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
     GoTo NXTRCD
  ENDIF
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))																									! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 																							! ENCONTRAR DELIMITADOR, COMA
    If (P-Q) < 3 THEN BEGIN  																								! REVISAR FALTA DE STRING
	     P = 0
	     Q = P + 1 			      																								! PREPARANDO POSICION PARA PROX. String
	     NO.VENTA = -1                 																				! Add Mod13dic2008
	     GoTo AGAIN		   																											! VUELVA A LEER OTRO REGISTRO
	  EndIf
	  If ((P-Q)-2) > 0 Then \
      B$ = MID$(INAREA$,Q+1,(P-Q)-2) Else 																 \! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
      GoTo Again
    B$ = B$+":" 																														! SUMA UN SEMI-COLON
    Q = P + 1 																															! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 																					! DETERMINA TIPO DE STRING
    If A = 0 THEN GoSub S0 : GoTo AGAIN																			! SI ES CHECKOUT TRANSACCION
    If A = 99 THEN GoSub S99 : GoTo AGAIN
    If (A < 0) OR (A > 21) THEN GOTO AGAIN
     
     On A GOSUB S1,S2,S3,S4,S5,S6,S7,S7,S9,S10, \
                S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21, S99
               
    AGAIN:
!     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
	
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSUB GETUNPK										! 
  TERMINAL$=RIGHT$(A$,4)					! CAPTURA NUMERO DE TERMINAL
  GOSUB GETUNPK										! 
  NRO.TRANS$=A$										! CAPTURA NUMERO DE TRANSACCION
  GOSUB GETUNPK										!
  FECHA$=LEFT$(A$,6)									! CAPTURA FECHA DE TRANSACCION
  HORA$ = RIGHT$(A$,4)	            						        ! CAPTURA HORA DE LA TRANSACCION
  GOSUB GETUNPK
  TIPO.TRANS=VAL(A$)		        						! CAPTURA EL TIPO DE LA TRANSACCION
!-- Comentareado 14/jun/2019 requerimento Comfandi
!  If TIPO.TRANS <> 0 Then Begin 
!     X.Anul% = -1
!     NO.VENTA = -1
!     GoTo NXTRCD
!  EndIf    

  FECHA.ARCH$=FECHA$
  IF (Val(HORA$) > 0900) And (Val(HORA$) <= 2100) Then Begin   						! Creacion archivos de salida
  	 If Val(N.Cliente$) < Val(Fecha$) Then \
        N.Cliente$ = FECHA$            						                        ! Toma fecha de movimiento del
  EndIf

  GOSUB GETUNPK
  NRO.REG=VAL(A$)  		        						! NUMERO DE STRINGS EN TODA LA TRANSACCION
  STRINGS.COMPLETOS = 0

  IF (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) THEN BEGIN 											! TIPO 2 PARA TENDER EXCHANGE Y TIPO 1 PARA TENDER CASHING ...
		NO.VENTA = 1
  ENDIF ELSE	\
	STRINGS.COMPLETOS = CUENTE.STRINGS								! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS

  GOSUB GETUNPK
  COD.OPERA$=A$  																		! NUMERO DEL OPERADOR
  GoSub GETUNPK 													  			  ! PASSWORD DEL OPERADOR
  GOSUB GETUNPK																			! Venta positiva
  GROSS.POSITIVO$=A$
  GOSUB GETUNPK																			! Venta negativa
  GROSS.NEGATIVO$=A$
  
  If STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) THEN  BEGIN			! TIPO 2 PARA TENDER EXCHANGE Y TIPO 1 PARA TENDER CASHING ...
     Call GRABE.IDENTIFICADOR
     If DUPLICADA THEN GOTO NXTRCD									! 
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

S4:
 J = 3
Return 

S5:
  J = 3
Return

S6:
  J = 3
Return

S7:
  J = 3!TAX
RETURN

S8:
  J = 3!TAX
Return

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
  J = 3
RETURN

S17:
  J = 3
RETURN

S18:
  J = 3
RETURN

S19:
  J = 3
RETURN

S20:
  J = 3
RETURN

S21:
  J = 3
RETURN

S99:
  J = 3! User Data
Return 


GETEXCPT:
  PROCESO$ = "EN GETEXCPT "
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  IF ((K-J)-1) > 0 THEN \ ! IF DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  ELSE \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BEGINNING OF NEXT FIELD
Return


!*********************************************************************
GETUNPK:
  PROCESO$ = "EN GETUNPK "
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If (K-J) > 0 Then \
     A$ = UNPACK$(MID$(B$,J,K-J)) Else \ ! UNPACK FIELD
     A$ = ""
  J = K + 1 ! POINT TO BEGINNING OF NEXT FIELD
Return
!*********************************************************************

!*********************************************************************
GETUNPK3:
  PROCESO$ = "EN GETUNPK3 "
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
Return
!*********************************************************************
GETUNPK4:
  PROCESO$ = "EN GETUNPK4 "
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = (MID$(B$,J,K-J)) ! FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
Return

!*********************************************************************
GETFLAG:
  PROCESO$ = "EN GETFLAG "
  FLAG = VAL(A$) ! CONVERT FLAG TO INTEGER
                 ! START BUILDING STRING TO CONTAIN INDIVIDUAL FLAGS
  IF (FLAG AND 00000001H) THEN A$ = "1" ELSE A$ = "0"
  FOR I = 1 TO 15
    FLAG = SHIFT(FLAG,1) ! SET UP NEXT BIT
    IF (FLAG AND 00000001H) THEN A$ = "1" + A$ ELSE A$ = "0" + A$
  NEXT I
Return

!*********************************************************************
!
REPORTE.FINAL:
        Call TERMINE.PROG
Return
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0

If ERR = "SS" THEN RESUME NXTRCD:
If ERR = "IH" THEN RESUME 

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 37 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 37  AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   BAN.PRG$ = "1"
   RESUME                                             ! retorna ejecucion
ENDIF                                                 ! del programa

If ERRF% = AREA1% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   CREATE POSFILE ARCH1$ KEYED 3,,,1000 RECL 104     \! si no existe lo crea.
          AS AREA1% 			       ! 
   BAN.PRG$ = "1"
   RESUME                                             ! retorna ejecucion
ENDIF                                                 ! del programa
IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN          \! Si encuentra eof()
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") THEN \
 BEGIN
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    BAN.PRG$ = "1"				      ! del archivo de 
    RESUME					      ! help del aplicativo
 ENDIF

IF ERRF% = TLOG   AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
     Call TERMINE.PROG
ENDIF				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$ + " PROC:"+PROCESO$
Print MEN$
Stop
!*********************************************************************
