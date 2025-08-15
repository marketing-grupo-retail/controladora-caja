!***************************************************
!Empresa : Casa Informatica Unidad Retail         *
!Programa : INTABONO.BAS                          *
!Autor : Oscar Valencia Sarmiento                 *
!Lenguaje : Basic 4690 IBM                        * 
!Fecha : JULIO 26 de 2.003                        *
!Observaciones : Generacion archivo plano con el  *
!                movimiento de Articulos comprados*
!                con bonos, para Comfandi Valle   *
!**************************************************
! Observaciones:

%ENVIRON C						   ! Ambiente de controlador

STRING GLOBAL DM.FACTURA$, FISCAL$, FILENAME$, SALIDA2$, FCLOSE$
STRING GLOBAL HORA.FINAL$, DATO.SO$
STRING GLOBAL NRO.BONO$, NRO.SEGU$, VLR.BONO$, REG.BONO$
INTEGER*1 GLOBAL IND.BONOS%

INTEGER*4 GLOBAL TMP.VALOR%, TOTAL.CONVENIOS%, TOTAL.COOPAGOS%, TOTAL.FORMULA%
REAL GLOBAL TMP.VALOR2%

%INCLUDE POSPVARI.BAS					   		   ! Variables del programa
%INCLUDE ADX_UPGM:EAMITEMR.J86			           ! Variable EAMITEMR
%INCLUDE ADX_UPGM:DMEXTR.J86    			   	   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   		   ! Rutinas Comunes
%INCLUDE ADX_UPGM:BASROUT.J86				       ! 


SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   STRING PARM2
END SUB

SUB ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   INTEGER*4 RETC
   STRING INFILE, OUTFILE
   INTEGER*2 OPT0, OPT1
END SUB

FUNCTION ENTRADA.LOG
  IF DATO.SO$ = "1" THEN CALL ADXSERVE(0,26,1,MEN$) \
   ELSE begin
     LOCATE 23,1: PRINT STRING$(78," ")
     LOCATE 23,1: PRINT "Msg : "+MEN$
   ENDIF
FEND


FUNCTION TERMINE.PROG
  MEN$ = "PROCESO TERMINADO SATISFACTORIAMENTE."
  CLOSE 23
  CALL ENTRADA.LOG
  STOP
END FUNCTION
!--- Fin de la ejecucion del programa

FUNCTION CALCULO.HORA
STRING A$, H$, M$, S$
A$ = TIME$							!
H$ = LEFT$(A$,2)						! Tomo hora
M$ = MID$(A$,3,2)						! Tomo minutos
S$ = RIGHT$(A$,2)						! Tomo segundos
M$ = STR$(VAL(M$) + 10)						! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)						!
IF VAL(M$) > 59 THEN BEGIN 					!
   M$ = STR$(VAL(M$) - 60)					!
   M$ = RIGHT$("00"+M$,2)					!
   H$ = STR$(VAL(H$) + 1)					!
   IF VAL(H$) > 23 THEN H$ = "00"				!
ENDIF								!
HORA.FINAL$ =  H$+M$+"00"					! Hora de Inicio 
END FUNCTION

FUNCTION TRX.FINAL
STRING DATA1$, DATA2$, DATA3$, DATA$
 MEN$ = "Organizando Archivo de Salida, Espere Por Favor .."
 CALL ENTRADA.LOG
 OPEN SALIDA$ AS 32
 IF FILENAME$ = "" THEN FILENAME$ = RIGHT$(DATE$,4): FCLOSE$ = "20"+DATE$
 
 SALIDA2$ = SALIDA2$ + FILENAME$ + ".IPS"
 CREATE POSFILE SALIDA2$ AS 33
 IF END #32 THEN FIN.TRX
 WHILE (1)
  READ #32;LINE DATA$
  DATA1$ = MID$(DATA$,1,148)
  DATA2$ = MID$(DATA$,157,26)
  WRITE FORM "C148 C8 C26 C2";#33; \
        DATA1$, FCLOSE$, DATA2$,FINR$
 WEND
 FIN.TRX:
  DELETE 32
  CLOSE  33
END FUNCTION
!--- Fin de organizaciion de la interface

FUNCTION LECTURA.CONTROL(PROYECTO%)  EXTERNAL     ! Lectura proyecto EPS
INTEGER*2 PROYECTO%
END FUNCTION

FUNCTION LEER.CABECERA
   BAN.PRG$ = "0"
   KEY$=PACK$("000001")                                     ! Llave cabecera control
   LEC$="C3 C4 C30 C30 C13 I2 C22"
   READ FORM LEC$;#AREA1% KEY KEY$;                        \! Lee Reg Cabecera 
        DM.LLAVE$,DM.ALMACEN$,DM.RAZON$,DM.SIGLA$,DM.NIT$, \!
        DM.NOPROY%,DM.BASURA$				    ! Control
  IF BAN.PRG$    = "1" THEN BEGIN \
     DM.RAZON$   = "NO DEFINIDO"
     DM.SIGLA$   = "NO DEFINIDO"
     DM.ALMACEN$ = "0000"
  ENDIF
END FUNCTION
!--- Fin de la funcion de lectura


FUNCTION PLANO
STRING TMP1$, F.NAME$, X.ART$, X.QTY$, X.PRIC$, X.SGN$, X.BONO$, X.DPT$
INTEGER*1 IND.TEMP%, IND.LEC%, IND.PRECIO%
INTEGER*4 X.I%
IF PRIMERA.VEZ = 1 THEN BEGIN
   PRIMERA.VEZ = 2
   F.NAME$ = "C:\ADX_UDT1\A"+RIGHT$(DM.ALMACEN$,3)+RIGHT$(FCLOSE$,4)+".DAT"
   LOCATE 12,15: PRINT "GENERANDO INTERFACE SALIDA "+F.NAME$
   CREATE POSFILE F.NAME$ AS 23
   WRITE FORM "C1 C4 C8 C2";#23;"1",DM.ALMACEN$,FCLOSE$,FINR$
ENDIF
IF IND.BONOS% = -1 THEN BEGIN
 FOR X.I% = 1 TO CONTADOR%
   X.ART$  = RIGHT$("000000"+ARTICULOS$(X.I%,0),6)         ! Item vendido
   X.PRIC$ = RIGHT$("0000000000"+ARTICULOS$(X.I%,1),10)    ! Precio
   X.QTY$  = RIGHT$("000000"+ARTICULOS$(X.I%,2),6)         ! Cantidad
   X.DPT$  = RIGHT$("000"+ARTICULOS$(X.I%,2),3)            ! Departamento
   X.BONO$ = NRO.BONO$ +  NRO.SEGU$                        ! Bono
   IF VAL(X.QTY$) >= 0 THEN X.SGN$ = "+" ELSE X.SGN$ = "-" ! Signo
   WRITE FORM "C1 C6 C3 C1 C6 C10 C11 C2";#23;   \!
         "2",          \! Tipo de registro
         X.ART$,       \! Codigo del articulo		 
         X.DPT$,       \! Departamento del articulo		 		 
         X.SGN$,       \! Signo trx
         X.QTY$,       \! Cantidad del articulo		 		 
         X.PRIC$,      \! Precio del articulo		 		 
		 X.BONO$,      \! Numero de bono
		 FINR$          ! Fin de registro
 NEXT X.I%
ENDIF
DIM ARTICULOS$(700,3)
END FUNCTION
!--- Fin de la generacion del plano

FUNCTION INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
  IND.BONOS% = 0
  TOTAL.CONVENIOS% = 0
  TOTAL.COOPAGOS%  = 0
  TOTAL.FORMULA% = 0
  DM.CAJA$=""
  DM.TRX$=""
  FISCAL$ = ""
  INP2$="0"
  CONTADOR% = 0				! Inicializo contador
  NRO.ERROR = 0
  BARRA% = 1
  UE.IND$ = "0"
  UE.IND2$ = "0"
  UE.IND3$ = "0"
  TOTAL.REG = 0
  CLIENTE.FREC = 0
  GRAN.TOTAL = 0
  ACUMULADOR% = 0
  TRANS.AUTONOMO = 0
  TRANS.NORMAL = 0
  CNTREG% = 1
  TOT.DEPTOS = 0
  NETMSC = 0
  TOT.TERM = 0
  TERM.INI = 0
  NUM.SEP$ = ","
  PRIMERA.VEZ = 1
  DIM IVA$(8)
  DIM ARTICULOS$(700,3)
  DM.CAJA$ = "0"
  DM.TRX$  = "0"
  FINR$=CHR$(13)+CHR$(10)
		     !  NUMEROS   DE  SESIONES
  TLOG = 25		:CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		:LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
  !ITEM.REC    = 32
FEND

FUNCTION INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q=1
CONTADOR% = 0				! Inicializo contador
NRO.STRINGS.FOUND = 0
NRO.ARTICULOS = 0
SW.CLIENTE = 0
  UE.IND$ = "0"
  UE.IND2$ = "0"
NO.VENTA = 0
SW.MISC = 0
DEPTO% = 0
UE.INDTRX$="1"
DEPTO$ = ""
NRO.REG = 0      ! Control del header a cero 
CONTADOR$ = "0"
FEND

FUNCTION APERTURA.ARCHIVOS.PRINC
!  OPEN B$ AS TLOG BUFFSIZE 51200 NOWRITE NODEL

  OPEN B$ AS TLOG NOWRITE NODEL
  ARCHIVO.PROC$ = B$
  TOT.TAMANO = SIZE(B$)
  B$ = ""
FEND

FUNCTION LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
STRING LLAVE$
!+++++++++++++++++++++++ Leemos cual transaction previo se procesa 
OPEN "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL	! Busca Eamtran Activo 
LLAVE$ = PACK$("9998")
READ FORM "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,CLOSEPNT,FECHA.CIER$,RESERVED$  
CLOSE 37
IF OSLOGNAME$="        "   THEN BEGIN 
  	MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
    CALL ENTRADA.LOG
	WAIT;1200
	B$ = SLOGNAME$ 
   ENDIF ELSE B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO

TOT.TAMANO = SIZE(B$)

FEND

FUNCTION PANTALLA.PRINCIPAL
INTEGER*1 ARC%
CALL CALCULO.HORA						                    ! Calcula hora inicio de programa
AREA1% = 11: AREA2% = 12: AREA3%=13: AREA4%=14              ! Definicion area de trabajo archivo
AREA5% = 15
FIN$="0"						    ! Variable control creacion archivo
ARCH1$="CONTROL"					    ! Asignar nombre archivo control
OPEN ARCH1$ KEYED RECL 104 AS AREA1% 			    ! Apertura control
!CALL LEER.CABECERA				            !

DATO.SO$ = COMMAND$						! Dato S.O

IF DATO.SO$ = "BACKGRND" THEN BEGIN
   ARC% = 0
   MEN$ = "Inicio Proceso a las "+HORA.FINAL$
   CALL ADXSERVE(0,26,1,MEN$)
   WHILE ARC% = 0
    IF TIME$ = HORA.FINAL$ THEN \  
       ARC% = 1
   WEND
   ARC% = 0
ENDIF

IF DATO.SO$ = "BACKGRND" THEN DATO.SO$ =""
B$ = DATO.SO$					  		!Toma parametro del sistema operativo
IF DATO.SO$ <> "BACKGRND" THEN BEGIN 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   LOCATE 2, 4: PRINT CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   LOCATE 3, 4: PRINT CHR$(179)
   LOCATE 4, 4: PRINT CHR$(179)
   LOCATE 3,12: PRINT "****  INTERFACE MODULO ARTICULOS X BONOS  Ver. 1.00 ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Agosto 29 de 2003 CISA  ***"
   LOCATE 4, 7: PRINT CHR$(27)+"b7"
   LOCATE 4,75: PRINT CHR$(179)
   LOCATE 5, 4: PRINT CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
ENDIF ELSE DATO.SO$ = "1"
IF LEN(B$) = 0 THEN BEGIN 					! Dia previo
   CALL LEA.REG.TRANS.SUM.LOG               ! DUELVE ARCHIVO PREVIO  
ENDIF
IF LEN(B$) = 1 THEN BEGIN 
   B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
ENDIF
LOCATE 10,15: PRINT "PROCESANDO : "+B$
END FUNCTION 


FUNCTION CUENTE.STRINGS		! Esta funcion cuenta primero el #string y lo
INTEGER*2 POS1%,CUENTE.STRINGS	! compara con el #reportado por SMA
STRING CARACTER
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
	CALL ENTRADA.LOG
	WAIT;1200
ENDIF
IF (NRO.STRINGS.FOUND > NRO.REG) THEN BEGIN
	MEN$ = "SOBRAN STRING'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	CALL ENTRADA.LOG
	WAIT;1200
ENDIF
IF (NRO.STRINGS.FOUND = NRO.REG) THEN CUENTE.STRINGS = 1	\ Retorna 1 si es
ELSE CUENTE.STRINGS = 0				! satisfactorio el resultado y 0
FEND						! si el resultado es fallido.

FUNCTION NEGATIVO(CADENA.NEG)	! CONVIERTE UN STRING A NEGATIVO
INTEGER*1 IL
STRING CADENA.NEG,NEGATIVO
IL=LEN(CADENA.NEG)
NEGATIVO="-"+RIGHT$(CADENA.NEG,IL-1)
FEND

FUNCTION BARRA.ESTADO
  INTEGER*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+STRING$(N,CHR$(219))+STRING$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"
FEND

FUNCTION GRABE.IDENTIFICADOR

 STRING C$, DINNER.POS$, DINNER.NEG$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0

TOTAL.REG = TOTAL.REG + 1
CALL BARRA.ESTADO
TOTAL.REG = TOTAL.REG + 1
MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
CALL ENTRADA.LOG
FEND

!--------------
!--- Programa Principal
!--------------

  ON ERROR GOTO IO.ARCHIVOS
  CALL INI.VAR.PROG					    ! Inicializamos Variables
  OPEN "C:\ADXALMA.DAT" AS 44
  READ #44;LINE DM.ALMACEN$
  CLOSE 44
  FCLOSE$ = MID$(DM.ALMACEN$,5,8)        ! Fecha de cierre
  DM.ALMACEN$ = LEFT$(DM.ALMACEN$,4)     ! Numero de almacen
  CALL PANTALLA.PRINCIPAL				    ! Captura de datos
  CALL APERTURA.ARCHIVOS.PRINC				    ! Apertura de archivos

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  CALL INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  PROCESO$ = " PROCESO PRINCIPAL "
  READ #TLOG; LINE INAREA$                      ! Lectura log de transacciones
  IF END #TLOG THEN REPORTE.FINAL               ! Si encuentra final del log
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  IF LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
	  GOTO NXTRCD
  ENDIF
  INAREA$ = INAREA$ + ","
  WHILE (Q < LEN(INAREA$))			! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 			! ENCONTRAR DELIMITADOR, COMA
    IF (P-Q) < 3 THEN \ 			! REVISAR FALTA DE STRING
    	BEGIN
	  P=0
	  MEN$ ="FALTA STRING ID TRANSACCION"
	  CALL ENTRADA.LOG
	  WAIT;1200
	  Q=P+1 				! PREPARANDO POSICION PARA PROX. STRING
	  GOTO AGAIN		   		! VUELVA A LEER OTRO REGISTRO
	ENDIF
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 		! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y "
    B$ = B$+":" 				! SUMA UN SEMI-COLON
    Q = P + 1 					! PREPARA POSC. PARA NUEVO DATO
    A = VAL(UNPACK$(LEFT$(B$,1))) 		! DETERMINA TIPO DE STRING
    IF A = 0 THEN GOSUB S0:GOTO AGAIN		! SI ES CHECKOUT TRANSACCION
    IF A = 99 THEN GOSUB S99:GOTO AGAIN
    IF (A < 0) OR (A > 21) THEN GOTO AGAIN
    IF (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) THEN \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
	IF (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
		CANT.PESO$ = "000000000" 
          	SW.ITEM = 0
       	ENDIF
    IF (A > 3) AND (SW.ITEM = 1) THEN \		!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	IF (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
          	SW.ITEM = 0
       	ENDIF
    ON A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
  AGAIN:
  IF (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
  IF NO.VENTA THEN GOTO NXTRCD			! Si no es una venta leemos otro registro
  WEND
  IF DM.CAJA$ <> TERMINAL$ OR DM.TRX$ <> NRO.TRANS$ THEN BEGIN
     CALL PLANO
	 IND.BONOS% = 0
     DM.CAJA$   = TERMINAL$			
     DM.TRX$    = NRO.TRANS$
  ENDIF

GOTO NXTRCD					! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO
  TERMINAL$=A$       		! CAPTURA NUMERO DE TERMINAL
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO Y ASI.......
  NRO.TRANS$=A$				! CAPTURA NUMERO DE TRANSACCION
  GOSUB GETUNPK
  FECHA$=LEFT$(A$,6)			! CAPTURA FECHA DE TRANSACCION
  IF DM.CODIGO$ <> "2" THEN FECHA.CIER$ = FECHA$
  HORA$ = RIGHT$(A$,4)+"00"		! CAPTURA HORA DE LA TRANSACCION

!  IF VAL(LEFT$(HORA$,2)) >= 12 AND VAL(LEFT$(HORA$,2)) <= 15 THEN BEGIN
!     FILENAME$ = RIGHT$(FECHA$,4)
!	 FCLOSE$ = "20"+FECHA$
!  ENDIF

  GOSUB GETUNPK
  TIPO.TRANS=VAL(A$)			! CAPTURA EL TIPO DE LA TRANSACCION
  GOSUB GETUNPK
  NRO.REG=VAL(A$)  			! NUMERO DE STRINGS EN TODA LA TRANSACCION
  STRINGS.COMPLETOS = 0
  IF (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) THEN BEGIN 	! TIPO 2 PARA TENDER EXCHANGE Y TIPO 1 PARA TENDER CASHING ...
		SW.CLIENTE = 0  	! APAGAR INDIC SI NO VENDE 
		NO.VENTA = 1
  ENDIF ELSE	\
	STRINGS.COMPLETOS = CUENTE.STRINGS	! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS
  GOSUB GETUNPK
  COD.OPERA$=RIGHT$(STRING$(8,"0")+A$,8) 	! NUMERO DEL OPERADOR
  GOSUB GETUNPK
  			!  PASSWORD DEL OPERADOR
  GOSUB GETUNPK		! Valor de la transaccion
  GROSS.POSITIVO$ = A$
  GOSUB GETUNPK
  GROSS.NEGATIVO$=RIGHT$(STRING$(12,"0")+A$,12)
  IF VAL(GROSS.POSITIVO$) = 0 THEN UE.INDTRX$ = "3"
  IF VAL(GROSS.NEGATIVO$) > VAL(GROSS.POSITIVO$) THEN \
     UE.IND2$ = "1":UE.INDTRX$ = "3"
  GOSUB GETUNPK
  !	RING.TIME = VAL(A$)	! TIEMPO DURACION DE TRANSACCION
  GOSUB GETUNPK
				! TENDER TIME= ",VAL(A$) TIEMPO DE PAGO
  GOSUB GETUNPK
  !	SPECIAL.TIME  = VAL(A$)	! TIEMPO EN SING-OFF
  GOSUB GETUNPK
  !	INACTIVE.TIME = VAL(A$)	! TIEMPO INACTIVA
  GOSUB GETUNPK
  IF STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) THEN  BEGIN	! TIPO 2 PARA TENDER EXCHANGE Y TIPO 1 PARA TENDER CASHING ...
  !++++++++++++++++++++++++++++ CAMPOS LISTOS PARA GRABAR  ++++++++++++++
     CALL GRABE.IDENTIFICADOR
     IF DUPLICADA THEN GOTO NXTRCD	! 
  ENDIF
RETURN

S1:
  J = 3
  CONTADOR% = CONTADOR% + 1
  GOSUB GETUNPK
  COD.ITEM$=RIGHT$(STRING$(12,"0")+A$,12) 	! CODIGO PLU O EAN
  GOSUB GETUNPK
  PRECIO$ = RIGHT$(STRING$(10,"0")+A$,10)	! PRECIO DEL ARTICULO
  CANTIDAD$ = "00001"				        ! INICIALIZAMOS CANTIDAD
  GOSUB GETUNPK
  DEPTO$ = RIGHT$("000"+A$,3)	 		    ! DEPARTAMENTO DEL ARTICULO
  DEPTO% = VAL(DEPTO$)	
  GOSUB GETUNPK                 ! Family number
  GOSUB GETUNPK					! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  GOSUB GETUNPK					! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = VAL(A$) 
  IF (INDICAT2 AND 0080H) THEN CANTIDAD$ = STR$(VAL(CANTIDAD$) * -1)
  IF (INDICAT2 AND 0040H) THEN CANTIDAD$ = STR$(VAL(CANTIDAD$) * -1)
  GOSUB GETUNPK					! INDICAT3   QUE VENDIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
  SW.ITEM = 1					! SE PRENDE BANDERA DE VENTA
  ARTICULOS$(CONTADOR%,0) = COD.ITEM$
  ARTICULOS$(CONTADOR%,1) = PRECIO$
  ARTICULOS$(CONTADOR%,2) = CANTIDAD$
  ARTICULOS$(CONTADOR%,3) = DEPTO$
RETURN

S2:
  J = 3     	! EXTENSION DE ENTRADA
  GOSUB GETUNPK !  NO USADO   MPGROUP    = ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   DEALQUAN   = ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   PRICE METH = ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   SALEQUAN   = ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   SALEPRIC   = ",VAL(A$)
  GOSUB GETUNPK !  CANTIDAD O PESO VENDIDO = QTYORWGT
  TVISOR$ = ARTICULOS$(CONTADOR%,2) 
  IF VAL(TVISOR$) < 0 THEN BEGIN 
     A$ = STR$(VAL(A$) * -1)
  ENDIF
  ARTICULOS$(CONTADOR%,2) = A$
  GOSUB GETUNPK !  NO UTILIZADO  INDICAT1= ",RIGHT$(A$,8)
RETURN

S3:
  J = 3
RETURN

S5:
  J = 3
RETURN

S7:
  J = 3!TAX
RETURN

S9:
  J = 3
RETURN

S10:
  J = 3
RETURN

S11:
  J = 3					! DATA ENTRY
  GOSUB GETUNPK				! Extrae dato del archivo
  IF A$ = "20030710" THEN BEGIN        \! Proyecto bonos
     GOSUB GETUNPK2			! 
     GOSUB GETUNPK2			! Numero del bono     
     NRO.BONO$=RIGHT$("00000000"+STR$(VAL(A$)),8)
     GOSUB GETUNPK2			! Valor del bono   
     VLR.BONO$=RIGHT$("000000000"+STR$(VAL(A$)),9)
     GOSUB GETUNPK2			! Forma de registro
     GOSUB GETUNPK2			! Forma de registro
     REG.BONO$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Numero de seguridad             
     GOSUB GETUNPK2			! Numero de seguridad             
     NRO.SEGU$=RIGHT$("000"+STR$(VAL(A$)),3)
	 IND.BONOS% = -1
  ENDIF
RETURN

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
 J=3
RETURN

S20:
  J = 3
RETURN

S21:
  J = 3
  GOSUB GETUNPK				! Extrae dato del archivo
  
RETURN

S99:
  J=3
RETURN

!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
RETURN
!*********************************************************************

GETUNPK2:
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
RETURN
!*********************************************************************
GETPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = MID$(B$,J,K-J) ! UNPACK FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
RETURN

REPORTE.FINAL:
	TOT.LEIDO = TOT.TAMANO
	CALL BARRA.ESTADO
	TOTAL.REG = TOTAL.REG + 1
	MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
	CALL ENTRADA.LOG
    CALL TERMINE.PROG
RETURN
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
IF ERR = "SS" THEN RESUME NXTRCD:
IF ERRF% = AREA1% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   CREATE POSFILE ARCH1$ KEYED 3,,,1000 RECL 104     \! si no existe lo crea.
          AS AREA1% 			       ! 
   BAN.PRG$ = "1"
   RESUME                                             ! retorna ejecucion
ENDIF                                                 ! del programa
IF ERRF% = AREA2% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   MEN$="Error: Archivo BNEFICIA No Existe ..."
   CALL ENTRADA.LOG
   STOP
ENDIF                                                 ! del programa
IF ERRF% = AREA3% AND                                \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   MEN$="Error: Archivo CONVENIO No Existe ..."
   CALL ENTRADA.LOG
   STOP
ENDIF                                                 ! del programa
IF ERRF% = 4 AND   	                             \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
   MEN$="Error: en apertura del Archivo EAMITEMR ..."
   CALL ENTRADA.LOG
   STOP
ENDIF                                                 ! del programa

IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !
IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !
IF ERRF% = AREA3% AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !
IF ERRF% = 4      AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !
IF ERRF% = 28     AND ERR = "OE" THEN BEGIN          \! Si encuentra fin de 
   MEN$="Error: Archivo EAMOPERA No Existe ..."
   CALL ENTRADA.LOG
   WAIT;2500
   CALL TERMINE.PROG 				      !
ENDIF				                      !
IF ERRF% = 28     AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! archivo en busqueda
   RESUME					      !
ENDIF				                      !
IF ERRF% = AREA4% AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
ENDIF				                      !
IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
   BAN.PRG$ = "1"				       ! del archivo de 
   RESUME					       ! help del aplicativo
ENDIF						       !

IF ERRF% = 23 AND ERR="OE" THEN BEGIN                 \! Valida la lectura
   MEN$="Error: CREACION interface de salida   ..."
   CALL ENTRADA.LOG
   WAIT;2500
   CALL TERMINE.PROG 				      !
ENDIF						      !

IF ERRF% = 37 AND                                    \! Validacion si existe 
   (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo EAMCSCF1
   MEN$="Error: Archivo EAMCSCF1 No Existe ..."       !
   CALL ENTRADA.LOG
   WAIT;2500
   CALL TERMINE.PROG 				      !
ENDIF
IF ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") THEN \
 BEGIN
    MEN$="Error: No Se Logr¢ Abrir Log de Transacciones "
    CALL ENTRADA.LOG
    WAIT;3000
    CALL TERMINE.PROG
 ENDIF
IF ERRF% = TLOG   AND ERR = "EF" THEN BEGIN          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     CALL ENTRADA.LOG
     WAIT;3000
     CALL TERMINE.PROG
ENDIF				                      !
CALL TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
CALL ENTRADA.LOG
STOP

!--- Fin del programa EPSP0301.BAS

