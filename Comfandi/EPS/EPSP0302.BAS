!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0303.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Enero 11 de 2.001                *
!Observaciones : Generacion archivo plano con el  *
!                movimiento de las EPS desarrolla-*
!                do para Colsubsidio              *
!**************************************************
! Observaciones:
! Se modifica el programa con base en el original para
! que pueda ser ejecutado en forma interactiva pasando como
! parametro el nombre del log de transacciones o ejecutarse
! en modalidad de background.
! Realizado en 2 de agosto de 2.002 por Oscar Valencia
!***************************************************
! Se modifica el programa de interface, tomando el 
! ajuste al cambio realizando en la transaccion como 
! descuento otorgado, realizado el 3 de septiembre 
! de 2.002 por Oscar Valencia Sarmiento.
!****************************************************
! Se modifica la interface de salida, informando el 
! ajuste al cambio en el campo de descuento y no
! restandolo de la cuota moderadora.
! solicitado por Colsubsidio el 21 de abril 2003
! desarrollado por Oscar Valencia S.
!*****************************************************
! Se corrige variable de conteo de articulos dentro de
! una transaccion, ya que solamente estaba controlando 
! hasta un maximo de 127 articulos por trx, modificado
! por Oscar Valencia 9 de Sept 2003
!------------------------------------------------------

%ENVIRON C						   ! Ambiente de controlador

STRING GLOBAL DM.FACTURA$, FISCAL$, FILENAME$, SALIDA2$, FCLOSE$
STRING GLOBAL HORA.FINAL$, DATO.SO$, CAJERO$
STRING DM.PTG1$, DM.PTG2$, DM.PTG3$, DM.PTG4$
INTEGER*4 GLOBAL TMP.VALOR%, TOTAL.CONVENIOS%, TOTAL.COOPAGOS%, TOTAL.FORMULA%
REAL GLOBAL TMP.VALOR2%, UE.AJCM%, UE.VLR.AJCM%

INTEGER*1 GLOBAL UE.IND.AJCAMBIO%

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
  
  DATA1$ = MID$(DATA$,1,175)
  DATA2$ = MID$(DATA$,184,5)
  WRITE FORM "C175 C8 C5 C2";#33; \
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

FUNCTION LEE.ITEMR(DATO1$)
STRING DATO1$
  IR.ITEMCODE$ = PACK$(RIGHT$("000000000000"+STR$(VAL(DATO1$)),12))
  BAN.PRG$ = "0"					      !
  %INCLUDE C:\ADX_UPGM\EAMIRRD1.J86                           ! Lectura del EAMITEMR     
END FUNCTION
!--- Fin de lectura del EAMITEMR

FUNCTION BUSQ.CONV(DATO$)				   ! Busqueda de planes
STRING DATO$               
  BAN.PRG$ = "0"					   !
  DATO$=PACK$(RIGHT$("000000000000"+DATO$,12))
!  LEC$="C6 C71 C5 2C1 C16"


  LEC$="C6 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 2C1"
  READ FORM LEC$;#AREA3% KEY DATO$;                    \! Lee Reg Cabecera 
       DM.LLAVE$,  DM.BASURA$, DM.BASURA$, DM.BASURA$,      \!
       DM.BASURA$, DM.BASURA$, DM.BASURA$, DM.BASURA$,      \!
       DM.BASURA$, DM.BASURA$, DM.PTG1$, DM.PTG2$,      \!
       DM.PTG3$,   DM.PTG4$,     DM.NIT$,   DM.BASURA$,      \!
       DM.FACTURA$, DM.BASURA$, DM.BASURA$,  DM.BASURA$,        \!
       DM.BASURA$,  DM.BASURA$, DM.BASURA$, DM.BASURA$,         \!
       DM.BASURA$

!  LEC$="C6 C71 C5 2C1 C18"
!  READ FORM LEC$;#AREA3% KEY DATO$;                  	  \! Lee Registro
!       DM.LLAVE$,  DM.BASURA$, DM.NIT$, DM.FACTURA$,	  \!
!       DM.BASURA$, DM.VALDIA$                             !


  IF BAN.PRG$ = "1" THEN DM.NIT$ = "0000"		   !
  IF BAN.PRG$ = "0" THEN DM.NIT$ = RIGHT$(UNPACK$(DM.NIT$),4)
END FUNCTION						   !	
!--- Fin de la funcion busqueda de convenios

FUNCTION BUSQ.AFIL(DATO1$,DATO2$)			   ! Busqueda afiliados
STRING DATO1$,DATO2$
  BAN.PRG$ = "0"					   ! Inicializa bandera 
  DATO2$ = RIGHT$("000000000000"+DATO2$,12)		   ! 12 Posiciones convenio
  DATO1$ = "0" + DATO1$					   ! 14 Posiciones beneficiario
  DATO1$ = PACK$(DATO2$ + DATO1$)			   ! Empaqueto la llave
  VISOR$ = DATO1$					   !
  LEC$="C13 C1 C52 C1 C32"				   ! Estructura registro
  READ FORM LEC$;#AREA2% KEY DATO1$;			  \! Busca beneficiario
        DM.LLAVE$, DM.TIPO$, DM.BASURA$,DM.TIPB$,         \!
        DM.BASURA$	
  IF BAN.PRG$ = "1" THEN BEGIN				   ! Si no lo encuentra
     DM.TIPO$ = "0"
     DM.TIPB$ = "0"
  ENDIF
END FUNCTION
!--- Fin busqueda de beneficiarios

FUNCTION PLANO
STRING TMP1$, UE.INDCPG$, SEP.CAMPO$, IVA.CAMP$
INTEGER*1 IND.LEC%, IND.PRECIO%
INTEGER*4 IND.TEMP%  ! Se cambia de I1 a I4 OVS 9 Sept 2003
IND.TEMP% = 1
IND.PRECIO% = 0
IF UE.IND.AJCAMBIO% = 0 THEN  UE.VLR.AJCM% = 0     ! Init variable ajuste al cambio
SEP.CAMPO$ = "|"						     ! Delimitador pipe
IF INP2$ <> "1" THEN BEGIN 					     ! No trx EPS
   DIM ARTICULOS$(500,3)					     ! Dimenciono vector articulos
   DIM ACUMMEDIO(70)						     ! Dimenciono control de pagos
   FOR I% = 1 TO 500						     !
    ARTICULOS$(I%,0) = "00"					     ! Inicializa vector con datos
    ARTICULOS$(I%,1) = ""					     ! Inicializa vector con datos
    ARTICULOS$(I%,2) = "00"					     ! Inicializa vector con datos
    ARTICULOS$(I%,3) = "00"					     ! Inicializa vector con datos
    MOVIMIENTO$(I%,1) = "00"					     !
    MOVIMIENTO$(I%,2) = "00"					     !
    MOVIMIENTO$(I%,3) = "00"					     !
    MOVIMIENTO$(I%,4) = "00"					     !
    MOVIMIENTO$(I%,5) = "00"					     !
    MOVIMIENTO$(I%,6) = "00"					     !
    MOVIMIENTO$(I%,7) = "00"					     !
	MOVIMIENTO$(I%,8) = "00"					     !
   NEXT I%							     !
   FOR I% = 1 TO 70						     ! Inicializo vector en ceros
      ACUMMEDIO(I%) = 0						     !
   NEXT I%							     !
   INP2$ = "0"							     !
   UE.IND3$ = "0"
   EXIT FUNCTION				     		     ! No dato EPS 
ENDIF
IF FIN$ = "0" THEN BEGIN					     ! Crea archivo salida CCCCaammdd.IPS
  FECHA.CIERRE$ = FECHA.CIER$					     !
  IF MID$(FECHA.CIERRE$,1,2) = "99" THEN        		    \! Manejo Y2K
   FECHA.CIERRE$ = "19"+FECHA.CIERRE$  ELSE     		    \!
   FECHA.CIERRE$ = "20"+FECHA.CIERRE$				     !
  SALIDA$  = "ADX_UDT1:TMPIPS.TMP"
  SALIDA2$ = "ADX_UDT1:"+DM.ALMACEN$
  CREATE POSFILE SALIDA$ AS 32					         !
  FIN$ = "1"						             		 !
ENDIF								     				 !
DM.VLRCON$ = RIGHT$("0000"+DM.VLRCON$,4)			     ! Codigo del Convenio
CALL BUSQ.CONV(DM.VLRCON$)				                 ! Retorna valor convenio
DM.VALIPS$ = RIGHT$("0000"+DM.VALIPS$,4)			     ! Codigo IPS
DM.VALMCA$ = RIGHT$("0000000000"+DM.VALMCA$,10)		     	     ! Numero de la formula
GROSS.POSITIVO$ = STR$(VAL(GROSS.POSITIVO$) - VAL(GROSS.NEGATIVO$))  ! Retiro basura

IF VAL(GROSS.POSITIVO$) < 0 THEN GROSS.POSITIVO$ = STR$(VAL(GROSS.POSITIVO$)*-1)

DM.VALOR$  = RIGHT$("000000000"+GROSS.POSITIVO$,9)		     ! Valor de la formula
DM.CAJA$ = UE.OLDCAJA$						     ! Numero de caja
DM.TRX$  = UE.OLDTRX$						     ! Numnero de transaccion
DM.VALMET$ = RIGHT$("0000000000000"+DM.VALMET$,13)		     ! Identificacion beneficiario
DM.VALMED$ = RIGHT$("000000000000"+DM.VALMED$,12)		     ! Registro medico
CAJERO$    = RIGHT$("00000"+CAJERO$,5)				     ! Codigo del cajero
CALL BUSQ.AFIL(DM.VALMET$,DM.VLRCON$)				     ! Busca beneficiario
UE.INDCPG$ = "0"
UE.CUOTA$    = STR$(ACUMMEDIO(T%))				     ! Tomo valor de la EPS

!IF VAL(CONTADOR$) > 0 THEN DM.VALOR$ = CONTADOR$ ELSE               \!
!   DM.VALOR$ = "0"      					     !

FOR I% = 1 TO 500						     ! Recorrer articulos
  UE.ARTICULO$ = ARTICULOS$(I%,0)				     ! Asigna articulo vendido
  IF UE.ARTICULO$ <> "00" THEN BEGIN				     ! Si existe articulo
     TMP1$ = "0"						     !
     DM.CAJA$     = RIGHT$("00"+DM.CAJA$,2)			     ! Numero de terminal
     DM.TRX$      = RIGHT$("0000"+DM.TRX$,4)			     ! Numero de trx
     UE.PRECIO$   = STR$(VAL(ARTICULOS$(I%,1)))			     ! Precio de venta
     TVISOR$      = ARTICULOS$(I%,2) 			     
     UE.CANTIDAD$ = STR$(VAL(ARTICULOS$(I%,2)))			     ! Cantidad Vendida
     IVA.CAMP$    = RIGHT$("00"+ARTICULOS$(I%,3),2)		     ! Toma iva del producto
     IF VAL(UE.CANTIDAD$) < 0 THEN BEGIN			     ! Devolucion
        UE.INDTRX$ = "3"					     ! Cambio indicador trx
        TMP1$ = "1"						     !
        IND.PRECIO% = -1					     !
        UE.CANTIDAD$ = STR$(VAL(UE.CANTIDAD$) * -1)		     !
     ENDIF							     !
     UE.CANTIDAD$ = RIGHT$("00000"+UE.CANTIDAD$,5)		     ! Formateo la salida
     UE.PRECIO$   = RIGHT$("00000000"+UE.PRECIO$,8)		     ! Formateo la salida
     UE.ARTICULO$ = RIGHT$("000000000000"+UE.ARTICULO$,12)	     ! Plu vendido
     BAN.PRG$ = "0"
     CALL LEE.ITEMR(UE.ARTICULO$)				     ! Busca Precio Real
     IF BAN.PRG$ = "0" THEN BEGIN				     ! Found producto
      IF IR.INDICAT2$ = PACK$("05") THEN BEGIN			     ! Codigo de barra
        IND.LEC% = 0						     ! Busqueda producto
        WHILE IND.LEC% = 0				      	     !
          UE.ARTICULO$ = UNPACK$(IR.SALEQUAN$)+UNPACK$(IR.SALEPRIC$) ! Busca articulos 
          UE.ARTICULO$ = RIGHT$("000000000000"+UE.ARTICULO$,12)	     ! Plu vendido
          CALL LEE.ITEMR(UE.ARTICULO$)				     ! Busca articulo
           IF IR.INDICAT2$ <> PACK$("05") OR 		            \! metodo de precio
             BAN.PRG$ = "1" THEN IND.LEC% = 1                        ! 5
          WEND						             !
      ENDIF							     !
     ENDIF							     !

     IF VAL(CONTADOR$) > 0 THEN IR.SALEPRIC$ = CONTADOR$ ELSE       \! Tomo vlr 
        IR.SALEPRIC$ = "0000000"				     ! Dscto otorgado
!--- Lineas adicionadas
!--- para manejo de ajuste al cambio OVS 21-abr-03
!---
     IR.SALEPRIC$ = STR$(VAL(IR.SALEPRIC$) + UE.VLR.AJCM%)
     IR.SALEPRIC$ = RIGHT$("0000000"+IR.SALEPRIC$,7)		 ! Ajusta formato de salida
     MOVIMIENTO$(IND.TEMP%,0) = IR.SALEPRIC$  			     !Descuento 
     MOVIMIENTO$(IND.TEMP%,1) = UE.ARTICULO$ 			     !Plu articulo
     MOVIMIENTO$(IND.TEMP%,2) = UE.CANTIDAD$ 			     !Cantidad Entregada
     MOVIMIENTO$(IND.TEMP%,8) = IVA.CAMP$                    !Ptg iva cobrado
     IF VAL(UE.PRECIO$) < 0 THEN BEGIN 				         !
        UE.PRECIO$   = STR$(VAL(UE.PRECIO$) * -1) 		     ! Valor a positivo
        UE.PRECIO$   = RIGHT$("00000000"+UE.PRECIO$,8)		 ! Formateo la salida
     ENDIF							     !
     MOVIMIENTO$(IND.TEMP%,3) = UE.PRECIO$ 			         !Vlr total unidades
     MOVIMIENTO$(IND.TEMP%,4) = UE.INDTRX$ 			         !Indicador Trx
     IND.TEMP% = IND.TEMP% + 1
     IF TMP1$ = "1" THEN UE.INDTRX$ = "1"			         !
 ENDIF
NEXT I%						 	             !

DM.VALOR$    = STR$(VAL(DM.VALOR$) - VAL(CONTADOR$) + UE.VLR.AJCM%)  ! Vlr factura
UE.COOPAGO$  = STR$(VAL(DM.VALOR$) - VAL(UE.CUOTA$))                 ! Vlr Coopago

IF VAL(UE.CUOTA$) = 0 THEN UE.COOPAGO$ = "00"

!IF VAL(UE.COOPAGO$) < 0 THEN UE.COOPAGO$ = "00"

IF VAL(UE.COOPAGO$) < 0 THEN UE.COOPAGO$ = STR$(VAL(UE.COOPAGO$)*-1)

! Se comentarean las lineas para que respete siempre la 
! cuota moderadora, 21 abr 2003

!IF VAL(CONTADOR$) > 0 THEN 			\!
!   UE.COOPAGO$=STR$(VAL(UE.COOPAGO$) - VAL(CONTADOR$))		     !
   
   
UE.CUOTA$    = RIGHT$("000000000"+UE.CUOTA$,9)		             ! Formateo salida
UE.COOPAGO$  = RIGHT$("0000000"+UE.COOPAGO$,7)                       ! Formateo salida
DM.DIA01$ = "0000000"					    	     !
DM.DIA02$ = "0000000"					             !
DM.DIA03$ = "0000000"					             !
!--- Lineas modificadas
IF VAL(UNPACK$(DM.PTG1$)) = VAL(UE.COOPAGO$) THEN DM.DIA01$ = UE.COOPAGO$                    ! Organiza el coopago
IF VAL(UNPACK$(DM.PTG2$)) = VAL(UE.COOPAGO$) THEN DM.DIA01$ = UE.COOPAGO$                    ! Organiza el coopago
IF VAL(UNPACK$(DM.PTG3$)) = VAL(UE.COOPAGO$) THEN DM.DIA01$ = UE.COOPAGO$                    ! Organiza el coopago

!IF VAL(DM.FACTURA$) = "1" THEN DM.DIA02$ = UE.COOPAGO$		     ! Dependiendo de la 
!IF VAL(DM.FACTURA$) = "2" THEN DM.DIA03$ = UE.COOPAGO$		     ! parametrizacion dada

IF VAL(DM.VALOR$) < 0 THEN DM.VALOR$ = STR$(VAL(DM.VALOR$) * -1 )

DM.VALOR$    = RIGHT$("000000000"+DM.VALOR$,9)		    	     ! Valor de la formula
UE.VLR.AJCM% = 0     ! Init variable ajuste al cambio
FOR I% = 1 TO (IND.TEMP% - 1)

     LEC$="C4 C1 C4 C1 C4 C1 C10 C1 C4 C1 C9 C1 C7 C1 C7 C1 C7 C1 C7 C1 C9 C1 C12 C1 C5 C1 C8 C1 C2 C1 C13 C1 C1 C1 C1 C1 C12 C1 C2 C1 C2 C1 C4 C1 C5 C1 C12 C1 C8 C1 C1 C1 C1 C1 C2"

     WRITE FORM LEC$;#32;DM.NIT$,	\! Convenio
			 SEP.CAMPO$,\
  			 DM.VALIPS$,	        \! IPS
  			 SEP.CAMPO$,\
			 DM.VLRCON$,	        \! Plan
			 SEP.CAMPO$,\
             DM.VALMCA$,	        \! Formula
			 SEP.CAMPO$,\
   			 DM.ALMACEN$,	        \! Centro Costo
			 SEP.CAMPO$,\
			 DM.VALOR$,	            \! Vlr formula
			 SEP.CAMPO$,\
             DM.DIA01$,            \! Cuota tipo 0
  		     SEP.CAMPO$,\
             DM.DIA02$,            \! Cuota tipo 1
			 SEP.CAMPO$,\
             DM.DIA03$,            \! Cuota tipo 2
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,0),    \! Dscto Otorgado
			 SEP.CAMPO$,\
			 UE.CUOTA$,	           \! Vlr cobro EPS
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,1),    \! Plu articulo
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,2),    \! Cantidad Entregada
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,3),    \! Vlr total unidades
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,8),    \! Iva Facturado
             SEP.CAMPO$,\
  			 DM.VALMET$,	       \! Beneficiario
			 SEP.CAMPO$,\
             DM.TIPO$,	           \! Tipo documento
			 SEP.CAMPO$,\
             DM.TIPB$,	           \! Tipo beneficiario
			 SEP.CAMPO$,\
			 DM.VALMED$,	       \! Medico
			 SEP.CAMPO$,\
	         DM.DIAGNOS$,	       \! Diagnostico
			 SEP.CAMPO$,\
			 DM.CAJA$,	           \! Terminal POS
			 SEP.CAMPO$,\
			 DM.TRX$,	           \! Numero trx
			 SEP.CAMPO$,\
             CAJERO$, 	           \! Codigo del cajero
			 SEP.CAMPO$, \			 
             FISCAL$,              \! Numero factura fiscal
             SEP.CAMPO$,\
			 FECHA.CIERRE$,	       \! Fecha movimiento
			 SEP.CAMPO$,\
             MOVIMIENTO$(I%,4),    \! Indicador Trx
			 SEP.CAMPO$,\
		     "N",		           \! Dato fijo
			 SEP.CAMPO$,\
   			 FINR$		            ! Fin de registro

NEXT I%						 	             !
DIM ARTICULOS$(500,3)						     ! Dimenciono vector articulos
DIM ACUMMEDIO(70)						     ! Dimenciono control de pagos
FOR I% = 1 TO 500						     !
    ARTICULOS$(I%,0) = "00"					     ! Inicializa vector con datos
    ARTICULOS$(I%,1) = ""					     ! Inicializa vector con datos
    ARTICULOS$(I%,2) = "00"					     ! Inicializa vector con datos
    ARTICULOS$(I%,3) = "00"					     ! Inicializa vector con datos
    MOVIMIENTO$(I%,1) = "00"					     !
    MOVIMIENTO$(I%,2) = "00"					     !
    MOVIMIENTO$(I%,3) = "00"					     !
    MOVIMIENTO$(I%,4) = "00"					     !
    MOVIMIENTO$(I%,5) = "00"					     !
    MOVIMIENTO$(I%,6) = "00"					     !
    MOVIMIENTO$(I%,7) = "00"					     !
NEXT I%								     !
FOR I% = 1 TO 70						     ! Inicializo vector en ceros
   ACUMMEDIO(I%) = 0						     !
NEXT I%								     !
INP2$ = "0"							     !
UE.IND3$ = "0"
END FUNCTION
!--- Fin de la generacion del plano

FUNCTION INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
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
  FINR$=CHR$(13)+CHR$(10)
		     !  NUMEROS   DE  SESIONES
  TLOG = 25		:CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		:LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
  !ITEM.REC    = 32
END FUNCTION 
!--- Fin inicializacion de variables

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
END FUNCTION 
!--- Fin inicializan variables

FUNCTION APERTURA.ARCHIVOS.PRINC
  OPEN B$ AS TLOG BUFFSIZE 51200 NOWRITE NODEL
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
ARCH2$="CONVENIO"					    ! Archivo de Convenios
ARCH3$="BNEFICIA"					    ! Archivo de Beneficiarios
ARCH4$="EAMITEMR"					    ! Maestro de articulos
OPEN ARCH1$ KEYED RECL 104 AS AREA1% 			    ! Apertura control
OPEN ARCH3$ KEYED RECL  99 AS AREA2%                        ! Abre archivo de Beneficiarios
OPEN ARCH2$ KEYED RECL 102 AS AREA3%                        ! Abre archivo de Planes
OPEN ARCH4$ KEYED RECL  46 AS 4				    ! Abre maestro articulos
CALL LEER.CABECERA				            !

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
   LOCATE 3,12: PRINT "****  INTERFACE MODULO EPS / CONVENIOS    Ver. 2.03 ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Sept. 09 de 2003  CISA  ***"
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

FUNCTION CARGA.IVA
  OPEN "TABLAIVA" AS 3 NOWRITE NODEL
  IF END #3 THEN FIN
  IVA$(0) = "00"
  FOR I% = 1 TO 8		! HASTA 8 TIPOS DE IVA...
      READ #3;LINE IVA$(I%)
  NEXT I%
  FIN:
  CLOSE 3
END FUNCTION

!--------------
!--- Programa Principal
!--------------

  ON ERROR GOTO IO.ARCHIVOS
  CALL INI.VAR.PROG					    ! Inicializamos Variables
  CALL CARGA.IVA
  DIM UE.PROYECTO(10)					    ! Dimensiono proyectos
  DIM ARTICULOS$(500,3)			 		    ! Dimenciono vector articulos
  DIM ACUMMEDIO(70)					    ! Control de medios de pago
  DIM MOVIMIENTO$(500,8)				    ! Movimiento de productos
  FOR I% = 1 TO 500					    !
     ARTICULOS$(I%,0) = "00"				    ! Inicializa vector con dato "00"
     ARTICULOS$(I%,1) = ""				    ! Inicializa vector con dato "00"
     ARTICULOS$(I%,2) = "00"				    ! Inicializa vector con dato "00"
     ARTICULOS$(I%,3) = "00"				    ! Inicializa vector con dato "00"
  NEXT I%						    !
  FOR I% = 1 TO 70					    ! Inicializo vector en ceros
   ACUMMEDIO(I%) = 0					    !
  NEXT I%						    !
  CALL LECTURA.CONTROL(4)				    ! Lectura Parametros Proyecto EPS
  DM.BASURA$ = PAR.PROY$(4,2) + PAR.PROY$(4,3)              ! Tomo Parametrizacion
  T% = VAL(DM.BASURA$)			    		    ! Asigno tipo,variedad EPS configurada
  DM.BASURA$= PAR.PROY$(5,2)				    ! Tomo grupo de descuento
  UE.DSCTO% = VAL(DM.BASURA$)				    !
  CALL LECTURA.CONTROL(1)						! Proyecto ajuste al cambio
  DM.BASURA$= PAR.PROY$(1,2)				    ! Tomo grupo de descuento
  UE.AJCM% = VAL(DM.BASURA$)				    ! Grupo descuento ajuste al cambio
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
     DM.CAJA$ = TERMINAL$			
     DM.TRX$  = NRO.TRANS$
     CAJERO$  = COD.OPERA$
     UE.IND.AJCAMBIO% = 0
  ENDIF
GOTO NXTRCD					! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO
  TERMINAL$=RIGHT$(A$,2)		! CAPTURA NUMERO DE TERMINAL
  GOSUB GETUNPK				! SIGUE AL PROXIMO CAMPO Y ASI.......
  NRO.TRANS$=A$				! CAPTURA NUMERO DE TRANSACCION
  GOSUB GETUNPK
  FECHA$=LEFT$(A$,6)			! CAPTURA FECHA DE TRANSACCION
  IF DM.CODIGO$ <> "2" THEN FECHA.CIER$ = FECHA$
  HORA$ = RIGHT$(A$,4)+"00"		! CAPTURA HORA DE LA TRANSACCION
  IF VAL(LEFT$(HORA$,2)) >= 12 AND VAL(LEFT$(HORA$,2)) <= 14 THEN BEGIN
     FILENAME$ = RIGHT$(FECHA$,4)
	 FCLOSE$ = "20"+FECHA$
  ENDIF
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
  CANTIDAD$ = "00001"				! INICIALIZAMOS CANTIDAD
  GOSUB GETUNPK
  DEPTO$ = RIGHT$("0000"+A$,4)	 		! DEPARTAMENTO DEL ARTICULO
  DEPTO% = VAL(DEPTO$)	
  GOSUB GETUNPK
  FAMILYNU1 = LEFT$(A$,3)			! CUPON FAMILIAR AUN NO USADO
  FAMILYNU2 = RIGHT$(A$,3)			! CUPON FAMILIAR AUN NO USADO
  GOSUB GETUNPK					! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  IVA% = 0
  PESO = 0
  INDICAT1 = VAL(A$)
  UE.INDI1% = INDICAT1                                 ! Discriminaci¢n Iva.
  IF (UE.INDI1% AND 01H) THEN                         \!
      UE.BIN$ = "1" ELSE UE.BIN$ = "0"                 !
  FOR I = 1 TO 7                                       !
       UE.INDI1% = SHIFT(UE.INDI1%,1)                  ! Inicializa sig. BIT
       IF (UE.INDI1% AND 01H) THEN UE.BIN$="1"+UE.BIN$\!
           ELSE UE.BIN$ ="0"+UE.BIN$                   !
  NEXT I                                               !
   IF LEFT$(UE.BIN$,4) = "1000" THEN                  \! Si hay Indic Plan A
                      IVA% = 1		      	      \! indica Plan = A
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "0100" THEN                  \! Si hay Indic Plan B
                      IVA% = 2		              \! indica Plan = B
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "0010" THEN                  \! Si hay Indic Plan C
                      IVA% = 3		              \! indica Plan = C
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "0001" THEN                  \! Si hay Indic Plan D
                      IVA% = 4		              \! indica Plan = D
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "1100" THEN BEGIN            \! Si hay Indic Plan AB
                      IVA% = 5		               ! indica Plan = E
   ENDIF \
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "0110" THEN                  \! Si hay Indic Plan BC
                      IVA% = 6		              \! indica Plan = F
   ELSE                                               \! si no,
   IF LEFT$(UE.BIN$,4) = "0011" THEN                  \! Si hay Indic Plan CD
                      IVA% = 7		              \! indica Plan = G
   ELSE                                               \! si no,
                      IVA% = 8		               ! indica 0% 
  
  GOSUB GETUNPK					! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = VAL(A$) 
  IF (INDICAT2 AND 0080H) THEN CANTIDAD$ = STR$(VAL(CANTIDAD$) * -1)
  IF (INDICAT2 AND 0040H) THEN CANTIDAD$ = STR$(VAL(CANTIDAD$) * -1)

!  IF INDICAT2 = 128 THEN 		       \!
!     CANTIDAD$ = STR$(VAL(CANTIDAD$) * -1)

  GOSUB GETUNPK					! INDICAT3   QUE VENDIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
  SW.ITEM = 1					! SE PRENDE BANDERA DE VENTA
  ARTICULOS$(CONTADOR%,0) = COD.ITEM$		
  ARTICULOS$(CONTADOR%,1) = PRECIO$
  ARTICULOS$(CONTADOR%,2) = CANTIDAD$
  
  IF IVA% <> 4 THEN \
     ARTICULOS$(CONTADOR%,3) = IVA$(IVA%)		\! Tasa de iva
   ELSE \
     ARTICULOS$(CONTADOR%,3) = "99"	 
	 
RETURN

S2:
  J = 3
		! EXTENSION DE ENTRADA
  GOSUB GETUNPK !  NO USADO   MPGROUP= ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   DEALQUAN= ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   PRICE METH= ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   SALEQUAN= ",VAL(A$)
  GOSUB GETUNPK !  NO USADO   SALEPRIC= ",VAL(A$)
  GOSUB GETUNPK !  CANTIDAD O PESO VENDIDO = QTYORWGT
  TVISOR$ = ARTICULOS$(CONTADOR%,2) 
  IF VAL(TVISOR$) < 0 THEN BEGIN 
     A$ = STR$(VAL(A$) * -1)
  ENDIF
!  ARTICULOS$(CONTADOR%,2) = STR$(VAL(A$) + VAL(ARTICULOS$(CONTADOR%,2)) - 1)

  ARTICULOS$(CONTADOR%,2) = A$
  TVISOR$ = ARTICULOS$(CONTADOR%,2) 

!  ARTICULOS$(CONTADOR%,1) = STR$(VAL(PRECIO$) / VAL(A$))    ! Actualiza precio

  GOSUB GETUNPK !  NO UTILIZADO  INDICAT1= ",RIGHT$(A$,8)
RETURN

S3:
  J = 3
  GOSUB GETUNPK         !  ASIGNACION DE DESCUENTO AL GRUPO DISGROUP= VAL(A$)
  TIPO.DESCTO$ = A$
  GOSUB GETUNPK         !  NO USADO PORCENTAJE DESCUENTO DISRATE= ",VAL(A$)
  GOSUB GETUNPK         !  VALOR DEL DESCUENTO =  AMTDISCO
  IF VAL(TIPO.DESCTO$) = UE.AJCM% THEN BEGIN   ! Si ajuste al cambio
     UE.VLR.AJCM% = VAL(A$)                    ! asigno
     UE.IND.AJCAMBIO% = -1
  ENDIF
  IF VAL(TIPO.DESCTO$) = UE.DSCTO%  THEN BEGIN
     UE.IND$ = "1"
     A$ = STR$(VAL(A$))
     IF VAL(A$) > 0 THEN BEGIN
	    CONTADOR$= STR$(VAL(CONTADOR$) + VAL(A$))
		CONTADOR$ = RIGHT$(STRING$(10,"0")+CONTADOR$,10) 
     ENDIF
  ENDIF
  GOSUB GETUNPK         !  CANTIDAD LIBRE DE IMPUESTO = TAXEXEMP
RETURN

S5:
  J = 3
  IF A = 5 THEN BEGIN
     SIGNO.PAGO$ = " "
     SW.PAGO = 1
  ENDIF ELSE\
     SIGNO.PAGO$ = "-"
  GOSUB GETUNPK
			!  FORMA O TIPO DE PAGO, TENDTYPE
  FORMA.PAGO$=A$
  GOSUB GETUNPK
			!  CANTIDAD DE PAGO,  AMTTENDE
  VALOR.PAGO$ = RIGHT$(STRING$(10,"0")+A$,10)
  GOSUB GETUNPK
			!  NO USADO   CANTIDAD TENDER FEE
  GOSUB GETUNPK
			!  NUMERO DE LA CUENTA DEL CLIENTE CUST NUM
  NRO.CUENTA$ = RIGHT$(STRING$(12,"0")+A$,12)
  GOSUB GETUNPK
S5CUPON:
  TRANSPAGO = VAL(SIGNO.PAGO$+"1") 
  ACUMMEDIO(VAL(FORMA.PAGO$)) = VAL(VALOR.PAGO$)  ! 
RETURN

S7:
  J = 3!TAX
RETURN

S9:
  J = 3
  GOSUB GETUNPK
  FORMA.PAGO$ = A$
  GOSUB GETUNPK
  CAMBIO$ = RIGHT$(STRING$(10,"0")+A$,10)	!  LAS VUELTAS,   AMTCHANGE USADO POR STORINT
  VALOR.PAGO$ = A$
  SIGNO.PAGO$ = "-"
  TRANSPAGO = 0 
RETURN

S10:
  J = 3
RETURN

S11:
  J = 3					! DATA ENTRY
  GOSUB GETUNPK				! Extrae dato del archivo
  IF A$ = "20082000" THEN BEGIN        \! Proyecto EPS/CONVENIOS
     INP2$="1"
     UE.OLDCAJA$ = TERMINAL$
     UE.OLDTRX$  = NRO.TRANS$
     GOSUB GETUNPK2			! 
     GOSUB GETUNPK2			! Numero del Convenio
     DM.VLRCON$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Numero del Medico
     DM.VALMED$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Codigo de la IPS
     DM.VALIPS$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Numero de la formula medica
     DM.VALMCA$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Codigo del beneficiario
     DM.VALMET$=STR$(VAL(A$))
     GOSUB GETUNPK2			! Codigo diagnostico
     DM.DIAGNOS$=STR$(VAL(A$))
     DM.DIAGNOS$=RIGHT$("00"+DM.DIAGNOS$,2)
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
  GOSUB GETUNPK
  IF A$="21" THEN BEGIN
	GOSUB GETPK
	FISCAL$ = UNPACK$(RIGHT$(A$,4))		! Nro factura fiscal
    FISCAL$ = RIGHT$("000000000000"+FISCAL$,12)
  ENDIF
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
        CALL PLANO
        CLOSE AREA1%: CLOSE AREA2%: CLOSE AREA3%: CLOSE 4
		CLOSE 32
		CALL TRX.FINAL
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
   MEN$="Error: Archivo ADX_IDT1:DESCUENT.DAT No Existe ..."
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

