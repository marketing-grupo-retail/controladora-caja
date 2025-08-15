!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : ACUMEASY.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Septimbre 3 de 2.004             *
!Observaciones : ACUMULADOR FISCAL PARA LA DIAN   *
!                Easy Colombia                    *
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

%ENVIRON C						   												! Ambiente de controlador

%INCLUDE POSPVARI.BAS					                  ! Variables del programa

String GLOBAL UE.AUTORIZA$, DATO.SO$, SERIAL$, ARC$, LOG$, Vlr.Venta$, N.Cliente$, abc$
Integer*4 Global PP, TAM%, Len.REG%, Vlr.Venta%, Vlr.Iva%,  UE.TABLAIVA(1), CITM%, CPAG%, CDSC%, \
          NRO.REG%, VTAS.TOTALES%, VLR.DSCTO%, Vlr.Base%, ovs1%, ovs2%, Tot.Dscto%
Integer*1 Global ARC%, X.PARA%, X.CAMBIO%, X.Anul%, Xtarifa%
String    Global HORA.FINAL$, FILEOUT$, FECHA.ARCH$, Asc.Mod.DateTime$, Old.Depto$
String    Global STORELINE$, Factura$, VTA.ITEM$(2), VTA.PAGO$(2), Ctrl.Trx$, VTA.DSCT$(2)
String    Global UE.CLIENTE$, UE.PUNTOS$, UE.REDIME$, UE.TMP1$, UE.TMP2$, UE.COMPRA$
String    Global Asc.Lcl.Rrn$, Asc.Lcl.Autor$, Asc.Lcl.Fneg$,  \
                 Asc.Lcl.Rthos$, Asc.Lcl.Comer$, Asc.Lcl.Idpos$, DSCTOS$(1), TIPO.DOC$, VLR.VTA$, XX$, Cat$, \
                 GR.IRDATA$, Donacion$
Real      Global New.Iva%
String    Global Gr.Ean$, Gr.Valor$, Gr.Qty$

String    Global \          
          CUSTNUM$,     RSTAT$,     LNAME$,     FNAME$,  \!
          ALINE1$,      ALINE2$,    CITY$,      STATE$,  \!
          ZIP$,         ZIPEXIT$,   HPHONA$,    HPHONE$, \!
          BPHONA$,      BPHONE$,    DRIVER$,     HSTORE$,\!
          CUSTDEMO$,                SSNBREXT$,  SSNBR$,  \!
          GENDER$,      INCOME$,    BDATE$,     EDATE$,  \!
          FSIZE$,       CHAGE1$,    CHAGE2$,    CHAGE3$, \!
          CHAGE4$,      CHAGE5$,    CHAGE6$,    CHAGE7$, \!
          TNBRA$,       TNBRR$,     TPADJP$,    TPADJN$, \!
          LADNBR$,      LRDNBR$,    LADATE$,    LRDATE$, \!
          CDATE$,       LRCDATE$,   CUSAUT$,    ALIAS$,  \!
          REFCUS$,      LASTRC$,    STATEEXT$,  ZIPASC$, \! Segun formato
          ZIPEXTASC$,   DIVEREXT$,  USERDATA$

%INCLUDE ADX_UPGM:EAMITEMR.J86			   ! Variable EAMITEMR
%INCLUDE ADX_UPGM:DMEXTR.J86    		   ! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS		  	           ! Rutinas Comunes
%INCLUDE ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion

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

Function ADXSTART(NOMBRE,PARAMETROS,MENSAJE) EXTERNAL 
 Integer*2 ADXSTART
 String    NOMBRE,PARAMETROS,MENSAJE
End Function


  FUNCTION GETN1(P1$,P2) EXTERNAL                       
     INTEGER*1 GETN1
     STRING P1$
     INTEGER*2 P2
  END FUNCTION

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
  MEN$ = "Venta Neta "+FECHA.CIER$+" = "+Str$(VTAS.TOTALES%)
  IF DATO.SO$ = "1" THEN Call ADXSERVE(PP,26,1,MEN$) \
    ELSE BEGIN
     LOCATE 22,1: PRINT MEN$
  EndIf
  Print "VALOR BASE "+STR$(OVS2%)
  Print "VALOR IVA  "+STR$(OVS1%)
  Call SETF("0000000")				   !
  Call CLRSCR					   !
  RET.ERR%= CLSDIS				   !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   !
  Delete 38
  wait ; 500
  A% = ADXSTART("ADX_UPGM:INFOEASY.286",FECHA.CIER$,"Actualizacion Recaudos "+FECHA.CIER$)
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

Sub CARGA.IVA                                                ! Carga las tarifas de IVA
integer*4 UE.PORCENTAJE, UE.LINEA
   Dim  UE.TABLAIVA(10)
   Open "ADX_IDT1:TABLAIVA.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarIfas de IVA
   If End # 91 Then  CONT.IVA                                ! Recorre el archivo
   LEER.IVA:
     READ # 91 ;  UE.PORCENTAJE
     If UE.PORCENTAJE <> 0 Then \
       Begin
         UE.LINEA = UE.LINEA + 1
         UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
       Endif
   GoTo LEER.IVA
   CONT.IVA:
    CLOSE 91
   
End Sub 

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
  Ctrl.Trx$      = ""
  VTAS.TOTALES%  = 0
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		:CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		:LOGINTERFAZ = 20
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
Dim VTA.ITEM$(500,5)
Dim VTA.PAGO$(30,4)
Dim VTA.DSCT$(10,2)
CITM%             = 1
CPAG%             = 1
CDSC%             = 1 
N.Cliente$        = "000000000000000000"
X.PARA%           = 0
X.CAMBIO%         = 0
IVA% = 0 
X.Anul% = 0 
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  IF X% = 0 THEN BEGIN                                          ! Si copia OK
     OPEN B$ AS TLOG BUFFSIZE 4000H NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     B$ = ""
  ENDIF ELSE BEGIN                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     WAIT;1800
     STOP
  ENDIF
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

If DATO.SO$ = "BACKGRND" THEN B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" THEN BEGIN 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   LOCATE 2, 4: PRINT CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   LOCATE 3, 4: PRINT CHR$(179)
   LOCATE 4, 4: PRINT CHR$(179)
   LOCATE 3,12: PRINT "****    ACUMULACION DE MOVIMIENTOS PARA LA DIAN     ****"
   LOCATE 3,75: PRINT CHR$(179)
   LOCATE 4,10: PRINT CHR$(27)+"b3"
   LOCATE 4,12: PRINT "***  Ultima Revision Software Noviembre 23 2009 G.R. ***"
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
 Clave$ = Pack$(Right$("0000"+Terminal$,4) + Right$("0000"+Nro.Trans$,4))
 BAN.PRG$ = "0"
 Read Form "C4" ;#38 Key Clave$ ; Xdummy$
 If BAN.PRG$ <> "0" Then Begin 
    Write Form "C4" ;#38 ; Clave$
 EndIf Else Begin 
    DUPLICADA = -1
 EndIf 

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

Function CALCULO.HORA
STRING A$, H$, M$, S$
A$ = TIME$							    !
H$ = LEFT$(A$,2)						! Tomo hora
M$ = MID$(A$,3,2)						! Tomo minutos
S$ = RIGHT$(A$,2)						! Tomo segundos
M$ = STR$(VAL(M$) + 10)			! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)			!
IF VAL(M$) > 59 THEN BEGIN 	!
   M$ = STR$(VAL(M$) - 60)	!
   M$ = RIGHT$("00"+M$,2)		!
   H$ = STR$(VAL(H$) + 1)		!
   IF VAL(H$) > 23 THEN H$ = "00"	 !
ENDIF								               !
HORA.FINAL$ =  H$+M$+"00"					 ! Hora de Inicio 
END FUNCTION

Function Grabacion.Terminales(X1%,X2%)
String Llave$, UU.Iva$, U.Tipo$, U.Stat$,BASURA$
Integer*4 UU.Base%, UU.Iva%, X1%, X2%
      PROCESO$ = "EN GRAB TERMINALES "
      UU.Iva$ = Str$(900 + Val(Terminal$))            ! Numero de la terminal
      Llave$ = PACK$("1"+FECHA.CIER$+UU.Iva$)     ! 1+Fecha Movimiento+terminal
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ; \! Si ya existe informacion
           UU.Iva$, UU.base%, UU.Iva%, basura$         ! 
      If BAN.PRG$ = "1" Then Begin               ! Si no existe init variables
         UU.Base% = 0 : UU.Iva% = 0
      EndIf 
      UU.Base% = UU.Base% + X1%
      UU.Iva%  = UU.Iva%  + X2%
      WRITE FORM "C5 2I4 C2"; #OUTFIL;             \! Graba informacion
                 Llave$,                           \! Tarifa de descuento
                 UU.base%,                          \! Valor Compra
                 UU.Iva%,                           \! Valor Descuento
                 BASURA$
End Function 
!--- Fin grabacion ventas por terminales

Sub Grabacion.Departamento
String Llave$, U.Dpto$, Tiva$, BASURA$
Integer*4 U.Precio%, U.Iva%
      PROCESO$ = "EN GRAB DEPTOS "
      Tiva$ = Str$(Iva%)
      Tiva$ = Right$("00"+Tiva$,2)
      Llave$ = PACK$(FECHA.CIER$+DEPTO$+Tiva$)     ! Fecha Movimiento+Departamento+Iva$
      BAN.PRG$ = "0"
      VTAS.TOTALES% = VTAS.TOTALES% + Val(PRECIO$)
      Read Form "C5 2I4 C2" ; #outfil Key Llave$ ; \! Si ya existe informacion
           U.Dpto$, U.Precio%, U.Iva%, BASURA$            ! 
      If BAN.PRG$ = "1" Then Begin               ! Si no existe init variables
         U.Precio% = 0 : U.Iva% = 0
      EndIf 
      If Signo$ = "" Then Begin 			     ! Trx positiva
         U.precio% = U.precio% + (Val(Precio$))   ! Actualiza saldos
         U.Iva%    = U.Iva% + Vlr.Iva%              ! 
      EndIf Else Begin 					     ! Anul o Devol
         U.precio% = U.precio% + (Val(Precio$))   ! Actualiza saldos
         U.Iva%    = U.Iva% + (Vlr.Iva% * -1)                ! 
      EndIf 
      U.Iva% = 0 
      Write Form "C5 2I4 C2"; #OUTFIL;          \! Graba informacion
                 Llave$,                        \! Departamento
                 U.Precio%,                     \! Precio de venta 
                 U.Iva%,                        \! Iva de la venta
                 BASURA$
End Sub 
!--- Fin grabacion departamento

Sub Grabacion.Iva
String Llave$, U.Iva$, BASURA$
Integer*4 U.Base%, U.Iva%
      PROCESO$ = "EN GRAB IVA "
      U.Iva$ = Right$("000"+Str$(Iva%),3)        ! Tariva de Iva
      Llave$ = PACK$("I"+FECHA.CIER$+U.Iva$)     ! I+Fecha Movimiento+tarifa iva
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #outfil Key Llave$ ; \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, BASURA$            ! 
      If BAN.PRG$ = "1" Then Begin               ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base% = U.Base% + Val(Precio$)
      U.Iva%  = U.Iva%  + 0       

     Write Form "C5 2I4 C2"; #OUTFIL;             \! Graba informacion
                 Llave$,                           \! Tarifa de IVA
                 U.base%,                          \! Base de Impuesto
                 U.Iva%,                           \! Vlr del Iva 
                 BASURA$
End Sub
!--- Ventas por departamento de IVA


Sub Grabacion.Iva.Dscto
String Llave$, U.Iva$, BASURA$
Integer*4 U.Base%, U.Iva%
      PROCESO$ = "EN GRAB IVA DSCTO "
      U.Iva$ = Right$("000"+Str$(Iva%),3)        ! Tariva de Iva
      Llave$ = PACK$("I"+FECHA.CIER$+U.Iva$)     ! I+Fecha Movimiento+tarifa iva
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #outfil Key Llave$ ; \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, BASURA$            ! 
      If BAN.PRG$ = "1" Then Begin               ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base%   = U.Base% - Val(Precio$)
      U.Iva%    = U.Iva%  + Val(Precio$)         ! Dscto entregado por tarifa de IVA
     
      Write Form "C5 2I4 C2"; #OUTFIL;          \! Graba informacion
                 Llave$,                        \! Tarifa de IVA
                 U.base%,                       \! Base de Impuesto
                 U.Iva%,                        \! Vlr del Iva 
                 BASURA$
End Sub
!--- Ventas por departamento de IVA - Dscto 


Sub Grabacion.Cupones
String Llave$, U.Dpto$, Tiva$, U.Alterno$, BASURA$
Integer*4 U.Precio%, U.Iva%
      PROCESO$ = "EN GRAB CUPONES "
      U.Alterno$ = "ABCDEFGH"                        ! Depende de 8 tarifas de IVA
      Tiva$ = MID$(U.ALTERNO$,Iva%,1)                ! Toma alterno tarifa de IVA
      Llave$ = PACK$(Tiva$+FECHA.CIER$+DEPTO$)       ! Tarifa Iva+Fecha Movimiento+Departamento
      BAN.PRG$ = "0"
      VTAS.TOTALES% = VTAS.TOTALES% - Val(PRECIO$)
      Read Form "C5 2I4 C2" ; #outfil Key Llave$ ;     \! Si ya existe informacion
           U.Dpto$, U.Precio%, U.Iva%                ! 
      If BAN.PRG$ = "1" Then Begin                   ! Si no existe init variables
         U.Precio% = 0 : U.Iva% = 0
      EndIf 
      U.precio% = U.precio% + (Val(Precio$)- VLR.IVA%)       ! Actualiza saldos
      U.Iva%    = U.Iva% + Vlr.Iva%                  ! 
      WRITE FORM "C5 2I4 C2"; #OUTFIL;                 \! Graba informacion
                 Llave$,                            \! Departamento
                 U.Precio%,                         \! Precio de venta 
                 U.Iva%,                            \! Iva de la venta
                 BASURA$
End Sub
!--- Fin grabacion de cupones


Sub Grabacion.Descuentos
String Llave$, U.Iva$, BASURA$
Integer*4 U.Base%, U.Iva%
      PROCESO$ = "EN GRAB DSCTOS "
      U.Iva$ = Right$("000"+TIPO.DESCTO$,3)      ! Tariva de descuento
      Llave$ = Pack$("P"+FECHA.CIER$+U.Iva$)     ! P+Fecha Movimiento+tipo de descuento
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ; \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, BASURA$      ! 
      If BAN.PRG$ = "1" Then Begin               ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base% = U.Base% + Val(Valor.Descto$)
      WRITE FORM "C5 2I4 C2"; #OUTFIL;          \! Graba informacion
                 Llave$,                        \! Tarifa de descuento
                 U.base%,                       \! Valor Descuento
                 U.Iva%,                        \! free
                 BASURA$
End Sub
!--- fin grabacion descuentos

Sub Grabacion.Cupon.Descuento
String Llave$, U.Iva$, X.Lista$, X.Plu$, BASURA$
Integer*4 U.Base%, U.Iva%, U.I%
Integer*2 U.Pos%
Integer*1 U.Found%
      PROCESO$ = "EN GRAB CUPON DSCTO "
      X.Plu$ = Str$(Val(Cod.Item$))
      U.Found% = -1
      For U.I% = 1 to 99
          X.Lista$ = DSCTOS$(U.I%)
          U.Pos% = MATCH(X.Plu$,X.Lista$,1)
          If U.Pos% > 0 Then Begin
             Tipo.Descto$ = Str$(U.I%)
             U.Found% = 0
             U.I% = 100
          EndIf 
      Next U.I%  
      If U.Found% = -1 Then Tipo.Descto$ = "99"
      U.Iva$ = Right$("000"+TIPO.DESCTO$,3)       ! Tariva de descuento
      Llave$ = PACK$("P"+FECHA.CIER$+U.Iva$)      ! P+Fecha Movimiento+tipo de descuento
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ;  \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, BASURA$       ! 
      If BAN.PRG$ = "1" Then Begin                ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base% = U.Base% + Val(Precio$)
      WRITE FORM "C5 2I4 C2"; #OUTFIL;                \! Graba informacion
                 Llave$,                           \! Tarifa de descuento
                 U.base%,                          \! Valor Descuento
                 U.Iva%,                           \! free
                 BASURA$
End Sub
!--- Fin grabacion cupon descuento


Function Grabacion.Miscelaneos(X1%,X2%)
String Llave$, U.Iva$, U.Tipo$, U.Stat$,BASURA$
Integer*4 U.Base%, U.Iva%, X1%, X2%
      PROCESO$ = "EN GRAB MISCELA "
      U.Tipo$ = "M"
      U.Iva$ = Str$(X1%)
      U.Iva$ = Right$("000"+U.Iva$,3)                  ! Numero del domicilio
      Llave$ = PACK$(U.TIPO$+FECHA.CIER$+U.Iva$)     ! M + Fecha Movimiento + terminal
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ;  \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, basura$          ! 
      If BAN.PRG$ = "1" Then Begin                   ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base% = U.Base% + X2%
      U.Iva%  = U.Iva%  + 0
      Write FORM "C5 2I4 C2"; #OUTFIL;             \! Graba informacion
                 Llave$,                           \! Tarifa de descuento
                 U.base%,                          \! Valor Miscelaneo
                 U.Iva%,                           \! Valor Descuento
                 BASURA$
End Function 
!--- Fin grabacion ventas por terminales

Function Grabacion.Fiscal
String Llave$, U.Iva$, U.Tipo$, basura$
Integer*4 U.Base%, U.Iva%, X1%, X2%
      PROCESO$ = "EN GRAB FISCAL "
      U.Tipo$ = Str$(Val(TIPO.DOC$))
      U.Iva$ = Right$("000"+Str$(Val(Terminal$)),3)         ! Numero de la terminal
      Llave$ = Pack$(U.Tipo$+FECHA.CIER$+U.Iva$)            ! 1+Tipo documento + Fecha Movimiento+terminal
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ; \! Si ya existe informacion
           U.Iva$, U.base%, U.Iva%, BASURA$         ! 
      If BAN.PRG$ = "1" Then Begin                  ! Si no existe init variables
         U.Base% = Val(FACTURA$) : U.Iva% = Val(FACTURA$)
      EndIf  Else Begin
         U.Iva% = Val(FACTURA$)
      EndIf 
      WRITE FORM "C5 2I4 C2"; #OUTFIL;             \! Graba informacion
                 Llave$,                           \! Tarifa de descuento
                 U.base%,                          \! Documento inicial
                 U.Iva%,                           \! Documento Final     
                 basura$
End Function 

Function Grabacion.Pagos(UX%)
String Llave$, U.Iva$, BASURA$
Integer*1 UX%
Integer*4 U.Base%, U.Iva%
      PROCESO$ = "EN GRAB PAGOS "
      U.Iva$ = Str$(Val(FORMA.PAGO$))			   ! Mod 09Dic2008
      U.Iva$ = Right$("000"+U.Iva$,3)              ! Tariva de descuento
      Llave$ = Pack$("X"+FECHA.CIER$+U.Iva$)       ! X+Fecha Movimiento+tipo de pago
      BAN.PRG$ = "0"
      Read Form "C5 2I4 C2" ; #Outfil Key Llave$ ;   \! Si ya existe informacion
           U.Iva$, U.Base%, U.Iva%, BASURA$              ! 
      If BAN.PRG$ = "1" Then Begin                 ! Si no existe init variables
         U.Base% = 0 : U.Iva% = 0
      EndIf 
      U.Base% = U.Base% + Val(VALOR.PAGO$)         ! Valor del pago
      If UX% = 1 Then Begin
         U.Iva% = U.Iva% + 1
      EndIf 
      Write Form "C5 2I4 C2"; #OUTFIL;            \! Graba informacion
                 Llave$,                          \! Tarifa de descuento
                 U.Base%,                         \! Valor Pago
                 U.Iva%,                          \! free
                 BASURA$
      llave$ = UnPack$(llave$)           
End Function 
!--- Fin grabacion de pagos

Sub Apertura.Archivos.Acumuladores
    OPEN "ADX_IDT1:EAMACFIS.DAT" KEYED RECL 15 AS OUTFIL BUFFSIZE 4000H    ! Archivo acumulador fiscal
End Sub
!--- Apertura archivos acumuladores

Sub Apertura.Cupones.Descuento
String U.Dpt$, U.Des$
Dim DSCTOS$(99)
   OPEN "ADX_IDT1:CPNDSCTO.DAT" AS 91 NOWRITE NODEL            ! 
   IF END # 91 THEN  CONT.CUPON                                ! Recorre el archivo
   LEER.CUPON:
    Read # 91 ; U.Dpt$, U.Des$
    Dsctos$(Val(U.dpt$)) = U.Des$
    GoTo LEER.CUPON 
   CONT.CUPON:
     Close 91
End Sub 

Sub Lectura.Producto
Integer*2 Xpos%
PROCESO$ = "EN LECTURA ITEM "
IR.ITEMCODE$ = Pack$(Right$("000000000000"+Gr.Ean$,12))
BAN.PRG$ = "0"
%INCLUDE EAMIREAS.J86
If BAN.PRG$ = "0" Then Begin                  ! Lo Encontro
   Xpos% = GETN1(GR.IRDATA$, 42)              ! Tipo de tarifa de IVA
   Xtarifa% = Xpos%
   Iva%  = UE.TABLAIVA(Xpos%)                 ! Entrega Posicion del IVA
   Depto$ = UnPack$(IR.DEPARTME$)             ! Numero de departamento ajustar
   Depto$ = Right$("00"+Str$(Val(Depto$)),2)  ! ajusta dato
EndIf Else Begin 
   Iva% = 00				      ! Default lo reporta a la tarifa 5 
   Xtarifa% = 5 
   Depto$ = "01"                              ! Departamento 01
EndIf    
End Sub

!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  On Error GoTo IO.ARCHIVOS
  Call INI.VAR.PROG		! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
  Call CALCULO.HORA
  ARC% = 0
  !Open "C:\EASY\:eamitemr.dat" KEYED RECL 254 AS 4  ! Apertura maestro de Articulos 
  
  CREATE POSFILE "Q:TRXDIA.DAT" KEYED 4,,,1000000 RECL 4 AS 38
  
  
  Open "EAMITEMR" KEYED RECL 254 AS 4  ! Apertura maestro de Articulos 
  
  Call Carga.Iva
  Call Apertura.Archivos.Acumuladores
  !Call Apertura.Cupones.Descuento
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  LOCATE 8,12: PRINT "Procesando .... "+ARCHIVO.PROC$

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  PROCESO$ = " PROCESO PRINCIPAL "
  Read #TLOG; LINE INAREA$
  IF END #TLOG THEN REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  IF LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
     GoTo NXTRCD
  ENDIF
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))	! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 	! ENCONTRAR DELIMITADOR, COMA
    IF (P-Q) < 3 THEN \ 	! REVISAR FALTA DE STRING
    	BEGIN
	  P=0
	  !MEN$ ="FALTA STRING ID TRANSACCION"
	  !Call ENTRADA.LOG
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. String
	  NO.VENTA = -1                 ! Add Mod13dic2008
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
     
     On A GOSUB S1,S2,S3,S4,S5,S6,S7,S7,S9,S10, \
                S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21, S99
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  PROCESO$ = "EN S0: "
  vlr.dscto% = 0
  X.Anul% = 0 
  GOSUB GETUNPK										! 
  TERMINAL$=RIGHT$(A$,4)								! CAPTURA NUMERO DE TERMINAL
  GOSUB GETUNPK										! 
  NRO.TRANS$=A$										! CAPTURA NUMERO DE TRANSACCION
  GOSUB GETUNPK										!
  FECHA$=LEFT$(A$,6)									! CAPTURA FECHA DE TRANSACCION
  HORA$ = RIGHT$(A$,4)+"00"	        						! CAPTURA HORA DE LA TRANSACCION
  GOSUB GETUNPK
  TIPO.TRANS=VAL(A$)		        						! CAPTURA EL TIPO DE LA TRANSACCION
  If TIPO.TRANS = 2 Then X.CAMBIO% = -1 
  If TIPO.TRANS = 16 OR TIPO.TRANS = 17 OR TIPO.TRANS = 18 Then Begin 
     X.Anul% = -1
     NO.VENTA = -1
     GoTo NXTRCD
  EndIf    
  If TIPO.TRANS = 3 OR TIPO.TRANS = 4 Then Begin 
     X.Anul% = -1
     NO.VENTA = -1
     GoTo NXTRCD
  EndIf    
  FECHA.ARCH$=FECHA$
  IF ARC% = 0 and TIPO.TRANS = 0 THEN BEGIN   						! Creacion archivos de salida
     FECHA.CIER$ = FECHA$               						! Toma fecha de movimiento del
     ARC% = 1
  ENDIF

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
  
  IF STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) THEN  BEGIN			! TIPO 2 PARA TENDER EXCHANGE Y TIPO 1 PARA TENDER CASHING ...
     Call GRABE.IDENTIFICADOR
     IF DUPLICADA THEN GOTO NXTRCD									! 
  EndIf

  Vlr.Venta% = ( VAL(GROSS.POSITIVO$) - VAL(GROSS.NEGATIVO$))
  
  Call Grabacion.Terminales(Vlr.Venta%,0)
  
  Vlr.Venta$    = Str$(Vlr.Venta%)

RETURN

S1:
  J = 3
  PROCESO$ = "EN S1: "
  SIGNO$ = ""
  GOSUB GETUNPK
  COD.ITEM$=RIGHT$(STRING$(12,"0")+A$,12) 					! CODIGO PLU O EAN
  GOSUB GETUNPK
  PRECIO$ = A$   ! PRECIO DEL ARTICULO
  CANTIDAD$ = "00001"							! INICIALIZAMOS CANTIDAD
  GOSUB GETUNPK
  Old.Depto$ = Depto$
  DEPTO$ = RIGHT$("00"+A$,2)	 					  ! DEPARTAMENTO DEL ARTICULO
  If Old.Depto$ = "" Then Old.Depto$ = Depto$       			  ! Si primera vez
  GoSub GETUNPK							  	  ! Cupon Family 3 y 3 Long 6
  GoSub GETUNPK 							  ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  PESO = 0
 
  INDICAT1 = VAL(A$)
  IF INDICAT1 AND 4000H THEN PESO = 1 ELSE PESO = 0

 INDICAT1 = VAL(A$)
 If INDICAT1 AND 4000H Then PESO = 1 Else PESO = 0

 UE.INDI1% = INDICAT1                                 ! Discriminaci¢n Iva.
 If (UE.INDI1% AND 01H) Then                         \!
     UE.BIN$ = "1" Else UE.BIN$ = "0"                 !


  GOSUB GETUNPK                                        ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = VAL(A$) 

  GoSub GETUNPK	   !   INDICAT3   QUE VENDIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
  Cat$ = A$       
  
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
  EndIf 
  
  If Cat$ <> "51" Then Begin   
     If signo$ = "" Then \
        ovs1% = ovs1% + vlr.base% Else \
        ovs1% = ovs1% - vlr.base%

     If signo$ = "" Then \
        ovs2% = ovs2% + vlr.iva% Else \
        ovs2% = ovs2% - vlr.iva%

        
     Call Grabacion.Departamento			        ! Ventas por Departamento
     Call Grabacion.Iva                                         ! Ventas por Iva
  EndIf Else Begin 
     Call Grabacion.Miscelaneos(Val(COD.ITEM$),(Val(Precio$)* -1)) 
  EndIf 
Return

S2:
  J = 3 		        ! EXTENSION DE ENTRADA
Return

S3:
  J = 3
  PROCESO$ = "EN S3: "
  GOSUB GETUNPK			!  ASIGNACION DE DESCUENTO AL GRUPO DISGROUP= VAL(A$)
  TIPO.DESCTO$ = A$
  GOSUB GETUNPK			!  NO USADO PORCENTAJE DESCUENTO DISRATE= ,VAL(A$)
  GOSUB GETUNPK 		!  VALOR DEL DESCUENTO =  AMTDISCO
  VALOR.DESCTO$=A$
  GOSUB GETUNPK			!  CANTIDAD LIBRE DE IMPUESTO = TAXEXEMP
  Call Grabacion.Descuentos                
  Tot.Dscto% = Tot.Dscto% + Val(valor.descto$)
  Call Grabacion.Terminales(0,Val(valor.descto$))                 ! Acumulados por terminales
Return

S4:
 J = 3
  PROCESO$ = "EN S4: "
  GOSUB GETUNPK			!  ASIGNACION DE DESCUENTO AL GRUPO DISGROUP= VAL(A$)
  TIPO.DESCTO$ = A$
  GOSUB GETUNPK			!  NO USADO PORCENTAJE DESCUENTO DISRATE= ,Val(A$)
  GOSUB GETUNPK 		!  VALOR DEL DESCUENTO =  AMTDISCO
  VALOR.DESCTO$= Str$(Val(A$) * -1)
  GOSUB GETUNPK			!  CANTIDAD LIBRE DE IMPUESTO = TAXEXEMP
  Call Grabacion.Descuentos                
  Call Grabacion.Terminales(0,Val(valor.descto$))                 ! Acumulados por terminales
Return 

S5:
  J = 3
  PROCESO$ = "EN S5: "
  GOSUB GETUNPK 			!  FORMA O TIPO DE PAGO, TENDTYPE
  FORMA.PAGO$=A$      !  Tipo de pago
  GOSUB GETUNPK 			!  CANTIDAD DE PAGO,  AMTTENDE
  VALOR.PAGO$ = A$    !  Valor del pago
  GOSUB GETUNPK 			!  NO USADO   CANTIDAD TENDER FEE
  Donacion$ = A$
  GOSUB GETUNPK 			!  NUMERO DE LA CUENTA DEL CLIENTE CUST NUM
  NRO.CUENTA$ = A$
  Call Grabacion.Pagos(1)
  If Val(Donacion$) > 0 Then Begin 
     Call Grabacion.Miscelaneos(9,Val(Donacion$)) 
  EndIf 
Return

S6:
  J = 3
  PROCESO$ = "EN S6: "
  GOSUB GETUNPK 			!  FORMA O TIPO DE PAGO, TENDTYPE
  FORMA.PAGO$=A$      !  Tipo de pago
  GOSUB GETUNPK 			!  CANTIDAD DE PAGO,  AMTTENDE
  VALOR.PAGO$ = STR$(Val(A$) * -1)    !  Valor del pago
  GOSUB GETUNPK 			!  NO USADO   CANTIDAD TENDER FEE
  Donacion$ = A$
  GOSUB GETUNPK 			!  NUMERO DE LA CUENTA DEL CLIENTE CUST NUM
  NRO.CUENTA$ = A$
  Call Grabacion.Pagos(1)
  If Val(Donacion$) > 0 Then Begin 
     Call Grabacion.Miscelaneos(9,Val(Donacion$)) 
  EndIf 
Return

S7:
  J = 3!TAX
RETURN

S8:
  J = 3!TAX
Return

S9:
  J = 3
  PROCESO$ = "EN S9: "
  GOSUB GETUNPK
  FORMA.PAGO$ = A$                      !  TIPO DE CAMBIO A DAR AL CLIENTE TENDTYPE, NO USADO
  GOSUB GETUNPK
  VALOR.PAGO$ = Str$(Val(A$) * -1)      !  LAS VUELTAS,   AMTCHANGE  
  Call Grabacion.Pagos(2)
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
  PROCESO$ = "EN S99: "
  If X.Anul% = -1 Then Return 
 
  GoSub GETUNPK					! LLAVE DEL ARCHIVO
   
! String Informacion Iva 99:10:F1:208200150616:16:00001097:00006853

  If A$ = "10" Then Begin 			! Numeracion Fiscal
     GoSub GETUNPK				! LLAVE DEL ARCHIVO
     
     If Val(a$) = 1 Then Begin 			! Dato Iva Articulo
        Vlr.Iva% = 0 
        Vlr.Base% = 0 
        GoSub GETUNPK 				! Codigo Articulo
        GoSub GETUNPK 				! Tarifa Iva
        Iva% = Val(A$)				! Tarifa de IVA
        GoSub GETUNPK 				! Vlr Iva 
        Vlr.Iva% = Val(A$)
        GoSub GETUNPK 				! Vlr Base
        Vlr.Base% = Val(A$)

        If Iva% = 58 Then Iva% = 0  : Vlr.Iva% = 0   ! Correccion Error grabacion iva Rhiscom
        Return 
     EndIf 

     If Val(a$) = 2 Then Begin 			! Dato Fiscal
        GoSub GETUNPK4				! Prefijo documento
        TIPO.DOC$ = "1"                         !    
        GoSub GETUNPK                           ! Numero de Terminal
        GoSub GETUNPK                           ! Numero de Factura
        FACTURA$ = A$                           ! Numero de documento
        FACTURA$ = TRANSLATE$(A$,"=","0")
        Call Grabacion.Fiscal                   ! Grabacion Consecutivo Fiscal        
        Return 
     EndIf 

     If Val(a$) = 45 Then Begin 		! Dato Descuento
        GoSub GETUNPK                           ! Numero de Terminal
        If Val(a$) = 1 Then Begin               ! Proceso de promocion 
           GoSub GETUNPK                        ! Ean del Producto
           Gr.Ean$ = A$
           GoSub GETUNPK                        ! Cantidad Vendida
           Gr.Qty$ = a$  
           GoSub GETUNPK                        ! Valor dscto otorgado
           Gr.Valor$ = A$
           Call Lectura.Producto                ! Busca el producto
           
           Vlr.Iva% = ROUND(FLOAT(Val(Gr.Valor$))/(1.+FLOAT(UE.TABLAIVA(Xtarifa%))/100.)*FLOAT(UE.TABLAIVA(Xtarifa%))/100.,0,0)
    
           !New.Iva% = Val(Gr.Valor$) / (1.+FLOAT(UE.TABLAIVA(Xtarifa%))/100.)*FLOAT(UE.TABLAIVA(Xtarifa%))/100.
           !Vlr.Iva% = New.Iva%
           
           Precio$ = Gr.Valor$ 
           
           !Vlr.Iva% = Val(Gr.Valor$) - ROUND(FLOAT(Val(Gr.Valor$))/(1.+FLOAT(UE.TABLAIVA(Xtarifa%))/100.),0,0)
           
           Call Grabacion.Iva.Dscto             ! Ventas por Iva
        EndIf 
        Return 
     EndIf 

  EndIf 

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
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BEGINNING OF NEXT FIELD
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
!If ERR = "IH" THEN RESUME NXTRCD:

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 38 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

IF ERRF% = OUTFIL AND (ERR = "FU" OR ERR = "OE") THEN BEGIN 
   men$ =  "Creando Archivo Acumulador Fiscal, Espere Por Favor..."
   Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
   CREATE POSFILE "EAMACFIS" KEYED 5,,,1000000 RECL 15 AS OUTFIL		     ! 
   Resume 
EndIf

IF ERRF% = AREA1% AND                                \! Validacion si existe 
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

IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
   BAN.PRG$ = "1"				      ! del archivo de 
   RESUME					      ! help del aplicativo
ENDIF						      !

IF ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") THEN \
 BEGIN
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
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
