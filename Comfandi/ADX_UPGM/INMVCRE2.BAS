!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : INMVCRE2.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Interface Movimientos            *
!                Creditos empresariales - F -     *
!**************************************************
!Modificaciones


%ENVIRON C						   												! Ambiente de controlador

%Include POSPVARI.BAS					        ! Variables del programa
%Include EAMITEMR.J86			            ! Variable EAMITEMR

String Global CT.TEMP3$, CT.TEMP1$, CT.DOLLARSEP$, CT.CENTSEP$

String    Global UE.AUTORIZA$, DATO.SO$, SERIAL$, ARC$, LOG$, Vlr.Venta$, N.Cliente$, NF.CLIENTE$
Integer*4 Global PP, TAM%, Len.REG%,  UE.TABLAIVA(1), CITM%, CPAG%, CDSC%, \
                 NRO.REG%, TOT.IVA%(2), TOT.DSCTO%
Integer*1 Global ARC%, PAGO%, Ind.Graba%
String    Global HORA.FINAL$, FILEOUT$, Asc.Mod.DateTime$, TIPO.DOC$
String    Global STORELINE$, Factura$, VTA.ITEM$(2), VTA.PAGO$(2), Ctrl.Trx$, VTA.DSCT$(2)
String    Global UE.CLIENTE$, UE.PUNTOS$, UE.REDIME$, UE.TMP1$, UE.TMP2$, UE.COMPRA$, FECHA.ARCH$
Integer*4 Global Vlr.Iva%, TOT.CON.IVA%, TOT.SIN.IVA%, TOT.DEL.IVA%, U.Tmp1%, U.Tmp2%, Vlr.Venta%
Integer*4 Global CR%, GK, GJ
String    Global Registro$(1), INI.MOV$, FIN.MOV$, FECMOV$, Trx$(1)

String Global  Nro.Convenio$, Nro.Medico$, Nro.Eps$, Nro.Formula$, \
               Nro.Beneficiario$, Nro.Diagnostico$, Nro.Estrato$, Nro.Datoadd$, \
               Cod.Beneficiario$, Cod.Sucursal$, Vlr.coopago$, Vlr.Credito$, Signo.T$

Integer*4 Global TotGral(1)

!--- Definicion de rutinas de la aplicacion
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include ADX_UPGM:POSPRUTI.BAS				  	      ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86                   !

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) External                  ! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End SUB


Function ENTRADA.LOG
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
   Else Locate 15,1: Print MEN$
  Wait;800
End Function

Function INICIO1
  Call.ORDER% = 11                                ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Function BARRA.ESTADO
  Integer*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+String$(N,CHR$(219))+String$(10-N,CHR$(177))+"® "+Str$(TOT.PORC.LEIDO)+"%"
End Function

Function TERMINE.PROG
String Buffer$, lec$
Integer*4 II
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+Str$(TOTAL.REG)
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
    Else Begin
     Locate 12,15: Print MEN$
  Endif

   If CR% > 0 Then Begin             ! Grabacion registros en Disco
     Locate 16,1 : Print "Grabando Informacion en disco, Espere ..."
     For II = 1 TO CR%
       Locate 16,43 : Print II
       buffer$ = Registro$(II)
       LEC$ = "C"+Str$(Len(Buffer$))                            ! Formatea la salida
       Write Form Lec$; #23 ;buffer$               							! Grabacion del registro
     Next II
     Locate 16,1 : Print String$(78," ")
     Dim Registro$(15000)
     CR% = 0
   Endif
  Close 23
  Call SETF("0000000")				   !
  Call CLRSCR					   !
  RET.ERR%= CLSDIS				   !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   !
  FIN.MOV$ = time$
  Locate 24, 1
  Stop
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Mid$(X.Tmp$,2,3)						   ! Numero del almacen
     FECMOV$     = MID$(X.TMP$,5,8)              ! Fecha de movimiento
     SERIAL$     = Right$(FECMOV$,4)
     Close 91
End Function
!--- Fin de la funcion de lectura

Sub GRABACION.INTERFACE
String Buffer$, LEC$, Iva1$, Biva1$, Iva2$, Biva2$, Iva3$, Biva3$, Iva4$, Biva4$, Iva5$, Biva5$
String Sg1$,Sg2$,Sg3$,Sg4$,Sg5$, SGT$, TIVA$, NRO.REG$, Sg6$
Integer*4  II
   ACUMULADOR% = ACUMULADOR% + 1
   NRO.REG$ = Right$("000000"+Str$(ACUMULADOR%),6)
   !Nro.Formula$ = LEFT$(UE.TMP2$,2) + Right$(UE.TMP2$,6)       ! Numero de documento - reemplaza minifactura
   
   !Nro.Formula$ = "9" + Right$(UE.TMP2$,7)       ! Numero de documento - reemplaza minifactura
  
  For JJ = 1 To Citm%
  
   Buffer$ = TRX$(JJ)                                    +    \! Datos de venta de productos
             Right$("00000000"+Nro.Formula$,8)           +    \! Nro documento                          C 8 
             Right$(STRING$(14,"0")+Nro.Beneficiario$,14)+    \! Cedula beneficiario                    C 14
             Nro.Convenio$     											     +    \! Nit Convenio														C 14
             "000"                                       +    \! ????                                   C 3
             "00000000"                                  +    \! Numero de medico                       C 8
             Right$("00000000"+Nro.Formula$,8)           +	  \! Numero Minifactura											C 8
             "0000000000"                                +    \! Numero diagnostico                     C 10
             Right$(String$(20," ")+UE.TMP2$,20)         +    \! Numero de trx fiscal										C 20
             "000000"                                    +    \! Cuota moderadora                       C 6
             " -- " + TERMINAL$ + " -- " + NRO.TRANS$    +    \! Cuota moderadora                       C 6
             Finr$                                             ! Fin de registro												C 2
             
!             "**"                                        +    \! Separador registro                     C 2
!             Right$("0000"+TERMINAL$,4)                  +    \! Numero transaccion                     C 4
!             Right$("0000"+NRO.TRANS$,4)                 +    \! Numero transaccion                     C 4
!             Finr$                                             ! Fin de registro												C 2

       CR% = CR% + 1
       Registro$(CR%) = BUFFER$

   If CR% > 14000 Then Begin             ! Grabacion registros en Disco
     Locate 16,1 : Print "Grabando Informacion en disco, Espere ..."
     For II = 1 TO CR%
       Locate 16,43 : Print II
       buffer$ = Registro$(II)
       LEC$ = "C"+Str$(Len(Buffer$))              ! Formatea la salida
       Write Form Lec$ ; #23 ;buffer$               		! Grabacion del registro
     Next II
     Locate 16,1 : Print String$(78," ")
     Dim Registro$(15000)
     CR% = 0
   Endif
Next JJ

End Sub
!--- Fin procedimiento impresion

Sub CARGA.IVA                                                ! Carga las tarIfas de IVA
String Data$
Integer*4 UE.PORCENTAJE, UE.LINEA
   UE.LINEA = 0
   Dim  UE.TABLAIVA(10)
   Open "C:\ADXALMA.DAT" AS 91 NOWRITE NODEL                 ! Abre las tarIfas de IVA
   If End # 91 Then  CONT.IVA                                ! Recorre el archivo
   While (1)
    Read #91 ;  Data$
    UE.LINEA = UE.LINEA + 1
    If UE.LINEA = 25 Then Begin 														 ! Tarifas de impuestos 
       UE.TABLAIVA(1) = Val(Mid$(DATA$,1,2))								 ! TARIFA 1
       UE.TABLAIVA(2) = Val(Mid$(DATA$,4,2))								 ! TARIFA 2
       UE.TABLAIVA(3) = Val(Mid$(DATA$,7,2))								 ! TARIFA 3
       UE.TABLAIVA(4) = Val(Mid$(DATA$,10,2))							   ! TARIFA 4
       UE.TABLAIVA(5) = Val(Mid$(DATA$,13,2))								 ! TARIFA 5
       UE.TABLAIVA(6) = 0
       UE.TABLAIVA(7) = 0
       UE.TABLAIVA(8) = 0
       GoTo CONT.IVA
    EndIf 
   Wend 
   CONT.IVA:
     CLOSE 91
End Sub

Function INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
  Vlr.Venta%     = 0
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
  N.Cliente$        = "000000000000000000"
  NF.CLIENTE$       = "00000000000000"
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		    :CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		  :LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
  Dim Registro$(15000)
  Dim TotGral(15)
  CR% = 0
  CT.CENTSEP$ = "."
  CT.DOLLARSEP$ = ""
End Function

Function INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q                 = 1
NRO.STRINGS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero
Dim VTA.ITEM$(800,6)
Dim VTA.PAGO$(30,4)
Dim VTA.DSCT$(10,2)
Dim TOT.IVA%(9,2)
TOT.DSCTO% = 0
PAGO% = -1
Vlr.Venta%        = 0
CPAG%             = 1
CDSC%             = 1
N.Cliente$        = "000000000000000000"
NF.CLIENTE$       = "00000000000000"
TOT.CON.IVA% = 0 : TOT.SIN.IVA% = 0 : TOT.DEL.IVA% = 0
End Function

Function APERTURA.ARCHIVOS.PRINC
Integer*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
     Open B$ AS TLOG BUFFSIZE 16384 NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     B$ = ""
  Endif Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     Stop
  Endif
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
   Endif Else Begin
         B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
         FECHA.CIER$ = UNPACK$(FECHA.CIER$)
         FECHA.CIER$ = LEFT$(FECHA.CIER$,6)	! Tomo primeros 6 caracteres aammdd
   Endif
Endif
TOT.TAMANO = SIZE(B$)
End Function

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    ! Inicializacion Variables Display Manager
CAMPO% = 0
AREA1% = 11: AREA2% = 4 : AREA3% = 23				            ! Definicion area de trabajo archivo

Call LEER.CABECERA
DM.CODIGO$ = "2"
DATO.SO$ = COMMAND$						! Dato S.O
If DATO.SO$ = "VERSION" Then Begin
   !Print "INTERFACE CREDITOS EMPRESARIALES - F - 19/Dic/2013 G.R"
   Print "INTERFACE CREDITOS EMPRESARIALES - F - 14/Ago/2018 G.R"
   Stop 
ENDIF 
If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  INTERFACE MOVIMIENTO CREDITOS EMPRESARIALES   ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Agosto     14 2018 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
   DATO.SO$ = "1"
   MEN$ = " Rev.SW 05/Sep/2013   GR..."
   Call Entrada.Log
EndIf

If LEN(B$) = 0 Then Begin 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO
Endif
If LEN(B$) = 1 Then Begin
   B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
   FECHA.CIER$ = DATE$						! Asigno fecha dia
Endif
FEnd

Function CUENTE.STRINGS	       	! Esta funcion cuenta primero el #String y lo
Integer*2 POS1%,CUENTE.STRINGS	! compara con el #reportado por SMA
String CARACTER
 POS1% = 1
 NRO.STRINGS.FOUND = 0
 CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
 While (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.STRINGS.FOUND = NRO.STRINGS.FOUND + 1
 WEnd
 NRO.STRINGS.FOUND = NRO.STRINGS.FOUND - 1
 If (NRO.STRINGS.FOUND < NRO.REG) Then Begin
	MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
 Endif
 If (NRO.STRINGS.FOUND > NRO.REG) Then Begin
	MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	!Call ENTRADA.LOG
 Endif
 If (NRO.STRINGS.FOUND = NRO.REG) Then CUENTE.STRINGS = 1	\ Retorna 1 si es
 Else CUENTE.STRINGS = 0				! satisfactorio el resultado y 0
End Function						! si el resultado es fallido.


Sub GRABE.IDENTIfICADOR
 String C$, DINNER.POS$, DINNER.NEG$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0
 TOTAL.REG = TOTAL.REG + 1
 Locate 20,25
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+Str$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Begin
   Locate 12,15: Print MEN$
 Endif
End Sub

Function ERRNStr$(ERRNUM)
Integer*1 I
Integer*4 ERRNUM,WORK
String HEX$,ERRNStr$,WORK$
    HEX$="0123456789ABCDEF"
    ERRNStr$="":WORK$=""
    For I = 1 TO 8
      WORK   = ERRNUM AND 0000000FH         ! AND OFF ALL BUT LOW NYBBLE
      WORK$  = MID$(HEX$,WORK+1,1)+WORK$    ! ADD HEX VALUE TO OUTPUT String
      ERRNUM = SHIfT(ERRNUM,4)              ! SET UP NEXT NYBBLE
    NEXT I
    ERRNStr$=WORK$                          ! Return Error Code
End Function

Function CALCULO.HORA
String A$, H$, M$, S$
A$ = TIME$							    !
H$ = LEFT$(A$,2)						! Tomo hora
M$ = MID$(A$,3,2)						! Tomo minutos
S$ = RIGHT$(A$,2)						! Tomo segundos
M$ = Str$(VAL(M$) + 10)			! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)			!
If VAL(M$) > 59 Then Begin 	!
   M$ = Str$(VAL(M$) - 60)	!
   M$ = RIGHT$("00"+M$,2)		!
   H$ = Str$(VAL(H$) + 1)		!
   If VAL(H$) > 23 Then H$ = "00"				!
Endif								!
HORA.FINAL$ =  H$+M$+"00"					! Hora de Inicio
End Function

!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GoTo IO.ARCHIVOS
  INI.MOV$ = time$
  Call INI.VAR.PROG		! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
  Call CALCULO.HORA
  ARC% = 0
  Call Carga.Iva
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  ON ERROR GoTo IO.ARCHIVOS
  Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  PROCESO$ = " PROCESO PRINCIPAL "
  READ #TLOG; LINE INAREA$
  If End #TLOG Then REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  If LEN(INAREA$) < 12 Then Begin		! SI HAY ALGO EN REGISTRO
	  GoTo NXTRCD
  Endif
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))	! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 	! ENCONTRAR DELIMITADOR, COMA
    If (P-Q) < 3 Then \ 	! REVISAR FALTA DE String
    	Begin
	  P=0
	  MEN$ ="FALTA String ID TRANSACCION"
	  !Call ENTRADA.LOG
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROU. String
	  NO.VENTA = -1
	  GoTo AGAIN		   	! VUELVA A LEER OTRO REGISTRO
	Endif
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 	! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
    B$ = B$+":" 			! SUMA UN SEMI-COLON
    Q = P + 1 				! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 	! DETERMINA TIPO DE String
    If A = 0 Then GoSub S0:GoTo AGAIN	! SI ES CHECKOUT TRANSACCION
    If A = 99 Then GoSub S99:GoTo AGAIN
    If (A < 0) OR (A > 21) Then GoTo AGAIN
    If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) Then \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
    If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
		   CANT.PESO$ = "000000000"
       SW.ITEM = 0
    Endif
    If (A > 3) AND (SW.ITEM = 1) Then \	!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
          	SW.ITEM = 0
     Endif

    ON A GoSub S1,S2,S3,S3,S5,S6,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21,S99

    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) Then NRO.ERROR = NRO.ERROR + 1:GoTo NXTRCD
     If NO.VENTA Then GoTo NXTRCD		! Si no es una venta leemos otro registro
  WEnd
GoTo NXTRCD			! VUELVA A LEER NUEVO REGISTRO

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  GJ = 3

  If Ind.Graba% = -1 Then \
     Call Grabacion.Interface                   ! Grabacion interface
  UE.TMP1$ = "     "+TIME$
  UE.TMP2$ = "NFND" + UE.TMP1$          						 ! Prefijo de la numeraci  
  Ind.Graba% = 0 
  Dim Trx$(700)
  Citm% = 0
  GoSub GetUnpk																			!
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GetUnpk																			!
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GoSub GetUnpk																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = FECHA$               						! Toma fecha de movimiento del
     FILEOUT$ = "ADX_UDT1:F"+DM.ALMACEN$+SERIAL$+".DAT"
     
     BAN.PRG$ = "0"
     Open FILEOUT$ AS 23 BUFFSIZE 16384 Append 
     If BAN.PRG$ <> "0" Then Begin 
        Create FILEOUT$ AS 23 BUFFSIZE 16384
        STORELINE$ = "1"+Right$(DM.ALMACEN$,4) + FECMOV$
        CR% = CR% + 1
        Registro$(CR%) = STORELINE$+FINR$              !Graba la cabecera
     EndIf 

     Locate 06,1: Print "Generando Interface :"+FILEOUT$
     ARC% = 1
  Endif
  HORA$ = RIGHT$(A$,4)     	        								! CAPTURA HORA DE LA TRANSACCION
  GoSub GetUnpk
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  GoSub GetUnpk
  NRO.REG=VAL(A$)  		        											! NUMERO DE STRINGS EN TODA LA TRANSACCION
  STRINGS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 											! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
		NO.VENTA = 1
  Endif Else	\
	STRINGS.COMPLETOS = CUENTE.STRINGS								! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS
  GoSub GetUnpk
  COD.OPERA$= Str$(Val(A$))													! NUMERO DEL OPERADOR
  COD.OPERA$ = Right$("000000"+COD.OPERA$,6)
  GoSub GetUnpk 													  			  ! PASSWORD DEL OPERADOR
  GoSub GetUnpk																			! Venta positiva
  GROSS.POSITIVO$=A$
  GoSub GetUnpk																			! Venta negativa
  GROSS.NEGATIVO$=A$
  Vlr.Venta% = (VAL(GROSS.POSITIVO$)-VAL(GROSS.NEGATIVO$))
  
  If Vlr.Venta% < 0 Then Begin 
     Signo.T$ = "-"  
     Vlr.Venta% = Vlr.Venta% * -1
  EndIf Else Signo.T$ = "+"

  Vlr.Venta$ = Right$("000000000"+Str$(Vlr.Venta%),9)
  
  If STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GoTo NXTRCD									!
  Endif

Return

S1:
  GJ = 3
  GoSub GetUnpk
  COD.ITEM$=RIGHT$(String$(12,"0")+A$,12) 					! CODIGO PLU O EAN
  GoSub GetUnpk
  PRECIO$ = RIGHT$(String$(10,"0")+A$,10)						! PRECIO DEL ARTICULO
  CANTIDAD$ = "000001"															! INICIALIZAMOS CANTIDAD
  GoSub GetUnpk
  DEPTO$ = RIGHT$("000"+A$,3)	 										  ! DEPARTAMENTO DEL ARTICULO
  GoSub GetUnpk																			! Cupon Family 3 y 3 Long 6
  GoSub GetUnpk 																	  ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
  IVA% = 0
  PESO = 0

  INDICAT1 = VAL(A$)
  If INDICAT1 AND 4000H Then PESO = 1 Else PESO = 0

  UE.INDI1% = INDICAT1                                 ! Discriminaci¢n Iva.
  If (UE.INDI1% AND 01H) Then                         \!
      UE.BIN$ = "1" Else UE.BIN$ = "0"                 !

  FOR I = 1 TO 7                                      !
       UE.INDI1% = SHIfT(UE.INDI1%,1)                  ! Inicializa sig. BIT
       If (UE.INDI1% AND 01H) Then UE.BIN$="1"+UE.BIN$\!
           Else UE.BIN$ ="0"+UE.BIN$                   !
  NEXT I                                              !

   If LEFT$(UE.BIN$,4) = "1000" Then                  \! Si hay Indic Plan A
                      IVA% = 1		                    \! indica Plan = A
   Else                                               \! si no,
   If LEFT$(UE.BIN$,4) = "0100" Then                  \! Si hay Indic Plan B
                      IVA% = 2		                    \! indica Plan = B
   Else                                               \! si no,
   If LEFT$(UE.BIN$,4) = "0010" Then                  \! Si hay Indic Plan C
                      IVA% = 3		                    \! indica Plan = C
   Else                                               \! si no,
   If LEFT$(UE.BIN$,4) = "0001" Then                  \! Si hay Indic Plan D
                      IVA% = 4		                    \! indica Plan = D
   Else                                               \! si no,
                      IVA% = 5		                     ! indica 0%

  GoSub GetUnpk                                        ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
  INDICAT2 = VAL(A$)
  GoSub GetUnpk                                        ! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
  INDICAT3 = VAL(A$)
  ALTO$ = LEFT$(A$,1)
  BAJO$ = RIGHT$(A$,1)
  ESCANEADO = 0
  SIGNO$ = "+"
  If ALTO$ = "8" OR ALTO$ = "2" OR ALTO$ = "5" \
  	Then SIGNO$ = "-" Else SIGNO$ = "+"

  If (INDICAT2 AND 0080H) AND (ALTO$ = "7") Then SIGNO$ = ""
  If (INDICAT2 AND 0040H) AND (ALTO$ = "7") Then SIGNO$ = ""
  If (INDICAT2 AND 0080H) AND (ALTO$ = "2") Then SIGNO$ = ""
  If (INDICAT2 AND 0080H) AND (ALTO$ = "4") Then SIGNO$ = "-"
  If (INDICAT2 AND 0040H) AND (ALTO$ = "4") Then SIGNO$ = "-"
  If (INDICAT2 AND 0080H) AND (ALTO$ = "1") Then SIGNO$ = "-"   ! PRUEBA DEPOSIT
  If (INDICAT2 AND 0040H) AND (ALTO$ = "1") Then SIGNO$ = "-"   ! PRUEBA DEPOSIT
  If INDICAT2 = 2054 Then SIGNO$ = "-"

  !If SIGNO$ = "-" Then Begin
  !   PRECIO$ = Str$( Val(PRECIO$) * -1 )
  !EndIf

  Vlr.Iva% = Val(precio$) -( Round(Float(Val(precio$))/(1.+Float(UE.TABLAIVA(IVA%))/100.),0,0) )
  TOT.IVA%(IVA%,1) = TOT.IVA%(IVA%,1) + ( Round(Float(Val(precio$))/(1.+Float(UE.TABLAIVA(IVA%))/100.),0,0) ) ! Base
  TOT.IVA%(IVA%,2) = TOT.IVA%(IVA%,2) + Vlr.Iva% ! IVA
  TOT.CON.IVA% = TOT.CON.IVA% + Vlr.Iva%

  Citm% = Citm% + 1 
  Trx$(Citm%) = "2"                                      +    \! Tipo de registro												C 1
             Right$(COD.ITEM$,6)                         +    \! Codigo del Item												C 6
             DEPTO$                                      +    \! Codigo del departamento								C 3
             SIGNO$                                      +    \! Signo de la operacion del Item         C 1
             CANTIDAD$                                   +    \! Cantidad vendida                       C 6
             PRECIO$                                           ! Precio total de venta Item             C 10

Return

S2:
  GJ = 3 		                   ! EXTENSION DE ENTRADA
 GoSub GetUnpk                 !  MPGROUP
 GoSub GetUnpk                 !  DEALQUAN
 GoSub GetUnpk                 !  PRICE METH
 GoSub GetUnpk                 !  SALEQUAN
 GoSub GetUnpk                 !  SALEPRIC
 GoSub GetUnpk                 !  QTYORWGT
 Cantidad$ = Right$("000000"+A$,6)
 Trx$(Citm%) = "2"                                      +    \! Tipo de registro												C 1
             Right$(COD.ITEM$,6)                         +    \! Codigo del Item												C 6
             DEPTO$                                      +    \! Codigo del departamento								C 3
             SIGNO$                                      +    \! Signo de la operacion del Item         C 1
             CANTIDAD$                                   +    \! Cantidad vendida                       C 6
             PRECIO$                                           ! Precio total de venta Item             C 10

Return

S3:
  GJ = 3
  GoSub GetUnpk			!  ASIGNACION DE DESCUENTO AL GRUPO DISGROUP= VAL(A$)
  TIPO.DESCTO$ = A$
  GoSub GetUnpk			!  NO USADO PORCENTAJE DESCUENTO DISRATE= ,Val(A$)
  GoSub GetUnpk 		!  VALOR DEL DESCUENTO =  AMTDISCO
  TOT.DSCTO% = TOT.DSCTO% + Val(A$)
  GoSub GetUnpk			!  CANTIDAD LIBRE DE IMPUESTO = TAXEXEMP
Return

S5:
  GJ = 3
Return

S6:
  GJ = 3
Return

S7:
  GJ = 3!TAX
Return

S9:
  GJ = 3
Return

S10:
  GJ = 3
Return

S11:
  GJ = 3!DTENTRY
Return

S12:
  GJ = 3!CHANGE PRICE
Return

S13:
  GJ = 3!LOAN-PICKUP
Return

S14:
  GJ = 3
Return

S15:
  GJ = 3
Return

S16:
Return

S20:
  GJ = 3
Return

S21:
  GJ = 3
Return

S99:
  GJ = 3!DTENTRY
  GoSub GetUnpk																	! LLAVE DEL ARCHIVO

  If A$ = "20130829" Then Begin 								! Modulo credito empresarial
     GoSub GetUnpk
     Nro.Beneficiario$ = A$											! Nro Beneficiario
     GoSub GetUnpk                              ! Vlr del credito
     Vlr.Credito$ = A$ 
     GoSub GetUnpk															! Nit de la empresa
     Nro.Convenio$ = A$
     GoSub GetUnpk4 														! Nombre beneficiario
     GoSub GetUnpk															! cupo total credito
     GoSub GetUnpk															! saldo disponible
     GoSub GetUnpk															! Tipo de movimiento
     If A$ = "03" Then Begin                    ! Es una anulacion de pago
     	  Vlr.Credito$ = Str$( Val(Vlr.Credito$) * -1) 
     EndIf
     GoSub GetUnpk4															! Trama mensaje
     GoSub GetUnpk4 														! Nombre empresa
     GoSub GetUnpk															! Documento
     Nro.Formula$  = Right$("00000000"+A$,8)
     GoSub GetUnpk4 														! Tipo de credito 1.Cafeteria 2.Otros
     Cod.Beneficiario$ = "00"
     If A$ = "2" Then                          \! Otros creditos
        Cod.Sucursal$ = "555"   Else           \! Definido por Comfandi 30 Oct 2013
        Cod.Sucursal$ = "777"                   ! Definido por Comfandi 19 Dic 2013

     
     
!-- Ajuste Datos para la interface
     Vlr.Coopago$ = Str$( Vlr.Venta% - Val(Vlr.Credito$) )
     Nro.Convenio$ = Right$("00000000000"+Nro.Convenio$,11) + "000"
     Nro.Beneficiario$ = Right$("000000000000"+Nro.Beneficiario$,12)
     Ind.Graba% = -1

  EndIf

  If A$ = "21" AND                                          \! Str Cierre/Trans
     MID$(B$,5,2) = "TR" Then Begin                          ! Ident TransVta
     GJ = 12                                                  !
     GoSub GetUnpk                                           ! Desempaq sig UPD
     A$ = Str$(Val(A$))
     UE.TMP1$ = RIGHT$(STRING$(11," ")+A$,11)                ! Numero de la factura
     GJ = 11                                                  !
     GoSub GetUnpk                                           ! Desempaq sig UPD
     UE.TMP2$ = Mid$(B$,8,4) + UE.TMP1$          						 ! Prefijo de la numeraci  

!     If Ind.Graba% = -1 Then \
!       Call Grabacion.Interface                   ! Grabacion interface
     
  EndIf
Return


!*********************************************************************
GetUnpk:
  GK = MATCH(":",B$,GJ) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,GJ,GK-GJ)) ! UNPACK FIELD
  GJ=GK+1 ! POINT TO Beginning OF NEXT FIELD
Return
!*********************************************************************

GetUnpk4:
  GK = MATCH(":",B$,GJ) ! SEARCH FOR FIELD SEPERATOR
  A$ = (MID$(B$,GJ,GK-GJ)) ! FIELD
  GJ=GK+1 ! POINT TO Beginning OF NEXT FIELD
Return

!*********************************************************************

REPORTE.FINAL:
        Call TERMINE.PROG
Return
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
If ERR = "SS" Then RESUME NXTRCD:
If ERR = "IH" Then RESUME
If ERRF% = TLOG  AND (ERR = "EF" OR ERR = "OE") Then Begin          \! Si encuentra fin de
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ENTRADA.LOG
     Wait;3000
     STOP
Endif				                      !


If ERRF% = 77 AND                                \! Validacion si existe
   (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
Endif                      !
If ERRF% = 77 AND ERR = "EF" Then Begin          \! Si encuentra eof()
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
Endif				                      !
If ERRF% = AREA2% AND ERR = "EF" Then Begin          \! Si encuentra eof()
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
Endif				                      !
If ERRF% = AREA3% AND ERR = "EF" Then Begin          \! Si encuentra eof()
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
Endif				                      !
If ERRF% = AREA2%  AND 				     \!
   (ERR = "OE" OR ERR = "FU")  Then Begin             ! Si encuentra Error en articulos
   MEN$="Error: Apertura EAMITEMR ..."		      !
   Call ENTRADA.LOG				      !
   Call TERMINE.PROG 				      !
Endif				                      !
If ERRF% = AREA3%  AND 				     \!
   (ERR = "OE" OR ERR = "FU")  Then Begin             ! Si encuentra Error en articulos
   BAN.PRG$ = "1"				      ! ejecucion normal del
   RESUME					      !
Endif				                      !
If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
   BAN.PRG$ = "1"				      ! del archivo de
   RESUME					      ! help del aplicativo
Endif						      !

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ADXSERVE(PP,26,1,MEN$)
    Wait;3000
    Call TERMINE.PROG
 Endif

!If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de
!     MEN$ = "No se han registrado ventas para este periodo ..."
!     Call ENTRADA.LOG
!     Wait;3000
!     Call TERMINE.PROG
!EndIf				                      !


Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+Str$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
Stop
!*********************************************************************
