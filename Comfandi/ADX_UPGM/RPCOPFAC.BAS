!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : RPCOPFAC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generacion copia factura         *
!**************************************************
! Mod 26Feb2014
! Se adiciona al reporte y forma de pantalla
! la captura del nit y nombre del cliente para la 
! impresion de esta información en el tiquete copia
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 19Oct2018
! Se modifica apertura y lectura archivo de numeracion
! fiscal TF:NUMFIS pasando de 42 a 45 caracteres por 
! adicion de fecha final de facturacion.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, TS.TEMP1$
INTEGER*1 Global ARC%, Gr.Itm.Tarifa%, Ind.Itm%, Xcont%, Terror%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%, UE.TABLAIVA(1), LPOS%, \
                 UE.TOTALS(1)
String    Global IR.USERDATA$, VTA.ITEM$(2), U.TER$, U.TRX$, Xitm$, Xgrp$, GPagos$, Gdscto$, \
                 IR.ITEMCODE$, IR.ITEMNAME$, IR.DUMMY$, IR.CUM$, Ue.Trailer$, NRO.FACTURA$, UE.TMP1$, UE.OPER$

String    Global Gr.Comprador$, Gr.Nit$, Gr.Opt$

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion


Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
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
  Call.ORDER% = 32                                 ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Function ENTRADA.LOG
  Call MSG.ERR(10,MEN$)                                 ! Mensaje
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
      Rbuffer$ = CHR$(15) + Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub Cabecera.Reporte
  CPAG% = CPAG% + 1 
  UE.DATA2$ = "        "+STRING$(08," ")+"           C O M F A N D I           "
  Call Write.Data
  UE.DATA2$ = "        "+STRING$(08," ")+"         NIT. 890.303.208-5          "
  Call Write.Data
  UE.DATA2$ = "        "
  Call Write.Data
  UE.DATA2$ = "        "
  Call Write.Data
  UE.DATA2$ = "        "+STRING$(08," ")+"COMPRADOR: "+Ucase$(Gr.Comprador$)
  Call Write.Data
  UE.DATA2$ = "        "+STRING$(08," ")+"NIT      : "+Gr.Nit$
  Call Write.Data
  UE.DATA2$ = "        "
  Call Write.Data
  UE.DATA2$ = "  PLU   DESCRIPCION ARTICULO                                          CANTIDAD    VLR/UNIT  TOTAL VENTA   IVA     CUM           LOTE      FEC/VTO"
  Call Write.Data
End Sub 
!---

Sub Grabacion.Interface
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      UE.LINEAS% = UE.LINEAS% + 1
      Call Write.Data
      If UE.LINEAS% > 64 Then Begin 
         Ue.Data2$ = STRING$(77,"-")  
         Call Write.Data
         Write #51; CHR$(12)
         Call cabecera.reporte
         UE.LINEAS% = 0
      EndIf 
End Sub
!--- Fin grabacion de interface

Function CONSULTA.PANTALLA                          ! Parametro Programa y archivo
  STRING HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  INTEGER*2 CNTI%, NRG%
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			                   ! Despliege de la pantalla
      BAN.PRG$ = "0"
      HLP.FILE$ = UE.SALIDA$
      OPEN HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      IF BAN.PRG$ = "1" THEN BEGIN                        \!
         MSG1$ = "Archivo Reporte "+HLP.FILE$+" No Existe o Sin Informacion"
         CALL MSG.ERR(19,MSG1$): WAIT;1800: EXIT FUNCTION
      ENDIF
    INP2$ = " ": NRG% = 1
    WHILE (INP2$ <> "X" )
      BAN.PRG$ = "0"
      FOR CNTI% = 1 TO 17                                  !
          READ #19; LINE REG.HLP$
          IF BAN.PRG$ = "0" THEN                          \!
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          IF BAN.PRG$ = "1" THEN BEGIN                    \!
             CNTI% = 18					   !
             CLOSE 19					   !
             OPEN HLP.FILE$ AS 19 NODEL                    ! Apertura Archivo Help
          ENDIF
      NEXT CNTI%  
      RET.ERR% = NXTF(-20) 	                               ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			                     !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      IF (ENDF = F1.AYUDA) THEN Call OSSHELL("PRINT "+UE.SALIDA$) 
      If (ENDF = F3.SALIR) THEN INP2$="X"
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			                   ! Despliege de la pantalla
    WEND
    CLOSE 19
    BAN.PRG$ = "0"
END FUNCTION
!--- Fin de la funcion de ayuda

Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 


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
  Dim VTA.ITEM$(500,6)
  Dim UE.TOTALS(10)
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
      Call MSG.ERR(10,MEN$)
      WAIT;800
	    B$ = SLOGNAME$ 
   EndIf ELSE B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
ENDIF
TOT.TAMANO = SIZE(B$)
FEND

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    ! Inicializacion Variables Display Manager
TERM$ = " "					            ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    !
Call DM.ERR(RET.ERR%,INITDM$)				    !
RET.ERR% = OPNDIS("C:\ADX_UPGM\RPMOVREC.PNT")	            ! Apertura de la For ma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call INICIO1 
      MSG$="Seleccione Destino del reporte"
      Call MSG.ERR(10,MSG$)                                 ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Copia Factura Venta      ","rpcopfac.txt") !
         Call INICIO1
      ENDIF
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                 ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                 ! Si presiona tecla ESC
      WHILE (MATCH(DM.OPCION$,"123",1) = 0 OR DM.OPCION$ = " ")
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                         ! Captura dato en pantalla
        DM.OPCION$ = INP$                                   ! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN \
           Call HELP("copia Factura Venta      ","rpcopfac.txt") !
           Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
      Call MSG.ERR(10,"Digite Origen de la Informacion ")
      DM.CODIGO$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC
      WHILE (MATCH(DM.CODIGO$,"123",1) = 0 OR DM.CODIGO$ = " ")
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.CODIGO$ = GET.DATOS                              ! Asigna valor capturado
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
      If DM.CODIGO$ = "3" THEN BEGIN \
         DM.NOMPRO$ = GET.DATOS
         If (ENDF = F3.SALIR) THEN Call SALIDA.PROG               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG               ! Si presiona tecla ESC
         WHILE DM.NOMPRO$ = " " 
            RET.ERR% = NXTF(-2)
            Call DM.ERR(RET.ERR%,NXTF$)
            DM.NOMPRO$ = GET.DATOS
            If (ENDF = F3.SALIR) THEN Call SALIDA.PROG            ! Si presiona tecla F3
            If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG            ! Si presiona tecla ESC
         WEND
      EndIf ELSE BEGIN
        RET.ERR% = NXTF(2)					   ! Salta un campo
        Call DM.ERR(RET.ERR%,NXTF$)
      EndIf
      Call MSG.ERR(10,"Numero de terminal origen de la transaccion")
      U.TER$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC

      Call MSG.ERR(10,"Numero de la transaccion origen de la transaccion")
      U.Trx$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC

      Call MSG.ERR(10,"Numero identificacion del cliente ")
      Gr.Nit$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC

      Call MSG.ERR(10,"Nombre o razon social del cliente")
      Gr.Comprador$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC

      U.TER$ = Right$("0000"+U.TER$,4)      
      U.TRX$ = Right$("0000"+U.TRX$,4)      
      If Gr.Nit$ = " " Then Gr.Nit$ = "_________________________________"
      If Gr.Comprador$ = " " Then Gr.Comprador$ = "_________________________________"
  If DM.CODIGO$ = "1" Then Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "2" THEN Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "3" THEN B$ = ALLTRIM(DM.NOMPRO$)      ! Busqueda de log digitado
  
  If LEN(B$) = 0 Then Begin 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  EndIf
  If LEN(B$) = 1 Then Begin 
     B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
     FECHA.CIER$ = DATE$						! Asigno fecha dia
  EndIf 
End Function 
!--- Fin pantalla principal


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
 Call MSG.ERR(10,MEN$)                                 ! Mensaje
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

Sub Detalle.Iva.Compra
Integer*4 K%, Gr.Total.Base%, Gr.Total.Impto%
String Gr.Ind.Iva$, TS.TEMP1$, prtline, TS.TEMP2$
    Ue.Data2$ = "  "
    Call Grabacion.Interface
    Ue.Data2$ = "---    RESUMEN TARIFAS DE IVA    ----"
    Call Grabacion.Interface
    Ue.Data2$ = " Tipo   Compra    Base/Imp.     IVA  "
    Call Grabacion.Interface
    
    Gr.Total.Base%  = 0
    Gr.Total.Impto% = 0
    FOR K% = 1 TO 6
     Gr.Ind.Iva$ = MID$("ABCDEFGH",K%,1)                      
    IF UE.TOTALS(K%) <> 0 THEN BEGIN 
       TS.TEMP1$ = RIGHT$("00"+Str$(UE.TABLAIVA(K%)),2)
       TS.TEMP1$ = Gr.Ind.Iva$+"="+TS.TEMP1$+"%"
       TS.TEMP1$ = Str$(UE.TOTALS(K%))
       TS.TEMP1$ = Str$(ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0))
       PrtLine = PrtLine + RIGHT$("          "+TS.TEMP1$,10)+"  "
       TS.TEMP1$ = Str$(ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.)*FLOAT(UE.TABLAIVA(K%))/100.,0,0))
       PrtLine = PrtLine + RIGHT$("         "+TS.TEMP1$,09)
       Ue.Data2$ = PrtLine
       Call Grabacion.Interface
       Gr.Total.Base%  = Gr.Total.Base%  + ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0)
       Gr.Total.Impto% = Gr.Total.Impto% + ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.)*FLOAT(UE.TABLAIVA(K%))/100.,0,0)
    ENDIF
    NEXT K%

    FOR K% = 7 TO 8
    IF UE.TOTALS(K%) <> 0 THEN BEGIN 
       IF K% = 7 THEN TS.TEMP1$ = "G=EXE " ELSE TS.TEMP1$ = "H=EXC " 
       PrtLine = TS.TEMP1$
       TS.TEMP1$ = Str$(UE.TOTALS(K%))
       PrtLine = PrtLine + RIGHT$("          "+TS.TEMP1$,10)+" "
       TS.TEMP1$ = Str$(ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0))
       PrtLine = PrtLine + RIGHT$("          "+TS.TEMP1$,10)+"  "
       TS.TEMP1$ = Str$(ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.)*FLOAT(UE.TABLAIVA(K%))/100.,0,0))
       PrtLine = PrtLine + RIGHT$("         "+TS.TEMP1$,09)
       Ue.Data2$ = PRTLINE
       Call Grabacion.Interface
       Gr.Total.Base%  = Gr.Total.Base%  + ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.),0,0)
       Gr.Total.Impto% = Gr.Total.Impto% + ROUND(FLOAT(UE.TOTALS(K%))/(1.+FLOAT(UE.TABLAIVA(K%))/100.)*FLOAT(UE.TABLAIVA(K%))/100.,0,0)
    ENDIF
    NEXT K%
    TS.TEMP2$ = RIGHT$("          "+Str$(Gr.Total.Base%),10)
    TS.TEMP1$ = RIGHT$("          "+Str$(Gr.Total.IMPTO%),10)
    Ue.Data2$ = "Totales          "+TS.TEMP2$+" "+TS.TEMP1$
    Call Grabacion.Interface

End Sub 

Sub DATOS.FISCALES																												! Retorna numeracion fiscal terminal
String X.Key$, X.SERIAL$, X.PREF$, X.FEC$, X.Lec$, X.I$, X.F$, X.Aut$,   \!
       X.FECV$, X.AUT2$
Integer*4 X.INI%, X.FIN%, X.ACT%
       X.KEY$ = RIGHT$("0000"+U.Ter$,4)																		!
       X.KEY$ = PACK$(X.KEY$)																							!
!       X.Lec$ = "C2 C15 C4 2I4 C6 C3 I4"	                                !
       X.Lec$ = "C2 C8 C7 C4 2I4 C6 2C3 I4"	                              ! Nuevo Formato 29Oct2018
       Terror% = -1
       Read FORM X.LEC$;#34 KEY X.KEY$;                                  \! Lee Reg Cabecera 
            X.KEY$, 																										 \! Numero de la terminal
            X.SERIAL$,  																						     \! Serial de la terminal
            X.AUT$, 																										 \! Numero resolucion
            X.PREF$, 						                                         \! Prefijo de facturacion
            X.INI%, 																										 \! Numero inicial de Facturacion
            X.FIN%, 																										 \! Numero final de Facturacion
            X.AUT2$, 																										 \! Numero resolucion
            X.FEC$, 																										 \! Fecha Autorizacion
            X.FECV$,                                                     \! Fecha de vencimiento
            X.ACT%																											  ! Numero Actual de Facturacion

       If Terror% = -1 Then Begin
          X.FEC$ = "20"+Unpack$(X.FEC$)
          X.AUT$ = Unpack$(X.AUT$)
          X.AUT$  = STR$(Val(X.AUT$))
          X.AUT$  = Right$("              "+X.AUT$,14)
          X.I$   = Right$("          "+Str$(X.INI%),10)
          X.F$   = Right$("          "+Str$(X.FIN%),10)
          TS.TEMP1$  = "RES.DIAN:"+X.AUT$+" DEL "+X.FEC$	  ! Numero Autorizacion
          TS.TEMP3$  = "RNG.AUT. "+X.Pref$+" "+X.I$+" AL "+X.F$
       EndIf Else Begin
       	  X.AUT$     = "NO DEFINIDO"
       	  X.FEC$     = "20"+DATE$
       	  X.Pref$    = "NDEF"
       	  X.I$       = "0000000"
       	  X.F$       = "0000000"  
          TS.TEMP1$  = "RES.DIAN:"+X.AUT$+" DEL "+X.FEC$	  ! Numero Autorizacion
          TS.TEMP3$  = "RNG.AUT. "+X.Pref$+" "+X.I$+" AL "+X.F$
       EndIf 

End Sub 

Sub IMPRIME.DOCUMENTO
String Gr.Ind.Iva$, Linebuf$
Integer*1 UE.TARIFA%
Integer*4 XI%, Xtmp%, Xtotal%, Xcompra%
String    Price$, Qty$, Ttal$, CUM$, LOTE$, FVTO$

 Xcompra% = 0 
 If CITM% > 0 Then Begin 
    For XI% = 0 To (CITM% - 1)
      Ue.Tarifa% = Val(VTA.ITEM$(XI%,3))
      Gr.Ind.Iva$ = MID$("ABCDEFGH",UE.TARIFA%,1)
      IR.ITEMCODE$ = Pack$(Right$("000000000000"+VTA.ITEM$(XI%,0),12))   ! ITM A BUSCAR
      Terror% = -1

      READ FORM "C6 C12 C60";#4 KEY IR.ITEMCODE$;    \! SESSION NUMBER AND RECORD KEY
                IR.DUMMY$, IR.CUM$, IR.ITEMNAME$
      
      If Terror% = 0 Then Begin 
      	IR.ITEMNAME$ = "ARTICULO NO DEFINIDO"
      	IR.CUM$ = "0"
      EndIf
      IR.ITEMNAME$ = Left$(IR.ITEMNAME$+STRING$(60," "),60)
      Xcompra% = Xcompra% + Val(VTA.ITEM$(XI%,1))															! Total de la venta
      UE.TOTALS(UE.TARIFA%) = UE.TOTALS(UE.TARIFA%) + Val(VTA.ITEM$(XI%,1))		! Total por tarifa de IVA  
      
      Xtmp% = Val(VTA.ITEM$(XI%,1)) / Val(VTA.ITEM$(XI%,2))
      Call Format.Valores( Xtmp% )
      Price$ = Right$("          "+TS.TEMP3$,10)

      Xtmp% = Val(VTA.ITEM$(XI%,2))
      Call Format.Valores( Xtmp% )
      QTY$ = Right$("          "+TS.TEMP3$,10)
      Xtotal% = Val(VTA.ITEM$(XI%,1))
      Call Format.Valores( Xtotal% )
      Ttal$ = Right$("          "+TS.TEMP3$,10)
      CUM$ = Right$("            "+IR.CUM$,12)
      LOTE$ = Right$("            "+VTA.ITEM$(XI%,4),12)
      FVTO$ = Right$("        "+VTA.ITEM$(XI%,5),8)
      Ue.Data2$ = Right$(VTA.ITEM$(XI%,0),7) + " " +  			\! Plu del articulo
                  IR.ITEMNAME$               + " " +  			\! Descripcion del articulo
                  Qty$                       + "  " +  			\! Cantidad
                  Price$                     + "   " +  		\! Precio Unitario
                  Ttal$                      + "    " +  		\! Total
                  Gr.Ind.Iva$                + "   " +  		\! Tarifa Iva
                  CUM$                       + " " +  			\! CUM Articulo
                  LOTE$                      + " " +  			\! Numero Lote
                  FVTO$                                			 ! Fecha Vencimiento

      Call Grabacion.Interface
      
    Next XI%
    Ue.Data2$ = "  "
    Call Grabacion.Interface
    Call Format.Valores( XCompra% )
    QTY$ = Right$("          "+TS.TEMP3$,15)
    Ue.Data2$ = String$(55," ")+"SUBTOTAL/TOTAL     :$"+QTY$
    Call Grabacion.Interface
    Ue.Data2$ = Ue.Trailer$
    Call Grabacion.Interface
    Ue.Data2$ = "PREGUNTE EN RECEPCION POR NUESTRO"
    Call Grabacion.Interface
    Ue.Data2$ = "PROGRAMA DE VECINO FIEL Y GANE BONOS"
    Call Grabacion.Interface
    Ue.Data2$ = "  "
    Call Grabacion.Interface
    Ue.Data2$ = "TIQUETE :"+NRO.FACTURA$
    Call Grabacion.Interface    
    Call DATOS.FISCALES
    Ue.Data2$ = TS.TEMP1$
    Call Grabacion.Interface
    Ue.Data2$ = TS.TEMP3$
    Call Grabacion.Interface
    Ue.Data2$ = " "
    Call Grabacion.Interface
    Call Detalle.Iva.Compra
    Ue.Data2$ = "  "
    Call Grabacion.Interface
    Ue.Data2$ = "ATENDIDO POR :"+UE.OPER$
    Call Grabacion.Interface
    Ue.Data2$ = "SOMOS AUTORETENEDORES SEGÚN RESOLUCION 1660 DE MARZO 7 DE 2000"
    Call Grabacion.Interface
    Ue.Data2$ = "SOMOS GRANDES CONTRIBUYENTES SUEGUN RESOLUCION 7029 DE NOV-22 DE 1996"
    Call Grabacion.Interface
    Ue.Data2$ = "******* C O P I A   F A C T U R A **********"
    Call Grabacion.Interface
        
 EndIf Else Begin
 	  UE.Data2$ = "MOVIMIENTO NO TIENE ARTICULOS O NO EXISTE :"+U.TER$+" -- "+U.TRX$
    Call Grabacion.Interface
 EndIf
End Sub

Sub CAPTURA.DATOS.ADICIONALES
Integer*4 XI%, Xstat%, Xopt%, Xpos%
String Xln$, Xopt$
Call.ORDER% = 31                                 ! Llamado Primera Pantalla D.M
RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
Call DM.ERR(RET.ERR%,DISPD$)

While Xstat% = 0
 Xpos% = 2
 Call MSG.ERR(Xpos%,"SEQ   PLU         CANTIDAD        TOTAL    NUM. LOTE   FEC. VCMTO ")
 For XI% = 0 To (CITM% - 1)
     XPOS% = XPOS% + 1 
     Xln$ = Right$("000"+Str$(XI%),3)      								+ "   " + \ Item Numero
            Right$(VTA.ITEM$(XI%,0),7)     								+ "   " + \ Plu articulo 
            Right$("0000000000"+VTA.ITEM$(XI%,2),10)     	+ "   " + \ Cantidad
            Right$("0000000000"+VTA.ITEM$(XI%,1),10)     	+ "   " + \ Valor 
            Right$("0000000000"+VTA.ITEM$(XI%,4),10)     	+ "   " + \ Lote 
            Right$("0000000000"+VTA.ITEM$(XI%,5),10)                ! Fecha vencimiento
     Call MSG.ERR(XPOS%,Xln$)
     If XPOS% = 17 Then XI% = CITM% + 1
 Next XI%
 
 MSG$="Seleccione Numero Secuencia a Modificar"
 Call MSG.ERR(21,MSG$)                                 												! Mensaje
 RET.ERR% = NXTF(-20) 			           								 												! Primer campo
 Call DM.ERR(RET.ERR%,NXTF$)																									!
 ATTR$ = SETF(PRM.ON$)                                												!
 INP$ = UPDF                                          												! Captura dato en pantalla
 Xopt$ = INP$                                    															! Asigna valor capturado
 If (ENDF = F3.SALIR) THEN Exit Sub                  													! Si presiona tecla F3
 If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG          													! Si presiona tecla ESC
 If Val(Xopt$) <= (Citm% - 1) Then Begin
      Xopt% = Val(Xopt$)
      Call MSG.ERR(21,"Ingrese Numero de Lote ")
      VTA.ITEM$(Xopt%,4) = GET.DATOS
      Call MSG.ERR(21,"Ingrese Fecha de Vencimiento")
      VTA.ITEM$(Xopt%,5) = GET.DATOS
 EndIf Else Begin
    Call MSG.ERR(21,"Numero de Secuencia Invalida....")
    Wait ; 2000
 EndIf

Wend 
End Sub 

Function TERMINE.PROG
String X.TMP$
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  Call MSG.ERR(10,MEN$)             ! Mensaje
  Call CAPTURA.DATOS.ADICIONALES
  Call IMPRIME.DOCUMENTO
  If DM.OPCION$ = "1" Then Call CONSULTA.PANTALLA
  If DM.OPCION$ = "2" Then Call OSSHELL("PRINT "+UE.SALIDA$) 
  If DM.OPCION$ = "3" Then Call MSG.ERR(21,"ARCHIVO GENERADO "+UE.SALIDA$) : WAIT ; 2000
  Call SALIDA.PROG  
End Function
!--- Fin de la ejecucion del programa



!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Gr.Opt$ = Command$
  If Ucase$(Gr.Opt$) = "VERSION" Then Begin
  	 Print "Grupo Retail Ltda"
  	 Print "Generacion Copia facturas venta Version 29/Oct/2018"
  	 Stop 
  EndIf

  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call CARGA.TARIFAS.IVA
  Call LEER.CABECERA
  ARC% = 0
  Open "TF:ITMCUM" KEYED RECL 78 AS 4									! Abre codigos cum items
  Open "TF:NUMFIS" KEYED RECL 45 AS 34  							! Abre control numeacion fiscal
  CALL INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  
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
	  NO.VENTA = -1
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
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  Call GRABE.IDENTIfICADOR
  GoSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GoSub GETUNPK																			!
  FECHA$ = Left$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$ = FECHA$
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:RPCOPFAC.TXT"
     Create Ue.Salida$ AS 51 
     Call Cabecera.Reporte
  EndIf
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
  
  
  If Val(TERMINAL$) = Val(U.TER$)  And    \!
    Val(NRO.TRANS$) = Val(U.TRX$) Then Begin
    Ue.Trailer$ = Left$(FECHA$,2)+"/"+Mid$(FECHA$,3,2)+"/"+Right$(FECHA$,2)+ " "+\
                Left$(Hora$,2)+":"+Mid$(Hora$,3,2)+" "+\
                Right$("   "+DM.ALMACEN$,3) + " " +\
                Right$("   "+Terminal$,3)+" "+\
	              Right$("0000" +NRO.TRANS$,4)+" "+\
                Str$(Val(COD.OPERA$))    
    Terror% = -1
    Open "EAMOPERA" KEYED RECL 72 AS 35    	! Read operator file
    If Terror% = -1 Then Begin


      COD.OPERA$=PACK$(Right$("0000000000"+COD.OPERA$,10))     ! Creacion llave usuario
      LEC$="C5 C4 C1 C22 C20 C20"
      Read ForM "C5 C4 C1 C22 C20 C20";#35 KEY COD.OPERA$;                \! Lee Reg usuario
        TS.TEMP1$,TS.TEMP1$,TS.TEMP1$,TS.TEMP1$,          \!
        UE.OPER$,TS.TEMP1$				   !
      If Terror% <> -1 Then UE.OPER$ = "OPERADOR NO DEFINIDO"
      Close 35
    EndIf Else UE.OPER$ = "OPERADOR NO DEFINIDO"
  EndIf  
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
       VTA.ITEM$(LPOS%,4) = "0"   			  																			! Num Lote
       VTA.ITEM$(LPOS%,5) = "00000000"	  																			! Fecha Vencimiento
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
  If A$ = "21" AND                                                           \! Str Cierre/Trans
     MID$(B$,5,2) = "TR" Then Begin                          									! Ident TransVta
   If Val(TERMINAL$) = Val(U.TER$)  And    \!
      Val(NRO.TRANS$) = Val(U.TRX$) Then Begin
     J = 12                                               										!
     GOSUB GETUNPK                                        										! Desempaq sig UPD
     A$ = Str$(Val(A$))
     UE.TMP1$ = RIGHT$(STRING$(7," ")+A$,7)               										! Numero de la factura
     J = 11                                               										!
     GOSUB GETUNPK                                        										! Desempaq sig UPD
     NRO.FACTURA$ = "    "+Mid$(B$,8,4)+UE.TMP1$              										! Prefijo de la numeracion
   EndIf
  EndIf 
  
  
  
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
        Stop 
Return
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
  Terror% = 0 
If ERR = "SS" Then RESUME NXTRCD:
If ERR = "IH" Then RESUME 
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

If ERRF% = 19 AND (ERR = "OE" OR ERR = "FU") Then Begin
	 BAN.PRG$ = "1"
   RESUME        
EndIf 

If ERRF% = 19 AND (ERR = "EF") Then Begin
   BAN.PRG$ = "1"
   RESUME        
EndIf 

If ERRF% = 4 and ERR = "EF" Then Begin 
   RESUME           
EndIf 

If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call MSG.ERR(10,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 34 and ERR = "EF" Then Begin 
   RESUME           
EndIf 

If ERRF% = 34 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro Numeracion Fiscal"
    Call MSG.ERR(10,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 35 and ERR = "EF" Then Begin 
   RESUME           
EndIf 

Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
