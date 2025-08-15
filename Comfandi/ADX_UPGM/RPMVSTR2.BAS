!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : RPMOVSTR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Octubre 10 de 2.007              *
!Observaciones : Reporte Movimientos String       *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$, Ue.Data3$, Xsgn$, NRO.REG$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$, NAME$, TIPO.TRANS$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$
Integer*1 Global ARC%, trx.inven%
Integer*4 Global UE.LINEAS%, U.STR%(1), TOT.MOV%
String    Global XCOD.ITEM$, XDEPTO$, XCANTIDAD$, XPRECIO$, XPESO$, PESO$, Gondola$, AreaInv$, Xbuff$, IR.USERDATA$

!%Include BKCNVVAR.011					                ! Variables del programa

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! EAMITEMR
%Include DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include BASROUT.J86

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

Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 

Function INICIO1
  Call.ORDER% = 1                                 ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Function ENTRADA.LOG
  Call MSG.ERR(5,MEN$)                                 ! Mensaje
End Function 

Function BARRA.ESTADO
  INTEGER*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+String$(N,CHR$(219))+String$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"+" "+STR$(TOT.LEIDO)
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
  CITM% = CITM% + 1 
  UE.DATA2$ = "STAMARIA"+STRING$(08," ")+"REPORTE NUMERO STRINGS POR TRANSANCC "+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "FECHA DEL REPORTE 20"+DATE$+" ALMACEN :"+DM.ALMACEN$
  Call Write.Data
  UE.DATA2$ = STRING$(93,"-")  
  Call Write.Data
  UE.DATA2$ = "Caja   Trx.   Fecha-Hora   TP   NS         Oper.     Gross +      Gross -"
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
         Ue.Data2$ = STRING$(93,"-")  
         Call Write.Data
         Write #51; CHR$(12)
         Call cabecera.reporte
         UE.LINEAS% = 6
      EndIf 
End Sub
!--- Fin grabacion de interface

Function CONSULTA.PANTALLA                          ! Parametro Programa y archivo
  STRING HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  INTEGER*2 CNTI%, NRG%
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
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
      RET.ERR% = NXTF(-20) 	                           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      IF (ENDF = F1.AYUDA) THEN Call OSSHELL("PRINT "+UE.SALIDA$) 
      If (ENDF = F3.SALIR) THEN INP2$="X"
      CALL.ORDER% = 2                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
    WEND
    CLOSE 19
    BAN.PRG$ = "0"
END FUNCTION
!--- Fin de la funcion de ayuda

Function TERMINE.PROG
String X.TMP$
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  Call MSG.ERR(5,MEN$)                                 ! Mensaje
  
  For I% = 0 To 100
   If U.STR%(I%) > 0 Then BEGIN 
 	    Ue.Data2$ = "String [" + Right$("00"+Str$(i%),2) + "] = " + Str$(U.STR%(I%))
 	    Cnt.Reg% = Cnt.Reg% + U.STR%(I%)
 	    Call Grabacion.Interface
    ENDIF
  Next i%
  
  UE.DATA2$ = STRING$(93,"-")  
  Call Grabacion.Interface
  Call Format.VALORES(TOT.MOV%)
  UE.DATA2$ = "Total de  Registros  "+TS.TEMP3$
  Call Grabacion.Interface
  
  If DM.OPCION$ = "1" Then Call CONSULTA.PANTALLA
  If DM.OPCION$ = "2" Then Call OSSHELL("PRINT "+UE.SALIDA$) 
  If DM.OPCION$ = "3" Then Call MSG.ERR(5,"ARCHIVO GENERADO "+UE.SALIDA$) : WAIT ; 2500
  Call SALIDA.PROG  
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
 FECMOV$ = DATE$
End Function
!--- Fin de la funcion de lectura



Function INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
  OLD.TRX$       = "XXX"
  CITM%          = 0
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
  Xbuff$ = ""
  Dim U.STR%(100)
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
      Call MSG.ERR(5,MEN$)
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
      MSG$="Seleccione Destino de la impresion"
      Call MSG.ERR(5,MSG$)                                 ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Reporte Inventario Fisico","rpmovinv.txt") !
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
           Call HELP("Reporte Inventario Fisico","rpmovinv.txt") !
           Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
      Call MSG.ERR(05,"Digite Origen de la Informacion ")
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
      
  If DM.CODIGO$ = "1" THEN Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "2" THEN Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  If DM.CODIGO$ = "3" THEN B$ = ALLTRIM(DM.NOMPRO$)      ! Busqueda de log digitado
  
  If LEN(B$) = 0 Then Begin 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
  EndIf
  If LEN(B$) = 1 Then Begin 
     B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
     FECHA.CIER$ = DATE$						! Asigno fecha dia
  EndIf 
  Ue.Salida$ = "ADX_UDT1:RPMOVSTR.TXT"
  Create Ue.Salida$ AS 51 BUFFSIZE 51200
  Call Cabecera.Reporte

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
 Call MSG.ERR(5,MEN$)                                 ! Mensaje
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
  CALL INICIALICE.VAR.TRANSACCION		       ! Inicializamos las Var de Trans.
  Open "EAMITEMR" KEYED RECL 77 AS 4       ! Abre maestra de articulos
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  
!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

NXTRCD:
 Call INICIALICE.VAR.TRANSACCION               ! Inicializamos las Var de Trans.
 PROCESO$ = " PROCESO PRINCIPAL "
 READ #TLOG; LINE INAREA$
 If End #TLOG Then REPORTE.FINAL
 TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2          ! PARA CALCULAR BYTES LEIDOS DEL TLOG
 Call BARRA.ESTADO
 Call MSG.ERR(5,BARRA$)
 If LEN(INAREA$) < 12 Then Begin               ! SI HAY ALGO EN REGISTRO
    GoTo NXTRCD
 EndIf 
 INAREA$ = INAREA$ + ","
 While (Q < LEN(INAREA$))      ! SI HAY ALGO EN REGISTRO
   P = MATCH (",",INAREA$,Q)   ! ENCONTRAR DELIMITADOR, COMA
   If (P-Q) < 3 Then \         ! REVISAR FALTA DE String
       Begin
         P=0
         MEN$ ="FALTA String ID TRANSACCION"
         !Call ENTRADA.LOG
         Q=P+1                               ! PREPARANDO POSICION PARA PROU. String
         NO.VENTA = -1
         GoTo AGAIN                    ! VUELVA A LEER OTRO REGISTRO
       Endif
   B$ = Mid$(INAREA$,Q+1,(P-Q)-2)      ! CAPTURA CADENA SIN COMILLAS
   B$ = B$+":"                         ! SUMA UN SEMI-COLON
   Q = P + 1                           ! PREPARA POSC. PARA NUEVO DATO
   A = Val(Unpack$(Left$(B$,1)))       ! DETERMINA TIPO DE String
   If A = 0 Then GoSub S0:GoTo AGAIN   ! SI ES CHECKOUT TRANSACCION
   If A = 99 Then GoSub S99:GoTo AGAIN
   If (A < 0) OR (A > 21) Then GoTo AGAIN
   If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) Then \   ! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
      SW.ITEM = 0
   Endif
   If (A > 3) AND (SW.ITEM = 1) Then \ !SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
          If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
               SW.ITEM = 0
    Endif

   ON A GoSub S1,S2,S3,S4,S5,S6,S7,S8,S9,S10, \
              S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21,S99

   AGAIN:
    !If (A=0) AND (STRINGS.COMPLETOS=0) Then NRO.ERROR = NRO.ERROR + 1:GoTo NXTRCD
    If NO.VENTA Then GoTo NXTRCD               ! Si no es una venta leemos otro registro
    
 Wend
 GoTo NXTRCD                     ! VUELVA A LEER NUEVO REGISTRO


S0:
 ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
 J = 3
 U.STR%(0) = U.STR%(0) + 1
Return

S1:
 J = 3
 U.STR%(1) = U.STR%(1) + 1
Return

S2:
 J = 3                         ! EXTENSION DE ENTRADA
  U.STR%(2) = U.STR%(2) + 1
Return

S3:
 J = 3
  U.STR%(3) = U.STR%(3) + 1
Return

S4:
 J = 3
  U.STR%(4) = U.STR%(4) + 1
Return


S5:
 J = 3
  U.STR%(5) = U.STR%(5) + 1
Return

S6:
 J = 3
  U.STR%(6) = U.STR%(6) + 1
Return

S7:
 J = 3
  U.STR%(7) = U.STR%(7) + 1
Return

S8:
 J = 3
  U.STR%(8) = U.STR%(8) + 1
Return


S9:
 J = 3
  U.STR%(9) = U.STR%(9) + 1
Return

S10:
 J = 3
  U.STR%(10) = U.STR%(10) + 1
Return

S11:
 J = 3
 U.STR%(11) = U.STR%(11) + 1
Return

S12:
 J = 3
  U.STR%(12) = U.STR%(12) + 1
Return

S13:
 J = 3
  U.STR%(13) = U.STR%(13) + 1
Return

S14:
 J = 3
  U.STR%(14) = U.STR%(14) + 1
Return

S15:
 J = 3
 U.STR%(15) = U.STR%(15) + 1
Return

S16:
 J = 3
  U.STR%(16) = U.STR%(16) + 1
Return

S20:
 J = 3
  !U.STR%(20) = U.STR%(20) + 1
Return

S21:
 J = 3
  U.STR%(21) = U.STR%(21) + 1
Return

S99:
 J = 3
  U.STR%(99) = U.STR%(99) + 1
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

If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 
If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
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
If ERRF% = 50 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Numeracion Fiscal "
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 50 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf

If ERRF% = 55 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Convenios"
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 55 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf


If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
 EndIf
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call MSG.ERR(5,MEN$)                                 ! Mensaje
     Call SALIDA.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
