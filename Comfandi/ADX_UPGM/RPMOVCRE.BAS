!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RPMOVCRE.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte Movimiento Creditos      *
!                Empresariales                    *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$
Integer*1 Global ARC%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, UTMP%, NRO.REG%, GJ%, GK%
Integer*4 Global Vta.Parcial%
String Global  Nro.Convenio$, Nro.Medico$, Nro.Eps$, Nro.Formula$, \
               Nro.Beneficiario$, Nro.Diagnostico$, Nro.Estrato$, Nro.Datoadd$, \
               Cod.Beneficiario$, Cod.Sucursal$, Vlr.coopago$, Vlr.Credito$, Signo.T$, \
               Tempo$

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion


Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   Integer*4 RETC
   String INFILE, OUTFILE
   Integer*2 OPT0, OPT1
End Sub


Function FORMAT.VALORES( FDAMT )
  Integer*1 Format.VALORES
  Integer*4 FDAMT
  Integer*2 THOU, MILL
  If FDAMT < 0 THEN                                     \
    TS.TEMP3$ = STR$(-FDAMT) + "-"                      \
  Else                                                  \
    TS.TEMP3$ = STR$(FDAMT)  + " "
  THOU = 4                                              ! for thousands
  MILL = 8                                              ! for millions
  If (Len(TS.TEMP3$) > THOU) THEN                       \ thousands
      TS.TEMP3$ = Left$(TS.TEMP3$, Len(TS.TEMP3$)-THOU)+\ ins $ sep
                "," + Right$(TS.TEMP3$, THOU)
  If (Len(TS.TEMP3$) > MILL) THEN                       \ millions
      TS.TEMP3$ = Left$(TS.TEMP3$, Len(TS.TEMP3$)-MILL)+\ ins $ sep
                "," + Right$(TS.TEMP3$, MILL)

End Function 
!--- Formateo de valores


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
  Integer*1 N, TOT.PORC.LEIDO
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

Sub Write.Data.tmp
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #76 ; Rbuffer$, Finr$           							! Graba registro
End Sub 


Sub Cabecera.Reporte
  CITM% = CITM% + 1 
  UE.DATA2$ = "COMFANDI"+String$(08," ")+"REPORTE MOVIMIENTOS CREDITOS EMPRESA"+String$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "FECHA DE MOVIMIENTO "+UE.FECMOV$
  Call Write.Data
  UE.DATA2$ = String$(77,"-")  
  Call Write.Data
  UE.DATA2$ = "Nit/Cedula      Empresa/Empleado          Valor   Docum.  Ccto"
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
         Ue.Data2$ = String$(77,"-")  
         Call Write.Data
         Write #51; CHR$(12)
         Call cabecera.reporte
         UE.LINEAS% = 0
      EndIf 
End Sub
!--- Fin grabacion de interface

Function CONSULTA.PANTALLA                          ! Parametro Programa y archivo
  String HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  Integer*2 CNTI%, NRG%
      Call.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
      BAN.PRG$ = "0"
      HLP.FILE$ = UE.SALIDA$
      Open HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      IF BAN.PRG$ = "1" THEN BEGIN                        \!
         MSG1$ = "Archivo Reporte "+HLP.FILE$+" No Existe o Sin Informacion"
         Call MSG.ERR(19,MSG1$): WAIT;1800: EXIT Function
      ENDIF
    INP2$ = " ": NRG% = 1
    While (INP2$ <> "X" )
      BAN.PRG$ = "0"
      For CNTI% = 1 TO 17                                  !
          Read #19; LINE REG.HLP$
          If BAN.PRG$ = "0" Then Begin                     !
          	 REG.HLP$ = Mid$(REG.HLP$,1,76)
             Call MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          EndIf
          If BAN.PRG$ = "1" Then Begin                    \!
             CNTI% = 18					   !
             CLOSE 19					   !
             Open HLP.FILE$ AS 19 NODEL                    ! Apertura Archivo Help
          EndIf
      Next CNTI%  
      RET.ERR% = NXTF(-20) 	                           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      If (ENDF = F1.AYUDA) THEN Call OSSHELL("PRINT "+UE.SALIDA$) 
      If (ENDF = F3.SALIR) THEN INP2$="X"
      Call.ORDER% = 2                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
    Wend
    CLOSE 19
    BAN.PRG$ = "0"
End Function
!--- Fin de la funcion de ayuda

Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 

Sub Procesa.reporte
String Xtmp$, Valor$, Xnit$, Xrun$, Xsale$
  Xnit$ = ""
  Vta.Parcial% = 0

!  Xsale$ = "ADX_UDT1:TM"+TIME$+".TMP"
!  Xrun$ = "SORT -+1 <"+TEMPO$+"> "+Xsale$
!  Call OSSHELL(Xrun$)
!  Wait ; 100
!  Open Tempo$ As 76
!  Wait ; 100
!  Delete 76
!  wait ; 100

  Open Tempo$ As 76 

  On Error GoTo Final.Reporte
  While (1)
    Read #76; Xtmp$													  ! Lectura registro
    NRO.REG% = NRO.REG% + 1 
    If Xnit$ <> Mid$(Xtmp$,  1,14) Then Begin
       If Xnit$ <> "" Then Begin
       	  Call Format.Valores(Vta.Parcial%)
       	  TS.TEMP3$ = Right$("          "+TS.TEMP3$,10)
       	  Ue.Data2$ = "Subtotal Empresa : " + TS.TEMP3$
       	  Call Write.Data
       EndIf
    	 Ue.Data2$ = Mid$(Xtmp$,  1,14) + "  " +  \! Nit Empresa
                   Left$(Mid$(Xtmp$, 15,35),20)  ! Razon social
       Call Write.Data
       Xnit$ = Mid$(Xtmp$,  1,14)
    EndIf
    VALOR$ = Mid$(Xtmp$, 99,10)               ! Valor trx
    VTAS.TOTALES% = VTAS.TOTALES% + Val( Valor$ )
    Vta.Parcial% = Vta.Parcial% + Val( Valor$ )
    Call FORMAT.VALORES( Val(valor$) )        ! formatea valor 
    TS.TEMP3$ = Right$("          "+TS.TEMP3$,10)
    Ue.Data2$ = Mid$(Xtmp$, 50,14) + "  " +  \! Cedula Empleado
                Left$(Mid$(Xtmp$, 64,35),20) + "  " +  \! Nombre Empleado
                TS.TEMP3$          + "  " +  \! Valor Credito
                Mid$(Xtmp$,109,08) + " " +   \! Numero documento
                DM.ALMACEN$                   ! Sucursal
    Call Write.Data
  Wend 
  Final.Reporte:
   If Xnit$ <> "" Then Begin
    Call Format.Valores(Vta.Parcial%)
    TS.TEMP3$ = Right$("          "+TS.TEMP3$,10)
    Ue.Data2$ = "Subtotal Empresa : " + TS.TEMP3$
    Call Write.Data
   EndIf
   Delete 76
End Sub 

Function TERMINE.PROG
String X.TMP$
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  Call MSG.ERR(5,MEN$)                                 ! Mensaje
  
  Close 76
  Call PROCESA.REPORTE
  
  
  UE.DATA2$ = String$(77,"-")  
  Call Grabacion.Interface
  Call Format.VALORES(VTAS.TOTALES%)
  UE.DATA2$ = "Total del Movimiento "+TS.TEMP3$
  Call Grabacion.Interface
  Call Format.VALORES(NRO.REG%)
  UE.DATA2$ = "Total de  Registros  "+TS.TEMP3$
  Call Grabacion.Interface
  If DM.OPCION$ = "1" Then Call CONSULTA.PANTALLA
  If DM.OPCION$ = "2" Then Call OSSHELL("PRINT "+UE.SALIDA$) 
  If DM.OPCION$ = "3" Then Call MSG.ERR(5,"ARCHIVO GENERADO "+UE.SALIDA$) : WAIT ; 2500
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
Integer*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
     Open B$ AS TLOG NOWRITE NODEL
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
RET.ERR% = OPNDIS("C:\ADX_UPGM\RPMOVPAY.PNT")	            ! Apertura de la For ma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call INICIO1 
      MSG$ = "REPORTE MOVIMIENTOS CREDITOS EMPRESA"
      Call MSG.ERR(6,MSG$)                                 ! Mensaje
      MSG$="Seleccione Destino de la impresion"
      Call MSG.ERR(5,MSG$)                                 ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Reporte Pagos Electronico","rpmovpef.txt") !
         Call INICIO1
      ENDIF
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                 ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                 ! Si presiona tecla ESC
      While (MATCH(DM.OPCION$,"123",1) = 0 OR DM.OPCION$ = " ")
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                         ! Captura dato en pantalla
        DM.OPCION$ = INP$                                   ! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN \
           Call HELP("Reporte Pagos Electronico","rpmovpef.txt") !
           Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      Wend
      Call MSG.ERR(05,"Digite Origen de la Informacion ")
      DM.CODIGO$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC
      While (MATCH(DM.CODIGO$,"123",1) = 0 OR DM.CODIGO$ = " ")
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.CODIGO$ = GET.DATOS                              ! Asigna valor capturado
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      Wend
      If DM.CODIGO$ = "3" THEN BEGIN \
         DM.NOMPRO$ = GET.DATOS
         If (ENDF = F3.SALIR) THEN Call SALIDA.PROG               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG               ! Si presiona tecla ESC
         While DM.NOMPRO$ = " " 
            RET.ERR% = NXTF(-2)
            Call DM.ERR(RET.ERR%,NXTF$)
            DM.NOMPRO$ = GET.DATOS
            If (ENDF = F3.SALIR) THEN Call SALIDA.PROG            ! Si presiona tecla F3
            If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG            ! Si presiona tecla ESC
         Wend
      EndIf ELSE BEGIN
        RET.ERR% = NXTF(2)					   ! Salta un campo
        Call DM.ERR(RET.ERR%,NXTF$)
      EndIf
      Call MSG.ERR(05,"Entre codigo del operador o Intro para Todos")
      DM.OPERADOR$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC
            
      
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
End Function 
!--- Fin pantalla principal


Function CUENTE.StringS	       	! Esta funcion cuenta primero el #String y lo
Integer*2 POS1%,CUENTE.StringS	! compara con el #reportado por SMA
String CARACTER
 POS1% = 1
 NRO.StringS.FOUND = 0
 CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
 While (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.StringS.FOUND = NRO.StringS.FOUND + 1
 Wend
 NRO.StringS.FOUND = NRO.StringS.FOUND - 1
 If (NRO.StringS.FOUND < NRO.REG) Then Begin
	MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND > NRO.REG) Then Begin
	MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
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

  ON ERROR GoTo IO.ARCHIVOS
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call LEER.CABECERA
  ARC% = 0
  Tempo$ = "ADX_UDT1:TM"+TIME$+".TMP"
  Create Tempo$  As 76					! Temporal proceso
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC

  
!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Q                 = 1
  NRO.StringS.FOUND = 0
  NRO.ARTICULOS     = 0
  NO.VENTA          = 0
  SW.MISC           = 0
  DEPTO%            = 0
  T%                = 0
  PROCESO$ = " PROCESO PRINCIPAL "
  Read #TLOG; LINE INAREA$
  If END #TLOG THEN REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  If LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
	  GoTo NXTRCD
  EndIf
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))	! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 	! ENCONTRAR DELIMITADOR, COMA
    If (P-Q) < 3 THEN \ 	! REVISAR FALTA DE String
    	BEGIN
	  P=0
	  !MEN$ ="FALTA String ID TRANSACCION"
	  !Call ENTRADA.LOG
	  NO.VENTA = -1
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. String
	  GoTo AGAIN		   	! VUELVA A LEER OTRO REGISTRO
	EndIf
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 	! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
    B$ = B$+":" 			! SUMA UN SEMI-COLON
    Q = P + 1 				! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 	! DETERMINA TIPO DE String
    If A = 0 THEN GoSub S0:GoTo AGAIN	! SI ES CHECKOUT TRANSACCION
    If A = 99 THEN GoSub S99:GoTo AGAIN
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
     If (A=0) AND (StringS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GoTo NXTRCD
     If NO.VENTA THEN GoTo NXTRCD		! Si no es una venta leemos otro registro
  Wend
GoTo NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  GJ% = 3
  GoSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GoSub GETUNPK																			! 
  NRO.TRANS$=RIGHT$(A$,4)    												! CAPTURA NUMERO DE TRANSACCION
  GoSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:RC"+TIME$+".RPT"
     Create Ue.Salida$ AS 51 
     Call Cabecera.Reporte
  EndIf
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  GoSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans = 17 Then GoTo NXTRCD
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
     If DUPLICADA Then GoTo NXTRCD									! 
  EndIf
Return

S1:
  GJ% = 3
Return

S2:
  GJ% = 3 		        ! EXTENSION DE ENTRADA
Return

S3:
  GJ% = 3
Return

S5:
  GJ% = 3
Return

S7:
  GJ% = 3!TAX
Return

S9:
  GJ% = 3
Return

S10:
  GJ% = 3
Return

S11:
  GJ% = 3!DTENTRY
Return

S12:
  GJ% = 3!CHANGE PRICE
Return

S13:
  GJ% = 3!LOAN-PICKUP
Return

S14:
  GJ% = 3
Return

S15:
  GJ% = 3
Return

S16:
Return

S20:
  GJ% = 3
Return

S21:
  GJ% = 3
Return

S99:
  GJ% = 3
  GoSub GetUnpk															    ! Codigo del proyecto
  If A$ = "20130829" Then Begin 								! Modulo credito empresarial
     GoSub GetUnpk															! Nit Beneficiario
     Nro.Beneficiario$ = A$
     GoSub GETUNPK                              ! Vlr del credito
     Vlr.Credito$ = A$
     GoSub GETUNPK															! Nit de la empresa
     Nro.Convenio$ = A$
     GoSub GETUNPK4 														! Nombre beneficiario
     Nro.Medico$ = A$
     GoSub GETUNPK															! cupo total credito
     GoSub GETUNPK															! saldo disponible
     GoSub GETUNPK															! Tipo de movimiento
     If A$ = "03" Then Begin                    ! Es una anulacion de pago
     	  Vlr.Credito$ = Str$( Val(Vlr.Credito$) * -1) 
     EndIf
     GoSub GetUnpk4															! Trama mensaje
     GoSub GetUnpk4 														! Nombre empresa
     Nro.Eps$ = A$
     GoSub GetUnpk															! Documento
     Nro.Formula$  = Right$("00000000"+A$,8)
     GoSub GetUnpk4 														! Tipo de credito 1.Cafeteria 2.Otros
     Cod.Beneficiario$ = "00"
     If A$ = "2" Then                          \! Otros creditos
        Cod.Sucursal$ = "555"   Else           \! Definido por Comfandi 30 Oct 2013
        Cod.Sucursal$ = "777"                   ! Definido por Comfandi 19 Dic 2013

     If Val(DM.OPERADOR$) = 0 Then Begin                             ! Todos operadores
       Ue.Data2$ = Right$("00000000000000"+Nro.Convenio$,14)      + \! NUMERO DEL NIT
                   Left$(Nro.Eps$+String$(35," "),35)             + \! NOMBRE EMPRESA
                   Right$("00000000000000"+Nro.Beneficiario$,14)  + \! NUMERO DEL BENEFICIARIO
                   Left$(Nro.Medico$+String$(35," "),35)          + \! NOMBRE BENEFICIARIO
                   Right$("0000000000"+Vlr.Credito$,10)           + \! VALOR DEL CREDITO
                   Nro.Formula$                                   + \! Numero de Credito
                   Cod.Sucursal$                                     ! Codigo de la sucursal
       Call Write.Data.Tmp
     EndIf Else Begin
     	If Val(DM.OPERADOR$) = Val(COD.OPERA$) Then Begin                ! Operador Seleccionado
         Ue.Data2$ = Right$("00000000000000"+Nro.Convenio$,14)      + \! NUMERO DEL NIT
                     Left$(Nro.Eps$+String$(35," "),35)             + \! NOMBRE EMPRESA
                     Right$("00000000000000"+Nro.Beneficiario$,14)  + \! NUMERO DEL BENEFICIARIO
                     Left$(Nro.Medico$+String$(35," "),35)          + \! NOMBRE BENEFICIARIO
                     Right$("0000000000"+Vlr.Credito$,10)           + \! VALOR DEL CREDITO
                     Right$("0000000000"+Vlr.Credito$,10)           + \! VALOR DEL CREDITO
                     Nro.Formula$                                   + \! Numero de Credito
                     Cod.Sucursal$                                     ! Codigo de la sucursal
         Call Write.Data.Tmp
     	EndIf
     EndIf  
  EndIf


Return

GETEXCPT:
  GK% = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If ((GK%-GJ%)-1) > 0 Then \ ! If DATA IN FIELD
    A$ = UNPACK$(MID$(B$,GJ%,(GK%-GJ%)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  Else \
    A$ = "" ! SET A$ = NULLS
  GJ%=GK%+2 ! POINT TO BeginNING OF NEXT FIELD
Return

!*********************************************************************
GETUNPK:
  GK% = MATCH(":",B$,GJ%) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,GJ%,GK%-GJ%)) ! UNPACK FIELD
  GJ%=GK%+1 ! POINT TO BeginNING OF NEXT FIELD
Return
!*********************************************************************

!*********************************************************************
GETUNPK3:
  GK% = MATCH(";",B$,GJ%) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,GJ%,GK%-GJ%)) ! UNPACK FIELD
  GJ%=GK%+1 ! POINT TO BeginNING OF NEXT FIELD
Return
!*********************************************************************
GETUNPK4:
  GK% = MATCH(":",B$,GJ%) ! SEARCH FOR FIELD SEPERATOR
  A$ = (MID$(B$,GJ%,GK%-GJ%)) ! FIELD
  GJ%=GK%+1 ! POINT TO BeginNING OF NEXT FIELD
Return

!*********************************************************************
GETFLAG:
  FLAG = VAL(A$) ! CONVERT FLAG TO Integer
                 ! START BUILDING String TO CONTAIN INDIVIDUAL FLAGS
  If (FLAG AND 00000001H) Then A$ = "1" Else A$ = "0"
  FOR I = 1 TO 15
    FLAG = SHIfT(FLAG,1) ! SET UP NEXT BIT
    If (FLAG AND 00000001H) Then A$ = "1" + A$ Else A$ = "0" + A$
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
If ERR = "SS" Then Resume NXTRCD:
If ERR = "IH" Then Resume 
If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   Resume           
EndIf 

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   Resume           
EndIf 
If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 19 AND (ERR = "OE" OR ERR = "FU") Then Begin
   BAN.PRG$ = "1"
   Resume        
EndIf 

If ERRF% = 19 AND (ERR = "EF") Then Begin
   BAN.PRG$ = "1"
   Resume        
EndIf 
If ERRF% = 50 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Numeracion Fiscal "
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 50 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   Resume        
EndIf

If ERRF% = 55 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Convenios"
    Call MSG.ERR(5,MEN$)                                 ! Mensaje
    Stop 
EndIf 

If ERRF% = 55 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   Resume        
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
