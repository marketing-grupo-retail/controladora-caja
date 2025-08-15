!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : GRCUADRE.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte cuadre tienda            *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$, Trx.Esp$(1)
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, Nit$, NitEmpresa$
Integer*1 Global ARC%, INDT%, Pesp%
String    Global IR.USERDATA$, BINES(2)
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%, Pagos(1), Dsctos(1), Ttrx%, Dptos(2)
Integer*2 Global Xtv%, Xvlr%
Integer*4 Global VtaDpto%, Vtaxitem%, Pagxtrx%, Dsctoxtx%, Qty.Bines%, XII%, PJ%, Xloc%, \
                 VentaDpto%, BB.PRECIO, TotDscto%


%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! Maestro Articulos
%Include DMEXTR.J86    		            					! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include BASROUT.J86

!--- Definicion de rutinas de la aplicacion


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

Sub Cabecera.Reporte
  CITM% = CITM% + 1 
  UE.DATA2$ = "Gretail "+STRING$(08," ")+"MOVIMIENTO CUADRE LOG TRANSACCIONES  "+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "FECHA DE MOVIMIENTO "+UE.FECMOV$
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
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


Function TERMINE.PROG
String X.TMP$, Xa$, Xb$, Xc$
Integer*4 XI%, VtaNta%, Tneta%

  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  Call MSG.ERR(5,MEN$)                                 ! Mensaje
  UE.DATA2$ = STRING$(77,"-")  
  Call Grabacion.Interface
  UE.DATA2$ = "VENTAS POR DEPARTAMENTO "
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface

  VentaDpto% = 0
  TotDscto% = 0
  Tneta% = 0
  For XI% = 1 To 999
    If Dptos(XI%,0) <> 0 Then BEGIN 
    	 
    	 VentaDpto%  = VentaDpto%  + Dptos(xi%,0)
    	 TotDscto%   = TotDscto%   + Dptos(xi%,1)
    	 VtaNta% = Dptos(xi%,0) - Dptos(xi%,1)
    	 Tneta% = Tneta% + VtaNta% 
    	 Call Format.VALORES(Dptos(Xi%,0))
    	 Xa$ = Right$("                "+TS.TEMP3$,13)
    	 Call Format.VALORES(Dptos(Xi%,1))
    	 Xb$ = Right$("                "+TS.TEMP3$,10)
    	 Call Format.VALORES(VtaNta%)
    	 Xc$ = Right$("                "+TS.TEMP3$,13)
    	 UE.DATA2$ =  "Departamento: "+Right$("000"+Str$(XI%),3) + " Valor : "+Xa$ + \
    	              " Dscto:"+Xb$ + " Total: "+Xc$
    	 Call Grabacion.Interface
    ENDIF 
  Next XI%
  UE.DATA2$ = " "
  Call Grabacion.Interface
  Call Format.VALORES(VentaDpto%)
  Xa$ = Right$("                "+TS.TEMP3$,13)
  Call Format.VALORES(TotDscto%)
  Xb$ = Right$("                "+TS.TEMP3$,13)
  Call Format.VALORES(Tneta%)
  Xc$ = Right$("                "+TS.TEMP3$,13)
  
  UE.DATA2$ = "TOTAL DEPARTAMENTOS: "+Xa$ + " Dsctos: "+Xb$ + " Neto: "+Xc$
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface

  UE.DATA2$ = "FORMAS DE PAGO REGISTRADAS "
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface


  NRO.PAGOS% = 0
  VTAS.TOTALES% = 0 
  XI% = 0
  For XI% = 1 To 100
    If PAGOS(XI%) <> 0 Then BEGIN 
    	 Call Format.VALORES(PAGOS(Xi%))
    	 VTAS.TOTALES% = VTAS.TOTALES% + Pagos(xi%)
    	 TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
    	 UE.DATA2$ =  "Tipo Variedad "+Right$("00"+Str$(XI%),2) + " Valor : "+TS.TEMP3$
    	 Call Grabacion.Interface
    ENDIF 
  Next XI%
  Call Format.Valores(VTAS.TOTALES%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  UE.DATA2$ = " "
  Call Grabacion.Interface
  UE.DATA2$ = "TOTAL PAGOS CAJA   : "+TS.TEMP3$
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface
 

  UE.DATA2$ = " "
  Call Grabacion.Interface
  UE.DATA2$ = "DESCUENTOS APLICADOS "
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface
  
  XI% = 0
  For XI% = 1 To 100
    If DSCTOS(XI%) <> 0 Then BEGIN
    	 Call Format.VALORES(DSCTOS(Xi%))
       TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
    	 NRO.PAGOS% = NRO.PAGOS% + DSCTOS(XI%)
    	 UE.DATA2$ = "GRUPO DSCTO "+Right$("00"+Str$(XI%),2) + " Valor : "+TS.TEMP3$
    	 Call Grabacion.Interface
    ENDIF 	 
  Next XI%

  
  UE.DATA2$ = " "
  Call Grabacion.Interface
  UE.DATA2$ = "TOTALES DE CUADRE    "
  Call Grabacion.Interface
  UE.DATA2$ = "---------------------------"
  Call Grabacion.Interface


  Call Format.Valores(VTAS.TOTALES% + NRO.PAGOS%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  UE.DATA2$ = "Ventas Brutas Tienda      : "+TS.TEMP3$
  Call Grabacion.Interface

  Call Format.VALORES(NRO.PAGOS%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)
  UE.DATA2$ = "Total Descuentos Aplicados : "+TS.TEMP3$
  Call Grabacion.Interface

  
  Call Format.VALORES(VTAS.TOTALES%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)  
  UE.DATA2$ = "Total Formas de Pago       : "+TS.TEMP3$
  Call Grabacion.Interface
  
  Call Format.VALORES(VtaDpto%-NRO.PAGOS%)
  TS.TEMP3$ = Right$("                "+TS.TEMP3$,16)  
  UE.DATA2$ = "Total Vtas Productos       : "+TS.TEMP3$
  Call Grabacion.Interface

  UE.DATA2$ = " "
  Call Grabacion.Interface

  UE.DATA2$ = "Transacciones Especiales     "
  Call Grabacion.Interface

  For Xi% = 1 To Ttrx%
    UE.DATA2$ = trx.esp$(xi%)
    Call Grabacion.Interface
  Next XI%

  UE.DATA2$ = "Reporte Bines Especiales "
  Call Grabacion.Interface

  For XI% = 1 To Qty.Bines%
      UE.DATA2$ = Bines(XI%,0) + " = "+ Bines(XI%,1)
      Call Grabacion.Interface
  Next XI%

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
  Vtaxitem% = 0
  Pagxtrx% = 0
  Dsctoxtx% = 0
  Dim PAGOS(100) 
  Dim Dsctos(100)
  Dim Dptos(1000,1)
  Dim Trx.Esp$(1000)
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
T%                = 0
NRO.REG           = 0      ! Control del header a cero 
Dim RECIBOS$(100,2)

CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
Cnt.Reg%       = 0   ! Contador de registros
NitEmpresa$ = "890310455"
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
     Call ENTRADA.LOG     
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
         Call HELP("Reporte Pagos Electronico","rpmovpay.txt") !
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
           Call HELP("Reporte Pagos Electronico","rpmovpay.txt") !
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

Sub Parametros.Interface
String Xtmp$, TS.TEMP1$, ue.plan$
Ue.Plan$ = ""
Dim Bines(100,2)
Qty.Bines% = 0
  OPEN "ASCNTRL" AS 94							  	   																! Apertura archivo parametrizacion 
  IF END #94 THEN UE.FIN.CRDEMP         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[INTERFACE TEF]" Then Begin		              				! Parametros interface TEF
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Ue.Plan$   = Mid$(TS.TEMP1$,30,80)           				    					! Lista formas de pago validas interface
         While (1)																													! Toma lista bines excentos 
           Read #94; TS.TEMP1$     																					! Lectura registro                 
           Xtmp$ = Mid$(TS.TEMP1$,30,06)                  				          ! Bin excento interface
           If Xtmp$ = "999999" Then GoTo UE.FIN.CRDEMP  										! Sale del ciclo de carga          
           Qty.Bines% = Qty.Bines% + 1 																			! Aumenta apuntado
           Bines(Qty.Bines%,0) = Xtmp$																			! Asigna bin excento
           Bines(Qty.Bines%,1) = "0"
         Wend 
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.CRDEMP:                                                           !
     Close 94																																! Cierra archivo

End Sub 


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS

  DATO.SO$ = COMMAND$                                             ! Dato S.O
  If DATO.SO$ = "VERSION" Then BEGIN 
     Print "REPORTE CUADRE TIENDA Ver 1.0  21/Jun/2018"
     Stop 
  ENDIF 
  Call Parametros.Interface
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  Call LEER.CABECERA
  ARC% = 0
  Open "EAMITEMR" KEYED RECL 77 AS 4
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
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. STRING
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
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 

S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  If (Vtaxitem%-Dsctoxtx%) <> Pagxtrx% Then Begin
  	 Ttrx% = Ttrx% + 1 
  	 Trx.Esp$(Ttrx%) =  "Caj:"+Terminal$+" Trx:"+NRO.TRANS$+" VLR:"+str$(Vtaxitem%-Dsctoxtx%)+" Pago:"+str$(Pagxtrx%)
  EndIf
     Vtaxitem% = 0
     Pagxtrx% = 0
     Dsctoxtx% = 0

  GOSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GOSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  GOSub GETUNPK																			!
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     UE.FECMOV$ = FECHA$
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:GRCUADRE.TXT"
     Create Ue.Salida$ AS 51 
     Call Cabecera.Reporte
  EndIf
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  GOSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  !If tipo.trans > 5 Then GOTO NXTRCD
  !If TIPO.TRANS = 2 Then X.CAMBIO% = -1 
  	
  GOSub GETUNPK
  NRO.REG=VAL(A$)  		        											! NUMERO DE StringS EN TODA LA TRANSACCION
  StringS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 											! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
		NO.VENTA = 1
  EndIf Else	\
	StringS.COMPLETOS = CUENTE.StringS								! FUNCION PARA CORROBORAR INTEGRIDAD DE StringS
  GOSub GETUNPK
  COD.OPERA$=A$  																		! NUMERO DEL OPERADOR
  GoSub GETUNPK 													  			  ! PASSWORD DEL OPERADOR
  GOSub GETUNPK																			! Venta positiva
  GROSS.POSITIVO$=A$
  GOSub GETUNPK																			! Venta negativa
  GROSS.NEGATIVO$=A$

!  IF VAL(GROSS.POSITIVO$) > 2000000 THEN BEGIN 
!  	 Ttrx% = Ttrx% + 1 
!  	 Trx.Esp$(Ttrx%) =  "Caj:"+Terminal$+" Trx:"+NRO.TRANS$+" VLR:"+GROSS.POSITIVO$
!  ENDIF

  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD									! 
  EndIf
Return

S1:
  J = 3
 GoSub GETUNPK
 GoSub GETUNPK
 PRECIO$ = A$																																	 	! VLR VENTA
 GoSub GETUNPK
 Depto$ = A$
 Depto% = Val(A$)
 GoSub GETUNPK                                                                 	! Cupon Family 3 y 3 Long 6
 GoSub GETUNPK                                                                 	! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
 GoSub GETUNPK                                        													! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
 INDICAT2 = VAL(A$)
 GoSub GETUNPK                                        													! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
 INDICAT3 = VAL(A$)
 ALTO$ = LEFT$(A$,1)
 BAJO$ = RIGHT$(A$,1)
 SIGNO$ = "+"
 !If ALTO$ = "4" Then SIGNO$ = "-"
 !If ALTO$ = "5" Then SIGNO$ = "-"
 If (INDICAT2 AND 0080H) Then SIGNO$ = "-"
 If (INDICAT2 AND 0040H) Then SIGNO$ = "-"
 If SIGNO$ = "+" Then Begin
    VtaDpto% = VtaDpto% + Val(precio$)
    vtaxitem% = vtaxitem% + Val(precio$)
    Dptos(Depto%,0) = Dptos(Depto%,0) + Val(precio$)
 EndIf Else Begin
	  VtaDpto%  = VtaDpto% - Val(precio$)
	  vtaxitem% = vtaxitem% - Val(precio$)
    Dptos(Depto%,0) = Dptos(Depto%,0) - Val(precio$)
 EndIf

Return

S2:
  J = 3 		        ! EXTENSION DE ENTRADA
Return

S3:
  J = 3
  GoSub GETUNPK     ! GRUPO DSCTO
  Xtv% = Val(A$)
  GoSub GETUNPK     ! PTG DSCTO
  GoSub GETUNPK     ! VALOR DSCTO
  Dsctos(Xtv%) = Dsctos(Xtv%) + Val(A$)
  Dsctoxtx% = Dsctoxtx% + Val(A$)
Return

S4:
  J = 3
  GoSub GETUNPK     ! GRUPO DSCTO
  Xtv% = Val(A$)
  GoSub GETUNPK     ! PTG DSCTO
  GoSub GETUNPK     ! VALOR DSCTO
  Dsctos(Xtv%) = Dsctos(Xtv%) - Val(A$)
  Dsctoxtx% = Dsctoxtx% - Val(A$)
Return


S5:
 J = 3 
  GoSub GETUNPK     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)  
  GoSub GETUNPK     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) + Val(A$)
  Pagxtrx% = pagxtrx% + Val(A$)
  If Pesp% = -1 Then Begin
  	 Bines(Xloc%,1) = Str$( Val(Bines(Xloc%,1)) + Val(A$) )
  EndIf
  Pesp% = 0
Return

S6:
 J = 3 
  GoSub GETUNPK     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)  
  GoSub GETUNPK     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) - Val(A$)
  Pagxtrx% = pagxtrx% - Val(A$)
  If Pesp% = -1 Then Begin
  	 Bines(Xloc%,1) = Str$( Val(Bines(Xloc%,1)) - Val(A$) )
  EndIf
  Pesp% = 0

Return

S7:
  J = 3!TAX
RETURN

S9:
  J = 3
  GoSub GETUNPK     ! TIPO VARIEDAD PAGO
  Xtv% = Val(A$)
  !Xtv% = 11  
  GoSub GETUNPK     ! VALOR PAGO
  Pagos(Xtv%) = Pagos(Xtv%) - Val(A$)
  Pagxtrx% = pagxtrx% - Val(A$)
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
  GOSUB GETUNPK 																														!  proyecto
  If A$ = "51" Then begin 																									! UD de Tef 
  	 GOSUB GETUNPK4																													! String de TEF
     For XII% = 1 To Qty.Bines%																							! Analiza si trx con bin excento
       PJ% = MATCH(Bines(XII%,0),A$,1)																				! Busca bin
       If PJ% > 0 Then Begin																								! Si es bin excento
       	  Bines(XII%,1) = Str$( Val(Bines(XII%,1)) + 1 )
       	  Xloc% = Xii%
       	  Pesp% = -1
       	  XII% = XII% + 2																											! Sale del proceso
       EndIf																																!
     Next XII%																																!
  EndIf					
!--- Add GR-OVS 28 ENE 2010  
  If A$ = "97"   Then Begin                										! Si es descuento motor promociones v2
  	 GoSub GetUnpk                                           	! Codigo del producto
  	 GoSub GetUnpk                                           	! Valor  del descuento
  	 BB.PRECIO = Val(A$)
  	 GoSub GetUnpk                                           	! Codigo promocion
  	 GoSub GetUnpk                                           	! descriptor producto o flag iva nuevo Str
  	 GoSub GetUnpk                                           	! cod depto
  	 Depto% = Val(A$)
  	 GoSub GetUnpk                                           	! estructura comercial
  	 GoSub GetUnpk                                           	! Signo de la operacion
  	If A$ = "01" Then BB.PRECIO = BB.PRECIO * -1 
  	 Dptos(Depto%,1) = Dptos(Depto%,1) + BB.PRECIO
  EndIf 																										 	! Fin dscto motor promociones v2
  
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
If ERR = "IH" Then RESUME
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
