!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : RPPLUPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte plu en promociones       *
!**************************************************
! Version 1.0 4-Jun-2018
!
%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, UQty$, UPrice$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$, Nit$, NitEmpresa$
Integer*1 Global ARC%, INDT%,ItmMisc%
Integer*4 Global CPAG%, CDSC%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%, TOT.QTY%
String    Global IR.USERDATA$, DM.ITEM$, DM.CAJERO$, Xtrailer$

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! Maestro Articulos
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
  Call.ORDER% = 30                                ! Llamado Primera Pantalla D.M
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
  UE.DATA2$ = "COMFANDI"+STRING$(08," ")+"      REPORTE PLU PROMOCIONADOS      "+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "Fecha y Hora del Reporte :20"+Left$(Date$,2)+"/"+Mid$(Date$,3,2)+"/"+Right$(Date$,2) + \
              " -- "+Left$(Time$,2) + ":"+ Mid$(Date$,3,2) + ":" + Right$(Date$,2)
  Call Write.Data
  BAN.PRG$ = "0"
  IR.ITEMCODE$ = Pack$(Right$("000000000000"+Str$(Val(DM.ITEM$)),12))
  %INCLUDE EAMIRRD1.J86
  If BAN.PRG$ = "1" Then IR.ITEMNAME$ = "ARTICULO NO EXISTE" : IR.SALEPRIC$ = PACK$("00")
  DM.ITEM$ = Right$("            "+DM.ITEM$,12)
  Call Format.Valores( Val(Unpack$(IR.SALEPRIC$)) )
  UE.DATA2$ = "Item :"+DM.ITEM$+"   "+IR.ITEMNAME$+ "   Precio Venta :$"+TS.TEMP3$
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
  Call Write.Data
  UE.DATA2$ = "PROMO      Fecha Inicial  Fecha Final    Estado"
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
String X.TMP$
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
  Cnt.Reg%       = 0               ! Contador de registros
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
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
Cnt.Reg%       = 0   ! Contador de registros

End Function 

Function APERTURA.ARCHIVOS.PRINC
Integer*4 Xp1%, Xp2%, Tpromo%
String Lista$, Promos$(2), Plu$, Fecha$, Estado$
Ue.Salida$ = "ADX_UDT1:RPPLUPRM.RPT" 
Create Ue.Salida$ As 51
Call Cabecera.Reporte
Open "PROMODAT:DPROMOTI.DAT" As 26																					! Apertura definicion promociones
If End # 26 Then FIN.DEF.PROMO																							! Si Error de apertura
Dim Promos$(5000,2)
tpromo% = 0
Plu$ = ">"+Str$(Val(Dm.Item$))+"</"
While (1)																																		! recorre archivo hasta el final
    Read #26; Line  Lista$
    Xp1% = Match("</cod>",Lista$,1)
    If Xp1% > 0 Then Begin																									! Hay promocion
       Xp2% = Match(Plu$,Lista$,1)
       If Xp2% > 0 Then Begin																								! Producto encontrado
    	    Tpromo% = Tpromo% + 1 																					  ! total promociones
    	    Promos$(Tpromo%,0) = Mid$(Lista$,11,(Xp1%-11))  									! Codigo de la promocion
          Xp1% = Match("<idt>",Lista$,1)																		!
          Promos$(Tpromo%,1) = Mid$(Lista$,Xp1%+5,10)    										! Fecha inicio promocion
          Xp1% = Match("<fdt>",Lista$,1)																		!
          Promos$(Tpromo%,2) = Mid$(Lista$,Xp1%+5,10)    										! Fecha inicio promocion
       EndIf
    EndIf																																		!
Wend 																																				! Fin recorrida archivo
FIN.DEF.PROMO:
    Close 26
    Xp1% = 0 
    For Xp1% = 1 To Tpromo%
     Fecha$ = Mid$(Promos$(Xp1%,1),3,2) + Mid$(Promos$(Xp1%,1),6,2) +      \!
              Mid$(Promos$(Xp1%,1),9,2)
     Estado$ = "PROMOCION INACTIVA"
     If Val(Date$) >= Val(Fecha$) Then Estado$ = "PROMOCION ACTIVA"         ! Fecha Inicio Promocion
     Fecha$ = Mid$(Promos$(Xp1%,2),3,2) + Mid$(Promos$(Xp1%,2),6,2) +      \!
              Mid$(Promos$(Xp1%,2),9,2)
     If Val(Date$) > Val(Fecha$) Then Estado$ = "PROMOCION INACTIVA"        ! Fecha Final Promocion
     	
     Ue.Data2$ = Right$("       "+Promos$(Xp1%,0),7) + "    " +            \!
                 Promos$(Xp1%,1) + "     " + Promos$(Xp1%,2)  +            \!
                 "     " + Estado$
     Call Write.Data
    Next Xp1%
    Ue.Data2$ = String$(78,"-")
    Call Write.Data
    Ue.Data2$ = "TOTAL PROMOCIONES CARGADAS :"+Str$(tpromo%)
    Call Write.Data
End Function 

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    ! Inicializacion Variables Display Manager
TERM$ = " "					            ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    !
Call DM.ERR(RET.ERR%,INITDM$)				    !
RET.ERR% = OPNDIS("C:\ADX_UPGM\GRPROMV2.PNT")	            ! Apertura de la For ma de pantalla
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
         Call HELP("Reporte Venta Articulos","RPVTAITM.txt") !
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
           Call HELP("Reporte Venta Articulos","RPVTAITM.txt") !
           Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
      
      Call MSG.ERR(05,"Codigo del Item a Consultar")
      DM.ITEM$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG               ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG               ! Si presiona tecla ESC
      WHILE Val(DM.ITEM$) <= 0  
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         DM.ITEM$ = GET.DATOS
         If (ENDF = F3.SALIR) THEN Call SALIDA.PROG            ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG            ! Si presiona tecla ESC
      WEND
           
End Function 
!--- Fin pantalla principal

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

!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  On Error GoTo Err.Appl
  Call INI.VAR.PROG	
  Call LEER.CABECERA
  ARC% = 0
  Open "EAMITEMR" KEYED RECL 77 AS 4
  Call INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  Call TERMINE.PROG
  Stop 
  
Err.Appl:
  ERRORCOD$ = ERR
  P=0
  BAN.PRG$ = "1"

  If ERR = "IH" Then Resume  
  If ERR = "EF" Then Resume

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

  Call TRADUCE.ERROR
  MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  Print MEN$
  MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
  Print MEN$
  Stop 
