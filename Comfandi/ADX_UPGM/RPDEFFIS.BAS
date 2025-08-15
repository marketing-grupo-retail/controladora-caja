!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : RPDEFFIS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reporte Definicion Fiscal        *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$
Integer*1 Global Terror% 
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, UE.LINEAS%, TOT.MOV%

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

 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function


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
  Call.ORDER% = 3                                 ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Function ENTRADA.LOG
  Call MSG.ERR(6,MEN$)                                 ! Mensaje
End Function 


Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Finr$ = Chr$(13) + Chr$(10)
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub Cabecera.Reporte
  CITM% = CITM% + 1 
  UE.DATA2$ = "COMFANDI"+STRING$(08," ")+"REPORTE DEFINICION NUMERACION FISCAL"+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "FECHA DEL REPORTE   20"+DATE$       
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
  Call Write.Data
  UE.DATA2$ = "                                           Rango Fechas "
  Call Write.Data
  UE.DATA2$ = "Caja Pref. Fac.Inic.  Fac.Final Fac.Actual Inici. Final  Nro.Autorizacion"
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
      CALL.ORDER% = 100                                    ! Definicion de la Pantalla Help DM
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
      FOR CNTI% = 1 TO 15                                  !
          READ #19; LINE REG.HLP$
          IF BAN.PRG$ = "0" THEN  Begin                   \!
             REG.HLP$ = Left$(REG.HLP$,76)
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          EndIf 
          IF BAN.PRG$ = "1" THEN BEGIN                    \!
             CNTI% = 16					   !
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
      CALL.ORDER% = 100                                      ! Definicion de la Pantalla Help DM
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
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  Call MSG.ERR(6,MEN$)                                 ! Mensaje
  UE.DATA2$ = STRING$(77,"-")  
  Call Grabacion.Interface
  
  Call Format.VALORES(TOT.MOV%)
  UE.DATA2$ = "Total de  Registros  "+TS.TEMP3$
  Call Grabacion.Interface
  
  If DM.OPCION$ = "2" Then Call CONSULTA.PANTALLA
  If DM.OPCION$ = "1" Then Call OSSHELL("PRINT "+UE.SALIDA$) 
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

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    ! Inicializacion Variables Display Manager
TERM$ = " "					            ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    !
Call DM.ERR(RET.ERR%,INITDM$)				    !
RET.ERR% = OPNDIS("C:\ADX_UPGM\FISCAL.PNT")	            ! Apertura de la For ma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call INICIO1
      MSG$="Seleccione Destino de la impresion"
      Call MSG.ERR(6,MSG$)                                 ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Reporte definicion fiscal","rpdeffis.txt") !
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
         Call HELP("Reporte definicion fiscal","rpdeffis.txt") !
         Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
       
End Function 
!--- Fin pantalla principal

Sub Proceso.Reporte
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%, UE.INDI1%, UE.TARIFA%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$, XCODENSA$, Prov$, IND1$, UE.BIN$, Tarifa$
String Rbuffer$, tag$, C07$, C08$, C09$, c10$, Gr.Fiscal$(2)
Integer*2 Key.Len, Rec.Len
Dim Gr.Fiscal$(200,8)
Ncli% = 0 
Terror% = -1
Open "TF:NMFECO" DIRECT RECL 512 As 33 buffsize 32640  	     ! Apertura control de promociones
REC.NO = 1					             ! Initialize Counter
I% = 1						             ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record

For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Call MSG.ERR(6,"PROCESANDO BLOQUE :"+STR$(REC.NO)+" DE "+STR$(BLK.NUM))
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
   
    Gr.Fiscal$(1,1) = Unpack$(MID$(H$,X+ 0,2))                           ! Numero de terminal
    Gr.Fiscal$(1,2) = Mid$(H$,X+ 2,4)                           				 ! Prefijo
    Gr.Fiscal$(1,3) = Unpack$(Mid$(H$,X+ 6,5))                   				 ! Rango inicial
    Gr.Fiscal$(1,4) = Unpack$(Mid$(H$,X+11,5))                   				 ! Rango final
    Gr.Fiscal$(1,5) = Unpack$(Mid$(H$,X+16,5))                   				 ! Num Actual
    Gr.Fiscal$(1,6) = Unpack$(Mid$(H$,X+21,7))                   				 ! Num Autor
    Gr.Fiscal$(1,7) = Unpack$(Mid$(H$,X+28,3))                   				 ! Fec inical
    Gr.Fiscal$(1,8) = Unpack$(Mid$(H$,X+31,3))                   				 ! Fec final
    
    Gr.Fiscal$(2,2) = Mid$(H$,X+34,4)                           				 ! Prefijo
    Gr.Fiscal$(2,3) = Unpack$(Mid$(H$,X+38,5))                   				 ! Rango inicial
    Gr.Fiscal$(2,4) = Unpack$(Mid$(H$,X+43,5))                   				 ! Rango final
    Gr.Fiscal$(2,5) = Unpack$(Mid$(H$,X+48,5))                   				 ! Num Actual
    Gr.Fiscal$(2,6) = Unpack$(Mid$(H$,X+53,7))                   				 ! Num Autor
    Gr.Fiscal$(2,7) = Unpack$(Mid$(H$,X+60,3))                   				 ! Fec inical
    Gr.Fiscal$(2,8) = Unpack$(Mid$(H$,X+63,3))                   				 ! Fec final

    TOT.MOV% = TOT.MOV% + 1
    Ue.Data2$ = Gr.Fiscal$(1,1)+" "+Gr.Fiscal$(1,2)+" "+Gr.Fiscal$(1,3)+" "+ \
                Gr.Fiscal$(1,4)+" "+Gr.Fiscal$(1,5)+" "+Gr.Fiscal$(1,7)+" "+ \
                Gr.Fiscal$(1,8)+" "+Gr.Fiscal$(1,6)+" "+"FISC"
    Call Grabacion.Interface
    Ue.Data2$ = "     "+Gr.Fiscal$(2,2)+" "+Gr.Fiscal$(2,3)+" "+ \
                Gr.Fiscal$(2,4)+" "+Gr.Fiscal$(2,5)+" "+Gr.Fiscal$(2,7)+" "+ \
                Gr.Fiscal$(2,8)+" "+Gr.Fiscal$(2,6)+" "+"CONT"
    Call Grabacion.Interface

    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33

End Sub 

!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Call LEER.CABECERA
  Ue.Salida$ = "ADX_UDT1:RPDEFFIS.RPT"
  Create Ue.Salida$ AS 51 
  Call Cabecera.Reporte
  Call PANTALLA.PRINCIPAL
  Call Proceso.Reporte
  Call TERMINE.PROG
  Stop 

IO.ARCHIVOS:
Terror% = 0 
BAN.PRG$ = "1"
If ERR = "EF" Then Resume 
	
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print MEN$
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
