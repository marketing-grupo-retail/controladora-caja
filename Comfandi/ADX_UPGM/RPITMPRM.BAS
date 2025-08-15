!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : RPITMPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Reporte Items Promocion          *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$, Ue.Data3$, Xsgn$, NRO.REG$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$, NAME$, TIPO.TRANS$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$, FECHA.ARCH$
Integer*1 Global ARC%, trx.inven%, Terror%
Integer*4 Global UE.LINEAS%, U.STR%(1), TOT.MOV%, Precio%, Indi2%, Compra%, Tmov%
String    Global XCOD.ITEM$, XDEPTO$, XCANTIDAD$, XPRECIO$, XPESO$, PESO$, Gondola$, AreaInv$, Xbuff$, IR.USERDATA$

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! EAMITEMR
%Include DMEXTR.J86    		                      ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include BASROUT.J86

!--- Definicion de rutinas de la aplicacion

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Sub SALIDA.PROG
  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
End Sub 

Function INICIO1
  Call.ORDER% = 21                                ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

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

Function ENTRADA.LOG
  Call MSG.ERR(5,MEN$)                                 ! Mensaje
End Function 

Function CONSULTA.PANTALLA                          ! Parametro Programa y archivo
  STRING HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  INTEGER*2 CNTI%, NRG%
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			                   ! Despliege de la pantalla
      Terror% = -1
      HLP.FILE$ = UE.SALIDA$
      OPEN HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      IF Terror% = 0 THEN BEGIN                           \!
         MSG1$ = "Archivo Reporte "+HLP.FILE$+" No Existe o Sin Informacion"
         CALL MSG.ERR(19,MSG1$): WAIT;2500: EXIT FUNCTION
      ENDIF
    INP2$ = " ": NRG% = 1
    WHILE (INP2$ <> "X" )
      Terror% = -1
      FOR CNTI% = 1 TO 17                                  !
          READ #19; LINE REG.HLP$
          IF Terror% = -1 THEN                          \!
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          IF Terror% = 0 THEN BEGIN                    \!
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
    Close 19
    Terror% = -1
END FUNCTION
!--- Fin de la funcion de ayuda

Function TERMINE.PROG
String X.TMP$
  Call SALIDA.PROG  
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
 FECMOV$ = DATE$
End Function
!--- Fin de la funcion de lectura

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
  UE.DATA2$ = "COMFANDI"+STRING$(08," ")+"    REPORTE PRODUCTO EN PROMOCION    "+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "Fecha del reporte :20"+MID$(DATE$,1,2)+"/"+MID$(DATE$,3,2)+"/"+MID$(DATE$,5,2) +\
              "                               Centro Costo: "+ALMACEN$
  Call Write.Data
  UE.DATA2$ = "Tipo Promocion (TP): 1.Descuento en Valor  2.Descuento en Porcentaje"
  Call Write.Data
  UE.DATA2$ = "                     3.Precio Final        4.Link y Promocode       "
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
  Call Write.Data
  UE.DATA2$ = "     PLU   DESCRIPCION         TP   BENEFICIO  PROMOCION"
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

Sub PROCESA.ITEMS.PFIZER
String ALM$, SKU$, TP$, NAME$, VLR$, PRM$ 
Call Cabecera.Reporte
Open "ADX_UDT1:RPITMPRM.TM2" As 61
If End #61 THEN UE.FIN.PFIZER
While (1)
    Read #61;SKU$, NAME$, TP$, VLR$, PRM$ 
    VLR$ = Right$("        "+Str$(Val(VLR$)),8)
    Ue.data2$ = SKU$+"   "+NAME$+"   "+TP$+"    "+VLR$+"       "+PRM$
    Call Grabacion.Interface
Wend
UE.FIN.PFIZER:
  Close 61
  Ue.Data2$ = String$(77,"-")
  Call Grabacion.Interface
End Sub 

Sub BUSCA.ITEMS.PFIZER
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$
Integer*2 Key.Len, Rec.Len
Ncli% = 0 

Open "MKTP:MKPPRITM.DAT" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de promociones
Create "ADX_UDT1:RPITMPRM.TMP" As 61
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
Terror% = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Call MSG.ERR(5,"PROCESANDO BLOQUE :"+STR$(REC.NO)+" DE : "+STR$(BLK.NUM))  ! PROCESA ARCHIVO
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$ = Unpack$(MID$(H$,X+0,8))                           ! SKU
    C02$ = (MID$(H$,X+8,1))                                  ! Tipo Promocion
    C03$ = Unpack$(MID$(H$,X+9,4))                           ! Valor Promocion
    C04$ = Unpack$(MID$(H$,X+13,2))                          ! Link
    C04$ = Unpack$(MID$(H$,X+15,2))                          ! Promocode
    C04$ = Unpack$(MID$(H$,X+17,12))                         ! Uso futuro
    C04$ = Unpack$(MID$(H$,X+29,2))                          ! Cod Promocion
    C01$ = LEFT$(C01$,12)                                    ! Toma Codigo el Item
    If Val(C01$) = Val(DM.CODIGO$) Then Begin 
       IR.ITEMCODE$ = Pack$(Right$("000000000000"+Str$(Val(C01$)),12))
       C01$ = Right$("        "+Str$(Val(C01$)),8)
       Terror% = -1
       %INCLUDE EAMIRRD1.J86                                    ! Lectura EamItemr
       If Terror% <> -1 Then IR.ITEMNAME$ = "*ITEM NO DEFINIDO*"
       UE.DATA2$ = C01$+","+IR.ITEMNAME$+","+C02$+","+C03$+","+C04$
       X.Len% = Len(Ue.Data2$)						  					  				 ! Toma longitud del registro
       X.Lec$ = "C"+Str$(X.len%)+" C2"								  				 ! Arma estructura de grabacion
       Write form X.Lec$; #61 ; Ue.Data2$, Finr$  							 ! Graba registro
    EndIf
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 Close 61
 WAIT ; 100
 Call MSG.ERR(5,"GENERANDO INFORME, ESPERE....")
 Call OSSHELL("SORT -+1 < ADX_UDT1:RPITMPRM.TMP > ADX_UDT1:RPITMPRM.TM2")
End Sub 

Sub VALIDA.ITEM.PFIZER  																										! Busca Precio Item
    Terror% = -1                                                            ! Control de errores
    IR.ITEMCODE$ = Pack$(Right$("000000000000"+DM.CODIGO$,12))		    			! Llave de busqueda
    %INCLUDE EAMIRRD1.J86																										! Lectura del registro
    If Terror% = -1 Then Begin 
      If UNPACK$(IR.INDICAT2$) = "05" Then Begin 															! Si metodo precio 5
    	   IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$														! Toma Item Ligado
    	   %INCLUDE EAMIRRD1.J86																								! Lectura del registro
    	   If Terror% = -1 Then DM.CODIGO$ = UNPACK$(IR.ITEMCODE$)
      EndIf																																		!
    EndIf
End Sub 

Function PROCESA.REPORTE
Call INICIADM 				                    ! Inicializacion Variables Display Manager
TERM$ = " "					            ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    !
Call DM.ERR(RET.ERR%,INITDM$)				    !
RET.ERR% = OPNDIS("C:\ADX_UPGM\RPMOVREC.PNT")	            ! Apertura de la For ma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
FINR$=CHR$(13)+CHR$(10)
Call INICIO1 
Ue.Salida$ = "ADX_UDT1:RPITMPRM.RPT"
Create Ue.Salida$ As 51 
      MSG$="Seleccione Destino de la consulta "
      Call MSG.ERR(5,MSG$)                                 ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Reporte Item Promocion   ","rpitmprm.txt") !
         Call INICIO1
      ENDIF
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                 ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                 ! Si presiona tecla ESC
      WHILE (MATCH(DM.OPCION$,"12",1) = 0 OR DM.OPCION$ = " ")
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                         ! Captura dato en pantalla
        DM.OPCION$ = INP$                                   ! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN \
           Call HELP("Reporte Actual. Items    ","rpitmprm.txt") !
           Call INICIO1
        ENDIF
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
      Call MSG.ERR(05,"Digite PLU / Codigo Barras a Consultar")
      DM.CODIGO$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                  ! Si presiona tecla ESC
      WHILE (DM.CODIGO$ = " " Or Val(DM.CODIGO$) <= 0)
        RET.ERR% = NXTF(-2)
        Call DM.ERR(RET.ERR%,NXTF$)
        DM.CODIGO$ = GET.DATOS                              ! Asigna valor capturado
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                ! Si presiona tecla ESC
      WEND
Call VALIDA.ITEM.PFIZER
If Terror% = -1 Then Begin
   Call BUSCA.ITEMS.PFIZER
   Call PROCESA.ITEMS.PFIZER
EndIf Else Begin 
	 Call MSG.ERR(5,"CODIGO PRODUCTO NO EXISTE EN MAESTRA")
	 WAIT ; 2500
EndIf
End Function 
!--- Fin pantalla principal

Sub MENSAJES
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$
Integer*2 Key.Len, Rec.Len
Ncli% = 0 

UE.DATA2$ = "MENSAJES DEFINIDOS PARA EL SISTEMA"
Call Grabacion.Interface
UE.DATA2$ = "TIPO MENSAJE           MSG#  COMPRA MIN."
Call Grabacion.Interface

Open "MKPDEFPR" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de promociones
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
Terror% = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Call MSG.ERR(5,"PROCESANDO MENSAJES :"+STR$(REC.NO)+" DE : "+STR$(BLK.NUM))  ! PROCESA ARCHIVO
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$ = Unpack$(MID$(H$,X+0,9))                           ! PROMOCION
    C02$ = Unpack$(MID$(H$,X+9,2))                           ! Nro Mensaje
    C03$ = (MID$(H$,X+11,10))                                ! Valor control
    C04$ = (MID$(H$,X+21,45))                                ! 
    C01$ = LEFT$(C01$,12)                                    ! Toma Codigo el Item
    If Left$(C01$,2) <> "31" Then BEGIN 
    	 If Left$(C01$,2) = "30" Then C01$ = "PRESENCIA ITEM       "+Right$(C01$,12)
    	 If Left$(C01$,2) = "32" Then C01$ = "NO PRESENCIA DPTO    "+Right$(C01$,12)
    	 If Left$(C01$,2) = "33" Then C01$ = "COMPRA MINIMA        "
       If Left$(C01$,2) = "34" Then C01$ = "NO PRESENCIA PLU     "+Right$(C01$,12)
       If Left$(C01$,2) = "35" Then C01$ = "PRESENCIA DEPTO      "+Right$(C01$,12)
       If Left$(C01$,2) = "36" Then C01$ = "PRESENCIA FORMA PAGO "+Right$(C01$,12)
       If Left$(C01$,2) = "37" Then C01$ = "TIPIFICACION CLIENTE "+Right$(C01$,12)
       UE.DATA2$ = C01$+"  "+C02$+"  "+C03$
       Call Grabacion.Interface
    ENDIF 
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 WAIT ; 100
 UE.DATA2$ = STRING$(77,"-")  
 Call GRABACION.INTERFACE
End Sub 

Sub MENSAJES.RIFAS
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$
Integer*2 Key.Len, Rec.Len
Ncli% = 0 
UE.DATA2$ = "PARTICIPACION ITEM EN RIFAS       "
Call Grabacion.Interface
Open "TF:A0004" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de RIFAS
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
Terror% = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Call MSG.ERR(5,"PROCESANDO RIFAS    :"+STR$(REC.NO)+" DE : "+STR$(BLK.NUM))  ! PROCESA ARCHIVO
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$ = Unpack$(MID$(H$,X+0,8))                           ! PROMOCION
    If Val(RIGHT$(C01$,12)) = Val(DM.CODIGO$) Then Begin 
       UE.DATA2$ = "ITEM :"+Left$(C01$,12)+" RIFA : "+Right$(C01$,4)
       Call Grabacion.Interface
    EndIf
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 WAIT ; 100
 UE.DATA2$ = STRING$(77,"-")  
 Call GRABACION.INTERFACE
End Sub 

Sub HAPPY.HOUR
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$
Integer*2 Key.Len, Rec.Len
Ncli% = 0 
UE.DATA2$ = "PARTICIPACION ITEM EN HAPPY HOUR  "
Call Grabacion.Interface
Open "TF:ITMHPH" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de RIFAS
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
Terror% = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  Call MSG.ERR(5,"PROCESANDO HAPPY HOUR :"+STR$(REC.NO)+" DE : "+STR$(BLK.NUM))  ! PROCESA ARCHIVO
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$ = MID$(H$,X+0,16)                                   ! PROMOCION
    C02$ = Unpack$(MID$(H$,X+16,7))                          ! PRECIO FINAL
    C02$ = Right$("          "+Str$(Val(C02$)),10)
    If Val(LEFT$(C01$,12)) = Val(DM.CODIGO$) Then Begin 
       UE.DATA2$ = "ITEM :"+Left$(C01$,12)+" PRECIO FINAL : "+C02$+ " TIPIFICACION :"+Right$(C01$,4)
       Call Grabacion.Interface
    EndIf
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 WAIT ; 100
 UE.DATA2$ = STRING$(77,"-")  
 Call GRABACION.INTERFACE


End Sub 

!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  On Error GoTo ErrAppl
  Open "EAMITEMR" KEYED RECL 77 AS 4
  Call LEER.CABECERA
  Call PROCESA.REPORTE
  Call MENSAJES
  Call MENSAJES.RIFAS
  Call HAPPY.HOUR
  If DM.OPCION$ = "1" Then Call CONSULTA.PANTALLA
  If DM.OPCION$ = "2" Then Call OSSHELL("PRINT "+UE.SALIDA$) 
  Call TERMINE.PROG
  Stop 

ErrAppl:
  Terror% = 0
  ERRORCOD$ = ERR
  Call TRADUCE.ERROR
  If ERR = "OE" Then Resume 
  If ERR = "EF" Then Resume 
  MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  Print MEN$
  MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
  Print MEN$
  STOP
!*********************************************************************
