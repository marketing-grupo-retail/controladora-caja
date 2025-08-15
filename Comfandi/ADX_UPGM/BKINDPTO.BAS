!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : BKINDPTO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Interface Ventas Departamentos   *
!**************************************************
!

%ENVIRON C                                                                                                                                             ! Ambiente de controlador

%Include POSPVARI.BAS                              ! Variables del programa
%Include EXIRTSVA.J86                              ! Variable EAMITEMR

String Global CT.TEMP3$, CT.TEMP1$, CT.DOLLARSEP$, CT.CENTSEP$, aString$, FECMOV$

String    Global UE.AUTORIZA$, DATO.SO$, SERIAL$, ARC$, LOG$, Vlr.Venta$, N.Cliente$, NF.CLIENTE$
Integer*4 Global PP, TAM%, Len.REG%, CITM%, CPAG%, CDSC%, \
                NRO.REG%, CR%, CITMD%, CCANASTA%
Integer*1 Global ARC%, COEF%, enable.Corr%
String    Global HORA.FINAL$, FILEOUT$, FECHA.ARCH$, Asc.Mod.DateTime$, TIPO.DOC$
String    Global STORELINE$, Factura$, VTA.ITEM$(2), VTA.PAGO$(2), Ctrl.Trx$, VTA.DSCT$(2), VTA.DSCT2$(2), VTA.CNTAS$(2)
String    Global UE.CLIENTE$, UE.PUNTOS$, UE.REDIME$, UE.TMP1$, UE.TMP2$, UE.COMPRA$
String    Global Asc.Lcl.Rrn$, Asc.Lcl.Autor$, Asc.Lcl.Fneg$,  \
                Asc.Lcl.Rthos$, Asc.Lcl.Comer$, Asc.Lcl.Idpos$, U.Prefijo$, FACTURA.ORI$, Ue.Data2$

Integer*4 Global Vlr.Iva%, TOT.CON.IVA%, TOT.SIN.IVA%, TOT.DEL.IVA%, U.Tmp1%, U.Tmp2%, Vlr.Venta%

!Integer*4 Global ASC.IR.PRECIO2%, ASC.IR.QTYORWGT%, ASC.IR.UNDMIN%

String    Global Registro$(1), INI.MOV$, FIN.MOV$, UE.RTA$(1), DM.LOTE$, Ofile$, Tiva$(1)

String    Global \
         CUSTNUM$,     RSTAT$,     LNAME$,     FNAME$,  \!
         ALINE1$,      ALINE2$,    CITY$,      STATE$,  \!
         ZIP$,         ZIPEXIT$,   HPHONA$,    HPHONE$, \!
         BPHONA$,      BPHONE$,    DRIVER$,     HSTORE$,\!
         CUSTDEMO$,                SSNBREXT$,  SSNBR$,  \!
         GEndER$,      INCOME$,    BDATE$,     EDATE$,  \!
         FSIZE$,       CHAGE1$,    CHAGE2$,    CHAGE3$, \!
         CHAGE4$,      CHAGE5$,    CHAGE6$,    CHAGE7$, \!
         TNBRA$,       TNBRR$,     TPADJP$,    TPADJN$, \!
         LADNBR$,      LRDNBR$,    LADATE$,    LRDATE$, \!
         CDATE$,       LRCDATE$,   CUSAUT$,    ALIAS$,  \!
         REFCUS$,      LASTRC$,    STATEEXT$,  ZIPASC$, \! Segun formato
         ZIPEXTASC$,   DIVEREXT$,  USERDATA$,  Old.Item$

String Global    Asc.Fis.Cliente$,                 \! IdentIficacion del cliente
                 Asc.Fis.Nombre$,                  \! Nombre del cliente
                 Asc.Fis.Direccion$,               \! Direccion del Cliente
                 Asc.Fis.Telefono$,                \! Telefono del cliente
                 TOT.DEL.IVA$,                     \!
                 TOT.SIN.IVA$,                     \!
                 TOT.CON.IVA$                       !
Integer*4 Global TotGral(1), InTrx%
String    Global bufferf$, lecf$, FILEOUT.NF$, FILEOUT.MAR$, FILEOUT.MSM$, FILEOUT.MN$
REAL      GLOBAL Xqty%, Precio.Pesado%
String    GLOBAL IR.USERDATA$

!--- Definicion de rutinas de la aplicacion
%Include EAMITEMR.J86                              ! Variable EAMITEMR

%Include DMEXTR.J86                                ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS                                        ! Rutinas Comunes
%Include BASROUT.J86                   !

SUB ADXSERVE(RET,FUNC,PARM1,PARM2) External                  ! Msg background
  Integer*4 RET
  Integer*2 FUNC,PARM1
  String PARM2
End SUB

SUB ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) External
  Integer*4 RETC
  String INFILE, OUTFILE
  Integer*2 OPT0, OPT1
End SUB

Function GETN4(P1$,P2) External
 Integer*4 GETN4
 String P1$
 Integer*2 P2
End Function

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


Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #39 ; Rbuffer$, Finr$           							! Graba registro
End Sub 



Sub Genere.Interface
INTEGER *4 KEY.LEN,         \! length of KEY
           REC.LEN,         \! length of RECORD
           IOS,             \! IO SESSION #
           IOT,             \! IO SESSION # for Output File 
           X,               \! record displacement
           R.S               ! RECORDS ON A GIVEN SECTOR
INTEGER *4 REC.NO,          \! # OF CURRENT RECORD
           BLK.NUM,         \! TOTAL # OF RLOCKS
           R,               \! record count
           MAX.R.SEC         ! MAX # OF RECORDS ON A SECTOR
STRING     FILEN$,          \! NAME OF KEYED FILE
           FIOUT$,          \! NAME OF OUTPUT FILE
           KEY$,            \! KEY
           WRT.KEY$,        \! KEY to write output file
           DEP$,            \! Department Code packed
           WRT.DEP$,        \! Department Code to write output file
           PRI$,            \! Item Record Price packed
           WRT.PRI$,        \! Item Record Price to write output file
           H$,              \! work area
           FSEP$,           \! Fields separator
           FINR$,           \! End of record characters
           PZERO$,          \! Packed zeros
           WRT.DESC$         ! DESCRIPTION OF Item
String Plu$, Cant$, Vta$, Impo$, Fsgn$
Integer*4 Cipo%, UE.TARIFA%, Baseiva%, Unita%
REAL IVA%, Itm%
InTrx% = 0
OPEN "ADX_UDT1:TMPDPTO.DAT" DIRECT RECL 512 AS 11 
REC.NO = 1
READ FORM "T43 I4 I2 T55 I2 C456"; #11, REC.NO;           \
     BLK.NUM, REC.LEN, KEY.LEN, H$
 PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))
 MAX.R.SEC = 508/REC.LEN
Locate 16,1 : Print "Bloques a Procesar :"+str$(blk.num)
FOR REC.NO = 2 TO BLK.NUM                        ! Cicle to read all blocks  
  READ FORM "T5 C508"; #11, REC.NO;  H$          ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)    ! Extract First key
  Locate 17,1 : Print "Bloques Procesados :"+str$(REC.NO)
  While KEY$  NE  PZERO$                         !  INSIDE SECTOR LOOP
    R.S = R.S + 1                                !  RECORDS ON THIS SECTOR
    R = R + 1                                    !  COUNT RECORDS
  	Plu$   = UNPACK$(MID$(H$,X+0,8))             ! Departamento
  	Cant$  = UNPACK$(MID$(H$,X+8,5))             ! Cantidad
  	Vta$   = UNPACK$(MID$(H$,X+13,5))            ! Venta
  	Impo$  = UNPACK$(MID$(H$,X+18,5))            ! Impoconsumo
    Cant$  = TRANSLATE$(Cant$,"=","-")
    Vta$   = TRANSLATE$(Vta$,"=","-")
    Fsgn$ = "+"
    If Val(Cant$) < 0 Then Cant$ = Str$( Val(Cant$) * -1)
    If Val(Vta$) < 0 Then begin 
    	 Vta$   = Str$( Val(Vta$) * -1)
    	 Fsgn$ = "-"
    endif 
    	   
    Ue.Data2$ = "2" + Right$(plu$,16)  +               \! Estructura Comercial
                     Right$(Fecmov$,4) +               \! Fecha del movimiento
                     Right$("0000000000"+Cant$,10) +   \! Cantidad vendida
                     Fsgn$  +                          \! Signo operacion
                     Right$("00000000000"+Vta$,11)      ! Valor vendido

    Call Write.Data
    X = X + REC.LEN                              ! INDEX TO NEXT KEY    
	  KEY$ = MID$(H$,X,KEY.LEN)                    ! PICK UP NEXT KEY
    If R.S = MAX.R.SEC THEN KEY$ = PZERO$
   WEND
NEXT REC.NO
CC1:  
Locate 17,1 : Print "Bloques Procesados :"+str$(REC.NO)
Ue.Data2$ = "9999999999999999999999999999999999999999999"
Call Write.Data
Ue.Data2$ = "TOTAL #REG GRABADOS "+Right$("000000000"+str$(R),9)+"00000000000000"
Call Write.Data
Delete 11 : Close 39 
Close tlog
Locate 18,1 : Print "Proceso Terminado..." 
D1:  PRINT STR$(R); "  RECORDS SELECTED  "

End Sub 


Function TERMINE.PROG
String Buffer$, lec$
Integer*4 II
 Call BARRA.ESTADO
 MEN$ = BARRA$+" Reg.Procesados => "+Str$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
   Else Begin
    Locate 12,15: Print MEN$
 Endif
 Close 11
 Call Genere.Interface
 Call SETF("0000000")                             !
 Call CLRSCR                                      !
 RET.ERR%= CLSDIS                                 !
 Call DM.ERR(RET.ERR%,CLSDIS$)                    !
 FIN.MOV$ = time$
 Stop
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

Function INI.VAR.PROG           ! Inicializa Variables del Programa en GRAL
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
 TLOG = 25                 :CONTROLFILE = 29   :INTERFAZ = 28
 LISTRAN  = 34         :OUTFIL = 36              :LOGINTERFAZ = 20
 SERDIAN = 27          :NUM.ALMACEN = 38
 Dim Registro$(15000)
 Dim TotGral(15)
 CR% = 0
 CT.CENTSEP$ = "."
 CT.DOLLARSEP$ = ""
End Function

Function INICIALICE.VAR.TRANSACCION             ! Para cada Transacci¢n Nueva
Q                 = 1
NRO.STRINGS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero
Dim VTA.ITEM$(800,5)
Dim VTA.DSCT2$(800,5)
Dim VTA.PAGO$(80,10)
Dim VTA.DSCT$(10,2)
Dim VTA.CNTAS$(800,3)
Vlr.Venta%        = 0
CITM%             = 1
CITMD% = 1 
CPAG%             = 1
CDSC%             = 1
N.Cliente$        = "000000000000000000"
NF.CLIENTE$       = "00000000000000"
TOT.CON.IVA% = 0 : TOT.SIN.IVA% = 0 : TOT.DEL.IVA% = 0
 FACTURA.ORI$ = "00"
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
    STOP
 Endif
End Function

Function LEA.REG.TRANS.SUM.LOG      ! DEVUELVE EL ARCHIVO PREVIO
String LLAVE$
 Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL      ! Busca Eamtran Activo
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
        B$ = OSLOGNAME$                        ! CARGA EAMTRAN PREVIO
        FECHA.CIER$ = UNPACK$(FECHA.CIER$)
        FECHA.CIER$ = LEFT$(FECHA.CIER$,6)     ! Tomo primeros 6 caracteres aammdd
  Endif
Endif
TOT.TAMANO = SIZE(B$)
End Function

Function PANTALLA.PRINCIPAL
Call INICIADM                                               ! Inicializacion Variables Display Manager
CAMPO% = 0
AREA1% = 11: AREA2% = 4 : AREA3% = 21                                       ! Definicion area de trabajo archivo
ARCH2$="EAMITEMR"
Open ARCH2$ KEYED RECL 77 AS AREA2%       ! Abre maestra de articulos
Call LEER.CABECERA
DM.CODIGO$ = "2"
DATO.SO$ = COMMAND$                                             ! Dato S.O

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "****  INTERFACE MOVIMIENTO VENTA DEPTOS    COMFANDI ****"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software 25/10/2010 by Gretail  ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 25/10/2010 Gretail" + Chr$(169)
  Call Entrada.Log
EndIf

If LEN(B$) = 0 Then Begin                                       ! Dia previo
  Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO
Endif
If LEN(B$) = 1 Then Begin
  B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT"                         ! Tomo log capturado
  FECHA.CIER$ = DATE$                                          ! Asigno fecha dia
Endif
FEnd

Function CUENTE.STRINGS         ! Esta funcion cuenta primero el #String y lo
Integer*2 POS1%,CUENTE.STRINGS  ! compara con el #reportado por SMA
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
 If (NRO.STRINGS.FOUND = NRO.REG) Then CUENTE.STRINGS = 1       \ Retorna 1 si es
 Else CUENTE.STRINGS = 0                                ! satisfactorio el resultado y 0
End Function                                            ! si el resultado es fallido.


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
     WORK$  = Mid$(HEX$,WORK+1,1)+WORK$    ! ADD HEX VALUE TO OUTPUT String
     ERRNUM = SHIfT(ERRNUM,4)              ! SET UP NEXT NYBBLE
   NEXT I
   ERRNStr$=WORK$                          ! Return Error Code
End Function

Function CALCULO.HORA
String A$, H$, M$, S$
A$ = TIME$                                                          !
H$ = LEFT$(A$,2)                                                ! Tomo hora
M$ = Mid$(A$,3,2)                                               ! Tomo minutos
S$ = RIGHT$(A$,2)                                               ! Tomo segundos
M$ = Str$(VAL(M$) + 10)                 ! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)                  !
If VAL(M$) > 59 Then Begin      !
  M$ = Str$(VAL(M$) - 60)      !
  M$ = RIGHT$("00"+M$,2)               !
  H$ = Str$(VAL(H$) + 1)               !
  If VAL(H$) > 23 Then H$ = "00"                               !
Endif                                                           !
HORA.FINAL$ =  H$+M$+"00"                                       ! Hora de Inicio
End Function

Sub ALMACENA.PRODUCTO(XCOD$, XPR$, XQTY$,Xsgn$)
 String XCOD$, XPR$, XQTY$, LEC$, XKEY$, \
        YITM$, YQTY$, YPRC$, YIPO$, Xsgn$
 Integer*4  ICON%, Xsgn%
 Xsgn% = 1
 
 If Xsgn$ = "-" Then Xsgn% = -1

 XKEY$ = PACK$(RIGHT$("0000000000000000"+XCOD$,16))
 
 LEC$="C8 3C5"
 BAN.PRG$ = "0"
 READ FORM LEC$;#11 KEY XKEY$;                            \! Lee Reg Cabecera 
       YITM$, YQTY$, YPRC$, YIPO$
  
 If BAN.PRG$ = "0" Then BEGIN 
    Yqty$ = unpack$(Yqty$)
    Yprc$ = unpack$(Yprc$)
    Yipo$ = unpack$(Yipo$)
    Yqty$ = TRANSLATE$(Yqty$,"=","-")
    Yprc$ = TRANSLATE$(Yprc$,"=","-")
    
    ICON% = 0 
    	
    Yqty$ = Str$( Val(Yqty$) + ( Val(Xqty$) * Xsgn% ))            ! Cantidad vendida
    Yprc$ = Str$( Val(Yprc$)  + ( Val(Xpr$) * Xsgn% ))                    ! Total venta
    Yipo$ = Str$(Icon% * Xsgn%)
    	
 ENDIF Else BEGIN 
    ICON% = 0 
    Yqty$ = str$( Val(Xqty$) * Xsgn%)
    Yprc$ = STR$( Val(Xpr$) * Xsgn%)
    Yipo$ = Str$(Icon% * Xsgn%)
 ENDIF 
 
 Yqty$ = Pack$(Right$("0000000000"+Yqty$,10))
 Yprc$ = Pack$(Right$("0000000000"+Yprc$,10))
 Yipo$ = Pack$(Right$("0000000000"+Yipo$,10))
 
 Write FORM LEC$;#11; XKEY$,                            \! Grabacion registro 
       YQTY$, YPRC$, YIPO$
       
End Sub 

Sub CARGAR.IVA
String XD1$, XD2$, XD3$
Integer*2 Xpos%
Dim Tiva$(10)
Xpos% = 0 
Open "TABLAIVA" AS 91 NOWRITE NODEL
If END # 91 Then CONT.IVA
LEER.IVA:
   READ # 91 ;XD1$
   Xpos% = Xpos% + 1 
   TIVA$(Xpos%) = XD1$
   If Xpos% = 10 Then GoTo CONT.IVA
   GOTO LEER.IVA 
CONT.IVA:
  Close 91

End Sub 
!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

 ON ERROR GoTo IO.ARCHIVOS
 INI.MOV$ = time$
 Call INI.VAR.PROG             ! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
 Call CALCULO.HORA
 Call CARGAR.IVA
 ARC% = 0
 Call PANTALLA.PRINCIPAL
 Call APERTURA.ARCHIVOS.PRINC
 Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$
 CREATE POSFILE "ADX_UDT1:TMPDPTO.DAT" KEYED 8,,,25000 RECL 23 As 11 ! si no existe lo crea.
 Ofile$ = "ADX_UDT1:L"+Right$(DM.ALMACEN$,3)+RIGHT$(FECMOV$,4)+".DAT"
 Locate 7,12: Print "Interface Creada .... "+Ofile$
 Create Ofile$ As 39 BUFFSIZE 1780
 Ue.Data2$ = "1000"                              + \! Cabecera archivo
             Fecmov$                             + \! Fecha de movimiento
             Right$("0000000000"+DM.ALMACEN$,10) + \! Numero de la tienda 
             String$(21," ")                        ! Relleno registro
 
 Call Write.Data
!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

 NXTRCD:
 InTrx% = -1
 Call INICIALICE.VAR.TRANSACCION               ! Inicializamos las Var de Trans.
 PROCESO$ = " PROCESO PRINCIPAL "
 READ #TLOG; LINE INAREA$
 If End #TLOG Then REPORTE.FINAL
 TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2          ! PARA CALCULAR BYTES LEIDOS DEL TLOG
 If LEN(INAREA$) < 12 Then Begin               ! SI HAY ALGO EN REGISTRO
         GoTo NXTRCD
 Endif
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
   B$ = Mid$(INAREA$,Q+1,(P-Q)-2)      ! CAPTURA CADENA SIN COMILLAS.  MENOS 13 Y 
   B$ = B$+":"                         ! SUMA UN SEMI-COLON
   Q = P + 1                           ! PREPARA POSC. PARA NUEVO DATO
   A = Val(Unpack$(Left$(B$,1)))       ! DETERMINA TIPO DE String
   If A = 0 Then GoSub S0:GoTo AGAIN   ! SI ES CHECKOUT TRANSACCION
   If A = 99 Then GoSub S99:GoTo AGAIN
   If (A < 0) OR (A > 21) Then GoTo AGAIN
   If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) Then \   ! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
                  CANT.PESO$ = "000000000"
      SW.ITEM = 0
   Endif
   If (A > 3) AND (SW.ITEM = 1) Then \ !SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
          If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) Then Begin
               SW.ITEM = 0
    Endif

   ON A GoSub S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
              S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21,S99

   AGAIN:
    If (A=0) AND (STRINGS.COMPLETOS=0) Then NRO.ERROR = NRO.ERROR + 1:GoTo NXTRCD
    If NO.VENTA Then GoTo NXTRCD               ! Si no es una venta leemos otro registro
 WEnd
GoTo NXTRCD                     ! VUELVA A LEER NUEVO REGISTRO

S0:
 ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
 J = 3
 GoSub GETUNPK                                                                                                                                                 !
 TERMINAL$=RIGHT$(A$,4)                                                                                        ! CAPTURA NUMERO DE TERMINAL
 GoSub GETUNPK                                                                                                                                                 !
 NRO.TRANS$=A$                                                                                                                         ! CAPTURA NUMERO DE TRANSACCION
 GoSub GETUNPK                                                                                                                                                 !
 FECHA$=LEFT$(A$,6)                                                                                                                            ! CAPTURA FECHA DE TRANSACCION
 FECHA.ARCH$=FECHA$
 HORA$ = RIGHT$(A$,4)+"00"                                                                             ! CAPTURA HORA DE LA TRANSACCION
 GoSub GETUNPK
 TIPO.TRANS=VAL(A$)                                                                                                    ! CAPTURA EL TIPO DE LA TRANSACCION
 GoSub GETUNPK
 NRO.REG=VAL(A$)                                                                                                               ! NUMERO DE STRINGS EN TODA LA TRANSACCION
 STRINGS.COMPLETOS = 0
 If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND         \
       (TIPO.TRANS <> 1) Then Begin                                                                                    ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
               NO.VENTA = 1
 Endif Else    \
       STRINGS.COMPLETOS = CUENTE.STRINGS                                                              ! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS
 GoSub GETUNPK
 COD.OPERA$=A$                                                                                                                                                 ! NUMERO DEL OPERADOR
 GoSub GETUNPK                                                                                                                                   ! PASSWORD DEL OPERADOR
 GoSub GETUNPK                                                                                                                                                 ! Venta positiva
 GROSS.POSITIVO$=A$
 GoSub GETUNPK                                                                                                                                                 ! Venta negativa
 GROSS.NEGATIVO$=A$
 If STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
       TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin                   ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
    Call GRABE.IDENTIfICADOR
    If DUPLICADA Then GoTo NXTRCD                                                                      !
 Endif
Return

S1:
 J = 3
 GoSub GETUNPK
 COD.ITEM$ = RIGHT$(String$(12,"0")+A$,12)                                     ! CODIGO PLU O EAN
 GoSub GETUNPK
 PRECIO$ = RIGHT$(String$(10,"0")+A$,10)                                       ! PRECIO DEL ARTICULO
 GoSub GETUNPK
 GoSub GETUNPK                                                                 ! Cupon Family 3 y 3 Long 6
 GoSub GETUNPK                                                                 ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
 IVA% = 0
 PESO = 0

 INDICAT1 = VAL(A$)
 If INDICAT1 AND 4000H Then PESO = 1 Else PESO = 0

 GoSub GETUNPK                                        ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
 INDICAT2 = VAL(A$)
 GoSub GETUNPK                                        ! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
 INDICAT3 = VAL(A$)
 ALTO$ = LEFT$(A$,1)
 BAJO$ = RIGHT$(A$,1)
 ESCANEADO = 0

 If ALTO$ = "8" OR ALTO$ = "2" OR ALTO$ = "5" \
       Then SIGNO$ = "-" Else SIGNO$ = ""

 If (INDICAT2 AND 0080H) AND (ALTO$ = "7") Then SIGNO$ = ""
 If (INDICAT2 AND 0040H) AND (ALTO$ = "7") Then SIGNO$ = ""
 If (INDICAT2 AND 0080H) AND (ALTO$ = "2") Then SIGNO$ = ""
 If (INDICAT2 AND 0080H) AND (ALTO$ = "4") Then SIGNO$ = "-"
 If (INDICAT2 AND 0040H) AND (ALTO$ = "4") Then SIGNO$ = "-"
 If (INDICAT2 AND 0080H) AND (ALTO$ = "1") Then SIGNO$ = "-"   ! PRUEBA DEPOSIT
 If (INDICAT2 AND 0040H) AND (ALTO$ = "1") Then SIGNO$ = "-"   ! PRUEBA DEPOSIT
 If INDICAT2 = 2054 Then SIGNO$ = "-"
 If INDICAT2 = 6    Then SIGNO$ = "-"
 
If INDICAT3 = 73 Then Begin         ! Cupon de tienda
 If INDICAT2 = 128 Then Begin       ! Anulacion producto
   SIGNO$ = "-"
 EndIf Else SIGNO$ = ""            ! Venta Producto
EndIf 

  If Peso = 0 Then \
  	 cantidad$ = "1000" Else cantidad$ = "0"
  If Left$(COD.ITEM$,6) <> "440000" Then begin 
   IR.ITEMCODE$ = PACK$(Right$("000000000000"+COD.ITEM$,12))
   BAN.PRG$ = "0" 
   %INCLUDE EAMIRRD1.J86
   If BAN.PRG$ = "1" Then ASC.IR.NDEPTO$ = "0"
   ASC.IR.NDEPTO$ = LEFT$(IR.USERDATA$,16)							! DEPARTAMENTO COMERCIAL
   If Val(ASC.IR.NDEPTO$) <= 0 Then ASC.IR.NDEPTO$ = "0000000000000001"
	 Call ALMACENA.PRODUCTO(ASC.IR.NDEPTO$,PRECIO$,cantidad$,Signo$)
	EndIf 
Return

S2:
 J = 3                         ! EXTENSION DE ENTRADA
 GoSub GETUNPK                 !  NO USADO   MPGROUP= ",VAL(A$)
 GoSub GETUNPK                 !  NO USADO   DEALQUAN= ",VAL(A$)
 GoSub GETUNPK                 !  NO USADO   PRICE METH= ",VAL(A$)
 GoSub GETUNPK                 !  NO USADO   SALEQUAN= ",VAL(A$)
 GoSub GETUNPK                 !  NO USADO   SALEPRIC= ",VAL(A$)"
 SALEPRIC$ = A$
 GoSub GETUNPK                 !  CANTIDAD O PESO VEndIDO = QTYORWGT
 If peso = 0 Then begin 
   A$ = Str$( (Val(A$)*1000) - 1000 )
 endif Else begin 
 	 A$ = Str$( Val(A$))
 endif 
 
  If Left$(COD.ITEM$,6) <> "440000" Then begin 
	 Call ALMACENA.PRODUCTO(ASC.IR.NDEPTO$,"0",A$,Signo$)
	EndIf 
 
Return

S3:
 J = 3
Return

S5:
 J = 3
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
 J = 3!DTENTRY
Return

GETEXCPT:
 K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
 If ((K-J)-1) > 0 Then \ ! If DATA IN FIELD
   A$ = UNPACK$(Mid$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
 Else \
   A$ = "" ! SET A$ = NULLS
 J=K+2 ! POINT TO BeginNING OF NEXT FIELD
Return


!*********************************************************************
GETUNPK:
 K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
 A$ = UNPACK$(Mid$(B$,J,K-J)) ! UNPACK FIELD
 J=K+1 ! POINT TO BeginNING OF NEXT FIELD
Return
!*********************************************************************

!*********************************************************************
GETUNPK3:
 K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
 A$ = UNPACK$(Mid$(B$,J,K-J)) ! UNPACK FIELD
 J=K+1 ! POINT TO BeginNING OF NEXT FIELD
Return
!*********************************************************************
GETUNPK4:
 K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
 A$ = (Mid$(B$,J,K-J)) ! FIELD
 J=K+1 ! POINT TO BeginNING OF NEXT FIELD
Return

GETUNPK5:
 K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
 A$ = (Mid$(B$,J,K-J)) ! FIELD
 J=K+1 ! POINT TO BeginNING OF NEXT FIELD
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

If InTrx% = -1 Then begin  
  If ERR = "SS" Then RESUME NXTRCD:
  If ERR = "SL" THEN RESUME NXTRCD:
  If ERR = "IH" THEN RESUME NXTRCD:
endif 

If ERR = "IH" THEN Resume
	
If ERRF% = AREA2% AND ERR = "EF" Then Begin          \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                 !

If ERRF% = 91 AND ERR = "EF" Then Begin          \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                 !

If ERRF% = 11 AND ERR = "EF" Then Begin          \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                 !


If ERRF% = AREA2%  AND                               \!
  (ERR = "OE" OR ERR = "FU")  Then Begin             ! Si encuentra Error en articulos
  MEN$="Error: Apertura EAMITEMR ..."                !
  Call ENTRADA.LOG                                   !
  Call TERMINE.PROG                                  !
Endif                                                 !
                                        !
If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
   MEN$="Error: No Se Logro Abrir Log de Transacciones "
   Call ADXSERVE(PP,26,1,MEN$)
   Print Men$
   Wait;3000
   Stop 
 Endif

If ERRF% = 11 AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
   MEN$="Error: No Se Logro Crear temporal movimiento inventario"
   Call ADXSERVE(PP,26,1,MEN$)
   Print Men$
   Wait;3000
   Stop 
 Endif


Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+Str$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
