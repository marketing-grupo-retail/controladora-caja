!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRINVTAS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Abril     3 de 2.013             *
!Observaciones : Interface Ventas Supermercado    *
!                Don Lucho - Guaviare - Sistema 1 *
!**************************************************

%ENVIRON C                                                                                                                                              ! Ambiente de controlador

%Include POSPVARI.BAS                              ! Variables del programa
%Include EAMITEMR.J86                              ! Variable EAMITEMR
%Include EXIRTSVA.J86                              ! Variable EAMITEMR

Integer*4 Global PP, TAM%, Len.REG%,  UE.TABLAIVA(1), CITM%, CPAG%, CDSC%, \
                 NRO.REG%, CR%, CITMD%, CCANASTA%
Integer*4 Global Vlr.Iva%, U.Tmp1%, U.Tmp2%, Vlr.Venta%, Vlr.Base%, \
                 VTA.PAGO%(1), Tdscto%

String    Global FileCop$, FILEOUT$, FECHA.ARCH$, Asc.Mod.DateTime$, TIPO.DOC$
String    Global Factura$, VTA.ITEM$(2), VTA.DSCT$(2), Pagos$(1)
String    Global UE.TMP1$, UE.TMP2$, UE.COMPRA$, U.Prefijo$, Utax$, UE.TMP3$
String    Global Treg1$, Treg2$, UE.COMPRA1$
String    Global bufferf$, lecf$, FILEOUT.NF$, FILEOUT.MAR$, FILEOUT.MSM$
String    Global CT.TEMP3$, CT.TEMP1$, CT.DOLLARSEP$, CT.CENTSEP$, PESO$, Ue.Data2$
String    Global UE.AUTORIZA$, DATO.SO$, SERIAL$, ARC$, LOG$, Vlr.Venta$, N.Cliente$, NF.CLIENTE$

!--- Definicion de rutinas de la aplicacion
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

Function FORMAT.DOLLARS ( FDAMT )                 !
 INTEGER*1 FORMAT.DOLLARS                        !
 INTEGER*4 FDAMT                                 !
 INTEGER*2 THOU, MILL, L%                        !

 IF FDAMT < 0 THEN BEGIN                         !
   CT.TEMP3$ =  Str$(-FDAMT) + "-"
 ENDIF ELSE BEGIN                                !
   CT.TEMP3$ = Str$(FDAMT)  + " "                !
 ENDIF                                           !

 IF CT.CENTSEP$ NE " "   THEN BEGIN              ! IS CENT SEP
   L% = LEN(CT.TEMP3$)                           !
   IF (L% < 3) THEN BEGIN                        ! ZERO FILL
     CT.TEMP3$ = CT.CENTSEP$ + RIGHT$( "00" + CT.TEMP3$, 3)
   ENDIF ELSE BEGIN                              !
       CT.TEMP3$ = LEFT$(CT.TEMP3$, L%-3) +      \ INS CENT SEP
       CT.CENTSEP$ + RIGHT$(CT.TEMP3$, 3)        !
   ENDIF                                         !

   THOU = 7                                      ! FOR THOUSANDS
   MILL = 11                                     ! FOR MILLIONS
 ENDIF ELSE BEGIN                                ! NO CENT SEP
   THOU = 4                                      ! FOR THOUSANDS
   MILL = 8                                      ! FOR MILLIONS
 ENDIF                                           !

 L% = LEN(CT.TEMP3$)
 IF (L% > THOU) THEN BEGIN                       ! THOUSANDS
   CT.TEMP3$ = LEFT$(CT.TEMP3$, L%-THOU) +       \ INS $ SEP
   CT.DOLLARSEP$ + RIGHT$(CT.TEMP3$, THOU)       !
 ENDIF                                           !

 L% = LEN(CT.TEMP3$)
 IF (L% > MILL) THEN BEGIN                       ! MILLIONS
   CT.TEMP3$ = LEFT$(CT.TEMP3$, L%-MILL) +       \ INS $ SEP
   CT.DOLLARSEP$ + RIGHT$(CT.TEMP3$, MILL)       !
 ENDIF                                           !

End Function                                    !


Function FORMAT.AMOUNT ( AMT1 ) Public          !
 Integer*1 FORMAT.AMOUNT                         !
 REAL AMT1                                  !

 Call FORMAT.DOLLARS( AMT1 )                     ! FORMAT AMT
 CT.TEMP1$ = CT.TEMP3$                           !
 If Right$(CT.TEMP1$,1) = "-" Then Begin
    CT.TEMP1$ = "-" + Left$(CT.TEMP1$,Len(CT.TEMP1$) -1)
 EndIf
End Function                                    !


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

Sub ENVIO.FTP
String Sndfile$
  Locate 20,1 : Print "Encolando Envio Archivo "+FileCop$
  Open "ADX_IDT1:GRTRXSND.DAT" AS 39 Append 
  SndFile$ = Right$(FILECOP$,12)
  Write form "C12 C2"; #39 ; SndFile$, Finr$           							! Graba registro
  Close 39
  
End Sub 


Function TERMINE.PROG
String Buffer$, lec$
Integer*4 II, PP
 Call BARRA.ESTADO
 MEN$ = BARRA$+" Reg.Procesados => "+Str$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
   Else Begin
    Locate 12,15: Print MEN$
 Endif
 Close 51
 WAIT ; 100
 Call ADXCOPYF(PP,FILEOUT$,FileCop$,0,0)
 LOCATE 22,1 : Print "Interface Generada "+FILECOP$
 Call OSSHELL("DEL "+FILEOUT$)

 !Call ENVIO.FTP 

 Stop
End Function
!--- Fin de la ejecucion del programa

FUNCTION LEER.CABECERA
 String PARM2$
 INTEGER*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)

End FUNCTION
!--- Fin de la funcion de lectura

Function Busca.Fiscal(u.key$)
String U.KEY$,U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, LEC$, U.K$, U.TIPO$, U.RESOL$
Integer*4 U.INI%, U.FIN%, U.ACT%, U.NOTA%, U.NC%

 BAN.PRG$ = "0"
 U.KEY$ = Pack$(Right$("0000"+U.Key$,4))

  LEC$="C2 C15 C4 2I4 C6 C3 I4"	                           !
  READ FORM LEC$;#AREA3% KEY U.KEY$;                      \! Lee Reg Cabecera 
  U.K$,U.SERIAL$, U.PREF$, U.INI%, U.FIN%, U.AUT$, U.FEC$, U.ACT%

 If BAN.PRG$ = "0" Then Begin
    U.PREFIJO$ = U.PREF$
 Endif Else U.PREFIJO$ = "NDEF"
End Function

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
 FINR$ = CHR$(13)+CHR$(10)
 TLOG = 25                 :CONTROLFILE = 29   :INTERFAZ = 28
 LISTRAN  = 34         :OUTFIL = 36              :LOGINTERFAZ = 20
 SERDIAN = 27          :NUM.ALMACEN = 38
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
Dim VTA.ITEM$(800,10)
Dim VTA.PAGO%(70)
Dim VTA.DSCT$(100,2)
Vlr.Venta%        = 0
CITM%             = 0
CITMD% = 1 
CCANASTA% = 1
CDSC%             = 1

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
ARCH2$="ADX_IDT1:EAMITEMR.DAT"
Open ARCH2$ KEYED RECL 46 AS AREA2%       ! Abre maestra de articulos
Open "ADX_IDT1:EAMNMFIS.DAT" KEYED RECL 42 AS AREA3%   ! Apertura datos numeracion fiscal
DM.CODIGO$ = "2"
DATO.SO$ = COMMAND$                                             ! Dato S.O

If DATO.SO$ = "VERSION" THEN BEGIN 
   PRINT "INTERFACE MOVIMIENTO DIARIO IBM-SISTEMA 1 31-JUL-2013 GR"
   STOP
ENDIF

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin                            ! Si entro dato
  DATO.SO$ = "0"
  CLEARS
  Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191) ! TODO LO DE ARRIBA
  Locate 3, 4: Print CHR$(179)
  Locate 4, 4: Print CHR$(179)
  Locate 3,12: Print "****  INTERFACE MOVIMIENTO DIARIO SUP. DON LUCHO    ****"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software Julio     31  2013 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 31/Jul/2013 G.R...."
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

Sub CARGA.IVA                                                ! Carga las tarIfas de IVA
Integer*4 UE.PORCENTAJE, UE.LINEA
  UE.LINEA = 0
  Dim  UE.TABLAIVA(10)
  Open "ADX_IDT1:TABLAIVA.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarIfas de IVA
  If End # 91 Then  CONT.IVA                                ! Recorre el archivo
  LEER.IVA:
     READ # 91 ;  UE.PORCENTAJE
     UE.LINEA = UE.LINEA + 1
     UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
  GoTo LEER.IVA
  CONT.IVA:
    CLOSE 91
End Sub


Sub Carga.Fecha.Cierre
  Open "ADX_IDT1:GRFECDIA.DAT" AS 91       ! Archivo Fecha Cierre
  If End # 91 Then  CONT.FEC               ! Recorre el archivo
  READ # 91 ; FECHA.ARCH$
  CONT.FEC:
   Close 91
End Sub 

Sub Carga.Equivale.Pagos
String Xpago$, Xdata$
Dim Pagos$(70)
  Open "ADX_IDT1:PAGOSDEF.DAT" AS 91 NOWRITE NODEL                       ! Abre las tarIfas de IVA
  If End # 91 Then  CONT.PAGO                               ! Recorre el archivo
  LEER.PAGO:
     READ # 91 ;  Xpago$, Xdata$
     Pagos$(Val(Xpago$)) = Xdata$
  GoTo LEER.PAGO
  CONT.PAGO:
    CLOSE 91
End Sub 

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

 On Error GoTo IO.ARCHIVOS

 !Call OSSHELL("ADX_UPGM:GRCALFEC.286")
 Call INI.VAR.PROG             ! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
 Call Carga.Iva
 Call Carga.Equivale.Pagos
 Call Carga.Fecha.Cierre
 Call LEER.CABECERA
 FILEOUT$ = "Q:\VT"+FECHA.ARCH$+".PO0"
 FileCop$ = "ADX_UDT1:VT"+FECHA.ARCH$+".PO0"
 CREATE FILEOUT$ As 51 BUFFSIZE 7680
 Call PANTALLA.PRINCIPAL
 Call APERTURA.ARCHIVOS.PRINC
 Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

 NXTRCD:
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

   ON A GoSub S1,S2,S3,S4,S5,S6,S7,S7,S9,S10, \
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
 TERMINAL$=RIGHT$(A$,4)                                                                ! CAPTURA NUMERO DE TERMINAL
 GoSub GETUNPK                                                                                                                                                 !
 NRO.TRANS$=A$                                                                         ! CAPTURA NUMERO DE TRANSACCION
 GoSub GETUNPK                                                                                                                                                 !
 FECHA$=LEFT$(A$,6)                                                                    ! CAPTURA FECHA DE TRANSACCION
 HORA$ = RIGHT$(A$,4)+"00"                      ! CAPTURA HORA DE LA TRANSACCION
 GoSub GETUNPK
 TIPO.TRANS=VAL(A$)                             ! CAPTURA EL TIPO DE LA TRANSACCION
 
 If TIPO.TRANS <> 0 Then GoTo NXTRCD            ! Si no es una venta

 GoSub GETUNPK
 NRO.REG=VAL(A$)                                ! NUMERO DE STRINGS EN TODA LA TRANSACCION
 STRINGS.COMPLETOS = 0
 If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND         \
       (TIPO.TRANS <> 1) Then Begin             ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
               NO.VENTA = 1
 Endif Else    \
       STRINGS.COMPLETOS = CUENTE.STRINGS       ! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS
 GoSub GETUNPK
 COD.OPERA$=A$                                  ! NUMERO DEL OPERADOR
 GoSub GETUNPK                                  ! PASSWORD DEL OPERADOR
 GoSub GETUNPK                                  ! Venta positiva
 GROSS.POSITIVO$=A$
 GoSub GETUNPK                                  ! Venta negativa
 GROSS.NEGATIVO$=A$
 If STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
       TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin                   ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
    Call GRABE.IDENTIfICADOR
    If DUPLICADA Then GoTo NXTRCD                                                                      !
 Endif
Return

S1:
 J = 3
 CITM% = CITM% + 1 
 GoSub GETUNPK       ! ItemCode
 COD.ITEM$ = A$                                                                ! CODIGO PLU O EAN
 GoSub GETUNPK       ! Precio Venta total Item
 PRECIO$ = A$                                                                  ! PRECIO DEL ARTICULO
 GoSub GETUNPK       ! Nro departamento
 GoSub GETUNPK       ! Family coupon
 GoSub GETUNPK       ! Indicat1
 INDICAT1 = Val(A$)																														 ! Tabla Indicat1
 GoSub GETUNPK       ! Indicat2
 INDICAT2 = Val(A$)																														 ! Tabla Indicat1
 GoSub GETUNPK       ! Indicat3
 INDICAT3 = Val(A$)																														 ! Tabla Indicat1
 Cantidad$ = "1"
 SIGNO$ = "+"
 If INDICAT1 AND 4000H Then PESO$ = "KLS" Else PESO$ = "UND"                   ! Si el Item es pesable
 IVA% = UE.TABLAIVA(8)
 If INDICAT1 AND 80H Then IVA% = UE.TABLAIVA(1)																 ! Primera tarifa de IVA
 If INDICAT1 AND 40H Then IVA% = UE.TABLAIVA(2)																 ! Segunda tarifa de IVa
 If INDICAT1 AND 20H Then IVA% = UE.TABLAIVA(3)																 ! Tercera tarifa de IVA
 If INDICAT1 AND 10H Then IVA% = UE.TABLAIVA(4)																 ! Cuarta  tarifa de IVA
 If INDICAT2 AND 40H Then	SIGNO$ = "-"																				 ! Es una anulacion
 If INDICAT2 AND 80H Then	SIGNO$ = "-"                                         ! Es una devolucion
 If INDICAT3 = 8     Then	SIGNO$ = "-"                                         ! Es una devolucion

 Vlr.Base% = Round(Float(Val(precio$))/(1.+Float(IVA%)/100.),0,0)
 Vlr.Iva%  = Val(precio$) - Round(Float(Val(precio$))/(1.+Float(IVA%)/100.),0,0)
 VTA.ITEM$(CITM%,0) = COD.ITEM$         ! Codigo del Item
 VTA.ITEM$(CITM%,1) = Str$(Vlr.Base%)   ! Precio Bruto Sin Iva
 VTA.ITEM$(CITM%,2) = Str$(Vlr.Iva%)    ! Valor del IVA
 VTA.ITEM$(CITM%,3) = Str$(Iva%)        ! Ptg del IVA
 VTA.ITEM$(CITM%,4) = Signo$            ! Signo de la operacion
 VTA.ITEM$(CITM%,7) = Peso$             ! Pesable o Unitario
 If INDICAT2 And 02H Then Begin         ! Item con Cantidad String 02
 EndIf Else Begin                       ! Sin cantidad, por default cantidad 1
 	VTA.ITEM$(CITM%,5) = "1"              ! Cantidad de venta
 	VTA.ITEM$(CITM%,6) = Str$(Vlr.Base%)  ! Precio Unitario Sin IVA
 EndIf

Return

S2:
  J = 3                        ! EXTENSION DE ENTRADA Item
 GoSub GETUNPK                 !  MPGROUP
 GoSub GETUNPK                 !  DEALQUAN
 GoSub GETUNPK                 !  PRICE METH
 GoSub GETUNPK                 !  SALEQUAN
 GoSub GETUNPK                 !  SALEPRIC
 Precio$ = A$
 GoSub GETUNPK                 !  QTYORWGT
 Cantidad$ = A$
 Vlr.Base% = Round(Float(Val(precio$))/(1.+Float(IVA%)/100.),0,0)
 VTA.ITEM$(CITM%,5) = Cantidad$        ! Cantidad de venta
 VTA.ITEM$(CITM%,6) = Str$(Vlr.Base%)  ! Precio Unitario Sin IVA
Return

S3:
J = 3   ! Dsctos 
GoSub GETUNPK                                  ! GRUPO DESCUENTO
UE.TMP1$ = A$
GoSub GETUNPK                                  ! PTG DESCUENTO
UE.TMP2$ = A$
GoSub GETUNPK                                  ! VALOR DEL DSCTO
VTA.DSCT$(Val(UE.TMP1$),0) = UE.TMP2$          ! Ptg Dscto
VTA.DSCT$(Val(UE.TMP1$),1) = Str$( Val(VTA.DSCT$(Val(UE.TMP1$),1)) + Val(A$)  )
Return

S4:
J = 3   ! Dsctos 
GoSub GETUNPK                                  ! GRUPO DESCUENTO
UE.TMP1$ = A$
GoSub GETUNPK                                  ! PTG DESCUENTO
UE.TMP2$ = A$
GoSub GETUNPK                                  ! VALOR DEL DSCTO
VTA.DSCT$(Val(UE.TMP1$),0) = UE.TMP2$          ! Ptg Dscto
VTA.DSCT$(Val(UE.TMP1$),1) = Str$( Val(VTA.DSCT$(Val(UE.TMP1$),1)) - Val(A$)  )

Return

S5:
 J = 3 ! PAGOS
 GoSub GETUNPK       ! Tipo Variedad Pago
 CPAG% = Val(A$)
 GoSub GETUNPK       ! Valor Pago
 VTA.PAGO%(CPAG%) = VTA.PAGO%(CPAG%) + Val(A$)
Return

S6:
 J = 3 ! PAGOS
 GoSub GETUNPK       ! Tipo Variedad Pago
 CPAG% = Val(A$)
 GoSub GETUNPK       ! Valor Pago
 VTA.PAGO%(CPAG%) = VTA.PAGO%(CPAG%) - Val(A$)
Return

S7:
 J = 3!TAX
Return

S9:
 J = 3 ! CAMBIO
 GoSub GETUNPK       ! Tipo Variedad Pago
 CPAG% = Val(A$)
 GoSub GETUNPK       ! Valor Pago
 VTA.PAGO%(CPAG%) = VTA.PAGO%(CPAG%) - Val(A$)

Return

S10:
 J = 3
Return

S11:
    J = 3 
    GOSUB GETUNPK																	! LLAVE DEL ARCHIVO
    If A$ = "24062004" Then Begin 								! Numeracion Fiscal
       GoSub GETUNPK3												  	! EXTRAIGO DATOS DE LA VENTA
       GoSub GETUNPK3													  ! Numero de la factura
       A$ = Str$(Val(A$))
       FACTURA$ = RIGHT$("0000000"+A$,7)
       Call Busca.Fiscal(TERMINAL$)                   ! Busca Prefijo fiscal
       FACTURA$ = FACTURA$
       For I% = 1 To CITM%
       
!      VTA.ITEM$(I%,0)  ! Codigo del Item
!      VTA.ITEM$(I%,1)  ! Precio Bruto Sin Iva
!      VTA.ITEM$(I%,2)  ! Valor del IVA
!      VTA.ITEM$(I%,3)  ! Ptg del IVA
! 	   VTA.ITEM$(I%,5)  ! Cantidad de venta
! 	   VTA.ITEM$(I%,6)  ! Precio Unitario Sin IVA
! 	   VTA.ITEM$(I%,7)  ! Pesable o unitario
!      VTA.ITEM$(I%,4)  ! Signo de la operacion
!      U.PREFIJO$       ! Prefijo Facturacion
!      FACTURA$         ! FACTURA DE VENTA

          If VTA.ITEM$(I%,7) = "UND" Then Begin      
             UE.TMP1$ = Right$("000000000"+VTA.ITEM$(I%,5),9) + "000" + VTA.ITEM$(I%,4)
          EndIf Else Begin 
          	 UE.TMP1$ = Right$("000000000000"+VTA.ITEM$(I%,5),12) + VTA.ITEM$(I%,4)
          EndIf
          UE.TMP2$ = Right$("000000000"+VTA.ITEM$(I%,6),9) + "00" +  VTA.ITEM$(I%,4)
          UE.COMPRA$ = STR$( VAL(VTA.ITEM$(I%,1)) + VAL(VTA.ITEM$(I%,2)) )
          UE.COMPRA$ = Right$("00000000000"+UE.COMPRA$,11) + "00" +  VTA.ITEM$(I%,4)

          UE.COMPRA1$ = STR$( VAL(VTA.ITEM$(I%,1)) )
          UE.COMPRA1$ = Right$("00000000000"+UE.COMPRA1$,11) + "00" +  VTA.ITEM$(I%,4)

          Utax$ = Right$("000000000"+VTA.ITEM$(I%,2),9) + "00" +  VTA.ITEM$(I%,4)
          COD.ITEM$ = LEFT$(VTA.ITEM$(I%,0)+"               ",15)
          Treg1$ = "01"                        + \! Tipo de Registro C2
     	             "20"+FECHA.ARCH$            + \! Fecha del movimiento     C8
                   DM.ALMACEN$                 + \! Codigo del almacen       C3
                   Right$("00"+TERMINAL$,2)    + \! Numero de la terminal C2
                   Right$("000000"+FACTURA$,6) + \! Numero de la factura  C6
                   Left$(U.PREFIJO$,2)         + \! Prefijo de la caja  C2
                   Right$("000000"+FACTURA$,6) + \! Numero de la factura  C6
                   "000000"                    + \! Codigo del Item C6
                   "   "                       + \! Extension del Item C3
                   UE.TMP1$                    + \! Cantidad Vendida C13
                   "   "                       + \! Lista de precio facturada C3
                   "  "                        + \! Lista de descuento C2 
                   "000000"                    + \! Comanda del servicio C6
                   UE.TMP2$                    + \! Precio unitario y signo C12
                   UE.COMPRA1$                 + \! Vlr Total Bruto C14
                   "0000"                      + \! Ptg Dscto x Item C4
                   "00000000000+"              + \! Liquidacion Dscto C12
                   Right$("00"+VTA.ITEM$(I%,3),2) + "00" + \! Ptg de IVA C4
                   Utax$                       + \! Valor IVA con signo C12
                   UE.COMPRA$                  + \! Vlr Neto Compra C14
                   "  "                        + \! Motivo de la venta C2
                   Left$(VTA.ITEM$(I%,7)+"   ",3) + \! Unidad de Medida Venta C3
                   "000010000"                 + \! Factor unidad de medida C9
                   "B"                         + \! Indicador de barras
                   COD.ITEM$                   + \! Codigo del Item C15
                   "               "              ! Referencia del Item

            Ue.Data2$ = Treg1$ 
            Call Write.Data

       Next I%
       I% = 0
       For I% = 1 To 70
         If VTA.PAGO%(I%) <> 0 Then Begin

             IF I% = 11 THEN VTA.PAGO%(I%) = VTA.PAGO%(I%) + VAL(VTA.DSCT$(1,1))
         	   If Vta.Pago%(I%) >= 0 Then Signo$ = "+" Else Signo$ = "-"
         	   If Vta.Pago%(I%) <  0 Then Vta.Pago%(I%) = Vta.Pago%(I%) * -1
         	   UE.TMP1$ = Right$("000000000000000"+Str$(Vta.Pago%(I%)),15) + "00" + Signo$
         	   Treg2$ = "02"               + \! Tipo de registro         C2
         	            "20"+FECHA.ARCH$   + \! Fecha del movimiento     C8
                      DM.ALMACEN$        + \! Codigo del almacen       C3
                      Right$("00"+TERMINAL$,2) + \! Numero de la terminal C2
                      Right$("      "+FACTURA$,6) + \! Numero de la factura  C6
                      Right$("0"+Pagos$(I%),1)    + \! Codigo de pago  C1
                      "001"              + \! Codigo del medio de pago C3 - definir otro campo en pagos 3 dig
                      "I"                + \! Indicador de ingreso     C1
                      "    "             + \! Codigo Banco del cheque C4
                      "      "           + \! Numero del Cheque C6
                      "                    " + \! Numero cuenta del cheque C20
                      "00000000"         + \! Fecha consignacion del cheque C8
                      "          "       + \! Ref. del medio del recaudo C10
                      "                    " + \! Numero cuenta del recaudo C20
                      "0000000000000+"   + \! Valor propina C14
                      "0000000000000+"   + \! Valor iva datafono C14
                      "   "              + \! Codigo cuenta Bancaria C3
                      "000000"           + \! Nro consignacion C6
                      "  "               + \! Cod Cond. Pago Cartera C2
                      "             "    + \! codigo del cliente C13
                      "  "               + \! Codigo sucursal del cliente C2  
                      UE.TMP1$           + \! Valor del recaudo C18           
                      "00000000000000000+"  + \! Valor recaudo moneda extranjera C18
                      "               "  + \! Nro Aut. medios pago recaudo cheq. C15
                      String$(30," ")       ! Observaciones C30
                      
               Ue.Data2$ = Treg2$ 
               Call Write.Data

         EndIf
       Next I%

       I% = 0
       For I% = 1 To 99 
         If Val(VTA.DSCT$(I%,1)) <> 0 Then BEGIN 
         	  Treg2$ = "03" + \! Invento dscto
         	           Right$("   "+Str$(i%),3) + \
         	           Right$("0000"+terminal$,4) + \
         	           Right$("0000"+nro.trans$,4) + \
         	           Right$("00"+str$(i),2) + \! grupo dscto
         	           Right$("0000"+VTA.DSCT$(I%,0),4) + \! ptg dscto
         	           Right$("0000000000"+VTA.DSCT$(I%,1),10) ! Vlr dscto
         	           
            Ue.Data2$ = Treg2$ 
            !Call Write.Data
         EndIf
       Next I%
    EndIf 
    
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
 J = 3 !DTENTRY
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

!If ERR = "SS" Then RESUME NXTRCD:
!If ERR = "SL" THEN RESUME NXTRCD:
If ERR = "IH" THEN RESUME NXTRCD:

If ERRF% = TLOG  AND (ERR = "EF" OR ERR = "OE") Then Begin          \! Si encuentra fin de
    MEN$ = "No se han registrado ventas para este periodo ..."
    Call ENTRADA.LOG
    Wait;3000
    STOP
Endif                                                 !

If ERRF% = AREA2% AND ERR = "EF" Then Begin          \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                !

If ERRF% = AREA3% AND ERR = "EF" Then Begin          \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                !

If ERRF% = 91 AND ERR = "EF" Then Begin             \! Si encuentra eof()
  BAN.PRG$ = "1"                                     ! ejecucion normal del
  RESUME                                             !
Endif                                                !

If ERRF% = 39 AND (ERR = "OE" OR ERR = "FU") Then  Begin 										! Error apertura 
   CREATE "ADX_IDT1:GRTRXSND.DAT" AS 39 
   Resume
EndIf 

If ERRF% = AREA2%  AND                               \!
  (ERR = "OE" OR ERR = "FU")  Then Begin             ! Si encuentra Error en articulos
  MEN$="Error: Apertura EAMITEMR ..."                !
  Call ENTRADA.LOG                                   !
  Call TERMINE.PROG                                  !
Endif                                                !

If ERRF% = AREA3%  AND                               \!
  (ERR = "OE" OR ERR = "FU")  Then Begin             ! Si encuentra Error en articulos
  MEN$="Error: Apertura EAMNMFIS ..."                !
  Call ENTRADA.LOG                                   !
  Call TERMINE.PROG                                  !
Endif                                                 !

If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
  BAN.PRG$ = "1"                                     ! del archivo de
  RESUME                                             ! help del aplicativo
Endif                                                 !

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
   MEN$="Error: No Se Logro Abrir Log de Transacciones "
   Call ADXSERVE(PP,26,1,MEN$)
   Wait;3000
   Call TERMINE.PROG
 Endif

Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+Str$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Print MEN$
STOP
!*********************************************************************
