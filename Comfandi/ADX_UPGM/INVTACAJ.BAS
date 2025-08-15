!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : INVTACAJ.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Abril de 2.008                   *
!Observaciones : Interface Ventas por cajero      *
!**************************************************
! Se adiciona la cantidad de ITEMS vendidos.
! solicitado por Comfandi 14 abril 2008
! desarrollado por OVS.
!---------------------------------------------------


%ENVIRON C                                                                                                                                              ! Ambiente de controlador

String    Global DATO.SO$, Vlr.Venta$, FECMOV$, Buffer$, Salida$, Vendedor$, Artic$, Xsigno$
Integer*4 Global Vlr.Venta%, PP, ARC%, Cont.Art%
String    Global Xinicaja$, Xinitrx$, Xventa$
Integer*1 Global Xgraba%

%Include ADX_UPGM:POSPVARI.BAS                              ! Variables del programa
%Include ADX_UPGM:DMEXTR.J86                                ! Inclucion Libreria Display Manager
%Include ADX_UPGM:POSPRUTI.BAS                              ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86                               !

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) External                 ! Msg background
  Integer*4 RET
  Integer*2 FUNC,PARM1
  String PARM2
End Sub

Function ENTRADA.LOG
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Locate 13,18: Print MEN$
End Function

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
 Call Entrada.Log
 MEN$ = "Proceso Terminado OK! "
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
 Else Locate 23,1: Print MEN$

 Close 23
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

Sub GRABACION.INTERFACE
String Lec$
   LEC$ = "C"+Str$(Len(Buffer$))
   Write Form Lec$; #23 ;buffer$                                                                                                                       ! Registro tipo 1 Cabecera
End Sub
!--- Fin procedimiento impresion


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
 Cont.Art%      = 0 
 Xgraba%        = 0 
 Xinicaja$      = "XXXX"
 FINR$          = CHR$(13)+CHR$(10)
 TLOG           = 25
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
Vlr.Venta%        = 0
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
    Call Entrada.Log
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
       Call Entrada.Log
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
  Locate 3,12: Print "****  INTERFACE MOVIMIENTO VENTAS POR CAJERO/VENDEDOR **"
  Locate 3,75: Print CHR$(179)
  Locate 4,10: Print CHR$(27)+"b3"
  Locate 4,12: Print "***  Ultima Revision Software Abril     07  2008 G.R. ***"
  Locate 4, 7: Print CHR$(27)+"b7"
  Locate 4,75: Print CHR$(179)
  Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
Endif Else Begin
  DATO.SO$ = "1"
  MEN$ = " Rev.SW 07/Abr/2008 G.R...."
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
 Call Entrada.Log
End Sub

Function ERRNSTR$(ERRNUM)
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

Sub Graba.Registro
    If Xinicaja$ = "XXXX" Then Exit Sub 
    If Xgraba% = 0 Then Exit Sub 
    If Cont.Art% < 0 Then Cont.Art% = Cont.Art% * -1
    Artic$    = Str$(Cont.Art%)
    ARTIC$    = Right$("0000000000"+Artic$,10)   ! Numero de articulos vendido
    
    Buffer$ = "2" +                         \! Tipo de registro  
              COD.OPERA$     +              \! Codigo del cajero
              Vendedor$      +              \! Codigo del vendedor
              XiniCaja$      +              \! Numero de Transaccion
              Xinitrx$       +              \! Numero de terminal
              Xsigno$        +              \! Signo del movimiento de venta
              Xventa$        +              \! Valor de la venta
              Artic$         +              \! Numero de articulos vendidos
              Finr$                          ! Fin de registro             
    Call GRABACION.INTERFACE
    Cont.Art% = 0 
    Xgraba%   = 0 
End Sub 


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

 ON ERROR GoTo IO.ARCHIVOS
 Call INI.VAR.PROG             ! INIZIALIZAMOS VARIABLES PRINCIPALES DEL PROGRAMA
 Call PANTALLA.PRINCIPAL
 Call APERTURA.ARCHIVOS.PRINC
 ON ERROR GoTo IO.ARCHIVOS
 Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$
 Salida$ = "ADX_UDT1:J"+Right$(DM.ALMACEN$,3)+Right$(FECMOV$,4)+".DAT"
 Create Salida$  As 23 
  Locate 9,12: Print "Interface Salida .... "+salida$
 Buffer$ = "1" + DM.ALMACEN$ + FECMOV$ + FINR$              ! REGISTRO CABECERA
 Call GRABACION.INTERFACE
 
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
 If Xgraba% = 0  Then Cont.Art% = 0 
 GoSub GETUNPK                                                                         !
 TERMINAL$=RIGHT$(A$,4)                                                                ! CAPTURA NUMERO DE TERMINAL
 GoSub GETUNPK                                                                         !
 NRO.TRANS$ = Right$("00000000"+A$,8)                                                  ! CAPTURA NUMERO DE TRANSACCION
 GoSub GETUNPK                                                                         !
 FECHA$=LEFT$(A$,6)                                                                    ! CAPTURA FECHA DE TRANSACCION
 HORA$ = RIGHT$(A$,4)+"00"                                                             ! CAPTURA HORA DE LA TRANSACCION
 GoSub GETUNPK
 TIPO.TRANS=VAL(A$)                                                                    ! CAPTURA EL TIPO DE LA TRANSACCION
 GoSub GETUNPK
 NRO.REG=VAL(A$)                                                                       ! NUMERO DE STRINGS EN TODA LA TRANSACCION
 STRINGS.COMPLETOS = 0
 If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND         \
    (TIPO.TRANS <> 1) Then Begin                                                       ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
    NO.VENTA = 1
 Endif Else    \
       STRINGS.COMPLETOS = CUENTE.STRINGS                                              ! FUNCION PARA CORROBORAR INTEGRIDAD DE STRINGS
 GoSub GETUNPK
 COD.OPERA$=Right$("0000000000"+A$,10)                                                 ! NUMERO DEL OPERADOR
 GoSub GETUNPK                                                                         ! PASSWORD DEL OPERADOR
 GoSub GETUNPK                                                                         ! Venta positiva
 GROSS.POSITIVO$=A$
 GoSub GETUNPK                                                                         ! Venta negativa
 GROSS.NEGATIVO$=A$
 Vlr.Venta% = (VAL(GROSS.POSITIVO$)-VAL(GROSS.NEGATIVO$))
 Vlr.Venta$ = Right$("0000000000"+Str$(Vlr.Venta%),10)
 If STRINGS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
       TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin                   ! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
    Call Graba.Registro								       ! 
    Xinicaja$ = TERMINAL$ 
    Xinitrx$  = NRO.TRANS$
    Call GRABE.IDENTIfICADOR
    If DUPLICADA Then GoTo NXTRCD                                                                      !
 Endif

Return

S1:
 J = 3
 GoSub GETUNPK  ! Codigo del articulo
 GoSub GETUNPK  ! Precio 
 GoSub GETUNPK  ! Departamento
 GoSub GETUNPK  ! Cupon Family 3 y 3 Long 6
 GoSub GETUNPK  ! INDICAT1  PARA IMPUESTOS, PESABLES, ALIAS  ETC...
 GoSub GETUNPK  ! INDICAT2   QUE SE OPRIMIO ?  KEY CANCEL, KEY DEVOLUC, KEY REFOUND
 INDICAT2 = VAL(A$)
 GoSub GETUNPK  ! INDICAT3   QUE VEndIO ?  MISCEL, CUPON, QUE CANCELO?, ESCANEO ITEM?
 INDICAT3 = VAL(A$)
 ALTO$ = LEFT$(A$,1)
 BAJO$ = RIGHT$(A$,1)
 
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


If INDICAT3 = 73 Then Begin         ! Cupon de tienda Add 11 Sept 2007
 If INDICAT2 = 128 Then Begin       ! Anulacion producto
   SIGNO$ = "-"
 EndIf Else SIGNO$ = ""            ! Venta Producto
EndIf 

 If SIGNO$ = "-" Then Begin
    Cont.Art% = Cont.Art% - 1
 EndIf Else Cont.Art% = Cont.Art% + 1

Return

S2:
 J = 3  
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
 J = 3
 GoSub GETUNPK 
 If A$ = "20080407" Then Begin 
    GoSub GETUNPK 
    VENDEDOR$ = Right$("0000000000"+Str$(Val(A$)),10) ! Toma codigo del Vendedor
    
    !GoSub GETUNPK 
    !A$ = TRANSLATE$(A$,"=","-")
    !If Val(A$) < 0 Then A$ = Str$(Val(A$) * -1)
    !ARTIC$    = Right$("0000000000"+A$,10)        ! Numero de articulos vendido
    
    If Val(Vlr.Venta$) >= 0 Then Xsigno$ = "+"  \
     Else Begin
      Vlr.Venta$ = Right$("0000000000"+Str$(Vlr.Venta% * -1),10) ! retiro el signo
      Xsigno$ = "-"
     EndIf 
     Xventa$ = Vlr.Venta$
     Xgraba% = -1 
 Endif 
Return

GETUNPK:
 K  = MATCH(":",B$,J)              ! Busca separador de campo
 A$ = UNPACK$(Mid$(B$,J,K-J))      ! Desempaqueta el dato
 J  = K+1                          ! Punto de inicio del proximo campo
Return

REPORTE.FINAL:
       Call TERMINE.PROG
Return

IO.ARCHIVOS:
 ERRORCOD$ = ERR
 P=0
!If ERR = "SS" Then RESUME NXTRCD:
!If ERR = "SL" THEN RESUME NXTRCD:
!If ERR = "IH" THEN RESUME NXTRCD:

If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
   MEN$="Error: No Se Logro Abrir Log de Transacciones "
   Call Entrada.Log
   Wait;3000
   Call TERMINE.PROG
 Endif

Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+Str$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
Else Locate 23,1: Print MEN$

STOP
!*********************************************************************
