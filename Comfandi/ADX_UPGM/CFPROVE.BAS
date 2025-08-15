!------------------------------------------------------- 
!    PROYECTO              : COMFAMILIAR ANDI 
!    Nombre del Fuente     : CFCLIFTE.BAS
!    Fecha de Modificaci¢n : Feb 08 del 2000    
!    Responsables          : Tec Sistemas POS   
!    Archivos que utiliza  : C:\ADX_IDT4\EAMTRANx.DAT
!                                   ( x = A, B, ¢ C )
!-------------------------------------------------------
!    Genera archivos de medios de pago, pagos y compras
!    para Cr‚dito y Cobranzas. Prepara movimiento  para
!    el Sistema Central de Inventarios.
!-------------------------------------------------------

INTEGER*1                  \  !
       P%,                 \  !
       Q%,                 \  ! Longiud de ADXALMA.DAT
       L2,                 \  ! Indicador de cambio de nivel
       L1,                 \  ! Indicador de cambio de nivel
       LR,                 \  ! Indicador de ultimo registro
       OV,                 \  ! Indicador de cambio de pagina
       IR,                 \  ! Indicador de identificacion de registro
       FTS,                \  !
       FT                     ! Indicador de primer ciclo

INTEGER*1                                                \
       A,                B,           C,                 \
       D,                E,           F,                 \
       N,                I,           J,                 \
       K,                L,           TLOG,              \
       OUTFIL1,          OUTFIL2,     LONG,              \
       OUTFIL3,          OUTFIL4,     OUTFIL5,           \
       OUTFIL6,          OUTFIL7,     OUTFIL8,           \
       OUTFIL9,          OUTFI10,     OUTFI11,           \    
       BB.PCR.FLAG,      BB.FLAG,     BB.FLAG2,          \
       FTV,              MES

INTEGER*2                                                \
       P,                Q,           CONSE,             \
       POS%,             II%,         I%,                \
       SI%,              CONLI%

INTEGER*4                NREC9% ,     NREC%,             \
       FLAG,             VALWRK,      VALOR,             \
       CHG,              NRO,         DEPI,              \
       NEF%,             NCH%,        NOT%,              \
       NRM%,             NTC%,        NCR%,              \
       EF%,              CH%,         OT%,               \
       NSU%,             CS%,         AC%,               \
       RM%,              TC%,         CR%,               \
       BB.PRECIO,        BB.AR.PRECIO(1),                \
       MTOTRAN,          NAC%                            
INTEGER*4                  \  !
       S%,                 \  !
       SX%,                \  !
       HX%,                \  !
       SUM%,               \  !
       TRANL1,             \  ! Numero de transacciones
       MONTL1,             \  ! Valor de las transacciones
       TOT,                \  !
       BASES%,             \  !
       AMTFEES,            \  !
       HD.GROSSPOS,        \  !
       HD.GROSSNEG,        \  !
       HD.NUMTRANS,        \  !
       LO.AMTCASH,         \  !
       PK.AMTCASH,         \  !
       CT.AMTCASH,         \  !
       SO.AMTCASH,         \  !
       SO.AMTCHECK,        \  !
       SO.AMTFOODS,        \  !
       SO.AMTMISC1,        \  !
       SO.AMTMISC2,        \  !
       SO.AMTMISC3,        \  !
       SO.AMTMANUF,        \  !
       SO.AMTSTORE,        \  !
       SA.AMTSALE,         \  !
       SA.AMTDISCO,        \  !
       SA.AMTDISC1,        \  !
       SA.AMT1,            \  !
       SA.AMT2,            \  !
       SA.AMT3,            \  !
       TO.DISVOID,         \  !
       TO.DISCOD,          \  !
       SA.AMTTAXEX,        \  !
       SA.AMTCANCE,        \  !
       ST.AMTVOIDS,        \  !
       ST.AMTTAF,          \  !
       SC.NUMITEMS,        \  !
       TT.INGRESO,         \  !
       TT.NUMITEM,         \  !
       TT.CHQ,             \  !
       TT.CLI,             \  !
       TT.TOT,             \  !
       TT.PAGOS,           \  !
       TT.CONTEO,          \  !
       TT.SOBFAL,          \  !
       TT.SOB,             \  !
       TT.FAL,             \  !
       TT.COM,             \  !
       PY.CRO,             \  !
       PY.ALP,             \  !
       PY.CHQ,             \  !
       PY.ASM,             \  !
       PY.TOTAL,           \  !
       PY.RESTART,         \  !
       TT.GROSSPOS,        \  !
       TT.NUMTRANS,        \  !
       TT.LOAN,            \  !
       TT.PICKUP,          \  !
       TT.COUNT,           \  !
       TT.SHORT,           \  !
       TT.SALES,           \  !
       TT.DISCO,           \  !
       TT.CANCEL,          \  !
       TT.ITEMS,           \  !
       TT.CRO,             \  !
       TT.ALP,             \  !
       TT.ASM,             \  !
       TT.PYTOT,           \  !
       TT.INGTOT,          \  !
       MED1(2),            \  !
       TT.ANUL           

  STRING                                                      \ 
            A$,               B$,          H$,  F$,           \
            C$,               D$,          E$,	G$,           \
            FEC1$,            NUAL$,       NOAL$,             \
            ALMA$,            MESES(1),                       \
            BB.AR.KEYTV$(1),  BB.KEYTV$,   CONSEC$,           \
            BB.AR.COMPR(1),   BB.COMPR,    BB.AR.FINAL(1),    \
            BB.FINAL,         BB.AGENC,    BB.AR.AGENC(1),    \
            AR.VALPAG(1),     AR.CUOT(1),  AR.CPR(1),         \
            AR.AUT(1),        NETVEN$(1),  INAREA$,           \
            RESP$,            ALMACEN$,    TITULO$,           \
            VALPAG,           NUMOPE$,     NUMTRAN,           \
            NRWRK$,           NUMTER,      TIPTRA,            \
            TIPPAG$,          FINREC,      COMPRO,            \
            TIPTRA1,          FECTRA,      FECHEQUE,          \
            KEYCF1$,          REGCF1$,     HORA$,             \
            CUOT$,            CPR$,        AUT$,              \
            NITEM$,           SIGNO$,      NUMPAG$,           \
            FIN$,             AGENC$,      VEND$,             \
            NETOPO$,          DEP$,        COD$,              \
            FRA.H$,           FRA.M$,      FRA.S$,            \
            FRA.TIM$,         FRA.FEC$,    FRA.AA$,           \
            FRA.MM$,          FRA.DD$,     FRA.LIN$,          \
            NOCUE$,           AMT$,        CCOSTO$,           \
            AMT1$,            AMT2$,       AMT3$,             \
            AMT9$,            FACTURA$,    ZETA$,             \
            NUMEF$,           NUMCH$,      NUMOT$,            \
            NUMRM$,           NUMTC$,      NUMCR$,            \
            TOTEF$,           TOTCH$,      TOTOT$,            \
            TOTRM$,           TOTTC$,      TOTCR$,            \
            BANCO$,           CHEQUE$,     CTACTE$,           \
            DATO4$,           DATO5$,      DATO6$,            \
            HORATR$,          CHANGE$,     VALCHQ,            \
            TIPPAW$,          TIREG$,      APROB$,            \
            HD.TYPE,          HD.OPERATOR, HD.DATETIME,       \
            NIT$            
!***************************************************************
  FUNCTION ERRNSTR$(ERRNUM)
!
    INTEGER*1 I
    INTEGER*4 ERRNUM,WORK
    STRING HEX$,ERRNSTR$,WORK$
!
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
!
    FOR I=1 TO 8
      WORK=ERRNUM AND 0000000FH         ! AND OFF ALL BUT LOW NYBBLE
      WORK$=MID$(HEX$,WORK+1,1)+WORK$   ! ADD HEX VALUE TO OUT STRING
      ERRNUM=SHIFT(ERRNUM,4)            ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$
!
  FEND

! -----  Rutina para Acumular Totales de Control Medios de Pago ---------


  SUB ACUMULA                                                 !
      IF LEFT$(TIPPAG$,1) = "1" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NEF% = NEF% + 1          :  \
            EF% = EF% + VAL(VALPAG)     \
               ELSE                     \
            EF% = EF% - VAL(VALPAG) 
      ENDIF
      IF LEFT$(TIPPAG$,1) = "2" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NCH% = NCH% + 1          :  \
            CH% = CH% + VAL(VALPAG)     \
               ELSE                     \
            CH% = CH% - VAL(VALPAG) 
      ENDIF
      IF LEFT$(TIPPAG$,1) = "3" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NOT% = NOT% + 1           : \
            OT% = OT% + VAL(VALPAG)     \
               ELSE                     \
            OT% = OT% - VAL(VALPAG) 
      ENDIF
      IF LEFT$(TIPPAG$,1) = "4" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NRM% = NRM% + 1           : \
            RM% = RM% + VAL(VALPAG)     \
               ELSE                     \
            RM% = RM% - VAL(VALPAG) 
      ENDIF
      IF LEFT$(TIPPAG$,1) = "5" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NTC% = NTC% + 1           : \
            TC% = TC% + VAL(VALPAG)     \
               ELSE                     \
            TC% = TC% - VAL(VALPAG) 
      ENDIF
      IF LEFT$(TIPPAG$,1) = "6" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NCR% = NCR% + 1           : \
            CR% = CR% + VAL(VALPAG)     \
               ELSE                     \
            CR% = CR% - VAL(VALPAG) 
      ENDIF
      IF TIPPAG$ = "23" THEN BEGIN                            ! Si es Ch Subsidio
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NSU% = NSU% + 1          :  \                     ! Acumula N.Cheques
            CS% = CS% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            CS% = CS% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
     ENDIF
      IF TIPPAG$ = "34" THEN BEGIN                            ! Si es Ch Subsidio
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NAC% = NAC% + 1          :  \                     ! Acumula N.Cheques
            AC% = AC% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            AC% = AC% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF                                                   ! Fin de Condic

      NUMCS$ = RIGHT$("0000000000" + STR$(NSU%) + "+", 10)
      TOTCS$ = RIGHT$("0000000000" + STR$(CS%) + "+", 10)
      IF CS% < 0 THEN                                \
         TOTCS$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CS%),LEN(STR$(CS%))-1),9) + "-"

      NUMAC$ = RIGHT$("0000000000" + STR$(NAC%) + "+", 10)
      TOTAC$ = RIGHT$("0000000000" + STR$(AC%) + "+", 10)
      IF AC% < 0 THEN                                \
         TOTAC$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(AC%),LEN(STR$(AC%))-1),9) + "-"

      NUMEF$ = RIGHT$("0000000000" + STR$(NEF%) + "+", 10)
      TOTEF$ = RIGHT$("0000000000" + STR$(EF%) + "+", 10)
      IF EF% < 0 THEN                                \
         TOTEF$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(EF%),LEN(STR$(EF%))-1),9) + "-"

      NUMCH$ = RIGHT$("0000000000" + STR$(NCH%) + "+", 10)
      TOTCH$ = RIGHT$("0000000000" + STR$(CH%) + "+", 10)
      IF CH% < 0 THEN                                \
         TOTCH$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CH%),LEN(STR$(CH%))-1),9) + "-"

      NUMOT$ = RIGHT$("0000000000" + STR$(NOT%) + "+", 10)
      TOTOT$ = RIGHT$("0000000000" + STR$(OT%) + "+", 10)
      IF OT% < 0 THEN                                \
         TOTOT$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(OT%),LEN(STR$(OT%))-1),9) + "-"

      NUMRM$ = RIGHT$("0000000000" + STR$(NRM%) + "+", 10)
      TOTRM$ = RIGHT$("0000000000" + STR$(RM%) + "+", 10)
      IF RM% < 0 THEN                                \
         TOTRM$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(RM%),LEN(STR$(RM%))-1),9) + "-"

      NUMTC$ = RIGHT$("0000000000" + STR$(NTC%) + "+", 10)
      TOTTC$ = RIGHT$("0000000000" + STR$(TC%) + "+", 10)
      IF TC% < 0 THEN                                \
         TOTTC$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(TC%),LEN(STR$(TC%))-1),9) + "-"

      NUMCR$ = RIGHT$("0000000000" + STR$(NCR%) + "+", 10)
      TOTCR$ = RIGHT$("0000000000" + STR$(CR%) + "+", 10)
      IF CR% < 0 THEN                                \
         TOTCR$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CR%),LEN(STR$(CR%))-1),9) + "-"
   END SUB

! -------------  Rutina para crear compaginar Hora y Fecha --------------

  SUB CREAFECHA
      FRA.TIM$ = TIME$                                        ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                            ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                           ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                           ! Arma segundos
      FRA.FEC$ = DATE$                                        ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                            ! Arma a¤o
      FRA.MM$  = MID$(FRA.FEC$,3,2)                           ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                           ! Arma dia
      MES      = VAL(FRA.MM$)                                 ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"                ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"               ! Si es Siglo XXI
      FHOY$    = MESES(MES)+" "+STR$(VAL(FRA.DD$))           \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                           !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +           \! Arma la Linea
               "    de "+FHOY$                                ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                           ! Salva Mes y Dia 
  END SUB

!------------------------------------------------------------
!----            PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------
   DIM MESES(12)
   FECHA$  = DATE$                                            ! Toma Fecha
   MESPRO  = VAL(MID$(FECHA$,3,2))                            ! Mes
   DIAPRO$ = RIGHT$(FECHA$,2)                                 ! Dia
   ANOPRO$ = LEFT$(FECHA$,2)                                  !
   FINR$   = CHR$(13) + CHR$(10)                              ! Caracter "0D0A"
   FINREC  = CHR$(13) + CHR$(10)                              ! Fin de Reg 0D0A
!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------
TITULO$ = "      ** CREACION ARCHIVO DIARIO CLIENTE FRECUENTE **"
CALL CREAFECHA                                                !
FEC1$ = FRA.LIN$                                              !
CONLI% = 60                                                   !  
EMPEZAR:                                                      !
AR0$ = "EAMCSCF1"                                             ! Arch Control Almac
AR1$ = "C:\ADX_UDT1\MED.DAT"                                  ! Medios de Pago
AR2$ = "C:\ADX_UDT1\PAGCOM.DAT"                               ! Pagos Compbte
AR3$ = "C:\ADX_UDT1\CHQPOS.DAT"                               ! Cheques Postfec
AR4$ = "C:\ADX_UDT1\PAGO.DAT"                                 ! Pagos Hoy
AR5$ = "C:\ADX_UDT1\COMP.DAT"                                 ! Compras Hoy
AR6$ = "C:\ADX_UDT1\PG.DAT"                                   ! Pagares
AR7$ = "C:\VEND1.DAT"                                         ! Vendedores
AR8$ = "C:\R"                                                 ! Rebajas
AR9$ = "C:\ADX_UDT1\CUO.DAT"                                  ! Medios de Pago
AR10$= "C:\C"                                                 ! Medios de Pago
AR11$= "C:\ADX_UDT1\T"                                        ! Medios de Pago
OPEN AR0$ KEYED RECL 36 AS 1 UNLOCKED NOWRITE NODEL           ! Arch. Control
OPEN "EAMACCTP" DIRECT RECL 512 AS 11 NOWRITE NODEL           !
KEYCF1$=PACK$("9998")                                         ! Reg. de Cierre
READ FORM "C36";#1 KEY KEYCF1$;REGCF1$                        ! Lectura
B$="C:\ADX_IDT4\" + MID$(REGCF1$,11,8) + ".DAT"               ! TSLOG Previo
!B$="C:\ADX_UDT1\EAMT" 
IF MID$(REGCF1$,11,8)="        " THEN B$="EAMTRANA"           ! TSLOG 1st Vez
CLOSE 1                                                       ! Cierra EAMCSCF1
OPEN "C:\ADXALMA.DAT" AS 24 BUFFSIZE 80                       ! Archivo Control Almacen
READ FORM "C80";#24;AL$                                       ! Lee Control Almac
CLOSE 24                                                      ! Lo Cierra 
AL$     = AL$ + STRING$(20," ")                               ! Ajusta Reg Ctrl
RESP$   = MID$(AL$,5,8)                                       ! Toma Fecha Control
NUAL$   = STR$(VAL(LEFT$(AL$, 4)))                            ! Guarda Num Almacen
CCOSTO$ = "0"+NUAL$                                           ! Guarda Centro Costo
NOAL$   = MID$(AL$, 13, 20)                                   ! Guarda Nombre
ALM$    = NUAL$                                               ! 
DAT1$   = MID$(AL$,9,12)                                      ! Dia mes control 
RESP$   = RESP$+"0"+NUAL$                                     ! Arma Reg Ctrl 
AR10$   = AR10$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
AR8$    = AR8$  + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
AR11$   = AR11$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
CLEARS                                                        !
PRINT  :   PRINT  :  PRINT  :   PRINT                         !
PRINT TITULO$                                                 !
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT "    Proceso del Almac‚n No. ";ALM$;"  ";NOAL$          !
PRINT "    Inicia a las: ";FEC1$                              ! Indica comienzo
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT "    Se est  procesando            : ";B$               ! Archivo en Proceso
PRINT "                                     "                 !
PRINT "        ARCHIVO DIARIO VECINO FIEL: ";AR11$            !
PRINT                                                         ! Avanza Linea
PRINT "                        Por Favor Esperar..."          !
PRINT                                                         !
TLOG    = 25  :  OUTFIL1 = 26   :     OUTFIL2 = 27            ! N£meros de
OUTFIL3 = 28  :  OUTFIL4 = 29   :     OUTFIL5 = 30            ! Sesi¢n Usados
OUTFIL6 = 31  :  OUTFIL7 = 32   :     OUTFIL8 = 33            ! por el Programa
OUTFIL9 = 34  :  OUTFI10 = 10   :     OUTFI11 = 35            !
                                                              ! 
OPEN   AR10$ DIRECT RECL 58  AS OUTFI10 BUFFSIZE 1780         !
CREATE AR11$ AS OUTFI11 BUFFSIZE 1780                         !
TIREG$  =  "1"                                                !
NITEM$  =  STRING$(14,"0")                                    !
!B$= B$+"DAT1$"+".DAT"
OPEN B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL                   ! 
B$ = ""                                                       !
ON ERROR GOTO ERRTN                                           !
WRITE FORM " C1 C4 C8 C2";#OUTFI11; "1",CCOSTO$,RESP$,FINREC  !
                                                              !
NXTRCD:                                                       !
DIM    BB.AR.KEYTV$(100),   BB.AR.PRECIO(100),               \!
       BB.AR.COMPR(100),    BB.AR.FINAL(100),                \!
       BB.AR.AGENC(100),    NETVEN$(100),    CODART$(500),   \!
       UNIVTA$(500),        MTOVTA$(500),    NITCLI$(500),   \!
       DEPART$(500),        SGNVTA$(500)
       
PTRART = 0
Q    = 1                                                      !
FT   = 0                                                      !
FTV  = 0                                                      !
FLAG = 0                                                      !
MTOTRAN = 0                                                   !
TIPPAG$="11"                                                  !
BB.FLAG = 0                                                   !
READ #TLOG; LINE INAREA$                                      !
INAREA$ = INAREA$ + ","                                       !
WHILE (Q < LEN(INAREA$))                                      !
  P = MATCH (",",INAREA$,Q)                                   ! Busca Delimit
  IF (P-Q) < 3 THEN BEGIN                                     ! Verif STRING Inv
    Q  =  P+1                                                 ! SETUP Sig STRING
    GOTO AGAIN                                                !
  ENDIF                                                       !
  B$ =  MID$(INAREA$,Q+1,(P-Q)-2)                             ! Quita Comillas
  B$ =  B$+":"                                                ! Adiciona :
  Q  =  P+1                                                   ! SETUP Sig STRING
  A  =  VAL(UNPACK$(LEFT$(B$,1)))                             ! Determina String
  IF A = 0 THEN GOSUB S0: GOTO AGAIN                          !
  IF (A < 0) OR (A > 11) THEN BEGIN                           !
    GOTO AGAIN                                                !
  ENDIF                                                       !
!PRINT "Subrutina: ";A
  IF A  =  1  THEN GOSUB S1                                   ! Trans item entry  
  IF A  =  2  THEN GOSUB S2                                   ! Trans entry ext
!  IF A  =  3  THEN GOSUB S3                                   ! Descu
 ! IF A  =  4  THEN GOSUB S4                                   ! Anul Desc
  IF A  =  5  THEN GOSUB S5                                   ! Medios de pago
  IF A  =  6  THEN GOSUB S6                                   ! Corr medios pago
  IF A  =  9  THEN GOSUB S9                                   ! Camb food stamp
  IF A  =  11 THEN GOSUB S11                                  ! Entrada datos
AGAIN:                                                        !
WEND                                                          !
IF FT = 1 AND FLAG = 0 THEN BEGIN                             !
  IF VALPAG <> "0000000000" THEN BEGIN                        !
    NREC% = VAL(CONSEC$) + 2                                  !
    CALL ACUMULA                                              !
!PRINT "FCTA GR : ";CHEQUE$,NOCUE$
    REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+          \!
              CHEQUE$+CTACTE$+NUMTER+NUMTRAN+                \!
              NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FINREC            !
    IF LEN(REGMED$) = 76 THEN BEGIN                           !
       !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$                !
    ENDIF ELSE BEGIN                                          !
       PRINT "ER ",TIREG$," ",CONSEC$                         !
    ENDIF                                                     !

  PORCEN = 0                                                 !
  IF MTOTRAN <> 0 THEN BEGIN                                 !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                  ! 
  ENDIF                                                      !

    PORCEN = 0                                                !
    IF MTOTRAN <> 0 THEN BEGIN                                !
       PORCEN = VAL(VALPAG) / MTOTRAN * 100                   ! 
    ENDIF                                                     !
    PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                   !
    NREC9% = NREC9% + 1                                       !
  ENDIF                                                       !
ENDIF                                                         !
!------------------------------------------------------
!             Grabamos registros de pagos
!------------------------------------------------------
FOR I = 0 TO D - 1
   TIPTRA1 = "00"
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=41 THEN TIPTRA1 = "06" 
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=42 THEN TIPTRA1 = "07" 
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=43 THEN TIPTRA1 = "00" 
   IF TIPTRA1 <> "00" THEN BEGIN
      NOCUE$=STRING$(7,"0")+UNPACK$(RIGHT$(BB.AR.KEYTV$(I),4))
      IF VAL(VALCHQ) < 1 THEN VALCHQ = "000000000000"
        NUMPAG$=BB.AR.COMPR(I)
        FIN$=BB.AR.FINAL(I)
        AGENC$=BB.AR.AGENC(I)
        VALPAG=RIGHT$(STRING$(10,"0")+STR$(BB.AR.PRECIO(I)),9)
        IF BB.AR.PRECIO(I) > 0 THEN BEGIN
           SIGNO$ = "+"
        ENDIF ELSE BEGIN
!                             VALPAG=STR$(BB.AR.PRECIO(I))
           SIGNO$ = "-"
        ENDIF
        IF VAL(VALPAG) > 0 THEN BEGIN
           VALPAG = RIGHT$(VALPAG,9)+"00"
        ENDIF ELSE BEGIN
           VALPAG = RIGHT$(VALPAG,9)
        ENDIF
     ENDIF
  NEXT I
  IF FTV=1 THEN BEGIN
     FOR E=1 TO F
        WRITE FORM "C3 C10 C2";#OUTFIL7;VEND$,NETVEN$(E),FINREC
     NEXT E
  ENDIF
  DIM      BB.AR.KEYTV$(0),  BB.AR.PRECIO(0),           \!
           BB.AR.COMPR(0),   BB.AR.FINAL(0),            \!
           CODART$(0),	     UNIVTA$(0),    MTOVTA$(0), \!
           NITCLI$(0),	     DEPART$(0),    SGNVTA$(0), \!
           NETVEN$(0)                                    ! Reset de arreglos
  D=0
  F=0
GOTO NXTRCD
WRITE FORM " C39 ";#OUTFI11;                            \!
"9999999999999999999999999999999999999999999999"

S0:                !  Transacciones header
  J = 3
  VALCHQ="0"
  GOSUB GETUNPK
  NUMTER  =  RIGHT$(STRING$(3,"0")+A$,3)
  GOSUB GETUNPK
  NUMTRAN =  RIGHT$(STRING$(4,"0")+A$,4)
  GOSUB GETUNPK
  FECTRA  =  MID$(A$,3,8)
  COMPRO  =  "1"+MID$(FECTRA,5,2)+NUMTRAN
  HORATR$ =  MID$(A$,7,4)
  GOSUB GETUNPK
  TIPTRA  =  RIGHT$(STRING$(1,"0")+A$,1)
  TIPTRA1 =  "00"
  BANCO$  =  "00"
  CTACTE$ =  STRING$(14,"0")
  CEDULA$ =  CTACTE$
  CHEQUE$ =  STRING$(8,"0")
  FACTURA$ = CHEQUE$
!  PRINT "#FACTURA: ";CHEQUE$,NOCUE$
  GOSUB GETUNPK
  GOSUB GETUNPK
  NUMOPE$  =  RIGHT$(STRING$(6,"0")+A$,6)
  RETURN


S1:
  J = 3
  PTRART = PTRART + 1

! RETURN
!         ----------------------------------------------------
!           Integrar el Resto del Parrafo cuando se
!           LIQUIDEN Comisiones para Vendedores
!         ----------------------------------------------------
  GOSUB GETUNPK
  COD$=RIGHT$(A$,6)                                     ! Cod Item para rebajas
  CODART$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)           ! 
  GOSUB GETUNPK                                         !
  MTOVTA$(PTRART)=RIGHT$(STRING$(10,"0")+A$,10)         !
  BB.PRECIO = VAL(A$)                                   ! Valor del pago
  NETOPO$=RIGHT$(STRING$(10,"0")+A$,10)                 ! Vr Item vendedores
  GOSUB GETUNPK                                         !
  DEP$=RIGHT$(A$,3)                                     ! Sec Plu para rebajas
  DEPART$(PTRART)=RIGHT$(STRING$(3,"0")+A$,3)           !
  UNIVTA$(PTRART)= "000001"                             !
  DEPI=VAL(DEP$)                                        !
  GOSUB GETUNPK                                         !
  GOSUB GETUNPK                                         !
  GOSUB GETFLAG                                         ! 
  GOSUB GETUNPK                                         !
  GOSUB GETFLAG                                         !

  SGNVTA$(PTRART) = "+"                                 !
  IF MID$(A$,9,1)="1" THEN BEGIN                        !
     SGNVTA$(PTRART) = "-"                              !
     BB.FLAG = 1:BB.PRECIO=BB.PRECIO*-1                 ! Si es anulacion
     NETOPO$ = STR$(VAL(NETOPO$)*-1)                    ! Vendedores
  ENDIF
  MTOTRAN = MTOTRAN + BB.PRECIO
GOSUB GETUNPK
 RETURN

S2:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  BB.PRECIO = VAL(A$)
  GOSUB GETUNPK
  UNIVTA$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)
  MTOTRAN = MTOTRAN + BB.PRECIO * VAL(UNIVTA$(PTRART))
  RETURN
  
S5:
  J = 3
     PORCEN = 0                                             !
     IF MTOTRAN <> 0 THEN BEGIN                             !
        PORCEN = VAL(VALPAG) / MTOTRAN * 100                ! 
     ENDIF                                                  !
     PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                !
     NREC9% = NREC9% + 1                                    !
     !WRITE FORM                                            \!
     !"C1 C6 C4 C9 C1 C8 C14 C3 C4 C6 2C2 C14 C3 C2";       \!
     !#OUTFIL9,NREC9%;TIREG$,CONSEC$,HORATR$,VALPAG,        \!
     !SIGNO$,CHEQUE$,CTACTE$,NUMTER,NUMTRAN,                \!
     !NUMOPE$,TIPPAG$,BANCO$,NOCUE$,PORCEN$,FINREC           !
     IF TIPPAG$ = "43" THEN BEGIN                          \! Cheque Postfechado
      !  WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";   \! Graba Reg Pago
       !        #OUTFIL3;RIGHT$(NOCUE$,7),FECTRA,"01",      \!
        !       "2","0000000",VALPAG,SIGNO$,"0000000",      \!
         !      "             ","0000000000000000000",FINREC !
     ENDIF                                                  !
!   ENDIF ELSE BEGIN                                        !
!     CONSE  = CONSE - 1                                    ! 
!     CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)        ! Consecutivo
! ENDIF                                                     !
!  ENDIF                                                    !
 
  GOSUB GETUNPK                                              ! Tip/Vrdad Pg 
  TIPPAG$  =  RIGHT$("00" + A$,2)                            ! Guarda Tip/Var
  IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                      ! Conv Otr/Pg a CR
  TIPPAW$  =  TIPPAG$                                        !
  GOSUB GETUNPK                                              ! Valor Pago 
  VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                    ! Guarda Vr Pag
  IF VAL(A$) < 0 THEN BEGIN                                  !
     SIGNO$ = "-"                                            !
      VALPAG  =  RIGHT$(STRING$(10,"0")+STR$(VAL(A$)*-1),9)   !
  ENDIF ELSE BEGIN                                           !
     SIGNO$ = "+"                                            !
  ENDIF                                                      !
  GOSUB GETUNPK                                              ! Vr Comisiones
  GOSUB GETUNPK                                              ! No. Cliente
  NCLIE$  =  A$
  NRWRK$  =  A$                                              ! Guarda Cliente
  DATO$  = LEFT$("000" + (DATO2$),3)                    ! 
  IF TIPPAG$ = "11" OR TIPPAG$ = "21"                       \!
  OR TIPPAG$ = "22" OR TIPPAG$ = "41"                       \!
  OR TIPPAG$ = "51" OR TIPPAG$ = "52"                       \!
  OR TIPPAG$ = "53" OR TIPPAG$ = "54"                       \!
  OR TIPPAG$ = "61" OR TIPPAG$ = "64"                       \!
  OR TIPPAG$ = "65" OR TIPPAG$ = "66" THEN BEGIN
    FOR INDART = 1 TO PTRART                                 !
  IF   VAL(DATO2$) >  99999              THEN BEGIN          !Si vecino fiel
      WRITE FORM "C1 C6 C3 C1 C6 C10 C14 C4 C2";            \!
      #OUTFI11;                                             \! 
      "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
      UNIVTA$(INDART),MTOVTA$(INDART),DATO2$,DAT1$,FINREC    !                                     !
  ENDIF                                                      ! Fin vecino fiel
    NEXT INDART                                              !  
    DATO2$ = ""
  ENDIF                                                      !
                                                             !

  LONG      =  LEN(A$)
  NOCUE$  =  RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14) ! N§ Cuenta 
  POS% = MATCH("##############",NOCUE$,1)                   ! Mira 14 Dig-->
  IF POS% = 0 THEN BEGIN                                    ! Hubo Caract Inv
     NOCUE$=RIGHT$(STRING$(24,"0")+LEFT$(NRWRK$,LONG-1),14) ! N§ Cuenta 
  ENDIF
  CONSE   =  CONSE + 1                                      ! Acum Consec
  CONSEC$  =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
  FT      =  1 
  GOSUB GRABA
  RETURN

S6:
  IF FT = 1 THEN BEGIN                                      !
     IF VALPAG <> "000000000" THEN BEGIN                    !
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUMTER+NUMTRAN+          \!
                  NUMOPE$+TIPPAG$+BANCO$+NRWRK$+FINREC      !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$          !
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
!PRINT "#FACTURA: ";CHEQUE$,NOCUE$
        PORCEN = 0                                          !
        IF MTOTRAN <> 0 THEN BEGIN                          !
           PORCEN = VAL(VALPAG) / MTOTRAN * 100             ! 
        ENDIF                                               !
        PORCEN$= RIGHT$("000" + STR$(PORCEN),3)             !
        NREC9% = NREC9% + 1                                 !
        !WRITE FORM                                         \!
        !"C1 C6 C4 C9 C1 C8 C14 C3 C4 C6 2C2 C14 C3 C2";    \!
        !#OUTFIL9,NREC9%;TIREG$,CONSEC$,HORATR$,VALPAG,     \!
        !SIGNO$,CHEQUE$,CTACTE$,NUMTER,NUMTRAN,             \!
        !NUMOPE$,TIPPAG$,BANCO$,NOCUE$,PORCEN$,FINREC        !
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
         !  WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
          !     #OUTFIL3;RIGHT$(NOCUE$,7),FECTRA,"01",      \!
           !    "2","0000000",VALPAG,SIGNO$,"0000000",      \!
            !   "             ","0000000000000000000",FINREC !
        ENDIF                                               !
     ENDIF ELSE BEGIN                                       !
        CONSE  = CONSE - 1                                  !
        CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)      ! Consecutivo
     ENDIF
     J = 3
     FT      =  1                                           !
     GOSUB GETUNPK                                          ! Toma Tipo/Var
     TIPPAG$  =  RIGHT$("00" + A$,2)                        ! en 2 Posic
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GOSUB GETUNPK                                          ! Toma Vr Pago
     VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                ! Guarda Vr Pag
     SIGNO$ = "-"                                           ! Asigna signo
     GOSUB GETUNPK                                          ! Toma Comision
     GOSUB GETUNPK                                          ! Toma No Cta
     IF A$ = "" THEN A$ = "0"                               !
     NRWRK$  =  A$                                          ! y lo guarda 
     IF LEFT$(TIPPAG$,1) <> "2" THEN BEGIN                  ! Id cheque 
        VALCHQ=RIGHT$(VALPAG,9)+"00"                        ! coloca cvs
        FECHEQUE  =  "0"                                    !
        BANCO$     =  "00"                                  !
        LONG      =  LEN(A$)                                ! Log del No Cta
     ENDIF                                                  !
     NOCUE$ = RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14)! Num de Cuenta 
     CONSE  = CONSE + 1                                     ! Suma Consec
     CONSEC$= RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
  ENDIF ELSE BEGIN                                          !
     J       =  3                                           !
     FLAG    =  0                                           !
     GOSUB GETUNPK                                          ! Toma Tipo/Var
     TIPPAG$  =  RIGHT$("00" + A$,2)                        ! en 2 Posic
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GOSUB GETUNPK                                          ! Toma Vr Pago
     VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                ! Guarda Vr Pag
     SIGNO$ = "-"                                           ! Asigna signo
     GOSUB GETUNPK                                          ! Toma Comision
     GOSUB GETUNPK                                          ! Toma No Cta
     IF A$ = "" THEN A$ = "0"                               !
     NRWRK$  =  A$                                          ! y lo guarda 
     IF LEFT$(TIPPAG$,1) <> "2" THEN BEGIN                  ! Id cheque 
        VALCHQ=RIGHT$(VALPAG,9)+"00"                        ! coloca cvs
        FECHEQUE  =  "0"                                    !
        BANCO$     =  "00"                                  !
        LONG      =  LEN(A$)                                ! Log del No Cta
     ENDIF                                                  !
     NOCUE$ = RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14)! Num. Cuenta 
     CONSE  = CONSE + 1                                     !
     CONSEC$= RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
     IF TIPPAG$ > "10" THEN BEGIN                           ! Estaba NO=11
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUMTER+NUMTRAN+          \!
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FINREC      !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$          !
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
!PRINT "#FACTURA: ";CHEQUE$,NOCUE$
        !IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           !WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
            !   #OUTFIL3;RIGHT$(NOCUE$,7),FECTRA,"01",      \!
             !  "2","0000000",VALPAG,SIGNO$,"0000000",      \!
              ! "             ","0000000000000000000",FINREC !
        !ENDIF                                               !
    ENDIF                                                   !
  ENDIF                                                     !
  PORCEN = 0                                                !
  IF MTOTRAN <> 0 THEN BEGIN                                !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                 ! 
  ENDIF                                                     !
  PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                   !
  BANCO = 0                                                 !
  BANCO = (100 - VAL(BANCO$))                               !
  BAN$ = RIGHT$("000" + STR$(BANCO),3)                      !
  NIT$ = RIGHT$("00000000000000" + NRWRK$,14)                !
  DATO$  = LEFT$("000" + (DATO2$),3)                    ! 
  IF TIPPAG$ = "11" OR TIPPAG$ = "21"                       \!
  OR TIPPAG$ = "22" OR TIPPAG$ = "41"                       \!
  OR TIPPAG$ = "51" OR TIPPAG$ = "52"                       \!
  OR TIPPAG$ = "53" OR TIPPAG$ = "54"                       \!
  OR TIPPAG$ = "61" OR TIPPAG$ = "64"                       \!
  OR TIPPAG$ = "65" OR TIPPAG$ = "66" THEN BEGIN
    FOR INDART = 1 TO PTRART                                 !
  !IF   VAL(DATO2$) >  517000000          THEN BEGIN          ! Si vecino fiel
    !  WRITE FORM "C1 C6 C3 C1 C6 C10 C14 C4 C2";            \!
     ! #OUTFI11;                                             \! 
     ! "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
     ! UNIVTA$(INDART),MTOVTA$(INDART),DATO2$,DAT1$,FINREC    !                                    !
  !ENDIF                                                      ! fin vecino fiel
    NEXT INDART                                              !
  ENDIF                                                      !
                                                             !
PRINT 

  RETURN                                                    !

S9:
  J = 3                                                     !
  IF FT = 1 AND FLAG = 0 THEN BEGIN                         !
     IF VALPAG <> "000000000" THEN BEGIN                    !
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUMTER+NUMTRAN+          \!
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FINREC      !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$          !
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
        !IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
         !  WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
          !    #OUTFIL3;RIGHT$(NOCUE$,7),FECTRA,"01",       \!
           !   "2","0000000",VALPAG,SIGNO$,"0000000",       \!
            !  "             ","0000000000000000000",FINREC  !
        !ENDIF                                               !
     ENDIF ELSE BEGIN                                       !
        CONSE  =  CONSE - 1                                 !
        CONSEC$ =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)     ! Consecutivo
     ENDIF
  ENDIF
  FLAG    =  1
  GOSUB GETUNPK                                             ! Tipo y Vrdad
  GOSUB GETUNPK                                             ! Cambio Efectivo
  VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)
  TIPPAG$ =  "11"
  BANCO$  =  "00"
  !NOCUE$  =  STRING$(14,"0")
  CHG     =  VAL(VALPAG)*-1
  VALPAG  =  STR$(CHG)
  IF VAL(VALPAG) < 0 THEN BEGIN
     SIGNO$ = "-"
     VALPAG  =  RIGHT$(STRING$(10,"0")+STR$(CHG*-1),9)
  ENDIF ELSE BEGIN
     SIGNO$ = "+"
  ENDIF
  CONSE   =  CONSE+1
  CONSEC$ =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)            ! Consecutivo
      NREC% = VAL(CONSEC$) + 2                               !
      CALL ACUMULA                                           !
      REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+       \!
                CHEQUE$+CTACTE$+NUMTER+NUMTRAN+             \!
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FINREC         !
      IF LEN(REGMED$) = 76 THEN BEGIN                        !
         !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$             !
      ENDIF ELSE BEGIN                                       !
         PRINT "ER ",TIREG$," ",CONSEC$                      !
      ENDIF                                                  !
  RETURN                                                     !

S11:
  J = 3                                                      !


!  GOSUB GETUNPK                                              !
!	IF A$ = "?" OR A$ = ">>" OR \ 			! Si es un data entry
!		A$ = "<<" THEN RETURN			! de Cliente Frecuente
!							! lo ignora. JGRP-CISA

  GOSUB GETUNPK
  DATO1$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO2$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO3$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO4$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO5$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
        CHEQUE$  = RIGHT$(STRING$(20,"0")+A$,8)

        IF DATO6$ <> "000000" THEN BEGIN
           REINT$ = CTACTE$   :   ZETA$  = CHEQUE$
           NCAJA$ = BANCO$    :   DOCUM$ = DATO4$
           ACTIV$ = DATO5$    :   NCLIE$ = DATO6$
        ENDIF
  RETURN
!------------------------------------------------------  
!    El resto del p rrafo S11 sirve para manejar 
!    pagos para Sistema de Cr. Ver User Exits para BG
!                                                Dic/94
!------------------------------------------------------  

  IF LEFT$(A$,1)<>"<" THEN BEGIN
    IF  VAL(A$) > 150 AND VAL(A$) < 200 THEN BEGIN
       VEND$=RIGHT$(A$,3)
       FTV=1
    ENDIF
  ENDIF
  CUOT$ = MID$(A$,2,2)
  CPR$ = MID$(A$,4,7)
  AUT$=RIGHT$(A$,4)
  IF TIPPAG$ = "41" THEN BEGIN
     IF CUOT$ > "24" THEN CUOT$ = "24"
  ENDIF
  IF TIPPAG$ = "42" THEN BEGIN
     IF CUOT$ > "36" THEN CUOT$ = "36"
  ENDIF
  IF VAL(VALPAG) > 0 THEN BEGIN
     VALPAG = RIGHT$(VALPAG,9) + "00"
  ENDIF ELSE BEGIN
     VALPAG = RIGHT$(VALPAG,9) 
  ENDIF
  IF TIPPAW$ = "41" OR TIPPAW$ = "42" THEN BEGIN\
     !WRITE FORM "C7 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";            \
      !     #OUTFIL5;RIGHT$(NOCUE$,7),FECTRA,"00","01","2",CPR$,  \
       !    VALPAG,"0"+AUT$,CUOT$,"              ",FINREC
    ! WRITE FORM "C7 C2 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";         \
     !      #OUTFIL6;RIGHT$(NOCUE$,7),TIPPAG$,FECTRA,"00","01","2",\
      !     CPR$,VALPAG,"0"+AUT$,CUOT$,"              ",FINREC
     TIPPAW$ = "00"
ENDIF ELSE BEGIN
  BB.KEYTV$=PACK$(MID$(A$,5,26))
  BB.FINAL=MID$(A$,32,1)
  BB.AGENC=MID$(A$,33,1)
  BB.COMPR=RIGHT$(A$,7)
  IF BB.FLAG THEN BEGIN                                  ! Si fue anulacion
     BB.FLAG = 0
     BB.FLAG2 = 0
     FOR I = 0 TO D - 1                                  ! Busca ANUL o DEV
         IF BB.KEYTV$ = BB.AR.KEYTV$(I) THEN            \!
            BB.FLAG2=-1   :    I=D-1                     ! Anulacion
     NEXT I
     IF BB.FLAG2 THEN BEGIN                              ! Si hay PLU en Transac
        IF D > 1 THEN BEGIN                              ! Arregla elementos
           FOR I = 0 TO D - 1                            ! del arreglo para
              IF BB.KEYTV$ = BB.AR.KEYTV$(I) THEN BEGIN  ! sacar Pago anulado
                 FOR J = I TO D - 1                      !
                     BB.AR.KEYTV$(J) = BB.AR.KEYTV$(J+1) !
                     BB.AR.PRECIO(J) = BB.AR.PRECIO(J+1) !
                     BB.AR.FINAL(J) = BB.AR.FINAL(J+1)   !
                     BB.AR.AGENC(J) = BB.AR.AGENC(J+1)   !
                     BB.AR.COMPR(J) = BB.AR.COMPR(J+1)   !
                 NEXT J
                 D = D - 1
                 RETURN
              ENDIF
            NEXT I
         ENDIF ELSE BEGIN
            D = 0
         ENDIF
     ENDIF ELSE BEGIN 
        BB.AR.KEYTV$(D) = BB.KEYTV$
        BB.AR.PRECIO(D) = BB.PRECIO
        BB.AR.FINAL(D) = BB.FINAL
        BB.AR.AGENC(D) = BB.AGENC
        BB.AR.COMPR(D) = BB.COMPR
        D = D + 1
     ENDIF
  ENDIF ELSE BEGIN                       
     BB.AR.KEYTV$(D) = BB.KEYTV$
     BB.AR.PRECIO(D) = BB.PRECIO
     BB.AR.FINAL(D)  = BB.FINAL
     BB.AR.AGENC(D)  = BB.AGENC
     BB.AR.COMPR(D)  = BB.COMPR
     D = D + 1
ENDIF
ENDIF
  RETURN
!-------------------------------------------------------------
GRABA:
  IF FT = 1 AND FLAG = 0 THEN BEGIN                          !
    IF VALPAG <> "000000000" THEN BEGIN                      !
      NREC% = VAL(CONSEC$) + 2                               !
       CALL ACUMULA                                          !
       REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+      \!
                CHEQUE$+CTACTE$+NUMTER+NUMTRAN+             \!
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FINREC         !
      IF LEN(REGMED$) = 76 THEN BEGIN                        !
         !WRITE FORM "C76";#OUTFIL1,NREC%;REGMED$             !
      ENDIF ELSE BEGIN                                       !
         PRINT "ER ",TIREG$," ",CONSEC$                      !
      ENDIF                                                  !
   ENDIF ELSE BEGIN                                          !
     CONSE  = CONSE - 1                                      ! 
     CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
   ENDIF                                                     !
 ENDIF                                                       ! 
FT = 0                                                       !
RETURN                                                       !  
! ---------------------------------------------------------------------

GETUNPK:
  K = MATCH(":",B$,J)               ! Busca separador de Campo
  A$ = UNPACK$(MID$(B$,J,K-J))      ! y lo desempaqueta
  H$ = MID$(B$,J,K-J)               !
  J=K+1                             ! Marca el inicio de sigui-
  RETURN                            ! ente campo

! ---------------------------------------------------------------------

GETFLAG:
 FLAG = VAL(A$)                     ! Convierte FLAG a INTEGER
                                    ! Empieza  con  STRING que
                                    ! contiene FLAGS individuales
  IF (FLAG AND 00000001H) THEN     \!
      A$ = "1" ELSE A$ = "0"        !
  FOR I = 1 TO 15                   ! 
    FLAG = SHIFT(FLAG,1)            ! Inicializa el siguiente BIT
    IF (FLAG AND 00000001H) THEN   \!
        A$ = "1" + A$              \!
    ELSE                           \!
        A$ = "0" + A$               !
  NEXT I                            !
  RETURN                            !

! ---------------------------------------------------------------------

  ERRTN:
  IF ERRF% = 24 THEN BEGIN
     CLEARS
     PRINT
     PRINT
     PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
     PRINT
     PRINT "               Consulte con el Depto de Sistemas."
     PRINT "                 Crear  C:\ADXALMA.DAT"
     STOP
  ENDIF
  IF ERR <> "EF" THEN  BEGIN                          ! Si NO es EOF
    PRINT "ERROR DE PROGRAMA..."                      !
    PRINT " ERR = ";ERR                               !
    PRINT "ARCH = ";STR$(ERRF%)                       !
    PRINT "ERRN = ";ERRNSTR$(ERRN)                    !
    PRINT "DATOS INVALIDOS DESDE EL"                  !
    PRINT "TRANSACTION SUMMARY LOG"                   !
    PRINT "EL PROCESO SE RESTAURA..."                 !
    RESUME NXTRCD:                                    !
  ENDIF ELSE BEGIN                                   \! Si es EOF TSLOG
    !WRITE FORM "C1 C12 6C10 C1 C2";                  \! Graba Reg Control
    !#OUTFIL1,1;"4",RESP$,TOTEF$,TOTCH$,TOTOT$,       \! de Medios de Pago
     !              TOTRM$,TOTTC$,TOTCR$,"*",FINREC    !
    !WRITE FORM "C1 C12 6C10 C1 C2";                  \! Graba Reg Control
    !#OUTFIL1,2;"5",RESP$,NUMEF$,NUMCH$,NUMOT$,       \! de Cantidades
     !              NUMRM$,NUMTC$,NUMCR$,"*",FINREC    !
    !CALL ACUDIA                                       ! 
    HORA$=TIME$                                       !
    HORA$=LEFT$(HORA$,2)+":"+                        \!
          MID$(HORA$,3,2)+":"+RIGHT$(HORA$,2)         !
   ! LPRINTER
    !PRINT
    !LPRINTER
  ENDIF                                               !
WRITE FORM " C39 ";#OUTFI11;                            \!
"9999999999999999999999999999999999999999999999"
  CLOSE 10
  STOP                                                !
! ---------------------------------------------------------------------
  FILERR:
  IF (ERR = "OE") OR (ERR = "ME") THEN BEGIN           ! SI ERROR OPEN/CREATE 
      IF ERRF% = 26 THEN                              \! IF OUTPUT FILE
        PRINT "ERROR EN APERTURA DE ARCHIVO...(OUT)"  \!
      ELSE                                            \!
        PRINT "ERROR EN APERTURA DE ARCHIVO...(IN)"    !
  ENDIF                                               \!
  ELSE                                                \! NO ES ERROR DE APERTURA
    PRINT "       OTRO  ERROR ..!"
  PRINT
  PRINT " ERR = ";ERR
  PRINT "ERRN = ";ERRNSTR$(ERRN)
  PRINT
  PRINT "EL PROGRAMA FINALIZA..."
  STOP
! ---------------------------------------------------------------------
!           ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ---------------------------------------------------------------------
