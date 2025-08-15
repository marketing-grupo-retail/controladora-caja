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
       OUTFIL9,          OUTFI10,     FI11,              \    
       BB.PCR.FLAG,      BB.FLAG,     BB.FLAG2,          \
       FTV,              MES,         HAY11%         

INTEGER*2                                                \
       P,                Q,           CONSE,             \
       POS%,             II%,         I%,                \
       SI%,              CONLI%,      DD%  

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
            ALMA$,            MESES(1),    DD$,               \
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
ON ERROR GOTO ERRTN                                           !
CALL CREAFECHA                                                !
FEC1$ = FRA.LIN$                                              !
CONLI% = 60                                                   !  
EMPEZAR:                                                      !
AR11$= "C:\ADX_UDT1\T515"                                     ! Medios de Pago
CLEARS                                                        !
PRINT  :   PRINT                                              !
PRINT TITULO$                                                 !
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT "    Proceso del Almac‚n No. ";ALM$;"  ";NOAL$          !
PRINT "    Inicia a las: ";FEC1$                              ! Indica comienzo
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT                                                         !
TLOG    = 25   :   FI11 = 35                                  !
                                                              ! 
INPUT " INDIQUE MES (mm)  "; DD$
DD1$ = DD$
AR11$   = AR11$+DD$+".DAT"                                    !
FILE$ = "C:\ADX_UDT1\EAMT"+DD$+".DAT"                         !
LOCATE 12,20                                                  !
PRINT "         Se est  procesando: ";FILE$                   ! Archivo en Proceso
LOCATE 13,20                                                  !
PRINT "Archivo Acumulado de Puntos: ";AR11$                   !
PRINT                                                         ! Avanza Linea
PRINT "                        Por Favor Esperar..."          !
OPEN AR11$ AS FI11 BUFFSIZE 5180 APPEND                       !
FOR I% = 1 TO 31                                              !
    EOF$ = "  "                                               !
    DD$ = DD1$ + RIGHT$("00"+STR$(I%),2)                      !
    FILE$ = "C:\ADX_UDT1\EAMT"+DD$+".DAT"                     !
    LOCATE 12,20                                              !
    PRINT "         Se est  procesando: ";FILE$               ! Archivo en Proceso
    OPEN FILE$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL            ! 
    IF EOF$ <> "  " THEN GOTO NOHUBO                          !
    TRANL1 = 0                                                !
    B$ = ""                                                   !
                                                              !
NXTRCD:                                                       !
IF HAY11% = 1 THEN BEGIN                                      !
   WRITE FORM "C25";#FI11;DATO11$                             !
ENDIF                                                         !
HAY11% = 0                                                    !
Q    = 1                                                      !
FT   = 0                                                      !
FTV  = 0                                                      !
FLAG = 0                                                      !
TIPPAG$="11"                                                  !
BB.FLAG = 0                                                   !
READ #TLOG; LINE INAREA$                                      !
TRANL1 = TRANL1 + LEN(INAREA$) + 2                            !
LOCATE 17, 10 : PRINT "Offset del Log: ";TRANL1;"        "    !
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
  IF A = 0 THEN BEGIN                                         !
     TITRA% = VAL(UNPACK$(MID$(B$,15,1)))                     !
     IF TITRA% <> 0  THEN GOTO NXTRCD                         !
  ENDIF
  IF A <> 11 THEN GOTO AGAIN                                  !
  IF A  =  11 THEN GOSUB S11                                  ! Entrada datos
  IF RIGHT$(DATO1$,2) = ">>"  THEN BEGIN                      ! Si vecino fiel
     HAY11% = 1                                               !
     DATO11$ = DATO2$ + DATO3$ + FINREC                       !
  ENDIF                                                       ! Fin vecino fiel
AGAIN:                                                        !
WEND                                                          !
GOTO NXTRCD                                                   !

S11:                                                          !
  J = 3                                                       !
  GOSUB GETUNPK                                               !
  DATO1$  =  RIGHT$(STRING$(20,"0")+A$,14)                    !
  GOSUB GETUNPK
  DATO2$  =  RIGHT$(STRING$(20,"0")+A$,9)
  GOSUB GETUNPK
  DATO3$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO4$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
  DATO5$  =  RIGHT$(STRING$(20,"0")+A$,14)
  GOSUB GETUNPK
        CHEQUE$  = RIGHT$(STRING$(20,"0")+A$,8)
  RETURN
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
  EOF$ = ERR
  IF ERR = "OE" AND ERRF% = 25 THEN BEGIN             !
     NOEXI$ = NOEXI$+" "+ RIGHT$(DD$,2)               !
     LOCATE 22,1                                      !
     PRINT "Faltan LOG's: ";NOEXI$                    !
     RESUME                                           !
  ENDIF                                               !
  IF ERRF% = 35 AND ERR="OE"  THEN BEGIN              !
     CREATE AR11$ AS FI11 BUFFSIZE 1780               !
     RESUME                                           !
  ENDIF                                               !
  IF ERR <> "EF" THEN  BEGIN                          ! Si NO es EOF
    PRINT "ERROR DE PROGRAMA..."                      !
    PRINT " ERR = ";ERR                               !
    PRINT "ARCH = ";STR$(ERRF%)                       !
    PRINT "ERRN = ";ERRNSTR$(ERRN)                    !
    PRINT "DATOS INVALIDOS DESDE EL"                  !
    PRINT "TRANSACTION SUMMARY LOG"                   !
    PRINT "EL PROCESO SE RESTAURA..."                 !
    STOP
    RESUME NXTRCD:                                    !
  ENDIF ELSE BEGIN                                   \! Si es EOF TSLOG
    HORA$=TIME$                                       !
    HORA$=LEFT$(HORA$,2)+":"+                        \!
          MID$(HORA$,3,2)+":"+RIGHT$(HORA$,2)         !
  ENDIF                                               !
OTROLOG:
    CLOSE 25
NOHUBO:
NEXT I%
STOP
! ---------------------------------------------------------------------
!           ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ---------------------------------------------------------------------
