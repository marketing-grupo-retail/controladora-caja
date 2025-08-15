! CFMEDPG2.BAS
!************************************************** 
!Empresa       : Grupo Retail Ltda  		  *
!Programa      : CFMEDPG2.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Febrero de 2008                  *
!Observaciones : Reporte Fiscal Comfandi          *
!**************************************************


REAL        IV,        CIENP                         !
STRING      AA$,       BB$,       HH$,     DEPI$     !
STRING      TI$(2),    FISCO$                        !
STRING      DEPA$,     IVGR$,     TPVA$,   ALI$, CONT$
INTEGER*1   GRDES%,    PESA%,     TFI%,    NOSU%     !
INTEGER*1   IND1,      IND1A,     SGN%               !
INTEGER*2   NT%,       TR%,       CADE%,   SREC%     !
INTEGER*2   ANOT%,     MEST%,     DIAT%,   NAC%      !
INTEGER*4   ASIZE,     TOTDES%,   TPG%(3), PAGO%     ! MAX A$ RECORDS 
INTEGER*4   IVATRA%(1), STR99%, IV.TF%(3), TRF%(2), TRFI%, \!
            BLK.NUM,   REC.LEN,   KEY.LEN,          \!
            CHA.TRE,   LON.CHA,   RES.UNO, TOT.REA, \!
            REA.UPD,   TOT.ADD,   WRI.UPD, RES.DOS, \!
            TOT.DEL,   TWR.1DE,   TWR.2DE, TWR.3DE, \!
            TWR.MAS,   TOT.OPE,   TOT.CLO, TOT.CLW, \!
            TOT.REL,   ESPERADO%, AC%                ! FOR K/FILE
String      Global Old.Pago$                         ! Add OVS 27 Dic 2006
            
Integer*4   TAMA%,     BASE%,     REBAJ%,   CAN%,   \!
            CANCON%,   VARCON%, IVAT%(2),  IVAD%(2),  IVATR%,   VRUNPLU% !

Integer*4   Global Asc.Vta.Domic%                    ! Add OVS 12 Ene 2006
Integer*4   Global Vta.Fonede%,NTFN%		     ! Add OVS 10 Jul 2006
Integer*4   Global Vta.Convenios%, NTCNV%,          \! Add OVS 27 Dic 2006
             NRGTD%, VTATD%, Vta.FinAmerica%, NTFFAM%,        \! Add OVS 05 Oct 2007
                   Vta.TarjComfandi%, NTTIC%          ! Add OVS 16 Sept 2006

Integer*1                  \  !
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
       N,                I,                              \
                         L,           TLOG,              \
       FIL1,             FIL2,        LONG,              \
       FIL3,             FIL4,        FIL5,              \
       FIL6,             FIL7,        FIL8,              \
       FIL9,             FIL10,       FIL11,             \    
       BB.PCR.FLAG,      BB.FLAG,     BB.FLAG2,          \
       FTV,              MES,         CREDI%

INTEGER*2                                                \
                         ICON%,       FECR%,             \
       P,                Q,           K,        J,       \
       PP,               QQ,          KK,       JJ,      \
       CONSE,            POSI%,                          \
       POS%,             II%,         I%,                \
       SI%,              CONLI%,      DEPI%

INTEGER*4                NREC9% ,     NREC%,             \
       FLAG,             VALWRK,      VALOR,             \
       CHG,              NRO,         ICO%,              \
       NEF%,             NCH%,        NOT%,              \
       NRM%,             NTC%,        NCR%,              \
       EF%,              CH%,         OT%,               \
       NSU%,             CS%,         NCC%,              \
       NNC%,             NC%,         TD%,               \
       NCB%,             CC%,         CB%, VF%, NVF%,    \
       RM%,              TC%,         CR%,               \
       BB.PRECIO,        BB.AR.PRECIO(1),                \
       MTOTRAN,          VALDEPP,      NUEVTOT,          \
       NTD%,             FO%,          NFO%  
INTEGER*4                  \  !
       YY%,                \  !
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
       TOTTRAN%,           \  !
       TT.ALP,             \  !
       TT.ASM,             \  !
       TT.PYTOT,           \  !
       TT.INGTOT,          \  !
       MED1(2),            \  !
       TT.ANUL,            \  !
       TT.VALDEP,    TDC%, NTDC%

  STRING                                                      \ 
            A$,               B$,          H$,  F$,           \
            C$,               D$,          E$,	G$,           \
            FEC1$,            NUAL$,       NOAL$,             \
            ALMA$,            TM$(1),                       \
            BB.AR.KEYTV$(1),  BB.KEYTV$,   CONSEC$,           \
            BB.AR.COMPR(1),   BB.COMPR,    BB.AR.FINAL(1),    \
            BB.FINAL,         BB.AGENC,    BB.AR.AGENC(1),    \
            AR.VALPAG(1),     AR.CUOT(1),  AR.CPR(1),         \
            AR.AUT(1),        NETVEN$(1),  INAREA$,           \
            RESP$,            ALMACEN$,    TITULO$,           \
            VALPAG,           NUMOPE$,     NTR,               \
            NRWRK$,           NUT,         TIPTRA,            \
            TIPPAG$,          FR$   ,      COMPRO,            \
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
            DAT5$,            VALIC$,      NO.FACTURA$,       \
            AUTO$,            CUOMOD$,     TOTTRAN$,SIGNO1$,  \
            TOTCB$,           TOTVF$,      NUMCB$,  EDUCA$,   \
            NUMVF$,           BAN1$,       BANCO1$, GASOCC$,   \
            NUMAC$,           TOTAC$ ,     DESCUE$,  VALEDU$
!***************************************************************
!
  SUB CREAFECHA Public 
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
      IF FRA.AA$ < "95" THEN SIGLO$ = " de 20"                ! Si es Siglo XXI
      FHOY$    = TM$(MES)+" "+STR$(VAL(FRA.DD$))             \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                           !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +           \! Arma la Linea
               "    de "+FHOY$                                ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                           ! Salva Mes y Dia 
  END Sub
  