!------------------------------------------------------------------------------
!    PROYECTO              : COMFAMILIAR ANDI 
!    Nombre del Fuente     : CFMEDPAG.BAS
!    Fecha de Modificaci¢n : MARZO DE 2007       
!    Responsables          : Tec Sistemas POS   
!    Archivos que utiliza  : C:\ADX_IDT4\EAMTRANx.DAT
!                                   ( x = A, B, ¢ C )
!------------------------------------------------------------------------------
!    Genera archivos de medios de pago, pagos y compras
!    para Cr‚dito y Cobranzas. Prepara movimiento  para
!    el Sistema Central de Inventarios. "Z para reportes diarios "
!------------------------------------------------------------------------------
!
!    Las Strings '99' del LOG de Transacciones con tipo de Registro=21
!    contienen los siguientes datos que van a conformar el Archivo ANUAL   
!    de Uso de Numeracion Fiscal ADX_UDT1:NFFHISyy.DAT:
! 
!      *   Tipo de String            '99'
!      *   Tipo de Registro          '21'        (String Cierre Transaccion)
!      A   Identificador Transac     'TR'
!      A   Signo de los Valores      '+'  ¢  '-' (Positivo/Negativo)
!      A   Prefijo autorizado DIAN   '0000'      (Default)
!      *   Num-Almacen/Num-Terminal  aaaaattt
!      *   Num consecutivo Fiscal    nnnnnnnn    (1-99999999)
!      *   Valor Neto Transac        v8
!      *   Valor Iva Tarifa 1        v8
!      *   Valor Iva Tarifa 2        v8
!      *   Valor Iva Tarifa 3        v8
!      *   Valor Iva Tarifa 4        v8
!      *   Valor Iva Tarifa 5        v8
!      *   Valor Iva Tarifa 6        v8
!      *   Valor Iva Tarifa 7        v8
!      *   Valor Iva Tarifa 8        v8
!
!      Equivalencias:   * = UPD Field  ;  A = ASCII Field
!-----------------------------------------------------------------------------
! Se adiciona al reporte el informe de movimientos por 
! venta a Domicilios, desarrollado por Asic Consulting
! OVS 12 Ene 2006 
!-----------------------------------------------------------------------------
! Se adiciona al reporte el informe de movimientos por 
! venta a FONEDE    , desarrollado por Asic Consulting
! OVS 11 Jul 2006 
! Nota: En la grabacion de los conceptos BXXX se hace informando
!       la linea donde debe grabar, tener esto en cuenta para 
!       futuras adiciones.
!-----------------------------------------------------------------------------
! Se adiciona en el reporte fiscal la forma de pago 6,6 para el
! manejo de FinAmerica. desarrollado por Grupo Retail 
! OVS 5 Oct. 2007
!------------------------------------------------------------------------

%Environ C

REAL        IV,        CIENP                         !
STRING      AA$,       BB$,       HH$,     DEPI$     !
STRING      TI$(2),    FISCO$                        !
STRING      DEPA$,     IVGR$,     TPVA$,   ALI$, CONT$
INTEGER*1   GRDES%,    PESA%,     TFI%,    NOSU%     !
INTEGER*1   IND1,      IND1A,     SGN%               !
INTEGER*2   NT%,       TR%,       CADE%,   SREC%     !
INTEGER*2   ANOT%,     MEST%,     DIAT%,   NAC%      !
INTEGER*4   ASIZE,     TOTDES%,   TPG%(3), PAGO%     ! MAX A$ RECORDS 
INTEGER*4   IVATRA%(1)
INTEGER*4   STR99%, IV.TF%(3), TRF%(2),   TRFI%, \   !
            Vta.TComf%, NTTIC%                ! Add OVS 16 Sept 2008
INTEGER*4   BLK.NUM,   REC.LEN,   KEY.LEN,          \!
            CHA.TRE,   LON.CHA,   RES.UNO, TOT.REA, \!
            REA.UPD,   TOT.ADD,   WRI.UPD, RES.DOS, \!
            TOT.DEL,   TWR.1DE,   TWR.2DE, TWR.3DE, \!
            TWR.MAS,   TOT.OPE,   TOT.CLO, TOT.CLW, \!
            TOT.REL,   ESPERADO%, AC%                ! FOR K/FILE
INTEGER*4   TAMA%,     BASE%,    REBAJ%,   CAN%,    \!
            CANCON%,   VARCON%,                     \!
            IVAT%(2),  IVAD%(2),  IVATR%,   VRUNPLU% !

Integer*4   Global Asc.Vta.Domic%                    ! Add OVS 12 Ene 2006
Integer*4   Global Vta.Fonede%,NTFN%								 ! Add OVS 10 Jul 2006
Integer*4   Global Vta.Convenios%, NTCNV%						 ! Add OVS 27 Dic 2006
String      Global Old.Pago$                         ! Add OVS 27 Dic 2006
Integer*4   Global NRGTD%, VTATD%,                  \!
                   Vta.FinAmerica%, NTFFAM%          ! Add OVS 05 Oct 2007
Integer*1   Global Gr.Ind.Rem%
INTEGER*1                  \  !
       P%,                 \  !
       Q%,                 \  ! Longiud de ADXALMA.DAT
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
       TT.LOAN%,           \  !
       TT.PICKUP%,         \  !
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

Function GETN4(P1$,P2) External
 Integer*4 GETN4
 String P1$
 Integer*2 P2
End Function

Function ERRNSTR$(ERRNUM)
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
!------------------------------------------------------------------------------
!                Busca campos de String Fiscal   Oct. 21 de 2002 JRS.
!------------------------------------------------------------------------------
SUB STRING.FISCAL                                      !
  PRE99$ = "TR"                                        !
  STR99% = MATCH(PRE99$,INAREA$,1)                     !
  IF STR99% > 0 THEN BEGIN                             ! Ident TransVta
     VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0        !
              IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0        !
     PRETR$ = MID$(INAREA$,STR99%+3,4)                 !
     NUFIS$ = UNPACK$(MID$(INAREA$,STR99%+7,8))        !
     NUFIS$ = RIGHT$(NUFIS$,11)                        !
  ENDIF                                                !
  EXIT SUB
End SUB
! ---------------------------------------------------------------------
!------------------------------------------------------------------------------
!             Subrutina para la Impresion de Comprobante Contable
!------------------------------------------------------------------------------
SUB IMPRI.COMPROBANTE                                !
  WRITE FORM "2C2";#FIL5;CHR$(12),FR$                !
!       WRITE FORM "C31 C2";#FIL5;S8CND$,FR$         ! Salto;1/8's;Conds
  WRITE FORM "C17 C63 C2";#FIL5;" ",                \!
        "* CAJA COMPENSACION FAMILIAR"   +          \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL5;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL5;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  ORIG$ = "Alm:" + ALM$+NOAL$+"          "        + \!
          "Elaborado: "+ TM$(VAL(MID$(DATE$,3,2)))+ \!
          " "+STR$(VAL(RIGHT$(DATE$,2))) + "/20"  + \!
          LEFT$(DATE$,2)+"   "+LEFT$(TIME$,2)     + \!
          ":"+MID$(TIME$,3,2)                        !
  WRITE FORM "C80 C2";#FIL5;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "DEP  DESCRIPCION       %IVA       " + \!
             "VR/BASE      VR/IVA    VR/IMPOC   " + \!
             "TOT/VENTA",FR$                         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOR II% = 1 TO 998                                 !
      MACH$ = "DP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,DEPA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+"  DEPTO SIN DEFINIR"!
      IF I%<>0 THEN DEPI$=MID$(DEPA$,I%+2,27)        !
      IF IVAD%(II%,1) <> 0 THEN BEGIN                !
         IF NOSU% <> 1 THEN BEGIN                     
         IVAD%(II%,4)=IVAD%(II%,1)+IVAD%(II%,2)+    \!
                      IVAD%(II%,3)                   !
         ENDIF                                          
         IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)          !
         IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)          !
         IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)          !
         IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)          !
         IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)          !
         WRITE FORM "C29 4PIC(########,###) C2";    \!
               #FIL5;DEPI$,IVAD%(II%,1),            \!
               IVAD%(II%,2),IVAD%(II%,3),           \!
               IVAD%(II%,4),FR$                      !
      ENDIF                                          !
  NEXT II%                                           !

  Write Form "C29 PIC(########,###) C2"; #FIL5;     \! Add OVS 12 Ene 2006
        "VENTA DOMICILIOS",Asc.Vta.Domic%,FR$        !

  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL5;     \!
        "    ** TOTAL --->",IVAD%(0,1),             \!
        IVAD%(0,2),IVAD%(0,3),IVAD%(0,4),FR$         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL5;                        \!
        "                     " +                   \!
        "**  T O T A L   T R I B U T A D O  **",FR$  !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "                %IVA             " +  \!
             " VR/BASE      VR/IVA    VR/IMPOC " +  \!
             "  TOT/VENTA",FR$                       !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOR I% = 1 TO 5                                    !
      IV = VAL(MID$(TARIF$,I%*3-2,2))                !
      IVAD%(0,0) = IV                                !
      IVAD%(0,1) = 0                                 !
      IVAD%(0,2) = 0                                 !
      IVAD%(0,3) = 0                                 !
      IVAD%(0,4) = 0                                 !
      IVAD%(0,5) = 0                                 !
      FOR II% = 1 TO 998                             !
          IF IVAD%(II%,0)=IV THEN BEGIN              !
             IF NOSU% <> 1 THEN BEGIN                !
                IVAD%(999,1)=IVAD%(999,1)+IVAD%(II%,1)
                IVAD%(999,2)=IVAD%(999,2)+IVAD%(II%,2)
                IVAD%(999,3)=IVAD%(999,3)+IVAD%(II%,3)
                IVAD%(999,4)=IVAD%(999,4)+IVAD%(II%,4)
                IVAD%(999,5)=IVAD%(999,5)+IVAD%(II%,5)
             ENDIF                                   !
             IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)      !
             IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)      !
             IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)      !
             IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)      !
             IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)      !
          ENDIF                                      !
      NEXT II%                                       !
      WRITE FORM "C29 4PIC(########,###) C2";#FIL5; \!
            "                 "+STR$(IVAD%(0,0))+   \!
            "%",IVAD%(0,1),IVAD%(0,2),IVAD%(0,3),   \!
            IVAD%(0,4),FR$                           !
  NEXT I%                                            !      
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL5;     \!
        "    ** TOTAL --->",IVAD%(999,1),           \!
        IVAD%(999,2),IVAD%(999,3),IVAD%(999,4),FR$   !
  IVAD%(0,1) = 0                                     !
  IVAD%(0,2) = 0                                     !
  IVAD%(0,3) = 0                                     !
  IVAD%(0,4) = 0                                     !
  IVAD%(0,5) = 0                                     !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "2C2";#FIL5;CHR$(12),FR$                !
  WRITE FORM "C13 C67 C2";#FIL5;" ",                \!
        "* CAJA DE COMPENSACION FAMILIAR DEL"   +   \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL5;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL5;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  WRITE FORM "C80 C2";#FIL5;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "TPV  N/SERIE   #INI   #FIN #CLI    "+ \!
             "SN/IVA     VR/BASE   VR/IVA "       + \!
             "VR/IMPOC  DSC/CUP",FR$                 !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  IF HAY99% = 1 THEN BEGIN                           !
     FOR II% = 1 TO 100                              !
         IVAT%(II%,0) = TRF%(II%,0)                  !
         IVAT%(II%,1) = TRF%(II%,1)                  !
         IVAT%(II%,2) = TRF%(II%,2)                  !
     NEXT II%                                        !
  ENDIF                                              !
  FOR II% = 1 TO 100                                 !
      MACH$ = "TP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,TPVA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+" SIN DEFINIR"       !
      IF I%<>0 THEN DEPI$=MID$(TPVA$,I%+2,23)        !
      LOCATE 1 , 1                                   !
      IF IVAT%(II%,2) <> 0 OR                       \!
         IVAT%(II%,3) <> 0 THEN BEGIN                !
            IF NOSU% <> 1 THEN BEGIN                 !
               IVAT%(0,2)=IVAT%(0,2)+IVAT%(II%,2)    !
               IVAT%(0,3)=IVAT%(0,3)+IVAT%(II%,3)    !
               IVAT%(0,4)=IVAT%(0,4)+IVAT%(II%,4)    !
               IVAT%(0,5)=IVAT%(0,5)+IVAT%(II%,5)    !
               IVAT%(0,6)=IVAT%(0,6)+IVAT%(II%,6)    !
               IVAT%(0,7)=IVAT%(0,7)+IVAT%(II%,7)    !
            ENDIF                                    !
         FOIM$="C12 2PIC(#######) PIC(#####)"   +   \!
               " 2PIC(###########) 3PIC(#########) C2"
         WRITE FORM FOIM$;                          \!
               #FIL5;LEFT$(DEPI$,3)+MID$(DEPI$,5,9),\!
               IVAT%(II%,0),IVAT%(II%,1),           \!
               IVAT%(II%,2),IVAT%(II%,5),           \!
               IVAT%(II%,3),IVAT%(II%,4),           \!
               IVAT%(II%,6),IVAT%(II%,7),FR$         !
      ENDIF                                          !
  NEXT II%                                           !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM FOIM$;                                 \!
        #FIL5;"** TOTAL --->",0,0,IVAT%(0,2),       \!
        IVAT%(0,5),IVAT%(0,3),IVAT%(0,4),           \!
        IVAT%(0,6),IVAT%(0,7),FR$                    !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL5;                        \!
        "              " + "**  T O T A L   "    +  \!
        "M E D I O S    D E    P A G O  **",FR$      !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "     "                             +  \!
             "       TIPO            VARIEDAD   " + \!
             "        Cant   V A L O R ",FR$         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOIM$ = "C10 2C18 PIC(####) PIC(#########,###) C2" !
  FOR I% = 1 TO 6                                    !
      IF TPG%(I%,0,1) <> 0 THEN BEGIN                !
         FOR II% = 1 TO 6                            !
             IF TPG%(I%,II%,1) <> 0 THEN BEGIN       !
                WRITE FORM FOIM$;#FIL5;" "," ",     \!
                TI$(I%,II%),TPG%(I%,II%,0),         \!
                            TPG%(I%,II%,1),FR$       !
             ENDIF                                   !
             IF II% = 6 THEN BEGIN                   !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"-"),FR$        !
                WRITE FORM FOIM$;#FIL5;" ",         \!
                TI$(I%,0)," ",TPG%(I%,0,0),         \!
                TPG%(I%,0,1),FR$                     !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"-"),FR$        !
             ENDIF                                   !
         NEXT II%                                    !
      ENDIF                                          !
  NEXT I%                                            !
  WRITE FORM FOIM$;#FIL5;" ",                       \!
        " ","** TOTAL PAGOS **",TPG%(0,0,0),        \!
                TPG%(0,0,1),FR$                      !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"="),FR$        !
End Sub                                              !

!------------------------------------------------------------------------------
!      Rutina para Acumular Totales de Control Medios de Pago 
!------------------------------------------------------------------------------

SUB ACUDIA                                                    !
    CALL IMPRI.COMPROBANTE                                    !
    NOSU% = 1                                                 ! 
    CALL IMPRI.COMPROBANTE                                    !
    TT.SOB = 0   : TT.FAL = 0                                 !
    OPEN "EAMACCTP" DIRECT RECL 512 AS 11 NOWRITE NODEL       !
    SREC% = SREC% + 1                                         !
    READ FORM "T43 I4 I2 T55 I2 17I4 C388";#11,SREC%;        \! HEADER DATA 
               BLK.NUM,REC.LEN,KEY.LEN,CHA.TRE,              \!
               LON.CHA,RES.UNO,TOT.REA,REA.UPD,              \!
               TOT.ADD,WRI.UPD,RES.DOS,TOT.DEL,              \!
               TWR.1DE,TWR.2DE,TWR.3DE,TWR.MAS,              \!
               TOT.OPE,TOT.CLO,TOT.CLW,TOT.REL,H$             ! FOR KEYED FILE
    ASIZE = 508/REC.LEN * BLK.NUM                             ! MAX A$ RECORDS 
    LOCATE 1,25                                               !
    PRINT "Contabilidad de ";BLK.NUM;" Bloques.."             !
FOR II% = 2 TO BLK.NUM                                        ! Loop Lec EAMACC
    READ FORM                                                \! segun formato
    "C4 C1 2C5 C7 4I4 C28 I4 C28 I4 C28 8I4 C44 I4 C302 ";   \! Lect directa
         #11,II%;KEYS$,HD.TYPE,HD.OPERATOR,HD.DATETIME,      \! segun formato
                 A$,HD.GROSSPOS,HD.GROSSNEG,HD.NUMTRANS,     \! de la Contab
                 LO.AMTCASH,B$,PK.AMTCASH,C$,CT.AMTCASH,     \! del dia anter
                 D$,SO.AMTCASH,SO.AMTCHECK,SO.AMTFOODS,      \! para tomar
                 SO.AMTMISC1,SO.AMTMISC2,SO.AMTMISC3,        \! Falt/Sob
                 SO.AMTMANUF,SO.AMTSTORE,E$,AMTFEES,F$        ! y tipo de Reg
    IF HD.TYPE = "2" THEN BEGIN                               ! Si es Cajera
       LOCATE 20,15                                           ! Ubica cursor
       PRINT "     DATOS CONTABLES  ";HD.TYPE;"  ";STR$(II%);\! y muestra dat
             " Op. ";STR$(VAL(UNPACK$(HD.OPERATOR)))          ! Operador
       BASES% = BASES% + LO.AMTCASH                           ! Acumula Bases
       TT.COM = TT.COM + AMTFEES                              ! Acumula Comis
       TT.CLI = TT.CLI + HD.NUMTRANS                          ! Acumula Clientes
       IF SO.AMTCASH  > 0 THEN TT.FAL = TT.FAL + SO.AMTCASH   ! Acum Falt Ef
       IF SO.AMTCHECK > 0 THEN TT.FAL = TT.FAL + SO.AMTCHECK  ! Acum Falt Ch
       IF SO.AMTMISC1 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC1  ! Acum Falt Mis1
       IF SO.AMTMISC2 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC2  ! Acum Falt Mis2
       IF SO.AMTMISC3 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC3  ! Acum Falt Mis3
       IF SO.AMTFOODS > 0 THEN TT.FAL = TT.FAL + SO.AMTFOODS  ! Acum Falt Mis2
       IF SO.AMTSTORE > 0 THEN TT.FAL = TT.FAL + SO.AMTSTORE  ! Acum Falt CupAl
       IF SO.AMTMANUF > 0 THEN TT.FAL = TT.FAL + SO.AMTMANUF  ! Acum Falt CupFa
       IF SO.AMTCASH  < 1 THEN TT.SOB = TT.SOB + SO.AMTCASH   ! Acum Falt Ef
       IF SO.AMTCHECK < 1 THEN TT.SOB = TT.SOB + SO.AMTCHECK  ! Acum Falt Ch
       IF SO.AMTMISC1 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC1  ! Acum Falt Mis1
       IF SO.AMTMISC2 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC2  ! Acum Falt Mis2
       IF SO.AMTMISC3 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC3  ! Acum Falt Mis3
       IF SO.AMTFOODS < 1 THEN TT.SOB = TT.SOB + SO.AMTFOODS  ! Acum Falt Mis2
       IF SO.AMTSTORE < 1 THEN TT.SOB = TT.SOB + SO.AMTSTORE  ! Acum Falt CupAl
       IF SO.AMTMANUF < 1 THEN TT.SOB = TT.SOB + SO.AMTMANUF  ! Acum Falt CupFa
    ENDIF                                                     ! 

NEXT II%                                                      !

FINALIZA:                                                     !
     TT.SOBFAL = TT.SOB + TT.FAL                              !
     PRINT "                   ";                             !
     
     PRINT "VR.TOTAL SOBRA(-)/FALTA(+) ";STR$(TT.SOBFAL)      ! 
     NRGTD% = (VAL(NUMRM$)-NTDC%-NTFN%-NTFCNV%-TT.LOAN%)                 ! Add OVS 12 JUL 2006
     VTATD% = (Val(TOTRM$)-TDC%-Vta.Fonede%-Vta.Convenios%-TT.PICKUP%)   ! Add OVS 12 JUL 2006
     
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                1;RESP$, "B001","EFECTIVO          ",        \!
                VAL(NUMEF$)-NEF6%,EF%-EF6%-TT.SOBFAL,0,0,FR$             ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                2;RESP$, "B002","CH/BANCOS LOCALES ",        \!
                VAL(NUMCH$)-VAL(NUMCS$)-VAL(NUMCC$)- VAL(NUMFO$)- \!
                VAL(NUMNC$),VAL(TOTCH$)-                     \!
                VAL(TOTCS$)-VAL(TOTCC$)-VAL(TOTNC$)-FO%,0,0,FR$   ! 
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                3;RESP$, "B003","TARJETAS DEBITO   ",        \!
                NRGTD% ,VTATD%,0,0,FR$ ! 
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                4;RESP$, "B029","PAGO ELECTRONICO  ",        \!
                NTDC%,TDC%,0,0,FR$ 

     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!  Add OVS 5 Oct 2007
                5;RESP$, "B030","PAGO FINAMERICA   ",        \!
                NTFFAM%,Vta.FinAmerica%,0,0,FR$ 

     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!  Add OVS 5 Oct 2007
                6;RESP$, "B021","TARJ. COMFANDI TIC",        \!
                NTTIC%,Vta.TComf%,0,0,FR$ 

     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                7;RESP$, "B004","TARJETAS CREDITO  ",        \!
                Val(NUMTC$)-NCB%-NTD%-NTFFAM%-NTTIC%,Val(TOTTC$)-CB%-TD%-Vta.FinAmerica%-Vta.TComf%,0,0,FR$ ! 
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                8;RESP$, "B024","TARJETAS DINNERS  ",        \!
                NTD%,TD%,0,0,FR$                              ! 
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                9;RESP$, "B005","CREDITO COMFANDI  ",        \!
                VAL(NUMCR$)+NTFCNV%,VAL(TOTCR$)+VTA.CONVENIOS%,0,0,FR$               ! 
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               10;RESP$, "B006","OTROS PAGOS       ",        \!
                VAL(NUMOT$)+NEF6%-VAL(NUMAC$)-VAL(NUMVF$),VAL(TOTOT$)+EF6%-    \!
                VAL(TOTAC$)-VAL(TOTVF$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                11;RESP$, "B010","CH/ANUL SUBSIDIO  ",        \!
                VAL(NUMCS$),VAL(TOTCS$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                12;RESP$, "B026","BONO VECINO FIEL    ",     \!
                VAL(NUMVF$),VAL(TOTVF$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               13;RESP$, "B016","CH/ANULADO COMFANDI  ",     \!
                VAL(NUMNC$),VAL(TOTNC$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               14;RESP$, "B019","TARJETA COMFANDI  ",        \!
                VAL(NUMCB$),VAL(TOTCB$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               15;RESP$, "B023","BONOS SUBSIDIO    ",        \!
                VAL(NUMAC$),VAL(TOTAC$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               16;RESP$, "B027","BONOS FONEDE      ",        \!
                VAL(NUMFO$),VAL(TOTFO$),0,0,FR$               ! 
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                17;RESP$, "B025","PAGO ELEC. FONEDE ",       \!
                NTFN%,Vta.Fonede%,0,0,FR$                     !                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                18;RESP$, "B032","FAMILIAS EN ACCION",       \!
                TT.LOAN%,TT.PICKUP%,0,0,FR$                     !                
                
     TT.CONTEO = EF%+VAL(TOTCH$)+VAL(TOTOT$)+                \!
                VAL(TOTRM$)+VAL(TOTTC$)+VAL(TOTCR$)
                
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               19;RESP$, "B011","TOTAL CONTEO      ",        \!
                0,TT.CONTEO-TT.SOBFAL,0,0,FR$                 !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               20;RESP$, "B012","TOTAL *ZETA*      ",        \!
                0,TT.CONTEO,0,0,FR$                           !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               21;RESP$, "B013","TOTAL DE FALTANTES",        \!
                0,TT.FAL,0,0,FR$                              !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                22;RESP$, "B014","TOTAL CONTABILIDAD",       \!
                0,TT.CONTEO,0,0,FR$                           !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                23;RESP$, "B015","TOTAL DE SOBRANTES",       \!
                0,(TT.SOB),0,0,FR$                            !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                24;RESP$, "C001","DESCUENTO CH/SUBS ",       \!
                0,IVAD%(001,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                25;RESP$, "C070","DESCUENTO COTRAIM ",       \!
                0,IVAD%(070,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                26;RESP$, "C074","DESC. TEXTOS 20%     ",    \!
                0,IVAD%(074,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                27;RESP$, "C029","DESC/TEMPO ESCOLAR  ",     \!
                0,IVAD%(029,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                28;RESP$, "C032","DESCUENTO ANCHETAS   ",    \!
                0,IVAD%(032,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                29;RESP$, "C033","DESCUENTO FRUVER 15%",     \!
                0,IVAD%(033,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                30;RESP$, "C038","DESCTO/VECINO FIEL   ",    \!
                0,IVAD%(038,1),0,0,FR$                        !
    WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               31;RESP$, "C039","DESCUENTO CARNES 10%",     \!
               0,IVAD%(039,1),0,0,FR$                        !
    WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
               32;RESP$, "C041","DESCUENTO MERCADOS  ",     \!
               0,IVAD%(041,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                33;RESP$, "C042","DESCU. VF medic. 10%",     \!
                0,IVAD%(042,1),0,0,FR$                        ! estaba 043 dcto bonos comfandi
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                34;RESP$, "C060","DESCUENTO COMFANDI ",      \!
                0,IVAD%(060,1),0,0,FR$                        !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                35;RESP$, "C049","DESC. BONOS COMFABU",      \!
                0,IVAD%(049,1),0,0,FR$                        !
           TOTDES%=(IVAD%(001,1)+TT.COM+IVAD%(028,1)+        \!
                    IVAD%(031,1)+IVAD%(032,1)+IVAD%(033,1)+  \!
                    IVAD%(038,1)+IVAD%(039,1)+IVAD%(041,1)+  \!
                    IVAD%(043,1)+IVAD%(048,1)+IVAD%(049,1) + \!
                    IVAD%(029,1)+IVAD%(042,1)+IVAD%(060,1)+  \!
                    IVAD%(070,1)+IVAD%(074,1))+IVAD%(074,1)
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                36;RESP$, "C000","TOTAL DESCUENTOS    ",     \!
                0,TOTDES%,0,0,FR$                             !
     WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,     \!
                37;RESP$, "C003","TOTAL CLIENTES    ",       \!
                TT.CLI,0,0,0,FR$                              ! 
     SREC% = 37
     FOR II% = 1 TO 998                                       !
         IF IVAD%(II%,1) <> 0 THEN BEGIN                      !
         IF IVAD%(II%,6) = 0 THEN BEGIN                       !
            SREC% = SREC% + 1                                 !
            PREF$ = "C"                                       !
            IF II% > 779 THEN BEGIN                           !
               CANCON% = CANCON% + IVAD%(II%,5)               !
               VARCON% = VARCON% + IVAD%(II%,1)               !
               PREF$ = "F"                                    !
            ENDIF                                             !
            MACH$ = "DP"+RIGHT$("000"+STR$(II%),3)            !
            I% = MATCH(MACH$,DEPA$,1)                         !
            IF I%= 0 THEN DEPI$=MACH$+"  DEPTO SIN DEFINIR"   !
            IF I%<>0 THEN DEPI$=MID$(DEPA$,I%+7,19)           !
            PREF$ = PREF$ + RIGHT$("000" + STR$(II%),3)       !
            WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";     \!
                  #FIL10,SREC%;RESP$,PREF$,DEPI$,            \!
                         IVAD%(II%,5),IVAD%(II%,1)  +        \!
                         IVAD%(II%,2) + IVAD%(II%,3),        \!
                         IVAD%(II%,3) , IVAD%(II%,2),FR$      !
         ENDIF                                                !
         ENDIF                                                !
    NEXT II%                                                  !
    WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,      \!
                SREC%+1;RESP$,"F998","TOTAL CONCESIONES",    \!
                CANCON%,VARCON%,0,0,FR$                       ! 
    ESPERADO%=IVAD%(999,1)+IVAD%(999,2)+IVAD%(999,3)-TOTDES%  !
    WRITE FORM "C12 C4 C20 4PIC(-999999999) C2";#FIL10,      \!
                SREC%+2;RESP$,"F000","TOTAL VENTAS DEPTO",   \!
                        IVAD%(999,5),ESPERADO%,              \!
                        IVAD%(999,3),IVAD%(999,2),FR$         !
  FOR$   = "###,###"                                          !
  FORMA$ = "$$####,###,###"                                   !
  INICIMP:                                                    !
  I% = 0                                                      !
  LPRINTER                                                    !
  IF END #10 THEN CERRAR                                      ! 
LECDIR:                                                       ! Lectura Directa
  I% = I% + 1                                                 ! Incr Cont
  READ FORM "C78";#10,I%;REGI$                                ! Lectura Cuadre dia
  IF MID$(REGI$,37,1) = " " THEN BEGIN                        ! Si es blanco en Cant
     REGI$=LEFT$(REGI$,36) + "+" + RIGHT$(REGI$,41)           ! coloca signo +
  ENDIF                                                       ! 
  IF MID$(REGI$,47,1) = " " THEN BEGIN                        ! Si es Blanco en Vr
     REGI$=LEFT$(REGI$,46) + "+" + RIGHT$(REGI$,31)           ! coloca signo +
  ENDIF 
  IF MID$(REGI$,13,4) = "F000" THEN BEGIN                     ! ARREGLO PULGA SUMATO
     VALIC$ = MID$(REGI$,48,9)                                ! coloca signo +
     TT.VALDEP = VAL(VALIC$)                                  !
     PUL% = 1                                                 !
  ENDIF                                                       !  
  WRITE FORM "C78";#10,I%;REGI$                               ! Graba segun Fmto
  FECH$ = MID$(REGI$,3,6)                                     ! Toma Fecha
  CONC$ = MID$(REGI$,13,4)                                    ! Concepto
  DESC$ = MID$(REGI$,17,20)                                   ! Descriptor
  SIGN$ = MID$(REGI$,37,1)                                    ! Signo
  CANT$ = MID$(REGI$,38,9)                                    ! Cantidad 
  VALO$ = MID$(REGI$,48,9)                                    ! y Valor
  IMPO$ = MID$(REGI$,58,9)
  VIVA$ = MID$(REGI$,68,9)

  IF SI% = 1 THEN SI% = 2                                     ! Bloquea el Flag
  IF CONLI% > 58 THEN BEGIN                                   ! Si salto de Pag
     CONLI% = 3                                               ! Asigna cont
     WRITE FORM "2C2";#FIL5;CHR$(12),FR$                      !
     WRITE FORM "C53 C27 C2";#FIL5;                          \!
           "Alm:"+ALM$+" "+NOAL$,"Procesado el : "+DATE$,FR$  ! 
     WRITE FORM "C23 C57 C2";#FIL5;                          \!
           " ","CIFRAS DE CUADRE DEL DIA: "+FECH$,FR$         !
     WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$            !
     WRITE FORM "C80 C2";#FIL5;                              \!
           "    CODIGO  CONCEPTO           "+                \!
           "CANTIDAD    VR/VENTA    I/CONSUMO "+             \!
           "      VR/IVA ",FR$                                !
     WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$            !
  ENDIF                                                       !
  
  IF TOTRED% > 0 AND CONC$ = "C048" THEN FORMA$ ="+"+ "$$###,###,###"                   !
  IF TOTRED% < 0 AND CONC$ = "C048" THEN FORMA$ ="-"+ "$$###,###,###"                   !
  
  IF CONC$ <> "C048" THEN FORMA$ = "$$####,###,###"                                   !
  IF SI%  = 0 AND LEFT$(CONC$,1) = "F" OR                    \! Si 1er Vez y Conces 
     CONC$ = "F999" THEN BEGIN                                ! o Total Concesiones
       SI% = 1                                                ! Indica Flag
  ENDIF                                                       !
  IF LEFT$(CONC$,1) = "C" AND                                \!
     VAL(CANT$)     =  0  AND                                \!
     VAL(VALO$)     =  0  THEN GOTO LECDIR                    !
  IF CONC$ = "B015" OR                                       \!
     CONC$ = "C000" OR                                       \!
     CONC$ = "C015" OR                                       \!
     CONC$ = "F998" OR                                       \!
     CONC$ = "F000" OR                                       \!
     CONC$ = "B011" OR                                       \!
     SI%   = 1 THEN BEGIN                                     ! o Flag de Totales
     CONLI% = CONLI% + 1                                      ! Incr Cont de Lineas
     WRITE FORM "C26 C54 C2";#FIL5;                          \!
     " ","       ------- ------------" +                     \! Subtotal
     "   -----------   ----------- ",FR$                      ! Subtotal
  ENDIF                                                       !
  WRITE FORM "C5 C6 C20 PIC(###,###) 3PIC($$#######,###) C2";\!
        #FIL5;" ",CONC$,DESC$,VAL(CANT$),VAL(VALO$),         \!
              VAL(IMPO$),VAL(VIVA$),FR$                       !
  CONLI% = CONLI% + 1                                         !
  GOTO LECDIR                                                 !
CERRAR:                                                       !
  WRITE FORM "C26 C54 C2";#FIL5;                             \!
  " ","       ------- ------------" +                        \! Subtotal
  "   -----------   ----------- ",FR$                         ! Subtotal
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                          !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                          !
  WRITE FORM "C80 C2";#FIL5;                                 \!
        "      ---------------------------"+                 \!
        "     ----------------------------",FR$               !
  WRITE FORM "C80 C2";#FIL5;                                 \!
        "        VoBo del Administrador   "+                 \! 
        "     Recibido Centro Inf Mercadeo ",FR$              !
  IF CANLIS% = 0 THEN BEGIN                                   ! Si solo 1er listado
     CONLI% = 60                                              ! Condic de Overflow
     CANLIS% = 1                                              ! Indica 2o Listado
     CLOSE 10                                                 ! Cierra Cuadre
     OPEN AR10$ DIRECT RECL 78 AS 10 BUFFSIZE 1780            ! Abre Cuadre
     GOTO INICIMP                                             ! Vuelve a empezar
  ENDIF                                                       ! Fin Condic
DESCU    = TT.CONTEO-(TT.VALDEP+(SA.AMTDISCO-TO.DISVOID))     !
DESCU$   = STR$(DESCU)                                        !
IF TT.CONTEO = ESPERADO% + TOTDES% THEN BEGIN                 !
    CLEARS                                                    !
    CONSOLE                                                   !
    CUADRE = 1                                                !
    PRINT : PRINT : PRINT                                     !
    PRINT " "                                                 ! 
    PRINT "      ****************************************************"   !
    PRINT "               P L A N I L L A   C U A D R A D A"  !
    PRINT "                                           "       !
    PRINT "          VALOR POR DEPARTAMENTOS :";              !
    PRINT USING FORMA$ ; ESPERADO%+TOTDES%                    !
    PRINT USING FORMA$ ; ESPERADO%                            !
    PRINT USING FORMA$ ; TOTDES%                              !
    PRINT "          VALOR POR DOMICILIOS    :";              !
    PRINT USING FORMA$ ; Asc.Vta.Domic%                       !
    PRINT "                                           "       !
    PRINT "                                           "       !
    PRINT "      VALOR ZETA O MEDIOS DE PAGO :";              !
    PRINT USING FORMA$;      TT.CONTEO                        !
    PRINT "                                           "       !
    PRINT "          Continue con los procesos y con la copia"!
    PRINT "          de archivos.                        "    !
    PRINT "          Este es un mensaje informativo.     "    !
    PRINT "      ****************************************************"
ENDIF ELSE BEGIN                                              !
    CLEARS                                                    !
    CUADRE = 0                                                !
    CONSOLE                                                   !
    PRINT : PRINT                                             !
    PRINT " "                                                 !
    PRINT "      ****************************************************" 
    PRINT "      ****************************************************"
    PRINT "                                                 " !
    PRINT "             P L A N I L L A   N O  C U A D R A  " !
    PRINT " "                                                 !
    PRINT "             ERROR; ERROR; ERROR; ERROR; ERROR ; " !
    PRINT "                                              "    !
    PRINT "             VALOR POR DEPARTAMENTOS :";           !
    PRINT USING FORMA$ ; ESPERADO% + TOTDES%                  !
    PRINT USING FORMA$ ; ESPERADO%                            !
    PRINT USING FORMA$ ; TOTDES%                              !
    PRINT "          VALOR POR DOMICILIOS    :";              !
    PRINT USING FORMA$ ; Asc.Vta.Domic%                       !    
    
!     TT.VALDEP+(-DESC4+SA.AMTDISCO-      \!
!               TO.DISVOID-VAL(TOTAC$))+TOTRED%-RETEFU        !
    PRINT "                                              "    !
    PRINT "         VALOR ZETA O MEDIOS DE PAGO :";           !
    PRINT USING FORMA$ ; TT.CONTEO                            !
    PRINT " "                                                 !
    PRINT "   ********************************************************  " !
    PRINT "   *   Revise los valores de los medios de pago           *  " !
    PRINT "   *   y las partidas por departamentos porque es         *  " !
    PRINT "   *   muy posible que haya un error, comuniquese con     *  " !
    PRINT "   *   el técnico de APOYO        !LO MAS PRONTO POSIBLE..*  " !
    PRINT "   *                                                      *  " !
    PRINT "   *   Y POR FAVOR NO ENVIE EL ARCHIVO NI LA PLANILLA     *  " !
    PRINT "   *   GRACIAS....................                        *  " !
    PRINT "   *                                                      *  " !
    PRINT "   *          ESTE ES UN MENSAJE INFORMATIVO.....!        *  " !
    PRINT "   ********************************************************  " !
LPRINTER
ENDIF
END SUB                                                       ! 

Sub ACUMULA                                                 !
      IF LEFT$(TIPPAG$,1) = "1" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NEF% = NEF% + 1          :  \
            EF% = EF% + VAL(VALPAG)     \
               ELSE                     \
            EF% = EF% - VAL(VALPAG) 
      ENDIF

      IF TIPPAG$ = "16" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NEF6% = NEF6% + 1          :  \
            EF6% = EF6% + VAL(VALPAG)     \
               ELSE                     \
            EF6% = EF6% - VAL(VALPAG) 
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

      IF TIPPAG$ = "43" THEN BEGIN
         IF SIGNO$ = "+" THEN             \
            NTDC% = NTDC% + 1          :  \
            TDC% = TDC% + VAL(VALPAG)     \
               ELSE                       \
            TDC% = TDC% - VAL(VALPAG) 
      EndIf
      
!--- Add OVS 10 Jul 2006      
      If TIPPAG$ = "44" Then Begin
         If SIGNO$ = "+" Then             \
            NTFN% = NTFN% + 1          :  \
            Vta.Fonede% = Vta.Fonede% + VAL(VALPAG)     \
               Else                       \
            Vta.Fonede% = Vta.Fonede% - VAL(VALPAG) 
      EndIf      

      If TIPPAG$ = "45" Then Begin			  ! Control de Convenios 
         If SIGNO$ = "+" Then             \
            NTFCNV% = NTFCNV% + 1      :  \
            Vta.Convenios% = Vta.Convenios% + VAL(VALPAG)     \
               Else                       \
            Vta.Convenios% = Vta.Convenios% - VAL(VALPAG) 
      EndIf      

      If TIPPAG$ = "46" Then Begin			  ! Control de Convenios 
         If SIGNO$ = "+" Then             \
            TT.LOAN% = TT.LOAN% + 1      :  \
            TT.PICKUP% = TT.PICKUP% + VAL(VALPAG)     \
               Else                       \
            TT.PICKUP% = TT.PICKUP% - VAL(VALPAG)     \
      EndIf      

      If TIPPAG$ = "56" Then Begin			  ! Registro FinAmerica
         If SIGNO$ = "+" Then             \
            NTFFAM% = NTFFAM% + 1      :  \
            Vta.FinAmerica% = Vta.FinAmerica% + VAL(VALPAG)     \
               Else                       \
            Vta.FinAmerica% = Vta.FinAmerica% - VAL(VALPAG) 
      EndIf      

      If TIPPAG$ = "55" Then Begin			  ! Registro FinAmerica
         If SIGNO$ = "+" Then             \
            NTTIC% = NTTIC% + 1      :  \
            Vta.TComf% = Vta.TComf% + VAL(VALPAG)     \
               Else                       \
            Vta.TComf% = Vta.TComf% - VAL(VALPAG) 
      EndIf      

      IF LEFT$(TIPPAG$,1) = "5" THEN Begin  ! Total Tarjetas Debito
         IF SIGNO$ = "+" THEN           \
            NTC% = NTC% + 1           : \
            TC% = TC% + VAL(VALPAG)     \
               ELSE                     \
            TC% = TC% - VAL(VALPAG) 
      EndIf

      IF LEFT$(TIPPAG$,1) = "6" THEN BEGIN
         IF SIGNO$ = "+" THEN           \
            NCR% = NCR% + 1           : \
            CR% = CR% + VAL(VALPAG)     \
               ELSE                     \
            CR% = CR% - VAL(VALPAG) 
      EndIf

      IF TIPPAG$ = "23" THEN BEGIN                            ! Si es Ch Subsidio
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NSU% = NSU% + 1          :  \                     ! Acumula N.Cheques
            CS% = CS% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            CS% = CS% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF                                                   ! Fin de Condic

      IF TIPPAG$ = "26" THEN BEGIN                            ! Si es Ch Subsidio
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NFO% = NFO% + 1          :  \                     ! Acumula N.Cheques
            FO% = FO% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            FO% = FO% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF                                                   ! Fin de Condic

      IF TIPPAG$ = "24" THEN BEGIN                            ! Si es Ch CONC.  
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NCC% = NCC% + 1          :  \                     ! Acumula N.Cheques
            CC% = CC% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            CC% = CC% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF 
      IF TIPPAG$ = "25" THEN BEGIN                            ! Si es Ch CONC.  
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NNC% = NNC% + 1          :  \                     ! Acumula N.Cheques
            NC% = NC% + VAL(VALPAG)     \                     ! Acum Vr Che.COMFANDI
               ELSE                     \                     ! de lo contrario
            NC% = NC% - VAL(VALPAG)                           ! Resta Vr Ch COMFANDI
      ENDIF                                                   ! Fin de Condic
      
      IF TIPPAG$ = "35" THEN BEGIN                            ! Si es Ch CONC.  
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NVF% = NVF% + 1          :  \                     ! Acumula N.Cheques
            VF% = VF% + VAL(VALPAG)     \                     ! Acum Vr Che Subsidio
               ELSE                     \                     ! de lo contrario
            VF% = VF% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF 
      IF TIPPAG$ = "34" THEN BEGIN                            ! Si es bono aceites
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NAC% = NAC% + 1          :  \                     ! Acumula bono
            AC% = AC% + VAL(VALPAG)     \                     ! Acum Vr bono 
               ELSE                     \                     ! de lo contrario
            AC% = AC% - VAL(VALPAG)                           ! Resta Vr Ch Subsidio
      ENDIF                                                   ! Fin de Condic
      IF TIPPAG$ = "54" THEN BEGIN                            ! Si es bono CFBUGA
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            NTD% = NTD% + 1          :  \                     ! Acumula bono
            TD% = TD% + VAL(VALPAG)     \                     ! Acum Vr bono 
               ELSE                     \                     ! de lo contrario
            TD% = TD% - VAL(VALPAG)                           ! Resta Vr BOCOMBUGA
      ENDIF                                                   ! Fin de Condic
      
      IF TIPPAG$ = "53" THEN BEGIN                            ! Si es TARJETA COMFANDI
         IF SIGNO$ = "+" THEN           \                     ! con signo +
            BCB% = BCB% + 1          :  \                     ! Acumula bono
            CB% = CB% + VAL(VALPAG)     \                     ! Acum Vr bono 
               ELSE                     \                     ! de lo contrario
            CB% = CB% - VAL(VALPAG)                           ! Resta Vr BOCOMBUGA
      ENDIF                                                   ! Fin de Condic
      NUMCS$ = RIGHT$("0000000000" + STR$(NSU%) + "+", 10)
      TOTCS$ = RIGHT$("0000000000" + STR$(CS%) + "+", 10)
      IF CS% < 0 THEN                                \
         TOTCS$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CS%),LEN(STR$(CS%))-1),9) + "-"

      NUMFO$ = RIGHT$("0000000000" + STR$(NFO%) + "+", 10)
      TOTFO$ = RIGHT$("0000000000" + STR$(FO%) + "+", 10)
      IF FO% < 0 THEN                                \
         TOTFO$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(FO%),LEN(STR$(CC%))-1),9) + "-"

      NUMVF$ = RIGHT$("0000000000" + STR$(NVF%) + "+", 10)
      TOTVF$ = RIGHT$("0000000000" + STR$(VF%) + "+", 10)
      IF CC% < 0 THEN                                \
         TOTVF$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CC%),LEN(STR$(CC%))-1),9) + "-"

      NUMNC$ = RIGHT$("0000000000" + STR$(NNC%) + "+", 10)
      TOTNC$ = RIGHT$("0000000000" + STR$(NC%) + "+", 10)
      IF NC% < 0 THEN                                \
         TOTNC$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(NC%),LEN(STR$(NC%))-1),9) + "-"

      NUMCB$ = RIGHT$("0000000000" + STR$(BCB%) + "+", 10)
      TOTCB$ = RIGHT$("0000000000" + STR$(CB%) + "+", 10)
      IF CB% < 0 THEN                                \
         TOTCB$ = RIGHT$("0000000000" +              \
         RIGHT$(STR$(CB%),LEN(STR$(CB%))-1),9) + "-"

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
End Sub 

! -------------  Rutina para crear compaginar Hora y Fecha --------------

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
      SIGLO$   = "07"
      FHOY$    = TM$(MES)+" "+STR$(VAL(FRA.DD$))             \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                           !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +           \! Arma la Linea
               "    de "+FHOY$                                ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                           ! Salva Mes y Dia 
  END Sub

!------------------------------------------------------------------------------
!           Subrutina para Tomar IVAS, Deptos y Terminales
!------------------------------------------------------------------------------
SUB TOMA.IVAS                                                 !
    DIM IVAD%(999,6)                                          !
    DIM IVAT%(100,7)                                          !
    OPEN "C:\ADX_IDT1\EAMITEMR.DAT" KEYED RECL 77 AS 22       !
    FOR II% = 1 TO 999                                        !
        KEY$ = PACK$("000099999" + RIGHT$("000"+STR$(II%),3)) !
        EOF$ = "  "                                           !
        READ FORM "C7 2I1 C15 C18 C35";#22 KEY KEY$;          \!
                   KEY$,IND1,IND1A,DATO4$,RESP$,DATO4$        !
        IF EOF$ = "  " THEN BEGIN                             !
           IVGR$ = " 0"                                       !
           IF IND1  AND 80H THEN IVGR$ = "16"                 !
           IF IND1  AND 40H THEN IVGR$ = "07"                 !
           IF IND1  AND 20H THEN IVGR$ = "35"                 !
           IF IND1  AND 10H THEN IVGR$ = "10"                 !
           IF IND1A AND 01H THEN IND1A = 1 ELSE IND1A = 0     !
           IF IND1A = 1 THEN CADE% = CADE% + 1                !
           IVAD%(II%,0) = VAL(IVGR$)                          !
           IVAD%(II%,6) = IND1A                               !
           IVGR$ = IVGR$ + "%"                                !
           DEPA$ = DEPA$ + "DP"+RIGHT$("000"+STR$(II%),3)+   \!
                   "  "  + RESP$ + " " + IVGR$                !
        ENDIF                                                 !
    NEXT II%                                                  !
    IF CADE% = 0 THEN BEGIN                                   !
       CLEARS                                                 !
       LOCATE 10,15                                           !
       PRINT "A L E R T A : En ‚ste Sistema NO se ha Definido"!
       LOCATE 11,35                                           !
       PRINT "ESQUEMA DE descuentos..!"                       !
       LOCATE 14,25                                           !
       PRINT "El Proceso se   S U S P E N D E..!"             !
       STOP                                                   !
    ENDIF                                                     !
    FOR II% = 1 TO 050                                        !
        KEY$ = PACK$("000098989" + RIGHT$("000"+STR$(II%),3)) !
        EOF$ = "  "                                           !
        READ FORM "C7 I1 C16 C18 C35";#22 KEY KEY$;           \!
                   KEY$,IND1,DATO4$,RESP$,DATO4$              !
        IF EOF$ = "  " THEN BEGIN                             !
           TPVA$ = TPVA$ + "TP" + RIGHT$("000"+STR$(II%),3)+ \!
                   "  " + RESP$                               !
        ENDIF                                                 !
    NEXT II%                                                  !
END SUB                                                       !
!------------------------------------------------------------------------------
!             Liquidacion de Bases, Ivas e ImpoConsumo por Transac
!------------------------------------------------------------------------------
SUB LIQUIVA.MED                                               !
  DETRA1$ = DETRA$                                            !
  WHILE LEN(DETRA1$) > 0                                      !
     DEPI%      = VAL(MID$(DETRA1$,3,3))                      !
     DETRA1$    = RIGHT$(DETRA1$,LEN(DETRA1$)-5)              !
     FOR TFI%   = 0 TO 4                                      !
         BASE%  = IV.TF%(DEPI%,TFI%,0) -                     \! Calc Base sin
                  IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         IV     = IVAD%(DEPI%,0)                              ! Tarifa IVA/DEP
         IV     = 1 + FLOAT(IV)/100                           ! Calc Fctr Iva
         BASE%  = ROUND(BASE% / IV, 0, 0)                     ! Deflc Iva Base
         ICO%   = IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         CAN%   = IV.TF%(DEPI%,TFI%,2)                        ! Cantidades
         IVATR% = IV.TF%(DEPI%,TFI%,0) - ICO% - BASE%         ! Calc IVA
         IF IV < 1.01 THEN BEGIN                              ! Ventas Exentas
            IVATRA%(0) = IVATRA%(0) + BASE%                   ! Suma Base Ex-Transac
         ENDIF ELSE BEGIN                                     !
           IF IV = 1.16 THEN IVATRA%(1) = IVATRA%(1) + BASE%
           IF IV = 1.07 THEN IVATRA%(2) = IVATRA%(2) + BASE% 
           IF IV = 1.35 THEN IVATRA%(3) = IVATRA%(3) + BASE%
           IF IV = 1.10 THEN IVATRA%(4) = IVATRA%(4) + BASE% 
            IVATRA%(5) = IVATRA%(5) + IVATR%                  ! Suma Ivas-Transac
         ENDIF                                                !
         IVATRA%(6) = IVATRA%(6) + ICO%                       ! Suma ImpCon-Transac
      NEXT TFI%                                               !
WEND                                                        !

END SUB                                                       !

!------------------------------------------------------------------------------
!             Liquidacion de Bases, Ivas e ImpoConsumo por Transac
!------------------------------------------------------------------------------
SUB LIQUIVA                                                   !
  SILIQ% = 0                                                  !
  WHILE LEN(DETRA$) > 0                                       !
     BLA$ = "              "                                  !
     DEPI%      = VAL(MID$(DETRA$,3,3))                       !
     DETRA$     = RIGHT$(DETRA$,LEN(DETRA$)-5)                !
     FOR TFI%   = 0 TO 4                                      !
         BASE%  = IV.TF%(DEPI%,TFI%,0) -                     \! Calc Base sin
                  IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         IV     = IVAD%(DEPI%,0)                              ! Tarifa IVA/DEP
         IV     = 1 + FLOAT(IV)/100                           ! Calc Fctr Iva
         BASE%  = ROUND(BASE% / IV, 0, 0)                     ! Deflc Iva Base
         ICO%   = IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         CAN%   = IV.TF%(DEPI%,TFI%,2)                        ! Cantidades
         IVATR% = IV.TF%(DEPI%,TFI%,0) - ICO% - BASE%         ! Calc IVA
         IVAD%(DEPI%,1) = IVAD%(DEPI%,1) + BASE%              !
         IVAD%(DEPI%,2) = IVAD%(DEPI%,2) + IVATR%             !
         IVAD%(DEPI%,3) = IVAD%(DEPI%,3) + ICO%               !
         IVAD%(DEPI%,5) = IVAD%(DEPI%,5) + CAN%               !
         IF IV = 1 THEN BEGIN                                 ! Ventas Exentas
            IVATRA%(0) = IVATRA%(0) + BASE%                   ! Suma Base Ex-Transac
            IVAT%(NT%,5) = IVAT%(NT%,5)+BASE%                 ! suma solo en Tot/Terminal
            IVAT%(NT%,4) = IVAT%(NT%,4)+IVATR%                ! Suma Ivas Terminal
         ENDIF ELSE BEGIN                                     !
            IVATRA%(TFI%) = IVATRA%(TFI%) + BASE%             ! Suma Base Iva-Transac
            IVAT%(NT%,3) = IVAT%(NT%,3)+BASE%                 ! suma solo en Tot/Terminal
            IVAT%(NT%,4) = IVAT%(NT%,4)+IVATR%                ! Suma Ivas Terminal
            IVATRA%(5) = IVATRA%(5) + IVATR%                  ! Suma Ivas-Transac
         ENDIF                                                !
         IVAT%(NT%,6) = IVAT%(NT%,6)+ICO%                     ! Suma ImpoC Terminal
         IVATRA%(6) = IVATRA%(6) + ICO%                       ! Suma ImpCon-Transac
      NEXT TFI%                                               !
  WEND                                                        !
  DIM IV.TF%(999,4,2)                                         !
END SUB                                                       !

SUB GRABA.OTROIVA                                             !
    CALL LIQUIVA.MED                                          !
    SG$ = "+"                                                 !
    REGMED9$ = LEFT$(REGMED$,74)                              !
    EXEIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(0))),9)   !
    PLAIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(1))),9)   !
    PLBIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(2))),9)   !
    PLCIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(3))),9)   !
    PLDIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(4))),9)   !
    GRTIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(5))),9)   ! 
    ICOIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(6))),9)   ! 
    FOR FI% = 0 TO 6                                          !
        TOTTRAN% = IVATRA%(FI%) + TOTTRAN%                    !
        IF TOTTRAN% < 0 THEN SG$ = "-"                        !
    NEXT FI%                                                  !
    TOTTRAN$ = RIGHT$("000000000" + STR$(ABS(TOTTRAN%)),9)    ! 
    IF TOTTRAN%=0 AND MID$(REGMED9$,21,1)="-" Then SG$ = "-"  !
    DESCUE$ = STR$(SA.AMTDISCO*-1)                               !
    DESCUE$ =RIGHT$(STRING$(6,"0")+DESCUE$,6)                 !
    FISCO$ = RIGHT$(STRING$(15,"0")+FISCO$,15)                ! Oct. 21 de 2002
    FISCO$ = PRENUF$                                          !
    If Gr.Ind.Rem% = -1 Then Begin 
    	 FISCO$ = String$(15,"X")
    EndIf 
    REGMED9$=REGMED9$+EXEIVA$+SG$+"16"+PLAIVA$     +         \!
             SG$+"07"+PLBIVA$+SG$+"35"+PLCIVA$     +         \!
             SG$+"10"+PLDIVA$+SG$+GRTIVA$+SG$      +         \!
             ICOIVA$+SG$+TOTTRAN$+SG$                         !
    WRITE FORM "C162 C15 C6 C2";#FIL9,POSI%;                 \! 
               REGMED9$,FISCO$,DESCUE$,FR$                    !
    POSI% = POSI% + 1                                         !
    DESCUE$ = ""
    SA.AMTDISCO = 0
    DIM IVATRA%(6)                                            !
    TOTTRAN% = 0                                              !
    CREDI% = 0 
END SUB                                                       !
!------------------------------------------------------------------------------
!                 Acumulados de IVA e Impuesto al Consumo
!------------------------------------------------------------------------------

SUB ACU.IVAICO                                   ! 
  EOF$ = "  "                                    ! Inicia Flag de Error
  LLAVE$ = RIGHT$("000000000000" + COPLU$,12)    ! Arma llave
  LLAVE$ = PACK$(LLAVE$)                         ! y la empaqueta
  TFI$   = RIGHT$("00"+STR$(IVAD%(DEPI%,0)),2)   ! Convierte a Str long 2 
  TFI%   = MATCH(TARIF$,TFI$,1)                  ! Busca Posic Tarifa IVA
  TFI%   = TFI% / 3 + 1                          ! Calcula Ocorrencia
  ICON%  = 0                                     ! ImpoConsumo Default
  
  READ FORM "C9 C1 C2 C4 C1 C5 C20 2I2 C31";#22 \! Lee EAMITEMR
       KEY LLAVE$;REGI$,IND2$,SEC$,FAGR$,       \! para localizar
           CONT$,ALI$,LINO$,ICON%,FECR%,Gdata$   ! Impuesto al Consumo

  IF EOF$ = "  " THEN BEGIN                      ! Si hay error Lectura
     IF RIGHT$(UNPACK$(IND2$),1)="5" THEN BEGIN  ! Si em Metodo/Precio 5
        SECCI$ = SEC$                            ! toma depto y
        LLAVE$ = PACK$("00")+ALI$                ! extrae Llave ALIAS
        READ FORM "C9 C1 C2 C4 C1 C5 C20 2I2 C31";  \! Lee nuevemente 
             #22 KEY LLAVE$;REGI$,IND2$,        \! EAMITEMR
                 SEC$,FAGR$,CONT$,ALI$,         \!
                 LINO$,ICON%,FECR%,Gdata$        ! para comparar Deptos
     ENDIF                                       !
  ENDIF                                          !
  Gtmp$ = Str$(GETN4(Gdata$,16))                 ! Valor del impoconsumo
  icon% = Val(Gtmp$)
  
  IF CAN% = 0 THEN CAN% = 1                      !
  ICO%  = ICON% * CAN% * SGN%                    ! Calcula Imp Consumo
  IV.TF%(DEPI%,TFI%,0) = IV.TF%(DEPI%,TFI%,0)+  \!
                         BB.PRECIO               ! Acumula Vta/PLU
  IV.TF%(DEPI%,TFI%,1) = IV.TF%(DEPI%,TFI%,1)+  \!
                         ICO%                    ! Acumula ImpCon/PLU
  IV.TF%(DEPI%,TFI%,2) = IV.TF%(DEPI%,TFI%,2)+  \!
                         CAN%                    ! Acumula Cantidad
End SUB                                          !
!------------------------------------------------------------------------------
!----                              PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------------------------
   FMTO1$="C74 PIC(-9999999) "              +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) 4PIC(-9999999) C2"                !
   OCH$   = CHR$(27)+CHR$(48)                         ! Impr a 1/8's 1B30
   CND$   = CHR$(27)+CHR$(15)                         ! Condensado 1B0F
   S8CND$ = CHR$(12)+OCH$+CND$                        ! Salto;1/8's;Condens
   SEX$   = CHR$(27)+CHR$(50)                         ! Impr a 1/6's 1B32
   DIS$   = CHR$(27)+CHR$(80)                         ! 10 CPI(NO Cond) 1B50
   S6NCN$ = SEX$+DIS$                                 ! Salto;1/6's;NO-Condens
   DIM TRF%(999,2)                                    !
   DIM IVATRA%(6)                                     !
   DIM IV.TF%(999,4,2)                                !
   DIM TPG%(6,6,1)                                    !
   DIM TI$(6,6)                                       !
   DIM TM$(12)                                        !
   CIENP = 100.0                                      !
   POSI% = 3
   FECHA$  = DATE$                                    ! Toma Fecha 
   MESPRO  = VAL(MID$(FECHA$,3,2))                    ! Mes
   DIAPRO$ = RIGHT$(FECHA$,2)                         ! Dia
   ANOPRO$ = LEFT$(FECHA$,2)                          !
   FINR$   = CHR$(13) + CHR$(10)                      ! Caracter "0D0A"
   FR$     = CHR$(13) + CHR$(10)                      ! Fin de Reg 0D0A
!----------------------   Llena arreglo de meses   ---------------------------
   TM$(1)="ENERO"     :  TM$(2)="FEBRERO"    :  TM$(3)="MARZO"
   TM$(4)="ABRIL"     :  TM$(5)="MAYO"       :  TM$(6)="JUNIO"
   TM$(7)="JULIO"     :  TM$(8)="AGOSTO"     :  TM$(9)="SEPTIEMBRE"
   TM$(10)="OCTUBRE"  :  TM$(11)="NOVIEMBRE" :  TM$(12)="DICIEMBRE"

   TI$(1,0)="EFECTIVO"  :  TI$(2,0)="CHEQUES"    :  TI$(3,0)="OTROS PAG/(MFac)"
   TI$(4,0)="RED MULTIC":  TI$(5,0)="TARJETAS CR":  TI$(6,0)="CREDITO COMFANDI"

   TI$(1,1)="Efectivo"              :  TI$(1,6)="Ajuste Redondeo"  
   TI$(2,1)="Bancos Locales"        :  TI$(2,2)="Con Tarj/Comfandi"
   TI$(2,3)="De Subsidio"           :  TI$(2,4)="Ch Concesionario"
   TI$(2,5)="Ch Comfandi"
   TI$(3,1)="Bonos Prepagados"      :  TI$(3,2)="Bonos Fabricante"  
   TI$(3,3)="Bonos Comfandi"        :  TI$(3,4)="Bonos Afiliados "  
                                    :  TI$(3,6)="CR Comfandi(mf)"  
   TI$(4,1)="Pagos Tarjetas/DB"     :  TI$(4,2)="Avances en Efectivo"
   TI$(5,1)="Credibanco"            :  TI$(5,2)="Credencial" 
   TI$(5,3)="Tarjeta comfandi"      :  TI$(5,4)="Diners" 
   TI$(5,6)="Bonos Comfabuga     "  :                                
   TI$(6,1)="Medicamentos Empresa"  :  TI$(6,2)="Temporada Escolar"
   TI$(6,3)="Credito Interno"       :  TI$(6,4)="Credito Especial"
   TI$(6,5)="Credito CVC 60%"       :  TI$(6,6)="CR sin Factura"
!----------------------------------------------------------------------------
ON ERROR GOTO ERRTN                                           !
TAMA% = VAL(COMMAND$)                                         ! Procesar cierto
IF TAMA% = 0 THEN TAMA% = 99999999                            ! tamano de LOG
CLEARS                                                        !
TITULO$ = "        ** CREACION DE LOS MEDIOS DE PAGO (MED.DAT) **"
LOCATE 12,20
PRINT "Toma Iva de PLU's Departamentales..."
CALL TOMA.IVAS
CALL CREAFECHA                                                !
FEC1$ = FRA.LIN$                                              !
CONLI% = 60                                                   !  
EMPEZAR:                                                      !
AR0$ = "EAMCSCF1"                                             ! Arch Cotrl Alm
AR1$ = "C:\ADX_UDT1\MED.DAT"                                  ! Medios de Pago
AR2$ = "C:\ADX_UDT1\PAGCOM.DAT"                               ! Pagos Compbte
AR3$ = "C:\ADX_UDT1\CHQPOS.DAT"                               ! Cheques Postfec
AR4$ = "C:\ADX_UDT1\PAGO.DAT"                                 ! Pagos Hoy
AR5$ = "C:\ADX_UDT1\Z"                                        ! Comprob Diario
AR6$ = "C:\ADX_UDT1\PG.DAT"                                   ! Pagares
AR7$ = "C:\VEND1.DAT"                                         ! Vendedores
AR8$ = "C:\ADX_UDT1\R"                                        ! Rebajas
AR9$ = "C:\ADX_UDT1\IVACRED.DAT"                              ! Medios de Pago
AR10$= "C:\C"                                                 ! Medios de Pago
AR11$= "C:\ADX_UDT1\F"                                        ! VTA EMPRESA
AR12$= "C:\ADX_UDT1\G"                                        ! GASES OCCIDE
!AR13$= "C:\ADX_UDT1\E"                                        ! EDUCACION

AR14$= "C:\ADX_UDT1\A"                                        ! BONOS SUBSIDO
!Open AR0$ KEYED RECL 36 AS 1 UNLOCKED NOWRITE NODEL           ! Arch. Control

Open AR0$ KEYED RECL 36 AS 1 NOWRITE NODEL           ! Arch. Control

KEYCF1$=PACK$("9998")                                         ! Reg. de Cierre
READ FORM "C36";#1 KEY KEYCF1$;REGCF1$                        ! Lectura
B$="C:\ADX_IDT4\" + MID$(REGCF1$,11,8) + ".DAT"               ! TSLOG Previo
IF MID$(REGCF1$,11,8)="        " THEN B$="EAMTRANA"           ! TSLOG 1st Vez
CLOSE 1                                                       ! Cierra EAMCSCF1
OPEN "C:\ADXALMA.DAT" AS 24 BUFFSIZE 80                       ! Arch Ctrl Alm
READ FORM "C80";#24;AL$                                       ! Lee Ctrl Alm
WHILE TIP$ <> "5"                                             ! Planes Iva
      READ FORM "C80";#24;ALL$                                ! Lee Cotrl Alm
      TIP$ = MID$(ALL$,78,1)                                  !
      IF TIP$ = "5" THEN TARIF$ = "00 "+ LEFT$(ALL$,12)       !
WEND                                                          !
CLOSE 24                                                      ! Lo Cierra 
AL$     = AL$ + STRING$(20," ")                               ! Ajusta Reg Ctrl
RESP$   = MID$(AL$,5,8)                                       ! Toma Fecha Control
NUAL$   = STR$(VAL(LEFT$(AL$, 4)))                            ! Guarda Num Almacen
CCOSTO$ = "0"+NUAL$                                           ! Guarda Centro Costo
NOAL$   = MID$(AL$, 13, 30)                                   ! Guarda Nombre
ALM$    = NUAL$                                               ! 
RESP$   = RESP$+"0"+NUAL$                                     ! Arma Reg Ctrl 
ANOT% = VAL(LEFT$(RESP$,4))                                   !
MEST% = VAL(MID$(RESP$,5,2))                                  !
DIAT% = VAL(MID$(RESP$,7,2))                                  !
LETMES$ = "EFMAYJLGSOND"                                      ! Equival Meses
LM$     = MID$(LETMES$,VAL(MID$(RESP$,5,2)),1)                !
AR10$   = AR10$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
AR8$    = AR8$  + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
!AR5$    = AR5$  + NUAL$ + LM$+MID$(RESP$,7,2) + ".DAT"        !
AR5$    = AR5$  + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
AR11$   = AR11$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
AR12$   = AR12$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
!AR13$   = AR13$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !

AR14$   = AR14$ + NUAL$ + MID$(RESP$,5,4) + ".DAT"            !
IF FRA.AA$ > MID$(AL$,7,2) THEN BEGIN                         ! Informa Cambio de A¤o
    CLEARS                                                    !
    PRINT : PRINT : PRINT : PRINT : PRINT                     !
    PRINT "        Registramos Complacidos un "               !
    PRINT                                                     !
    PRINT "        **** N U E V O   A ¥ O ***"                !
    PRINT                                                     !
    PRINT "   Las  CONVERSIONES  de las ventas diarias"
    PRINT "   a  partir  de  la Fecha, quedar n con el"
    PRINT "   NUEVO a¤o. Hoy se MODIFICA  este dato en"
    PRINT "   forma AUTOMATICA en C:\ADXALMA.DAT."            !
    PRINT                                                     !
    PRINT "   Este es un mensaje INFORMATIVO..! "             ! 
    PRINT                                                     !
    INPUT "   ..Feliz A¤o Nuevo..!    Una vez m s con.... INTRO.." ; CONTRO$
    PRINT                                                     !
ENDIF                                                         !
CLEARS                                                        !
PRINT  :   PRINT  :  PRINT  :   PRINT                         !
PRINT TITULO$                                                 !
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT "    Proceso del Almac‚n No. ";ALM$;"  ";NOAL$          !
PRINT "    Inicia a las: ";FEC1$                              ! Indica comienzo
PRINT                                                         ! Avanza Linea
PRINT                                                         ! Avanza Linea
PRINT "    *(";STR$(CADE%);")    Se est  procesando   : ";B$  ! Archivo en Proceso
PRINT "    Se ACTUALIZARAN Archivos de   :  "                 !
PRINT "                 MEDIOS DE PAGO   : ";AR1$             !
PRINT "               RESUMEN CONTABLE   : ";AR10$            !
PRINT "             CUOTAS MODERADORAS   : ";AR9$             !
PRINT "             VENTAS POR EMPRESA   : ";AR11$            !
PRINT "           REBAJAS Y DESCUENTOS   : ";AR8$             !
PRINT "     COMPROBANTE INFORME DIARIO   : ";AR5$             !
PRINT "   "                                                   ! Avanza Linea
PRINT "                        Por Favor Esperar..."          !
PRINT "                         "                             !

TLOG = 25  :  FIL1 = 26    :     FIL2 = 27                    ! N£meros de
FIL3 = 28  :  FIL4 = 29    :     FIL5 = 30                    ! Sesi¢n Usados
FIL6 = 31  :  FIL7 = 32    :     FIL8 = 33                    ! por el Programa
FIL9 = 34  :  FIL10 = 10   :     FIL11 = 35                   !
FIL12 = 36 :  FIL13 = 37   :     FIL14 = 38                   !  
CREATE AR1$  DIRECT RECL 76  AS FIL1 BUFFSIZE 7680            ! Creaci¢n de
CREATE AR10$ DIRECT RECL 78  AS FIL10 BUFFSIZE 1780           !
CREATE AR9$  DIRECT RECL 185 AS FIL9 BUFFSIZE 5120            !   
CREATE AR2$  AS FIL2 BUFFSIZE 2560                            ! Archivos en
CREATE AR3$  AS FIL3 BUFFSIZE 2560                            ! ADX_UDT1
CREATE AR4$  AS FIL4 BUFFSIZE 2560                            ! 
CREATE AR5$  AS FIL5 BUFFSIZE 2560                            !
CREATE AR6$  AS FIL6 BUFFSIZE 2560                            ! 
CREATE AR7$  AS FIL7 BUFFSIZE 1780                            !
CREATE AR8$  AS FIL8 BUFFSIZE 1780                            !
CREATE AR11$ AS FIL11 BUFFSIZE 1780                           !
CREATE AR12$ AS FIL12 BUFFSIZE 1780                           !
!CREATE AR13$ AS FIL13 BUFFSIZE 1780                           !

CREATE AR14$ AS FIL14 BUFFSIZE 1780                           !
TIREG$  =  "1"                                                !
NITEM$  =  STRING$(14,"0")                                    !
OPEN B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL                   ! 
B$ = ""                                                       !
WRITE FORM " C1 C4 C8 C2";#FIL11; "1",CCOSTO$,RESP$,FR$       !
WRITE FORM " C1 C4 C8 C2";#FIL12; "1",CCOSTO$,RESP$,FR$       !
!Write FORM " C1 C4 C8 C2";#FIL13; "1",CCOSTO$,RESP$,FR$       !

WRITE FORM " C1 C4 C8 C2";#FIL14; "1",CCOSTO$,RESP$,FR$       !
                                                              !
NXTRCD:                                                       !
    DATOFOR$ = ("00000000000000000000") 
    FORMULA$ = ("00000000")
    DIAGN$ = ("0000")
    MEDICO$ = ("00000000")
    !EDUCA$ = "00000000000"
DIM    BB.AR.KEYTV$(100),   BB.AR.PRECIO(100),               \!
       BB.AR.COMPR(100),    BB.AR.FINAL(100),                \!
       BB.AR.AGENC(100),    NETVEN$(100),    CODART$(500),   \!
       UNIVTA$(500),        MTOVTA$(500),   NITCLI$(500),    \!
       DEPART$(500),        SGNVTA$(500)
       
PTRART = 0
Q    = 1                                                      !
FT   = 0                                                      !
FTV  = 0                                                      !
FLAG = 0                                                      !
MTOTRAN = 0                                                   !
TIPPAG$="11"                                                  !
BB.FLAG = 0                                                   !
IF SILIQ% = 1 THEN CALL LIQUIVA                               !
READ #TLOG; LINE INAREA$                                      !
YY% = YY% + LEN(INAREA$) + 2                                  !
IF YY% > TAMA% THEN CALL ACUDIA
LOCATE 11,20                                                  !
PRINT "Offset del Log : ";YY%                                 !
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
  IF A = 0 THEN GOSUB S0 : GOTO AGAIN                         !
  IF (A < 0) OR (A > 11 AND A < 99) THEN GOTO AGAIN           !

  IF A  =  1  THEN GOSUB S1                                   ! Trans item entry  
  IF A  =  2  THEN GOSUB S2                                   ! Trans entry ext
  IF A  =  3  THEN GOSUB S3                                   ! Descu
  IF A  =  4  THEN GOSUB S4                                   ! Anul Desc
  IF A  =  5  THEN GOSUB S5                                   ! Medios de pago
  IF A  =  6  THEN GOSUB S6                                   ! Corr medios pago
  IF A  =  9  THEN GOSUB S9                                   ! Camb food stamp
  IF A  =  11 THEN GOSUB S11                                  ! Entrada datos
  IF A  =  99 THEN GOSUB S99                                  ! String 99
AGAIN:                                                        !
WEND                                                          !
IF FT = 1 AND FLAG = 0 THEN BEGIN                             !
  IF VALPAG <> "0000000000" THEN BEGIN                        !
    NREC% = VAL(CONSEC$) + 2                                  !
    CALL ACUMULA                                              !
!   PRINT "FCTA GR : ";CHEQUE$,NOCUE$
    REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+          \!
              CHEQUE$+CTACTE$+NUT+NTR+                       \! TPV/TRANS
              NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$               !
    IF LEN(REGMED$) = 76 THEN BEGIN                           !
       WRITE FORM "C76";#FIL1,NREC%;REGMED$                   !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !PRINT TOTCRE$
    ENDIF ELSE BEGIN                                          !
       PRINT "ER ",TIREG$," ",CONSEC$                         !
    ENDIF                                                     !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                             !
!------------------------------------------------------------------------------

  PORCEN = 0                                                  !
  IF MTOTRAN <> 0 THEN BEGIN                                  !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                   ! 
  ENDIF                                                       !

!  PRINT "#FACTURA: ";CHEQUE$,NOCUE$
    PORCEN = 0                                                !
    IF MTOTRAN <> 0 THEN BEGIN                                !
       PORCEN = VAL(VALPAG) / MTOTRAN * 100                   ! 
    ENDIF                                                     !
    IF TIPPAG$ = "43" THEN BEGIN                             \! Cheque PostFechado
       WRITE FORM "C7 C8 C2 C1 C7 C10 C7 C13 C19 C2";        \! Graba Reg Pago
       #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",                   \!
       "2","0000000",VALPAG,"0000000",                       \!
       "             ","0000000000000000000",FR$              !
    ENDIF                                                     !
  ENDIF                                                       !
ENDIF                                                         !
!------------------------------------------------------------------------------
!             Grabamos registros de pagos
!------------------------------------------------------------------------------
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
        WRITE FORM "C7 C8 C2 C2 C1 C7 C11 C12 C1 C1 C7 C2";#FIL2;      \
              RIGHT$(NOCUE$,7),FECTRA,"00","51","2",COMPRO,VALPAG,     \
              VALCHQ,FIN$,AGENC$,NUMPAG$,FR$   
        WRITE FORM "C7 C2 C8 C2 C2 C1 C7 C11 C12 C1 C1 C7 C2";#FIL4;   \
              RIGHT$(NOCUE$,7),TIPTRA1,FECTRA,"00","51","2",COMPRO,VALPAG,\
              VALCHQ,FIN$,AGENC$,NUMPAG$,FR$   
     ENDIF
  NEXT I
  IF FTV=1 THEN BEGIN
     FOR E=1 TO F
        WRITE FORM "C3 C10 C2";#FIL7;VEND$,NETVEN$(E),FR$   
     NEXT E
  ENDIF
  DIM      BB.AR.KEYTV$(0),  BB.AR.PRECIO(0),           \!
           BB.AR.COMPR(0),   BB.AR.FINAL(0),            \!
           CODART$(0),	     UNIVTA$(0),    MTOVTA$(0), \!
           NITCLI$(0),	     DEPART$(0),    SGNVTA$(0), \!
           NETVEN$(0)                                    ! Reset de arreglos
  D=0
  F=0
GOTO NXTRCD                                              !
WRITE FORM " C39 ";#FIL11;                              \!
"9999999999999999999999999999999999999999"               !

S0:                                                  !  Header de Transac
  CREDI% = 0
  Gr.Ind.Rem% = 0 
  J = 3                                              !
  VALCHQ="0"                                         !
  GOSUB GETUNPK                                      !
  NUT  =  RIGHT$(STRING$(3,"0")+A$,3)                ! N.Terminal
  NT% = VAL(NUT)                                     !
  GOSUB GETUNPK                                      !
  NTR =  RIGHT$(STRING$(4,"0")+A$,4)                 ! N.Transacc
  TR% = VAL(NTR)                                     !
  GOSUB GETUNPK                                      !
  FECTRA  =  MID$(A$,3,8)                            !
  COMPRO  =  "1"+MID$(FECTRA,5,2)+NTR                !
  HORATR$ =  MID$(A$,7,4)                            !
  GOSUB GETUNPK                                      !
  TIPTRA  =  RIGHT$(STRING$(1,"0")+A$,1)             ! Tipo Transac
  IF VAL(TIPTRA) = 0 OR VAL(TIPTRA) = 2 THEN BEGIN   ! Trans de Vta
     DIM IVATRA%(6)                                  !
     IF IVAT%(NT%,0) <= 0   THEN IVAT%(NT%,0) = TR%  ! Trans Inicial
     IF IVAT%(NT%,1) <  TR% THEN IVAT%(NT%,1) = TR%  ! Trans Final
     IVAT%(NT%,2) = IVAT%(NT%,2) + 1                 ! Incr No Clientes
     IF VAL(TIPTRA) = 0 THEN BEGIN                   ! Oct. 21 de 2002
        CALL STRING.FISCAL                           ! Ajuste para obtener
        IF STR99% > 0 THEN BEGIN                     ! datos String 99
           PRENUF.ANT$ = PRENUF$                     ! TR Fiscal
           PRENUF$ =  PRETR$ + NUFIS$                !
           IF PRENUF.ANT$="" THEN PRENUF.ANT$=PRENUF$!
        ENDIF                                        !
     ENDIF                                           !
  ENDIF                                              !
  TIPTRA1 =  "00"
  BANCO$  =  "00"
  CTACTE$ =  STRING$(14,"0")
  CEDULA$ =  CTACTE$
  CHEQUE$ =  STRING$(8,"0")
  FACTURA$ = CHEQUE$
  GOSUB GETUNPK
  GOSUB GETUNPK
  NUMOPE$  =  RIGHT$(STRING$(6,"0")+A$,6)
  GOSUB GETUNPK
  GOSUB GETUNPK
  GROSPOSS$  =  RIGHT$(STRING$(8,"0")+A$,8)
  GOSUB GETUNPK
  GROSNEGS$  =  RIGHT$(STRING$(8,"0")+A$,8)
  MONTOTR$   =  STR$(VAL(GROSPOSS$)-VAL(GROSNEGS$))
  GOSUB GETUNPK
  RETURN


S1:
  J = 3
  PTRART = PTRART + 1
  GoSub GETUNPK
  COPLU$ = A$                                           ! PLU para Liq IVA
  ANCHE$ = STRING$(12,"0")                              !
  CAN% = 0
  IF LEN(A$) = 12 THEN BEGIN                            ! Si PLU 12 Digitos
     ANCHE$ = LEFT$(A$,6)                               ! Prefijo Ancheta
     ANCME$ = MID$(A$,7,4)                              ! Ancheta o Mercado
     REBAJ$ = RIGHT$(A$,2)                              !
  ENDIF                                                 !
  COD$   = RIGHT$("000000" + A$,6)                      ! Cod Item para rebajas
  CODART$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)           ! 
  GOSUB GETUNPK                                         ! Toma PrecTotal
  MTOVTA$(PTRART)=RIGHT$(STRING$(10,"0")+A$,10)         !
  BB.PRECIO = VAL(A$)                                   ! Valor del pago
  NETOPO$=RIGHT$(STRING$(10,"0")+A$,10)                 ! Vr Item vendedores
  GOSUB GETUNPK                                         !
  DEP$=RIGHT$("000"+A$,3)                               ! Dep para rebajas
  MACH$ = "DP" + DEP$                                   !
  DEPI% = MATCH(MACH$,DETRA$,1)                         !
  IF DEPI% = 0 THEN                                    \!
     DETRA$ = DETRA$ + "DP" + DEP$                      !
  DEPART$(PTRART)=RIGHT$(STRING$(3,"0")+A$,3)           !
  IF DEP$ ="861" THEN BEGIN
      IF BB.PRECIO > 0 THEN SIGNO1$ = "+"
      IF BB.PRECIO < 0 THEN SIGNO1$ = "-"
      VALEDU$ = STR$(BB.PRECIO)
  !   PRINT DEP$+VALEDU$
  ENDIF
  UNIVTA$(PTRART)= "000001"                             !
  DEPI% = VAL(DEP$)                                     !
!  LOCATE 2,1 : PRINT "Depto:";DEPI%;"  "; : INPUT "  ";ZZZ$
  GOSUB GETUNPK                                         ! Toma Flags Fam
  GOSUB GETUNPK                                         ! Indic1 del Log
  GOSUB GETFLAG                                         ! Flags de Indic1
  PESA% = 0                                             !
  IF MID$(A$,2,1) = "1" THEN CAN%=1  :  PESA%=1         ! Si es Pesable
  GOSUB GETUNPK                                         ! Indic2 del Log
  GOSUB GETFLAG                                         ! Flags del Indic2
  SGNVTA$(PTRART) = "+"                                 !
  SGN% = 1                                              !
  VRUNPLU% = 0
  IF MID$(A$,15,1) = "1" AND                           \! Si hay Extension
     PESA% = 0 THEN BEGIN                               !
     PP = MATCH(",",INAREA$,Q)                          !
     BB$ = MID$(INAREA$,Q+1,(PP-Q)-2)                   !
     BB$ = BB$ + ":"                                    !
     JJ = 3                                             !
     GOSUB GETEXTE                                      ! Grupo Multiprec
     GOSUB GETEXTE                                      ! Items en Deal
     GOSUB GETEXTE                                      ! Metodo Precio
     GOSUB GETEXTE                                      ! Deal Quant
     CONT% = VAL(AA$)
     GOSUB GETEXTE                                      ! Precio de Vta
     VRUNPLU% = VAL(AA$)                                !
     GOSUB GETEXTE                                      ! Unidades o Peso
     CAN% = VAL(AA$)                                    !
!     LOCATE 1,1 : PRINT VRUNPLU%,CAN%,"   ";            !
!     INPUT "  ";ZZZ$                                    !
  ENDIF                                                 !
  IF MID$(A$,09,1) = "1" OR                            \! Si Item Cancel
     MID$(A$,10,1) = "1" OR                            \! or Refund
     MID$(A$,11,1) = "1" OR                            \! or Cupon Almac
     MID$(A$,12,1) = "1" THEN BEGIN                     ! or Cupon Provee
     SGN% = -1                                          !
     SGNVTA$(PTRART) = "-"                              !
     SIGNO1$ = "-"
     BB.FLAG = 1:BB.PRECIO=BB.PRECIO*-1                 ! Si es anulacion
     NETOPO$ = STR$(VAL(NETOPO$)*-1)                    ! Vendedores
     IF BB.FLAG < 0 THEN SIGNO1$ = "-"
  ENDIF                                                 !
  IF ANCHE$ = "999999" AND                             \!
     REBAJ$ = "99"     THEN BEGIN                       !
        IF VAL(ANCME$) < 50 THEN DEPI% = 32             !
        IF VAL(ANCME$) > 49 THEN DEPI% = 41             !
  ENDIF                                                 !

!------------------------------------------------------------------------------
!                Equivalencias para Valores de DEPI%
!
!      ------------------   ---     --------------------------------------
!        DESCRIPTOR         ddd           O b s e r v a c i o n e s
!      ------------------   ---     --------------------------------------
!      Desc Ch/Subsid    =  001  
!      Retefuente        =  009
!      Descto COOTRAIM   =  028
!      Descto 10% COMFAN =  031                                            
!      Descto Anchetas   =  032     NOTA: Cada uno de estos conceptos debe
!      Descto Fruver     =  033           tener su respectivo PLU Deptal y
!      Descto Vecino Fiel=  038           el 'User Flag 4' = Y
!      Descto Carnes     =  039
!      Descto Medic VF   =  042           
!      Descto Mercados   =  041 
!      Descto Bonos      =  043
!      Redondeo Trans    =  048
!      Descto 8% Carnes  =  049
!      Dcto 3 % cotraim  =  070
!------------------------------------------------------------------------------
  IF IVAD%(DEPI%,6) = 1 THEN BEGIN                      ! Si el Depto esta
        IVAT%(NT%,7) = IVAT%(NT%,7) + BB.PRECIO*(-1)    ! asociado a un
        IVAD%(DEPI%,1) = IVAD%(DEPI%,1)+BB.PRECIO*(-1)  ! DESCUENTO,no Liquida 
        IVAD%(DEPI%,5) = IVAD%(DEPI%,5)+CAN%            ! Incrementa Cant
  ENDIF ELSE BEGIN                                      !
     SILIQ% = 1 
     CALL ACU.IVAICO                                    !
  ENDIF                                                 !
  !MTOTRAN = MTOTRAN + BB.PRECIO                        ! prueba trn miscel 
GOSUB GETUNPK
IF LEFT$(A$,1) = "4" OR LEFT$(A$,1) = "5" THEN BEGIN 
   MTOTRAN = MTOTRAN-BB.PRECIO
   DESC4 = DESC4+BB.PRECIO
ENDIF 

RETURN

S2:
  J = 3
  GOSUB GETUNPK                                        ! Grupo Multiprec
  GOSUB GETUNPK                                        ! Items en Deal
  GOSUB GETUNPK                                        ! Metodo Precio
  GOSUB GETUNPK                                        ! Deal Quant
  GOSUB GETUNPK                                        ! Precio de Vta
  BB.PRECIO = VAL(A$)                                  !
  GOSUB GETUNPK                                        ! Unidades o Peso
  UNIVTA$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)          !
  MTOTRAN = MTOTRAN + BB.PRECIO * VAL(UNIVTA$(PTRART)) !
  RETURN
  
S3:
  J = 3
  GOSUB GETUNPK                                   ! Grupo de Descto
  GRDES% = VAL(A$)                                !
  AMT1$=RIGHT$(A$,10)                             !
  GOSUB GETUNPK                                   ! Rata de Descto
  GOSUB GETUNPK                                   ! Vr Descto
  AMT$=RIGHT$(A$,10)                               
  REBAJ% = VAL(AMT$)*(-1)                         ! Vr Rebaja
  NETVEN$(F) = STR$(VAL(NETVEN$(F)) - VAL(AMT$))  !
  IF GRDES%=1 THEN IVAD%(33,1)=IVAD%(33,1)+REBAJ% !
  IF GRDES%=2 THEN IVAD%(39,1)=IVAD%(39,1)+REBAJ% !
  IF GRDES%=3 THEN IVAD%(28,1)=IVAD%(28,1)+REBAJ% !
  IF GRDES%=4 THEN IVAD%(32,1)=IVAD%(32,1)+REBAJ% !
  IF GRDES%=7 THEN IVAD%(29,1)=IVAD%(29,1)+REBAJ% !
  IF GRDES%=8 THEN IVAD%(38,1)=IVAD%(38,1)+REBAJ% !
  !IF GRDES%=9 THEN IVAD%(29,1)=IVAD%(29,1)+REBAJ% !
  IF GRDES%=9 THEN IVAD%(38,1)=IVAD%(38,1)+REBAJ% !
  IF GRDES%=5 THEN IVAD%(60,1)=IVAD%(60,1)+REBAJ% !
  IF GRDES%=10 THEN IVAD%(32,1)=IVAD%(32,1)+REBAJ% !
  If GRDES%=6 THEN IVAD%(32,1)=IVAD%(32,1)+REBAJ% !
  IF GRDES%=11 THEN IVAD%(32,1)=IVAD%(32,1)+REBAJ% !
  IF GRDES%=12 THEN IVAD%(74,1)=IVAD%(74,1)+REBAJ% !
  If grdes%=15 Then ivad%(42,1)=ivad%(42,1)+rebaj% !
  IVAT%(NT%,7) = IVAT%(NT%,7) + REBAJ%            !

  IF VAL(AMT1$)=01 THEN BEGIN
     SA.AMT1 = SA.AMT1+(VAL(NETVEN$(F)))
  ENDIF
  IF VAL(AMT1$)=02 THEN BEGIN
     SA.AMT2 = SA.AMT2+(VAL(NETVEN$(F)))
  ENDIF
  IF VAL(AMT1$)=03 THEN BEGIN
     SA.AMT3 = SA.AMT3+(VAL(NETVEN$(F)))
  ENDIF
  IF VAL(AMT1$)=04 THEN BEGIN
     SA.AMT4 = SA.AMT4+(VAL(NETVEN$(F)))
  ENDIF
  IF VAL(AMT1$)=05 THEN BEGIN
     SA.AMT5 = SA.AMT5+(VAL(NETVEN$(F)))
  ENDIF
  IF VAL(AMT1$)=08 THEN BEGIN
     SA.AMT8 = SA.AMT8+(VAL(NETVEN$(F)))
  ENDIF

  WRITE FORM "C1 C5 C6 C4 C9 C3 C14 C2 ";#FIL8;" ",       \!
              NTR,COD$,DEP$,AMT$,AMT1$,CEDULA$,FR$         ! Rebajas
  SA.AMTDISCO = SA.AMTDISCO+(VAL(NETVEN$(F)))
  ! DESCUE$ = NETVEN$(F) 
 RETURN

S4:
  J = 3
  GOSUB GETUNPK
  AMNUL$=RIGHT$(A$,10)
  GOSUB GETUNPK
  GOSUB GETUNPK
  AMT$=RIGHT$(A$,10)
  NETVEN$(F) = STR$(VAL(NETVEN$(F)) + VAL(AMT$))
  AMT$=STR$(VAL(AMT$)*-1)                               ! Rebajas
  TO.DISVOID = TO.DISVOID+(VAL(AMT$))
  IF VAL(AMNUL$)=01 THEN BEGIN
     SA.ANUL1 = SA.ANUL1+(VAL(AMT$))
  ENDIF
  IF VAL(AMNUL$)=02 THEN BEGIN
     SA.ANUL2 = SA.ANUL2+(VAL(AMT$))
  ENDIF
  IF VAL(AMNUL$)=03 THEN BEGIN
     SA.ANUL3 = SA.ANUL3+(VAL(AMT$))
  ENDIF
  IF VAL(AMNUL$)=04 THEN BEGIN
     SA.ANUL4 = SA.ANUL4+(VAL(AMT$))
  ENDIF
  IF VAL(AMNUL$)=05 THEN BEGIN
     SA.ANUL5 = SA.ANUL5+(VAL(AMT$))
  ENDIF

  IF VAL(AMNUL$)=08 THEN BEGIN
     SA.ANUL8 = SA.ANUL8+(VAL(AMT$))
     IVAD%(38,1)= IVAD%(38,1)-(VAL(AMT$))
  ENDIF

  IF VAL(AMNUL$)=09 THEN BEGIN
     SA.ANUL9 = SA.ANUL9+(VAL(AMT$))
     IVAD%(38,1)= IVAD%(38,1)-(VAL(AMT$))
  ENDIF
  IF VAL(AMNUL$) > 09 THEN BEGIN
     SA.ANUL9 = SA.ANUL9+(VAL(AMT$))
     IVAD%(32,1)= IVAD%(32,1)-(VAL(AMT$))
  ENDIF
  WRITE FORM "C1 C5 C6 C3 C10 C3 C14 C2";#FIL8;" ",        \!
              NTR,COD$,DEP$,AMT$,AMNUL$,CEDULA$,FR$         ! Rebajas
  RETURN

S5:
  J = 3
     PORCEN = 0                                               !
     IF MTOTRAN <> 0 THEN BEGIN                               !
        PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                ! 
        POR = PORCEN -100
     ENDIF                                                  !
     IF TIPPAG$ = "43" THEN BEGIN                          \! Cheque Postfechado
        WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";   \! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
     ENDIF                                                  !
 
  GOSUB GETUNPK                                              ! Tip/Vrdad Pg 
  TIPPAG$  =  RIGHT$("00" + A$,2)                            ! Guarda Tip/Var
  IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
  IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                      ! Conv Otr/Pg a CR
  OLD.PAGO$ = TIPPAG$
  IF TIPPAG$ = "45" THEN Begin 
    OLD.PAGO$ = TIPPAG$
  	TIPPAG$ = "61"                      ! Conv Otr/Pg a CR
  EndIf 
  TIPPAW$  =  TIPPAG$                                        !
  GOSUB GETUNPK                                              ! Valor Pago 
  VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                    ! Guarda Vr Pag
!------------------------------------------------------------------------------
  TI% = VAL(LEFT$(TIPPAG$,1))                                ! Acumula Pagos
  VA% = VAL(RIGHT$(TIPPAG$,1))                               ! para Compr
  PAGO% = VAL(A$)                                            ! Diario de IVA
  TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%                  !
  TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                      !
  TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                    !
  TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                        !
  TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                      !
  TPG%(0,0,0)     = TPG%(0,0,0) + 1                          !
  
  If OLD.PAGO$ = "45" THEN Begin 
     TIPPAG$ = OLD.PAGO$
  EndIf 
!------------------------------------------------------------------------------
  IF VAL(A$) < 0 THEN BEGIN                                  !
     SIGNO$ = "-"                                            !
      VALPAG  =  RIGHT$(STRING$(10,"0")+STR$(VAL(A$)*-1),9)  !
  ENDIF ELSE BEGIN                                           !
     SIGNO$ = "+"                                            !
  ENDIF                                                      !
  GOSUB GETUNPK                                              ! Vr Comisiones
  GOSUB GETUNPK                                              ! No. Cliente
  NRWRK$  =  A$                                              ! Guarda Cliente
  PORCEN = 0                                                 !
  IF MTOTRAN <> 0 THEN BEGIN                                 !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                  ! 
  ENDIF                                                      !
  
  PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                    !
  BANCO = 0                                                  !
  BANCO = (100 - VAL(BANCO$))                                !
  BAN$ = RIGHT$("000" + STR$(BANCO),3)                       !
  NIT$ = RIGHT$("00000000000000" + NRWRK$,14)                !
  MEDICO$ = RIGHT$("00000000" + MEDICO$,8)                   !
  FORMULA$ = RIGHT$("00000000" + FORMULA$,8)                 !
  DIAGN$ = RIGHT$("0000" + DIAGN$,4)                         !
  IF TIPPAG$ = "61" OR TIPPAG$ = "60" OR                    \!
     TIPPAG$ = "65" OR TIPPAG$ = "66"    THEN BEGIN          !
    FOR INDART = 1 TO PTRART                                 !
     NO.FACTURA$ = AUTO$+"0"+RESP$+NUT+NTR
      WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C14 C14 C3 C8 C8 C4 C26 C6 C2 ";    \!
      #FIL11;                                               \! 
      "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
      UNIVTA$(INDART),MTOVTA$(INDART),CHEQUE$,CTACTE$,      \!
      NIT$,BAN1$,MEDICO$,FORMULA$,DIAGN$,NO.FACTURA$,CUOMOD$,FR$ !
    NEXT INDART                                              !  
  ENDIF                                                      !


      IF TIPPAG$ = "26" THEN BEGIN
         FOR INDART = 1 TO PTRART                             !
      IF VAL(NIT$) > 99999999 THEN NIT$ = MID$(NIT$,1,7): NIT$ = \
                                RIGHT$("00000000"+NIT$,8)                     !
         WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C9 C2";#36;"2", CODART$(INDART),\!
                DEPART$(INDART),SGNVTA$(INDART),UNIVTA$(INDART),\!
                MTOVTA$(INDART),RIGHT$(NIT$,8),VALPAG,FR$
         NEXT INDART   
      ENDIF

!     IF TIPPAG$ > "00" THEN BEGIN
           FOR INDART = 1 TO PTRART                                !
          IF DEPART$(INDART) = "861" THEN BEGIN
!             WRITE FORM"C10 C1 C11 C8 C3 C4 C2 C2";#37;MTOVTA$(INDART),\
!             SGNVTA$(INDART),EDUCA$,RESP$,NUT,NTR,TIPPAG$,FR$
              SIGNO1$ = SGNVTA$(INDART)
              VALEDU$ = MTOVTA$(INDART)
         ENDIF
           NEXT INDART
!    ! ENDIF

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
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NRWRK$+FR$         !
!        IF MID$(REGMED$,28,2) = "--" THEN MID$(REGMED$,28,2) = "00"
        
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
        
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
!    PRINT TOTCRE$
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
        IF VAL(TIPPAG$) >= 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                               !
        PORCEN = 0                                          !
        IF MTOTRAN <> 0 THEN BEGIN                          !
           PORCEN = (VAL(VALPAG) / MTOTRAN) * 100           ! 
        ENDIF                                               !
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
        ENDIF                                               !
     ENDIF ELSE BEGIN                                       !
        CONSE  = CONSE - 1                                  !
        CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)      ! Consecutivo
     ENDIF
     J = 3
     FT      =  1                                           !
     GOSUB GETUNPK                                          ! Toma Tipo/Var
     TIPPAG$  =  RIGHT$("00" + A$,2)                        ! en 2 Posic
     IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GOSUB GETUNPK                                          ! Toma Vr Pago
     VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                ! Guarda Vr Pag
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%              !
     TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                  !
     TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                !
     TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                    !
     TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                  !
     TPG%(0,0,0)     = TPG%(0,0,0) + 1                      !
!------------------------------------------------------------------------------
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
     IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GOSUB GETUNPK                                          ! Toma Vr Pago
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%              !
     TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                  !
     TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                !
     TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                    !
     TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                  !
     TPG%(0,0,0)     = TPG%(0,0,0) + 1                      !
!------------------------------------------------------------------------------
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
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$         !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !PRINT TOTCRE$
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                               !
!------------------------------------------------------------------------------
!PRINT "#FACTURA: ";CHEQUE$,NOCUE$
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
        ENDIF                                               !
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
  NIT$ = RIGHT$("00000000000000" + NRWRK$,14)               !
  MEDICO$ = RIGHT$("00000000" + MEDICO$,8)                  !
  FORMULA$ = RIGHT$("00000000" + FORMULA$,8)                !
  DIAGN$ = RIGHT$("0000" + DIAGN$,4)                        !
  IF TIPPAG$ = "61" OR TIPPAG$ = "64"                      \!
  OR TIPPAG$ = "65" OR TIPPAG$ = "66"    THEN BEGIN         !
    FOR INDART = 1 TO PTRART                                 !
        NO.FACTURA$ = AUTO$+"0"+RESP$+NUT+NTR
!       PRINT NO.FACTURA$
      WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C14 C14 C3 C8 C8 C4 C26 C6 C2 ";    \!
      #FIL11;                                               \! 
      "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
      UNIVTA$(INDART),MTOVTA$(INDART),CHEQUE$,CTACTE$,      \!
      NIT$,BAN1$,MEDICO$,FORMULA$,DIAGN$,NO.FACTURA$,CUOMOD$,FR$ !
    NEXT INDART                                              !  
    !DATOFOR$ = " " 
  ENDIF                                                      !
     
      IF TIPPAG$ = "26" THEN BEGIN

        IF VAL(NIT$) > 99999999 THEN NIT$ = MID$(NIT$,1,7):NIT$= \
                                RIGHT$("00000000"+NIT$,8)                     !
         FOR INDART = 1 TO PTRART                             !
         WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C9 C2";#36;"2", CODART$(INDART),\!
                DEPART$(INDART),SGNVTA$(INDART),UNIVTA$(INDART),\!
                MTOVTA$(INDART),RIGHT$(NIT$,8),VALPAG,FR$
         NEXT INDART   
      ENDIF


    FOR INDART = 1 TO PTRART                                !
         IF DEPART$(INDART) = "861" THEN BEGIN
         SIGNO1$ = SGNVTA$(INDART) 
         VALEDU$ = MTOVTA$(INDART) 
        ENDIF
    NEXT INDART                                              !  
  RETURN                                                    !
PRINT "NUT " + NUT + " TRAN  " + NTR 
S9:
  J = 3                                                     !
  IF FT = 1 AND FLAG = 0 THEN BEGIN                         !
     IF VALPAG <> "000000000" THEN BEGIN                    !
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$         !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
   ! PRINT TOTCRE$
        ENDIF ELSE BEGIN                                    !
           PRINT "ER ",TIREG$," ",CONSEC$                   !
        ENDIF                                               !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                               !
!------------------------------------------------------------------------------
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
              #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",          \!
              "2","0000000",VALPAG,SIGNO$,"0000000",       \!
              "             ","0000000000000000000",FR$     !
        ENDIF                                               !
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
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1)   = TPG%(TI%,VA%,1) + PAGO%            !
!    TPG%(TI%,VA%,0)   = TPG%(TI%,VA%,0) + 1                !
     TPG%(TI%,0,1)     = TPG%(TI%,0,1) + PAGO%              !
!     TPG%(TI%,0,0)     = TPG%(TI%,0,0) + 1                  !
     TPG%(0,0,1)       = TPG%(0,0,1) + PAGO%                !
!     TPG%(0,0,0)       = TPG%(0,0,0) + 1                    !
!------------------------------------------------------------------------------
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
                CHEQUE$+CTACTE$+NUT+NTR+                    \! TPV/TRANS
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$            !
      IF LEN(REGMED$) = 76 THEN BEGIN                        !
         WRITE FORM "C76";#FIL1,NREC%;REGMED$                !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
   ! PRINT TOTCRE$
      ENDIF ELSE BEGIN                                       !
         PRINT "ER ",TIREG$," ",CONSEC$                      !
      ENDIF                                                  !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                               !
!------------------------------------------------------------------------------
  RETURN                                                     !


S11:
  J = 3                                                      !
       

      NUMCS$ = RIGHT$("0000000000" + STR$(NSU%) + "+", 10)
  GOSUB GETUNPK                                              !
     IF A$ = "?" OR A$ = ">>" OR                  \
        A$ = "<<" OR A$ = "=="  THEN RETURN

         CTACTE$  = RIGHT$(STRING$(20,"0")+A$,14)            !
         BENE$    = RIGHT$(STRING$(2,"0")+A$,2) 
         APROB$   = CTACTE$    
         EDUCA$ = "00000000000"

         IF LEFT$(A$,8) ="20040510" THEN BEGIN 
            EDUCA$   = RIGHT$(STRING$(19,"0")+A$,19)    
            EDUCA$   = RIGHT$(EDUCA$,11)
            VALEDU$=RIGHT$(STRING$(10,"0")+VALEDU$,10)
         !   WRITE FORM"C10 C1 C11 C8 C3 C4 C2 C2";#37;VALEDU$,\
         !              SIGNO1$,EDUCA$,RESP$,NUT,NTR,TIPPAG$,FR$
         ENDIF

!--- Se reemplaza >> por 99

         IF MID$(CTACTE$,12,2) = "99" THEN BEGIN             
            BENE$   = "00"
            CTACTE$ = MID$(CTACTE$,2,10)                        
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            CTACTE$ = CTACTE$+BENE$                           
         ENDIF ELSE BEGIN
           IF MID$(CTACTE$,11,2) = "99" THEN BEGIN             
            CTACTE$ = MID$(CTACTE$,1,10)                        
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            CTACTE$ = CTACTE$+BENE$                           
           ENDIF ELSE BEGIN
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            BENE$   = "00"
            CTACTE$ = CTACTE$+BENE$                           
           ENDIF
         ENDIF
         CEDULA$  = CTACTE$
         DATOFOR$ = A$                                       !
        IF LEN(DATOFOR$) = 21  THEN BEGIN 
          DATOFOR$ = RIGHT$(STRING$(21,"0")+A$,21)           !
          DIAGN$   = MID$(DATOFOR$,17,5)
          MEDICO$  = MID$(DATOFOR$,1,8)
          FORMULA$ = MID$(DATOFOR$,9,8)
        ENDIF
        IF LEN(DATOFOR$) = 8  AND \
           VAL(DATOFOR$) = 14052004 THEN BEGIN 
           !PRINT DATOFOR$
           CREDI% = 1 
        ENDIF
! PRINT DATOFOR$
  GOSUB GETUNPK
        CHEQUE$  = RIGHT$("00000000000000000000"+(A$),8)
        IF MID$(CHEQUE$,7,2) = "--" THEN BEGIN
            CHEQUE$ = "00000000"  
         ENDIF 

  GOSUB GETUNPK
        BANCO1$   = RIGHT$(STRING$(20,"0")+A$,9)
        GASOCC$   = RIGHT$(BANCO1$,8)
        BAN1$     = LEFT$(BANCO1$,3)
        CUOMOD$   = RIGHT$(BANCO1$,6)
        BANCO$    = RIGHT$(STRING$(20,"0")+BANCO1$,2)
  GOSUB GETUNPK
        DATO4$   = RIGHT$(STRING$(20,"0")+A$,20)
        FACTURA$ = MID$(DATO4$,1,7)
        FACTURA$ = RIGHT$(STRING$(8,"0")+FACTURA$,8)
        IF FACTURA$ > "00000000" THEN CHEQUE$  = FACTURA$   
        
  GOSUB GETUNPK
        AUTO$    = RIGHT$(STRING$(20,"0")+A$,10)
        DATO5$   = LEFT$(STRING$(20,"0")+A$,6)
!        PRINT AUTO$
        IF VAL(AUTO$) > 111111 THEN BEGIN                       ! Busca 
           AUTO$ = RIGHT$("000000"+(AUTO$),6) ENDIF ELSE BEGIN  ! autorizacion
           AUTO$ = "000000"                                     ! en credito
        ENDIF                                                   !y lo guarda
  GOSUB GETUNPK
        DATO6$   = RIGHT$(STRING$(20,"0")+A$,6)
        DAT6    = VAL(DATO6$)
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
     WRITE FORM "C7 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";            \
           #OUTFIL5;RIGHT$(NOCUE$,7),FECTRA,"00","01","2",CPR$,  \
           VALPAG,"0"+AUT$,CUOT$,"              ",FR$
     WRITE FORM "C7 C2 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";         \
           #OUTFIL6;RIGHT$(NOCUE$,7),TIPPAG$,FECTRA,"00","01","2",\
           CPR$,VALPAG,"0"+AUT$,CUOT$,"              ",FR$
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
  IF FT = 1 AND FLAG = 0 THEN BEGIN                         !
    IF VALPAG <> "000000000" THEN BEGIN                     !
      NREC% = VAL(CONSEC$) + 2                              !
       CALL ACUMULA                                         !
       REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+     \!
                CHEQUE$+CTACTE$+NUT+NTR+                   \! TPV/TRANS
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$           !
      IF LEN(REGMED$) = 76 THEN BEGIN                       !
         WRITE FORM "C76";#FIL1,NREC%;REGMED$               !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !PRINT TOTCRE$
      ENDIF ELSE BEGIN                                      !
         PRINT "ER ",TIREG$," ",CONSEC$                     !
      ENDIF                                                 !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        ENDIF                                               !
!------------------------------------------------------------------------------
   ENDIF ELSE BEGIN                                          !
     CONSE  = CONSE - 1                                      ! 
     CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
   ENDIF                                                     !
 ENDIF                                                       ! 
FT = 0                                                       !
RETURN                                                       !  

S99:                                                         !
  J = 3                                                      ! Posic 3
  GOSUB GETUNPK                                              ! Desempaq sig UPD
!--- Add GR-OVS 28 ENE 2010  
  If A$ = "45"  Then Begin                                   ! Si es una remision de mercancia
    Gr.Ind.Rem% = -1
  EndIf 
  
!-- Add OVS 12 Ene 2006  
  If A$ = "20060104" Then Begin                              ! Movimiento Domicilios 
     Asc.Vta.Domic% = Asc.Vta.Domic% + Val(MontoTr$)         ! Monto Ventas Domicilios
  EndIf 
  IF A$ = "21" AND                                          \! Str Cierre/Trans
     MID$(B$,5,2) = "TR" THEN BEGIN                          ! Ident TransVta
       TRANL1 = TRANL1 + 1
        HAY99% = 1                                           !
        VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0           !
                 IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0           !
        IF LEN(B$) < 25 THEN BEGIN                           !
           J = 11                                            !
           GOSUB GETUNPK                                     ! Desempaq sig UPD
           TRAF$ = A$                                        !
           LOCATE 20,55                                      !
           PRINT MID$(B$,5,7);" ";STR$(VAL(TRAF$))           !
           PREF$ = MID$(B$,7,4)                              !
           ALTE$ = PACK$(RIGHT$("00000000"+TRAF$,8))         !
           TRFI% = VAL(RIGHT$(TRAF$,8))                      !
           IF TRF%(NT%,0) <= 0 THEN TRF%(NT%,0)=TRFI%        ! Trans Inicial
           IF TRF%(NT%,1) < TRFI% THEN TRF%(NT%,1)=TRFI%     ! Trans Final
           TRF%(NT%,2) = TRF%(NT%,2) + 1                     ! Incr No Clientes
           VTA% = VRVTA%                                     !
           RETURN                                            ! Si String sin Vrs
        ENDIF                                                !
        J = 12                                               !
        GOSUB GETUNPK                                        ! Desempaq sig UPD
        TRAF$ = RIGHT$(STRING$(15,"0")+A$,15)                !
        TRFI% = VAL(RIGHT$(TRAF$,8))                         !
        LOCATE 20,55                                         !
        PRINT MID$(B$,5,7);" ";TRAF$                         !
          !INPUT "  ";ZZZ$                                    !
        SIGN$ = MID$(B$,7,1)                                 !
        PREF$ = MID$(B$,8,4)                                 !
        ALTE$ = PACK$(RIGHT$("0"+LEFT$(TRAF$,7),8))          !
        IF TRF%(NT%,0) <= 0 THEN TRF%(NT%,0)=TRFI%           ! Trans Inicial
        IF TRF%(NT%,1) < TRFI% THEN TRF%(NT%,1)=TRFI%        ! Trans Final
        TRF%(NT%,2) = TRF%(NT%,2) + 1                        ! Incr No Clientes
!        FISCO$ = PREF$+RIGHT$(STRING$(11,"0")+TRAF$,11)
        GOSUB GETUNPK                                        ! Vr Neto Vta
        VTA% = VAL(A$)                                       !
        IF SIGN$ = "-" THEN VTA% = -1 * VTA%                 !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 1
        IV1% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 2
        IV2% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 3
        IV3% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 4
        IV4% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 5
        IV5% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 6
        IV6% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 7
        IV7% = VAL(A$)                                       !
        GOSUB GETUNPK                                        ! Vr Iva Tarifa 8
        IV8% = VAL(A$)                                       !
        IF IV1% > VTA% THEN IV1% = 0                         !
        IF IV2% > VTA% THEN IV2% = 0                         !
        IF IV3% > VTA% THEN IV3% = 0                         !
        IF IV4% > VTA% THEN IV4% = 0                         !
        IF IV5% > VTA% THEN IV5% = 0                         !
        IF IV6% > VTA% THEN IV6% = 0                         !
        IF IV7% > VTA% THEN IV7% = 0                         !
        IF IV8% > VTA% THEN IV8% = 0                         !

  ENDIF                                                      !

  RETURN                                                     !

! ---------------------------------------------------------------------
GETUNPK:                                               !
  K  = MATCH(":",B$,J)                                 ! Busca sep de Campo
  A$ = UNPACK$(MID$(B$,J,K-J))                         ! y lo desempaqueta
  A$ = TRANSLATE$(A$,"=","-")                          ! Signo Negativo
  H$ = MID$(B$,J,K-J)                                  !
  J  = K + 1                                           ! Marca el inicio de sigui-
RETURN                                                 ! ente campo

! ---------------------------------------------------------------------

GETFLAG:                                               !
 FLAG = VAL(A$)                                        ! Conv FLAG a INT
                                                       ! Inicia STRING con
                                                       ! FLAGS individuales
  IF (FLAG AND 00000001H) THEN                        \!
      A$ = "1" ELSE A$ = "0"                           !
  FOR I = 1 TO 15                                      ! 
    FLAG = SHIFT(FLAG,1)                               ! Inicializa sig BIT
    IF (FLAG AND 00000001H) THEN                      \!
        A$ = "1" + A$                                 \!
    ELSE                                              \!
        A$ = "0" + A$                                  !
  NEXT I                                               !
  RETURN                                               !

! ---------------------------------------------------------------------
GETEXTE:
  KK = MATCH(":",BB$,JJ)            ! Busca separador de Campo
  AA$ = UNPACK$(MID$(BB$,JJ,KK-JJ)) ! y lo desempaqueta
  HH$ = MID$(BB$,JJ,KK-JJ)          !
  JJ=KK+1                           ! Marca el inicio de sigui-
  RETURN                            ! ente campo


  ERRTN:
  EOF$ = ERR
  IF ERR = "IH" THEN RESUME
  IF ERRF% = 22 THEN RESUME                            !
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
    LOCATE 22,1
    PRINT " ERR=";ERR;                                !
    PRINT "  ARCH=";STR$(ERRF%);                      !
    PRINT "  ERRN=";ERRNSTR$(ERRN);"         ";       !
    INPUT "  ";ZZZ$                                   !
    RESUME NXTRCD:                                    !
  ENDIF ELSE BEGIN                                   \! Si es EOF TSLOG
    
   CABECER$ = STRING$(109,"0")
WRITE FORM "C1 C12 6C10 C1 C2";                      \! Graba Reg Control
    #FIL1,1;"4",RESP$,TOTEF$,TOTCH$,TOTOT$,          \! de Medios de Pago
                   TOTRM$,TOTTC$,TOTCR$,"*",FR$       !
   WRITE FORM "C1 C12 6C10 C1 C109 C2";               \! Graba Reg Control
    #FIL9,1;"4",RESP$,TOTEF$,TOTCH$,TOTOT$,          \! de Medios de Pago
                    TOTRM$,TOTTC$,TOTCR$,"*",CABECER$,FR$       !
    WRITE FORM "C1 C12 6C10 C1 C2";                  \! Graba Reg Control
    #FIL1,2;"5",RESP$,NUMEF$,NUMCH$,NUMOT$,          \! de Cantidades
                   NUMRM$,NUMTC$,NUMCR$,"*",FR$       !
    WRITE FORM "C1 C12 6C10 C1 C109 C2";              \! Graba Reg Control
    #FIL9,2;"5",RESP$,NUMEF$,NUMCH$,NUMOT$,          \! de Cantidades
                  NUMRM$,NUMTC$,NUMCR$,"*",CABECER$,FR$       !
    CALL ACUDIA                                       ! 
    HORA$=TIME$                                       !
    HORA$=LEFT$(HORA$,2)+":"+                        \!
          MID$(HORA$,3,2)+":"+RIGHT$(HORA$,2)         !
    LPRINTER
   IF CUADRE = 1 THEN BEGIN                           !
      WRITE FORM "C80 C2";#FIL5;" ",FR$               !
      WRITE FORM "C10 C70 C2";#FIL5;                 \!
            "  "," PLANILLA DE CUADRE OK.! "+HORA$    !
    ENDIF ELSE BEGIN
      WRITE FORM "C80 C2";#FIL5;" ",FR$               !
      WRITE FORM "C10 C35 PIC($$#######,###) C2";    \!
            #FIL5;" ",                               \!
            " ** ERROR EN PLANILLA POR VALOR DE:",   \!
            ESPERADO% + TOTDES% - TT.CONTEO,FR$
    ENDIF                                            !
    WRITE FORM "C31 C2";#FIL5;S6NCN$,FR$             ! Salto;1/6's;NO-Conds
    WRITE FORM "C31 C2";#FIL5;"** Fin **",FR$        ! Reset de Impres
  ENDIF                                              !
  STOP                                               !
! ---------------------------------------------------------------------
FINAL:                                               !
  PRINT                                              !
  PRINT "EL PROGRAMA FINALIZA..."                    !
  STOP

FILERR:
  Print " Error de Aplicacion "
  PRINT " ERR = ";ERR
  PRINT "ERRN = ";ERRNSTR$(ERRN)
  PRINT
  PRINT "EL PROGRAMA FINALIZA..."
  Stop
! ---------------------------------------------------------------------
!           ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ---------------------------------------------------------------------
