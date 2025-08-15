

!---------------------------------------------------------
!    PROYECTO COMFAMILIAR ANDI
!    Nombre del Fuente     : CFRELCRE.BAS
!    Incluir en            : 
!    Fecha de Modificaci¢n : ENERO DE 1995
!    Autor                 : Indata Ltda.
!    Archivos que utiliza  : C:\ADX_UPGM\MEDPAG2.DAT
!
!---------------------------------------------------------
!    Lista Cr‚dito de Medicamentos
!---------------------------------------------------------

STRING                     \ !
       A$,                 \  !
       AA$,                \  !
       B$,                 \  !
       C$,                 \  !
       D$,                 \  !
       DD$,                \  !
       E$,                 \  !
       F$,                 \  !
       G$,                 \  !
       H$,                 \  !
       I$,                 \  !
       T$,                 \  !
       M$,                 \  !
       MM$,                \  !
       S$,                 \  !
       Z$,                 \  !
       ERRFX$,             \  !
       MEDPAG$(1),         \  ! Nombres de medios de pago
       NUMOPEL2,           \  ! Control de cambio de nivel de operador
       TIPPAGL1,           \  ! Control de cambio de nivel de tipo de pago
       FORMAT,             \  ! Formato para impresion
       FORMCH,             \  ! Formato para impresion
       FORMCU,             \  ! Formato para impresion
       CONSEC$,            \  !
       NUMOPE,             \  !
       TIPO$,              \  ! Indice para Variedad de Pago
       NUAL$,              \  !
       NOAL$,              \  !
       ALMA$,              \  !
       SIGNO$,             \  !
       FECHA$,             \  !
       HORATR$,            \  !
       CHEQUE$,            \  !
       TIREG$,             \  !
       CTACTE$,            \  !
       HORA$,              \  !
       NOMBRE$,            \  !
       AL$,                \  !
       NUMOPE$,            \  !
       NUMTER,             \  !
       TIPPAG$,            \  !
       BANCO$,             \  !
       NROCUE$,            \  !
       VALPAG$,            \  !
       TIPTRA,             \  !
       NUMTRA,             \  !
       ERRORCOD$,          \  !
       KEYPAG$,            \  !
       IVAS$,              \  !
       FECHEQUE,           \  !
       FINREC,             \ !
       FECHAR$,            \ !
       CENCOS$, ARCH4$        

INTEGER*1 L2,              \  ! Indicador de cambio de nivel
          L1,              \  ! Indicador de cambio de nivel
          LR,              \  ! Indicador de ultimo registro
          OV,              \  ! Indicador de cambio de pagina
          P%,              \  !
          Q%,              \  ! Longiud de ADXALMA.DAT
          IR,              \  ! Indicador de identificacion de registro
          FT                  ! Indicador de primer ciclo

INTEGER*4                  \  !
       S%,                 \  !
       SX%,                \  !
       HX%,                \  !
       SUM%,               \  !
       TRANL2,             \  ! Numero de transacciones
       MONTL2,             \  ! Valor de las transacciones
       TRANL1,             \  ! Numero de transacciones
       MONTL1,             \  ! Valor de las transacciones
       MONTLE,             \  ! Valor de las transacciones
       MONTLA,             \  ! Valor de las transacciones
       MONTLB,             \  ! Valor de las transacciones
       MONTLC,             \  ! Valor de las transacciones
       MONTLD,             \  ! Valor de las transacciones
       MONTOI,             \  ! Valor de las transacciones
       MONTOT                 ! Valor de las transacciones

! -------  Rutina para Verificar Existencia de EMPRE1.DAT --------------

FONNC$ = CHR$(26)+CHR$(50) !+CHR$(27)+CHR$(80)
SUB VERIFICA
    IF NROCUE$ = TAG$ THEN EXIT SUB                    ! Verifica Cambio Empr
    ERRORCOD$ = "  "                                   ! Inicia Estado Leer
    KEY$ = PACK$(TIPPAG$+NROCUE$)                      ! Arma llave para leer
    READ FORM "C8 C30 2C3 C1";#26 KEY KEY$;          \ ! Lee EMPRE.DAT
         KEY$,NOMD$,FECRED$,FEMODD$,NOVED$             ! Nomb;F/Crea;F/Mod;Nov
    IF ERRORCOD$ = "  " THEN BEGIN                     ! Si existe Registro
       FECRE$ = FECRED$                                ! Guarda Fecha Creacion
    ENDIF ELSE BEGIN                                   ! Fin Cond
       NOMD$ = "   Emp No Existe "                     ! Error Empresa
    ENDIF                                              ! Fin Cond
    ERRORCOD$ = "  "                                   ! Inicia Estado Leer
    TAG$ = NROCUE$
END SUB

! -------  Rutina para Totalizar por Variedad --------------

SUB TOUTPUT
   IF L2 = 1 AND                                      \! Si hay Totales
      TRANL2 <> 0 THEN BEGIN                           !
        PRINT                                          !
        PRINT USING FORMCH;TRANL2;                     !
        PRINT "  Transaccion(es)  Gran Total por: ";TAB(44);
        PRINT USING FORMAT;MONTL2;TAB(59);             !
        PRINT USING FORMAT;MONTLE;TAB(69);             !
        PRINT USING FORMAT;MONTLA;TAB(79);             !
        PRINT USING FORMAT;MONTLB;TAB(89);             !
        PRINT USING FORMAT;MONTLC;TAB(99);             !
        PRINT USING FORMAT;MONTLD;TAB(113);            !
        PRINT USING FORMAT;MONTOI;TAB(123);            !
        PRINT USING FORMAT;MONTOT                      !
        PRINT                                          !
        OV = OV + 2                                    !
        TRANL2 = 0  :  MONTL2 = 0 : MONTLE = 0         !
        MONTLA = 0  :  MONTLB = 0 : MONTLC = 0         !
        MONTLD = 0  :  MONTOI = 0 : MONTOT = 0         !
   ENDIF
   IF L1 = 1 AND                                      \! Si hay Totales
      TRANL1 <> 0 THEN BEGIN                           !
        PRINT                                          !
        PRINT USING FORMCH;TRANL1;                     !
        PRINT "  Transaccion(es)   Por Valor de : ";TAB(54);
        PRINT USING FORMAT;MONTL1                      !
        PRINT                                          !
        OV = OV + 3                                    !
        TIPPAGL1 = TIPPAG$                             !
        TRANL1 = 0:MONTL1 = 0                          !
   ENDIF
END SUB

! -------  Rutina para Acumular Ventas a Credito --------------

SUB DCALC
   IF SIGNO$ = " " THEN BEGIN
       TRANL2 = TRANL2 + 1
       MONTL2 = MONTL2 + VAL(VALPAG$)
       TRANL1 = TRANL1 + 1
       MONTL1 = MONTL1 + VAL(VALPAG$)
       MONTLE = MONTLE + VAL(EXENTO$)
       MONTLA = MONTLA + VAL(BAS16$)
       MONTLB = MONTLB + VAL(BAS20$)
       MONTLC = MONTLC + VAL(BAS35$)
       MONTLD = MONTLD + VAL(BAS10$)
       MONTOI = MONTOI + VAL(TOTIV$)
       MONTIC = MONTIC + VAL(ICON$)
       MONTOT = MONTOT + VAL(TOTRAN$)
   ENDIF ELSE BEGIN
       MONTL2 = MONTL2 - VAL(VALPAG$)
       MONTL1 = MONTL1 - VAL(VALPAG$)
       MONTLE = MONTLE - VAL(EXENTO$)
       MONTLA = MONTLA - VAL(BAS16$)
       MONTLB = MONTLB - VAL(BAS20$)
       MONTLC = MONTLC - VAL(BAS35$)
       MONTLD = MONTLD - VAL(BAS10$)
       MONTOI = MONTOI - VAL(TOTIV$)
       MONTIC = MONTIC - VAL(ICON$)
       MONTOT = MONTOT - VAL(TOTRAN$)
ENDIF
END SUB

! -------  Rutina para Verificar Cambio de Pagina --------------

SUB OVERFLOW
   IF FT = 1 THEN PRINT CHR$(12)
   P%=P% + 1

   PRINT " 0    CFRELCRE                                    COMFAMILIAR ANDI          ";ALMA$;FECHA$
                   
   PRINT TAB(3);HORA$;TAB(23);"RELACION DE CREDITOS DE MEDICAMENTOS  CON SU RESPECTIVO IVA ";    \
         TAB(70);"PAG.";TAB(75);STR$(P%) 
   PRINT "                                                                                                           RELACION DE IVAS          "
   PRINT "CONSE.  N§ FACT.   ID/CLIENTE      VALOR    NIT EMPRESA      EXENTO    BASE16%    BASE20%    BASE35%    BASE10%    TOT/IVA  VR/TRANSAC"
   PRINT "------  -------- -------------- --------- --------------     -------  ---------  ---------  ---------  ---------  ---------  -------- " 
   OV = 5
END SUB

! -------  Rutina para Imprimir los Creditos Individualmente --------------

SUB HDOUTPUT

   IF L1 = 1 THEN BEGIN
      CALL OVERFLOW
      PRINT TAB(25);MEDPAG$(VAL(TIPO$))
      PRINT
      OV = OV + 2
   ENDIF
   IF IR = 1 THEN BEGIN 
     CALL VERIFICA
      IF NOMD$ <> STRING$(30," ")   THEN BEGIN      ! Verifica Cambio Empr
        PRINT "  Nombre de la Empresa: ";TAB(44);
        PRINT NOMD$                                        !
      NOMD$ = STRING$(30," ")
      OV = OV + 1
      ENDIF
      PRINT CONSEC$;TAB(9);   
      PRINT USING FORMCH;VAL(CHEQUE$);TAB(18);
      PRINT USING FORMCU;VAL(CTACTE$);TAB(33);
      PRINT USING FORMAT;VAL(VALPAG$);
      PRINT SIGNO$;TAB(44);
      PRINT USING FORMCU;VAL(NROCUE$);TAB(59);
      !PRINT NOMD$
      PRINT USING FORMAT;VAL(EXENTO$);TAB(69);
      PRINT USING FORMAT;VAL(BAS16$);TAB(79);
      PRINT USING FORMAT;VAL(BAS20$);TAB(89);
      PRINT USING FORMAT;VAL(BAS35$);TAB(99);
      PRINT USING FORMAT;VAL(BAS10$);TAB(113);
      PRINT USING FORMAT;VAL(TOTIV$);TAB(123);
      PRINT USING FORMAT;VAL(TOTRAN$)
!      PRINT NOMD$
      NOMD$ = STRING$(30," ")
      OV = OV + 1
    ENDIF
END SUB

! -------  Rutina para Inicializar Variables de Cambio --------------
SUB SETOF
   L2 = 0
   L1 = 0
   IR = 0
END SUB
 
! -------  Rutina para Lectura de Medios de Pago --------------
SUB LEER
   READ FORM                                               \ !
       "C1 C6 C4 C9 C1 C8 C14 C3 C4 C6 2C2 C14 C9 C1 C2 C9 C1 C2 C9 C1 C2 C9 C1 C2 C9 C1 C9 C1 C9 C1 C9 C1 C2"; \ !
        #1;TIREG$,CONSEC$,HORATR$,VALPAG$,                 \ !
         SIGNO$,CHEQUE$,CTACTE$,NUMTER,NUMTRA,             \ !
         NUMOPE$,TIPPAG$,BANCO$,NROCUE$,EXENTO$,SIGA$,      \!
        A$,BAS16$,SIGB$,B$,BAS20$,SIGC$,C$,BAS35$,         \!
        SIGD$,D$,BAS10$,SIGE$,TOTIV$,SIGF$,ICON$,SIGG$,    \!
        TOTRAN$,SIG$,FINREC               !
   IF SIGNO$ = "+" THEN SIGNO$ = " "   
END SUB

SUB TCALC
END SUB

!----------------------------------------------------
!            PROGRAMA PRINCIPAL
!----------------------------------------------------

   ON ERROR GOTO RUTINA.ERRORES

   OPEN "C:\ADXALMA.DAT" AS 27 BUFFSIZE 80         ! Archivo Control Almacen
   READ FORM "C80";#27;AL$                         ! Lee Control Almac
   Q% =  MATCH(CHR$(13), AL$,1)                    ! Long Reg Control
   AL$ = LEFT$(AL$,Q%-1) + STRING$(20," ")         ! Ajusta Reg Ctrl
   NUAL$ = STR$(VAL(LEFT$(AL$, 4)))                ! Guarda Num Almacen
   NOAL$ = MID$(AL$, 13, 20)                       ! Guarda Nombre
   ALMA$ = "   N§ " + NUAL$ + NOAL$                ! con Num.Almac
   CENCOS$   = RIGHT$(NUAL$,3)  
   FECHAR$    = MID$(AL$,9,4)
   ARCH3$ = "C:\ADX_IDT1\EMPRE.DAT"                ! Define Empresas
   !ARCH4$ = "C:\ADX_UDT1\P"+CENCOS$+FECHAR$+".DAT"  
   ARCH4$ = "C:\ADX_UDT1\IVACRED2.DAT"  
   OPEN ARCH3$ KEYED RECL 45 AS 26                 ! Abre Empresas KL=8
   OPEN ARCH4$ AS 1
   PRINT ARCH4$
   LPRINTER
   DIM MEDPAG$(25)
   MEDPAG$(0) = "- CHEQ/BCOS LOCALES -"
   MEDPAG$(1) = "- CHEQ/TARJ COMFANDI -"
   MEDPAG$(2) = "- CHEQUES SUBSIDIO -"
   MEDPAG$(3) = "      "
   MEDPAG$(4) = "      "
   MEDPAG$(5) = "- BONOS PREPAGADOS -"
   MEDPAG$(6) = "- BONOS FABRICANTE -"
   MEDPAG$(7) = "- BONOS COMFANDI -"
   MEDPAG$(8) = "      "
   MEDPAG$(9) = "      "
   MEDPAG$(10) = "- RED MULTICOLOR -"
   MEDPAG$(11) = "- AVANCE EFECTIVO -"
   MEDPAG$(12) = "     "
   MEDPAG$(13) = "- EFECTIVO -"
   MEDPAG$(14) = "- CREDIBANCO -"
   MEDPAG$(15) = "- CREDENCIAL -"
   MEDPAG$(16) = "- B.I.C. -"
   MEDPAG$(17) = "- DINERS -"
   MEDPAG$(18) = "- OTRAS TARJ/CR -"
   MEDPAG$(19) = "      "
   MEDPAG$(20) = "- MEDICAM/EMPRESAS -"
   MEDPAG$(21) = "- TEMPORADA ESCOLAR -"
   MEDPAG$(22) = "- CREDITO INTERNO -"
   MEDPAG$(23) = "- ESPECIAL EMPRESAS -"
   MEDPAG$(24) = "- C.V.C. 60% -"
   MEDPAG$(25) = "- CREDITO SIN FACTURA -"
   FORMAT = "##,###,###"
   FORMCH = "########"
   FORMCU = "##############"
   T$=TIME$  
      H$=LEFT$(T$,2)   :  M$=MID$(T$,3,2)   :  S$=RIGHT$(T$,2)
   F$=DATE$
      AA$=LEFT$(F$,2)  :  MM$=MID$(F$,3,2)  :  DD$=RIGHT$(F$,2)
   FECHA$ = DD$ + "-" + MM$ + "-" + AA$
   HORA$  = H$  + ":" + M$  + ":" + S$
   WHILE ERRORCOD$ = "  "
      CALL HDOUTPUT
      CALL SETOF
      CALL LEER
   OTRAVEZ:
      IF ERRORCOD$ = "EF" THEN BEGIN
         LR = 1
         L2 = 1
         L1 = 1
         GOTO TOTALC
      ENDIF
      IF LEFT$(TIPPAG$,1) < "6" OR                  \! Si NO son Medicamentos
         TIREG$ > "3" THEN BEGIN
         CALL LEER
         GOTO OTRAVEZ
      ENDIF 
      IF FT = 0 THEN BEGIN 
         FT = 1  :  L1 = 1
         TIPPAGL1 = TIPPAG$
      ENDIF
      IF TIPPAG$ = "61" THEN TIPO$ = "20"
      IF TIPPAG$ = "62" THEN TIPO$ = "21"
      IF TIPPAG$ = "63" THEN TIPO$ = "22"
      IF TIPPAG$ = "64" THEN TIPO$ = "23"
      IF TIPPAG$ = "65" THEN TIPO$ = "24"
      IF TIPPAG$ = "66" THEN TIPO$ = "25"
      IR = 1
      IF ERRORCOD$ = "EF" THEN BEGIN          ! Si es EOF
         LR = 1                               !
         L2 = 1                               !
         L1 = 1                               !
         GOTO TOTALC                          !
      ENDIF
      IF TIPPAG$ <> TIPPAGL1 THEN L1 = 1      ! Verifica Cambio Tip CR
      IF NROCUE$ <> TAG$     THEN L2 = 1      ! Verifica Cambio Empr

TOTALC:   
        IF FT = 1 THEN BEGIN 
           CALL TCALC
           CALL TOUTPUT
        ENDIF
 !       IF ERRORCOD$ = "EF" THEN PRINT " 1   " :  PRINT "  FIN  " : STOP
        IF ERRORCOD$ = "EF" THEN PRINT FONNC$ : STOP
         IF OV > 70 THEN CALL OVERFLOW
        CALL DCALC
   WEND
PRINT " F-"
   
  PRINT " F                           "
  CONSOLE
   STOP
!----------------------------------------------------------------------
!      *                RUTINA PARA ERRORES                      *
!----------------------------------------------------------------------

RUTINA.ERRORES:
    HX% = ERRN
    ERRFX$=""
    FOR S% = 28 TO 0 STEP -4
        SX% = SHIFT(HX%,S%)
        SUM% = SX% AND 000FH
        IF SUM% > 9 THEN     \
           SUM% = SUM% + 55  \
        ELSE                 \
           SUM% = SUM% + 48
        Z$ = CHR$(SUM%)
        ERRFX$ = ERRFX$ + Z$
    NEXT S%
!       PRINT "ERROR = ";ERR; " ";ERRFX$;"  ARCH= ";            \
!       STR$(ERRF%);"  LIN = ";STR$(ERRL)
IF ERR = "EF" THEN ERRORCOD$=ERR                    ! END FILE.
IF ERR = "EF" AND ERRF% = 26 THEN ERRORCOD$="NO"    ! REG NO EXISTE.
RESUME
   END
!----------------------------------------------------------------------
!            ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!----------------------------------------------------------------------
