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
! Modificaciones:
! Mod 06-Abr-2016
! Se modifica la forma de lectura del archivo
! para generacion de reporte de créditos.
! Desarrollado por Gretail - OVS
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
       FINREC , fecalm$, ARCHIV$, MES$, DIA$

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

FINR$=CHR$(13)+CHR$(10)		!CREA UN FIN DE REGISTRO

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
 If montl2 > 0 Then begin
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
 endif
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
                   
   PRINT TAB(3);FECALM$;TAB(23);"RELACION DE CREDITOS DE MEDICAMENTOS  CON SU RESPECTIVO IVA ";    \
         TAB(70);"PAG.";TAB(75);STR$(P%) 
   PRINT "                                                                                                           RELACION DE IVAS          "
   PRINT "CONSE.  N§ FACT.   ID/CLIENTE      VALOR    NIT EMPRESA      EXENTO    BASE16%    BASE20%    BASE35%    BASE10%    TOT/IVA  VR/TRANSAC"
   PRINT "------  -------- -------------- --------- --------------     -------  ---------  ---------  ---------  ---------  ---------  -------- " 
   OV = 5
END SUB

! -------  Rutina para Imprimir los Creditos Individualmente --------------

SUB HDOUTPUT
 If Val(valpag$) > 0 Then begin
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
 endif   
END SUB

! -------  Rutina para Inicializar Variables de Cambio --------------
SUB SETOF
   L2 = 0
   L1 = 0
   IR = 0
END SUB
 
! -------  Rutina para Lectura de Medios de Pago --------------
!SUB LEER
!   READ FORM                                               \ !
!       "C1 C6 C4 C9 C1 C8 C14 C3 C4 C6 2C2 C14 C9 C1 C2 C9 C1 C2 C9 C1 C2 C9 C1 C2 C9 C1 C9 C1 C9 C1 C9 C1 C21 C2"; \ !
!        #1;TIREG$,CONSEC$,HORATR$,VALPAG$,                 \ !
!         SIGNO$,CHEQUE$,CTACTE$,NUMTER,NUMTRA,             \ !
!         NUMOPE$,TIPPAG$,BANCO$,NROCUE$,EXENTO$,SIGA$,      \!
!        A$,BAS16$,SIGB$,B$,BAS20$,SIGC$,C$,BAS35$,         \!
!        SIGD$,D$,BAS10$,SIGE$,TOTIV$,SIGF$,ICON$,SIGG$,    \!
!        TOTRAN$,SIG$,FISCO$,FINREC               !
!   IF SIGNO$ = "+" THEN SIGNO$ = " "   
!END SUB


Sub Leer
String TS.TEMP1$
   Read #1; TS.TEMP1$
   TIREG$   = Mid$(TS.TEMP1$,1,1)
   CONSEC$  = Mid$(TS.TEMP1$,2,6)
   HORATR$  = Mid$(TS.TEMP1$,8,4)
   VALPAG$  = Mid$(TS.TEMP1$,12,9)
   SIGNO$   = Mid$(TS.TEMP1$,21,1)
   CHEQUE$  = Mid$(TS.TEMP1$,22,8)
   CTACTE$  = Mid$(TS.TEMP1$,30,14)
   NUMTER   = Mid$(TS.TEMP1$,44,3)
   NUMTRA   = Mid$(TS.TEMP1$,47,4)
   NUMOPE$  = Mid$(TS.TEMP1$,51,6)
   TIPPAG$  = Mid$(TS.TEMP1$,57,2)
   BANCO$   = Mid$(TS.TEMP1$,59,2)
   NROCUE$  = Mid$(TS.TEMP1$,61,14)
   EXENTO$  = Mid$(TS.TEMP1$,75,9)
   SIGA$    = Mid$(TS.TEMP1$,84,1)
   A$       = Mid$(TS.TEMP1$,85,2)
   BAS16$   = Mid$(TS.TEMP1$,87,9)
   SIGB$    = Mid$(TS.TEMP1$,96,1)
   B$       = Mid$(TS.TEMP1$,97,2)
   BAS20$   = Mid$(TS.TEMP1$,99,9)
   SIGC$    = Mid$(TS.TEMP1$,108,1)
   C$       = Mid$(TS.TEMP1$,109,2)
   BAS35$   = Mid$(TS.TEMP1$,111,9)
   SIGD$    = Mid$(TS.TEMP1$,120,1)
   D$       = Mid$(TS.TEMP1$,121,2)
   BAS10$   = Mid$(TS.TEMP1$,123,9)
   SIGE$    = Mid$(TS.TEMP1$,132,1)
   TOTIV$   = Mid$(TS.TEMP1$,133,9)
   SIGF$    = Mid$(TS.TEMP1$,142,1)
   ICON$    = Mid$(TS.TEMP1$,143,9)
   SIGG$    = Mid$(TS.TEMP1$,152,1)
   TOTRAN$  = Mid$(TS.TEMP1$,153,9)
   SIG$     = Mid$(TS.TEMP1$,162,1)
   FISCO$   = Mid$(TS.TEMP1$,163,21)
   If SIGNO$ = "+" THEN SIGNO$ = " "   
   FINREC$ = CHR$(13) + CHR$(10)
End Sub 

SUB TCALC
END SUB

!----------------------------------------------------
!            PROGRAMA PRINCIPAL
!----------------------------------------------------

ON ERROR GOTO RUTINA.ERRORES

!--------------------	MENU CAPTURA DATOS ----------------
T%=1
WHILE T%<>0
INICIO:
CLEARS  
PRINT "		"
PRINT "                     SOPORTE POS				 "
print "        PROGRAMA PARA REIMPRIMIR REPORTE DE CREDITOS 		 "
PRINT "                   Ing. Tulio A. Giraldo          		 "
PRINT "                                                      		 "
PRINT "                                                      		 " 
INPUT "	DIGITE MES - 2 DIGITOS (MM ej: 02,09,11):       ";MES$  
IF VAL(MES$)<1 OR VAL(MES$)>12 THEN BEGIN
	CLEARS
	PRINT "EL MES DIGITADO NO ES VALIDO, INTENTE NUEVAMENTE..." 
	PRINT "		"
	PRINT "		" 
	INPUT "  DIGITE INTRO PARA REPETIR.... ";RST$
	GOTO INICIO
ENDIF 

INPUT "	DIGITE DIA - 2 DIGITOS (DD ej: 07,16,27):	";DIA$
IF VAL(DIA$)<1 OR VAL(DIA$)>31 THEN BEGIN
	CLEARS
	PRINT "EL DIA DIGITADO NO ES VALIDO, INTENTE NUEVAMENTE..."
	PRINT "		"
	PRINT "		" 
	INPUT "  DIGITE INTRO PARA REPETIR.... ";RST$
	GOTO INICIO
ENDIF

IF (VAL(MES$)>0 AND VAL(MES$)<13) AND (VAL(DIA$)>1 AND VAL(DIA$)<32) THEN BEGIN
	T%=0
ENDIF
WEND

!************************************************* 

   OPEN "C:\ADXALMA.DAT" AS 27 BUFFSIZE 80         ! Archivo Control Almacen
   READ FORM "C80";#27;AL$                         ! Lee Control Almac
   Q% =  MATCH(CHR$(13), AL$,1)                    ! Long Reg Control
   AL$ = LEFT$(AL$,Q%-1) + STRING$(20," ")         ! Ajusta Reg Ctrl
   NUAL$ = STR$(VAL(LEFT$(AL$, 4)))                ! Guarda Num Almacen
   NOAL$ = MID$(AL$, 13, 20)                       ! Guarda Nombre
   ALMA$ = "   N§ " + NUAL$ + NOAL$                ! con Num.Almac
   !FECALM$ =  MID$(AL$, 5, 8)                      ! Guarda fecha adxalma
!   FECALM$ = MES$+DIA$
!   FECHA1$ =  MID$(AL$, 9, 4)                      ! Guarda fecha adxalma
   ANO$ =  MID$(AL$, 5, 4)                      ! Guarda fecha adxalma
   FECALM$ = ANO$+MES$+DIA$
   ARCH3$ = "C:\ADX_IDT1\EMPRE.DAT"                ! Define Empresas
   OPEN ARCH3$ KEYED RECL 45 AS 26                 ! Abre Empresas KL=8
   ARCHIV$="C:\ADX_UDT1\P"+ALMA$+MES$+DIA$+".DAT"   
ON ERROR GOTO ERRORTRAP
   OPEN "C:\ADX_UDT1\P"+NUAL$+MES$+DIA$+".DAT" AS 1  
   LPRINTER
   DIM MEDPAG$(25)
   MEDPAG$(0) = "-NUEVOS CREDITOS(FORMULA MEDICA)- "
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

! -------  Crea archivo de creditos --------------

!CREATE "C:/ADX_UDT1/CRED"+MM$+DD$+"."+NUAL$ AS 9

!CREATE "C:/COMFANDI/RPT_CRED.BAT" AS 3

!WRITE FORM "C70 C2";#3;"COPY C:\ADX_IOSS\ADXIOSPD.* C:\ADX_UDT1\CRED"+FECHA1$+"."+NUAL$,FINR$

! -------------------------------------------------

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




ERRORTRAP:
ERRORCOD$=ERR
!IF ERRORCOD$="OE" OR ERRF%=26 THEN BEGIN
IF ERRORCOD$="OE" THEN BEGIN
	CLEARS
	PRINT "   "
	PRINT "   "
	PRINT "   "
	PRINT "   "
	PRINT "        ERROR !!! ARCHIVO DE CREDITO NO EXISTE !! "
	PRINT "        FECHA BUSCADA, MES= "+MES$+" DIA= "+DIA$
	PRINT "		"
	PRINT "		"
	PRINT "		"
	PRINT "		"
	PRINT "		" 
	INPUT "  DIGITE INTRO PARA SALIR.... ";PRT$
	STOP
ENDIF
END

!----------------------------------------------------------------------
!            ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!----------------------------------------------------------------------
