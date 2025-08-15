!---------------------------------------------------------
!    PROYECTO COMFAMILIAR ANDI
!    Nombre del Fuente     : CFRELCHQ.BAS
!    Incluir en            : 
!    Fecha de Modificaci¢n : ENERO DE 1995
!    Autor                 : Indata Ltda.
!    Archivos que utiliza  : C:\ADX_UPGM\MEDPAG2.DAT
!
!---------------------------------------------------------
!    Lista cheque
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
       FECHEQUE,           \  !
       FINREC

INTEGER*1 L2,              \  ! Indicador de cambio de nivel
          L1,              \  ! Indicador de cambio de nivel
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
       TRANL1,             \  ! Numero de transacciones
       MONTL1              \  ! Valor de las transacciones

!----------------------------------------
! Seccion de Rutinas
!----------------------------------------
!
SUB TOUTPUT
   IF L1 = 1 THEN BEGIN 
      PRINT
      PRINT USING FORMAT;TRANL1;
      PRINT "  Transacciones   Por Valor de :";TAB(54);
      PRINT USING FORMAT;MONTL1
      PRINT
      OV = OV + 4
      TIPPAGL1 = TIPPAG$
      TRANL1 = 0:MONTL1 = 0
   ENDIF
END SUB

SUB DCALC
   IF SIGNO$ = " " THEN BEGIN
      TRANL1 = TRANL1 + 1
      MONTL1 = MONTL1 + VAL(VALPAG$)
   ENDIF ELSE BEGIN
      MONTL1 = MONTL1 - VAL(VALPAG$)
   ENDIF
END SUB

SUB OVERFLOW
   IF FT = 1 THEN PRINT CHR$(12)
   P%=P% + 1
   PRINT TAB(3);"CFRELCHQ";TAB(23);"COMFAMILIAR ANDI"; \
                 ALMA$;TAB(70);FECHA$
   PRINT TAB(3);HORA$;TAB(33);"RELACION DE CHEQUES";    \
         TAB(70);"PAG.";TAB(75);STR$(P%) 
   PRINT
   PRINT "CONSE.  BC   N§ CHQ   NIT/CC GIRADOR       VALOR      N§  CTA-CTE"
   PRINT "------  --  -------- ---------------  ------------  ---------------" 
   OV = 4
END SUB

SUB HDOUTPUT
!           AND TER <> "001" AND TER <> "002" THEN BEGIN     ! Solo Pagos
   IF LEFT$(TIPPAG$,1) = "2" THEN BEGIN
   IF L1 = 1 THEN CALL OVERFLOW
   IF L1 = 1 THEN BEGIN
      PRINT TAB(25);MEDPAG$(VAL(TIPO$))
      OV = OV + 2
      PRINT
   ENDIF
   IF IR = 1 THEN BEGIN 
	PRINT CONSEC$;TAB(9);BANCO$;TAB(13);   
        PRINT USING FORMCH;VAL(CHEQUE$);TAB(23);    
        PRINT USING FORMCU;VAL(NROCUE$);TAB(40);
        PRINT USING FORMAT;VAL(VALPAG$);
        PRINT SIGNO$;TAB(54);
        PRINT USING FORMCU;VAL(CTACTE$)
     OV = OV + 1
   ENDIF
   ENDIF
END SUB

SUB SETOF
   L2 = 0
   L1 = 0
   IR = 0
END SUB
 
SUB LEER
   READ FORM                                               \ !
       "C1 C6 C4 C9 C1 C8 C14 C3 C4 C6 2C2 C14 C2";        \ !
        #1;TIREG$,CONSEC$,HORATR$,VALPAG$,                 \ !
         SIGNO$,CHEQUE$,CTACTE$,NUMTER,NUMTRA,             \ !
         NUMOPE$,TIPPAG$,BANCO$,NROCUE$,FINREC               !
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

   OPEN "C:\ADX_UDT1\MEDPAG4.DAT" AS 1
   LPRINTER
   PRINT CHR$(12)
   DIM MEDPAG$(25)
   MEDPAG$(0) = "- CHEQ/BCOS LOCALES -"
   MEDPAG$(1) = "- CHEQ/TARJ COMFANDI -"
   MEDPAG$(2) = "- CHEQUES SUBSIDIO -"
   MEDPAG$(3) = "- CHEQUES NOMINA COM  "
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
   MEDPAG$(25) = "      "
   FORMAT = "#######,###"
   FORMCH = "########"
   FORMCU = "##############"
   T$=TIME$:H$=LEFT$(T$,2):M$=MID$(T$,3,2):S$=RIGHT$(T$,2)
   F$=DATE$:AA$=LEFT$(F$,2):MM$=MID$(F$,3,2):DD$=RIGHT$(F$,2)
   FECHA$=DD$+"/"+MM$+"/"+AA$
   HORA$=H$+":"+M$+":"+S$
   WHILE ERRORCOD$ = "  "
      CALL HDOUTPUT
      CALL SETOF
      CALL LEER
   OTRAVEZ:
      IF ERRORCOD$ = "EF" THEN BEGIN
         L2 = 1
         L1 = 1
         GOTO TOTALC
      ENDIF
      IF LEFT$(TIPPAG$,1) < "2" OR                  \! Si es Efectivo ¢
         LEFT$(TIPPAG$,1) > "2" OR                  \! Otr, Red, TCr, Med
         TIREG$ > "3" THEN BEGIN                     ! Reg de Control
         CALL LEER
         GOTO OTRAVEZ
      ENDIF 
      IF FT = 0 THEN BEGIN 
         FT = 1  :  L1 = 1
         TIPPAGL1 = TIPPAG$
      ENDIF
      IF TIPPAG$ = "21" THEN TIPO$ = "0"
      IF TIPPAG$ = "22" THEN TIPO$ = "1"
      IF TIPPAG$ = "23" THEN TIPO$ = "2"
      IF TIPPAG$ = "24" THEN TIPO$ = "3"
      IF TIPPAG$ = "25" THEN TIPO$ = "4"
      IR = 1
      IF TIPPAG$ <> TIPPAGL1 THEN L1 = 1
TOTALC:   
      IF FT = 1 THEN BEGIN 
         CALL TCALC
         CALL TOUTPUT
      ENDIF
      IF ERRORCOD$ = "EF" THEN STOP
      IF OV > 55 THEN CALL OVERFLOW
      CALL DCALC
   WEND
   CONSOLE
   STOP

!**********************************************************  
!                RUTINA PARA ERRORES                      *
!**********************************************************

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
IF ERR = "EF" THEN ERRORCOD$="EF"          ! END FILE.
IF ERR = "OE" THEN ERRORCOD$="OE"          ! ARCHIVO A ABRIR NO EXISTE.
ERRORCOD$ = ERR
!    PRINT "EC=",ERRORCOD$
!    GOTO FIN
    RESUME
   END
!----------------------------------------------------------------------
!            ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!----------------------------------------------------------------------
