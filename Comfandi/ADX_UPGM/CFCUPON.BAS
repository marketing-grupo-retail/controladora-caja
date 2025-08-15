!             PROGRAMA CFCRIVA.BAS
!           PROYECTO *** COMFANDI Fase 2 ***
!----------------------------------------------------------
!  Este programa  COLOCA los Flags de Plan de USER OPTION a "Y"
!  al Archivo EAMITEMR.DAT, PARA DESCUENTOS Y PROMOCIONES
!
!------------------------------------------------------------

! Ambiente de Controlador
  %ENVIRON C 


! Definicion de variables de usuario locales
  STRING        COD$,      KEY$,         NOM$,           \!
                Z$,        LLAV1$,       KEY.ARRAY$,     \!
                ERRFX$,    FEC$,         ALM$,           \!
                ALMA$,     NUAL$,        NOAL$,          \!
                REGI$,     DEP$,         HORA$,          \!
                IND2$,     RES$,         RESP$,          \!
                KEYS$,     FAMGR$,       CANT$,          \!
                KEI$,      LINK$,        DES$

  INTEGER*1     DMCREQ,    DMCMT,        DMCPO,          \!
                IND0%,     IND1%,        DMCESTAT,       \!
                SW.FIN,    SWREP,        IND1A%,         \!
                SWERROR,   CON.LIN%,     MES

  INTEGER*2     DMCRCSF,   DMCSTSF,      DMCERRCT,       \!
                UEX1%,     UEX2%,        FLAG,           \!
                POINT1,    POINT2,       I%

  INTEGER*4     DMCERROF,  TOTBUE,       TOTCERO,        \!
                TOTLEI,    PEPRREQ%,     PREREQ%,        \!
                PLUPRE%,   SINDES%,      S%, SX%,        \!
                NUM%,      REG%,         ICO%,           \!
                I16%,      I20%,         I35%,           \!
                NOE%

  String GLOBAL GR.DATA$

! -------------  Rutina para crear compaginar Hora y Fecha --------------
  SUB CREAFECHA
      FRA.TIM$ = TIME$                              ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                  ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                 ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                 ! Arma segundos
      FRA.FEC$ = DATE$                              ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                  ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                 ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                 ! Arma dia
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    El: "+FRA.DD$+"/"+FRA.MM$+"/"+ \! de Franqueo con
               FRA.AA$                              ! Hora y Fecha
  END SUB
!----------------------  PROGRAMA  PRINCIPAL  ----------------------------
   TITULO$ = "       ** PRENDE FLAGS PARA DESCUENTOS ARTICULOS **"
   ON ERROR GOTO ERRTRAP                               ! Rutina de Salida ERROR
   ALM$ = "LABORATORIO COMFANDI"                       !
   NOAL$ = "003"                                       !
   MENS$= "  PLU NO EXISTE.. "                         !
   FR$ = CHR$(13) + CHR$(10)                           !
   CLEARS                                              !
   PRINT  :  PRINT  :   PRINT                          !
   PRINT TITULO$                                       !
   PRINT                                               ! Avanza Linea
   PRINT                                               ! Avanza Linea
   PRINT "     Proceso para el Almacen :";ALM$;"  ";NOAL$
   PRINT "             Fecha: ";DATE$                  ! 
   PRINT "      Inicia a las: ";TIME$                  ! 
   PRINT                                               ! Avanza Linea
   OPEN "EAMITEMR" KEYED RECL 77 AS 25                 ! Archivo de Art¡culos
   OPEN "C:\PROMO.DAT"   AS 26 BUFFSIZE 5280           ! Archivo Novedades IVA
   CREATE POSFILE "C:\PROLIS.TXT" AS 27                !
   NUM% = 0                                            !
   IF END #26 THEN CERRAR                              !
LEER:                                                  !
   NUM% = NUM% + 1                                     !
   READ #26;LINE REGI$                                 ! Lee Novedades
   CONSOLE                                             !
   LOCATE 15,15                                        !
   PRINT "Registros Le¡dos: ";STR$(NUM%)               !
   KEY$ = "000000000000"                               !
   KEY$ = RIGHT$(KEY$+STR$(VAL(LEFT$(REGI$,8))),12)    !
   KEY$ = PACK$(KEY$)                                  !
   KEI$ = KEY$                                         !
   ERRLEC$ = " "                                       !
   READ FORM "C6 3I1 C1 C2 C4 C1 C5 C2 C18 2I2 C31";#25   \!  
        KEY KEY$;KEY$,IND0%,IND1%,IND1A%,IND2$,DEP$,  \!
            FAMGR$,CANT$,VAL$,LINK$,DES$,UEX1%,UEX2%, \!
            GR.DATA$
   IF ERRLEC$ <> " " THEN BEGIN                        !
      WRITE FORM "C8 C8 C20 C12 C2";#27;" ",          \!
            STR$(NUM%),MENS$,UNPACK$(KEI$),FR$         !
      NOE% = NOE% + 1                                  !
      GOTO LEER                                        !
   ENDIF                                               !
!   IND1% = 0                                          !
   IF MID$(REGI$,10,2) = "03" THEN BEGIN               !
      I35% = I35% + 1                                  !
      IND1A% = 02H                                     !
   ENDIF                                               !
   WRITE FORM "C6 3I1 C1 C2 C4 C1 C5 C2 C18 2I2 C31";#25; \!  
         KEY$,IND0%,IND1%,IND1A%,IND2$,DEP$,FAMGR$,   \!
         CANT$,VAL$,LINK$,DES$,UEX1%,UEX2%,GR.DATA$    !
   GOTO LEER                                           !
CERRAR:                                                !
   NUM% = NUM% - 1                                     !
   PRINT "      CERRANDO.. a las: ";TIME$              !
   PRINT                                               !
   PRINT " ARTICULOS CON CUPON     ";STR$(I35%)        !
   PRINT "   No existen ";STR$(NOE%)                   !
   PRINT "Tot Novedades ";STR$(NUM%)                   !
   WRITE FORM "C32 C2";#27;                           \!
         "  ** CIFRAS DE CONTROL ** ",FR$              !
   WRITE FORM "C20 PIC(###,###) C2";#27;              \!
         "ITEMS CON CUPON ",I35%,FR$                     !
   WRITE FORM "C20 PIC(###,###) C2";#27;              \!
         "   No existen ",NOE%,FR$                     !
   WRITE FORM "C20 PIC(###,###) C2";#27;              \!
         "Tot Novedades ",NUM%,FR$                     !
   CLOSE 25                                            !
   CLOSE 26                                            !
STOP                                                   ! 
ERRTRAP:
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
    IF ERRF% = 26 THEN BEGIN
       CLEARS
       PRINT
       PRINT
       PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE NOVEDADES DE IVA....!"
       PRINT
       PRINT "               Consulte con el Depto de Sistemas."
       PRINT "                 Crear  C:\promo.txt   "
       STOP
    ENDIF
    IF ERRF% = 25 THEN BEGIN
       IF ERR = "EF" THEN BEGIN
          ERRLEC$ = "**"
          RESUME
       ENDIF ELSE BEGIN
          PRINT "Error en Archivo de NOVEDADES..."
          PRINT "ERR = ";ERR," ERRL =";ERRL
          PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
          PRINT REGI$
       ENDIF
    ENDIF
END
! ********************************************
!***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ********************************************
