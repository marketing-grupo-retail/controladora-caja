!------------------------------------------------------------------------------
!
!          Programa: INVMENU.BAS
!          Proyecto: Inventarios Fisicos
!        Desarrollo: Redsis S.A. 
!             Fecha: Agosto de 2001
!
!------------------------------------------------------------------------------
!
!       Descripci¢n: Programa Menu de Opciones
!
!      NOTA: Compilaci¢n y LINK86 con MACRO COX.BAT
!                              ¢ 
!            LINK86 con librer¡a ADXACRCL.L86
!
!--------------------------------------------------------------------

 %ENVIRON C                                            ! Ambiente Controlador

 STRING      COD1$,     KEY$,     NOM$,      RSHL$,     BATCH$,   \!
             CODC$,     CERO$,    FECHA$,    FECHAPD$ , NOMD$,    \!
             TIT$(1),   LOG$(1),  PREF$(1),                       \!
             ERRFX$,    Z$,       LLAV1$,    REG$,      FINAL$,   \!
             SIRAN$,    FECRE$,   FEMOD$,    FECRED$,   FEMODD$,  \!
             NOVE$,     VARI$,    ERCOD$,     SEQUENCE$,          \!
             FEC$,      ALM$,     INDICAT2$, IND$,      M$(1)      !

 INTEGER*1   T1%,       T2%,      T3%,       T4%,       T5%,  T6%  !

 INTEGER*2   NLO%,      NRE%                                       !
 INTEGER*2   ZERO,      CNTR%,    NUCAR%,    CARDIA%               !
 INTEGER*2   DMCRCSF,   DMCSTSF,  DMCERRCT,  UEX1,      UEX2,     \!
             NUMTRA%,   NUCO%,    PAGI%,     TECLA%,    MES1,     \!
             NU1%,      NU2%,     NU3%,      Z%,        ZZ%,      \!
             II%,       LE%,      NU%,       CAM%,      NUEVO%,   \!
             LI%(1),    TER%(1),  TPV%(1),   LL%,       KK%,      \!
             RAN1%,     RAN2%,    REPE%,     K%,        I%         !

  INTEGER*4   FISCAM%,FISPVE%,                              \! Cant Camb,PtosVenc
              FISG01%,FISG02%,FISG03%,FISG04%,FISG05%,      \! Puntos Ganados  
              FISG06%,FISG07%,FISG08%,FISG09%,FISG10%,      \! Ultimos 12 meses
              FISG11%,FISG12%,FISG13%,                      \!
              FISA01%,FISA02%,FISA03%,FISA04%,FISA05%,      \! Puntos Acumulad
              FISA06%,FISA07%,FISA08%,FISA09%,FISA10%,      \! Ultimos 12 meses
              FISA11%,FISA12%,FISA13%,                      \!
              FISR01%,FISR02%,FISR03%,FISR04%,FISR05%,      \! Puntos Redimidos
              FISR06%,FISR07%,FISR08%,FISR09%,FISR10%,      \! Ultimos 12 meses
              FISR11%,FISR12%,FISR13%                        !

 INTEGER*4   TOT1%,      TOT2%,       TOT3%,      NODO%,    \! 
             ANOS%,      MESE%,       DIAS%,                \!
             BARR%,      TELRI%,      DES%, HAS%,           \!
             TOPE%,      W%,          J%,   S%,   P%,       \!
             REG%,       CNT%,        SX%,  HX%,  SUM%,     \!
             REGC%,      RESTA%,      USERE%,     VRTRAN%,  \! 
             COD1%,      COD2%,       COMP%,                \! 
             DMCERROF,   TOTBUE,      TOTCERO,              \!
             MAXTRU,     TCO%,        TCD%,                 \!
             FISCON%,    FISNEQ%,     FISNZE%,              \! Fecha X y Z Contad.
             NUME%(1)                                        !

  INTEGER*1     CONLI,     SWERROR,      SWREP,   FINBA%     !
  INTEGER*1     DMCREQ,    DMCMT,        DMCPO,   INDICAT0, \!
                INDICAT1,  DMCESTAT,     FIRST,   SW.FIN,   \!
                SW0,       INDICAT1A                        \!

!----------------------------------------------------------------------------
!       Funci¢n para Desplegar Mensajes y hacer Entradas al LOG
!----------------------------------------------------------------------------
FUNCTION ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE) EXTERNAL
    INTEGER*1    TERM,   MSGNUM,  ADXERROR              !
    INTEGER*2    EVENT,  MSGGRP,  SEVERITY              !
    STRING       UNIQUE                                 !
END FUNCTION                                            !

!----------------------------------------------------------------------------
!           Rutina de Calculo de Digito de Chequeo Modulo 10
!----------------------------------------------------------------------------
SUB DIGICHECK                                           !
    NU1% = 0  : NU2% = 0  : NU3% = 0                    !
    FOR II% = 1 TO 11 STEP 2                            !
        NU1% = NU1% + VAL(MID$(EANPCA$,II%,1))          !
    NEXT II%                                            !
    FOR II% = 2 TO 12 STEP 2                            !
        NU2% = VAL(MID$(EANPCA$,II%,1)) * 2             !
        NU2$ = RIGHT$("00" + STR$(NU2%),2)              !
        NU3% = NU3%+VAL(LEFT$(NU2$,1)) +               \!
                    VAL(RIGHT$(NU2$,1))                 !
    NEXT II%                                            !
    NU1% = NU1% + NU3%                                  !
    NU2% = 10 - VAL(RIGHT$(STR$(NU1%),1))               !
    DIGCTL$ = RIGHT$(STR$(NU2%),1)                      !
END SUB                                                 !
!------------------------------------------------------------------------------
!        Subrutina para Recuperar Cant Afiliados del Dia
!------------------------------------------------------------------------------
SUB CUANTOS.NUEVOS                                      !
!     LOCATE 1,1 : PRINT "ABRIR3.."; : INPUT "  ";ZZZ$
    OPEN ARCH4$ KEYED  RECL 259 AS 4                    ! Abre Variabl
    FOR II% = 1 TO 9999                                 !
        LOCATE 23,50 : PRINT "Busca Afiliados: ";II%    !
        KEY$=PACK$(DATE$+"99"+RIGHT$("0000"+STR$(II%),4))  
        ERCOD$ = "  "                                   ! Arma llave para
        READ FORM "2C6 C3 C244";#4 KEY KEY$;           \! leer Reg Especiales
             FISFEA$,FISNIT$,FISEAN$,FISUSR$            ! de Afiliados dia
        IF ERCOD$ <> "  " THEN BEGIN                    ! Si NO encuentra
           WAIT ; 1500                                  !
           CARDIA% = II% - 1                            ! Reg Especial
           II% = 9999                                   ! Asigna Cant de
        ENDIF                                           ! CARNETS del dia
    NEXT II%                                            !
    CLOSE 4                                             !
END SUB                                                 !      
!----------------------------------------------------------------------------
!            Rutina para crear un Batch Control Record (PRB)
!----------------------------------------------------------------------------
  SUB CREAPRB
      SEQU$    = PACK$("01")                            !
      KEY.VAL$ = PACK$(TIME$) + SEQU$                   ! Llave de control
      LABE$    = "ML:"+RIGHT$("000"+STR$(CNTR%),3)+"..A"!
      WRITE FORM "C4 C9 2I2 C7";#14;                   \!
            KEY.VAL$,LABE$,NUCAR%,0,"A"                 !
  END SUB                                               !

!------------------------------------------------------------------------------
!         Rutina para CREAR BATCH con Carnets de Cliente Amigo
!------------------------------------------------------------------------------
  SUB CREAFILE
!   CALL LEERCTRL                                       ! Lee Reg Ctrl BATCH
    FILE.NAME$ = "C:\ADX_IDT1\EAMSL" +                 \!
                 RIGHT$("000"+STR$(CNTR%),3)            !
    CREATE POSFILE FILE.NAME$ AS 11                     ! Se crea acceso excl
    CARNETS$ = "C:\ADX_UDT1\CARN"+RIGHT$(DATE$,4)+".TXT"! Se crean CARNETS
    CREATE POSFILE CARNETS$ AS 12                       ! Se crea acceso excl
    FIRST = 1                                           ! 
  END SUB                                               !

!------------------------------------------------------------------------------
!            Rutina para grabar Registros en Archivo EASL:nnn
!------------------------------------------------------------------------------
  SUB GRABAREG
    IF OPC$ = "1" THEN BEGIN                            !
       NUCAR% = NUCAR% + 1                              ! Incr Cant CARNETS
       CARDIA% = CARDIA% + 1                            ! Incr CARNETS Dia
       NUCAR$ = RIGHT$("0000"+STR$(CARDIA%),4)          !
       FISFEA$ = PACK$(DATE$+"99"+NUCAR$)               !
       WRITE FORM "2C6 C3 C244";#4;                    \! Graba Reg especial 
          FISFEA$,FISNIT$,"EAN",PACK$(STRING$(488,"0")) ! que indica quien
    ENDIF                                               !
    EANPCA$ = "1"+RIGHT$(UNPACK$(FISNIT$),11)           ! se ha afiliado
    CALL DIGICHECK                                      !
    WRITE FORM "C5 C10 2C12 C1 C2 C9 C14 C3";#11;      \! Formato 1 Shelf Lab
          PREFIJO$,FISAPE$,EANPCA$,EANPCA$,DIGCTL$,    \! "1"," Apellidos,EAN
          UNPACK$(FISBAR$),RIGHT$(UNPACK$(FISTRE$),7), \! NIT/DigCtrl, Barrio
          FISNOM$,FINR$                                 ! Tel, Nombres
    AFILIAC$ = UNPACK$(FISAFI$)                         !
    T1% = MATCH(" ",FISAPE$,1)                          !    
    T2% = MATCH(" ",FIS2AP$,1)                          !    
    T3% = MATCH(" ",FISNOM$,1)                          !    
    IF T1% = 0 THEN T1% = 13                            !
    IF T2% = 0 THEN T2% = 13                            !
    IF T3% = 0 THEN T3% = 21                            !
    APENOM$ = LEFT$(FISAPE$,T1%-1) + " "                !
    IF T2% > 1 THEN                                    \!
       APENOM$ = APENOM$ + LEFT$(FIS2AP$,T2%-1) + " "   !
    APENOM$ = APENOM$ + FISNOM$                         !
    IF OPC$ = "3" THEN NUCAR% = NUCAR% + 1              ! Incr Cant CARNETS
    WRITE FORM "C12 C1 C40 C1 C8 C2";#12;              \!
          EANPCA$,",",APENOM$,",",AFILIAC$,FR$          ! Apell,Nombres,Cli,Fec
  END SUB

!------------------------------------------------------------------------------
!                   Rutina final de cierre del archivo
!------------------------------------------------------------------------------
  SUB FINSHELF                                          !
    IF OPC$ = "3" OR                                   \!
       NUCAR% > 0 THEN BEGIN                            !
       CLOSE 11                                         ! Cierra EAMSL:nnn
       CALL CREAPRB                                     ! Graba Control
       CLOSE 14                                         ! Cierra EAMSLCTL.DAT
       FIRST = 0  :  NUCAR% = 0                         !
    ENDIF                                               !
  END SUB                                               ! 
!----------------------------------------------------------------------------
!            Funci¢n para Solicitar Servicios de la Aplicaci¢n
!----------------------------------------------------------------------------
SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL            !
    STRING       PARM2$                                 !
    INTEGER*4    RET                                    !
    INTEGER*2    FUNC,   PARM1                          !
END SUB                                                 !
!----------------------------------------------------------------------------
!                  Subrutina para Toma de Teclas de Funcion
!----------------------------------------------------------------------------
SUB TECLA                                               !
  WHILE NOT CONSTAT%                                    !
    IF VIS% = 0 THEN EN% = INKEY                        !
    IF VIS% = 1 THEN EN% = CONCHAR%                     !
    SW2 = SW2 + 1                                       !
    IF SW2 = 1 THEN EN$ = CHR$(EN%) :  EL%=EN%          !
    EN$ = CHR$(EN%)                                     !
    P% = CONSTAT%                                       !
    IF NOT P% THEN EXIT SUB                             !
  WEND                                                  !
    EN% = INKEY                                         !
    IF EN% = 80 THEN EN$ = "F6"                         !
    IF EN% = 81 THEN EN$ = "F7"                         !
    IF EN% = 82 THEN EN$ = "F8"                         !
    IF EN% = 83 THEN EN$ = "F1"                         !
    IF EN% = 84 THEN EN$ = "F2"                         !
    IF EN% = 85 THEN EN$ = "F3"                         !
    IF EN% = 86 THEN EN$ = "F4"                         !
    IF EN% = 87 THEN EN$ = "F5"                         !
END SUB                                                 !
!----------------------------------------------------------------------------
!                  Subrutina para Linea de Teclas de Funcion
!----------------------------------------------------------------------------
SUB FUNCKEY                                             !
   LOCATE 23,1     : PRINT STRING$(80," ")              !
   LOCATE 23,1                                          !
   PRINT "F1"      ; C27$ + "p" ; C27$ + B03$;          !
   PRINT " Ayuda " ; C27$ + "q" ; C27$ + B07$;          !
   PRINT " F2"     ; C27$ + "p" ; C27$ + B03$;          !
   PRINT "       " ; C27$ + "q" ; C27$ + B07$;          !
   PRINT "       " ;                                    !
   PRINT " F3"     ; C27$ + "p" ; C27$ + B03$;          !
   PRINT " Salir " ; C27$ + "q" ; C27$ + B07$;          !
END SUB                                                 !

!------------------------------------------------------------------------------
!                   Subrutina de Borrar Variables
!------------------------------------------------------------------------------
SUB LIMPIA                                              !
  FOR II% = 12 TO 22                                    !
      LOCATE II%,1                                      !
      PRINT STRING$(80," ")                             !
  NEXT II%                                              !
END SUB                                                 !
!------------------------------------------------------------------------------
!                   Subrutina de Fin de Pantalla
!------------------------------------------------------------------------------
SUB FINPANTA                                            !
  NOTECLA:                                              !
    VIS% = 0                                            !
    CALL FUNCKEY                                        !
    LOCATE 23,1                                         !
    PRINT "      Para Continuar, presione ENTER ";      !
    CALL TECLA                                          !
    IF EN$ <> "F3" AND EN% <> 13 THEN GOTO NOTECLA      !
    IF EN$ = "F3" THEN CLEARS : STOP                    !
END SUB                                                 !
!----------------------------------------------------------------------------
!                     Funci¢n para Toma de Datos
!----------------------------------------------------------------------------
FUNCTION DATO$(TX$,FI%,CL%,TI$,LN%)                     ! Fila,Col,Tipo y Long
  INTEGER*2 FI%, CL%, LN%                               !
  STRING    TX$, TI$, DATO$                             !
!        LOCATE 1,1 : PRINT ".";DATO1$;"."
  LOCATE FI%,CL%                                        ! Ubica Cursor
  PRINT C27$+"p" ; C27$ + B02$;                         ! Inic Video Rever/Verde
  IF TX$ = "" THEN PRINT STRING$(LN%," ");    ELSE     \! Despliega Texto
     PRINT TX$;                                         !
  PRINT C27$+"q" ; C27$+B07$                            ! Fin Video Rever/Gris
  LOCATE FI%,CL%                                        ! Ubica Cursor
  CL% = CL% + LEN(TX$)                                  ! Modifica Columna +
  IF CL% > 75 THEN BEGIN                                !
     DAT$ = TX$                                         !
     CL% = CL% - LEN(TX$)                               ! Modifica Columna -
  ENDIF                                                 !
  LOCATE FI%,CL%                                        ! Ubica Cursor
  DAT$ = ""                                             ! Inicia en nulos
  FOR I% = 1 TO LN%                                     ! Inicia Loop Digitacion
      IF OPC$  = "" THEN VIS% = 0                       ! Caracteres Invisibles
      IF OPC$ <> "" THEN VIS% = 1                       ! Caracteres Visibles
      SW2 = 0 : EL% = 0                                 !
      CALL TECLA                                        !
      X% = EN%                                          !
      IF X% = 27 THEN EN$ = "F3"                        !
      IF EN$ = "F3" THEN GOTO REGRESO                   !
      IF X% = 9  THEN X%  =  13 : EN% = X%              !
      IF X% = 13 THEN BEGIN                             ! Si es Enter
         IF LEN(DAT$)=0 AND SW1=2 THEN DAT$=DATO1$      !
         IF TI$ = "A" THEN BEGIN                        ! con letrero
            DAT$ = LEFT$(DAT$+STRING$(LN%," "),LN%)     ! Ajusta Spaces
         ENDIF ELSE BEGIN                               ! Si son
            IF TI$ = "9" THEN BEGIN                     ! numeros
               DAT$ = RIGHT$(STRING$(LN%,"0")+DAT$,LN%) ! Ajusta CEROS
            ENDIF                                       ! y termina
         ENDIF                                          !
         I% = LN%                                       ! Reset de I
         DATO$ = DAT$                                   !
         IF DAT$ <> STRING$(LEN(DAT$)," ") THEN BEGIN   ! 
            LOCATE FI%,CL%                              ! Ubica Cursor
            PRINT C27$+"p" ; C27$+B03$;                 ! Inic Video Rever/Cyan
            PRINT DAT$;                                 ! Despliega Texto
            PRINT C27$+"q" ; C27$+B07$                  ! Fin Video Rever/Gris
         ENDIF                                          !
         EXIT FUNCTION                                  !
      ENDIF                                             !
      IF I% > 1 THEN BEGIN                              !
         IF (EL%=8 OR EL%=27 OR EL%=127) AND           \!
            (X%=8 OR X%=68 OR X%=127) THEN BEGIN        !
            DAT$ = LEFT$(DAT$,LEN(DAT$)-1)              !
            LOCATE FI%,CL%                              !
            PRINT DAT$ + "  "                           !
            I% = I% - 1                                 !
            X% = 0                                      !
         ENDIF                                          !
      ENDIF                                             !
      CAR$ = ""                                         !
      IF EL% = X% THEN CAR$ = CHR$(X%)                  ! Guarda Letra/Caracter
      IF TI$ = "A" THEN BEGIN                           ! Val Letras/Carac Esp
         II% = MATCH(CAR$,                             \!
               " ABCDEFGHIJKLMN¥OPQRSTUVWXYZ"    +     \!
               ".,:;-#/()1234567890",1)                 !
      ENDIF                                             !
      IF TI$ = "a" THEN BEGIN                           ! Val Letras/Carac Esp
         II% = MATCH(CAR$,                             \!
               " abcdefghijkklmn¤opqrstuvwxyz"    +    \!
               ".,:;-+/*#@%()1234567890",1)             !
      ENDIF                                             !
      IF TI$ = "9" THEN BEGIN                           ! Val Numeros
         II% = MATCH(CAR$,"0123456789",1)               !
      ENDIF                                             !
      IF II% = 0 THEN I%=I%-1 : LOCATE FI%,CL%+I%       ! NO esta en Match.Repi
      IF II% > 0 THEN DAT$ = DAT$ + CHR$(X%)            ! Arma letrero
  NEXT I%                                               !
  LOCATE FI%,CL%                                        ! Ubica Cursor
  PRINT C27$+"p" ; C27$+B03$;                           ! Inic Video Rever/Cyan
  PRINT DAT$;                                           ! Despliega Texto
  PRINT C27$+"q" ; C27$+B07$                            ! Fin Video Rever/Gris
  DATO$ = DAT$                                          !
  EXIT FUNCTION                                         !
  REGRESO:                                              !
  IF VIS% = 0 THEN BEGIN                                !
     CALL FINSHELF                                      !
     CLEARS                                             !
     STOP                                               ! 
  ENDIF                                                 !
FEND                                                    !
!----------------------------------------------------------------------------
!                     Funci¢n para Despliegue Datos
!----------------------------------------------------------------------------
FUNCTION RLIN(TX$,FL%,CL%)                              ! Func/Despl. Data
   IF TX$ = STRING$(LEN(TX$),"0") AND                  \!
      LEN(TX$) > 1 THEN BEGIN                           !
          TX$ = STRING$(LEN(TX$)," ")                   !
   ENDIF                                                !
   LOCATE FL%,CL%                                       ! Ubica Cursor
   PRINT C27$+"p" ; C27$+B03$;                          ! Inic Video Rever/Rojo
   PRINT TX$                                            ! y la muestra
   PRINT C27$+"q" ; C27$+B07$;                          ! Fin Video Rever/Gris
FEND                                                    !
!----------------------------------------------------------------------------
!                     Funci¢n para Despliegue de Errores
!----------------------------------------------------------------------------
FUNCTION VERROR(VERROR$)                                ! Func/Despl. Errores
   LOCATE 23,1                                          ! Ubica Cursor
   PRINT C27$ + B04$;                                   ! Inic Video Rever/Rojo
   PRINT VERROR$;                                       ! y la muestra
   PRINT C27$+B07$;                                     ! Fin Video Rever/Gris
   CALL TECLA                                           ! Espera desbloqueo
   LOCATE 23,1                                          ! Ubica Cursor  
   PRINT STRING$(70," ");                               ! y Borra Linea
FEND                                                    !

!----------------------------------------------------------------------------
!                     Funci¢n para Re-desplegar Datos
!----------------------------------------------------------------------------
SUB REFRESH                                             !
  LOCATE 17,1  :  PRINT STRING$(80," ")                 !
  LOCATE 18,1  :  PRINT STRING$(80," ")                 !
  LOCATE 18,15                                          !
  PRINT "RESUMEN HISTORICO DEL PROGRAMA (20";           !
  PRINT RIGHT$("00"+STR$(VAL(LEFT$(DATE$,2))-1),2);     !
  PRINT "/20";RIGHT$("00"+LEFT$(DATE$,2),2);")"         !
  I% = VAL(MID$(DATE$,3,2))                             !
  PRINT "       ";                                      !
  FOR II% = 1 TO 12                                     !
      I% = I% + 1                                       !
      IF I% > 12 THEN I% = I% - 12                      !
      PRINT M$(I%);"  ";                                !
  NEXT II%                                              !
  PRINT "*TOTAL*"                                       !
 !CALL SUMAR.PUNTOS                                     !
  PRINT STRING$(80," ")                                 !
  CALL FUNCKEY                                          !
END SUB                                                 !
!----------------------------------------------------------------------------
!                   Funci¢n para Toma de Llave y Lectura
!----------------------------------------------------------------------------
SUB TOMANIT                                             !
  ERCOD$ = "  "                                         !
  IF CARN% <> 1 THEN NIT1$=DATO$(TEX$,ROW%,CLM%,"9",12) !
  KEY$ = PACK$(NIT1$)                                   !
  NIT2$ = MID$(NIT1$,7,3)                               !
  NIT3$ = RIGHT$(NIT1$,3)                               !
  NIT1$ = STR$(VAL(LEFT$(NIT1$,6)))                     !
  IF VAL(NIT1$) + VAL(NIT2$) +                         \!
     VAL(NIT3$)+VAL(DIGI$) = 0 THEN BEGIN               !
     ERCOD$ = "ER"                                      !
     MENS$ = "ERROR...  NIT Inv lido..!"                !
     CALL VERROR(MENS$)                                 !
     EXIT SUB                                           !
  ENDIF                                                 !
  IF VAL(NIT1$) = 0 THEN BEGIN                          !
     NIT1$ = RIGHT$("        "+NIT2$,8)+"."+NIT3$       !
  ENDIF ELSE BEGIN                                      !
     NIT1$ = RIGHT$("     "+NIT1$,6)+"."+NIT2$+"."+NIT3$!
  ENDIF                                                 !
  CALL RLIN(NIT1$,ROW%,CLM%)                            !
  IF OPC$ = "3" THEN CALL RLIN(NIT1$,ROW%+17,CLM%)      !
END SUB                                                 !

SUB MARCO                                               !
  DEZ% = 25                                             !
  IF NU% = 13 OR NU% = 16 THEN DEZ% = 1                 !
  LOCATE 17,DEZ%  :   PRINT MAR1$                       !
  LOCATE 18,DEZ%  :   PRINT MAR2$                       !
  LOCATE 19,DEZ%  :   PRINT MAR2$                       !
  LOCATE 20,DEZ%  :   PRINT MAR2$                       !
  LOCATE 21,DEZ%  :   PRINT MAR2$                       !
  LOCATE 22,DEZ%  :   PRINT MAR2$                       !
  LOCATE 23,DEZ%  :   PRINT MAR2$                       !
  LOCATE 23,01    :   PRINT STRING$(80," ")             !
  LOCATE 23,DEZ%  :   PRINT MAR3$;                      !
END SUB                                                 !

SUB AYUDABAR                                            !
  CALL MARCO                                            !
  DES% = 0  :  HAS% = LEN(STRBAR$)/19 : RE$ = "A"       !
  WHILE RE$ <> "S"                                      !
        LOCATE 23,01                                    !
        PRINT "Sale/Av/Reg: S/A/R ? ";                  !
        IF DES% <> 0 THEN RE%=CONCHAR% : RE$=CHR$(RE%)  !
        IF I%*19 > LEN(STRBAR$) THEN                   \!
           DES%=LEN(STRBAR$)/19+MOD(LEN(STRBAR$),4)  : \!  
           RE$ = "R"                                    ! 
        IF RE$ = "R" THEN BEGIN                         !
           IF DES% < 08 THEN                           \!
              DES% = 1                                 \!
           ELSE                                        \!
              DES% = DES% - 08  : RE$ = "A"             !
        ENDIF                                           !
        IF RE$ = "A" THEN BEGIN                         !
           DES% = DES% + 1                              !
           TOPE% = DES% + 3                             !
           II% = 0  :  LOCATE 18,35                     !
           PRINT C27$ + B06$;                           ! Inic Video Brown
           PRINT "** AYUDA **"                          !
           PRINT C27$ + B07$                            ! Fin Video Rever/Gris
           FOR I% = DES% TO TOPE%                       ! 
               DEPTELR$ = MID$(STRBAR$,I%*19-17,18)     !
               IF I%*19 > LEN(STRBAR$) THEN            \!
                  DEPTELR$ = STRING$(20," ")            !
               II% = II% + 1                            !
               LOCATE 18+II%,30                         !
               PRINT C27$ + B06$;                       ! Inic Video Brown
               PRINT DEPTELR$;                          !
               PRINT C27$ + B07$                        ! Fin Video Rever/Gris
           NEXT I%                                      !
           DES% = DES% + 3                              !
        ENDIF                                           !
  WEND                                                  !
END SUB                                                 !

!----------------------------------------------------------------------------
!               Subrutina para Desplegar Ayudas en Pantalla
!----------------------------------------------------------------------------
SUB AYUDA                                               !
  CALL MARCO                                            !
  FOR I% = K% TO W% STEP 1                              !
      IF LEFT$(AY$(I%),1) = " " AND                    \!
         RIGHT$(AY$(I%),1)<>"*" THEN GOTO BLANCO        !
      IF I% > K% THEN BEGIN                             !
         IF RIGHT$(AY$(I%),2)="**" THEN BEGIN           !
            I% = W%                                     !
            WW% = 6                                     !
            GOTO AVAREG                                 !
         ENDIF                                          !
      ENDIF                                             ! 
      WW% = WW% + 1                                     !
      IF WW% > 5 THEN WW% = 1                           !
      LOCATE 17+WW%,DEZ%+3                              !
      PRINT C27$ + B06$;                                ! Inic Video Brown
      PRINT LEFT$(AY$(I%),34);                          !
      PRINT C27$ + B07$                                 ! Fin Video Rever/Gris
   AVAREG:                                              !
  !   PRINT WW%, I%, W% ; : INPUT "  " ; XZX$
     IF WW% > 5  AND I% < W%+1 THEN BEGIN               !
        LOCATE 23,01                                    !
        PRINT "Sale/Av/Reg: S/A/R ? ";                  !
        RE% = CONCHAR%                                  !
        RE$ = CHR$(RE%)                                 !
        IF RE$ <> "S" AND                              \!
           RE$ <> "A" AND                              \!
           RE$ <> "R" THEN GOTO AVAREG                  !
        IF RE$="A" AND LEFT$(AY$(I%),1)=" " THEN RE$="R"!
        IF RE$ = "R" THEN BEGIN                         !
           IF I%-KK% > 11 THEN I%=I%-12 : K%=I%  ELSE  \!
              I% = I% - 5                               !
        ENDIF                                           !
        IF RE$ = "A" THEN K% = I%                       !
        IF RE$ = "S" THEN I% = W%                       !
     ENDIF                                              !
  BLANCO:                                               !
  NEXT I%                                               !
END SUB                                                 !

SUB BORRAR                                              !
  FOR I% = 1 TO 11                                      !
      LOCATE I% + 12 , 1                                !
      IF I% < 4 THEN                                   \!
         I% = I%                                       \!
      ELSE                                             \!
         PRINT STRING$(70," ")                          ! 
  NEXT I%                                               !
  PRINT STRING$(70," ");                                !
END SUB                                                 !
!------------------------------------------------------------------------------
!                 Rutina para Verificar Registro del Cliente
!------------------------------------------------------------------------------

SUB VERICLI                                             !
    IF OPC$ <> "3" THEN OPEN ARCH4$ KEYED RECL 259 AS 4 ! Abre Variabl
    ERCOD$ = "  "                                       ! Inicia Estado Leer
    LEC$ = "C6 C1 2C5 2C12 C20 C30 C40 6C4 26I4"        !
    READ FORM LEC$;#4 KEY KEY$;FISNIT$,                \! Lee Reg Cliente
         FISBAR$,FISTRE$,FISTOF$,FISAPE$,FIS2AP$,      \! Barr,Tel,Ofic,Ape,2Ap
         FISNOM$,FISDIR$,FISMAI$,FISNAC$,FISAFI$,      \! Nom,Dir,Mail,Nac,Afil
         FISRED$,FISULC$,FISUCA$,FISCOR$,              \! Rede,UltC,FCamb,FCort
         FISCAM%,FISPVE%,                              \! Cant Camb,PtosVenc
         FISG01%,FISG02%,FISG03%,FISG04%,FISG05%,      \! Puntos Ganados  
         FISG06%,FISG07%,FISG08%,FISG09%,FISG10%,      \! Ultimos 12 meses
         FISG11%,FISG12%,                              \!
         FISR01%,FISR02%,FISR03%,FISR04%,FISR05%,      \! Puntos Redimidos
         FISR06%,FISR07%,FISR08%,FISR09%,FISR10%,      \! Ultimos 12 meses
         FISR11%,FISR12%                                !
    IF ERCOD$ = "  " THEN BEGIN                         ! Si existe Registro
       OBSER$ = "Y"                                     !
    ENDIF ELSE BEGIN                                    ! Fin Cond
       OBSER$ = "N"                                     ! Error Empresa
    ENDIF                                               ! Fin Cond
    IF LEFT$(FISTOF$,3) = "EAN" THEN BEGIN              !
       OBSER$ = "N"                                     !
       ERCOD$ = "EF"                                    !
    ENDIF                                               !
    IF OPC$ <> "3" THEN CLOSE 4                         !
END SUB                                                 !
!----------------------------------------------------------------------------
!                     Subrutinas para Despliegue de Pantallas
!----------------------------------------------------------------------------
SUB PANTA0
CLEARS                                                   !
PRINT "  INVHL001           OPERACION DE INVENTARIO FISICO DE ALMACEN"
PRINT   :  PRINT                                         !
PRINT "             ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
PRINT "             º                                                      º"
PRINT "             º   Este Programa puede  ser usado para Inicializar ¢  º"
PRINT "             º   Modificar Variables del Programa de toma de        º"
PRINT "             º   Inventarios Fisicos de Almacen                     º"
PRINT "             º                                                      º"
PRINT "             ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
CALL FINPANTA                                            !
OPC$ = ""                                                !
END SUB                                                  !

!----------------------------------------------------------------------------
!               Subrutina para Desplegar Ayudas en Pantalla
!----------------------------------------------------------------------------
SUB AYUDAOP                                              !
CLEARS                                                   !
PRINT "INVHL002         AYUDA : TOMA DE INVENTARIO FISICO           P g 1 de 1"
PRINT C27$ + B02$
PRINT "   1  ASIGNACION DE       Asigne SOLO las Terminales que intervienen "
PRINT "      TERMINALES          en la toma del Inventario en la fecha y hora"
PRINT "                          asignadas."
PRINT "   2  LOCALIZACIONES      Indique la cantidad de localizaciones que van"
PRINT "                          a tener Inventario. Consulte el plano fisico"
PRINT "                          del Almacen. Se controlara la toma sobre el 100%"
PRINT
PRINT "   3  AVANCE DEL          Le permite revisar el estado de cada Localizacion"
PRINT "      INVENTARIO          los conteos y los ajustes al conteo principal"
PRINT
PRINT "   4  REPORTES            Permite elaborar listados comparativos de conteos,"
PRINT "                          por Localiz/Terminal."
PRINT
PRINT "   5  INTERFACES          Permite obtener los archivos definitivos de la toma"
PRINT "                          fisica del Inventario consolidado por PLU"
PRINT                                                    !
PRINT "   6  "                                           !
PRINT "      "                                           !                     
PRINT C27$ + B04$                                        !
PRINT "Presione F1 para Regresar"                        !
PRINT C27$ + B07$                                        !
END SUB                                                  !
                                                     
SUB PANTA4                                               !
PAN4:                                                    !
CLEARS                                                   !
LOCATE 4,1                                               !
PRINT " INVHL003      PROGRAMA DE TOMA DE INVENTARIO FISICO"
PRINT                                                    !
PRINT "            Seleccione una de las siguientes Opciones:"
PRINT  :  PRINT                                          !
FOR I% = 1 TO 8                                          !
    IF TIT$(I%) <> " " THEN BEGIN                        !
       LOCATE 7+I%,15                                    ! 
       PRINT TIT$(I%)                                    !
    ENDIF                                                !
NEXT I%                                                  !
OPCION:                                                  !
LOCATE 15,1                                              !
PRINT "            Digite el n£mero Seleccionado.  "     !
PRINT  :  PRINT                                          !
PRINT ALMA$                                              !
PRINT "          MATERIAL BAJO LICENCIA - PROPIEDAD DE: RedSis Ltda(RS)"
CALL FUNCKEY                                             !
LOCATE 15,44                                             !
CALL TECLA                                               !
IF EN$ = "F3" THEN BEGIN                                 !
   CALL FINSHELF                                         !
   CLEARS                                                !
   STOP                                                  !
ENDIF                                                    !
IF EN$ = "F1" THEN BEGIN                                 !
   CALL AYUDAOP                                          !
   LOCATE 22,30                                          !
   CALL TECLA                                            !
   GOTO PAN4                                             !
ENDIF                                                    !
EN% = EN% - 48                                           !
IF EN% < 1 OR EN% > 6 THEN GOTO OPCION                   !
TITU$ = RIGHT$(TIT$(EN%),38)                             !
END SUB                                                  !
SUB PANTA1                                               !
    CLEARS                                               !
    LOCATE 01,15 : PRINT  "PARAMETROS PARA TOMA DE INVENTARIO FISICO"
    LOCATE 03,10 : PRINT "           Almacen No.: ";     !
                   PRINT NSTOR$;"   Nombre:  ";NEGO$     !
    LOCATE 05,10 : PRINT "Fecha Inicio(AAAAMMDD):";      !
                   PRINT "                Hora: "        !
    LOCATE 06,10 : PRINT "     Termina(AAAAMMDD):";      !
                   PRINT "                Hora: "        !
    LOCATE 08,10 : PRINT "    No. Localizaciones: "      !
    LOCATE 09,10 : PRINT "    No. Refer/Localiz.: "      !
    LOCATE 10,10 : PRINT " Status del Inventario: "      !
    LOCATE 23,1  :  PRINT STRING$(70," ");               !
END SUB                                                  !

SUB PANTA6                                               !
CAR1$ = "1.  Fecha del Inventario: "                     !
CAR2$ = "2.  Localizaciones Activas por Terminal"        !
CAR3$ = "3.  Terminales Activas en Inventario"           !
CAR4$ = "4.  No. de Transaccion: "                       !                                              !
CLEARS                                                   !
LOCATE 4,1                                               !
PRINT "INVHL006               ";                         !
PRINT "AVANCE DEL INVENTARIO FISICO"                     !
PRINT "                        ";                        !
PRINT C27$+"p";" Consultas y Estado del Inventario ";C27$+"q"                           !
PRINT                                                    !
PRINT "            Seleccione una de las siguientes Opciones:"
LOCATE 10,15 : PRINT CAR1$                               !
LOCATE 11,15 : PRINT CAR2$                               !
LOCATE 12,15 : PRINT CAR3$                               !
LOCATE 13,15 : PRINT CAR4$                               !
OPCION6:                                                 !
LOCATE 15,1                                              !
PRINT "            Digite el n£mero Seleccionado.   "    !
CALL FUNCKEY                                             !
LOCATE 15,44                                             !
CALL TECLA                                               !
EN% = EN% - 48                                           !
IF EN$ = "F3" THEN EXIT SUB                              !
CARN% = EN%                                              !
IF EN% < 1 OR EN% > 4 THEN GOTO OPCION6                  !
IF EN% = 1 THEN TITU$ = CAR1$                            !
IF EN% = 2 THEN TITU$ = CAR2$                            !
IF EN% = 3 THEN TITU$ = CAR3$                            !
IF EN% = 4 THEN TITU$ = CAR4$                            !
END SUB                                                  !

SUB PANTA7                                              !
CLEARS                                                  !
PRINT "  PCAMI007              AFILIACION AL PROGRAMA ""CLIENTE AMIGO"""
PRINT "                        ";                       !
PRINT C27$+"p";" Elaboraci¢n de Carnets-Programa PCA ";C27$+"q"                           !
LOCATE 07,20  :  PRINT "    Apellidos"                  !
LOCATE 08,20  :  PRINT                                  !
LOCATE 10,20  :  PRINT "      Nombres"                  !
LOCATE 12,20  :  PRINT "    Direcci¢n"                  !
LOCATE 14,20  :  PRINT " Tel‚fono Res"                  !
LOCATE 16,20  :  PRINT "       Barrio"                  !
LOCATE 18,20  :  PRINT " Ptos/Acumul."                  !
END SUB                                                 !

SUB ERROR1                                              !
CLEARS                                                  !
LOCATE 5,1                                              !
PRINT "  VSUHL001              INICIALIZACION DE VARIABLES "
LOCATE 11,1                                             !
PRINT "      Usted debe INSTALAR previamente el Archivo de Seguridad ..!!"
LOCATE 17,1                                             !
PRINT "                          Para Continuar, Presione ENTER"
CALL TECLA                                              !
CLEARS                                                  !
STOP                                                    !
END SUB                                                 !

SUB ERROR2                                              !
CLEARS                                                  !
LOCATE 5,1                                              !
PRINT "  VSUHL001      PERSONALIZACION PARA INVENTARIOS FISICOS"
LOCATE 11,1                                             !
PRINT "     ..E R R O R...!  No EXISTE  el Archivo de Control del Almac‚n .!!"
LOCATE 17,1                                             !
PRINT "                          Para Continuar, Presione ENTER"
CALL TECLA                                              !
CLEARS                                                  !
STOP                                                    !
END SUB                                                 !
!----------------------------------------------------------------------------
!                Rutina para Crear/Compaginar Hora y Fecha 
!----------------------------------------------------------------------------

  SUB CREAFECHA                                         ! 
      FRA.TIM$ = TIME$                                  ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                      ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                     ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                     ! Arma segundos
      FRA.FEC$ = DATE$                                  ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                      ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                     ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                     ! Arma dia
      MES1     = VAL(FRA.MM$)                           ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"          ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"         ! Si es Siglo XXI
      FHOY$   = M$(MES1)+". "+STR$(VAL(FRA.DD$))       \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                     !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +     \! Arma la Linea
               "    de "+FHOY$                          ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                     ! Salva Mes y Dia
END SUB                                                 !
!---------------------------------------------------------------------------
!                Rutina de Impresion de Encabezado de Pagina
!---------------------------------------------------------------------------
SUB CABEZA                                              !
   LPRINTER                                             !
   IF CONLI > 55 THEN BEGIN                             ! Si Hoja Full
      CONLI = 5                                         ! Inicia Cont Lin
      PAGI% = PAGI% + 1                                 !
      PRINT CHR$(12)                                    ! Salto de Pag
      PRINT " "                                         !
      PRINT "       MANTENIMIENTO DE LOS ARCHIVOS                             Pag. ";PAGI%
      PRINT "       ";NOAL$;"  No : ";ALM$;             !
      PRINT "  "                                        !
      PRINT "TIP  PAGO EST  CC/NIT       CC/NIT                         NOMBRE DEL "
      PRINT "REG TI/VA CTA  AFILIADO     EMPRESA       OBSERVACIONES     AFILIADO  "
      PRINT "---------------------------------------------------------------------------------------"
   ENDIF                                                ! Fin Cond
END SUB                                                 !

SUB DESPLEGAR                                           !
  FISNIT$ = UNPACK$(FISNIT$)                            ! Llave,Nit,CF,Nom,Dir
  NIT1$   = LEFT$(FISNIT$,6)                            !
  NIT2$   = MID$(FISNIT$,7,3)                           !
  NIT3$   = RIGHT$(FISNIT$,3)                           !
  BAR$    = UNPACK$(FISBAR$)                            !
  TRE$    = UNPACK$(FISTRE$)                            !
  TOF$    = UNPACK$(FISTOF$)                            !
  AP1$    = FISAPE$                                     !
  AP2$    = FIS2AP$                                     !
  NOM$    = FISNOM$                                     ! Nombres
  DIR$    = FISDIR$                                     !
  MAI$    = FISMAI$                                     ! Correo e-mail
  NAC$    = UNPACK$(FISNAC$)                            !
  AFI$    = UNPACK$(FISAFI$)                            !
  RED$    = UNPACK$(FISRED$)                            !
  ULC$    = UNPACK$(FISULC$)                            !
  UCA$    = UNPACK$(FISUCA$)                            !
  COR$    = UNPACK$(FISCOR$)                            !
  NCA$    = RIGHT$("00"   + STR$(FISCAM%),2)            !
  PVE$    = RIGHT$("0000" + STR$(FISPVE%),4)            !
  IF VAL(NIT1$) = 0 THEN BEGIN                          !
     NIT1$ = RIGHT$("        "+NIT2$,8)+"."+NIT3$       !
  ENDIF ELSE BEGIN                                      !
     NIT1$ = RIGHT$("     "+STR$(VAL(NIT1$)),6)+"."+   \!
             NIT2$ + "." + NIT3$                        !
  ENDIF                                                 !
  CALL RLIN(NIT1$,05,07)                                !
  NOBAR$ = "**FALTA BARRIO**"                           !
  MACH$ = "B" + BAR$                                    !
  BARR% = MATCH(MACH$,STRBAR$,1)                        !
  IF BARR% <> 0 THEN BEGIN                              !
     NOBAR$=MID$(STRBAR$,BARR%+1,03)                    ! 
     CALL RLIN(MID$(STRBAR$,BARR%+4,15),06,28)          !
  ENDIF                                                 !
  CALL RLIN(NOBAR$,05,28)                               !
  CALL RLIN(LEFT$(TRE$,3)+"-"+RIGHT$(TRE$,7),05,40)     !
  CALL RLIN(LEFT$(TOF$,3)+"-"+RIGHT$(TOF$,7),05,54)     !
  CALL RLIN(AP1$,08,05)                                 !
  CALL RLIN(AP2$,08,23)                                 !
  CALL RLIN(NOM$,08,40)                                 !
  CALL RLIN(DIR$,11,05)                                 !
  CALL RLIN(MAI$,11,40)                                 !
  FUM$ = "  "                                           !
  IF VAL(NAC$) <> 0 THEN BEGIN                          !
     FUM$ = M$(VAL(MID$(NAC$,5,2))) + " "  +           \! Nacimiento
            STR$(VAL(MID$(NAC$,7,2)))+"/"  +           \!
            LEFT$(NAC$,4)                               !
  ENDIF                                                 !
  CALL RLIN(FUM$,13,20)                                 !
  FUM$    = M$(VAL(MID$(AFI$,5,2))) + " "  +           \! Afiliacion
            STR$(VAL(MID$(AFI$,7,2)))+"/"  +           \!
            LEFT$(AFI$,4)                               !
  CALL RLIN(FUM$,14,20)                                 !
  FUM$ = "  "                                           !
  IF VAL(RED$) <> 0 THEN BEGIN                          !
     FUM$ = M$(VAL(MID$(RED$,5,2))) + " "  +           \! Ultima Redencion
            STR$(VAL(MID$(RED$,7,2)))+"/"  +           \!
            LEFT$(RED$,4)                               !
  ENDIF                                                 !
  CALL RLIN(FUM$,15,20)                                 !
  FUM$ = "  "                                           !
  IF VAL(ULC$) <> 0 THEN BEGIN                          ! Ultima Compra
     FUM$ = M$(VAL(MID$(ULC$,5,2))) + " "  +           \!
            STR$(VAL(MID$(ULC$,7,2)))+"/"  +           \!
            LEFT$(ULC$,4)                               !
  ENDIF                                                 !
  CALL RLIN(FUM$,16,20)                                 !
  FUM$ = "  "                                           !
  IF VAL(COR$) <> 0 THEN BEGIN                          !
     FUM$ = M$(VAL(MID$(COR$,5,2))) + " "  +           \! Ultima Correccion
            STR$(VAL(MID$(COR$,7,2)))+"/"  +           \!
            LEFT$(COR$,4)                               !
  ENDIF                                                 !
  CALL RLIN(FUM$,14,50)                                 !
  CALL RLIN(NCA$,15,50)                                 !
  CALL RLIN(PVE$,16,50)                                 !
  CALL REFRESH                                          !
  LOCATE 23,1                                           !
  PRINT "      Para Continuar, presione ENTER ";        !
  CALL TECLA                                            !
END SUB                                                 !

!  --------------   Rutina final de cierre del archivo   ----------------
SUB FINAL
    SW.FIN = 1                                          !
    CLOSE 25                                            ! Cierra CUPOS.DAT
    CLOSE 26                                            ! Cierra EMPRE.DAT
    LOCATE 15,10 : PRINT "    Proceso Finalizado..";    !
                   PRINT "                        ";    !
    CALL FINSHELF                                       !
    STOP                                                !
END SUB                                                 !
! -----------------------------------------------------------------------------
!                       PROGRAMA  PRINCIPAL           
!------------------------------------------------------------------------------
   FINR$    = CHR$(34)+CHR$(13)+CHR$(10)                ! Marca Reg "0D0A
   PREFIJO$ = CHR$(34)+"1"+CHR$(34)+CHR$(44)+CHR$(34)   ! Fijo "1","
   ON ERROR GOTO ERRTRAP                                !
   FUNC = 4                                             !
   CALL ADXSERVE(RET,FUNC,PARM1,PARM2$)                 !
   CLEARS                                               !
   PRINT :  PRINT                                       !
   PRINT "                     Application Status Information"!
   PRINT                                                !
   PRINT "                        N£mero de Almac‚n : ";!
   PRINT LEFT$(PARM2$,4)                                !
   NSTOR$ = LEFT$(PARM2$,4)                             !
   PRINT "                      Formato de la Fecha : ";!
   IF MID$(PARM2$,5,1) = "1" THEN PRINT "mm/dd/aa"      !
   IF MID$(PARM2$,5,1) = "2" THEN PRINT "dd/mm/aa"      !
   IF MID$(PARM2$,5,1) = "3" THEN PRINT "mm.dd.aa"      !
   IF MID$(PARM2$,5,1) = "4" THEN PRINT "dd.mm.aa"      !
   PRINT "                       Formato de la Hora : ";!
   IF MID$(PARM2$,6,1) = "1" THEN PRINT "hh:mm:ss"      !
   IF MID$(PARM2$,6,1) = "2" THEN PRINT "hh.mm.ss"      !
   PRINT "                        Formato Monetario : ";!
   IF MID$(PARM2$,7,1) = "1" THEN PRINT "NNN,NNN.nn"    !
   IF MID$(PARM2$,7,1) = "2" THEN PRINT "NNN.NNN,nn"    !
   IF MID$(PARM2$,7,1) = "3" THEN PRINT "NNN NNN nn"    !
   PRINT "                Tipo de Monitor Utilizado : ";!
   IF MID$(PARM2$,8,1) = "0" THEN PRINT "Desconocido"   !
   IF MID$(PARM2$,8,1) = "1" THEN PRINT "Mono-Cromatico"!
   IF MID$(PARM2$,8,1) = "2" THEN PRINT "A Color"       !
   PRINT "                  Terminales Configuradas : ";!
   PRINT MID$(PARM2$,9,3)                               !
   PRINT "                   N£mero del Controlador : ";!
   PRINT MID$(PARM2$,12,4)                              !
   PRINT "                Identificaci¢n del Master : ";!
   IF MID$(PARM2$,18,2) ="00" THEN PRINT "00=No Identificado"!
   IF MID$(PARM2$,18,2)<>"00" THEN PRINT MID$(PARM2$,18,2)
   PRINT "                        L¡neas a Imprimir : ";!
   PRINT MID$(PARM2$,20,3)                              !
   PRINT "             Cantidad de Cifras Decimales : ";!
   PRINT MID$(PARM2$,23,1)                              !
   PRINT "                    Red de Area Local LAN : ";!
   IF MID$(PARM2$,24,1) = "0" THEN PRINT "INACTIVA"     !
   IF MID$(PARM2$,24,1) = "1" THEN PRINT "ACTIVA"       !
   PRINT "                El Controlador Act£a como : ";!
   IF MID$(PARM2$,25,2) = "00" THEN PRINT "No en RED"   !
   IF MID$(PARM2$,25,2) = "01" THEN PRINT "Controlador en RED"
   IF MID$(PARM2$,25,2) = "02" THEN PRINT "File Server Alterno"
   IF MID$(PARM2$,25,2) = "04" THEN PRINT "Master File Server"
   IF MID$(PARM2$,25,2) = "08" THEN PRINT "Master Alterno"
   IF MID$(PARM2$,25,2) = "11" THEN PRINT "Master y File Server Alterno"
   IF MID$(PARM2$,25,2) = "16" THEN PRINT "Master Controlador"
   IF MID$(PARM2$,25,2) = "21" THEN PRINT "Master Controlador/Servidor"
   PRINT "          Impresora Asociada a la Consola : "; !
   PRINT MID$(PARM2$,27,1)                               !
   PRINT "          Consola Asociada al Controlador : "; !
   PRINT MID$(PARM2$,28,1)                               !
   PRINT "            Tipo de Red de Area Local LAN : "; !
   IF MID$(PARM2$,28,1) = "0" THEN PRINT "LAN No Instalada"
   IF MID$(PARM2$,28,1) = "1" THEN PRINT "LAN Token Ring"!
   IF MID$(PARM2$,28,1) = "2" THEN PRINT "LAN Ethernet"  !
   C27$ = CHR$(27)                                       ! Escape
   IF MID$(PARM2$,8,1) = "2" THEN BEGIN                  !
      B00$ = "b0" :  B01$ = "b1"  :  B02$ = "b2"         ! Negro, Azul y Verde
      B03$ = "b3" :  B04$ = "b4"  :  B05$ = "b5"         ! Cyan, Rojo y Magenta
      B06$ = "b6" :  B07$ = "b7"  :  B08$ = "b8"         ! Pardo, Gris Cl, Gris
      B09$ = "b9"                                        ! Azul cielo
   ENDIF ELSE BEGIN                                      !
      B00$ = "b7" :  B01$ = "b7"  :  B02$ = "b7"         ! Todos en Gris Claro
      B03$ = "b7" :  B04$ = "b7"  :  B05$ = "b7"         ! Todos en Gris Claro
      B06$ = "b7" :  B07$ = "b7"  :  B08$ = "b7"         ! Todos en Gris Claro
      B09$ = "b7"                                        ! Todos en Gris Claro
   ENDIF                                                 ! 

   CALL FINPANTA                                         !

   IN1$ = "1234567890"                                   !
   OU1$ = "8561094327"                                   !
   IN2$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                   !
   OU2$ = "9T8IVR0N5P3AD2BH1CG4JF7E6K"                   !
   MAR1$ = "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"     !
   MAR2$ = "³                                     ³"     !
   MAR3$ = "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"     ! 
   DIM TIT$(10)                                          !
   TIT$(01) = "1   Asignacion de Terminales "            !
   TIT$(02) = "2   Localizaciones "                      !
   TIT$(03) = "3   Avance del Inventario Fisico "        !
   TIT$(04) = "4   Reportes  "                           !
   TIT$(05) = "5   Interfaces   "                        !
   TIT$(06) = " "  : TIT$(07) = " "  :  TIT$(08) = " "   !
   TIT$(09) = " "  : TIT$(10) = " "                      ! 
   IVAS$ = "1520354560100000"                            !
   T1%=VAL(MID$(IVAS$,01,2))  :  T2%=VAL(MID$(IVAS$,03,2))
   T3%=VAL(MID$(IVAS$,05,2))  :  T4%=VAL(MID$(IVAS$,07,2))
   T5%=VAL(MID$(IVAS$,09,2))  :  T6%=VAL(MID$(IVAS$,11,2))
   T7%=VAL(MID$(IVAS$,13,2))  :  T8%=VAL(MID$(IVAS$,15,2))
!----------------------   Llena arreglo (de meses   ----------------------------
   DIM M$(12)
   M$(01)="Ene"  :  M$(02)="Feb"  :  M$(03)="Mar"        !
   M$(04)="Abr"  :  M$(05)="May"  :  M$(06)="Jun"        !
   M$(07)="Jul"  :  M$(08)="Ago"  :  M$(09)="Sep"        !
   M$(10)="Oct"  :  M$(11)="Nov"  :  M$(12)="Dic"        !
!----------------------   Llena arreglo de Ayudas   ---------------------------
   DIM LI%(999)                                          ! 
   DIM NUME%(999)                                        ! 
   DIM PREF$(999)                                        ! 
   DIM TER%(999)                                         !
   DIM AY$(1500)                                         !
   AY$(01) = "         ** AYUDA **"                      !
   AY$(02) = ". Hora Inicio del Inventario ."            !
   AY$(03) = "Ingrese la Hora exacta de Inicio"          !
   AY$(04) = "del Inventario. Las terminales  "          !
   AY$(05) = "asignadas dejan de VENDER..  "             !
   AY$(06) = " "                                         !
   AY$(07) = "         ** AYUDA **"                      !
   AY$(08) = ".       Tarifas de IVA      ."             !
   AY$(09) = "Iva  %    Iva  %    Iva  %"                !
   AY$(10) = "-------  --------  --------"               !
   AY$(11) = "(1)="+STR$(T1%)+"    (2)="+STR$(T2%)+     \!
             "    (3)="+STR$(T3%)                        !
   AY$(12) = "(4)="+STR$(T4%)+"    (5)="+STR$(T5%)+     \!
             "    (6)="+STR$(T6%)                        !
   AY$(13) = "         ** AYUDA **"                      !
   AY$(14) = ".  Hora Cierre del Inventario."            !
   AY$(15) = "Las terminales en Inventario, deben "      !
   AY$(16) = "finalizar esta labor. Los registros "      !
   AY$(17) = "sucesivos se consideran Vta del dia."      !
   AY$(18) = " "                                         !
   AY$(19) = "         ** AYUDA **"                      !
   AY$(20) = ".      Localizaciones...       ."          !
   AY$(21) = "Hace referencia a la CANTIDAD de"          !
   AY$(22) = "sitios FISICOS a los que se les "          !
   AY$(23) = "hace Inventario.(Ver Plano Almac)"         !
   AY$(24) = " "                                         !
   AY$(25) = "         ** AYUDA **"                      !
   AY$(26) = ".   Fecha Inicio del Inventario  ."        !
   AY$(27) = "Indique la Fecha exacta de INICIO del"     !
   AY$(28) = "Inventario. Se permite la fecha actual"    !
   AY$(29) = "con efecto a la siguiente hora.  "         !
   AY$(30) = " "                                         !
   AY$(31) = "         ** AYUDA **"                      !
   AY$(32) = ".  Fecha Terminacion Inventario ."         !
   AY$(33) = "Digite ENTER para asumir la Fecha"         !
   AY$(34) = "de Inicio. Si NO, digite la Fecha"         !
   AY$(35) = "estimada para CERRAR Inv/Fisico  "         !
   AY$(36) = " "                                         !
   AY$(37) = "         ** AYUDA **"                      !
   AY$(38) = ".  Referencias por Localizacion  ."        !
   AY$(39) = "Cada Localizacion puede tener hasta 4"     !
   AY$(40) = "REFERENCIAS(1-4). Verifique el Plano "     !
   AY$(41) = "del Almac‚n para cada localizaci¢n "       !
   AY$(42) = " "                                         !
   AY$(100) = "        ** AYUDA **"                      !
   AY$(101) = "  "                                       !
   AY$(102) = "  "                                       !
   AY$(200) = "        ** AYUDA **"                      !
   AY$(201) = "  "                                       !
   AY$(201) = "  "                                       !
!------------------------------------------------------------------------------
   FR$   = CHR$(13)+CHR$(10)                             ! 0D0A
   M1 = VAL(MID$(DATE$,3,2))                             ! Toma Mes Actual
   D$ = RIGHT$(DATE$,2)                                  ! Toma Dia Actual
   A$ = LEFT$(DATE$,2)                                   ! y Ano Actual
   IF LEFT$(A$,1)<"9" THEN A$=" del 20" ELSE A$=" de 19" ! Prefijo Ano 2000-2089
   IF LEFT$(D$,1) = "0" THEN D$ = RIGHT$(D$,1)           ! Quita CERO Izq
   DATEM$ = M$(M1)+"/"+D$+A$+LEFT$(DATE$,2)              ! Arma la Fecha
   DATEM$ = LEFT$(DATEM$+STRING$(80," "),60)             ! Arma la Fecha
   TIM$ = TIME$                                          !
   HH$ = LEFT$(TIM$,2)                                   !
   MM$ = MID$(TIM$,3,2)                                  !
   SS$ = RIGHT$(TIM$,2)                                  !
   TIM$ = "Hora: "+HH$+":"+MM$+":"+SS$                   !
   CL%    = 56                                           !
   CR$    = CHR$(10)                                     !
   LF$    = CHR$(13)                                     !
   COLON$ = CHR$(34)                                     !
   QUOTE$ = CHR$(58)                                     !
   PLX$   = "*"                                          !

   ARCH3$ = "C:\ADX_UDT1\NFFCNTRL.DAT"                   ! Define Ctrl Almac
   ARCH4$ = "C:\ADX_UDT1\INVCNTRL.DAT"                   ! Define Reg Inicializac
   FIS.CTR% = 46                                         !
   NUEVO% = 1                                            !
   OPEN ARCH4$ KEYED  RECL 082 AS 4                      ! Abre Inventario
   IF NUEVO% = 1 THEN BEGIN                              !
      KEY$ = PACK$("999999")                             !
      READ FORM "C3 C2 C30 3C14 2I2 C1";#4 KEY KEY$;    \!
           KEY$,NUNEG$,NEGO$,FCR$,FCO$,                 \!
           FFI$,NLO%,NRE%,ESTA$                          !
           FCO$ = LEFT$(FCO$,8)                          !
           FFI$ = LEFT$(FFI$,8)                          !
           HIN$ = MID$(FCO$,9,4)                         !
           HFI$ = MID$(FFI$,9,4)                         !
      NUEVO% = 0 : SW1 = 2                               !
   ENDIF                                                 !

INICIO:                                                  !
  IF NUEVO% = 1 THEN SW1 = 2                             !
  CALL PANTA1                                            !
  IF NUEVO% = 0 THEN BEGIN                               !
     TOMAR.NEGO:                                         !
     IF SW1 = 2 THEN DATO1$ = NEGO$                      !
     NEGO$ = DATO$("",03,50,"A",28)                      !
     IF NEGO$=STRING$(LEN(NEGO$)," ") THEN GOTO TOMAR.NEGO

     FECHINI:                                            !
     K% = 25 :  W% = 29  :  KK% = K%  :  CALL AYUDA      !
     LOCATE 05,35 : PRINT STRING$(12," ")                !
     IF SW1 = 2 THEN DATO1$ = FCO$                       !
     FCO$ = DATO$("",05,35,"9",08)                       !
     IF FCO$ = "00000000" THEN FCO$ = "20"+DATE$         !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     IF VAL(FCO$) <> 0 AND                              \!
       (LEN(FCO$) <> 8          OR                      \!
        FCO$ < "20" + DATE$     OR                      \!
        MID$(FCO$,5,2)  < "01"  OR                      \!
        MID$(FCO$,5,2)  > "12"  OR                      \!
       (MID$(FCO$,5,2) = "02"  AND                      \!
        MID$(FCO$,7,2)  > "29") OR                      \!
        MID$(FCO$,7,2)  < "01"  OR                      \!
        MID$(FCO$,7,2)  > "31") THEN BEGIN               !
           MENS$ = "Error de Fecha. Verifique Formato..!"!
           CALL VERROR(MENS$)                            !
           GOTO FECHINI                                  !
     ENDIF                                               !
     LOCATE 05,34                                        !
     PRINT C27$+"p" ; C27$+B03$;" ";                     ! Inic Video Rever/Cyan
     PRINT M$(VAL(MID$(FCO$,5,2)));" ";                  ! Busca el Mes
     PRINT RIGHT$(FCO$,2);"/";LEFT$(FCO$,4);" ";         ! dia y a¤o de Nac
     PRINT C27$ + "q" ; C27$ + B07$                      ! Fin Video Rever/Gris
     CALL BORRAR                                         ! Limpia Ayuda
     IF SW1 = 1 THEN RETURN                              !

     FECHFIN:                                            !
     K% = 31 :  W% = 35  :  KK% = K%  :  CALL AYUDA      !
     LOCATE 06,35 : PRINT STRING$(12," ")                !
     IF SW1 = 2 THEN DATO1$ = FFI$                       !
     FFI$ = DATO$("",06,35,"9",08)                       !
     IF FFI$ = "00000000" THEN FFI$ = FCO$               !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     IF LEN(FFI$) <> 8          OR                      \!
        FFI$ < FCO$             OR                      \!
        MID$(FFI$,5,2)  < "01"  OR                      \!
        MID$(FFI$,5,2)  > "12"  OR                      \!
       (MID$(FFI$,5,2)  = "02"  AND                     \!
        MID$(FFI$,7,2)  > "29") OR                      \!
        MID$(FFI$,7,2)  < "01"  OR                      \!
        MID$(FFI$,7,2)  > "31"  THEN BEGIN               !
           MENS$ = "Error de Fecha. Verifique Formato..!"!
           CALL VERROR(MENS$)                            !
           GOTO FECHFIN                                  !
     ENDIF                                               !
     LOCATE 06,34                                        !
     PRINT C27$+"p" ; C27$+B03$;" ";                     ! Inic Video Rever/Cyan
     PRINT M$(VAL(MID$(FFI$,5,2)));" ";                  ! Busca el Mes
     PRINT RIGHT$(FFI$,2);"/";LEFT$(FFI$,4);" ";         ! dia y a¤o de Nac
     PRINT C27$ + "q" ; C27$ + B07$                      ! Fin Video Rever/Gris
     CALL BORRAR                                         ! Limpia Ayuda
     IF SW1 = 1 THEN RETURN                              !
 
     HRINI:                                              !
     K% = 01 :  W% = 05  :  KK% = K%  :  CALL AYUDA      !
     LOCATE 5,55 : PRINT STRING$(12," ")                 !
     IF SW1 = 2 THEN DATO1$ = HIN$                       !
     HIN$ = DATO$("",05,55,"9",04)                       !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     IF VAL(HIN$) = 0 OR                                \!
        VAL(LEFT$(HIN$,2))  < 00 OR                     \!
        VAL(LEFT$(HIN$,2))  > 23 OR                     \!
        VAL(RIGHT$(HIN$,2)) < 00 OR                     \!
        VAL(RIGHT$(HIN$,2)) > 59 THEN BEGIN              !
           MENS$ = "Hora de Inicio Errada..!"            !
           CALL VERROR(MENS$)                            !
           GOTO HRINI                                    !
     ENDIF                                               !
     IF HIN$ < "1201" THEN AMP$ = " AM."                 ! 
     IF HIN$ = "1200" THEN AMP$ = " M"                   ! 
     IF HIN$ > "1200" THEN AMP$ = " PM."                 ! 
     LOCATE 05,55                                        !
     PRINT C27$+"p" ; C27$+B03$;                         ! Inic Video Rever/Cyan
     PRINT LEFT$(HIN$,2)+":"+RIGHT$(HIN$,2);AMP$;        !
     PRINT C27$ + "q" ; C27$ + B07$                      ! Fin Video Rever/Gris
     CALL BORRAR                                         !
     IF SW1 = 1 THEN RETURN                              !
 
     HRFIN:                                              !
     K% = 13 :  W% = 18  :  KK% = K%  :  CALL AYUDA      !
     LOCATE 6,55 : PRINT STRING$(12," ")                 !
     IF SW1 = 2 THEN DATO1$ = HFI$                       !
     HFI$ = DATO$("",06,55,"9",04)                       !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     IF VAL(HFI$) = 0 OR                                \!
       (FCO$ = FFI$   AND                               \!
        HFI$ < HIN$)  OR                                \!
        VAL(LEFT$(HFI$,2))  < 00 OR                     \!
        VAL(LEFT$(HFI$,2))  > 23 OR                     \!
        VAL(RIGHT$(HFI$,2)) < 00 OR                     \!
        VAL(RIGHT$(HFI$,2)) > 59 THEN BEGIN              !
           MENS$ = "Hora de Finalizacion Errada..!"      !
           CALL VERROR(MENS$)                            !
           GOTO HRFIN                                    !
     ENDIF                                               !
     IF HFI$ < "1201" THEN AMP$ = " AM."                 ! 
     IF HFI$ = "1200" THEN AMP$ = " M"                   ! 
     IF HFI$ > "1200" THEN AMP$ = " PM."                 ! 
     LOCATE 06,55                                        !
     PRINT C27$+"p" ; C27$+B03$;                         ! Inic Video Rever/Cyan
     PRINT LEFT$(HFI$,2)+":"+RIGHT$(HFI$,2);AMP$         !
     PRINT C27$ + "q" ; C27$ + B07$                      ! Fin Video Rever/Gris
     CALL BORRAR                                         !
     IF SW1 = 1 THEN RETURN                              !
  
     LOCALI:                                             !
     K% = 19 :  W% = 23  :  KK% = K%  :  CALL AYUDA      !
     IF SW1 = 2 THEN DATO1$ = STR$(NLO%)                 !
     NLO$ = DATO$("",08,40,"9",04)                       !
     IF VAL(NLO$) < 1 OR                                \!
        VAL(NLO$) > 9998 THEN BEGIN                      !
           MENS$ = "Total de Localizaciones INVALIDO..!" !
        CALL VERROR(MENS$)                               !
        GOTO LOCALI                                      !
     ENDIF                                               !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     CALL BORRAR                                         !
     IF SW1 = 1 THEN RETURN                              !
  
     REFEREN:                                            !
     K% = 37 :  W% = 41  :  KK% = K%  :  CALL AYUDA      !
     IF SW1 = 2 THEN DATO1$ = STR$(NRE%)                 !
     NRE$ = DATO$("",09,40,"9",02)                       !
     IF VAL(NRE$) < 1 OR                                \!
        VAL(NRE$) > 04 THEN BEGIN                        !
           MENS$ = "Cantidad de Referencias Excede 4..!" !
        CALL VERROR(MENS$)                               !
        GOTO REFEREN                                     !
     ENDIF                                               !
     IF EN$ = "F3" THEN GOTO INICIO                      ! 
     CALL BORRAR                                         !
     IF SW1 = 1 THEN RETURN                              !
     LOCATE 10,45
     EST$ = "ACTIVO..!"
     IF ESTA$ = "0" THEN EST$ = "CERRADO..!"
     PRINT C27$+"p" ; C27$+B03$;                         ! Inic Video Rever/Cyan
     PRINT EST$                                          !
     PRINT C27$ + "q" ; C27$ + B07$                      ! Fin Video Rever/Gris
     CAM$ = DATO$("Esta Seguro S/N ? ",22,5,"A",1)       !
     IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO INICIO     !
     LOCATE 22,1  :  PRINT STRING$(70," ");              !
     IF CAM$ = "N" THEN GOTO INICIO                      !
     KEY$ = PACK$("999999")                              !
     NSTOR$ = PACK$(NSTOR$)                              !
     NLO% = VAL(NLO$)                                    !
     NRE% = VAL(NRE$)                                    !
     ESTA$ = "1"
     WRITE FORM "C3 C2 C30 3C14 2I2 C1";#4;             \!
           KEY$,NSTOR$,NEGO$,"20" + DATE$ + TIME$,      \!
           FCO$ + HIN$  + "00",FFI$ + HFI$ + "00",      \!
           NLO%,NRE%,ESTA$                               !
  ENDIF                                                  !
  STOP
LEER:
     READ FORM "C80";#3,CNT%;AL$                          ! Lee Control Almac
   TIPO$ = MID$(AL$,78,1)                                 ! Toma Tipo Registro
  SUBT$ = MID$(AL$,74,3)                                 ! Toma Tipo Registro
  IF TIPO$ = "1" THEN BEGIN                              ! Si es Reg tipo 1
     IDENTIF$ = MID$(AL$,67,4)                           ! Identif de Transac
     NUAL$ = LEFT$(AL$, 4)                               ! Guarda Num Almacen
     C$ = MID$(AL$,5,8)                                  ! Toma la Fecha
     ALMA$ = STRING$(20," ")+"Almac‚n: "                 !
     ALMA$ = ALMA$ + MID$(AL$,2,3) + "  "                !
     ALMA$ = ALMA$ + MID$(AL$,14,30)                     !
  ENDIF                                                  ! Fin
  IF TIPO$ = "2" THEN BEGIN                              ! Si es Reg tipo 2
     PGDVV$ = MID$(AL$,69,2)                             ! Tip/Varie Pag x Dev
     PGDVC$ = MID$(AL$,72,2)                            ! Tip/Varie Emision
     TI.VA$ = PGDVV$                                    ! Tip/Varie Causac
     FIS.CAU$=PGDVC$                                    !
  ENDIF                                                 ! Fin
  IF TIPO$ = "9" THEN BEGIN                             ! Si es Final
     CLOSE 3                                            ! Cierra Arch Ctrl
     TER%(999) = 1                                      ! Guarda Ult TPV
     GOTO INICIO                                        ! y Continua
  ENDIF                                                 ! Fin de Ultimo Reg
  IF TIPO$ = "4" THEN BEGIN                             ! Si es Reg de TPV
     FOR I% = 1 TO 18                                   ! Loop para guardar TPV
         TE$ = MID$(AL$, 4*I%-3,4)                      ! Busca No de TPV
         IF TE$ <> "    "  AND                         \! y valida contenido
            VAL(TE$) < 999 THEN BEGIN                   ! controlando Rango
               TER%(VAL(TE$)) = 1                       ! Guarda No. TPV
         ENDIF                                          ! Fin de Reg de TPV  
     NEXT I%                                            ! Siguiente Loop
  ENDIF                                                 ! Fin Reg Tipo TPV
  IF TIPO$ = "8" THEN BEGIN                             ! Si es Reg Admon
     IF SUBT$ = "002" THEN BEGIN                        !
        STRBAR$=STRBAR$+"B"+MID$(AL$,01,2)+MID$(AL$,03,16)
        STRBAR$=STRBAR$+"B"+MID$(AL$,19,2)+MID$(AL$,21,16)
        STRBAR$=STRBAR$+"B"+MID$(AL$,37,2)+MID$(AL$,39,16)
        STRBAR$=STRBAR$+"B"+MID$(AL$,55,2)+MID$(AL$,57,16)
     ENDIF                                              ! Fin Reg Tipo TPV
  ENDIF                                                 ! Fin Reg Tipo TPV
  GOTO LEER                                             ! Vuelve a Leer Control
INICIOO:                                                 !
   TEX$ = ""  :  ROW% = 5   :   CLM% = 7                !
   CALL CREAFECHA                                       !
   FEC1$ = FRA.LIN$                                     !
   CALL PANTA4                                          !
   WHILE SW.FIN = 0                                     ! Ciclo paso informacion
         GOSUB PROCESO                                  ! Pasa Datos 
   WEND                                                 ! Fin del ciclo
   CALL FINAL                                           !
PROCESO:                                                !
  OPC$ = STR$(EN%)                                      !
  IF EN% = 1 OR EN% = 3 THEN NUEVO% = 0                 !
  IF EN% < 3 THEN CALL CUANTOS.NUEVOS                   !
  ON EN% GOSUB CREAR,MODIF,CARNETS,VER,LISTAR           !
RETURN                                                  !

CREAR:                                                  !
UNO:                                                    !
  CALL TOMANIT                                          !
  IF EN$ = "F3" THEN GOTO INICIO                        ! 
  IF ERCOD$ <> "  " THEN  GOTO UNO                      !
  CALL VERICLI                                          !
  IF OBSER$ = "Y" THEN BEGIN                            !
     FIRST = 1                                          !
     MENS$ = "Registro YA existe..!"                    !
     CALL VERROR(MENS$)                                 !
     FISNIT$ = KEY$                                     ! Llave,Nit
     CALL DESPLEGAR                                     !
     NUEVO% = 1                                         !
     GOTO CAMBIO                                        !
  ENDIF                                                 !
DOS:                                                    !
  K% = 100 :  W% = 200  :  KK% = K%  :  CALL AYUDABAR   !
  BAR$ = DATO$("",05,28,"9",2)                          !
  IF EN$ = "F3" THEN GOTO INICIO                        ! 
  MACH$ = "B" + BAR$                                    !
  BARR% = MATCH(MACH$,STRBAR$,1)                        !
  IF BARR% = 0 THEN BEGIN                               !
     MENS$ = "No. Barrio Errado..!"                     !
     CALL VERROR(MENS$)                                 !
     GOTO DOS                                           !
  ENDIF                                                 !
  LOCATE 5,28                                           !
  PRINT C27$+"p" ; C27$+B03$;                           ! Inic Video Rever/Cyan
  PRINT MID$(STRBAR$,BARR%+1,03);                       !
  PRINT C27$ + "q" ; C27$ + B07$                        ! Fin Video Rever/Gris
  LOCATE 6,28                                           !
  PRINT C27$+"p" ; C27$+B03$;                           ! Inic Video Rever/Cyan
  PRINT MID$(STRBAR$,BARR%+4,15);                       !
  PRINT C27$ + "q" ; C27$ + B07$                        ! Fin Video Rever/Gris
  CALL BORRAR                                           !
  IF SW1 = 1 THEN RETURN                                !


CAMBIO:                                                 !
  NU% = 0                                               !
  LOCATE 23,1  :  PRINT STRING$(70," ");                !
  CAM$ = DATO$("Hay Cambios S/N ? ",23,5,"A",1)         !
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO CAMBIO       !
  LOCATE 23,1  :  PRINT STRING$(70," ");                !
!  IF CAM$ = "N" THEN GOTO GRABAR                        !
  IF CAM$ = "S"  THEN BEGIN                             !
     NU$ = DATO$("Qu‚ N£m.?  ",23,14,"9",2)             !
     NU% = VAL(NU$)                                     !
  ENDIF                                                 !
  IF NU% > 11 OR NU% < 2 THEN NU%=0                     !
  IF NU% = 0 THEN GOTO CAMBIO                           !
  LOCATE 23,1  :  PRINT STRING$(70," ");                !
  SW1 = 1                                               !
  IF LEFT$(FISNOM$,6) = "AMIGO" THEN SW1 = 2            ! Obliga Tomar Datos
!  ON NU% GOSUB UNO,DOS,TRE,CUA,CIN,SEI,SIE,OCH,        \!
!               NUE,DIE,ONC                              !
  SW1 = 0                                               !
  GOTO CAMBIO                                           !

GRABAR:                                                 !
  LOCATE 23,1  :  PRINT STRING$(70," ");                !
  CAM$ = DATO$(".. Est  Seguro S/N ? ",23,5,"A",1)      !
  IF EN$ = "F3" THEN GOTO INICIO                        ! 
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO GRABAR       !
  LOCATE 23,1  :  PRINT STRING$(70," ");                !
  IF CAM$ = "N" THEN GOTO CAMBIO                        !
  FISNIT$ = KEY$                                        ! Llave,Nit
  FISBAR$ = PACK$(BAR$)                                 ! Barrio
  FISTRE$ = PACK$(TRE$)                                 ! Tel Residencia
  FISTOF$ = PACK$(TOF$)                                 ! Tel Oficina
  FISAPE$ = AP1$                                        ! Prim Apellido
  FIS2AP$ = AP2$                                        ! Segu Apellido
  FISNOM$ = NOM$                                        ! Nombres
  FISDIR$ = DIR$                                        ! Direccion
  FISMAI$ = MAI$                                        ! Correo e-mail
  FISNAC$ = PACK$(NAC$)                                 !
  FISAFI$ = PACK$(AFI$)                                 !
  FISRED$ = PACK$("00000000")                           !
  FISULC$ = PACK$("00000000")                           !
  FISUCA$ = PACK$("20" + DATE$)                         !
  FISCOR$ = PACK$("20" + DATE$)                         !
  FISCAM% = FISCAM% + 1                                 !
  FISPVE% = 0                                           !
  NCA$    = RIGHT$("00"   + STR$(FISCAM%),2)            !
  PVE$    = RIGHT$("0000" + STR$(FISPVE%),4)            !
!    LOCATE 1,1 : PRINT "ABRIR1.."; : INPUT "  ";ZZZ$
  OPEN ARCH4$ KEYED  RECL 259 AS 4                      ! Abre Cliente
  LEC$ = "C6 C1 2C5 2C12 C20 C30 C40 6C4 26I4"          !
  WRITE FORM LEC$;#4;FISNIT$,                          \! Lee Reg Variables
         FISBAR$,FISTRE$,FISTOF$,FISAPE$,FIS2AP$,      \! Barr,Tel,Ofic,Ape,2Ap
         FISNOM$,FISDIR$,FISMAI$,FISNAC$,FISAFI$,      \! Nom,Dir,Mail,Nac,Afil
         FISRED$,FISULC$,FISUCA$,FISCOR$,              \! Rede,UltC,FCamb,FCort
         FISCAM%,FISPVE%,                              \! Cant Camb,PtosVenc
         FISG01%,FISG02%,FISG03%,FISG04%,FISG05%,      \! Puntos Ganados  
         FISG06%,FISG07%,FISG08%,FISG09%,FISG10%,      \! Ultimos 12 meses
         FISG11%,FISG12%,                              \!
         FISR01%,FISR02%,FISR03%,FISR04%,FISR05%,      \! Puntos Redimidos
         FISR06%,FISR07%,FISR08%,FISR09%,FISR10%,      \! Ultimos 12 meses
         FISR11%,FISR12%                                !
!     LOCATE 1,1 : PRINT "CERRAR1..";NUEVO%; : INPUT "  ";ZZZ$
  IF FIRST  = 0 THEN CALL CREAFILE                      ! Crea Batch Shelf
  CALL GRABAREG                                         ! Graba Client en Batch
  CLOSE 4                                               !
  FUM$    = UNPACK$(FISUCA$)                            ! Ult/Camb,Rsrv,Cod/Pw
  FUM$    = M$(VAL(MID$(FUM$,5,2))) + " "  +           \!
            STR$(VAL(MID$(FUM$,7,2)))+"/"  +           \!
            LEFT$(FUM$,4)                               !
  CALL RLIN(FUM$,14,50)                                 !
  CALL RLIN(NCA$,15,50)                                 !
  CALL RLIN(PVE$,16,50)                                 !
  LOCATE 23,1                                           !
  PRINT "      Para Continuar, presione ENTER ";        !
  CALL TECLA                                            !
!  IF NUEVO% = 0 THEN GOTO CREAR                         ! 
!  IF NUEVO% = 1 THEN GOTO MODIF                         ! 
RETURN                                                  !

MODIF:                                                  !
!  CALL PANTA5                                           !
  NUEVO% = 1                                            ! FIRST EN 0
  CALL TOMANIT                                          !
  IF EN$ = "F3" THEN BEGIN                              !
     GOTO INICIO                                        ! 
  ENDIF                                                 !
  IF ERCOD$ <> "  " THEN  GOTO MODIF                    !
  CALL VERICLI                                          !
  IF OBSER$ = "N" THEN BEGIN                            !
     MENS$ = "No Existe Registro para ‚ste Cliente..!"  !
     CALL VERROR(MENS$)                                 !
     GOTO MODIF                                         !
  ENDIF                                                 !
  CALL DESPLEGAR                                        !
  NUEVO% = 1                                            !
  GOTO CAMBIO                                           !
RETURN                                                  !

CARNETS:                                                !
  Z% = 1  : ZZ% = 0                                     !
  CALL PANTA6                                           !
  IF EN$ = "F3" THEN GOTO INICIO                        ! 
  OPEN ARCH4$ KEYED  RECL 259 AS 4                      ! Abre Variabl
  FECAR:                                                ! 
  IF CARN% = 1 THEN BEGIN                               !
     CALL PANTA7                                        !
     TEX$ = "Ptos/M¡n: "                                !
     PTO$ = DATO$(TEX$,04,03,"9",4)                     !
     TEX$ = "#Term:(nn) TODAS=99): "                    !
     TER$ = DATO$(TEX$,05,03,"9",2)                     !
     Z% = VAL(TER$)                                     !
     IF VAL(TER$) = 99 THEN Z% = 1                      !
     IF VAL(TER$) = 98 THEN Z% = 99                     !
     IF VAL(TER$) = 0  OR                              \!
        VAL(TER$) > 15 AND                             \!
        VAL(TER$) < 98 THEN BEGIN                       !
           MENS$ = "#Terminal fuera de Rango (01-15)..!"!
           CALL VERROR(MENS$)                           !
           GOTO FECAR                                   !
     ENDIF                                              !
     TEX$ = "Desde:(AAAAMMDD) "                         !
     KAR$ = DATO$(TEX$,04,34,"9",8)                     !
     IF EN$ = "F3" THEN GOTO INICIO                     ! 
     IF LEN(KAR$) <> 8          OR                     \!
        KAR$ > "20" + DATE$     OR                     \!
        MID$(KAR$,5,2)  < "01"  OR                     \!
        MID$(KAR$,5,2)  > "12"  OR                     \!
        (MID$(KAR$,5,2) = "02"  AND                    \!
        MID$(KAR$,7,2)  > "29") OR                     \!
        MID$(KAR$,7,2)  < "01"  OR                     \!
        MID$(KAR$,7,2)  > "31"  THEN BEGIN              !
           MENS$ = "Error de Fecha. Verifique Formato..!"
           CALL VERROR(MENS$)                           !
           GOTO FECAR                                   !
     ENDIF                                              !
     LOCATE 04,51                                       !
     PRINT C27$+"p" ; C27$+B03$;                        ! Inic Video Rever/Cyan
     PRINT M$(VAL(MID$(KAR$,5,2)));" ";                 ! Busca el Mes
     PRINT RIGHT$(KAR$,2);"/";LEFT$(KAR$,4)             ! dia y a¤o de Nac
     PRINT C27$ + "q" ; C27$ + B07$                     ! Fin Video Rever/Gris
  FEKAR:                                                !
     TEX$ = "Hasta:(AAAAMMDD) "                         !
     RAK$ = DATO$(TEX$,05,34,"9",8)                     !
     IF EN$ = "F3" THEN GOTO INICIO                     ! 
     IF LEN(RAK$) <> 8          OR                     \!
        RAK$ > "20" + DATE$     OR                     \!
        RAK$ < KAR$             OR                     \!
        MID$(RAK$,5,2)  < "01"  OR                     \!
        MID$(RAK$,5,2)  > "12"  OR                     \!
        (MID$(RAK$,5,2) = "02"  AND                    \!
        MID$(RAK$,7,2)  > "29") OR                     \!
        MID$(RAK$,7,2)  < "01"  OR                     \!
        MID$(RAK$,7,2)  > "31"  THEN BEGIN              !
           MENS$ = "Error de Fecha. Verifique Formato..!"
           CALL VERROR(MENS$)                           !
           GOTO FEKAR                                   !
     ENDIF                                              !
     LOCATE 04,63                                       !
     PRINT "a "+C27$+"p" ; C27$+B03$;                   ! Inic Video Rever/Cyan
     PRINT M$(VAL(MID$(RAK$,5,2)));" ";                 ! Busca el Mes
     PRINT RIGHT$(RAK$,2);"/";LEFT$(RAK$,4)             ! dia y a¤o de Nac
     PRINT C27$ + "q" ; C27$ + B07$                     ! Fin Video Rever/Gris
  ENDIF                                                 !
  IF FIRST = 0 THEN CALL CREAFILE                       ! Crea Batch Shelf

  BATCHE:                                               !
  IF CARN% = 2 THEN BEGIN                               !
     TEX$ = "Nit del Cliente: "                         !
     CALL PANTA7                                        !
  ENDIF                                                 !
  ROW% = 05  :   CLM% = 34                              !
  FINBA% = 0                                            !
  IF CARN% = 1 THEN BEGIN                               !
     WHILE NOT FINBA%                                   !
       ZZ% = ZZ% + 1                                    !
       KEY$ = RIGHT$(KAR$,6)+RIGHT$("00"+STR$(Z%),2)+  \!
              RIGHT$("0000"+STR$(ZZ%),4)                !
       KEY$ = PACK$(KEY$)                               !
       ERCOD$ = "  "                                    !
       LOCATE 6,34 : PRINT "CTRL: "+UNPACK$(KEY$);      !
       READ FORM "2C6 C3 C244";#4 KEY KEY$;            \! Lee Reg Especiales
                     FISFEA$,FISNIT$,FISEAN$,FISUSR$             ! de Afiliados dia
       IF ERCOD$ = "  " THEN FINBA% = -1                ! Si lo encuentra
       IF Z% = 15 AND ZZ% = 1000 THEN Z% = 98           !
       IF ZZ% = 1000 THEN BEGIN                         !
          IF Z% < 100 THEN BEGIN                        !
             IF Z% = 99 THEN BEGIN                      !
                IF KAR$ <> RAK$ THEN BEGIN              !
                   ANOS% = VAL(MID$(KAR$,1,4))          !
                   MESE% = VAL(MID$(KAR$,5,2))          !
                   DIAS% = VAL(MID$(KAR$,7,2))          !
                   DIAS% = DIAS% + 1                    !
                   Z% = 0  : ZZ% = 0                    !
                   IF DIAS% > 30 THEN BEGIN             !
                      MESE% = MESE% + 1                 !
                      DIAS% = 1                         !
                   ENDIF                                !
                   IF MESE% > 12 THEN BEGIN             !
                      ANOS% = ANOS% + 1                 !
                      MESE% = 1                         !
                   ENDIF                                !
                   KAR$ = STR$(ANOS%) +                \!
                          RIGHT$("00"+STR$(MESE%),2) + \!
                          RIGHT$("00"+STR$(DIAS%),2)    !
                ENDIF ELSE BEGIN                        !
                   EN$ = "F3"                           !
                   FINBA% = -1                          !
                   LOCATE 23,5                          !
                   PRINT "  Closing Files.. Please wait.!"
                ENDIF                                   !
             ENDIF                                      !
             Z% = Z% + 1                                !
            ZZ% = 0                                     !
          ENDIF                                         !
       ENDIF                                            !
     WEND                                               !
     NIT1$ = UNPACK$(FISNIT$)                           !
  ENDIF                                                 ! hace Bypass
  IF EN$ = "F3" THEN BEGIN                              !
     CLOSE 4                                            !
 !    CALL FINSHELF                                      !
 !    GOTO INICIO                                        ! 
  ENDIF                                                 !
  CALL TOMANIT                                          !
  IF ERCOD$ <> "  " THEN  GOTO BATCHE                   !
  CALL VERICLI                                          !
!  CALL SUMAR.PUNTOS                                     !
  IF FISA13% < VAL(PTO$) THEN GOTO BATCHE               !
  IF LEFT$(FIS2AP$,9) = "OCASIONAL" THEN               \!
     FIS2AP$ = STRING$(12," ")                          !
  IF OBSER$ = "N" THEN BEGIN                            !
     MENS$ = "No Existe Registro..!"                    !
     CALL VERROR(MENS$)                                 !
     GOTO BATCHE                                        !
  ENDIF                                                 !
  EANPCA$ = "1"+RIGHT$(UNPACK$(FISNIT$),11)             !
  CALL DIGICHECK                                        !
  TRE$ = UNPACK$(FISTRE$)                               !
  TRE$ = LEFT$(TRE$,3)+"-"+RIGHT$(TRE$,7)               !
  CALL RLIN("-"+DIGCTL$+"            ",5,48)            !
  CALL RLIN(FISAPE$,07,34)                              !
  CALL RLIN(FIS2AP$,08,34)                              !
  CALL RLIN(FISNOM$,10,34)                              !
  CALL RLIN(FISDIR$,12,34)                              !
  CALL RLIN(TRE$,14,34)                                 !
  CALL RLIN(UNPACK$(FISBAR$),16,34)                     !
  CALL RLIN(STR$(FLOAT(FISA13%)/1000)+"    ",18,34)     !
  ARGU$ = "N" + FISNIT$                                 !
  NODO% = MATCH(ARGU$,CARHOY$,1)                        !
  IF NODO% = 0 THEN BEGIN                               !
     CALL GRABAREG                                      !
     CARHOY$ = CARHOY$ + ARGU$                          !
     LOCATE 3,5 : PRINT LEN(CARHOY$)                    !
  ENDIF                                                 !
  IF CARN% = 2 THEN CALL FINPANTA                       !
!  GOTO BATCHE                                           !

VER:                                                    !
!  CALL PANTA5                                           !
  CALL TOMANIT                                          !
  IF EN$ = "F3" THEN BEGIN                              !
     CALL FINSHELF                                      !
     GOTO INICIO                                        ! 
  ENDIF                                                 !
  IF ERCOD$ <> "  " THEN  GOTO VER                      !
  CALL VERICLI                                          !
  IF OBSER$ = "N" THEN BEGIN                            !
     MENS$ = "No Existe Registro..!"                    !
     CALL VERROR(MENS$)                                 !
     GOTO VER                                           !
  ENDIF                                                 !
  CALL DESPLEGAR                                        !
  CALL FINPANTA                                         !
  GOTO VER                                              !
RETURN                                                  !

LISTAR:                                                 !
CALL VERICLI                                            !
IF OBSER$ = "N" THEN BEGIN                              !
   MENS$ = "No Existe Registro..!"                      !
   CALL VERROR(MENS$)                                   !
   GOTO INICIO                                          !
ENDIF                                                   !
CALL FINPANTA                                           !
GOTO INICIO                                             !
RETURN                                                  !

ERRTRAP:                                                !
   HX% = ERRN                                           !
   ERRFX$ =""                                           !
   FOR S% = 28 TO 0 STEP -4                             !
       SX% = SHIFT(HX%,S%)                              !
       SUM% = SX% AND 000FH                             !
   IF SUM% > 9 THEN                                    \!
        SUM% = SUM% + 55                               \!
   ELSE                                                \!
        SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                 !
        ERRFX$ = ERRFX$ + Z$                            !
   NEXT S%                                              !
   ERCOD$ = ERR                                         !
   IF ERR = "IH" THEN RESUME                            !
   IF ERR = "OE" AND ERRF% = 22 THEN BEGIN              !
      CREATE "ADX_IPGM:ADXLOGOD.DAT" AS 22              !
      RESUME                                            !
   ENDIF                                                !
   IF ERRF% = 4  AND ERR = "EF" THEN RESUME             !
!------------------------------------------------------------------------------
    LOCATE 23,60 : PRINT ERR; " ";ERRF%;" ";ERRFX$;    !
    INPUT " ";AA$                                      !
!------------------------------------------------------------------------------
   IF ERRF% = 4  AND                                   \!
      ERR = "OE" AND                                   \!
      ERRFX$ = "80204010" THEN BEGIN                    !
      MENS$ = "Se CREARA Archivo de Control del INVENTARIO..! Digite ENTER." 
      CALL VERROR(MENS$)                                !
      NUEVO% = 0
      CREATE POSFILE ARCH4$ KEYED 3,,,10000 RECL 082 AS 4
      RESUME                                            !
   ENDIF                                                !
   IF ERRF% = 46 AND ERR = "EF" THEN RESUME             !
   IF ERRF% = 1  AND ERR = "OE" THEN CALL ERROR1        !
   IF ERRF% = 3  AND ERR = "OE" THEN CALL ERROR2        !
END                                                     !
!-----------------------------------------------------------------------
!              ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!------------------------------------------------------------------------
