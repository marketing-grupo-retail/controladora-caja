!-----------------------------------------------------------------------------
!
!          Programa: CFOFERTA.BAS
!          Proyecto: COMFANDI
!        Desarrollo: Casa Inform tica - Cali
!             Fecha: Noviembre de 1999
!
!      
!    NOTA: Usar la siguiente Liga:   LINK86 CFOFERTA,ADXACRCL.L86,EAMCSASC
!
!------------------------------------------------------------------------------
!
!            ***    Descripci¢n del Fuente      ***
!
!    Programa para manejar la Venta de Ofertas y/o Promociones
!    
!    Los PLU's de Ofertas/Promociones estan en el rango de 1 a 9999
!    Este RANGO de Ofertas/Promociones existen en EAMITEMR con el 
!    PREFIJO '99999999' para conformar la siguiente estructura:
!
!               99999999pppp, donde:    pppp = No. de Of/Promoc (1-9999)
!
!    Adicionalmente todos los PLU's de los Articulos que conforman la Promocion
!    se numeran de 01-99. Tambien utilizan un PREFIJO y se identifican de la
!    siguiente manera:
!
!               999999ppppnn, donde:    pppp = No. de Of/Promoc   (1-9999)
!                                         nn = No. de PLU Of/Prom (1-99)
!
!    Cada PLU 'nn' en la estructura anterior estara enlazado con un dato de LIGA
!    o 'Linked Item Code' con el valor 'nn+1'. La ultima liga sera el PLU
!    999999pppp99, que representa el DESCUENTO de la Oferta. Este ULTIMO Plu de
!    la LIGA, debe TENER un 'Linked Code' = 0 (CERO)
!
!    La caracteristica principal es: PRICING METHOD = 5 con un 'Alias Code'
!    que identifica el verdadero PLU de Venta.
!    
!    Toda la CADENA de 'Linked Items' se hace a traves de los PLU's con
!    PRFIJO 999999pppp para evitar el encadenamiento en la Venta Individual
!    de cualquiera de los Items de la Cadena.
!------------------------------------------------------------------------------
! Mod 16Abr2010
! Se modifica el acceso a la maestra de articulos, pasando de 46 a 77 
! caracteres, desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------------------------
!     
! Ambiente de Controlador

 %ENVIRON C

 STRING      COD1$,     KEY$,     NOM$,      REGISTRO$, BATCH$,   \!
             CODC$,     CERO$,    FECHA$,    FECHAPD$ , NOMD$,    \!
             TIT$(1),   DS$(1),   DP$(1),                         \!
             ERRFX$,    Z$,       LLAV1$,    REG$,      FINAL$,   \!
             SIRAN$,    FECRE$,   FEMOD$,    FECRED$,   FEMODD$,  \!
             NOVE$,     VARI$,    ECOD$,     SEQUENCE$, COUNTER$, \!
             FEC$,      ALM$,     INDICAT2$, IND$,      M$(1)      !

 INTEGER*1   DMCREQ,    CONLI,    DMCPO,     INDICAT0,  SWERROR,  \!
             INDICAT1,  DMCESTAT, SW.FIN,    SWREP,     INDICAT1A  !

 INTEGER*2   DMCRCSF,   DMCSTSF,  DMCERRCT,  UEX1,      UEX2,     \!
             NUMTRA%,   NUCO%,    PAGI%,     TECLA%,    MES1,     \!
             II%,       LE%,      NU%,       CAM%,      NUE%,     \!
             LI%(1),    TER%(1),  TPV%(1),   LL%,                 \!
             RAN1%,     RAN2%,    REP%,      REF%(1),   IND%       !
!------------------------  Registro de Ofertas --------------------------------
 STRING      OFNUM$                                      ! Llave Ofer
 STRING      OFNOM$                                      ! Nom Ofer
 STRING      OFDES$                                      ! Fec Ini
 STRING      OFHAS$                                      ! Fec Fin
 INTEGER*2   OFCAN%                                      ! Cantid
 INTEGER*2   OFVEN%                                      ! Vendidas
 INTEGER*4   OFVRO%                                      ! Vr Oferta
 INTEGER*4   OFVRC%                                      ! Vr Comerc
 STRING      OFCOD$                                      ! Plus Ofer
 STRING      OFVRV$                                      ! Vrs Plus
 STRING      OFCNT$                                      ! Cant de Items
!------------------------------------------------------------------------------
 REAL        PORC                                        !     
 INTEGER*2   CODBA%                                      !
 INTEGER*4   PRECI%,    FAC%,     CN%(1)                 !
 INTEGER*4   CO%(1),    VA%(1),   ACUVTA%,   DESCTO%     !
 STRING      CO$(1),    ZERO$,    GR.DATA$               !

 INTEGER*4   TOT1%,      \! Cantidad Total de Reg. Grab.
             TOT2%,      \! Cantidad de Reg. Leidos
             TOT3%,      \! Cantidad de Reg. con I/O Error
             I%,   J%,   \!
             S%,   P%,   \!
             REG%,       \!
             SX%,        \!
             HX%,        \!
             SUM%,       \!
             REGC%,      \!
             RESTA%,     \! Campo de RESTART
             USERE%,     \! Para Acumular Compras (Vr.Diario)
             VRTRAN%,    \! Valor de las Compras Acumuladas
             COD1%,      \! Codigo Inicial o Cupo Parcial
             COD2%,      \! Codigo Final o Cupo Total
             COMP%,      \! Valor Compras Acumul
             DMCERROF,   \!
             TOTBUE,     \!
             TOTCERO,    \!
             MAXTRU,     \!
             TCO%,       \!
             TCD%,       \!
             FISCON%,    \!
             FISNEQ%,    \!
             FISNZE%,    \! Fecha X y Z Contad.
             NUME%(1)     !
!------------------------------------------------------------------------------
!                       Funcion para Formatear Valores
!------------------------------------------------------------------------------
FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL                  !
  INTEGER*1 FORMAT.AMOUNT                              !
  INTEGER*4 AMT1                                       !
END FUNCTION                                           ! 
!------------------------------------------------------------------------------
!                       Subrutina para Compaginar Valores
!------------------------------------------------------------------------------
SUB COMPAG                                             !
  SIGNO$ = " "                                         !
  IF RIGHT$(VR$,1) = "-" THEN BEGIN                    !
     SIGNO$ = "-"                                      !
     VR$ = LEFT$(VR$,LEN(VR$)-1)                       !
  ENDIF                                                !
  IF LEFT$(VR$,1) = "-" THEN BEGIN                     !
     SIGNO$ = "-"                                      !
     VR$ = RIGHT$(VR$,LEN(VR$)-1)                      !
  ENDIF                                                !
  IF LEN(VR$) > 6 THEN BEGIN                           !
     VR$ = LEFT$(VR$,LEN(VR$)-6)   +                  \!
           "," + MID$(VR$,LEN(VR$)-5,3)   +           \!
           "," + RIGHT$(VR$,3)                         !
  ENDIF ELSE BEGIN                                     !
    IF LEN(VR$) > 3 THEN BEGIN                         !
       VR$ = LEFT$(VR$,LEN(VR$)-3) + ","+RIGHT$(VR$,3) !
    ENDIF                                              !
  ENDIF                                                !
  CODIGO$ = VR$                                        !
  VR$ = RIGHT$("          " + VR$ , 10) + SIGNO$       !
END SUB                                                !
!------------------------------------------------------------------------------
!                       Subrutina para Lectura EAMITEMR
!------------------------------------------------------------------------------
SUB LEA.ITEM                                           !
   LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"                 !
   READ FORM LEC$;#2 KEY KEY$;                        \! Lee EAMITEMR
        KEY$,FIL1$,DEPT$,FIL2$,PRIC$,LNK$,DESC$,FIL2$,\!
        GR.DATA$
   IF RIGHT$(UNPACK$(FIL1$),1) = "5" THEN BEGIN        !
      KEY$ = PACK$("00") + PRIC$                       !
      CODIG$ = UNPACK$(KEY$)                           ! 
      READ FORM LEC$;#2 KEY KEY$;                     \! Lee EAMITEMR
      KEY$,FIL1$,DEPT$,FIL2$,PRIC$,LNK$,DESC$,FIL2$,  \!
      GR.DATA$
   ENDIF                                               !
END SUB                                                !
!------------------------------------------------------------------------------
!
!                             GETN1/GETN2/GETN4
!
!      These routines extract a one/two/four byte integer from a string.
!      P2 is the offset within the string
!
!------------------------------------------------------------------------------
 
  FUNCTION GETN1(P1$,P2) EXTERNAL
     INTEGER*1 GETN1
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN2(P1$,P2) EXTERNAL
     INTEGER*2 GETN2
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
!------------------------------------------------------------------------------
!
!                            PUTN1/PUTN2/PUTN4
!
!       These routines insert a one/two/four byte integer into a string.
!       P2 is the offset within the string and P3 is the source integer
!
!------------------------------------------------------------------------------
  
  FUNCTION PUTN1(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*1 P3
  END FUNCTION
  
  FUNCTION PUTN2(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2,P3
  END FUNCTION
  
  FUNCTION PUTN4(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*4 P3
  END FUNCTION
!----------------------------------------------------------------------------
!                  Subrutina para Toma de Teclas de Funcion
!----------------------------------------------------------------------------
SUB TECLA                                               !
  WHILE NOT CONSTAT%                                    !
    IF VIS% = 0 THEN EN% = INKEY                        !
    IF VIS% = 1 THEN EN% = CONCHAR%                     !
    SW1 = SW1 + 1                                       !
    IF SW1 = 1 THEN EN$ = CHR$(EN%) :  EL%=EN%          !
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
   LOCATE 23,1                                          !
   PRINT "Esc"     ; C27$ + "p" ; C27$ + B03$;          !
   PRINT " *Fin* " ; C27$ + "q" ; C27$ + B07$;          !
   PRINT "F1"      ; C27$ + "p" ; C27$ + B03$;          !
   PRINT " Ayuda " ; C27$ + "q" ; C27$ + B07$;          !
   PRINT " F2"     ; C27$ + "p" ; C27$ + B03$;          !
   PRINT "       " ; C27$ + "q" ; C27$ + B07$;          !
   PRINT " F3"     ; C27$ + "p" ; C27$ + B03$;          !
   PRINT " Salir " ; C27$ + "q" ; C27$ + B07$           !
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
    LOCATE 22,5                                         !
    PRINT "   Para Continuar, presione ENTER ";         !
    CALL TECLA                                          !
    IF EN% = 9 THEN EN$ = "F3"                          !
    IF EN$ <> "F3" AND EN% <> 13 THEN GOTO NOTECLA      !
    IF EN$ = "F3" THEN CLEARS : STOP                    !
END SUB                                                 !
!------------------------------------------------------------------------------
!                Verifica Codigos EAN/UPC co Check Digit
!------------------------------------------------------------------------------
SUB VERIFEAN                                            !
  IF LEFT$(DAT$,1) = "-" THEN BEGIN                     !
     DAT$ = RIGHT$(DAT$,LEN(DAT$)-1) + "-"              !
  ENDIF                                                 !
  BARRA$ = DAT$                                         !
  IF CODBA% = 1 THEN BEGIN                              !
     IF LEFT$(BARRA$,6) = "000000" THEN CODBA% = 0      !
     IF CODBA% = 1 THEN BEGIN                           !
        IF RIGHT$(BARRA$,1) = "-" THEN                 \!
           DAT$ = LEFT$(BARRA$,LEN(BARRA$)-2) +  "-"   \!
        ELSE                                           \!
           DAT$ = LEFT$(BARRA$,LEN(BARRA$)-1)           !
     ENDIF                                              !     
     CODBA% = 0                                         !
ENDIF                                                   !
END SUB                                                 !
!----------------------------------------------------------------------------
!                     Funci¢n para Toma de Datos
!----------------------------------------------------------------------------
FUNCTION DATO$(TX$,FI%,CL%,TI$,LN%)                     ! Fila,Col,Tipo y Long
  INTEGER*2 FI%, CL%, LN%                               !
  STRING    TX$, TI$, DATO$                             !
  LOCATE FI%,CL%                                        ! Ubica Cursor
  PRINT C27$+"p" ; C27$ + B02$;                         ! Inic Video Rever/Verde
  IF TX$ = "" THEN PRINT STRING$(LN%," ");    ELSE     \! Despliega Texto
     PRINT TX$                                          !
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
      SW1 = 0 : EL% = 0                                 !
      CALL TECLA                                        !
      X% = EN%                                          !
      IF X% = 27 THEN EN$ = "F3"                        !
      IF X% = 9  THEN X%  =  13                         !
      IF EN$ = "F3" THEN GOTO REGRESO                   !
      IF X% = 13 THEN BEGIN                             ! Si es Enter
         IF TI$ = "A" THEN BEGIN                        ! con letrero
            DAT$ = LEFT$(DAT$+STRING$(LN%," "),LN%)     ! Ajusta Spaces
         ENDIF ELSE BEGIN                               ! Si son
            IF TI$ = "9" THEN BEGIN                     ! numeros
               IF LEFT$(DAT$,1) = "-" THEN BEGIN        !
                  DAT$ = RIGHT$(DAT$,LEN(DAT$)-1) + "-" !
               ENDIF                                    !
               DAT$ = RIGHT$(STRING$(LN%,"0")+DAT$,LN%) ! Ajusta CEROS
            ENDIF                                       ! y termina
         ENDIF                                          !
         I% = LN%                                       ! Reset de I
         CALL VERIFEAN                                  !
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
               ".,:;-#/%()1234567890",1)                !
      ENDIF                                             !
      IF TI$ = "9" THEN BEGIN                           ! Val Numeros
         II% = MATCH(CAR$,"0123456789-%",1)             !
      ENDIF                                             !
      IF II% = 0 THEN I%=I%-1 : LOCATE FI%,CL%+I%       ! NO esta en Match.Repi
      IF II% > 0 THEN DAT$ = DAT$ + CHR$(X%)            ! Arma letrero
  NEXT I%                                               !
  CALL VERIFEAN                                         !
  DATO$ = DAT$                                          !
  LOCATE FI%,CL%                                        ! Ubica Cursor
  PRINT C27$+"p" ; C27$+B03$;                           ! Inic Video Rever/Cyan
  PRINT DAT$;                                           ! Despliega Texto
  PRINT C27$+"q" ; C27$+B07$                            ! Fin Video Rever/Gris
  EXIT FUNCTION                                         !
  REGRESO:                                              !
  IF VIS% = 0 THEN CLEARS : STOP                        ! 
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
   LOCATE 22,1                                          ! Ubica Cursor
   PRINT C27$ + B04$;                                   ! Inic Video Rever/Rojo
   PRINT VERROR$+"                         ";           ! y la muestra
   PRINT C27$+B07$;                                     ! Fin Video Rever/Gris
   CALL TECLA                                           ! Espera desbloqueo
   LOCATE 22,1                                          ! Ubica Cursor  
   PRINT STRING$(70," ");                               ! y Borra Linea
FEND                                                    !

SUB TOMANIT                                             !
  ERCOD$ = "  "                                         !
  NIT1$ = DATO$("",05,14,"9",03)                        !
  NIT2$ = DATO$("",05,18,"9",03)                        !
  NIT3$ = DATO$("",05,22,"9",03)                        !
  DIGI$ = DATO$("",05,26,"9",01)                        !
  IF VAL(NIT1$) + VAL(NIT2$) +                         \!
     VAL(NIT3$)+VAL(DIGI$) = 0 THEN BEGIN               !
     ERCOD$ = "ER"                                      !
     MENS$ = "ERROR...  NIT Inv lido..!"                !
     CALL VERROR(MENS$)                                 !
  ENDIF                                                 !
END SUB                                                 !
!----------------------------------------------------------------------------
!               Subrutina para Desplegar Ayudas en Pantalla
!----------------------------------------------------------------------------
SUB AYUDA                                                !
  FOR I% = K% TO 500 STEP 1                              !
  IF  I% > K% THEN BEGIN                                 !
     IF RIGHT$(AY$(I%),2)="**" THEN BEGIN                !
        I%=500                                           !
        GOTO AVAREG                                      !
     ENDIF                                               !
  ENDIF                                                  !
  LOCATE 17+I%-K%,30                                     !
  PRINT C27$ + B06$;                                     ! Inic Video Brown
  PRINT AY$(I%);                                         !
  PRINT C27$ + B07$                                      ! Fin Video Rever/Gris
AVAREG:                                                  !
  IF I%-K% > 5  AND I% < 500 THEN BEGIN                  !
     LOCATE 22,01                                        !
     PRINT "Sale/Av/Reg: S/A/R ? ";                      !
     RE% = CONCHAR%                                      !
     RE$ = CHR$(RE%)                                     !
     IF RE$ <> "S" AND                                  \!
        RE$ <> "A" AND                                  \!
        RE$ <> "R" THEN GOTO AVAREG                      !
     IF RE$="A" AND LEFT$(AY$(I%),1)=" " THEN RE$="R"    !
     IF RE$ = "R" THEN BEGIN                             !
        IF I%-KK% > 11 THEN I%=I%-12 : K% = I%  ELSE    \!
           I% = I% - 6                                   !
     ENDIF                                               !
     IF RE$ = "A" THEN K% = I%                           !
     IF RE$ = "S" THEN I% = 500                          !
  ENDIF                                                  !
  NEXT I%                                                !
END SUB                                                  !

!------------------------------------------------------------------------------
!                   Borra parte Final de la Pantalla
!------------------------------------------------------------------------------
SUB BORRAR                                               !
  FOR I% = 1 TO 10                                       !
      LOCATE I% + 11 , 1                                 !
      PRINT STRING$(70," ")                              !
  NEXT I%                                                !
  PRINT STRING$(70," ");                                 !
END SUB                                                  !
!------------------------------------------------------------------------------
!                   Rutina para Verificar Registro de Ofertas
!------------------------------------------------------------------------------
SUB LEAOFER                                              !
    ECOD$ = "  "                                         ! Inicia Estado Leer
    LEC$ = "C2 C18 2C4 2I2 2I4 C180 C120 C30"            !
    KEY$ = PACK$(OFERT$)                                 ! Arma llave para leer
    READ FORM LEC$;#1 KEY KEY$;                         \! Lee Reg Var/Fiscales
         OFNUM$,OFNOM$,OFDES$,OFHAS$,OFCAN%,            \! Llave,Nit,CF,Nom,Dir
         OFVEN%,OFVRO%,OFVRC%,OFCOD$,OFVRV$,OFCNT$       !
          
    IF ECOD$ = "  " THEN BEGIN                           ! Si existe Registro
       FIS.INI$ = UNPACK$(OFDES$)                        !
       FIS.FIN$ = UNPACK$(OFHAS$)                        !
       CANOF% = OFCAN%                                   !
       CANVE% = OFVEN%                                   !
       VROFE% = OFVRO%                                   !
       VRCOM% = OFVRC%                                   !
       DIM CN%(30)                                       !
       DIM CO%(30)                                       !
       DIM CO$(30)                                       !
       FOR Z% = 1 TO 30                                  !
           CO$(Z%) = ZERO$                               !
       NEXT Z%                                           !
       DIM VA%(30)                                       !
       FOR J% = 1 TO 30                                  !
           CO$(J%) = UNPACK$(MID$(OFCOD$,J%*6 - 5,6))    !
           VA%(J%) = GETN4(OFVRV$,J%*4 - 4)              !
           CN%(J%) = GETN1(OFCNT$,J% - 1)                !
       NEXT J%                                           !
       OBSER$ = "Y"                                      !
    ENDIF ELSE BEGIN                                     ! Fin Cond
       OBSER$ = "N"                                      ! Error Empresa
    ENDIF                                                ! Fin Cond
END SUB                                                  !
!------------------------------------------------------------------------------
!                     Rutina Depliegue de Pantalla
!------------------------------------------------------------------------------
SUB PANTA1                                               !
 OFEMER:                                                 !
  CLEARS                                                 !
  LOCATE 2,1                                             !
  PRINT "  CF001S                  OFERTAS Y"  +        \!
        " MERCADOS COMFANDI     %Desc ¢ Vr?  nnnn%"      !
  PRINT "                        ";DATEM$;"   ";TIM$     !
  OFME$=DATO$("   Oferta(O)  Mercado(M) ? ",04,05,"A",01)!
  IF EN$ = "F3" THEN CLEARS : STOP                       !
  IF OFME$ <> "O" AND OFME$ <> "o" AND                  \!
     OFME$ <> "M" AND OFME$ <> "m" THEN BEGIN            !
     LOCATE 22,1                                         !
     MENS$ = "Debe Seleccionar entre Oferta ¢ Mercado..!"!
         CALL VERROR(MENS$)                              !
         GOTO OFEMER                                     !
  ENDIF                                                  !
  IF OFME$ = "o" OR OFME$ = "O" THEN                    \!
     OFME$ = "O" : LEOM$ = "Oferta"                      !
  IF OFME$ = "m" OR OFME$ = "M" THEN                    \!
     OFME$ = "M" : LEOM$ = "Mercad"                      !
  LOCATE 4,1 : PRINT STRING$(80," ")                     !
  LOCATE 5,1                                             !
  PRINT "   ";LEOM$;" No.:";                             !
  PRINT "      Cantidad:      Vendidas:       Vr/Neto ";LEOM$;":"
  PRINT "  Descripci¢n:                                     Vr/Desc Acumul:"
  PRINT "        Desde:               Hasta:               Vr Comer/";LEOM$;":"
  PRINT "       C¢digo:                                    Precio/Vta Item:"
  PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄ¿"
  PRINT "³  C¢d Item ³Can³   Valor ³  C¢d Item ³Can³   Valor ³  C¢d Item ³Can³   Valor ³"
  PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
END SUB                                                  !

!----------------------------------------------------------------------------
!                Rutina para Crear/Compaginar Hora y Fecha 
!----------------------------------------------------------------------------

  SUB CREAFECHA                                          !
      FRA.TIM$ = TIME$                                   ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                       ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                      ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                      ! Arma segundos
      FRA.FEC$ = DATE$                                   ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                       ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                      ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                      ! Arma dia
      MES1     = VAL(FRA.MM$)                            ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"           ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"          ! Si es Siglo XXI
      FHOY$   = M$(MES1)+". "+STR$(VAL(FRA.DD$))        \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                      !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +      \! Arma la Linea
               "    de "+FHOY$                           ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                      ! Salva Mes y Dia
END SUB                                                  !
!---------------------------------------------------------------------------
!              Rutina para Verificar Fechas y Rangos de Fechas
!-----------------------------------------------------------------------------
SUB FIS.VALFEC                                           !
    IF VAL(LEFT$(DATE$,2)) < 98  THEN BEGIN              ! Define ano 2000
       FIS.FHO$ = "20"+DATE$                             ! y coloca prefijo
    ENDIF ELSE BEGIN                                     ! de nuevo siglo
       FIS.FHO$ = "19"+DATE$                             ! o siglo XX
    ENDIF                                                !
    IF FIS.ANO$ < FIS.FHO$ THEN BEGIN                    !
       FIS.ERR% = 5                                      ! Si Fecha Z mayor HOY 
    ENDIF                                                !
    IF FIS.ENT% > 0 AND                                 \! Si Fecha Fin Z menor
       FIS.ANO$ < FIS.INI$ THEN BEGIN                    !
       FIS.ERR% = 4                                      ! fecha Inicial o
    ENDIF                                                !
    IF VAL(RIGHT$(FIS.ANO$,2)) > 31 OR                  \! Mes o dia fuera de 
       VAL(RIGHT$(FIS.ANO$,2)) < 01 OR                  \! rangos, entonces 
       VAL(MID$(FIS.ANO$,5,2)) < 01 OR                  \! indica cod de Error
       VAL(MID$(FIS.ANO$,5,2)) > 12 THEN FIS.ERR%=3      ! 
END SUB                                                  !
!---------------------------------------------------------------------------
!                Rutina de Impresion de Encabezado de Pagina
!---------------------------------------------------------------------------
SUB CABEZA                                               !
   LPRINTER                                              !
   IF CONLI > 55 THEN BEGIN                              ! Si Hoja Full
      CONLI = 5                                          ! Inicia Cont Lin
      PAGI% = PAGI% + 1                                  !
      PRINT CHR$(12)                                     ! Salto de Pag
      PRINT " "                                          !
      PRINT "         LISTADO DE OFERTAS         "  +   \!
            "                             Pag. ";PAGI%   !
      PRINT "                        ";DATEM$;"   ";TIM$ !
      PRINT "   ";LEOM$;" No.:";                         !
      PRINT "      Cantidad:      Vendidas:       Vr/Neto ";LEOM$;":"
      PRINT "  Descripci¢n:                                    Valor Descuento:"
      PRINT "        Desde:               Hasta:               Vr Comer/";LEOM$;":"
      PRINT "       C¢digo:                                    Precio/Vta Item:"
      PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄ¿"
      PRINT "³  C¢d Item ³Can³   Valor ³  C¢d Item ³Can³   Valor ³  C¢d Item ³Can³   Valor ³"
      PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
      PRINT "       ";NOAL$;"  No : ";ALM$;              !
      PRINT "  "                                         !
      PRINT "---------------------------------------------------------------------------------------"
   ENDIF                                                 ! Fin Cond
END SUB                                                  !

SUB DESPLEGAR                                            !
  CALL RLIN(NIT1$,05,14)                                 !
  LOCATE 22,1                                            !
  PRINT "   Para Continuar, presione ENTER ";            !
  CALL TECLA                                             !
END SUB                                                  !

!------------------------------------------------------------------------------
!                   Rutina final de cierre del Archivo
!------------------------------------------------------------------------------
SUB FINAL                                                !
   SW.FIN = 1                                            !
   CLOSE 1                                               ! Cierra OFERTAS
   CLOSE 2                                               ! Cierra EAMITEMR
   LOCATE 22,10 : PRINT "    Proceso Finalizado.."  +   \!
                        "                         "      !
   STOP                                                  !
END SUB                                                  !
!------------------------------------------------------------------------------
!                      Rutina de Despliegue de Datos
!------------------------------------------------------------------------------
SUB LISTAR                                               !
    CALL BORRAR                                          !
    LINEA$ = ""                                          !
    T% = 0                                               !
    LOCATE 12,1                                          !
    FOR I% = 1 TO 30                                     !
        CONL% = 0                                        !
        IF CO$(I%) <> ZERO$ THEN BEGIN                   !
           VR$ = STR$(CN%(I%))                           !
           CALL COMPAG                                   !
           VR$ = LEFT$(VR$,LEN(VR$)-1)                   !
           CNTI$ = RIGHT$("   "+VR$,3)                   !

           VR$ = CO$(I%)                                 !
           CALL COMPAG                                   !
           VR$ = CODIGO$                                 !
           CODI$ = ""                                    !
           FOR K% = 1 TO LEN(VR$)                        !
               IF MID$(VR$,K%,1) <> "," THEN BEGIN       !
                  CODI$=CODI$+MID$(VR$,K%,1)             !
               ENDIF                                     !
           NEXT K%                                       !
           CODI$ = RIGHT$("            "+CODI$,12)       !

           VR$ = STR$(VA%(I%)*CN%(I%))                   !
           CALL COMPAG                                   !
           VALO$ = RIGHT$("         "+VR$,11)            !

           LINEA$ = LINEA$ + CODI$ + CNTI$ + VALO$       !
           LOCATE 12+T%,1                                !
           PRINT " "+LINEA$+STRING$(79-LEN(LINEA$)," ")  !
           IF LEN(LINEA$) > 70 THEN BEGIN                !
              T% = T% + 1                                !
              LINEA$ = ""                                !
              CONL% = CONL% + 1                          !
              IF CONL% = 14 THEN CONL% = 0 : CALL BORRAR !
           ENDIF                                         !
        ENDIF                                            !
    NEXT I%                                              !
END SUB                                                  !
SUB ACUMULA                                              !
   ACUVTA% = ACUVTA% + (PRECI%)                          !
   IF PORC <> 0 THEN BEGIN                               !
      DESCTO% = INT(ACUVTA% * PORC)                      !
      OFVRO% = ACUVTA% - DESCTO%                         !
   ENDIF ELSE BEGIN                                      !
      IF VAL(DESCU$) > 0 THEN BEGIN                      !
         DESCTO% = VAL(DESCU$)                           !
         OFVRO% = ACUVTA% - DESCTO%                      !
      ENDIF                                              !
   ENDIF                                                 !
   IF VAL(DESCU$) = 0 THEN BEGIN                         !
      DESCTO% = ACUVTA% - OFVRO%                         !
   ENDIF                                                 !

   VR$ = STR$(OFVRO%)                                    !
   CALL COMPAG                                           !
   VALO$ = VR$                                           !
   CALL RLIN(VALO$,5,70)                                 !

   VR$ = STR$(DESCTO%)                                   !
   CALL COMPAG                                           !
   VALO$ = VR$                                           !
   CALL RLIN(VALO$,6,70)                                 !

   VR$ = STR$(ACUVTA%)                                   !
   CALL COMPAG                                           !
   VALO$ = VR$                                           !
   CALL RLIN(VALO$,7,70)                                 !

   CALL RLIN(DESC$,8,31)                                 !
   VR$ = STR$(PRECI%)                                    !
   CALL COMPAG                                           !
   VALO$ = VR$                                           !
   CALL RLIN(VALO$,8,70)                                 !
END SUB
! ---------------------  INICIO DEL PROGRAMA   -----------------------------
   ON ERROR GOTO ERRTRAP                                 !
   C27$ = CHR$(27)                                       ! Escape
   OPC$ = "1"                                            !
   ZERO$ = "000000000000"                                !
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
!----------------------   Llena arreglo de meses   ----------------------------
   DIM M$(12)
   M$(01)="Ene"  :  M$(02)="Feb"  :  M$(03)="Mar"        !
   M$(04)="Abr"  :  M$(05)="May"  :  M$(06)="Jun"        !
   M$(07)="Jul"  :  M$(08)="Ago"  :  M$(09)="Sep"        !
   M$(10)="Oct"  :  M$(11)="Nov"  :  M$(12)="Dic"        !
!----------------------   Llena arreglo de Ayudas   ---------------------------
   DIM LI%(999)                                          ! 
   DIM NUME%(999)                                        ! 
   DIM TER%(999)                                         !
   DIM AY$(500)                                          !
!------------------------------------------------------------------------------
   FR$   = CHR$(13)+CHR$(10)                             ! 0D0A
   M1 = VAL(MID$(DATE$,3,2))                             ! Toma Mes Actual
   D$ = RIGHT$(DATE$,2)                                  ! Toma Dia Actual
   A$ = LEFT$(DATE$,2)                                   ! y Ano Actual
   FO1$ = "###.##"                                       !
   IF LEFT$(A$,1)<"9" THEN A$=" del 20" ELSE A$=" de 19" ! Prefijo Ano 2000-2089
   IF LEFT$(D$,1) = "0" THEN D$ = RIGHT$(D$,1)           ! Quita CERO Izq
   DATEM$ = M$(M1)+"/"+D$+A$+LEFT$(DATE$,2)              ! Arma la Fecha
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
   ARCH1$ = "C:\ADX_UDT1\CFOFERTA.DAT"                   ! Define Ctrl Almac
   ARCH2$ = "EAMITEMR"                                   ! Define Arch Artic
   OPEN ARCH1$ KEYED  RECL 370 AS 1                      ! Abre Ofertas
   OPEN ARCH2$ KEYED  RECL 77  AS 2                      ! Abre EAMITEMR
   CALL CREAFECHA                                        !
   FEC1$ = FRA.LIN$                                      !
OFERTA:                                                  !
   CALL PANTA1                                           !
   CALL FUNCKEY                                          !
   LOCATE 5,18                                           !
   OFERT$ = DATO$("",05,16,"9",04)                       !
   IF EN$ = "F3" THEN CLEARS : STOP                      !
   IF VAL(OFERT$) = 0 THEN GOTO GRABAR                   !
   KEY$ = PACK$("88888888" + OFERT$)                     !
   SWLEC% = 1                                            !
   CALL LEA.ITEM                                         !
   IF SWLEC% = 0 THEN SWLEC% = 2                         !
   IF SWLEC% = 0 THEN GOTO OFERTA                        !
   DIM CN%(30)                                           !
   DIM CO%(30)                                           !
   DIM CO$(30)                                           !
   PORC = 0                                              !
   FOR Z% = 1 TO 30                                      !
       CO$(Z%) = ZERO$                                   !
   NEXT Z%                                               !
   DIM VA%(30)                                           !
   DIM DP$(30)                                           !
   DIM DS$(30)                                           !
   CALL LEAOFER                                          !
   IF ERCOD$ <> "  " THEN  GOTO CANTID                   !
   IF OBSER$ = "Y" THEN BEGIN                            !
      MENS$ = "   OJO....YA existe ESTA Oferta/Mercad..!"!
      CALL VERROR(MENS$)                                 !
      CALL RLIN(STR$(OFCAN%),05,31)                      !
      CALL RLIN(STR$(OFNVE%),05,46)                      !
      VR$ = STR$(OFVRO%)                                 !
      CALL COMPAG                                        !
      VALO$ = VR$                                        !
      CALL RLIN(VALO$,05,68)                             !
      CALL RLIN(OFNOM$,06,16)                            !
      VR$ = STR$(OFVRC%-OFVRO%)                          !
      CALL COMPAG                                        !
      VALO$ = VR$                                        !
      CALL RLIN(VALO$,06,68)                             !

      LOCATE 7,16                                        !
      PRINT LEFT$(FIS.INI$,4)  + "/" +                  \!
            MID$(FIS.INI$,5,2) + "/" +                  \!
            RIGHT$(FIS.INI$,2)                           !
      LOCATE 7,37                                        !
      PRINT LEFT$(FIS.FIN$,4)  + "/" +                  \!
            MID$(FIS.FIN$,5,2) + "/" +                  \!
            RIGHT$(FIS.FIN$,2)                           !
      VR$ = STR$(OFVRC%)                                 !
      CALL COMPAG                                        !
      VALO$ = VR$                                        !
      CALL RLIN(VALO$,07,68)                             !
      CALL LISTAR                                        !
      NUE% = 1                                           !
      GOTO BORRAREG                                      !
   ENDIF                                                 !
   ACUVTA% = 0  :  DESCTO% = 0  : FIS.ENT% = 0           !
   OFNUM$ = PACK$(OFERT$)                                !
DESCUEN:                                                 !
   IF SWLEC% = 1 THEN BEGIN                              !
      DESCU$ = "0"                                       !
   ENDIF ELSE BEGIN                                      !
      DESCU$ = DATO$("",03,70,"9",07)                    !
   ENDIF                                                 !
   LOCATE 3,70 : PRINT DESCU$                            !
   IF EN$ = "F3" THEN GOTO OFERTA                        !
   IF RIGHT$(DESCU$,1) = "%" THEN BEGIN                  !
      DESCU$ = STR$(VAL(LEFT$(DESCU$,LEN(DESCU$)-1)))    !
      IF LEN(DESCU$) < 3 THEN BEGIN                      !
         MENS$ = "Req. %Desc con 2 Decimales.."+        \!
                 "  Ej.: 1% --> 100 y ENTER"             !  
         CALL VERROR(MENS$)                              !
         GOTO DESCUEN                                    !
      ENDIF                                              !
      FAC% = VAL(DESCU$)                                 !
      PORC = FLOAT(FAC%) / 10000                         !
      LOCATE 3,70 : PRINT USING FO1$;FLOAT(FAC%)/100;    !
      PRINT "%"                                          !
   ENDIF ELSE BEGIN                                      !
      DESCTO% = VAL(DESCU$)                              !
      VR$ = STR$(VAL(DESCU$))                            !
      CALL COMPAG                                        !
      VALO$ = VR$                                        !
      CALL RLIN(VALO$,3,70)                              !
   ENDIF                                                 !
   VROFE$ = "0"                                          !
   IF VAL(DESCU$) = 0 THEN BEGIN                         !
      IF SWLEC% = 1 THEN BEGIN                           !
         VROFE$ = UNPACK$(PRIC$)                         !
      ENDIF ELSE BEGIN                                   !
         VROFE$ = DATO$("",05,70,"9",08)                 !
      ENDIF                                              !
      IF VAL(VROFE$) = 0 THEN BEGIN                      !
         MENS$ = "La Oferta/Mercad debe tener Descuento ¢ Valor Fijo..!"
         CALL VERROR(MENS$)                              !
         GOTO OFERTA                                     !
      ENDIF                                              !
      IF EN$ = "F3" THEN GOTO OFERTA                     !
      OFVRO% = VAL(VROFE$)                               !
   ENDIF                                                 !
   VR$ = STR$(VAL(VROFE$))                               !
   CALL COMPAG                                           !
   VALO$ = VR$                                           !
   CALL RLIN(VALO$,5,70)                                 !
TOMANOF:                                                 !
   IF SWLEC% = 1 THEN BEGIN                              !
      NOFER$ = DESC$                                     !
      LOCATE 6,16 : PRINT DESC$                          !
   ENDIF ELSE BEGIN                                      !
      NOFER$ = DATO$("",06,16,"A",18)                    !
   ENDIF                                                 !
   OFNOM$ = NOFER$                                       !
   IF EN$ = "F3" THEN GOTO OFERTA                        !
CANTID:                                                  !
   CANTI$ = DATO$("",05,30,"9",04)                       !
   IF EN$ = "F3" THEN GOTO OFERTA                        !
   OFCAN% = VAL(CANTI$)                                  !
   IF OFCAN% = 0 THEN GOTO CANTID                        !
FISLOOP:                                                 !
   IF FIS.ENT% = 0 THEN BEGIN                            ! Pide Fecha Inicial
      DESDE$ = DATO$("",07,16,"9",06)                    !
      DAT$ = DESDE$                                      !
   ENDIF ELSE BEGIN                                      !
      HASTA$ = DATO$("",07,37,"9",06)                    !
      DAT$ = HASTA$                                      !
   ENDIF                                                 !
   IF VAL(LEFT$(DAT$,2)) < 98  THEN BEGIN                !
      FIS.ANO$ = "20"+RIGHT$(DAT$,6)                     !
   ENDIF ELSE BEGIN                                      !
      FIS.ANO$ = "19"+RIGHT$(DAT$,6)                     !
   ENDIF                                                 !
   IF EN$ = "F3" THEN GOTO OFERTA                        !
   CALL FIS.VALFEC                                       !
   IF FIS.ERR% = 3 THEN BEGIN                            !
      MENS$ = "Fecha Errada..  Intente Otra Vez.! "      !  
      CALL VERROR(MENS$)                                 !
   ENDIF                                                 !
   IF FIS.ERR% = 4 THEN BEGIN                            !
      MENS$ = "Fecha Final debe ser Mayor a Fecha de Inicio..!"
      CALL VERROR(MENS$)                                 !
   ENDIF                                                 !
   IF FIS.ERR% = 5 THEN BEGIN                            !
      MENS$ = "Esta Fecha SOLO puede ir desde HOY..!"    !
      CALL VERROR(MENS$)                                 !
   ENDIF                                                 !
   IF FIS.ERR% > 0 THEN BEGIN                            !
      FIS.ERR%=0  :  FIS.ENT%=0  :  FIS.BOR%=0           !
      GOTO FISLOOP                                       !
   ENDIF                                                 !
   IF FIS.ENT% = 0 THEN FIS.INI$ = FIS.ANO$              !
   IF FIS.ENT% = 1 THEN FIS.FIN$ = FIS.ANO$              !
   IF FIS.ENT% < 1 THEN BEGIN                            !
      FIS.ENT% = FIS.ENT% + 1                            !
      GOTO FISLOOP                                       !
   ENDIF                                                 !
   LOCATE 7,16                                           !
   PRINT LEFT$(FIS.INI$,4)  + "/" +                     \!
         MID$(FIS.INI$,5,2) + "/" +                     \!
         RIGHT$(FIS.INI$,2)                              !
   LOCATE 7,37                                           !
   PRINT LEFT$(FIS.FIN$,4)  + "/" +                     \!
         MID$(FIS.FIN$,5,2) + "/" +                     \!
         RIGHT$(FIS.FIN$,2)                              !
   
   OFDES$ = PACK$(FIS.INI$)                              !
   OFHAS$ = PACK$(FIS.FIN$)                              !
   OFVEN% = 0                                            !
ENTRAR:                                                  !
   CODBA% = 1                                            !
   CODIG$ = DATO$("",08,16,"9",14)                       !
   IF EN$ = "F3" THEN GOTO OFERTA                        !
   BOR% = 1                                              !
   IF LEFT$(CODIG$,1) = "-" THEN BEGIN                   ! Si es Eliminar
      BOR% = -1                                          ! Marca Flag
      CODIG$ = RIGHT$(CODIG$,LEN(CODIG$)-1)              ! Quita signo Menos
   ENDIF                                                 !
   IF RIGHT$(CODIG$,1) = "-" THEN BEGIN                  ! Si es Eliminar
      BOR% = -1                                          ! Marca Flag
      CODIG$ = LEFT$(CODIG$,LEN(CODIG$)-1)               ! Quita signo Menos
   ENDIF                                                 !
   CODIG$ = RIGHT$(CODIG$,12)                            ! 
   IF VAL(CODIG$) = 0 THEN BEGIN                         !
      IF DESCTO% < 0 OR                                 \!
         DESCTO% > OFVRO% OR                            \!
         ACUVTA% < 1 THEN BEGIN                          !
         MENS$ = "La Oferta/Mercado no es Comercialmente Recomendable..!"
         CALL VERROR(MENS$)                              !
         GOTO ENTRAR                                     !
      ENDIF ELSE BEGIN                                   !
         GOTO GRABAR                                     !
      ENDIF                                              !
   ENDIF                                                 !
   KEY$ = PACK$(RIGHT$("000000000000"+ CODIG$,12))       !
   LEC$ = "C6 C4 C2 2C5 C2 C18 C4"                       !
   CALL LEA.ITEM                                         !
   PRECI% = VAL(UNPACK$(PRIC$))*BOR%                     !
   IF PRECI% = 0 THEN GOTO ENTRAR                        !
   FOR I% = 1 TO 30                                      !
       IF CO$(I%) = CODIG$ OR                           \!
          (CO$(I%) = ZERO$ AND                          \!
           BOR% > 0) THEN BEGIN                          !
             CO$(I%) = CODIG$                            !
             CN%(I%) = CN%(I%) + BOR%                    !
             VA%(I%) = ABS(PRECI%)                       !
             DS$(I%) = DESC$                             !
             DP$(I%) = DEPT$                             !
             REP% = CN%(I%)                              !
             IF CN%(I%) < 1 THEN BEGIN                   !
                  CN%(I%)=0 : CO$(I%)=ZERO$ : VA%(I%)=0  !
             ENDIF                                       !
             REP% = CN%(I%)                              !
             CALL ACUMULA                                !
             I% = 30                                     !
       ENDIF                                             !
   NEXT I%                                               !
   CALL LISTAR                                           !
   GOTO ENTRAR                                           !
GRABAR:                                                  !
  CAM$ = DATO$("Desea Grabar Oferta/Mercado S/N ? ",22,5,"A",1)   
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO GRABAR        !
  LOCATE 22,5  :  PRINT STRING$(70," ")                  !
  IF CAM$ = "N" THEN GOTO BORRAREG                       !
  CAM$ = DATO$(".. Est  Seguro S/N ? ",22,5,"A",1)       !
  IF EN$ = "F3" THEN CALL FINAL                          ! 
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO GRABAR        !
  LOCATE 22,5  :  PRINT STRING$(70," ")                  !
  IF CAM$ = "N" THEN GOTO GRABAR                         !

  IF ACUVTA% < 1 THEN GOTO OFERTA                        !
  KEY$  = PACK$("99999999") + OFNUM$                     ! Arma Llave OFERTA
  FIL1$ = PACK$("00000000")                              ! Indicat CERO Mercado
  IF OFME$ = "O" THEN FIL1$ = PACK$("00000100")          ! Indicat UNO Oferta
  DEPT$ = PACK$("0995")                                  ! Depto 
  FIL2$ = "0000000000"                                   ! Fam,Mult,Cant CERO
   PRIC$ = PACK$(RIGHT$(FIL2$ + STR$(OFVRO%),10))        ! Precio Oferta
!  PRIC$ = PACK$(RIGHT$(FIL2$ + STR$(ACUVTA%),10))        ! Precio Oferta
  FIL2$ = PACK$(FIL2$)                                   ! User Data
  FIL3$ = FIL2$                                          ! Guarda Dato User

  DIM REF%(30)  :  IND% = 0                              ! Array Referencial
  FOR I% = 1 TO 30                                       ! Loop para COMPRIMIR
      IF CO$(I%) <> ZERO$ THEN BEGIN                     ! Posic con Datos
         IND% = IND% + 1                                 ! Incr Indice
         REF%(IND%) = I%                                 ! Guarda Apuntador
         ULTLIGA$ = STR$(IND%)                           !
      ENDIF                                              !
  NEXT I%                                                ! Siguiente Loop
  IND% = 0                                               !
  ULTLIGA$ = RIGHT$("0000"+ULTLIGA$,4)                   !
  LNK$  = PACK$(ULTLIGA$)                                ! Link para Desc.Misc
  IF DESCTO% <> 0 THEN LNK$ = PACK$("0099")              ! Link para Desc.Misc
  DESC$ = NOFER$                                         ! Descrip Oferta
  LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"                    ! Formato Grabac
  WRITE FORM LEC$;#2;KEY$,FIL1$,DEPT$,                  \! Graba EAMITEMR
        FIL2$,PRIC$,LNK$,DESC$,FIL3$, GR.DATA$           !
  IF DESCTO% <> 0 THEN BEGIN                             !
     KEY$  = PACK$("999999") + OFNUM$ + PACK$("99")      ! Arma Llave DESCUENTO
     FIL1$ = PACK$("00020050")                           ! Indicat Miscel.Refu
     DEPT$ = PACK$("0000")                               ! Sin Dpto
     FIL2$ = "0000000000"                                ! Fam,Mult,Cant CERO
     PRIC$ = PACK$(RIGHT$(FIL2$ + STR$(DESCTO%),10))     ! Valor DESCUENTO
     FIL2$ = "0009950000"                                ! Fam,Mult,Cant CERO
     FIL2$ = PACK$(FIL2$)                                ! User Data
     LNK$  = PACK$("0000")                               ! Link para Desc.Misc
     DESC$ = "Desc/Espec."+LEOM$                         ! Descrip Oferta
     LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"                 ! Formato Grabac
     WRITE FORM LEC$;#2;KEY$,FIL1$,DEPT$,               \! Graba EAMITEMR
           FIL2$,PRIC$,LNK$,DESC$,FIL3$,GR.DATA$         !
  ENDIF                                                  !
  OFCOD$ = ""                                            ! Plus Oferta UPD
  OFVRV$ = PACK$(STRING$(240,"0"))                       ! Valores INT*4
  OFCNT$ = PACK$(STRING$(060,"0"))                       ! Valores INT*1
  FOR I% = 1 TO 30                                       ! Loop para TRASLADO
      IF REF%(I%) <> 0 THEN BEGIN                        ! de Datos de Oferta
         IND% = REF%(I%)                                 ! Guarda Posic Ocupada
         CO$(I%) = CO$(IND%)                             ! Traslada:
         VA%(I%) = VA%(IND%)                             !          Codigo
         CN%(I%) = CN%(IND%)                             !          Cantidad
         DP$(I%) = DP$(IND%)                             !          Depto
         DS$(I%) = DS$(IND%)                             !          Descripcion
         IF I% <> IND% THEN BEGIN                        ! Si hubo Cambio Posic
            CO$(IND%) = ZERO$                            ! Borra:
            VA%(IND%) = 0                                !          Codigo
            CN%(IND%) = 0                                !          Cantidad
            DP$(IND%) = ""                               !          Depto
            DS$(IND%) = ""                               !          Descripcion
         ENDIF                                           !
      ENDIF                                              !
  NEXT I%                                                !
  PRINT
  IND% = 0                                               !
  FOR J% = 1 TO 30                                       ! Loop guardar OFERTAS
      OFCOD$ = OFCOD$                          +        \! Guarda ALIAS de Ofer
               PACK$(RIGHT$("000000000000"     +        \!
               CO$(J%),12))                              !
      CALL PUTN4(OFVRV$,J%*4 - 4,VA%(J%))                ! Intercala Vrs INT*4
      CALL PUTN1(OFCNT$,J%*1 - 1,CN%(J%))                ! Intercala Vrs INT*1

      LIGA$ = RIGHT$("00" + STR$(J%+IND%),2)             ! Asigna No. Item Ofr
      KEY$  = PACK$("999999")+OFNUM$+PACK$(LIGA$)        ! Arma Llave ALIAS
      FIL1$ = PACK$("00000005")                          ! Indicat2 M/P=5
      DEPT$ = DP$(J%)                                    ! Dpto PLU Original
      FIL2$ = "000000000000"                             ! 
      PRIC$ = PACK$(RIGHT$(FIL2$ + CO$(J%),12))          ! A L I A S en Precio
      FOR I% = 1 TO CN%(J%)                              ! Loop para GRABAR
          LNK$=PACK$(RIGHT$("0000"+STR$(J%+IND%+1),4))   ! Siguiente LIGA
          IF CO$(J%) <> ZERO$ THEN BEGIN                 ! PLU's con CANTIDAD
             DESC$ = DS$(J%)                             ! Descrip PLU Oferta
             FIL2$ = "00000000"                          ! Fam,Mult,Cant CERO
             FIL2$ = PACK$(FIL2$)                        ! User Data
             IF I% = CN%(J%) AND                        \!
                CO$(J%+1) = ZERO$ THEN                  \!
                            LNK$ = PACK$("0000")         ! Si no HAY mas Ligas
             LEC$ = "C6 C4 C2 C4 C6 C2 C18 C4 C31"       ! Formato Grabac
             WRITE FORM LEC$;#2;KEY$,FIL1$,DEPT$,       \! Graba EAMITEMR
                   FIL2$,PRIC$,LNK$,DESC$,FIL2$,GR.DATA$ !
             IF CN%(J%) > 1 THEN BEGIN                   !
                IND%  = IND% + 1                         ! de UNO en UNO los
                LIGA$ = RIGHT$("00" + STR$(J%+IND%),2)   ! Asigna No. Item Ofr
                KEY$  = PACK$("999999") +               \!
                        OFNUM$ + PACK$(LIGA$)            ! Arma Llave ALIAS
             ENDIF                                       !
          ENDIF                                          !
     NEXT I%                                             !
     IF CN%(J%) > 1 THEN IND% = IND% - 1                 ! de UNO en UNO los
  NEXT J%                                                !

  KEY$ = OFNUM$                                          !
  OFVRC% = ACUVTA%                                       !
  LEC$ = "C2 C18 2C4 2I2 2I4 C180 C120 C30"              !
  WRITE FORM LEC$;#1;                                   \! Lee Reg Var/Fiscales
        OFNUM$,OFNOM$,OFDES$,OFHAS$,OFCAN%,             \! Llave,Nit,CF,Nom,Dir
        OFVEN%,OFVRO%,OFVRC%,OFCOD$,OFVRV$,OFCNT$        !
  CALL FINPANTA                                          !
  GOTO OFERTA                                            !
BORRAREG:                                                !
  CAM$ = DATO$("Desea BORRAR la Oferta/Mercado S/N ? ",22,15,"A",1)   
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO BORRAREG      !
  LOCATE 22,5  :  PRINT STRING$(70," ")                  !
  CALL FINPANTA                                          !
  LOCATE 23,1 : PRINT STRING$(78," ");                   !
  IF CAM$ = "N" THEN GOTO OFERTA                         !
  CAM$ = DATO$(".. Est  Seguro de BORRAR.. S/N ? ",22,5,"A",1)
  IF EN$ = "F3" THEN CALL FINAL                          ! 
  IF CAM$ <> "N" AND CAM$ <> "S" THEN GOTO BORRAREG      !
  LOCATE 22,5  :  PRINT STRING$(70," ")                  !
  IF CAM$ = "N" THEN GOTO BORRAREG                       !
!------------------------------------------------------------------------------
!             B O R R A D O  de Registros EAMITEMR Y CFOFERTA.DAT
!------------------------------------------------------------------------------
  DELREC 1 ; KEY$                                        ! BORRA Ofertas 
  KEY$  = PACK$("99999999") + OFNUM$                     ! Arma Llave OFERTA
  DELREC 2 ; KEY$                                        ! Borra Reg Master Of
  KEY$  = PACK$("999999") + OFNUM$ + PACK$("99")         ! Arma Llave DESCUENTO
  DELREC 2 ; KEY$                                        ! Borra Reg Desc Ofer
  FOR J% = 1 TO 30                                       !
      IF CO$(J%) <> ZERO$ THEN BEGIN                     !
         LIGA$ = RIGHT$("00" + STR$(J%),2)               ! Asigna No. Item Ofr
         KEY$  = PACK$("999999")+OFNUM$+PACK$(LIGA$)     ! Arma Llave ALIAS
         DELREC 2 ; KEY$                                 !
      ENDIF                                              !
  NEXT J%                                                !
  CALL FINPANTA                                          !
  GOTO OFERTA                                            !
ERRTRAP:                                                 !
   HX% = ERRN                                            !
   ERRFX$ =""                                            !
   FOR S% = 28 TO 0 STEP -4                              !
       SX% = SHIFT(HX%,S%)                               !
       SUM% = SX% AND 000FH                              !
   IF SUM% > 9 THEN                                     \!
        SUM% = SUM% + 55                                \!
   ELSE                                                 \!
        SUM% = SUM% + 48                                 !
        Z$ = CHR$(SUM%)                                  !
        ERRFX$ = ERRFX$ + Z$                             !
   NEXT S%                                               !
   ECOD$ = ERR                                           !
   IF ERR = "IH" THEN RESUME                             !
!------------------------------------------------------------------------------
!     LOCATE 22,60 : PRINT ERR; " ";ERRF%;" ";ERRFX$;    !
!     INPUT " ";AA$                                      !
!------------------------------------------------------------------------------
   IF ERRF% = 1 AND ERR = "EF" THEN RESUME               !
   IF ERRF% = 2 AND ERR = "EF" THEN BEGIN                !
      IF SWLEC% = 1 THEN BEGIN                           !
         MENS$ = "   ...NO Existe EAN/UPC de la Oferta/Mercado "+OFERT$  
         SWLEC% = 0                                      !
      ENDIF ELSE BEGIN                                   !
         MENS$ = " P L U ....NO Existe para ‚sta Oferta/Mercado..!" 
         PRECI% = 0 : DESC$ = "     *****        "       !
         CALL ACUMULA                                    !
      ENDIF                                              !
      CALL VERROR(MENS$)                                 !
      RESUME                                             !
   ENDIF                                                 !
   IF ERRF% = 1 AND                                     \!
     (ERR = "OE" OR ERR = "FU") THEN BEGIN               !
      CREATE POSFILE ARCH1$ KEYED 2,,,1000 RECL 370 AS 1 ! 
      RESUME                                             !
   ENDIF                                                 !
END                                                      !
!-----------------------------------------------------------------------
!              ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
!------------------------------------------------------------------------
