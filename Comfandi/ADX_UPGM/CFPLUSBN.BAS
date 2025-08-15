!    Programa   =   CFPLUSBN.BAS
!               **PROYECTO COMFANDI**
!-------------------------------------------------------
!  Este programa extrae datos del archivo de movimiento 
!  diario (EAMIMOVO)  y  los convierte  a formato ASCII
!  para posteriormente transmitirlos al AS/400 para  el 
!  proceso de actualizacion de inventarios.
!
!  El programa usa inicialmente la rutina de extraccion
!  de las llaves en  forma ascendente  y a continuacion 
!  se produce el archivo final (INVmmdd.DAT)  
!             mm = MES   ;   dd = DIA
!-------------------------------------------------------
%ENVIRON C                             ! Ambiente de controlador
!---------- Definicion de variables de usuario locales -----------
 REAL CANTR
 STRING                                                        \
          COD$,         KEY$,       NOM$,       REGI$,         \
          ERRFX$,       Z$,         LLAV1$,     KEY.ARRAY$,    \
          KEYS$,        ZERO.KEY$,  SECTOR$,    TEMP$,         \
          RESP$,        RES$,       INDICAT2$,  FEC$,          \
          ALM$,         INVEN$,     MESES(1)

  INTEGER*1                                                    \
          INDICAT0,     INDICAT1,   SW.FIN,     SWREP,         \
          INDICAT1A,    SWERROR,    MES

 INTEGER*2                                                     \
          END.OF.SCAN,  FLAG,       POINTER,    POINT1,        \
          POINT2,       I%,         UEX1,       UEX2

 INTEGER*4                                                     \
          VRREM%,       VRVEN%,     S%,         SX%,           \
          NUREM%,       ARVEN%,     REC.PNTR%,  FILE.THERE,    \
          HX%,          SUM%,       RESTART,    NITEM,         \
          VRSALE,       WEIGHT,     TOTBUE,     LEIDOS,        \
          VRVENTAS,     NREGIS,     AMTTRANS,   NUMTRANS,      \
          USEREXIT,     GRAVEN%,    VRGRA%,     VRNOES%,       \
          ARNOES%,      VALOR%

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
      MES      = VAL(FRA.MM$)                       ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"      ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"     ! Si es Siglo XXI
      FHOY$    = MESES(MES)+" "+STR$(VAL(FRA.DD$)) \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                 !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    de "+FHOY$                     ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                 ! Salva Mes y Dia 
  END SUB

!------------------------------------------------------------
!----            PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------
   DIM MESES(12)
   FECHA$=DATE$
   MESPRO=VAL(MID$(FECHA$,3,2))
   DIAPRO$=RIGHT$(FECHA$,2)
   ANOPRO$=LEFT$(FECHA$,2)
   FINR$ = CHR$(13)+CHR$(10)                         ! Caracter "0D0A"
!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------
TITULO$ = "         ** CONVERSION DE VENTAS DIARIAS (Icccmmdd.DAT) **"
ON ERROR GOTO ERRTRAP
CALL CREAFECHA
FEC1$ = FRA.LIN$
  
EMPEZAR:
OPEN "C:\ADXALMA.DAT" AS 27 BUFFSIZE 80              ! Archivo Control Almacen
READ FORM "C80";#27;AL$                              ! Lee Control Almac
RESP$   = MID$(AL$,9,4)                              ! Toma Fecha Control
CLEARS
PRINT                                                ! Avanza Linea
PRINT                                                ! Avanza Linea
PRINT "    Proceso para RECONSTRUIR las Ventas Diarias"   
PRINT "    de una FECHA DIFERENTE a la de Hoy..       "
PRINT "                                               "
PRINT "    NOTA : Este Proceso toma como Archivo de   "
PRINT "    entrada ADX_UDT1:EAMImmdd.DAT "
PRINT "                                               "
INPUT "    Indique el Mes y el D¡a (MMDD) " ; RESP$
FEC$    = RESP$                                      !
INVEN$  = RESP$                                      ! Guarda Mes y Dia
NUAL$   = STR$(VAL(LEFT$(AL$, 4)))                   ! Guarda Num Almacen
CONTRO$ = "4"+MID$(AL$,5,4)+LEFT$(RESP$,4)           ! Arma Reg Ctrl SIGED
NOAL$   = RIGHT$(AL$, LEN(AL$) - 12)                 ! Guarda Nombre
ALM$    = NUAL$                                      ! 
RESP$   = "0"+NUAL$                                  !
INVEN$  = ALM$ + INVEN$                              ! Adiciona No Almac
FILNAME1$ = "C:\ADX_UDT1\EAMI"                       ! Prefijo Archivo Entrada
FILNAME1$ = FILNAME1$ + FEC$ + ".DAT"                ! Sufijo Arch entrada  
IF FRA.AA$ > MID$(AL$,7,2) THEN BEGIN                ! Informa Cambio de A¤o
    CLEARS                                           !
    PRINT : PRINT : PRINT : PRINT : PRINT            !
    PRINT "        Registramos Complacidos un "      !
    PRINT                                            !
    PRINT "        **** N U E V O   A ¥ O ***"       !
    PRINT                                            !
    PRINT "   Las  CONVERSIONES  de las ventas diarias"
    PRINT "   a  partir  de  la Fecha, quedar n con el"
    PRINT "   NUEVO a¤o. Hoy se MODIFICA  este dato en"
    PRINT "   forma AUTOMATICA en C:\ADXALMA.DAT."   !
    PRINT                                            !
    PRINT "   Este es un mensaje INFORMATIVO..! "    ! 
    PRINT                                            !
    INPUT "   ..Feliz A¤o Nuevo..!    Una vez m s con.... INTRO.." ; CONTRO$
    PRINT
ENDIF
CLEARS
PRINT  :   PRINT  :  PRINT  :   PRINT
PRINT TITULO$
PRINT                                                ! Avanza Linea
PRINT                                                ! Avanza Linea
PRINT "       Proceso para el Almac‚n No. " ; ALM$;"  ";NOAL$
PRINT "       Inicia a las: ";FEC1$                  ! Indica comienzo
PRINT                                                ! Avanza Linea
PRINT "       Se est  procesando              : ";FILNAME1$! Archivo en Proceso
PRINT "       Se CREARA el Archivo para SIGED : C:\I";NUAL$;FEC$;".DAT"
PRINT                                                ! Avanza Linea
PRINT "                                        Por Favor Esperar..." 
FILNAME2$ = "C:\I" + INVEN$ + ".DAT"                 ! Prefijo Arch. Salida
FILNAME3$ = "C:\V" + RIGHT$(FILNAME2$,11)            ! Prefijo Variac. Prec
CREATE POSFILE FILNAME2$ AS 3 LOCAL                  ! Crea Mov. SIGED
CREATE POSFILE FILNAME3$ AS 4 LOCAL                  ! Crea Variac. Prec
OPEN "EAMITEMR" KEYED RECL 46 AS 25 NOWRITE NODEL    ! Abre Articulos
OPEN FILNAME1$ DIRECT RECL 22 AS 26 NODEL            ! Abre Ventas Dia
CONTRO$ = CONTRO$ + RESP$                            ! Arma Reg Contrl SIGED
WRITE FORM "C13 C16 C2";#3;                         \! Graba registro Dia
            CONTRO$,STRING$(16," "),FINR$            ! con espacios al final
WRITE FORM "C13 C47 C2";#4;                         \! Graba registro Dia
            CONTRO$,STRING$(47," "),FINR$            ! con espacios al final
NUM% = 0                                             ! Inicia Contador
IF END #26 THEN SALIR                                ! Rutina Error
LEER:
NUM% = NUM% + 1                                      ! Incr. Contador Lec
  READ FORM "C6 4I4"; #26,NUM%;KEY.ARRAY$,          \! Lectura del archivo
       NITEM,VRSALE,WEIGHT,RESTART                   ! directo EAMImmdd.DAT
  KEY$ = RIGHT$("0000"+UNPACK$(KEY.ARRAY$),7)        ! Arma la LLave
  IF KEY$ = "9999999" THEN BEGIN                     ! Lee Reg de control
     GOTO LEER                                       ! Y regresa a leer
  ENDIF                                              !
  IF VRSALE < 0 THEN NITEM = -1*NITEM                ! Si es Reintegro
! GOTO NOLEE                                         ! Bypass de Lec EAMITEMR
  READ FORM "C46"; #25 KEY KEY.ARRAY$;REGI$          ! Lectura de EAMITEMR
  NOM$ = MID$(REGI$,25,18)                           ! Nombre Depto
  DEP$ = RIGHT$("000"+UNPACK$(MID$(REGI$,11,2)),3)   ! Codigo Depto 
  VAL$ = RIGHT$("00000000"+                         \! Inserta 0's al
         UNPACK$(MID$(REGI$,18,5)),8)                ! Valor Unitario
  UNI$ = RIGHT$("00"+UNPACK$(MID$(REGI$,17,1)),2)    ! Contenido
  IF UNI$ = "00" THEN UNI$ = "01"                    ! Valor por Defecto=1
  ESTA$ = UNPACK$(MID$(REGI$,10,1))                  ! Cond Vta Normal
NOLEE:                                               !
!ITM$ = RIGHT$("000000"+STR$(NITEM),6)              ! Cantidad vendida

CANTR = NITEM*1000  
IF VAL(UNI$)  > 0 THEN BEGIN  
   CANTR = CANTR/VAL(UNI$)
   CANTR = INT(CANTR)
ENDIF
ITM$ = RIGHT$("000000000"+STR$(CANTR),9)             ! Cantidad vendida 
  VALOR% = VRSALE - NITEM * VAL(VAL$)                ! Dif Vr Precio POS
  SIG2$ = "+"                                        !
  IF VALOR% < 0 THEN BEGIN                           ! 
     VALOR% = -1 * VALOR%                            ! Si el Vr es "-"
     SIG2$ = "-"                                     ! Vuelve "+" el Vr
  ENDIF                                              ! y separa SIGNO
  VAR$ = RIGHT$("0000000"+STR$(VALOR%),7)            ! Vr a Precio POS
  ITM$ = ITM$ + "000"                                ! Cant *1000
  ARVEN%  = ARVEN%  + NITEM                          ! Artic Vendidos
  GRAVEN% = GRAVEN% + WEIGHT                         ! Gramos Vendidos
  WT$  = RIGHT$("0000000000"+STR$(WEIGHT),9)         ! Peso total vendido
  IF ESTA$ = "" THEN BEGIN                           ! Si NO HAY PLU
        ARNOES% = ARNOES% + NITEM                    ! Artic Vend sin PLU
        VRNOES% = VRNOES% + VRSALE                   ! Ventas sin PLU
  ENDIF                                              !
  IF ESTA$ = "00" THEN BEGIN                         ! Si es PLU de Vta Norm
     IF WEIGHT <> 0 THEN BEGIN                       ! Si es Item Peso
        VRGRA% = VRGRA% + VRSALE                     ! Acumulado de Ventas
        TIP$ = "1" : ITM$ = WT$                      ! coloca Indic
     ENDIF ELSE BEGIN                                ! de lo contrario
        VRVEN% = VRVEN% + VRSALE                     ! Acumulado de Ventas
        TIP$ = "0"                                   ! es Vta normal
     ENDIF                                           !
  ENDIF                                              !
  
  IF UNPACK$(MID$(REGI$,10,1)) = "20" THEN BEGIN     ! Si en un Reembolso
     VRREM% = VRREM% + VRSALE                        ! Suma Reembolsos
     NUREM% = NUREM% + NITEM                         ! Cant Reembolsos
     VRSALE = -1 * VRSALE                            ! Se cambia Signo a (-)
  ENDIF                                              !
  IF VRSALE < 0 THEN                                \! Si es Reintegro
     SIG$ = "-"                                     \! Coloca signo -
  ELSE                                              \! de lo Contrario
     SIG$ = "+"                                      ! Coloca +
  VRVENTAS = VRVENTAS + VRSALE                       ! Acumulado de Ventas
  IF VRSALE < 0 THEN VRSALE = -1*VRSALE              ! Si es Reintegro
  SL$  = RIGHT$("00000000000"+STR$(VRSALE),7)        ! Valor total vendido
  TOTBUE = TOTBUE + 1                                ! Contador correctos
IF VAL(VAL$) = 0 THEN VAL$ = "1"

IF VAL(UNI$) > 1 THEN BEGIN
 ITM$ = STR$(INT(VAL(SL$)*1000/VAL(VAL$)))
 ITM$ = RIGHT$("00000000"+ITM$,9)                     ! Cantidad vendida 
ENDIF 
  
  WRITE FORM "C1 C7 C4 C9 C1 C7 C2 C2";#3;             \! Graba registro SIGED
        "1",KEY$,FEC$,ITM$,SIG$,SL$,FINR$            ! Longitud = 31 Bytes
  IF VAL(VAL$) = 0 OR                               \! Si Vr Unid POS=0
     VAL(VAR$) = 0 THEN GOTO LEER                    ! ¢ Diferenc=0, By Pass
  WRITE FORM "C1 C7 C4 C9 C1 C7 C18 C3 C1 C7 2C2";  \! Graba registro Var.Pr
        #4;"1",KEY$,FEC$,ITM$,SIG$,SL$,NOM$,DEP$,   \! Longitud = 62 Bytes
            SIG2$,VAR$,UNI$,FINR$                    !
  GOTO LEER                                          ! Vuelve a leer
REPORTA:
CONSOLE
PRINT "                 ------------------------------------------"
PRINT "                |  El archivo EAMImmdd.DAT de la fecha NO  |"
PRINT "                |  se encuentra en el Directorio ADX_UDT1: |"
PRINT "                |  Si  desea convertir las ventas  de  un  |"
PRINT "                |  dia distinto, REINSTALE el Archivo que  |"
PRINT "                |  corresponda a la Fecha Digitada, luego  |"
PRINT "                |  REINICIE..!                             |"
PRINT "                |               Digite INTRO por Favor..!  |"
INPUT "                 ------------------------------------------";NOM$
STOP

SALIR:
FOR$   = "######,###,###"
FORMA$ = "$$######,###,###"
LPRINTER  
  PRINT TITULO$;CHR$(10)
  PRINT " "
  PRINT "         REEMBOLSOS/DEVOLUC/CH.SUB : ";
  PRINT USING FOR$;NUREM%;
  PRINT USING FORMA$;VRREM%
  PRINT " "
  PRINT "ARTICULOS VENDIDOS POR PESO EN GR. : ";
  PRINT USING FOR$;GRAVEN%;
  PRINT USING FORMA$;VRGRA%
  PRINT " "
  PRINT "ARTICULOS VENDIDOS POR UNIDADES    : ";
  PRINT USING FOR$;ARVEN%;
  PRINT USING FORMA$;VRVEN%
  PRINT " "
  PRINT "ARTICULOS VENDIDOS SIN PLU         : ";
  PRINT USING FOR$;ARNOES%;
  PRINT USING FORMA$;VRNOES%
  PRINT " "
  PRINT "        VALOR REGISTROS GRABADOS   : ";
  PRINT USING FOR$;TOTBUE;
  PRINT USING FORMA$;VRVENTAS
  PRINT " "
  CALL CREAFECHA
  FEC2$ = FRA.LIN$
  PRINT " "
  PRINT "      ----------------------------------------------------------- "
  PRINT "      ** INICIA  A LAS: ";FEC1$;" **ARCHIVO I";INVEN$;".DAT"
  PRINT "      ** TERMINA A LAS: ";FEC2$
  PRINT "       Proceso para ";"*** Almac‚n No. ";ALM$;"  ";NOAL$;" ***"
  PRINT "      ----------------------------------------------------------- "
  PRINT " "
  PRINT " "
  PRINT " "
  WRITE FORM "C29 C2";#3;                          \! Reg Final SIGED
              STRING$(29,"9"),FINR$                 ! Longitud = 31 Bytes
  WRITE FORM "C60 C2";#4;                          \! Reg Final Var.Pr
              STRING$(60,"9"),FINR$                 ! Longitud = 62 Bytes
  CLOSE 3                                           ! Cierre Icccmmdd.DAT
  CLOSE 4                                           ! Cierre Vcccmmdd.DAT
  CLOSE 25                                          ! Cierre EAMImmdd.DAT
  CLOSE 26                                          ! Cierre EAMITEMR.DAT
  CLOSE 27                                          ! Cierre ADXALMA.DAT
STOP                                                ! PARAR
ERRTRAP:
IF ERR = "OE" AND ERRF% = 25 THEN RESUME REPORTA
IF ERR = "EF" AND ERRF% = 25 THEN                                 \
   PRINT "    NO EXISTE PLU :  ";KEY$            :                \
   RESUME 
IF ERRF% = 27 THEN BEGIN
   CLEARS
   PRINT
   PRINT
   PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
   PRINT
   PRINT "               Consulte con el Depto de Sistemas."
   PRINT "                 Crear  C:\ADXALMA.DAT"
   STOP
ENDIF
IF ERRF% <> 25 THEN BEGIN
   CONSOLE
   PRINT "                 ------------------------------------------"
   PRINT "                |  No  olvide  que para este proceso debe  |"
   PRINT "                |  haber CONVERTIDO la copia de BACKUP del |"
   PRINT "                |  archivo EAMIMOVO.DAT al archivo DIRECTO |"
   PRINT "                |  EAMImmdd.DAT.          REINICIE..!      |"
   PRINT "                |               Digite INTRO por Favor..!  |"
   INPUT "                 ------------------------------------------";NOM$
   LPRINTER
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
   PRINT "ERROR..: ";ERRL;"  ";ERR;"  ";ERRF%;"  ";ERRN;"  ";ERRFX$
   CONSOLE
   RESUME SALIR
ENDIF
END
! ********************************************
!***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ********************************************
