!           Programa   =   CFPLUSVN.BAS
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
! Mod 12Nov2009
! Se modifica el programa para que reporte en el archivo
! TF:TRXSND la interface a ser enviada de manera automatica
! al servidor AS/400 
! Desarrollado por Grupo Retail - OVS 
!---------------------------------------------------------
! Mod 15Abr2010
! Se modifica el almacenamiento del valor del impoconsumo,
! la fecha de actualizacion del archivo y la estructura 
! comercial del producto en la expansion de la maestra de 
! articulos, desarrollado por Grupo Retail - OVS 
!---------------------------------------------------------

   %ENVIRON C                                                   ! Amb Contr.
!
!---------- Definicion de variables de usuario locales -----------
 REAL     CANTR,        ITM,        IVA                         !
 REAL     TAR1,         TAR2,       TAR3,       TAR4            !

 STRING                                                        \!
          COD$,         KEY$,       NOM$,       REGI$,         \!
          ERRFX$,       Z$,         LLAV1$,     KEY.ARRAY$,    \!
          KEYS$,        ZERO.KEY$,  SECTOR$,    TEMP$,         \!
          RESP$,        RES$,       INDICAT2$,  FEC$,          \!
          ALM$,         INVEN$,     MESES(1),   DEPS$(1),      \!
          LE$,          SLS$,       PULG$,      PULGA$,        \!
          PULGA0$,      PULGA1$,    SndFile$,   Finr$  
 INTEGER*1                                                     \!
          IND0%,        IND1%,      SW.FIN,     SWREP,         \!
          IND1A%,       SWERROR,    MES                         !

 INTEGER*2                                                     \!
          END.OF.SCAN,  FLAG,       POINTER,    POINT1,  SI%,  \!
          POINT2,       I%,         UEX1,       UEX2,    REG%   !

 INTEGER*4                                                     \!
          IPOCO%,       FECHA%,     UNITA%,     BASEIVA%,      \!
          VALI%,                                               \!
          VRREM%,       VRVEN%,     S%,         SX%,           \!
          NUREM%,       ARVEN%,     REC.PNTR%,  FILE.THERE,    \!
          HX%,          SUM%,       RESTART,    NITEM,         \!
          VECES%,                                              \!
          VRSALE,       WEIGHT,     TOTBUE,     LEIDOS,        \!
          VRVENTAS,     NREGIS,     AMTTRANS,   NUMTRANS,      \!
          USEREXIT,     GRAVEN%,    VRGRA%,     VRNOES%,       \!
          VIPO%(1),     VIVA%(1),                              \!
          ARNOES%,      VALOR%,     VDEP%(1),   CDEP%(1),      \!
          VCONCES%,     NCONCES%,   CONLI%,     II%, Gr.Con%

String  GLOBAL GR.DATA$


 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function

          
! -------------  Rutina para crear compaginar Hora y Fecha --------------

  SUB CREAFECHA
      FRA.TIM$ = TIME$                              ! Toma Hr Sist
      FRA.H$   = LEFT$(FRA.TIM$,2)                  ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                 ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                 ! Arma segundos
      FRA.FEC$ = DATE$                              ! Toma Fec Sist
      FRA.AA$  = LEFT$(FRA.FEC$,2)                  ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                 ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                 ! Arma dia
      MES      = VAL(FRA.MM$)                       ! Toma Mes
      IF FRA.AA$ > "94" THEN SIGLO$ = " de 19"      ! Si es a£n Siglo XX
      IF FRA.AA$ < "95" THEN SIGLO$ = " del 20"     ! Si es Siglo XXI
      FHOY$    = MESES(MES)+" "+STR$(VAL(FRA.DD$)) \! Arma Fecha Hoy
                 + SIGLO$ + FRA.AA$                 !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    de "+FHOY$                      ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                 ! Salva Mes y Dia 
  END SUB                                           !

!------  Subrutina para llenar tabla de Ventas por Dpto ------

SUB ACUDEP                                          ! Rutina Acumul/Dpto
  NOM$ = "  "                                       ! Reset Nombre
  LLAVE$ = PACK$("000099999" + DEP$)                ! Arma KEY Plu Dptal
  READ FORM "C77"; #25 KEY LLAVE$;REGI$             ! Lectura de EAMITEMR
  IF NOM$ = "**" THEN BEGIN                         ! Si no leyo Depto
     NOM$ = "*DPTO SIN DEFINIR*"                    ! No esta en Store Opts?
  ENDIF ELSE BEGIN                                  !
     NOM$ = MID$(REGI$,25,18)                       ! Nombre Depto
  ENDIF                                             !
  DEPS$(VAL(DEP$)) = NOM$                           ! Guarda Nombre del PLU
  IF SIG$ = "-" THEN BEGIN                          ! Si es Vta Negativa
     VDEP%(VAL(DEP$)) = VDEP%(VAL(DEP$))-VAL(SL$)   ! Acumula Restando
     CDEP%(VAL(DEP$)) = CDEP%(VAL(DEP$))-NITEM      ! Acumula Restando
     VIPO%(VAL(DEP$)) = VIPO%(VAL(DEP$))-IPOCO%     ! Acumula ImpoConsumo
     VIVA%(VAL(DEP$)) = VIVA%(VAL(DEP$))-VALI%      ! Acumula IVAS
  ENDIF                                             !
  IF SIG$ = "+" THEN BEGIN                          ! Si es Vta Positiva
     VDEP%(VAL(DEP$)) = VDEP%(VAL(DEP$))+VAL(SL$)   ! Acumula la Venta
     CDEP%(VAL(DEP$)) = CDEP%(VAL(DEP$))+NITEM      ! Acumula la Cant
     VIPO%(VAL(DEP$)) = VIPO%(VAL(DEP$))+IPOCO%     ! Acumula ImpoConsumo
     VIVA%(VAL(DEP$)) = VIVA%(VAL(DEP$))+VALI%      ! Acumula IVAS
  ENDIF                                             ! 
END SUB

!------------------------------------------------------------
!----            PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------
   DIM MESES(12)                                    ! Inicio de arreglos
   DIM DEPS$(999)                                   ! para meses, Deptos,
   DIM VDEP%(999)                                   ! Valores y 
   DIM CDEP%(999)                                   ! Cantidades
   DIM VIPO%(999)                                   ! ImpoConsumos
   DIM VIVA%(999)                                   ! Ivas Unitarios
   FECHA$=DATE$                                     ! Guarda fecha
   OPCION$ = COMMAND$                               !
   MESPRO=VAL(MID$(FECHA$,3,2))                     ! Mes de Proceso
   DIAPRO$=RIGHT$(FECHA$,2)                         ! Dia
   ANOPRO$=LEFT$(FECHA$,2)                          ! Ano
   FINR$ = CHR$(13)+CHR$(10)                        ! Caracter "0D0A"
   DEPS$(01) = "DESCUENTO CH/SUBS"                  ! Guarda Nombre del PLU
   DEPS$(27) = "BOTIQUIN"                           ! Guarda Nombre del PLU
   DEPS$(28) = "BOTIQUIN CON IVA"                   ! Guarda Nombre del PLU
   PULG$     = "000"
   PULGA$    = "TOTAL VENTAS DPTO"
   PULGA1$   = "000000000"
   PULGA0$   = " "  


!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------
TITULO$ = "         ** CONVERSION DE VENTAS DIARIAS (Icccmmdd.DAT) **"
ON ERROR GOTO ERRTRAP                               !
CALL CREAFECHA                                      !
FEC1$ = FRA.LIN$                                    !
CONLI% = 60                                         !  
EMPEZAR:                                            !
OPEN "C:\ADXALMA.DAT" AS 27 BUFFSIZE 80             ! Archivo Control Almacen
READ FORM "C80";#27;AL$                             ! Lee Control Almac
RESP$   = MID$(AL$,9,4)                             ! Toma Fecha Control
FEC$    = RESP$                                     !
INVEN$  = RESP$                                     ! Guarda Mes y Dia
NUAL$   = STR$(VAL(LEFT$(AL$, 4)))                  ! Guarda Num Almacen
CONTRO$ = "4000"+MID$(AL$,5,4)+LEFT$(RESP$,4)       ! Arma Reg Ctrl SIGED
NOAL$   = RIGHT$(AL$, LEN(AL$) - 12)                ! Guarda Nombre
IF LEN(NOAL$) > 25 THEN BEGIN                       ! Si mide + de 25
   NOAL$ = LEFT$(NOAL$ , 25)                        ! Se recorta a 25
ENDIF                                               !
ALM$    = NUAL$                                     ! 
RESP$   = "0"+NUAL$                                 !
RESP1$  = MID$(AL$,5,8) + RESP$                     !
INVEN$  = ALM$ + INVEN$                             ! Adiciona No Almac
FILNAME1$ = "C:\ADX_UDT1\EAMI"                      ! Prefijo Archivo Entrada
FILNAME1$ = FILNAME1$ + FEC$ + ".DAT"               ! Sufijo Arch entrada  
FILNAME2$ = "C:\I" + INVEN$ + ".DAT"                ! Prefijo Arch. Salida
FILNAME3$ = "C:\V" + RIGHT$(FILNAME2$,11)           ! Prefijo Variac. Prec
FILNAME4$ = "C:\C" + NUAL$ + FEC$ + ".DAT"          ! Prefijo Archivo Entrada
FILNAME5$ = "C:\NOPLU.DAT"                          ! Prefijo Archivo Entrada
IF FRA.AA$ > MID$(AL$,7,2) THEN BEGIN               ! Informa Cambio de A¤o
    CLEARS                                          !
    PRINT : PRINT : PRINT : PRINT : PRINT           !
    PRINT "        Registramos Complacidos un "     !
    PRINT                                           !
    PRINT "        **** N U E V O   A ¥ O ***"      !
    PRINT                                           !
    PRINT "   Las  CONVERSIONES  de las ventas diarias"
    PRINT "   a  partir  de  la Fecha, quedar n con el"
    PRINT "   NUEVO a¤o. Hoy se MODIFICA  este dato en"
    PRINT "   forma AUTOMATICA en C:\ADXALMA.DAT."  !
    PRINT                                           !
    PRINT "   Este es un mensaje INFORMATIVO..! "   ! 
    PRINT                                           !
    INPUT "   ..Feliz A¤o Nuevo..!    Una vez m s con.... INTRO.." ; CONTRO$
    PRINT
ENDIF
WHILE TIP$ <> "9"                                   !
      READ FORM "C80";#27;AL$                       ! Lee Control Almac
      TIP$ = MID$(AL$,78,1)                         !
      IF TIP$ = "5" THEN BEGIN                      !
         TAR1 = 1+FLOAT(VAL(MID$(AL$,01,2)))/100    ! Toma Tarifa Iva1
         TAR2 = 1+FLOAT(VAL(MID$(AL$,04,2)))/100    ! Toma Tarifa Iva2
         TAR3 = 1+FLOAT(VAL(MID$(AL$,07,2)))/100    ! Toma Tarifa Iva3
         TAR4 = 1+FLOAT(VAL(MID$(AL$,10,2)))/100    ! Toma Tarifa Iva4
         TAR5 = 1+FLOAT(VAL(MID$(AL$,13,2)))/100    ! Toma Tarifa Iva5
         TAR6 = 1+FLOAT(VAL(MID$(AL$,16,2)))/100    ! Toma Tarifa Iva6
         TIP$ = "9"                                 !
      ENDIF                                         !
WEND
CLOSE 27                                            ! Cierre ADXALMA.DAT
CLEARS
PRINT  :   PRINT  :  PRINT  :   PRINT
PRINT TITULO$
PRINT                                                ! Avanza Linea
PRINT                                                ! Avanza Linea
PRINT "       Proceso para el Almac‚n No. " ; ALM$;"  ";NOAL$
PRINT "       Inicia a las: ";FEC1$                  ! Indica comienzo
PRINT                                                ! Avanza Linea
PRINT "   Se est  procesando                  : ";FILNAME1$ 
PRINT "   Se CREARA el Archivo de CUADRE      : ";FILNAME4$
PRINT "   Se CREARA el Archivo para SIGED     : ";FILNAME2$
PRINT "   Se CREARA el Archivo de VARIACIONES : ";FILNAME3$
PRINT                                                ! Avanza Linea
PRINT "                    Por Favor Esperar..."     !
LOCATE 6,9  :   PRINT "Factores de IVA: ";           !
PRINT USING "###.##";TAR1,TAR2,TAR3,TAR4,TAR5,TAR6   !
CREATE POSFILE FILNAME2$ AS 3 LOCAL                  ! Crea Mov. SIGED
CREATE POSFILE FILNAME3$ AS 4 LOCAL                  ! Crea Variac. Prec
CREATE POSFILE FILNAME5$ AS 54 LOCAL                 ! Crea Arch Errores
CREATE FILNAME4$ DIRECT RECL 78 AS 10 BUFFSIZE 1780  !
WRITE FORM "C2 C2";#54;CHR$(12),FINR$                !
FOR II% = 1 TO 24                                    ! Inicia Loop 
    WRITE FORM "C76 C2";#10,II%;" ",FINR$            ! para Crear Reg
NEXT II%                                             ! en vac¡o 
IF OPCION$ = "1" THEN BEGIN                          !
   OPEN "C:\FER\EAMITEMR.DAT" KEYED                 \!
            RECL 77 AS 25 NOWRITE NODEL              ! Abre Articulos
ENDIF ELSE BEGIN                                     !
    OPEN "EAMITEMR" KEYED RECL 77 AS 25 NOWRITE NODEL! Abre Articulos
ENDIF                                                !
OPEN FILNAME1$ DIRECT RECL 22 AS 26 BUFFSIZE 5600 NODEL ! Abre Ventas Dia
CONTRO$ = CONTRO$ + "000000" + RESP$                 ! Arma Reg Contrl SIGED
WRITE FORM "C22 C21 C2";#3;                         \! Graba registro Dia
            CONTRO$," ",FINR$                        ! con espacios al final
WRITE FORM "C22 C45 C2";#4;                         \! Graba registro Dia
            CONTRO$," ",FINR$                        ! con espacios al final
NUM% = 0                                             ! Inicia Contador
IF END #26 THEN SALIR                                ! Rutina Error
!DESMIS$ = PACK$("999999")
LEER:
NUM% = NUM% + 1                                      ! Incr. Contador Lec
  READ FORM "C6 4I4"; #26,NUM%;KEY.ARRAY$,          \! Lectura del archivo
       NITEM,VRSALE,WEIGHT,RESTART                   ! directo EAMImmdd.DAT
  KEY$ = RIGHT$("0000"+UNPACK$(KEY.ARRAY$),7)        ! Arma la LLave
  LOCATE 20,15 : PRINT "Reg. Le¡dos: ";NUM%
  KEI$ = UNPACK$(KEY.ARRAY$)                         ! Arma la LLave
  IF LEFT$(KEI$,6) = "999999" AND                   \!
     RIGHT$(KEI$,2)= "99" THEN BEGIN                 !
        KEY.ARRAY$ = PACK$("88888888"+MID$(KEI$,7,4))!
        VRSALE = 0                                   !
!   LOCATE 1,1 : PRINT "Oferta:"; UNPACK$(KEY.ARRAY$); : INPUT "  ";ZZ$
  ENDIF  
  
!  IF VAL(KEY$) = 7 THEN PRINT VRSALE  
  IF KEY$ = "9999999" THEN BEGIN                     ! Lee Reg de control
     GOTO LEER                                       ! Y regresa a leer
  ENDIF                                              ! Fin de Condic
  !IF DESMIS$ = LEFT$(KEY.ARRAY$,3) THEN BEGIN        ! Si hay prefijo Conces
   !  GOTO LEER                                       ! Y regresa a leer
  !ENDIF                                              !
  IF LEFT$(KEY$,4) = "0888" THEN BEGIN               ! Si hay prefijo Conces
     DEPS$(998) = "TOTAL CONCESIONES  "              ! 
     CDEP%(998) = CDEP%(998) + 1                     ! Acumula Restando
     VDEP%(998) = VDEP%(998) + VRSALE                ! Acumula Restando
  ENDIF                                              !
IF VRSALE < 0 THEN NITEM = -1*NITEM                  ! Si es Reintegro
VECES% = NITEM
KEY$ = KEY.ARRAY$                                    ! Restaura Llave PD
ERROR$ = "  "                                        ! Indicativo Lec OK
READ FORM "C6 3I1 C1 C2 C4 C1 C5 C2 C18 2I2 C31";#25    \! Lectura EAMITEMR
     KEY KEY$;KEY$,IND0%,IND1%,IND1A%,IND2$,DEP$,   \! segun formato
     FAMGR$,CANT$,VAL$,LINK$,DES$,UEX1,UEX2,GR.DATA$ !

Gr.Con% = GETN4(ASC.IR.DATA$,16)                     ! Valor del impoconsumo
      
IVA = 1                                              !
IF IND1% AND 80H THEN IVA = TAR1                     !
IF IND1% AND 40H THEN IVA = TAR2                     !
IF IND1% AND 20H THEN IVA = TAR3                     !
IF IND1% AND 10H THEN IVA = TAR4                     !
!IPOCO% = VECES% * UEX1                              !

IPOCO% = VECES% * Gr.Con%                            ! Valor del Impoconsumo

IPOC$ = RIGHT$("0000000"+STR$(IPOCO%),7)             !
IF ERROR$ <> "  " THEN BEGIN                         ! Si NO existe Item
   KEY$ = RIGHT$("0000"+UNPACK$(KEY.ARRAY$),7)       ! Arma la LLave
   DES$ = STRING$(18, " ")                           ! Reset de Descrip
   DEP$ = "999"                                      ! Reset Depto 
   VALI$ = "00000000000"                             ! Reset Valor
   UNI$ = "01"                                       ! Unidad = 1
ENDIF ELSE BEGIN                                     ! de lo Contrario
   NOM$ = DES$                                       ! Nombre Depto
   KEY$  = RIGHT$(UNPACK$(KEY$),12)                  ! Arma Llave Extendida
   DEP$ = UNPACK$(DEP$)                              ! Desempaq Cod Depto 
   DEP$ = RIGHT$("000"+ DEP$,3)                      ! Codigo Depto 
   CANTI$ = "00"  : VALI$ = "00000000"               ! Reset de Cant/Vr
   IF RIGHT$(UNPACK$(IND2$),1) = "5" THEN BEGIN      ! Metodo Precio 5
      KEY$  = CANT$ + VAL$                           ! Arma Llave del Alias
      KEY$  = RIGHT$(UNPACK$(KEY$),12)               ! Arma Llave Extendida
   ENDIF ELSE BEGIN                                  !   
      CANTI$=RIGHT$("00"+UNPACK$(CANT$),2)           ! Obtiene Cantidad
      VALI$ =RIGHT$("0000000000" + UNPACK$(VAL$),8)  ! Obtiene Valor
   ENDIF                                             !
ENDIF                                                !
VAL$ = VALI$                                         ! Valor Unitario
UNI$ = CANTI$                                        ! Contenido
IF UNI$ = "00" THEN UNI$ = "01"                      ! Valor por Defecto=1
! ESTA$ = LEFT$(UNPACK$(IND2$),1)                      ! Cond Vta Normal
ESTA$ = UNPACK$(MID$(REGI$,10,1))                    ! Cond Vta Normal
NOLEE:                                               !
!ITM$ = RIGHT$("000000"+STR$(NITEM),6)                ! Cantidad vendida 


CANTR = NITEM*1000  
IF CANTI$ = "0" THEN CANTI$ = "1"
IF VAL(CANTI$)  > 0 THEN BEGIN  
   MCANTR = CANTR/VAL(CANTI$)
   CANTR = INT(CANTR)
ENDIF

ITM$ = RIGHT$("000000000"+STR$(CANTR),9)             ! Cantidad vendida 

!IF VAL(CANTI$) > 1 THEN BEGIN 
 ! ITM  = VAL(ITM$)/VAL(CANTI$)
  !ITM$ = RIGHT$("000000000"+STR$(ITM),9)                ! Cantidad vendida 
!ENDIF
VALOR% = VRSALE - NITEM * VAL(VAL$)                  ! Dif Vr Precio POS
SIG2$ = "+"                                          !
IF VALOR% < 0 THEN BEGIN                             ! 
   VALOR% = -1 * VALOR%                              ! Si el Vr es "-"
   SIG2$ = "-"                                       ! Vuelve "+" el Vr
ENDIF                                                ! y separa SIGNO
VAR$ = RIGHT$("0000000"+STR$(VALOR%),7)              ! Vr a Precio POS
!ITM$ = ITM$ + "000"                                  ! Cant *1000
ARVEN%  = ARVEN%  + NITEM                            ! Artic Vendidos
GRAVEN% = GRAVEN% + WEIGHT                           ! Gramos Vendidos
WT$  = RIGHT$("0000000000"+STR$(WEIGHT),9)           ! Peso total vendido
!IF ESTA$ = "" THEN BEGIN                             ! Si NO HAY PLU
IF UNPACK$(IND2$) = "" THEN BEGIN                    ! Si es PLU de Vta Norm
      ARNOES% = ARNOES% + NITEM                      ! Artic Vend sin PLU
      VRNOES% = VRNOES% + VRSALE                     ! Ventas sin PLU
ENDIF                                                !
IF ESTA$ = "00" THEN BEGIN                           ! Si es PLU de Vta Norm
   IF WEIGHT <> 0 THEN BEGIN                         ! Si es Item Peso
      VRGRA% = VRGRA% + VRSALE                       ! Acumulado de Ventas
      TIP$ = "1" : ITM$ = WT$                        ! coloca Indic
   ENDIF ELSE BEGIN                                  ! de lo contrario
      VRVEN% = VRVEN% + VRSALE                       ! Acumulado de Ventas
      TIP$ = "0"                                     ! es Vta normal
   ENDIF                                             !
ENDIF                                                !
IF UNPACK$(IND2$) = "20" THEN BEGIN                  ! Si en un Reembolso
   VRREM% = VRREM% + VRSALE                          ! Suma Reembolsos
   NUREM% = NUREM% + NITEM                           ! Cant Reembolsos
   VRSALE = -1 * VRSALE                              ! Se cambia Signo a (-)
ENDIF                                                !
IF VRSALE < 0 THEN                                  \! Si es Reintegro
   SIG$ = "-"                                       \! Coloca signo -
ELSE                                                \! de lo Contrario
   SIG$ = "+"                                        ! Coloca +
IF DEP$ <> "048" THEN VRVENTAS = VRVENTAS + VRSALE   ! Acumulado de Ventas
IF VRSALE < 0 THEN VRSALE = -1*VRSALE                ! Si es Reintegro
SL$  = RIGHT$("00000000000"+STR$(VRSALE),9)          ! Valor total vendido
SLS$ = RIGHT$("00000000000"+STR$(VRVENTAS),9)          ! Valor total vendido
!PRINT "TOTAL";SLS$
TOTBUE = TOTBUE + 1                                  ! Contador correctos
KEY$  = RIGHT$(KEY$,7)                               ! Arma Llave Extendida
IF VAL(VAL$) = 0 THEN VAL$ = "1"

IF VAL(CANTI$) > 1 THEN BEGIN
 ITM$ = STR$(INT(VAL(SL$)*1000/VAL(VAL$)))
 ITM$ = RIGHT$("00000000"+ITM$,9)                     ! Cantidad vendida 
ENDIF

!   PRINT KEY$
!------------------------------------------------------------------------------
!      Febrero 1/2002  Calculo de Base Imponible e Iva (Transaction Mode)
!------------------------------------------------------------------------------
ITM      = VAL(ITM$) / 1000                          ! Cant Unds Vendidas
IF ITM = 0  THEN ITM=1                               !
UNITA%   = (VAL(SL$) - VAL(IPOC$)) / ITM             ! Calc Vr/Unit sin ImpoC
BASEIVA% = UNITA% / IVA                              ! Deflacta Iva
! LOCATE 18,60 : PRINT "Base Uni ";BASEIVA%          
BASEIVA% = BASEIVA% * ITM                            ! Calcula Vr/Bases
! LOCATE 19,60 : PRINT "Base Tot ";BASEIVA%          
VALI%    = UNITA% * ITM - BASEIVA%                   ! Calcula IVA(Remanente)
VALI$    = RIGHT$("0000000" + STR$(ABS(VALI%)), 7)   ! Calcula IVA(Remanente)
! LOCATE 22,1
! PRINT IVA,ITM,UNITA%,BASEIVA%,VALI%;"             ";
! INPUT "  ";AAA$ 
WRITE FORM "C1 C7 C4 C9 C1 C9 2C7 C2";#3;             \! Graba registro SIGED
      "1",KEY$,FEC$,ITM$,SIG$,SL$,IPOC$,VALI$,FINR$  ! VALI$,FINR$  ! Longitud = 33 Bytes
!------------------------------------------------------------------------------
IF VAL(VAL$) = 0 OR                                 \! Si Vr Unid POS=0
   VAL(VAR$) = 0 THEN GOTO SALE                      ! ¢ Diferenc=0, By Pass
WRITE FORM "C1 C7 C4 C9 C1 C9 C18 C3 C1 C7 2C2";    \! Graba registro Var.Pr
      #4;"1",KEY$,FEC$,ITM$,SIG$,SL$,NOM$,DEP$,     \! Longitud = 64 Bytes
          SIG2$,VAR$,UNI$,FINR$                      !
SALE:                                                !
  CALL ACUDEP                                        !
  GOTO LEER                                          ! Vuelve a leer
REPORTA:
CONSOLE
PRINT "                 ------------------------------------------"
PRINT "                |  El archivo EAMITEMR.DAT que v  a USAR   |"
PRINT "                |  NO  se  encuentra  en  el  Directorio   |"
PRINT "                |  ADX_IDT1:                               |"
PRINT "                |                                          |"
PRINT "                |  Si esta usando la OPCION de ejecucion,  |"
PRINT "                |  verifique su existencia en Otro DIR.    |"
PRINT "                |               Digite INTRO por Favor..!  |"
INPUT "                 ------------------------------------------";NOM$
STOP
SALIR:                                               !
FOR$   = "######,###,###"                            !
FORMA$ = "$$######,###,###"                          !
LPRINTER                                             !
  PRINT TITULO$;CHR$(10)                             !
  PRINT " "                                          !
  PRINT "         REEMBOLSOS/DEVOLUC/CH.SUB : ";     !
  PRINT USING FOR$;NUREM%;                           !
  PRINT USING FORMA$;VRREM%                          !
  PRINT " "                                          !
  PRINT "ARTICULOS VENDIDOS POR PESO EN GR. : ";     !
  PRINT USING FOR$;GRAVEN%;                          !
  PRINT USING FORMA$;VRGRA%                          !
  PRINT " "                                          !
  PRINT "ARTICULOS VENDIDOS POR UNIDADES    : ";     !
  PRINT USING FOR$;ARVEN%;                           !
  PRINT USING FORMA$;VRVEN%                          !
  PRINT " "                                          !
  PRINT "ARTICULOS VENDIDOS SIN PLU         : ";     !
  PRINT USING FOR$;ARNOES%;                          !
  PRINT USING FORMA$;VRNOES%                         !
  PRINT " "                                          !
  PRINT "        VALOR REGISTROS GRABADOS   : ";     !
  PRINT USING FOR$;TOTBUE;                           !
  PRINT USING FORMA$;VRVENTAS                        !
  PRINT " "                                          !
  CALL CREAFECHA                                     !
  FEC2$ = FRA.LIN$                                   !
  PRINT " "                                          !
  PRINT "      ----------------------------------------------------------- "
  PRINT "      ** INICIA  A LAS: ";FEC1$;" **ARCHIVO I";INVEN$;".DAT"
  PRINT "      ** TERMINA A LAS: ";FEC2$
  PRINT "       Proceso para ";"*** Almac‚n No. ";ALM$;"  ";NOAL$;" ***"
  PRINT "      ----------------------------------------------------------- "
  PRINT " "                                          !
  PRINT " "                                          !
  PRINT " "                                          !
  WRITE FORM "C43 C2";#3;                           \! Reg Final SIGED
              STRING$(43,"9"),FINR$                  ! Longitud = 45 Bytes
  FER$ = RIGHT$("000000000"+STR$(TOTBUE),9)          ! Cantidad vendida 
  JOR$ = RIGHT$("TOTAL #REG GRABADOS ",20)           ! Cantidad vendida 
  WRITE FORM "C20 C9 C14 C2";#3;                    \! Reg Final SIGED
              JOR$,FER$,"00000000000000",FINR$       ! Longitud = 45 Bytes
  WRITE FORM "C60 C2";#4;                           \! Reg Final Var.Pr
              STRING$(60,"9"),FINR$                  ! Longitud = 62 Bytes

CONSOLE
  LOCATE 22,20 : PRINT JOR$,FER$
  REG% = 13  :  LE$ = "C"                            ! Reg Inic y Letra
  FOR I% = 1 TO 999                                  ! Loop para Grabar
    IF I% = 2 THEN REG% = 24                         ! Si es reg 13(COMIS)
    IF I% > 201 THEN LE$ = "F"                       ! Si es Conces camb Let
    II$ = RIGHT$("000" + STR$(I%) , 3)               ! Toma Depto
     IF VDEP%(I%) <> 0 OR                           \! Verif si hubo Vta
       I% = 01                       THEN BEGIN      ! o son BOTIQUINES o SUBS
       IF VAL(II$) <> 48 THEN BEGIN                  ! si no es redondeo 
       WRITE FORM "C12 C4 C20 4PIC(-999999999) C2"; \! Graba segun Fmto
             #10,REG%;RESP1$,LE$+II$,DEPS$(I%),     \! arch directo
       CDEP%(I%),VDEP%(I%),VIPO%(I%),VIVA%(I%),FINR$ ! con datos de Vtas
       REG% = REG% + 1                               ! Incr Cont Reg
     ENDIF                                           ! Fin de Condic
       ENDIF
  NEXT I%                                            ! Retorno Loop
  WRITE FORM "C12 C4 C21 C9 C1 C9 2C10 C2";         \! Graba segun Fmto
  #10,REG%;RESP1$,LE$+PULG$,PULGA$,                 \! Arch Directo
  PULGA1$,PULGA0$,SLS$," 000000000"," 000000000",FINR$ ! con datos de Vtas
  CLOSE 3                                            ! Cierre Icccmmdd.DAT
  CLOSE 4                                            ! Cierre Vcccmmdd.DAT
  CLOSE 10                                           ! Cierre CUADIA.DAT
  CLOSE 25                                           ! Cierre EAMImmdd.DAT
  CLOSE 26                                           ! Cierre EAMITEMR.DAT
  Open "TF:TRXSND" AS 77 Append                      ! Archivo para transmitir
  SndFile$ = Right$(FILNAME2$,12)                    ! Toma nombre del archivo
   X.Len% = Len(SndFile$)														 ! Toma longitud del registro
   X.Lec$ = "C"+Str$(X.len%)+" C2"									 ! Arma estructura de grabacion
   Write form X.Lec$; #77 ; SndFile$, Finr$          ! Graba registro
   Close 77 					     ! Cierra archivo
Stop                                                 ! PARAR

ERRTRAP:                                             !
IF ERR = "OE" AND ERRF% = 25 THEN RESUME REPORTA     !
IF ERR = "EF" AND ERRF% = 25 THEN BEGIN              !
   ERROR$ = ERR                                      !
   NOM$ = "**"                                       !
   DEP$ = "999"                                      !
   WRITE FORM "C40 C2";#54;" NO EXISTE PLU  "+KEI$, \!
               FINR$                                 !
   RESUME                                            !
ENDIF                                                !
IF ERRF% = 27 THEN BEGIN                             !
   CLEARS                                            !
   PRINT                                             !
   PRINT                                             ! 
   PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
   PRINT
   PRINT "               Consulte con el Depto de Sistemas."
   PRINT "                 Crear  C:\ADXALMA.DAT"    !
   STOP                                              !
ENDIF                                                !
IF ERRF% <> 25 THEN BEGIN                            ! 
   CONSOLE                                           !
   PRINT "                 ------------------------------------------"
   PRINT "                |  No  olvide  que para este proceso debe  |"
   PRINT "                |  haber CONVERTIDO la copia de BACKUP del |"
   PRINT "                |  archivo EAMIMOVO.DAT al archivo DIRECTO |"
   PRINT "                |  EAMImmdd.DAT.          REINICIE..!      |"
   PRINT "                |               Digite INTRO por Favor..!  |"
   INPUT "                 ------------------------------------------";NOM$
   HX% = ERRN                                        !
   ERRFX$=""                                         !
   FOR S% = 28 TO 0 STEP -4                          !
       SX% = SHIFT(HX%,S%)                           !
       SUM% = SX% AND 000FH                          !
       IF SUM% > 9 THEN                             \!
          SUM% = SUM% + 55                          \!
       ELSE                                         \!
          SUM% = SUM% + 48                           !
       Z$ = CHR$(SUM%)                               !
       ERRFX$ = ERRFX$ + Z$                          !
   NEXT S%                                           !
   CONSOLE                                           !
   PRINT "ERROR..: ";ERRL;"  ";ERR;"  ";ERRF%;"  ";ERRN;"  ";ERRFX$
   STOP                                              !
ENDIF                                                !
If ERRF% = 77 AND (ERR = "OE" OR ERR = "FU") Then  Begin 										! Error apertura 
   CREATE "TF:TRXSND" AS 39 
   Resume
EndIf 

END                                                  !
! ********************************************
!***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ********************************************
