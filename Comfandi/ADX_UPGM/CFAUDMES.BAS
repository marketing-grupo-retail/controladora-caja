!                (CFCREBAT.BAS)      (Ver 1.0)
!
! Este programa sirve para RENOMBRAR los archivos de las Ventas
! del Per¡odo ANTERIOR :
!
!        Los Archivos son los Siguientes :
!
!             EAMTRANx.DAT  EAMTRANx.IDX  EAMTRANx.MDX 
!             EAMIMOVO.DAT  EAMTPROO.DAT  EAMTLISP.DAT
!             EAMEXCPO.DAT  EAMITEMR.DAT  EAMOPERA.DAT
!             EAMOPTNA.DAT
!
! La entrada  es  cualquier  archivo Transaction Summary Log
! A, B, o C .DAT en C:\ADX_IDT4.
!
! *********************** CFRENAM.BAS ***************************
 %ENVIRON C
  STRING       VALOR$,COJ$,COJ1$,           \!
               RESER, CAD$,CAD1$,           \!
               MESES(1),CAD2$,              \!
               ALMAS(1),COJ2$,              \!
               FORMAT,  COJ3$,              \!
               FORMATO,                     \!
               DD1$, DD2$, DD3$, DD4$,DD5$, \!
               DD6$
  INTEGER*1                                 \
           A%,       B%,          C%,       \
           D%,       E%,          F%,       \
           G%,       H%,          I%,       \
           J%,       K%,          L%
  INTEGER*2                                 \
           MES,      MESPRO

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
!****************************************************************
! ******               INICIO DEL PROGRAMA              ******
!****************************************************************

   DIM MESES(12)
   FECHA$=DATE$
   MESPRO=VAL(MID$(FECHA$,3,2))
   DIAPRO$=RIGHT$(FECHA$,2)
   ANOPRO$=LEFT$(FECHA$,2)
!----------------------   Llena arreglo de meses   ---------------------------
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
!----------------------------------------------------------------------------

EMPEZAR:
FINR$ = CHR$(13)+CHR$(10)
CALL CREAFECHA
FEC1$ = FRA.LIN$
ON ERROR GOTO ERRTN
CONSOLE
CLEARS
PRINT "  CFAUDMES.286"
PRINT 
PRINT "             PROGRAMA QUE PREPARA LOS ARCHIVOS DE COMANDO PARA"
PRINT "                 EJECUCION DE LAS SIGUIENTES TAREAS MENSUAMES "
PRINT "  "
PRINT "   C:\COMFANDI\COPIMES.BAT   Elaborar Copias de Seguridad AUDITORIA"
PRINT "  "
PRINT "                A L E R T A :  Fecha de Hoy : ";FHOY$
PRINT
GETNAME:
INPUT "    Indique MES  del Per¡odo que est  COPIANDO (MM) " ; RESP$
PRINT                                               !
IF LEN(RESP$)      <>  2  OR                       \! Si no son 8 Dig 
   RESP$ < "01" OR                                 \! o mes < 01
   RESP$ > "12" THEN BEGIN                          ! o mes > 12
   PRINT " "                                        ! Avanza Linea
   PRINT "  El mes  debe tener DOS   D¡gitos....."  ! Mensaje de Error
   PRINT "  en el Formato MM (Mes ).        "       ! de Formato
   INPUT "       Corrija el ERROR..." ; RESP$       ! Espera teclado
   GOTO EMPEZAR                                     ! y vuelve atras 
ENDIF                                               !
MES = VAL(RESP$)                                         !
PRINT TAB(18);MESES(MES) 
PRINT "  "                                          !
INPUT "       Confirme  SI/NO :  ";FAYER$           !
IF FAYER$ = "si" OR FAYER$ = "SI" THEN GOTO CONTI   !
GOTO EMPEZAR                                        !
CONTI:
INVEN$ = ALM$ + INVEN$                              ! Adiciona No Almac
PRINT
PRINT "    Por Favor Esperar..."                    !
CREATE "C:\COMFANDI\COPIMES.BAT" AS 36
CAD$ = "CD \"
CAD1$ ="CD EJ"
CAD2$ ="CD J"

BACK$ = "BACKUP C:\ADX_UDT1\AUDI"+RESP$+".DAT A:"
IF RESP$ = "10" THEN RESP$ = "0A"
IF RESP$ = "11" THEN RESP$ = "0B"
IF RESP$ = "12" THEN RESP$ = "0C"

COJ$ = "ADXNSXZL C C:\ADX_UDT1\AUDI"+RESP$+".DAT  J"+RIGHT$(RESP$,1)+"*"

WRITE FORM "C70 C2";#36;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#36;"CLS     ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO               Proceso de Copia MENSUAL DE  ROLLO AUDITORIA..!",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO                       Espere un Momento.. Por Favor..!",FINR$
WRITE FORM "C70 C2";#36;"ECHO ON",FINR$
WRITE FORM "C70 C2";#36;CAD$,FINR$
WRITE FORM "C70 C2";#36;CAD1$,FINR$
WRITE FORM "C70 C2";#36;CAD2$,FINR$
WRITE FORM "C70 C2";#36;COJ$,FINR$
WRITE FORM "C70 C2";#36;BACK$,FINR$
WRITE FORM "C70 C2";#36;"ECHO OFF",FINR$
WRITE FORM "C70 C2";#36;"CLS     ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO   ",FINR$
WRITE FORM "C70 C2";#36;"ECHO         COPIA DE ROLLO EN DISCO  O.K ..!",FINR$
WRITE FORM "C70 C2";#36;"PAUSE",FINR$
WRITE FORM "C70 C2";#36;"CD \",FINR$
WRITE FORM "C70 C2";#36;"CD COMFANDI",FINR$
WRITE FORM "C70 C2";#36;"BATCH DIRDKT.BAT",FINR$
WRITE FORM "C70 C2";#36;"AYUDA",FINR$

GOTO FINOK
!*************************************************************
  ERRTN:
  ERRORCOD$ = ERR
IF ERRF% = 29 THEN BEGIN
   CLEARS
   PRINT
   PRINT
   PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
   PRINT "        Arch =  ";ERRF%;"   Error = ";ERR;"   Cod = ";ERRN
   PRINT "               Consulte con el Depto de Sistemas."
   PRINT "                 Crear  C:\ADXALMA.DAT"
   STOP
ENDIF
  IF ERR <> "EF" AND                \
     ERRF% = 25 THEN BEGIN            ! Si NO es END OF FILE Sess#25
     CONSOLE
     PRINT
     PRINT "VERIFIQUE ARCHIVO ASIGNADO ..!"
     PRINT "DATOS INVALIDOS DESDE CONTROL "
     PRINT"  "
     STOP
  ENDIF  
  IF ERR = "EF" AND                \
     ERRF% = 25 THEN BEGIN           ! Si es END OF FILE Sess#25
        PRINT "     NORMAL EOJ..."
        PRINT "FIN DEL PROCESO NORMAL..!"
        STOP
  ENDIF    
!**************************************************************
FINOK:
!RESP$ = ANON$ + RIGHT$(RESP$,4)                     ! A¤o en ADXALMA.DAT
!AL$ = LEFT$(AL$,4)+RESP$+RIGHT$(AL$,LEN(AL$)-12)    ! Actualiza nuevo Dia
!WRITE FORM "C80" ;#29,1;AL$                         ! Graba ADXALMA.DAT
CLOSE 36
STOP
END
  !*****************   FIN DEL PROGRAMA PRINCIPAL  *************
