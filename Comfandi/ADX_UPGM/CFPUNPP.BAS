STRING MESES(1),RESP$
INTEGER*4  LEI%,  GRA%,   NEX%
DIM MESES(12)
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

OPEN "C:\ADXALMA.DAT" DIRECT RECL 80 AS 29          ! Archivo Control Almacen
DIM ALMAS$(100)                                     ! Para Salvar ADXALMA.DAT
FOR K% = 1 TO 100                                   ! 
    READ FORM "C80";#29,K%;AL$                      ! Lee Control Almac
    ALMAS$(K%) = AL$                                ! y lo guarda
NEXT K%                                             !
AL$ = ALMAS$(1)                                     ! Lo Recupera
POS%  = MATCH("!",AL$,1)                            ! Verif Cant Digitos del Reg
IF POS%  < 12 THEN BEGIN                            ! Detecta Dise¤o Anterior
   AL$ = LEFT$(AL$,8)+RESP$+RIGHT$(AL$,LEN(AL$)-8)  ! Inserta Fecha Digitada
ENDIF                                               !
NUAL$ = STR$(VAL(LEFT$(AL$, 4)))                    ! Guarda Num Almacen
ANON$ = MID$(AL$,5,4)                               ! Guarda Ano Actual
AM$   = MID$(AL$,7,4)                               ! Guarda Ano/Mes Actual
AYER$ = MID$(AL$,5,8)                               ! A¤o Mes y Dia de Proceso
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
CLEARS
PRINT : PRINT : PRINT : PRINT 
EMPEZAR:
FINR$ = CHR$(13)+CHR$(10)
CALL CREAFECHA
FEC1$ = FRA.LIN$
!IF END #25 THEN FINOK
CONSOLE
CLEARS
PRINT "  CFPANTE.286"
PRINT 
PRINT "             PROGRAMA QUE PREPARA EL ARCHIVO DE INTERFACE  "
PRINT "              PARA ACTUALIZAR PUNTOS PERIODO EN LA APLI- "
PRINT "              CACION DE BUSSINESS OBJECT   "
PRINT "  "
PRINT "                A L E R T A :  Fecha de Hoy : ";FHOY$
PRINT
GETNAME:
INPUT "    Indique MES DEL PERIODO QUE ESTA CERRANDO  (MM) " ; RESP$
PRINT                                               !
IF LEN(RESP$)      <>  2  OR                       \! Si no son 8 Dig 
   RESP$ < "01" OR                                 \! o mes < 01
   RESP$ > "12" THEN BEGIN                         \! o mes > 12
   PRINT " "                                        ! Avanza Linea
   PRINT " Verifique el mes que est… procesando."   ! Mensaje de Error
   PRINT "  en el Formato MM  (Mes).               "! de Formato
   INPUT "       Corrija el ERROR..." ; RESP$       ! Espera teclado
  ! CLOSE 29                                         !
   GOTO EMPEZAR                                     ! y vuelve atras 
ENDIF                                               !
FILE2$ = "C:\PU"+NUAL$ + RESP$+".DAT"
!FILE2$ = "PUNTOSA.DAT"
!CREATE POSFILE FILE2$ AS 3 LOCAL
CREATE POSFILE "C:\PUNTOSA.DAT" AS 3 LOCAL
OPEN "C:\EAMFBAPP.DIR" DIRECT RECL 36 AS 1
IF END #1 THEN SALIR
ON ERROR GOTO RUTER
LEER:
LEI%  = LEI% + 1
READ FORM "C9 C2 C2 C3 C3 C4 C2 C4 C7";#1,  \
LEI%;KEY$,DATES$,DATEE$,DATEL$,PUNTOSL$,PUNTOSP$,TRANSP$,REDIM$,REST$

!PUNT% = VAL(UNPACK$(PUNP$))
EOF$ = "  "
IF EOF$ = "  " THEN BEGIN
FECHAI$    = UNPACK$(DATES$)
REDIM$     = UNPACK$(REDIM$)
FECHAF$    = UNPACK$(DATEE$)
FECHAU$    = UNPACK$(DATEL$)
PUNTOSU$   = UNPACK$(PUNTOSL$)
CLIENTE$ = (UNPACK$(KEY$))
CLIENTE$ = RIGHT$(CLIENTE$,9)
PUNTOSP$ = UNPACK$(PUNTOSP$)
TRANSP$  = UNPACK$(TRANSP$)

IF VAL(PUNTOSP$) > 0 THEN BEGIN
 WRITE FORM "C9 C4 C4 C6 C6 C8 C4 C8 C2 ";#3;CLIENTE$,FECHAI$,FECHAF$,\
FECHAU$,PUNTOSU$,PUNTOSP$,TRANSP$,REDIM$,FINR$ 
GRA% = GRA%+1
ENDIF
ENDIF ELSE BEGIN
   NEX% = NEX% + 1
ENDIF
LOCATE 13,5  :  PRINT " C. Costo : ";NUAL$
LOCATE 15,5  :  PRINT "    Le¡dos: ";LEI%
LOCATE 16,5  :  PRINT "  Grabados: ";GRA%
LOCATE 17,5  :  PRINT "No Existen: ";NEX%
LOCATE 18,5  :  PRINT "Cuenta No : ";unpack$(key$)
LOCATE 14,5  :  PRINT "puntos per: ";PUNTOSP$
LOCATE 19,5  :  PRINT "Se est… escribiendo sobre: ";file2$ 
LOCATE 20,5  :  PRINT "Mes Cerrado  : ";MESES(VAL(RESP$))
GOTO LEER

RUTER:
EOF$ = ERR
IF ERR = "EF" AND ERRF% = 2 THEN RESUME
!IF ERR = "FU" AND ERRF% = 1 THEN RESUME
PRINT "ERROR "; ERR;"  FILE ";ERRF%
SALIR:
close 1 : close 3 : CLOSE 29
STOP
