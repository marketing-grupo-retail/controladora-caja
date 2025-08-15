INTEGER*4  LEI%,  GRA%,   NEX%
STRING MESES(1)
DIM MESES(12)
   MESES(1)="ENERO"     :  MESES(2)="FEBRERO"    :  MESES(3)="MARZO"
   MESES(4)="ABRIL"     :  MESES(5)="MAYO"       :  MESES(6)="JUNIO"
   MESES(7)="JULIO"     :  MESES(8)="AGOSTO"     :  MESES(9)="SEPTIEMBRE"
   MESES(10)="OCTUBRE"  :  MESES(11)="NOVIEMBRE" :  MESES(12)="DICIEMBRE"
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
CALL CREAFECHA                                      !
FEC1$ = FRA.LIN$                                    !
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
APLANO$= "C:\CFIEL"+NUAL$+".DAT" 
ADIREC$= "C:\CLIENTES.DIR"
CREATE POSFILE "APLANO$" AS 3 LOCAL
FINR$ = CHR$(13)+CHR$(10)
OPEN "C:\CLIENTES.DIR" DIRECT RECL 254 AS 1
!OPEN "EAMFBACT" KEYED RECL 101 AS 2
CLEARS
PRINT : PRINT : PRINT : PRINT 
PRINT "       Convierte EAMFBCUS en archivo Plano para B. OBJECT    "
PRINT "       Proceso para el Almac‚n No. " ; ALM$;"  ";NOAL$
PRINT "       Inicia a las: ";FEC1$                  ! Indica comienzo
PRINT                                                ! Avanza Linea
PRINT "   Se est  procesando                  : ";ADIREC$
PRINT "   Se CREARA el Archivo de INTERFACE   : ";APLANO$
PRINT "                    Por Favor Esperar..."     !
IF END #1 THEN SALIR
ON ERROR GOTO RUTER
LEER:
LEI%  = LEI% + 1
READ FORM "C9 C1 C17 C13 C25 C25 C20 C2 C3 C2 C2 C4 C2 C4 C20 C3 C4 C2 C5 C1 C1 C3 C3 C1 C61 C5 C16 ";#1,LEI%;KEY$,N$,AP$,NOM$,DI1$,DI2$,CIU$,ST$,ZPO$,ZPO1$,T$,TEL$,T2$,TEL2$,EMA$,\
STO$,PAR1$,SS1$,SS2$,SEX$,NAPL$,NAC$,PAR0$,TFL$,PAR2$,EMA2$,PAR4$ 
EOF$ = "  "
IF EOF$ = "  " THEN BEGIN
   GRA% = GRA% + 1
NAC$     = UNPACK$(NAC$)
ZPO$     = UNPACK$(ZPO$)
ZPO1$    = UNPACK$(ZPO1$)
TELE$    = UNPACK$(TEL$)
TEL2$    = UNPACK$(TEL2$)
TFL$     = UNPACK$(TFL$)
CEDULA1$ = UNPACK$(SS1$)
CEDULA2$ = UNPACK$(SS2$)
CLIENTE$ = (UNPACK$(KEY$))
CLIENTE$ = RIGHT$(CLIENTE$,9)
WRITE FORM "C9 C13 C17 C25 C25 C20 C2 C6 C4 C8 C8 C20 C5 C10 C1 C6 C2 C2";#3; \
CLIENTE$,AP$,NOM$,DI1$,DI2$,CIU$,ST$,ZPO$,ZPO1$,TELE$, \
TEL2$,EMA$,EMA2$,CEDULA2$,SEX$,NAC$,TFL$,FINR$ 
ENDIF ELSE BEGIN
   NEX% = NEX% + 1
ENDIF
LOCATE 14,5  :  PRINT "    Le¡dos: ";LEI%
LOCATE 15,5  :  PRINT "  Grabados: ";GRA%
LOCATE 16,5  :  PRINT "No Existen: ";NEX%
LOCATE 17,5  :  PRINT "Cuenta No : ";unpack$(key$)

GOTO LEER

RUTER:
EOF$ = ERR
IF ERR = "EF" AND ERRF% = 2 THEN RESUME
PRINT "ERROR "; ERR;"  FILE ";ERRF%
SALIR:
close 1 : close 3 :close 27
STOP
