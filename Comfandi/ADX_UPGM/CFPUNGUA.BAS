INTEGER*4  LEI%,  GRA%,   NEX%, cod%																! variables
INTEGER*4  punt%,REDI%									  																! variables
String FINR$,punp$,puna$,punp2$,llave$,puntosp$,puntosf$,REDI$,PART3$																												! variables string

CREATE POSFILE "C:\PUNTOS.DAT" AS 3 LOCAL

FINR$ = CHR$(13)+CHR$(10)																						!
OPEN "C:\EAMFBAPP.008" DIRECT RECL 36 AS 1												! Abre archivo actividad previa
OPEN "EAMFBACT" KEYED RECL 101 AS 2																	! Abre archivo actividad

CLEARS																															! Limpia pantalla
PRINT : PRINT : PRINT : PRINT																					
PRINT "          Programa para ACUMULAR  Puntos/Mes anterior a "		!
PRINT "          puntos periodo ACTUAL OJO Solo para un cierre  "		!muestra en pantalla
PRINT "          de puntos que se quiere reversar              "		!

IF END #1 THEN SALIR																								!	si fin archivo 1
ON ERROR GOTO RUTER																									!	rutina erroes
LEER:																																!	lee archivo
LEI%  = LEI% + 1																										
READ FORM "C9 C10 C4 C2 C4 C7";#1,LEI%;KEY$,PART1$,PUNP$,PART2$,REDI$,PART3$!lee eamfbapp
PUNT% = VAL(UNPACK$(PUNP$))	
REDI% = VAL(UNPACK$(REDI$))												! puntos anteriores
EOF$ = "  "
READ FORM "C9 C4 C41 C4 C43";#2 KEY KEY$;KEY$,PUNA$,REST1$,PUNP1$,REST$	!lee eamfbact
IF EOF$ = "  " THEN BEGIN
   GRA% = GRA% + 1
!   PUNA$ =  PACK$(RIGHT$("00000000" + STR$(VAL(UNPACK$(PUNA$))-PUNT%),8))	!
    PUNP2$ = PACK$(RIGHT$("00000000" + STR$(VAL(UNPACK$(PUNP1$))+PUNT%-REDI%),8))	!

    !PUNP2$ = PACK$(RIGHT$("00000000" + STR$(PUNT%),8))												!

   WRITE FORM "C9 C4 C41 C4 C43";#2;KEY$,PUNA$,REST1$,PUNP2$,REST$						!escribe en actividad
   llave$ =		RIGHT$("0000000000000"+UNPACK$(KEY$),13)
   puntosp$ = right$("00000000"+unpack$(PUNp$),8)
   puntosf$ = right$("00000000"+unpack$(PUNp2$),8)
     if punt% > 1 then begin
 			WRITE FORM "C13 C8 C8 C2";#3;llave$,puntosp$,puntosf$,FINR$ 
     endif
ENDIF ELSE BEGIN
   NEX% = NEX% + 1
ENDIF
LOCATE 10,5  :  PRINT "    Le¡dos: ";LEI%
LOCATE 11,5  :  PRINT "  Grabados: ";GRA%
LOCATE 12,5  :  PRINT "No Existen: ";NEX%
LOCATE 13,5  :  PRINT "Cuenta No : ";unpack$(key$)
LOCATE 14,5  :  PRINT "puntos per: ";unpack$(punp1$)
LOCATE 15,5  :  PRINT "pu. anteri: ";punt%
LOCATE 16,5  :  PRINT "pu.ahora  : ";unpack$(punp2$)

GOTO LEER

RUTER:
EOF$ = ERR
If ERR = "IH" Then Resume 
IF ERR = "EF" AND ERRF% = 2 THEN RESUME
IF ERR = "OE" AND ERRF% = 2 THEN RESUME
IF ERR = "KF" AND ERRF% = 2 THEN RESUME
PRINT "ERROR "; ERR;"  FILE ";ERRF%
SALIR:
close 1 : close 2
STOP
