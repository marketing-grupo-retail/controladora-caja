! este programa aplica una novedad al archivo de clientes
! ligandole un mensaje 
! archivo cupoari.txt numero de cliente, y mensaje a asignar

INTEGER*4  LEI%,  GRA%
INTEGER*2  NEX%
OPEN "C:\CUPOARI.TXT" AS 1
OPEN "EAMFBACT" KEYED RECL 101 AS 2
CLEARS
PRINT : PRINT : PRINT : PRINT 
PRINT "          Programa para Aplicar la novedad de clientes ARIEL    "
IF END #1 THEN SALIR
ON ERROR GOTO RUTER
LEER:
READ #1;LINE REG$ 
LEI%  = LEI% + 1
KEY$ = PACK$(RIGHT$(STRING$(18,"0")+LEFT$(REG$,9),18))
PUNT% = VAL(RIGHT$(REG$,2))
EOF$ = "  "
READ FORM "C9 C20 C1 C71";#2 KEY KEY$;KEY$,PUNA$,MENS$,REST$
IF EOF$ = "  " THEN BEGIN
   MENS$ = PACK$(RIGHT$("00" + STR$(PUNT%),2))
   WRITE FORM "C9 C20 C1 C71";#2;KEY$,PUNA$,MENS$,REST$
   GRA% = GRA% + 1
ENDIF ELSE BEGIN
 NEX% = NEX% + 1
  RESUME 
ENDIF
cuenta$ = unpack$(key$)
LOCATE 6,5   :  PRINT "Cod. Cliente:";cuenta$
LOCATE 8,5   :  PRINT "Mensaje Asig";punt%
LOCATE 10,5  :  PRINT "    Le¡dos: ";LEI%
LOCATE 11,5  :  PRINT "  Grabados: ";GRA%
LOCATE 12,5  :  PRINT "No Existen: ";NEX%
GOTO LEER

RUTER:
RESUME
EOF$ = ERR
IF ERR = "EF" AND ERRF% = 1 THEN RESUME

SALIR:
close 1 : close 2
STOP
