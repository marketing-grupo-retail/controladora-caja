!--- Carga de opciones de terminal
!--  Grupo Retail Ltda 

Integer*1 GLOBAl N.TERM%, I%,	OK%
Integer*2 GLOBAL 	UE%,STATUS%, IN.TERM%
String	GLOBAL COMPL$, KEY$

On Error GoTo ERRORES
Clears
IN.TERM% = 50
Open "EAMTERMS" KEYED RECL 72 AS 28
For I% = 1 TO IN.TERM%
  Print "Cargando Opciones Terminal "+Str$(I%)
  KEY$ = PACK$(RIGHT$(STRING$(4,"0")+STR$(I%),4))
  OK% = 1
  READ FORM "C2 C66 I2 I2";#28 AUTOLOCK KEY KEY$;KEY$,COMPL$,STATUS%,UE%
  STATUS% = STATUS% OR 0800H
  IF OK% = 1 THEN \
	  Write FORM "C2 C66 I2 I2";#28 AUTOUNLOCK;KEY$,COMPL$,STATUS%,UE%
Next I%
Close 28
Stop 

ERRORES:
OK% = 0
IF ERR = "EF" Then Resume
PRINT "ERROR : ";ERR		
Stop 
