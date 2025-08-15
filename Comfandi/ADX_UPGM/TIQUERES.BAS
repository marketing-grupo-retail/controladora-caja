! PROGRAMA PARA IMPRIMIR TIQUES, PARTIENDO DEL ROLLO DE AUDITORIA
! TULIO ANGEL GIRALDO
!---------------------------------------------------------------------------

STRING MES$,DIA$,NTER$,NTER1$,NTRAN$,FECHA$,FECHA1$,FECHA2$,TIQUE$,NUMCB$
STRING NTRAN1$,FIELDS$,NTRAN2$,NTRAN3$,NTRAN4$,FECHA3$,NTRAN5$,NTRAN6$,NTRAN7$
STRING NTRAN8$, CCC$, UBICA$
INTEGER*1 A%,B%,C%,D%
INTEGER*2 NTRAN%,NTRAN1%,NTRAN10%
!----------------------------------------------------------------------------

FINR$=CHR$(13)+CHR$(10)		!CREA UN FIN DE REGISTRO
CLEARS				!LIMPIA PANTALLA

A%=1 

!*********** para extraer el numero del almacen **********

OPEN "c:/adxalma.dat" DIRECT RECL 80 AS 15

!IF END #15 THEN DONE2:  
DIM ALMAS$(100)
FOR K%=1 TO 100
	 READ FORM "C80";#15,K%;AL$
	 ALMAS$(K%)=AL$
NEXT K%

AL$=ALMAS$(1)
CENC$=STR$(VAL(LEFT$(AL$,4)))

!********************************************* 


!--------------------	MENU CAPTURA DATOS TIQUETE	----------------

!WHILE  A%<>0

!IF A%<>0 THEN BEGIN

CLEARS 
PRINT "		"
PRINT "	                   SOPORTE POS				 "
print "                   PROGRAMA PARA DESCOMPRIMIR BACKUP DE AUDITORIA 		 "
PRINT "                       Tec. Tulio A. Giraldo          		 "
PRINT "                                                      		 "
PRINT "                                                      		 " 
INPUT "	DIGITE ANO - 2 DIGITOS (AA ej: 01,07,12): 	";ANO$ 
INPUT "	DIGITE MES - 2 DIGITOS (MM ej: 02,09,11):       ";MES$ 
INPUT "	DIGITE DIA - 2 DIGITOS (DD ej: 07,16,27):	";DIA$
INPUT "	DIGITE CENTRO DE COSTO (CCC ej: 510, 535,802):	";CCC$
PRINT "	DIGITE UBICACION ROLLO SIN ESPACIOS Y RUTA COMPLETA "
INPUT "	              Ej: c:/datos/tique/              :";UBICA$


!------ VALIDA INFORMACION INGRESADA --------------- 

IF VAL(ANO$)<0 OR VAL(ANO$)>99 THEN BEGIN
	 CLEARS
	 PRINT "		 LOS ANOS VAN DESDE 00 A 99" 
	A%=1
	 WAIT;2000
ENDIF

IF VAL(MES$)>13 THEN BEGIN
	 CLEARS
	 PRINT "		 LOS MESES VAN DESDE 01 A 12"  
	A%=1
	 WAIT;2000 
ENDIF  
IF VAL(DIA$)<1 OR VAL(DIA$)>31 THEN BEGIN
	 CLEARS
	 PRINT "		 LOS DIAS VAN DESDE 01 A 31" 
	A%=1
	 WAIT;2000 
ENDIF

IF VAL(CCC$)<1 OR VAL(CCC$)>999 THEN BEGIN
	 CLEARS
	 PRINT "		 EL CENTRO DE COSTO NO ES EL ESPERADO" 
	A%=1
	 WAIT;2000 
ENDIF

!ENDIF ELSE BEGIN 
!A%=0
!ENDIF

!WEND

!AUD$="C:/ADX_UDT1/AUD"+MES$+DIA$+".DAT"
!AUDI$="C:/ADX_UDT1/JE"+ANO$+MES$+DIA$+"."+CENC$

!WRITE FORM "C70 C2";#5;CREA$,FINR$  

CREA$="ADXNSXZL S "+UBICA$+"je"+ANO$+MES$+DIA$+"."+CCC$+" C:/ej/j/"
!CREA1$="ADXNSXZL S "+AUDI$+" C:/EJ/J/"

CREATE "C:/COMFANDI/DESCOTIQ.BAT" AS 5

WRITE FORM "C70 C2";#5;"ECHO OF",FINR$ 
WRITE FORM "C70 C2";#5;CREA$,FINR$   
!WRITE FORM "C70 C2";#5;"EXIT",FINR$ 


END 
