INTEGER*4  CON%,  AC1%,  AC2%

OPEN "C:\ACT" DIRECT RECL 101 AS 1
OPEN "EAMFBACT" KEYED RECL 101 AS 2
CLEARS
IF END #1 THEN SALIR
WHILE NOT VAL(A$)
   CON% = CON% + 1
   READ FORM "C101"; #1,CON%;REC$ 
   KEY$ = LEFT$(REC$,9)
   PUNA$ = MID$(REC$,10,4)
   TPUN$ = MID$(REC$,55,4)
   PAR1$ = RIGHT$(REC$,88)
   LOCATE 10,5
   PRINT "Reg Leidos: ";CON%;" Key: ";UNPACK$(KEY$);"  Pt/Per: ";
   PRINT "  Pt/Tot: "; VAL(UNPACK$(PUNA$));
   PRINT "  Pt/Per: "; VAL(UNPACK$(TPUN$));"            "
   IF VAL(UNPACK$(PUNA$)) > 0 THEN AC1% = AC1% + 1
   IF VAL(UNPACK$(TPUN$)) > 0 THEN AC2% = AC2% + 1
   LOCATE 11,10
   PRINT "Clientes con P/Acum: ";AC1%;
   PRINT " Clientes con P/Per: ";AC2%;"            "
   WRITE FORM "C9 C4 C88";#2;KEY$,TPUN$,PAR1$
WEND
SALIR:
STOP
