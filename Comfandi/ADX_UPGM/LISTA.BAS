STRING A$, X$, J$, Y$
INTEGER*2 I%

X$ = "81008104000000007012"
Y$ = ""
INPUT "PROMO ";A$
FOR I% = 1 TO 20 STEP 4
    J$ = MID$(X$,I%,4)
    PRINT J$
    IF J$ = "0000" THEN BEGIN
       J$ = A$
       A$ = "0000"
    ENDIF 
    Y$ = Y$ + J$
NEXT I%
PRINT "RESULTADO "+Y$
Y$ = ""
FOR I% = 1 TO 20 STEP 4
    J$ = MID$(X$,I%,4)
    PRINT J$
    IF J$ = A$ THEN BEGIN
       J$ = "0000"
       A$ = "0000"
    ENDIF 
    Y$ = Y$ + J$
NEXT I%
PRINT "RESULTADO "+Y$


STOP 
