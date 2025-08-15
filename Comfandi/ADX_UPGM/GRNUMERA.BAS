!Programa para separar Movimientos de dias
!de un log de transacciones
!
STRING      Global     KEY$, RECORD$,  FILEN$ , FINR$, FECHA$, Salida$, FProc$, Term$, Tmp$, Trx$, New$
String Global Tran$
INTEGER*2  Global X, IOS, KEY.LEN, REC.LEN, F, Z, G
INTEGER*4  Global C, R, TRX%(1), Long%, Nlog%, I%

!%INCLUDE INT42HEX.R86
! INITIALIZE FIELDS
FINR$ = CHR$(13)+CHR$(10)
On ERROR GOTO E1       ! GOTO ERROR HANDLER
CLEARS
Input "Nombre Log Trx a procesar :";Filen$
Input "Nombre Log Destino        :";Salida$
DIM TRX%(9999)
FOR C = 1 TO 9999
    TRX%(C) = 0 
NEXT C
C = 0
CREATE POSFILE Salida$ AS 2
OPEN  FILEN$  AS 1  NOWRITE NODEL   ! OPEN A TRANSACTION LOG
IF END # 1 THEN  C1
GOSUB READ.A.RECORD
PROXIMO:
    IF KEY$ = PACK$("00") THEN  BEGIN  \ 
       FECHA$=MID$(RECORD$,10,3)
       IF FECHA$=FECHA$ THEN BEGIN \
          GOSUB WRITE.A.RECORD
          GOSUB READ.A.RECORD
          WHILE KEY$ <> PACK$("00") AND \
                KEY$ <> PACK$("21") AND \
                KEY$ <> PACK$("20")
            GOSUB WRITE.A.RECORD                  
            GOSUB READ.A.RECORD
          WEND 
          ENDIF ELSE  GOSUB READ.A.RECORD 
       ENDIF ELSE GOSUB READ.A.RECORD
    GOTO PROXIMO                                     ! TRY NEXT SECTOR
C1: CLOSE 1, 2                                      ! EXTRACT DONE			

PRINT  " NUMERO DE REGISTROS= ";  R 
STOP        

WRITE.A.RECORD:
PRINT USING "&"; #2;RECORD$
R=R+1
!IF INT%(R/1000)*1000=R THEN 
	
 LOCATE 12,10 :PRINT "GRABADOS:",R
RETURN

READ.A.RECORD:
       
       READ #1 ; LINE RECORD$
       KEY$ = MID$(RECORD$,2,1)
       Term$ = Unpack$(MID$(RECORD$,4,2))
       Tran$ = Unpack$(MID$(RECORD$,7,2))
       Long% = Len(Record$)
       Trx%(Val(Term$)) = Trx%(Val(Term$)) + 1 
       Trx$ = Str$( Trx%(Val(Term$)) )
       Trx$ = Pack$(Right$("0000"+Trx$,4))
       
       I% = 0
       TMP$ = ""
       For I% = 1 to Long%
           IF I% < 7 OR I% > 8 THEN BEGIN
           	  TMP$ = TMP$ + MID$(RECORD$,I%,1)
           ENDIF ELSE BEGIN
           	IF I% = 8 THEN TMP$ = TMP$ + TRX$
           ENDIF
       Next I%
       
       NLOG% = LEN(TMP$)
       RECORD$ = TMP$
       
       LOCATE 15,1 : PRINT STRING$(60," ")
       LOCATE 15,1 : PRINT "TERMINAL "+TERM$+" TRX "+TRAN$+" Nueva "+str$(Trx%(Val(Term$))) + " - " + \
                     str$(long%) + " - "+str$(nlog%)
              
       C = C + 1
       !IF INT%(C/1000)*1000=C THEN LOCATE 10,10 :PRINT "LEIDOS  :",C
       	
       LOCATE 10,10 :PRINT "LEIDOS  :",C
RETURN
!  *************END OF LTRANS.BAS****************************

E1:
 IF ERR = "IH" THEN RESUME 
 LOCATE 21,1 : PRINT "ERR = "; ERR;" FILE=";ERRF%
STOP 
