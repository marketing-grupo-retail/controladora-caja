! CAMFECHA.BAS
!
!
STRING      GLOBAL KEY$, RECORD$,  FILEN$ , FILEN2$, FINR$, FECHA$, R1$, R2$, R3$, Caj$, Trx$, FECHA.NEW$, Fec$
INTEGER *4  GLOBAL X, IOS, KEY.LEN, REC.LEN, F, Z, G, T%(1), TER, NT, I
INTEGER *4  GLOBAL C, R, J
%INCLUDE INT42HEX.R86

CLEARS
PRINT "PROGRAMA PARA CAMBIAR LA FECHA AL LOG DE TRANSACCIONES "

! INITIALIZE FIELDS
R = 1
C = 1
NT = 900
DIM T%(NT)
FOR I = 0 TO NT
  T%(I) = 1
 NEXT I
INPUT "INGRESE ARCHIVO ORIGEN          :";FILEN$
FINR$ = CHR$(13)+CHR$(10)
On ERROR GOTO E1       ! GOTO ERROR HANDLER

Input "INGRESE ARCHIVO A  CREAR        :";FILEN2$

Input "INGRESE LA FECHA DESEADA AAMMDD :";FECHA.NEW$

CREATE POSFILE FILEN2$ AS 2
OPEN  FILEN$  AS 1  NOWRITE NODEL   ! OPEN A TRANSACTION LOG
IF END # 1 THEN  C1
GOSUB READ.A.RECORD
PROXIMO:
    IF KEY$ = PACK$("00") THEN  BEGIN  
       Caj$ = UnPack$(MID$(RECORD$,4,2))
       Trx$ = UnPack$(MID$(RECORD$,7,2))
       Fec$ = UnPack$(MID$(RECORD$,10,5))

       locate 17,1 : Print "fecha leida "+Fec$
       
!       FECHA.NEW$ = Left$(Fec$,6)         ! Fecha del log
!       If Mid$(Fec$,3,4) = "0501" Then FECHA.NEW$ = "150601"
!       If Mid$(Fec$,3,4) = "0502" Then FECHA.NEW$ = "150602"
      	
       	locate 16,1 : Print "almacena fecha "+FECHA.NEW$+ Right$(Fec$,4)
       	
       locate 18,1 : Print string$(78," ")
       locate 18,1 : Print "Term :"+Caj$+" Trx :"+Trx$
       J = LEN(RECORD$)
       R1$=Left$(RECORD$,9)
       R2$=PACK$(RIGHT$("000000"+FECHA.NEW$,6) + Right$(Fec$,4))
       R3$="" 
       For I = 15 To J
         R3$=R3$+MID$(RECORD$,I,1)
       Next I
       RECORD$=R1$+R2$+R3$
       T%(TER) = T%(TER) + 1
       SIGUIENTE:
       GoSub Write.A.RECORD
       GoSub READ.A.RECORD
    ENDIF ELSE GOSUB READ.A.RECORD
    GOTO PROXIMO                                     ! TRY NEXT SECTOR

C1: CLOSE 1, 2                                      ! EXTRACT DONE			

PRINT  " NUMERO DE REGISTROS= ";  R 
STOP        

E1: 
 Print "ERR = "; ERR;" FILE=";ERRF%;" ERRN = "; INT42HEX(ERRN)
Resume


WRITE.A.RECORD:
PRINT USING "&"; #2;RECORD$
R=R+1
LOCATE 12,10
PRINT "GRABADOS:",R
Return 

READ.A.RECORD:
       READ #1;LINE RECORD$
       IF END # 1 THEN  C1
       KEY$=MID$(RECORD$,2,1)
       C=C+1
       LOCATE 10,10
       PRINT "LEIDOS  :",C
Return 
!  *************END OF LTRANS.BAS****************************
