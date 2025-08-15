!SCREEN 12
!COLOR 15
!                         (LOGMESAR.BAS)
!                           (Ver 1.01)
!______________________________________________________________________________
! FECHA: AGOSTO 01 DE 2009
! AUTOR: ING. PASCUAL BARRERA
! DESCRIPCION
!      PROGRAMA PARA EXTRAER LA VENTA DE COGIGOS EN VARIOS DIAS (30)
!            LOS ARCHIVOS ENTRANTES (EAMTmmdd.DAT) DEBEN ESTAR
!            EN LA CARPETA \A\
! ! *************************** SMKTTLOG.BAS ***************************
!
  INTEGER*4 FLAG, REI%, REL%, LAR%, TER%, TRA%, PIBO%
  INTEGER*2 P,Q
  INTEGER*1 A,B,I,J,K,TLOG,OUTFIL,NREG%,CONT%
  STRING INAREA$,A$,B$,PIBO$,FECTRA$,HORA$,CADENA$,MES$,FILE$,DIA$
  STRING PREC$,CODI$,NREC$,CADENA1$,SG$,DEP$,CODARTI$
!
!
!**********************************************************************
!
  FUNCTION ERRNSTR$(ERRNUM)
!
    INTEGER*1 I
    INTEGER*4 ERRNUM,WORK
    STRING HEX$,ERRNSTR$,WORK$
!
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
!
    FOR I=1 TO 8
      WORK=ERRNUM AND 0000000FH ! AND OFF ALL BUT LOW NYBBLE
      WORK$=MID$(HEX$,WORK+1,1)+WORK$ ! ADD HEX VALUE TO OUTPUT STRING
      ERRNUM=SHIFT(ERRNUM,4) ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$
!
 FEND
!
!**********************************************************************
!                   ****** PROGRAM STARTS ******
!**********************************************************************
!
!  ON ERROR GOTO FILERR
  ON ERRORP GOTO ERRORES
  IF END #25 THEN SALIDA
  CONSOLE
  CLEARS
  PRINT : PRINT : PRINT
  PRINT "      ***** PROGRAMA PARA VER EL TRANSACTION SUMMARY LOG  ******"
  PRINT
  PRINT "          Produce un Archivo Tipo TEXTO en: C:\A\FILMMDD.DAT "
  PRINT "          Donde MMDD Es el mes dia corespondiente al digitado"
  PRINT "          Por el usuario apartir de un dia de Inicio ......"
  GETNAME:
  PRINT
  PRINT "          Los archivos que se estraen la informacion deben"
  PRINT "          estar en el Directorio  C:\A\EAMTMMDD.DAT"
  PRINT "          donde MMDD es coresponde al digitado por el Usuario"
  PRINT

   TERMS = 25                                          !
   OPEN "EAMTERMS" KEYED RECL 72 AS TERMS NOWRITE NODEL!
   KEY$ = PACK$("9999")                                !
   READ FORM "C2 C8 C62";#TERMS KEY KEY$;KEY$,TRAN$,RESTO$
   CLOSE TERMS                                         !

!TRAN$ = "EAMTRANA"                                  ! PASCUAL OTRO HOY
!B$     = "ADXLXACN::\ADX_UDT1\EAMT"+C$+".DAT"        !  PASCUAL
  PIBO% = 0

      IF TRAN$ = "EAMTRANC" AND PIBO% = 0 THEN BEGIN 
   !PRINT "HOLA"
        TRAN$ = "EAMTRANB"  
        PIBO% = 1
      ENDIF
      IF TRAN$ = "EAMTRANB" AND PIBO% = 0 THEN BEGIN 
         TRAN$ = "EAMTRANA"  
         PIBO% = 1
     ENDIF
      IF TRAN$ = "EAMTRANA" AND PIBO% = 0 THEN BEGIN
         TRAN$ = "EAMTRANC"  
         PIBO% = 1
     ENDIF
     B$     = "C:\ADX_IDT4\"+TRAN$+".DAT"         !  PASCUAL
   NUR$ = "1"
   TER$ = "999"

LOCATE 14,20
  INPUT "        DIGITE MES ";C$
LOCATE 15,20
  INPUT " DIGITE DIA INICIO ";NREG% 
LOCATE 16,20
  INPUT " DIGITE CODIGO ARTICULO ";CODARTI$ 

! DIA1$ = RIGHT$("00"+STR$(NREG%),2)

!B1$ = "C:\A\EAMT"+C$+DIA1$+".DAT"   ! CAMBIO PASCUAL 

  FILE1$ = "C:\DATOS\ARTICULO.DAT"   ! CAMBIO PASCUAL 
  CREATE FILE1$  AS 5
!  WRITE #OUTFIL1; "TRANSACTION LOG = ",B$
  WRITE #5; " FECHA   HORA TER TRA  COD     OPE    DEP  VALOR   SG"
!  OPEN B1$ AS TLOG1 BUFFSIZE 5120 NOWRITE NODEL

CONT%=0
NREG1%=1


FOR n=1 TO NREG1% 

!WHILE NREG%>0 AND NREG%<32
DIA$ = RIGHT$("00"+STR$(NREG1%),2)
LOCATE 17,10
PRINT " ษอออออออออออออออออออออออออออออออออหออออออออออออออออออป  "
LOCATE 18,10
PRINT " บ  REGISTROS LEIDOS Y PROCESADOS  บ                  บ   "
LOCATE 19,10
PRINT " ศอออออออออออออออออออออออออออออออออสออออออออออออออออออผ   "

!LOCATE 18,55
PRINT " ";NREG%

B$ = "C:\ADX_UDT1\EAMT"+C$+DIA$+".DAT"   ! CAMBIO PASCUAL 
   REI% = VAL(NUR$)
   IF VAL(TER$) > 0 THEN  REI% = 0
!  PRINT

LOCATE 22,20
   PRINT "PROCESSING ";B$
   PRINT
  TLOG = 25:OUTFIL = 26
!................................................................


 
!  FILE$ = "C:\A\FIL"+C$+DIA$+".DAT"   ! CAMBIO PASCUAL 
!  CREATE FILE$  AS OUTFIL BUFFSIZE 5120 LOCKED
!  WRITE #OUTFIL; "TRANSACTION LOG = ",B$
!  WRITE #OUTFIL; " FECHA   HORA TER TRA  COD     OPE    DEP  VALOR   SG"
  OPEN B$ AS TLOG !BUFFSIZE 5120 NOWRITE NODEL
  NREG% = NREG% + 1
  B$ = ""
  ON ERROR GOTO ERRTN
  NXTRCD:
  Q=1
  READ #TLOG; LINE INAREA$
  LAR% = LAR% + LEN(INAREA$)
  REL% = REL% + 1
  CONSOLE
  LOCATE 20,10
  PRINT "Registros Leกdos: ";STR$(REL%);"     Offset TLOG: ";STR$(LAR%)
  IF REL% < REI% THEN GOTO NXTRCD
  INAREA$ = INAREA$ + ","
  WHILE (Q < LEN(INAREA$))
    P = MATCH (",",INAREA$,Q)                ! FIND STRING DELIMETER
    IF (P-Q) < 3 THEN BEGIN                  ! TEST FOR MISSING STRING
!      WRITE #OUTFIL;" "
!      WRITE #OUTFIL;"MISSING STRING"
!      PRINT "MISSING STRING"
      Q=P+1                                  ! SET UP FOR NEXT STRING
      GOTO AGAIN
    ENDIF
    B$ = MID$(INAREA$,Q+1,(P-Q)-2)           ! MOVE STRING WITHOUT QUOTES
    B$ = B$+":"                              ! ADD TRAILING SEMI-COLON
    Q=P+1                                    ! SET UP FOR NEXT STRING
    A=VAL(UNPACK$(LEFT$(B$,1)))              ! DETERMINE STRING TYPE
    IF (A < 0) OR (A > 19) THEN BEGIN
      GOTO AGAIN
    ENDIF
    AA% = A
    IF A <> 0 THEN BEGIN
!      WRITE #OUTFIL;"***STR TYPE = ",A      ! PRINT "STRING TYPE = ";A
    ENDIF
    IF A = 0 THEN GOSUB S0:GOTO AGAIN
    IF A = 99 THEN GOSUB S99:GOTO AGAIN
    ON A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21


AGAIN:
  WEND
  GOTO NXTRCD
!PRINT "HOLA", CONT%
IMPRIME:
!WRITE #OUTFIL;CADENA1$
!NEXT n
RETURN

! IF  VAL(CODI$) < 1000  \   CRNE DE RES Y CERDO 0% 

!  IF  VAL(DEP$) = 79 OR \  ! CRNE DE RES Y CERDO 0% 
!      VAL(DEP$) = 80 OR \  ! PESCADERIA
!      VAL(DEP$) = 81 OR \  ! POLLO Y PEZ DEL 10%
!      VAL(DEP$) = 82    \  ! RES Y CERDO DEL 10%


S1:
  J = 3
  GOSUB GETUNPK

  CODI$ = A$
!  WRITE #OUTFIL;"  ITEM CODE = ",A$
  GOSUB GETUNPK
  PREC$ = A$
!  WRITE #OUTFIL;"      PRICE = ",VAL(A$)
  GOSUB GETUNPK

 DEP$=A$
  IF  VAL(DEP$) > 0  \  ! CRNE DE RES Y CERDO 0% 
       THEN BEGIN  
! WRITE #OUTFIL;"   DEPARTME = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"   FAMILYNU = ",VAL(A$)
  GOSUB GETUNPK
  GOSUB GETFLAG
!  WRITE #OUTFIL;"INDICAT1 HI = ",LEFT$(A$,8)
!  WRITE #OUTFIL;"INDICAT1 LO = ",RIGHT$(A$,8)
  GOSUB GETUNPK
  GOSUB GETFLAG
!  WRITE #OUTFIL;"INDICAT2 HI = ",LEFT$(A$,8)
  SG$ =MID$(RIGHT$(A$,8),1,1)
! WRITE #OUTFIL;"INDICAT2 LO = ",RIGHT$(A$,8)
  GOSUB GETUNPK
! WRITE #OUTFIL;"INDICAT3    = ",VAL(A$)

  CADENA1$ = " "
! IF CODI$ = "999010"  THEN CADENA1$ = FECTRA$+" "+CODI$+"  "+RIGHT$("000000"+NREC$,6)+" "+ RIGHT$("00000000"+PREC$,8)+" "+SG$ 
! IF CODI$ = "999011"  THEN CADENA1$ = FECTRA$+" "+CODI$+"  "+RIGHT$("000000"+NREC$,6)+" "+ RIGHT$("00000000"+PREC$,8)+" "+SG$ 
! IF CODI$ = "999012"  THEN CADENA1$ = FECTRA$+" "+CODI$+"  "+RIGHT$("000000"+NREC$,6)+" "+ RIGHT$("00000000"+PREC$,8)+" "+SG$ 
! WRITE #OUTFIL;CADENA1$


IF CODARTI$=CODI$ THEN BEGIN

 CADENA1$ = FECTRA$+" "+HORA$+" "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("0000000"+CODI$,7)+" "+ RIGHT$("000000"+NUOPE$,6)\ 
            +" "+RIGHT$("000"+DEP$,3)+" "+RIGHT$("00000000"+PREC$,8)+" "+SG$ 

! WRITE #OUTFIL;CADENA1$ 
 WRITE #5;CADENA1$
CONT%=CONT%+1

ENDIF

ENDIF
  RETURN

S2:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"     MPGROUP= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    DEALQUAN= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  PRICE METH= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    SALEQUAN= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    SALEPRIC= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    QTYORWGT= ",VAL(A$)
  GOSUB GETUNPK
  GOSUB GETFLAG
! WRITE #OUTFIL;"    INDICAT1= ",RIGHT$(A$,8)
  RETURN

S3:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"      DISGROUP= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"       DISRATE= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"      AMTDISCO= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"      TAXEXEMP= ",VAL(A$)
  RETURN

S5:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"   TENDER TYPE= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  AMT TENDERED= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    TENDER FEE= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"      CUST NUM= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"        STATUS= ",VAL(A$)
  RETURN

S7:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX A PAID = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX B PAID = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX C PAID = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX D PAID = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX A AMNT = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX B AMNT = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX C AMNT = ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" TAX D AMNT = ",VAL(A$)
  RETURN

S9:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"TEND TYPE CODE= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"    AMT CHANGE= ",VAL(A$)
  RETURN

S10:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"OVERIDE NUM= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"     REASON= ",VAL(A$)
  RETURN

S11:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"  USER DATA1= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  USER DATA2= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  USER DATA3= ",VAL(A$)
  GOSUB GETUNPK
  NREC$ = A$
! WRITE #OUTFIL;"  USER DATA4= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  USER DATA5= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  USER DATA6= ",VAL(A$)

 CADENA1$ = " "
 IF SG$="1" THEN SG$ ="-"
 IF SG$="0" THEN SG$ ="+"
 CADENA1$ = FECTRA$+" "+CODI$+"  "+HORA$+" "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("000000"+NREC$,6)+" "+RIGHT$("00000000"+PREC$,8)+" "+SG$ 

!   IF CODI$ = "888140" THEN  GOTO IMPRIME  ! CENTROAGUAS
!   IF CODI$ = "888139" THEN  GOTO IMPRIME  !
!   IF CODI$ = "888140" THEN  GOTO IMPRIME  !
! ENDIF
! IF CODI$ = "999011"  THEN CADENA1$ = FECTRA$+" "+CODI$+"  "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("000000"+NREC$,6)+" "+ RIGHT$("00000000"+PREC$,8)+" "+SG$  GOTO IMPRIME
! IF CODI$ = "999012"  THEN CADENA1$ = FECTRA$+" "+CODI$+"  "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("000000"+NREC$,6)+" "+ RIGHT$("00000000"+PREC$,8)+" "+SG$  GOTO IMPRIME
! WRITE #OUTFIL;CADENA1$

  RETURN

S12:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"     ITEM CODE= 2",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" NEW DEAL QUAN= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"     NEW PRICE= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;" OLD DEAL QUAN= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"     OLD PRICE= ",VAL(A$)
  RETURN

S13:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"  NUMBER LOANS= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  AMOUNT LOANS= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  NUMBER PKUPS= ",VAL(A$)
  GOSUB GETUNPK
! WRITE #OUTFIL;"  AMOUNT PKUPS= ",VAL(A$)
  GOSUB GETUNPK
  WHILE (VAL(A$) <> 99)
 !  WRITE #OUTFIL;"TEND TYPE CODE= ",VAL(A$)
    GOSUB GETUNPK
  ! WRITE #OUTFIL;" AMT OF TENDER= ",VAL(A$)
    GOSUB GETUNPK
  WEND
 !  WRITE #OUTFIL;"     DELIMITER= ",VAL(A$)
  WHILE (J < LEN(B$))
    GOSUB GETUNPK
  ! WRITE #OUTFIL;"TEND TYPE CODE= ",VAL(A$)
    GOSUB GETUNPK
  ! WRITE #OUTFIL;" AMT OF TENDER= ",VAL(A$)
  WEND
  RETURN

S14:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"   OLD DATA = ",MID$(B$,J,(K-J)-1)
  GOSUB GETUNPK
! WRITE #OUTFIL;"   NEW DATA = ",MID$(B$,J,(K-J)-1)
  RETURN

S15:
  J = 3
  GOSUB GETUNPK
! WRITE #OUTFIL;"      STAMPS= ",VAL(A$)
  RETURN

S16:
! WRITE #OUTFIL;"STRING OUT OF RANGE"
 RETURN


S20:
  J = 3
  GOSUB GETEXCPT
  WRITE #OUTFIL;" TERMINAL NUM= ",VAL(A$)
  GOSUB GETEXCPT
  WRITE #OUTFIL;" TRANSACT NUM= ",VAL(A$)
  GOSUB GETEXCPT
  WRITE #OUTFIL;"DATE AND TIME= ",A$
  GOSUB GETEXCPT
  WRITE #OUTFIL;" EXCEPTN CODE= ",VAL(A$)
  GOSUB GETEXCPT
  IF (LEN(A$) > 0) THEN \ ! IF OPER NUM PRESENT
    WRITE #OUTFIL;" OPERATOR NUM= ",VAL(A$) \
  ELSE \
    BEGIN
      A$ = "NONE"
      WRITE #OUTFIL;" OPERATOR NUM= ",A$
    ENDIF

  WHILE (J < LEN(B$)) ! FORMAT REST OF RECORD
    B = J ! SAVE START POSITION
    GOSUB GETEXCPT ! FIND LENGTH OF NEXT FIELD
    IF (LEN(A$) > 0) THEN \
    BEGIN
      IF J > LEN(B$) THEN \ ! IF LAST FIELD
        WRITE #OUTFIL;"EXCPT RCD FLD= ",MID$(B$,B,K-B) \
      ELSE \
        WRITE #OUTFIL;"EXCPT RCD FLD= ",MID$(B$,B,(K-B)-1)
    ENDIF \
    ELSE \
    BEGIN
      A$ = "NONE"
      WRITE #OUTFIL;"EXCPT RCD FLD= ",A$
    ENDIF
  WEND

  RETURN


S0:
  J = 3
  GOSUB GETUNPK                          ! No de Terminal
  TER% = VAL(A$)
  GOSUB GETUNPK                          ! No de Transacc
  TRA% = VAL(A$)
  IF REI% > 0 THEN TER$ = STR$(TER%)     !
  GOSUB GETUNPK                          ! Fecha y Hora
  FEHO$ = A$
  GOSUB GETUNPK                          ! Tipo Transacc
  TITRA$ = A$
  GOSUB GETUNPK                          ! Num Strimgs
  NUSTR$ = A$                             
  GOSUB GETUNPK                          ! Num Operad
  NUOPE$ = A$
  GOSUB GETUNPK                          ! Clave
  NUCLA$ = A$                               
  GOSUB GETUNPK                          ! Bruto Posit
  BRMAS$ = A$                                   
  GOSUB GETUNPK                          ! Bruto Nega
  BRMEN$ = A$                                 
  GOSUB GETUNPK                          ! Tiempo en Red
  TIRED$ = A$
  GOSUB GETUNPK                          ! Tiempo de Vta
  TIVTA$ = A$                               
  GOSUB GETUNPK                          ! Tiempo Especial
  TIESP$ = A$                                  
  GOSUB GETUNPK                          ! Tiempo Inactivo
  TINAC$ = A$                                  
  GOSUB GETUNPK                          ! Flags DE VTA
  GOSUB GETFLAG                    ! CONVERT REMAINING 16 BITS OF INDICAT1
  FOR I = 1 TO 16                  !
    FLAG = SHIFT(FLAG,1)           ! SET UP NEXT BIT
    IF (FLAG AND 00000001H) THEN A$="1"+A$ ELSE A$="0"+A$
  NEXT I
  FLAGA$ = A$

!IF VAL(TER$) > 0 AND TER% = VAL(TER$) THEN BEGIN
IF VAL(TER$) > 0 THEN BEGIN
! WRITE #OUTFIL;" "                     !
!pWRITE #OUTFIL;"*************************** INICIA NUEVA TRANSACCION ***********"
  FECTRA$ = "20"+MID$(FEHO$,1,2)+MID$(FEHO$,3,2)+MID$(FEHO$,5,2)
  HORA$   = MID$(FEHO$,7,2)+":"+MID$(FEHO$,9,2)
 ! CADENA$ = FECTRA$+"  "+RIGHT$("000000"+NUOPE$,6)+"  "+HORA$+"  "+RIGHT$("00"+STR$(TER%),2)\
 !           +"  " +RIGHT$("0000"+STR$(TRA%),4)+"  "+RIGHT$("0000000000"+BRMAS$,10)+"  "+    \
 !           RIGHT$("0000000000"+BRMEN$,10)+"  "+TITRA$  
 !            WRITE #OUTFIL;CADENA$


! WRITE #OUTFIL;"---STR TYPE = ",AA%    ! PRINT "STRING TYPE = ";A
!pWRITE #OUTFIL;"NUMTER=",TER%
!pWRITE #OUTFIL;"NUMTRA=",TRA%
!pWRITE #OUTFIL;"FECHA =",VAL(FEHO$)
! WRITE #OUTFIL;"TRANSACT TYPE= ",VAL(TITRA$)
! WRITE #OUTFIL;"NUM OF STRINGS= ",VAL(NUSTR$)
!pWRITE #OUTFIL;"CAJERO=",VAL(NUOPE$)
! WRITE #OUTFIL;"      PASSWORD= ",VAL(NUCLA$)
!pWRITE #OUTFIL;"VTAPOS=",VAL(BRMAS$)
!pWRITE #OUTFIL;"VTANEG=",VAL(BRMEN$)
! WRITE #OUTFIL;"     RING TIME= ",VAL(TIRED$)
! WRITE #OUTFIL;"   TENDER TIME= ",VAL(TIVTA$)
! WRITE #OUTFIL;"  SPECIAL TIME= ",VAL(TIESP$)
! WRITE #OUTFIL;" INACTIVE TIME= ",VAL(TINAC$)
! WRITE #OUTFIL;"INDICAT1   0-7= ",LEFT$(FLAGA$,8)
! WRITE #OUTFIL;"INDICAT1  8-15= ",MID$(FLAGA$,9,8)
! WRITE #OUTFIL;"INDICAT1 16-23= ",MID$(FLAGA$,17,8)
! WRITE #OUTFIL;"INDICAT1 24-31= ",RIGHT$(FLAGA$,8)
ENDIF ELSE BEGIN
   Q = LEN(INAREA$)
ENDIF
  RETURN
GETEXCPT:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  IF ((K-J)-1) > 0 THEN \ ! IF DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  ELSE \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BEGINNING OF NEXT FIELD
  RETURN


S21:
  J = 3
  GOSUB GETUNPK
  WRITE #OUTFIL;" DATE AND TIME= ",A$
  GOSUB GETUNPK
! WRITE #OUTFIL;"      INDICAT1= ",VAL(A$)
  GOSUB GETUNPK
  GOSUB GETFLAG
! WRITE #OUTFIL;"      INDICAT2= ",RIGHT$(A$,8)
  RETURN

S99:
  WRITE #OUTFIL;"     USER DATA= ",MID$(B$,3,LEN(B$)-4)
  RETURN

!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J)                       ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J))              ! UNPACK FIELD
  J=K+1                                     ! POINT TO BEGINNING OF NEXT FIELD
  RETURN
!*********************************************************************

!*********************************************************************
GETFLAG:
  FLAG = VAL(A$) ! CONVERT FLAG TO INTEGER
                 ! START BUILDING STRING TO CONTAIN INDIVIDUAL FLAGS
  IF (FLAG AND 00000001H) THEN A$ = "1" ELSE A$ = "0"
  FOR I = 1 TO 15
    FLAG = SHIFT(FLAG,1) ! SET UP NEXT BIT
    IF (FLAG AND 00000001H) THEN A$ = "1" + A$ ELSE A$ = "0" + A$
  NEXT I
  RETURN
!*************************************************************
  ERRTN:
	NEXT n
  ERRORCOD$ = ERR
  IF ERR <> "EF" AND                \
     ERRF% = 25 THEN BEGIN            ! Si NO es END OF FILE Sess#25
     CONSOLE
     CLEARS
     PRINT
     PRINT "VERIFIQUE ARCHIVO ASIGNADO ..!"
     PRINT "DATOS INVALIDOS DESDE TSLOG "
     PRINT"  "
     LOCATE 14,20
     IF CONT%=0 THEN BEGIN
	PRINT "NO SE ENCONTRARON REGISTROS"
     ENDIF
     IF CONT%>0 THEN BEGIN
	PRINT "REGISTROS ENCONTRADOS: ",CONT%
!	MODO LPT1=COM1
!	LPRINT FILE1$
     ENDIF
     STOP
  ENDIF    


  IF ERR = "OE" THEN BEGIN           ! Si es END OF FILE Sess#25
!        PRINT "     NORMAL EOJ..."
!        PRINT "FIN DEL PROCESO NORMAL..!"
	RESUME
	 RETURN
        STOP
  ENDIF    


!**************************************************************
SALIDA:

CLOSE TLOG
CLOSE OUTFIL
CLOSE 5
!IF CONT%=0 THEN PRINT "NO SE ENCONTRARON REGISTROS..."
!ENDIF  
!NEXT n
!WEND ! FIN DE LA LECTURA DEL ARCHIVO FPLANO.PAS
!CLOSE 5 
STOP

ERRORES:
	IF ERRORP$ = "OE" THEN RESUME
!	   ERRORP$ = ERR
!	   	IF ERRORP$ = "EF" THEN PRINT "Fin de archivo ..." \
!	   	ELSE PRINT "Error ";ERRN;" ";ERR;" archivo: ";ERRF%
!	   	RESUME
!		ENDIF
	   	IF ERRORP$ = "NF" THEN RESUME
!		ENDIF 
!	ENDIF

END

