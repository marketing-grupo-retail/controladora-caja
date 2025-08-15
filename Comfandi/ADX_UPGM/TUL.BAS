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
  INTEGER*1 A,B,I,J,K,TLOG,OUTFIL,NREG%
  STRING INAREA$,A$,B$,PIBO$,FECTRA$,HORA$,CADENA$,MES$,FILE$,DIA$
  STRING PREC$,CODI$,NREC$,CADENA1$,SG$,DEP$
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

  PIBO% = 0

      IF TRAN$ = "EAMTRANC" AND PIBO% = 0 THEN BEGIN 
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

WHILE NREG% < = 31
DIA$ = RIGHT$("00"+STR$(NREG%),2)
LOCATE 17,10
PRINT " ษอออออออออออออออออออออออออออออออออหออออออออออออออออออป  "
LOCATE 18,10
PRINT " บ  REGISTROS LEIDOS Y PROCESADOS  บ                  บ   "
LOCATE 19,10
PRINT " ศอออออออออออออออออออออออออออออออออสออออออออออออออออออผ   "

LOCATE 18,55
PRINT " ";NREG%
    B$ = "C:\ADX_UDT1\EAMT"+C$+DIA$+".DAT"   ! CAMBIO PASCUAL 
   REI% = VAL(NUR$)
   IF VAL(TER$) > 0 THEN  REI% = 0

LOCATE 22,20
   PRINT "PROCESSING ";B$
   PRINT
  TLOG = 25:OUTFIL = 26
!................................................................

  FILE$ = "C:\DATOS\ARTICULO.DAT"   ! CAMBIO PASCUAL 
  CREATE FILE$  AS OUTFIL BUFFSIZE 5120 LOCKED
  WRITE #OUTFIL; "TRANSACTION LOG = ",B$
  WRITE #OUTFIL; " FECHA   HORA TER TRA  COD     OPE    DEP  VALOR   SG"
  OPEN B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL

 
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
    ENDIF
    IF A = 0 THEN GOSUB S0:GOTO AGAIN
    IF A = 99 THEN GOSUB S99:GOTO AGAIN
    ON A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
AGAIN:
  WEND
  GOTO NXTRCD

IMPRIME:
WRITE #OUTFIL;CADENA1$
RETURN


S1:
  J = 3
  GOSUB GETUNPK

  CODI$ = A$
  GOSUB GETUNPK
  PREC$ = A$
  GOSUB GETUNPK

 DEP$=A$
  IF  VAL(DEP$) > 0  \  ! CRNE DE RES Y CERDO 0% 
       THEN BEGIN  
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETFLAG
  GOSUB GETUNPK
  GOSUB GETFLAG
  SG$ =MID$(RIGHT$(A$,8),1,1)
  GOSUB GETUNPK

  CADENA1$ = " " 

 CADENA1$ = FECTRA$+" "+HORA$+" "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("0000000"+CODI$,7)+" "+ RIGHT$("000000"+NUOPE$,6)\ 
            +" "+RIGHT$("000"+DEP$,3)+" "+RIGHT$("00000000"+PREC$,8)+" "+SG$ 

 WRITE #OUTFIL;CADENA1$

ENDIF
  RETURN

S2:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETFLAG
  RETURN

S3:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S5:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S7:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S9:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S10:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S11:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  NREC$ = A$
  GOSUB GETUNPK
  GOSUB GETUNPK

 CADENA1$ = " "
 IF SG$="1" THEN SG$ ="-"
 IF SG$="0" THEN SG$ ="+"
 CADENA1$ = FECTRA$+" "+CODI$+"  "+HORA$+" "+RIGHT$("00"+STR$(TER%),2)+" "+ RIGHT$("0000"+STR$(TRA%),4)+" "+RIGHT$("000000"+NREC$,6)+" "+RIGHT$("00000000"+PREC$,8)+" "+SG$ 

  RETURN

S12:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S13:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  GOSUB GETUNPK
  WHILE (VAL(A$) <> 99)
    GOSUB GETUNPK
    GOSUB GETUNPK
  WEND
  WHILE (J < LEN(B$))
    GOSUB GETUNPK
    GOSUB GETUNPK
  WEND
  RETURN

S14:
  J = 3
  GOSUB GETUNPK
  GOSUB GETUNPK
  RETURN

S15:
  J = 3
  GOSUB GETUNPK
  RETURN

S16:
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

IF VAL(TER$) > 0 THEN BEGIN
  FECTRA$ = "20"+MID$(FEHO$,1,2)+MID$(FEHO$,3,2)+MID$(FEHO$,5,2)
  HORA$   = MID$(FEHO$,7,2)+":"+MID$(FEHO$,9,2)
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
  GOSUB GETUNPK
  GOSUB GETFLAG
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
  ERRORCOD$ = ERR
  IF ERR <> "EF" AND                \
     ERRF% = 25 THEN BEGIN            ! Si NO es END OF FILE Sess#25
     CONSOLE
     PRINT
     PRINT "VERIFIQUE ARCHIVO ASIGNADO ..!"
     PRINT "DATOS INVALIDOS DESDE TSLOG "
     PRINT"  "
     STOP
  ENDIF  


!**************************************************************
SALIDA:

CLOSE TLOG
CLOSE OUTFIL

WEND ! FIN DE LA LECTURA DEL ARCHIVO FPLANO.PAS
!CLOSE 5

STOP
ERRORES:
	IF ERRF% = 1 THEN BEGIN
	   ERRORP$ = ERR
	   IF ERRORP$ = "EF" THEN PRINT "Fin de archivo ..." \
	   ELSE PRINT "Error ";ERRN;" ";ERR;" archivo: ";ERRF%
	   RESUME
	ENDIF 

END

