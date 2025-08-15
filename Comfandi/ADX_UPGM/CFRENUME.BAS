!-------------------------------------------------------
!    PROYECTO              : COMFAMILIAR ANDI 
!    Nombre del Fuente     : CFRENUME.BAS
!    Fecha de Modificaci¢n : Abril 9 de 1996
!    Archivos que utiliza  : C:\ADX_IDT4\EAMTRANx.DAT
!                                   ( x = A, B, ¢ C )
!-------------------------------------------------------
!    Renumera No. de Transaccion.
!-------------------------------------------------------

  INTEGER*1                                                   \
            A,                B,           C,                 \
            D,                E,           F,                 \
            N,                I,           J,                 \
            K,                L,           TLOG,              \
            OUTFIL1,          OUTFIL2,     LONG,              \
            FTV,              FT,          CERO

  INTEGER*2                   ANCHO%,                         \
            P,                Q,           CONSE,             \
            POS%,             TER%(1),     T%

  INTEGER*4 FLAGW,            INDIC1,      NREC%,             \
            FLAG,             VALWRK,      VALOR,             \
            CHG,              NRO,         DEPI,              \
            NUEVON
  STRING                                                      \ 
            A$,               B$,          H$,                \
            FEC1$,            NUAL$,       NOAL$,             \
            ALMA$,            FINR$,       BB$,               \
            RESP$,            ALMACEN$,    TITULO$
	
!
!***************************************************************
!
  FUNCTION ERRNSTR$(ERRNUM)
    INTEGER*1 I
    INTEGER*4 ERRNUM,WORK
    STRING HEX$,ERRNSTR$,WORK$
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
    FOR I=1 TO 8
      WORK=ERRNUM AND 0000000FH         ! AND OFF ALL BUT LOW NYBBLE
      WORK$=MID$(HEX$,WORK+1,1)+WORK$   ! ADD HEX VALUE TO OUT STRING
      ERRNUM=SHIFT(ERRNUM,4)            ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$
  FEND


SUB PREGUTOF
  J = 18
  FOR I = 1 TO 9
      POS% = MATCH(":",INAREA$,J)
      J = POS% + 1
  NEXT I
  FINSTR$ = CHR$(34)+","+CHR$(34)
  J = MATCH(FINSTR$,INAREA$,POS%)
  ANCHO% = J - POS% - 1
  FLAGW = VAL(UNPACK$(MID$(INAREA$,POS%+1,ANCHO%)))
  FLAG = FLAGW
END SUB
! -------------  Rutina para crear compaginar Hora y Fecha --------------
  SUB CREAFECHA
      FRA.TIM$ = TIME$                              ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                  ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                 ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                 ! Arma segundos
      FRA.FEC$ = DATE$                              ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                  ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                 ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                 ! Arma dia
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    El: "+FRA.DD$+"/"+FRA.MM$+"/"+ \! de Franqueo con
               FRA.AA$                              ! Hora y Fecha
  END SUB
!------------------------------------------------------------
!----            PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------
ON ERROR GOTO ERRTN                                 !
TITULO$ = "      ** RENUMERA No. DE TRANSACCION DE TSLOG **"
CALL CREAFECHA
FEC1$ = FRA.LIN$
FINR$ = CHR$(13)+CHR$(10)  
EMPEZAR:
CLEARS
PRINT TITULO$
PRINT
PRINT                                               ! Avanza Linea
PRINT                                               ! Avanza Linea
PRINT "Indique  A,  B,  C  del TSLOG a RENUMERAR " 
PRINT "NOTA: La Salida queda en el Archivo"
INPUT "                C:\EAMTLOG.DAT     " ; B$ 
PRINT
PRINT "Indique No.de Transacci¢n para RENUMERAR el"
INPUT "nuevo Transaction LOG :                    " ; BB$ 
CONSOLE
PRINT                                               ! Avanza Linea
PRINT "    Por Favor Esperar..."                    !
PRINT " "  : PRINT
PRINT "      CFRENUME.286 Renumerando archivo TSLOG "
DIM TER%(1200)
GETNAME:
B$="C:\ADX_IDT4\EAMTRAN" + B$ + ".DAT"              ! TSLOG Previo
  HORA$=TIME$                                       ! Toma la hora
  HORA$=LEFT$(HORA$,2)+":"+MID$(HORA$,3,2)+      \  ! Formato Hora
                       ":"+RIGHT$(HORA$,2)          !
  PRINT                                             !
  PRINT "Procesando ";B$;" ";HORA$                  ! 
  PRINT                                             !
  TLOG    = 25  :  OUTFIL1 = 26   
CREATE "Q:\EAMTLOG.DAT" AS OUTFIL1 BUFFSIZE 5120    ! Creaci¢n de
OPEN B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL         ! 
B$ = ""                                             !

NXTRCD:                                             !
  Q    = 1                                          !
  READ #TLOG; LINE INAREA$                          !
  A  =  VAL(UNPACK$(MID$(INAREA$,2,1)))             ! Determina String
  IF A <> 0 THEN GOTO AGAIN
  TITRA =  VAL(UNPACK$(MID$(INAREA$,16,1)))         ! Determina TipTran
  IF TITRA = 18 THEN GOTO NXTRCD
  IF TITRA = 0 THEN BEGIN
     CALL PREGUTOF                                     ! STR=00 Transac en TOF?
     IF FLAG <> 0 THEN BEGIN                           ! Si hay Indicat1
        GOSUB GETFLAG                                  ! Verifica Bit 31
        IF MID$(A$,2,1) = "1" THEN BEGIN               ! Si hubo TOF
           NREC% = NREC% + 1                           !
           FLAG = FLAGW AND 00000000H                  ! Deshab Flag Bit 31
           NUEVOI1$ = PACK$(RIGHT$("0000000000"+      \! Deja el Indicativo 
                      STR$(FLAG),ANCHO%*2))            ! de la misma long
           INAREA$ = LEFT$(INAREA$,POS%) + NUEVOI1$ + \! de antes
                     RIGHT$(INAREA$,LEN(INAREA$)-J+1)  !
        ENDIF                                          !
     ENDIF                                             !
  ENDIF
IF A = 0 THEN GOTO S0                               ! Si es Header 00
                                                    ! se procesa

AGAIN:                                              !
    SALIDA$ = MID$(INAREA$,2,LEN(INAREA$)-2)        ! Nueva String Renumerada 
    IF CERO = 1 THEN BEGIN                          ! Hay para Grabar ?
       CERO = 0                                     ! Restaura Condic
       SALIDA$=LEFT$(SALIDA$,5)+REMPLA$+           \! Arma nueva String
               RIGHT$(SALIDA$,LEN(SALIDA$)-7)       ! con nuevo Numero
    ENDIF                                           ! Fin de Condic
    WRITE #OUTFIL1;SALIDA$                          ! Graba el TSLOG

GOTO NXTRCD                                         ! Cont Sgte Reg

S0:                                                 ! String de Encabezado
  CERO = 1                                          ! Indica que va a RENUM
  A$ = UNPACK$(MID$(INAREA$,7,2))                   ! Obtiene No.Trans Actual
  T% = VAL(UNPACK$(MID$(INAREA$,4,2)))              ! Obtiene No. Terminal
  IF FT = 0 THEN BEGIN                              ! Es 1er Vez ?
     FT = 1                                         ! Marca 1er Vez
     AA$ = A$                                       ! Guarda No. de Terminal
  ENDIF                                             ! Fin de 1er vez
  LOCATE 21,10                                      ! Ubica Cursor
  PRINT "PROC TERM:"+STR$(T%)+" TRAN # ";A$;"  NUEVO ";AA$                 ! Muestra nuevo Numero
  IF T% > 9000 THEN GOTO NXTRCD

  TER%(T%) = TER%(T%) + 1                           ! Increm Num/Tran por Term
  NUEVON = TER%(T%)                                 ! No. Definitivo
  REMPLA$ = RIGHT$("0000"+STR$(NUEVON),4)           ! Arma String del Numero
  REMPLA$ = PACK$(REMPLA$)                          ! Empaqueta el Numero
  AA$ = STR$(NUEVON)                                ! Salva el Nuevo Numero
  LOCATE 20,10                                      ! Ubica Cursor
  PRINT "TERM:"+STR$(T%)+" TRAN # ";A$;"  NUEVO ";AA$                 ! Muestra nuevo Numero
  IF A$ = AA$ THEN BEGIN
     CERO = 0
  ENDIF
  GOTO AGAIN                                         ! Retorna

GETFLAG:                                             ! START BUILDING STRING 
                                                     ! TO CONTAIN INDIVID FLAGS
  IF (FLAG AND 0000000000000001H) THEN              \!
     A$ = "1" ELSE A$ = "0"                          !
  FOR I = 1 TO 31                                    !
      FLAG = SHIFT(FLAG,1)                           ! SET UP NEXT BIT
      IF (FLAG AND 0000000000000001H) THEN          \!
         A$ = "1" + A$ ELSE A$ = "0" + A$            !
  NEXT I                                             !
RETURN                                               !
!*********************************************************************
  ERRTN:
  If ERR = "IH" Then Resume
!  IF ERR = "SS" THEN RESUME
!  IF ERR = "RN" THEN RESUME 
  IF ERR = "EF" THEN  BEGIN                       ! Si NO es EOF
    HORA$=TIME$                                    !
    HORA$=LEFT$(HORA$,2)+":"+                    \ !
          MID$(HORA$,3,2)+":"+RIGHT$(HORA$,2)      !
    PRINT " FIN NORMAL DEL PROCESO..  ";HORA$
    PRINT "Transacciones en TOF.. ";NREC%
    STOP 
  ENDIF

  IF (ERR = "OE") OR (ERR = "ME") THEN BEGIN           ! SI ERROR OPEN/CREATE 
      IF ERRF% = 26 THEN                             \ ! IF OUTPUT FILE
        PRINT "ERROR EN APERTURA DE ARCHIVO...(OUT)" \ !
      ELSE                                           \ !
        PRINT "ERROR EN APERTURA DE ARCHIVO...(IN)"    !
  ENDIF                                              \ !
  ELSE                                               \ ! NO ES ERROR DE APERTURA
    PRINT "       OTRO  ERROR ..!"
  PRINT
  PRINT " ERR = ";ERR
  PRINT "ERRN = ";ERRNSTR$(ERRN)
  PRINT
  PRINT "EL PROGRAMA FINALIZA..."
  STOP
!*********************************************************************
