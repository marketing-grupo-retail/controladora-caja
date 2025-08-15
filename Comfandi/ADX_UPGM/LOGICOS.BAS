\/* TIME STAMP BLOCK ************************************************
\* END OF TIME STAMP BLOCK *****************************************/
!*******************************************************************!
! TITLE: Electronic Marketing Add Logical Names Program            *!
!                                                                  *!
!   COPYRIGHT: 5799-PNH THIS MODULE IS "RESTRICTED MATERIALS OF    *!
!              IBM" (c) COPYRIGHT IBM CORP 1991 ALL RIGHTS         *!
!              RESERVED LICENSED MATERIALS - PROPERTY OF IBM       *!
!                                                                  *!
!              REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER         *!
!              G120-2083                                           *!
!                                                                  *!
!*******************************************************************!
!                     LOGNAMES
!
!    THIS CODE MOVES LOGICAL NAMES FROM A FILE NAMED LOGNAMES.DAT
!    ON ADX_IPGM TO THE USER LOGICAL NAMES FILES ON CC AND DD.
!
\/*                                                                 */
\/* CHANGE ACTIVITY                                                 */
\/*                                                                 */
\/* IR23604 - On a LAN system, the logical names definitions for    */
\/*           the master controller are not being cleared prior to  */
\/*           processing the logical names for the alternate.       */
\/*           BAH  MGV-A  24Aug93                                   */
\/*                                                                 */
!*******************************************************************
    INTEGER I%, J%, K%, REPLACEMENT, NEED.REPLACE
    INTEGER*4 ADDED, X%
    STRING IN.NAME1$, IN.NAME2$, OUT.NAME1$, OUT.NAME2$
    STRING CID$, MCID$, X$, LFN$, CRLF$, A$, B$
    STRING OLD.NAMES$, OLD.DATA$, IN.PARM$
    STRING MSG$(1), COMPLETE.MESSAGE$

 %INCLUDE EAMXXCPY.J86              ! Copyright notice

    SUB ADXSERVE(RETC,FUNC,PARM1,PARM2$) EXTERNAL
        INTEGER*4 RETC
        INTEGER*2 FUNC,PARM1
        STRING    PARM2$
    END SUB

%INCLUDE EAMASMCT.J86 ! ASSEMBLER LANGUAGE SUBROUTINES

FUNCTION ERRNHEX$                      ! Error logging routine
 STRING ERRNHEX$,A$
 A$ = "1234"
 CALL PUTN4(A$,0,ERRN)
 ERRNHEX$ = MID$(A$,4,1) + MID$(A$,3,1) + MID$(A$,2,1) + MID$(A$,1,1)
END FUNCTION

SUB SUBEXSTR(SUBVAR$,IN$,REPLACE$)
    STRING SUBVAR$, IN$, REPLACE$
    INTEGER I%, J%
    I% = MATCH(SUBVAR$,IN$,1)                 ! LOCATE START
    J% = LEN(IN$)
    IF I% > 0 THEN BEGIN
      IN$ = LEFT$(IN$,I%-1) + REPLACE$ + RIGHT$(IN$,J%-I%-1) !REPLACE VARIABLE
    ENDIF
END SUB

!!********************************************************************
!!**  PERFORM INITIALIZATION AND COMPUTE OUTPUT FILE NAME           **
!!********************************************************************
    ON ERROR GOTO ERROR.ROUTINE
    OPEN "PRN:" AS 11                        ! OPEN PRINTER
    DIM MSG$(26)
    OPEN "EAMEDESC" RECL 49 AS 61 NOWRITE NODEL
    READ #61, 1112;COMPLETE.MESSAGE$
    FOR I% = 1 TO 19
      READ #61, 1112+I%; MSG$(I%)                  ! READ IN DESCRIPTORS
    NEXT I%
    READ #61, 1110;MSG$(20)                     ! "  ERROR = "
    READ #61, 1111;MSG$(21)                     ! "  LINE =  "
    READ #61, 1109;MSG$(22)                     ! "INPUT FILE ...NOT FOUND"
    READ #61, 1138;MSG$(23)                     ! READ IN DESCRIPTORS
    READ #61, 1139;MSG$(24)                     ! READ IN DESCRIPTORS
    READ #61, 1140;MSG$(25)                     ! READ IN DESCRIPTORS
    READ #61, 1141;MSG$(26)                     ! READ IN DESCRIPTORS
    CLOSE 61
    CRLF$ = CHR$(13) + CHR$(10)              ! CRLF
    IN.PARM$ = UCASE$(COMMAND$)              ! GET INPUT
    REPLACEMENT = MATCH("REPLACE",IN.PARM$,1)
    IF IN.PARM$ NE "" THEN BEGIN
       IF REPLACEMENT = 0 THEN BEGIN
          A$ = " 'REPLACE' IS THE ONLY VALID PARAMETER     "
          A$ = MSG$(25) + MSG$(26)
          WRITE FORM "C70,C2"; #11; A$,CRLF$
       ENDIF
    ENDIF
    ADDED = 0
    X$ = STRING$(128,CHR$(0))                ! SET UP FOR ADXSERVE
    CALL ADXSERVE(X%,4,0,X$)                 ! GET STATUS DATA
    CID$ = MID$(X$,14,2)                     ! SAVE CONTROLLER ID
    MCID$ = MID$(X$,18,2)                    ! SAVE MASTER ID
!   A$ = " THIS CONTROLLER = " + CID$        \
!      + " MASTER CONTROLLER = " + MCID$     !
    A$ = MSG$(1) + MSG$(2)
    CALL SUBEXSTR("%1",A$,CID$)              ! REPLACE VARIABLE
    CALL SUBEXSTR("%2",A$,MCID$)             ! REPLACE VARIABLE
    WRITE FORM "C70,C2"; #11; A$,CRLF$
    IF MCID$ < "CC" THEN MCID$ = "CC"        !
    IF CID$ < "CC" THEN CID$ = "CC"          !
!   A$ = "THE LOGNAMES PROGRAM SHOULD BE RUN ON THE MASTER CONTROLLER"
    A$ = MSG$(3) + MSG$(4)
    WRITE FORM "C70,C2"; #11; A$,CRLF$
!   A$ = "AND ON ALL CONTROLLERS OTHER THAN DD  "
    A$ = MSG$(23) + MSG$(24)
    WRITE FORM "C70,C2"; #11; A$,CRLF$
    LFN$ = "ADXLXAAN::ADX_SPGM:ADXDE" + CID$ \ DEVELOP OUTPUT FILE NAME
        + "F.DAT"                            !
REPEAT.DD:
!   A$ = " ADD LOGICAL NAMES TO " + LFN$
    IF REPLACEMENT THEN BEGIN
!      A$ = A$ + " WITH REPLACEMENT "
       A$ = MSG$(6) + MSG$(7)
    ENDIF ELSE BEGIN
       A$ = MSG$(5)
    ENDIF
    CALL SUBEXSTR("%1",A$,LFN$)              ! REPLACE VARIABLE
    WRITE FORM "C70,C2"; #11; A$,CRLF$

!!********************************************************************
!!**  OPEN INPUT AND OUTPUT FILES                                   **
!!********************************************************************
    IF END #61 THEN NO.FILE1                 ! PREPARE FOR NO FILE
    OPEN LFN$ AS 61 NOWRITE NODEL            ! OPEN OUTPUT FILE TO READ
    IF END #62 THEN NO.FILEC                 ! PREPARE FOR NO FILE
    Print "APERTURA LOGICOS.DAT"
    OPEN "C:\ADX_UDT1\LOGICOS.DAT" AS 62    \ OPEN INPUT FILE
         NOWRITE NODEL
    GOTO FOUND.ON.C
NO.FILEC:
     IF END #62 THEN NO.FILE2                 ! PREPARE FOR NO FILE
     OPEN "A:LOGNAMES.DAT" AS 62              \ OPEN INPUT FILE
          NOWRITE NODEL
FOUND.ON.C:

!!********************************************************************
!!**  CAPTURE ALL PREVIOUSLY DEFINED LOGICAL NAMES                  **
!!********************************************************************
    OLD.NAMES$ = ""                          ! CLEAR PRIOR OLDNAMES
!AIR23604
    OLD.DATA$ = ""                           ! CLEAR PRIOR OLD DATA
!EIR23604
    WHILE 1 = 1
      IF END #61 THEN DONE.OLDFILE           ! END LOOP AT EOF
      READ FORM "C138"; # 61; X$             ! READ OLD NAME
      OLD.NAMES$ = OLD.NAMES$ + LEFT$(X$,10) ! SAVE OLD NAME
      B$ = MID$(X$,11,40)                    ! GET OLD DATA
      B$ = TRANSLATE$(B$,CHR$(0)," ")        ! TRANSLATE X"00" TO X"20"
      OLD.DATA$  = OLD.DATA$  + B$           ! SAVE OLD DATA
    WEND
DONE.OLDFILE:

    CLOSE 61
    OPEN LFN$ AS 61 APPEND                   ! OPEN OUTPUT FILE TO WRITE
    IF REPLACEMENT THEN \
      OPEN LFN$ DIRECT RECL 138 AS 63        \ OPEN OUTPUT FILE TO WRITE
                BUFFSIZE 256

!!********************************************************************
!!**  READ NEW NAMES AND ADD TO FILE IF NOT ALREADY DEFINED         **
!!********************************************************************
    X$ = STRING$(128,CHR$(0))                ! SET UP STRING OUTPUT
    WHILE 1 = 1
      IF END #62 THEN DONE.INFILE            ! END LOOP AT EOF
REREAD:
      READ # 62; IN.NAME1$                   ! READ Input
      Print "PROCESANDO LOGICO "+IN.NAME1$
      IF LEN(IN.NAME1$) > 9 OR               \ BYPASS BAD DATA
         LEN(IN.NAME1$) < 3 THEN GOTO REREAD ! BYPASS COMMENTS
      IF ASC(IN.NAME1$) < 30H THEN GOTO REREAD ! BYPASS COMMENTS
      READ # 62; IN.NAME2$                   ! READ REST OF INPUT
      IF LEN(IN.NAME2$) > 50 OR              \ BYPASS BAD DATA
         LEN(IN.NAME2$) < 8 THEN GOTO REREAD ! BYPASS COMMENTS

      OUT.NAME1$ = LEFT$(IN.NAME1$ + X$, 10) ! FORMAT OUTPUT
      OUT.NAME2$ = LEFT$(IN.NAME2$ + X$, 128)
      IN.NAME1$  = LEFT$(IN.NAME1$ + "      ",8) ! FORMAT FOR PRINT

      I% = MATCH(OUT.NAME1$,OLD.NAMES$,1)    ! CHECK FOR PRESENT
      IF I% = 0 THEN BEGIN                   ! NOT PRESENT
        WRITE FORM "C138"; # 61; OUT.NAME1$ + OUT.NAME2$ ! ADD NAME
!       A$ = IN.NAME1$ + " ADDED AS " + IN.NAME2$
        A$ = MSG$(8)
        CALL SUBEXSTR("%1",A$,IN.NAME1$)         ! REPLACE VARIABLE
        CALL SUBEXSTR("%2",A$,IN.NAME2$)         ! REPLACE VARIABLE
        WRITE FORM "C70,C2"; #11; A$,CRLF$
      ENDIF ELSE BEGIN
        I% = I% / 10
        J% = (I% * 40) + 1
        B$ = MID$(OLD.DATA$,J%,40)
        IF IN.NAME2$ NE B$ THEN BEGIN
          IF REPLACEMENT THEN BEGIN
            J% = I% + 1
            WRITE FORM "C138"; # 63, J%; OUT.NAME1$ + OUT.NAME2$
!           A$ = IN.NAME1$ + " ALREADY PRESENT, REPLACED WITH NEW NAME"
            A$ = MSG$(9) + MSG$(10)
            CALL SUBEXSTR("%1",A$,IN.NAME1$)         ! REPLACE VARIABLE
            WRITE FORM "C70,C2"; #11; A$,CRLF$
          ENDIF ELSE BEGIN
!           A$ = IN.NAME1$ + " ALREADY PRESENT, LEFT AS IS"
            A$ = MSG$(11)
            CALL SUBEXSTR("%1",A$,IN.NAME1$)         ! REPLACE VARIABLE
            WRITE FORM "C70,C2"; #11; A$,CRLF$
            NEED.REPLACE = -1
          ENDIF

!         A$ = "  OLD NAME: " + B$
          A$ = MSG$(12)
          CALL SUBEXSTR("%1",A$,B$)               ! REPLACE VARIABLE
          WRITE FORM "C70,C2"; #11; A$,CRLF$
!         A$ = "  NEW NAME: " + IN.NAME2$
          A$ = MSG$(13)
          CALL SUBEXSTR("%1",A$,IN.NAME2$)         ! REPLACE VARIABLE
          WRITE FORM "C70,C2"; #11; A$,CRLF$
        ENDIF ELSE BEGIN
!         A$ = IN.NAME1$ + " ALREADY PRESENT, LEFT AS IS"
          A$ = MSG$(11)
          CALL SUBEXSTR("%1",A$,IN.NAME1$)         ! REPLACE VARIABLE
          WRITE FORM "C70,C2"; #11; A$,CRLF$
        ENDIF
      ENDIF
    WEND
DONE.INFILE:
    IF NEED.REPLACE THEN BEGIN
!     A$ = "*** SPECIFY ""LOGNAMES REPLACE"" TO REPLACE MISMATCHING NAMES ***"
      A$ = MSG$(14) + MSG$(15)
      WRITE FORM "C70,C2"; #11; A$,CRLF$
    ENDIF
    A$ = " "
    WRITE FORM "C70,C2"; #11; A$,CRLF$

!!********************************************************************
!!**  CLOSE FILES , SHOW FINAL STATUS, AND END                      **
!!********************************************************************
    CLOSE 61, 62
    IF REPLACEMENT THEN CLOSE 63
    IF CID$ = MCID$ THEN BEGIN              ! ON MASTER
      X$ = "ADXLXAAN::ADX_SPGM:ADXDEDDF.DAT" ! DEVELOP OUTPUT FILE NAME
      IF MCID$ = "DD" THEN                  \ MASTER IS DD
        X$ = "ADXLXAAN::ADX_SPGM:ADXDECCF.DAT" ! DEVELOP OUTPUT FILE NAME
      IF X$ NE LFN$ THEN BEGIN              ! FIRST PASS
        LFN$ = X$                           ! SET FOR DD
        GOTO REPEAT.DD                      ! REPEAT FOR DD CONTROLLER
      ENDIF
    ENDIF ELSE BEGIN                        ! NOT ON MASTER
      X$ = "ADXLXAAN::ADX_SPGM:ADXDE" + MCID$ \ DEVELOP OUTPUT FILE NAME
         + "F.DAT"                            !
      IF X$ NE LFN$ THEN BEGIN              ! FIRST PASS
        LFN$ = X$                           ! SET FOR MASTER
        GOTO REPEAT.DD                      ! REPEAT FOR MASTER
      ENDIF
    ENDIF                                   ! NOT ON MASTER
!   A$ = "ALL INPUT SUCCESSFULLY PROCESSED"
    A$ = COMPLETE.MESSAGE$
    WRITE FORM "C70,C2"; #11; A$,CRLF$
    WRITE FORM "C1"; #11; CHR$(12)          ! EJECT PAGE
    STOP

!!********************************************************************
!!**  ERROR ROUTINES                                                **
!!********************************************************************
ERROR.ROUTINE:
     IF ERRN = 80210002H THEN GOTO NO.FILE2
!    A$ = "ERROR = " +ERR +" " +STR$(ERRF%) +" " +UNPACK$(ERRNHEX$)
     A$ = MSG$(20)   +ERR +" " +STR$(ERRF%) +" " +UNPACK$(ERRNHEX$)
     WRITE FORM "C70,C2"; #11; A$,CRLF$
     WRITE FORM "C1"; #11; CHR$(12)          ! EJECT PAGE
     STOP
NO.FILE1:
!    A$ = "THE LOGICAL NAME FILE " + LFN$ + " WAS NOT FOUND."
     A$ = MSG$(16) + MSG$(17)
     CALL SUBEXSTR("%1",A$,LFN$)              ! REPLACE VARIABLE
     WRITE FORM "C70,C2"; #11; A$,CRLF$
     IF K% < 2 THEN BEGIN                    ! NOT HERE BEFORE
       K% = K% + 1
       CREATE POSFILE LFN$ AS 59 COMPOUND ATCLOSE
       CLOSE 59
!      A$ = "CREATE LOGICAL NAME FILE " + LFN$ + " AND RETRY"
       A$ = MSG$(18) + MSG$(19)
       CALL SUBEXSTR("%1",A$,LFN$)              ! REPLACE VARIABLE
       WRITE FORM "C70,C2"; #11; A$,CRLF$
       GOTO REPEAT.DD                        ! REPEAT FOR MASTER
     ENDIF                                   ! NOT HERE BEFORE
     WRITE FORM "C1"; #11; CHR$(12)          ! EJECT PAGE
     STOP
NO.FILE2:
!    A$ = "THE INPUT FILE C:\ADX_IPGM\LOGNAMES.DAT WAS NOT FOUND."
     A$ = MSG$(22)
     CALL SUBEXSTR("%1",A$,"LOGNAMES")       ! REPLACE VARIABLE
     WRITE FORM "C70,C2"; #11; A$,CRLF$
     WRITE FORM "C1"; #11; CHR$(12)          ! EJECT PAGE
     STOP

