\/* TIME STAMP BLOCK ************************************************
\*LIB   TT22 TPRCBAS  B86      07/06/90 14:50:20 LEVERETT *    *                
\** END OF TIME STAMP BLOCK ****************************************/
!*****************************************************************************
!*                                                                           *
!*  Sample CPIC File Requester Program TPRCBAS.BAS for 4680 Operating System *
!*                                                                           *
!*  This sample file requester transaction program allocates a conversation  *
!*  with file server program TPSCBAS to transfer the file name indicated as  *
!*  a command line argument.  It can be invoked from command mode by         *
!*  issuing  TPRCBAS <requested_filename>.  The following CPIC calls are     *
!*  issued in this program :                                                 *
!*                                                                           *
!*                      CMINIT (Initialize_Conversation)                     *
!*                      CMSSL  (Set_Sync_Level)                              *
!*                      CMALLC (Allocate_Conversation)                       *
!*                      CMSEND (Send_Data)                                   *
!*                      CMCFM  (Confirm)                                     *
!*                      CMRCV  (Receive_Data)                                *
!*                      CMCFMD (Confirmed)                                   *
!*                                                                           *
!*  A Symbolic Destination Name of TPRCBAS must be configured in 4680 Ctlr   *
!*  Communications Configuration specifying Local LU, Partner LU, MODE and   *
!*  Partner TP Name.                                                         *
!*                                                                           *
!*  The compiled source code was linked with the 4680 CPIC "CBASIC" library  *
!*  ADXHSV0L.L86 shipped on the Optional Programs Diskette.                  *
!*                                                                           *
!*  (C) COPYRIGHT IBM CORP. 1990                                             *
!*  LISCENSED MATERIAL - PROGRAM PROPERTY OF IBM                             *
!*  ALL RIGHTS RESERVED                                                      *
!*                                                                           *
!*****************************************************************************
!
!
%INCLUDE CPIC_BAS.DEF                           ! Include Pseudonym Definitions
!
!
! Program Declarations
!
INTEGER*2 TRUE
INTEGER*2 FALSE
!
STRING BUFFER
INTEGER*2 SYS.ERR
STRING FILE.NAME
INTEGER*2 FILE.NAME.LEN
STRING WRITE.LEN
INTEGER*2 BUF.SIZE
INTEGER*2 CONTINUE
STRING ERR.MSG
STRING ERR.RC$
INTEGER*2 FILE.OPEN
INTEGER*2 I%
!
!  CPIC External Function Subroutines Include
!
%INCLUDE CPIC_BAS.SUB
!
!  Local Program Subroutine Declarations
!
SUB DISP.MSG(MSG)
  STRING MSG
  PRINT
  PRINT MSG
END SUB
!
SUB RECEIVE
  REQUESTED.LENGTH = BUF.SIZE
  !
  CALL DISP.MSG("Receive")
  CALL CMRCV(CONVERSATION.ID,                  \
             BUFFER,                   \
             REQUESTED.LENGTH,         \
             DATA.RECEIVED,            \
             RECEIVED.LENGTH,          \
             STATUS.RECEIVED,          \
             REQUEST.TO.SEND.RECEIVED, \
             RETURN.CODE)
  !
  IF RETURN.CODE <> CM.OK THEN \
    BEGIN
    SYS.ERR = TRUE
    PRINT "Receive Error rc = ", RETURN.CODE
    ERR.MSG = "Receive error."
    CALL DISP.MSG(ERR.MSG)
    ENDIF
END SUB
!
FUNCTION D2X$(LONG.DEC)
  STRING D2X$
  INTEGER*2 J
  INTEGER*4 TEMPC, TEMPD, LONG.DEC
  STRING HEXTAB, ERRNC

    HEXTAB = "0123456789ABCDEF"

    TEMPC = LONG.DEC
    ERRNC = ""
    FOR J = 1 TO 8
      TEMPD = TEMPC AND 0FH
      ERRNC = MID$(HEXTAB,(TEMPD + 1),1) + ERRNC
      TEMPC = SHIFT(TEMPC,4)
    NEXT J
  D2X$ = ERRNC
  END FUNCTION
!
! Include CPIC Pseudonym Equates
!
%INCLUDE CPIC_BAS.EQU
!
TRUE =  1
FALSE = 0
BUF.SIZE = 4096
SYS.ERR = FALSE
FILE.OPEN = FALSE
!
!  Main Program Begins Here
!
ON ERROR GOTO ERROR.HANDLER
!
FILE.NAME = COMMAND$
!
!
PRINT "Requested file = ",FILE.NAME
!
IF FILE.NAME = "" THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Invalid/missing command-line argument(s)."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
!  Initialize Conversation
!
CALL DISP.MSG("Issuing Initialize Conversation")
SYM.DEST.NAME = "TPRCBAS "                      ! Eight character sym_dest_name
CALL CMINIT(CONVERSATION.ID,    \
            SYM.DEST.NAME,      \
            RETURN.CODE)
!
IF RETURN.CODE <> CM.OK THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Initialize.Conversation error."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
!  Set sync level to Confirm
!
CALL DISP.MSG("Set Sync Level to Confirm")
SYNC.LEVEL = CM.CONFIRM
CALL CMSSL(CONVERSATION.ID,     \
           SYNC.LEVEL,          \
           RETURN.CODE)
!
IF RETURN.CODE <> CM.OK THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Set.Sync.Level error."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
!  Allocate Conversation
!
CALL DISP.MSG("Allocate Conversation")
CALL CMALLC(CONVERSATION.ID,     \
            RETURN.CODE)
!
IF RETURN.CODE <> CM.OK THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Allocate error."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
SEND.LENGTH = LEN(FILE.NAME)
BUFFER = FILE.NAME
!
CALL DISP.MSG("Send Requested File Name to Server")
CALL CMSEND(CONVERSATION.ID,          \
            BUFFER,                   \
            SEND.LENGTH,              \
            REQUEST.TO.SEND.RECEIVED, \
            RETURN.CODE)
!
IF RETURN.CODE <> CM.OK THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Send.Data error."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
BUFFER = STRING$(BUF.SIZE, " ")
!
CONTINUE = TRUE
WHILE CONTINUE = TRUE
  CALL RECEIVE
  !
  IF NOT SYS.ERR THEN \
    BEGIN
    IF NOT FILE.OPEN THEN \
      BEGIN
      PRINT "About to create file"
      CREATE FILE.NAME RECL BUF.SIZE AS 1 BUFFSIZE BUF.SIZE
      FILE.OPEN = TRUE
      ENDIF
    !
    IF ((DATA.RECEIVED = CM.COMPLETE.DATA.RECEIVED) OR \
        (DATA.RECEIVED = CM.INCOMPLETE.DATA.RECEIVED)) THEN \
      BEGIN
      IF RECEIVED.LENGTH = BUF.SIZE THEN \
        BEGIN
        WRITE FORM "C4096"; #1; BUFFER          ! Receive 4K blocks of data
        ENDIF \
      ELSE \                                    ! Else if < 4K sent then
        BEGIN                                   ! write on byte at a time
        FOR I% = 1 TO RECEIVED.LENGTH
          PUT 1, ASC(MID$(BUFFER, I%, 1))
        NEXT I%
        ENDIF \
      ENDIF \
    ELSE \
      BEGIN
      IF DATA.RECEIVED <> CM.NO.DATA.RECEIVED THEN \
        BEGIN
        SYS.ERR = TRUE
        ERR.MSG = "Error receiving requested file."
        CALL DISP.MSG(ERR.MSG)
        CONTINUE = FALSE
        ENDIF
      ENDIF
    !
    IF CONTINUE = TRUE THEN \
      BEGIN
      IF STATUS.RECEIVED = CM.CONFIRM.DEALLOC.RECEIVED THEN \
        BEGIN
        CLOSE 1
        !
        CALL DISP.MSG("Issuing Confirmed")
        CALL CMCFMD(CONVERSATION.ID,     \
                    RETURN.CODE)
        !
        CONTINUE = FALSE
        ENDIF
      ENDIF
  ENDIF \
ELSE \
  BEGIN
  IF FILE.OPEN = FALSE THEN \
    BEGIN
    ERR.MSG = "File not found at server."
    ENDIF \
  ELSE \
    BEGIN
    ERR.MSG = "Error receiving requested file."
    ENDIF
  CONTINUE = FALSE
  ENDIF
WEND
!
STOP
!
ERROR.HANDLER:
  ERR.RC$ = D2X$(ERRN)
  CALL DISP.MSG(ERR)
  CALL DISP.MSG(ERR.RC$)
  IF ERR = "OE" THEN \
    BEGIN
    FILE.OPEN = FALSE
    SYS.ERR = TRUE
    ERR.MSG = "Unable to open file."
    CALL DISP.MSG(ERR.MSG)
    RESUME
    ENDIF
  !
  STOP
!
END
