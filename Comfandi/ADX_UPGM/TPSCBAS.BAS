\/* TIME STAMP BLOCK ************************************************
\*LIB   TT22 TPSCBAS  B86      07/06/90 14:50:30 LEVERETT *    *                
\** END OF TIME STAMP BLOCK ****************************************/
!*****************************************************************************
!*                                                                           *
!*  Sample CPIC File Server Program TPSCBAS.BAS for 4680 Operating System    *
!*                                                                           *
!*  This sample file server transaction program accepts a conversation       *
!*  allocated by partner transaction program TPRCBAS. It will be started     *
!*  as a 4680 Background Application.  The requester first sends the         *
!*  file name of the requested file and this server sends it to the          *
!*  requester if the file exists.  An error notification is sent if the      *
!*  file does not exist on this controller.  The following CPIC calls        *
!*  are contained in this program :                                          *
!*                                                                           *
!*                      CMACCP (Accept Conversation)                         *
!*                      CMRCV  (Receive_Data)                                *
!*                      CMCFMD (Confirmed)                                   *
!*                      CMSEND (Send_Data)                                   *
!*                      CMSERR (Send_Error)                                  *
!*                      CMSDT  (Set_Deallocate_Type)                         *
!*                      CMDEAL (Deallocate)                                  *
!*                                                                           *
!*  A Remotely Attachable Local TP record must be configured in 4680         *
!*  Controller Communications Configuration specifying a TP Name identical   *
!*  to that configured in the TPR Symbolic Destination Name - Partner TP     *
!*  Name.  The conversation type is Mapped and the sync level is Confirm.    *
!*  The filespec should contain the drive, subdirectory and filename of      *
!*  the executable copy of this server transaction program, e.g.             *
!*  C:\ADX_UPGM\TPSCBAS.286                                                  *
!*  Also, the Remotely Attachable Local TP record name must be referenced    *
!*  in a Symbolic Destination Name record on the server controller.          *
!*                                                                           *
!*  Note : Since background applications cannot write directly to the        *
!*         screen, all messages that this program generates are written      *
!*         to a log file named TPSCBAS.LOG created in ADX_UPGM.              *
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
INTEGER*2 I%, BYTE%
INTEGER*4 FILE.SIZE
INTEGER*2 NUM.OF.4K.BLOCKS
INTEGER*2 LAST.BLOCK
STRING LAST.BLOCK$
INTEGER*2 BUF.SIZE
INTEGER*2 CONTINUE
STRING ARGS
STRING ERR.MSG
INTEGER*2 FILE.OPEN
INTEGER*4 RECORD.NUM
STRING READ.ERROR$
!
!  CPIC External Function Subroutines Include
!
%INCLUDE CPIC_BAS.SUB
!
!  Local Program Subroutine Declarations
!
SUB DISP.MSG(MSG)
  STRING MSG
  PRINT #2; MSG
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
    ERR.MSG = "Receive error."
    CALL DISP.MSG(ERR.MSG)
    ENDIF
END SUB
!
SUB SEND.ERROR
  CALL CMSERR(CONVERSATION.ID,                  \
              REQUEST.TO.SEND.RECEIVED, \
              RETURN.CODE)
  !
  IF RETURN.CODE <> CM.OK THEN \
    BEGIN
    SYS.ERR = TRUE
    ERR.MSG = "Send.Error error."
    CALL DISP.MSG(ERR.MSG)
    ENDIF
END SUB
!
SUB SEND.DATA
  CALL CMSEND(CONVERSATION.ID,                  \
              BUFFER,                   \
              SEND.LENGTH,              \
              REQUEST.TO.SEND.RECEIVED, \
              RETURN.CODE)
  !
  PRINT #2; "Send_data rc = ",RETURN.CODE
  IF RETURN.CODE <> CM.OK THEN \
    BEGIN
    SYS.ERR = TRUE
    ERR.MSG = "Send.Data error."
    CALL DISP.MSG(ERR.MSG)
    ENDIF
END SUB
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
! Open log file for display message subroutine
!
ON ERROR GOTO ERROR.HANDLER
CREATE "ADX_UPGM:TPSCBAS.LOG" AS 2
!
CALL CMACCP(CONVERSATION.ID,     \
            RETURN.CODE)
!
IF RETURN.CODE <> CM.OK THEN \
  BEGIN
  SYS.ERR = TRUE
  ERR.MSG = "Accept.Conversation error."
  CALL DISP.MSG(ERR.MSG)
  STOP
  ENDIF
!
CALL RECEIVE
!
IF NOT SYS.ERR THEN \
  BEGIN
  IF DATA.RECEIVED = CM.COMPLETE.DATA.RECEIVED THEN \
    BEGIN
    FILE.NAME.LEN = RECEIVED.LENGTH
    FILE.NAME = MID$(BUFFER, 1, FILE.NAME.LEN)
    CALL DISP.MSG(FILE.NAME)
    CALL RECEIVE
    IF NOT SYS.ERR THEN \
      BEGIN
      IF STATUS.RECEIVED = CM.SEND.RECEIVED THEN \
        BEGIN
        FILE.OPEN = TRUE
        ! OPEN FILE.NAME DIRECT RECL BUF.SIZE AS 1 BUFFSIZE BUF.SIZE
        OPEN FILE.NAME RECL BUF.SIZE AS 1 BUFFSIZE BUF.SIZE
        ENDIF \
      ELSE \
        BEGIN
        SYS.ERR = TRUE
        ERR.MSG = "Permission to send not received."
        CALL DISP.MSG(ERR.MSG)
        ENDIF
      ENDIF
    ENDIF \
  ELSE \
    BEGIN
    SYS.ERR = TRUE
    ERR.MSG = "Complete request not received."
    CALL DISP.MSG(ERR.MSG)
    ENDIF
  ENDIF
!
IF FILE.OPEN = TRUE THEN \
  BEGIN

  ! Calculate number of 4K blocks to transfer
  FILE.SIZE = SIZE(FILE.NAME)
  NUM.OF.4K.BLOCKS = INT%(FILE.SIZE/BUF.SIZE)


  ! Determine size of last block to send
  LAST.BLOCK = MOD(FILE.SIZE, BUF.SIZE)


  ! Read file and send 4K blocks
  SEND.LENGTH = BUF.SIZE
  FOR I% = 1 TO NUM.OF.4K.BLOCKS
    READ FORM "C4096"; #1, I%; BUFFER
    CALL DISP.MSG("Issuing Send_Data")
    CALL SEND.DATA
  NEXT I%

  ! Send last block or only block if file size < 4K
  ! GET data one byte at a time since record < 4K
  ! First clear send buffer
  BUFFER = ""
  IF LAST.BLOCK <> 0 THEN \
    BEGIN
    FOR I% = 1 TO LAST.BLOCK
      BYTE% = GET(1)
      BUFFER = BUFFER + CHR$(BYTE%)
    NEXT I%
    CALL DISP.MSG("Issuing Last Send_Data")
    CALL DISP.MSG(LAST.BLOCK$)
    SEND.LENGTH = LAST.BLOCK
    CALL SEND.DATA
    ENDIF
    !
  CLOSE 1
  ENDIF \
ELSE \
  BEGIN
  SYS.ERR = TRUE
  !
  CALL DISP.MSG("Issuing Send_Error")
  CALL SEND.ERROR
  !
  ERR.MSG = "Unable to open requested file."
  CALL DISP.MSG(ERR.MSG)
  !
  CALL DISP.MSG("Issuing Set_Deal_Type")
  CALL CMSDT(CONVERSATION.ID,             \
             CM.DEALLOCATE.ABEND, \
             RETURN.CODE)
  ENDIF
!
CALL DISP.MSG("Issuing Deallocate")
CALL CMDEAL(CONVERSATION.ID,     \
            RETURN.CODE)
!
CLOSE 2
STOP
!
ERROR.HANDLER:
  CALL DISP.MSG("In Error Handler")
  IF ERR = "OE" THEN \
    BEGIN
    FILE.OPEN = FALSE
    SYS.ERR = TRUE
    ERR.MSG = "Unable to open requested file."
    CALL DISP.MSG(ERR.MSG)
    RESUME
    ENDIF \
  ELSE \
    BEGIN
    PRINT #2; "ERR = ",ERR
    READ.ERROR$ = MID$( ERR, 1, 1 )
    IF READ.ERROR$ = "R" THEN \
      BEGIN
      SYS.ERR = TRUE
      ERR.MSG = "Error reading requested file"
      CALL DISP.MSG(ERR.MSG)
      RESUME
      ENDIF \
    ENDIF
  !
  STOP
!
END
