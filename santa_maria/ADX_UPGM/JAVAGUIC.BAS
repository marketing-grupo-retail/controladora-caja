!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! Module Name:       javaguic.bas
!
!
! Descriptive Name:  Generic CBASIC routines to handle Java GUI interface
!
!
! Copyright:
!            IBM Supermarket Application Value Pack 2001 for 4690 OS
!            5639-M34 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"
!            (C) COPYRIGHT IBM CORP 2000 ALL RIGHTS RESERVED
!            LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT
!            INSTRUCTIONS FORM NUMBER G120-2083
!
! Status: Release 1 Level 0
!
! Person Responsible =
!
! Function:
!                    This module contains the routines necessary to send
!                    informational messages to a JVM.
!
! Change Activity:
!
! IR52982   Restructured the way in which the java gui applications
!           interact with SA. The module has been rewritten. To see
!           the previous code, check version 1.3 on CMVC.
!           IBM KHG 28Jan04
!
! IO07382   JAVA.INIT should not be on if no java is needed based on
!           the command tail or other means. If no aefhooks are needed
!           to be called and there is no gui, leave init off so that
!           javaevent will not process or call javahealth.
!           IBM CRM 21Aug2007
!
! IO07467   Implement SCS requirements already implemented by ACE.
!           See JAVAAEFR.BDC for details.
!           IBM GGK 05Sep2007
!
! IO08970   Add virtual session Failover support, including processing
!           the new F1 command tail flag.
!           IBM CJN 06Jun2008
!
! END-OF-SPECIFICATIONS
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
%ENVIRON T

%INCLUDE JAVAGUIV.J86                            ! Global Variable include
%INCLUDE EAMTSWKG.J86                            ! supermarket appl wking storage
%INCLUDE EAMTOPTS.J86                            ! terminal options
                                                 ! referenced in the SA base code
INTEGER*1 GLOBAL JAVA.REDIRECT,                  \
                 TS.DS.AMOUNT.OVERFLOW           ! amount overflow occurred


INTEGER*1 GLOBAL RECURSE.FLAG                    !

INTEGER*2 GLOBAL                                 \
          TS.MONEY.SYMBOL,                       \ disp formatting at present 3458
          UK.SCALES.OPTION,                      \  set if to.scale.type > 1
          UK.ALLSCALES.OPTION                    ! set if to.scale.type = 1

STRING    GLOBAL                                 \
          JAVA.AMT$,                             \ formatted dollar amount
          TS2.TEMP1$,                            \
          TS.WEIGHTSEP$,                         \
          JAVA.WGT$                              ! formatted weight

%INCLUDE EAMXXCPY.J86                            ! Copyright Include

! external subroutine definitions

%INCLUDE JAVAGUIU.J86                            ! externs for user exits

%INCLUDE JAVA_EXT.J86                            ! externals for java

%INCLUDE EAMERRNH.J86


! application initialization functions
FUNCTION javaVPInit EXTERNAL
END FUNCTION


SUB javaVPEvent(eventNum) EXTERNAL
  INTEGER*2                                      \
            eventNum

  STRING                                         \
            temp$
END SUB

FUNCTION aefBlockForJava EXTERNAL
END FUNCTION


FUNCTION javaSVInit EXTERNAL
END FUNCTION


SUB javaSVEvent(eventNum) EXTERNAL
  INTEGER*2                                      \
            eventNum
  STRING                                         \
            temp$
END SUB



SUB handleOptionsSV EXTERNAL
END SUB


FUNCTION javaAefInit EXTERNAL
END FUNCTION

SUB javaAEFEvent(eventNum) EXTERNAL
  INTEGER*2                                      \
            eventNum
  STRING                                         \
            temp$
END SUB


SUB handleOptionsAEF EXTERNAL
END SUB


SUB javaAEFException(event, data$, exception$) EXTERNAL
  INTEGER*2                                      \
             event
  STRING                                         \
             data$,                              \
             exception$
END SUB


SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL        ! adxserve
  INTEGER*4     RET                                 !
  INTEGER*2     FUNC, PARM1                         !
  STRING        PARM2$                              !
END SUB

!*******************************************************************
! read the command tail for a terminal application
!*******************************************************************

FUNCTION command EXTERNAL
   STRING command
END FUNCTION


!*********************************************************************
!
! SUBSTR
!
! This routine performs a merge of two strings. String P3$, offset P4
! is merged into string P1$ offset P2 for a length of P5 bytes
!
! CAUTION: P1$ must be long enough to totally contain P3$
!
!*********************************************************************

 FUNCTION SUBSTR(P1$,P2,P3$,P4,P5) EXTERNAL
    STRING P1$,P3$
    INTEGER*2 P2,P4,P5
 END FUNCTION


\ REM  ****************************************************************
\ REM
\ REM   FUNCTION FC.FORMAT.AMOUNT
\ REM
\ REM   END FUNCTION
\ REM
\ REM  ****************************************************************
  FUNCTION FC.FORMAT.AMOUNT ( AMT1, FMT.LN.SLOT ) EXTERNAL          !
     INTEGER*1 JAVA.FC.FORMAT.AMOUNT                      !
     INTEGER*4 AMT1                                       !
     INTEGER*2 FMT.LN.SLOT
  END FUNCTION


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Application tracing routine
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION EAMTRACE(TRXX) EXTERNAL          !
  INTEGER*1 EAMTRACE                      !
  INTEGER*2 TRXX                          !
END FUNCTION                              !


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! UK scales support
! Feature exit code for scale support
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION TSSAEC01(FUNCTION.REQUESTED) EXTERNAL
   INTEGER*2 FUNCTION.REQUESTED,         \
             TSSAEC01                    !
END FUNCTION                              ! tssaec01


!*****************************************************************************/
!* Call UK scale feature exit setting trace string and using PARM            */
!*****************************************************************************/
 FUNCTION CALL.TSSAEC01(PARM)
    INTEGER*1 CALL.TSSAEC01
    INTEGER*2 PARM
    CALL EAMTRACE(4153H)
    CALL TSSAEC01(PARM)
    CALL EAMTRACE(5344H)
 END FUNCTION

\ REM  ****************************************************************
\ REM  ************** FORMAT DOLLAR AMOUNT ****************************
\ REM
\ REM   FUNCTION FORMAT.DOLLARS
\ REM
\ REM     ADD SIGN CHARS TO AMOUNT FIELD
\ REM     IF MORE THAN 8 DIGITS THEN
\ REM     : TRUNCATE TO 8 DIGITS
\ REM     : REPLACE LAST 2 DIGITS WITH **
\ REM     ENDIF
\ REM     IF CENT SEP FIELD IS DEFINED THEN
\ REM     : INSERT CENT SEPARATOR
\ REM     ENDIF
\ REM     IF THOUSANDS SEP NEEDS INSERTING THEN
\ REM     : INSERT IT
\ REM     ENDIF
\ REM     IF MILLIONS SEP NEEDS INSERTING THEN
\ REM     : INSERT IT
\ REM     ENDIF
\ REM
\ REM   END FUNCTION
\ REM  ****************************************************************
  FUNCTION FORMAT.DOLLARS( FDAMT )                !
     INTEGER*1 FORMAT.DOLLARS                        !
     INTEGER*4 FDAMT                                 !
     INTEGER*2 THOU, MILL, L%                        !

     IF FDAMT < 0 THEN BEGIN                         !
       TS.TEMP3$ = STR$(-FDAMT) + "-"                !
     ENDIF ELSE BEGIN                                !
       TS.TEMP3$ = STR$(FDAMT)  + " "                !
     ENDIF                                           !

! IF UK SCALES ARE SUPPORTED AND A MONETARY SYMBOL IS TO BE ADDED
! TO THE DISPLAYED AMOUNT THEN IT'S NECESSARY TO SHORTEN THE MAX
! LENGTH OF AMOUNT STRINGS TO COPE WITH THE MONETARY SYMBOL.
     IF (LEN(TS.TEMP3$) > 8) AND                     \ > 8 DIGITS
        (TS.MONEY.SYMBOL AND UK.ALLSCALES.OPTION) THEN BEGIN
       TS.TEMP3$ = RIGHT$(TS.TEMP3$, 8)              ! TRUNCATE
       CALL SUBSTR (TS.TEMP3$,5,"**",0,2)            ! FLAG **
       TS.DS.AMOUNT.OVERFLOW = -1                    ! AMOUNT OVERFLOW OCCURRED
     ENDIF ELSE BEGIN                                !
       IF (LEN(TS.TEMP3$) > 9) THEN BEGIN            ! > 8 DIGITS
         TS.TEMP3$ = RIGHT$(TS.TEMP3$, 9)            ! TRUNCATE
         CALL SUBSTR (TS.TEMP3$,6,"**",0,2)          ! FLAG WITH **
         TS.DS.AMOUNT.OVERFLOW = -1                  ! AMOUNT OVERFLOW OCCURRED
       ENDIF                                         !
     ENDIF                                           !

     IF TS.CENTSEP$ NE " "   THEN BEGIN              ! IS CENT SEP
       L% = LEN(TS.TEMP3$)                           !
       IF (L% < 3) THEN BEGIN                        ! ZERO FILL
         TS.TEMP3$ = TS.CENTSEP$ + RIGHT$( "00" + TS.TEMP3$, 3)
       ENDIF ELSE BEGIN                              !
         TS.TEMP3$ = LEFT$(TS.TEMP3$, L%-3) +        \ INS CENT SEP
                     TS.CENTSEP$ +                   \
                     RIGHT$(TS.TEMP3$, 3)            !
       ENDIF                                         !

       IF UK.ALLSCALES.OPTION THEN BEGIN             !
         CALL CALL.TSSAEC01(11)
       ENDIF                                         !

       THOU = 7                                      ! FOR THOUSANDS
       MILL = 11                                     ! FOR MILLIONS
     ENDIF ELSE BEGIN                                ! NO CENT SEP
       THOU = 4                                      ! FOR THOUSANDS
       MILL = 8                                      ! FOR MILLIONS
     ENDIF                                           !

     TS2.TEMP1$ = RIGHT$(TS.TEMP3$,8)                ! SET SHOPPER AMOUNT

     L% = LEN(TS.TEMP3$)
     IF (L% > THOU) THEN BEGIN                       ! THOUSANDS
       TS.TEMP3$ = LEFT$(TS.TEMP3$, L%-THOU) +       \ INS $ SEP
                   TS.DOLLARSEP$ +                   \
                   RIGHT$(TS.TEMP3$, THOU)           !
     ENDIF                                           !

     L% = LEN(TS.TEMP3$)
     IF (L% > MILL) THEN BEGIN                       ! MILLIONS
       TS.TEMP3$ = LEFT$(TS.TEMP3$, L%-MILL) +       \ INS $ SEP
                   TS.DOLLARSEP$ +                   \
                   RIGHT$(TS.TEMP3$, MILL)           !
     ENDIF                                           !

! EXIT FOR LEAD MINUS SIGN AND MONETARY SYMBOL
     IF UK.ALLSCALES.OPTION THEN BEGIN               !
       CALL CALL.TSSAEC01(13)
     ENDIF                                           !

  END FUNCTION

\ REM  ****************************************************************
\ REM
\ REM   FUNCTION JAVA.FORMAT.AMOUNT
\ REM
\ REM     CALL FORMAT.DOLLARS TO CONVERT AMOUNT FIELD TO DOLLAR STRING
\ REM     MOVE DISPLAY LINE TO OUTPUT PARAMETER
\ REM
\ REM   END FUNCTION
\ REM
\ REM  ****************************************************************
  FUNCTION JAVA.FORMAT.AMOUNT ( AMT1 ) PUBLIC          !
     INTEGER*1 JAVA.FORMAT.AMOUNT                         !
     INTEGER*4 AMT1                                       !

     CALL FORMAT.DOLLARS( AMT1 )                          ! FORMAT AMT
     JAVA.AMT$ = TS.TEMP3$                                !

  END FUNCTION



!****************** JAVA.FC.FORMAT.AMOUNT *************************
! This function adjusts the number of decimal places, the cents   *
! separator, and the dollars separator based on what is defined   *
! in the Exchange Rate File "DECIMAL" field.                      *
!******************************************************************

FUNCTION JAVA.FC.FORMAT.AMOUNT ( AMT, FMT.LN.SLOT ) PUBLIC

  INTEGER*1                                      \
            JAVA.FC.FORMAT.AMOUNT                ! Return Code

  INTEGER*2                                      \
            FMT.LN.SLOT                          ! Foreign Currency Slot Number

  INTEGER*4                                      \
            AMT                                  ! Amount to Format

  CALL FC.FORMAT.AMOUNT(AMT, FMT.LN.SLOT)
  JAVA.AMT$ = TS.TEMP3$

END FUNCTION


\ REM  ****************************************************************
\ REM
\ REM   FUNCTION JAVA.FORMAT.WEIGHT
\ REM
\ REM     CONVERT WEIGHT FIELD TO STRING
\ REM     ADD DECIMAL POINT IN POSITION SPECIFIED IN OPTIONS
\ REM
\ REM   END FUNCTION
\ REM
\ REM  ****************************************************************
  FUNCTION JAVA.FORMAT.WEIGHT ( WGT ) PUBLIC      !
     INTEGER*1 JAVA.FORMAT.WEIGHT                    !
     INTEGER*4 WGT                                   !

     JAVA.WGT$ = STR$ ( WGT )                        !
     IF (LEN(JAVA.WGT$) < TO.WGT.DECPOS + 1) THEN BEGIN   ! MORE 0S REQD
       JAVA.WGT$ = RIGHT$( "0000" + JAVA.WGT$,       \
                   TO.WGT.DECPOS+1)
     ENDIF                                           !
     JAVA.WGT$ = LEFT$(JAVA.WGT$, LEN(JAVA.WGT$)-TO.WGT.DECPOS ) + \
                 TS.WEIGHTSEP$ +                                   \
                 RIGHT$(JAVA.WGT$, TO.WGT.DECPOS)

  END FUNCTION

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! sub-routine javaGuiAppTrace
!
! parms: trace - string containing information regarding the tracing
!                required by the application specific tracing routine
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaGuiAppTrace(trace$) PUBLIC               ! javaGuiAppTrace
  STRING                                         \
          trace$                                 ! passed value to trace

  IF (trace$ = javaTraceUser$) THEN BEGIN        ! Going to java user exit
    CALL EAMTRACE(756AH)                         ! Trace 'ju'
  ENDIF                                          !

  IF (trace$ = javaTraceGui$) THEN BEGIN         ! Going to java gui code
    CALL EAMTRACE(676AH)                         ! Trace 'jg'
  ENDIF                                          !

  IF (trace$ = javaTraceApp$) THEN BEGIN         ! Going to appl specific code
    CALL EAMTRACE(616AH)                         ! Trace 'ja'
  ENDIF                                          !

END SUB                                          ! javaGuiAppTrace


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! Function JavaHealth
!
!   return data:   -1 If the java pipe is detected indicating java health.
!
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaHealth PUBLIC RECURSIVE                     ! javaHealth

  INTEGER*2                                              \
            javaHealth                                   ! passed event #

  STRING                                                 \
            openErr$,                                    \ Error from Open
            openErrh$                                    ! Open errn value

  javaHealth = -1
  ON ERROR GOTO javaHealthErr                            ! set local error

  IF tssEnabled THEN BEGIN
!   OPEN "pi:AEFjava" AS javapipenum NOREAD NOWRITE NODEL    ! open pipe
    OPEN "pi:tssjava" AS javapipenum NOREAD NOWRITE NODEL ! open pipe
  ENDIF ELSE BEGIN
!   OPEN "pi:AEFjava" AS javaPipeNum NOREAD NOWRITE NODEL    ! open pipe
    OPEN "pi:acejava" AS javaPipeNum NOREAD NOWRITE NODEL ! open pipe
  ENDIF
  CLOSE javaPipeNum                                      ! close pipe

  EXIT FUNCTION                                          ! get out

javaHealthErr:                                           ! local error
  openErr$ = ERR                                         ! save error
  openErrh$ = ERRNHEX$                                   ! save errn value

  IF (openErr$ <> "CU") THEN BEGIN                       ! not CU error
    javaHealth = 0                                       ! set event to not go
  ENDIF                                                  ! not CU error

  RESUME                                                 ! just resume

END FUNCTION                                             ! javaHealth

!***********************************************************************
!
!  SUB transmitToJVM
!
!  Parms:   eventNum
!           temp$
!           eventData$
!           eventSource (0 = value pack gui, 1 = surevision, 2 = aef)
!
!  This subroutine provides the interface between SA and java
!
!***********************************************************************

SUB transmitToJVM(eventNum, temp$, eventData$, eventSource) PUBLIC

   INTEGER*2                                                 \
     eventNum,                                               \
     eventSource


   STRING                                                     \
     eventData$,                                              \
     javaData$,                                               \ Data for Java
     javaException$,                                          \ Java Exception Data
     temp$

   ! send the event off to the java application

     CALL javaCall.Initialize.Request(javaGuiClass$,          \
                                      javaGuiMethod$,         \
                                      javaData$)              !

     CALL javaCall.AddParameter.String(javaData$, eventData$) !
     CALL javaCall.InvokeMethod.ReturnVoid(javaData$, javaException$) !

     CALL javaGuiAppTrace(javaTraceApp$)                      ! trace appl

     CALL javaGUIUserException(eventNum,                      \ Event #
                             temp$,                           \ Its data
                             javaException$)                  ! the exception

     CALL javaGuiAppTrace(javaTraceGui$)                      ! trace GUI

     CALL javaGuiAppTrace(javaTraceUser$)                     ! trace user
     CALL javaGuiUserException(eventNum,                      \ Event #
                               temp$,                         \ Its data
                               javaException$)                ! the exception
     CALL javaGuiAppTrace(javaTraceGui$)                      ! trace GUI


END SUB

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
!   function javaEvent
!
!   parameters:    event number (based upon enumerated list)
!   return data:   None
!   references:    java gui event processing user exit
!                  cbasic -> java routines
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

FUNCTION javaEvent(eventNum) PUBLIC RECURSIVE

   INTEGER*2        \
     eventNum,      \
     sendEvent

   sendEvent = 0

   IF (JAVA.INIT = -1) THEN BEGIN
       IF (javaHealth = -1) THEN BEGIN
          sendEvent = -1
       ENDIF

      IF (sendEvent = -1) THEN BEGIN
         IF (eventNum <= jGuiEndAppEvents) THEN BEGIN       ! appl event
           IF javaGuiActive = -1 THEN BEGIN                 !
              CALL javaGuiAppTrace(javaTraceApp$)           ! appl trace
              CALL javaVPEvent(eventNum)                    ! vp gui
              CALL javaGuiAppTrace(javaTraceGui$)           ! Trace back

           ENDIF ELSE IF (javaGuiActive = -2) THEN BEGIN
              CALL javaGuiAppTrace(javaTraceApp$)           ! appl trace
              CALL javaSVEvent(eventNum)                    ! surevision gui
              CALL javaGuiAppTrace(javaTraceGui$)           ! Trace back
           ENDIF
         ENDIF
      ENDIF

      ! If command tail option specifies that we should block
      ! then the next line will block until a pipe created in
      ! the JVM is detected.  If the blockForJava options is not
      ! set, or the stub version of the aef hooks is linked in,
      ! the call will return a zero.
      sendEvent = aefBlockForJava
      IF (sendEvent = 0) THEN BEGIN
         IF (javaHealth = -1) THEN BEGIN
            sendEvent = -1
         ENDIF
      ENDIF
      IF (sendEvent = -1) THEN BEGIN
         IF (eventNum <= jGuiEndAppEvents) THEN BEGIN
            IF (callAEFHooks) THEN BEGIN
               CALL javaGuiAppTrace(javaTraceApp$)           ! appl trace
               CALL javaAEFEvent(eventNum)                   ! aef XML event
               CALL javaGuiAppTrace(javaTraceGui$)           ! Trace back
            ENDIF
         ENDIF
      ENDIF
   ENDIF

END FUNCTION


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!   function javaGuiInit
!
!   parameters:          initialize for Java Gui processing
!   return data:         none
!   data areas modified: enumerated values of events
!                        java class & method variables
!   references:          java gui initialization user exit
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaGuiInit PUBLIC                      ! JavaGuiInit

  STRING                                          \
            thisAppl$,                            \
            TEMP$
  INTEGER*2                                       \
            startLoc
  INTEGER*1                                       \
            bypassCmd

  javaGuiClass$ = ""                             ! free heap
  javaGuiMethod$ = ""                            !
  aefUnitOfWorkFlag = 0                          ! reset unit of work flag
  javaSOrestOptsRead = 0                         ! need to reset store options vars for restricted items
  JAVA.TS.TRAINING = 0                           ! reset training flag
  JAVA.EM.RD.PRINTLINE$ = ""                     ! reset EM based variables
  JAVA.EM.RD.ITEMNAME$ = ""                      !
  JAVA.EM.RD.POINTS$ = ""                        !
  JAVA.EM.PT.USERTYPE$ = ""                      !

  callAefHooks = -1                              !
  javaGuiActive = 0
  surevisionGuiEnabled = 0
  blockForJava = -1
  tssEnabled = 0
  JAVA.FAILOVER.ENABLED = 0                      ! Must be enabled via "F1" command tail. IO08970

!AIO08970
!  The following code block allows for debugging virtual sessions.
!  Since a command tail cannot be passed with the debugger, set a
!  breakpoint here, then set the bypassCmd to -1, step over the
!  TEMP$ assignment, and then modify the value of TEMP$ to set
!  the T and F command tail options as necessary.
!EIO08970
  TEMP$ = command
  IF (bypassCmd) THEN BEGIN
!AIO08970 Add Failover flag
!    TEMP$ = "A1 B1 T0       "
     TEMP$ = "A1 B1 T0 F0    "
!EIO08970
 ENDIF


  IF LEN(TEMP$) <> 0 THEN BEGIN                          ! have a command tail
     tailPassed = -1
     startLoc = MATCH("A", TEMP$, 1)
     IF (startLoc > 0) THEN BEGIN
        thisAppl$ = MID$(TEMP$, startLoc, 2)
        IF RIGHT$(thisAppl$, 1) = "0" THEN BEGIN         ! aef disabled
           callAefHooks = 0                              !
        ENDIF
     ENDIF
     startLoc = MATCH("G", TEMP$, 1)
     IF (startLoc > 0) THEN BEGIN
        thisAppl$ = MID$(TEMP$, startLoc, 2)
        IF RIGHT$(thisAppl$, 1) = "1" THEN BEGIN         ! vp enabled
           javaGuiActive = -1
        ENDIF ELSE IF RIGHT$(thisAppl$, 1) = "2" THEN BEGIN
           javaGuiActive = -2                            ! surevision enabled
        ENDIF ELSE IF RIGHT$(thisAppl$, 1) = "3" THEN BEGIN
           ! some new gui option would be processed here.
        ENDIF
     ENDIF
     startLoc = MATCH("T", TEMP$, 1)
     IF (startLoc > 0) THEN BEGIN
        thisAppl$ = MID$(TEMP$, startLoc, 2)
        IF RIGHT$(thisAppl$, 1) = "1" THEN BEGIN
           tssEnabled = -1                               ! this is a tss session
        ENDIF
     ENDIF
     startLoc = MATCH("B", TEMP$, 1)
     IF (startLoc > 0) THEN BEGIN
        thisAppl$ = MID$(TEMP$, startLoc, 2)
        IF RIGHT$(thisAppl$, 1) = "0" THEN BEGIN
           blockForJava = 0                              ! do not wait for java health
        ENDIF
     ENDIF
!AIO08970 If the command tail includes an "F1", then failover is enabled.
     startLoc = MATCH("F", TEMP$, 1)
     IF (startLoc > 0) THEN BEGIN
       thisAppl$ = MID$(TEMP$, startLoc, 2)
       IF RIGHT$(thisAppl$, 1) = "1" THEN BEGIN
         JAVA.FAILOVER.ENABLED = -1                    ! Failover enabled.
       ENDIF
     ENDIF
!EIO08970
  ENDIF



  updateReceiptMsg               = 1

  updateTotalMsg                 = updateReceiptMsg              + 1  !  2

  updateScaleMsg                 = updateTotalMsg                + 1  !  3

  addAccountTenderMsg            = updateScaleMsg                + 1  !  4

  addDiscountEntryMsg            = addAccountTenderMsg           + 1  !  5

  addItemEntryMsg                = addDiscountEntryMsg           + 1  !  6

  addTenderEntryMsg              = addItemEntryMsg               + 1  !  7

  changeGivenMsg                 = addTenderEntryMsg             + 1  !  8

  endAccountingTransactionMsg    = changeGivenMsg                + 1  !  9

  endSalesTransactionMsg         = endAccountingTransactionMsg   + 1  ! 10

  endTransactionMsg              = endSalesTransactionMsg        + 1  ! 11

  operatorSignedOffMsg           = endTransactionMsg             + 1  ! 12

  operatorSignedOnMsg            = operatorSignedOffMsg          + 1  ! 13

  optionsLoadedMsg               = operatorSignedOnMsg           + 1  ! 14

  priceVerifyMsg                 = optionsLoadedMsg              + 1  ! 15

  procedureCompletedMsg          = priceVerifyMsg                + 1  ! 16

  procedureStartedMsg            = procedureCompletedMsg         + 1  ! 17

  startAccountingTransactionMsg  = procedureStartedMsg           + 1  ! 18

  startSalesTransactionMsg       = startAccountingTransactionMsg + 1  ! 19

  startTransactionMsg            = startSalesTransactionMsg      + 1  ! 20

  reportOutputMsg                = startTransactionMsg           + 1  ! 21

  scrollMsg                      = reportOutputMsg               + 1  ! 22

  displayVelocityCodesMsg        = scrollMsg                     + 1  ! 23

  hideVelocityCodesMsg           = displayVelocityCodesMsg       + 1  ! 24

  redrawReceiptWindowMsg         = hideVelocityCodesMsg          + 1  ! 25

  updateStatusMsg                = redrawReceiptWindowMsg        + 1  ! 26

  customerCardScannedMsg         = updateStatusMsg               + 1  ! 27

  javaKeysMsg                    = customerCardScannedMsg        + 1  ! 28

  add2x20Msg                     = javaKeysMsg                   + 1  ! 29

  terminalSubStateMsg            = add2x20Msg                    + 1  ! 30

  optionsLoadingStartedMsg       = terminalSubStateMsg           + 1  ! 31

  optionsLoadingEndedMsg         = optionsLoadingStartedMsg      + 1  ! 32

  optionsDataMsg                 = optionsLoadingEndedMsg        + 1  ! 33

  itemJustAddedMsg               = optionsDataMsg                + 1  ! 34

  voidTransactionMsg             = itemJustAddedMsg              + 1  ! 35

  foodStampTotalMsg              = voidTransactionMsg            + 1  ! 36

  foreignTotalMsg                = foodStampTotalMsg             + 1  ! 37

  terminalConnectionChangeMsg    = foreignTotalMsg               + 1  ! 38

  otrMsg                         = terminalConnectionChangeMsg   + 1  ! 39

  discountJustAddedMsg           = otrMsg                        + 1  ! 40

  operatorSpecialSignedOnMsg     = discountJustAddedMsg          + 1  ! 41

  updateTotalsDataMsg            = operatorSpecialSignedOnMsg    + 1  ! 42

  aefEMredemptionCpnMsg          = updateTotalsDataMsg           + 1  ! 43

  aefUnitOfWorkMsg               = aefEMredemptionCpnMsg         + 1  ! 44

  operatorSpecialSignedOffMsg    = aefUnitOfWorkMsg              + 1  ! 45

  procedureStartedMsg2           = operatorSpecialSignedOffMsg   + 1  ! 46

  operatorDisplay                = procedureStartedMsg2          + 1  ! 47

  customerDisplay                = operatorDisplay               + 1  ! 48

  noSaleStartMsg                 = customerDisplay               + 1  ! 49

  noSaleEndMsg                   = noSaleStartMsg                + 1  ! 50
!AIO07467/17667
\   jGuiEndAppEvents             = noSaleEndMsg                  + 1  !
  tillExchangeOccurred           = noSaleEndMsg                  + 1  ! 51
!AIO08970 Initialize new tenderDeclinedMsg constant.
  transactionUpdateMsg           = tillExchangeOccurred          + 1  ! 52

  tenderDeclinedMsg              = transactionUpdateMsg          + 1  ! 53
  
  efUserEvent                    = tenderDeclinedMsg             + 1  !
   
  aefUserNuoEvent                = efUserEvent                   + 1  !

  jGuiEndAppEvents               = aefUserNuoEvent               + 1  !
!EIO08970
!EIO07467/17667


  expectingItemEvent             = 1001
  expectingTenderEvent           = 1002
  expectingForeignTenderEvent    = 1004
  expectingCouponEvent           = 1005
  expectingManagerOverrideEvent  =1006
  expectingOperatorOverrideEvent =1007
  expectingProcedureEvent        =1008
  priceVerifyOccurredEvent       =1009
  totalPressed                   =1010
  expectingLoanAmount            =1011
  expectingPickupAmount          =1012
  expectingTenderCountAmount     =1013
!    Note 10000 gets added to the following
!    events within the SA java feature code.
  trainingModeEvent              =1045
  standaloneEvent                =1046
  transactionVoidedEvent         =1048
  fsBalDueOccurredEvent          =1070
!
! The range of 5000-5999 is reserved for SA
! substates which do not correspond to descriptors.
!
  ManagerOverrideSuccessEvent    =5000             !
  ManagerOverrideCancelEvent     =5001             !
  OperatorOverrideSuccessEvent   =5002             !
  OperatorOverrideCancelEvent    =5003             !
  ValidAccountEvent              =5004             !
  ItemPromptCancelEvent          =5005             !
  ItemPromptSatisfiedEvent       =5006             !
  wicStartEvent                  =5007             !
!AIO08970 New substates must be raised by the payment system.
  waitingForPinpad               =5008             !
  waitingForSignature            =5009             !
  waitingForAccountNumAtPINPAD   =5010             !
!EIO08970
  printerPaperCoverOverride      =5893             ! for overrideable B893 paper out/cover open

! tsue's for user exit use

  jtsue01 = 1001
  jtsue02 = jtsue01 + 1
  jtsue03 = jtsue02 + 1
  jtsue04 = jtsue03 + 1
  jtsue05 = jtsue04 + 1
  jtsue06 = jtsue05 + 1
  jtsue07 = jtsue06 + 1
  jtsue08 = jtsue07 + 1
  jtsue09 = jtsue08 + 1
  jtsue10 = jtsue09 + 1
  jtsue11 = jtsue10 + 1
  jtsue12 = jtsue11 + 1
  jtsue13 = jtsue12 + 1
  jtsue14 = jtsue13 + 1
  jtsue15 = jtsue14 + 1
  jtsue16 = jtsue15 + 1
  jtsue17 = jtsue16 + 1
  jtsue18 = jtsue17 + 1
  jtsue19 = jtsue18 + 1
  jtsue20 = jtsue19 + 1
  jtsue21 = jtsue20 + 1
  jtsue22 = jtsue21 + 1
  jtsue23 = jtsue22 + 1
  jtsue24 = jtsue23 + 1
  jtsue25 = jtsue24 + 1
  jtsue26 = jtsue25 + 1
  jtsue27 = jtsue26 + 1
  jtsue28 = jtsue27 + 1
  jtsue29 = jtsue28 + 1
  jtsue30 = jtsue29 + 1
  jtsue31 = jtsue30 + 1
  jtsue32 = jtsue31 + 1
  jtsue33 = jtsue32 + 1
  jtsue34 = jtsue33 + 1
  jtsue35 = jtsue34 + 1
  jtsue36 = jtsue35 + 1
  jtsue37 = jtsue36 + 1
  jtsue38 = jtsue37 + 1
  jtsue39 = jtsue38 + 1
  jtsue40 = jtsue39 + 1
  jtsue41 = jtsue40 + 1
  jtsue42 = jtsue41 + 1
  jtsue43 = jtsue42 + 1
  jtsue44 = jtsue43 + 1
  jtsue45 = jtsue44 + 1
  jtsue46 = jtsue45 + 1
  jtsue47 = jtsue46 + 1
  jtsue48 = jtsue47 + 1
  jtsue49 = jtsue48 + 1
  jtsue50 = jtsue49 + 1
  jtsue51 = jtsue50 + 1
  jtsue52 = jtsue51 + 1
  jtsue53 = jtsue52 + 1
  jtsue54 = jtsue53 + 1
  jtsue55 = jtsue54 + 1
  jtsue56 = jtsue55 + 1
  jtsue57 = jtsue56 + 1
  jtsue58 = jtsue57 + 1
  jtsue59 = jtsue58 + 1
  jtsue60 = jtsue59 + 1
  jtsue61 = jtsue60 + 1
  jtsue62 = jtsue61 + 1
  jtsue63 = jtsue62 + 1
  jtsue64 = jtsue63 + 1
  jtsue65 = jtsue64 + 1
  jtsue66 = jtsue65 + 1
  jtsue67 = jtsue66 + 1
  jtsue68 = jtsue67 + 1
  jtsue69 = jtsue68 + 1
  jtsue70 = jtsue69 + 1
  jtsue71 = jtsue70 + 1
  jtsue72 = jtsue71 + 1
  jtsue73 = jtsue72 + 1
  jtsue74 = jtsue73 + 1
  jtsue75 = jtsue74 + 1
  jtsue76 = jtsue75 + 1
  jtsue77 = jtsue76 + 1
  jtsue78 = jtsue77 + 1
  jtsue79 = jtsue78 + 1
  jtsue80 = jtsue79 + 1
  jtsue81 = jtsue80 + 1
  jtsue82 = jtsue81 + 1
  jtsue83 = jtsue82 + 1
  jtsue84 = jtsue83 + 1
  jtsue85 = jtsue84 + 1
  jtsue86 = jtsue85 + 1
  jtsue87 = jtsue86 + 1
  jtsue88 = jtsue87 + 1
  jtsue89 = jtsue88 + 1
  jtsue90 = jtsue89 + 1
  jtsue91 = jtsue90 + 1
  jtsue92 = jtsue91 + 1
  jtsue93 = jtsue92 + 1
  jtsue94 = jtsue93 + 1
  jtsue95 = jtsue94 + 1
  jtsue96 = jtsue95 + 1
  jtsue97 = jtsue96 + 1
  jtsue98 = jtsue97 + 1
  jtsue99 = jtsue98 + 1


  jGuiStartUserEvent             = 2001          ! start for user added

  jGuiEndEventList               = 32767         ! maximum end

  !Range for Store Option Keys (100)
  StoreOptionsKeys                    = 100
  StoreInfoKey                        = StoreOptionsKeys + 1
  !Range for Security Options Keys (200)
  SecurityOptionsKeys                 = 200
  NoSalePriceVerifyInTransactionKey   = SecurityOptionsKeys + 1
  NegTransAtSingleTermKey             = NoSalePriceVerifyInTransactionKey + 1
  ManagersKeyNeededKey                = NegTransAtSingleTermKey + 1
  AllowTenderCashinginTransKey        = ManagersKeyNeededKey + 1
  MaxTransactionSizeKey               = AllowTenderCashinginTransKey + 1
  TransactionSizeWarningKey           = MaxTransactionSizeKey + 1
  RefundReasonCodes                   = TransactionSizeWarningKey + 1
  !Range for Food Stamp Options Keys (300)
  FoodStampsOptionsData               = 300
  AllowFoodStampsKey                  = FoodStampsOptionsData + 1
  FSTenderAfterFSTotalKey             = AllowFoodStampsKey + 1
  !Range for Tender Options Keys (400)
  TenderOptionsData                   = 400
  TenderTypeAttributesKey             = TenderOptionsData + 1
  !Range for LoyaltyOptionsKeys (500)
  LoyaltyOptionsKeys                  = 500
  EMCustomerIDKey                     = LoyaltyOptionsKeys + 1
  !Range for WIC Options Keys (600)
  WICOptionsData                      = 600
  !Range for Suspend Options Keys (700)
  SuspendRetrieveOptionsData          = 700
  MaxAllowedSuspendsPerTerminalKey    = SuspendRetrieveOptionsData + 1
  !Range for Coupon Options Keys (800)
  CouponOptionsData                   = 800
  !Range for Weight Options (900)
  WeightOptionsData                   = 900
  TareOptionsData                     = WeightOptionsData + 1
! DepartmentKeyData                   = TareOptionsData + 1    !@tal04012003 -> 1200
  !Range for Tare Options Keys (1000)
  TareOptionsKeys                     = 1000
  !Discounts Options Keys (1100)
  DiscountOptionsData                 = 1100
  TransactionDiscounts                = DiscountOptionsData + 1
  DepartmentDiscounts                 = TransactionDiscounts + 1
  LineItemDiscountOptions             = DepartmentDiscounts + 1
  !@tal04012003 - department lookup keys is now 1200
  DepartmentKeyData                   = 1200
  !Reserve blocks for NRSC and/or BP's extensions (3000)
  StartBusinessPartner                = 3000
  !Reserve blocks for customer extensions (6000)
  StartCustomer                       = 6000

!AIO08970 Clear flags which may have been set by the payment system.
  JAVA.PAYMENT.SENT.TO.HOST = 0
  JAVA.SIGNATURE.LINE = 0
!EIO08970

  javaToken$ = CHR$(08h)                         ! set token for msg

  javaTraceUser$ = "ju"                          ! set trace tokens
  javaTraceGui$  = "jg"                          !
  javaTraceApp$  = "ja"                          !

  jGuiMaxSavedStateIndex=30
  DIM jGuiSavedSubState(jGuiMaxSavedStateIndex)
  DIM jGuiSavedSubStateDesc$(jGuiMaxSavedStateIndex)
  jGuiSavedSubStateIndex=1

  CALL javaGuiAppTrace(javaTraceApp$)            ! Trace application specific
  CALL javaVPInit                                ! init value pack
  CALL javaSVInit                                ! init surevision
  CALL javaAefInit                               ! init aef
  CALL javaGuiAppTrace(javaTraceUser$)           ! Trace GUI code

  CALL javaGuiInitUserExit(jGuiStartUserEvent)   ! allow the user to add events
                                                 ! or turn code on or off

! It may not make sense to switch to the java screen.
! Let the java code handle that.
! if we are still active at this point, then we might as well flip over
! to the java display...
!  IF (javaGuiActive) THEN BEGIN                  ! jgui active
!    ! flip to the screen
!    CALL ADXSERVE(adxSrvRet,55,1,"")             ! show Java
!  ENDIF                                          ! jgui active

   IF (javaGuiActive OR callAefHooks) THEN BEGIN !
      IF (javaPipeNum = 0) THEN BEGIN            ! no pipe #
        javaPipeNum = 91                         ! set default
      ENDIF                                      ! no pipe #
      JAVA.INIT = -1                             ! GUI is in use IO07382
   ENDIF

!AIO07382 If a GUI is not used, don't set this flag (set it above)
\ JAVA.INIT = -1
!EIO07382

  CALL javaEvent(optionsLoadingStartedMsg)


END FUNCTION                                     ! JavaGuiInit



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
!   function javaKey
!
!   parameters:    key stem to test
!   return data:   -1 indicates the passed keystem is a java defined key
!                  -0 indicates the passed keystem is NOT a java defined key
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION javaKey(keyStem) PUBLIC                 ! javaKey

  INTEGER*2                                      \
            javaKey,                             \ return value
            tempReturn,                          \ temporary value
            keyStem                              ! passed key stem

  CALL javaGuiAppTrace(javaTraceGui$)             ! trace GUI

  IF (javaKeysActive = 0) THEN BEGIN             ! Not active
    javaKey = 0                                  ! not available
    EXIT FUNCTION                                ! early exit
  ENDIF                                          ! java keys are not active

  IF (keyStem = javaHelpKey        ) OR          \
     (keyStem = javaPageUpKey      ) OR          \
     (keyStem = javaPageDnKey      ) OR          \
     (keyStem = javaScrollUpKey    ) OR          \
     (keyStem = javaScrollDnKey    ) OR          \
     (keyStem = javaTabKey         ) OR          \
     (keyStem = javaBackTabKey     ) OR          \
     (keyStem = javaClickKey       ) OR          \
     (keyStem = javaSelectPaneKey  ) OR          \
     (keyStem = javaSelectWindowKey) THEN BEGIN  !

    tempReturn = -1                              ! Key was pressed
  ENDIF ELSE BEGIN                               ! not a defined java key
    tempReturn = 0                               ! set return
  ENDIF                                          ! java key?

  CALL javaGuiAppTrace(javaTraceUser$)           ! trace user
  CALL javaGuiUserJavaKey(keyStem,tempReturn)    ! see if user wants to modify
  CALL javaGuiAppTrace(javaTraceGui$)            ! trace GUI

  javaKey = tempReturn                           ! return the value

END FUNCTION                                     ! javaKey

!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
! handle sending options information to the JVM
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
SUB handleOptionsForJava PUBLIC

   IF (javaGuiActive = -2) THEN BEGIN
      CALL handleOptionsSV
   ENDIF

   IF (callAefHooks) THEN BEGIN !
      CALL handleOptionsAEF
   ENDIF

END SUB


!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! sub-routine javaReadScale
!
! Modelled after TSHIEC05
!
! Read the scale & store the weight in ts.temp1i4
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
SUB javaReadScale PUBLIC                           ! javaReadScale

  INTEGER*2                                        \
            TS.LINEDATA                            ! local linedata variable

  TS.TEMP1I4 = 0                                   ! clear temp1i4

 ! if uk approved scales are not attached then use std scales
 ! which are accessed via the "scale:" device driver.
 ! the uk function places the weight read in ts.temp1i4.
  IF (UK.SCALES.OPTION) THEN BEGIN                 ! if UK Scales
!   No Support
!   call tssaec01(3)                               ! get the weight
  ENDIF ELSE BEGIN                                 ! not UK scales
    TS.ER.RETURN = -1                              ! prepare for error
    READ FORM "I4" ; # 39 ; TS.TEMP1I4             ! read weight from scale
    IF (TS.ER.RETURN = 0) THEN BEGIN               ! if error then begin
      IF TS.ERRN = (80a30564h) THEN BEGIN          ! scale reads zero
        TS.TEMP1I4 = 0                             ! set weight = 0
      ENDIF ELSE BEGIN                             ! scale not zero
        TS.LINEDATA = 0                            ! clear linedata
!AIR51735 Handle any scale RC as an error.
!       if (ts.errn = 80a30563h) or \              ! scale is in motion
!          (ts.errn = 80a3056ch) or \              ! or scale limit error
!          (ts.errn = 80a30562h) or \              ! bad data from scale
!          (ts.errn = 80a3056dh) or \              ! or under zero
!          (ts.errn = 80a30561h) then begin        ! read err. on  scale
        IF ((TS.ERRN AND 00FF0000h) = 00a30000H) THEN BEGIN ! any scale error
!EIR51735
          IF (UK.ALLSCALES.OPTION) THEN BEGIN      ! if uk scales
          ENDIF ELSE BEGIN                         ! else not uk scales
            TS.LINEDATA = 33                       ! msgb033
            javaScaleErrn = TS.ERRN                ! Save the error
          ENDIF                                    ! uk scales?
        ENDIF ELSE BEGIN                           ! no error
! don't allow consecutive weighing if weight not changed in uk
!         no support for UK
!         if uk.scanscale.option and               \ if uk scan/scale
!           ts.errn = (80a3056ah) then begin       ! read err. on scale
!         ENDIF                                    ! uk scanner/scale
        ENDIF                                      ! error?
        IF (TS.LINEDATA) THEN BEGIN                ! if linedata set
          TS.TEMP1I4 = -1                          ! flag scale error
        ENDIF                                      ! linedata set
      ENDIF                                        ! not scale zero error
      TS.ER.RETURN = 0                             ! clear err return
    ENDIF                                          ! error encountered
  ENDIF                                            ! not uk scale

END SUB                                            ! java read scale



!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
!
! function strip.mod3$
!
! parms: print buffer to strip control characters from
! Return Value: string w/o control characters
!
!컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION STRIP.MOD3$(printString$) PUBLIC        ! strip.mod3$
  INTEGER*2                                      \
            length,                              \ length of string
            escPos                               ! Escape Position
  STRING                                         \
            printString$,                        \ Passed string to convert
            STRIP.MOD3$,                         \ returned string
            escChar$                             ! Escape Character

  escChar$ = CHR$(1bh)                           ! escape character
  length = LEN(printString$)                     ! get the length
  escPos = MATCH(escChar$, printString$, 1)      ! see if ESC character

  WHILE (length > 1) AND                         \ sufficient size &
        (escPos > 0)                             !  escape in string

    printString$ = LEFT$(printString$,           \ Re-build string
                        (escPos - 1)) +          \
                  RIGHT$(printString$,           \
                         (length -               \
                          (escPos + 1)))         !
    ! re-check values
    length = LEN(printString$)                   ! get the length
    escPos = MATCH(escChar$,                     \
                   printString$, 1)              ! see if ESC character

  WEND                                           ! ok size & escape in string

  ! if just escape left for some reason
  IF (printString$ = escChar$) THEN BEGIN
    printString$ = ""                            ! set space
  ENDIF                                          ! just escape left

  STRIP.MOD3$ = printString$                     ! set return
  escChar$ = ""                                  ! Clean up

END FUNCTION                                     ! strip.mod3$

