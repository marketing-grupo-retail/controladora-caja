\/* TIME STAMP BLOCK ***********************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMWERMC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC EBT REPORTS MAIN MENU               */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*       5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"    */
\/*  (c) COPYRIGHT IBM CORP 1986,2004 ALL RIGHTS RESERVED LICENSED */
\/*       MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT           */
\/*       INSTRUCTIONS FORM NUMBER G120-2083                       */
\/*                                                                */
\/*      STATUS: RELEASE 1 LEVEL 0                                 */
\/*                                                                */
\/*      PERSON RESPONSIBLE =  GEIGER, KENNETH                     */
\/*                                                                */
\/*      FUNCTION: EAMRPMMC                                        */
\/*                                                                */
\/*             Allows user to select one of four WIC EBT reports. */
\/*                reports.                                        */
\/*                                                                */
\/*      NOTES:                                                    */
\/*                                                                */
\/*         DEPENDENCIES: NONE                                     */
\/*                                                                */
\/*         RESTRICTIONS: NONE                                     */
\/*                                                                */
\/*         REGISTER CONVENTIONS:                                  */
\/*                                                                */
\/*            RESTRICTED REGISTERS = N/A                          */
\/*                                                                */
\/*            REGISTERS USED = N/A                                */
\/*                                                                */
\/*      MODULE TYPE: CBASIC                                       */
\/*                                                                */
\/*         PROCESSOR: CBASIC COMPILER                             */
\/*         MODULE SIZE: 25152  bytes                              */
\/*                                                                */
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPMMEC01                                     */
\/*                                                                */
\/*         PURPOSE: PROGRAM LABEL                                 */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CONTROL TRANSFERRED FROM FEATURES SELECTION   */
\/*                                          MENU.                 */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME: KEYBOARD                                */
\/*                                                                */
\/*         DESCRIPTION: User input data supplied via screen       */
\/*                                                                */
\/*                Used to get user choice of WIC report (1 of 4). */
\/*                                                                */
\/*                ***** RESPONSE INPUT *****                      */
\/*                                                                */
\/*         SYMBOLIC NAME: MMCHAINP                                */
\/*                                                                */
\/*         DESCRIPTION: Chaining parameters                       */
\/*                                                                */
\/*                ***** FILE INPUT *****                          */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMEER1S                                */
\/*                                                                */
\/*         DESCRIPTION: Reports Display File                      */
\/*                                                                */
\/*                This file contains all of the screens used      */
\/*                in this menu and the reports.                   */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*         SYMBOLIC NAME: SCREEN                                  */
\/*                                                                */
\/*         DESCRIPTION: Output to screens via Display Manager     */
\/*                                                                */
\/*                   EFT Reports Main Menu.                       */
\/*                                                                */
\/*                ***** FILE/REQUEST OUTPUT *****                 */
\/*                                                                */
\/*         SYMBOLIC NAME: MMCHAINP                                */
\/*                                                                */
\/*         DESCRIPTION: Chaining parameters                       */
\/*                                                                */
\/*                ***** PRIME OUTPUT *****                        */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMEESDF                                */
\/*                                                                */
\/*         DESCRIPTION: Screen default file                       */
\/*                                                                */
\/*                The user input is written to this file.         */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPMMEW01                                     */
\/*                                                                */
\/*         PURPOSE: Entry point on error condition.               */
\/*                                                                */
\/*                ***** NO RETURN *****                           */
\/*                                                                */
\/*         LINKAGE: CONTROL TRANSFERRED FROM RUN-TIME ROUTINES.   */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME: RP11WERR                                */
\/*                                                                */
\/*         DESCRIPTION: Error code                                */
\/*                                                                */
\/*                Error code passed from run-time routines.       */
\/*                                                                */
\/*                ***** PRIME INPUT *****                         */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      EXIT-NORMAL: Via a chain back to the main menu application*/
\/*                   or the accounting functions.                 */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*      EXIT-ERROR : RPMMEW01                                     */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*         ABEND CODES:                                           */
\/*                                                                */
\/*         ERROR MESSAGES:  Invalid function key                  */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*             PRINTER : Output device                            */
\/*             SCREEN  : Output device                            */
\/*             MMCHAINP: Chaining parameters                      */
\/*             EAMEESDF: Screen default file                      */
\/*                                                                */
\/*           REFERENCED:                                          */
\/*                                                                */
\/*                                                                */
\/*         CONTROL BLOCKS: NONE                                   */
\/*                                                                */
\/*         MACROS: NONE                                           */
\/*                                                                */
\/*         TABLES: NONE                                           */
\/*                                                                */
\/*      CHANGE ACTIVITY: LEVEL 0                                  */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/
\
\INCLUDE EAMUSED.J86          !variable definitions for use
\INCLUDE EAMATTRD.J86         !variable definitions for display manager
\INCLUDE EAMCFINC.J86         !variable definitions for calling MMCFECnn
\INCLUDE EAMRPINC.J86         !variable definitions for calling RPCFECnn
\INCLUDE EAMERRNH.J86         !variable definitions for M. T. records
\INCLUDE EAMUSE.J86           !use statement
\INCLUDE DMEXTR.J86           !Display manager external routines
\INCLUDE EAMCHAIN.J86         !Chain to next application
\INCLUDE EAMRPCON.J86         !initialise report variables
\INCLUDE EAMATTRI.J86         !initialise display manager variables
\INCLUDE EAMXXCPY.J86         !Copyright Statement
\ REM!!****************** B B D L *********************************!!
\
\ REM!!**************** OVERVIEW OF PROGRAM ***********************!!
\ REM!!                                                            !!
\ REM!!  Provides Main Menu for WIC EBT reports.                   !!
\ REM!!                                                            !!
\ REM!!  This program is chained to from Electronic Debit Support. !!
\ REM!!                                                            !!
\ REM!!  It allows the user to select one of four WIC  reports.    !!
\ REM!!                                                            !!
\ REM!!************************************************************!!
\
\ REM!!**********************  VARIABLES  *************************!!
\
\ REM!!**************** FUNCTIONS AND SUBPROGRAMS *****************!!
\
\ REM!!********************** TRACE *******************************!!
\ REM!! Passes a message to the printer.                           !!
\ REM!!                                                            !!
\ REM!! SUB TRACE (string)                                         !!
\ REM!!                                                            !!
\ REM!!     open printer session                                   !!
\ REM!!     write string to printer                                !!
\ REM!!     close printer session                                  !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!
\
\ REM!!****************** CHAIN.SUB *******************************!!
\ REM!! This subprogram chains to a specified program.             !!
\ REM!!                                                            !!
\ REM!! SUB CHAIN.SUB(TO.WHERE$)                                   !!
\ REM!!                                                            !!
\ REM!!   set up name of program to chain to                       !!
\ REM!!   CHAIN to that program                                    !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!
\ REM!!*******************  EXIT  *********************************!!
\ REM!!************************************************************!!
\
\ REM!!********************* MMCFEC03 *****************************!!
\ REM!! This subprogram displays a message on line 23.             !!
\ REM!!************************************************************!!
\
\ REM!!****************** MMCFEC04 ********************************!!
\ REM!! This subprogram determines the data termination key.       !!
\ REM!!************************************************************!!
\
\ REM!!****************** MMCFEC05 ********************************!!
\ REM!! This subprogram handles fatal display errors.              !!
\ REM!!************************************************************!!
\
\ REM!!****************** MMCFEC99 ********************************!!
\ REM!! This subprogram writes an error log entry.                 !!
\ REM!!************************************************************!!
\
\ REM!!****************** MMCFEC01 ********************************!!
\ REM!! This subprogram is called by the common routine MMCFEC04   !!
\ REM!! and by this  program to re-display a screen.               !!
\ REM!!                                                            !!
\ REM!! SUB MMCFEC01 PUBLIC                                        !!
\ REM!!                                                            !!
\ REM!!     CALL DISPD to put up display                           !!
\ REM!!     CALL MMCFEC05 to check for error                       !!
\ REM!!                                                            !!
\ REM!!     write output to input field                            !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!
\
\ REM!!****************** MMCFEC02 ********************************!!
\ REM!! This subprogram is called by the common routine MMCFEC04   !!
\ REM!! to cope with miscellaneous keys.                           !!
\ REM!!                                                            !!
\ REM!! SUB MMCFEC02 PUBLIC                                        !!
\ REM!!                                                            !!
\ REM!!   call MMCFEC03       (ie. no additional F-keys allowed)   !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!
\
\ REM!!**************** MAIN LINE START  **************************!!
\ REM!!                                                            !!
\ REM!!  ON ERROR GOTO RPMMEW01                                    !!
\ REM!!                                                            !!
\ REM!!  initialise constants                                      !!
\ REM!!                                                            !!
\ REM!!  initialise display manager                                !!
\ REM!!  put up selection screen                                   !!
\ REM!!  set up input range checking and various dm flags          !!
\ REM!!  position cursor                                           !!
\ REM!!                                                            !!
\ REM!!  While neither quit nor escape pressed                     !!
\ REM!!                                                            !!
\ REM!!    if message to be shown on line 23                       !!
\ REM!!      display it                                            !!
\ REM!!      reset flag                                            !!
\ REM!!    endif                                                   !!
\ REM!!                                                            !!
\ REM!!    display field default                                   !!
\ REM!!    get field input                                         !!
\ REM!!                                                            !!
\ REM!!    if enter was pressed                                    !!
\ REM!!      if 1 chosen set destination to Hot card report        !!
\ REM!!      if 2 chosen set destination to Authorized items Rpt   !!
\ REM!!      if 3 chosen set destination to Max price report       !!
\ REM!!      if 4 chosen set destination to Reconciliation Rpt     !!
\ REM!!                                                            !!
\ REM!!      update screen defaults file                           !!
\ REM!!                                                            !!
\ REM!!      call CHAIN.SUB (chains to destination)                !!
\ REM!!    endif                                                   !!
\ REM!!                                                            !!
\ REM!!  Wend                                                      !!
\ REM!!                                                            !!
\ REM!!  close menu's display file                                 !!
\ REM!!                                                            !!
\ REM!!  CHAIN to Feature Reports Selection(parent menu)           !!
\ REM!!                                                            !!
\ REM!!************************************************************!!
\ REM!!*******************  EXIT  *********************************!!
\ REM!!************************************************************!!
\
\ REM!!**********************  CONSTANTS  *************************!!
\
\ REM!!*************** RUN TIME ERROR PROCESSING ******************!!
\ REM!! This block of code executes when a run-time error is       !!
\ REM!! detected by the ON ERROR statement.                        !!
\ REM!! Errors will be logged and the process will be terminated.  !!
\ REM!!                                                            !!
\ REM!! RPMMEW01:                                                  !!
\ REM!!                                                            !!
\ REM!!   Call MMCFEC99 to log the error                           !!
\ REM!!   Chain back to Feature reports selection.                 !!
\ REM!!                                                            !!
\ REM!!*********************  EXIT  *******************************!!
\ REM!!************************************************************!!
\
\ REM!!**************** END OF BBDL *******************************!!

\ REM!!**************** OVERVIEW OF PROGRAM ***********************!!
\ REM!!                                                            !!
\ REM!!  Provides Main Menu for WIC EBT reports.                   !!
\ REM!!                                                            !!
\ REM!!  This program is chained to from Feature Report Selection. !!
\ REM!!                                                            !!
\ REM!!  It allows the user to select one of four wic reports.     !!
\ REM!!                                                            !!
\ REM!!************************************************************!!

\ REM!!**********************  VARIABLES  *************************!!

%INCLUDE EAMUSED.J86             !variable definitions for use
%INCLUDE EAMATTRD.J86            !variable definitions for display manager
%INCLUDE EAMCFINC.J86            !variable definitions for calling MMCFECnn
%INCLUDE EAMRPINC.J86            !variable definitions for calling RPCFECnn

INTEGER*4      RET.VAL,          \Display Manager return value
               POSS,             \field cursor is in
               MSG.NO            !mmcfec03 message number

STRING         CHAINTO$,         \chaining destination
               FIELD$,           \input field
               DISPLAY.FILE$,    \display filename for Display Manager
               MSG.STR$          !string passed to mmcfec03

\ REM!!******************  END OF VARIABLES  **********************!!


\ REM!!**************** FUNCTIONS AND SUBPROGRAMS *****************!!

%INCLUDE EAMXXCPY.J86            !COPYRIGHT
%INCLUDE DMEXTR.J86              !Display manager external routines
%INCLUDE EAMERRNH.J86            !gives errn in hex as ERRNHEX$

\ REM!!********************** TRACE *******************************!!
\ REM!! Passes a message to the printer.                           !!
\ REM!!                                                            !!
\ REM!! SUB TRACE (string)                                         !!
\ REM!!                                                            !!
\ REM!!     open printer session                                   !!
\ REM!!     write string to printer                                !!
\ REM!!     close printer session                                  !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!

SUB TRACE(TRACE.STR$)
  STRING TRACE.STR$, LINEFEED$

  OPEN "PRN:" AS 16
  LINEFEED$ = CHR$(13) + CHR$(10)
  WRITE FORM "C80, C2"; #16; TRACE.STR$, LINEFEED$
  CLOSE 16

END SUB

\ REM!!****************** CHAIN.SUB *******************************!!
\ REM!! This subprogram chains to a specified program.             !!
\ REM!!                                                            !!
\ REM!! SUB CHAIN.SUB(TO.WHERE$)                                   !!
\ REM!!                                                            !!
\ REM!!   set up name of program to chain to                       !!
\ REM!!   CHAIN to that program                                    !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!
\ REM!!*******************  EXIT  *********************************!!
\ REM!!************************************************************!!

SUB CHAIN.SUB(TO.WHERE$)

  STRING                                         \
    TO.WHERE$,                                   \Name of program to chain to
    CHAINTO$                                     \ "    "      "

  MM14PCN1$ = MM.CHAINFROM$
  CHAINTO$ = TO.WHERE$
  MM.CHAINFROM$ = "EAMWERML.286"

%INCLUDE  EAMCHAIN.J86

END SUB

\ REM!!********************* MMCFEC03 *****************************!!
\ REM!! This subprogram displays a message on line 23.             !!
\ REM!!************************************************************!!

SUB MMCFEC03(MSG.NO,MSG.STR$) EXTERNAL
  INTEGER*2  MSG.NO                              ! number of msg in message file
  STRING     MSG.STR$
END SUB

\ REM!!****************** MMCFEC04 ********************************!!
\ REM!! This subprogram determines the data termination key.       !!
\ REM!!************************************************************!!

SUB MMCFEC04(FIELD$) EXTERNAL
  STRING FIELD$
END SUB

\ REM!!****************** MMCFEC17 ********************************!!
\ REM!! This subprogram flags whether a store close in in progress.!!
\ REM!!************************************************************!!

FUNCTION  MMCFEC17 EXTERNAL
  INTEGER*2 MMCFEC17
END FUNCTION

\ REM!!****************** MMCFEC05 ********************************!!
\ REM!! This subprogram handles fatal display errors.              !!
\ REM!!************************************************************!!

SUB MMCFEC05(R.VALUE,DM.CMD$) EXTERNAL
  INTEGER*2   R.VALUE                            !Return code from Display Manager
  STRING      DM.CMD$                            !Module id "MM" + function called
END SUB

\ REM!!****************** MMCFEC99 ********************************!!
\ REM!! This subprogram writes an error log entry.                 !!
\ REM!!************************************************************!!

SUB MMCFEC99(IN.MODULE$) EXTERNAL
  STRING  IN.MODULE$
END SUB

\ REM!!****************** MMCFEC01 ********************************!!
\ REM!! This subprogram is called by the common routine MMCFEC04   !!
\ REM!! and by this  program to re-display a screen.               !!
\ REM!!                                                            !!
\ REM!! SUB MMCFEC01 PUBLIC                                        !!
\ REM!!                                                            !!
\ REM!!     CALL DISPD to put up display                           !!
\ REM!!     CALL MMCFEC05 to check for error                       !!
\ REM!!                                                            !!
\ REM!!     write output to input field                            !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!

SUB MMCFEC01 PUBLIC

  RET.VAL =  DISPD(1)
  CALL MMCFEC05(RET.VAL,"MM 2")                  !to check for error

  RET.VAL = POSF(1)                              !to position cursor
  CALL MMCFEC05(RET.VAL,"MM 3")                  !to check for error
  RET.VAL =  PUTF(FIELD$)                        !to write field to screen
  CALL MMCFEC05(RET.VAL,"MM 7")                  !to check for error

END SUB

\ REM!!****************** MMCFEC02 ********************************!!
\ REM!! This subprogram is called by the common routine MMCFEC04   !!
\ REM!! to cope with miscellaneous keys.                           !!
\ REM!!                                                            !!
\ REM!! SUB MMCFEC02 PUBLIC                                        !!
\ REM!!                                                            !!
\ REM!!   call MMCFEC03       (ie. no additional F-keys allowed)   !!
\ REM!!                                                            !!
\ REM!! END SUB                                                    !!
\ REM!!************************************************************!!

SUB MMCFEC02 PUBLIC

  CALL MMCFEC03(109,NULL$)                       !"Invalid function key" message

END SUB

\ REM!!**************** MAIN LINE START  **************************!!
\ REM!!                                                            !!
\ REM!!  ON ERROR GOTO RPWEEW01                                    !!
\ REM!!                                                            !!
\ REM!!  initialise constants                                      !!
\ REM!!                                                            !!
\ REM!!  initialise display manager                                !!
\ REM!!  put up selection screen                                   !!
\ REM!!  set up input range checking and various dm flags          !!
\ REM!!  position cursor                                           !!
\ REM!!                                                            !!
\ REM!!  While neither quit nor escape pressed                     !!
\ REM!!                                                            !!
\ REM!!    if message to be shown on line 23                       !!
\ REM!!      display it                                            !!
\ REM!!      reset flag                                            !!
\ REM!!    endif                                                   !!
\ REM!!                                                            !!
\ REM!!    display field default                                   !!
\ REM!!    get field input                                         !!
\ REM!!                                                            !!
\ REM!!    if enter was pressed                                    !!
\ REM!!      if 1 chosen set destination to Hot card               !!
\ REM!!      if 2 chosen set destination to Authorized item        !!
\ REM!!      if 3 chosen set destination to Max price              !!
\ REM!!      if 4 chosen set destination to reconciliation         !!
\ REM!!                                                            !!
\ REM!!      update screen defaults file                           !!
\ REM!!                                                            !!
\ REM!!      call CHAIN.SUB (chains to destination)                !!
\ REM!!    endif                                                   !!
\ REM!!                                                            !!
\ REM!!  Wend                                                      !!
\ REM!!                                                            !!
\ REM!!  close menu's display file                                 !!
\ REM!!                                                            !!
\ REM!!  CHAIN to Reports Selection Menu   (parent menu)           !!
\ REM!!                                                            !!
\ REM!!************************************************************!!
\ REM!!*******************  EXIT  *********************************!!
\ REM!!************************************************************!!

 RPWEEC01:
  ON ERROR GOTO RPWEEW01:

\ REM!!**********************  CONSTANTS  *************************!!

%INCLUDE EAMUSE.J86                              !use statement
%INCLUDE EAMRPCON.J86                            !initialise report variables
%INCLUDE EAMATTRI.J86                            !initialise display manager vars

\ REM!!******************  END OF CONSTANTS  **********************!!

  RET.VAL = INITDM(MM11PDMA$)                    !initialise display manager
  CALL MMCFEC05(RET.VAL,"MM 4")                  !check for error
  DISPLAY.FILE$ = "EAMRPP3S"
  DM.DISPFILE$ = "RPM"                           !this unique bit of display file's
                                                 !name forms part of Display
                                                 !Manager error messages
  RET.VAL = OPNDIS(DISPLAY.FILE$)                !open WIC reports display file
  CALL MMCFEC05(RET.VAL,"MM 6")                  !check for error

AFTER.36.READ:

  LOW.END$         = "1"                         !set range check as between 1
  HIGH.END$      = "4"                           !and 4
  NEED.RANGE.CHECK = ON1
  BLANK.OK         = OFF0                        !some input required

  HELP.AVAILABLE  = ON1                          !flag - help is available
  FULL.SCREEN     = ON1
  HELP.SCR.NO     = 101                          !help scr. no. in display file
  MAX.HELP        = 2
  MAX.PAGE        = 1                            !only one selection screen page
  MSG.NO          = 0                            !set msg.no: when 0, no message
  MSG.STR$        = NULL$                        !set msg.str$
  POSS            = 1                            !input field number (where to
                                                 !  stick the cursor)
  CALL MMCFEC01                                  !display reports selection screen

  ENTER.PRESSED   = OFF0
  QUIT.PRESSED    = OFF0
  ESC.PRESSED     = OFF0

  WHILE NOT (QUIT.PRESSED OR ESC.PRESSED)        !until user wants to quit:
    IF MSG.NO <> 0 THEN BEGIN                    !if message to be shown
        CALL MMCFEC03(MSG.NO,MSG.STR$)           !display it
        MSG.STR$ = NULL$
        MSG.NO   = 0                             !reset flag to "no message"
    ENDIF

    CALL MMCFEC05 (PUTF (FIELD$),"MM 7")         !display field's default

    CALL MMCFEC04 (FIELD$)                       !get field input

    IF NOT MMCFEC17 THEN BEGIN                   !if store close not happening
       IF ENTER.PRESSED THEN BEGIN               !if enter was pressed
         IF FIELD$ = "1" THEN BEGIN              !  if field input was "1"
            CHAINTO$ = "EAMRPHCL.286"            !    set so chains to option 1
         ENDIF ELSE BEGIN
           IF FIELD$ = "2" THEN BEGIN            !  if field input was "2"
              CHAINTO$ = "EAMRPAPL.286"          !    set so chains to option 2
           ENDIF ELSE BEGIN
             IF FIELD$ = "3" THEN BEGIN          !  if field input was "3"
                CHAINTO$ = "EAMRPMPL.286"        !    set so chains to option 3
             ENDIF ELSE BEGIN
               IF FIELD$ = "4" THEN BEGIN        !  if field input was 4
                  CHAINTO$ = "EAMRPWRL.286"      !  chains to: EFT Activity Report
               ENDIF                             !
             ENDIF
           ENDIF
         ENDIF
         CALL CHAIN.SUB(CHAINTO$)                !chain to set destination
       ENDIF
    ENDIF

  WEND                                           !when user wants to quit:

  CALL CLSDIS                                    !close display file

  IF QUIT.PRESSED THEN BEGIN
    CHAINTO$ = "EAMRP11L.286"                    ! Report Main Menu
  ENDIF ELSE BEGIN
    CHAINTO$ = "EAMMM00L.286"                    !SMA main menu
  ENDIF

  CALL CHAIN.SUB(CHAINTO$)                       !chain out

\ REM!!*************** RUN TIME ERROR PROCESSING ******************!!
\ REM!! This block of code executes when a run-time error is       !!
\ REM!! detected by the ON ERROR statement.                        !!
\ REM!! Errors will be logged and the process will be terminated.  !!
\ REM!!                                                            !!
\ REM!! RPMMEW01:                                                  !!
\ REM!!                                                            !!
\ REM!!   Call MMCFEC99 to log the error                           !!
\ REM!!   Chain back to Report main menu.                          !!
\ REM!!                                                            !!
\ REM!!*********************  EXIT  *******************************!!
\ REM!!************************************************************!!

RPWEEW01:

  IF ERR <> "OE" AND ERRF% = 36 THEN BEGIN       !ERROR READING SCREEN DEFAULTS
    FIELD$ = "1"
    RESUME AFTER.36.READ
  ENDIF

  CALL MMCFEC99("MM")                            !log error

  CALL CHAIN.SUB("EAMWERML.286")                 !chain to parent menu

  END
