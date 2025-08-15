\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMRPWRC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC RECONCILIATION REPORT               */
\/*                                                                */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 1986, 2004 ALL RIGHTS RESERVED     */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*                                                                */
\/*      PERSON RESPONSIBLE = GEIGER, K. H.                        */
\/*                                                                */
\/*      FUNCTION: EAMRPWRC                                        */
\/*                                                                */
\/*                ROUTINES USED BY THE RECONCILIATION REPORT      */
\/*                          PROCEDURE                             */
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
\/*         MODULE SIZE: XXXX...                                   */
\/*                Estimated size in bytes.                        */
\/*                                                                */
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC01                                     */
\/*                                                                */
\/*         PURPOSE: OUTPUT REPORT LINE ENTRY POINT                */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM MODULES SELECTING REPORT TYPE     */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*         SYMBOLIC NAME: SCREEN                                  */
\/*                                                                */
\/*         DESCRIPTION: OUTPUT VIA DISPLAY MANAGER                */
\/*                                                                */
\/*                REPORT LINE                                     */
\/*                                                                */
\/*                ***** PRIME OUTPUT *****                        */
\/*                                                                */
\/*         SYMBOLIC NAME: PRINTER                                 */
\/*                                                                */
\/*         DESCRIPTION: OUTPUT TO PRINTER                         */
\/*                                                                */
\/*                REPORT LINE                                     */
\/*                                                                */
\/*                ***** PRIME OUTPUT *****                        */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRPT15                                */
\/*                                                                */
\/*         DESCRIPTION: SAVED REPORTS FILE                        */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC02                                     */
\/*                                                                */
\/*         PURPOSE: RIGHT JUSTIFY STRING TO SPECIFIED LENGTH      */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC03                                     */
\/*                                                                */
\/*         PURPOSE: LEFT JUSTIFY STRING TO SPECIFIED LENGTH       */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC04                                     */
\/*                                                                */
\/*         PURPOSE: CONCATENATES STRINGS TO SPECIFIED LENGTH      */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC05                                     */
\/*                                                                */
\/*         PURPOSE: CONVERTS KEY DATA TO OPERATOR/TERMINAL ID     */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      ENTRY POINT: RPCFEC10                                     */
\/*                                                                */
\/*         PURPOSE: ROUTINE TO OUTPUT FIRST FOUR LINES OF         */
\/*                  REPORT HEADER TO DESTINATION                  */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM REPORT MODULES                    */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMRDESC                                */
\/*                                                                */
\/*         DESCRIPTION: REPORT DESCRIPTORS FILE.                  */
\/*                                                                */
\/*                ***** FILE INPUT *****                          */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*      EXIT-NORMAL: RETURN TO CALLER                             */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*      EXIT-ERROR: CALLING MODULES ERROR ROUTINE                 */
\/*                                                                */
\/*         RETURN CODE: RP11WERR                                  */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*         ABEND CODES:                                           */
\/*                                                                */
\/*         ERROR MESSAGES:                                        */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*                                                                */
\/*           NAME: MMCFEC05 - CHECKS DISPLAY MANAGER RETURN CODES */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*             PRINTER : OUTPUT TO PRINTER FROM RPCFEC01          */
\/*             SCREEN  : OUTPUT VIA DISPLAY MANAGER               */
\/*             EAMRPT15  REPORT FILE                              */
\/*                                                                */
\/*           REFERENCED:                                          */
\/*                                                                */
\/*             EAMRDESC: REPORT DESCRIPTORS FILE                  */
\/*                                                                */
\/*         CONTROL BLOCKS: NONE                                   */
\/*                                                                */
\/*         MACROS: NONE                                           */
\/*                                                                */
\/*         TABLES: NONE                                           */
\/*                                                                */
\/*      CHANGE ACTIVITY: LEVEL 0                                  */
\/*                                                                */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

%INCLUDE EAMUSED.J86                   ! variable definitions for use
%INCLUDE EAMCFINC.J86                  ! declarations for common functions
%INCLUDE EAMATTRD.J86                  ! declarations for display mgr attributes
%INCLUDE EAMWICEF.J86
%INCLUDE EAMRPINC.J86                  ! variable definitions for calling RPCFECnn

  STRING GLOBAL                        \
    A$,                                \ work string
    ATTR$,                             \ field attribute
    INP.FIELD$(1),                     \ input field array
    SCOPE.PROMPTS$(1),                 \ descriptor array
    HIGH$(1),                          \ upper input value array
    MENU.PROMPTS$(1),                  \ display test
    RPT.TITLE$,                        \ report title
    RPT.TITLES$(1),                    \ report titles array
    STORE$,                            \ store number
    RPT.STR$(1),                       \ line in report
    OUT.STR$,                          \ string to be output
    FILE.KEYS$(1),                     \ keys in file
    COL.HEADINGS$(1),                  \ report column headings
    ERROR.MESSAGES$(1),                \ Line 22 red error messages
    AGENCY.TITLE$,                     \ "AGENCY ID:"
    WIC.ERR$                           ! CBASIC ERROR CODE

  INTEGER*2 GLOBAL                     \
    CURRENT.FIELD,                     \ current screen pos
    FIRST.FIELD,                       \ starting screen pos
    LAST.FIELD,                        \ ending screen pos
    LAST.PAGE,                         \ keep record of last page
    PAGE.SO.FAR,                       \ pages processed so far
    PAGE.LEN,                          \ num lines per page
    NUM.COL.LINES,                     \
    GC.PRINTLEN,                       \ length of print line
    MENU.DISPLAYED,                    \
    NEXT.FIELD,                        \
    GC.PRINTER,                        \ flag if system contains a printer
    MSG.NO,                            \ status/error message number
    TS.LINEDATA2,                      \ OTHER DATA
    MSG.NUM,                           \
    SCOPE,                             \ scope of report
    COMPLETED,                         \ flag indicating that the report ended
    RET.VAL,                           \ ADXERROR Return value parameter
    THRESHOLD.ITEM,                    \ flag for pricing method 3 threshold
    TS.ERRF,                           \ ERROR SESSION NUMBER
    TS.ERRFSAV(1),                     \ STACKED ERROR SESSION NUMBER
    TS.ER.UNEX,                        \ INDICATES UNEXPECTED ERROR HAS OCCURRED
    TS.ER.RETURN,                      \ INDICATES WHETHER APPLICATION WISHES TO
    NOT.FOUND,                         \ item not found flag
    SHOW.ERROR,                        \ error.messages(show.error)
    I                                  ! loop counter

%INCLUDE EAMXXCPY.J86                  ! copyright statement
%INCLUDE EAMADXRT.J86                  ! system services interface
%INCLUDE EAMASMCT.J86                  ! assembler routines
%INCLUDE DMEXTR.J86                    ! display manager external function def.

!****************************************************************************!
!* Subroutine Name: MMCFEC03                                                *!
!* Description    : This subprogram displays a message on line 23.          *!
!****************************************************************************!
SUB MMCFEC03(MSG.NO,MSG.STR$) EXTERNAL
  INTEGER*2 MSG.NO
  STRING MSG.STR$
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC04                                                *!
!* Description    : Common routine accepts data entered from the screen.    *!
!****************************************************************************!
SUB MMCFEC04(INPUT.FIELD$) EXTERNAL
  STRING INPUT.FIELD$
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC05                                                *!
!* Description    : This subprogram handles fatal display manager errors.   *!
!****************************************************************************!
SUB MMCFEC05(RET.VAL,DM.CMD$) EXTERNAL
   INTEGER*2 RET.VAL                   ! return code
   STRING    DM.CMD$                   ! module ID + command ID
END SUB

!****************************************************************************!
!* Function Name  : MMCFEC06$                                               *!
!* Description    : Format a monetary amount with or without a decimal      *!
!*                  place.                                                  *!
!****************************************************************************!
FUNCTION MMCFEC06$(NUM,NEED.DEC,NEED.DEL) EXTERNAL
  STRING MMCFEC06$                     ! returns a formatted numeric str
  INTEGER*4 NUM
  INTEGER*2 NEED.DEC, NEED.DEL
END FUNCTION

!****************************************************************************!
!* Function Name  : MMCFEC11$                                               *!
!* Description    : This function returns a formatted system date and time. *!
!****************************************************************************!
FUNCTION MMCFEC11$ EXTERNAL
  STRING MMCFEC11$
END FUNCTION

!****************************************************************************!
!* Function Name  : MMCFEC14$                                               *!
!* Description    : Format a date and time.                                 *!
!****************************************************************************!
FUNCTION MMCFEC14$(DATETIME$) EXTERNAL ! formats the passed date and time
  STRING MMCFEC14$,DATETIME$
END FUNCTION

!****************************************************************************!
!* Subroutine Name: CHAIN.BACK                                              *!
!* Description    : This routine chains to the calling program.             *!
!****************************************************************************!
SUB CHAIN.BACK EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC99                                                *!
!* Description    : This subprogram writes an error log entry.              *!
!****************************************************************************!
SUB MMCFEC99(IN.MODULE$) EXTERNAL
  STRING    IN.MODULE$
END SUB

!****************************************************************************!
!* Subroutine Name: FIND.FILES                                              *!
!* Description    : This routine builds an array of file names that meet    *!
!*                  the criteria passed in "pattern$".                      *!
!****************************************************************************!
SUB FIND.FILES(PATTERN$) EXTERNAL
  STRING PATTERN$
END SUB

!****************************************************************************!
!* Subroutine Name: REMOVE.LEADING.ZEROS                                    *!
!* Description    : This routine removes leading zeros from a string that   *!
!*                  is passed to it.                                        *!
!****************************************************************************!
SUB REMOVE.LEADING.ZEROS(WORK$) EXTERNAL
  STRING WORK$
END SUB

!****************************************************************************!
!* Subroutine Name: ACCESS.CHAIN.PARAMETERS                                 *!
!* Description    : This routine accesses the chaining variables by         *!
!*                  including the base SA chain module EAMUSE.J86           *!
!****************************************************************************!
SUB ACCESS.CHAIN.PARAMETERS EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: GET.DESCRIPTORS                                         *!
!* Description    : This routine gets and stores the necessary report       *!
!*                  descriptors.                                            *!
!****************************************************************************!
SUB GET.DESCRIPTORS EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC01                                                *!
!* Description    : Display report general screen 3.                        *!
!****************************************************************************!
SUB MMCFEC01 EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: INITIALIZE.DISPLAY.MANAGER                              *!
!* Description    : This routine initializes the display manager.           *!
!****************************************************************************!
SUB INITIALIZE.DISPLAY.MANAGER EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: DISPLAY.FILES                                           *!
!* Description    : This routine displays the file names that were found by *!
!*                  slookup.  It also provides a file name index to use to  *!
!*                  select a file for processing.                           *!
!****************************************************************************!
SUB DISPLAY.FILES EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: SORT.KEYS                                               *!
!* Description    : This routine sorts the hot card key array.              *!
!****************************************************************************!
SUB SORT.KEYS EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: SUB FIND.KEYS                                           *!
!* Description    : This subroutine searches a keyed file, extracts the     *!
!*                  keys, and places the keys into an array.                *!
!****************************************************************************!
SUB FIND.KEYS(REC.LEN, KEY.LEN) EXTERNAL
   INTEGER*2                           \
      REC.LEN,                         \ record length
      KEY.LEN
END SUB

!****************************************************************************!
!* Subroutine Name: EXTRACT.DATA                                            *!
!* Description    : This routine reads the hot card file and populates an   *!
!*                  array with the contents of each record.                 *!
!****************************************************************************!
SUB EXTRACT.DATA(REC.LEN) EXTERNAL
  INTEGER*2 REC.LEN
END SUB

!*****************************************************************************
!                    REPORT COMMON OUTPUT ROUTINES
!****************************************************************************!
!* Subroutine Name: RPCFEC01                                                *!
!* Description    : This routine outputs a report line to the desired       *!
!*                  destination.                                            *!
!****************************************************************************!
SUB RPCFEC01(LINE$) EXTERNAL
  STRING LINE$
END SUB

!****************************************************************************!
!* Subroutine Name: RPCFEC10                                                *!
!* Description    : This subroutine builds and outputs the first four lines *!
!*                  of the header to the selected destination.              *!
!****************************************************************************!
SUB RPCFEC10(TITLE.NO) EXTERNAL
  INTEGER*2 TITLE.NO
END SUB

!****************************************************************************!
!* Subroutine Name: WIC.BUILD.HEADER                                        *!
!* Description    : This subroutine builds and outputs the header for the   *!
!*                  enhanced item movement files.                           *!
!****************************************************************************!
SUB WIC.BUILD.HEADER EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: WIC.BUILD.HEADINGS                                      *!
!* Description    : This routine builds the current report headings.        *!
!****************************************************************************!
SUB WIC.BUILD.HEADINGS(INDEX) EXTERNAL
  INTEGER*2 INDEX
END SUB

!****************************************************************************!
!* Subroutine Name: WIC.OUTPUT                                              *!
!* Description    : This subroutine outputs a passed string to the          *!
!*                  specified destination.                                  *!
!****************************************************************************!
SUB WIC.OUTPUT(LINE$) EXTERNAL
  STRING LINE$
END SUB

!****************************************************************************!
!* Subroutine Name: SETUP.NEWPAGE                                           *!
!* Description    : This routine handles report paging.                     *!
!****************************************************************************!
SUB SETUP.NEWPAGE(INDEX) EXTERNAL
  INTEGER*2 INDEX
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC02                                                *!
!* Description    : Stub to handle function keys.                           *!
!****************************************************************************!
SUB MMCFEC02 PUBLIC
END SUB

!****************************************************************************!
!* Subroutine Name: INITIALIZE.VARIABLES                                    *!
!* Description    : Setup display manager variables.                        *!
!****************************************************************************!
SUB INITIALIZE.VARIABLES

  %INCLUDE EAMATTRI.J86
  %INCLUDE EAMRPCON.J86

  DIM FKEY.CHECK(10)

  FOR I = 1 to 10
    FKEY.CHECK(1)  = OFF0                        ! Set Fn function key off
  NEXT I
  FKEY.CHECK(3)  = ON1                           ! Set F3 function key on

  PAGE = 1                                       ! start with page 1
  NEED.RANGE.CHECK = ON1                         ! do input range check
  HELP.AVAILABLE = OFF0                          ! there is a help screen
  FULL.SCREEN = ON1                              !
  ENTER.PRESSED = OFF0                           ! init as off
  QUIT.PRESSED = OFF0
  ESC.PRESSED = OFF0
  NEED.TEST = OFF0
  GC.PRINTER = -1                                ! printer connected
  VISIBLE$    = "0"
  INVISIBLE$  = "1"
  DIM HIGH$(3)
  HIGH$(1)       = "3"                           ! 1st screen has 3 fields
  HIGH$(2)       = "3"
  LAST.FIELD = 2                                 ! set last field on this page
  NUM.COL.LINES = 2

  IF NOT.FOUND = 0 THEN BEGIN
    DIM INP.FIELD$(6)
    OPEN "EAMSDEF1" RECL 40 AS 36 NODEL          ! open defaults file
    READ #36,112;A$                              ! read menu defaults
    IF LEN(A$) <> 3 THEN BEGIN                   ! if first time ever
      A$ = "11 "                                 ! establish a reasonable default
    ENDIF
    INP.FIELD$(1) = MID$(A$,1,1)                 ! destination
    INP.FIELD$(2) = MID$(A$,2,1)                 ! scope
    INP.FIELD$(3) = MID$(A$,3,1)                 ! (not used)
  ENDIF

  FIRST.FIELD = 1                                ! setup input handler
  CURRENT.FIELD = FIRST.FIELD
  NEXT.FIELD = CURRENT.FIELD + 1
  LAST.PAGE = 0

END SUB ! INITIALIZE.VARIABLES

!****************************************************************************!
!* Subroutine Name: SET.VISIBLE                                             *!
!* Description    : This routine hides or shows fields depending on the     *!
!*                  parameters passed to it.                                *!
!****************************************************************************!
SUB SET.VISIBLE(FIELD, VIS)
  INTEGER*2                                      \
    FIELD,                                       \
    VIS

  IF VIS THEN BEGIN
    CALL MMCFEC05(POSF(FIELD),"WR 3")            !
    ATTR$ = SETF(VISIBLE$)                       ! show
  ENDIF ELSE BEGIN
    CALL MMCFEC05(POSF(FIELD),"WR 3")            !
    ATTR$ = SETF(INVISIBLE$)                     ! hide
  ENDIF
END SUB ! SET.VISIBLE

!****************************************************************************!
!* Subroutine Name: DISPLAY.MENU                                            *!
!* Description    : This routine populates the WIC report screen Num 3      *!
!*                  with the text that is unique to that report.            *!
!****************************************************************************!
SUB DISPLAY.MENU

  INTEGER I

  CALL MMCFEC01

  CALL SET.VISIBLE(100, -1)
  CALL MMCFEC05(PUTF(RPT.TITLES$(4)),"WR 7")     ! auth prod list report
  RPT.TITLE$ = RPT.TITLES$(4)

  CALL SET.VISIBLE(1, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(1)),"WR 7")      ! destination input field

  CALL SET.VISIBLE(2, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(2)),"WR 7")      ! scope input field

  ! scope choices
  FOR I = 10 TO 12
    CALL SET.VISIBLE(I+92, -1)
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(I)),"WR 7") ! range of UPCs
  NEXT I

  CALL SET.VISIBLE(105, -1)
  CALL SET.VISIBLE(115, 0)                       ! not used in this report

  CALL MMCFEC05(POSF(109),"MP 3")                ! yes/no hint
  ATTR$ = SETF(INVISIBLE$)                       ! hide it

  IF DESTINATION <> DISPLAY AND                  \ not going to display and
     COMPLETED THEN BEGIN                        ! report completed
    CALL MMCFEC03(480,NULL$)                     ! show status
    QUIT.PRESSED = OFF0                          ! setup variables
    ENTER.PRESSED = OFF0
    ESC.PRESSED = OFF0
    CURRENT.FIELD = 1
  ENDIF

END SUB ! DISPLAY.MENU

!****************************************************************************!
!* Subroutine Name: BUILD.RPT.LINE                                          *!
!* Description    : This routine builds the report line based on the        *!
!*                  scope of the report.                                    *!
!****************************************************************************!
SUB BUILD.RPT.LINE(I)
  INTEGER*2                                      \
    I                                            !

  STRING                                         \
    WORK$,                                       \
    TEMP$                                        !

  INTEGER*4                                      \
    TEMP,                                        \
    CLAIM.AMT,                                   \
    SETTLE.AMT,                                  \
    ADJ.AMT                                      !

  OUT.STR$ = " "           +                     \
             FILE.KEYS$(I) +                     \ record key
             " "                                 !
BADREAD:

  TEMP$ = MID$(RPT.STR$(I), 10, 5)               ! build claim date and time
  TEMP$ = UNPACK$(TEMP$)                         ! string YY MM DD hh mm
  TEMP$ = MMCFEC14$(TEMP$)                       ! format it
  OUT.STR$ = OUT.STR$ +                          \ add claim date and time
             TEMP$    +                          \
             " "                                 !
  TEMP$ = MID$(RPT.STR$(I), 16, 6)
  TEMP$ = UNPACK$(TEMP$)
  TEMP  = VAL(TEMP$)
  CLAIM.AMT = TEMP                               ! save claim amt
  TEMP$ = MMCFEC06$(TEMP,ON1,ON1)                ! format it
  TEMP$ = RIGHT$("           " + TEMP$, 11)
  OUT.STR$ = OUT.STR$ +                          \ add claim total
             TEMP$                               !
  OUT.STR$ = OUT.STR$               +            \ add status code
             RIGHT$(RPT.STR$(I), 1) +            \
             "  "                                !
  TEMP$ = MID$(RPT.STR$(I), 32, 3)               ! build response date string
  TEMP$ = UNPACK$(TEMP$)                         ! YY MM DD
  TEMP$ = MMCFEC14$(TEMP$+"0000")
  TEMP$ = LEFT$(TEMP$, 8)
  OUT.STR$ = OUT.STR$ +                          \ add response date
             TEMP$    +                          \
             "  "                                !
  TEMP$ = MID$(RPT.STR$(I), 22, 6)
  TEMP$ = UNPACK$(TEMP$)
  TEMP  = VAL(TEMP$)
  SETTLE.AMT = TEMP
  TEMP$ = MMCFEC06$(TEMP,ON1,ON1)
  TEMP$ = RIGHT$("         " + TEMP$, 9)
  OUT.STR$ = OUT.STR$ +                          \ add settlement total
             TEMP$    +                          \
             " "                                 !
  ADJ.AMT = SETTLE.AMT - CLAIM.AMT
  TEMP$ = MMCFEC06$(ADJ.AMT,ON1,ON1)
  TEMP$ = RIGHT$("         " + TEMP$, 9)
  OUT.STR$ = OUT.STR$ +                          \ add adjustment total
             TEMP$    +                          \
             " "                                 !
  TEMP$ = MID$(RPT.STR$(I), 29, 3)               ! build settlement date string
  TEMP$ = UNPACK$(TEMP$)                         ! YY MM DD
  TEMP$ = MMCFEC14$(TEMP$+"0000")
  TEMP$ = LEFT$(TEMP$, 8)
  OUT.STR$ = OUT.STR$ +                          \ add settlement date
             TEMP$                               !

DONE:

END SUB ! BUILD.RPT.LINE

!****************************************************************************!
!* Subroutine Name: BUILD.REPORT                                            *!
!* Description    : Creates and displays/prints/files the entire report.    *!
!****************************************************************************!
SUB BUILD.REPORT

  STRING                                         \
    WORK$

  INTEGER*2                                      \
    OK,                                          \
    SAVE.START,                                  \
    LAST.LINE,                                   \
    FILE1.OPEN,                                  \ flag for closing #1
    LOOP.COUNTER,                                \
    I

  STORE$ = LEFT$(MM.STATUS$, 4)
  RP.DATETIME$ = MMCFEC11$
  FILE1.OPEN = 0

  IF DESTINATION = PRINTER AND NOT GC.PRINTER THEN BEGIN
    MSG.NO = 149

    CALL CHAIN.BACK                              ! chain back to required module
    EXIT SUB
  ENDIF

  IF END #1 THEN RPM

  IF DESTINATION = DISPLAY THEN BEGIN            ! report to screen
    CALL MMCFEC05(DISPD(100), "WR 2")            ! common report screen
    PAGE.LEN  = 20                               ! setup page length
  ENDIF ELSE BEGIN                               ! else report to elsewhere
    PAGE.LEN = 60                                ! setup page length
    IF DESTINATION = PRINTER THEN BEGIN          ! if going to printer
      SHOW.ERROR = 5                             ! "Report was printed."
      OPEN "PRN:" AS 1                           ! open printer
    ENDIF ELSE BEGIN                             ! else
      SHOW.ERROR = 6                             ! "Report was written to EAMRPT15."
      OPEN "EAMRPT15" AS 1 BUFF 8 LOCKED APPEND  ! open file
    ENDIF
    FILE1.OPEN = -1
  ENDIF
  GOTO FILEOK:

RPM:     ! This is run if the report file did not previously exist

  CREATE POSFILE "EAMRPT15" AS 1 BUFF 8 LOCKED MIRRORED ATCLOSE ! if necessary create file
  FILE1.OPEN = -1

FILEOK:

  NO.MAX.PAGE  = OFF0                            ! init vars
  LAST.PAGE    = OFF0
  FIRST.HEADER = ON1
  PAGE.SO.FAR  = 0
  PAGE         = 1

  MAX.PAGE = INT%(REC.CNT + (PAGE.LEN - 7) - 1)  \
                        /(PAGE.LEN - 7)
  IF SCOPE < 3 THEN BEGIN                        ! show only adjusted claims
    I = 0                                        ! number of adjusted claims
    FOR LOOP.COUNTER = 1 TO REC.CNT
      WORK$ = RIGHT$(RPT.STR$(LOOP.COUNTER), 1)  ! "E" or "X"
      IF (SCOPE = 1 AND WORK$ = "E") OR          \ Errors Only
         (SCOPE = 2 AND WORK$ = "X") THEN BEGIN  ! Adjusted Claims Only
        I = I + 1                                ! one more error/adjusted claim
      ENDIF
    NEXT LOOP.COUNTER
    IF I = 0 THEN BEGIN                          ! no records found
      SHOW.ERROR = 3                             ! "Items were not found"
      GOTO DONE
    ENDIF
    MAX.PAGE = INT%(I + (PAGE.LEN - 7) - 1)      \ recalculate because there
                       /(PAGE.LEN - 7)           ! are fewer to show
  ENDIF


  AGENCY.TITLE$ = ""                             ! "AGENCY ID:" not used here
  CALL WIC.BUILD.HEADINGS(7)
  CALL WIC.BUILD.HEADER
  LINE.NO = 7                                    ! init report vars
  QUIT.PRESSED = OFF0
  ENTER.PRESSED = OFF0
  ESC.PRESSED = OFF0
  COMPLETED = OFF0

  FOR LOOP.COUNTER = 1 TO REC.CNT
    IF SCOPE = 1 THEN BEGIN                      ! report errors only
      IF RIGHT$(RPT.STR$(LOOP.COUNTER), 1) = "E" THEN BEGIN
        CALL BUILD.RPT.LINE(LOOP.COUNTER)
        CALL WIC.OUTPUT(OUT.STR$)                ! write to its destination
      ENDIF
    ENDIF ELSE BEGIN
      IF SCOPE = 2 THEN BEGIN                    ! report adjustments
        IF RIGHT$(RPT.STR$(LOOP.COUNTER), 1) = "X" THEN BEGIN
          CALL BUILD.RPT.LINE(LOOP.COUNTER)
          CALL WIC.OUTPUT(OUT.STR$)              ! write to its destination
        ENDIF
      ENDIF ELSE BEGIN                           ! report all claim files
        CALL BUILD.RPT.LINE(LOOP.COUNTER)
        CALL WIC.OUTPUT(OUT.STR$)                ! write to its destination
      ENDIF
    ENDIF

    IF (LINE.NO >= PAGE.LEN) OR                  \ end of page
       (LOOP.COUNTER = REC.CNT) THEN BEGIN       ! last page
      YESORNO = OFF0                             ! turn off checks
      NEED.RANGE.CHECK = OFF0

      IF DESTINATION = DISPLAY THEN BEGIN        ! for display
        IF LOOP.COUNTER < REC.CNT THEN BEGIN     ! not on the last page
          CALL MMCFEC03(100,NULL$)               ! "For more information; Press PgDn"
        ENDIF
        CALL MMCFEC05(POSF(239),"MP 3")          ! position cursor on inp fld

        OK = OFF0                                ! init flag

        WHILE NOT OK                             ! while not set

          ENTER.PRESSED = ON1                    ! disallow ENTER here
          WHILE ENTER.PRESSED
            CALL MMCFEC04(WORK$)                 ! get input
          WEND

          OK = ON1                               ! set flag

          IF PGDN.PRESSED THEN BEGIN             ! if PgDn
            LAST.LINE = LINE.NO - 1
            PGDN.PRESSED = OFF0

            IF PAGE = MAX.PAGE THEN BEGIN        ! check for last page and
              PAGE.LEN = (REC.CNT - LOOP.COUNTER) + 7 ! num lines on last page
              LAST.PAGE = ON1                    ! show that it is last page
            ENDIF
          ENDIF ELSE BEGIN
            IF PGUP.PRESSED THEN BEGIN           ! if PgUp
              LOOP.COUNTER = LOOP.COUNTER - (LINE.NO - 7) - 13 ! reset parms
              IF LOOP.COUNTER < 0 THEN BEGIN     ! do not backup before beginning
                LOOP.COUNTER = 0                 ! NEXT increments it to 1
              ENDIF
              PGUP.PRESSED = OFF0

              IF PAGE.LEN < 20 THEN PAGE.LEN = 20 ! ensure num lines is correct
            ENDIF ELSE BEGIN
              IF (ESC.PRESSED) OR                \ finished
                 (QUIT.PRESSED) OR               \
                 (ENTER.PRESSED) THEN BEGIN      !
                COMPLETED = ON1
                LOOP.COUNTER = REC.CNT
              ENDIF ELSE BEGIN
                OK = OFF0                        ! turn off flag
                CALL MMCFEC03(109,NULL$)         ! invalid function key
              ENDIF ! invalid key
            ENDIF ! not PgUp
          ENDIF ! not PgDn
        WEND ! not OK
      ENDIF ! DESTINATION = DISPLAY
      IF LOOP.COUNTER < REC.CNT THEN BEGIN       ! not on the last page
        CALL SETUP.NEWPAGE(7)                    ! put up titles
      ENDIF
    ENDIF ! end of a page
  NEXT LOOP.COUNTER

DONE:
  IF FILE1.OPEN THEN                             \ printer or file open?
    CLOSE 1

END SUB ! BUILD.REPORT

!****************************************************************************!
!* Subroutine Name: HANDLE.USER.INPUT                                       *!
!* Description    : This routine gets the user input, determining option    *!
!*                  changes.                                                *!
!****************************************************************************!
SUB HANDLE.USER.INPUT

  INTEGER*2                                      \
    DATA.NOT.OK

  WHILE NOT (QUIT.PRESSED OR ENTER.PRESSED OR ESC.PRESSED)

    CALL SET.VISIBLE(250, -1)                    ! error message line
    IF SHOW.ERROR THEN BEGIN                     ! is an error pending?
      CALL MMCFEC05(PUTF(ERROR.MESSAGES$(SHOW.ERROR)),"MP 7") ! show error msg
      SHOW.ERROR = 0                             ! clear flag for next time
    ENDIF ELSE BEGIN                             ! no error yet
      CALL MMCFEC05(PUTF(""),"MP 7")             ! blank the error message line
    ENDIF

    ! set up input field parameters
    NEED.RANGE.CHECK = ON1                       ! do a range check on input
    LOW.END$ = "1"                               ! set ranges
    HIGH.END$ = HIGH$(CURRENT.FIELD)
    ASTRK.OK  = OFF0                             ! no * or Y/N
    YESORNO = OFF0

    IF CURRENT.FIELD = 3 THEN BEGIN              ! if past last field
      CURRENT.FIELD = 1                          ! make it first field
    ENDIF

    ! get user input
    CALL MMCFEC05(POSF(CURRENT.FIELD),"WR 3")    ! position cursor
    CALL MMCFEC04(INP.FIELD$(CURRENT.FIELD))     ! get field input

    ! display prompt lines depending on the scope selection that is made

    IF NOT (ESC.PRESSED OR QUIT.PRESSED) THEN BEGIN

      ! process terminating key

      RET.VAL = ON1
      IF TAB.PRESSED THEN BEGIN                  ! adjust cursor position
        CURRENT.FIELD = MOD(CURRENT.FIELD,LAST.FIELD) + 1 ! bump field loc
      ENDIF ELSE BEGIN
        IF END.PRESSED THEN BEGIN                ! end key
          CURRENT.FIELD = LAST.FIELD             ! goto last field
        ENDIF ELSE BEGIN
          IF HOME.PRESSED THEN BEGIN             ! home key
            CURRENT.FIELD = 1                    ! goto first field
          ENDIF ELSE BEGIN
            IF BACKTAB.PRESSED THEN BEGIN        ! back tab
              IF CURRENT.FIELD = 3 THEN BEGIN    ! past last field
                CURRENT.FIELD = 1                ! set up for first field
              ENDIF ELSE BEGIN
                IF CURRENT.FIELD = 1 THEN BEGIN  ! on first field
                  CURRENT.FIELD = LAST.FIELD     ! set up for last field
                ENDIF ELSE BEGIN
                  CURRENT.FIELD = CURRENT.FIELD -1 ! else back up one field
                ENDIF
              ENDIF
            ENDIF                                !
          ENDIF
        ENDIF
      ENDIF

      IF ENTER.PRESSED THEN BEGIN                ! enter pressed
        WRITE #36,112;INP.FIELD$(1) +            \ update menu defaults
                      INP.FIELD$(2) + INP.FIELD$(3)
        SCOPE         = VAL(INP.FIELD$(2))       ! set scope
        DESTINATION   = VAL(INP.FIELD$(1))       ! set destination
        ENTER.PRESSED = ON1
        QUIT.PRESSED  = OFF0

        IF (DESTINATION <> DISPLAY) AND          \
           (MSG.NUM = 0)   THEN BEGIN
          MSG.NUM = 480                          ! Report processing done
        ENDIF
      ENDIF
    ENDIF
  WEND

END SUB ! HANDLE.USER.INPUT

!****************************************************************************!
!* MAIN                                                                     *!
!****************************************************************************!

  ON ERROR GOTO TRAP.IT

  FILENAME$ = "WIC1:EAMWERCL.DAT"
  DIM FILE.KEYS$(5000)

  CALL ACCESS.CHAIN.PARAMETERS                   ! get the chain parameters

  CALL INITIALIZE.VARIABLES                      ! init the variables

  CALL GET.DESCRIPTORS                           ! report descriptors

  CALL INITIALIZE.DISPLAY.MANAGER                ! init the display manager

REDO:
  MENU.DISPLAYED = 0

  WHILE NOT (QUIT.PRESSED OR ESC.PRESSED)
    IF NOT MENU.DISPLAYED THEN BEGIN             ! not already on display
      CALL DISPLAY.MENU                          ! display report menu
    ENDIF ELSE BEGIN
      MENU.DISPLAYED = 0                         ! turn of flag
    ENDIF

    CALL HANDLE.USER.INPUT                       ! get user input

    IF ENTER.PRESSED THEN BEGIN
      CALL FIND.KEYS(35, 8)                      ! pass rec len and key len
      CALL SORT.KEYS                             ! sort the keys
      CALL EXTRACT.DATA(35)                      ! pass record length
      CALL BUILD.REPORT
      ENTER.PRESSED = OFF0
      QUIT.PRESSED = OFF0
      END.PRESSED = OFF0
    ENDIF
  WEND
  CLOSE 36

  CALL CHAIN.BACK                                ! chain back to required module
  STOP

TRAP.IT:

  INWE.ERR$ = ERR
  INWE.ERRN% = ERRN
  INWE.ERRF% = ERRF%

  IF (INWE.ERR$ = "XI") AND (INWE.ERRF% = 4) THEN BEGIN
    WIC.ERR$ = "EF"
    RESUME
  ENDIF

  IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 7) OR  \
     (INWE.ERR$ = "EF") AND (INWE.ERRF% = 7) THEN BEGIN
    RESUME
  ENDIF

  IF (INWE.ERR$ = "EF") AND                      \
     ((INWE.ERRF% = 4) OR                        \
      (INWE.ERRF% = 10) OR                       \
      (INWE.ERRF% = 8)) THEN BEGIN
    WIC.ERR$ = "EF"
    RESUME
  ENDIF

  IF (INWE.ERR$ = "DF") AND (INWE.ERRF% = 1) THEN BEGIN
    CLOSE 1
    RESUME RETRY
  ENDIF

  IF (INWE.ERR$ = "OE") AND (INWE.ERRN% = 82) THEN BEGIN
    RESUME
  ENDIF

  CALL MMCFEC99("WR")                            ! log an error message
  STOP                                           ! do not continue

! End MAIN
