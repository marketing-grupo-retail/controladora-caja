\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMRPHCC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC EBT HOT CARD LIST REPORT            */
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
\/*      FUNCTION: EAMRPHCC                                        */
\/*                                                                */
\/*                ROUTINES USED BY APPROVED PRODUCT LIST          */
\/*                        REPORT PROCEDURE                        */
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
    CARD.NUM$,                         \ single item enquiry number
    RPT.STR$(1),                       \ line in report
    OUT.STR$,                          \ string to be output
    FILE.KEYS$(1),                     \ keys in file
    COL.HEADINGS$(1),                  \ report column headings
    RECORD$,                           \
    ERROR.MESSAGES$(1),                \ Line 22 red error messages
    WIC.ERR$                           ! CBASIC ERROR CODE


  INTEGER*2 GLOBAL                     \
    CURRENT.FIELD,                     \ current screen pos
    FIRST.FIELD,                       \ starting screen pos
    LAST.FIELD,                        \ ending screen pos
    LAST.PAGE,                         \ keep record of last page
    PAGE.SO.FAR,                       \ pages processed so far
    PAGE.LEN,                          \ num lines per page
    GC.PRINTLEN,                       \ length of print line
    NEXT.FIELD,                        \
    GC.PRINTER,                        \ flag if system contains a printer
    NUM.COL.LINES,                     \
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
    INVALID.CARD,                      \
    NOT.FOUND,                         \ item not found flag
    SHOW.ERROR,                        \ error.messages(show.error)
    I                                  ! loop counter

  INTEGER*4 GLOBAL                     \
    FCNT,                              \ SLOOKUP number of files found
    RET.CODE                           ! return code from slookup

%INCLUDE EAMXXCPY.J86                  ! copyright statement
%INCLUDE EAMADXRT.J86                  ! system services interface
%INCLUDE EAMASMCT.J86                  ! assembler routines
%INCLUDE EAMWEESC.J86                  ! c routines to handle large hcl file
%INCLUDE SYSFUNC.J86                   ! def for slookup
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
!* Description    : This routine sorts the hot card key array               *!
!****************************************************************************!
SUB SORT.KEYS EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: FIND.KEYS                                               *!
!* Description    : This subroutine searches a keyed file, extracts the     *!
!*                  keys and places them into an array.                     *!
!****************************************************************************!
SUB FIND.KEYS(REC.LEN, KEY.LEN) EXTERNAL
  INTEGER*2                            \
    REC.LEN,                           \ record length
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
  HIGH$(2)       = "2"
  LAST.FIELD = 3                                 ! set last field on this page
  NUM.COL.LINES = 1                              ! col heading rows

  DIM INP.FIELD$(6)
  OPEN "EAMSDEF1" RECL 40 AS 36 NODEL            ! open defaults file
  READ #36,109;A$                                ! read menu defaults
  IF LEN(A$) <> 3 THEN BEGIN                     ! if first time ever
    A$ = "11N"                                   ! establish a reasonable default
  ENDIF
  INP.FIELD$(1) = MID$(A$,1,1)                   ! destination
  INP.FIELD$(2) = MID$(A$,2,1)                   ! scope
  INP.FIELD$(5) = MID$(A$,3,1)                   ! Y or N

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
    CALL MMCFEC05(POSF(FIELD),"HC 3")            ! start of UPC range
    ATTR$ = SETF(VISIBLE$)                       ! hide
  ENDIF ELSE BEGIN
    CALL MMCFEC05(POSF(FIELD),"HC 3")            ! start of UPC range
    ATTR$ = SETF(INVISIBLE$)                     ! hide
  ENDIF
END SUB ! SET.VISIBLE

!****************************************************************************!
!* Subroutine Name: DISPLAY.MENU                                            *!
!* Description    : This routine populates the WIC report screen Num 3 with *!
!*                  the text that is unique to that report.                 *!
!****************************************************************************!
SUB DISPLAY.MENU

  INTEGER I

  CALL MMCFEC01

  CALL SET.VISIBLE(100, -1)
  CALL MMCFEC05(PUTF(RPT.TITLES$(1)),"HC 7")     ! hot card list report
  RPT.TITLE$ = RPT.TITLES$(1)

  CALL SET.VISIBLE(1, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(1)),"HC 7")      ! destination input field

  CALL SET.VISIBLE(2, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(2)),"HC 7")      ! scope input field

  ! scope choices
  FOR I = 1 TO 2
    CALL SET.VISIBLE(I+101, -1)
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(I)),"HC 7") !
  NEXT I

  CALL SET.VISIBLE(105, -1)

  IF INP.FIELD$(2) = "1" THEN BEGIN              ! SCOPE = single card
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(3)),"HC 7")
    CALL MMCFEC05(POSF(3),"HC 3")                ! reposition cursor for input
    CALL MMCFEC05(PUTF(INP.FIELD$(3)),"HC 7")    ! put up card number
    ATTR$ = SETF(VISIBLE$)                       ! set input field to be shown
  ENDIF

  CALL MMCFEC05(POSF(109),"HC 3")                ! yes/no hint
  ATTR$ = SETF(INVISIBLE$)                       !
  CALL SET.VISIBLE(115, 0)                       ! not used in this report

  CALL MMCFEC05(POSF(112),"HC 3")                ! output amt warning
  ATTR$ = SETF(VISIBLE$)                         !

  IF DESTINATION <> DISPLAY AND                  \ not going to display and
     COMPLETED THEN BEGIN                        ! report completed
    CALL MMCFEC03(480,NULL$)                     ! show status
    QUIT.PRESSED = OFF0                          ! setup variables
    ENTER.PRESSED = OFF0
    ESC.PRESSED = OFF0
    CURRENT.FIELD = 1
  ENDIF ELSE BEGIN
    IF INVALID.CARD THEN BEGIN
      CALL MMCFEC03(409,NULL$)
      QUIT.PRESSED = OFF0                        ! setup variables
      ENTER.PRESSED = OFF0
      ESC.PRESSED = OFF0
      CURRENT.FIELD = 1
    ENDIF
  ENDIF

END SUB ! DISPLAY.MENU

!****************************************************************************!
!* Subroutine Name: BUILD.RPT.LINE                                          *!
!* Description    : This routine builds the report line based on the scope  *!
!*                  of the report.                                          *!
!****************************************************************************!
SUB BUILD.RPT.LINE(I)

  STRING                                         \
    WORK$,                                       \ work var
    CCYY$,                                       \ year
    MM$,                                         \ month
    DD$,                                         \ day
    HR$,                                         \ hour
    MIN$,                                        \ minute
    TEMP$                                        ! var

  INTEGER*4                                      \
    RC

  INTEGER*2                                      \
    I                                            ! index

  IF I = 0 THEN BEGIN
    OUT.STR$ = "  "               +              \
               RIGHT$(UNPACK$(CARD.NUM$),19) +   \ record key
               "    "                            !
    GOSUB FINISH.IT                              !
  ENDIF ELSE BEGIN
    RC = 0
    RECORD$ = STRING$(22, " ")                   ! rqrd to prevent heap probs

    CALL GETNEXT(RC, RECORD$)                    ! read next rec from lrg sorted
                                                 ! array
    IF RC = 1 THEN BEGIN

      OUT.STR$ = "  "                        +   \
                 RIGHT$(UNPACK$(LEFT$(RECORD$, 10)),19) + \ record key
                 "    "                          !
      GOSUB FINISH.IT
    ENDIF
  ENDIF
  EXIT SUB

FINISH.IT:

  TEMP$ = MID$(RECORD$, 12, 7)                   ! get packed data
  TEMP$ = UNPACK$(TEMP$)                         ! unpack it
  CCYY$ = LEFT$(TEMP$, 4)                        ! extract year
  MM$ = MID$(TEMP$, 5, 2)                        !   month
  DD$ = MID$(TEMP$, 7, 2)                        !   day
  HR$ = MID$(TEMP$, 9, 2)                        !   hour
  MIN$ = MID$(TEMP$, 11, 2)                      !   minute
  TEMP$ = MM$ + "/" + DD$ + "/" + CCYY$ +        \ build the date and time
         "      " + HR$ + ":" + MIN$ + "       " ! data for reporting
  OUT.STR$ = OUT.STR$ + TEMP$                    ! add date time to report string
  OUT.STR$ = OUT.STR$ + RIGHT$(RECORD$, 4)       ! add reason code to rpt string

  RETURN

END SUB ! BUILD.RPT.LINE

!****************************************************************************!
!* Subroutine Name: BUILD.REPORT                                            *!
!* Description    : Creates and displays/prints/files the entire report.    *!
!****************************************************************************!
SUB BUILD.REPORT

  STRING                                         \
    WORK$,                                       \
    TEMP$                                        !

  INTEGER*4                                      \
    START,                                       \
    END.CNT,                                     \
    LOOP.COUNTER,                                \
    TEMP                                         !

  INTEGER*2                                      \
    PRICE.TYPE,                                  \ WIC price type, constant 1
    OK,                                          \
    SAVE.START,                                  \
    LAST.LINE,                                   \
    TEMP.PAGE,                                   \ used to inhibit PgUp/PgDn
    TEMP.MAX.PAGE,                               \ used to inhibit PgUp/PgDn
    FILE1.OPEN,                                  \ flag for closing #1
    I                                            !

  STORE$ = LEFT$(MM.STATUS$, 4)
  RP.DATETIME$ = MMCFEC11$
  FILE1.OPEN = 0

  IF DESTINATION = PRINTER AND NOT GC.PRINTER THEN BEGIN
    MSG.NO = 149
    IF INP.FIELD$(2) = "1" THEN BEGIN            ! SCOPE = single card
      WORK$ = UNPACK$(CARD.NUM$)
      CALL REMOVE.LEADING.ZEROS(WORK$)
      INP.FIELD$(3) = WORK$
      CALL DISPLAY.MENU
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"HC 7")  ! hot card number
      CALL MMCFEC03(MSG.NO,NULL$)                ! display it
      CALL MMCFEC05(POSF(3),"HC 3")
    ENDIF

    CALL CHAIN.BACK                              ! chain back to required module
    EXIT SUB
  ENDIF

  IF END #1 THEN RPM

  IF DESTINATION = DISPLAY THEN BEGIN            ! report to screen
    CALL MMCFEC05(DISPD(100), "HC 2")            ! common report screen
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

  IF SCOPE = 1 THEN BEGIN                        ! report single card
    END.CNT = 1                                  ! one item
    MAX.PAGE = 1                                 ! one report page

    INVALID.CARD = 0
    OPEN FILENAME$ KEYED RECL 22 AS 4 NOWRITE NODEL
    TS.ER.RETURN = -1
    WIC.ERR$ = ""
    READ FORM "C22"; #4 KEY CARD.NUM$; RECORD$

    IF (WIC.ERR$ = "EF") THEN BEGIN
      INVALID.CARD = -1
      SHOW.ERROR = 7                             ! "Card was not found."
      CLOSE 4
      EXIT SUB
    ENDIF

    CLOSE 4
    END.CNT = 1
  ENDIF ELSE BEGIN
    IF SCOPE = 2 THEN BEGIN                      ! report all cards
      END.CNT = REC.CNT
    ENDIF
  ENDIF

  MAX.PAGE = INT%(END.CNT + (PAGE.LEN - 6) - 1)  \
                        /(PAGE.LEN - 6)

  CALL WIC.BUILD.HEADINGS(1)
  CALL WIC.BUILD.HEADER
  LINE.NO = 6                                    ! init report vars
  QUIT.PRESSED = OFF0
  ENTER.PRESSED = OFF0
  ESC.PRESSED = OFF0
  COMPLETED = OFF0

  ! WHILE NOT COMPLETED
  FOR LOOP.COUNTER = 1 TO END.CNT

    IF END.CNT = 1 THEN BEGIN
      CALL BUILD.RPT.LINE(0)                     ! format the output line
    ENDIF ELSE BEGIN
      CALL BUILD.RPT.LINE(LOOP.COUNTER)          ! format the output line
    ENDIF
    CALL WIC.OUTPUT(OUT.STR$)                    ! write to its destination

    IF (SCOPE = 1) OR                            \ single card rpt
       (LOOP.COUNTER = END.CNT) OR               \ last line of report
       (LINE.NO >= PAGE.LEN) THEN BEGIN          ! end of page
      YESORNO = OFF0                             ! turn off checks
      NEED.RANGE.CHECK = OFF0
      IF LOOP.COUNTER >= END.CNT THEN BEGIN      ! if last page of rpt
        LAST.PAGE = ON1
      ENDIF

      IF DESTINATION = DISPLAY THEN BEGIN        ! for display
        IF PAGE < MAX.PAGE THEN BEGIN            ! prompt if more pgs to display
          CALL MMCFEC03(100,NULL$)
        ENDIF
        CALL MMCFEC05(POSF(239),"MP 3")          ! position cursor on inp fld
      ENDIF

      OK = OFF0                                  ! init flag

      WHILE NOT OK                               ! while not set
        IF DESTINATION = DISPLAY THEN BEGIN      ! rpt to display

          ! we can't page up, so temporarily alter the page number to 1
          TEMP.PAGE = PAGE
          TEMP.MAX.PAGE = MAX.PAGE
          IF PAGE = MAX.PAGE THEN BEGIN
            MAX.PAGE = 1
          ENDIF
          PAGE = 1

          ENTER.PRESSED = ON1                  ! disallow ENTER here
          WHILE ENTER.PRESSED
            CALL MMCFEC04(WORK$)               ! get input
          WEND

          PAGE = TEMP.PAGE
          MAX.PAGE = TEMP.MAX.PAGE
          IF PGDN.PRESSED THEN BEGIN
            PAGE = PAGE + 1
          ENDIF

        ENDIF ELSE BEGIN                         ! printer or file
          IF LOOP.COUNTER < END.CNT THEN BEGIN
            CALL SETUP.NEWPAGE(1)                ! put up titles
          ENDIF
        ENDIF
        OK = ON1                                 ! set flag

        IF (PGDN.PRESSED) AND                    \ if PgDn and
           (NOT LAST.PAGE) THEN BEGIN            ! not last rpt page
          CALL SETUP.NEWPAGE(1)                  ! put up titles
          LAST.LINE = LINE.NO - 1
          PGDN.PRESSED = OFF0                    ! reset flag

          IF PAGE = MAX.PAGE THEN BEGIN          ! check for last page and
            PAGE.LEN = (END.CNT - LOOP.COUNTER) + 6 ! num lines on last page
            LAST.PAGE = ON1                      ! show that it is last page
          ENDIF
        ENDIF ELSE BEGIN
          IF PGUP.PRESSED THEN BEGIN             ! if PgUp
            LOOP.COUNTER = LOOP.COUNTER - (LINE.NO - 6) - 14 ! reset parms
            IF LOOP.COUNTER < 0 THEN BEGIN       ! do not backup before beginning
              LOOP.COUNTER = 0                   ! NEXT increments it to START
            ENDIF
            PGUP.PRESSED = OFF0
            CALL SETUP.NEWPAGE(1)                ! put up titles

            IF PAGE.LEN < 20 THEN PAGE.LEN = 20  ! ensure num lines is correct
          ENDIF ELSE BEGIN
            IF (ESC.PRESSED) OR                  \ finished
               (QUIT.PRESSED) OR                 \
               (ENTER.PRESSED) THEN BEGIN        !
              COMPLETED = ON1
              LOOP.COUNTER = END.CNT
            ENDIF ELSE BEGIN
              IF DESTINATION = DISPLAY THEN BEGIN ! for display
                OK = OFF0                        ! turn off flag
                CALL MMCFEC03(109,NULL$)         ! invalid function key
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      WEND
    ENDIF
GET.NEXT:
  NEXT LOOP.COUNTER
  IF FILE1.OPEN THEN                             \ printer or file open?
    CLOSE 1

!! DONE:

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
    IF NOT.FOUND THEN BEGIN
      CURRENT.FIELD = 3
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"HC 7")  ! single hot card number
      NOT.FOUND = 0                              ! reset flag
    ENDIF

    IF CURRENT.FIELD = 4 THEN BEGIN              ! if past last field
      CURRENT.FIELD = 1                          ! make it first field
    ENDIF
    IF CURRENT.FIELD < 3 THEN BEGIN              ! if dest or scope n
      NEED.RANGE.CHECK = ON1                     ! do a range check on input
      LOW.END$ = "1"                             ! set ranges
      HIGH.END$ = HIGH$(CURRENT.FIELD)
      ASTRK.OK  = OFF0                           ! no * or Y/N
      YESORNO = OFF0
    ENDIF
    IF CURRENT.FIELD = 3 THEN BEGIN
      YESORNO = OFF0                             ! turn off checks
      NEED.RANGE.CHECK = OFF0
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"HC 7")  ! single hot card number
    ENDIF

    ! get user input
    CALL MMCFEC05(POSF(CURRENT.FIELD),"HC 3")    ! position cursor
    CALL MMCFEC04(INP.FIELD$(CURRENT.FIELD))     ! get field input

    ! display prompt lines depending on the scope selection that is made

    IF NOT (ESC.PRESSED OR QUIT.PRESSED) THEN BEGIN

      IF CURRENT.FIELD = 2 THEN BEGIN            ! scope
        IF INP.FIELD$(2) = "1" THEN BEGIN        ! single card
          CALL SET.VISIBLE(105, -1)
          CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(3)),"HC 7") ! put up prompt single UPC
          CALL SET.VISIBLE(3, -1)
        ENDIF
        IF INP.FIELD$(2) = "2" THEN BEGIN        ! all cards
          CALL SET.VISIBLE(105, 0)
          CALL SET.VISIBLE(3, 0)
        ENDIF
      ENDIF

      ! process terminating key

      RET.VAL = ON1
      IF TAB.PRESSED THEN BEGIN                  ! adjust cursor position
        CURRENT.FIELD = MOD(CURRENT.FIELD,LAST.FIELD) + 1 ! bump field loc
        IF (CURRENT.FIELD = 3 AND                \ scope
          INP.FIELD$(2) = "2") THEN BEGIN        ! report all hot cards
          CURRENT.FIELD = 1                      ! goto first field
        ENDIF
      ENDIF
      IF END.PRESSED THEN BEGIN                  ! end key
        CURRENT.FIELD = LAST.FIELD               ! goto last field
        IF INP.FIELD$(2) = "2" THEN BEGIN        ! report all hot cards
          CURRENT.FIELD = 2                      ! goto second field
        ENDIF
      ENDIF
      IF HOME.PRESSED THEN BEGIN                 ! home key
        CURRENT.FIELD = 1                        ! goto first field
      ENDIF
      IF BACKTAB.PRESSED THEN BEGIN              ! back tab
        CURRENT.FIELD = MOD(CURRENT.FIELD + LAST.FIELD - 2,LAST.FIELD) + 1 ! back 1
        IF (INP.FIELD$(2) = "2") THEN BEGIN      ! all cards
          IF (CURRENT.FIELD = 3) THEN BEGIN      ! field 3 is not shown
            CURRENT.FIELD = 2                    ! goto last field
          ENDIF
        ENDIF
      ENDIF

      IF ENTER.PRESSED THEN BEGIN                ! enter pressed
        IF INP.FIELD$(2) = "1" THEN BEGIN        ! SCOPE = single card
          CARD.NUM$     = INP.FIELD$(3)          ! set card number
          IF CARD.NUM$ = "" THEN BEGIN           ! if not yet set
            CURRENT.FIELD = 3                    ! ensure user enters it
            ENTER.PRESSED = OFF0                 ! don't end yet
          ENDIF
        ENDIF

        DESTINATION   = VAL(INP.FIELD$(1))       ! set destination
        SCOPE         = VAL(INP.FIELD$(2))       ! set scope
        QUIT.PRESSED  = OFF0
        ESC.PRESSED   = OFF0

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

  CALL ACCESS.CHAIN.PARAMETERS                   ! get the chain parameters

  CALL INITIALIZE.VARIABLES                      ! init the variables

  CALL GET.DESCRIPTORS                           ! report descriptors

  CALL INITIALIZE.DISPLAY.MANAGER                ! init the display manager

REDO:
  WHILE NOT (QUIT.PRESSED OR ESC.PRESSED)
    CALL DISPLAY.MENU                            ! display report menu
    ENTER.PRESSED = 0                            ! clear it

    CALL HANDLE.USER.INPUT                       ! get user input

    IF ENTER.PRESSED THEN BEGIN
      ! note: the blank space, at the end of the search pattern, is required
      !       by "slookup"
      CALL FIND.FILES("???????.HCL ")            ! look for HCL files

      IF FCNT = 0 THEN BEGIN                     ! if no files found
        SHOW.ERROR = 4                           ! "No HCL files were found"
      ENDIF ELSE BEGIN
        CALL DISPLAY.FILES                       ! display file names
        IF ENTER.PRESSED THEN BEGIN

          RET.CODE = 0

          IF SCOPE = 2 THEN BEGIN                ! all hot cards
            RET.CODE = LOADBINS(FILENAME$)       ! function to sort lrg hcl file
          ENDIF ELSE BEGIN                       ! single hot card
            REC.CNT = 1
            RET.CODE = 1
            CARD.NUM$ = RIGHT$("00000000000000000000" + CARD.NUM$, 20)
            CARD.NUM$ = PACK$(CARD.NUM$)
          ENDIF

          IF RET.CODE > 0 THEN BEGIN
            REC.CNT = RET.CODE
            WRITE #36,109;INP.FIELD$(1) +        \ update menu defaults
                          INP.FIELD$(2) + INP.FIELD$(5)
            CALL BUILD.REPORT
            RET.CODE = FREEALL                   ! free the LOADBINS structures
            QUIT.PRESSED = 0                     ! return to initial menu
          ENDIF ELSE BEGIN
            CALL DISPLAY.MENU
            CURRENT.FIELD = 1
            CALL MMCFEC03(629, NULL$)
            CALL MMCFEC05(POSF(CURRENT.FIELD),"HC 3") ! position cursor
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  WEND

  CLOSE 36
  CALL CHAIN.BACK                                ! chain back to required module
  STOP

TRAP.IT:

  INWE.ERR$ = ERR
  INWE.ERRN% = ERRN
  INWE.ERRF% = ERRF%

  IF ((INWE.ERR$ = "XI") OR (INWE.ERR$ = "KF")) AND \
     (INWE.ERRF% = 4) THEN BEGIN
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

  CALL MMCFEC99("HC")                            ! log an error message
  STOP                                           ! do not continue

! End MAIN
