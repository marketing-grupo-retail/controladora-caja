\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMRPAPC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC EBT APPROVED PRODUCT LIST REPORT    */
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
\/* IO00625 - Settings for the Sort By Category and Report Just    */
\/*           UPCs Found flags toggle when the jump key is used.   */
\/*           GGK IBM 10Mar2005                                    */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

%INCLUDE EAMUSED.J86                   ! variable definitions for use
%INCLUDE EAMCFINC.J86                  ! declarations for common functions
%INCLUDE EAMATTRD.J86                  ! declarations for display mgr attributes
%INCLUDE EAMIRRFG.J86                  ! Item Record File GLOBALS
%INCLUDE EAMITEMR.J86                  ! GLOBAL Item Record File variables
%INCLUDE EAMWICEF.J86                  !
%INCLUDE EAMRPINC.J86                  ! variable definitions for calling RPCFECnn

  STRING GLOBAL                        \
    A$,                                \ work string
    ATTR$,                             \ field attribute
    IN.IRRF$,                          \ item record flag
    EAMITEMR$,                         \ item record
    INP.FIELD$(1),                     \ input field array
    SCOPE.PROMPTS$(1),                 \ descriptor array
    HIGH$(1),                          \ upper input value array
    MENU.PROMPTS$(1),                  \ display test
    RPT.TITLE$,                        \ report title
    RPT.TITLES$(1),                    \ report titles array
    STORE$,                            \ store number
    NOT.FOUND.MSG$,                    \ item not found message
    UPC.NUM$,                          \ single item enquiry number
    START.RNG$,                        \ start of range enquiry
    END.RNG$,                          \ end of range enquiry
    RPT.STR$(1),                       \ line in report
    OUT.STR$,                          \ string to be output
    FILE.KEYS$(1),                     \ keys in file
    COL.HEADINGS$(1),                  \ report column headings
    ERROR.MESSAGES$(1),                \ Line 22 red error messages
    WIC.ERR$                           ! CBASIC ERROR CODE

  INTEGER*4 GLOBAL                     \
    FCNT                               ! SLOOKUP number of files found

  INTEGER*2 GLOBAL                     \
    CURRENT.FIELD,                     \ current screen pos
    FIRST.FIELD,                       \ starting screen pos
    LAST.FIELD,                        \ ending screen pos
    LAST.PAGE,                         \ keep record of last page
    PAGE.SO.FAR,                       \ pages processed so far
    PAGE.LEN,                          \ num lines per page
    GC.PRINTLEN,                       \ length of print line
    NUM.COL.LINES,                     \
    NEXT.FIELD,                        \
    GC.PRINTER,                        \ flag if system contains a printer
    MSG.NO,                            \ status/error message number
    TS.LINEDATA2,                      \ OTHER DATA
    MSG.NUM,                           \
    SCOPE,                             \ scope of report
    COMPLETED,                         \ flag indicating that the report ended
    RET.VAL,                           \ ADXERROR Return value parameter
    THRESHOLD.ITEM,                    \ flag for pricing method 3 threshold
    FOUND.ONLY,                        \ flag to report only items = ITEM REC
    TS.ERRF,                           \ ERROR SESSION NUMBER
    TS.ERRFSAV(1),                     \ STACKED ERROR SESSION NUMBER
    TS.ER.UNEX,                        \ INDICATES UNEXPECTED ERROR HAS OCCURRED
    TS.ER.RETURN,                      \ INDICATES WHETHER APPLICATION WISHES TO
    NOT.FOUND,                         \ item not found flag
    SHOW.ERROR                         ! error.messages(show.error)

%INCLUDE EAMXXCPY.J86                  ! copyright statement
%INCLUDE EAMADXRT.J86                  ! system services interface
%INCLUDE EAMASMCT.J86                  ! assembler routines
%INCLUDE DMEXTR.J86                    ! display manager external function def.

!****************************************************************************!
!* Subroutine Name: IRRFEC.READ02                                           *!
!* Description    : Read the item record with key IR.ITEMCODE$ into the     *!
!*                  IR.???? variables.                                      *!
!****************************************************************************!
SUB IRRFEC.READ02 (SESS.NO, LOCK.IT) EXTERNAL
  INTEGER*2 SESS.NO                    !* SESSION number to be used.        *!
  INTEGER*2 LOCK.IT                    !* Lock the record ?                 *!
END SUB

!****************************************************************************!
!* Subroutine Name: IRRFEC.APPLINIT                                         *!
!* Description    : Application specific initialization for the IRRF        *!
!*                  support.                                                *!
!****************************************************************************!
SUB IRRFEC.APPLINIT EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC03                                                *!
!* Description    : This subprogram displays a message on line 23.          *!
!****************************************************************************!
SUB MMCFEC03(MSG.NO,MSG.STR$) EXTERNAL
  INTEGER*2 MSG.NO
  STRING    MSG.STR$
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
!* Subroutine Name: FIND.KEYS                                               *!
!* Description    : This subroutine searches a keyed file and extracts the  *!
!*                  keys.  It places the keys into an array.                *!
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

!****************************************************************************!
!                    REPORT COMMON OUTPUT ROUTINES                           !
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
!* Subroutine Name: HANDLE.MISSING.ITEMS                                    *!
!* Description    : This routine removes WIC items that are not in the      *!
!*                  current item record file.                               *!
!****************************************************************************!
SUB HANDLE.MISSING.ITEMS(EXCEEDED.FLAG) EXTERNAL
  INTEGER*2 EXCEEDED.FLAG
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

  INTEGER*2                            \
    I

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
  FULL.SCREEN = ON1
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
  LAST.FIELD = 6                                 ! set last field on this page
  NUM.COL.LINES = 1

  IF NOT.FOUND = 0 THEN BEGIN
    DIM INP.FIELD$(LAST.FIELD + 2)
    OPEN "EAMSDEF1" RECL 40 AS 36 NODEL          ! open defaults file
    READ #36,110;A$                              ! read menu defaults
    IF LEN(A$) <> 4 THEN BEGIN                   ! if first time ever
      A$ = "11NN"                                ! establish a reasonable default
    ENDIF
    INP.FIELD$(1) = MID$(A$,1,1)                 ! destination
    INP.FIELD$(2) = MID$(A$,2,1)                 ! scope
    INP.FIELD$(5) = MID$(A$,4,1)                 ! sort by category
    INP.FIELD$(6) = MID$(A$,3,1)                 ! only for items on file
  ENDIF

  IF INP.FIELD$(5) <> "Y" THEN BEGIN             ! Make it 'Y' or 'N'
    INP.FIELD$(5) = "N"
  ENDIF
  IF INP.FIELD$(6) <> "Y" THEN BEGIN             ! Make it 'Y' or 'N'
    INP.FIELD$(6) = "N"
  ENDIF

  FIRST.FIELD = 1                                ! setup input handler
  CURRENT.FIELD = FIRST.FIELD
  NEXT.FIELD = CURRENT.FIELD + 1
  LAST.PAGE = 0

END SUB ! INITIALIZE.VARIABLES

!****************************************************************************!
!* Subroutine Name: SET.VISIBLE                                             *!
!* Description    : This routine hides or shows fields depending on the     *!
!*                  passed parameters.                                      *!
!****************************************************************************!
SUB SET.VISIBLE(FIELD, VIS)
  INTEGER*2                                      \
    FIELD,                                       \
    VIS

  IF VIS THEN BEGIN
    CALL MMCFEC05(POSF(FIELD),"MP 3")            ! start of UPC range
    ATTR$ = SETF(VISIBLE$)                       ! hide
  ENDIF ELSE BEGIN
    CALL MMCFEC05(POSF(FIELD),"MP 3")            ! start of UPC range
    ATTR$ = SETF(INVISIBLE$)                     ! hide
  ENDIF
END SUB ! SET.VISIBLE

!****************************************************************************!
!* Subroutine Name: DISPLAY.MENU                                            *!
!* Description    : This routine populates the WIC report screen Number 3   *!
!*                  with the text that is unique to that report.            *!
!****************************************************************************!
SUB DISPLAY.MENU

  INTEGER I

  CALL MMCFEC01

  CALL SET.VISIBLE(100, -1)
  CALL MMCFEC05(PUTF(RPT.TITLES$(2)),"MP 7")     ! auth prod list report
  RPT.TITLE$ = RPT.TITLES$(2)

  CALL SET.VISIBLE(1, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(1)),"MP 7")      ! destination input field

  CALL SET.VISIBLE(2, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(2)),"MP 7")      ! scope input field

  ! scope choices
  FOR I = 4 TO 6
    CALL SET.VISIBLE(I+98, -1)
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(I)),"MP 7") ! range of UPCs
  NEXT I

  CALL SET.VISIBLE(105, -1)
  IF INP.FIELD$(2) = "1" THEN BEGIN                 ! if "Single UPC"
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(7)),"MP 7")   ! "Enter UPC"
  ENDIF ELSE BEGIN
    IF INP.FIELD$(2) = "2" THEN BEGIN               ! if "Range of UPCs"
      CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(8)),"MP 7") ! "Enter start of range"
      CALL SET.VISIBLE(106, -1)
      CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(9)),"MP 7") ! "Enter end of range"
    ENDIF
  ENDIF

  CALL MMCFEC05(POSF(107),"MP 3")                ! sort by category instead
  CALL MMCFEC05(PUTF(ERROR.MESSAGES$(9)),"MP 7") ! of by UPC
  CALL MMCFEC05(POSF(108),"MP 3")                ! report only items found in
  CALL MMCFEC05(PUTF(MENU.PROMPTS$(1)),"MP 7")   ! item record file
  IF INP.FIELD$(2) = "1" THEN BEGIN
    CALL SET.VISIBLE(107, 0)
    CALL SET.VISIBLE(108, 0)
    CALL SET.VISIBLE(5, 0)
    CALL SET.VISIBLE(6, 0)
    CALL SET.VISIBLE(109, 0)
    CALL SET.VISIBLE(115, 0)
  ENDIF ELSE BEGIN
    CALL SET.VISIBLE(107, -1)
    CALL SET.VISIBLE(108, -1)
    CALL SET.VISIBLE(5, -1)
    CALL MMCFEC05(PUTF(INP.FIELD$(5)),"MP 7")    ! show as Y or N
    CALL SET.VISIBLE(6, -1)
    CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
    CALL SET.VISIBLE(109, -1)
    CALL SET.VISIBLE(115, -1)
  ENDIF

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
!* Description    : This routine builds the report line based on the scope  *!
!*                  of the report.                                          *!
!****************************************************************************!
SUB BUILD.RPT.LINE(I)

  STRING                                         \
    WORK$,                                       \ work vars
    TEMP$                                        !

  INTEGER*4                                      \
    TEMP,                                        \
    CLAIM.AMT,                                   \
    SETTLE.AMT,                                  \
    ADJ.AMT                                      !

  INTEGER*2                                      \
    I                                            !

  OUT.STR$ = " "                   +             \
             RIGHT$(UNPACK$(FILE.KEYS$(I)),12) + \ record key
             " "                                 !
  TS.ER.RETURN = -1
  WIC.ERR$ = ""
  CALL IRRFEC.READ02 (4, 0)

BADREAD:

  NOT.FOUND = 0

  IF (WIC.ERR$ = "EF") THEN BEGIN
    IR.ITEMNAME$ = LEFT$(ERROR.MESSAGES$(10),18)  ! "NO RECORD FOUND   "
    NOT.FOUND = -1                               ! set flag
  ENDIF ELSE BEGIN
    IR.ITEMNAME$ = LEFT$(IR.ITEMNAME$, 18)       ! found, so get item desc
  ENDIF

  IF (FOUND.ONLY AND                             \ only report irrec items
      NOT.FOUND) THEN BEGIN                      ! not on item rec
    GOTO DONE
  ENDIF ELSE BEGIN                               ! else it was found on item rec
    OUT.STR$ = OUT.STR$     +                    \ add product name
               IR.ITEMNAME$ +                    \
               "  "                              !
    TEMP$ = MID$(RPT.STR$(I), 9, 3)              ! build category and sub cat
    TEMP$ = UNPACK$(TEMP$)
    OUT.STR$ = OUT.STR$         +                \ add category
               LEFT$(TEMP$, 2)  + " " +          \
               RIGHT$(TEMP$, 3) + "  "           ! and subcategory
    TEMP$ = MID$(RPT.STR$(I), 35, 1)

    IF TEMP$ = CHR$(0) THEN BEGIN
      TEMP$ = "N"
    ENDIF ELSE BEGIN
      TEMP$ = "Y"
    ENDIF

    OUT.STR$ = OUT.STR$ +                        \
               TEMP$    +                        \ add zero subcategory flag
               "  "                              !
    TEMP$ = MID$(RPT.STR$(I), 12, 3)
    TEMP$ = UNPACK$(TEMP$)
    TEMP  = VAL(TEMP$)
    TEMP$ = MMCFEC06$(TEMP,ON1,ON1)
    TEMP$ = RIGHT$("     " + TEMP$, 6)
    OUT.STR$ = OUT.STR$ +                        \ add unit of measure
               TEMP$                             !
    OUT.STR$ = OUT.STR$                  +       \ add desc of unit of measure
               MID$(RPT.STR$(I), 15, 10) +       \
               " "                               !
    TEMP$ = MID$(RPT.STR$(I), 29, 3)             ! build start date string
    TEMP$ = UNPACK$(TEMP$)
    TEMP$ = MMCFEC14$(TEMP$ + "0000")
    TEMP$ = LEFT$(TEMP$, 8)
    OUT.STR$ = OUT.STR$ +                        \ add start date
               TEMP$    +                        \
               " "                               !

    TEMP$ = MID$(RPT.STR$(I), 32, 3)             ! build end date string
    TEMP$ = UNPACK$(TEMP$)

    IF VAL(TEMP$) = 0 THEN BEGIN
      TEMP$ = "Not Set "
    ENDIF ELSE BEGIN
      TEMP$ = MMCFEC14$(TEMP$+"0000")
      TEMP$ = LEFT$(TEMP$, 8)
    ENDIF

    OUT.STR$ = OUT.STR$ +                        \ add end date
               TEMP$                             !
  ENDIF
DONE:

END SUB ! BUILD.RPT.LINE

!****************************************************************************!
!* Subroutine Name: BUILD.REPORT                                            *!
!* Description    : Creates and displays/prints/files the entire report.    *!
!****************************************************************************!
SUB BUILD.REPORT

  STRING                                         \
    WORK$,                                       \
    TEMP$,                                       \
    SORTI$, SORTJ$                               !

  INTEGER*4                                      \
    START,                                       \
    END.CNT,                                     \
    TEMP                                         !

  INTEGER*2                                      \
    PRICE.TYPE,                                  \ WIC price type, constant 1
    OK,                                          \
    LAST.LINE,                                   \
    LOOP.COUNTER,                                \
    FILE1.OPEN,                                  \ flag for closing #1
    I, J                                         !

  !**************************************************************************!
  !* Open the keyed file in direct mode and use the record length from      *!
  !* the first sector keyed file information.                               *!
  !* We open it record length 48 as we only really need the keyed file      *!
  !* record length information which is at offset 46.  We therefore         *!
  !* tab to the 47th byte and read a two byte integer at this position.     *!
  !* This should be faster and create less loop traffic and less code than  *!
  !* reading a full sector of information.                                  *!
  !**************************************************************************!
  OPEN EAMITEMR$ DIRECT RECL 48 AS 4 NOWRITE NODEL
  READ FORM "T47 I2"; #4, 1; IRRF.RECL
  CLOSE 4

  OPEN EAMITEMR$ KEYED RECL IRRF.RECL AS 4 NOWRITE NODEL
  IRRF.FORM1$ = "C" + STR$(IRRF.RECL)
  START = 1
  FILE1.OPEN = 0

  STORE$ = LEFT$(MM.STATUS$, 4)
  RP.DATETIME$ = MMCFEC11$

  IF DESTINATION = PRINTER AND NOT GC.PRINTER THEN BEGIN
    MSG.NO = 149
    IF INP.FIELD$(2) = "1" THEN BEGIN            ! single UPC
      WORK$ = UNPACK$(UPC.NUM$)
      CALL REMOVE.LEADING.ZEROS(WORK$)
      INP.FIELD$(3) = WORK$
      CALL DISPLAY.MENU
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"MP 7")  ! UPC input field
      CALL MMCFEC03(MSG.NO,NULL$)                ! display it
      CALL MMCFEC05(POSF(3),"MP 3")
    ENDIF

    CALL CHAIN.BACK                              ! chain back to required module
    EXIT SUB
  ENDIF

  IF END #1 THEN RPM

  IF DESTINATION = DISPLAY THEN BEGIN            ! report to screen
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

  IF SCOPE = 1 THEN BEGIN                        ! report single UPC
    END.CNT = 1                                  ! one item
    MAX.PAGE = 1                                 ! one report page
  ENDIF ELSE BEGIN
    IF SCOPE = 2 THEN BEGIN                      ! report UPC range
      I = 1
      TEMP$ = UNPACK$(FILE.KEYS$(I))
      START.RNG$ = RIGHT$("00000000000000" + START.RNG$, 14)
      WHILE (TEMP$ < START.RNG$) AND             \ look for point to start srch
            (I < REC.CNT)                        ! don't go beyond array limit
        I = I + 1
        TEMP$ = UNPACK$(FILE.KEYS$(I))
      WEND
      START = I                                  ! first entry to report

      TEMP$ = UNPACK$(FILE.KEYS$(I))
      END.RNG$ = RIGHT$("00000000000000" + END.RNG$, 14)
      WHILE (TEMP$ < END.RNG$) AND               \ look for point to end search
            (I < REC.CNT)                        ! don't go beyond array limit
        I = I + 1
        TEMP$ = UNPACK$(FILE.KEYS$(I))
      WEND
      IF (TEMP$ > END.RNG$) THEN BEGIN           ! too big?
        I = I - 1
      ENDIF
      IF (TEMP$ < START.RNG$) THEN BEGIN         ! too small?
        I = 0
      ENDIF

      IF (I = 0) OR                              \ no records found
         (I < START) THEN BEGIN                  ! no records in range
        SHOW.ERROR = 3                           ! "Items were not found"
        GOTO DONE
      ENDIF ELSE BEGIN                           ! handle the number found
        END.CNT = I
      ENDIF
    ENDIF ELSE BEGIN                             ! handle them all
      END.CNT = REC.CNT
    ENDIF
  ENDIF

  IF (INP.FIELD$(2) <> "1") AND                  \ if not a single UPC
     (INP.FIELD$(6) = "Y") THEN BEGIN            ! remove items not on file
    REC.CNT = END.CNT                            ! where to stop
    CALL HANDLE.MISSING.ITEMS(0)                 ! check each item
    END.CNT = REC.CNT                            ! where to stop
  ENDIF

  MAX.PAGE = INT%((END.CNT - START + 1) + (PAGE.LEN - 6) - 1) \ round #lines
                        / (PAGE.LEN - 6)                      ! up to get #pages


  ! The keys are read in and sorted, and the range of indexes is determined.
  ! Now rearrange those entries if a Category sort was specified.
  IF INP.FIELD$(5) = "Y" THEN BEGIN              ! Sort by category
    FOR I = START TO END.CNT - 1
      SORTI$ = MID$(RPT.STR$(I), 9, 3) +         \ cat/subcat
               FILE.KEYS$(I)                     ! UPC
      FOR J = I + 1 TO END.CNT
        SORTJ$ = MID$(RPT.STR$(J), 9, 3) +       \ cat/subcat
                 FILE.KEYS$(J)                   ! UPC
        IF SORTI$ > SORTJ$ THEN BEGIN            ! If cat/sub+UPC is reversed
          TEMP$ = RPT.STR$(I)                    ! swap the entries
          RPT.STR$(I) = RPT.STR$(J)
          RPT.STR$(J) = TEMP$

          TEMP$ = FILE.KEYS$(I)                  ! swap the keys
          FILE.KEYS$(I) = FILE.KEYS$(J)
          FILE.KEYS$(J) = TEMP$
          SORTI$ = MID$(RPT.STR$(I), 9, 3) +     \ cat/subcat
                   FILE.KEYS$(I)                 ! UPC
          J = J - 1                              ! repeat this test
        ENDIF
      NEXT J
    NEXT I
  ENDIF                                          ! Sort by category

  IF DESTINATION = DISPLAY THEN BEGIN            ! report to screen
    CALL MMCFEC05(DISPD(100), "MP 2")            ! common report screen
  ENDIF

  CALL WIC.BUILD.HEADINGS(3)
  CALL WIC.BUILD.HEADER
  LINE.NO = 6                                    ! init report vars
  QUIT.PRESSED = OFF0
  ENTER.PRESSED = OFF0
  ESC.PRESSED = OFF0
  COMPLETED = OFF0

  ! Now output each UPC line
  FOR LOOP.COUNTER = START TO END.CNT
    IR.ITEMCODE$ = RIGHT$(FILE.KEYS$(LOOP.COUNTER), 6)
    TS.ER.RETURN = -1                            ! expect error
    WIC.ERR$ = ""                                ! init
    CALL IRRFEC.READ02 (4, 0)                    ! read item record

    IF (WIC.ERR$ = "EF") THEN BEGIN              ! item not in item rec file
      IR.ITEMNAME$ = LEFT$(ERROR.MESSAGES$(10),18) ! "NO RECORD FOUND   "
    ENDIF

    IF (FOUND.ONLY) AND                          \ only rpt if max price exceeded
       (NOT.FOUND) THEN BEGIN                    ! shelf price < max price
      GOTO GET.NEXT                              ! nothing to report
    ENDIF ELSE BEGIN
      CALL BUILD.RPT.LINE(LOOP.COUNTER)          ! format the output line
      CALL WIC.OUTPUT(OUT.STR$)                  ! write to its destination

      IF (SCOPE = 1) OR                          \ only 1 item to show
         (LOOP.COUNTER = END.CNT) OR             \ last line of report
         (LINE.NO >= PAGE.LEN) THEN BEGIN        ! end of page
        IF DESTINATION = DISPLAY THEN BEGIN      ! report to screen
          YESORNO = OFF0                         ! turn off checks
          NEED.RANGE.CHECK = OFF0

          IF PAGE < MAX.PAGE THEN BEGIN          ! prompt if more pgs to display
            CALL MMCFEC03(100,NULL$)
          ENDIF

          CALL MMCFEC05(POSF(239),"MP 3")        ! position cursor on inp fld
          OK = OFF0                              ! init flag

          WHILE NOT OK                           ! while not set

            ENTER.PRESSED = ON1                  ! disallow ENTER here
            WHILE ENTER.PRESSED
              CALL MMCFEC04(WORK$)               ! get input
            WEND

            OK = ON1                             ! set flag

            IF PGDN.PRESSED THEN BEGIN           ! if PgDn
              CALL SETUP.NEWPAGE(3)              ! put up titles
              LAST.LINE = LINE.NO - 1
              PGDN.PRESSED = OFF0
            ENDIF ELSE BEGIN
              IF PGUP.PRESSED THEN BEGIN         ! if PgUp
                LOOP.COUNTER = LOOP.COUNTER - (LINE.NO - 6) - 14 ! reset parms
                IF LOOP.COUNTER < (START-1) THEN BEGIN ! do not backup before beginning
                  LOOP.COUNTER = START - 1       ! NEXT increments it to START
                ENDIF
                PGUP.PRESSED = OFF0
                IF PAGE.LEN < 20 THEN PAGE.LEN = 20 ! ensure num lines is correct
              ENDIF ELSE BEGIN
                IF (ESC.PRESSED) OR              \ finished
                   (QUIT.PRESSED) OR             \
                   (ENTER.PRESSED) THEN BEGIN
                  COMPLETED = ON1
                  LOOP.COUNTER = END.CNT + 1     ! ensure loop exit
                ENDIF ELSE BEGIN
                  OK = OFF0                      ! turn off flag
                  CALL MMCFEC03(109,NULL$)       ! invalid function key
                ENDIF
              ENDIF
            ENDIF
          WEND
        ENDIF                                    ! destination = display
        IF LOOP.COUNTER < END.CNT THEN BEGIN
          ! We just finished a page.  Prepare for the next, if any
          CALL SETUP.NEWPAGE(3)                  ! put up titles
        ENDIF
      ENDIF ! end of a page
    ENDIF
GET.NEXT:
  NEXT LOOP.COUNTER

DONE:

  CLOSE 4
  IF FILE1.OPEN THEN                             \ printer or file open?
    CLOSE 1
  IF LOOP.COUNTER > END.CNT THEN BEGIN           ! reached the end of range
    COMPLETED = ON1                              ! set completed flag
  ENDIF
  CURRENT.FIELD = 1                              ! first field
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
      IF SHOW.ERROR = 8 THEN BEGIN               ! if range is invalid
        CURRENT.FIELD = 3                        ! position at start field
      ENDIF
      SHOW.ERROR = 0                             ! clear flag for next time
    ENDIF ELSE BEGIN                             ! no error yet
      CALL MMCFEC05(PUTF(""),"MP 7")             ! blank the error message line
    ENDIF

    ! set up input field parameters
    IF NOT.FOUND AND                             \
      (INP.FIELD$(2) <> "3") THEN BEGIN          ! if a UPC or a range was used
      CURRENT.FIELD = 3
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"MP 7")  ! lower range
      NOT.FOUND = 0                              ! reset flag
    ENDIF

    IF (INP.FIELD$(2) = "1") OR                  \ single UPC
       (INP.FIELD$(2) = "2") THEN BEGIN          ! range of UPCs
      CALL SET.VISIBLE(3, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(3)),"MP 7")  ! show lower range
    ENDIF
    IF INP.FIELD$(2) = "2" THEN BEGIN            ! range of UPCs
      CALL SET.VISIBLE(4, -1)
      CALL MMCFEC05(PUTF(INP.FIELD$(4)),"MP 7")  ! show upper range
    ENDIF

    IF CURRENT.FIELD = 7 THEN BEGIN              ! if past last field
      CURRENT.FIELD = 1                          ! make it first field
    ENDIF
    IF CURRENT.FIELD < 3 THEN BEGIN              ! if dest or scope n
      NEED.RANGE.CHECK = ON1                     ! do a range check on input
      LOW.END$ = "1"                             ! set ranges
      HIGH.END$ = HIGH$(CURRENT.FIELD)
      ASTRK.OK  = OFF0                           ! no * or Y/N
      YESORNO = OFF0
    ENDIF
    IF CURRENT.FIELD > 2 AND                     \ if entering UPC
      CURRENT.FIELD < 5 THEN BEGIN               !
      YESORNO = OFF0                             ! turn off checks
      NEED.RANGE.CHECK = OFF0
    ENDIF
    IF (CURRENT.FIELD = 5) OR                    \ if one of the last
       (CURRENT.FIELD = 6) THEN BEGIN            !   two fields
      YESORNO = ON1                              ! turn on y/n check
      NEED.RANGE.CHECK = ON1                     ! do a Y|N range check on input
    ENDIF

    ! get user input
    CALL MMCFEC05(POSF(CURRENT.FIELD),"MP 3")    ! position cursor
    CALL MMCFEC04(INP.FIELD$(CURRENT.FIELD))     ! get field input

    ! display prompt lines depending on the scope selection that is made

    IF NOT (ESC.PRESSED OR QUIT.PRESSED) THEN BEGIN

      IF CURRENT.FIELD = 2 THEN BEGIN            ! scope
        IF INP.FIELD$(2) = "3" THEN BEGIN        ! all UPCs
          CALL SET.VISIBLE(105, 0)               ! hide the low UPC
          CALL SET.VISIBLE(3, 0)
          CALL SET.VISIBLE(106, 0)               ! hide the high UPC
          CALL SET.VISIBLE(4, 0)

          CALL SET.VISIBLE(107, -1)              ! "Sort By Category"
          CALL MMCFEC05(PUTF(ERROR.MESSAGES$(9)),"MP 7")
          CALL SET.VISIBLE(5, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(5)),"MP 7") ! show as Y or N
          CALL SET.VISIBLE(115, -1)

          CALL SET.VISIBLE(108, -1)              ! only on file?
          CALL MMCFEC05(PUTF(MENU.PROMPTS$(1)),"MP 7")
          CALL SET.VISIBLE(6, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
          CALL SET.VISIBLE(109, -1)

        ENDIF ELSE BEGIN
          IF INP.FIELD$(2) = "1" THEN BEGIN      ! single UPC
            CALL SET.VISIBLE(105, -1)            ! low UPC
            CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(7)),"MP 7") ! put up prompt single UPC
            CALL SET.VISIBLE(4, 0)
            CALL SET.VISIBLE(106, 0)
            CALL SET.VISIBLE(107, 0)
            CALL SET.VISIBLE(108, 0)
            CALL SET.VISIBLE(109, 0)
            CALL SET.VISIBLE(115, 0)
            CALL SET.VISIBLE(5, 0)
            CALL SET.VISIBLE(6, 0)
          ENDIF
          IF INP.FIELD$(2) = "2" THEN BEGIN      ! range of UPCs
            CALL SET.VISIBLE(105, -1)
            CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(8)),"MP 7") ! put up prompt for low end
            CALL SET.VISIBLE(106, -1)
            CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(9)),"MP 7") ! put up prompt for high end

          ENDIF
          IF (INP.FIELD$(2) = "2") OR              \ range of UPCs
             (INP.FIELD$(2) = "3") THEN BEGIN      ! all UPCs

            CALL SET.VISIBLE(107, -1)
            CALL MMCFEC05(PUTF(ERROR.MESSAGES$(9)),"MP 7") ! put up prompt for sort by category
            CALL SET.VISIBLE(5, -1)
!AIO00625 These fields were reversed
\           CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7") ! put up Y or N selection
            CALL MMCFEC05(PUTF(INP.FIELD$(5)),"MP 7") ! put up Y or N selection
!EIO00625
            CALL SET.VISIBLE(115, -1)

            CALL SET.VISIBLE(108, -1)
            CALL MMCFEC05(PUTF(MENU.PROMPTS$(1)),"MP 7") ! display prices > max
            CALL SET.VISIBLE(6, -1)
!AIO00625 These fields were reversed
\           CALL MMCFEC05(PUTF(INP.FIELD$(5)),"MP 7")    ! show as Y or N
            CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
!EIO00625
            CALL SET.VISIBLE(109, -1)

          ENDIF
          CALL SET.VISIBLE(3, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(3)),"MP 7") ! lower range
        ENDIF
      ENDIF

      IF (CURRENT.FIELD = 3 AND                  \ lower range field
          INP.FIELD$(2) = "2") THEN BEGIN        ! range of UPCs selected
        CALL SET.VISIBLE(4, -1)
        CALL MMCFEC05(PUTF(INP.FIELD$(4)),"MP 7") ! get upper range
      ENDIF

      IF CURRENT.FIELD = 5 THEN BEGIN            ! y/n field
        INP.FIELD$(5) = UCASE$(INP.FIELD$(5))    ! make it uppercase
        CALL MMCFEC05(PUTF(INP.FIELD$(5)),"MP 7") ! redisplay it
      ENDIF

      IF CURRENT.FIELD = 6 THEN BEGIN            ! y/n field
        INP.FIELD$(6) = UCASE$(INP.FIELD$(6))    ! make it uppercase
        CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7") ! redisplay it
      ENDIF

      ! process terminating key
      RET.VAL = ON1
      IF TAB.PRESSED THEN BEGIN                  ! adjust cursor position
        CURRENT.FIELD = MOD(CURRENT.FIELD,LAST.FIELD) + 1 ! bump field loc
        IF (INP.FIELD$(2) = "1") THEN BEGIN      ! single UPC
          IF (CURRENT.FIELD > 3) THEN BEGIN      ! fields 4 - 5 are not shown
            CURRENT.FIELD = 1                    ! goto first field
          ENDIF
        ENDIF
        IF (INP.FIELD$(2) = "3") THEN BEGIN      ! report all UPCs
          IF (CURRENT.FIELD = 3) OR              \ fields 3 and 4 are not shown
             (CURRENT.FIELD = 4) THEN BEGIN
            CURRENT.FIELD = 5                    ! goto last field
          ENDIF
        ENDIF
      ENDIF
      IF END.PRESSED THEN BEGIN                  ! end key
        CURRENT.FIELD = LAST.FIELD               ! goto last field
        IF (INP.FIELD$(2) = "1") THEN BEGIN      ! single UPC
          CURRENT.FIELD = 3                      ! last field is #3
        ENDIF
      ENDIF
      IF HOME.PRESSED THEN BEGIN                 ! home key
        CURRENT.FIELD = 1                        ! goto first field
      ENDIF
      IF BACKTAB.PRESSED THEN BEGIN              ! back tab
        CURRENT.FIELD = MOD(CURRENT.FIELD + LAST.FIELD - 2,LAST.FIELD) + 1 ! back 1
        IF (INP.FIELD$(2) = "1") THEN BEGIN      ! single UPC
          IF (CURRENT.FIELD > 3) THEN BEGIN      ! fields 4 - 6 are not shown
            CURRENT.FIELD = 3                    ! goto last field
          ENDIF
        ENDIF
        IF (INP.FIELD$(2) = "3") THEN BEGIN      ! report all UPCs
          IF (CURRENT.FIELD = 3) OR              \ fields 3 and 4 are not shown
             (CURRENT.FIELD = 4) THEN BEGIN
            CURRENT.FIELD = 2                    ! goto second field
          ENDIF
        ENDIF
      ENDIF

      IF ENTER.PRESSED THEN BEGIN                ! enter pressed
        IF INP.FIELD$(2) = "1" THEN BEGIN        ! single UPC selected
          UPC.NUM$     = INP.FIELD$(3)           ! set UPC number
          IF UPC.NUM$ = "" THEN BEGIN            ! if not yet set
            CURRENT.FIELD = 3                    ! ensure user enters it
            ENTER.PRESSED = OFF0                 ! don't end yet
          ENDIF
        ENDIF
        IF INP.FIELD$(2) = "2" THEN BEGIN        ! range of UPCs selected
          START.RNG$ = INP.FIELD$(3)             ! start of UPC search
          END.RNG$ = INP.FIELD$(4)               ! end of UPC search
          IF END.RNG$ = "" THEN BEGIN            ! if not yet set
            CURRENT.FIELD = 4                    ! ensure user enters it
            ENTER.PRESSED = OFF0                 ! don't end yet
          ENDIF
          IF START.RNG$ = "" THEN BEGIN          ! if not yet set
            CURRENT.FIELD = 3                    ! ensure user enters it
            ENTER.PRESSED = OFF0                 ! don't end yet
          ENDIF
        ENDIF

        SCOPE         = VAL(INP.FIELD$(2))       ! set scope
        DESTINATION   = VAL(INP.FIELD$(1))       ! set destination
        QUIT.PRESSED  = OFF0
        ESC.PRESSED   = OFF0

        IF (DESTINATION <> DISPLAY) AND          \
           (MSG.NUM = 0)   THEN BEGIN
          MSG.NUM = 480                          ! Report processing done
        ENDIF
      ENDIF
    ENDIF
  WEND ! not QUIT, ENTER or ESC

END SUB ! HANDLE.USER.INPUT

!****************************************************************************!
!* MAIN                                                                     *!
!****************************************************************************!

  ON ERROR GOTO TRAP.IT

  EAMITEMR$ = "$AMITEMR"
  DIM FILE.KEYS$(5000)
  DIM IRRF.EXIT(10)

  CALL ACCESS.CHAIN.PARAMETERS                   ! get the chain parameters

  NOT.FOUND = 0
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
      IF (SCOPE = 2) AND                         \ Range of UPCs
         (RIGHT$("00000000000000" + START.RNG$,14) > \ begin UPC greater than
          RIGHT$("00000000000000" + END.RNG$,14))    \ end UPC
      THEN BEGIN
        SHOW.ERROR = 8                           ! "Range is invalid."
      ENDIF ELSE IF NOT.FOUND = 0 THEN BEGIN
        CALL FIND.FILES("???????.APL ")          ! look for APL files
      ENDIF
      IF SHOW.ERROR <> 0 THEN BEGIN              ! if any error
        ! do nothing but show the error (later)
      ENDIF ELSE IF FCNT = 0 THEN BEGIN          ! if no files found
        SHOW.ERROR = 2                           ! error message line
      ENDIF ELSE BEGIN
        CALL DISPLAY.FILES                       ! display file names

        IF ENTER.PRESSED THEN BEGIN
          IF SCOPE > 1 THEN BEGIN                ! all records
            CALL FIND.KEYS(39, 7)                ! pass rec len and key len
            CALL SORT.KEYS                       ! sort the keys
          ENDIF ELSE BEGIN                       ! single UPC
            REC.CNT = 1
            UPC.NUM$ = RIGHT$("00000000000000" + UPC.NUM$, 14)
            UPC.NUM$ = PACK$(UPC.NUM$)
            FILE.KEYS$(1) = UPC.NUM$
          ENDIF

          NOT.FOUND = 0
          CALL EXTRACT.DATA(39)                  ! pass record length

          IF NOT.FOUND THEN BEGIN
            SHOW.ERROR = 3                       ! "Items were not found"
            CALL INITIALIZE.VARIABLES
            GOTO REDO
          ENDIF
          IF INP.FIELD$(6) = "Y" THEN BEGIN      ! is it Y or N ?
            FOUND.ONLY = -1                      ! set report flag ON
          ENDIF ELSE BEGIN
            FOUND.ONLY = 0                       ! set report flag OFF
          ENDIF
          WRITE #36,110;INP.FIELD$(1) +          \ update menu defaults
                        INP.FIELD$(2) + INP.FIELD$(5) + INP.FIELD$(6)
          NOT.FOUND = 0

          CALL BUILD.REPORT
          IF NOT.FOUND THEN BEGIN
            SHOW.ERROR = 3                       ! "Items were not found"
          ENDIF
        ENDIF ! ENTER.PRESSED
      ENDIF ! FCNT
      CURRENT.FIELD = LAST.FIELD + 1             ! return to previous menu
      QUIT.PRESSED = 0                           ! return to initial menu
    ENDIF ! ENTER.PRESSED
  WEND
  CLOSE 36                                       ! menu selections

  CALL CHAIN.BACK                                ! chain back to required module

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

  IF (INWE.ERR$ = "OE") AND (INWE.ERRN% = 82) THEN BEGIN
    RESUME
  ENDIF

  CALL MMCFEC99("AP")                            ! log an error message
  STOP                                           ! do not continue

! End MAIN
