\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMWRCFC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC REPORT COMMON FUNCTIONS/ROUTINES    */
\/*                                                                */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 2004 ALL RIGHTS RESERVED           */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*                                                                */
\/*      PERSON RESPONSIBLE = GEIGER, K. H.                        */
\/*                                                                */
\/*      FUNCTION: EAMRPCFC                                        */
\/*                                                                */
\/*                ROUTINES USED BY WIC EBT REPORTS                */
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
\/*         LINKAGE: CALLED FROM MODULES PRINTING/DISPLAYING       */
\/*                  REPORT DATA.                                  */
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
\/*           NAME: MMCFEC08 - STRIPS LEADING AND TRAILING BLANKS  */
\/*           NAME: MMCFEC11 - GETS SYSTEM DATE AND TIME           */
\/*           NAME: MMCFEC14 - FORMATS DATE AND TIME               */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*             PRINTER : OUTPUT TO PRINTER FROM RPCFEC01          */
\/*             SCREEN  : OUTPUT VIA DISPLAY MANAGER               */
\/*             EAMRPT15: REPORT FILE                              */
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
\/*   IO00563 - WIC EBT Hot Card Report does not show page headers */
\/*             if the maximum number of pages to show is 1000 or  */
\/*             more.                                              */
\/*             GGK IBM 08Mar2005                                  */
\/*                                                                */
\/*   IO06355 - Reports fail with KF 80F306C2 if the number of     */
\/*             sectors in the keyed file is not exactly a         */
\/*             multiple of 8 (plus sector zero).                  */
\/*             GGK IBM 02Apr2007                                  */
\/*                                                                */
\/*   IO10187 - Price type 3 items do not have a maximum price,    */
\/*             therefore do not show them on the Maximum Price    */
\/*             report.                                            */
\/*             GGK IBM 06Feb2009                                  */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

%INCLUDE EAMUSED.J86             ! variable definitions for use
%INCLUDE EAMCFINC.J86            ! declarations for common functions
%INCLUDE EAMATTRD.J86            ! declarations for display manager attributes
%INCLUDE EAMITEMR.J86            ! GLOBAL Item Record File variables
%INCLUDE EAMWICEF.J86
%INCLUDE EAMRPINC.J86            ! variable definitions for calling RPCFECnn

  STRING GLOBAL                  \
    ATTR$,                       \ display manager field attribute
    MENU.PROMPTS$(1),            \ conditional prompt
    RPT.TITLES$(1),              \ report title array
    RPT.TITLE$,                  \ current report title
    STORE$,                      \ store number
    NOT.FOUND.MSG$,              \ item not found msg
    FIELD$(1),                   \ input fields
    HIGH$(1),                    \ upper range limit
    SCOPE.PROMPTS$(1),           \ report scope prompts
    COL.HEADINGS$(1),            \ report col headings array
    COL.HEADING$,                \ current report col headings
    COL.HEADING2$,               \ second line of heading
    ERROR.MESSAGES$(1),          \ screen error messages
    CARD.NUM$,                   \ single card report
    WIC.ERR$,                    \ CBASIC ERROR CODE
    FILE.LIST$(1),               \ list of files meeting search criteria
    RPT.STR$(1),                 \ raw report data
    FILE.KEYS$(1),               \ sorted file keys
    INP.FIELD$(1),               \ input field array from menus
    HCL.KEYS$(1),                \ hot card list keys
    AGENCY.TITLE$,               \ "AGENCY ID:"
    FILENAME.INDEX$              ! user-selected number for file name

  INTEGER*2 GLOBAL               \
    SAVE.INDEX,                  \ saved array index
    CURRENT.FIELD,               \ current screen pos
    NUM.COL.LINES,               \
    FIRST.FIELD,                 \ starting screen pos
    LAST.FIELD,                  \ ending screen pos
    LAST.PAGE,                   \ keep record of last page
    SCOPE,                       \ report scope
    NOT.FOUND,                   \ item not found flag
    NO.DISP,                     \ Flag if not to display screen in MMCFEC01
    TS.ER.RETURN,                \ INDICATES ERROR CODE
    SHOW.ERROR                   ! error.messages(show.error)

  INTEGER*4 GLOBAL               \
    START,                       \ first array index to display/file/print
    FCNT,                        \ number of files found
    SL.IE.SALEPRIC,              \ unit price from EAMITEMR
    RET.CODE                     ! return code from slookup

  ! added for reports
  STRING                         \
    FMT$,                        \ used in WRITE FORM
    REPORTED.AT$,                \ descriptor
    STORE.TITLE$,                \
    AGENCY.ID$,                  \
    OF$,                         \
    PAGE$,                       \
    WORK$,                       \
    WORK2$                       !

  INTEGER*4                      \
    MAX.PRICE,                   \ max WIC allowable price
    SHELF.PRICE                  ! single item shelf price

  INTEGER*2                      \
    LAST.DESTIN,                 \ destination of the previous report
    LEN.PAGE.STR,                \
    TITLE.DESC.NO,               \ title descriptor number
    WORK                         !

  INTEGER*1 GLOBAL               \
    SL.IE.METHOD                 ! PRICING METHOD

%INCLUDE SYSFUNC.J86             ! needed for slookup
%INCLUDE DMEXTR.J86              ! display manager external function def.
%INCLUDE EAMASMCT.J86            ! assembler routines

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
SUB MMCFEC04(FIELD$) EXTERNAL
  STRING FIELD$
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC05                                                *!
!* Description    : This subprogram handles fatal display manager errors.   *!
!****************************************************************************!
SUB MMCFEC05(RET.VAL,DM.CMD$) EXTERNAL
  INTEGER*2 RET.VAL                              ! return code
  STRING    DM.CMD$                              ! module ID + command ID
END SUB

!****************************************************************************!
!* Function Name  : MMCFEC08$                                               *!
!* Description    : This function strips leading and trailing blanks from a *!
!*                  passed string.                                          *!
!****************************************************************************!
FUNCTION MMCFEC08$(BLANK$) EXTERNAL
  STRING MMCFEC08$, BLANK$
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
!* Description    : This function formats the passed date and time.         *!
!****************************************************************************!
FUNCTION MMCFEC14$(DATETIME$) EXTERNAL
  STRING MMCFEC14$,DATETIME$
END FUNCTION

!****************************************************************************!
!* Subroutine Name: MMCFEC99                                                *!
!* Description    : This subprogram writes an error log entry.              *!
!****************************************************************************!
SUB MMCFEC99(IN.MODULE$) EXTERNAL
  STRING    IN.MODULE$
END SUB

!****************************************************************************!
!* Function Name  : RPCFEC02$                                               *!
!* Description    : This function pads out a string to a fixed length (the  *!
!*                  string will be "right justified").                      *!
!****************************************************************************!
FUNCTION RPCFEC02$(IN.STR$,LENGTH) EXTERNAL
  STRING RPCFEC02$,IN.STR$
  INTEGER LENGTH
END FUNCTION

!****************************************************************************!
!* Function Name  : RPCFEC04$                                               *!
!* Description    : This function builds a string of fixed length from the  *!
!*                  two strings passed. "string1.....string2".  If the      *!
!*                  strings are too long the right one will overlay the left*!
!****************************************************************************!
FUNCTION RPCFEC04$(L.STR$,R.STR$,LENGTH) EXTERNAL
   STRING RPCFEC04$,L.STR$,R.STR$
   INTEGER*2 LENGTH
END FUNCTION

!****************************************************************************!
!* Function Name  : ADXERROR                                                *!
!* Description    : This function logs an error message.                    *!
!****************************************************************************!
FUNCTION ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE) EXTERNAL
INTEGER*2 TERM,MSGNUM
INTEGER*1 SEVERITY,MSGGRP,EVENT
STRING    UNIQUE
END FUNCTION

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
!* Subroutine Name: CALC.TOTAL.MULTIPRICE                                   *!
!* Description    : This routine will calculate the price of total amount   *!
!*                  of a multipriced item.                                  *!
!****************************************************************************!
FUNCTION CALC.TOTAL.MULTIPRICE (QUANTITY)
  INTEGER*2 QUANTITY
END FUNCTION

!****************************************************************************!
!* Subroutine Name: ACCESS.CHAIN.PARAMETERS                                 *!
!* Description    : This routine accesses the chaining variables by         *!
!*                  including the base SA chain module EAMUSE.J86           *!
!****************************************************************************!
SUB ACCESS.CHAIN.PARAMETERS PUBLIC

  %INCLUDE EAMUSE.J86                            ! access chain parameters
! %INCLUDE EAMUDBUG.J86                          ! uncomment when debugging

END SUB ! ACCESS.CHAIN.PARAMETERS

!****************************************************************************!
!* Subroutine Name: REMOVE.LEADING.ZEROS                                    *!
!* Description    : This routine removes leading zeros from a string that   *!
!*                  is passed to it.                                        *!
!****************************************************************************!
SUB REMOVE.LEADING.ZEROS(STR.TO.CHG$) PUBLIC

  STRING                                         \
    STR.TO.CHG$

  INTEGER*2                                      \
    STR.LEN


  WHILE (LEFT$(STR.TO.CHG$, 1) = "0")
    STR.LEN = LEN(STR.TO.CHG$)
    STR.TO.CHG$ = RIGHT$(STR.TO.CHG$, STR.LEN - 1) ! remove one "0"
  WEND

END SUB ! REMOVE.LEADING.ZEROS

!****************************************************************************!
!* Subroutine Name: MMCFEC01                                                *!
!* Description    : Display report general screen.                          *!
!****************************************************************************!
SUB MMCFEC01 PUBLIC
  IF NOT NO.DISP THEN BEGIN
    DM.SCRNNO = 3
    CALL MMCFEC05(DISPD(DM.SCRNNO),"HC 2")
  ENDIF
END SUB ! MMCFEC01

!****************************************************************************!
!* Subroutine Name: INITIALIZE.DISPLAY.MANAGER                              *!
!* Description    : This routine initializes the display manager.           *!
!****************************************************************************!
SUB INITIALIZE.DISPLAY.MANAGER PUBLIC

  DM.DISPFILE$ = "RPP"                           ! Set screen ID for err log
  CALL MMCFEC05(INITDM(""),"RP 4")               ! initialize display mgr
  CALL MMCFEC05(OPNDIS("EAMRPP3S"),"RP 6")       ! open screen file

END SUB ! INITIALIZE.DISPLAY.MANAGER

!****************************************************************************!
!* Subroutine Name: GET.DESCRIPTORS                                         *!
!* Description    : This subprogram reads in the procedure descriptors from *!
!*                  the reports descriptors file.  It also sets up the      *!
!*                  words used in describing the entry.                     *!
!*                                                                          *!
!* SUB GET.DESCRIPTORS                                                      *!
!*                                                                          *!
!* Open file                                                                *!
!* Read report title                                                        *!
!* Read in the entry key-words                                              *!
!* Read in the print header words                                           *!
!* Close the file                                                           *!
!*                                                                          *!
!* END SUB                                                                  *!
!*                                                                          *!
!****************************************************************************!
SUB GET.DESCRIPTORS PUBLIC
  INTEGER*2 INDEX

  DIM RPT.TITLES$(5)
  DIM SCOPE.PROMPTS$(13)
  DIM MENU.PROMPTS$(4)
  DIM COL.HEADINGS$(11)
  DIM ERROR.MESSAGES$(10)

  OPEN "EAMRDESC" RECL 49 AS 6  NOWRITE NODEL    ! Open report descriptor file
  READ #6,3;REPORTED.AT$
  READ #6,4;PAGE$
  PAGE$ = MMCFEC08$(PAGE$)                       ! remove lead/trail blanks
  READ #6,5;OF$
  LEN.PAGE.STR = 8 + LEN(PAGE$) + LEN(OF$)
  READ #6, 6; STORE.TITLE$
  READ #6,1736 ;NOT.FOUND.MSG$                   ! "Item not on file"

  FOR INDEX = 4530 TO 4533
    READ #6,INDEX ;RPT.TITLES$(INDEX - 4529)     ! Read report titles
  NEXT INDEX

  FOR INDEX = 4534 TO 4545
     READ #6,INDEX ;SCOPE.PROMPTS$(INDEX - 4533) ! Read in scope descriptors
  NEXT INDEX

  FOR INDEX = 4546 TO 4548
    READ #6,INDEX ;MENU.PROMPTS$(INDEX - 4545)   ! Read prompts for selection
  NEXT INDEX

  READ #6, 4549; AGENCY.TITLE$                   ! agency id

  FOR INDEX = 4550 TO 4559
    READ #6, INDEX; COL.HEADINGS$(INDEX - 4549)  ! reports column headings
  NEXT INDEX

  FOR INDEX = 4562 TO 4571
    READ #6, INDEX; ERROR.MESSAGES$(INDEX - 4561) ! screen error messages
  NEXT INDEX
  CLOSE 6                                        ! Close file

END SUB ! GET.DESCRIPTORS

!****************************************************************************!
!* Subroutine Name: SUB FIND.FILES                                          *!
!* Description    : This subroutine uses the sysfunc function "slookup" to  *!
!*                  search adx_idt1 for the type of file that is passed to  *!
!*                  it in "pattern$".  An array is filled with the file     *!
!*                  names that meet the search criteria.                    *!
!****************************************************************************!
SUB FIND.FILES(PATTERN$) PUBLIC

  INTEGER*2                                      \
    I, J                                         !

  STRING                                         \
    PATTERN$,                                    \ search pattern
    BUFFER$,                                     \ 4 bytes for each possible file
    LOGICAL.FILE$,                               \ slookup parm
    TEMP$                                        !

  BUFFER$ = STRING$(850, CHR$(0))                ! init things

  RET.CODE = 0
  DIM FILE.LIST$(0)
  LOGICAL.FILE$ = "WIC1:" + PATTERN$             ! build search parm
  ! Lookup how many files there are
  CALL SLOOKUP(RET.CODE, LOGICAL.FILE$, BUFFER$, TEMP$, 0)

  IF RET.CODE > 0 THEN BEGIN                     ! found files
    FCNT = RET.CODE                              ! setup count
    DIM FILE.LIST$(FCNT + 1)                     ! dim list array

    FOR I = 1 TO FCNT
      ! when using slookup, the receiving string/array element must be
      ! initialized to at least 36 characters in length. This is a
      ! requirement of SLOOKUP
      FILE.LIST$(I) = STRING$(36, " ")           ! initialize the array
    NEXT I
    ! populate the array with the file names
    CALL SLOOKUP(RET.CODE, LOGICAL.FILE$, BUFFER$, FILE.LIST$(1), FCNT)

    ! sort the file names alphabetically, in ascending order
    FOR I = 1 TO (FCNT - 1)
      FOR J = (I + 1) TO FCNT
        IF FILE.LIST$(I) > FILE.LIST$(J) THEN BEGIN
          TEMP$ = FILE.LIST$(I)
          FILE.LIST$(I) = FILE.LIST$(J)
          FILE.LIST$(J) = TEMP$
        ENDIF
      NEXT J
    NEXT I

    ! The menu will blow up if we ever get more than 40 files.
    ! Prevent the blow-up, and fix with an APAR later, if needed.
    IF (FCNT > 40) THEN  \
      FCNT = 40

  ENDIF


END SUB ! FIND.FILES

!****************************************************************************!
!* Subroutine Name: DISPLAY.COL.HDR                                         *!
!* Description    : This routine displays the file list column headings     *!
!****************************************************************************!
SUB DISPLAY.COL.HDR(START.LOC)

  INTEGER*2                                      \
    START.LOC

  CALL MMCFEC05(POSF(START.LOC),"HC 3")          ! column 2 header
  ATTR$ = SETF(VISIBLE$)
  CALL MMCFEC05(POSF(START.LOC+1),"HC 3")        !
  ATTR$ = SETF(VISIBLE$)
  CALL MMCFEC05(POSF(START.LOC+2),"HC 3")        !
  ATTR$ = SETF(VISIBLE$)
END SUB ! DISPLAY.COL.HDR

!****************************************************************************!
!* Subroutine Name: DISPLAY.FILES                                           *!
!* Description    : This routine displays up to 40 file names, along with   *!
!*                  an index that allows the user to select a file for      *!
!*                  processing.                                             *!
!****************************************************************************!
SUB DISPLAY.FILES PUBLIC

  INTEGER*2                                      \
    SAVE.LAST.PAGE,                              \
    DATA.NOT.OK,                                 \
    NUM.COLS,                                    \
    ARRAY.INDEX                                  !

  INTEGER                                        \
    I1                                           !

  CALL MMCFEC05(DISPD(50), "HC 2")               ! screen to hold list of files

  CALL MMCFEC05(POSF(200),"HC 3")                ! report title
  CALL MMCFEC05(PUTF(RPT.TITLE$),"HC 7")
  ATTR$ = SETF(VISIBLE$)

  NUM.COLS = (FCNT - 1)/10                       ! num cols to display
  SAVE.LAST.PAGE = LAST.PAGE                     ! just in case
  LAST.PAGE = (FCNT - 1)/40                      ! num potential displays screens

  IF LAST.PAGE = 0 THEN BEGIN                    ! if only 1 page
    IF NUM.COLS > 0 THEN BEGIN                   ! if 2 cols
      CALL DISPLAY.COL.HDR(41)                   ! display col hdrs
    ENDIF                                        ! end 2 cols
    IF NUM.COLS > 1 THEN BEGIN                   ! if 3 cols
      CALL DISPLAY.COL.HDR(44)                   ! display col hdrs
    ENDIF                                        ! end 3 cols
    IF NUM.COLS > 2 THEN BEGIN                   ! if 4 cols
      CALL DISPLAY.COL.HDR(47)                   ! display col hdrs
    ENDIF                                        ! end 4 cols
  ENDIF

  LAST.PAGE = SAVE.LAST.PAGE                     ! now restore it

  FOR I1 = 1 TO FCNT                             ! populate the screen with the
    CALL MMCFEC05(POSF(I1),"HC 3")               !   names of the files that meet
    CALL MMCFEC05(PUTF(STR$(I1)),"HC 7")         !   the selection criteria
    ATTR$ = SETF(VISIBLE$)

    CALL MMCFEC05(POSF(I1+100),"HC 3")
    CALL MMCFEC05(PUTF(FILE.LIST$(I1)),"HC 7")   ! the file name
    ATTR$ = SETF(VISIBLE$)
  NEXT

  QUIT.PRESSED = 0                               ! init
  ESC.PRESSED = 0
  ENTER.PRESSED = 0

  WHILE NOT (QUIT.PRESSED OR                     \ while user still editing options
             ESC.PRESSED OR                      \
             ENTER.PRESSED)

    CALL MMCFEC05(POSF(50),"HC 3")               ! Position cursor

    ATTR$ = SETF(VISIBLE$)
    CALL MMCFEC05(PUTF(FILENAME.INDEX$),"HC 7")  ! user-typed number

    YESORNO = OFF0                               ! majority not yes/no field
    NEED.RANGE.CHECK = ON1                       ! assume we'll need a range check
    MAX.HELP = 1                                 ! 1 help screen
    LOW.END$ = "1"
    HIGH.END$ = STR$(FCNT)
    DATA.NOT.OK = -1                             ! loop to make sure data OK
    WHILE DATA.NOT.OK                            ! while trying to get valid data
      DATA.NOT.OK = 0                            ! reset loop flag
      CALL MMCFEC04(FILENAME.INDEX$)             ! Get operator input
      CALL MMCFEC05(POSF(50),"HC 3")
      ARRAY.INDEX = VAL(FILENAME.INDEX$)         ! set to integer format
      SAVE.INDEX = ARRAY.INDEX
    WEND
  WEND

  IF ENTER.PRESSED THEN BEGIN                    ! if user pressed enter
    FILENAME$ = FILE.LIST$(ARRAY.INDEX)          ! setup file name
    FILENAME$ = "WIC1:" + FILENAME$              ! add path to file name
    AGENCY.ID$ = MID$(FILENAME$, 11, 2)          ! save agency id
    CALL MMCFEC03(254, FILENAME$)
  ENDIF

END SUB ! DISPLAY.FILES

!****************************************************************************!
!* Subroutine Name: SORT.KEYS                                               *!
!* Description    : This routine sorts the hot card key array.              *!
!****************************************************************************!
SUB SORT.KEYS PUBLIC

  STRING                                         \
    WORK1$                                       ! work string

  INTEGER*2                                      \
\   PT(1),      \ Not used, deleted by IO10187   \
    WORK,                                        \
    A,                                           \
    B,                                           \
    C                                            !

\ DIM PT(REC.CNT) ! Not used, deleted by IO10187 ! dimension array of pointers

  A = 1                                          !
  WHILE A <= REC.CNT                             ! while not sorted
    B = A                                        ! points at current one
    C = B - 1                                    ! points at last one
                                                 ! if we need to move this one
    WHILE (FILE.KEYS$(B) < FILE.KEYS$(C)) AND (C > 0)
      WORK1$ = FILE.KEYS$(B)                     ! swap the record keys
      FILE.KEYS$(B) = FILE.KEYS$(C)              !
      FILE.KEYS$(C) = WORK1$                     !
      B = B - 1                                  ! move back one
      C = C - 1                                  !
    WEND                                         !

    A = A + 1
  WEND

END SUB ! SORT.KEYS

!****************************************************************************!
!* Subroutine Name: FIND.KEYS                                               *!
!* Description    : This subroutine searches a keyed file, extracts the     *!
!*                  keys and places the keys into an array.                 *!
!****************************************************************************!
SUB FIND.KEYS(REC.LEN, KEY.LEN) PUBLIC

  STRING                                         \
    KEYCODE$,                                    \ item code
    A$,                                          \ work string
    NULLKEY$,                                    \ null packed item code string
    FORM$,                                       \ FORM for reading data IO06355
    TEMP$                                        ! work string

  INTEGER*2                                      \
    REC.LEN,                                     \ record length
    KEY.LEN,                                     \ key length
    RECORDS%,                                    \ record count
    I%,                                          \ indices
    J%,                                          \
    K%                                           !

  INTEGER*4                                      \
    SECTORS%                                     ! sector count

  OPEN FILENAME$ AS 10 BUFFSIZE 4096 NOWRITE NODEL
  READ FORM "C512"; #10; A$                      ! READ THE FIRST SECTOR
  SECTORS% = SIZE(FILENAME$)/512 - 1             ! GET THE FILE SIZE
  RECORDS% = 0                                   ! INITIALIZE RECORD COUNT
  NULLKEY$ = STRING$(KEY.LEN,CHR$(0))            ! ZERO STRING
  FILE.KEYS$(0) = STRING$(KEY.LEN," ")
  I% = -1                                        ! FLAG NOT AT END OF FILE
  FORM$ = "C4096"                                ! Read 8 sectors IO06355

  !*************************************************************!
  !* SEARCH WIC STATUS FILE FOR KEYS                           *!
  !*************************************************************!
  WHILE  I%                                      ! while more sectors
!AIO06355 Read 4096 bytes at a time, or the actual remainder if less than 4096
\   READ FORM "C4096"; #10; A$                   ! READ 8 SECTORS
    IF (SECTORS% < 8) AND (SECTORS% > 0) THEN BEGIN ! LESS THAN 4096 BYTES
      FORM$ = "C" + STR$(SECTORS% * 512)            ! ACTUAL REMAINING BYTES
    ENDIF
    READ FORM FORM$; #10; A$                     ! READ 1 TO 8 SECTORS
!EIO06355
    SECTORS% = SECTORS% - 8                      ! DEC. SECTORS LEFT

    IF SECTORS% < 1 THEN BEGIN                   ! IF LAST READ THEN
      I% = 0                                     ! SET FLAG TO END LOOP
      IF SECTORS% < 0 THEN BEGIN                 ! OVERSHOT FILE
        J% = (8 + SECTORS%) * 512                ! COMPUTE REAL DATA
        K% = (0 - SECTORS%) * 512                ! COMPUTE JUNK DATA
        A$ = LEFT$(A$,J%) +                      \ SAVE GOOD STUFF AND
                  STRING$(K%,CHR$(0))            ! ZERO JUNK FROM READ
      ENDIF
    ENDIF

    FOR K% = 0 TO 3584 STEP 512                  ! FOR EACH OF 8 SECTORS
      ! FOR EACH OF THE RECORDS
      FOR J% = K%+5 TO K%+511-REC.LEN STEP REC.LEN
        KEYCODE$ = MID$(A$,J%,KEY.LEN)           ! CAPTURE THE KEYCODE

        IF KEYCODE$ = NULLKEY$ THEN BEGIN        ! IF KEY IS ZERO THEN
          J% = K% + (511-REC.LEN)                ! END PROCESS FOR SECTOR
        ENDIF ELSE BEGIN
          RECORDS% = RECORDS% + 1                ! INC RECORD COUNTER
          FILE.KEYS$(RECORDS%) = KEYCODE$
        ENDIF
NEXT.REC:
      NEXT J%                                    ! NEXT RECORD
    NEXT K%                                      ! NEXT SECTOR
  WEND

  CLOSE 10                                       ! close the file
  REC.CNT = RECORDS%                             ! save record count
END SUB ! FIND.KEYS

!****************************************************************************!
!* Subroutine Name: HANDLE.MISSING.ITEMS                                    *!
!* Description    : This routine removes WIC items that are not in the      *!
!*                  current item record file.                               *!
!****************************************************************************!
SUB HANDLE.MISSING.ITEMS(EXCEEDED.ONLY) PUBLIC
  INTEGER *2                                     \
    EXCEEDED.ONLY,                               \ flag for items > max price
    PRICE.TYPE,                                  \ WIC price type, constant 1
    QUANTITY,                                    \ item quantity
    I%, J%                                       ! loop variables

  INTEGER *4                                     \
    MAX.PRICE                                    ! max WIC allowable price

  ! all WIC items were read in, and EAMITEMR is open.
  ! Remove items 'not on file'.  This subroutine is called unconditionally
  ! in the Max Price report, and conditionally in the All Items report.
  FOR I% = START TO REC.CNT
    ! if this item is not on file, null its key in the list
    IR.ITEMCODE$ = RIGHT$(FILE.KEYS$(I%), 6)
    TS.ER.RETURN = -1                            ! expect error
    WIC.ERR$ = ""                                ! init
    CALL IRRFEC.READ02 (4, 0)                    ! read item record

    IF (WIC.ERR$ = "EF") THEN BEGIN              ! item not in item rec file
      FILE.KEYS$(I%) = ""                        ! flag removal of this record
    ENDIF ELSE BEGIN                             ! item record exists
!AIO10187 There is no max price for PRICE.TYPE=3 items.  Remove them
      PRICE.TYPE = VAL(UNPACK$(MID$(RPT.STR$(I%), 28, 1))) ! WIC price type "1"
      IF PRICE.TYPE = 3 THEN BEGIN
        FILE.KEYS$(I%) = ""                      ! flag removal of this record
      ENDIF
!EIO10187
      IF EXCEEDED.ONLY THEN BEGIN                ! show price exceeded only?
        MAX.PRICE = VAL(UNPACK$(MID$(RPT.STR$(I%), 25, 3))) ! get WIC max allowed price
!AIO10187 next line was moved up
\       PRICE.TYPE = VAL(UNPACK$(MID$(RPT.STR$(I%), 28, 1))) ! WIC price type "1"
!EIO10187
        QUANTITY = 1                             ! want single item price
        IF SL.IE.METHOD <= 1 THEN BEGIN          ! price method 0 or 1
          SL.IE.SALEPRIC = PACKBIN4 (IR.SALEPRIC$, 1, 4) ! single item price
        ENDIF ELSE BEGIN                         ! price method > 1
          SL.IE.SALEPRIC = CALC.TOTAL.MULTIPRICE (QUANTITY) ! get price of 1 item
        ENDIF
        SHELF.PRICE = SL.IE.SALEPRIC             ! what this store charges
        IF SHELF.PRICE <= MAX.PRICE THEN BEGIN   ! shelf price under max price
          FILE.KEYS$(I%) = ""                    ! flag removal of this record
        ENDIF
      ENDIF ! EXCEEDED.ONLY
    ENDIF
  NEXT I%

  ! now remove all the items that were flagged as not found, or price is OK
  J% = 0
  FOR I% = START TO REC.CNT
    IF FILE.KEYS$(I%) <> "" THEN BEGIN           ! if NOT marked for removal
      J% = J% + 1                                ! keep it
      FILE.KEYS$(J%) = FILE.KEYS$(I%)            ! record key
      RPT.STR$(J%) = RPT.STR$(I%)                ! report string
    ENDIF
  NEXT I%
  REC.CNT = J%                                   ! the new record count
  START = 1                                      ! now starts at 1
END SUB ! HANDLE.MISSING.ITEMS

!****************************************************************************!
!* Subroutine Name: EXTRACT.DATA                                            *!
!* Description    : This routine reads a file and populates an array        *!
!*                  with the contents of each record.                       *!
!****************************************************************************!
SUB EXTRACT.DATA(REC.LEN) PUBLIC

  INTEGER*2                                      \
    I,                                           \
    REC.LEN

  STRING                                         \
    READ.FMT$

  DIM RPT.STR$(REC.CNT)

  OPEN FILENAME$ KEYED RECL REC.LEN AS 10 NOWRITE NODEL ! open input file

  READ.FMT$ = "C" + STR$(REC.LEN)
  WIC.ERR$ = ""                                  ! init

  FOR I = 1 TO REC.CNT
    READ FORM READ.FMT$; #10 KEY FILE.KEYS$(I); RPT.STR$(I) ! extract data

    IF (WIC.ERR$ = "EF") THEN BEGIN              ! item not in item rec file
      NOT.FOUND = -1
    ENDIF
  NEXT I

  CLOSE 10

END SUB ! EXTRACT.DATA

!****************************************************************************!
!* Function Name  : FIRST.HEADER$                                           *!
!* Description    : This function builds the first line of the report       *!
!*                  header.                                                 *!
!****************************************************************************!
FUNCTION FIRST.HEADER$

  STRING FIRST.HEADER$

  WORK$ = MMCFEC08$(RPT.TITLE$) +                \ report title
          STORE.TITLE$          +                \ "store"
          STORE$                                 ! store number

  WORK = (80 - (2*LEN.PAGE.STR) - LEN(WORK$))    ! center it

  IF WORK < 0 THEN BEGIN
    WORK = 11
  ENDIF ELSE BEGIN
    WORK = (WORK/2) + LEN.PAGE.STR - 2
  ENDIF

  IF DESTINATION = DISPLAY THEN BEGIN
    WORK = WORK - 10
  ENDIF

  FIRST.HEADER$ = STRING$(WORK," ") + WORK$

END FUNCTION ! FIRST.HEADER$

!****************************************************************************!
!* Function Name  : SECOND.HEADER$                                          *!
!* Description    : This function builds the second line of the report      *!
!*                  header.                                                 *!
!****************************************************************************!
FUNCTION SECOND.HEADER$

  STRING SECOND.HEADER$

  WORK$ = REPORTED.AT$ + MMCFEC11$
  WORK2$ = AGENCY.TITLE$ +                       \
           AGENCY.ID$                            !

  ! Concatenate the two strings to form the second line.
  SECOND.HEADER$ = RPCFEC04$(WORK2$,WORK$,78)

END FUNCTION ! SECOND.HEADER$

!****************************************************************************!
!* Function Name  : PAGE.OF.PAGE.STR$                                       *!
!* Description    : This function builds the 'Page xx of xx' string.        *!
!****************************************************************************!
FUNCTION PAGE.OF.PAGE.STR$

  STRING PAGE.OF.PAGE.STR$

  WORK$ = PAGE$ + RPCFEC02$(STR$(PAGE),4)

  IF MAX.PAGE = 1 THEN BEGIN                     ! maximum number of pages is 1
    PAGE.OF.PAGE.STR$ = NULL$
  ENDIF ELSE BEGIN
    IF NO.MAX.PAGE THEN BEGIN                    ! no maximum page
      PAGE.OF.PAGE.STR$ = WORK$
    ENDIF ELSE BEGIN                             ! bld retd str as pg xxx of xxx
      PAGE.OF.PAGE.STR$ = WORK$ + OF$ + RPCFEC02$(STR$(MAX.PAGE),4)
    ENDIF
  ENDIF

END FUNCTION ! PAGE.OF.PAGE.STR$

!****************************************************************************!
!* Subroutine Name: WIC.OUTPUT                                              *!
!* Description    : This subroutine outputs a passed string to the          *!
!*                  specified destination.                                  *!
!* NOTE: This feature was unable to use the common output function,         *!
!*       RPCFEC01, found in EAMRPCFC due to the fact that a local variable, *!
!*       SKIP$, was not initialized unless a call was made to RPCFEC10      *!
!*       first.                                                             *!
!****************************************************************************!
SUB WIC.OUTPUT(LINE$) PUBLIC

  STRING                                         \
    LINE$,                                       \ line being written
    FMT$,                                        \ write format
    SKIP$                                        ! carriage return + line feed

%INCLUDE EAMRPU01.J86                            ! REPORTS USER EXIT

  IF DESTINATION = DISPLAY THEN BEGIN            ! if to screen
    CALL MMCFEC05(POSF(LINE.NO),"2E 3")          ! position cursor
    CALL MMCFEC05(PUTF(LINE$),"2E 7")            ! display string
  ENDIF ELSE BEGIN                               ! if to file/printer
    SKIP$ = CHR$(13) + CHR$(10)                  ! 0D0A
    FMT$ = "C" + STR$(LEN(LINE$)) + ",C2"        !
    WRITE FORM FMT$;#1;LINE$,SKIP$               ! write string
  ENDIF

  LINE.NO = LINE.NO + 1                          ! bump line number

END SUB ! WIC.OUTPUT

!****************************************************************************!
!* Subroutine Name: WIC.BUILD.HEADINGS                                      *!
!* Description    : This routine builds the current report headings.        *!
!****************************************************************************!
SUB WIC.BUILD.HEADINGS(INDEX) PUBLIC

  INTEGER*2                                      \
    INDEX                                        ! index to col hdings array

  STRING                                         \
    LINE1$,                                      \ first 40 chars
    LINE2$                                       ! last 40 chars

  LINE1$ = COL.HEADINGS$(INDEX)                  ! get the heading
  LINE2$ = COL.HEADINGS$(INDEX + 1)
  COL.HEADING$ = LINE1$ + LINE2$                 ! make one line
  COL.HEADING2$ = ""                             ! start with null

  IF NUM.COL.LINES = 2 THEN BEGIN                ! should be 2 col headings
    LINE1$ = COL.HEADINGS$(INDEX + 2)            ! get the pieces
    LINE2$ = COL.HEADINGS$(INDEX + 3)
    COL.HEADING2$ = LINE1$ + LINE2$              ! make one line
  ENDIF

END SUB ! WIC.BUILD.HEADINGS

!****************************************************************************!
!* Subroutine Name: WIC.BUILD.HEADER                                        *!
!* Description    : This subroutine builds and outputs the header for the   *!
!*                  report.                                                 *!
!****************************************************************************!
SUB WIC.BUILD.HEADER PUBLIC

  STRING                                         \
    HEADER.ONE$,                                 \
    HEADER.TWO$,                                 \
    WIC.ERR$                                     ! ERROR reading a record

  INTEGER*2                                      \
    TITLE.NO,                                    \ RDESC descriptor record
    WIC.ERRF%                                    ! ERRF%

  INTEGER*4                                      \
    WIC.ERRN                                     ! ERRN

  ON ERROR GOTO BUILD.HEADER.ERR

  IF LAST.DESTIN <> DESTINATION THEN BEGIN
    LAST.DESTIN = DESTINATION
    FIRST.HEADER = ON1
  ENDIF

  IF FIRST.HEADER THEN BEGIN                     ! first time called for this rpt
    HEADER.ONE$ = FIRST.HEADER$                  ! build first line
    HEADER.TWO$ = SECOND.HEADER$                 ! build second line
    FIRST.HEADER = OFF0
    IF DESTINATION = DISPLAY THEN BEGIN          ! set page number
      PAGE = 1
    ENDIF ELSE BEGIN
      PAGE = 0
    ENDIF
  ENDIF

  ! if not to screen and no compressed print, write the eject character
  ! and bump the page number.
  IF (DESTINATION <> DISPLAY) AND (NOT COMPRESSED.PRINT) THEN BEGIN
    WRITE FORM "C1";#1;CHR$(12)
    PAGE = PAGE + 1
  ENDIF

  ! if not to screen and compressed print, write the eject and compress
  ! the print string, then bump the page number.
  IF (DESTINATION <> DISPLAY) AND COMPRESSED.PRINT THEN BEGIN
    FMT$ = "C" + STR$(LEN(COMPRESSED.PRINT.ON$) + 1)
    WRITE FORM FMT$;#1;CHR$(12),COMPRESSED.PRINT.ON$
    PAGE = PAGE + 1
  ENDIF

  IF DESTINATION = DISPLAY THEN BEGIN
    WORK = 68
  ENDIF ELSE BEGIN
    WORK = 78
  ENDIF

  WORK2$ = RPCFEC04$(HEADER.ONE$,PAGE.OF.PAGE.STR$,WORK)

  LINE.NO = 1                                    ! reset position

  CALL WIC.OUTPUT(WORK2$)                        ! first line of header
  CALL WIC.OUTPUT(NULL$)                         ! skip a line
  CALL WIC.OUTPUT(HEADER.TWO$)                   ! second line of header
  CALL WIC.OUTPUT(NULL$)                         ! skip a line
  CALL WIC.OUTPUT(COL.HEADING$)                  ! write line

  IF LEN(COL.HEADING2$) > 0 THEN BEGIN           ! have a 2nd col heading line
    CALL WIC.OUTPUT(COL.HEADING2$)               ! write it
  ENDIF

EXIT.BUILD.HEADER:

  EXIT SUB

BUILD.HEADER.ERR:

  WIC.ERR$ = ERR
  WIC.ERRN = ERRN
  WIC.ERRF% = ERRF%

  IF WIC.ERR$ = "OE" THEN BEGIN                  ! if open error
    RESUME EXIT.BUILD.HEADER                     ! exit the subroutine
  ENDIF

  ! log unforeseen errors and exit the subroutine.
  CALL MMCFEC99("2E")                            ! Log error
  RESUME EXIT.BUILD.HEADER                       ! exit the subroutine

END SUB ! WIC.BUILD.HEADER

!****************************************************************************!
!* Subroutine Name: SETUP.NEWPAGE                                           *!
!* Description    : This subroutine creates the next page on a display.     *!
!****************************************************************************!
SUB SETUP.NEWPAGE(INDEX) PUBLIC

  INTEGER*2                                      \
    INDEX

  IF DESTINATION = DISPLAY THEN BEGIN
    CALL MMCFEC05(DISPD(100), "MP 2")            ! common report screen
  ENDIF
  CALL WIC.BUILD.HEADINGS(INDEX)
  CALL WIC.BUILD.HEADER

  IF NUM.COL.LINES = 1 THEN BEGIN                ! 1 line col heading
    LINE.NO = 6
  ENDIF ELSE BEGIN                               ! 2 line col heading
    LINE.NO = 7
  ENDIF

END SUB ! SETUP.NEWPAGE

!****************************************************************************!
!* Subroutine Name: CHAIN.BACK                                              *!
!* Description    : This routine chains this program back.                  *!
!****************************************************************************!
SUB CHAIN.BACK PUBLIC

  STRING                                         \
    CHAINTO$                                     ! name of program to chain to

  IF ESC.PRESSED THEN BEGIN                      ! if escape pressed
    CHAINTO$ = "EAMMM00L.286"                    ! return to SA main menu
  ENDIF ELSE BEGIN                               ! else escape not pressed
    CHAINTO$ = MM.CHAINFROM$                     ! rtn to personalization main menu
  ENDIF                                          ! if escape pressed

  %INCLUDE EAMCHAIN.J86                          ! base SA code for chaining

END SUB ! CHAIN.BACK
