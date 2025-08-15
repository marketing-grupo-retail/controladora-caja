\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMRPMPC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: WIC EBT MAXIMUM PRICE REPORT            */
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
\/*                ROUTINES USED BY MAX PRICE REPORT PROCEDURE     */
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
\/*   IO00563 - WIC Max Prices Per Agency report shows the wrong   */
\/*             set of items if a range is specified and some WIC  */
\/*             items are not on file.                             */
\/*             GGK IBM 01Mar2005                                  */
\/*                                                                */
\/*   IO10187 - A SU error occurs while displaying the Maximum     */
\/*             Price report.  The initial setting for the maximum */
\/*             number of items is inadequate.                     */
\/*             GGK IBM 06Feb2009                                  */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

%INCLUDE EAMUSED.J86                   ! variable definitions for use
%INCLUDE EAMCFINC.J86                  ! declarations for common functions
%INCLUDE EAMATTRD.J86                  ! declarations for display mgr attributes
%INCLUDE EAMIRRFG.J86                  ! Item Record File GLOBALS
%INCLUDE EAMITEMR.J86                  ! GLOBAL Item Record File variables
%INCLUDE EAMWICEF.J86
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
    START,                             \ first UPC to print IO00563
    MAX.PRICE,                         \ max WIC allowable price
    SHELF.PRICE,                       \ single item shelf price
    WEIGHT.MULTIPLYER,                 \ 10 to power of places after point
    SL.IT.XPRICE,                      \ INTEGER FIELD FOR PRICE
    SL.IE.SALEPRIC2,                   \ ALTERNATIVE PRICE FROM IR
    SL.IE.SALEPRIC,                    \ UNIT PRICE
    SL.IE.QTYORWGT                     ! QUANTITY OR WEIGHT

  INTEGER*4 GLOBAL                     \
    IR.PRICE1,                         \
    IR.PRICE2,                         \
    FCNT                               ! SLOOKUP number of files found

  INTEGER*2 GLOBAL                     \
    I,                                 \ scratch
    CURRENT.FIELD,                     \ current screen pos
    FIRST.FIELD,                       \ starting screen pos
    LAST.FIELD,                        \ ending screen pos
    LAST.PAGE,                         \ keep record of last page
    PAGE.SO.FAR,                       \ pages processed so far
    PAGE.LEN,                          \ num lines per page
    GC.PRINTLEN,                       \ length of print line
    NEXT.FIELD,                        \
    GC.PRINTER,                        \ flag if system contains a printer
    MSG.NO,                            \ status/error message number
    TS.LINEDATA2,                      \ OTHER DATA
    IR.ITEMTYPE,                       \
    IR.PRICING.METHOD,                 \
    NUM.COL.LINES,                     \
    MSG.NUM,                           \
    SCOPE,                             \ scope of report
    COMPLETED,                         \ flag indicating that the report ended
    RET.VAL,                           \ ADXERROR Return value parameter
    THRESHOLD.ITEM,                    \ flag for pricing method 3 threshold
    EXCEEDED.ONLY,                     \ flag to report only items > max price
    UK.ALLSCALES.OPTION,               \ Set if to.scale.type > 1
    TO.WGT.DECPOS,                     \ DECIMAL PLACES FOR WEIGHTS
    TS.ERRF,                           \ ERROR SESSION NUMBER
    TS.ERRFSAV(1),                     \ STACKED ERROR SESSION NUMBER
    TS.ER.UNEX,                        \ INDICATES UNEXPECTED ERROR HAS OCCURRED
    TS.ER.RETURN,                      \ INDICATES WHETHER APPLICATION WISHES TO
    NOT.FOUND,                         \ item not found flag
    MAX.KEYS,                          \ max number of items to process
    SHOW.ERROR                         ! error.messages(show.error)

  INTEGER*1 GLOBAL                     \
    TO.WGTROUND,                       \    WEIGHT ROUNDING
    TO.PRICEROUND,                     \    PRICE ROUNDING
    SL.IE.LQD.DEALLIMIT,               \ Limited Qty Discount deal limit quantity
    SL.IE.LQD.REDUCEDQTY,              \ Limited Qty Discount reduced quantity
    SL.IT.INDICAT3,                    \ INDICATOR 3 (SEE BELOW)
    SL.IT.INDICAT3A,                   \ INDICATOR 3 HIGH NIBBLE
                                       \ 0 = NORMAL SALE
                                       \ 1 = DEPOSIT
                                       \ 2 = REFUND
                                       \ 3 = DEPOSIT RETURN
                                       \ 4 = MISC TRANS RECEIPT
                                       \ 5 = MISC TRANS PAYOUT
                                       \ 6 = MANF COUPON
                                       \ 7 = STORE COUPON
                                       \ 8 = ITEM SALE CANCEL
                                       \ 9 = DEPOSIT CANCEL
    SL.IT.INDICAT3B,                   \ INDICATOR 3 LOW NIBBLE
                                       \ 0 = SCANNED ITEM CODE
                                       \ 1 = KEYED ITEM CODE
                                       \ 2 = ITEM LOOKUP KEY USED
                                       \ 3 = ITEM CODE LINKED TO
                                       \ 4 = ITEM CODE WANDED
                                       \ >4 RESERVED
    SL.IE.INDICAT1,                    \ INDICATOR 1
                                       \   BIT 0 - CANNOT BE USED
                                       \   SL.IEWGTKEY  1 - WEIGHT KEYED
                                       \   SL.IEQTYKEY  2 - QUANTITY KEYED
                                       \   SL.IETARKEY  3 - TARE CODE KEYED
                                       \   SL.IEWGTSCL  4 - WEIGHT FROM SCALE
                                       \   SL.IEQTYREQ  5 - QUANTITY REQUIRED
                                       \   SL.IEQTYREP  6 - QUANTITY FROM REPEAT ENTRY
                                       \   SL.IECPENQT  7 - COUPON ENLARGED QUANTITY
                                       \                 >7 - RESERVED
    SL.IE.SALEQUAN,                    \ PRICING QUANTITY
    SL.IE.METHOD                       ! PRICING METHOD

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
  STRING MMCFEC14$,DATETIME$           !
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
!*                  array with the contents of each record                  *!
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
!* Subroutine Name: CHAIN.BACK                                              *!
!* Description    : This routine chains to the calling program.             *!
!****************************************************************************!
SUB CHAIN.BACK EXTERNAL
END SUB

!****************************************************************************!
!* Subroutine Name: MMCFEC99                                                *!
!* Description    : Log a message in the Application Error Log.             *!
!****************************************************************************!
SUB MMCFEC99(PROGRAM.NAME$) EXTERNAL
  STRING PROGRAM.NAME$
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

!!NOTE..NOTE..NOTE..NOTE..NOTE..NOTE..NOTE..NOTE..NOTE..NOTE.NOTE..NOTE!!!!!
!
! The following price calculation functions are duplicated in EAMTSIEC. Changes
! made there, or here, must be coordinated.

!****************************************************************************!
!* Subroutine Name: IE.PCOMP.XPRICE                                         *!
!* Description    : Return scale calculated extended price to pricing       *!
!*                  method zero price calculation.                          *!
!****************************************************************************!
SUB IE.PCOMP.XPRICE
END SUB

!****************************************************************************!
!* Function Name  : TSSAEC01                                                *!
!* Description    : Main functional entry point.  First two characters of   *!
!*                  subroutine name indicate which base Supermarket module  *!
!*                  has called the feature module.                          *!
!****************************************************************************!
FUNCTION TSSAEC01(FUNCTION.REQUESTED) PUBLIC RECURSIVE

  INTEGER*2 TSSAEC01,                  \
            FUNCTION.REQUESTED         !

END FUNCTION ! TSSAEC01

!****************************************************************************!
!* Subroutine Name: ZALQEC02                                                *!
!* Description    : Obtain the Limited Quantity Discount Quantities from    *!
!*                  the LQD Offset.                                         *!
!****************************************************************************!
SUB ZALQEC02 PUBLIC

  SL.IE.LQD.DEALLIMIT = VAL(UNPACK$(MID$(IR.USERDATA$,IRRF.LQDOffset - 46,1)))
  IF SL.IE.LQD.DEALLIMIT = 0 THEN SL.IE.LQD.DEALLIMIT = 1
  SL.IE.LQD.REDUCEDQTY = VAL(UNPACK$(MID$(IR.USERDATA$,IRRF.LQDOffset - 45,1)))
  IF SL.IE.LQD.REDUCEDQTY = 0 THEN SL.IE.LQD.REDUCEDQTY = 1

END SUB ! ZALQEC02

!****************************************************************************!
!*                                                                          *!
!* Function Name  : IROUND                                                  *!
!*                                                                          *!
!*    This routine will perform a divide and give an answer                 *!
!*    that depends on the rounding option passed.                           *!
!*    This routine takes three parameters, the divisor, dividend            *!
!*    and the rounding method.  The rounding method is:                     *!
!*                                                                          *!
!*    0 - round conventionally                                              *!
!*    1 - round upwards                                                     *!
!*    2 - round downwards                                                   *!
!*                                                                          *!
!*********************  B B D L  ********************************************!
!*                                                                          *!
!*    calculate the divide and the remainder                                *!
!*    IF rounding is not down THEN                                          *!
!*    : IF rounding is upwards THEN                                         *!
!*    : : IF remainder is not zero THEN                                     *!
!*    : : : add one to the answer                                           *!
!*    : : ENDIF                                                             *!
!*    : ELSE rounding is conventional                                       *!
!*    : : IF twice remainder is not less than the divisor THEN              *!
!*    : : : add one to the answer                                           *!
!*    : : ENDIF                                                             *!
!*    : ENDIF                                                               *!
!*    ENDIF                                                                 *!
!*                                                                          *!
!*  END FUNCTION                                                            *!
!*                                                                          *!
!****************************************************************************!
FUNCTION IROUND (DIVISOR, DIVIDEND, ROUNDING.METHOD) PUBLIC
  INTEGER*4 DIVISOR, ANSWER
  INTEGER*4 IROUND
  INTEGER*2 DIVIDEND, ROUNDING.METHOD, REMAINDER

  ! For WIC EBT max price exceeded reporting, we will always round the price
  ! down so that any shelf pricing differences are in the WIC agencies favor.
  ROUNDING.METHOD = 2
  ANSWER =  DIVISOR / DIVIDEND

  IF ROUNDING.METHOD <> 2 THEN BEGIN             ! not round down
    REMAINDER = MOD (DIVISOR, DIVIDEND)
    IF ROUNDING.METHOD = 1 THEN BEGIN            ! round up
      IF REMAINDER <> 0 THEN BEGIN
        ANSWER = ANSWER + 1
      ENDIF
    ENDIF ELSE BEGIN                             ! conventional rounding
      IF REMAINDER + REMAINDER >= DIVIDEND THEN BEGIN
        ANSWER = ANSWER + 1
      ENDIF
    ENDIF
  ENDIF
  IROUND = ANSWER

END FUNCTION ! IROUND

!****************************************************************************!
!*                                                                          *!
!* Function Name  : CALC.TOTAL.MULTIPRICE                                   *!
!*                                                                          *!
!* This routine will calculate the price of total amount of                 *!
!* a multipriced item, given as a parameter the total                       *!
!* quantity of the item.                                                    *!
!*                                                                          *!
!* The following fields have been set up:                                   *!
!*                                                                          *!
!*  SL.IE.METHOD                                                            *!
!*  SL.IE.DEALQUAN                                                          *!
!*  SL.IE.SALEPRIC                                                          *!
!*  SL.IE.SALEPRIC2 (not applicable if method 1)                            *!
!*                                                                          *!
!* THRESHOLD.ITEM is a flag that is to be set if the threshold item has     *!
!* been sold or voided in this entry for pricing method three or four.      *!
!*                                                                          *!
!* This function is called twice for each multipriced item entry.  Once     *!
!* with the previous quantity and once with the new total quantity.  If the *!
!* quantity is not less than the threshold quantity, then the threshold     *!
!* flag is flipped.  The threshold flag is initially set off, so that if,   *!
!* after two calls, the flag is set on then the quantity must have been     *!
!* over the threshold one time and under the threshold the other time.  In  *!
!* other words, the threshold item must have been sold or voided.           *!
!*                                                                          *!
!* Also note that the threshold item is only set for an item sale.          *!
!*                                                                          *!
!*********************  B B D L  ********************************************!
!*                                                                          *!
!*    IF base plus 1 method (1) THEN                                        *!
!*    : multiply price by quantity                                          *!
!*    : divide price by deal quantity and round (routine)                   *!
!*    ELSE                                                                  *!
!*    : IF group threshold method (2) THEN                                  *!
!*    : : IF deal quantity is zero or one THEN                              *!
!*    : : : multiply quantity by price to get total price                   *!
!*    : : ELSE                                                              *!
!*    : : : divide quantity by deal quantity                                *!
!*    : : : multiply result by deal price and remainder by price            *!
!*    : : : add the two together to get total price                         *!
!*    : : ENDIF                                                             *!
!*    : ELSE                                                                *!
!*    : : IF deal method with retroactive change THEN                       *!
!*    : : : IF quantity is less than deal quantity THEN                     *!
!*    : : : : multiply quantity by price                                    *!
!*    : : : ELSE we have reached the deal threshold                         *!
!*    : : : : multiply quantity by deal price                               *!
!*    : : : : flip threshold flag if an item sale                           *!
!*    : : : ENDIF                                                           *!
!*    : : ELSE deal method without retroactive change                       *!
!*    : : : IF quantity is less than deal quantity THEN                     *!
!*    : : : : multiply quantity by price                                    *!
!*    : : : ELSE we have reached the deal threshold                         *!
!*    : : : : multiply quantity by deal price                               *!
!*    : : : : deduct deal quantity * (discount on first items)              *!
!*    : : : ENDIF                                                           *!
!*    : : ENDIF                                                             *!
!*    : ENDIF                                                               *!
!*    ENDIF                                                                 *!
!*                                                                          *!
!*  END FUNCTION                                                            *!
!*                                                                          *!
!****************************************************************************!
FUNCTION CALC.TOTAL.MULTIPRICE (QUANTITY)
  INTEGER*4 CALC.TOTAL.MULTIPRICE, TOTAL.MULTIPRICE
  INTEGER*2 QUANTITY, SIGN

  SIGN = SGN(QUANTITY)
  QUANTITY = QUANTITY * SIGN

  ! IF PRICING METHOD 1 THEN CALCULATE

  IF SL.IE.METHOD = 1 THEN BEGIN
    TOTAL.MULTIPRICE = IROUND ( SL.IE.SALEPRIC * QUANTITY, \
      SL.IE.SALEQUAN, TO.PRICEROUND)
  ENDIF ELSE BEGIN

    ! IF PRICING METHOD 2 THEN CALCULATE

    IF SL.IE.METHOD = 2 THEN BEGIN
      IF SL.IE.SALEQUAN <= 1 THEN BEGIN
        TOTAL.MULTIPRICE = SL.IE.SALEPRIC2 * QUANTITY
      ENDIF ELSE BEGIN
        TOTAL.MULTIPRICE =                                 \
           SL.IE.SALEPRIC * (QUANTITY / SL.IE.SALEQUAN) +  \
           SL.IE.SALEPRIC2 * MOD(QUANTITY, SL.IE.SALEQUAN) !
      ENDIF
    ENDIF ELSE BEGIN

      ! IF PRICING METHOD 3 THEN CALCULATE AND SET THRESHOLD FLAG

      IF SL.IE.METHOD = 3 THEN BEGIN
        IF QUANTITY < SL.IE.SALEQUAN THEN BEGIN
          TOTAL.MULTIPRICE = SL.IE.SALEPRIC * QUANTITY
        ENDIF ELSE BEGIN
          TS.LINEDATA2 = 1                       ! indicate pric2 used
          TOTAL.MULTIPRICE = SL.IE.SALEPRIC2 * QUANTITY
          THRESHOLD.ITEM = NOT THRESHOLD.ITEM    \
            AND SL.IT.INDICAT3A = 0              !
        ENDIF

      ENDIF ELSE BEGIN

        ! MUST BE PRICING METHOD 4

        IF (IRRF.LQDOffset > 0) AND              \ LQD offset specified and
           (IRRF.RECL > 46) THEN BEGIN           ! Item record file has room

          ! Limited Quantity Discount (LQD) pricing method
          CALL ZALQEC02                          ! get LQD quantity offsets
          IF QUANTITY <= SL.IE.LQD.DEALLIMIT THEN BEGIN
            TS.LINEDATA2 = 1                     ! indicate pric2 used
            TOTAL.MULTIPRICE = IROUND ( SL.IE.SALEPRIC2 * QUANTITY, \
              SL.IE.SALEQUAN, TO.PRICEROUND)                        !
          ENDIF ELSE BEGIN
            THRESHOLD.ITEM = NOT THRESHOLD.ITEM  \
              AND SL.IT.INDICAT3A = 0            !
            TOTAL.MULTIPRICE = IROUND(SL.IE.SALEPRIC           \
              * (QUANTITY - SL.IE.LQD.DEALLIMIT),              \
              SL.IE.LQD.REDUCEDQTY, TO.PRICEROUND)             \
              + IROUND(SL.IE.SALEPRIC2 * SL.IE.LQD.DEALLIMIT , \
              SL.IE.SALEQUAN, TO.PRICEROUND)                   !
            TS.LINEDATA2 = 0                     ! INDICATE PRICE 1 USED
          ENDIF
        ENDIF ELSE BEGIN

          ! Old pricing method 4

          IF QUANTITY <= SL.IE.SALEQUAN THEN BEGIN
            TS.LINEDATA2 = 1                     ! indicate pric2 used
            TOTAL.MULTIPRICE = SL.IE.SALEPRIC2 * QUANTITY
          ENDIF ELSE BEGIN
            THRESHOLD.ITEM = NOT THRESHOLD.ITEM  \
              AND SL.IT.INDICAT3A = 0            !
            TOTAL.MULTIPRICE = SL.IE.SALEPRIC * QUANTITY            \
              - SL.IE.SALEQUAN * (SL.IE.SALEPRIC - SL.IE.SALEPRIC2) !
            TS.LINEDATA2 = 0                     ! INDICATE PRICE 1 USED
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  CALC.TOTAL.MULTIPRICE = TOTAL.MULTIPRICE * SIGN

END FUNCTION ! CALC.TOTAL.MULTIPRICE

!****************************************************************************!
!*                                                                          *!
!* Subroutine Name: METHOD.ZERO                                             *!
!*                                                                          *!
!* Should now have the correct pricing information in                       *!
!* working storage.  Quantity is also available.                            *!
!* This routine assumes that the quantity or weight and unit                *!
!* price are positive, results are otherwise unpredictable.                 *!
!*                                                                          *!
!*   multiply unit price by quantity                                        *!
!*   IF weight item THEN                                                    *!
!*   : multiply up deal quantity to get in fractions of weight              *!
!*   : calculate price to weight rounding                                   *!
!*   ELSE not a weight item                                                 *!
!*   : IF split package method then                                         *!
!*   : : calculate price and round                                          *!
!*   : : (multiply by quantity first only if quantity hit)                  *!
!*   : : (divide by sale quantity first if greater than one)                *!
!*   : : calculate price and round                                          *!
!*   : ENDIF                                                                *!
!*   ENDIF                                                                  *!
!*                                                                          *!
!* END SUB                                                                  *!
!*                                                                          *!
!****************************************************************************!
SUB METHOD.ZERO

  INTEGER*2 LOOP.COUNT

  IF (SL.IE.INDICAT1 AND (40H + 08H)) THEN BEGIN
    ! If UK price computing scale then use scale price
    IF (NOT UK.ALLSCALES.OPTION) OR              \
      TSSAEC01(5) THEN BEGIN                     !
      WEIGHT.MULTIPLYER = 1

      FOR LOOP.COUNT = 1 TO TO.WGT.DECPOS
        WEIGHT.MULTIPLYER = WEIGHT.MULTIPLYER * 10
      NEXT LOOP.COUNT

      SL.IT.XPRICE = IROUND (SL.IE.SALEPRIC * SL.IE.QTYORWGT, \
      SL.IE.SALEQUAN * WEIGHT.MULTIPLYER, TO.WGTROUND)        !
    ENDIF
  ENDIF ELSE BEGIN
    IF (SL.IE.INDICAT1 AND 20H) THEN BEGIN
      SL.IT.XPRICE = IROUND (SL.IE.SALEPRIC * SL.IE.QTYORWGT, \
        SL.IE.SALEQUAN, TO.PRICEROUND)                        !
    ENDIF ELSE BEGIN
      IF SL.IE.SALEQUAN <= 1 THEN BEGIN
        SL.IT.XPRICE = SL.IE.SALEPRIC
      ENDIF ELSE BEGIN
        SL.IT.XPRICE = IROUND (SL.IE.SALEPRIC,   \
          SL.IE.SALEQUAN, TO.PRICEROUND)         !
      ENDIF
    ENDIF
  ENDIF

END SUB ! METHOD.ZERO
!!END NOTE..END NOTE..END NOTE..END NOTE..END NOTE..END NOTE..END NOTE!!!!!

!****************************************************************************!
!* Subroutine Name: INITIALIZE.VARIABLES                                    *!
!* Description    : Setup display manager variables                         *!
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
  LAST.FIELD = 6                                 ! set last field on this page
  NUM.COL.LINES = 1

  IF NOT.FOUND = 0 THEN BEGIN
    DIM INP.FIELD$(LAST.FIELD + 1)
    OPEN "EAMSDEF1" RECL 40 AS 36 NODEL          ! open defaults file
    READ #36,111;A$                              ! read menu defaults
    IF LEN(A$) <> 3 THEN BEGIN                   ! if first time ever
      A$ = "11N"                                 ! establish a reasonable default
    ENDIF
    INP.FIELD$(1) = MID$(A$,1,1)                 ! destination
    INP.FIELD$(2) = MID$(A$,2,1)                 ! scope
    INP.FIELD$(6) = MID$(A$,3,1)                 ! Y or N
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
  CALL MMCFEC05(PUTF(RPT.TITLES$(3)),"MP 7")     ! max price report
  RPT.TITLE$ = RPT.TITLES$(3)

  CALL SET.VISIBLE(1, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(1)),"MP 7")      ! destination input field

  CALL SET.VISIBLE(2, -1)
  CALL MMCFEC05(PUTF(INP.FIELD$(2)),"MP 7")      ! scope input field

  ! scope choices
  FOR I = 4 TO 6
    CALL SET.VISIBLE(I+98, -1)
    CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(I)),"MP 7") ! range of UPCs
  NEXT I

  CALL SET.VISIBLE(115, 0)                          ! not used in this report
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

  CALL MMCFEC05(POSF(107),"MP 3")                ! report only items found in
  CALL MMCFEC05(PUTF(MENU.PROMPTS$(2)),"MP 7")   ! item record file
  IF INP.FIELD$(2) = "1" THEN BEGIN
    ATTR$ = SETF(INVISIBLE$)                     ! single UPC
  ENDIF ELSE BEGIN
    ATTR$ = SETF(VISIBLE$)                       ! range of UPCs, or all UPCs
  ENDIF
  CALL MMCFEC05(POSF(108),"MP 3")                ! report only items found in
  CALL MMCFEC05(PUTF(MENU.PROMPTS$(3)),"MP 7")   ! item record file
  IF INP.FIELD$(2) = "1" THEN BEGIN
    ATTR$ = SETF(INVISIBLE$)                     ! single UPC
  ENDIF ELSE BEGIN
    ATTR$ = SETF(VISIBLE$)                       ! range of UPCs, or all UPCs
    CALL SET.VISIBLE(6, -1)
    CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
  ENDIF

  CALL MMCFEC05(POSF(109),"MP 3")                ! yes/no hint
  IF INP.FIELD$(2) = "1" THEN BEGIN
    ATTR$ = SETF(INVISIBLE$)                     ! single UPC
  ENDIF ELSE BEGIN
    ATTR$ = SETF(VISIBLE$)                       ! range of UPCs, or all UPCs
  ENDIF

  IF NOT.FOUND THEN BEGIN
    CALL MMCFEC03(470, FILENAME$)
    CALL SET.VISIBLE(105, -1)
    FILENAME$ = ""
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
    TEMP                                         ! work var


  INTEGER*2                                      \
    I,                                           \ index
    IR.PM,                                       \
    PRICE.TYPE                                   ! WIC price type, constant 1


  OUT.STR$ = "  " + RIGHT$(UNPACK$(FILE.KEYS$(I)),12) + "   " ! record key

  IR.ITEMNAME$ = LEFT$(IR.ITEMNAME$, 18)         ! found, so get item desc
  OUT.STR$ = OUT.STR$ + IR.ITEMNAME$ + "    "    ! add product name

  TEMP$ = MMCFEC06$(SHELF.PRICE, -1, 0)          ! get and format shelf price
  TEMP$ = RIGHT$("          " + TEMP$, 10)
  OUT.STR$ = OUT.STR$ + TEMP$ + "  "

  TEMP$ = MMCFEC06$(MAX.PRICE, -1, 0)            ! get and format max price
  TEMP$ = RIGHT$("          " + TEMP$, 10)
  OUT.STR$ = OUT.STR$ + TEMP$

END SUB ! BUILD.RPT.LINE

!****************************************************************************!
!* Subroutine Name: BUILD.REPORT                                            *!
!* Description    : Creates and displays/prints/files the entire report.    *!
!****************************************************************************!
SUB BUILD.REPORT

  STRING                                         \
    SAV.ITEMCODE$,                               \
    SAV.ITEMNAME$,                               \
    WORK.STRING$,                                \
    WORK$,                                       \
    TEMP$                                        !

  INTEGER*4                                      \
\AIO00563 'START' must be global
\   START,                                       \
\EIO00563
    END.CNT,                                     \
    TEMP                                         !

  INTEGER*2                                      \
    CHAIN.COUNT,                                 \
    WORK,                                        \
    PRICE.TYPE,                                  \ WIC price type, constant 1
    OK,                                          \
    LAST.LINE,                                   \
    LOOP.COUNTER,                                \
    FILE1.OPEN,                                  \ flag for closing #1
    I                                            !

  !**************************************************************************!
  !* Open the keyed file in direct mode and use the record length from      *!
  !* the first sector keyed file information.                               *!
  !* We open it record length 48 as we only really need the keyed file      *!
  !* record length information which is at offset 46.  We therefore         *!
  !* tab to the 47th byte and read a two byte integer at this position.     *!
  !* This should be faster and create less loop traffic and less code than  *!
  !* reading a full sector of information.                                  *!
  !**************************************************************************!
  OPEN EAMITEMR$ DIRECT RECL 48 AS 4 NOWRITE NODEL ! determine record length
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
      WHILE (TEMP$ < START.RNG$) AND             \ look for point to start search
            (I < REC.CNT)                        ! don't go off the end
        I = I + 1
        TEMP$ = UNPACK$(FILE.KEYS$(I))
      WEND
      START = I                                  ! the first entry to report

      TEMP$ = UNPACK$(FILE.KEYS$(I))
      END.RNG$ = RIGHT$("00000000000000" + END.RNG$, 14)
      WHILE (TEMP$ < END.RNG$) AND               \ look for point to end search
            (I < REC.CNT)                        ! don't go off the end
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
        NOT.FOUND = ON1                          ! ensure a line 22 error msg
        GOTO DONE
      ENDIF ELSE BEGIN                           ! handle the number found
        END.CNT = I                              ! the last entry to report
      ENDIF
    ENDIF ELSE BEGIN                             ! handle them all
      END.CNT = REC.CNT
    ENDIF
  ENDIF

  IF (INP.FIELD$(2) <> "1") THEN BEGIN           ! if not a single UPC
    REC.CNT = END.CNT                            ! establish range to check
    CALL HANDLE.MISSING.ITEMS(EXCEEDED.ONLY)     ! check each item
    ! note that START and REC.CNT get modified for the new range
    END.CNT = REC.CNT                            ! new max range
    IF START > END.CNT THEN BEGIN                ! if no items to show
      NOT.FOUND = ON1                            ! ensure a line 22 error msg
      GOTO DONE
    ENDIF
  ENDIF

  IF DESTINATION = DISPLAY THEN BEGIN            ! report to screen
    CALL MMCFEC05(DISPD(100), "MP 2")            ! common report screen
  ENDIF                                          ! else report to elsewhere

  MAX.PAGE = END.CNT - START + 1                  ! number of lines to show
  MAX.PAGE = INT%(MAX.PAGE + (PAGE.LEN - 6) - 1)  \ round up to get #pages
                        / (PAGE.LEN - 6)

  CALL WIC.BUILD.HEADINGS(5)
  CALL WIC.BUILD.HEADER
  LINE.NO = 6                                    ! init report vars
  QUIT.PRESSED = OFF0
  ENTER.PRESSED = OFF0
  ESC.PRESSED = OFF0
  COMPLETED = OFF0
  NOT.FOUND = ON1

  ! check prices and format each entry
  FOR LOOP.COUNTER = START TO END.CNT
    IR.ITEMCODE$ = RIGHT$(FILE.KEYS$(LOOP.COUNTER), 6)
    TS.ER.RETURN = -1                            ! expect error
    WIC.ERR$ = ""                                ! init
    CALL IRRFEC.READ02 (4, 0)                    ! read item record

    ! Set up summary log variables similar to EAMTSIEC.BAS
    IR.ITEMTYPE = 0
    IR.PRICING.METHOD = 0
    IR.PRICE1 = 0
    IR.PRICE2 = 0

    IF (WIC.ERR$ = "") THEN BEGIN                ! if no error on item read
      IF (IR.INDICAT2$ <> CHR$(0)) THEN BEGIN
        WORK = PACKBIN2(IR.INDICAT2$, 0, 1)
        IR.ITEMTYPE = WORK / 10
        IR.PRICING.METHOD = MOD(WORK, 10)
        IF (IR.PRICING.METHOD = 5) THEN BEGIN    ! if alias record
          CHAIN.COUNT = 0
          SAV.ITEMCODE$ = IR.ITEMCODE$
          SAV.ITEMNAME$ = IR.ITEMNAME$
          WHILE (IR.PRICING.METHOD = 5)
            CHAIN.COUNT = CHAIN.COUNT + 1
            IF (CHAIN.COUNT >= 5) THEN BEGIN     ! seems to be endless loop
              IR.PRICING.METHOD = 0              ! bail out
            ENDIF ELSE BEGIN                     ! else not too much chaining
              IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$
              CALL IRRFEC.READ02(4,0)
              IF (WIC.ERR$ = "") THEN BEGIN      ! if no error
                IR.PRICING.METHOD = MOD((PACKBIN2(IR.INDICAT2$,0,1)),10)
              ENDIF ELSE BEGIN                   ! else error on read
                IR.PRICING.METHOD = 0
              ENDIF                              ! if no error
            ENDIF                                ! seems to be endless loop
          WEND
          IR.ITEMCODE$ = SAV.ITEMCODE$           ! restore item code
          IR.ITEMNAME$ = SAV.ITEMNAME$           ! restore item name
          SAV.ITEMCODE$ = ""                     ! cleanup
          SAV.ITEMNAME$ = ""
        ENDIF                                    ! if alias record
      ENDIF
      IF (IR.PRICING.METHOD <= 1) THEN BEGIN
        IR.PRICE1 = PACKBIN4(IR.SALEPRIC$, 1, 4)
      ENDIF ELSE BEGIN
        WORK.STRING$ = UNPACK$(IR.SALEPRIC$)
        IR.PRICE1 = INT%(VAL(LEFT$(WORK.STRING$, 5)))
        IR.PRICE2 = INT%(VAL(RIGHT$(WORK.STRING$, 5)))
      ENDIF
    ENDIF                                        ! if no error on item read

    SL.IT.INDICAT3A = IR.ITEMTYPE
    SL.IE.METHOD = IR.PRICING.METHOD
    SL.IE.SALEQUAN = PACKBIN2(IR.SALEQUAN$, 0, 1)
    SL.IE.SALEPRIC = IR.PRICE1
    SL.IE.SALEPRIC2= IR.PRICE2

    MAX.PRICE = VAL(UNPACK$(MID$(RPT.STR$(LOOP.COUNTER), 25, 3))) ! get WIC max allowed price
    PRICE.TYPE = VAL(UNPACK$(MID$(RPT.STR$(LOOP.COUNTER), 28, 1))) ! WIC price type "1"
    QUANTITY = 1                                 ! want single item price

    IF PRICE.TYPE = 1 THEN BEGIN                 ! only report max price items
      IF (WIC.ERR$ = "EF") THEN BEGIN            ! item not in item rec file
        IR.ITEMNAME$ = LEFT$(ERROR.MESSAGES$(10),18) ! "NO RECORD FOUND   "
        IR.SALEPRIC$ = "0"
        SL.IE.SALEPRIC = PACKBIN4 (IR.SALEPRIC$, 1, 4) ! single item price
        SL.IE.METHOD = 0
      ENDIF ELSE BEGIN                           ! item in item rec file
        IF SL.IE.METHOD <= 1 THEN BEGIN          ! price method 0 or 1
          SL.IE.SALEPRIC = PACKBIN4 (IR.SALEPRIC$, 1, 4) ! single item price
        ENDIF ELSE BEGIN                         ! price method > 1
          SL.IE.SALEPRIC = CALC.TOTAL.MULTIPRICE (QUANTITY) ! get price of 1 item
        ENDIF
      ENDIF

      SHELF.PRICE = SL.IE.SALEPRIC               ! what this store charges

      IF (EXCEEDED.ONLY) AND                     \ only rpt if max price exceeded
         (SHELF.PRICE <= MAX.PRICE) THEN BEGIN   ! shelf price not > max price
        GOTO GET.NEXT                            ! nothing to report
      ENDIF ELSE BEGIN                           ! report all, or price too high
        NOT.FOUND = OFF0                         ! we found at least 1 item
        CALL BUILD.RPT.LINE(LOOP.COUNTER)        ! format the output line
        CALL WIC.OUTPUT(OUT.STR$)                ! write to its destination

        IF (SCOPE = 1) OR                        \ this page is complete
           (LOOP.COUNTER = END.CNT) OR           \ last item on last page
           (LINE.NO >= PAGE.LEN) THEN BEGIN      ! end of page
          IF DESTINATION = DISPLAY THEN BEGIN    !
            YESORNO = OFF0                       ! turn off checks
            NEED.RANGE.CHECK = OFF0

            IF PAGE < MAX.PAGE THEN BEGIN        ! prompt if more pgs to display
              CALL MMCFEC03(100,NULL$)
            ENDIF

            CALL MMCFEC05(POSF(239),"MP 3")      ! position cursor on inp fld
            OK = OFF0                            ! init flag

            WHILE NOT OK                         ! while not set

              ENTER.PRESSED = ON1                ! disallow ENTER here
              WHILE ENTER.PRESSED
                CALL MMCFEC04(WORK$)             ! get input
              WEND

              OK = ON1                           ! set flag

              IF PGDN.PRESSED THEN BEGIN         ! if PgDn
                CALL SETUP.NEWPAGE(5)            ! put up titles
                LAST.LINE = LINE.NO - 1
                PGDN.PRESSED = OFF0
                IF PAGE = MAX.PAGE THEN BEGIN    ! check for last page and
                  PAGE.LEN = MOD(((END.CNT-START) + 1), 14) + 6 ! adjust # of lines
                ENDIF
              ENDIF ELSE BEGIN
                IF PGUP.PRESSED THEN BEGIN       ! if PgUp
                  LOOP.COUNTER = LOOP.COUNTER - (LINE.NO - 6) - 14 ! reset parms
                  IF LOOP.COUNTER < (START-1) THEN BEGIN ! do not backup before beginning
                    LOOP.COUNTER = START - 1     ! NEXT increments it to START
                  ENDIF
                  PGUP.PRESSED = OFF0
                  IF PAGE.LEN < 20 THEN PAGE.LEN = 20 ! ensure num lines is correct
                ENDIF ELSE BEGIN
                  IF (ESC.PRESSED) OR            \ finished, return to main menu
                     (QUIT.PRESSED) OR           \
                     (ENTER.PRESSED) THEN BEGIN
                    COMPLETED = ON1
                    CLOSE 4
                    EXIT SUB
                  ENDIF ELSE BEGIN
                    OK = OFF0                    ! turn off flag
                    CALL MMCFEC03(109,NULL$)     ! invalid function key
                  ENDIF
                ENDIF
              ENDIF
            WEND
          ENDIF ! destination = DISPLAY
          ! We just finished a page.  Prepare for the next, if any
          IF LOOP.COUNTER < END.CNT THEN BEGIN   ! last item on last page?
            CALL SETUP.NEWPAGE(5)                ! put up titles
          ENDIF
        ENDIF
      ENDIF
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
    IF NOT.FOUND THEN BEGIN
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

    IF CURRENT.FIELD > LAST.FIELD THEN BEGIN     ! if past last field
      CURRENT.FIELD = 1                          ! make it first field
    ENDIF
    IF CURRENT.FIELD < 3 THEN BEGIN              ! if dest or scope n
      NEED.RANGE.CHECK = ON1                     ! do a range check on input
      LOW.END$ = "1"                             ! set ranges
      HIGH.END$ = HIGH$(CURRENT.FIELD)
      ASTRK.OK  = OFF0                           ! no * or Y/N
      YESORNO = OFF0
    ENDIF
    IF CURRENT.FIELD >= 3 AND                    \ if entering UPC
      CURRENT.FIELD <= 4 THEN BEGIN              !
      YESORNO = OFF0                             ! turn off checks
      NEED.RANGE.CHECK = OFF0
    ENDIF
    IF CURRENT.FIELD = 6 THEN BEGIN              ! if last field
      YESORNO = ON1                              ! turn on y/n check
      NEED.RANGE.CHECK = ON1                     ! do a Y|N range check on input
    ENDIF

    ! get user input
    CALL MMCFEC05(POSF(CURRENT.FIELD),"MP 3")    ! position cursor
    CALL MMCFEC04(INP.FIELD$(CURRENT.FIELD))     ! get field input

    ! display prompt lines depending on the scope selection that is made

    IF NOT (ESC.PRESSED OR QUIT.PRESSED) THEN BEGIN

      IF CURRENT.FIELD = 2 THEN BEGIN            ! scope
        IF INP.FIELD$(2) = "1" THEN BEGIN        ! single UPC
          CALL SET.VISIBLE(105, -1)
          CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(7)),"MP 7") ! put up prompt single UPC
          CALL SET.VISIBLE(3, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(3)),"MP 7") ! lower range
          CALL SET.VISIBLE(4, 0)
          CALL SET.VISIBLE(106, 0)
          CALL SET.VISIBLE(107, 0)
          CALL SET.VISIBLE(108, 0)
          CALL SET.VISIBLE(109, 0)
          CALL SET.VISIBLE(6, 0)
        ENDIF
        IF INP.FIELD$(2) = "2" THEN BEGIN    ! range of UPCs
          CALL SET.VISIBLE(105, -1)
          CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(8)),"MP 7") ! put up prompt for low end
          CALL SET.VISIBLE(106, -1)
          CALL MMCFEC05(PUTF(SCOPE.PROMPTS$(9)),"MP 7") ! put up prompt for high end
          CALL SET.VISIBLE(107, -1)
          CALL MMCFEC05(PUTF(MENU.PROMPTS$(2)),"MP 7") ! display prices > max
          CALL SET.VISIBLE(108, -1)
          CALL MMCFEC05(PUTF(MENU.PROMPTS$(3)),"MP 7") ! display prices > max
          CALL SET.VISIBLE(6, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
          CALL SET.VISIBLE(109, -1)
        ENDIF
        IF INP.FIELD$(2) = "3" THEN BEGIN        ! all UPCs
          CALL SET.VISIBLE(105, 0)
          CALL SET.VISIBLE(3, 0)
          CALL SET.VISIBLE(106, 0)
          CALL SET.VISIBLE(4, 0)
          CALL SET.VISIBLE(107, -1)
          CALL MMCFEC05(PUTF(MENU.PROMPTS$(2)),"MP 7") ! display prices > max
          CALL SET.VISIBLE(108, -1)
          CALL MMCFEC05(PUTF(MENU.PROMPTS$(3)),"MP 7") ! display prices > max
          CALL SET.VISIBLE(6, -1)
          CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7")    ! show as Y or N
          CALL SET.VISIBLE(109, -1)
        ENDIF
      ENDIF

      ! in case user enters data here
      IF CURRENT.FIELD = 6 THEN BEGIN            ! y/n field
        INP.FIELD$(6) = UCASE$(INP.FIELD$(6))    ! make it uppercase
        CALL MMCFEC05(PUTF(INP.FIELD$(6)),"MP 7") ! redisplay it
      ENDIF

      IF (CURRENT.FIELD = 3 AND                  \ lower range field
          INP.FIELD$(2) = "2") THEN BEGIN        ! range of UPCs selected
        CALL SET.VISIBLE(4, -1)
        CALL MMCFEC05(PUTF(INP.FIELD$(4)),"MP 7") ! get upper range
      ENDIF

      ! process terminating key
      RET.VAL = ON1
      IF TAB.PRESSED THEN BEGIN                  ! adjust cursor position
        CURRENT.FIELD = MOD(CURRENT.FIELD,LAST.FIELD) + 1 ! bump field loc
        IF CURRENT.FIELD = 5 THEN BEGIN          ! 5 is not used on this report
          CURRENT.FIELD = 6
        ENDIF
        IF (INP.FIELD$(2) = "1") THEN BEGIN      ! single UPC
          IF (CURRENT.FIELD > 3) THEN BEGIN      ! fields 4 - 5 are not shown
            CURRENT.FIELD = 1                    ! goto first field
          ENDIF
        ENDIF
        IF (INP.FIELD$(2) = "3") THEN BEGIN      ! report all UPCs
          IF (CURRENT.FIELD = 3) OR              \ fields 3 and 4 are not shown
             (CURRENT.FIELD = 4) THEN BEGIN
            CURRENT.FIELD = LAST.FIELD           ! goto last field
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
        IF CURRENT.FIELD = 5 THEN BEGIN          ! 5 is not used on this report
          CURRENT.FIELD = 4
        ENDIF
        IF (INP.FIELD$(2) = "1") THEN BEGIN      ! single UPC
          IF (CURRENT.FIELD > 3) THEN BEGIN      ! fields 4 - 5 are not shown
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
      ENDIF ! ENTER.PRESSED
    ENDIF ! not ESC OR QUIT
  WEND ! not QUIT, ENTER or ESC

END SUB ! HANDLE.USER.INPUT

!****************************************************************************!
!* MAIN                                                                     *!
!****************************************************************************!

  ON ERROR GOTO TRAP.IT

  EAMITEMR$ = "$AMITEMR"
!AIO10187 Bump the max allowed to approximately the largest
!         number that CBASIC can reliably handle.
\ MAX.KEYS = 5000
  MAX.KEYS = 15000
!EIO10187
  DIM FILE.KEYS$(MAX.KEYS + 1)
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

        IF ENTER.PRESSED THEN BEGIN              ! if user did not QUIT or ESC
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
            CALL INITIALIZE.VARIABLES
            SHOW.ERROR = 3                       ! "Items were not found"
            GOTO REDO
          ENDIF ELSE BEGIN
            IF (INP.FIELD$(6) = "Y") AND         \ is it Y or N ?
               (INP.FIELD$(2) <> "1") THEN BEGIN ! not a single UPC
              EXCEEDED.ONLY = -1                 ! set report flag ON
            ENDIF ELSE BEGIN
              EXCEEDED.ONLY = 0                  ! set report flag OFF
            ENDIF

            WRITE #36,111;INP.FIELD$(1) +        \ update menu defaults
                          INP.FIELD$(2) + INP.FIELD$(6)
            NOT.FOUND = 0
          ENDIF

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

  CALL MMCFEC99("MP")                            ! log an error message
  STOP                                           ! do not continue

! End MAIN
