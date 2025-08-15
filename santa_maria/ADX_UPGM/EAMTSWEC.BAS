\/*********************************************************************/
\/*                                                                   */
\/*      MODULE NAME: EAMTSWEC                                        */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: WIC EBT SMARTCARD FUNCTION                 */
\/*                                                                   */
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*      5799-QZH THIS MODULE IS "RESTRICTED MATERIALS OF IBM"        */
\/*      (C) COPYRIGHT IBM CORP 1986, 2004 ALL RIGHTS RESERVED        */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT      */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                           */
\/*                                                                   */
\/*      STATUS: RELEASE 1 LEVEL 0                                    */
\/*                                                                   */
\/*                                                                   */
\/*      PERSON RESPONSIBLE = Tom Gaskill                             */
\/*                                                                   */
\/*      FUNCTION: EAMTSWEC                                           */
\/*                                                                   */
\/*                THIS MODULE HANDLES THE TERMINAL SALES PORTION OF  */
\/*                WIC EBT PROCESSING.                          ,     */
\/*                                                                   */
\/*      WE = WIC EBT                                                 */
\/*                                                                   */
\/*      CHANGE ACTIVITY: LEVEL 0                                     */
\/*                                                                   */
\/*      IO00625 Coupon Association not working when voiding items    */
\/*              with cents coupon linked using Qty key vs. no Qty    */
\/*              key. The problem was caused by calling the method    */
\/*              WE.REMOVE.ITEM.ENTRY from the subroutine,            */
\/*              HANDLE.VOID:.  The index of the loop was reduced,    */
\/*              causing the iteration to be incorrect. The iteration */
\/*              loop index resetting in WE.REMOVE.ITEM.ENTRY assumes */
\/*              the loop is going from low to high instead of high   */
\/*              to low as it's done in HANDLE.VOID:.                 */
\/*              LTG IBM 09Mar2005                                    */
\/*                                                                   */
\/*      IO00754 SIF GUI was not updating totals correctly on a trnx  */
\/*              recovery.                                            */
\/*              LTG IBM 29Mar2005                                    */
\/*                                                                   */
\/*      IO01214 The item amounts print incorrectly if selling a      */
\/*              quantity of multipriced items                        */
\/*              LTG IBM 11May2005                                    */
\/*                                                                   */
\/*      IO03864 Reformat the PAN printing to consider account        */
\/*              numbers less than 19 digits.  Digits 13-16 should    */
\/*              be unmasked based on the size of the number, not on  */
\/*              a static size.                                       */
\/*              LTG/GGK IBM 28Feb2006                                */
\/*                                                                   */
\/*      IO05348 AE IO03864.  The LocalXactionDateTime field of the   */
\/*              D4 WIC Claim File Detail Record is sometimes wrong.  */
\/*              IO03864 attempted to fix a problem with transactions */
\/*              that span the time period involving the exact        */
\/*              expiration date and time, but caused a new problem.  */
\/*              To properly fix this problem, the design is being    */
\/*              changed.  A new field is added to the 80:02 t-log    */
\/*              string.  This field contains the date and time that  */
\/*              the WIC EBT card was queried for benefits.  Checkout */
\/*              Support uses this new field in the D4 record.        */
\/*              GGK IBM 11Oct2006                                    */
\/*                                                                   */
\/* END-OF-SPECIFICATIONS *********************************************/

%ENVIRON T

!*******************************************************************************
! global variable declarations
!*******************************************************************************

%INCLUDE EAMTSWEV.J86                  ! WIC EBT feature variables
%INCLUDE WEITEMVR.J86                  ! WIC EBT feature variables
%INCLUDE EMWEBTVA.J86                  ! Item to Coupon association variables
%INCLUDE EAMTSWKG.J86                  ! working storage
%INCLUDE EAMTRANS.J86                  ! Tlog variables
%INCLUDE EAMITEMR.J86                  ! item record file
%INCLUDE EAMIRRFG.J86                  ! GLOBAL Routine Variables
%INCLUDE EAMTOPTS.J86                  ! Term options vars
%INCLUDE EAMP4VAJ.J86                  ! 4610 Printer feature exit
%INCLUDE EAMP4VGJ.J86                  ! 4610 Variable definitions
%INCLUDE JAVAGUIV.J86                  ! Java Gui Variables

INTEGER*1 GLOBAL                       \
          PP.80.REMOVE.CARD.SENT,      \ flag remove card msg sent
          PRT4610.REPRINT,             \ 4610 flag if we're just re-printing
          WIC.ENABLE                   ! Value Pack flag to indicate WIC feature

INTEGER*2 GLOBAL                       \
          SL.END                       ! last index to be used in summary log

INTEGER*4 GLOBAL                       \
          EMSS.PR.BALDUE               ! EM variable that keeps up with Bal Due

!*******************************************************************************
! local variable declarations
!*******************************************************************************

STRING                                 \
       WE.STATE.CODE$,                 \ state codes passed to pinpad API
       PROCESSING.REQUEST.MSG$,        \ message prompt
       WAIT.ON.CUSTOMER.MSG$,          \ message prompt
       TRNX.DISCOUNT.DESC$,            \ descriptor for printing trnx discounts
       CAT.DESC.NOT.FOUND$,            \ print if category descriptor not found
       TRAINING.MODE.DESC$,            \ descriptor if in training mode
       WE.DUP.RECPT.HDR1$,             \ Text for header one of dup receipts
       WE.BNA.HEADER1$,                \ "Benefits Not Available" header 1 text
       WE.BNA.HEADER2$,                \ "Benefits Not Available" header 2 text
       WE.AGENCY.MAIN.OPTION$(1),      \ rec data read for agencies main option
       WE.AGENCY.BENEFIT.HDR$(1),      \ Agency Options Benefit header lfs/fonts
       WE.AGENCY.BENEFIT.ITM$(1),      \ Agency Options Benefit item lfs/fonts
       WE.AGENCY.BENEFIT.TRL$(1),      \ Agency Options Benefit trailer lfs/font
       WE.AGENCY.REDEMPT.HDR$(1),      \ Agency Options Redemption hdr lfs/fonts
       WE.AGENCY.REDEMPT.ITM$(1),      \ Agency Options Redemption itm lfs/font
       WE.AGENCY.REDEMPT.TRL$(1),      \ Agency Options Redemption trl lfs/fonts
       WE.STATE.ID$,                   \ current customer state id
       WE.BIN.NUMBER$,                 \ current customer BIN number
       WE.MERCHANT.ID$,                \ current customer merchant ID
       WE.FORWARD.INSTITUTE$,          \ current customer forwarding institute
       WE.RECEIVE.INSTITUTE$,          \ current customer receiving institute
       WE.BENEFIT.HDR$(1),             \ current benefit header print lines
       WE.BENEFIT.ITM$(1),             \ current benefit item print lines
       WE.BENEFIT.TRL$(1),             \ current benefit trailer print lines
       WE.REDEMPT.HDR$(1),             \ current redemption header print lines
       WE.REDEMPT.ITM$(1),             \ current redemption item print lines
       WE.REDEMPT.TRL$(1),             \ current redemption trailer print lines
       WE.CATEGORY$(1),                \ category from prescription
       WE.CATEGORY.QTY$(1),            \ category quantity
       WE.CATEGORY.UPD.QTY$(1),        \ category updated quantity
       WE.CATEGORY.UNIT$(1),           \ category unit descriptor (i.e. jar,lb)
       WE.CATEGORY.DESC$(1),           \ category descriptor
       WE.CATEGORY.ITEM.PTR$(1),       \ pointer to items for categories
       WE.ORG.CATEGORY.QTY$(1),        \ save the original category quantity
       WE.ORG.CATEGORY.UNIT$(1),       \ save the original category unit
       WE.ORG.CATEGORY.DESC$(1),       \ save the original category description
       WE.ITEM.FULL.CATEGORY$(1),      \ full category of item
       WE.ITEM.CODE$(1),               \ item code for prescription items
       WE.ITEM.CHECKDIGIT$(1),         \ item code check digit for items
       WE.ITEM.UNIT.PRICES$(1),        \ all the prices in instance of item code
       WE.ITEM.0.CHILD.ARRAY$(1),      \ create bit array to flag zero WIC'ed
       WE.ITEM.N.CHILD.ARRAY$(1),      \ create bit array to flag nonzero WIC'ed
       WE.ITEM.APL.FILE.DATA$(1),      \ item data read from the UPC/PLU file
       WE.ITEM.UNIT.DESC$(1),          \ unit descriptor from the item desc file
       WE.ITEM.CPN.0.POINTER$(1),      \ pointer to coupons for zero categories
       WE.ITEM.CPN.N.POINTER$(1),      \ pointer to coupons for nonzero cats
       WE.ITEM.CPN.0.TLOG$(1),         \ zero category coupon tlog strings
       WE.ITEM.CPN.N.TLOG$(1),         \ nonzero category coupon tlog strings
       WE.ITEMS.TO.DECREMENT$,         \ list of items to decrement (_30 msg)
       WE.ERR$,                        \ ERR
       WE.ERRN$                        ! ERRN code used for error logging

INTEGER*1                              \
          WE.DAYLIGHT.SAVINGS,         \ Flag if observing daylight savings time
          WE.GMT.LOCAL.DIFF,           \ Diff in hours between local GMT
          WE.DUP.RECPT.HDR1.LF,        \ linefeeds for duplicate receipt hdr 1
          WE.BNA.HEADER1.LF,           \ linefeed for "Benefits Not Available" 1
          WE.BNA.HEADER2.LF,           \ linefeed for "Benefits Not Available" 2
          WE.BENEFIT.HDR.LF(1),        \ current benefit header linefeed
          WE.BENEFIT.ITM.LF(1),        \ current benefit item linefeed
          WE.BENEFIT.TRL.LF(1),        \ current benefit trailer linefeed
          WE.REDEMPT.HDR.LF(1),        \ current redemption header linefeed
          WE.REDEMPT.ITM.LF(1),        \ current redemption item linefeed
          WE.REDEMPT.TRL.LF(1),        \ current redemption trailer linefeed
          WE.CATEGORY.HAS.ITEMS(1),    \ flag if prescription is actually used
          WE.ITEM.USED(1),             \ flag if going to be used with prescript
          WE.INITIALIZE.PINPAD         ! Flag if need to initialize the PinPad

INTEGER*2                              \
          WE.SESSION.NUM1,             \ sess num used by feat, defaults to 71
          WE.LEN.WIC.DATA,             \ curr  length of the wic data
          WE.PINPAD.INIT.ERR.CNT,      \ num of times err occurred initializing
          WE.NUMBER.OF.AGENCIES,       \ Total number of agencies available
          WE.NUM.BENEFIT.HDR%,         \ total number of benefit headers
          WE.NUM.BENEFIT.ITM%,         \ total number of benefit item lines
          WE.NUM.BENEFIT.TRL%,         \ total number of benefit trailers
          WE.NUM.REDEMPT.HDR%,         \ total number of redemption headers
          WE.NUM.REDEMPT.ITM%,         \ total number of redemption item lines
          WE.NUM.REDEMPT.TRL%,         \ total number of redemption trailers
          WE.NUM.CATEGORIES,           \ number of prescription items
          WE.NUM.ORG.CATEGORIES,       \ original number of prescription items
          WE.ITEM.COUNT,               \ count of wic'able items
          WE.ITEM.INDICAT1(1),         \ saved indicator 1
          WE.ITEM.INDICAT2(1),         \ saved indicator 2
          WE.ITEM.0.RX.INDEX(1),       \ index into zero cat prescription array
          WE.ITEM.N.RX.INDEX(1),       \ index into nonzero cat prescription
          WE.CPN.COUNT,                \ count of coupons that match wic'ble itm
          WE.CPN.TLOG.OFFSET(1)        ! offset into Tlog for coupons

INTEGER*4                              \
          WE.TOTAL.ITEM.VALUE,         \ accumulation of the total item value
          WE.TOTAL.DISC.VALUE,         \ accumulation of total discount value
          WE.TOTAL.CLAIM.AMOUNT,       \ total WIC EBT tender amount for trnx
          WE.TAX(1),                   \ saved accumulations for tax plans
          WE.CATEGORY.DEBIT.QTY(1),    \ quantity debited from category
          WE.ITEM.BENEFIT.QTY(1),      \ benefit quantity of item
          WE.ITEM.N.PURCH.QTY(1),      \ qty of items of nonzero subcategory
          WE.ITEM.0.PURCH.QTY(1),      \ qty of items of zero subcategory
          WE.ITEM.N.COST(1),           \ total cost for this entry of nonzero
          WE.ITEM.0.COST(1),           \ total cost for this entry of zero
\IO01214  WE.ITEM.PRICE(1),            \ extended price of item for printing
          WE.ITEM.XPRICE(1),           \ saved extended price
          WE.ITEM.DEALQTY(1),          \ modified quantity of deal for items
          WE.CPN.QUANTITY(1),          \ cpn quantities that match wic'able item
          WE.CPN.VALUE(1)              ! coupon value that match wic'able items

!*******************************************************************************
! External Routines and Copyright
!*******************************************************************************

%INCLUDE EAMXXCPY.J86                  ! copyright statement
%INCLUDE EAMADXRT.J86                  ! system services interface
%INCLUDE EAMASMCT.J86                  ! assembler routines
%INCLUDE JAVAGUIC.J86                  ! Java externals
%INCLUDE EAMP4EGJ.J86                  ! 4610 external function declarations
%INCLUDE EAMTSXHC.J86                  ! summary log functions
%INCLUDE EAMWEDVC.J86                  ! WIC EBT SmartCard API externals
%INCLUDE EMWEBTSU.J86                  ! Item to Coupon association functions
%INCLUDE EAMERRNH.J86                  ! Convert to HEX routine for ERRN value.

FUNCTION CALC.TOTAL.MULTIPRICE(QUANTITY) EXTERNAL
  INTEGER*4 CALC.TOTAL.MULTIPRICE
  INTEGER*2 QUANTITY
END FUNCTION

SUB DO.GUILLOTINE.CUT EXTERNAL         ! Base routine to cut the receipt
END SUB

FUNCTION EAMTRACE(TRXX) EXTERNAL       ! Base tracing function
  INTEGER*1 EAMTRACE
  INTEGER*2 TRXX
END FUNCTION

FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL  ! Base function to format amounts
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
END FUNCTION

SUB PRINT.RTN EXTERNAL                 ! Printing routine
END SUB

SUB REPRINT.RECEIPT EXTERNAL           ! 4610 routine to reprint the receipt
END SUB

SUB TSCSEC03 EXTERNAL                  ! add error message to message stack
END SUB

SUB TSCSEC09 EXTERNAL                  ! non sales initialization
END SUB

SUB TSCSEC10 EXTERNAL                  ! non sales termination
END SUB

SUB TSDSEC01 EXTERNAL                  ! display routine
END SUB

SUB TSPREC01 EXTERNAL                  ! print routine
END SUB

SUB TSTPEC01 EXTERNAL                  ! subroutine to log data entry string
END SUB

SUB TSTPEC04 EXTERNAL                  ! totals save file checkpoint
END SUB

!*******************************************************************************
! Functions and Subroutines
!*******************************************************************************

%INCLUDE DATEFUNC.J86                  ! date functions


!*******************************************************************************
! Subroutine Name: WE.RESET.PRINT.VARS
! Description: Reset all the feature printing variables.
!*******************************************************************************
SUB WE.RESET.PRINT.VARS

  WE.PRN.PAN$ = ""
  WE.PRN.START.DATE$ = ""
  WE.PRN.END.DATE$ = ""
  WE.PRN.PRSCRPT.QTY$ = ""
  WE.PRN.PRSCRPT.UNIT$ = ""
  WE.PRN.PRSCRPT.DESC$ = ""
  WE.PRN.DEBIT.QTY$ = ""
  WE.PRN.ITEM.UNIT.DESC$ = ""
  WE.PRN.ITEM.QTY$ = ""
  WE.PRN.ITEM.DESC$ = ""
  WE.PRN.ITEM.PRICE$ = ""
  WE.PRN.CPN.DESC$ = ""
  WE.PRN.CPN.QUANTITY$ = ""
  WE.PRN.CPN.PRICE$ = ""

END SUB                                          ! end WE.RESET.PRINT.VARS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.RESET.TRANSACTION.VARS
! Description: Reset the work variables used in a WIC EBT transaction.
!*******************************************************************************
SUB WE.RESET.TRANSACTION.VARS

  WE.BENFT.RDMPT.IDX = 0
  WE.BUFFER.BR.RECEIPT = 0
  WE.BUFFER.BR.ONLY = 0

  WE.WIC.EBT.TENDERED = 0
  WE.TENDERING = 0
  WE.SL.IDENTIFIER = 0
  WE.SL.ICC.CODE = 0
  WE.NUM.CATEGORIES = 0
  WE.NUM.ORG.CATEGORIES = 0
  WE.ITEM.COUNT = 0
  WE.CPN.COUNT = 0
  WE.TOTAL.ITEM.VALUE = 0
  WE.TOTAL.DISC.VALUE = 0
  WE.TOTAL.CLAIM.AMOUNT = 0

  WE.SL.AGENCY.ID$ = ""
  WE.SL.PAN$ = ""
  WE.SL.PAN.LEN$ = ""
  WE.SL.ERR.CODE$ = ""
  WE.SL.CLAIM.AMOUNT$ = ""
  WE.SL.POS.DATA.CODE$ = ""
  WE.SL.ISSUING.AGENCY$ = ""
  WE.SL.FIRST.DATE.SPEND$ = ""
  WE.SL.LAST.DATE.SPEND$ = ""
  WE.SL.DISCOUNT.AMOUNT$ = ""
  WE.SL.TRNX.SIGNATURE$ = ""
  WE.SL.ITEM.CATEGORY$ = ""
  WE.SL.ITEM.SUBCATEGORY$ = ""
  WE.SL.ITEM.BENEFIT.QTY$ = ""
  WE.SL.ITEM.N.PURCH.QTY$ = ""
  WE.SL.ITEM.0.PURCH.QTY$ = ""
  WE.SL.ITEM.N.COST$ = ""
  WE.SL.ITEM.0.COST$ = ""
  WE.SL.ITEM.TLOG.INDEX$ = ""
  WE.SL.ITEM.UPC$ = ""
  WE.SL.ITEM.N.COUPON$ = ""
  WE.SL.ITEM.0.COUPON$ = ""
  WE.SL.ITEM.N.CHILD.ARRAY$ = ""
  WE.SL.ITEM.0.CHILD.ARRAY$ = ""
  WE.SL.TENDER.TIMEDATE$ = ""                ! IO05348

  WE.ITEMS.TO.DECREMENT$ = ""

  DIM WE.BENFT.RDMPT.RCPT$(0)
  DIM WE.BENFT.RDMPT.FRMT$(0)

  DIM WE.CATEGORY$(0)
  DIM WE.CATEGORY.QTY$(0)
  DIM WE.CATEGORY.UPD.QTY$(0)
  DIM WE.CATEGORY.UNIT$(0)
  DIM WE.CATEGORY.DESC$(0)
  DIM WE.CATEGORY.ITEM.PTR$(0)
  DIM WE.CATEGORY.HAS.ITEMS(0)
  DIM WE.CATEGORY.DEBIT.QTY(0)

  DIM WE.ORG.CATEGORY.QTY$(0)
  DIM WE.ORG.CATEGORY.UNIT$(0)
  DIM WE.ORG.CATEGORY.DESC$(0)

  DIM WE.ITEM.FULL.CATEGORY$(0)
  DIM WE.ITEM.CODE$(0)
  DIM WE.ITEM.CHECKDIGIT$(0)
  DIM WE.ITEM.UNIT.PRICES$(0)
  DIM WE.ITEM.0.CHILD.ARRAY$(0)
  DIM WE.ITEM.N.CHILD.ARRAY$(0)
  DIM WE.ITEM.APL.FILE.DATA$(0)
  DIM WE.ITEM.UNIT.DESC$(0)
  DIM WE.ITEM.CPN.0.POINTER$(0)
  DIM WE.ITEM.CPN.N.POINTER$(0)
  DIM WE.ITEM.CPN.0.TLOG$(0)
  DIM WE.ITEM.CPN.N.TLOG$(0)
  DIM WE.ITEM.USED(0)
  DIM WE.ITEM.INDICAT1(0)
  DIM WE.ITEM.INDICAT2(0)
  DIM WE.ITEM.0.RX.INDEX(0)
  DIM WE.ITEM.N.RX.INDEX(0)
  DIM WE.ITEM.BENEFIT.QTY(0)
  DIM WE.ITEM.N.PURCH.QTY(0)
  DIM WE.ITEM.0.PURCH.QTY(0)
  DIM WE.ITEM.N.COST(0)
  DIM WE.ITEM.0.COST(0)
! DIM WE.ITEM.PRICE(0)  IO01214
  DIM WE.ITEM.XPRICE(0)
  DIM WE.ITEM.DEALQTY(0)

  DIM WE.CPN.TLOG.OFFSET(0)
  DIM WE.CPN.QUANTITY(0)
  DIM WE.CPN.VALUE(0)

  DIM WE.TAX(0)
  DIM WE.SL.TAX$(0)

END SUB                                          ! end WE.RESET.TRANSACTION.VARS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.RESET.OPT.WORK.VARS
! Description: Reset the feature option work variables.  These variables hold
!              the current State option data.
!*******************************************************************************
SUB WE.RESET.OPT.WORK.VARS

  WE.STATE.ID$ = ""                              ! customer state id
  WE.BIN.NUMBER$ = ""                            ! customer BIN number
  WE.MERCHANT.ID$ = ""                           ! customer merchant ID
  WE.FORWARD.INSTITUTE$ = ""                     ! customer forwarding institute
  WE.RECEIVE.INSTITUTE$ = ""                     ! customer receiving institute

  DIM WE.BENEFIT.HDR$(0)                         ! benefit header print lines
  DIM WE.BENEFIT.ITM$(0)                         ! benefit item print lines
  DIM WE.BENEFIT.TRL$(0)                         ! benefit trailer print lines
  DIM WE.REDEMPT.HDR$(0)                         ! redemption header print lines
  DIM WE.REDEMPT.ITM$(0)                         ! redemption item print lines
  DIM WE.REDEMPT.TRL$(0)                         ! redemption trailer print line

  DIM WE.BENEFIT.HDR.LF(0)                       ! benefit header linefeed
  DIM WE.BENEFIT.ITM.LF(0)                       ! benefit item linefeed
  DIM WE.BENEFIT.TRL.LF(0)                       ! benefit trailer linefeed
  DIM WE.REDEMPT.HDR.LF(0)                       ! redemption header linefeed
  DIM WE.REDEMPT.ITM.LF(0)                       ! redemption item linefeed
  DIM WE.REDEMPT.TRL.LF(0)                       ! redemption trailer linefeed

END SUB                                          ! end WE.RESET.OPT.WORK.VARS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.RESET.OPT.VARS
! Description: Reset all the feature option variables.
!*******************************************************************************
SUB WE.RESET.OPT.VARS

  WE.STATE.CODE$ = ""                            ! reset the State Code string
  WE.INITIALIZE.PINPAD = 0                       ! reset initialize PinPad flag

  WE.NUMBER.OF.AGENCIES = 0                      ! Tot num of agencies available
  WE.TENDER.TYPE = 0                             ! Tender type for EBT WIC
  WE.TENDER.VARIETY = 0                          ! Tender variety for EBT WIC

  WE.DUP.RECPT.HDR1$ = ""                        ! Hdr 1 text of dup receipts
  WE.DUP.RECPT.HDR1.LF = 0                       ! linefeeds for dup receipt 1

  WE.BNA.HEADER1$ = ""                           ! "Benefits Not Available" 1
  WE.BNA.HEADER1.LF = 0                          ! linefeeds for BNA header 1

  WE.BNA.HEADER2$ = ""                           ! "Benefits Not Available" 2
  WE.BNA.HEADER2.LF = 0                          ! linefeeds for BNA" header 2

  WE.DAYLIGHT.SAVINGS = 0                        ! daylight savings time flag
  WE.GMT.LOCAL.DIFF = 0                          ! local and GMT diff in hours

  DIM WE.AGENCY.MAIN.OPTION$(0)                  ! Agencies main option rec data
  DIM WE.AGENCY.BENEFIT.HDR$(0)                  ! Benefit hdr/linefeeds/fonts
  DIM WE.AGENCY.BENEFIT.ITM$(0)                  ! Benefit item linefeeds/fonts
  DIM WE.AGENCY.BENEFIT.TRL$(0)                  ! Benefit trailer lfs/fonts
  DIM WE.AGENCY.REDEMPT.HDR$(0)                  ! Redemption header lfs/fonts
  DIM WE.AGENCY.REDEMPT.ITM$(0)                  ! Redemption item lfs/fonts
  DIM WE.AGENCY.REDEMPT.TRL$(0)                  ! Redemption trailer lfs/fonts

  CALL WE.RESET.OPT.WORK.VARS                    ! reset all the work variables
  CALL WE.RESET.TRANSACTION.VARS                 ! reset the trnx variables
  CALL WE.RESET.PRINT.VARS                       ! reset all the printing vars

END SUB                                          ! end WE.RESET.OPT.VARS
!*******************************************************************************


!*******************************************************************************
! Function Name: TRANSLATED.ERRCODE
! Description: Translate the passed error code into an integer.
!              We return a 4 byte to prevent going negative.
!              For example, if "FFFF" is passed in, then we return a 65535
!              instead of -1.
!              Most of these errors should be coming from the PinPad and be 2
!              bytes.
!              Any unique errors we log in the terminal such as a bad read on a
!              file will be greater than two bytes, (i.e. value greater than
!              FFFF).  We never use the upper nibble of the 4 bytes to prevent
!              overflow errors.
!*******************************************************************************
FUNCTION TRANSLATED.ERRCODE(ERR.CODE$)

  STRING                               \
         ERR.CODE$                     ! error code to translate

  INTEGER*4                            \
            TRANSLATED.ERRCODE         ! function return

  TRANSLATED.ERRCODE = 0                         ! initialize function

  ! assure we only look at three bytes and that it's uppercase
  ERR.CODE$ = UCASE$(RIGHT$(STRING$(6,"0") + ERR.CODE$,6))

  ERR.CODE$ = TRANSLATE$(ERR.CODE$,"ABCDEF",":;<=>?")

  ! Assure input is good data
  K% = LEN(ERR.CODE$)
  FOR I% = 1 TO K%
    IF (MID$(ERR.CODE$,I%,1) < "0") OR           \ if invalid data
       (MID$(ERR.CODE$,I%,1) > "?")              \
    THEN BEGIN
     EXIT FUNCTION                               ! get out, bad input
    ENDIF                                        ! if invalid data
  NEXT I%

  ERR.CODE$ = PACK$(ERR.CODE$)                   ! pack it
  ERR.CODE$ = RIGHT$(ERR.CODE$,1)                \ reverse the bytes
            + MID$(ERR.CODE$,2,1)                \
            + LEFT$(ERR.CODE$,1)                 \
            + CHR$(0)                            ! add to make full 4 bytes

  TRANSLATED.ERRCODE = GETN4(ERR.CODE$,0)        ! return the integer value

END FUNCTION                                     ! end TRANSLATED.ERRCODE
!*******************************************************************************


!*******************************************************************************
! Function Name: STRIP.BLANKS$
! Description: This function strips any leading and trailing blanks from a
!              passed string
!*******************************************************************************
FUNCTION STRIP.BLANKS$(PASSED.STRING$)

  STRING                               \
         PASSED.STRING$,               \ string passed to function
         STRIP.BLANKS$                 ! function return

  STRIP.BLANKS$ = ""                             ! initialize function return

  IF (PASSED.STRING$ = "") THEN BEGIN            ! if nothing in string
    EXIT FUNCTION                                ! we're done
  ENDIF                                          ! if nothing in string

  ! strip left side of string
  WHILE (LEFT$(PASSED.STRING$,1) = " ")
    PASSED.STRING$ = RIGHT$(PASSED.STRING$, LEN(PASSED.STRING$) - 1)
    IF (PASSED.STRING$ = "") THEN BEGIN
      EXIT FUNCTION
    ENDIF
  WEND

  ! strip right side of string
  WHILE (RIGHT$(PASSED.STRING$,1) = " ")
    PASSED.STRING$ = LEFT$(PASSED.STRING$, LEN(PASSED.STRING$) - 1)
  WEND

  STRIP.BLANKS$ = PASSED.STRING$

END FUNCTION                                     ! end STRIP.BLANKS$
!*******************************************************************************


!*******************************************************************************
! Function Name: WE.ADD.FONT$
! Description: This function adds font characters to the passed string.
!              If 4610 is not enabled then only the passed string is returned.
!
! The value for the passed font should be...
!
! 0=Normal              4=Bold
! 1=Medium (font B)     5=Medium Bold
! 2=Double Wide         6=Double Wide Bold
! 3=Double High Wide    7=Double High Wide Bold
!*******************************************************************************
FUNCTION WE.ADD.FONT$(PRINT.LINE$,FONT)

  STRING                               \
         PRINT.LINE$,                  \ string passed to subroutine
         WE.ADD.FONT$                  ! function return

  INTEGER*1                            \
            FONT                       ! font setting

  INTEGER*2                            \
            FONT.SETTING               ! mask setting for font

  WE.ADD.FONT$ = PRINT.LINE$                     ! ready to return org string

  IF (PRT4610.ENABLE = 0) THEN BEGIN             ! if not a 4610 printer
    EXIT FUNCTION
  ENDIF                                          ! if not a 4610 printer

  IF (FONT = 0) THEN BEGIN                       ! if no changes to font
    EXIT FUNCTION                                ! get out, nothing to do
  ENDIF                                          ! if no changes to font

  IF (FONT < 0) OR (FONT > 7) THEN BEGIN         ! if invalid font setting
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid font setting

  IF (PRINT.LINE$ = "") THEN BEGIN               ! if nothing to print
    EXIT FUNCTION
  ENDIF                                          ! if nothing to print

  ! Use an indicator field to set up the fonts.
  ! Hopefully this will make it clearer if any changes are needed later

  FONT.SETTING = 0                               ! use as a indicator field

  ON FONT GOSUB FONT1,FONT2,FONT3,FONT4,FONT5,FONT6,FONT7

  WE.ADD.FONT$ = CHR$(1BH) + CHR$(21H) + CHR$(FONT.SETTING) + PRINT.LINE$

  EXIT FUNCTION

FONT1:   ! Medium (font B)

  FONT.SETTING = FONT.SETTING OR 1
  RETURN

FONT2:   ! Double Wide

  FONT.SETTING = FONT.SETTING OR 32
  RETURN

FONT3:   ! Double High Wide

  FONT.SETTING = FONT.SETTING OR 16
  FONT.SETTING = FONT.SETTING OR 32
  RETURN

FONT4:   ! Bold

  FONT.SETTING = FONT.SETTING OR 8
  RETURN

FONT5:   ! Medium (font B) Bold

  FONT.SETTING = FONT.SETTING OR 1
  FONT.SETTING = FONT.SETTING OR 8
  RETURN

FONT6:   ! Double Wide Bold

  FONT.SETTING = FONT.SETTING OR 8
  FONT.SETTING = FONT.SETTING OR 32
  RETURN

FONT7:   ! Double High Wide Bold

  FONT.SETTING = FONT.SETTING OR 8
  FONT.SETTING = FONT.SETTING OR 16
  FONT.SETTING = FONT.SETTING OR 32
  RETURN

END FUNCTION                                     ! end WE.ADD.FONT$
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PARSE.OPTION.DATA
! Description: This routine parses all the option data into work variables for
!              the passed State code.  If the State is not found in the option
!              data then a unique error is returned.
!              The error codes are set up as strings to work with the pinPad API
!              error return code logging functions.
!*******************************************************************************
SUB PARSE.OPTION.DATA(ERR.CODE$,STATE$)

  STRING                               \
         STATE$,                       \ State code returned from API
         ERR.CODE$                     ! error return code from this method

  INTEGER*1                            \
            FONT                       ! font pulled from option data

  INTEGER*2                            \
            WE.INDEX                   ! index into option for passed state code

  IF (WE.STATE.ID$ = STATE$) THEN BEGIN          ! if we've already set up opts
    EXIT SUB                                     ! get out
  ENDIF                                          ! if we've already set up opts

  ON ERROR GOTO PARSE.OPTION.ERR

  WE.INDEX = 0

  FOR I% = 1 TO WE.NUMBER.OF.AGENCIES
    A$ = LEFT$(WE.AGENCY.MAIN.OPTION$(I%),2)     ! extract state from options
    IF (A$ = STATE$) THEN BEGIN                  ! if we've found a match
      WE.INDEX = I%                              ! save index
      I% = WE.NUMBER.OF.AGENCIES                 ! break loop
    ENDIF                                        ! if we've found a match
  NEXT I%

  IF (WE.INDEX <= 0) THEN BEGIN                  ! if we did not find a match
    ERR.CODE$ = "10000"                          ! flag unique application error
    EXIT SUB                                     ! we're done
  ENDIF                                          ! if we did not find a match

  A$ = WE.AGENCY.MAIN.OPTION$(WE.INDEX)          ! put option data into work var

  WE.STATE.ID$ = STATE$
  WE.BIN.NUMBER$ = UNPACK$(MID$(A$,4,3))
  WE.MERCHANT.ID$ = MID$(A$,7,15)
  WE.FORWARD.INSTITUTE$ = RIGHT$(UNPACK$(MID$(A$,22,6)),11)
  WE.RECEIVE.INSTITUTE$ = RIGHT$(UNPACK$(MID$(A$,28,6)),11)

  ! *************** Format the benefit header lines ***************

  A$ = WE.AGENCY.BENEFIT.HDR$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.BENEFIT.HDR%                       ! current number of benefit hdr

  DIM WE.BENEFIT.HDR$(J%)
  DIM WE.BENEFIT.HDR.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.BENEFIT.HDR$(I%) = MID$(A$,K%,38)
    WE.BENEFIT.HDR.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.BENEFIT.HDR$(I%) = WE.ADD.FONT$(WE.BENEFIT.HDR$(I%),FONT)
    K% = K% + 40
  NEXT I%

  ! *************** Format the benefit item line ***************

  A$ = WE.AGENCY.BENEFIT.ITM$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.BENEFIT.ITM%                       ! curr number of benefit items

  DIM WE.BENEFIT.ITM$(J%)
  DIM WE.BENEFIT.ITM.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.BENEFIT.ITM$(I%) = MID$(A$,K%,38)
    WE.BENEFIT.ITM.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.BENEFIT.ITM$(I%) = WE.ADD.FONT$(WE.BENEFIT.ITM$(I%),FONT)
    K% = K% + 40
  NEXT I%

  ! *************** Format the benefit trailer lines ***************

  A$ = WE.AGENCY.BENEFIT.TRL$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.BENEFIT.TRL%                       ! curr num of benefit trailers

  DIM WE.BENEFIT.TRL$(J%)
  DIM WE.BENEFIT.TRL.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.BENEFIT.TRL$(I%) = MID$(A$,K%,38)
    WE.BENEFIT.TRL.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.BENEFIT.TRL$(I%) = WE.ADD.FONT$(WE.BENEFIT.TRL$(I%),FONT)
    K% = K% + 40
  NEXT I%

  ! *************** Format the redemption header lines ***************

  A$ = WE.AGENCY.REDEMPT.HDR$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.REDEMPT.HDR%                       ! curr num of redemption hdrs

  DIM WE.REDEMPT.HDR$(J%)
  DIM WE.REDEMPT.HDR.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.REDEMPT.HDR$(I%) = MID$(A$,K%,38)
    WE.REDEMPT.HDR.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.REDEMPT.HDR$(I%) = WE.ADD.FONT$(WE.REDEMPT.HDR$(I%),FONT)
    K% = K% + 40
  NEXT I%

  ! *************** Format the redemption item lines ***************

  A$ = WE.AGENCY.REDEMPT.ITM$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.REDEMPT.ITM%                       ! curr num of redemption items

  DIM WE.REDEMPT.ITM$(J%)
  DIM WE.REDEMPT.ITM.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.REDEMPT.ITM$(I%) = MID$(A$,K%,38)
    WE.REDEMPT.ITM.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.REDEMPT.ITM$(I%) = WE.ADD.FONT$(WE.REDEMPT.ITM$(I%),FONT)
    K% = K% + 40
  NEXT I%

  ! *************** Format the redemption trailer lines ***************

  A$ = WE.AGENCY.REDEMPT.TRL$(WE.INDEX)          ! put option data into work var
  J% = WE.NUM.REDEMPT.TRL%                       ! curr num of redemption trls

  DIM WE.REDEMPT.TRL$(J%)
  DIM WE.REDEMPT.TRL.LF(J%)

  K% = 1
  FOR I% = 1 TO J%
    WE.REDEMPT.TRL$(I%) = MID$(A$,K%,38)
    WE.REDEMPT.TRL.LF(I%) = VAL(UNPACK$(MID$(A$,K% + 38,1)))
    FONT = VAL(UNPACK$(MID$(A$,K% + 39,1)))
    WE.REDEMPT.TRL$(I%) = WE.ADD.FONT$(WE.REDEMPT.TRL$(I%),FONT)
    K% = K% + 40
  NEXT I%

PARSE.OPTION.EXIT:
  A$ = ""                                        ! cleanup
  EXIT SUB

PARSE.OPTION.ERR:

  WE.ERR$ = ERR
  WE.ERRN$ = ERRNHEX$                            ! Convert error code

  ERR.CODE$ = "20000"                            ! flag unique error

  ! build unique error message with error codes
  A$ = "WEpo "                                   \
     + WE.ERR$                                   \
     + " "                                       \
     + STATE$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  RESUME PARSE.OPTION.EXIT                       ! always resume from error

END SUB                                          ! end PARSE.OPTION.DATA
!*******************************************************************************


!*******************************************************************************
! Function Name: SET.AGENCY.NULL.TO.BLANK$
! Description: Function translates the agency print lines from Null to blanks
!              during initialization. It's possible the read of the options will
!              leave nulls in the print line which will cause printer errors.
!*******************************************************************************
FUNCTION SET.AGENCY.NULL.TO.BLANK$(AGENCY.PRINTLINES$,MAX.LINES)

  STRING                               \
         AGENCY.PRINTLINES$,           \ print lines we need to translate
         SET.AGENCY.NULL.TO.BLANK$     ! Function return

  INTEGER*2                            \
            MAX.LINES                  ! max number of lines we're looking for

  ! if the max number of lines not valid, then just return the passed string
  IF (MAX.LINES <= 0) THEN BEGIN                 ! if invalid max lines
    SET.AGENCY.NULL.TO.BLANK$ = AGENCY.PRINTLINES$
    EXIT FUNCTION
  ENDIF                                          ! if invalid max lines

  A$ = AGENCY.PRINTLINES$                        ! save entire string
  I% = MAX.LINES * 40                            ! calculate max size of string
  A$ = LEFT$(A$ + STRING$(I%," "),I%)            ! make sure string is right
  AGENCY.PRINTLINES$ = ""                        ! reset string
  FOR I% = 1 TO MAX.LINES                        ! for each print line
    B$ = LEFT$(A$,40)                            ! save print line and lf/font
    A$ = RIGHT$(A$,LEN(A$) - 40)                 ! truncate the data
    C$ = LEFT$(B$,38)                            ! extract just the print line
    IF (MATCHB(CHR$(0),C$,1)) THEN BEGIN         ! if null characters found
      D$ = RIGHT$(B$,2)                          ! extract linefeed/font
      C$ = TRANSLATE$(C$,CHR$(0)," ")            ! translate nulls to blanks
      B$ = C$ + D$                               ! rebuild translated print line
    ENDIF                                        ! if null characters found
    AGENCY.PRINTLINES$ = AGENCY.PRINTLINES$ + B$ ! build new string
  NEXT I%                                        ! for each print line

  ! return converted string
  SET.AGENCY.NULL.TO.BLANK$ = AGENCY.PRINTLINES$

  A$ = ""                                        ! cleanup global work variables
  B$ = ""
  C$ = ""
  D$ = ""

END FUNCTION                                     ! end SET.AGENCY.NULL.TO.BLANK$
!*******************************************************************************


!*******************************************************************************
! Function Name: END.WIC.EBT.TRANSACTION
! Description: This function ends the WIC EBT transaction by calling the
!              WICEndTransaction method (sends the _50 message that really just
!              puts up a text message) and the WICRemoveCard method (sends the
!              _80 message that prompts to remove the SmartCard).
!             The State Code and Completion Code, (text message), is passed in
!             by the caller.
!             This method should be called whenever we're done with the PinPad.
!*******************************************************************************
FUNCTION END.WIC.EBT.TRANSACTION(STATE$,COMPLETION.CODE$)

  STRING                               \
         ET.ERR.CODE$,                 \ Error return code for WICEndTransaction
         RC.ERR.CODE$,                 \ Error return code for WICRemoveCard
         STATE$,                       \ State code
         COMPLETION.CODE$              ! Completion Code to send pinpad

  ! Note: The WICEndTransaction method is really to display text message and the
  ! WICRemoveCard method really cleans up the PinPad.
  ! This is left over implementation from the ACE development project.

  IF (STATE$ <> "00") AND (STATE$ <> "")         \ if we have a State code
  THEN BEGIN
    CALL WICEndTransaction(STATE$,COMPLETION.CODE$,ET.ERR.CODE$)
    IF (ET.ERR.CODE$ <> "0000")                  \ if an error on WIC function
    THEN BEGIN

      ! build unique error message with error codes
      A$ = "WEet"                                \
         + LEFT$(STATE$ + STRING$(2," "),2)

      IF (LEN(A$ + ET.ERR.CODE$) <= 10)          \ if room for error code
      THEN BEGIN
        A$ = A$ + ET.ERR.CODE$                   ! add error code to message
      ENDIF ELSE BEGIN                           ! else no room for error code
        ! mark continuation for msg
        A$ = LEFT$(A$ + " *" + STRING$(10," "),10)

        B$ = "WEet" + ET.ERR.CODE$               ! build msg with just err code

        CALL ADXERROR(VAL(TS.TERMINAL$),         \ log error
                      ASC("U"),                  \
                      0,                         \
                      1,                         \ severity level
                      2,                         \
                      B$)
        B$ = ""                                  ! cleanup
      ENDIF                                      ! if room for error code

      CALL ADXERROR(VAL(TS.TERMINAL$),           \ log error
                    ASC("U"),                    \
                    0,                           \
                    1,                           \ severity level
                    2,                           \
                    A$)

      A$ = ""                                    ! cleanup
    ENDIF                                        ! if an error on WIC function
  ENDIF                                          ! if we have a State code

  CALL WICRemoveCard(RC.ERR.CODE$)

  IF (RC.ERR.CODE$ <> "0000")                    \ if an error on WIC function
  THEN BEGIN

    ! build unique error message with error codes
    A$ = "WErc "                                 \
       + RC.ERR.CODE$

    CALL ADXERROR(VAL(TS.TERMINAL$),             \ log error
                  ASC("U"),                      \
                  0,                             \
                  1,                             \ severity level
                  2,                             \
                  A$)

    A$ = ""                                      ! cleanup
  ENDIF                                          ! if an error on WIC function

  ET.ERR.CODE$ = ""                              ! cleanup
  RC.ERR.CODE$ = ""

END FUNCTION                                     ! end END.WIC.EBT.TRANSACTION
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.READ.OPTIONS
! Description: Read the WIC EBT option file EAMWEOPT.DAT.
!*******************************************************************************
SUB WE.READ.OPTIONS

  STRING                               \
         NUM.AGENCIES$,                \ Number of agencies supported
         WIC.TENDER$,                  \ Tender type/variety for EBT WIC tender
         DUP.HDR1$,                    \ Text for header 1 of duplicate receipts
         DH1.LF$,                      \ linefeed for Duplicate header 1
         DH1.FONT$,                    \ font for Duplicate header 1
         BNA.HDR1$,                    \ "Benefits Not Available" header 1 text
         BNA1.LF$,                     \ linefeed for "Benefits Not Available" 1
         BNA1.FONT$,                   \ font for "Benefits Not Available" 1
         BNA.HDR2$,                    \ "Benefits Not Available" header 2 text
         BNA2.LF$,                     \ linefeed for "Benefits Not Available" 2
         BNA2.FONT$,                   \ font for "Benefits Not Available" 2
         DAYLIGHT.SAVING$,             \ observed/not observed flag
         GMT.DIFF$,                    \ difference between gmt and local time
         CONTROL.FMT$,                 \ format string for the control record
         AGENCY.FMT$,                  \ format string for the agency records
         ERR.MARK$                     ! set mark for error logging

  INTEGER*1                            \
            FONT                       ! font read from options

  ON ERROR GOTO WE.READ.OPT.ERR                  ! handle errors locally

  ERR.MARK$ = " "

  ! create the read format for the records
  CONTROL.FMT$ = "2C1,C38,2C1,C38,2C1,C38,4C1,3C255,C135"
  AGENCY.FMT$ = "C33,C240,C40,C120,C240,C120,C80,C151"

  CLOSE 8                                        ! make sure session freed

  ERR.MARK$ = "1"                                ! mark opening option file

  OPEN "R::$AMWEOPT" DIRECT RECL 1024 AS 8 BUFFSIZE 1024 NOWRITE NODEL

  ERR.MARK$ = "2"                                ! mark reading control record

  ! Read the control record
  READ FORM CONTROL.FMT$; #8,100;                  \
                                 NUM.AGENCIES$,    \
                                 WIC.TENDER$,      \
                                 DUP.HDR1$,        \
                                 DH1.LF$,          \
                                 DH1.FONT$,        \
                                 BNA.HDR1$,        \
                                 BNA1.LF$,         \
                                 BNA1.FONT$,       \
                                 BNA.HDR2$,        \
                                 BNA2.LF$,         \
                                 BNA2.FONT$,       \
                                 DAYLIGHT.SAVING$, \
                                 GMT.DIFF$,        \
                                 A$,B$,B$,B$       ! reserved data

  ERR.MARK$ = "3"                                ! mark formatting control rec

  ! Format control record data

  WE.NUMBER.OF.AGENCIES = VAL(UNPACK$(NUM.AGENCIES$))

  IF (WE.NUMBER.OF.AGENCIES > 0) THEN BEGIN      ! if valid number of agencies

    WIC.TENDER$ = RIGHT$("00" + UNPACK$(WIC.TENDER$),2)
    WE.TENDER.TYPE = VAL(LEFT$(WIC.TENDER$,1))
    WE.TENDER.VARIETY = VAL(RIGHT$(WIC.TENDER$,1))

    ! convert null characters to blanks in print string
    WE.DUP.RECPT.HDR1$ = TRANSLATE$(DUP.HDR1$,CHR$(0)," ")
    WE.DUP.RECPT.HDR1.LF = VAL(UNPACK$(DH1.LF$))
    FONT = VAL(UNPACK$(DH1.FONT$))
    WE.DUP.RECPT.HDR1$ = WE.ADD.FONT$(WE.DUP.RECPT.HDR1$,FONT)

    ! convert null characters to blanks in print string
    WE.BNA.HEADER1$ = TRANSLATE$(BNA.HDR1$,CHR$(0)," ")
    WE.BNA.HEADER1.LF = VAL(UNPACK$(BNA1.LF$))
    FONT = VAL(UNPACK$(BNA1.FONT$))
    WE.BNA.HEADER1$ = WE.ADD.FONT$(WE.BNA.HEADER1$,FONT)

    ! convert null characters to blanks in print string
    WE.BNA.HEADER2$ = TRANSLATE$(BNA.HDR2$,CHR$(0)," ")
    WE.BNA.HEADER2.LF = VAL(UNPACK$(BNA2.LF$))
    FONT = VAL(UNPACK$(BNA2.FONT$))
    WE.BNA.HEADER2$ = WE.ADD.FONT$(WE.BNA.HEADER2$,FONT)

    IF (DAYLIGHT.SAVING$ = "Y") THEN BEGIN       ! if daylight savings state
      WE.DAYLIGHT.SAVINGS = -1
    ENDIF ELSE BEGIN
      WE.DAYLIGHT.SAVINGS = 0
    ENDIF                                        ! if daylight savings state

    WE.GMT.LOCAL.DIFF = VAL(UNPACK$(GMT.DIFF$))  ! diff between local and GMT

    ERR.MARK$ = "4"                              ! mark reading agency records

    DIM WE.AGENCY.MAIN.OPTION$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.BENEFIT.HDR$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.BENEFIT.ITM$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.BENEFIT.TRL$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.REDEMPT.HDR$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.REDEMPT.ITM$(WE.NUMBER.OF.AGENCIES)
    DIM WE.AGENCY.REDEMPT.TRL$(WE.NUMBER.OF.AGENCIES)

    ! Now read all the agency records
    FOR I% = 1 TO WE.NUMBER.OF.AGENCIES
      READ FORM AGENCY.FMT$; #8,I%;                             \
                                   WE.AGENCY.MAIN.OPTION$(I%),  \
                                   WE.AGENCY.BENEFIT.HDR$(I%),  \
                                   WE.AGENCY.BENEFIT.ITM$(I%),  \
                                   WE.AGENCY.BENEFIT.TRL$(I%),  \
                                   WE.AGENCY.REDEMPT.HDR$(I%),  \
                                   WE.AGENCY.REDEMPT.ITM$(I%),  \
                                   WE.AGENCY.REDEMPT.TRL$(I%),  \
                                   A$                           ! reserved
    NEXT I%

  ENDIF ELSE BEGIN                               ! else no agencies specified
    TO.WE.ENABLED = 0                            ! disable WIC EBT feature
  ENDIF                                          ! if valid number of agencies

WE.READ.OPT.EXIT:

  CLOSE 8                                        ! make sure session closed
  NUM.AGENCIES$ = ""                             ! cleanup
  WIC.TENDER$ = ""
  DUP.HDR1$ = ""
  DH1.LF$ = ""
  DH1.FONT$ = ""
  BNA.HDR1$ = ""
  BNA1.LF$ = ""
  BNA1.FONT$ = ""
  BNA.HDR2$ = ""
  BNA2.LF$ = ""
  BNA2.FONT$ = ""
  DAYLIGHT.SAVING$ = ""
  GMT.DIFF$ = ""
  CONTROL.FMT$ = ""
  AGENCY.FMT$ = ""
  A$ = ""
  B$ = ""

  EXIT SUB

!************************
! Error Routine
!************************
WE.READ.OPT.ERR:

  WE.ERR$ = ERR
  WE.ERRN$ = ERRNHEX$                            ! Convert error code

  IF (WE.ERR$ = "CU") THEN BEGIN                 ! Tried to close a session
    RESUME                                       ! number that is not active
  ENDIF

  ! build unique error message
  A$ = LEFT$("WEo" + ERR.MARK$ + STRING$(4," "),4)  \
     + WE.ERR$                                      \
     + WE.ERRN$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  TO.WE.ENABLED = 0                              ! disable WIC EBT feature

  RESUME WE.READ.OPT.EXIT                        ! always resume from on error

END SUB                                          ! end WE.READ.OPTIONS
!*******************************************************************************


!*******************************************************************************
! Function Name: WE.LOG.TLOG.ERROR
! Description: This function logs the terminal WIC error string (0x80) with
!              indicator (0x03) to the Tlog.
!              The Agency ID, (state code), PAN (Personal Account Number), and
!              the Err Code is passed in to be logged.  The error code is passed
!              in as both a string value, (such as "008C"), and as an int value,
!              (such as 140 for 0x8c) to be used for setting up the ICC code.
!              The length of the PAN is calculated from the passed string
!              before formatting it.
!*******************************************************************************
FUNCTION WE.LOG.TLOG.ERROR(AGENCY.ID$,PAN$,ERR.CODE$,ERR.CODE)

  STRING                               \
         AGENCY.ID$,                   \ Agency ID (state code)
         PAN$,                         \ Personal Account Number
         PAN.LEN$,                     \ PAN length in packed format
         ERR.CODE$                     ! error to be logged

  INTEGER*1                            \
            ICC.CODE                   ! code to log for ICC system

  INTEGER*4                            \
            ERR.CODE                   ! error code in integer form

  IF TS.TRAINING THEN BEGIN                      ! if training mode
    EXIT FUNCTION                                ! no logging in training mode
  ENDIF                                          ! if training mode

  WE.SL.AGENCY.ID$ = AGENCY.ID$

  ! format Pan data for logging
  WE.SL.PAN$ = PACK$(RIGHT$(STRING$(20,"0") + PAN$,20))
  WE.SL.PAN.LEN$ = PACK$(STR$(LEN(PAN$)))        ! Format length of PAN

  ! Now set up the ICC code for logging
  WE.SL.ICC.CODE = 0                             ! initialize the ICC code

  ! 009DH = Attempt was made to access a blocked card
  IF (ERR.CODE = 009DH) THEN BEGIN
    WE.SL.ICC.CODE = 1

  ! 0095H = Card Serial Number could not be retrieved from card
  ENDIF ELSE IF (ERR.CODE = 0095H) THEN BEGIN
    WE.SL.ICC.CODE = 2

  ! 0096H = Card PAN could not be read
  ENDIF ELSE IF (ERR.CODE = 0096H) THEN BEGIN
    WE.SL.ICC.CODE = 3

  ! 0098H = Cryptographic authentication of card failed
  ENDIF ELSE IF (ERR.CODE = 0098H) THEN BEGIN
    WE.SL.ICC.CODE = 4

  ! 0097H = Error occurred reading Administrative file on the card
  ENDIF ELSE IF (ERR.CODE = 0097H) THEN BEGIN
    WE.SL.ICC.CODE = 5

  ! 0001H = Applicable ACR was not fulfilled
  ENDIF ELSE IF (ERR.CODE = 0001H) THEN BEGIN
    WE.SL.ICC.CODE = 6

  ! 0099H = BIN for WIC Authority has expired or is not longer authorized
  ENDIF ELSE IF (ERR.CODE = 0099H) THEN BEGIN
    WE.SL.ICC.CODE = 7

  ! 009AH = Card failed to block access after Max number of PIN entries
  ENDIF ELSE IF (ERR.CODE = 009AH) THEN BEGIN
    WE.SL.ICC.CODE = 8

  ! 009BH = WIC Administrative file contains invalid values
  ENDIF ELSE IF (ERR.CODE = 009BH) THEN BEGIN
    WE.SL.ICC.CODE = 9

  ! 0088H = WIC benefit parameter on card is out of range
  ENDIF ELSE IF (ERR.CODE = 0088H) THEN BEGIN
    WE.SL.ICC.CODE = 10

  ! 008AH = Staged benefits are not valid for the current date
  ! 008FH = No Prescription data on card
  ENDIF ELSE IF (ERR.CODE = 008AH) OR            \
                (ERR.CODE = 008FH)               \
  THEN BEGIN
    WE.SL.ICC.CODE = 11

  ! 000AH = Card has been blocked - too many PIN attempts
  ENDIF ELSE IF (ERR.CODE = 000AH) THEN BEGIN
    WE.SL.ICC.CODE = 12

  ! 009CH = Card is being blocked because it is a "Hot" card
  ! 009FH = Card LOCKED, date and/or reason code is invalid
  ENDIF ELSE IF (ERR.CODE = 009CH) OR            \
                (ERR.CODE = 009FH)               \
  THEN BEGIN
    WE.SL.ICC.CODE = 13

  ! 0086H = Debit and Balance Inquiry are blocked on card
  ENDIF ELSE IF (ERR.CODE = 0086H) THEN BEGIN
    WE.SL.ICC.CODE = 14

  ENDIF

  WE.SL.ERR.CODE$ = ERR.CODE$

  WE.SL.IDENTIFIER = 3                           ! Identifier for exception log

  TS.TEMP1I1 = 80                                ! WIC EBT tlog string type
  CALL TSTPEC01                                  ! log the exception

  CALL EAMTRACE(4557H)                           ! trace "WE"

END FUNCTION                                     ! end WE.LOG.TLOG.ERROR
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PINPAD.RESPONSE.ERROR
! Description: Handle the errors passed back from the pinpad when reading the
!              Card.  This can be for events such as reading the PAN, blocking
!              the Card, etc.
!              This method is called whenever the WIC EBT tender is not to be
!              taken.
!*******************************************************************************
SUB PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)

  STRING                               \
         STATE$,                       \ State code returned from API
         PAN$,                         \ Personal Account Num returned from API
         ERR.CODE$                     ! error code returned from pinpad API

  INTEGER*4                            \
            ERR.CODE                   ! Translated Error code

  ! Translate the error code to integer for performance.
  ! Don't want to be doing string checks
  ERR.CODE = TRANSLATED.ERRCODE(ERR.CODE$)

  IF (ERR.CODE = 008CH) OR             \ if customer cancelled tender
     (ERR.CODE = 008EH)                \ customer cancelled PIN Entry
  THEN BEGIN
    TS.LINEDATA = 883                  ! Custr has canceled WIC tender
  ENDIF ELSE IF (ERR.CODE = 9993H)     \ if a tender mismatch
  THEN BEGIN
    TS.LINEDATA = 884                  ! Tender Mismatch Select correct tendr"
  ENDIF ELSE IF (ERR.CODE = 9995H) OR  \ No active state to send message to
                (ERR.CODE = 9998H)     \ State module in _00 msg does not exist
  THEN BEGIN
    TS.LINEDATA = 885                  ! "WIC EBT Agency not active"
  ENDIF ELSE IF (ERR.CODE = 0006H) OR  \ Card has been removed
                (ERR.CODE = 000AH) OR  \ Card's blocked-too many PIN attempts
                (ERR.CODE = 0086H) OR  \ Debit/Balance Inquiry blocked on card
                (ERR.CODE = 0087H) OR  \ WIC benefits on card have expired
                (ERR.CODE = 008AH) OR  \ Staged benefits not valid for curr date
                (ERR.CODE = 008FH) OR  \ No Prescription data on card
                (ERR.CODE = 0090H) OR  \ Customer PIN is invalid
                (ERR.CODE = 0093H) OR  \ Mode does not match
                (ERR.CODE = 009CH) OR  \ Card is blocked because it's "Hot" card
                (ERR.CODE = 009FH) OR  \ Card LOCKED, date/reason code invalid.
                (ERR.CODE = 9997H)     \ State Module does not exist for card
  THEN BEGIN
    TS.LINEDATA = 886                  ! "WIC EBT Transaction cannot complete"
  ENDIF ELSE IF (ERR.CODE <= 0FFFFH)   \ else unhandled PinPad error response
  THEN BEGIN
    TS.LINEDATA = 887                  ! "Check SmartCard reader on PinPad"
  ENDIF ELSE BEGIN                     ! else WIC EBT application error
    IF (ERR.CODE = 60000H) THEN BEGIN  ! no WIC EBT items found
      TS.LINEDATA = 889                ! "NO WIC EBT   ITEMS FOUND"
    ENDIF ELSE IF (ERR.CODE = 500000H) \ else Card already in Pin Pad device
    THEN BEGIN
      TS.LINEDATA = 896                ! 'REMOVE SMARTCARD FROM DEVICE'
    ENDIF ELSE IF (ERR.CODE = 200000H) \ else operator cleared from POS terminal
    THEN BEGIN
      TS.LINEDATA = 897                ! 'WIC EBT TRNX HAS BEEN CANCELED'
    ENDIF ELSE BEGIN
      TS.LINEDATA = 888                ! "WIC EBT APPL   ERR, CHECK EVENT LOG"
    ENDIF                              ! no WIC EBT items found
  ENDIF                                ! if customer canceled tender at pinpad

  ! reset all the stack error values to zero so not overrideable
  FOR I% = 0 TO 6
    TS.STACKERR(I%) = 0
  NEXT I%

  TS.LINETYPE = 8                      ! default message type
  TS.PURGE.ERR = -1                    ! purge any other errors
  CALL TSCSEC03                        ! Stack the error

  IF (ERR.CODE <> 008CH) THEN BEGIN    ! if customer didn't cancel at pinpad
    ! If we have a State code and PAN number, then log an error string
    IF (STATE$ <> "") AND (PAN$ <> "") \ if we have a State and Pan
    THEN BEGIN
      CALL WE.LOG.TLOG.ERROR(STATE$,PAN$,ERR.CODE$,ERR.CODE)
    ENDIF                              ! if we have a State and Pan
  ENDIF                                ! if customer didn't cancel at pinpad

  IF (ERR.CODE = 500000H) THEN BEGIN   ! if card from prev trnx error

    CALL ADXERROR(VAL(TS.TERMINAL$),   \ log error
                  ASC("U"),            \
                  0,                   \
                  1,                   \ severity level
                  2,                   \
                  "WEprevCard")

  ENDIF ELSE BEGIN                     ! else not card from prev trnx error

    ! If the operator cleared the wic transaction, then reset the state id,
    ! (Agency ID) to prevent the _50 message from being sent.
    ! When the operator clears, the pin pad is reset by the pin pad API code.
    IF (ERR.CODE = 200000H) THEN BEGIN ! if operator cleared the wic transaction
      STATE$ = ""                      ! reset state code
    ENDIF                              ! if operator cleared the wic transaction

    ! Call function to end the WIC trnx
    CALL END.WIC.EBT.TRANSACTION(STATE$,"01")
  ENDIF                                ! if card from prev trnx error

END SUB                                          ! end PINPAD.RESPONSE.ERROR
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.ADD.COUPON.TO.WIC.QTY
! Description: This routine adds the coupons associated with the WIC'ed item to
!              the WIC Tlog array for WIC quantity.
!*******************************************************************************
SUB WE.ADD.COUPON.TO.WIC.QTY(CPN.DATA$)

  STRING                               \
         CPN.DATA$                     ! Coupon data to be processed

  INTEGER*2                            \
            CPN.OFFSET.QTY(2)          ! hold the coupon offset and quantity

  IF (CPN.DATA$ = "") THEN BEGIN                 ! if no coupon data to process
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no coupon data to process

  A$ = UNPACK$(CPN.DATA$)                        ! get data unpacked

  IF (LEN(A$) < 8) THEN BEGIN                    ! if bad data
    A$ = ""                                      ! cleanup
    EXIT SUB                                     ! get out
  ENDIF                                          ! if bad data

  N% = INT%(LEN(CPN.DATA$) / 4)                  ! how many entries do we have
  DIM CPN.OFFSET.QTY(1,N%)                       ! 2 entries - offset and qty

  ! build an array of coupon data
  FOR I% = 1 TO N%
    B$ = LEFT$(A$,8)                             ! get index/quantity of coupon
    A$ = RIGHT$(A$,LEN(A$) - 8)                  ! truncate data
    TS.ER.RETURN = -1                            ! return from error
    CPN.OFFSET.QTY(0,I%) = VAL(LEFT$(B$,4))      ! get offset into Tlog array
    IF TS.ER.RETURN THEN BEGIN                   ! if no error on VAL statement
      CPN.OFFSET.QTY(1,I%) = VAL(RIGHT$(B$,4))   ! get quantity of coupon
    ENDIF                                        ! if no error on VAL statement
    IF TS.ER.RETURN THEN BEGIN                   ! if no errors
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else we had errors
      A$ = ""                                    ! cleanup
      B$ = ""
      DIM CPN.OFFSET.QTY(0,0)
      EXIT SUB                                   ! get out, this is garbage data
    ENDIF                                        ! if no errors
    IF (A$ = "") OR (LEN(A$) < 8) THEN BEGIN     ! if we're done with entry
      I% = N%                                    ! break loop
    ENDIF                                        ! if we're done with entry
  NEXT I%

  FOR I% = 1 TO N%                               ! now add coupons to Tlog array
    IF (CPN.OFFSET.QTY(0,I%) > 0) AND            \ if an offset into Tlog and
       (CPN.OFFSET.QTY(1,I%) > 0)                \ there is a quantity of cpns
    THEN BEGIN                                   ! if we have a coupon entry

      ! add this coupon amount to the Tlog quantity
      WE.WIC.QTY(CPN.OFFSET.QTY(0,I%)) = WE.WIC.QTY(CPN.OFFSET.QTY(0,I%))      \
                                       + CPN.OFFSET.QTY(1,I%)

    ENDIF                                        ! if we have a coupon entry
  NEXT I%                                        ! now add coupons to Tlog array

  A$ = ""                                        ! cleanup
  B$ = ""
  DIM CPN.OFFSET.QTY(0,0)

END SUB                                          ! end WE.ADD.COUPON.TO.WIC.QTY
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: REMOVE.AGENCY.RECORD
! Description: This routine shifts all the agency record elements down to
!              remove the indexed.
!*******************************************************************************
SUB REMOVE.AGENCY.RECORD(INDEX%)

  INTEGER*2                            \
            INDEX%,                    \ index of entry we need to remove
            WE.I%                      ! work variable

  IF (INDEX% > WE.NUMBER.OF.AGENCIES) OR         \ if invalid entry
     (WE.NUMBER.OF.AGENCIES <= 0)                \
  THEN BEGIN
    EXIT SUB
  ENDIF                                          ! if invalid entry

  ! shift the entries down
  FOR WE.I% = INDEX% TO (WE.NUMBER.OF.AGENCIES - 1)
    WE.AGENCY.MAIN.OPTION$(WE.I%) = WE.AGENCY.MAIN.OPTION$(WE.I% + 1)
    WE.AGENCY.BENEFIT.HDR$(WE.I%) = WE.AGENCY.BENEFIT.HDR$(WE.I% + 1)
    WE.AGENCY.BENEFIT.ITM$(WE.I%) = WE.AGENCY.BENEFIT.ITM$(WE.I% + 1)
    WE.AGENCY.BENEFIT.TRL$(WE.I%) = WE.AGENCY.BENEFIT.TRL$(WE.I% + 1)
    WE.AGENCY.REDEMPT.HDR$(WE.I%) = WE.AGENCY.REDEMPT.HDR$(WE.I% + 1)
    WE.AGENCY.REDEMPT.ITM$(WE.I%) = WE.AGENCY.REDEMPT.ITM$(WE.I% + 1)
    WE.AGENCY.REDEMPT.TRL$(WE.I%) = WE.AGENCY.REDEMPT.TRL$(WE.I% + 1)
  NEXT WE.I%

  WE.AGENCY.MAIN.OPTION$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.BENEFIT.HDR$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.BENEFIT.ITM$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.BENEFIT.TRL$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.REDEMPT.HDR$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.REDEMPT.ITM$(WE.NUMBER.OF.AGENCIES) = ""
  WE.AGENCY.REDEMPT.TRL$(WE.NUMBER.OF.AGENCIES) = ""

  WE.NUMBER.OF.AGENCIES = WE.NUMBER.OF.AGENCIES - 1
  INDEX% = INDEX% - 1                            ! reduce the index for the loop

END SUB                                          ! end REMOVE.AGENCY.RECORD
!*******************************************************************************


!*******************************************************************************
! Function Name: HOT.CARD.FILE.DATA$
! Description: This function reads the hot card file.  It returns the data read
!              from the file for the Passed PAN.
!*******************************************************************************
FUNCTION HOT.CARD.FILE.DATA$(PAN$,STATE$)

  STRING                               \
         HC.FMT$,                      \ format for read of file
         HC.KEY$,                      \ set up key for hot card file
         HOT.CARD.FILE$,               \ file name for Hot Card
         PAN$,                         \ Personal Account Number
         STATE$,                       \ State code to use in file lookup
         HOT.CARD.FILE.DATA$           ! function return

  INTEGER*2                            \
            HC.RECL                    ! record length for the Hot Card file

  HOT.CARD.FILE.DATA$ = ""                       ! assume no data to read

  ON ERROR GOTO HOT.CARD.DATA.ERR

  HC.RECL = 22
  HC.FMT$ = "C" + STR$(HC.RECL)
  HC.KEY$ = PACK$(RIGHT$(STRING$(20,"0") + PAN$,20))

  HOT.CARD.FILE$ = "R::WIC1:EAMWE" + STATE$ + ".HCL"

  CLOSE WE.SESSION.NUM1                          ! make sure session free

  OPEN HOT.CARD.FILE$ KEYED RECL HC.RECL AS WE.SESSION.NUM1 NOWRITE NODEL

  READ FORM HC.FMT$; #WE.SESSION.NUM1 KEY HC.KEY$; A$

  IF (A$ <> "") THEN BEGIN                       ! if data read
    IF (LEN(A$) = HC.RECL) THEN BEGIN            ! if data seems to be valid
      HOT.CARD.FILE.DATA$ = A$                   ! return it to the caller
    ENDIF                                        ! if data seems to be valid
  ENDIF                                          ! if data read

CLOSE.HOT.CARD.FILE:
  CLOSE WE.SESSION.NUM1
  HOT.CARD.FILE$ = ""                            ! cleanup
  HC.FMT$ = ""
  HC.KEY$ = ""
  A$ = ""
EXIT.HOT.CARD.DATA:
  EXIT FUNCTION

HOT.CARD.DATA.ERR:

  WE.ERR$ = ERR
  WE.ERRN$ = ERRNHEX$                            ! Convert error code

  IF (WE.ERR$ = "CU") THEN BEGIN                 ! Tried to close a session
    RESUME                                       ! number that is not active
  ENDIF

  IF (WE.ERR$ = "EF") THEN BEGIN                 ! Can't find record
    RESUME CLOSE.HOT.CARD.FILE                   ! resume to close file
  ENDIF

  ! build unique error message with error codes
  A$ = "WEhc"                                    \
     + WE.ERR$                                   \
     + WE.ERRN$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  ! build unique error message with state ID of problem
  A$ = "WEhc " + STATE$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  HOT.CARD.FILE.DATA$ = "ERROR"                  ! return an error

  RESUME EXIT.HOT.CARD.DATA                      ! always resume from error

END FUNCTION                                     ! end HOT.CARD.FILE.DATA$
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: CHECK.HOT.CARD.FILE
! Description: This routine checks if the PAN number is on the hot card file.
!*******************************************************************************
SUB CHECK.HOT.CARD.FILE(ERR.CODE$,PAN$,STATE$)

  STRING                               \
         HOT.CARD.DATA$,               \ Data read from the Hot Card File
         HC.PAN$,                      \ PAN read from the Hot Card File
         HC.PAN.LEN$,                  \ Length of the PAN
         HC.DATE.TIME.BAD$,            \ Date/Time PAN invalid (CCYYMMDDHHMMSS)
         HC.REASON.CODE$,              \ Message Reason Code for adding to file
         PAN$,                         \ Personal Account Number
         STATE$,                       \ State code to use in file lookup
         ERR.CODE$                     ! error code returned from pinpad API

  INTEGER*4                            \
            ERR.CODE                   ! Translated Error code

  ! read the file for the card data (if any)
  HOT.CARD.DATA$ = HOT.CARD.FILE.DATA$(PAN$,STATE$)

  IF (HOT.CARD.DATA$ = "") THEN BEGIN            ! if no data on hot card file
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no data on hot card file

  IF (HOT.CARD.DATA$ = "ERROR") THEN BEGIN       ! if an error on hot card file
    ERR.CODE$ = "30000"                          ! set up unique error
    EXIT SUB
  ENDIF                                          ! if an error on hot card file

  ! Parse the data read from the Hot Card File.
  ! Length of the data was validated in read function.
  HC.PAN$ = UNPACK$(LEFT$(HOT.CARD.DATA$,10))
  HC.PAN.LEN$ = UNPACK$(MID$(HOT.CARD.DATA$,11,1))
  HC.DATE.TIME.BAD$ = UNPACK$(MID$(HOT.CARD.DATA$,12,7))
  HC.REASON.CODE$ = RIGHT$(HOT.CARD.DATA$,4)

  IF DATE.TIME.VALID(HC.DATE.TIME.BAD$)          \ if date seems to be valid
  THEN BEGIN
    ! convert the GMT to local time for PinPad
    CALL CONVERT.GMT.TO.LOCAL.TIME(HC.DATE.TIME.BAD$,        \
                                   WE.GMT.LOCAL.DIFF,        \
                                   WE.DAYLIGHT.SAVINGS)
  ENDIF                                          ! if date seems to be valid

  ! call API to block the card
  CALL WICBlockCard(STATE$, HC.DATE.TIME.BAD$, HC.REASON.CODE$, ERR.CODE$)

  ERR.CODE = TRANSLATED.ERRCODE(ERR.CODE$)

  ! If the return code is "Card has been reauthenticated after date of hot card
  ! list" (0x008b), or "Card not locked because date is future and reason not
  ! immediate" (0x009e), then log an 81 exception string but continue with WIC
  ! transaction.  Else the card has been blocked so we prompt guidance and this
  ! is not a WIC transaction.
  IF (ERR.CODE = 008BH) OR                       \ Card has been reauthenticated
     (ERR.CODE = 009EH)                          \ Card not locked because date
  THEN BEGIN                                     ! if PinPad did not block card

    ! We log the error for auditing purposes, but we shouldn't stop the WIC EBT
    ! transaction
    CALL WE.LOG.TLOG.ERROR(STATE$,PAN$,ERR.CODE$,ERR.CODE)
    ERR.CODE$ = "0000"                           ! set up that this card is ok

  ENDIF ELSE BEGIN                               ! else PinPad blocked card
    IF (ERR.CODE$ = "0000") THEN BEGIN           ! if Card successfully blocked
      ERR.CODE$ = "009C"                         ! change to Hot Card blocked
    ENDIF                                        ! if Card successfully blocked
  ENDIF                                          ! if PinPad did not block card

END SUB                                          ! end CHECK.HOT.CARD.FILE
!*******************************************************************************


!*******************************************************************************
! Function Name: GET.PRINT.BUFF.ITEM.DESC$
! Description: This function returns the item descriptor from the 4610 CR print
!              buffer, CR.PRINT.BUFFER$. The Tlog index from the item is passed
!              to this method.
!*******************************************************************************
FUNCTION GET.PRINT.BUFF.ITEM.DESC$(TLOG.INDEX)

  STRING                               \
         GET.PRINT.BUFF.ITEM.DESC$     ! function return

  INTEGER*2                            \
            TLOG.INDEX                 ! Index into Tlog for print buffer

  GET.PRINT.BUFF.ITEM.DESC$ = ""                 ! initialize for no return

  IF (TLOG.INDEX <= 0) THEN BEGIN                ! if invalid data passed in
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid data passed in

  ! put item print line to work variable
  A$ = CR.PRINT.BUFFER$(TLOG.CR.4610.INDEX(TLOG.INDEX))

  IF (LEN(A$) < 30) THEN BEGIN                   ! if invalid data
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid data

  A$ = LEFT$(RIGHT$(A$,30),18)                   ! extract desc from print buff

  GET.PRINT.BUFF.ITEM.DESC$ = A$                 ! return descriptor of item

  A$ = ""                                        ! cleanup

END FUNCTION                                     ! end GET.PRINT.BUFF.ITEM.DESC$
!*******************************************************************************


!*******************************************************************************
! Function Name: FORMAT.UNIT.QTY$
! Description: This function returns the unit quantity with the decimal place.
!              It also formats the numbers to line up on the receipt.
!*******************************************************************************
FUNCTION FORMAT.UNIT.QTY$(UNIT.QTY$)

  STRING                               \
         UNIT.QTY$,                    \ Quantity that needs formatting
         FORMAT.UNIT.QTY$              ! function return

  ! make sure string is correct length
  UNIT.QTY$ = RIGHT$(STRING$(5,"0") + UNIT.QTY$,5)

  IF (RIGHT$(UNIT.QTY$,2) = "00") THEN BEGIN      ! if no decimal position
    ! just whole amount, blank out decimal position and two fractional parts
    UNIT.QTY$ = LEFT$(UNIT.QTY$,3) + STRING$(3," ")
  ENDIF ELSE BEGIN                               ! else value in decimal
    UNIT.QTY$ = LEFT$(UNIT.QTY$,3)               \ whole amount
                  + "."                          \ decimal position
                  + RIGHT$(UNIT.QTY$,2)          ! fractional part
    IF (RIGHT$(UNIT.QTY$,1) = "0") THEN BEGIN    ! if ending zero
       ! replace ending zero with blank
       CALL SUBSTR(UNIT.QTY$,LEN(UNIT.QTY$) - 1," ",0,1)
    ENDIF                                        ! if ending zero
  ENDIF                                          ! if no decimal position

  ! strip leading zeroes
  WHILE (LEFT$(UNIT.QTY$,1) = "0")               ! while leading zero in string
    UNIT.QTY$ = RIGHT$(UNIT.QTY$,LEN(UNIT.QTY$) - 1)
  WEND                                           ! while leading zero in string

  IF (UNIT.QTY$ = "") OR                         \ if nothing left in string
     (LEFT$(UNIT.QTY$,1) = ".")                  \ or we need a leading zero
  THEN BEGIN                                     ! if we need to add a zero
    UNIT.QTY$ = "0" + UNIT.QTY$
  ENDIF                                          ! if we need to add a zero

  IF (UNIT.QTY$ = "0") THEN BEGIN                ! if nothing left
    ! format to not line up with decimal position
    UNIT.QTY$ = "0   "
  ENDIF                                          ! if nothing left

  ! Format to 6 places with leading blanks to force it to line up on the receipt
  UNIT.QTY$ = RIGHT$(STRING$(6," ") + UNIT.QTY$,6)

  FORMAT.UNIT.QTY$ = UNIT.QTY$                   ! returned formatted quantity

END FUNCTION                                     ! end FORMAT.UNIT.QTY$
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: BUILD.PRESCRIPTION.DATA
! Description: This routine builds the benefit prescription data for printing
!              on the benefit receipt.
!*******************************************************************************
SUB BUILD.PRESCRIPTION.DATA(STATE$,WIC.ITEMS$)

  STRING                               \
         DF.FMT$,                      \ format for reading descriptor file
         DF.KEY$,                      \ key of descriptor file
         DESCRIPTOR.FILE$,             \ WIC EBT file for benefit descriptors
         STATE$,                       \ State code returned from API
         WIC.ITEMS$                    ! WIC'able items from prescription

  INTEGER*1                            \
            NO.ERROR                   ! flag if an error occurs

  INTEGER*2                            \
            DF.RECL,                   \ record length of descriptor file
            WE.I%,WE.K%                ! work variables

  INTEGER*4                            \
            WE.X%,                     \ work variable
            CURR.FULL.CAT,             \ current full category in int form
            PREV.FULL.CAT              ! previous full category in in form

  WE.NUM.CATEGORIES = 0                          ! initialize count variable
  NO.ERROR = 0                                   ! reset error flag

  ON ERROR GOTO BUILD.PRESCRIPTION.ERR

  ! Calculate how many are in each record
  WE.NUM.CATEGORIES = INT%(LEN(WIC.ITEMS$) / WE.LEN.WIC.DATA)

  IF (WE.NUM.CATEGORIES <= 0) THEN BEGIN
    EXIT SUB
  ENDIF

  CLOSE WE.SESSION.NUM1                          ! make sure sess number is free

  DESCRIPTOR.FILE$ = "R::WIC1:EAMWE" + STATE$ + ".DES"
  DF.RECL = 46
  DF.FMT$ = "C3,C10,C33"

  OPEN DESCRIPTOR.FILE$ KEYED RECL DF.RECL AS WE.SESSION.NUM1 NOWRITE NODEL

  DIM WE.CATEGORY$(0)
  DIM WE.CATEGORY.QTY$(0)
  DIM WE.CATEGORY.UPD.QTY$(0)
  DIM WE.CATEGORY.UNIT$(0)
  DIM WE.CATEGORY.DESC$(0)

  DIM WE.CATEGORY$(WE.NUM.CATEGORIES)
  DIM WE.CATEGORY.QTY$(WE.NUM.CATEGORIES)
  DIM WE.CATEGORY.UPD.QTY$(WE.NUM.CATEGORIES)
  DIM WE.CATEGORY.UNIT$(WE.NUM.CATEGORIES)
  DIM WE.CATEGORY.DESC$(WE.NUM.CATEGORIES)

  WE.K% = 1                                      ! use for parsing through data

  FOR WE.I% = 1 TO WE.NUM.CATEGORIES

    A$ = MID$(WIC.ITEMS$,WE.K%,10)               ! get category record

    ! test if data is numeric

    NO.ERROR = -1

    WE.X% = VAL(LEFT$(A$,5))                     ! test that cat is numeric

    IF NO.ERROR THEN BEGIN                       ! if no error on VAL statement
      WE.CATEGORY$(WE.I%) = LEFT$(A$,5)          ! save category just in case
      B$ = RIGHT$(A$,5)                          ! get quantity field
      WE.X% = VAL(B$)                            ! test that quantity is numeric
      IF NO.ERROR THEN BEGIN                     ! if no error on quantity
        ! get quantity of prescription
        WE.CATEGORY.UPD.QTY$(WE.I%) = B$
        WE.CATEGORY.QTY$(WE.I%) = FORMAT.UNIT.QTY$(B$)
      ENDIF                                      ! if no error on quantity
    ENDIF ELSE BEGIN
      WE.CATEGORY$(WE.I%) = ""                   ! set to null
    ENDIF                                        ! if no error on VAL statement

    ! if either category or quantity is bad, then reset quantity data
    IF (NO.ERROR = 0) THEN BEGIN                 ! if error processing field
      WE.CATEGORY.QTY$(WE.I%) = FORMAT.UNIT.QTY$("0")
      WE.CATEGORY.UPD.QTY$(WE.I%) = "0"
    ENDIF                                        ! if error processing field

    ! if no error so far, then data is numeric.  Read file
    IF NO.ERROR THEN BEGIN                       ! if no error so far

      DF.KEY$ = PACK$(RIGHT$(STRING$(6,"0") + LEFT$(A$,5),6))

      READ FORM DF.FMT$; #WE.SESSION.NUM1 KEY DF.KEY$; B$,C$,D$

      IF NO.ERROR THEN BEGIN                     ! if no error on Read
        CURR.FULL.CAT = VAL(RIGHT$(UNPACK$(B$),5)) ! test if data seems valid
      ENDIF                                      ! if no error on Read

    ENDIF                                        ! if no error so far

    IF NO.ERROR THEN BEGIN                       ! if no problems getting data
      NO.ERROR = 0                               ! reset error flag
      WE.CATEGORY$(WE.I%) = RIGHT$(UNPACK$(B$),5)       ! save cat/subcategory
      WE.CATEGORY.UNIT$(WE.I%) = STRIP.BLANKS$(C$)      ! get unit descriptor
      WE.CATEGORY.DESC$(WE.I%) = STRIP.BLANKS$(D$)      ! get cat descriptor

    ENDIF ELSE BEGIN                             ! else problems getting data
      ! If we've had problems along the way, set up dummy data.
      ! We still want to print the quantity that was on the customer card
      CURR.FULL.CAT = 0
      WE.CATEGORY.UNIT$(WE.I%) = CAT.DESC.NOT.FOUND$
      WE.CATEGORY.DESC$(WE.I%) = " "
    ENDIF                                        ! if no problems getting data

    GOSUB SORT.CATEGORY.DATA                     ! sort the data

    WE.K% = WE.K% + WE.LEN.WIC.DATA              ! bump up for next read

  NEXT WE.I%

  ! Now build structures to hold the original data for these categories.
  ! We'll use these for printing the total category list on our benefit receipt.
  ! The ones just processed will be modified while collecting matching items.
  WE.NUM.ORG.CATEGORIES = WE.NUM.CATEGORIES
  DIM WE.ORG.CATEGORY.QTY$(WE.NUM.ORG.CATEGORIES)
  DIM WE.ORG.CATEGORY.UNIT$(WE.NUM.ORG.CATEGORIES)
  DIM WE.ORG.CATEGORY.DESC$(WE.NUM.ORG.CATEGORIES)
  FOR WE.I% = 1 TO WE.NUM.ORG.CATEGORIES         ! for all original categories
    WE.ORG.CATEGORY.QTY$(WE.I%) = WE.CATEGORY.QTY$(WE.I%)
    WE.ORG.CATEGORY.UNIT$(WE.I%) = WE.CATEGORY.UNIT$(WE.I%)
    WE.ORG.CATEGORY.DESC$(WE.I%) = WE.CATEGORY.DESC$(WE.I%)
  NEXT WE.I%                                     ! for all original categories

BUILD.PRESCRIPTION.EXIT:
  CLOSE WE.SESSION.NUM1                          ! Free up session number
  A$ = ""                                        ! cleanup
  B$ = ""
  C$ = ""
  D$ = ""
  E$ = ""
  EXIT SUB

SORT.CATEGORY.DATA:                              ! subroutine to sort the data

  ! We want to sort in order of largest to smallest so we will look at zero
  ! subcategories last when searching for item matches.

  FOR I% = (WE.I% - 1) TO 1 STEP -1
    PREV.FULL.CAT = VAL(WE.CATEGORY$(I%))        ! get category id from previous
    IF (PREV.FULL.CAT < CURR.FULL.CAT)           \ if prev is smaller then swap
    THEN BEGIN
      E$ = WE.CATEGORY.QTY$(I%)
      WE.CATEGORY.QTY$(I%) = WE.CATEGORY.QTY$(I% + 1)
      WE.CATEGORY.QTY$(I% + 1) = E$
      E$ = WE.CATEGORY.UPD.QTY$(I%)
      WE.CATEGORY.UPD.QTY$(I%) = WE.CATEGORY.UPD.QTY$(I% + 1)
      WE.CATEGORY.UPD.QTY$(I% + 1) = E$
      E$ = WE.CATEGORY$(I%)
      WE.CATEGORY$(I%) = WE.CATEGORY$(I% + 1)
      WE.CATEGORY$(I% + 1) = E$
      E$ = WE.CATEGORY.UNIT$(I%)
      WE.CATEGORY.UNIT$(I%) = WE.CATEGORY.UNIT$(I% + 1)
      WE.CATEGORY.UNIT$(I% + 1) = E$
      E$ = WE.CATEGORY.DESC$(I%)
      WE.CATEGORY.DESC$(I%) = WE.CATEGORY.DESC$(I% + 1)
      WE.CATEGORY.DESC$(I% + 1) = E$
    ENDIF                                        ! if prev is smaller then swap
  NEXT I%

  RETURN

BUILD.PRESCRIPTION.ERR:

  WE.ERR$ = ERR
  WE.ERRN$ = ERRNHEX$                            ! Convert error code

  IF (WE.ERR$ = "CU") THEN BEGIN                 ! Tried to close a session
    RESUME                                       ! number that is not active
  ENDIF

  IF NO.ERROR THEN BEGIN                         ! if flagged set to test if err
    NO.ERROR = 0                                 ! flag we had an error
    RESUME                                       ! go back
  ENDIF                                          ! if flagged set to test if err

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                "WEds" + WE.ERR$ + WE.ERRN$)     ! log unique message

  WE.NUM.CATEGORIES = 0                          ! set so an error will occur

  RESUME BUILD.PRESCRIPTION.EXIT                 ! always resume from on error

END SUB                                          ! end BUILD.PRESCRIPTION.DATA
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.REMOVE.ITEM.ENTRY
! Description: This routine shifts all the item entry array elements down to
!              remove the indexed.
!*******************************************************************************
SUB WE.REMOVE.ITEM.ENTRY(INDEX%)

  INTEGER*2                            \
            INDEX%,                    \ index of entry we need to remove
            WE.I%                      ! work variable

  IF (INDEX% > WE.ITEM.COUNT) OR                 \ if invalid entry
     (WE.ITEM.COUNT <= 0)                        \
  THEN BEGIN
    EXIT SUB
  ENDIF                                          ! if invalid entry

  ! shift the entries down
  FOR WE.I% = INDEX% TO (WE.ITEM.COUNT - 1)
    WE.ITEM.TLOG.INDEX(WE.I%) = WE.ITEM.TLOG.INDEX(WE.I% + 1)
    WE.ITEM.USED(WE.I%) = WE.ITEM.USED(WE.I% + 1)
    WE.ITEM.FULL.CATEGORY$(WE.I%) = WE.ITEM.FULL.CATEGORY$(WE.I% + 1)
    WE.ITEM.BENEFIT.QTY(WE.I%) = WE.ITEM.BENEFIT.QTY(WE.I% + 1)
    WE.ITEM.N.PURCH.QTY(WE.I%) = WE.ITEM.N.PURCH.QTY(WE.I% + 1)
    WE.ITEM.0.PURCH.QTY(WE.I%) = WE.ITEM.0.PURCH.QTY(WE.I% + 1)
    WE.ITEM.N.COST(WE.I%) = WE.ITEM.N.COST(WE.I% + 1)
    WE.ITEM.0.COST(WE.I%) = WE.ITEM.0.COST(WE.I% + 1)
    WE.ITEM.CODE$(WE.I%) = WE.ITEM.CODE$(WE.I% + 1)
    WE.ITEM.UNIT.PRICES$(WE.I%) = WE.ITEM.UNIT.PRICES$(WE.I% + 1)
!   WE.ITEM.PRICE(WE.I%) = WE.ITEM.PRICE(WE.I% + 1)   IO01214
    WE.ITEM.0.CHILD.ARRAY$(WE.I%) = WE.ITEM.0.CHILD.ARRAY$(WE.I% + 1)
    WE.ITEM.N.CHILD.ARRAY$(WE.I%) = WE.ITEM.N.CHILD.ARRAY$(WE.I% + 1)
    WE.ITEM.INDICAT1(WE.I%) = WE.ITEM.INDICAT1(WE.I% + 1)
    WE.ITEM.INDICAT2(WE.I%) = WE.ITEM.INDICAT2(WE.I% + 1)
    WE.ITEM.0.RX.INDEX(WE.I%) = WE.ITEM.0.RX.INDEX(WE.I% + 1)
    WE.ITEM.N.RX.INDEX(WE.I%) = WE.ITEM.N.RX.INDEX(WE.I% + 1)
    WE.ITEM.XPRICE(WE.I%) = WE.ITEM.XPRICE(WE.I% + 1)
    WE.ITEM.DEALQTY(WE.I%) = WE.ITEM.DEALQTY(WE.I% + 1)
    WE.ITEM.QUANTITY(WE.I%) = WE.ITEM.QUANTITY(WE.I% + 1)
    WE.ITEM.UNIT.DESC$(WE.I%) = WE.ITEM.UNIT.DESC$(WE.I% + 1)
    WE.ITEM.APL.FILE.DATA$(WE.I%) = WE.ITEM.APL.FILE.DATA$(WE.I% + 1)
  NEXT WE.I%

  WE.ITEM.TLOG.INDEX(WE.ITEM.COUNT) = 0          ! set instance invalid
  WE.ITEM.USED(WE.ITEM.COUNT) = 0                ! set instance invalid
  WE.ITEM.QUANTITY(WE.ITEM.COUNT) = 0            ! make sure quantity is reset
  WE.ITEM.FULL.CATEGORY$(WE.ITEM.COUNT) = ""     ! reset item category
  WE.ITEM.CODE$(WE.ITEM.COUNT) = ""              ! reset item code
  WE.ITEM.UNIT.PRICES$(WE.ITEM.COUNT) = ""       ! reset the item prices
  WE.ITEM.0.CHILD.ARRAY$(WE.ITEM.COUNT) = ""     ! reset the zero children array
  WE.ITEM.N.CHILD.ARRAY$(WE.ITEM.COUNT) = ""     ! reset nonzero children array
  WE.ITEM.UNIT.DESC$(WE.ITEM.COUNT) = ""         ! reset item's unit descriptor
  WE.ITEM.APL.FILE.DATA$(WE.ITEM.COUNT) = ""     ! reset UPC/PLU data
  WE.ITEM.COUNT = WE.ITEM.COUNT - 1              ! reduce our count
  INDEX% = INDEX% - 1                            ! reduce the index for the loop

END SUB                                          ! end WE.REMOVE.ITEM.ENTRY
!*******************************************************************************


!*******************************************************************************
! Function Name: WE.TOTAL.COUPON.AMOUNT
! Description: This function accumulates the total coupon amount against this
!              item entry.
!              This is based on the EM item/coupon association arrays.
!*******************************************************************************
FUNCTION WE.TOTAL.COUPON.AMOUNT(ITEM.INDEX)

  INTEGER*2                            \
            WE.I%,                     \ local work variable
            ITEM.INDEX                 ! index in the item arrays

  INTEGER*4                            \
            CURR.ITEM.UPC,             \ last 8 digits of current item upc
            TOTAL.CPN.AMOUNT,          \ accumulation of the total coupon amount
            WE.TOTAL.COUPON.AMOUNT     ! function return

  WE.TOTAL.COUPON.AMOUNT = 0                     ! assume there are no coupons

  IF (WEBT.SAVE <= 0) THEN BEGIN                 ! if no item/cpn association
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if no item/cpn association

  TS.ER.RETURN = -1                              ! set flag in VAL error

  ! Get last 8 digits of item code to compare with EM item/coupon association
  CURR.ITEM.UPC = VAL(RIGHT$(UNPACK$(WE.ITEM.CODE$(ITEM.INDEX)),8))

  IF (TS.ER.RETURN = 0) THEN BEGIN               ! if invalid item code
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid item code

  TS.ER.RETURN = 0                               ! make sure base flag is reset

  TOTAL.CPN.AMOUNT = 0                           ! initialize coupon amount

  ! Now go through and accumulate the coupon totals for this item
  FOR WE.I% = 1 TO WEBT.SAVE                     ! for all coupon associations
    IF (WEBT.IOFF(WE.I%) = WE.ITEM.TLOG.INDEX(ITEM.INDEX))  \
    THEN BEGIN                                   ! if a tlog array index match
      IF (WEBT.IUPC(WE.I%) = CURR.ITEM.UPC)      \ if UPC's match
      THEN BEGIN
        ! accumulate coupon totals
        TOTAL.CPN.AMOUNT = TOTAL.CPN.AMOUNT + WEBT.CVAL(WE.I%)
      ENDIF                                      ! if UPC's match
    ENDIF                                        ! if a tlog array index match
  NEXT WE.I%                                     ! for all coupon associations

  WE.TOTAL.COUPON.AMOUNT = TOTAL.CPN.AMOUNT      ! return accumulated amounts

END FUNCTION                                     ! end WE.TOTAL.COUPON.AMOUNT
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: GET.POSSIBLE.WIC.ITEMS
! Description: This routine scans the transaction looking for non-voided items.
!              The function in this routine is patterned after subroutine
!              EXTRACT.DATA in EAMTSTPC.BAS.
!              Once all the items have been scanned, then go through the
!              collected items and try to toss out ones we might not use as WIC
!              items, (i.e. BOGO's, negative price due to deal, etc.)
!              Make sure all the items are collected from the Tlog first before
!              removing possible non-wic entries to make sure we compare the
!              voids with their entries.
!*******************************************************************************
SUB GET.POSSIBLE.WIC.ITEMS

  STRING                               \
         TLOG.STRING$,                 \ string read from Tlog
         ITEMCODE$,                    \ item code from string
         SAVE.ITEMCODE$,               \ work string for item code
         PRICING.DATA$,                \ pricing data from item rec ext string
         VOID.ITEMCODE$,               \ voided item's itemcode
         VOID.PRICING.DATA$,           \ voided item's pricing data
         WE.A$,WE.B$                   ! work variables

  INTEGER*1                            \
            NOT.WIC.ITEM               ! flag if item is not a possible WIC item

  INTEGER*2                            \
            DEPARTME,                  \ department number
            INDICAT1,                  \ indicator one
            INDICAT2,                  \ indicator two
            ITEMTYPE,                  \ indicat3 item type
            PM,                        \ pricing method
            IE.INDICAT1,               \ indicator 1 from item extension string
            VOID.DEPARTME,             \ voided item's department number
            VOID.INDICAT1,             \ voided item's indicator one
            VOID.INDICAT2,             \ voided item's indicator two
            VOID.ITEMTYPE,             \ voided item's indicat3 item type
            VOID.IE.INDICAT1,          \ voided item's indicator one from ext
            VOID.TOTAL,                \ how many of the curr item we must void
            START,                     \ used for positioning in string
            POINTER,                   \ pointer when parsing strings
            CURR.INDEX%,               \ index when parsing data
            INDEX%                     ! index into tlog array

  INTEGER*4                            \
            XPRICE,                    \ extended price for item
            DEALQTY,                   \ deal quantity of item
            SALEQUAN,                  \ sale quantity
            PRICE1,                    \ price one from extension
            PRICE2,                    \ price two from extension
            QTYORWGT,                  \ quantity or weight for item
            QUANTITY,                  \ quantity of item
            VOID.XPRICE,               \ voided item's extended price for item
            VOID.DEALQTY,              \ voided item's deal quantity
            VOID.QTYORWGT,             \ voided item's quantity/weight for item
            VOID.QUANTITY,             \ voided item's total quantity of item
            RESULT,                    \ subroutine call results
            WE.X%, WE.Y%               ! work variables

  WE.ITEM.COUNT = 0
  DIM WE.ITEM.TLOG.INDEX(SL.END)
  DIM WE.ITEM.USED(SL.END)
  DIM WE.ITEM.FULL.CATEGORY$(SL.END)
  DIM WE.ITEM.BENEFIT.QTY(SL.END)
  DIM WE.ITEM.N.PURCH.QTY(SL.END)
  DIM WE.ITEM.0.PURCH.QTY(SL.END)
  DIM WE.ITEM.N.COST(SL.END)
  DIM WE.ITEM.0.COST(SL.END)
  DIM WE.ITEM.CODE$(SL.END)
  DIM WE.ITEM.UNIT.PRICES$(SL.END)
! DIM WE.ITEM.PRICE(SL.END)  IO01214
  DIM WE.ITEM.0.CHILD.ARRAY$(SL.END)
  DIM WE.ITEM.N.CHILD.ARRAY$(SL.END)
  DIM WE.ITEM.INDICAT1(SL.END)
  DIM WE.ITEM.INDICAT2(SL.END)
  DIM WE.ITEM.0.RX.INDEX(SL.END)
  DIM WE.ITEM.N.RX.INDEX(SL.END)
  DIM WE.ITEM.XPRICE(SL.END)
  DIM WE.ITEM.DEALQTY(SL.END)
  DIM WE.ITEM.QUANTITY(SL.END)
  DIM WE.ITEM.UNIT.DESC$(SL.END)
  DIM WE.ITEM.APL.FILE.DATA$(SL.END)

  FOR INDEX% = 1 TO SL.END                       ! for all the entries
    TLOG.STRING$ = READ.SL.STR$(INDEX%) + ":"    ! read entry
    IF (LEN(TLOG.STRING$) > 7) THEN BEGIN        ! if string seems valid
      K% = ASC(TLOG.STRING$)                     ! get string type
      IF (K% = 1) THEN BEGIN                     ! if item string
        CURR.INDEX% = INDEX%                     ! set current index
        GOSUB EXTRACT.ITEM.DATA                  ! dump tlog data into work vars
        IF (ITEMCODE$ <> "") THEN BEGIN          ! if item code returned
          IF ((ITEMTYPE = 0) OR (ITEMTYPE = 4))  \ if normal/misc item sale
             AND (INDICAT1 AND 0008H)            \ and this is a foodstamp item
          THEN BEGIN                             ! if valid WIC item type
            IF (INDICAT2 AND 0080H) THEN BEGIN   ! if a void
              GOSUB HANDLE.VOIDS                 ! try to handle the voids
            ENDIF ELSE BEGIN                     ! else not a void
              WE.ITEM.COUNT = WE.ITEM.COUNT + 1

              ! save index of item entry in Tlog
              WE.ITEM.TLOG.INDEX(WE.ITEM.COUNT) = INDEX%

              ! save item code after packing and padding it to UPC file specs
              WE.ITEM.CODE$(WE.ITEM.COUNT) = PACK$(RIGHT$(STRING$(14,"0")  \
                                           + ITEMCODE$,14))

              ! save item data
              WE.ITEM.INDICAT1(WE.ITEM.COUNT) = INDICAT1
              WE.ITEM.INDICAT2(WE.ITEM.COUNT) = INDICAT2
              WE.ITEM.XPRICE(WE.ITEM.COUNT) = XPRICE
              WE.ITEM.DEALQTY(WE.ITEM.COUNT) = DEALQTY
              WE.ITEM.QUANTITY(WE.ITEM.COUNT) = QUANTITY
              IF (QUANTITY > 1) THEN BEGIN       ! if a quantity on this item
                GOSUB BUILD.UNIT.PRICES          ! build prices of the quantity
              ENDIF ELSE BEGIN                   ! else only a quantity of one
                WE.ITEM.UNIT.PRICES$(WE.ITEM.COUNT) = STR$(XPRICE) + ":"
                WE.ITEM.0.CHILD.ARRAY$(WE.ITEM.COUNT) = "0"
                WE.ITEM.N.CHILD.ARRAY$(WE.ITEM.COUNT) = "0"
              ENDIF                              ! if a quantity on this item
            ENDIF                                ! if a void
          ENDIF                                  ! if valid WIC item type
        ENDIF                                    ! if item code returned
      ENDIF                                      ! if item string
    ENDIF                                        ! if string seems valid
  NEXT INDEX%                                    ! for all the entries

  ! now go through and try to toss out any entries we may not use as wic'able
  FOR INDEX% = 1 TO WE.ITEM.COUNT
    NOT.WIC.ITEM = -1                            ! assume it will be invalid
    IF (WE.ITEM.TLOG.INDEX(INDEX%) > 0)          \ if it seems to be valid
    THEN BEGIN
      IF ((WE.ITEM.INDICAT2(INDEX%) AND 0004H) = 0) \
      THEN BEGIN                                 ! if not neg price due to deal
        IF (WE.ITEM.XPRICE(INDEX%) > 0) AND                           \
           (WE.TOTAL.COUPON.AMOUNT(INDEX%) < WE.ITEM.XPRICE(INDEX%))  \
        THEN BEGIN                               ! if not a free item
          NOT.WIC.ITEM = 0                       ! this is a possible WIC item
        ENDIF                                    ! if not a free item
      ENDIF                                      ! if not neg price due to deal
    ENDIF                                        ! if it seems to be valid
    IF NOT.WIC.ITEM THEN BEGIN                   ! if not possible wic item
      CALL WE.REMOVE.ITEM.ENTRY(INDEX%)          ! remove the record
    ENDIF                                        ! if not possible wic item
  NEXT INDEX%

  TLOG.STRING$ = ""                              ! cleanup
  ITEMCODE$ = ""
  SAVE.ITEMCODE$ = ""
  PRICING.DATA$ = ""
  VOID.ITEMCODE$ = ""
  VOID.PRICING.DATA$ = ""
  WE.A$ = ""
  A$ = ""
  B$ = ""
  C$ = ""
  EXIT SUB                                       ! get out


BUILD.UNIT.PRICES:

  GOSUB SPLIT.PRICING.DATA                       ! split up the pricing data

  WE.A$ = ""
  WE.B$ = ""
  B$ = ""

  ! First go through and figure out this entries individual unit price
  GOSUB CALCULATE.SINGLE.UNIT.PRICE

  ! Now collect all the unit prices in this entry
  IF (PM = 0) THEN BEGIN                         ! if pricing method 0 item
    B$ = STR$(XPRICE / QUANTITY) + ":"
    FOR WE.X% = 1 TO QUANTITY
      WE.A$ = WE.A$ + B$
      WE.B$ = WE.B$ + "0"
    NEXT WE.X%
  ENDIF ELSE BEGIN                               ! else not a pricing method 0
    SL.IE.METHOD = PM                            ! put in base variables
    SL.IE.SALEQUAN = SALEQUAN
    SL.IE.SALEPRIC = PRICE1
    SL.IE.SALEPRIC2 = PRICE2

    IF (DEALQTY < 1) THEN BEGIN                  ! if no valid deal quantity

      WE.A$ = STR$(XPRICE) + ":"                 ! just save the item price
      WE.B$ = "0"

    ENDIF ELSE BEGIN                             ! else we have a deal quantity

      FOR WE.X% = 1 TO QUANTITY                  ! for each item in this entry
        DEALQTY = DEALQTY - 1                    ! reduce to get next unit price
        IF (DEALQTY >= 0) THEN BEGIN             ! if valid deal quantity
          ! go through and calculate each unit's price
          WE.Y% = CALC.TOTAL.MULTIPRICE(DEALQTY + 1) -   \
                  CALC.TOTAL.MULTIPRICE(DEALQTY)
          WE.A$ = WE.A$ + STR$(WE.Y%) + ":"      ! save that price
          WE.B$ = WE.B$ + "0"                    ! save bit position
        ENDIF ELSE BEGIN                         ! else not valid deal quantity
          WE.X% = QUANTITY                       ! break loop
        ENDIF                                    ! if valid deal quantity
      NEXT WE.X%                                 ! for each item in this entry

    ENDIF                                        ! if no valid deal quantity

    SL.IE.METHOD = 0                             ! reset base variables
    SL.IE.SALEQUAN = 0
    SL.IE.SALEPRIC = 0
    SL.IE.SALEPRIC2 = 0
  ENDIF                                          ! if pricing method 0 item

  WE.ITEM.UNIT.PRICES$(WE.ITEM.COUNT) = WE.A$    ! save all the unit prices
  WE.ITEM.0.CHILD.ARRAY$(WE.ITEM.COUNT) = WE.B$  ! save zero children array
  WE.ITEM.N.CHILD.ARRAY$(WE.ITEM.COUNT) = WE.B$  ! save nonzero children array

  RETURN                                         ! BUILD.UNIT.PRICES:


HANDLE.VOIDS:

  VOID.ITEMCODE$ = ITEMCODE$                     ! save the voided item data
  VOID.PRICING.DATA$ = PRICING.DATA$
  VOID.DEPARTME = DEPARTME
  VOID.INDICAT1 = INDICAT1
  VOID.INDICAT2 = INDICAT2
  VOID.ITEMTYPE = ITEMTYPE
  VOID.IE.INDICAT1 = IE.INDICAT1
  VOID.XPRICE = XPRICE
  VOID.DEALQTY = DEALQTY
  VOID.QTYORWGT = QTYORWGT
  VOID.QUANTITY = QUANTITY

  FOR K% = WE.ITEM.COUNT TO 1 STEP -1            ! loop through previous entries
    IF (WE.ITEM.TLOG.INDEX(K%) > 0) THEN BEGIN   ! if valid Tlog entry
      TLOG.STRING$ = READ.SL.STR$(WE.ITEM.TLOG.INDEX(K%)) + ":"
      CURR.INDEX% = WE.ITEM.TLOG.INDEX(K%)
      GOSUB EXTRACT.ITEM.DATA
      IF (ITEMCODE$ = VOID.ITEMCODE$)            \ if itemcodes match
      THEN BEGIN
        IF (ITEMTYPE = VOID.ITEMTYPE) AND        \ if item type/department match
           (DEPARTME = VOID.DEPARTME)            \
        THEN BEGIN
          IF (INDICAT1 AND 40FAH) =              \
             (VOID.INDICAT1 AND 40FAH)           \
          THEN BEGIN                             ! if needed flags match

            XPRICE = WE.ITEM.XPRICE(K%)          ! replace xprice with saved
            DEALQTY = WE.ITEM.DEALQTY(K%)        ! replace deal quantity
            QUANTITY = WE.ITEM.QUANTITY(K%)      ! replace quantity with saved
            INDICAT2 = WE.ITEM.INDICAT2(K%)      ! replace indicat2 with saved

            IF (INDICAT2 AND 0004H) THEN BEGIN   ! if negative price due to deal
              XPRICE = -XPRICE                   ! negate the extended price
            ENDIF                                ! if negative price due to deal

            IF (VOID.INDICAT2 AND 0004H)         \ if negative price due to deal
            THEN BEGIN
              VOID.XPRICE = -VOID.XPRICE         ! negate the extended price
            ENDIF                                ! if negative price due to deal

            IF (INDICAT1 AND 04000H) THEN BEGIN  ! if weight item
              IF (XPRICE = VOID.XPRICE)          \ if prices match
              THEN BEGIN
                CALL WE.REMOVE.ITEM.ENTRY(K%)    ! wipe out this entry
                RETURN                           ! go back, entire item voided
              ENDIF                              ! if prices match
            ENDIF ELSE BEGIN                     ! else not weight item
              IF ((INDICAT2 AND 0002H) = 0) AND  \ no item extension on item
                 ((VOID.INDICAT2 AND 0002H) = 0) \ no item extension on void
              THEN BEGIN                         ! no item extension on either
                IF (XPRICE = VOID.XPRICE)        \ if prices match
                THEN BEGIN
                  CALL WE.REMOVE.ITEM.ENTRY(K%)  ! wipe out this entry
                  RETURN                         ! go back, entire item voided
                ENDIF                            ! if prices match
              ENDIF ELSE BEGIN                   ! else an item has an item ext

                ! we now know we have non-weight items where at least one has an
                ! item extension string.  This is where we need to determine
                ! quantities and pricing

                IF (XPRICE = VOID.XPRICE) AND    \ extended prices match
                   (QUANTITY = VOID.QUANTITY)    \ quantities match
                THEN BEGIN                       ! if price/quantity match
                  CALL WE.REMOVE.ITEM.ENTRY(K%)  ! wipe out this entry
                  RETURN                         ! entire item was voided
                ENDIF ELSE BEGIN                 ! else price/qty does not match

                  ! Begin subtracting void amount from current item record,
                  ! adjusting each as we go.   If we use up all of the current
                  ! item before finishing with the void, then continue finding
                  ! the rest of the item to void.

                  IF (INDICAT2 AND 0002H)        \
                  THEN BEGIN                     ! if extension on current item
                    IF (QUANTITY > VOID.QUANTITY)  \
                    THEN BEGIN                   ! if more item than voided
                      GOSUB SPLIT.PRICING.DATA   ! split the pricing data
                      WE.ITEM.XPRICE(K%) = WE.ITEM.XPRICE(K%) - VOID.XPRICE
                      WE.ITEM.QUANTITY(K%) = WE.ITEM.QUANTITY(K%)         \
                                           - VOID.QUANTITY
                      IF (PM > 1) AND (SALEQUAN > 1) \
                      THEN BEGIN                 ! if a deal on this item
                        ! replace voided deal quantity with this one
                        WE.ITEM.DEALQTY(K%) = VOID.DEALQTY
                        ! reset work variable for calculating single price
                        DEALQTY = VOID.DEALQTY
                      ENDIF                      ! if a deal on this item
                      IF (WE.ITEM.XPRICE(K%) < 0)  \
                      THEN BEGIN                 ! if negative price
                        ! set "negative price due to deal" flag
                        WE.ITEM.INDICAT2(K%) = WE.ITEM.INDICAT2(K%) OR 0004H
                      ENDIF                      ! if negative price

                      ! go through and recalculate this entries individual unit
                      ! price
                      GOSUB CALCULATE.SINGLE.UNIT.PRICE

                      ! Now remove all voided unit prices, we'll set them to a
                      ! negative one as a flag
                      L% = -1
                      A$ = WE.ITEM.UNIT.PRICES$(K%)
                      B$ = ""
                      C$ = STR$(L%) + ":"
                      L% = QTYORWGT - WE.ITEM.QUANTITY(K%)
                      J% = 0

                      ! Loop through to get position of all voided items
                      FOR I% = 1 TO L%
                        J% = MATCHB(":",A$,J% + 1)
                        IF (J% = 0) THEN BEGIN   ! if no delimiter found
                          I% = L%                ! break loop
                        ENDIF ELSE BEGIN         ! else delimiter found
                          B$ = B$ + C$           ! build negative price string
                        ENDIF                    ! if no delimiter found
                      NEXT I%

                      IF (J% > 0) THEN BEGIN     ! if we have string to replace
                        A$ = RIGHT$(A$,LEN(A$) - J%)
                        A$ = B$ + A$             ! replace with negative prices
                      ENDIF                      ! if we have string to replace

                      ! replace unit prices with voided ones
                      WE.ITEM.UNIT.PRICES$(K%) = A$

                      RETURN                     ! we're done with this void

                    ENDIF ELSE BEGIN             ! else more voided than item

                      VOID.QUANTITY = VOID.QUANTITY - QUANTITY
                      IF (VOID.QUANTITY <= 0)    \ if nothing left of void
                      THEN BEGIN
                        RETURN                   ! we're done
                      ENDIF                      ! if nothing left of void
                      VOID.XPRICE = VOID.XPRICE - XPRICE
                      IF (VOID.XPRICE < 0)       \ if negative price
                      THEN BEGIN
                        VOID.XPRICE = -VOID.XPRICE
                        ! set "negative price due to deal" flag
                        VOID.INDICAT2 = VOID.INDICAT2 OR 0004H
                      ENDIF                      ! if negative price
                      IF (VOID.DEALQTY > DEALQTY) \
                      THEN BEGIN                 ! if a deal on this item
                        VOID.DEALQTY = VOID.DEALQTY - DEALQTY
                      ENDIF                      ! if a deal on this item
                      ! remove voided item
                      CALL WE.REMOVE.ITEM.ENTRY(K%)
                      K% = K% + 1                ! IO00625 reset loop iteration
                    ENDIF                        ! if more item than voided

                  ENDIF ELSE BEGIN               ! else no extension on current

                    ! If no extension on current, then there must be one on the
                    ! voided.  In this case, simply void the current, decrement
                    ! the void, and go back to continue looking for more of the
                    ! item if there is more left to void

                    ! wipe out this entry
                    CALL WE.REMOVE.ITEM.ENTRY(K%)
                    K% = K% + 1                  ! IO00625 reset loop iteration

                    VOID.QUANTITY = VOID.QUANTITY - 1
                    IF (VOID.QUANTITY <= 0)      \ if no quantity left
                    THEN BEGIN
                      RETURN                     ! we're done
                    ENDIF                        ! if no quantity left

                    ! remove extended price from voided
                    VOID.XPRICE = VOID.XPRICE - XPRICE

                    IF (VOID.XPRICE < 0)         \ if negative extended price
                    THEN BEGIN
                      VOID.XPRICE = -VOID.XPRICE ! make positive
                      ! negative price due to deal
                      VOID.INDICAT2 = VOID.INDICAT2 OR 0004H
                    ENDIF                        ! if negative extended price
                  ENDIF                          ! if extension on current item
                ENDIF                            ! if price/quantity match
              ENDIF                              ! no item extension on either
            ENDIF                                ! if weight item
          ENDIF                                  ! if needed flags match
        ENDIF                                    ! if item type/department match
      ENDIF                                      ! if itemcodes match
    ENDIF                                        ! if valid Tlog Entry
  NEXT K%                                        ! loop through previous entries

  RETURN                                         ! return from HANDLE.VOIDS:


CALCULATE.SINGLE.UNIT.PRICE:

  ! NOTE: subroutine SPLIT.PRICING.DATA must be called before this method.
  !       The unit price is returned in local work variable WE.X%.

  WE.X% = 0                                      ! initialize return variable
  IF (PM < 2) THEN BEGIN                         ! if pricing method 0 or 1
    IF (SALEQUAN > 1) THEN BEGIN                 ! if a sale quantity on item
      WE.X% = INT%(PRICE1 / SALEQUAN)            ! we'll just round down
    ENDIF ELSE BEGIN                             ! else no sale quantity on item
      WE.X% = PRICE1                             ! save price as it is
    ENDIF                                        ! if a sale quantity on item
  ENDIF ELSE BEGIN                               ! else not pricing meth 0 or 1
    IF (PM = 2) THEN BEGIN                       ! if pricing method 2
      WE.X% = PRICE2                             ! just save unit price
    ENDIF ELSE BEGIN                             ! else pricing method 3 o 4
      IF (DEALQTY >= SALEQUAN) THEN BEGIN        ! if we've reached threshold
        WE.X% = PRICE2                           ! save deal price
      ENDIF ELSE BEGIN                           ! else not reached threshold
        WE.X% = PRICE1                           ! save unit price
      ENDIF                                      ! if we've reached threshold
    ENDIF                                        ! if pricing method 2
  ENDIF                                          ! if pricing method 0 or 1

  RETURN                                         ! CALCULATE.SINGLE.UNIT.PRICE


SPLIT.PRICING.DATA:

  PM = 0
  SALEQUAN = 0
  PRICE1 = 0
  PRICE2 = 0

  IF (PRICING.DATA$ = "") THEN BEGIN             ! if no pricing data
    RETURN                                       ! go back
  ENDIF                                          ! if no pricing data

  PM = ASC(PRICING.DATA$) AND 0FH                ! pricing method

  IF (PM = 10 ) THEN BEGIN                       ! if first char 3AH (pm 0)
    PM = 0
    POINTER = 1
  ENDIF ELSE BEGIN                               ! else pricing method 1-4
    POINTER = 2
  ENDIF                                          ! if first char 3AH (pm 0)

  TLOG.STRING$ = PRICING.DATA$

  GOSUB GET.DATA
  SALEQUAN = RESULT
  GOSUB GET.DATA
  IF (PM > 1) THEN BEGIN                         ! if pricing method 2-4 item
    PRICE1 = RESULT / 100000
    PRICE2 = MOD(RESULT, 10000)
  ENDIF ELSE BEGIN                               ! else pricing method 0 or 1
    PRICE1 = RESULT                              ! get price of item
  ENDIF                                          ! if pricing method 2-4 item

  RETURN                                         ! SPLIT.PRICING.DATA:


EXTRACT.ITEM.DATA:

  ITEMCODE$ = ""                                 ! initialize item data
  PRICING.DATA$ = ""
  DEPARTME = 0
  INDICAT1 = 0
  INDICAT2 = 0
  ITEMTYPE = 0
  XPRICE = 0
  DEALQTY = 0
  QTYORWGT = 0
  QUANTITY = 0

  POINTER = MATCHB(":", TLOG.STRING$, 3)         ! find position of item code
  IF (POINTER <= 3) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid
  ! save the item code
  SAVE.ITEMCODE$ = UNPACK$(MID$(TLOG.STRING$,3,POINTER - 3))

  GOSUB GET.DATA                                 ! get the extended price
  XPRICE = RESULT

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  GOSUB GET.DATA                                 ! get the department number
  DEPARTME = RESULT

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  GOSUB SKIP                                     ! skip the family codes

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  GOSUB GET.DATA                                 ! get indicat1
  INDICAT1 = RESULT

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  GOSUB GET.DATA                                 ! get indicat1
  INDICAT2 = RESULT

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  GOSUB GET.DATA                                 ! get item type
  ITEMTYPE = RESULT / 10
  IF (ITEMTYPE >= 8) THEN BEGIN
    ITEMTYPE = ITEMTYPE - 8
  ENDIF

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  IF (INDICAT2 AND 0002H) THEN BEGIN             ! if item extension string

    ! get extension string
    TLOG.STRING$ = READ.SL.STR$(CURR.INDEX% + 1) + ":"

    POINTER = 2                                  ! address first field
    GOSUB SKIP                                   ! skip the multipricing grp

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    GOSUB GET.DATA                               ! get the total deal qty

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    DEALQTY = RESULT

    START = POINTER + 1

    GOSUB SKIP

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    GOSUB SKIP

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    GOSUB SKIP

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    PRICING.DATA$ = MID$(TLOG.STRING$, START, POINTER - START + 1)

    GOSUB GET.DATA

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    QTYORWGT = RESULT

    IF (INDICAT1 AND 04000H) THEN BEGIN          ! if a weight item
      QUANTITY = 1                               ! only a quantity of 1
    ENDIF ELSE BEGIN                             ! else not a weight item
      QUANTITY = QTYORWGT                        ! save quantity
    ENDIF                                        ! if a weight item

    GOSUB GET.DATA

    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      RETURN                                     ! go back
    ENDIF                                        ! if string doesn't seem valid

    IE.INDICAT1 = RESULT
  ENDIF ELSE BEGIN                               ! else no item extension string
    QTYORWGT = 1
    QUANTITY = QTYORWGT
    PRICING.DATA$ = ""
  ENDIF                                          ! if item extension string

  ! we're done so save item code
  ITEMCODE$ = RIGHT$("000000000000" + SAVE.ITEMCODE$, 12)

  SAVE.ITEMCODE$ = ""                            ! reset work variable

  RETURN                                         ! EXTRACT.ITEM.DATA:


SKIP:
  POINTER = MATCHB(":", TLOG.STRING$, POINTER + 1)
  RETURN                                         ! return from SKIP:


GET.DATA:
  RESULT = 0
  START = POINTER
  POINTER = MATCHB(":",TLOG.STRING$,POINTER + 1)

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    RETURN                                       ! go back
  ENDIF                                          ! if string doesn't seem valid

  RESULT = PACKBIN4(TLOG.STRING$,START,POINTER - START - 1)

  RETURN                                         ! return from GET.DATA:

END SUB                                          ! end GET.POSSIBLE.WIC.ITEMS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: GET.UPC.PLU.FILE.ITEMS
! Description: This routine reads the PLU/UPC file looking for matching items
!              from this transaction.
!*******************************************************************************
SUB GET.UPC.PLU.FILE.ITEMS(ERR.CODE$,STATE$)

  STRING                               \
         UPC.PLU.FILE$,                \ UPC/PLU file for looking up benefits
         AP.FMT$,                      \ format string for reading UPC/PLU file
         AP.KEY$,                      \ key for read of record
         REC.DATA$,                    \ record data read from UPC/PLU file
         STATE$,                       \ State code returned from API
         ERR.CODE$                     ! error return code from this method

  INTEGER*1                            \
            RECORD.EXISTS              ! flag if upc record exists in file

  INTEGER*2                            \
            AP.RECL,                   \ record length for the UPC/PLU file
            WE.I%,WE.K%                ! work variables

  ON ERROR GOTO UPC.PLU.FILE.ERR

  AP.RECL = 39
  AP.FMT$ = "C" + STR$(AP.RECL)
  UPC.PLU.FILE$ = "R::WIC1:EAMWE" + STATE$ + ".APL"

  CLOSE WE.SESSION.NUM1                          ! make sure session free

  OPEN UPC.PLU.FILE$ KEYED RECL AP.RECL AS WE.SESSION.NUM1 NOWRITE NODEL

  FOR WE.I% = 1 TO WE.ITEM.COUNT
    IF (WE.ITEM.TLOG.INDEX(WE.I%) > 0)           \ if valid Tlog entry
    THEN BEGIN
      AP.KEY$ = WE.ITEM.CODE$(WE.I%)
      IF (AP.KEY$ <> "") THEN BEGIN              ! if key data
        RECORD.EXISTS = -1                       ! assume record will exist
        READ FORM AP.FMT$; #WE.SESSION.NUM1 KEY AP.KEY$; REC.DATA$
        IF RECORD.EXISTS THEN BEGIN              ! if record read from file
          RECORD.EXISTS = 0                      ! reset error flag
          ! Save the data.  We only want to read it here, we'll process it later
          WE.ITEM.APL.FILE.DATA$(WE.I%) = REC.DATA$
        ENDIF ELSE BEGIN                         ! else rec not read from file
          CALL WE.REMOVE.ITEM.ENTRY(WE.I%)       ! remove the record
        ENDIF                                    ! if record read from file
      ENDIF ELSE BEGIN                           ! else key data not valid
        CALL WE.REMOVE.ITEM.ENTRY(WE.I%)         ! remove the record
      ENDIF                                      ! if key data
    ENDIF                                        ! if valid Tlog entry
  NEXT WE.I%

UPC.PLU.FILE.EXIT:
  CLOSE WE.SESSION.NUM1                          ! make sure session free
  UPC.PLU.FILE$ = ""                             ! cleanup
  AP.FMT$ = ""
  AP.KEY$ = ""
  REC.DATA$ = ""
  EXIT SUB

UPC.PLU.FILE.ERR:

  WE.ERR$ = ERR
  WE.ERRN$ = ERRNHEX$                            ! Convert error code

  IF (WE.ERR$ = "CU") THEN BEGIN                 ! Tried to close a session
    RESUME                                       ! number that is not active
  ENDIF

  IF (WE.ERR$ = "EF") THEN BEGIN                 ! If record can't be found
    RECORD.EXISTS = 0                            ! flag record wasn't found
    RESUME                                       ! go back
  ENDIF                                          ! If record can't be found

  ! build unique error message with error codes
  A$ = "WEap"                                    \
     + WE.ERR$                                   \
     + WE.ERRN$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  ! build unique error message with state ID of problem
  A$ = "WEap " + STATE$

  CALL ADXERROR(VAL(TS.TERMINAL$),               \ log error
                ASC("U"),                        \
                0,                               \
                1,                               \ severity level
                2,                               \
                A$)

  WE.ITEM.COUNT = 0                              ! reset the item count
  ERR.CODE$ = "40000"                            ! return unique error code

  RESUME UPC.PLU.FILE.EXIT                       ! always resume from on error

END SUB                                          ! end GET.UPC.PLU.FILE.ITEMS
!*******************************************************************************


!*******************************************************************************
! Function Name: WE.ITEM.DATE.VALID
! Description: This function tests if the passed start date and end date is
!              valid.  If either the start date or end date is invalid, then
!              the date is deemed ok.  If there is no end date, then only the
!              start date is checked.
!*******************************************************************************
FUNCTION WE.ITEM.DATE.VALID(BEG.DATE$,END.DATE$)

  STRING                               \
         BEG.DATE$,                    \ starting date passed in (YYDDMM)
         END.DATE$                     ! ending date passed in (YYDDMM)

  INTEGER*1                            \
            WE.ITEM.DATE.VALID         ! function return

  INTEGER*4                            \
            CUR.DATE,                  \ current date in integer format
            BEG.DATE,                  \ start date in integer format
            END.DATE                   ! end date in integer format

  WE.ITEM.DATE.VALID = -1                        ! assume date will be ok

  ON ERROR GOTO WE.ITEM.DATE.ERR                 ! check error in case of VAL

  CUR.DATE = VAL(DATE$)                          ! get curr date in int format
  BEG.DATE = VAL(BEG.DATE$)                      ! get begin date in int format
  END.DATE = VAL(END.DATE$)                      ! get end date in int format

  IF (BEG.DATE <= 0) THEN BEGIN                  ! if invalid starting date
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid starting date

  IF (END.DATE <= 0) THEN BEGIN                  ! if no end date
    IF (BEG.DATE > CUR.DATE) THEN BEGIN          ! if after current date
      WE.ITEM.DATE.VALID = 0                     ! date not valid
    ENDIF                                        ! if after current date
    EXIT FUNCTION
  ENDIF

  IF (CUR.DATE < BEG.DATE) OR                    \ if date is before begin date
     (CUR.DATE > END.DATE)                       \ or date is after end date
  THEN BEGIN                                     ! item should not be wic'able
    WE.ITEM.DATE.VALID = 0                       ! flag so date is found invalid
  ENDIF                                          ! item should not be wic'able

WE.ITEM.DATE.EXIT:
  EXIT FUNCTION

WE.ITEM.DATE.ERR:

  RESUME WE.ITEM.DATE.EXIT                       ! always resume from error

END FUNCTION                                     ! end WE.ITEM.DATE.VALID
!*******************************************************************************


!*******************************************************************************
! Function Name: GET.INDICAT1
! Description: This function returns the indicat1 field from the passed Tlog
!              offset.
!*******************************************************************************
FUNCTION GET.INDICAT1(TLOG.OFFSET)

  STRING                               \
         TLOG.STRING$                  ! string read from Tlog

  INTEGER*2                            \
            START,                     \ used for positioning in string
            POINTER,                   \ pointer when parsing strings
            WE.I%,                     \ work variable
            TLOG.OFFSET,               \ offset into tlog for field
            GET.INDICAT1               ! function return

  INTEGER*4                            \
            RESULT                     ! subroutine call results

  GET.INDICAT1 = 0                               ! initialize function return

  IF (TLOG.OFFSET > SL.END) THEN BEGIN           ! if invalid offset
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid offset

  ! get entry
  TLOG.STRING$ = READ.SL.STR$(TLOG.OFFSET) + ":"

  IF (LEN(TLOG.STRING$) < 7) THEN BEGIN          ! if string not valid
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if string not valid

  IF (ASC(TLOG.STRING$) <> 1) THEN BEGIN         ! if not correct string type
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if not correct string type

  POINTER = MATCHB(":", TLOG.STRING$, 3)         ! find position of item code

  IF (POINTER <= 3) THEN BEGIN                   ! if string doesn't seem valid
    EXIT FUNCTION                                ! go back
  ENDIF                                          ! if string doesn't seem valid

  FOR WE.I% = 1 TO 3                             ! skip leading fields
    POINTER = MATCHB(":", TLOG.STRING$, POINTER + 1)
    IF (POINTER <= 0) THEN BEGIN                 ! if string doesn't seem valid
      EXIT FUNCTION                              ! go back
    ENDIF                                        ! if string doesn't seem valid
  NEXT WE.I%                                     ! skip leading fields

  START = POINTER                                ! start of indicat1 field
  POINTER = MATCHB(":",TLOG.STRING$,POINTER + 1)

  IF (POINTER <= 0) THEN BEGIN                   ! if string doesn't seem valid
    EXIT FUNCTION                                ! go back
  ENDIF                                          ! if string doesn't seem valid

  RESULT = PACKBIN4(TLOG.STRING$,START,POINTER - START - 1)
  GET.INDICAT1 = RESULT                          ! return result

  TLOG.STRING$ = ""                              ! cleanup

END FUNCTION                                     ! end GET.INDICAT1
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: GET.PRESCRIPTION.ITEMS
! Description: This routine finds the items that match the customer's
!              prescription.
!*******************************************************************************
SUB GET.PRESCRIPTION.ITEMS

  STRING                               \
         RX.CATEGORY$,                 \ main category from current prescription
         RX.SUBCATEGORY$,              \ subcategory from current prescription
         REC.DATA$,                    \ record data read from UPC/PLU file
         CHECKDIGIT$,                  \ Check Digi read from the UPC/PLU file
         CATEGORY$,                    \ Category read from the UPC/PLU file
         SUBCATEGORY$,                 \ SubCategory read from the UPC/PLU file
         BENEFITQTY$,                  \ Benefit Quantity read from the UPC/PLU
         BENEFITUNIT$,                 \ Benefit Unit read from the UPC/PLU file
         ITEMPRICE$,                   \ Item Price read from the UPC/PLU file
         PRICETYPE$,                   \ Price Type read from the UPC/PLU file
         DATEEFFECTIVE$,               \ Date Effective read from the UPC/PLU
         DATEEND$,                     \ Date End read from the UPC/PLU file
         ZEROSUBCATFLAG$,              \ Zero Sub Category flag read from file
         SAVE.0.CHILD.ARRAY$(1),       \ save the zero children array
         SAVE.N.CHILD.ARRAY$(1),       \ save the nonzero children array
         ITEM.CHILDREN.ARRAY$(1),      \ children array for current item
         CPN.POINTER$                  ! pointer to coupons for items

  INTEGER*1                            \
            WE.RX.ITEM.ASSOCIATED(1),  \ flag if category has an item associated
            CPN.VALIDATION(1),         \ validation type of the coupon
            CPN.USED(1)                ! flag array if any coupons were used

  INTEGER*2                            \
            RX.INDEX,                  \ index marker into category arrays
            CATEGORY.ITEMS,            \ num of items that match curr category
            INDICAT1,                  \ items indicat 1 field
            CPN.INDICAT1,              \ coupons indicat 1 field
            ITEM.CAT.IDX(1),           \ save item indexes for categories
            ITEM.INDEX,                \ index used in splitting quantity prices
            ITEM.UNIT.PRICE.CNT,       \ item price count of unit prices
            CPN.INDEX,                 \ index into our coupon arrays
            WEBT.POINTER(1),           \ pointer into item/coupon association
            CURR.CPN.ARRAY.SIZE,       \ current size of coupon arrays
            TEMP.LOOP,                 \ local work var for adding temp data
            I2.TEMP.ARRAY(1),          \ hold array data for expansion
            WE.I%,WE.K%                ! local work variables

  INTEGER*4                            \
            ITEM.NET.PRICE,            \ saved item net price
            ITEM.NET.PRICES(1),        \ items net prices for compare
            ITEM.UNIT.PRICES(1),       \ individual item prices split from qty
            SORT.SAVE,                 \ hold variable for sort routine
            ITEM.QTY,                  \ work variable for item entry quantity
            RX.QTY,                    \ quantity of benefit units
            PURCH.QTY,                 \ Quantity of single item that contribute
            COST,                      \ Cost of single item that contribute
            ITM.BEN.QTY,               \ quantity of item benefit units
            TOT.ITM.BEN.QTY,           \ total benefit quantity for the item
            TOT.BENEFIT.QTY,           \ total quantity if item benefit units
            CURR.ITEM.UPC,             \ last 8 digits of current item upc
            TOT.CPN.COST,              \ total coupon cost for an item
            CPN.VALUE(1),              \ value of coupons associated with item
            I4.TEMP.ARRAY(1),          \ hold array data for expansion
            WE.X%                      ! work variable

  DIM WE.RX.ITEM.ASSOCIATED(WE.NUM.CATEGORIES)

  ! First go through and toss out any items that doesn't have a valid category
  FOR WE.I% = 1 TO WE.ITEM.COUNT                 ! for all items in the list
    IF (WE.ITEM.TLOG.INDEX(WE.I%) > 0)           \ if valid Tlog entry
    THEN BEGIN
      REC.DATA$ = WE.ITEM.APL.FILE.DATA$(WE.I%)
      IF (REC.DATA$ <> "") THEN BEGIN            ! if data intact
        GOSUB PARSE.APL.DATA                     ! parse rec data into work vars
        IF WE.ITEM.DATE.VALID(DATEEFFECTIVE$,DATEEND$)  \
        THEN BEGIN                               ! if valid date on item
          RX.INDEX = 0                           ! no match found yet
          FOR WE.K% = 1 TO WE.NUM.CATEGORIES     ! search through all
            IF (WE.CATEGORY$(WE.K%) <> "")       \ if data in prescription
            THEN BEGIN
              RX.CATEGORY$ = LEFT$(WE.CATEGORY$(WE.K%),2)
              IF (RX.CATEGORY$ = CATEGORY$)      \ if matching category
              THEN BEGIN

                ! Get the benefit quantities from the prescription category and
                ! the item.  Use the base error return flag for VAL function,
                ! didn't want to implement an ON ERROR in this subroutine

                TS.ER.RETURN = -1                ! base error return flag
                RX.QTY = VAL(WE.CATEGORY.UPD.QTY$(WE.K%))
                IF TS.ER.RETURN THEN BEGIN       ! if no error on VAL function
                  ITM.BEN.QTY = VAL(BENEFITQTY$)
                ENDIF                            ! if no error on VAL function

                IF (ITM.BEN.QTY > 0) AND (ITM.BEN.QTY <= RX.QTY) AND \
                   TS.ER.RETURN                                      \
                THEN BEGIN                       ! if category isn't decremented

                  TS.ER.RETURN = 0               ! reset base error flag

                  ! Now test for subcategory match.
                  ! If the subcategories match outright or this is the zero sub
                  ! category and the zero sub category flag is set in the
                  ! UPC/PLU file then continue.  We should have always reached
                  ! the zero subcategory last since we've sorted the category
                  ! arrays from largest to smallest

                  RX.SUBCATEGORY$ = RIGHT$(WE.CATEGORY$(WE.K%),3)
                  IF (RX.SUBCATEGORY$ = SUBCATEGORY$) OR  \
                     ((RX.SUBCATEGORY$ = "000") AND       \
                      (ZEROSUBCATFLAG$ <> "00"))          \
                  THEN BEGIN                     ! if a valid subcategory
                    RX.INDEX = WE.K%             ! save index for matching cat
                    ! flag this category has an item associated with it
                    WE.RX.ITEM.ASSOCIATED(WE.K%) = -1
                  ENDIF                          ! if a valid subcategory
                ENDIF                            ! if category isn't decremented
                TS.ER.RETURN = 0                 ! make sure base flag is reset
              ENDIF                              ! if matching category
            ENDIF                                ! if data in prescription
          NEXT WE.K%                             ! search through all
          IF (RX.INDEX <= 0) THEN BEGIN          ! if no match found in the Rx
            CALL WE.REMOVE.ITEM.ENTRY(WE.I%)     ! remove this record
          ENDIF                                  ! if no match found in the Rx
        ENDIF ELSE BEGIN                         ! else invalid date on item
          CALL WE.REMOVE.ITEM.ENTRY(WE.I%)       ! get rid of record
        ENDIF                                    ! if valid date on item
      ENDIF ELSE BEGIN                           ! else no data saved
        CALL WE.REMOVE.ITEM.ENTRY(WE.I%)         ! get rid of record
      ENDIF                                      ! if data intact
    ENDIF                                        ! if valid Tlog entry
  NEXT WE.I%                                     ! for all the items in the list

  IF (WE.ITEM.COUNT > 0) THEN BEGIN              ! if items left over

    ! now trim out all categories that do not have an item associated with it
    ! or seem to be invalid
    FOR WE.I% = 1 TO WE.NUM.CATEGORIES
      WE.K% = -1                                 ! use as a flag
      IF (WE.RX.ITEM.ASSOCIATED(WE.I%) = -1)     \ if item associated
      THEN BEGIN
        IF (WE.CATEGORY$(WE.I%) <> "")           \ if category data
        THEN BEGIN

          ! Use the base error return flag for VAL function, didn't want to
          ! implement an ON ERROR in this subroutine

          TS.ER.RETURN = -1                      ! base error return flag
          RX.QTY = VAL(WE.CATEGORY.UPD.QTY$(WE.I%))
          IF TS.ER.RETURN THEN BEGIN             ! if no error occurred
            TS.ER.RETURN = 0                     ! reset base error flag
          ENDIF ELSE BEGIN                       ! else error occurred
            RX.QTY = 0                           ! make sure quantity is 0
          ENDIF                                  ! if no error occurred
          IF (RX.QTY > 0) THEN BEGIN             ! if category quantity
            WE.K% = 0                            ! reset flag not removed
          ENDIF                                  ! if category quantity
        ENDIF                                    ! if category data
      ENDIF                                      ! if item associated
      IF WE.K% THEN BEGIN                        ! if not using this category
        GOSUB REMOVE.RX.CATEGORY                 ! get rid of this category
      ENDIF                                      ! if not using this category
    NEXT WE.I%

    ! Initial call to EM item/coupon association.
    ! This call is to do the initial pro-rating, the second call is an attempt
    ! to move coupons from our WIC'able items.
    WE.ITEM.MAX.COUNT = WE.ITEM.COUNT            ! set global count
    CALL WEBT.FINISH.COUPONS                     ! finish wic coupon process

    ! Save the children arrays, we'll need to reset them after this first loop
    DIM SAVE.0.CHILD.ARRAY$(WE.ITEM.COUNT)
    DIM SAVE.N.CHILD.ARRAY$(WE.ITEM.COUNT)

    FOR WE.I% = 1 TO WE.ITEM.COUNT
      SAVE.0.CHILD.ARRAY$(WE.I%) = WE.ITEM.0.CHILD.ARRAY$(WE.I%)
      SAVE.N.CHILD.ARRAY$(WE.I%) = WE.ITEM.N.CHILD.ARRAY$(WE.I%)
    NEXT WE.I%

    IF (WEBT.SAVE > 0) THEN BEGIN                ! if associated coupons saved
      DIM CPN.USED(WEBT.SAVE)                    ! flag array if coupons used
    ENDIF                                        ! if associated coupons saved

    ! First go through and try to find any items that are left over after any
    ! categories have been exhausted
    FOR WE.I% = 1 TO WE.NUM.CATEGORIES           ! loop through our cat arrays
      RX.CATEGORY$ = LEFT$(WE.CATEGORY$(WE.I%),2)
      RX.SUBCATEGORY$ = RIGHT$(WE.CATEGORY$(WE.I%),3)
      RX.QTY = VAL(WE.CATEGORY.UPD.QTY$(WE.I%))  ! get quantity
      GOSUB BUILD.CATEGORY.ITEMS                 ! build data for matching items

      TOT.BENEFIT.QTY = 0                        ! start with 0 total item qty
      FOR WE.K% = 1 TO CATEGORY.ITEMS            ! for matching category items
        IF (ITEM.CAT.IDX(WE.K%) > 0)             \ if a matching category
        THEN BEGIN
          ITEM.INDEX = ITEM.CAT.IDX(WE.K%)       ! save index into work variable
          REC.DATA$ = WE.ITEM.APL.FILE.DATA$(ITEM.INDEX)
          GOSUB PARSE.APL.DATA                   ! parse item record data
          ITM.BEN.QTY = VAL(BENEFITQTY$)         ! get benefit quantity
          IF (ITM.BEN.QTY <= RX.QTY) AND         \ if 1 is less than what's left
             (ITM.BEN.QTY > 0)                   \ and benefit quantity on item
          THEN BEGIN                             ! if contributing item to Rx

            TOT.ITM.BEN.QTY = 0                  ! total benefit quantity
            PURCH.QTY = 0
            COST = 0

            GOSUB SPLIT.UNIT.PRICES              ! split up the unit prices

            GOSUB GET.ITEM.COUPONS               ! get all coupons associated

            ! now go through and try to toss out any prices that may match a
            ! coupon price.
            IF (CPN.INDEX > 0) THEN BEGIN        ! if any coupons collected
              FOR I% = 1 TO ITEM.UNIT.PRICE.CNT  ! for all our item prices
                IF (ITEM.UNIT.PRICES(I%) > 0)    \ if there's a value
                THEN BEGIN
                  FOR K% = 1 TO CPN.INDEX
                    IF (CPN.VALUE(K%) > 0)       \ if we have a coupon value
                    THEN BEGIN
                      IF (ITEM.UNIT.PRICES(I%) = CPN.VALUE(K%)) AND  \
                         (WEBT.CTYP(WEBT.POINTER(K%)) <> 5)          \
                      THEN BEGIN                 ! if bogo coupon
                        ITEM.UNIT.PRICES(I%) = -1  ! reset price invalid
                        CPN.VALUE(K%) = 0        ! we're done with this coupon
                        K% = CPN.INDEX           ! break inner loop
                      ENDIF                      ! if bogo coupon
                    ENDIF                        ! if we have a coupon value
                  NEXT K%
                ENDIF                            ! if there's a value
              NEXT I%                            ! for all our item prices
            ENDIF                                ! if any coupons collected

            FOR I% = ITEM.UNIT.PRICE.CNT TO 1 STEP -1
              IF (ITEM.UNIT.PRICES(I%) > 0)      \ if a valid unit price
              THEN BEGIN
                ITEM.CHILDREN.ARRAY$(I%) = "1"   ! flag this as used
                TOT.ITM.BEN.QTY = TOT.ITM.BEN.QTY + ITM.BEN.QTY
                ! Check to see if one more will make total quantity go negative
                IF ((TOT.ITM.BEN.QTY + ITM.BEN.QTY) > RX.QTY)  \
                THEN BEGIN                       ! if 1 more will cause negative
                  I% = 1                         ! break loop
                ENDIF                            ! if 1 more will cause negative
              ENDIF                              ! if a valid unit price
            NEXT I%

            IF (TOT.ITM.BEN.QTY > 0) THEN BEGIN  ! if added item to quantity

              WE.ITEM.USED(ITEM.INDEX) = -1      ! flag this item will be used

              ! build new children array
              A$ = ""
              FOR I% = 1 TO ITEM.UNIT.PRICE.CNT
                A$ = A$ + ITEM.CHILDREN.ARRAY$(I%)
              NEXT I%
              IF (RX.SUBCATEGORY$ = "000")       \ if working on zero subcat
              THEN BEGIN
                WE.ITEM.0.CHILD.ARRAY$(ITEM.INDEX) = A$
              ENDIF ELSE BEGIN                   ! else working on nonzero
                WE.ITEM.N.CHILD.ARRAY$(ITEM.INDEX) = A$
              ENDIF                              ! if working on zero subcat
              RX.QTY = RX.QTY - TOT.ITM.BEN.QTY  ! reduce the quantity
            ENDIF                                ! if added item to quantity
          ENDIF                                  ! if contributing item to Rx

          IF (RX.QTY <= 0) THEN BEGIN            ! if reduced the whole category
            WE.K% = CATEGORY.ITEMS               ! break loop
          ENDIF                                  ! if reduced the whole category

        ENDIF                                    ! if a matching category
      NEXT WE.K%                                 ! for matching category items
    NEXT WE.I%                                   ! loop through our cat arrays

    ! Restore the children arrays
    FOR WE.I% = 1 TO WE.ITEM.COUNT
      WE.ITEM.0.CHILD.ARRAY$(WE.I%) = SAVE.0.CHILD.ARRAY$(WE.I%)
      WE.ITEM.N.CHILD.ARRAY$(WE.I%) = SAVE.N.CHILD.ARRAY$(WE.I%)
    NEXT WE.I%

    ! Restore heap
    DIM SAVE.0.CHILD.ARRAY$(0)
    DIM SAVE.N.CHILD.ARRAY$(0)

    ! Now go through and toss out any remaining items that may not be used
    ! These should only be tossed if we run out of matching category
    FOR WE.I% = 1 TO WE.ITEM.COUNT
      IF (WE.ITEM.USED(WE.I%) <> -1) THEN BEGIN  ! if item not used in order
        CALL WE.REMOVE.ITEM.ENTRY(WE.I%)         ! get rid of record
      ENDIF                                      ! if item not used in order
    NEXT WE.I%

    ! Now call the item/coupon association routine again.
    ! We may move more coupons away from our wic'able items
    WE.ITEM.MAX.COUNT = WE.ITEM.COUNT            ! set global count
    CALL WEBT.FINISH.COUPONS                     ! finish wic coupon process

    ! pointer to coupons for zero categories
    DIM WE.ITEM.CPN.0.POINTER$(WE.ITEM.COUNT)
    ! pointer to coupons for nonzero categories
    DIM WE.ITEM.CPN.N.POINTER$(WE.ITEM.COUNT)
    ! item check digit read from file
    DIM WE.ITEM.CHECKDIGIT$(WE.ITEM.COUNT)

    ! set up data to match associated coupons to wic'able items
    IF (WEBT.SAVE > 0) THEN BEGIN                ! if associated coupons saved
      DIM CPN.USED(WEBT.SAVE)                    ! flag array if coupons used
    ENDIF                                        ! if associated coupons saved

    ! Not sure how big to make these since we could have pro-rated coupons
    ! dispersed throughout.  We'll set them to the size of Tlog entries to
    ! begin with but expand them later if we have to.
    WE.CPN.COUNT = 0
    CURR.CPN.ARRAY.SIZE = SL.END                 ! current size of coupon arrays
    DIM WE.CPN.TLOG.OFFSET(CURR.CPN.ARRAY.SIZE)
    DIM WE.CPN.QUANTITY(CURR.CPN.ARRAY.SIZE)
    DIM WE.CPN.VALUE(CURR.CPN.ARRAY.SIZE)

    DIM WE.CATEGORY.HAS.ITEMS(WE.NUM.CATEGORIES) ! flag if category used
    DIM WE.CATEGORY.ITEM.PTR$(WE.NUM.CATEGORIES) ! pointer to items for cats
    DIM WE.CATEGORY.DEBIT.QTY(WE.NUM.CATEGORIES) ! quantity debited from cat

    WE.TOTAL.ITEM.VALUE = 0                      ! initialize total item value
    WE.TOTAL.DISC.VALUE = 0                      ! initialize total discount
    DIM WE.TAX(0)
    DIM WE.TAX(4)                                ! reset tax accumulator

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Now we have a list of items that match the customer's prescription.
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    FOR WE.I% = 1 TO WE.NUM.CATEGORIES           ! loop through our categories
      RX.CATEGORY$ = LEFT$(WE.CATEGORY$(WE.I%),2)
      RX.SUBCATEGORY$ = RIGHT$(WE.CATEGORY$(WE.I%),3)
      RX.QTY = VAL(WE.CATEGORY.UPD.QTY$(WE.I%))  ! get quantity
      GOSUB BUILD.CATEGORY.ITEMS                 ! build data for matching items

      TOT.BENEFIT.QTY = 0                        ! start with 0 total quantity
      FOR WE.K% = 1 TO CATEGORY.ITEMS            ! for all the matching items
        IF (ITEM.CAT.IDX(WE.K%) > 0)             \ if a matching category
        THEN BEGIN
          ITEM.INDEX = ITEM.CAT.IDX(WE.K%)       ! save index into work variable
          REC.DATA$ = WE.ITEM.APL.FILE.DATA$(ITEM.INDEX)
          GOSUB PARSE.APL.DATA                   ! parse item data in work vars
          ITM.BEN.QTY = VAL(BENEFITQTY$)         ! get benefit quantity
          IF (ITM.BEN.QTY <= RX.QTY) AND         \ if 1 is less than what's left
             (ITM.BEN.QTY > 0)                   \ and a benefit quantity
          THEN BEGIN                             ! if item contributes to Rx

            TOT.ITM.BEN.QTY = 0                  ! total benefit quantity
            PURCH.QTY = 0
            COST = 0

            GOSUB SPLIT.UNIT.PRICES              ! split up the unit prices

            GOSUB GET.ITEM.COUPONS               ! get all the cpns associated

            ! now go through and try to toss out any prices that match a coupon
            IF (CPN.INDEX > 0) THEN BEGIN        ! if any coupons collected
              FOR I% = 1 TO ITEM.UNIT.PRICE.CNT  ! for all our item prices
                IF (ITEM.UNIT.PRICES(I%) > 0)    \ if a value for this entry
                THEN BEGIN
                  FOR K% = 1 TO CPN.INDEX
                    IF (CPN.VALUE(K%) > 0)       \ if we have a coupon value
                    THEN BEGIN
                      IF (ITEM.UNIT.PRICES(I%) = CPN.VALUE(K%)) AND  \
                         (WEBT.CTYP(WEBT.POINTER(K%)) <> 5)          \
                      THEN BEGIN                 ! if bogo coupon
                        ITEM.UNIT.PRICES(I%) = -1  ! reset price invalid
                        CPN.VALUE(K%) = 0        ! we're done with this coupon
                        K% = CPN.INDEX           ! break inner loop
                      ENDIF                      ! if bogo coupon
                    ENDIF                        ! if we have a coupon value
                  NEXT K%
                ENDIF                            ! if a value for this entry
              NEXT I%                            ! for all our item prices
            ENDIF                                ! if any coupons collected

            FOR I% = ITEM.UNIT.PRICE.CNT TO 1 STEP -1
              IF (ITEM.UNIT.PRICES(I%) > 0)      \ if a valid unit price
              THEN BEGIN
                IF ((PURCH.QTY + 1) <= WE.ITEM.QUANTITY(ITEM.INDEX))  \
                THEN BEGIN                       ! sanity check on item data
                  ITEM.CHILDREN.ARRAY$(I%) = "1" ! flag this as used
                  PURCH.QTY = PURCH.QTY + 1
                  COST = COST + ITEM.UNIT.PRICES(I%)
                  TOT.ITM.BEN.QTY = TOT.ITM.BEN.QTY + ITM.BEN.QTY
                  ! Check to see if one more will make total category quantity
                  ! go negative
                  IF ((TOT.ITM.BEN.QTY + ITM.BEN.QTY) > RX.QTY)  \
                  THEN BEGIN                     ! if 1 more will go negative
                    I% = 1                       ! break loop
                  ENDIF                          ! if 1 more will go negative
                ENDIF                            ! sanity check on item data
              ENDIF                              ! if a valid unit price
            NEXT I%

            IF (TOT.ITM.BEN.QTY > 0) THEN BEGIN  ! if added item to quantity

              ! build new children array
              A$ = ""
              FOR I% = 1 TO ITEM.UNIT.PRICE.CNT
                A$ = A$ + ITEM.CHILDREN.ARRAY$(I%)
              NEXT I%
              IF (RX.SUBCATEGORY$ = "000")       \ if working on 0 subcategory
              THEN BEGIN
                WE.ITEM.0.CHILD.ARRAY$(ITEM.INDEX) = A$
                IF (WE.ITEM.FULL.CATEGORY$(ITEM.INDEX) = "")  \
                THEN BEGIN                       ! if not stored a full category
                  WE.ITEM.FULL.CATEGORY$(ITEM.INDEX) = RX.CATEGORY$       \
                                                     + RX.SUBCATEGORY$
                ENDIF                            ! if not stored a full category
                WE.ITEM.0.RX.INDEX(ITEM.INDEX) = WE.I%
                WE.ITEM.0.PURCH.QTY(ITEM.INDEX) = PURCH.QTY
                WE.ITEM.0.COST(ITEM.INDEX) = COST
              ENDIF ELSE BEGIN                   ! else working on nonzero
                WE.ITEM.N.CHILD.ARRAY$(ITEM.INDEX) = A$
                WE.ITEM.FULL.CATEGORY$(ITEM.INDEX) = RX.CATEGORY$         \
                                                   + RX.SUBCATEGORY$
                WE.ITEM.N.RX.INDEX(ITEM.INDEX) = WE.I%
                WE.ITEM.N.PURCH.QTY(ITEM.INDEX) = PURCH.QTY
                WE.ITEM.N.COST(ITEM.INDEX) = COST
              ENDIF                              ! if working on 0 subcategory

              WE.ITEM.BENEFIT.QTY(ITEM.INDEX) = ITM.BEN.QTY
              WE.ITEM.UNIT.DESC$(ITEM.INDEX) = BENEFITUNIT$
              WE.ITEM.CHECKDIGIT$(ITEM.INDEX) = CHECKDIGIT$
! IO01214     WE.ITEM.PRICE(ITEM.INDEX) = COST

              WE.CATEGORY.DEBIT.QTY(WE.I%) = WE.CATEGORY.DEBIT.QTY(WE.I%)      \
                                           + (ITM.BEN.QTY * PURCH.QTY)

              ! add to the total item value
              WE.TOTAL.ITEM.VALUE = WE.TOTAL.ITEM.VALUE + COST

              INDICAT1 = WE.ITEM.INDICAT1(ITEM.INDEX)
              IF (INDICAT1 AND 00F0H) THEN BEGIN ! if tax flags set for item
                IF (INDICAT1 AND 0080H)          \ if tax plan A
                THEN BEGIN
                  WE.TAX(1) = WE.TAX(1) + COST   ! accumulate tax cost
                ENDIF                            ! if tax plan A
                IF (INDICAT1 AND 0040H)          \ if tax plan B
                THEN BEGIN
                  WE.TAX(2) = WE.TAX(2) + COST   ! accumulate tax cost
                ENDIF                            ! if tax plan B
                IF (INDICAT1 AND 0020H)          \ if tax plan C
                THEN BEGIN
                  WE.TAX(3) = WE.TAX(3) + COST   ! accumulate tax cost
                ENDIF                            ! if tax plan C
                IF (INDICAT1 AND 0010H)          \ if tax plan D
                THEN BEGIN
                  WE.TAX(4) = WE.TAX(4) + COST   ! accumulate tax cost
                ENDIF                            ! if tax plan D
              ENDIF                              ! if tax flags set for item

              ! Collect any coupon information for this item
              ! We'll just start adding coupons until they're all added or we
              ! run out of cost on the item(s). We should have already validated
              ! that this entry can contain the entire coupon total.

              TOT.CPN.COST = 0
              CPN.POINTER$ = ""
              IF (CPN.INDEX > 0) THEN BEGIN      ! if any coupons collected

                ! First go through and see if we can remove any coupons that
                ! are not wic'able based on item price matching
                FOR I% = 1 TO ITEM.UNIT.PRICE.CNT
                  IF (ITEM.UNIT.PRICES(I%) > 0) AND                  \
                     (ITEM.CHILDREN.ARRAY$(I%) = "0")                \
                  THEN BEGIN                     ! if non-wic'able entry
                    M% = 0
                    DIM CPN.VALIDATION(CPN.INDEX)
                    FOR K% = 1 TO CPN.INDEX
                      IF (CPN.VALUE(K%) > 0) AND              \
                         (CPN.USED(WEBT.POINTER(K%)) <> -1)   \
                      THEN BEGIN                 ! if coupon value and not used
                        IF (WEBT.CTYP(WEBT.POINTER(K%)) <> 5)        \
                        THEN BEGIN               ! if not a pro-rate coupon
                          IF (WEBT.IVAL(WEBT.POINTER(K%)) =          \
                              ITEM.UNIT.PRICES(I%))                  \
                          THEN BEGIN             ! if item price matches coupon

                            ! Now check if this particular item entry has had
                            ! a similar coupon associated with it.  The EM
                            ! rules dictate that only one per validation coupon,
                            ! (i.e. promo, mfr,etc.), can match the item.
                            ! In other words, you can only have one promo code,
                            ! one mfr validated, etc. per item but you can have
                            ! a combination of differently validated coupons.
                            ! We won't do this on a pro-rated coupon though,
                            ! (i.e.WEBT.CTYP = 5).

                            J% = -1              ! use as a flag
                            FOR L% = 1 TO M%
                              IF (CPN.VALIDATION(L%) =                      \
                                  WEBT.CVLD(WEBT.POINTER(K%)))              \
                              THEN BEGIN         ! if already have one like it
                                J% = 0           ! flag so this one won't go
                                L% = M%          ! break loop
                              ENDIF              ! if already have one like it
                            NEXT L%
                            IF J% THEN BEGIN     ! didn't find prev validation
                              CPN.VALUE(K%) = 0
                              M% = M% + 1          ! save validation for coupon
                              CPN.VALIDATION(M%) = WEBT.CVLD(WEBT.POINTER(K%))
                            ENDIF                ! didn't find prev validation
                          ENDIF                  ! if item price matches coupon
                        ENDIF                    ! if not a pro-rate coupon
                      ENDIF                      ! if coupon value and not used
                    NEXT K%
                    DIM CPN.VALIDATION(0)        ! restore heap
                  ENDIF                          ! if non-wic'able entry
                NEXT I%

                ! Now go through and try to match coupons with the wic'able
                ! items
                FOR I% = 1 TO ITEM.UNIT.PRICE.CNT  ! for all our item prices
                  IF (ITEM.UNIT.PRICES(I%) > 0) AND                  \
                     (ITEM.CHILDREN.ARRAY$(I%) = "1")                \
                  THEN BEGIN                     ! if a value for this entry
                    M% = 0
                    DIM CPN.VALIDATION(CPN.INDEX)
                    FOR K% = 1 TO CPN.INDEX
                      IF (CPN.VALUE(K%) > 0) AND                     \
                         (CPN.USED(WEBT.POINTER(K%)) <> -1)          \
                      THEN BEGIN                 ! if coupon value and not used
                        IF ((CPN.VALUE(K%) + TOT.CPN.COST) < COST)   \
                        THEN BEGIN               ! if cost not becoming negative

                          ! Now check if this particular item entry has already
                          ! had a similar coupon associated with it.  The EM
                          ! rules dictate that only one per validation coupon,
                          ! (i.e. promo, mfr,etc.), can match the item.
                          ! In other words, you can only have one promo code,
                          ! one mfr validated, etc. per item but you can have
                          ! a combination of differently validated coupons.
                          ! We won't do this on a pro-rated coupon though, (i.e.
                          ! WEBT.CTYP = 5).
                          ! We only want to do this if the purchase quantity,
                          ! (i.e. items we're making wic'able), is less than
                          ! the original full quantity.  If the full quantity,
                          ! then give all the coupons to this entry.

                          J% = -1                ! use as a flag

                          IF (PURCH.QTY < WE.ITEM.QUANTITY(ITEM.INDEX))   \
                          THEN BEGIN             ! if purch qty < than original
                            FOR L% = 1 TO M%
                              IF (CPN.VALIDATION(L%) =                    \
                                  WEBT.CVLD(WEBT.POINTER(K%))) AND        \
                                 (WEBT.CTYP(WEBT.POINTER(K%)) <> 5)       \
                              THEN BEGIN         ! if already have one like it
                                J% = 0           ! flag so this one won't go
                                L% = M%          ! break loop
                              ENDIF              ! if already have one like it
                            NEXT L%
                          ENDIF                  ! if purch qty < than original

                          IF J% THEN BEGIN       ! if we didn't find a match

                            WE.CPN.COUNT = WE.CPN.COUNT + 1
                            IF (WE.CPN.COUNT > CURR.CPN.ARRAY.SIZE)       \
                            THEN BEGIN           ! if too many coupons
                              ! expand the arrays
                              GOSUB EXPAND.WE.CPN.ARRAYS
                            ENDIF                ! if too many coupon
                            CPN.POINTER$ = CPN.POINTER$                   \
                                         + STR$(WE.CPN.COUNT) + ":"

                            IF (WEBT.CTYP(WEBT.POINTER(K%)) = 5)               \
                            THEN BEGIN           ! if a pro-rated discount

                              ! calculate the discount amount for pro-rated cpns
                              WE.X% = (PURCH.QTY * 10000) /                    \
                                      WE.ITEM.QUANTITY(ITEM.INDEX)

                              WE.X% = ((WE.X% * CPN.VALUE(K%)) + 5000) / 10000

                              CPN.VALUE(K%) = WE.X%

                              ! Now check if this is a transaction discount vs.
                              ! a normal coupon discount.  If the item code is
                              ! zero then assume it's a trnx discount.
                              IF (WEBT.CUPC(WEBT.POINTER(K%)) = 0)   \
                              THEN BEGIN         ! if trnx discount
                                ! set offset invalid so no printing
                                WE.CPN.TLOG.OFFSET(WE.CPN.COUNT) = -1
                              ENDIF ELSE BEGIN   ! else not a trnx discount
                                WE.CPN.TLOG.OFFSET(WE.CPN.COUNT) =             \
                                                     WEBT.COFF(WEBT.POINTER(K%))
                              ENDIF              ! if trnx discount

                            ENDIF ELSE BEGIN     ! else not a pro-rated discount
                              WE.CPN.TLOG.OFFSET(WE.CPN.COUNT) =               \
                                                     WEBT.COFF(WEBT.POINTER(K%))

                              M% = M% + 1          ! save validation for coupon
                              CPN.VALIDATION(M%) = WEBT.CVLD(WEBT.POINTER(K%))
                            ENDIF                ! if a pro-rated discount

                            WE.CPN.QUANTITY(WE.CPN.COUNT) = 1
                            WE.CPN.VALUE(WE.CPN.COUNT) = CPN.VALUE(K%)

                            CPN.INDICAT1 = GET.INDICAT1(                       \
                                              WEBT.COFF(WEBT.POINTER(K%)))

                            IF (CPN.INDICAT1 AND 00F0H)              \
                            THEN BEGIN           ! if tax flags set
                              IF (CPN.INDICAT1 AND 0080H) AND        \
                                 (INDICAT1 AND 0080H)                \
                              THEN BEGIN         ! if tax plan A
                                WE.TAX(1) = WE.TAX(1) - CPN.VALUE(K%)
                                IF (WE.TAX(1) < 0) THEN BEGIN
                                  WE.TAX(1) = 0
                                ENDIF
                              ENDIF              ! if tax plan A
                              IF (CPN.INDICAT1 AND 0040H) AND        \
                                 (INDICAT1 AND 0040H)                \
                              THEN BEGIN         ! if tax plan B
                                WE.TAX(2) = WE.TAX(2) - CPN.VALUE(K%)
                                IF (WE.TAX(2) < 0) THEN BEGIN
                                  WE.TAX(2) = 0
                                ENDIF
                              ENDIF              ! if tax plan B
                              IF (CPN.INDICAT1 AND 0020H) AND        \
                                 (INDICAT1 AND 0020H)                \
                              THEN BEGIN         ! if tax plan C
                                WE.TAX(3) = WE.TAX(3) - CPN.VALUE(K%)
                                IF (WE.TAX(3) < 0) THEN BEGIN
                                  WE.TAX(3) = 0
                                ENDIF
                              ENDIF              ! if tax plan C
                              IF (CPN.INDICAT1 AND 0010H) AND        \
                                 (INDICAT1 AND 0010H)                \
                              THEN BEGIN         ! if tax plan D
                                WE.TAX(4) = WE.TAX(4) - CPN.VALUE(K%)
                                IF (WE.TAX(4) < 0) THEN BEGIN
                                  WE.TAX(4) = 0
                                ENDIF
                              ENDIF              ! if tax plan D
                            ENDIF                ! if tax flags set

                            CPN.USED(WEBT.POINTER(K%)) = -1
                            TOT.CPN.COST = TOT.CPN.COST + CPN.VALUE(K%)

                          ENDIF                  ! if we didn't find a match
                        ENDIF                    ! if cost not becoming negative
                      ENDIF                      ! if coupon value and not used
                    NEXT K%
                    DIM CPN.VALIDATION(0)        ! restore heap
                  ENDIF                          ! if a value for this entry
                NEXT I%                          ! for all our item prices
              ENDIF                              ! if any coupons collected

              ! add to the total discount value
              WE.TOTAL.DISC.VALUE = WE.TOTAL.DISC.VALUE + TOT.CPN.COST

              ! build item pointer for this category
              WE.CATEGORY.ITEM.PTR$(WE.I%) = WE.CATEGORY.ITEM.PTR$(WE.I%)      \
                                           + STR$(ITEM.INDEX) + ":"

              WE.CATEGORY.HAS.ITEMS(WE.I%) = -1  ! flag this category is used

              ! save pointers into coupon array for item's category.
              ! We'll use these for printing any coupons on the redempt receipt
              IF (RX.SUBCATEGORY$ = "000")       \ if working on 0 subcategory
              THEN BEGIN
                WE.ITEM.CPN.0.POINTER$(ITEM.INDEX) = CPN.POINTER$
              ENDIF ELSE BEGIN                   ! else working on non-0 subcat
                WE.ITEM.CPN.N.POINTER$(ITEM.INDEX) = CPN.POINTER$
              ENDIF                              ! if working on 0 subcategory

              TOT.BENEFIT.QTY = TOT.BENEFIT.QTY + TOT.ITM.BEN.QTY
              RX.QTY = RX.QTY - TOT.ITM.BEN.QTY  ! reduce the quantity
            ENDIF                                ! if added item to quantity
          ENDIF                                  ! if item contributes to Rx

          IF (RX.QTY <= 0) THEN BEGIN            ! if reduced the whole category
            WE.K% = CATEGORY.ITEMS               ! break loop
          ENDIF                                  ! if reduced the whole category

        ENDIF                                    ! if a matching category
      NEXT WE.K%                                 ! for all the matching items

      IF (TOT.BENEFIT.QTY > 0) THEN BEGIN        ! if something to reduce cat
          WE.ITEMS.TO.DECREMENT$ = WE.ITEMS.TO.DECREMENT$                  \
                                 + RX.CATEGORY$                            \
                                 + RX.SUBCATEGORY$                         \
                                 + RIGHT$(STRING$(5,"0") +                 \
                                          STR$(TOT.BENEFIT.QTY),5)

      ENDIF                                      ! if something to reduce cat
    NEXT WE.I%                                   ! loop through our categories
  ENDIF                                          ! if items left over

  RX.CATEGORY$ = ""                              ! cleanup
  RX.SUBCATEGORY$ = ""
  REC.DATA$ = ""
  CHECKDIGIT$ = ""
  CATEGORY$ = ""
  SUBCATEGORY$ = ""
  BENEFITQTY$ = ""
  BENEFITUNIT$ = ""
  ITEMPRICE$ = ""
  PRICETYPE$ = ""
  DATEEFFECTIVE$ = ""
  DATEEND$ = ""
  ZEROSUBCATFLAG$ = ""
  TS.TEMP1$ = ""
  CPN.POINTER$ = ""
  DIM ITEM.CHILDREN.ARRAY$(0)
  DIM WE.RX.ITEM.ASSOCIATED(0)
  DIM CPN.USED(0)
  DIM ITEM.CAT.IDX(0)
  DIM WEBT.POINTER(0)
  DIM ITEM.NET.PRICES(0)
  DIM ITEM.UNIT.PRICES(0)
  DIM CPN.VALUE(0)

  EXIT SUB

BUILD.CATEGORY.ITEMS:                            ! build item data for category

  CATEGORY.ITEMS = 0                             ! initialize counter
  DIM ITEM.NET.PRICES(WE.ITEM.COUNT)
  DIM ITEM.CAT.IDX(WE.ITEM.COUNT)

  FOR WE.K% = 1 TO WE.ITEM.COUNT                 ! loop through item data
    IF (WE.ITEM.TLOG.INDEX(WE.K%) > 0)           \ if valid Tlog entry
    THEN BEGIN
      REC.DATA$ = WE.ITEM.APL.FILE.DATA$(WE.K%)
      GOSUB PARSE.APL.DATA                       ! parse item record data
      IF (RX.CATEGORY$ = CATEGORY$)              \ if categories match
      THEN BEGIN

        ! Use the base error return flag for VAL function, didn't want to
        ! implement an ON ERROR in this subroutine

        TS.ER.RETURN = -1                        ! base error return flag
        ITM.BEN.QTY = VAL(BENEFITQTY$)

        IF (ITM.BEN.QTY > 0) AND (ITM.BEN.QTY <= RX.QTY) AND \
           TS.ER.RETURN                                      \
        THEN BEGIN                               ! if category isn't decremented

          TS.ER.RETURN = 0                       ! reset base error flag

          ! Now test for subcategory match.
          ! If the subcategories match outright or this is the zero sub category
          ! and the zero sub category flag is set in the UPC/PLU file then
          ! continue.  We should have always reached the zero subcategory last
          ! since we've sorted the category arrays from largest to smallest.

          IF (RX.SUBCATEGORY$ = SUBCATEGORY$) OR \
             ((RX.SUBCATEGORY$ = "000") AND      \
              (ZEROSUBCATFLAG$ <> "00"))         \
          THEN BEGIN                             ! if a valid subcategory

            ! Figure out the item net price which is the total extended price
            ! minus all coupons divided by the quantity.  We'll add items to the
            ! category based on the lowest net price.
            ITEM.NET.PRICE = (WE.ITEM.XPRICE(WE.K%) -                          \
                              WE.TOTAL.COUPON.AMOUNT(WE.K%)) /                 \
                             WE.ITEM.QUANTITY(WE.K%)

            IF (ITEM.NET.PRICE > 0)              \ if not gone negative in price
            THEN BEGIN

              CATEGORY.ITEMS = CATEGORY.ITEMS + 1
              ! save for sorting
              ITEM.NET.PRICES(CATEGORY.ITEMS) = ITEM.NET.PRICE
              ! save index
              ITEM.CAT.IDX(CATEGORY.ITEMS) = WE.K%

              ! Now insert this entry into any previous entries.
              ! We sort from largest net price to smallest when adding items
              FOR I% = (CATEGORY.ITEMS - 1) TO 1 STEP -1
                IF (ITEM.NET.PRICES(I%) < ITEM.NET.PRICES(I% + 1)) \
                THEN BEGIN                       ! if we need to sort
                  SORT.SAVE = ITEM.NET.PRICES(I% + 1)
                  ITEM.NET.PRICES(I% + 1) = ITEM.NET.PRICES(I%)
                  ITEM.NET.PRICES(I%) = SORT.SAVE
                  SORT.SAVE = ITEM.CAT.IDX(I% + 1)
                  ITEM.CAT.IDX(I% + 1) = ITEM.CAT.IDX(I%)
                  ITEM.CAT.IDX(I%) = SORT.SAVE
                ENDIF                            ! if we need to sort
              NEXT I%

            ENDIF                                ! if not gone negative in price
          ENDIF                                  ! if a valid subcategory
        ENDIF                                    ! if category isn't decremented
        TS.ER.RETURN = 0                         ! make sure base flag is reset
      ENDIF                                      ! if categories match
    ENDIF                                        ! if valid Tlog entry
  NEXT WE.K%                                     ! loop through item data

  RETURN                                         ! BUILD.CATEGORY.ITEMS:


SPLIT.UNIT.PRICES:                               ! split up individual prices

  ITEM.UNIT.PRICE.CNT = 0

  L% = LEN(WE.ITEM.UNIT.PRICES$(ITEM.INDEX))
  J% = 0
  K% = 0
  J% = MATCHB(":",WE.ITEM.UNIT.PRICES$(ITEM.INDEX),J% + 1)
  ! first count the number of prices, may not match the quantity
  WHILE J% > 0
    K% = K% + 1
    IF (J% < L%) THEN BEGIN                      ! if we're not on the last one
      J% = MATCHB(":",WE.ITEM.UNIT.PRICES$(ITEM.INDEX),J% + 1)
    ENDIF ELSE BEGIN                             ! else this is the last of it
      J% = 0                                     ! break loop
    ENDIF                                        ! if we're not on the last one
  WEND

  IF (K% > 0) THEN BEGIN                         ! if we have some prices
    ITEM.UNIT.PRICE.CNT = K%
    DIM ITEM.UNIT.PRICES(ITEM.UNIT.PRICE.CNT)
    DIM ITEM.CHILDREN.ARRAY$(ITEM.UNIT.PRICE.CNT)
    A$ = WE.ITEM.UNIT.PRICES$(ITEM.INDEX)
    J% = 0

    FOR I% = 1 TO ITEM.UNIT.PRICE.CNT
      J% = MATCHB(":",A$,1)
      B$ = LEFT$(A$,J% - 1)
      A$ = RIGHT$(A$,LEN(A$) - J%)
      IF (RX.SUBCATEGORY$ = "000") THEN BEGIN    ! if working on zero subcat
        ITEM.CHILDREN.ARRAY$(I%) = MID$(WE.ITEM.0.CHILD.ARRAY$(ITEM.INDEX),I%,1)
        C$ = MID$(WE.ITEM.N.CHILD.ARRAY$(ITEM.INDEX),I%,1)
      ENDIF ELSE BEGIN                           ! else working on nonzero
        ITEM.CHILDREN.ARRAY$(I%) = MID$(WE.ITEM.N.CHILD.ARRAY$(ITEM.INDEX),I%,1)
        C$ = MID$(WE.ITEM.0.CHILD.ARRAY$(ITEM.INDEX),I%,1)
      ENDIF                                      ! if working on zero subca
      TS.ER.RETURN = -1
      ITEM.UNIT.PRICES(I%) = VAL(B$)
      IF TS.ER.RETURN THEN BEGIN                 ! if no error on VAL function
        TS.ER.RETURN = 0                         ! reset error flag
        ! get array flag for this price
        IF (ITEM.CHILDREN.ARRAY$(I%) = "1") OR   \ if item has been processed
           (C$ = "1")                            \
        THEN BEGIN
          ITEM.UNIT.PRICES(I%) = -1              ! set as invalid
        ENDIF                                    ! if item has been processed
      ENDIF ELSE BEGIN                           ! else an error on VAL function
        ITEM.UNIT.PRICES(I%) = -1                ! set price to invalid value
      ENDIF                                      ! if no error on VAL function
    NEXT I%
  ENDIF                                          ! if we have some prices

  RETURN                                         ! SPLIT.UNIT.PRICES:


GET.ITEM.COUPONS:                                ! get all cpns for current item

  CPN.INDEX = 0                                  ! no associated cpns found yet

  IF (WEBT.SAVE <= 0) THEN BEGIN                 ! if no associated cpns saved
    RETURN                                       ! go back
  ENDIF                                          ! if no associated cpns saved

  TS.ER.RETURN = -1                              ! set flag in case VAL error

  ! Get last 8 digits of item code to compare with EM item/coupon association
  CURR.ITEM.UPC = VAL(RIGHT$(UNPACK$(WE.ITEM.CODE$(ITEM.INDEX)),8))

  IF (TS.ER.RETURN = 0) THEN BEGIN               ! if invalid item code
    RETURN                                       ! get out
  ENDIF                                          ! if invalid item code

  TS.ER.RETURN = 0                               ! make sure base flag is reset

  DIM CPN.VALUE(WEBT.SAVE)
  DIM WEBT.POINTER(WEBT.SAVE)

  FOR I% = 1 TO WEBT.SAVE                        ! for all item/cpn associated
    IF (CPN.USED(I%) <> -1) THEN BEGIN           ! if not already used this cpn
      IF (WEBT.IOFF(I%) = WE.ITEM.TLOG.INDEX(ITEM.INDEX))  \
      THEN BEGIN                                 ! if a tlog array index match
        IF (WEBT.IUPC(I%) = CURR.ITEM.UPC)       \ if UPC's match
        THEN BEGIN
          IF (WEBT.CVAL(I%) > 0) THEN BEGIN      ! if this coupon has value
            CPN.INDEX = CPN.INDEX + 1
            CPN.VALUE(CPN.INDEX) = WEBT.CVAL(I%)
            WEBT.POINTER(CPN.INDEX) = I%         ! flag position of this index
          ENDIF ELSE BEGIN                       ! else no value or voided cpn
            IF (WEBT.CVAL(I%) < 0) THEN BEGIN    ! appears to be voided coupon
              FOR J% = CPN.INDEX TO 1 STEP -1    ! loop back to find voided cpn
                IF (WEBT.CUPC(I%) = WEBT.CUPC(WEBT.POINTER(J%))) \
                THEN BEGIN                       ! if coupon item codes match
                  IF (ABS(WEBT.CVAL(I%)) <= ABS(WEBT.CVAL(WEBT.POINTER(J%))))  \
                  THEN BEGIN                     ! if value could contain void
                    IF (ABS(WEBT.CVAL(I%)) <= ABS(CPN.VALUE(J%)))              \
                    THEN BEGIN                   ! if saved could contain void
                      IF (ABS(WEBT.CVAL(I%)) = ABS(CPN.VALUE(J%)))             \
                      THEN BEGIN                 ! if void matches saved
                        ! shift all the entries down
                        FOR K% = J% TO (CPN.INDEX - 1)
                          CPN.VALUE(K%) = CPN.VALUE(K% + 1)
                          WEBT.POINTER(K%) = WEBT.POINTER(K% + 1)
                        NEXT K%
                        CPN.VALUE(CPN.INDEX) = 0 ! reset arrays
                        WEBT.POINTER(CPN.INDEX) = 0
                        CPN.INDEX = CPN.INDEX - 1 ! decrement the count of cpns
                      ENDIF ELSE BEGIN           ! else void is less than saved
                        ! remove voided amount from saved
                        CPN.VALUE(J%) = CPN.VALUE(J%) + WEBT.CVAL(I%)
                      ENDIF                      ! if void matches saved
                      J% = 1                     ! break loop
                    ENDIF                        ! if saved could contain void
                  ENDIF                          ! if value could contain void
                ENDIF                            ! if coupon item codes match
              NEXT J%                            ! loop back to find voided cpn
            ENDIF                                ! appears to be voided coupon
          ENDIF                                  ! if this coupon has value
        ENDIF                                    ! if UPC's match
      ENDIF                                      ! if a tlog array index match
    ENDIF                                        ! if not already used this cpn
  NEXT I%                                        ! for all item/cpn associated

  RETURN                                         ! return from GET.ITEM.COUPONS:


REMOVE.RX.CATEGORY:                              ! remove un-used categories

  IF (WE.I% > WE.NUM.CATEGORIES) OR              \ if invalid entry
     (WE.NUM.CATEGORIES <= 0)                    \
  THEN BEGIN
    RETURN
  ENDIF                                          ! if invalid entry

  ! shift the entries down
  FOR WE.K% = WE.I% TO (WE.NUM.CATEGORIES - 1)
    WE.CATEGORY$(WE.K%) = WE.CATEGORY$(WE.K% + 1)
    WE.CATEGORY.QTY$(WE.K%) = WE.CATEGORY.QTY$(WE.K% + 1)
    WE.CATEGORY.UPD.QTY$(WE.K%) = WE.CATEGORY.UPD.QTY$(WE.K% + 1)
    WE.CATEGORY.UNIT$(WE.K%) = WE.CATEGORY.UNIT$(WE.K% + 1)
    WE.CATEGORY.DESC$(WE.K%) = WE.CATEGORY.DESC$(WE.K% + 1)
    WE.RX.ITEM.ASSOCIATED(WE.K%) = WE.RX.ITEM.ASSOCIATED(WE.K% + 1)
  NEXT WE.K%

  WE.CATEGORY$(WE.NUM.CATEGORIES) = ""
  WE.CATEGORY.QTY$(WE.NUM.CATEGORIES) = ""
  WE.CATEGORY.UPD.QTY$(WE.NUM.CATEGORIES) = ""
  WE.CATEGORY.UNIT$(WE.NUM.CATEGORIES) = ""
  WE.CATEGORY.DESC$(WE.NUM.CATEGORIES) = ""
  WE.RX.ITEM.ASSOCIATED(WE.NUM.CATEGORIES) = 0
  WE.NUM.CATEGORIES = WE.NUM.CATEGORIES - 1      ! reduce number of categories
  WE.I% = WE.I% - 1                              ! reduce index for the loop

  RETURN                                         ! REMOVE.RX.CATEGORY:


EXPAND.WE.CPN.ARRAYS:                            ! expand cpn arrays when needed

  ! Since we can't tell how many coupons we'll have with all the possible
  ! pro-rated coupons, (i.e., these coupons can be split up across individual
  ! items), we added this expansion routine to help prevent array overflow
  ! errors.  These WE.CPN.xxx arrays are initially dimmed to SL.END, but just
  ! in case, we have this function to increase the size of these arrays.
  ! This will probably never be called in the "real" world, but it's possible
  ! the test crew may max these out.

  ! increase the size of our array
  CURR.CPN.ARRAY.SIZE = CURR.CPN.ARRAY.SIZE + SL.END

  DIM I2.TEMP.ARRAY(CURR.CPN.ARRAY.SIZE - SL.END)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    I2.TEMP.ARRAY(TEMP.LOOP) = WE.CPN.TLOG.OFFSET(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM WE.CPN.TLOG.OFFSET(0)
  DIM WE.CPN.TLOG.OFFSET(CURR.CPN.ARRAY.SIZE)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    WE.CPN.TLOG.OFFSET(TEMP.LOOP) = I2.TEMP.ARRAY(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM I4.TEMP.ARRAY(CURR.CPN.ARRAY.SIZE - SL.END)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    I4.TEMP.ARRAY(TEMP.LOOP) = WE.CPN.QUANTITY(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM WE.CPN.QUANTITY(0)
  DIM WE.CPN.QUANTITY(CURR.CPN.ARRAY.SIZE)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    WE.CPN.QUANTITY(TEMP.LOOP) = I4.TEMP.ARRAY(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM I4.TEMP.ARRAY(0)
  DIM I4.TEMP.ARRAY(CURR.CPN.ARRAY.SIZE - SL.END)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    I4.TEMP.ARRAY(TEMP.LOOP) = WE.CPN.VALUE(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM WE.CPN.VALUE(0)
  DIM WE.CPN.VALUE(CURR.CPN.ARRAY.SIZE)

  FOR TEMP.LOOP = 1 TO (CURR.CPN.ARRAY.SIZE - SL.END)
    WE.CPN.VALUE(TEMP.LOOP) = I4.TEMP.ARRAY(TEMP.LOOP)
  NEXT TEMP.LOOP

  DIM I2.TEMP.ARRAY(0)                           ! reclaim heap
  DIM I4.TEMP.ARRAY(0)

  RETURN                                         ! EXPAND.WE.CPN.ARRAYS:


PARSE.APL.DATA:                                  ! break UPC/PLU data into vars

  CHECKDIGIT$ = MID$(REC.DATA$,8,1)
  CATEGORY$ = RIGHT$(STRING$(2,"0") + UNPACK$(MID$(REC.DATA$,9,1)),2)
  SUBCATEGORY$ = RIGHT$(STRING$(3,"0") + UNPACK$(MID$(REC.DATA$,10,2)),3)
  BENEFITQTY$ = UNPACK$(MID$(REC.DATA$,12,3))
  BENEFITUNIT$ = MID$(REC.DATA$,15,10)
  ITEMPRICE$ = UNPACK$(MID$(REC.DATA$,25,3))
  PRICETYPE$ = UNPACK$(MID$(REC.DATA$,28,1))
  DATEEFFECTIVE$ = UNPACK$(MID$(REC.DATA$,29,3))
  DATEEND$ = UNPACK$(MID$(REC.DATA$,32,3))
  ZEROSUBCATFLAG$ = UNPACK$(MID$(REC.DATA$,35,1))

  RETURN                                         ! return from PARSE.APL.DATA:

END SUB                                          ! end GET.PRESCRIPTION.ITEMS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: GET.WIC.ITEMS
! Description: This routine scans the transaction looking for the WIC items that
!              match the customer's prescription.
!*******************************************************************************
SUB GET.WIC.ITEMS(ERR.CODE$,STATE$)

  STRING                               \
         STATE$,                       \ State code returned from API
         ERR.CODE$                     ! error return code from this method

  ! First go through the tlog entries and make a list of all possible wic items
  CALL GET.POSSIBLE.WIC.ITEMS

  IF (WE.ITEM.COUNT <= 0) THEN BEGIN             ! if no valid items found
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no valid items found

  ! Now call the routine to find the items on the PLU/UPC list
  CALL GET.UPC.PLU.FILE.ITEMS(ERR.CODE$,STATE$)

  IF (WE.ITEM.COUNT <= 0) THEN BEGIN             ! if no valid items found
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no valid items found

  ! If we're here then we should have a list of non-voided items sold in the
  ! transaction that are wic'able, (found in the UPC/PLU file).
  ! Now go find the ones that match the customer's prescription.
  CALL GET.PRESCRIPTION.ITEMS

END SUB                                          ! end GET.WIC.ITEMS
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PRINT.BENFT.RDMPT.RECEIPT
! Description: This routine prints everything in the benefit/redemption receipt
!*******************************************************************************
SUB PRINT.BENFT.RDMPT.RECEIPT

  INTEGER*2                            \
            WE.I%                      ! local work variable

  FOR WE.I% = 1 TO WE.BENFT.RDMPT.IDX

    ! Go through and count the number of linefeeds in the format
    A$ = WE.BENFT.RDMPT.FRMT$(WE.I%)
    K% = 0
    I% = MATCHB(CHR$(0AH),A$,1)
    WHILE (I% > 0)
      K% = K% + 1
      I% = MATCHB(CHR$(0AH),A$,I% + 1)
    WEND
    A$ = ""                                      ! cleanup
    TS.SAVPRT$ = WE.BENFT.RDMPT.RCPT$(WE.I%)
    TS.PRT.OPT = 4000H + (100H * K%)
    TS.LINETYPE = 32                             ! WIC EBT print
    TS.LINEDATA = 100                            ! re-printing receipts
    TS.LINEDATA2 = 0
    CALL TSPREC01                                ! print the line

  NEXT WE.I%

END SUB                                          ! end PRINT.BENFT.RDMPT.RECEIPT
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PRINT.BENEFIT.RECEIPT
! Description: This routine prints the Benefit Receipt.  The API WICReadBalance
!              should have been called prior to calling this routine.
!*******************************************************************************
SUB PRINT.BENEFIT.RECEIPT(STATE$,                \
                          PAN$,                  \
                          LAST.DATE$,            \
                          FIRST.DATE$,           \
                          WIC.ITEMS$)

  STRING                               \
         WIC.ITEMS$,                   \ WIC'able items from cust's prescription
         FIRST.DATE$,                  \ first date to spend returned from API
         LAST.DATE$,                   \ last date to spend returned from API
         PAN$,                         \ Personal Account Number
         STATE$                        ! State code returned from API

  INTEGER*2                            \
            WE.I%,WE.K%                ! work variables

  CALL WE.RESET.PRINT.VARS                       ! reset all printing variables

  ! Format the Pan for printing, we need to mask all but digits 13 through 16
!AIO03864 - take the original pan number into consideration when padding.
! WE.PRN.PAN$ = RIGHT$(STRING$(19,"0") + PAN$,19)
! WE.PRN.PAN$ = STRING$(12,"*") + MID$(WE.PRN.PAN$,13,4) + STRING$(3,"*")
  C$ = ""                                        ! clear work variable
  WE.I% = LEN(PAN$)                              ! get Pan length
  IF (WE.I% < 16) THEN BEGIN                     ! if not valid 16 digits
    C$ = RIGHT$(STRING$(16,"0") + PAN$,16)       ! pad with zeroes to 16 digits
    WE.I% = 16                                   ! length is now 16
  ENDIF ELSE BEGIN                               ! else pan is 16 digits or more
    C$ = PAN$                                    ! use pan as is
  ENDIF                                          ! if not valid 16 digits
  C$ = MID$(C$,13,4)                             ! get unmasked digits
  WE.PRN.PAN$ = STRING$(WE.I%,"*")               ! all astericks
  CALL SUBSTR(WE.PRN.PAN$,12,C$,0,LEN(C$))       ! slot in unmasked digits
  ! make sure string size is still 19 characters for printing
  WE.PRN.PAN$ = RIGHT$(STRING$(19," ") + WE.PRN.PAN$,19)
!EIO03864

  ! Format the start and end date to be MM/DD/YYYY
  WE.PRN.START.DATE$ = RIGHT$(STRING$(8,"0") + FIRST.DATE$,8)
  WE.PRN.START.DATE$ = MID$(WE.PRN.START.DATE$,5,2)        \
                     + "/"                                 \
                     + RIGHT$(WE.PRN.START.DATE$,2)        \
                     + "/"                                 \
                     + LEFT$(WE.PRN.START.DATE$,4)

  WE.PRN.END.DATE$ = RIGHT$(STRING$(8,"0") + LAST.DATE$,8)
  WE.PRN.END.DATE$ = MID$(WE.PRN.END.DATE$,5,2)            \
                     + "/"                                 \
                     + RIGHT$(WE.PRN.END.DATE$,2)          \
                     + "/"                                 \
                     + LEFT$(WE.PRN.END.DATE$,4)

  ! print the training mode descriptor if in training mode
  IF TS.TRAINING THEN BEGIN                      ! if training mode
    TS.LINETYPE = 32                             ! WIC EBT print
    TS.LINEDATA = 1                              ! printing benefit receipt
    TS.LINEDATA2 = 1                             ! printing header
    TS.SAVPRT$ = TRAINING.MODE.DESC$             ! training mode descriptor
    TS.PRT.OPT = 4100H                           ! one linefeed
    CALL TSPREC01                                ! print it
  ENDIF                                          ! if training mode

  IF (WIC.ITEMS$ = "") THEN BEGIN                ! if no benefits returned

    IF (WE.BNA.HEADER1.LF > 0) THEN BEGIN        ! if benefits not available lfs
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 1                            ! printing benefit receipt
      TS.LINEDATA2 = 1                           ! printing header
      TS.SAVPRT$ = WE.BNA.HEADER1$
      TS.PRT.OPT = 4000H + (WE.BNA.HEADER1.LF * 100H)
      CALL TSPREC01
    ENDIF                                        ! if benefits not available lfs

    IF (WE.BNA.HEADER2.LF > 0) THEN BEGIN        ! if benefits not available lfs
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 1                            ! printing benefit receipt
      TS.LINEDATA2 = 1                           ! printing header
      TS.SAVPRT$ = WE.BNA.HEADER2$
      TS.PRT.OPT = 4000H + (WE.BNA.HEADER2.LF * 100H)
      CALL TSPREC01
    ENDIF                                        ! if benefits not available lfs

    EXIT SUB

  ENDIF                                          ! if no benefits returned

  ! Print the benefit receipt header

  FOR WE.I% = 1 TO WE.NUM.BENEFIT.HDR%
    IF (WE.BENEFIT.HDR.LF(WE.I%) > 0) THEN BEGIN ! if linefeed set for this line
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 1                            ! printing benefit receipt
      TS.LINEDATA2 = 1                           ! printing header
      TS.SAVPRT$ = WE.BENEFIT.HDR$(WE.I%)
      TS.PRT.OPT = 4000H + (WE.BENEFIT.HDR.LF(WE.I%) * 100H)
      CALL TSPREC01
    ENDIF                                        ! if linefeed set for this line
  NEXT WE.I%

  ! Print all the categories (items) from the prescription

  FOR WE.K% = 1 TO WE.NUM.ORG.CATEGORIES
    WE.PRN.PRSCRPT.QTY$ = WE.ORG.CATEGORY.QTY$(WE.K%)
    WE.PRN.PRSCRPT.UNIT$ = WE.ORG.CATEGORY.UNIT$(WE.K%)
    WE.PRN.PRSCRPT.DESC$ = WE.ORG.CATEGORY.DESC$(WE.K%)
    FOR WE.I% = 1 TO WE.NUM.BENEFIT.ITM%
      IF (WE.BENEFIT.ITM.LF(WE.I%) > 0)          \ if linefeed set for this line
      THEN BEGIN
        TS.LINETYPE = 32                         ! WIC EBT print
        TS.LINEDATA = 1                          ! printing benefit receipt
        TS.LINEDATA2 = 2                         ! printing items
        TS.SAVPRT$ = WE.BENEFIT.ITM$(WE.I%)
        TS.PRT.OPT = 4000H + (WE.BENEFIT.ITM.LF(WE.I%) * 100H)
        CALL TSPREC01
      ENDIF                                      ! if linefeed set for this line
    NEXT WE.I%
  NEXT WE.K%

  ! Print the benefit trailer.

  ! Send a linefeed in first to separate the item lines
  TS.SAVPRT$ = CHR$(0AH)                         ! linefeed
  TS.PRT.OPT = 4000H
  TS.LINETYPE = 32                               ! WIC EBT print
  TS.LINEDATA = 1                                ! printing benefit receipt
  TS.LINEDATA2 = 3                               ! printing trailer
  CALL TSPREC01                                  ! print the blank

  FOR WE.I% = 1 TO WE.NUM.BENEFIT.TRL%
    IF (WE.BENEFIT.TRL.LF(WE.I%) > 0) THEN BEGIN ! if linefeed set for this line
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 1                            ! printing benefit receipt
      TS.LINEDATA2 = 3                           ! printing trailer
      TS.SAVPRT$ = WE.BENEFIT.TRL$(WE.I%)
      TS.PRT.OPT = 4000H + (WE.BENEFIT.TRL.LF(WE.I%) * 100H)
      CALL TSPREC01
    ENDIF                                        ! if linefeed set for this line
  NEXT WE.I%

END SUB                                          ! end PRINT.BENEFIT.RECEIPT
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PRINT.REDEMPTION.RECEIPT
! Description: This routine prints the Redemption Receipt.
!              The API WICReadBalance should have been called prior to calling
!              this routine and the method PRINT.BENEFIT.RECEIPT should have
!              been successfully executed. Various variables/arrays would have
!              been set up in PRINT.BENEFIT.RECEIPT
!*******************************************************************************
SUB PRINT.REDEMPTION.RECEIPT(STATE$,PAN$)

  STRING                               \
         RX.SUBCATEGORY$,              \ subcategory of prescription
         PAN$,                         \ Personal Account Number
         STATE$                        ! State code returned from API

  INTEGER*2                            \
            ITEM.COUNT,                \ count of items for category
            ITEM.OFFSETS(1),           \ offsets of items associated with cat
            CPN.INDEX,                 \ index into coupon work arrays
            CPN.TLOG.OFFSET(1),        \ offset into tlog for coupons
            WE.I%,WE.J%,WE.K%,WE.L%    ! work variables

  INTEGER*4                            \
            CPN.QUANTITY(1),           \ quantity of coupons
            CPN.VALUE(1)               ! value of coupons

  ! Print the redemption receipt header

  FOR WE.I% = 1 TO WE.NUM.REDEMPT.HDR%
    IF (WE.REDEMPT.HDR.LF(WE.I%) > 0) THEN BEGIN ! if linefeed set for this line
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 2                            ! printing redemption receipt
      TS.LINEDATA2 = 1                           ! printing header
      TS.SAVPRT$ = WE.REDEMPT.HDR$(WE.I%)
      TS.PRT.OPT = 4000H + (WE.REDEMPT.HDR.LF(WE.I%) * 100H)
      CALL TSPREC01
    ENDIF                                        ! if linefeed set for this line
  NEXT WE.I%

  IF (WE.ITEM.COUNT <= 0) THEN BEGIN             ! if no items were found
    WE.ITEM.COUNT = 0                            ! make sure this is zero
    WE.NUM.CATEGORIES = 0                        ! reset category data
    DIM WE.CATEGORY$(0)
    DIM WE.CATEGORY.QTY$(0)
    DIM WE.CATEGORY.UPD.QTY$(0)
    DIM WE.CATEGORY.UNIT$(0)
    DIM WE.CATEGORY.DESC$(0)
  ENDIF                                          ! if no items were found

  DIM WE.ITEM.CPN.0.TLOG$(WE.ITEM.COUNT)         ! zero cat cpn tlog strings
  DIM WE.ITEM.CPN.N.TLOG$(WE.ITEM.COUNT)         ! nonzero cat cpn tlog strings
  FOR WE.J% = 1 TO WE.NUM.CATEGORIES             ! for all categories used
    IF (WE.CATEGORY.HAS.ITEMS(WE.J%) = -1)       \ if prescription has items
    THEN BEGIN
      IF (WE.REDEMPT.ITM.LF(1) > 0) THEN BEGIN   ! if linefeed set for this line
        WE.PRN.DEBIT.QTY$ = FORMAT.UNIT.QTY$(STR$(WE.CATEGORY.DEBIT.QTY(WE.J%)))
        WE.PRN.PRSCRPT.UNIT$ = STRIP.BLANKS$(WE.CATEGORY.UNIT$(WE.J%))
        WE.PRN.PRSCRPT.DESC$ = STRIP.BLANKS$(WE.CATEGORY.DESC$(WE.J%))
        TS.LINETYPE = 32                         ! WIC EBT print
        TS.LINEDATA = 2                          ! printing redemption receipt
        TS.LINEDATA2 = 2                         ! printing item line
        TS.SAVPRT$ = WE.REDEMPT.ITM$(1)
        TS.PRT.OPT = 4000H + (WE.REDEMPT.ITM.LF(1) * 100H)
        CALL TSPREC01
      ENDIF ELSE BEGIN                           ! else no linefeed set
        WE.PRN.DEBIT.QTY$ = ""
        WE.PRN.PRSCRPT.UNIT$ = ""
        WE.PRN.PRSCRPT.DESC$ = ""
      ENDIF                                      ! if linefeed set for this line
      RX.SUBCATEGORY$ = RIGHT$(WE.CATEGORY$(WE.J%),3)
      A$ = WE.CATEGORY.ITEM.PTR$(WE.J%)          ! extract item pointers
      GOSUB EXTRACT.CATEGORY.ITEMS               ! get items for this category
      FOR WE.L% = 1 TO ITEM.COUNT                ! for all WIC items found
        WE.K% = ITEM.OFFSETS(WE.L%)              ! put offset to work variable
        IF (WE.ITEM.TLOG.INDEX(WE.K%) > 0)       \ if valid Tlog entry
        THEN BEGIN
          WE.PRN.ITEM.DESC$ = GET.PRINT.BUFF.ITEM.DESC$(                       \
                                                      WE.ITEM.TLOG.INDEX(WE.K%))
          WE.PRN.ITEM.UNIT.DESC$ = STRIP.BLANKS$(WE.ITEM.UNIT.DESC$(WE.K%))
!AIO01214
!         CALL FORMAT.AMOUNT(WE.ITEM.PRICE(WE.K%))
          IF (RX.SUBCATEGORY$ = "000")           \ if zero category
          THEN BEGIN
            CALL FORMAT.AMOUNT(WE.ITEM.0.COST(WE.K%))
          ENDIF ELSE BEGIN                       ! else not zero category
            CALL FORMAT.AMOUNT(WE.ITEM.N.COST(WE.K%))
          ENDIF                                  ! if zero category
!EIO01214
          WE.PRN.ITEM.PRICE$ = TS.TEMP1$
          IF (RX.SUBCATEGORY$ <> "000")          \ if not the zero category
          THEN BEGIN
            IF (WE.ITEM.N.RX.INDEX(WE.K%) > 0)   \ if nonzero category data
            THEN BEGIN
              WE.PRN.ITEM.QTY$ = STR$(WE.ITEM.N.PURCH.QTY(WE.K%))
              IF (WE.REDEMPT.ITM.LF(2) > 0)      \ if linefeed set for this line
              THEN BEGIN
                TS.LINETYPE = 32                 ! WIC EBT print
                TS.LINEDATA = 2                  ! printing redemption receipt
                TS.LINEDATA2 = 2                 ! printing item line
                TS.SAVPRT$ = WE.REDEMPT.ITM$(2)
                TS.PRT.OPT = 4000H + (WE.REDEMPT.ITM.LF(2) * 100H)
                CALL TSPREC01
              ENDIF                              ! if linefeed set for this line

              ! Now we need to print all needed coupon lines (line 3) and to
              ! handle the tlog data for these coupons

              WE.ITEM.CPN.N.TLOG$(WE.K%) = ""    ! reset coupon string
              A$ = WE.ITEM.CPN.N.POINTER$(WE.K%)
              IF (A$ <> "") THEN BEGIN           ! if coupon data saved
                GOSUB EXTRACT.COUPON.DATA        ! get the coupon data
                IF (CPN.INDEX > 0) THEN BEGIN    ! if coupon data extracted
                  GOSUB PRINT.COUPON.LINES       ! print cpn info for category
                  GOSUB BUILD.CPN.TLOG.DATA      ! build the coupon data
                  WE.ITEM.CPN.N.TLOG$(WE.K%) = A$ ! data returned in work var
                ENDIF                            ! if coupon data extracted
              ENDIF                              ! if coupon data saved
            ENDIF                                ! if nonzero category data
          ENDIF ELSE BEGIN                       ! else must be zero category
            IF (WE.ITEM.0.RX.INDEX(WE.K%) > 0)   \ if zero category data
            THEN BEGIN
              WE.PRN.ITEM.QTY$ = STR$(WE.ITEM.0.PURCH.QTY(WE.K%))
              IF (WE.REDEMPT.ITM.LF(2) > 0)      \ if linefeed set for this line
              THEN BEGIN
                TS.LINETYPE = 32                 ! WIC EBT print
                TS.LINEDATA = 2                  ! printing redemption receipt
                TS.LINEDATA2 = 2                 ! printing item line
                TS.SAVPRT$ = WE.REDEMPT.ITM$(2)
                TS.PRT.OPT = 4000H + (WE.REDEMPT.ITM.LF(2) * 100H)
                CALL TSPREC01
              ENDIF                              ! if linefeed set for this line

              ! Now we need to print all needed coupon lines (line 3) and to
              ! handle the tlog data for these coupons

              WE.ITEM.CPN.0.TLOG$(WE.K%) = ""    ! reset coupon string
              A$ = WE.ITEM.CPN.0.POINTER$(WE.K%)
              IF (A$ <> "") THEN BEGIN           ! if coupon data saved
                GOSUB EXTRACT.COUPON.DATA        ! get the coupon data
                IF (CPN.INDEX > 0) THEN BEGIN    ! if coupon data extracted
                  GOSUB PRINT.COUPON.LINES       ! print cpn info for category
                  GOSUB BUILD.CPN.TLOG.DATA      ! build the coupon data
                  WE.ITEM.CPN.0.TLOG$(WE.K%) = A$ ! data returned in work var
                ENDIF                            ! if coupon data extracted
              ENDIF                              ! if coupon data saved
            ENDIF                                ! if zero category data
          ENDIF                                  ! if not the zero category
          WE.PRN.DEBIT.QTY$ = ""                 ! reset print variables
          WE.PRN.ITEM.DESC$ = ""
          WE.PRN.ITEM.UNIT.DESC$ = ""
          WE.PRN.ITEM.PRICE$ = ""
          WE.PRN.PRSCRPT.UNIT$ = ""
          WE.PRN.PRSCRPT.DESC$ = ""
          WE.PRN.ITEM.QTY$ = ""
        ENDIF                                    ! if valid Tlog entry
      NEXT WE.L%                                 ! for all WIC items found
      ! Send a linefeed in to separate the item lines
      TS.SAVPRT$ = CHR$(0AH)                     ! linefeed
      TS.PRT.OPT = 4000H
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 2                            ! printing redemption receipt
      TS.LINEDATA2 = 2                           ! printing item line
      CALL TSPREC01                              ! print the blank
    ENDIF                                        \ if prescription has items
  NEXT WE.J%                                     ! for all categories used

  ! Print the redemption trailer.

  FOR WE.I% = 1 TO WE.NUM.REDEMPT.TRL%
    IF (WE.REDEMPT.TRL.LF(WE.I%) > 0) THEN BEGIN ! if linefeed set for this line
      TS.LINETYPE = 32                           ! WIC EBT print
      TS.LINEDATA = 2                            ! printing redemption receipt
      TS.LINEDATA2 = 3                           ! printing trailer
      TS.SAVPRT$ = WE.REDEMPT.TRL$(WE.I%)
      TS.PRT.OPT = 4000H + (WE.REDEMPT.TRL.LF(WE.I%) * 100H)
      CALL TSPREC01
    ENDIF                                        ! if linefeed set for this line
  NEXT WE.I%

  RX.SUBCATEGORY$ = ""                           ! cleanup
  DIM ITEM.OFFSETS(0)
  DIM CPN.TLOG.OFFSET(0)
  DIM CPN.QUANTITY(0)
  DIM CPN.VALUE(0)

  EXIT SUB

EXTRACT.CATEGORY.ITEMS:

  ! Note: item pointer info was first dumped into work variable A$

  ITEM.COUNT = 0                                 ! assume no valid data

  L% = LEN(A$)
  J% = 0
  K% = 0
  J% = MATCHB(":",A$,J% + 1)

  ! first count the number of indexes
  WHILE J% > 0
    K% = K% + 1
    IF (J% < L%) THEN BEGIN                      ! if we're not on the last one
      J% = MATCHB(":",A$,J% + 1)
    ENDIF ELSE BEGIN                             ! else this is the last of it
      J% = 0                                     ! break loop
    ENDIF                                        ! if we're not on the last one
  WEND

  IF (K% > 0) THEN BEGIN                         ! if we have index into arrays
    DIM ITEM.OFFSETS(K%)
    FOR I% = 1 TO K%
      J% = MATCHB(":",A$,1)
      B$ = LEFT$(A$,J% - 1)
      A$ = RIGHT$(A$,LEN(A$) - J%)               ! truncate data
      TS.ER.RETURN = -1
      M% = VAL(B$)                               ! get value of index
      IF TS.ER.RETURN THEN BEGIN                 ! if no error on VAL statement
        TS.ER.RETURN = 0                         ! reset error
        IF (M% <= WE.ITEM.COUNT) AND             \ if valid entry
           (M% > 0)                              \
        THEN BEGIN
          ITEM.COUNT = ITEM.COUNT + 1
          ITEM.OFFSETS(ITEM.COUNT) = M%
        ENDIF                                    ! if valid entry
      ENDIF                                      ! if no error on VAL statement
    NEXT I%
  ENDIF                                          ! if we have index into arrays

  RETURN                                         ! EXTRACT.CATEGORY.ITEMS:


EXTRACT.COUPON.DATA:

  ! Note: cpn pointer info was first dumped into work variable A$

  CPN.INDEX = 0                                  ! assume no valid data

  L% = LEN(A$)
  J% = 0
  K% = 0
  J% = MATCHB(":",A$,J% + 1)

  ! first count the number of indexes
  WHILE J% > 0
    K% = K% + 1
    IF (J% < L%) THEN BEGIN                      ! if we're not on the last one
      J% = MATCHB(":",A$,J% + 1)
    ENDIF ELSE BEGIN                             ! else this is the last of it
      J% = 0                                     ! break loop
    ENDIF                                        ! if we're not on the last one
  WEND

  IF (K% > 0) THEN BEGIN                         ! if we have index into arrays
    DIM CPN.TLOG.OFFSET(K%)
    DIM CPN.QUANTITY(K%)
    DIM CPN.VALUE(K%)
    FOR I% = 1 TO K%
      J% = MATCHB(":",A$,1)
      B$ = LEFT$(A$,J% - 1)
      A$ = RIGHT$(A$,LEN(A$) - J%)               ! truncate data
      TS.ER.RETURN = -1
      M% = VAL(B$)                               ! get value of index
      IF TS.ER.RETURN THEN BEGIN                 ! if no error on VAL statement
        TS.ER.RETURN = 0                         ! reset error
        IF (M% <= WE.CPN.COUNT) AND              \ if valid entry
           (M% > 0)                              \
        THEN BEGIN
          CPN.INDEX = CPN.INDEX + 1
          CPN.TLOG.OFFSET(CPN.INDEX) = WE.CPN.TLOG.OFFSET(M%)
          CPN.QUANTITY(CPN.INDEX) = WE.CPN.QUANTITY(M%)
          CPN.VALUE(CPN.INDEX) = WE.CPN.VALUE(M%)
        ENDIF                                    ! if valid entry
      ENDIF                                      ! if no error on VAL statement
    NEXT I%
  ENDIF                                          ! if we have index into arrays

  RETURN                                         ! EXTRACT.COUPON.DATA:


PRINT.COUPON.LINES:

  IF (WE.REDEMPT.ITM.LF(3) <= 0) THEN BEGIN      ! if no coupon line linefeeds
    RETURN                                       ! just go back
  ENDIF                                          ! if no coupon line linefeeds

  FOR WE.I% = 1 TO CPN.INDEX
    IF (CPN.TLOG.OFFSET(WE.I%) = -1) THEN BEGIN  ! if transaction discount
      WE.PRN.CPN.DESC$ = TRNX.DISCOUNT.DESC$
    ENDIF ELSE BEGIN                             ! else not transaction discount
      WE.PRN.CPN.DESC$ = GET.PRINT.BUFF.ITEM.DESC$(CPN.TLOG.OFFSET(WE.I%))
    ENDIF                                        ! if transaction discount
    WE.PRN.CPN.QUANTITY$ = STR$(CPN.QUANTITY(WE.I%))
    CALL FORMAT.AMOUNT(-CPN.VALUE(WE.I%))        ! make value negative for print
    WE.PRN.CPN.PRICE$ = TS.TEMP1$
    TS.LINETYPE = 32                             ! WIC EBT print
    TS.LINEDATA = 2                              ! printing redemption receipt
    TS.LINEDATA2 = 2                             ! printing item line
    TS.SAVPRT$ = WE.REDEMPT.ITM$(3)
    TS.PRT.OPT = 4000H + (WE.REDEMPT.ITM.LF(3) * 100H)
    CALL TSPREC01
  NEXT WE.I%

  RETURN                                         ! PRINT.COUPON.LINES:


BUILD.CPN.TLOG.DATA:

  ! Note: Any transaction discounts are not put into the Tlog data.
  !       This data is primarily used to help prevent WIC'able coupons from
  !       being voided on recoveries.  The Tlog offset for transaction
  !       discounts are set to -1 previously to flag these unique entries.

  A$ = ""                                        ! reset work string

  ! first go through and build quantities of similar tlog entries
  FOR I% = 1 TO (CPN.INDEX - 1)
    IF (CPN.TLOG.OFFSET(I%) > 0) THEN BEGIN      ! if not reset
      FOR K% = (I% + 1) TO CPN.INDEX
        IF (CPN.TLOG.OFFSET(K%) > 0) THEN BEGIN  ! if not reset
          IF (CPN.TLOG.OFFSET(I%) = CPN.TLOG.OFFSET(K%))  \
          THEN BEGIN                             ! if offsets match
            CPN.QUANTITY(I%) = CPN.QUANTITY(I%) + CPN.QUANTITY(K%)
            CPN.TLOG.OFFSET(K%) = 0
          ENDIF                                  ! if offsets match
        ENDIF                                    ! if not reset
      NEXT K%
    ENDIF                                        ! if not reset
  NEXT I%

  ! now build a string of the remaining entries
  FOR I% = 1 TO CPN.INDEX
    IF (CPN.TLOG.OFFSET(I%) > 0) THEN BEGIN      ! if not reset
      A$ = A$                                                   \
         + RIGHT$(STRING$(4,"0") + STR$(CPN.TLOG.OFFSET(I%)),4)  \
         + RIGHT$(STRING$(4,"0") + STR$(CPN.QUANTITY(I%)),4)
    ENDIF                                        ! if not reset
  NEXT I%

  RETURN                                         ! BUILD.CPN.TLOG.DATA:

END SUB                                          ! end PRINT.REDEMPTION.RECEIPT
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: PROCESS.PRESCRIPTION
! Description: Process the prescription data just read from the SmartCard.
!*******************************************************************************
SUB PROCESS.PRESCRIPTION(ERR.CODE$,              \
                         STATE$,                 \
                         PAN$,                   \
                         LAST.DATE$,             \
                         FIRST.DATE$,            \
                         WIC.ITEMS$)

  STRING                               \
         WIC.ITEMS$,                   \ WIC'able items returned from API
         FIRST.DATE$,                  \ first date to spend returned from API
         LAST.DATE$,                   \ last date to spend returned from API
         PAN$,                         \ Personal Account Number
         STATE$,                       \ State code returned from API
         ERR.CODE$                     ! error return code from this method

  INTEGER*2                            \
            SAVE.POST.PRINT,           \ save 4610 var EP.ETO.PRINT.POST.TRANS
            SAVE.DUP.NUMBER            ! save 4610 var EP.ETO.DUPLICATE.NUMBER

  ERR.CODE$ = "0000"                             ! assume there will be no error
  WE.ITEMS.TO.DECREMENT$ = ""                    ! items we may be decrementing

  CALL PARSE.OPTION.DATA(ERR.CODE$,STATE$)       ! dump all option data

  IF (ERR.CODE$ <> "0000") THEN BEGIN            ! if unable to parse opt data
    EXIT SUB
  ENDIF

  ! call routine to build prescription data
  CALL BUILD.PRESCRIPTION.DATA(STATE$,WIC.ITEMS$)

  IF (WE.NUM.CATEGORIES <= 0) THEN BEGIN         ! if invalid cats returned
    ERR.CODE$ = "50000"
    EXIT SUB
  ENDIF                                          ! if invalid cats returned

  CALL GET.WIC.ITEMS(ERR.CODE$,STATE$)           ! routine to find Rx items

  IF (ERR.CODE$ <> "0000") THEN BEGIN            ! if error returned
    EXIT SUB
  ENDIF                                          ! if error return

  SAVE.POST.PRINT = EP.ETO.PRINT.POST.TRANS      ! save post-printing flag
  EP.ETO.PRINT.POST.TRANS = 0                    ! turn off post-printing

  FLAG.4610.NO.BUFFERING = -1                    ! flag so receipts not buffered

  CALL DISABLE.BUFFERING                         ! disable 4610 buffering

  CALL DO.GUILLOTINE.CUT                         ! cut the receipt

  DIM WE.BENFT.RDMPT.RCPT$(SIZE.OF.CR.BUFFER)    ! array buffer to hold lines
  DIM WE.BENFT.RDMPT.FRMT$(SIZE.OF.CR.BUFFER)    ! array buffer to hold formats
  WE.BENFT.RDMPT.IDX = 0                         ! reset index
  WE.BUFFER.BR.RECEIPT = -1                      ! flag buffer these print lines

  CALL PRINT.BENEFIT.RECEIPT(STATE$,             \
                             PAN$,               \
                             LAST.DATE$,         \
                             FIRST.DATE$,        \
                             WIC.ITEMS$)

  CALL PRINT.REDEMPTION.RECEIPT(STATE$,PAN$)

  WE.BUFFER.BR.RECEIPT = 0                       ! reset flag WE buffer flag

  CALL DO.GUILLOTINE.CUT                         ! cut the receipt

  EP.ETO.PRINT.POST.TRANS = SAVE.POST.PRINT      ! restore post print flag

  ! Reprint the customer receipt if we're not post-printing
  IF (EP.ETO.PRINT.POST.TRANS = 0) THEN BEGIN    ! if we're not post-printing
    SAVE.DUP.NUMBER = EP.ETO.DUPLICATE.NUMBER    ! save the 4610 variable
    EP.ETO.DUPLICATE.NUMBER = 1                  ! only print one receipt
    PRT4610.REPRINT = -1                         ! flag re-printing the receipt
    CALL REPRINT.RECEIPT                         ! reprint the receipt
    PRT4610.REPRINT = 0                          ! reset flag
    EP.ETO.DUPLICATE.NUMBER = SAVE.DUP.NUMBER    ! restore 4610 variable
  ENDIF                                          ! if we're not post-printing

  FLAG.4610.NO.BUFFERING = 0                     ! reset 4610 flag

  CALL ENABLE.BUFFERING                          ! allow 4610 code to buffer

  ERR.CODE$ = "0000"                             ! flag procedure processed

  IF (WE.ITEM.COUNT <= 0) THEN BEGIN             ! if no WIC'able items found
    WE.ITEMS.TO.DECREMENT$ = ""                  ! make sure this reset
  ENDIF                                          ! if no WIC'able items found

  IF (WE.ITEMS.TO.DECREMENT$ = "") THEN BEGIN    ! if no items to decrement
    ERR.CODE$ = "60000"                          ! flag unique error
  ENDIF                                          ! if no items to decrement

END SUB                                          ! end PROCESS.PRESCRIPTION
!*******************************************************************************


!*******************************************************************************
! Function Name: BUILD.TRAINING.WIC.ITEMS$
! Description: This function builds the decremented item list when in training
!              mode.  The PinPad does not do any card decrementing when in
!              training mode so we simulate the entry by building our own item
!              string after subtracting the decremented amounts.
!*******************************************************************************
FUNCTION BUILD.TRAINING.WIC.ITEMS$(WIC.ITEMS$,ITEMS.TO.DECREMENT$)

  STRING                               \
         NEW.WIC.ITEMS$,               \ the new built wic string
         WIC.ITEMS$,                   \ original prescription read from card
         ITEMS.TO.DECREMENT$,          \ item list we have to decrement
         BUILD.TRAINING.WIC.ITEMS$     ! function return

  INTEGER*4                            \
            WIC.ITEMS(2),              \ wic item data in integer format
            DEC.ITEMS(2),              \ decremented item data in integer format
            WE.X%                      ! work variable


  BUILD.TRAINING.WIC.ITEMS$ = WIC.ITEMS$         ! set function to passed items

  ! Calculate how many are in each record
  N% = INT%(LEN(WIC.ITEMS$) / WE.LEN.WIC.DATA)
  M% = INT%(LEN(ITEMS.TO.DECREMENT$) / WE.LEN.WIC.DATA)

  IF (N% <= 0) OR (M% <= 0) THEN BEGIN           ! if invalid data
    EXIT FUNCTION                                ! get out
  ENDIF                                          ! if invalid data

  ! build original wic item data structure
  DIM WIC.ITEMS(1,N%)
  K% = 1                                         ! use for parsing through data
  FOR I% = 1 TO N%
    A$ = MID$(WIC.ITEMS$,K%,10)                  ! get category record
    TS.ER.RETURN = -1                            ! return on an error
    WE.X% = VAL(LEFT$(A$,5))                     ! test that cat is numeric
    IF TS.ER.RETURN THEN BEGIN                   ! if no error
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else an error
      EXIT FUNCTION                              ! get out, bad data
    ENDIF                                        ! if no error
    WIC.ITEMS(0,I%) = WE.X%                      ! save category/subcat data
    TS.ER.RETURN = -1                            ! return on an error
    WE.X% = VAL(RIGHT$(A$,5))                    ! test that qty is numeric
    IF TS.ER.RETURN THEN BEGIN                   ! if no error
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else an error
      EXIT FUNCTION                              ! get out, bad data
    ENDIF                                        ! if no error
    WIC.ITEMS(1,I%) = WE.X%                      ! save category quantity data
    K% = K% + WE.LEN.WIC.DATA                    ! move pointer to next record
  NEXT I%

  ! build decremented item data structure
  DIM DEC.ITEMS(1,M%)
  K% = 1                                         ! use for parsing through data
  FOR I% = 1 TO M%
    A$ = MID$(ITEMS.TO.DECREMENT$,K%,10)         ! get category record
    TS.ER.RETURN = -1                            ! return on an error
    WE.X% = VAL(LEFT$(A$,5))                     ! test that cat is numeric
    IF TS.ER.RETURN THEN BEGIN                   ! if no error
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else an error
      EXIT FUNCTION                              ! get out, bad data
    ENDIF                                        ! if no error
    DEC.ITEMS(0,I%) = WE.X%                      ! save category/subcat data
    TS.ER.RETURN = -1                            ! return on an error
    WE.X% = VAL(RIGHT$(A$,5))                    ! test that qty is numeric
    IF TS.ER.RETURN THEN BEGIN                   ! if no error
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else an error
      EXIT FUNCTION                              ! get out, bad data
    ENDIF                                        ! if no error
    DEC.ITEMS(1,I%) = WE.X%                      ! save category quantity data
    K% = K% + WE.LEN.WIC.DATA                    ! move pointer to next record
  NEXT I%

  ! now go through and decrement the quantities of the wic items
  FOR I% = 1 TO N%
    FOR K% = 1 TO M%
      IF (WIC.ITEMS(0,I%) = DEC.ITEMS(0,K%))     \ if matching cat/subcat
      THEN BEGIN
        WIC.ITEMS(1,I%) = WIC.ITEMS(1,I%) - DEC.ITEMS(1,K%)
        IF (WIC.ITEMS(1,I%) < 0) THEN BEGIN      ! make sure no negative number
          WIC.ITEMS(1,I%) = 0                    ! set to zero
        ENDIF                                    ! make sure no negative number
      ENDIF                                      ! if matching cat/subcat
    NEXT K%
  NEXT I%

  NEW.WIC.ITEMS$ = ""                            ! reset work string

  ! build new wic items
  FOR I% = 1 TO N%
    NEW.WIC.ITEMS$ = NEW.WIC.ITEMS$                                       \
                   + RIGHT$(STRING$(5,"0") + STR$(WIC.ITEMS(0,I%)),5)     \
                   + RIGHT$(STRING$(5,"0") + STR$(WIC.ITEMS(1,I%)),5)
  NEXT I%

  BUILD.TRAINING.WIC.ITEMS$ = NEW.WIC.ITEMS$     ! return the updated string

  A$ = ""                                        ! cleanup
  NEW.WIC.ITEMS$ = ""
  WIC.ITEMS$ = ""
  ITEMS.TO.DECREMENT$ = ""
  DIM WIC.ITEMS(0,0)
  DIM DEC.ITEMS(0,0)

END FUNCTION                                     ! end BUILD.TRAINING.WIC.ITEMS$
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WE.REMOVE.TAXES
! Description: Remove Wic'able amounts from the tax buckets.
!*******************************************************************************
SUB WE.REMOVE.TAXES

  FOR I% = 1 TO 4                                ! for each of the 4 tax plans
    IF (WE.TAX(I%) > 0)                          \ if this tax plan has value
    THEN BEGIN
      TS.TOTALS(I%,0,0) = TS.TOTALS(I%,0,0) - WE.TAX(I%)
      IF (TS.TOTALS(I%,0,0) < 0) THEN BEGIN
        TS.TOTALS(I%,0,0) = 0                    ! don't drive negative
      ENDIF
      TS.TOTALS(I%,1,0) = TS.TOTALS(I%,1,0) - WE.TAX(I%)
      IF (TS.TOTALS(I%,1,0) < 0) THEN BEGIN
        TS.TOTALS(I%,1,0) = 0                    ! don't drive negative
      ENDIF
      TS.TOTALS(I%,1,1) = TS.TOTALS(I%,1,1) - WE.TAX(I%)
      IF (TS.TOTALS(I%,1,1) < 0) THEN BEGIN
        TS.TOTALS(I%,1,1) = 0                    ! don't drive negative
      ENDIF
    ENDIF                                        ! if this tax plan has value
  NEXT I%                                        ! for each of the 4 tax plans

END SUB                                          ! end WE.REMOVE.TAXES
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.INITIALIZATION
! Description: Initialize the WIC EBT feature.
!
! Note: If this feature is active and there are agencies defined, then the
!       Value Pack WIC feature will be disabled.
!*******************************************************************************
SUB WIC.EBT.INITIALIZATION PUBLIC

  STRING                               \
         ERR.CODE$                     ! error code for subroutine call

  INTEGER*2                            \
            WE.I%                      ! local work variable

  CALL EAMTRACE(4557H)                           ! trace "WE"

  WE.SESSION.NUM1 = 71                           ! Set up default session num
  WE.NUM.BENEFIT.HDR% = 6                        ! curr num of benefit headers
  WE.NUM.BENEFIT.ITM% = 1                        ! curr num of benefit itm lines
  WE.NUM.BENEFIT.TRL% = 3                        ! curr num of benefit trailers
  WE.NUM.REDEMPT.HDR% = 6                        ! curr num of redemption hdrs
  WE.NUM.REDEMPT.ITM% = 3                        ! curr num of redemption itm
  WE.NUM.REDEMPT.TRL% = 2                        ! curr num of redemption trls
  WE.LEN.WIC.DATA = 10                           ! curr len of each cat data
  WE.PINPAD.INIT.ERR.CNT = 0                     ! number of times err occurred

  CALL WE.RESET.OPT.VARS                         ! reset feature option vars

  ! reset feature descriptors for display
  PROCESSING.REQUEST.MSG$ = ""
  WAIT.ON.CUSTOMER.MSG$ = ""
  TRNX.DISCOUNT.DESC$ = ""
  CAT.DESC.NOT.FOUND$ = ""
  TRAINING.MODE.DESC$ = ""

  ! If not using 4610's, then turn off feature.
  IF (PRT4610.ENABLE = 0) THEN BEGIN             ! if not a 4610 printer system
    TO.WE.ENABLED = 0                            ! disable feature
    EXIT SUB                                     ! get out
  ENDIF                                          ! if not a 4610 printer system

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT disabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT disabled

  CALL WE.READ.OPTIONS                           ! read opts from EAMWEOPT.DAT

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT disabled
    CALL WE.RESET.OPT.VARS                       ! reset the option variables
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT disabled

  ! Loop through to grab the state codes from the data just read from options
  ! and the active flag.  If the state is active, then add to string to pass to
  ! pinpad API.  This string should look such as TXARLA for Texas, Arkansas and
  ! Louisiana.  We'll also convert the print data for each agency from null to
  ! blanks to prevent printer errors
  FOR WE.I% = 1 TO WE.NUMBER.OF.AGENCIES

    ! grab the state code and Active flag
    A$ = LEFT$(WE.AGENCY.MAIN.OPTION$(WE.I%) + STRING$(3," "),3)

    IF (RIGHT$(A$,1) = "Y") THEN BEGIN           ! if State is active

      ! add State code to state string
      WE.STATE.CODE$ = WE.STATE.CODE$ + LEFT$(A$,2)

      ! Convert any null characters in the printing options to blanks to prevent
      ! print errors.  The option program does not enforce blanks instead of
      ! file nulls.  For each of these arrays, the first 38 characters are print
      ! data but the last two are linefeed and fonts so we don't want to
      ! translate those since they could be packed zero.

      ! Check Agency Benefit header
      WE.AGENCY.BENEFIT.HDR$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.BENEFIT.HDR$(WE.I%), \
                                                WE.NUM.BENEFIT.HDR%)

      ! Check Agency Benefit items
      WE.AGENCY.BENEFIT.ITM$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.BENEFIT.ITM$(WE.I%), \
                                                WE.NUM.BENEFIT.ITM%)

      ! Check Agency Benefit trailer
      WE.AGENCY.BENEFIT.TRL$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.BENEFIT.TRL$(WE.I%), \
                                                WE.NUM.BENEFIT.TRL%)

      ! Check Agency Redemption header
      WE.AGENCY.REDEMPT.HDR$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.REDEMPT.HDR$(WE.I%), \
                                                WE.NUM.REDEMPT.HDR%)

      ! Check Agency Redemption items
      WE.AGENCY.REDEMPT.ITM$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.REDEMPT.ITM$(WE.I%), \
                                                WE.NUM.REDEMPT.ITM%)

      ! Check Agency Redemption trailer
      WE.AGENCY.REDEMPT.TRL$(WE.I%) = SET.AGENCY.NULL.TO.BLANK$(               \
                                                WE.AGENCY.REDEMPT.TRL$(WE.I%), \
                                                WE.NUM.REDEMPT.TRL%)

      ! If this is the first string added to the state code variable then parse
      ! the option data into the work variables.  We'll assume the first state
      ! code in the options is the most used option.  If the subroutine returns
      ! an error then wipe out this state code.
      IF (LEN(WE.STATE.CODE$) = 2) THEN BEGIN    ! if first state added
        IF (WE.STATE.CODE$ <> "") THEN BEGIN     ! if data seems valid
          ERR.CODE$ = ""                         ! set up error flag
          CALL PARSE.OPTION.DATA(ERR.CODE$,WE.STATE.CODE$)
          IF (ERR.CODE$ <> "") THEN BEGIN        ! if an error returned
            WE.STATE.CODE$ = ""                  ! no good
            CALL WE.RESET.OPT.WORK.VARS          ! reset the option work vars
          ENDIF                                  ! if an error returned
        ENDIF                                    ! if data seems valid
      ENDIF                                      ! if first state added
    ENDIF ELSE BEGIN                             ! else state is not active
      CALL REMOVE.AGENCY.RECORD(WE.I%)           ! remove this agency from array
    ENDIF                                        ! if State is active

    A$ = ""                                      ! cleanup

  NEXT WE.I%

  IF (WE.STATE.CODE$ = "") OR                    \ If no state codes active
     (WE.NUMBER.OF.AGENCIES <= 0)                \ no agency data
  THEN BEGIN
    TO.WE.ENABLED = 0                            ! deactivate feature
    CALL WE.RESET.OPT.VARS                       ! reset the option variables
    EXIT SUB                                     ! get out
  ENDIF                                          ! If no state codes active

  WE.INITIALIZE.PINPAD = -1                      ! Flag PinPad needs initialize

  TS.SERIAL.BAUDRATE = 9600                      ! opening serial port speed

  WIC.ENABLE = 0                                 ! disable Value Pack WIC

  FLAG.4610.TLOG.CR.ASC = -1                     ! flag keeping Tlog/Print

  READ #20, 0056; PROCESSING.REQUEST.MSG$        ! read display/print messages
  READ #20, 0058; WAIT.ON.CUSTOMER.MSG$
  READ #20, 1221; TRNX.DISCOUNT.DESC$
  READ #20, 1222; CAT.DESC.NOT.FOUND$
  READ #20, 1225; TRAINING.MODE.DESC$

  ! Format messages
  TRNX.DISCOUNT.DESC$ = LEFT$(TRNX.DISCOUNT.DESC$ + STRING$(18," "),18)

END SUB                                          ! end WIC.EBT.INITIALIZATION
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.PINPAD.INITIALIZE
! Description: This routine initializes the Pin Pad after a terminal load
!              We have to initialize the Pin Pad outside of the read of terminal
!              options to give the SA EFT application time to run it's own
!              pin pad initialization.
!*******************************************************************************
SUB WIC.EBT.PINPAD.INITIALIZE PUBLIC

  STRING                               \
         ERR.CODE$,                    \ error code returned from pinpad API
         STATES.VER$                   ! version number for each state code

  INTEGER*1                            \
            PROC.MODE                  ! processing mode for pinpad API

  INTEGER*2                            \
            MAX.BUFF.SIZE              ! max buffer size passed to pinpad API

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  IF (TS.SIGNED.ON = 0) THEN BEGIN               ! No initialize if signed off
    EXIT SUB                                     ! bail
  ENDIF                                          ! No initialize if signed off

  IF (WE.INITIALIZE.PINPAD = 0) THEN BEGIN       ! if not flagged to initialize
    EXIT SUB
  ENDIF                                          ! if not flagged to initialize

  IF (WE.STATE.CODE$ = "") THEN BEGIN            ! if no state to initialize
    WE.INITIALIZE.PINPAD = 0                     ! reset the initialize flag
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no state to initialize

  ! Set up the processing mode
  IF TO.WE.CERTIFY.MODE THEN BEGIN               ! if certification mode
    PROC.MODE = 2
  ENDIF ELSE BEGIN                               ! else not certification mode
    PROC.MODE = 0
  ENDIF                                          ! if certification mode

  MAX.BUFF.SIZE = 256
  ERR.CODE$ = ""
  STATES.VER$ = ""

  ! Call routine to initialize the pinpad
  CALL WICInitialize(MAX.BUFF.SIZE,              \
                     PROC.MODE,                  \
                     WE.STATE.CODE$,             \
                     ERR.CODE$,                  \
                     STATES.VER$)

  IF (ERR.CODE$ = "0000") THEN BEGIN             ! if no error
    WE.INITIALIZE.PINPAD = 0                     ! Flag done with Initialization
    WE.PINPAD.INIT.ERR.CNT = 0                   ! reset error count
    EXIT SUB                                     ! we're done
  ENDIF                                          ! if no error

  ! If the API returned this error response, then we'll need to keep trying to
  ! initialize. The PinPad could be loading or processing other messages so we
  ! shouldn't interrupt right now.  We put the logic for this test in the API to
  ! make it easier to plug in other EFT features such as StorePay. Otherwise we
  ! could add code prior to the call to WICInitalize such as...
  ! IF (TS.ET.ACTIVE = 0) THEN EXIT SUB
  IF (ERR.CODE$ = "RTRY") THEN BEGIN
    EXIT SUB
  ENDIF

  IF (ERR.CODE$ = "9998") THEN BEGIN             ! if State Mod does not exist
    IF (STATES.VER$ <> "") THEN BEGIN            ! if returned State Ver valid
      IF (LEN(WE.STATE.CODE$) = LEN(STATES.VER$)) \
      THEN BEGIN                                 ! if the version matches
        I% = LEN(STATES.VER$) - 1
        FOR K% = 1 TO I% STEP 2
          A$ = MID$(STATES.VER$,K%,2)
          IF (A$ = "FF") THEN BEGIN              ! if state code not valid
            B$ = MID$(WE.STATE.CODE$,K%,2)       ! Get State
            A$ = "WEinvSt " + B$                 ! build error msg with state ID
            CALL ADXERROR(VAL(TS.TERMINAL$),     \ log error
                          ASC("U"),              \
                          0,                     \
                          3,                     \ severity level
                          2,                     \
                          A$)
          ENDIF                                  ! if state code not valid
        NEXT K%
        A$ = ""                                  ! cleanup
        B$ = ""
      ENDIF                                      ! if the version matches
    ENDIF                                        ! if returned States Ver valid
    WE.INITIALIZE.PINPAD = 0                     ! flag done with initialization
    WE.PINPAD.INIT.ERR.CNT = 0                   ! reset error count
    EXIT SUB                                     ! We're done, get out
  ENDIF                                          ! if State Mod does not exist

  ! If we've reached this position in the code then we a problem initializing
  ! the PinPad for WIC EBT.  We'll try five times to initialize, but after that
  ! we log an error and turn off the initialization flag.  We'll try again on
  ! the next Sign/On if our error count is greater than zero.

  ! Increase the error count
  WE.PINPAD.INIT.ERR.CNT = WE.PINPAD.INIT.ERR.CNT + 1

  ! We'll allow 5 tries to initialize the PinPad.  After that we log an error
  ! and turn off the initialization flag until the next Sign/On.
  IF (WE.PINPAD.INIT.ERR.CNT > 5) THEN BEGIN     ! if error tries reached max
    WE.INITIALIZE.PINPAD = 0                     ! Flag done with Initialization
    A$ = "WEppi " + ERR.CODE$                    ! build msg with error code
    CALL ADXERROR(VAL(TS.TERMINAL$),             \ log error
                  ASC("U"),                      \
                  0,                             \
                  1,                             \ severity level
                  2,                             \
                  A$)

    A$ = ""                                      ! cleanup
  ENDIF                                          ! if error tries reached max

END SUB                                          ! end WIC.EBT.PINPAD.INITIALIZE
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.SIGN.ON
! Description: Called when an operator signs onto the terminal
!*******************************************************************************
SUB WIC.EBT.SIGN.ON PUBLIC

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  ! Reset the flag that an _80 message was sent.
  ! The EFT code will reset the PinPad on SignOn, so if a card is still
  ! in the reader then too bad. Resetting this flag should prevent an
  ! unnecessary status check.
  PP.80.REMOVE.CARD.SENT = 0                     ! reset message on sign-on

  IF (WE.PINPAD.INIT.ERR.CNT > 0) THEN BEGIN     ! problems initializing PinPad
    WE.PINPAD.INIT.ERR.CNT = 0                   ! reset error count
    WE.INITIALIZE.PINPAD = -1                    ! try again to initialize
  ENDIF                                          ! problems initializing PinPad

END SUB                                          ! end WIC.EBT.SIGN.ON
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.TENDER
! Description: Validate the WIC EBT tender.
!*******************************************************************************
SUB WIC.EBT.TENDER PUBLIC

  STRING                               \
         SIGNATURE$,                   \ Transaction signature
         WIC.ITEMS$,                   \ WIC'able items from customer's Rx
         ORG.WIC.ITEMS$,               \ original WIC items used for training
         TRN.WIC.ITEMS$,               \ WIC items used for training mode
         ISSUER$,                      \ benefits issuing entity
         FIRST.DATE$,                  \ first date to spend
         LAST.DATE$,                   \ last date to spend
         AUTH.RC$,                     \ Authentication result code
         PAN$,                         \ Personal Account Number
         STATE$,                       \ State code
         ERR.CODE$                     ! error code

  INTEGER*1                            \
            RESET.TRNX.VARS,           \ flag if we need to reset variables
            PROC.MODE                  ! processing mode for pinpad API

  INTEGER*2                            \
            MAX.PIN.RETRIES            ! how many times can we retry PIN

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  IF (WE.TENDER.TYPE <> SL.TE.TENDTYPE) OR       \
     (WE.TENDER.VARIETY <> SL.TE.TENDVAR)        \
  THEN BEGIN                                     ! not WIC tender type/variety
    EXIT SUB                                     ! get out
  ENDIF                                          ! not WIC tender type/variety

  IF (TS.PROCEDURE = 1) OR                       \ if tender cashing
     (TS.TENDERED(0) <> 0) OR                    \ if this not first tender
     (TS.IO.KEYS(1) = 70)                        \ or voiding wic tender
  THEN BEGIN                                     ! if not able to tender WIC EBT

    ! reset all the stack error values to zero so not overrideable
    FOR I% = 0 TO 6
      TS.STACKERR(I%) = 0
    NEXT I%

    TS.LINETYPE = 8                              ! default message type

    IF (TS.PROCEDURE = 1) THEN BEGIN             ! if tender cashing
      ! "Check Tender Type"
      TS.LINEDATA = 90
    ENDIF ELSE BEGIN                             ! else not tender cashing
      IF (TS.IO.KEYS(1) = 70) THEN BEGIN         ! if voiding wic ebt tender
        ! "Wic ebt tender cannot be voided"
        TS.LINEDATA = 894
      ENDIF ELSE BEGIN                           ! else void key not pressed
        IF WE.WIC.EBT.TENDERED THEN BEGIN        ! if wic tender already taken
          ! "Wic ebt tender already taken"
          TS.LINEDATA = 881
        ENDIF ELSE BEGIN                         ! else wic tender not taken yet
          ! "Void tenders before tendering WIC"
          TS.LINEDATA = 882
        ENDIF                                    ! if wic tender already taken
      ENDIF                                      ! if voiding wic ebt tender
    ENDIF                                        ! if tender cashing

    TS.PURGE.ERR = -1                            ! purge any other errors
    CALL TSCSEC03                                ! Stack the error
    EXIT SUB                                     ! get out

  ENDIF                                          ! if not able to tender WIC EBT

  SL.TE.AMTTENDE = 0                             ! reset amount tendered

  ! Put up display waiting on customer
  TS.LINETYPE = 12
  TS.TEMP1$ = LEFT$(WAIT.ON.CUSTOMER.MSG$,20)
  TS.TEMP2$ = RIGHT$(WAIT.ON.CUSTOMER.MSG$,20)
  CALL TSDSEC01

  ! Set up the processing mode
  ! Note training is set the same in certification as in redemption mode
  ! 0 = Normal
  ! 1 = Training
  ! 2 = Certification
  IF TS.TRAINING THEN BEGIN                      ! if training mode
    PROC.MODE = 1                                ! set training mode
  ENDIF ELSE BEGIN                               ! else not training mode
    IF TO.WE.CERTIFY.MODE THEN BEGIN             ! if certification mode
      PROC.MODE = 2                              ! set certification mode
    ENDIF ELSE BEGIN                             ! else not certification mode
      PROC.MODE = 0                              ! set redemption mode
    ENDIF                                        ! if certification mode
  ENDIF                                          ! if training mode

  RESET.TRNX.VARS = -1                           ! assume need to reset vars

  ERR.CODE$ = ""                                 ! initialize return variables
  STATE$ = ""
  PAN$ = ""

  CALL WICGetPAN(PROC.MODE,ERR.CODE$,STATE$,PAN$)

  IF (ERR.CODE$ = "0000") THEN BEGIN             ! if getting PAN number OK
    CALL CHECK.HOT.CARD.FILE(ERR.CODE$,PAN$,STATE$)
    IF (ERR.CODE$ = "0000") THEN BEGIN           ! if not an invalid Hot Card

      ERR.CODE$ = ""                             ! reset err code
      AUTH.RC$ = ""                              ! reset Authentication return
      MAX.PIN.RETRIES = 7                        ! try PIN entry seven times

      ! Call API to get PIN number
      CALL WICAuthenticate(STATE$,MAX.PIN.RETRIES,ERR.CODE$,AUTH.RC$)

      IF (ERR.CODE$ = "0000") THEN BEGIN         ! if getting PIN number OK

        ERR.CODE$ = ""
        LAST.DATE$ = ""
        FIRST.DATE$ = ""
        ISSUER$ = ""
        WIC.ITEMS$ = ""

        ! Put up display that we're processing
        TS.LINETYPE = 12
        TS.TEMP1$ = LEFT$(PROCESSING.REQUEST.MSG$,20)
        TS.TEMP2$ = RIGHT$(PROCESSING.REQUEST.MSG$,20)
        CALL TSDSEC01

!AIO05348 Save the date and time when the card balance was read, for
!         subsequent use at the end of the 80:02 t-log record. 20yymmddhhmmss
        WE.SL.TENDER.TIMEDATE$ = PACK$("20" + DATE$ \ date of query
                                       + TIME$)     ! time of query
!EIO05348

        ! Call API to read Card Balance
        CALL WICReadBalance(STATE$,              \
                            ERR.CODE$,           \
                            LAST.DATE$,          \
                            FIRST.DATE$,         \
                            ISSUER$,             \
                            WIC.ITEMS$)

        IF (ERR.CODE$ = "0000") THEN BEGIN       ! if reading card balance OK

          IF TS.TRAINING THEN BEGIN              ! if in training mode
            ORG.WIC.ITEMS$ = WIC.ITEMS$          ! save the wic items
          ENDIF                                  ! if in training mode

          ERR.CODE$ = ""                         ! reset error code

          CALL PROCESS.PRESCRIPTION(ERR.CODE$,   \
                                    STATE$,      \
                                    PAN$,        \
                                    LAST.DATE$,  \
                                    FIRST.DATE$, \
                                    WIC.ITEMS$)

          IF (ERR.CODE$ = "0000") THEN BEGIN     ! if processing prescription OK

            ERR.CODE$ = ""                       ! reset error code
            SIGNATURE$ = ""                      ! reset return value

            ! If in training mode we need to build our own decremented item list
            ! for printing, the PinPad does not decrement the card.
            IF TS.TRAINING THEN BEGIN            ! if in training mode
              TRN.WIC.ITEMS$ = BUILD.TRAINING.WIC.ITEMS$(ORG.WIC.ITEMS$,       \
                                                         WE.ITEMS.TO.DECREMENT$)
            ENDIF                                ! if in training mode

            ! Put up display waiting on customer
            TS.LINETYPE = 12
            TS.TEMP1$ = LEFT$(WAIT.ON.CUSTOMER.MSG$,20)
            TS.TEMP2$ = RIGHT$(WAIT.ON.CUSTOMER.MSG$,20)
            CALL TSDSEC01

            ! Call API to debit the card balance
            CALL WICDebitBalance(STATE$,                   \
                                 WE.ITEMS.TO.DECREMENT$,   \
                                 ERR.CODE$,                \
                                 SIGNATURE$)

            IF (ERR.CODE$ = "0000") THEN BEGIN   ! if debiting card balance OK

              RESET.TRNX.VARS = 0                ! don't reset the variables

              WE.TOTAL.CLAIM.AMOUNT = WE.TOTAL.ITEM.VALUE - WE.TOTAL.DISC.VALUE

              ! remove any taxable amounts for items sold with wic
              CALL WE.REMOVE.TAXES

              SL.TE.AMTTENDE = WE.TOTAL.CLAIM.AMOUNT

              WE.SL.AGENCY.ID$ = STATE$
              WE.SL.PAN$ = PACK$(RIGHT$(STRING$(20,"0") + PAN$,20))
              WE.SL.PAN.LEN$ = PACK$(STR$(LEN(PAN$)))
              WE.SL.CLAIM.AMOUNT$ = PACK$(STR$(WE.TOTAL.CLAIM.AMOUNT))
              WE.SL.POS.DATA.CODE$ = ""          ! not currently used
              WE.SL.ISSUING.AGENCY$ = ISSUER$
              WE.SL.FIRST.DATE.SPEND$ = RIGHT$(STRING$(8," ") + FIRST.DATE$,8)
              WE.SL.FIRST.DATE.SPEND$ = PACK$(WE.SL.FIRST.DATE.SPEND$)
              WE.SL.LAST.DATE.SPEND$ = RIGHT$(STRING$(8," ") + LAST.DATE$,8)
              WE.SL.LAST.DATE.SPEND$ = PACK$(WE.SL.LAST.DATE.SPEND$)
              WE.SL.DISCOUNT.AMOUNT$ = PACK$(STR$(WE.TOTAL.DISC.VALUE))
              WE.SL.TRNX.SIGNATURE$ = SIGNATURE$

              DIM WE.SL.TAX$(4)
              FOR I% = 1 TO 4
                WE.SL.TAX$(I%) = PACK$(STR$(WE.TAX(I%)))
              NEXT I%

              WE.TENDERING = -1                  ! flag this is a WIC EBT tender

              ! read the card balance after debiting the balance

              ERR.CODE$ = ""
              LAST.DATE$ = ""
              FIRST.DATE$ = ""
              ISSUER$ = ""
              WIC.ITEMS$ = ""

              CALL WICReadBalance(STATE$,        \
                                  ERR.CODE$,     \
                                  LAST.DATE$,    \
                                  FIRST.DATE$,   \
                                  ISSUER$,       \
                                  WIC.ITEMS$)

              IF (ERR.CODE$ = "0000")            \ if reading card balance OK
              THEN BEGIN

                IF TS.TRAINING THEN BEGIN        ! if training mode
                  WIC.ITEMS$ = TRN.WIC.ITEMS$    ! set wic items to rebuilt ones
                ENDIF                            ! if training mode

                ! call routine to build prescription data
                CALL BUILD.PRESCRIPTION.DATA(STATE$,WIC.ITEMS$)

                IF (WE.NUM.CATEGORIES <= 0)      \ if invalid cats returned
                THEN BEGIN
                  WIC.ITEMS$ = ""                ! make sure this reset
                ENDIF                            ! if invalid cats returned

              ENDIF ELSE BEGIN                   ! else problem reading balance
                WIC.ITEMS$ = ""                  ! make sure this reset
              ENDIF                              ! if reading card balance OK

              FLAG.4610.NO.BUFFERING = -1        ! flag so receipts not buffered
              CALL DISABLE.BUFFERING             ! disable 4610 buffering
              WE.BUFFER.BR.ONLY = -1             ! flag to only buffer lines

              ! buffer the remaining balance
              CALL PRINT.BENEFIT.RECEIPT(STATE$,           \
                                         PAN$,             \
                                         LAST.DATE$,       \
                                         FIRST.DATE$,      \
                                         WIC.ITEMS$)

              WE.BUFFER.BR.ONLY = 0              ! reset WIC EBT buffer flag
              FLAG.4610.NO.BUFFERING = 0         ! reset 4610 flag
              CALL ENABLE.BUFFERING              ! allow 4610 code to buffer

              ! Call function to end the WIC trnx
              CALL END.WIC.EBT.TRANSACTION(STATE$,"01")

            ENDIF ELSE BEGIN                     ! else error debiting balance
              CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
            ENDIF                                ! if debiting card balance OK
          ENDIF ELSE BEGIN                       ! else can't process Rx
            CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
          ENDIF                                  ! if processing prescription OK
        ENDIF ELSE BEGIN                         ! else error reading card bal
          CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
        ENDIF                                    ! if reading card balance OK
      ENDIF ELSE BEGIN                           ! else error getting PIN number
        CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
      ENDIF                                      ! if getting PIN number OK
    ENDIF ELSE BEGIN                             ! else PAN on the Hot Card file
      CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
    ENDIF                                        ! if not an invalid Hot Card
  ENDIF ELSE BEGIN                               ! else error getting PAN number
    CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
  ENDIF                                          ! if getting PAN number OK

  IF RESET.TRNX.VARS THEN BEGIN                  ! if some problem along the way
    CALL WE.RESET.TRANSACTION.VARS               ! reset the trnx variables
    CALL WE.RESET.PRINT.VARS                     ! reset all the printing vars
  ENDIF                                          ! if some problem along the way

  ERR.CODE$ = ""                                 ! cleanup
  AUTH.RC$ = ""
  LAST.DATE$ = ""
  FIRST.DATE$ = ""
  ISSUER$ = ""
  WIC.ITEMS$ = ""
  ORG.WIC.ITEMS$ = ""
  TRN.WIC.ITEMS$ = ""
  SIGNATURE$ = ""
  STATE$ = ""
  PAN$ = ""

END SUB                                          ! end WIC.EBT.TENDER
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.LOG.ITEM.SUMMARY
! Description: Log the item strings and summary string to the Tlog after a
!              WIC EBT tender is logged.
!*******************************************************************************
SUB WIC.EBT.LOG.ITEM.SUMMARY PUBLIC

  INTEGER*2                            \
            WE.I%                      ! local work variable

  CALL EAMTRACE(4557H)                           ! trace "WE"

  WE.TENDERING = 0                               ! reset tendering flag

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  WE.WIC.EBT.TENDERED = -1                       ! flag this is a WIC EBT trnx

  ! Need to update EM variable since we could have changed the taxable amount
  EMSS.PR.BALDUE = TS.TOTALS(0,0,0) + TS.TAXES(0,0)

  FOR WE.I% = 1 TO WE.ITEM.COUNT                 ! for all the items in the trnx
    WE.SL.ITEM.CATEGORY$ = PACK$(LEFT$(WE.ITEM.FULL.CATEGORY$(WE.I%),2))
    WE.SL.ITEM.SUBCATEGORY$ = PACK$(RIGHT$(WE.ITEM.FULL.CATEGORY$(WE.I%),3))
    WE.SL.ITEM.BENEFIT.QTY$ = PACK$(STR$(WE.ITEM.BENEFIT.QTY(WE.I%)))
    WE.SL.ITEM.N.PURCH.QTY$ = PACK$(STR$(WE.ITEM.N.PURCH.QTY(WE.I%)))
    WE.SL.ITEM.0.PURCH.QTY$ = PACK$(STR$(WE.ITEM.0.PURCH.QTY(WE.I%)))
    WE.SL.ITEM.N.COST$ = PACK$(STR$(WE.ITEM.N.COST(WE.I%)))
    WE.SL.ITEM.0.COST$ = PACK$(STR$(WE.ITEM.0.COST(WE.I%)))
    WE.SL.ITEM.TLOG.INDEX$ = PACK$(STR$(WE.ITEM.TLOG.INDEX(WE.I%)))
    A$ = UNPACK$(WE.ITEM.CODE$(WE.I%))
    A$ = PACK$(RIGHT$(STRING$(14,"0") + A$ + WE.ITEM.CHECKDIGIT$(WE.I%),14))
    WE.SL.ITEM.UPC$ = A$
    WE.SL.ITEM.N.COUPON$ = PACK$(WE.ITEM.CPN.N.TLOG$(WE.I%))
    WE.SL.ITEM.0.COUPON$ = PACK$(WE.ITEM.CPN.0.TLOG$(WE.I%))
    WE.SL.ITEM.N.CHILD.ARRAY$ = PACK$(WE.ITEM.N.CHILD.ARRAY$(WE.I%))
    WE.SL.ITEM.0.CHILD.ARRAY$ = PACK$(WE.ITEM.0.CHILD.ARRAY$(WE.I%))

    IF (TS.TRAINING = 0) THEN BEGIN              ! if not in training mode
      WE.SL.IDENTIFIER = 1                       ! Identifier for WE item log
      TS.TEMP1I1 = 80                            ! WIC EBT tlog string type
      CALL TSTPEC01                              ! log the item string
      CALL EAMTRACE(4557H)                       ! trace "WE"
    ENDIF                                        ! if not in training mode

    ! Now add this item entry to the Tlog array to prevent it from being voided.
    M% = WE.ITEM.TLOG.INDEX(WE.I%)               ! tlog index for this item
    WE.WIC.QTY(M%) = WE.WIC.QTY(M%)              \ add up this item entry
                   + WE.ITEM.N.PURCH.QTY(WE.I%)  \ non-zero purchase quantity
                   + WE.ITEM.0.PURCH.QTY(WE.I%)  ! zero purchase quantity

    ! Add coupon entries to the Tlog Array
    CALL WE.ADD.COUPON.TO.WIC.QTY(WE.SL.ITEM.N.COUPON$)  ! add non-zero coupons
    CALL WE.ADD.COUPON.TO.WIC.QTY(WE.SL.ITEM.0.COUPON$)  ! add zero coupons

  NEXT WE.I%                                     ! for all the items in the trnx

  ! We can drop out now if in training mode since the rest of the routine is
  ! just to log Tlog strings.
  IF TS.TRAINING THEN BEGIN                      ! if training mode
    EXIT SUB                                     ! no logging in training mode
  ENDIF                                          ! if training mode

  ! Now log the summary string
  WE.SL.IDENTIFIER = 2                           ! Identifier for WE summary
  TS.TEMP1I1 = 80                                ! WIC EBT tlog string type
  CALL TSTPEC01                                  ! log the summary string
  CALL EAMTRACE(4557H)                           ! trace "WE"

  CALL TSTPEC04                                  ! flush to total save file

END SUB                                          ! end WIC.EBT.LOG.ITEM.SUMMARY
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.END.OF.TRANSACTION
! Description: End of transaction processing for WIC EBT feature.
!*******************************************************************************
SUB WIC.EBT.END.OF.TRANSACTION PUBLIC

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  ! Make sure 4610 is reset at end of transaction
  FLAG.4610.NO.BUFFERING = 0                     ! make sure this flag is reset
  CALL ENABLE.BUFFERING                          ! make sure buffering receipt

  IF (WE.BENFT.RDMPT.IDX > 0) THEN BEGIN         ! if we need to print WE lines
    WE.BUFFER.BR.RECEIPT = 0                     ! make sure flag reset
    WE.BUFFER.BR.ONLY = 0
    CALL PRINT.BENFT.RDMPT.RECEIPT               ! print the receipts
  ENDIF                                          ! if we need to print WE lines

  CALL WE.RESET.TRANSACTION.VARS                 ! reset the trnx variables

END SUB                                          ! WIC.EBT.END.OF.TRANSACTION
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.BALANCE.INQUIRY
! Description: Handle the WIC EBT Balance Inquiry Procedure.
!*******************************************************************************
SUB WIC.EBT.BALANCE.INQUIRY PUBLIC

  STRING                               \
         SAVDISP1$,                    \ save TS.SAVDISP1$
         SAVDISP2$,                    \ save TS.SAVDISP2$
         WIC.ITEMS$,                   \ WIC'able items from prescription
         ISSUER$,                      \ benefits issuing entity
         FIRST.DATE$,                  \ first date to spend
         LAST.DATE$,                   \ last date to spend
         AUTH.RC$,                     \ Authentication result code
         PAN$,                         \ Personal Account Number
         STATE$,                       \ State code
         ERR.CODE$                     ! error code

  INTEGER*1                            \
            PROC.MODE                  ! processing mode for pinpad API

  INTEGER*2                            \
            SAVE.TS.IO.KEYS5,          \ save value in TS.IO.KEYS(5)
            MAX.PIN.RETRIES            ! how many times can we retry PIN

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    TS.PROCEDURE = 0                             ! reset procedure number
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  CALL TSCSEC09                                  ! non-sales initialization

  IF (TS.PROCEDURE = 0) THEN BEGIN               ! if procedure was rejected
    EXIT SUB                                     ! get out
  ENDIF                                          ! if procedure was rejected

  SAVDISP1$ = TS.SAVDISP1$                       ! save display variables
  SAVDISP2$ = TS.SAVDISP2$

  ! Put up display waiting on customer
  TS.LINETYPE = 12
  TS.TEMP1$ = LEFT$(WAIT.ON.CUSTOMER.MSG$,20)
  TS.TEMP2$ = RIGHT$(WAIT.ON.CUSTOMER.MSG$,20)
  TS.SAVDISP1$ = TS.TEMP1$
  TS.SAVDISP2$ = TS.TEMP2$
  CALL TSDSEC01

  ! Set up the processing mode
  ! Note training is set the same in certification as in redemption mode
  ! 0 = Normal
  ! 1 = Training
  ! 2 = Certification
  IF TS.TRAINING THEN BEGIN                      ! if training mode
    PROC.MODE = 1                                ! set training mode
  ENDIF ELSE BEGIN                               ! else not training mode
    IF TO.WE.CERTIFY.MODE THEN BEGIN             ! if certification mode
      PROC.MODE = 2                              ! set certification mode
    ENDIF ELSE BEGIN                             ! else not certification mode
      PROC.MODE = 0                              ! set redemption mode
    ENDIF                                        ! if certification mode
  ENDIF                                          ! if training mode

  ERR.CODE$ = ""                                 ! initialize return variables
  STATE$ = ""
  PAN$ = ""

  CALL WICGetPAN(PROC.MODE,ERR.CODE$,STATE$,PAN$)

  IF (ERR.CODE$ = "0000") THEN BEGIN             ! if getting the PAN number OK
    CALL CHECK.HOT.CARD.FILE(ERR.CODE$,PAN$,STATE$)
    IF (ERR.CODE$ = "0000") THEN BEGIN           ! if PAN not invalid Hot Card
      ERR.CODE$ = ""                             ! reset for authenticate
      AUTH.RC$ = ""                              ! reset Authentication code
      MAX.PIN.RETRIES = 7                        ! try PIN entry seven times

      ! Call method to get customer's PIN number
      CALL WICAuthenticate(STATE$,MAX.PIN.RETRIES,ERR.CODE$,AUTH.RC$)

      IF (ERR.CODE$ = "0000") THEN BEGIN         ! if no error getting PIN

        ERR.CODE$ = ""
        LAST.DATE$ = ""
        FIRST.DATE$ = ""
        ISSUER$ = ""
        WIC.ITEMS$ = ""

        ! Put up display that we're processing
        TS.LINETYPE = 12
        TS.TEMP1$ = LEFT$(PROCESSING.REQUEST.MSG$,20)
        TS.TEMP2$ = RIGHT$(PROCESSING.REQUEST.MSG$,20)
        CALL TSDSEC01

        ! read the card balance
        CALL WICReadBalance(STATE$,              \
                            ERR.CODE$,           \
                            LAST.DATE$,          \
                            FIRST.DATE$,         \
                            ISSUER$,             \
                            WIC.ITEMS$)

        IF (ERR.CODE$ = "0000") THEN BEGIN       ! if reading card balance OK

          ! dump all the needed option data into work variables
          CALL PARSE.OPTION.DATA(ERR.CODE$,STATE$)

          IF (ERR.CODE$ = "0000") THEN BEGIN     ! if able to parse option data

            ! call routine to build prescription data
            CALL BUILD.PRESCRIPTION.DATA(STATE$,WIC.ITEMS$)

            IF (WE.NUM.CATEGORIES <= 0)          \ if invalid cats returned
            THEN BEGIN
              ERR.CODE$ = "50000"
            ENDIF                                ! if invalid cats returned

            IF (ERR.CODE$ = "0000") THEN BEGIN   ! if no error so far

              CALL PRINT.BENEFIT.RECEIPT(STATE$,           \
                                         PAN$,             \
                                         LAST.DATE$,       \
                                         FIRST.DATE$,      \
                                         WIC.ITEMS$)

              ! Call function to end the WIC trnx
              CALL END.WIC.EBT.TRANSACTION(STATE$,"01")

            ENDIF ELSE BEGIN                     ! else we have errors
              CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
            ENDIF                                ! if no error so far
          ENDIF ELSE BEGIN                       ! else unable to parse option
            CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
          ENDIF                                  ! if able to parse option data
        ENDIF ELSE BEGIN                         ! else error reading balance
          CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
        ENDIF                                    ! if reading card balance OK
      ENDIF ELSE BEGIN                           ! else an error getting PIN
        CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
      ENDIF                                      ! if no error getting PIN
    ENDIF ELSE BEGIN                             ! else PAN on the Hot Card file
      CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
    ENDIF                                        ! if PAN not invalid Hot Card
  ENDIF ELSE BEGIN                               ! else error getting the PAN
    CALL PINPAD.RESPONSE.ERROR(STATE$,PAN$,ERR.CODE$)
  ENDIF                                          ! if getting the PAN number OK

  TS.SAVDISP1$ = SAVDISP1$                       ! restore display variables
  TS.SAVDISP2$ = SAVDISP2$

  ERR.CODE$ = ""                                 ! cleanup
  AUTH.RC$ = ""
  LAST.DATE$ = ""
  FIRST.DATE$ = ""
  ISSUER$ = ""
  WIC.ITEMS$ = ""
  STATE$ = ""
  PAN$ = ""
  SAVDISP1$ = ""
  SAVDISP2$ = ""

  ! Before calling TSCSEC10 to end the nonSales trnx we have to reset keys
  ! bucket 5,(it's set to 61).  Base Electronic Marketing will reset SL.END on
  ! any Sign/On procedure causing errors to be lost that could be logged with
  ! this balance inquiry string, (see EAMTSX20.J86 for problem code).
  ! Probably should be fixed in EM but we'll add this hack instead.
  SAVE.TS.IO.KEYS5 = TS.IO.KEYS(5)               ! save bucket 5
  TS.IO.KEYS(5) = 0

  CALL TSCSEC10                                  ! end the non-sales transaction

  TS.IO.KEYS(5) = SAVE.TS.IO.KEYS5               ! restore bucket

  CALL WE.RESET.TRANSACTION.VARS                 ! reset the trnx variables
  CALL WE.RESET.PRINT.VARS                       ! reset all the printing vars

END SUB                                          ! end WIC.EBT.BALANCE.INQUIRY
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.REPRINT.RECEIPT
! Description: Reprint the benefit/redemption receipts from the transaction
!*******************************************************************************
SUB WIC.EBT.REPRINT.RECEIPT PUBLIC

  STRING                               \
         TS.SV.TEMP1$,                 \ save first display
         TS.SV.TEMP2$                  ! save second display

  INTEGER*2                            \
            SAVE.POST.PRINT,           \ save 4610 var EP.ETO.PRINT.POST.TRANS
            SAVE.DUP.NUMBER,           \ save 4610 var EP.ETO.DUPLICATE.NUMBER
            TS.SV.LINETYPE             ! save the linetype

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  IF (WE.BENFT.RDMPT.IDX <= 0) THEN BEGIN        ! if no lines to print
    TS.GUIDANCE = 1094                           ! Procedure not available
    EXIT SUB                                     ! get out
  ENDIF                                          ! if no lines to print

  TS.SV.TEMP1$ = TS.DISP1$                       ! Save Display Line 1
  TS.SV.TEMP2$ = TS.DISP2$                       ! Save Display Line 2
  TS.SV.LINETYPE = TS.LINETYPE

  SAVE.POST.PRINT = EP.ETO.PRINT.POST.TRANS      ! save post-printing flag
  EP.ETO.PRINT.POST.TRANS = 0                    ! turn off post-printing

  FLAG.4610.NO.BUFFERING = -1                    ! flag so receipts not buffered
  CALL DISABLE.BUFFERING                         ! disable 4610 buffering

  CALL DO.GUILLOTINE.CUT                         ! cut the receipt

  GOSUB PRINT.DUPLICATE.RECEIPT.MSG              ! print duplicate msg before

  CALL PRINT.BENFT.RDMPT.RECEIPT                 ! print the receipts

  GOSUB PRINT.DUPLICATE.RECEIPT.MSG              ! print duplicate msg after

  CALL DO.GUILLOTINE.CUT                         ! cut the receipt

  EP.ETO.PRINT.POST.TRANS = SAVE.POST.PRINT      ! restore post print flag

  ! Reprint the customer receipt if we're not post-printing
  IF (EP.ETO.PRINT.POST.TRANS = 0) THEN BEGIN    ! if we're not post-printing
    SAVE.DUP.NUMBER = EP.ETO.DUPLICATE.NUMBER    ! save the 4610 variable
    EP.ETO.DUPLICATE.NUMBER = 1                  ! only print one receipt
    PRT4610.REPRINT = -1                         ! flag re-printing the receipt
    CALL REPRINT.RECEIPT                         ! reprint the receipt
    PRT4610.REPRINT = 0                          ! reset flag
    EP.ETO.DUPLICATE.NUMBER = SAVE.DUP.NUMBER    ! restore 4610 variable
  ENDIF                                          ! if we're not post-printing

  FLAG.4610.NO.BUFFERING = 0                     ! reset 4610 flag
  CALL ENABLE.BUFFERING                          ! allow 4610 code to buffer

  ! Display message prior to calling
  TS.TEMP1$ = TS.SV.TEMP1$                       ! Set up first line of disp.
  TS.TEMP2$ = TS.SV.TEMP2$                       ! Set up sec. line of disp.
  TS.LINETYPE = 12                               ! Set to disp as passed
  CALL TSDSEC01                                  ! Display routine
  TS.LINETYPE = TS.SV.LINETYPE                   ! Restore line type

  TS.SV.TEMP1$ = ""                              ! cleanup
  TS.SV.TEMP2$ = ""

  EXIT SUB                                       ! we're done

PRINT.DUPLICATE.RECEIPT.MSG:                     ! print the duplicate msg

  IF (WE.DUP.RECPT.HDR1.LF <= 0) THEN BEGIN      ! if line printing turned off
    RETURN                                       ! just go back
  ENDIF                                          ! if line printing turned off

  TS.LINETYPE = 32                               ! WIC EBT print
  TS.LINEDATA = 100                              ! re-printing receipts
  TS.LINEDATA2 = 1                               ! printing duplicate msg
  TS.SAVPRT$ = WE.DUP.RECPT.HDR1$
  TS.PRT.OPT = 4000H + (WE.DUP.RECPT.HDR1.LF * 100H)
  CALL TSPREC01

  RETURN                                         ! PRINT.DUPLICATE.RECEIPT.MSG

END SUB                                          ! end WIC.EBT.REPRINT.RECEIPT
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.PRINT.HELP
! Description: Print the WIC EBT procedures on the help
!*******************************************************************************
SUB WIC.EBT.PRINT.HELP PUBLIC

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  TS.ER.RETURN = -1                              ! return on error
  READ #20, 1223; TS.TEMP1$                      ! 22 = WIC EBT BALANCE INQUIRY
  IF TS.ER.RETURN THEN BEGIN                     ! if no error
    TS.ER.RETURN = 0                             ! reset error flag
    TS.PRTBUF$ = LEFT$(TS.TEMP1$ + BLANK$,38)
    CALL PRINT.RTN                               ! print the descriptor
  ENDIF                                          ! if no error

END SUB                                          ! end WIC.EBT.PRINT.HELP
!*******************************************************************************


!*******************************************************************************
! Subroutine Name: WIC.EBT.TRNX.RECOVERY
! Description: Handle the WIC EBT transaction on recovery.
!*******************************************************************************
SUB WIC.EBT.TRNX.RECOVERY PUBLIC

  INTEGER*2                            \
            POINTER,                   \ pointer into string
            START,                     \ start of tlog pointer
            WE.I%                      ! work variable

  INTEGER*4                            \
            WE.X%                      ! work variable

  CALL EAMTRACE(4557H)                           ! trace "WE"

  IF (TO.WE.ENABLED = 0) THEN BEGIN              ! if WIC EBT not enabled
    EXIT SUB                                     ! get out
  ENDIF                                          ! if WIC EBT not enabled

  L% = LEN(SL.STR.ENTRY$)

  IF (L% < 10) THEN BEGIN                        ! if string doesn't seem valid
    EXIT SUB                                     ! get out
  ENDIF                                          ! if string doesn't seem valid

  N% = PACKBIN2(SL.STR.ENTRY$,2,1)               ! Get identifier

  ON N% GOSUB                                    \
              WE.RECOVER.ITEM.STRING,            \ 01 string
              WE.RECOVER.SUMMARY.STRING,         \ 02 string
              WE.RECOVER.ERROR.STRING            ! 03 string

  A$ = ""                                        ! cleanup

  EXIT SUB                                       ! we're done


WE.RECOVER.ITEM.STRING:                          ! recover 01 string

  ! We need to rebuild the WIC item quantity array on recoveries

  ! Move up to the non-zero purchase quantity
  POINTER = 0
  FOR I% = 1 TO 6                                ! loop through delimiters
    POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)
    IF (POINTER <= 0) THEN BEGIN                 ! if invalid match
      RETURN                                     ! we're done
    ENDIF                                        ! if invalid match
  NEXT I%                                        ! loop through delimiters

  START = POINTER + 1                            ! save start of non-zero qty
  POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)     ! find end of non-zero qty

  IF (POINTER <= 0) THEN BEGIN                   ! if invalid match
    RETURN                                       ! we're done
  ENDIF                                          ! if invalid match

  N% = 0                                         ! hold the non-zero quantity
  M% = 0                                         ! hold the zero quantity

  IF ((POINTER - START) > 0) THEN BEGIN          ! if data in field
    A$ = MID$(SL.STR.ENTRY$,START,POINTER - START)    ! get non-zero quantity
    TS.ER.RETURN = -1                            ! set base error flag
    N% = VAL(UNPACK$(A$))                        ! get non-zero quantity
    IF TS.ER.RETURN THEN BEGIN                   ! if no error on VAL statement
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else we had an error
      RETURN                                     ! go back, invalid data
    ENDIF                                        ! if no error on VAL statement
  ENDIF                                          ! if data in field

  START = POINTER + 1                            ! save start of zero qty
  POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)    ! find end of zero qty

  IF (POINTER <= 0) THEN BEGIN                   ! if invalid match
    RETURN                                       ! we're done
  ENDIF                                          ! if invalid match

  IF ((POINTER - START) > 0) THEN BEGIN          ! if data in field
    A$ = MID$(SL.STR.ENTRY$,START,POINTER - START)    ! get zero quantity
    TS.ER.RETURN = -1                            ! set base error flag
    M% = VAL(UNPACK$(A$))                        ! get non-zero quantity
    IF TS.ER.RETURN THEN BEGIN                   ! if no error on VAL statement
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else we had an error
      RETURN                                     ! go back, invalid data
    ENDIF                                        ! if no error on VAL statement
  ENDIF                                          ! if data in field

  ! move up to the transaction index (offset into Tlog)
  FOR I% = 1 TO 2                                ! loop through delimiters
    POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)
    IF (POINTER <= 0) THEN BEGIN                 ! if invalid match
      RETURN                                     ! we're done
    ENDIF                                        ! if invalid match
  NEXT I%                                        ! loop through delimiters

  START = POINTER + 1                            ! save start of trnx index
  POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)  ! find end of trnx index

  IF (POINTER <= 0) THEN BEGIN                   ! if invalid match
    RETURN                                       ! we're done
  ENDIF                                          ! if invalid match

  I% = 0                                         ! hold the trnx index

  IF ((POINTER - START) > 0) THEN BEGIN          ! if data in field
    A$ = MID$(SL.STR.ENTRY$,START,POINTER - START)  ! get transaction index data
    TS.ER.RETURN = -1                            ! set base error flag
    I% = VAL(UNPACK$(A$))                        ! get transaction index
    IF TS.ER.RETURN THEN BEGIN                   ! if no error on VAL statement
      TS.ER.RETURN = 0                           ! reset error flag
    ENDIF ELSE BEGIN                             ! else we had an error
      RETURN                                     ! go back, invalid data
    ENDIF                                        ! if no error on VAL statement
  ENDIF                                          ! if data in field

  IF (I% <= 0) THEN BEGIN                        ! if invalid entry
    RETURN
  ENDIF                                          ! if invalid entry

  WE.WIC.QTY(I%) = WE.WIC.QTY(I%)                \ add up this item entry
                 + N%                            \ non-zero purchase quantity
                 + M%                            ! zero purchase quantity

  WE.WIC.EBT.TENDERED = -1                       ! flag this is a WIC EBT trnx

  ! now go get coupons if any

  POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1) ! find end of UPC field

  IF (POINTER <= 0) THEN BEGIN                   ! if invalid match
    RETURN                                       ! we're done
  ENDIF                                          ! if invalid match

  FOR WE.I% = 1 TO 2                             ! for both non and zero cpns

    START = POINTER + 1                          ! save start of coupons
    POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)   ! find end of coupon field

    IF (POINTER <= 0) THEN BEGIN                 ! if invalid match
      RETURN                                     ! we're done
    ENDIF                                        ! if invalid match

    IF ((POINTER - START) > 0) THEN BEGIN        ! if data in field
      A$ = MID$(SL.STR.ENTRY$,START,POINTER - START)  ! get coupon data
      CALL WE.ADD.COUPON.TO.WIC.QTY(A$)          ! add cpn entries to Tlog array
    ENDIF                                        ! if data in field

  NEXT WE.I%                                     ! for both non and zero cpns

  RETURN                                         ! WE.RECOVER.ITEM.STRING


WE.RECOVER.SUMMARY.STRING:                       ! recover 02 string

  WE.WIC.EBT.TENDERED = -1                       ! flag this is a WIC EBT trnx

  ! Move up to the tax fields
  POINTER = 0
  FOR I% = 1 TO 12                               ! loop through delimiters
    POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)
    IF (POINTER <= 0) THEN BEGIN                 ! if invalid match
      RETURN                                     ! we're done
    ENDIF                                        ! if invalid match
  NEXT I%                                        ! loop through delimiters

  DIM WE.TAX(4)                                  ! dim to hold the tax plans

  FOR WE.I% = 1 TO 4                             ! for all the tax plans

    START = POINTER + 1                          ! save start of tax plan
    POINTER = MATCHB(":",SL.STR.ENTRY$,POINTER + 1)   ! find end of tax plan

    IF (POINTER <= 0) THEN BEGIN                 ! if invalid match
      WE.I% = 4                                  ! break the loop
    ENDIF ELSE BEGIN                             ! else a valid match
      IF ((POINTER - START) > 0) THEN BEGIN      ! if data in field
        A$ = MID$(SL.STR.ENTRY$,START,POINTER - START)    ! get tax plan
        TS.ER.RETURN = -1                        ! set base error flag
        WE.X% = VAL(UNPACK$(A$))                 ! get tax plan
        IF TS.ER.RETURN THEN BEGIN               ! if no error on VAL statement
          TS.ER.RETURN = 0                       ! reset error flag
          WE.TAX(WE.I%) = WE.X%                  ! set tax plan
        ENDIF                                    ! if no error on VAL statement
      ENDIF                                      ! if data in field
    ENDIF                                        ! if invalid match

  NEXT WE.I%                                     ! for all the tax plans

  CALL WE.REMOVE.TAXES                           ! remove any taxable amounts
  DIM WE.TAX(0)                                  ! cleanup

!AIO00754
  CALL EAMTRACE(676AH)                           ! trace "jg"
  CALL javaEvent(updateTotalsDataMsg)            ! update totals (enhanced)
  CALL EAMTRACE(4557H)                           ! trace "WE"
!EIO00754

  RETURN                                         ! WE.RECOVER.SUMMARY.STRING


WE.RECOVER.ERROR.STRING:                         ! recover 03 string

  RETURN                                         ! WE.RECOVER.ERROR.STRING

END SUB                                          ! end WIC.EBT.TRNX.RECOVERY
!*******************************************************************************


!****************************** END EAMTSWEC.BAS *******************************
