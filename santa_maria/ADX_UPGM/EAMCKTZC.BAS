\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/*********************************************************************/
\/*                                                                   */
\/*      MODULE NAME: EAMCKTZC                                        */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: CODE MODULE FOR SUPERMARKET                */
\/*                        CHEQUE PRINTING FEATURE IN TERMINAL        */
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*       5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"       */
\/*       (c) COPYRIGHT IBM CORP 1991,1993 ALL RIGHTS RESERVED LICENSED    */
\/*       MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT              */
\/*       INSTRUCTIONS FORM NUMBER G120-2083                          */
\/*                                                                   */
\/*      STATUS: RELEASE 1 LEVEL 0                                    */
\/*                                                                   */
\/*      PERSON RESPONSIBLE = MARTIN MAENZA                           */
\/*                                                                   */
\/*      FUNCTION: CODE USED BY TERMINAL TO PERFORM CHEQUE PRINTING   */
\/*                IN SUPERMARKET APPLICATION.                        */
\/*                                                                   */
\/*                                                                   */
\/*      INTERNAL ROUTINES:                                           */
\/*                CKTSEC01   -  Called at terminal initialization    */
\/*                CKTSEC02   -  Called at tender time to print cheque*/
\/*                                                                   */
\/*      EXTERNAL ROUTINES:                                           */
\/*                ADXSERVE                                           */
\/*                ALPHA.DATE$  -  Returns date in DDMMMYY format     */
\/*                CHECK.PRESENT - Examines DI station for cheque     */
\/*                LEGAL$       -  Returns amount in words            */
\/*                NEED.CARD.NO -  Examines cheque format to see if   */
\/*                                   a card number should be entered.*/
\/*                FIELDS.TO.PRINT - Puts data into cheque format.    */
\/*                MOVE.TO.CHECK.START                                */
\/*                PRINT.CHECK                                        */
\/*                READ.CPDESC                                        */
\/*                READ.FORMAT                                        */
\/*                READ.FORMATOP                                      */
\/*                TSDSEC01                                           */
\/*                TSCSEC08                                           */
\/*                TSCSECRK                                           */
\/*                TSHIECET                                           */
\/*                TSUPEC54 - wait for IOPROC userexit                */
\/*                                                                   */
\/* CHANGE ACTIVITY:                                                  */
\/*                                                                   */
\/*    IR22496  Conditional code required around code that unlocks    */
\/*             and locks the MSR (session 41).  If a MSR isn't       */
\/*             attached, it results in an OE 41 80004005 error on SJ.*/
\/*             MCM     IBM/9EF      3/31/93                          */
\/*                                                                   */
\/*    IR33229  Added IBM 4610 support.                               */
\/*             Save/restore keys & data in CKTSEC03.                 */
\/*             ADW     MATRA        07/01/96                         */
\/*                                                                   */
\/*    IR33750  Ensure that CKTSEC03 does nothing on multiple calls.  */
\/*             11/15/96 MATRA (adw).                                 */
\/*                                                                   */
\/*             Added code to exit if bypass check printing is        */
\/*             chosen with 4610 printer.                             */
\/*             MWD     IBM          12/20/96                         */
\/*                                                                   */
\/*    IR35829  PE IR33750 Correct problem with code not requesting   */
\/*             to verify signature even though check is not printed. */
\/*             LTG     MGVA         7/02/97                          */
\/*                                                                   */
\/*    IR35815  When using a 4610 printer without check flipper but   */
\/*             using the check printing feature, the check is franked*/
\/*             and then, with no prompt to the operator to remove the*/
\/*             check, is printed on the same side of the check.      */
\/*             LTG     MGVA         7/02/97                          */
\/*                                                                   */
\/*    IR36401  Remove wait for 4610 printer.                         */
\/*             CMJ     MGVA         9/09/97                          */
\/*                                                                   */
\/*    IR37053  When printing the face of a check and not franking,   */
\/*             the print head needs to go home to print properly.    */
\/*             RSH     MGVA        12/29/97                          */
\/*                                                                   */
\/*    IR37157  On a MICR enabled printer, the balance due line is    */
\/*             doubled when voiding a check that has been printed.   */
\/*             The voided check is also printed twice with no print  */
\/*             of the original entered check.                        */
\/*             LTG     MGVA         1/19/98                          */
\/*                                                                   */
\/*    IR37432  On a non-micr sequence printer, if check printing is  */
\/*             bypassed and then the tender is voided using          */
\/*             VOID/ENTER when prompted B870 VERIFY THE SIGNATURE ON */
\/*             CHEQUE, the user is still prompted to frank the check.*/
\/*             LTG MGA 02Mar98                                       */
\/*                                                                   */
\/*    IR38857  When using a 4610 printer, if the check jams while    */
\/*             being flipped, some linefeeds may be lost, which      */
\/*             results in the lines being printed too low on the     */
\/*             check.                                                */
\/*             RSH MGA 16Oct98                                       */
\/*                                                                   */
\/*    IR39486  Add support for SA Multiple Currency Feature.         */
\/*             Select proper amount to print on check.               */
\/*             Select proper Monetary ID/Symbol to print on the      */
\/*             check if field I is defined.                          */
\/*             RDG IBM 19JAN99                                       */
\/*                                                                   */
\/*    IR42012  Override modifier MO does not print on receipt when   */
\/*             an override is entered and check printing is on.      */
\/*             The code was changed to ensure that the modifier MO   */
\/*             is printed.                                           */
\/*             MED MGV 13Jan2000                                     */
\/*                                                                   */
\/*    IR42464  Print override is not being reset when check tender   */
\/*             is cancelled using the clear key.  Change to Global   */
\/*             and reset in main line module.                        */
\/*             CMJ MGVA 2Mar2000                                     */
\/*                                                                   */
\/*    IR44077  Correct problem when VOID/ENTER is pressed during     */
\/*             Cheque Printing which doubles the selected currency.  */
\/*             CMJ MGVA 16Oct2000                                    */
\/*                                                                   */
\/*    IR46823  The minus sign is incorrectly printed beside the      */
\/*             currency tender line if the CHECK tender is entered   */
\/*             then voided during the Cheque Printing.               */
\/*             LLU MGVA 11Oct2001                                    */
\/*                                                                   */
\/*    IR48344  When a MICR check is tendered, the tender appears     */
\/*             twice on the receipt.  Use a flag to ensure that it   */
\/*             is printed only once.                                 */
\/*             GGK IBM  10May2002                                    */
\/*                                                                   */
\/*    IR48971  PE IR48344 and IR46823.  Backed out both APARS        */
\/*             and redesigned the fix for the problem reported       */
\/*             in IR46823.                                           */
\/*             GGK IBM  16Aug2002                                    */
\/*                                                                   */
\/*    IR51720  Add support for check imaging.Co-Req APAR IR51721     */
\/*             CRM IBM  16Jul2003                                    */
\/*                                                                   */
\/*    IR52980  Modified java GUI hooks for use with VP,              */
\/*             SureVision and AEF.                                   */
\/*             KHG IBM 27Jan2004                                     */
\/*                                                                   */
\/* END-OF-SPECIFICATIONS *********************************************/
\ REM!!************************ B B D L ******************************!!
\ REM********************* END BBDL STATEMENTS *************************

  %ENVIRON T

\ REM!!****************** INCLUDE FILES ******************************!!
\ REM!!    CPVARS.J86       Common variables for Cheque Printing      !!
\ REM!!    EAMTSWKG.J86     Common supermarket global variables       !!
\ REM!!    EAMTRANS.J86     Common supermarket transaction variables  !!
\ REM!!    EAMETWKB.J86     Keyboard variables including MSR          !!
\ REM!!***************************************************************!!

  %INCLUDE CPVARS.J86     ! Common variables for Cheque Printing
  %INCLUDE EAMTSWKG.J86   ! Common supermarket global variables
  %INCLUDE EAMTRANS.J86   ! Common supermarket transaction variables
  %INCLUDE EAMETWKB.J86   ! Keyboard variables  IR22496

  STRING GLOBAL           \
         FIELD.DATA$(1),  \ Array for check fields' data
         ERR.MSG.870$,    \ Verify signature message
         ERR.MSG.871$,    \ Enter card number message
         ERR.MSG.873$,    \ Insert cheque to be printed
         ERR.MSG.874$,    \ Remove cheque from printer
         OVR.PROMPT$,     \ ENTER to bypass, CLEAR to print
         CANNOT.PRINT$,   \ Cheque cannot be printed
         MM.STATUS$,      \ Used in ADXSERVE call
         PAYEE$,          \ PAYEE descriptor
         PAYEE.VOID$,     \ PAYEE void descriptor IR51720
         LOCATION$,       \ LOCATION descriptor
         CURR.DENOM$      ! CURRENCY DENOMINATION descriptor

  INTEGER*1  GLOBAL            \
             CUTOFF,           \ Was field truncated when placed in format?
             CHK.DIRECTION,    \ Direction check was inserted
             EJC.DIRECTION,    \ Direction check is to be ejected
             TO.NUMTNDR,       \ Number of tenders defined
             RELATE.FORMAT(1), \ Array corresponding format options to those
                               \   actually read into this terminal.
             PRINTING.CHEQUE,  \ Flag to say if in check printing
             TO.USEREXIT(1),   \ Terminal options userexit flags
             TO.DI.SPACING,    \ Option for DI spacing value
             TO.CR.SPACING,    \ Option for CR spacing value
             TO.SJ.SPACING     ! Option for SJ spacing value

  INTEGER*1 GLOBAL MICR.PRINT.SEQUENCE   ! IR37157 MICR Print seq. used

!AIR48971 Remove fix for IR48344
! INTEGER*1 GLOBAL MICR.PRINT.CHECKTND   ! IR48344 MICR Check tender on receipt
!EIR48971

  INTEGER*1 GLOBAL VOID.ENTER.CHECK      ! IR37432

  INTEGER*1 GLOBAL MC.VOID.ENTER.CHECK   ! IR44077

  INTEGER*2  GLOBAL               \
             TO.CHEQUE.FORMAT(1)  ! Array holding tender flag for CP

!AIR33229 - Added for IBM 4610 support
  %INCLUDE EAMP4VAJ.J86           ! 4610 flags
  %INCLUDE EAMP4VGJ.J86           ! Extended terminal options

  INTEGER*2 GLOBAL DI.FLIP        ! IR35829

!AIR42464 make override prompt global
! INTEGER*2  PRINT.OVR.PROMPT     ! bypass prompt result
  INTEGER*2 GLOBAL PRINT.OVR.PROMPT  ! bypass prompt result
!EIR42464

!AIR39486
  %INCLUDE EAMEXCHV.J86           ! Multiple Currency Feature Variable Def.
  STRING    GLOBAL                \
            MC.CONV.SEP$,         \ Feature Convenience Separator
            CPOP.ARR$(2),         \ Check Print options array.
            SAVED.MM.STATUS$      ! Saved MM Status byte 37

  INTEGER*2 GLOBAL                \
            PBC.SLOT,             \ PBC Slot Variable
            SEL.FC.SLOT           ! Selected FC Slot Variable

!AIR52980
  INTEGER*2 GLOBAL                \
            PRESET.SUBSTATE       !
!EIR52980

!EIR39486

  %INCLUDE JAVAGUIV.J86           ! IR52980

  %INCLUDE EAMP4EXJ.J86           ! external routines
!EIR33229

!AIR39486 - SA Multiple Currency Feature Routine Prototypes
  %INCLUDE EAMASMRT.J86           !  ASM SUBROUTINES
  %INCLUDE EAMMCTSJ.J86           ! Multiple Currency Feature Prototypes
!EIR39486

  %INCLUDE EAMXXCPY.J86           ! Copyright statement

!********************************************************
!  External Subroutine Declarations
!********************************************************

%INCLUDE JAVAGUIC.J86             ! IR52980

  SUB ADXSERVE (ADX.RET, FUNC, PARM, PARM$) EXTERNAL
  INTEGER*4  ADX.RET
  INTEGER*2  FUNC, PARM
  STRING     PARM$
  END SUB

  FUNCTION ALPHA.DATE$ EXTERNAL
  STRING    ALPHA.DATE$
  END FUNCTION

  FUNCTION CHECK.PRESENT (DI.SESSNUM) EXTERNAL
  INTEGER*1 CHECK.PRESENT
  INTEGER*2 DI.SESSNUM
  END FUNCTION

  FUNCTION LEGAL$ (AMOUNT$, DEC.POS) EXTERNAL
  STRING    AMOUNT$, LEGAL$
  INTEGER*1 DEC.POS
  END FUNCTION

  FUNCTION NEED.CARD.NO EXTERNAL
  INTEGER*1 NEED.CARD.NO
  END FUNCTION

  SUB FIELDS.TO.PRINT$ (INSERT$) EXTERNAL
  STRING INSERT$
  END SUB

  SUB MOVE.TO.CHECK.START (DI.SESSNUM, IN.DIRECTION) EXTERNAL
  INTEGER*2 DI.SESSNUM
  INTEGER*1 IN.DIRECTION
  END SUB

  SUB PRINT.CHECK (DI.SESSNUM, IN.DIRECTION) EXTERNAL
  INTEGER*2 DI.SESSNUM
  INTEGER*1 IN.DIRECTION
  END SUB

  SUB READ.CPDESC EXTERNAL
  END SUB

  SUB READ.FORMAT EXTERNAL
  END SUB

  SUB READ.FORMATOP EXTERNAL
  END SUB

  SUB TSDSEC01 EXTERNAL                   ! Display message
  END SUB

  SUB TSCSEC08 EXTERNAL                   ! Display and require clear only
  END SUB

  SUB TSCSECRK EXTERNAL                   ! Read the keyboard or scanner
  END SUB

  SUB TSHIECET EXTERNAL                   ! Issue a tone warning
  END SUB

  SUB TSPREC01 EXTERNAL                   ! Print routine
  END SUB

  SUB TSTPEC01 EXTERNAL                   ! Summary log string write
  END SUB

  FUNCTION TSUPEC54 EXTERNAL              ! userexit waiting for IOPROC
  END FUNCTION

!AIR51720
  FUNCTION ?IMAGE.NEEDED EXTERNAL
  INTEGER * 1 ?IMAGE.NEEDED
  END FUNCTION
!EIR51720

! Declarations for the userexits for Cheque Printing.

SUB CKUPEC01 EXTERNAL
END SUB

SUB CKUPEC02 EXTERNAL
END SUB

SUB CKUPEC03 EXTERNAL
END SUB

SUB CKUPEC04 EXTERNAL
END SUB


!***************************************
! TERMINAL SALES FEATURE EXIT FUNCTIONS
!***************************************

SUB CKTSEC01 PUBLIC             ! Subroutine called at Initialization

INTEGER*1 I,                    \
          NUM.CHEQUE.FORMATS    ! Number of cheque formats defined

!AIR33750 - Initialize check printing override variable to zero
    PRINT.OVR.PROMPT = 0
!EIR33750 - End modification

!!*****************************************************************************
!!   Dim Arrays to null to clear pointer space (saves room on reintializes).
!!*****************************************************************************

DIM FF.ROW(0,0),                \
    FF.START(0,0),              \
    FF.LENGTH(0,0),             \
    FF.FONT(0,0),               \
    FF.DS(0,0),                 \
    FF.CASE(0,0),               \
    FF.PAD.CHAR(0,0),           \
    FF.PAD.BEFORE(0,0),         \
    FF.PAD.AFTER(0,0),          \
    LEGAL.HUND.SEP$(0),         \
    LEGAL.CENT.SEP$(0),         \
    LEGAL.CENT.DESC$(0),        \
    LEGAL.DOL.DESC$(0),         \
    LEGAL.ONE.DOL.DESC$(0),     \
    LEGAL.EXACT$(0),            \
    CONV.SEP$(0),               \
    CP.DATEFORM(0),             \
    CP.DENOM$(0),               \
    CP.MONTH$(0)

   OPEN "R::ADXCPLGD" AS 64 NOWRITE NODEL           ! Read special proc.        ad speci
   READ #64; SPEC.PROC                              !   character for
   CLOSE 64                                         !   LEGAL$ function.


   READ # 20    ,1197; PAYEE$, TS.TEMP1$            ! Read in sales descs.
   READ # 20    ,1198; LOCATION$, TS.TEMP1$
   READ # 20    ,1199; CURR.DENOM$, TS.TEMP1$
   READ # 20    ,1200; ERR.MSG.870$, TS.TEMP1$
   READ # 20    ,1201; ERR.MSG.871$, TS.TEMP1$
   READ # 20    ,1203; ERR.MSG.873$, TS.TEMP1$
   READ # 20    ,1204; ERR.MSG.874$, TS.TEMP1$
   READ # 20    ,1205; OVR.PROMPT$, TS.TEMP1$
   READ # 20    ,1192; PAYEE.VOID$, TS.TEMP1$       !IR51720 get VOID for Payee


! For each tender defined, check the Cheque Format option.  If the option
! is used (non zero), set up indicator for reading formats later.

   DIM RELATE.FORMAT(6)
   FOR I = 0 TO 6
      RELATE.FORMAT(I) = 0
   NEXT I
   NUM.CHEQUE.FORMATS = 0

   FOR I = 1 TO TO.NUMTNDR
      IF TO.CHEQUE.FORMAT(I) <> 0 THEN BEGIN
         IF RELATE.FORMAT(TO.CHEQUE.FORMAT(I)) = 0 THEN BEGIN
            NUM.CHEQUE.FORMATS = NUM.CHEQUE.FORMATS + 1
            RELATE.FORMAT(TO.CHEQUE.FORMAT(I)) = NUM.CHEQUE.FORMATS
         ENDIF
      ENDIF
   NEXT I

   IF NUM.CHEQUE.FORMATS = 0 THEN GOTO DONE.01

DIM CP.DENOM$(39),              \
    CP.MONTH$(12)

   OPEN "R::ADXCPTRD" RECL 50 AS 64 NOWRITE NODEL   ! Read in descriptors
   CALL READ.CPDESC                                 !   for cheque.
   READ #64, 52; CANNOT.PRINT$
   CLOSE 64

!!*****************************************************************************
!!   Dim Arrays- 33 fields, NUM.CHEQUE.FORMATS total
!!*****************************************************************************

DIM FF.ROW(NUM.CHEQUE.FORMATS,33),               \
    FF.START(NUM.CHEQUE.FORMATS,33),             \
    FF.LENGTH(NUM.CHEQUE.FORMATS,33),            \
    FF.FONT(NUM.CHEQUE.FORMATS,33),              \
    FF.DS(NUM.CHEQUE.FORMATS,33),                \
    FF.CASE(NUM.CHEQUE.FORMATS,33),              \
    FF.PAD.CHAR(NUM.CHEQUE.FORMATS,33),          \
    FF.PAD.BEFORE(NUM.CHEQUE.FORMATS,33),        \
    FF.PAD.AFTER(NUM.CHEQUE.FORMATS,33),         \
    LEGAL.HUND.SEP$(NUM.CHEQUE.FORMATS),         \
    LEGAL.CENT.SEP$(NUM.CHEQUE.FORMATS),         \
    LEGAL.CENT.DESC$(NUM.CHEQUE.FORMATS),        \
    LEGAL.DOL.DESC$(NUM.CHEQUE.FORMATS),         \
    LEGAL.ONE.DOL.DESC$(NUM.CHEQUE.FORMATS),     \
    LEGAL.EXACT$(NUM.CHEQUE.FORMATS),            \
    CONV.SEP$(NUM.CHEQUE.FORMATS),               \
    CP.DATEFORM(NUM.CHEQUE.FORMATS)              !

!AIR39486 - Increase size of arrays if MC feature is enabled.
    IF MC.ENABLED THEN BEGIN                     ! If MC Enabled
      DIM FF.ROW(NUM.CHEQUE.FORMATS,34),         \ Dim Field Row
          FF.START(NUM.CHEQUE.FORMATS,34),       \     Field Start position
          FF.LENGTH(NUM.CHEQUE.FORMATS,34),      \     Field Length
          FF.FONT(NUM.CHEQUE.FORMATS,34),        \     Field Font
          FF.DS(NUM.CHEQUE.FORMATS,34),          \     Field Double Strike
          FF.CASE(NUM.CHEQUE.FORMATS,34),        \     Field Case
          FF.PAD.CHAR(NUM.CHEQUE.FORMATS,34),    \     Field Pad Character
          FF.PAD.BEFORE(NUM.CHEQUE.FORMATS,34),  \     Field Pad Before
          FF.PAD.AFTER(NUM.CHEQUE.FORMATS,34)    !     Field Pad After
    ENDIF                                        ! MC Enabled
!EIR39486
! Open the format file, and for each format declared read in that format.

   OPEN "R::ADXCPFMD" RECL 75 AS 64 BUFFSIZE 198 NOWRITE NODEL
   FOR I = 1 TO 6
      IF RELATE.FORMAT(I) <> 0 THEN BEGIN
         RD.CPFORMAT = I
         CPFORMAT = RELATE.FORMAT(I)
         CALL READ.FORMAT
         CALL READ.FORMATOP
      ENDIF
   NEXT I
   CLOSE 64

DONE.01:
   CALL CKUPEC02                 ! call cheque printing initial. exit

END SUB

!****************************************************************

SUB CKTSEC02 PUBLIC              ! Subroutine called at Tender Time

   INTEGER*4  RET                ! Return code from ADXSERVE

   INTEGER*4 PUTLI4              ! Working storage for GETLONG / PUTLONG

   INTEGER*2  PARM1,             \ Used in ADXSERVE call
              SAV.KEYS(1),       \ IR27950
              LEN.TMP,           \
              I, J               ! Loop control

   STRING TMP.VAL$,              \ Conversion string for packed fields
          SAV.DATA$(1),          \
          L.AMOUNT$,             \ Left portion of amount string
          R.AMOUNT$,             \ Right portion of amount string
          CP.DATE$,              \ Date of check being printed
          CP.TIME$               ! Time of check being printed


   CALL ADXSERVE (RET, 4, PARM1, MM.STATUS$)

   MC.VOID.ENTER.CHECK = 0       ! Ensure varible reset IR44077
! Determine is cheque printing is done for this tender.  If not, return to
! EAMTSTDC to continue tendering.

!AIR27950 save the keys as passed in case they are changed in routine
   DIM SAV.KEYS(10)
   DIM SAV.DATA$(10)
!  FOR I = 1 TO  10              ! IR42012 Remove this line
   FOR I = 0 TO  10              ! IR42012 Save all keys including pos 0
     SAV.KEYS(I)  = TS.IO.KEYS(I)
     SAV.DATA$(I) = TS.IO.DATA$(I)
   NEXT
!EIR27950

!AIR35829 Change code to allow verify signature message but eject check if a
!         check flip machine
!AIR33750 - added code to exit if bypass check printing is chosen
   ! If check printing already bypassed then exit
!  IF (PRT4610.ENABLE <> 0) AND (PRINT.OVR.PROMPT = 80) THEN GOTO DONE.ROUTINE
   IF (PRT4610.ENABLE <> 0) AND (PRINT.OVR.PROMPT = 80) AND (DI.FLIP) THEN BEGIN
     CALL PRT4610.DI.EJECT(35)
   ENDIF
!EIR33750
!EIR35829

   IF SL.TE.AMTTENDE = 0 THEN GOTO DONE.ROUTINE
   IF TS.IO.KEYS(1) = 70 THEN GOTO DONE.ROUTINE      ! Leave if VOID

   CPFORMAT = RELATE.FORMAT(TO.CHEQUE.FORMAT(TS.TDR.INDEX))

   CALL CKUPEC03

   IF CPFORMAT = 0 THEN GOTO DONE.ROUTINE

!AIR33229 - Skip check printing on 4610 if disabled
   IF PRT4610.ENABLE AND NOT EP.ETO.MICR.PRINT.FACE THEN GOTO DONE.ROUTINE
!EIR33229

! Set up data for cheque printing.  Values are put into FIELD.DATA$ array.

!AIR39486 - Set number of fields based on if SA MC Feature is present or not.
!  DIM FIELD.DATA$(33)
!  FOR CPFIELD = 1 TO 33
!      FIELD.DATA$(CPFIELD) = ""
!  NEXT CPFIELD

   IF NOT(MC.ENABLED) THEN BEGIN                     ! If not MC Enable
     DIM FIELD.DATA$(33)                             ! Dim requried fields
     FOR CPFIELD = 1 TO 33                           ! Loop through fields
       FIELD.DATA$(CPFIELD) = ""                     ! reset data
     NEXT CPFIELD                                    ! next field
   ENDIF ELSE BEGIN                                  ! else if MC Enabled
     DIM FIELD.DATA$(34)                             ! Dim required fields
     FOR CPFIELD = 1 TO 34                           ! loop through fields
       FIELD.DATA$(CPFIELD) = ""                     ! reset data
     NEXT CPFIELD                                    ! next field
   ENDIF                                             ! not MC Enabled
!EIR39486

   IF NOT(PRT4610.ENABLE) THEN           \ Don't wait 4610 ptr IR36401
   WAIT ; 2000                           ! Wait to all other printing complete

! If a card number is required for this cheque format, prompt the operator
! to Enter one.

   IF NEED.CARD.NO THEN BEGIN
GET.CARD:
      TS.TEMP1$ = "B871 " + LEFT$(ERR.MSG.871$+BLANK$, 15)
      TS.TEMP2$ = MID$(ERR.MSG.871$+BLANK$, 16, 20)          ! Prompt to enter
      TS.LINETYPE = 12                                       !   card number.
      jGuiSubState = 210001                                  ! IR52980
      PRESET.SUBSTATE = -1                                   ! IR52980
      CALL TSCSEC08
      UNLOCKDEV 32, 8, PRIORITY                              ! Set input state
                                                             !   to ACCT/OVR.
!AIR22496
!     UNLOCKDEV 41                                           ! Unlock the MSR.
!     WAIT 32, 41; 10000
      IF TS.MSR.ONKBD THEN UNLOCKDEV 41                      ! Unlock the MSR.
      IF TS.MSR.ONKBD THEN BEGIN
         WAIT 32, 41; 10000
      ENDIF ELSE WAIT 32; 10000
!EIR22496

      IF TO.USEREXIT(54) THEN BEGIN                          ! IF USER EXIT SET
         CALL TSUPEC54                                       ! EXIT AT TIMEOUT
      ENDIF

      IF EVENT% = 0 THEN GOTO GET.CARD                       ! If timeout, try again
      IF EVENT% = 32 THEN BEGIN                              ! If input from IOPROC then
         CALL TSCSECRK                                       !   read the keyboard
      ENDIF

      IF EVENT% = 41 THEN BEGIN                              ! If input from MSR then,
         TS.IO.MOTORKEY = 80                                 !   act like an ENTER
         READ #41 ; LINE TS.TEMP1$                           !   read the data
         TS.TEMP1I2 = MATCH(CHR$(13),TS.TEMP1$,1)
         IF TS.TEMP1I2 = 0 THEN BEGIN                        ! Break out the account
            TS.TEMP1I2 = 99                                  !  (card) number from
         ENDIF                                               !  the MSR data string
         TS.TEMP2I2 = MATCH(CHR$(61),TS.TEMP1$,1)            !  entered.
         IF TS.TEMP2I2 = 0 THEN BEGIN
            TS.TEMP2I2 = 99
         ENDIF
         IF TS.TEMP2I2 < TS.TEMP1I2 THEN BEGIN
            TS.TEMP1I2 = TS.TEMP2I2
         ENDIF
         TS.TEMP2$ = LEFT$(TS.TEMP1$,TS.TEMP1I2-1)
         TS.IO.DATA$(10) = UNPACK$(PACK$(TS.TEMP2$))
      ENDIF
      LOCKDEV 32

!AIR22496
!     LOCKDEV 41
      IF TS.MSR.ONKBD THEN LOCKDEV 41
!EIR22496

      IF TS.IO.MOTORKEY = 0 THEN GOTO GET.CARD               ! If an error, retry.
      IF (TS.IO.MOTORKEY = 73) THEN BEGIN                    ! If CLEAR then,
         FIELD.DATA$(7) = ""                                 ! no card number entered.
      ENDIF ELSE IF (TS.IO.MOTORKEY = 80) AND                \ If ENTER pressed and
         (TS.IO.DATA$(10) <> "")  THEN BEGIN                 !    a number entered,
         FIELD.DATA$(7) = TS.IO.DATA$(10)                    ! assign card number.
      ENDIF ELSE BEGIN                                       ! Else invalid key.
         GOTO GET.CARD
      ENDIF
   ENDIF


! Set up date for FIELD.DATA$(1).  Check option CP.DATEFORM to determine if the
! date is to be in SYSTEM CONFIGURED format or DDMMMYY format.

   CP.DATE$ = DATE$              ! in YYMMDD format
   CP.TIME$ = TIME$              ! in HHMMSS format
   IF CP.DATEFORM(CPFORMAT) = 2 THEN BEGIN                 ! Use format where
      FIELD.DATA$(1) = ALPHA.DATE$                         ! 1/1/91 is 1JAN91
   ENDIF ELSE BEGIN
      IF (VAL(MID$(MM.STATUS$,5,1)) = 1) OR                   \ mm/dd/yy or
         (VAL(MID$(MM.STATUS$,5,1)) = 3) THEN BEGIN           !     mm.dd.yy
         FIELD.DATA$(1) = MID$(CP.DATE$,3,2) +  TS.DATESEP$ + \
                          RIGHT$(CP.DATE$,2) +  TS.DATESEP$ + \
                          LEFT$(CP.DATE$,2)
      ENDIF ELSE BEGIN                                        ! dd/mm/yy or
         FIELD.DATA$(1) = RIGHT$(CP.DATE$,2) +  TS.DATESEP$ + \     dd.mm.yy
                          MID$(CP.DATE$,3,2) +  TS.DATESEP$ + \
                          LEFT$(CP.DATE$,2)
      ENDIF
   ENDIF

!AIR39486
   IF MC.ENABLED THEN BEGIN                                ! If MC Feat Enabled
     IF SEL.FC.SLOT = 0 THEN BEGIN                         ! If not set
       SEL.FC.SLOT = 1                                     ! force to PBC
     ENDIF                                                 ! not set
     IF CPOP.ARR$(SEL.FC.SLOT,8) = "2" THEN BEGIN          ! Use format where
       FIELD.DATA$(1) = ALPHA.DATE$                        ! 1/1/91 is 1JAN91
     ENDIF                                                 ! use 1/1/91 format
   ENDIF                                                   ! MC Feat Enabled
!EIR39486

! Set up Convenience Amount in FIELD.DATA$(3) from amount tendered.  Place the
! proper seperator between the whole dollar amount and change.

   FIELD.DATA$(3) = UNPACK$(STR$(SL.TE.AMTTENDE))

!AIR39486 - Select proper amount to Print
   IF MC.ENABLED THEN BEGIN                                ! If MC Feat Active
     IF SEL.FC.SLOT <> PBC.SLOT THEN BEGIN                 ! If !Pri Base Slot
       FIELD.DATA$(3) = UNPACK$(STR$(FC.AMTTENDE))         ! Use FC Amount
     ENDIF                                                 ! !Pri Base Slot
   ENDIF                                                   ! MC Feat Active
!EIR39486

   TMP.VAL$ = ""
   LEN.TMP = LEN(FIELD.DATA$(3))
   FOR I = 1 TO LEN.TMP STEP 2
      TMP.VAL$ = TMP.VAL$ + STR$(VAL(MID$(FIELD.DATA$(3),I,2)) - 30)
   NEXT I
   FIELD.DATA$(3) = TMP.VAL$
   LEN.TMP = LEN(FIELD.DATA$(3))
!AIR39486
   IF MC.ENABLED THEN BEGIN           ! If MC Feat Enabled
     SAVED.MM.STATUS$ = ""            ! Reset Variable
     IF MID$(MM.STATUS$,37,1) <>      \ If System decimal flag ne
             FCER.ARR$(SEL.FC.SLOT,7) \  FC Exchange Rate decimal flag
                           THEN BEGIN !
       SAVED.MM.STATUS$ =             \ Save System decimal flag
                MID$(MM.STATUS$,37,1) !
       CALL SUBSTR(MM.STATUS$,36,     \ Insert FC Exchange Rate decimal
        FCER.ARR$(SEL.FC.SLOT,7),0,1) !  flag.
     ENDIF                            ! System decimal flag ne ER decimal flag
   ENDIF                              ! MC Feat Enabled
!EIR39486
   IF VAL(MID$(MM.STATUS$,37,1)) <> 0 THEN BEGIN
      IF LEN(FIELD.DATA$(3)) = 1 THEN \
         FIELD.DATA$(3) = "0" + FIELD.DATA$(3)
      IF LEN(FIELD.DATA$(3)) = 2 THEN \
         FIELD.DATA$(3) = "0" + FIELD.DATA$(3)
      L.AMOUNT$   = LEFT$(FIELD.DATA$(3), LEN(FIELD.DATA$(3)) - 2)
      R.AMOUNT$   = RIGHT$(FIELD.DATA$(3), 2)
      IF CONV.SEP$(CPFORMAT) = "" THEN BEGIN
         FIELD.DATA$(3) = L.AMOUNT$ + TS.CENTSEP$ + R.AMOUNT$
      ENDIF ELSE BEGIN
         FIELD.DATA$(3) = L.AMOUNT$ + CONV.SEP$(CPFORMAT) + R.AMOUNT$
      ENDIF
!AIR39486 - Set proper Convenience Separator
      IF MC.ENABLED THEN BEGIN                             ! If MC Feat Enabled
        IF SEL.FC.SLOT = 0 THEN BEGIN                      ! If not set
          SEL.FC.SLOT = 1                                  ! force to PBC
        ENDIF                                              ! not set
        IF CPOP.ARR$(SEL.FC.SLOT,7) <> "" THEN BEGIN       ! If defined in ERF
          MC.CONV.SEP$ = CPOP.ARR$(SEL.FC.SLOT,7)          ! get from ERF
        ENDIF ELSE BEGIN                                   ! else if not defined
          MC.CONV.SEP$ = CONV.SEP$(CPFORMAT)               ! use base definition
        ENDIF                                              ! defined in ERF
        if MC.CONV.SEP$ = "" THEN BEGIN                    ! If no separator
          if FCER.ARR$(SEL.FC.SLOT,7) = "0" then BEGIN     ! if curr. doesn't
                                                           !  use separator.
            FIELD.DATA$(3) = L.AMOUNT$                   + \ Assemble data
                             R.AMOUNT$                     !  without sep.
          ENDIF ELSE BEGIN
            FIELD.DATA$(3) = L.AMOUNT$                   + \ Use System
                             TS.CENTSEP$                 + \  defined.
                             R.AMOUNT$                     !
          ENDIF
        ENDIF ELSE BEGIN                                   ! else if separator
          FIELD.DATA$(3) = L.AMOUNT$                     + \
                           MC.CONV.SEP$                  + \ use feature defined
                           R.AMOUNT$                       !
        ENDIF                                              ! no separator
      ENDIF                                                ! MC Feat Enabled
!EIR39486
   ENDIF

   FIELD.DATA$(4) = LEGAL$(FIELD.DATA$(3), 37)         ! Legal Amount
!AIR39486
   IF MC.ENABLED THEN BEGIN                            ! If MC Feat Enabled
     IF SAVED.MM.STATUS$ <> "" THEN BEGIN              ! If saved value
       CALL SUBSTR(MM.STATUS$,36, SAVED.MM.STATUS$,0,1)! Restore System decimal flag
     ENDIF                                             ! saved value
   ENDIF                                               ! MC Feat Enabled
!EIR39486
   FIELD.DATA$(2) = PAYEE$                             ! Payee
   FIELD.DATA$(5) = CURR.DENOM$                        ! Currency Denom.
   FIELD.DATA$(6) = LOCATION$                          ! Location
   FIELD.DATA$(8) = TS.STORE$                          ! Store number
   FIELD.DATA$(9) = UNPACK$(TS.OPER$)                  ! Operator number
   FIELD.DATA$(10) = TS.TERMINAL$                      ! Terminal number
   FIELD.DATA$(11) = UNPACK$(STR$(SL.HD.TRANSNUM))     ! Transaction number

!AIR51720 override user field 20 (field.data$(32)) line with void void sdesc 1192
   IF ?IMAGE.NEEDED THEN BEGIN
     FIELD.DATA$(32) = PAYEE.VOID$                      ! user field 20
   ENDIF
!EIR51720

   TMP.VAL$ = ""
   LEN.TMP = LEN(FIELD.DATA$(11))
   FOR I = 1 TO LEN.TMP STEP 2
      TMP.VAL$ = TMP.VAL$ + STR$(VAL(MID$(FIELD.DATA$(11),I,2)) - 30)
   NEXT I
   FIELD.DATA$(11) = RIGHT$("0000" + STR$(VAL(TMP.VAL$)+1),4)
   FIELD.DATA$(12) = LEFT$(CP.TIME$,2) + TS.TIMESEP$ + MID$(CP.TIME$,3,2)

!AIR39486 - Get the selected Currency ID/Symbol
   IF MC.ENABLED THEN BEGIN                            ! If MC Feature active
     CALL GET.CURRENCY.ID                              ! get the currency ID
     FIELD.DATA$(34) = TS.TEMP1$                       ! put into field data
   ENDIF                                               ! MC Feature active
!EIR39486


! To save on memory usage, clear the strings that are no longer needed.
   TMP.VAL$ = ""
   L.AMOUNT$ = ""
   R.AMOUNT$ = ""
   CP.DATE$ = ""
   CP.TIME$ = ""
   TS.TEMP1$ = ""
   TS.TEMP2$ = ""

   CALL CKUPEC01                          ! Call userexit to set up user fields

   DIM CHECK.PRINT$(8,8)                  ! Dimension lines for cheque output.
   FOR I = 1 TO 8                         ! For each line and
      FOR J = 1 TO 8                      !   for each font combination,
         CHECK.PRINT$(I,J) = ""           ! Initial the output lines to null.
      NEXT J
   NEXT I

!AIR39486 - Verify fields (Number depends on if SA MC Feature is active)
!  FOR CPFIELD = 1 TO 33
!     CUTOFF = 0
!     CALL FIELDS.TO.PRINT$(FIELD.DATA$(CPFIELD))
!     IF CUTOFF THEN BEGIN                                     ! Cannot print cheque
!        TMP.VAL$ = STR$(CPFIELD)                              !   due to truncation
!        IF LEN(TMP.VAL$) = 1 THEN TMP.VAL$ = "0" + TMP.VAL$   !   of field CPFIELD.
!        TS.TEMP1$ = LEFT$(CANNOT.PRINT$,20)
!        TS.TEMP2$ = MID$(CANNOT.PRINT$,21,18) + TMP.VAL$
!        TS.LINETYPE = 12                                      ! Display message
!        CALL TSCSEC08                                         ! Wait for CLEAR
!        DIM FIELD.DATA$(0)
!        GOTO GET.VRF                                          ! Verify signaturnting.
!     ENDIF
!  NEXT CPFIELD

   IF NOT(MC.ENABLED) THEN BEGIN                           ! If not MC Enabled
     FOR CPFIELD = 1 TO 33                                 ! Loop through fields
       CUTOFF = 0                                          ! reset variable
       CALL FIELDS.TO.PRINT$(FIELD.DATA$(CPFIELD))         ! get fields
       IF CUTOFF THEN BEGIN                                ! Cannot print cheque
         TMP.VAL$ = STR$(CPFIELD)                          !  due to truncation
         IF LEN(TMP.VAL$) = 1 THEN TMP.VAL$ = "0"        + \
                                                TMP.VAL$   !  of field CPFIELD.
         TS.TEMP1$ = LEFT$(CANNOT.PRINT$,20)               ! Set temp1 variable
         TS.TEMP2$ = MID$(CANNOT.PRINT$,21,18) + TMP.VAL$  ! Set temp2 variable
         TS.LINETYPE = 12                                  ! Display message
         CALL TSCSEC08                                     ! Wait for CLEAR
         DIM FIELD.DATA$(0)                                ! Clear field
         GOTO GET.VRF                                      ! Verify signaturnting.
       ENDIF                                               ! cannot print cheque
     NEXT CPFIELD                                          ! next field
   ENDIF ELSE BEGIN                                        ! else if MC Enabled
     FOR CPFIELD = 1 TO 34                                 ! Loop through fields
       CUTOFF = 0                                          ! reset variable
       CALL FIELDS.TO.PRINT$(FIELD.DATA$(CPFIELD))         ! get fields
       IF CUTOFF THEN BEGIN                                ! Cannot print cheque   ue
         TMP.VAL$ = STR$(CPFIELD)                          !  due to truncation    n
         IF LEN(TMP.VAL$) = 1 THEN TMP.VAL$ = "0"        + \
                                                TMP.VAL$   !  of field CPFIELD.
         TS.TEMP1$ = LEFT$(CANNOT.PRINT$,20)               ! Set temp1 variable
         TS.TEMP2$ = MID$(CANNOT.PRINT$,21,18) + TMP.VAL$  ! Set temp2 variable
         TS.LINETYPE = 12                                  ! Display message
         CALL TSCSEC08                                     ! Wait for CLEAR
         DIM FIELD.DATA$(0)                                ! Clear field
         GOTO GET.VRF                                      ! Verify signaturnting. ng.
       ENDIF                                               ! cannot print cheque
     NEXT CPFIELD                                          ! next field
   ENDIF                                                   ! not MC Enabled
!EIR39486
   DIM FIELD.DATA$(0)

!AIR48971 Deleted the entire block moved here by IR46823
!AIR46823 Moved IR37157 here to print bal and tender before VOID/ENTER
!AIR37157 make sure check is printed first before void reduction
!  IF MICR.PRINT.SEQUENCE THEN BEGIN
!    IF (TS.PR.BALDUE <> (TS.TOTALS(0,0,0) + TS.TAXES(0,0))) THEN BEGIN
!      IF (TS.PROCEDURE <> 1) THEN BEGIN    ! if fn not tdr cashing
!        TS.FSBAL.PRINTED = 0               ! fs bal due not printed
!        TS.PR.BALDUE = TS.TOTALS(0,0,0) + TS.TAXES(0,0)
!                                           ! save printed baldue
!        TS.TAX.SAVE = TS.TAXES(0, 0)       ! save tax amt printed
!        TS.LINETYPE = 7                    ! set print parms
!        TS.LINEDATA = 1                    ! regular bal due
!        CALL TSPREC01                      ! print bal/tax
!      ENDIF                                !
!    ENDIF                                  !
!    IF (TS.ACNUM$ <> "") THEN BEGIN        ! if tender verified
!      TS.XXMOD = 11                        ! set "VF" modifier
!    ENDIF                                  !
!    TS.LINETYPE = 2
!    TS.PRT.PARM = TS.PRNTOPTS(19)          ! set print options
!    CALL TSPREC01
!    MICR.PRINT.CHECKTND = -1               ! Check tender is printed on receipt IR48344
!  ENDIF
!EIR37157
!EIR46823
!EIR48971

!AIR33229 - If 4610 override prompt already done then branch
   ! If check printing already bypassed then go get signature verification
   IF PRINT.OVR.PROMPT = 80 THEN GOTO GET.VRF

   ! If check printing has been confirmed then ...
   IF PRINT.OVR.PROMPT = 73 THEN BEGIN
      ! If a check is present assume it is face down
      CHK.DIRECTION = CHECK.PRESENT(35)
      IF CHK.DIRECTION <> 0 THEN BEGIN
        IF (DI.FLIP) OR (NOT(PRT4610.ENABLE)) THEN BEGIN   ! IR35815
         ! Flip the check
         CALL PRT4610.DI.FLIP(35)
!AIR38857  Ensure flip is complete
         TCLOSE 35
!EIR38857
         ! Print the check now if no pause
         IF NOT EP.ETO.MICR.PAUSE THEN GOTO BEGIN.PRT
        ENDIF !IR35815 If 4610 printer put no flipper, then insert check
      ENDIF
   ENDIF
!EIR33229

!AIR37053 - This code was added to ensure that the print head is home
!           prior to writing the face of a check.  Otherwise, the face of
!           the check may be missing the bottom line.
   IF (PRT4610.ENABLE) THEN BEGIN                         ! 4610 printer
     CALL PRT4610.DI.HOME(35,0)                           ! ensure head is home
     CALL PRT4610.DI.TIMEOUT(35,4,2)                      ! grab check when present
   ENDIF                                                  ! 4610 printer
!EIR37053

INSERT.CHK:
   TS.TEMP1$ = "B873 " + LEFT$(ERR.MSG.873$+BLANK$, 15)
   TS.TEMP2$ = MID$(ERR.MSG.873$+BLANK$, 16, 20)
   TS.LINETYPE = 12                                       ! Prompt to insert
   jGuiSubState = 210002                                  ! IR52980
   PRESET.SUBSTATE = -1                                   ! IR52980
   CALL TSCSEC08                                          !  check, wait for CLEAR
   CHK.DIRECTION = CHECK.PRESENT(35)                      ! If check in,
   IF CHK.DIRECTION <> 0 THEN GOTO BEGIN.PRT              !   print it.

GET.OVR:
!AIR33229 - If bypass prompt already done then branch
   IF PRINT.OVR.PROMPT = 80 THEN GOTO GET.VRF
   IF PRINT.OVR.PROMPT = 73 THEN GOTO PRE.PRT
!EIR33229

   TS.TEMP1$ = LEFT$(OVR.PROMPT$, 20)                     ! Prompt with bypass/
   TS.TEMP2$ = RIGHT$(OVR.PROMPT$, 20)                    !   print message
   TS.LINETYPE = 12
   jGuiSubState = 210004                                  ! IR52980
   PRESET.SUBSTATE = -1                                   ! IR52980
   CALL TSDSEC01
   UNLOCKDEV 32, 9, PRIORITY                              ! Set input state
!AIR52980                                                 !   to ENT/CLR.
   WAIT 32;10
   IF EVENT% = 32 THEN BEGIN
!EIR52980
     CALL TSCSECRK                                        ! Read the keyboard.
     LOCKDEV 32
!AIR52980
   ENDIF ELSE BEGIN
     GOTO GET.OVR
   ENDIF
!EIR52980
   IF TS.IO.MOTORKEY = 80 THEN BEGIN                      ! If ENTER then
      GOTO GET.VRF                                        !    verify signature.
   ENDIF
   IF TS.IO.MOTORKEY <> 73 THEN GOTO GET.OVR              ! If not CLEAR, reget key

!AIR33229 - Added label below
PRE.PRT:
!EIR33229
   PRINTING.CHEQUE = -1                                   ! Printing is about to begin.
   CHK.DIRECTION = CHECK.PRESENT(35)
   IF CHK.DIRECTION = 0 THEN BEGIN
      GOTO INSERT.CHK
   ENDIF

BEGIN.PRT:
   PRINTING.CHEQUE = -1                           ! Printing is about to begin.
   TS.TEMP1$ = STRING$(20," ")                    ! Display a blank prompt to
   TS.TEMP2$ = STRING$(20," ")                    ! avoid the bypass/print
   TS.LINETYPE = 12                               ! message from appearing
   CALL TSDSEC01                                  ! during cheque printing.

   CALL MOVE.TO.CHECK.START(35, CHK.DIRECTION)
   EJC.DIRECTION = CHK.DIRECTION
   CALL CKUPEC04                                  ! call userexit if the
                                                  !  eject direction is
                                                  !  to be changed.
   CALL PRINT.CHECK(35, EJC.DIRECTION)
   DIM CHECK.PRINT$(0,0)

   TCLOSE  35

   CHK.DIRECTION = CHECK.PRESENT(35)
   IF CHK.DIRECTION <> 0 THEN BEGIN               ! If doc still present
      CALL TSHIECET                               ! Sound Tone
      TS.TEMP1$ = "B874 " + LEFT$(ERR.MSG.874$+BLANK$, 15)
      TS.TEMP2$ = MID$(ERR.MSG.874$+BLANK$, 16, 20)
      TS.LINETYPE = 12                            ! Display as passed
      jGuiSubState = 210003                       ! IR52980
      PRESET.SUBSTATE = -1                        ! IR52980
      CALL TSDSEC01                               ! Display "B874"
      WHILE CHK.DIRECTION <> 0                    ! While still present
         WAIT ;250                                ! Delay a little
         CHK.DIRECTION = CHECK.PRESENT(35)        ! Status from printer
      WEND
   ENDIF
   PRINTING.CHEQUE = 0                            ! Printing is complete.


! This code is to reset the printer station attributes as were defined back
! in initialization.  This insures that none of the station activity has
! left some wrong bits set.

!AIR33229 - Added IBM 4610 support
IF NOT PRT4610.ENABLE THEN BEGIN
!EIR33229

   PUTLI4 = 000000010H                            ! Automatic DI, no doc
                                                  !   removal during print lines,
                                                  !   no doc insert during CR printing
                                                  !   no doc sensing required for
                                                  !   CR printing, doc linefeed in
                                                  !   normal mode.
   IF TO.DI.SPACING THEN BEGIN                    ! Reset DI spacing if 8 LPI
      PUTLI4 = PUTLI4 + 000008000H
   ENDIF
   IF TO.SJ.SPACING THEN BEGIN                    ! Reset SJ spacing if 8 LPI
      PUTLI4 = PUTLI4 + 000004000H
   ENDIF
   IF TO.CR.SPACING THEN BEGIN                    ! Reset CR spacing if 8 LPI
      PUTLI4 = PUTLI4 + 000002000H
   ENDIF
   PUTLONG 35, PUTLI4

!AIR33229 - Added IBM 4610 support
ENDIF
!EIR33229

GET.VRF:
   CALL TSHIECET                                          ! Sound Tone
   TS.TEMP1$ = "B870 " + LEFT$(ERR.MSG.870$+BLANK$, 15)   ! Prompt to
   TS.TEMP2$ = MID$(ERR.MSG.870$+BLANK$, 16, 20)          !   verify signature
   TS.LINETYPE = 12
   jGuiSubState = 210000                                  ! IR52980
   PRESET.SUBSTATE = -1                                   ! IR52980
   CALL TSDSEC01
   UNLOCKDEV 32, 10, PRIORITY                             ! Set input state
!AIR52980
RETRY.GETKEY:                                             !   to MAIN.
   WAIT 32;1000
   IF EVENT% = 32 THEN BEGIN
!EIR52980
     CALL TSCSECRK                                        ! Read the keyboard.
     LOCKDEV 32

!AIR52980
   ENDIF ELSE BEGIN
     GOTO RETRY.GETKEY
   ENDIF
!EIR52980
   IF TS.IO.MOTORKEY <> 80 THEN GOTO GET.VRF              !  If not ENTER
                                                          !     do again.

   IF TS.TEMP1I2 = 2 AND                                  \  If 2 keys,
      TS.IO.KEYS(1) = 70 AND                              \     VOID and
      TS.IO.KEYS(2) = 80 THEN BEGIN                       !     ENTER,
                                                          !    cancel check

      TS.TEMP1I1 = 5                                      ! Tender string
      CALL TSTPEC01                                       ! Log first tender,
                                                          !  to balance VOID
                                                          !  tender following.
!AIR48971 Restored the save of VOID+ENTER.  Also changed VOID+ENTER
!         to ENTER so that the check appears as a +tender first.
!AIR46823
!     SAV.KEYS(1) = 70                         ! save void key
!     SAV.KEYS(2) = 80                         ! save enter key

      SAV.KEYS(1) = 70                         ! save void key
      SAV.KEYS(2) = 80                         ! save enter key
      TS.IO.KEYS(1) = 80                       ! change VOID to ENTER
!!code below for printing bal and tender is moved prior to GET.VRF
!!AIR37157 make sure check is printed first before void reduction
! The block of code below was removed with IR46823, reinstated with IR48971
      IF MICR.PRINT.SEQUENCE THEN BEGIN
        IF (TS.PR.BALDUE <> (TS.TOTALS(0,0,0) + TS.TAXES(0,0))) THEN BEGIN
          IF (TS.PROCEDURE <> 1) THEN BEGIN    ! if fn not tdr cashing
            TS.FSBAL.PRINTED = 0               ! fs bal due not printed
            TS.PR.BALDUE = TS.TOTALS(0,0,0) + TS.TAXES(0,0)
                                               ! save printed baldue
            TS.TAX.SAVE = TS.TAXES(0, 0)       ! save tax amt printed
            TS.LINETYPE = 7                    ! set print parms
            TS.LINEDATA = 1                    ! regular bal due
            CALL TSPREC01                      ! print bal/tax
          ENDIF                                !
        ENDIF                                  !
        IF (TS.ACNUM$ <> "") THEN BEGIN        ! if tender verified
          TS.XXMOD = 11                        ! set "VF" modifier
        ENDIF                                  !
        TS.LINETYPE = 2
        TS.PRT.PARM = TS.PRNTOPTS(19)          ! set print options
        CALL TSPREC01
      ENDIF
!!EIR37157
!EIR46823
!EIR48971

!  Subtract tender from total tender as well as total for tender type.
!  Subtract tender from verified tender amounts as well as # of tenders.
!  Add tender back to balance due.  Subtract any fee from balance due.
!  Reverse tender and fee, and do a write to CR and SJ.
!  Set IO.KEY to 1 so that regular tender processing will process
!  the tender as a VOIDED tender for TLOG.

      TS.TENDERED(0   ) = TS.TENDERED(0   ) - SL.TE.AMTTENDE
      TS.TENDERED(SL.TE.TENDTYPE) = \
         TS.TENDERED(SL.TE.TENDTYPE) - SL.TE.AMTTENDE
!AIR26878 decrement the number of tenders by 1
!     TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) - SL.TE.AMTTENDE
      TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) - 1  !IR26878
!EIR26878
      TS.TENDVAMT(TS.TDR.INDEX) = TS.TENDVAMT(TS.TDR.INDEX) - SL.TE.AMTTENDE
      TS.BALDUE(0) = TS.BALDUE(0) + SL.TE.AMTTENDE
      TS.BALDUE(0) = TS.BALDUE(0) - SL.TE.AMTTNFEE
      SL.TE.AMTTNFEE = -SL.TE.AMTTNFEE
      SL.TE.AMTTENDE = -SL.TE.AMTTENDE
!AIR37157 print negative line if not micr sequence
      IF (NOT(MICR.PRINT.SEQUENCE)) THEN BEGIN
        TS.XXMOD = 8
        TS.LINETYPE = 2
        TS.PRT.PARM = TS.PRNTOPTS(19)
        CALL TSPREC01
        VOID.ENTER.CHECK = -1     ! IR37432 set flag so no franking will occur
        MC.VOID.ENTER.CHECK = -1  ! IR44077
      ENDIF
!EIR37157
!AIR48971
!!    TS.IO.KEYS(1) = 70          ! IR46823
      TS.IO.KEYS(1) = 70          ! Restore this for IR48971
!EIR48971
   ENDIF ELSE BEGIN
      IF TS.TEMP1I2 > 1 THEN GOTO GET.VRF         ! If ENTER and any
                                                  !  other key then do
                                                  !  again.
   ENDIF

DONE.ROUTINE:

   TS.TEMP1$ = STRING$(20," ")                    ! Display a blank prompt to
   TS.TEMP2$ = STRING$(20," ")                    ! avoid the verify signature
   TS.LINETYPE = 12                               ! message from appearing
   CALL TSDSEC01                                  ! during franking.

   DIM CHECK.PRINT$(0,0)                          ! Clear array to reclaim memory

!AIR27950 restore keys as passed
!  FOR I = 1 TO  10              ! IR42012 Remove this line
   FOR I = 0 TO  10              ! IR42012 Restore all keys including pos 0
     TS.IO.KEYS(I)  = SAV.KEYS(I)
     TS.IO.DATA$(I) = SAV.DATA$(I)
   NEXT
!EIR27950

   PRINTING.CHEQUE = 0                            ! IR24496 in 4680 code

!AIR33750 - Clear check printing override variable
   PRINT.OVR.PROMPT = 0
!EIR33750 - End modification

END SUB

!
!AIR33229 - Added following pre-franking check routine
!
SUB CKTSEC03 PUBLIC
   INTEGER*2 I, SAV.KEYS(1)
   STRING SAV.DATA$(1)



!AIR33750 - Do nothing if prompt has already been presented
!  ! Assume no override prompt
!  PRINT.OVR.PROMPT = 0
   IF PRINT.OVR.PROMPT <> 0 THEN EXIT SUB
!EIR33750 - End modification

   ! If not IBM 4610 then just leave
   IF NOT PRT4610.ENABLE THEN EXIT SUB

   ! If no tender amount then just leave
   IF SL.TE.AMTTENDE = 0 THEN EXIT SUB

   ! If void then just leave
   IF TS.IO.KEYS(1) = 70 THEN EXIT SUB

   ! If no check format for this tender variety then just leave
   IF (RELATE.FORMAT(TO.CHEQUE.FORMAT(TS.TDR.INDEX)) = 0) THEN EXIT SUB

   ! If 4610 check printing disabled then leave
   IF NOT EP.ETO.MICR.PRINT.FACE THEN EXIT SUB

   DIM SAV.KEYS(10)
   DIM SAV.DATA$(10)
   FOR I = 1 TO 10
     SAV.KEYS(I)  = TS.IO.KEYS(I)
     SAV.DATA$(I) = TS.IO.DATA$(I)
   NEXT I

   ! Prompt until CLEAR or ENTER pressed
!AIR52980
   TS.TEMP1$ = LEFT$(OVR.PROMPT$, 20)      !   Prompt with bypass
   TS.TEMP2$ = RIGHT$(OVR.PROMPT$, 20)     !   print message
   TS.LINETYPE = 12
   CALL TSDSEC01
   UNLOCKDEV 32, 9, PRIORITY            ! Set input state to ENT/CLR
!EIR52980
   WHILE PRINT.OVR.PROMPT = 0
!AIR52980
!      TS.TEMP1$ = LEFT$(OVR.PROMPT$, 20)   !   Prompt with bypass
!      TS.TEMP2$ = RIGHT$(OVR.PROMPT$, 20)  !   print message
!      TS.LINETYPE = 12
!      CALL TSDSEC01

!      UNLOCKDEV 32, 9, PRIORITY            ! Set input state to ENT/CLR
      WAIT 32;10
      IF EVENT% = 32 THEN BEGIN              !
!EIR52980
        CALL TSCSECRK                        ! Read the keyboard.
        LOCKDEV 32                           ! Lock keyboard
        PRINT.OVR.PROMPT = TS.IO.MOTORKEY    ! Save motor key
        TS.TEMP1$ = STRING$(20, " ")         ! blank display
        TS.TEMP2$ = STRING$(20, " ")
        TS.LINETYPE = 12
!AIR52980
        CALL TSDSEC01
        IF PRINT.OVR.PROMPT = 0 THEN BEGIN
           TS.TEMP1$ = LEFT$(OVR.PROMPT$, 20)   !   Prompt with bypass
           TS.TEMP2$ = RIGHT$(OVR.PROMPT$, 20)  !   print message
           TS.LINETYPE = 12
           CALL TSDSEC01
           UNLOCKDEV 32, 9, PRIORITY            ! Set input state to ENT/CLR
        ENDIF
      ENDIF
!EIR52980
   WEND

   FOR I = 1 TO 10
     TS.IO.KEYS(I)  = SAV.KEYS(I)
     TS.IO.DATA$(I) = SAV.DATA$(I)
   NEXT I
   DIM SAV.KEYS(0)
   DIM SAV.DATA$(0)

END SUB
!EIR33229
