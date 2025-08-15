\/* TIME STAMP BLOCK ***********************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/*********************************************************************/
\/*                                                                   */
\/*      MODULE NAME: CPTERM                                          */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: COMMON FUNCTIONS/SUBROUTINES               */
\/*                        FOR TERMINAL CHECK PRINTING                */
\/*                                                                   */
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"        */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED        */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT      */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                           */
\/*                                                                   */
\/*      STATUS: RELEASE 2 LEVEL 0                                    */
\/*                                                                   */
\/*      PERSON RESPONSIBLE = MARTIN MAENZA AND BRIAN SUBJECT         */
\/*                                                                   */
\/*      FUNCTION: COMMON ROUTINES USED BY TERMINAL FOR CHECK         */
\/*                PRINTING.                                          */
\/*                                                                   */
\/*                                                                   */
\/*      INTERNAL ROUTINES:                                           */
\/*                                                                   */
\/*                                                                   */
\/*                                                                   */
\/*      EXTERNAL ROUTINES:                                           */
\/*                                                                   */
\/*                                                                   */
\/*      CHANGE HISTORY:                                              */
\/*                                                                   */
\/*         IR33750 - Issue 4 line feeds prior to check printing.     */
\/*                 - Ensure that check print line is never longer    */
\/*                   than 4 inches (5.33 - 1.33) on 4610 printer.    */
\/*                   11/12/96 - MATRA (adw).                         */
\/*                                                                   */
\/*                 - Ensure that fields are no longer than 4 inches  */
\/*                   on 4610 printer. Legal amount extension field   */
\/*                   should be used in check format to print long    */
\/*                   legal amounts on 4610 because of this 4 inch    */
\/*                   limitation.                                     */
\/*                   11/17/96 - MATRA (adw).                         */
\/*                                                                   */
\/*                 - Made changes to make 4610 check printing match  */
\/*                   model 3 as much as possible.                    */
\/*                   + decreased line-feeds back to 3 at IBM request */
\/*                   + correct CPI values for 4610 (15 is 16, etc.)  */
\/*                   + adjust horizontal spacing of fields on 4610   */
\/*                   + Fixed different CPI fields on same print line */
\/*                   12/02/96 - MATRA (adw).                         */
\/*                                                                   */
\/*                 - Changed length for 4610 printer to allow amounts*/
\/*                   over 99.99.  Also, added code to prevent legal  */
\/*                   line from overwriting pre-printed stuff.        */
\/*                   MWD IBM     12Dec96                             */
\/*                                                                   */
\/*                 - Optimize 4610 check printing code to only do    */
\/*                   carriage return when absolutely necessary.      */
\/*                   Jan.05.97 - MATRA (adw).                        */
\/*                                                                   */
\/*         IR35943 - PE Apar for IR33750.  When writing a check on a */
\/*                   4610 printer, if the legal amount is longer     */
\/*                   than the legal amount field, it is not split    */
\/*                   between the legal amount field and the legal    */
\/*                   amount extention field.                         */
\/*                   RSH MGV-A 07Jul97.                              */
\/*                                                                   */
\/*         IR36036 - AE Apar for IR35943.  Correct calculation of    */
\/*                   maximum number of characters that can be        */
\/*                   printed in the legal amount field when 4610     */
\/*                   printer is used and the legal amount field data */
\/*                   is larger than the legal amount field.          */
\/*                   RSH MGV-A 28Jul97.                              */
\/*                                                                   */
\/*         IR37203 - PE Apar for IR33750.  Too many characters could */
\/*                   be printed on the 4610 causing the print        */
\/*                   buffer to wrap.  This apar reduces the CPI used */
\/*                   for calculating the number of characters for the*/
\/*                   print line when using FONT 2.                   */
\/*                   RSH MGV-A 23Jan98.                              */
\/*                                                                   */
\/*         IR38857 - When using a 4610 printer, if the check jams    */
\/*                   while being flipped, some linefeeds may be lost,*/
\/*                   which results in the lines being printed too    */
\/*                   low on the check.                               */
\/*                   RSH MGA 16Oct98                                 */
\/*                                                                   */
\/*         IR39486 - Added support for Multiple Currency Feature.    */
\/*                   Added global variable MC.ENABLED (used by       */
\/*                   the CPCOMMON.J86 code.)                         */
\/*                   GUS IBM   19Jan99.                              */
\/*                                                                   */
\/*         IR40144 - Allow customer to set user variables to help    */
\/*                   control placement of printing for checks        */
\/*                   larger than 2.5" x 6.0"                         */
\/*                   RSH MGVA 01Apr99                                */
\/*                                                                   */
\/*         IR43250 - PE IR33750. When using a 4610 printer, check    */
\/*                   printing legal amount field overwrites          */
\/*                   convenience amount field erroneously if legal   */
\/*                   amount data is longer than the field length.    */
\/*                   LLU MGVA 19Jun00                                */
\/*                                                                   */
\/*         IR43255 - Add ValuePack 2001 Java GUI Support hooks       */
\/*                   RSH IBM 20Jun00                                 */
\/*                                                                   */
\/*         IR45900 - Fix completion IR43250. When using a 4610       */
\/*                   and check printing, the convenience amount does */
\/*                   does not print when using font 1.               */
\/*                   JAM MGVA 06Jun01                                */
\/*                                                                   */
\/*         IR52980 - Modified java GUI hooks for use with VP,        */
\/*                   SureVision and AEF.                             */
\/*                   KHG IBM 30Jan2004                               */
\/*                                                                   */
\/* END-OF-SPECIFICATIONS *********************************************/
\ REM!!************************ B B D L ******************************!!
\ REM********************* END BBDL STATEMENTS *************************
\ REM!!****************** INCLUDE FILES ******************************!!
\ REM!!                                                               !!
\ REM!!                                                               !!
\ REM!!***************************************************************!!
\ REM******************** BEGIN CODE STATEMENTS ************************

  %ENVIRON T
  %INCLUDE CPVARS.J86           ! Check Printing common variables

   STRING  GLOBAL               \
           FIELD.DATA$(1),      \
           MM.STATUS$           !

!AIR40144
   REAL      GLOBAL                \
           USER.CHEQUE.4610.CK.LEN ! inches - cheque length
!EIR40144

   INTEGER*1 GLOBAL             \
\AIR40144
           USER.CHEQUE.MOD3.LF, \ Additional Linefeeds for Model 3 printer
           USER.CHEQUE.4610.LF, \ Additional Linefeeds for 4610 printer
\EIR40144
           CUTOFF,              \ flag set if field is truncated
           DEF.SPACING,         \ DI default spacing
           I, J

!AIR39486 - Added Feature Flag for Mutlitple Currency Support
   STRING    GLOBAL                \
           CPOP.ARR$(2),           \ Feature Check Print Options Array
           MC.LEGAL.HUND.SEP$,     \ hundredths seperator
           MC.LEGAL.CENT.SEP$,     \ cents seperator
           MC.LEGAL.CENT.DESC$,    \ cents descriptor
           MC.LEGAL.DOL.DESC$,     \ dollars descriptor
           MC.LEGAL.ONE.DOL.DESC$, \ dollar descriptor
           MC.LEGAL.EXACT$,        \ exact amount descriptor
           MC.CONV.SEP$,           \ conv amount seperator
           MC.CP.DATEFORM$         ! Type of format of date field


   INTEGER*2 GLOBAL             \
           PBC.SLOT,            \ Primary Base Currency Slog
           MC.ENABLED,          \ SA Multiple Currency Feature Flag
           SEL.FC.SLOT          ! Selected Foreigh Currency Slot
!EIR39486

  %INCLUDE JAVAGUIV.J86         ! Java Gui Variables ! IR43255

!AIR33229 Added IBM 4610 support
  %INCLUDE EAMP4VAJ.J86         ! 4610 flags

  %INCLUDE EAMP4EXJ.J86         ! external routines
!EIR33229

  %INCLUDE JAVAGUIC.J86         ! Java Gui externals ! IR43255

!AIR43255
FUNCTION EAMTRACE(TRXX) EXTERNAL! EAMTRACE - trace module entry/exit
  INTEGER*1 EAMTRACE            !
  INTEGER*2 TRXX                ! trace code
END FUNCTION                    ! eamtrace
!EIR43255


  %INCLUDE CPCOMMON.J86         ! Check Printing common routines

  %INCLUDE EAMXXCPY.J86




\ REM!!************************  FIELDS.TO.PRINT$  *************************!!
\ REM!!  This subroutine takes a format field attribute and places it into  !!
\ REM!!  an output string for the check.  Two strings are used: one for     !!
\ REM!!  15/7.5 cpi attributes and one for 12 cpi attributes.               !!
\ REM!!*********************************************************************!!

SUB FIELDS.TO.PRINT$(INSERT$) PUBLIC

STRING INSERT$,         \ Input string
       TRASH$,          \ discarded parts of truncated strings
       RLINE$,          \ Right of field
       LLINE$           ! left of field

INTEGER*1 LINENO,       \ Check output line to change
          CHARLENGTH,   \ # of chars in a double wide field
          OFFSET,       \ Column offset (to offset for font codes)
          SETNO         ! array column (1=15/7.5 cpi, 2=12 cpi)

REAL      CPI           ! Characters per inch setting

!AIR33750 - Added new variable to track 4610 check print area in inches
REAL      INCHES
!EIR33750 - End modification
!AIR45900
REAL LWORK              ! Legal length calculation work
!EIR45900

   INSERT$ = REMOVE.BLANKS$(INSERT$)               ! Remove lead and trail blanks.
   IF FF.ROW(CPFORMAT,CPFIELD) <> 0 THEN BEGIN     ! If attribute is in check
      OFFSET = 0                                   !    reset column pointer
      LINENO = FF.ROW(CPFORMAT,CPFIELD)            !    get check line number
      FONT = FF.FONT(CPFORMAT,CPFIELD)
      IF FF.DS(CPFORMAT,CPFIELD) <> -1  THEN BEGIN ! If field is not double strike
         SETNO = 1                                 !    start setno at 1
         OFFSET = 0                                !    start codes offset at 0
      ENDIF ELSE BEGIN
         SETNO = 0                                 ! Start setno at 0
         OFFSET = 2                                ! Start codes offet at 2
      ENDIF

!AIR33229 Added support for IBM 4610 printer
      IF PRT4610.ENABLE THEN BEGIN
         ! Double strike mode not supported
         SETNO = 1
         OFFSET = 0
         ! Double high not supported
         IF FONT = 4 THEN FONT = 3
!AIR33750 - Set 4610 check max print length in inches
         INCHES = 4      ! Max print 4610 print line is about 4 inches
!EIR33750 - End modification

!AIR40144 - allow wider print area
         IF (USER.CHEQUE.4610.CK.LEN > 0) AND          \ Sanity check
            (USER.CHEQUE.4610.CK.LEN <= 10) THEN BEGIN !
           INCHES = USER.CHEQUE.4610.CK.LEN -          \ print field lth LESS
                    2.125                              ! hard margins
           IF (INCHES > 5.33) THEN BEGIN               ! Too big
             INCHES = 5.33                             ! Set max
           ENDIF                                       ! Too big
         ENDIF                                         ! User set cheque len
!EIR40144

      ENDIF
!EIR33229

      IF FONT = 1 THEN BEGIN                       !    If 15 cpi,
         SETNO = SETNO + 1
         CPI   = 15
!AIR33750 - Make adjustments for 4610
         IF PRT4610.ENABLE THEN BEGIN
            CPI = 16       ! Resident font A on 4610 is 16 CPI actually
            OFFSET = 6     ! There are 6 font control characters
         ENDIF
!EIR33750 - End modification
         IF (OFFSET = 2) AND (CHECK.PRINT$(SETNO,LINENO) = "") THEN BEGIN
            CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(69) + STRING$(80," ")
         ENDIF ELSE BEGIN
            IF CHECK.PRINT$(SETNO,LINENO) = "" THEN BEGIN
               CHECK.PRINT$(SETNO,LINENO) = STRING$(80," ")
!AIR33229 Added IBM 4610 support
               IF PRT4610.ENABLE THEN BEGIN
                  ! Redo the print line
!AIR33750 - Calculate number of characters for 4610
                  CHECK.PRINT$(SETNO,LINENO) = \
                     CHR$(1BH) + CHR$(20H) + CHR$(03H) + \! 15 CPI
                     CHR$(1BH) + CHR$(21H) + CHR$(00H) + \! normal
                     STRING$(INCHES * CPI," ")
!EIR33750 - End modification
               ENDIF
!EIR33229
            ENDIF
         ENDIF
      ENDIF
      IF FONT = 2 THEN BEGIN                       !    If 12 cpi,
         SETNO = SETNO + 3
         OFFSET = OFFSET + 2
         CPI   = 12
!AIR33750 - Make adjustments for 4610
         IF PRT4610.ENABLE THEN BEGIN
!AIR37203
!           CPI = 13       ! Resident font B on 4610 is 13 CPI actually
            CPI = 12.75    ! Resident font B on 4610 is < 13 CPI
!EIR37203
            OFFSET = 6     ! There are 6 font control characters
         ENDIF
!EIR33750 - End modification
         IF (OFFSET = 4) AND (CHECK.PRINT$(SETNO,LINENO) = "") THEN BEGIN
            CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(58) + CHR$(27) + CHR$(69) + STRING$(64," ")
         ENDIF ELSE BEGIN
            IF CHECK.PRINT$(SETNO,LINENO) = "" THEN BEGIN
               CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(58) + STRING$(64," ")
!AIR33229 - Added IBM 4610 support
               IF PRT4610.ENABLE THEN BEGIN
                  ! redo the print line
!AIR33750 - Calculate number of characters for 4610 and use resident font B
                  CHECK.PRINT$(SETNO,LINENO) = \
                     CHR$(1BH) + CHR$(20H) + CHR$(03H) + \! 12 CPI
                     CHR$(1BH) + CHR$(21H) + CHR$(01H) + \! normal
                     STRING$(INCHES * CPI, " ")
!EIR33750 - End modification
               ENDIF
!EIR33229
            ENDIF
         ENDIF
      ENDIF
      IF FONT = 3 THEN BEGIN                       !    If 7.5 cpi,
         SETNO = SETNO + 5
         OFFSET = OFFSET + 2
         CPI   = 7.5
!AIR33750 - Adjust CPI for 4610
         IF PRT4610.ENABLE THEN BEGIN
            CPI = 8        ! Resident font A double-wide is 8 CPI
            OFFSET = 6     ! There are 6 font control characters
         ENDIF
!EIR33750 - End modification
         IF (OFFSET = 4) AND (CHECK.PRINT$(SETNO,LINENO) = "") THEN BEGIN
            CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(69) + CHR$(27) + CHR$(14) + STRING$(39," ")
         ENDIF ELSE BEGIN
            IF CHECK.PRINT$(SETNO,LINENO) = "" THEN BEGIN
               CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(14) + STRING$(39," ")
!AIR33229 - Added IBM 4610 support
               IF PRT4610.ENABLE THEN BEGIN
                  ! redo the print line
!AIR33750 - Calculate number of characters for 4610
                  CHECK.PRINT$(SETNO,LINENO) = \
                     CHR$(1BH) + CHR$(20H) + CHR$(03H) + \! 15 CPI
                     CHR$(1BH) + CHR$(21H) + CHR$(20H) + \! double-wide
                     STRING$(INCHES * CPI," ")
!EIR33750 - End modification
               ENDIF
!EIR33229
            ENDIF
         ENDIF
      ENDIF
      IF FONT = 4 THEN BEGIN                       !    If 7.5 DH cpi,
         SETNO = SETNO + 7
         OFFSET = OFFSET + 2
         CPI   = 7.5
         IF (OFFSET = 4) AND (CHECK.PRINT$(SETNO,LINENO) = "") THEN BEGIN
            CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(69) + CHR$(27) + CHR$(23) + STRING$(39," ")
         ENDIF ELSE BEGIN
            IF CHECK.PRINT$(SETNO,LINENO) = "" THEN BEGIN
               CHECK.PRINT$(SETNO,LINENO) = CHR$(27) + CHR$(23) + STRING$(39," ")
            ENDIF
         ENDIF
      ENDIF
      START = FF.START(CPFORMAT,CPFIELD) * CPI     ! Get start column, rounded
!AIR33229 - Kludge for 4610 printer which cannot print in
!           column zero. It has some dead space.
      IF PRT4610.ENABLE THEN BEGIN
!AIR45900 This code resolves the problem of where moving the conv. field left
!         one inch causes the legal field to overlay it. If so, shorten the
!         legal field. This may cause the legal field to print on 2 lines
        IF (CPFIELD = 4) AND (INCHES <= 4) THEN BEGIN ! If legal and space limited
          LWORK = FF.LENGTH(CPFORMAT,3)            ! Length of conv. field
          LWORK = LWORK + FF.LENGTH(CPFORMAT,CPFIELD) ! Add length of legal
          IF LWORK > INCHES THEN BEGIN             ! If fields total > space
            LWORK = LWORK - INCHES                 ! Compute excess and adjust
            FF.LENGTH(CPFORMAT,CPFIELD) = (FF.LENGTH(CPFORMAT,CPFIELD) - LWORK)
          ENDIF
        ENDIF
!EIR45900
         ! Adjust START to account for unusable print space on 4610
!AIR40144
!!AIR33750 - Adjust START to allow amounts over 99.99
!!        START = START - (0.80 * CPI)
!         START = START - CPI
!!EIR33750
         IF (INCHES <= 4) THEN BEGIN               ! If limited print area
           START = START - CPI                     ! Adjust start column
         ENDIF                                     ! Limited print area
!EIR40144
         ! Ensure that START is non-negative after adjustment
         IF START < 0 THEN START = 0
      ENDIF
!EIR33229
      STARTCOL = INT%(ROUND(START,0,1))            !   to the next whole column
      IF STARTCOL = 0 THEN STARTCOL = 1
      LENGTH = FF.LENGTH(CPFORMAT,CPFIELD) * CPI   ! Get length in columns,
!AIR33750 - Ensure that field is no longer than max inches for 4610 printer so
!!!!!!!!! - that long legal amount field overflows into extension field right.
      IF PRT4610.ENABLE THEN BEGIN
!AIR45900 Removes IR43250
!        IF (START + LENGTH) > (INCHES * CPI) THEN       ! IR43250
!       IF (START + LENGTH) > (INCHES * CPI) THEN BEGIN  ! IR43250
         IF (START + LENGTH) > (INCHES * CPI) THEN       \ IR43250
\EIR45900
            LENGTH = (INCHES * CPI) - START
!         - prevent legal line from overrwriting pre-printed stuff if possible
         IF CPFIELD = 4 THEN BEGIN                 ! If legal string
           IF LEN(INSERT$) < (LENGTH - (CPI * 0.70)) THEN BEGIN ! padding needed
             LENGTH = LENGTH - (CPI * 0.70)        ! padding should not overlay
                                                   !   pre-printed part
           ENDIF ELSE BEGIN
!AIR35943 Correct length calculation for legal amount field
             !LENGTH = LEN(INSERT$) + 1            ! no padding necessary
!AIR36036
!            IF ((LEN(INSERT$)+1)/CPI) > INCHES \ ! if legal amt data is too
!            THEN BEGIN                           ! long for the field
!              LENGTH = INCHES * CPI ! set length to what the field can hold -
!                                    ! the excess will end up in the extention
!AIR45900 Correcting a case of overlap of the legal amount into the convenience
!         amount field. Please note there is a design issue which may cause the
!         legal amount field to overlap the convenience field. In the code above
!         START(starting column) is reduced by the amount of CPI. This an attempt
!         to move the starting position 1 inch to the left for each field. Note
!         that some fields like the legal amount may have a starting position of
!         less than 1 inch from the left edge of the check. This results in the
!         legal field moving typically .3 of an inch and the following field
!         like the convenience amount moving a full inch. This may result in
!         the overlap of the fields.
!            IF (LEN(INSERT$) + 1) > ((INCHES * CPI) - START) THEN BEGIN
             IF (LEN(INSERT$) + 1) > ((FF.LENGTH(CPFORMAT,CPFIELD) * CPI) - START) THEN BEGIN
                LENGTH = (FF.LENGTH(CPFORMAT,CPFIELD) * CPI)
!EIR45900
             ! If appending a character to insert$ makes insert$ larger
             ! than the maximum length that the field can hold, then the
             ! length is already properly set up as ((INCHES * CPI) - START).
             ! This is the maximum number of chars that the field can hold.
             ! Since the length is already set up, we do nothing here.
!EIR36036
             ENDIF ELSE BEGIN        ! otherwise,
               LENGTH = LEN(INSERT$) + 1       ! NO PADDING NECESSARY
             ENDIF                             !
!EIR35943
           ENDIF
         ENDIF
!AIR45900 Removes IR43250
!       ENDIF                                  ! IR43250
!EIR45900
      ENDIF
!EIR33750 - End modification
      FIELDLENGTH = INT%(ROUND(LENGTH,0,1))        !   rounded to next whole column
      ENDCOL = FIELDLENGTH + STARTCOL - 1          ! Calculate ending column

!     IF FONT = 1 THEN OR                          \ If 15 CPI or
!        FONT > 2 THEN BEGIN                       !    a 7.5 CPI font,
!     ENDIF                                        !    into the check field.
!     IF FONT = 2 THEN BEGIN                       ! If 12 CPI,
!        DATALENGTH = INT%(ROUND(LENGTH* 0.8,0,1)) !    determine data that will fit
!     ENDIF                                        !    into the check field.
      DATALENGTH = INT%(ROUND(LENGTH,0,1))         !    determine data that will fit

      CALL CASE.CONVERT(INSERT$)                   ! Make the right case
      IF CPFIELD = 4 THEN BEGIN                    ! If Legal string,
         CALL PAD.STRING(INSERT$, FIELD.DATA$(33)) !   pass it legal extension
      ENDIF ELSE BEGIN
         TRASH$ = ""                               !   initial to null
         CALL PAD.STRING(INSERT$, TRASH$)          !   add pad characters
         IF TRASH$ <> "" THEN CUTOFF = -1          !   if not null, set cutoff
      ENDIF

      STARTCOL = STARTCOL + OFFSET
      ENDCOL   = ENDCOL + OFFSET
      IF (STARTCOL - 1) < 0 THEN BEGIN
         LLINE$ = ""
      ENDIF ELSE BEGIN
         LLINE$ = LEFT$(CHECK.PRINT$(SETNO,LINENO),STARTCOL-1)
      ENDIF
      IF (LEN(CHECK.PRINT$(SETNO,LINENO)) - ENDCOL) < 0 THEN BEGIN
         RLINE$ = ""
      ENDIF ELSE BEGIN
         RLINE$ = RIGHT$(CHECK.PRINT$(SETNO,LINENO), \
                  LEN(CHECK.PRINT$(SETNO, LINENO)) - ENDCOL)
      ENDIF
      CHECK.PRINT$(SETNO,LINENO) = LLINE$ + INSERT$ + RLINE$
!AIR33750 - Check print line is truncated to max inches on 4610 to avoid wrap
      IF PRT4610.ENABLE THEN BEGIN
         CHECK.PRINT$(SETNO,LINENO) = \
            LEFT$(CHECK.PRINT$(SETNO,LINENO), (INCHES * CPI) + OFFSET)
      ENDIF
!EIR33750 - End modification
   ENDIF
END SUB

\ REM!!************************  NEED.CARD.NO ******************************!!
\ REM!!  This function returns whether or not a card number is needed for   !!
\ REM!!  the check format to be printed.                                    !!
\ REM!!*********************************************************************!!

FUNCTION NEED.CARD.NO PUBLIC

INTEGER*1 NEED.CARD.NO

   IF FF.ROW(CPFORMAT,7) <> 0 THEN BEGIN
      NEED.CARD.NO = -1
   ENDIF ELSE NEED.CARD.NO = 0

END FUNCTION


\ REM!!************************  CHECK.PRESENT  ****************************!!
\ REM!!  This function will attempt to sense a check entering the DI        !!
\ REM!!  station (either from the top or from the front)    and will make   !!
\ REM!!  note of the direction of entry.  A "0" is returned if no check     !!
\ REM!!  is present, a "-1" if from the top, and a "-2" if from the front.  !!
\ REM!!*********************************************************************!!

FUNCTION CHECK.PRESENT (DI.SESSNUM) PUBLIC

INTEGER*1 CHECK.PRESENT,                   \
          IN.TOP,                          \
          IN.FRONT                         !

INTEGER*2 DI.SESSNUM                       ! Session number of DI station
INTEGER*4 WORKI4                           ! Working storage from GETLONG

!AIR43255
!   if (javaGuiActive) then begin           ! java gui active IR52980
   CALL EAMTRACE(676AH)                    ! trace 'jg'
   CALL javaEvent(updateStatusMsg)         ! update status bar
   CALL EAMTRACE(7063H)                    ! trace return 'cp'
!   endif                                   ! java gui active IR52980
!EIR43255

!AIR33229 - Added IBM 4610 support
   IF PRT4610.ENABLE THEN BEGIN
      WORKI4 = PRT4610.DI.GETLONG(DI.SESSNUM)
   ENDIF ELSE BEGIN
      WORKI4 = GETLONG(DI.SESSNUM)            ! Get DI bytes
   ENDIF
!EIR33229

   IN.TOP = (WORKI4 AND 001H) = 01H        ! Check SS bit to see if in TOP
   IN.FRONT = (WORKI4 AND 010H) = 010H     ! Check SS bit to see if in FRONT
   IF IN.TOP THEN BEGIN
      CHECK.PRESENT = -1
   ENDIF ELSE IF IN.FRONT THEN BEGIN
      CHECK.PRESENT = -2
   ENDIF ELSE CHECK.PRESENT = 0

END FUNCTION


\ REM!!**************** MOVE.TO.CHECK.START   ******************************!!
\ REM!!  This subroutine will set the check printing spacing to 6 LPI.      !!
\ REM!!  The prior DI setting will be stored in DEF.SPACING to be reset     !!
\ REM!!  in PRINT.CHECK.  Then this subroutine will move the check to the   !!
\ REM!!  starting position ( 2 1/2 inches from the bottom right hand corner)!!
\ REM!!  - which conforms to the US, APACs and Eurocheck formats.           !!
\ REM!!                                                                     !!
\ REM!!  Note: This routine assumes Auto Insertion at the DI has been set   !!
\ REM!!        (prior to call, make sure the MM Bit 4 has been set to 1     !!
\ REM!!         using the PUTLONG statement, which sets Auto Insertion)!    !!
\ REM!!*********************************************************************!!

SUB MOVE.TO.CHECK.START (DI.SESSNUM,IN.DIRECTION) PUBLIC

INTEGER*2 DI.SESSNUM                       ! Session number of DI station

INTEGER*1 I, J,                            \
          IN.DIRECTION                     ! Note direction check came in
                                           !   so we can eject out again.
INTEGER*4 WORKI4                           ! Working storage from GETLONG.

!AIR40144
STRING                                     \
          FORM$                            ! Form for write form / User LF's
!EIR40144

!AIR33229 - Added IBM 4610 support
 IF PRT4610.ENABLE THEN BEGIN

    ! Place 4610 DI in landscape mode
    CALL PRT4610.DI.LANDSCAPE(DI.SESSNUM, 1)

    ! Now get current LPI setting for DI landscape
    WORKI4 = PRT4610.DI.GETLONG(DI.SESSNUM)
    DEF.SPACING = (WORKI4 AND 0400000H) = 0400000H

    ! Set DI landscape to 6 LPI
    CALL PRT4610.DI.LPI(DI.SESSNUM, 6)            ! set to 6 LPI

    ! Advance to start

!AIR38857 - This change will cause print data to be sent to
!           the DI station instead of just linefeeds, which will
!           not move the print head from the home position.

!!AIR33750 - Issue 3 line feeds
!   CALL PRT4610.DI.FEED(DI.SESSNUM, 3)
!!EIR33750 - End modification
    WRITE # DI.SESSNUM; STRING$(3," " + CHR$(0AH))  ! Space + LF 3x's
!EIR38857

!AIR40144 - Allow user to select additional LF's for Cheque
    IF (USER.CHEQUE.4610.LF > 0) AND                \ Reasonability
       (USER.CHEQUE.4610.LF < 10) THEN BEGIN        !  check
      WRITE # DI.SESSNUM;                           \ Perform additional
              STRING$(USER.CHEQUE.4610.LF,          \  Linefeeds
                      " " + CHR$(0AH))              !
    ENDIF                                           ! User specified Add Lf
!EIR40144

 ENDIF ELSE BEGIN                                   ! not 4610 printer
!EIR33229

   WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
   DEF.SPACING = (WORKI4 AND 0800000H) = 0800000H   ! Check LL bit for DI spacing
   WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
   WORKI4 = WORKI4 AND 00000FFFFH                   ! Map off reserved bits
   IF  DEF.SPACING  THEN BEGIN                      ! If 8 LPI default
      WORKI4 = WORKI4 AND 0007FFFH                  !    set to 6 LPI
      PUTLONG DI.SESSNUM, WORKI4
   ENDIF

   IF IN.DIRECTION = -1 THEN BEGIN                  ! If in from top,
      WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
      WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
      WORKI4 = WORKI4 AND 00000FFFFH                   ! Map off reserved bits
      WORKI4 = WORKI4 OR  000000002H                   ! Set direction to top-to-bot
      PUTLONG DI.SESSNUM, WORKI4
      WRITE FORM "C0,A7"; # DI.SESSNUM; ""             ! Advance to start
                                                       !   auto insert top spot.

!AIR40144 - Allow user to select additional LF's for Cheque
      IF (USER.CHEQUE.MOD3.LF > 0) AND                 \ Reasonability
         (USER.CHEQUE.MOD3.LF < 10) THEN BEGIN         !  check
        FORM$ = "C0 A" + STR$(USER.CHEQUE.MOD3.LF)     ! Set form
        WRITE FORM FORM$; # DI.SESSNUM; ""             ! Advance to start
        FORM$ = ""                                     ! Conserve heap
      ENDIF                                            ! User specified Add Lf
!EIR40144

      WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
      WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
      WORKI4 = WORKI4 AND 00000FFFDH                   ! Map off reserved bits,
                                                       !   return to line feeds,
                                                       !   and change to bottom to top
      PUTLONG DI.SESSNUM, WORKI4
   ENDIF ELSE BEGIN                                 ! Else if in from front,
      WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
      WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
      WORKI4 = WORKI4 AND 00000FFFFH                   ! Map off reserved bits
      WORKI4 = WORKI4 OR  000000002H                   ! Set direction to top-to-bot
      PUTLONG DI.SESSNUM, WORKI4
      WRITE FORM "C0,A7"; # DI.SESSNUM; ""             ! Advance to start

!AIR40144 - Allow user to select additional LF's for Cheque
      IF (USER.CHEQUE.MOD3.LF > 0) AND                 \ Reasonability
         (USER.CHEQUE.MOD3.LF < 10) THEN BEGIN         !  check
        FORM$ = "C0 A" + STR$(USER.CHEQUE.MOD3.LF)     ! Set form
        WRITE FORM FORM$; # DI.SESSNUM; ""             ! Advance to start
        FORM$ = ""                                     ! Conserve heap
      ENDIF                                            ! User specified Add Lf
!EIR40144

   ENDIF


! *** Check is at the starting position (2 1/2 inches from the bottom right hand corner).
!     The DI printing direction is Top to Bottom at this point.

   WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
   WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
   WORKI4 = WORKI4 AND 00000FFEDH                   ! Map off reserved bits,
                                                    !   return to line feeds,
                                                    !   remove auto insert,
                                                    !   and change to bottom to top
   PUTLONG DI.SESSNUM, WORKI4

!AIR33229 - Added IBM 4610 support
 ENDIF
!EIR33229

END SUB


\ REM!!************************  PRINT.CHECK  ******************************!!
\ REM!!  This subroutine does the actual printing of the check at the       !!
\ REM!!  terminal.  It is passed the session number of the Document Insert  !!
\ REM!!  used by the terminal application.  The check will already be in    !!
\ REM!!  the DI station when this routine is called.  The subroutine resets !!
\ REM!!  the DI spacing value store in DEF.SPACING to its former value and  !!
\ REM!!  ejects the printed check out in the same direction it was entered. !!
\ REM!!                                (ex. in from bottom, out to bottom). !!
\ REM!!*********************************************************************!!

SUB PRINT.CHECK (DI.SESSNUM,IN.DIRECTION) PUBLIC

INTEGER*2 DI.SESSNUM                       ! Session number of DI station

INTEGER*1 I, J,                            \
          FONT.LEN,                        \ Length of fonts for a line
          IN.DIRECTION                     ! Note direction check came in
                                           !   so we can eject out again.
INTEGER*4 WORKI4                           ! Working storage from GETLONG.
STRING    FORMDI$                          ! FORM for WRITE statement.

!AIR33750 - Added new var to count strings printed on same check line
INTEGER*1 SCOUNT
!EIR33750 - End modification

!AIR33229 - Added IBM 4610 support
 IF PRT4610.ENABLE THEN BEGIN

   ! Added font save and restore
   CALL PRT4610.SAVE.FONT.AND.SPACING

!AIR40144
   IF (USER.CHEQUE.4610.CK.LEN > 0) AND         \ Valid length
      (USER.CHEQUE.4610.CK.LEN <= 10) THEN BEGIN!
     WRITE # DI.SESSNUM; CHR$(1BH) +            \ Set to align Right
                         CHR$(61H) +            \
                         CHR$(02H)              !
   ENDIF                                        ! Valid length
!EIR40144

   FONT.LEN = 6                                 ! Font length is 6

   FOR I = 8 TO 1 STEP -1
!AIR33750 - Optimize check printing code
!         - Correct problem with different CPI fields on same line
!     ! set line-feed to be carriage return only
!     WRITE # DI.SESSNUM; CHR$(1BH) + CHR$(33H) + CHR$(00H)
      SCOUNT = 0
!     FOR J = 1 TO 8
      FOR J = 2 TO 6 STEP 2
!EIR33750 - End modification
         IF LEN(REMOVE.BLANKS$(CHECK.PRINT$(J,I))) > FONT.LEN THEN BEGIN
!AIR33750 - Optimize check printing code
            IF SCOUNT > 0 THEN BEGIN
               IF SCOUNT = 1 THEN BEGIN
                  WRITE # DI.SESSNUM; CHR$(1BH) + CHR$(33H) + CHR$(00H)
               ENDIF
               WRITE # DI.SESSNUM; CHR$(0AH) + CHECK.PRINT$(J,I)
            ENDIF ELSE BEGIN
               WRITE # DI.SESSNUM; CHECK.PRINT$(J,I)
            ENDIF
            SCOUNT = SCOUNT + 1
!EIR33750 - End modification
         ENDIF
      NEXT J
!AIR33750 - Optimize check printing code
      IF SCOUNT > 1 THEN BEGIN
!         - Now restore line-feed and advance to next print line
         WRITE # DI.SESSNUM; CHR$(1BH) + CHR$(32H)       ! restore line-feed
         WRITE # DI.SESSNUM; " " + CHR$(0AH)             ! Write advance.
      ENDIF ELSE BEGIN
         WRITE # DI.SESSNUM; CHR$(0AH)
      ENDIF
!EIR33750 - End modification
   NEXT I

!AIR40144
   IF (USER.CHEQUE.4610.CK.LEN > 0) AND         \ Valid length
      (USER.CHEQUE.4610.CK.LEN <= 10) THEN BEGIN!
     WRITE # DI.SESSNUM; CHR$(1BH) +            \ Set alignment
                         CHR$(61H) +            \  back to left
                         CHR$(00H)              !
   ENDIF                                        ! Valid length
!EIR40144

   ! Added font save and restore
   CALL PRT4610.RESTORE.FONT.AND.SPACING(35)

   ! Restore 8 LPI to DI landscape if need be
   IF DEF.SPACING THEN \
      CALL PRT4610.DI.LPI(DI.SESSNUM, 8)

   ! Restore DI to portrait mode
   CALL PRT4610.DI.LANDSCAPE(DI.SESSNUM, 0)

   ! Feed DI by 2
   CALL PRT4610.DI.FEED(DI.SESSNUM, 2)

   ! Finally eject the check
   CALL PRT4610.DI.EJECT(DI.SESSNUM)

 ENDIF ELSE BEGIN
!EIR33229

   FOR I = 1 TO 8
      FOR J = 1 TO 8
         IF J = 1 THEN FONT.LEN = 0 ELSE FONT.LEN = 2   ! 15 CPI lines have no font
         IF MOD(J,2) <> 0 THEN FONT.LEN = FONT.LEN + 2  ! DS lines are odd ones

         IF LEN(REMOVE.BLANKS$(CHECK.PRINT$(J,I))) > FONT.LEN THEN BEGIN
            IF (J = 1) OR (J = 2) THEN BEGIN          ! 15 CPI,
               FORMDI$ = "C" + STR$(LEN(CHECK.PRINT$( \
                  J,I))+4) + "A0"                     ! Write out line
               WRITE FORM FORMDI$; # DI.SESSNUM;      \
                     "    " + CHECK.PRINT$(J,I)       !   (no advance).
            ENDIF ELSE IF (J = 3) OR (J = 4) THEN BEGIN !   12 CPI,
               FORMDI$ = "C" + STR$(LEN(CHECK.PRINT$( \
                  J,I))+3) + "A0"                     ! Write out line
               WRITE FORM FORMDI$; # DI.SESSNUM;      \
                     "   " + CHECK.PRINT$(J,I)        !   (no advance).
            ENDIF ELSE BEGIN                          ! 7.5 CPI
               FORMDI$ = "C" + STR$(LEN(CHECK.PRINT$( \
                  J,I))+2) + "A0"                     ! Write out line
               WRITE FORM FORMDI$; # DI.SESSNUM;      \
                     "  " + CHECK.PRINT$(J,I)         !   (no advance).
            ENDIF
         ENDIF
      NEXT J
      WRITE FORM "C0A1"; # DI.SESSNUM; ""                 !  Write advance.
   NEXT I

!  WAIT ; 5000
   WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
   WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
   WORKI4 = WORKI4 AND 00000FFFFH                   ! Map off reserved bits
   IF  DEF.SPACING THEN BEGIN                       ! If 8 LPI default
      WORKI4 = WORKI4 OR 0008000H                   !    reset it to 8 LPI.
   ENDIF
   IF IN.DIRECTION = -1 THEN BEGIN                  ! If in from top,
      WORKI4 = WORKI4 AND  000FFFDH                 !    go back out top.
   ENDIF ELSE BEGIN                                 ! Else,
      WORKI4 = WORKI4 OR  0000002H                  !    go back out front.
   ENDIF
   PUTLONG DI.SESSNUM, WORKI4
   WRITE FORM "C0A1"; # DI.SESSNUM; ""                   !  Write advance.
   WRITE FORM "C0A1"; # DI.SESSNUM; ""                   !  Write advance.
   WRITE FORM "C2A0"; # DI.SESSNUM; CHR$(27) + CHR$(12)  !  Eject check

   WORKI4 = GETLONG(DI.SESSNUM)                     ! Get DI bytes
   WORKI4 = SHIFT(WORKI4,8)                         ! Shift bits
   WORKI4 = WORKI4 AND 00000FFFFH                   ! Map off reserved bits
   WORKI4 = WORKI4 OR 00000010H                     ! return auto insert
   PUTLONG DI.SESSNUM, WORKI4

!AIR33229 - Added IBM 4610 support
 ENDIF
!EIR33229

END SUB
