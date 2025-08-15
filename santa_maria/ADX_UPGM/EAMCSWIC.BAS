\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMCSWIC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: PROCESS WIC EBT STRINGS               */
\/*                                                              */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 2004 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*                                                              */
\/*      PERSON RESPONSIBLE = GEIGER, KENNETH H                  */
\/*                                                              */
\/*      FUNCTION: EAMCSWIC                                      */
\/*                                                              */
\/*                THIS MODULE IS INVOKED BY CHECKOUT SUPPORT    */
\/*                MAINLINE TO PROCESS WIC EBT ITEM AND          */
\/*                WIC EBT SUMMARY STRINGS                       */
\/*                                                              */
\/*                                                              */
\/*         DEPENDENCIES:                                        */
\/*                                                              */
\/*                COUNT OF SPECIAL SIGN OFFS AND CASH DRAWER    */
\/*                OPENINGS ARE DONE IN MAINLINE                 */
\/*                                                              */
\/*         RESTRICTIONS:                                        */
\/*                                                              */
\/*                NONE                                          */
\/*                                                              */
\/*         REGISTER CONVENTIONS:                                */
\/*                                                              */
\/*            RESTRICTED REGISTERS = N/A                        */
\/*                                                              */
\/*            REGISTERS USED = N/A                              */
\/*                                                              */
\/*      MODULE TYPE: CBASIC                                     */
\/*                                                              */
\/*         PROCESSOR: CBASIC COMPILER                           */
\/*         MODULE SIZE: 500 LOC                                 */
\/*                                                              */
\/*         ATTRIBUTES: REENTRANT                                */
\/*                                                              */
\/*      ENTRY POINT: CSSAEC01                                   */
\/*                                                              */
\/*         PURPOSE: ENTRY POINT USED BY EAMCSFUC WHEN AN ITEM   */
\/*                  ENTRY STRING IS TO BE PROCESSED.            */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM EAMCSFUC                        */
\/*                                                              */
\/*      ENTRY POINT: CSSAEC02                                   */
\/*                                                              */
\/*         PURPOSE: ENTRY POINT USED BY EAMCSFUC WHEN A         */
\/*                  DISCOUNT STRING IS TO BE PROCESSED.         */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE: CALLED FROM EAMCSFUC                        */
\/*                                                              */
\/*      INPUT:                                                  */
\/*                                                              */
\/*         SYMBOLIC NAME: EAMSOPTS                              */
\/*                                                              */
\/*         DESCRIPTION: STORE OPTIONS                           */
\/*                                                              */
\/*                THIS FILE CONTAINS STORE WIDE OPTIONS DATA    */
\/*                                                              */
\/*                ***** FILE INPUT *****                        */
\/*                                                              */
\/*      OUTPUT:                                                 */
\/*                                                              */
\/*      EXIT-NORMAL: RETURN TO CALLER                           */
\/*                                                              */
\/*         RETURN CODE:                                         */
\/*                                                              */
\/*      EXIT-ERROR: CS11EW01                                    */
\/*                                                              */
\/*         RETURN CODE:                                         */
\/*                                                              */
\/*            REASON CODE:                                      */
\/*            MESSAGE ID:                                       */
\/*                                                              */
\/*         ABEND CODES:                                         */
\/*                                                              */
\/*         ERROR MESSAGES:                                      */
\/*                                                              */
\/*      EXTERNAL REFERENCES:                                    */
\/*                                                              */
\/*                                                              */
\/*         ROUTINES:                                            */
\/*                                                              */
\/*           NAME: CSMLEW01  ! MAINLINE ERROR HANDLING          */
\/*           NAME: CSMLEC02  ! MODULE TRACING                   */
\/*           NAME: CSUPEC03  ! USER EXIT - BEFORE SALES ITEM    */
\/*           NAME: CSVFEC02  ! STRING SPLITTING ROUTINE         */
\/*           NAME: CSIMEC01  ! ITEM MOVEMENT PROCESSING         */
\/*           NAME: CSDPEC01  ! DEPARTMENT TOTALS PROCESSING     */
\/*           NAME: CSWREC52  ! WRITE TO EXCEPTION LOG           */
\/*           NAME: CSRDEC09  ! READ MISC TRANS RECORD           */
\/*           NAME: CSWREC09  ! WRITE MISC TRANS RECORD          */
\/*                                                              */
\/*         DATA AREAS:                                          */
\/*                                                              */
\/*           MODIFIED:                                          */
\/*                                                              */
\/*           REFERENCED:                                        */
\/*                                                              */
\/*             EAMSOPTS: STORE OPTIONS                          */
\/*                                                              */
\/*         CONTROL BLOCKS: NONE                                 */
\/*                                                              */
\/*         MACROS: NONE                                         */
\/*                                                              */
\/*         TABLES: NONE                                         */
\/*                                                              */
\/*      CHANGE ACTIVITY: LEVEL 0                                */
\/*                                                              */
\/*                                                              */
\/* IO01214 - There can be one or two E3 addenda records for     */
\/*           each item, depending upon the PurchaseQuantity and */
\/*           ZeroPurchaseQuantity fields.  Previously, these    */
\/*           two fields were combined into one E3 record.       */
\/*           GGK IBM 12May2005                                  */
\/*                                                              */
\/* IO02996 - Daylight savings time is amended beginning in      */
\/*           2007.  Starting date goes from the first Sunday in */
\/*           April to the second Sunday in March.  Ending date  */
\/*           goes from the last Sunday in October to the first  */
\/*           Sunday in November.                                */
\/*           GGK IBM 14Nov2005                                  */
\/*                                                              */
\/* IO03864 - Two WIC EBT fixes:                                 */
\/*           1. We will adjust the local transaction Time so    */
\/*              that it is 23:59 on the previous Month.         */
\/*              This is on the D4 record only.                  */
\/*              JTG/GGK 17Feb2006                               */
\/*           2. Change the date for ClaimDate in Claim File     */
\/*              record A1 (ClaimDate=offset108) and record Z1   */
\/*              (ClaimDate=offset43) to be local time instead   */
\/*              of GMT time.  This new matching local time      */
\/*              should be the same exact time as the calculated */
\/*              GMT time minus the GMT algorithm calculation    */
\/*              (i.e. don't want it to be a second off which    */
\/*              could happen if extracting two different        */
\/*              date/times).                                    */
\/*              LTG/GGK IBM 01Mar2006                           */
\/*                                                              */
\/* IO04597 - If the last WIC entry in the Tlog is an exception  */
\/*           log that includes an ICC code that requires its    */
\/*           own "D4" record, and a restart of Checkout support */
\/*           is done, then the sequence number is out of order  */
\/*           on the restart.                                    */
\/*           LTG/GGK IBM 16Jun2006                              */
\/*                                                              */
\/* IO05348 - AE IO03864.  The LocalXactionDateTime field of the */
\/*           D4 WIC Claim File Detail Record is wrong.          */
\/*           IO03864 fixed a problem with transactions that     */
\/*           span the time period involving the exact           */
\/*           expiration date and time, but caused a new problem.*/
\/*           If a card has an expiration date that is not the   */
\/*           last day of a calendar month, every transaction    */
\/*           using that card that occurred in the prior         */
\/*           calendar month would have its transaction date     */
\/*           changed to the expiration date of the card.        */
\/*           This design change uses a new field at the end     */
\/*           of the 80:02 record, which is the transaction date */
\/*           and time when benefits were read from the card.    */
\/*           GGK IBM 11Oct2006                                  */
\/*                                                              */
\/* IO07522 - Variable XL.CODE$ should contain Packed Decimal    */
\/*           data when user exit CSUPEC11 is called.  Failure   */
\/*           to do that could cause the Exception Log report    */
\/*           to abend.  Also setup certain strings so that they */
\/*           are available for that user exit.                  */
\/*           GGK IBM 05Nov2007                                  */
\/*                                                              */
\/* IO10055 - Missing RESUME in ON ERROR routine NO.WISTATUS.    */
\/*           GGK IBM 19Jan2009                                  */
\/*                                                              */
\/* END-OF-SPECIFICATIONS ****************************************/


%INCLUDE EAMCSCF1.J86
%INCLUDE EAMCSGLO.J86
%INCLUDE EAMTRANS.J86
%INCLUDE EAMCSXPT.J86
%INCLUDE EAMSOPTS.J86
%INCLUDE EAMWICEF.J86

!AIO07522 Make this variable GLOBAL
  INTEGER*2 GLOBAL                               \
    WIC.ACTIVE                                   \ wic active flag
!EIO07522

  INTEGER*2                                      \
    I,                                           \ work index
    J,                                           \
\AIO07522 Make this variable GLOBAL
\   WIC.ACTIVE,                                  \ wic active flag
\EIO07522
    CURRENT.JULIAN,                              \ YDDD for today
    ITEM.CNT                                     ! trans WIC item count

  INTEGER*2 GLOBAL                               \
    DECADE.BREAK                                 ! opens a 10-year window of use

  INTEGER*4                                      \
    WE.RESTART,                                  \ CS.RESTART comparison variable
    PURCH.COST                                   ! trans WIC amount


  STRING                                         \
\AIO03864
    A1.Z1.CLAIM.DATE$,                           \ save local ClaimDate
    WIC.END.DATE$,                               \ For Month End rollover
\EIO03864
    WIC.AGENCY$,                                 \ WIC agency
    WIC.STORE.LINE1$,                            \ store name/address line 1
    WIC.STORE.LINE2$,                            \ store name/address line 2
    WORK.ARRAY$(1),                              \ string work array
    WIC.ERR.MSG$                                 ! WIC error msg

  STRING GLOBAL                                  \
    TE.ST.DATETIME$,                             \ transaction date/time
    TE.ST.SLOGNAME$                              ! current TLOG name


!*********************************************************************
! PACKBIN2/4
!
! These routines take a packed decimal string and convert to integer
! (The equivalent of CBASIC INT%(VAL(UNPACK$())) )
! P1 is the source string, P2 the string offset and P3 the length.
!
! NOTE: THE OFFSET PASSED TO THIS ROUTINE IS ZERO INDEXED
!
!*********************************************************************
FUNCTION PACKBIN2(P1$,P2,P3) EXTERNAL
  INTEGER*2 PACKBIN2
  STRING P1$
  INTEGER*2 P2,P3
END FUNCTION

FUNCTION PACKBIN4(P1$,P2,P3) EXTERNAL
  INTEGER*4 PACKBIN4
  STRING P1$
  INTEGER*2 P2,P3
END FUNCTION


!************************************************************
! ADXSERVE
!
! This function does application services.
!************************************************************
SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL
  INTEGER*4 RET
  INTEGER*2 FUNC,PARM1
  STRING    PARM2$
END SUB

!************************************************************
! ADXCOPYF
!
! This function provides file copying services.
!************************************************************

SUB ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1,OPT2) EXTERNAL
  INTEGER*1 ADXCOPYF
  INTEGER*4 RETC
  INTEGER*2 OPT0,OPT1,OPT2
  STRING INFILE, OUTFILE
END SUB


!************************************************************
! ADXERROR
!
! This external function logs errors to OCF.
!************************************************************
FUNCTION ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
  INTEGER*2 TERM,NUM
  INTEGER*1 SVRTY,GRP,EVENT
  STRING    UNQ$
END FUNCTION


!************************************************************
! RENAME.WAIT
!
! This subprogram is called whenever a rename function fails
! during a close.  It logs a message, updates the background
! message and waits 20 seconds before returning.
!************************************************************
SUB RENAME.WAIT(NEW.NAME$, OLD.NAME$) EXTERNAL
  STRING NEW.NAME$, OLD.NAME$
END SUB


!************************************************************
! CSMLEC03
!
! This function returns the IDs of the three most recently
! called modules.  It is called whenever an error is to be
! logged with ADXERROR.
!************************************************************
FUNCTION CSMLEC03  EXTERNAL
  STRING CSMLEC03
END FUNCTION


!************************************************************
! CSMLEC02
!
! This is the module tracing routine.  It takes as input a two
! byte module ID.
!************************************************************
FUNCTION CSMLEC02( P1 ) EXTERNAL
  INTEGER*2 P1
END FUNCTION

!*********************************************************************
!
! PUTN1/PUTN2/PUTN4
!
! These routines insert a one/two/four byte integer into a string.
! P2 is the offset within the string and P3 is the source integer
!
! NOTE: THE OFFSET PASSED TO THIS ROUTINE IS ZERO INDEXED
!
!*********************************************************************
FUNCTION PUTN2(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2,P3
END FUNCTION


!************************************************************
! CSUPEC11
!
! User exit called before writing exception log record.
!************************************************************
FUNCTION CSUPEC11 EXTERNAL
END FUNCTION


!***************************************************************
! ?GREG.JULIAN ( GREG.DATE$ )                                  *
!***************************************************************
!                                                              *
! Description: Converts the given gregorian (calendar) date    *
!                into a julian date (base 01/01/1900)          *
!                                                              *
! Inputs:                                                      *
!   GREG.DATE$    - date to be converted (MM/DD/YYYY)          *
!                                                              *
! Outputs:                                                     *
!   ?GREG.JULIAN  - converted julian date, zero if an invalid  *
!                   date was supplied (function return).  The  *
!                   range of return values is 1..366 for leap  *
!                   years and 1..365 for other years.          *
!                                                              *
!***************************************************************
FUNCTION ?GREG.JULIAN ( GREG.DATE$ )                !
  INTEGER*4 ?GREG.JULIAN                            ! Function return
  STRING    GREG.DATE$                              ! Date to be converted

  INTEGER*2 GREG.MONTH,                             \ Gregorian month
            GREG.DAY,                               \ Gregorian day
            GREG.YEAR                               ! Gregorian year

  INTEGER*4 JULIAN.WORK                             ! Work area
  STRING    DAYS$                                   ! Day number of start of month

  GREG.DATE$ = RIGHT$("0000000000" +                \ Pad with zeros
               GREG.DATE$,10)                       !
  GREG.MONTH = VAL(MID$(GREG.DATE$,1,2))            ! Parse month
  GREG.DAY = VAL(MID$(GREG.DATE$,4,2))              ! Parse day
  GREG.YEAR = VAL(MID$(GREG.DATE$,7,4))             ! Parse year
  GREG.YEAR = GREG.YEAR - 1900                      ! Remove base
  IF MOD(GREG.YEAR,4) = 0 THEN BEGIN                ! Leap year?
           !  JanFebMarAprMayJunJulAugSepOctNovDec
     DAYS$ = "000031060091121152182213244274305335" ! Beg day of month
  ENDIF ELSE BEGIN                                  ! Not a leap year
           !  JanFebMarAprMayJunJulAugSepOctNovDec
     DAYS$ = "000031059090120151181212243273304334" ! Beg day of month
  ENDIF                                             !
  IF (GREG.MONTH = 0) OR                            \ Not a valid month?
     (GREG.MONTH > 12) THEN BEGIN                   !
     JULIAN.WORK = 0                                ! Return a zero
  ENDIF ELSE BEGIN                                  ! Have a valid month

     JULIAN.WORK = VAL(MID$(DAYS$,(GREG.MONTH-1)*3+1,3)) +   \
                   GREG.DAY                                  !
  ENDIF                                             !
  ?GREG.JULIAN = JULIAN.WORK                        ! Return julian date
  DAYS$ = ""                                        ! Free memory
END FUNCTION


!************************  CALCULATE THE DAY OF WEEK  ************************
!
!   This function returns the day of the week.  It accepts a date and
!   converts it to a numeric day of week.
!   The day of the week is returned as 0 = sunday,1 = monday .... 6 = saturday.
!
!   Algorithm:
!   This function uses Zeller's Congruence to calculate the day of the week
!   on which any date falls.
!   The following is an explanation of this algorithm
!
!   (1) Since February is the only month with a variable number of days, adjust
!       the month and year so that January and February are the last months of
!       the preceding year.
!
!   (2) Compute the contribution to the day of the week caused by the fact
!       that the months have different numbers of days.  Let m be the adjusted
!       month number, (from (1)), compute 26m - 2 and divide the result by 10,
!       discarding the remainder.
!
!   (3) Compute the contribution to the day of the week caused by the year.
!       Determine the century and the year within the century of the adjusted
!       year.  Add the following: the year within the century, the whole number
!       part of the result of dividing the year within the century by 4, the
!       whole number part of the result of dividing the century by 4, and
!       5 times the century.
!
!   (4) Add the Day, the contribution from the month, and contribution from
!       the year.  MOD this sum by 7 and the result will be the day of the
!       week.
!
!   The day of the week is returned as 0 = sunday,1 = monday .... 6 = saturday.
!
!******************************************************************************
FUNCTION ?DAY.OF.WEEK(EIM.DATE$) PUBLIC

  INTEGER*2 ?DAY.OF.WEEK, YEAR, MONTH, DAY, CENTURY, ADJMONTH, ADJYEAR,    \
            MONTHCORRECTION, YEARCORRECTION, LSTTWO

  STRING EIM.DATE$, WORK$

  WORK$ = EIM.DATE$
  IF VAL(LEFT$(WORK$,2)) < 90 THEN BEGIN         ! Determine if turn of century
    YEAR = 2000 + (INT% (VAL(LEFT$(WORK$, 2))))
  ENDIF ELSE BEGIN
    YEAR = 1900 + (INT% (VAL(LEFT$(WORK$, 2))))
  ENDIF
  MONTH= INT% (VAL (MID$ (WORK$, 3, 2)))
  DAY  = INT% (VAL (RIGHT$ (WORK$, 2)))

  IF MONTH <= 2 THEN BEGIN
    ADJMONTH = 10 + MONTH
    ADJYEAR = YEAR - 1
  ENDIF ELSE BEGIN
    ADJMONTH = MONTH - 2
    ADJYEAR = YEAR
  ENDIF

  MONTHCORRECTION = (26 * ADJMONTH - 2) / 10

  CENTURY = ADJYEAR / 100
  LSTTWO = MOD(ADJYEAR,100)
  YEARCORRECTION = LSTTWO + (LSTTWO / 4) + (CENTURY / 4) + 5 * CENTURY

  ?DAY.OF.WEEK = MOD((DAY + MONTHCORRECTION + YEARCORRECTION),7)

END FUNCTION


!*****************************************************************************
! SUB CNVRT.TIME
!
!  This subroutine adjusts the local time to make GMT.  It makes necessary
!  adjustments if daylight saving time is in effect.  It also checks for
!  necessary date adjustments as a result of the time adjustments.
!  The input string is of the form "YYMMDDhhmmss".
!*****************************************************************************
SUB CNVRT.TIME(DATE.TIME$)

  STRING                                         \
    DATE.TIME$,                                  \ date\time to convert
    LOCAL.TIME$,                                 \ local time
    LOCAL.DATE$,                                 \ local date
    DAYS$,                                       \
    DD$,                                         \ day
    MM$,                                         \ month
    YY$,                                         \ year
    HH$,                                         \ hour
    SECONDS$                                     ! seconds

  INTEGER*4                                      \
    START.DAY,                                   \ date DST starts
    END.DAY                                      ! date DST ends

  INTEGER*2                                      \
    DAY,                                         \ day of week
    MAR.31,                                      \ last day in Mar
    NOV.1                                        ! 1st day in Nov

  INTEGER*1                                      \
    HR,                                          \ hour
    MONTH,                                       \ month
    YEAR                                         ! year

    SECONDS$ = RIGHT$(DATE.TIME$, 2)             ! get seconds
    DATE.TIME$ = LEFT$(DATE.TIME$, 10)           ! strip seconds
    LOCAL.DATE$ = LEFT$(DATE.TIME$, 6)           ! get date

!AIO03864 2.
    ! Save the local time for the ClaimDate fields in the A1 and Z1 records
    ! before converting to GMT time
    A1.Z1.CLAIM.DATE$ = "20" + LOCAL.DATE$
!EIO03864

    LOCAL.TIME$ = RIGHT$(DATE.TIME$, 4)          ! get time
    DD$ = RIGHT$(LOCAL.DATE$, 2)                 ! get day
    MM$ = MID$(LOCAL.DATE$, 3, 2)                ! get month
    YY$ = LEFT$(LOCAL.DATE$, 2)                  ! get year
    HH$ = LEFT$(LOCAL.TIME$, 2)                  ! get hour
    WORK$ = MM$ + "/" + DD$ + "/" + "20" + YY$   ! reformat date
    THIS.DAY = ?GREG.JULIAN( WORK$ )             ! day of the year

!AIO02996 Establish the day of week of the last possible day that is always
!         standard time and then add 1..7 days to get to the following Sunday.
!         We can't just shift the 2006 algorithm back by 3 weeks, because in
!         some years the difference is 4 weeks (if April 5..7 falls on a Sunday).
    IF YEAR >= 7 THEN BEGIN                      ! 2007 or later
      WORK$ = YY$ + "0307"                       ! last possible standard time day
    ENDIF ELSE BEGIN                             ! else 2006 or before
!EIO02996
      WORK$ = YY$ + "0331"                       ! last day of march
    ENDIF                                        ! IO02996
    MAR.31 = ?DAY.OF.WEEK(WORK$)                 ! day of week for last day Mar
!AIO02996 2007 DST changes
    IF YEAR >= 7 THEN BEGIN                      ! 2007 or later
      WORK$ = "03/07/" + "20" + YY$              ! reformat date 2007 or after
    ENDIF ELSE BEGIN
!EIO02996
      WORK$ = "03/31/" + "20" + YY$              ! reformat date 2006 or before
    ENDIF                                        ! IO02996
    START.DAY = ?GREG.JULIAN( WORK$ )            ! day of the year for Mar 31
    START.DAY = START.DAY + (7 - MAR.31)         ! daylight saving start day

!AIO02996 Old calc was off by 1 day, which could delay the ending by 1 week
\   WORK$ = YY$ + "1101"                         ! Nov 1
\   NOV.1 = ?DAY.OF.WEEK(WORK$)                  ! get day of week
\   WORK$ = "11/01/" + "20" + YY$                ! reformat date
    WORK$ = YY$ + "1031"                         ! October 31 is latest ending day
    NOV.1 = ?DAY.OF.WEEK(WORK$)                  ! get day of week
    WORK$ = "10/31/" + "20" + YY$                ! reformat date
!EIO02996
    END.DAY = ?GREG.JULIAN( WORK$ )              ! get day of the year
    END.DAY = END.DAY - NOV.1                    ! daylight saving ends

    HR = VAL(HH$)                                ! get hour
    DAY = VAL(DD$)                               ! get some numbers
    MONTH = VAL(MM$)
    YEAR = VAL(YY$)
!AIO02996 In 2007, ending date goes from the last Sunday in October to the
!         first Sunday in November.  This is always a 7-day adjustment.
    IF YEAR >= 7 THEN BEGIN                      ! 2007 or later
      END.DAY = END.DAY + 7                      ! last Sun Oct -> first Sun Nov
    ENDIF
!EIO02996
    IF DAYLIGHT.SAVE% THEN BEGIN                 ! observe daylight savings?
      IF THIS.DAY = START.DAY THEN BEGIN         ! daylight saving starts today
        IF VAL(HH$) >= 2 THEN BEGIN              ! 2am or later
          HR = HR - 1                            ! back out the extra hr
        ENDIF                                    ! end hour added daylight saving
      ENDIF ELSE BEGIN
        IF THIS.DAY = END.DAY THEN BEGIN         ! daylight saving end today
          IF VAL(HH$) < 2 THEN BEGIN             ! not yet 2am
            HR = HR - 1                          ! back out hour
          ENDIF                                  ! end not yet ended
        ENDIF ELSE BEGIN
          IF THIS.DAY > START.DAY AND            \ in daylight saving
             THIS.DAY < END.DAY THEN BEGIN
            HR = HR - 1                          ! back out hour
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    HR = HR + VAL(GMT.DIFF$)                     ! adjust hour for GMT

    IF MOD(YEAR, 4) THEN BEGIN                   ! not leap year
      DAYS$ = "031059090120151181212243273304334365"
    ENDIF ELSE BEGIN                             ! leap year
      DAYS$ = "031060091121152182213244274305335366"
    ENDIF

    IF HR >= 24 THEN BEGIN                       ! past midnight
      HR = HR - 24                               ! adjust hour
      DAY = DAY + 1                              ! adjust day
      DD$ = RIGHT$("00" + STR$(DAY), 2)          ! make string
      THIS.DAY = THIS.DAY + 1                    ! adjust day of year
      I = 1                                      ! init
      J = 1
      DAY = 31                                   ! last day of Jan
      END.DAY = VAL(RIGHT$(DAYS$, 3))            ! num days in this year
      IF THIS.DAY > END.DAY THEN BEGIN           ! did the year change
        YEAR = YEAR + 1                          ! bump the year
        MM$ = "01"
        DD$ = "01"
      ENDIF ELSE BEGIN                           ! if year didn't change
        WHILE DAY < THIS.DAY                     ! get month based on day of the
          I = I + 3                              ! year
          J = J + 1
          DAY = VAL(MID$(DAYS$, I, 3))           ! last day of month
        WEND
        IF J > VAL(MM$) THEN BEGIN               ! did it change
          DD$ = "01"                             ! day is 1
        ENDIF
      ENDIF
      MM$ = RIGHT$("00" + STR$(J), 2)            ! month may have changed
    ENDIF

  YY$ = RIGHT$("00" + STR$(YEAR), 2)             ! build new date and time
  HH$ = RIGHT$("00" + STR$(HR), 2)
  GMT.DATE$ = "20" + YY$ + MM$ + DD$
  GMT.TIME$ = HH$ + RIGHT$(LOCAL.TIME$, 2) + SECONDS$
END SUB


!******************************************************************************
! Subroutine Name: OPEN.READ.OPTION.FILE
! Description: This routine opens then reads the option file, EAMWEOPT.DAT
!******************************************************************************
SUB OPEN.READ.OPTION.FILE

  STRING                                         \
    DUMMY$                                       ! work var

  INTEGER*2                                      \
    I%                                           ! work var


  INTEGER*4                                      \
    WIC.RTN                                      !

  ON ERROR GOTO NOT.FOUND

  NUM.AGENCIES$ = PACK$("00")                    ! init
  WIC.TENDER$ = PACK$("00")
  GMT.DIFF$ = PACK$("5")
  WIC.ACTIVE = -1
  OPEN "EAMWEOPT" DIRECT RECL EAMWEOPT.RECL AS EAMWEOPT.SN BUFFSIZE 1024

  IF WIC.ACTIVE THEN BEGIN                       ! options file found
    ! Read the control record
    READ FORM CONTROL.FMT$; #EAMWEOPT.SN,100;                  \
                                             NUM.AGENCIES$,    \
                                             WIC.TENDER$,      \
                                             DUMMY$,           \
                                             DAYLIGHT.SAVING$, \
                                             GMT.DIFF$,        \
                                             DUMMY$,           \
                                             DUMMY$,           \
                                             DUMMY$,           \
                                             DUMMY$            !
  ENDIF
  ! Now validate the control data
  NUM.AGENCIES$ = UNPACK$(NUM.AGENCIES$)

  ERR.RESUME = -1                                ! set flag to resume from errors

  NUM.AGENCIES% = VAL(NUM.AGENCIES$)             ! Get value of agency in integer format

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    IF (NUM.AGENCIES% > MAX.AGENCIES) THEN BEGIN ! if invalid number of agencies
      NUM.AGENCIES% = MAX.AGENCIES               ! limit to max
    ENDIF ELSE BEGIN                             ! else not more than max
      IF (NUM.AGENCIES% < 0) THEN BEGIN          ! if invalid
        NUM.AGENCIES% = 0                        ! reset to 0
      ENDIF                                      ! if invalid
    ENDIF                                        ! if invalid number of agencies
    NUM.AGENCIES$ = STR$(NUM.AGENCIES%)          ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    NUM.AGENCIES$ = "00"                         ! default to zero
    NUM.AGENCIES% = 0
  ENDIF                                          ! if no error occurred

  ORG.NUM.AGENCIES% = NUM.AGENCIES%              ! save number of agencies when file initially read

  WIC.TENDER$ = UNPACK$(WIC.TENDER$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(WIC.TENDER$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    WIC.TENDER$ = RIGHT$("00" + WIC.TENDER$,2)   ! make sure it's formatted for display
  ENDIF ELSE BEGIN                               ! else an error occurred
    WIC.TENDER$ = "00"                           ! default to zero
  ENDIF                                          ! if no error occurred


  ERR.RESUME = -1                                ! set flag to resume from error

  GMT.DIFF$ = UNPACK$(GMT.DIFF$)

  I% = VAL(GMT.DIFF$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    IF I% >4 AND I% < 10 THEN BEGIN
      GMT.DIFF$ = STR$(I%)                       ! format data
    ENDIF ELSE BEGIN
      GMT.DIFF$ = "5"                            ! default to EST
    ENDIF
  ENDIF ELSE BEGIN                               ! else an error occurred
    GMT.DIFF$ = "5"                              ! default to EST
  ENDIF                                          ! if no error occurred

  IF WIC.ACTIVE THEN BEGIN                       ! options file found
    ! Now read all the agency records
    FOR I% = 1 TO NUM.AGENCIES%
      READ FORM AGENCY.FMT$; #EAMWEOPT.SN,I%;                         \
                                             AGENCY.MAIN.OPTION$(I%), \
                                             DUMMY$,                  \
                                             DUMMY$,                  \
                                             DUMMY$,                  \
                                             DUMMY$,                  \
                                             DUMMY$,                  \
                                             DUMMY$,                  \
                                             DUMMY$                   ! reserved
    NEXT I%

    DUMMY$ = ""                                    ! cleanup

  ENDIF

  CLOSE EAMWEOPT.SN

  EXIT SUB

NOT.FOUND:

  INWE.ERR$ = ERR
  INWE.ERRF% = ERRF%

  IF INWE.ERR$ = "CU" THEN RESUME                ! close unopened


  IF INWE.ERR$ = "OE" AND                        \
     INWE.ERRF% = EAMWEOPT.SN THEN BEGIN         !
    WIC.ACTIVE = 0                               ! no processing is possible
    CALL ADXSERVE(WIC.RTN,                       \
                  26,                            \
                  LEN(WIC.ERR.MSG$),             \
                  WIC.ERR.MSG$)                  ! display BG msg
    WAIT ; 20000
    RESUME
  ENDIF

END SUB                                          ! end OPEN.READ.OPTION.FILE


!******************************************************************************
! Subroutine Name: PARSE.AGENCY.DATA
! Description: This routine parses the agency record data into work variables.
!******************************************************************************
SUB PARSE.AGENCY.DATA(CURR.REC)

  INTEGER*2                                      \
    CURR.REC                                     ! record into data array

  STRING                                         \
    A$                                           ! work string

  A$ = AGENCY.MAIN.OPTION$(CURR.REC)             ! put data into work variable

  WIC.AGENCY$ = LEFT$(A$,2)
  AGENCY.ARRAY$(CURR.REC) = WIC.AGENCY$
  ACTIVE.FLAG.ARRAY$(CURR.REC) = MID$(A$,3,1)
  BIN.NUMBER.ARRAY$(CURR.REC) = UNPACK$(MID$(A$,4,3))
  MERCHANT.ID.ARRAY$(CURR.REC) = MID$(A$,7,15)
  FORWARD.INSTITUTE.ARRAY$(CURR.REC) = RIGHT$(UNPACK$(MID$(A$,22,6)),11)
  RECEIVE.INSTITUTE.ARRAY$(CURR.REC) = RIGHT$(UNPACK$(MID$(A$,28,6)),11)

END SUB                                          ! end PARSE.AGENCY.DATA


!******************************************************************************
! FIND.KEYS
!
! This subroutine searches a keyed file and extracts the keys.
! It places the keys into an array.
!******************************************************************************
SUB FIND.KEYS

  STRING                                         \
      KEYCODE$,                                  \ item code
      A$,                                        \ work string
      NULLKEY$,                                  \ null packed item code string
      TEMP$                                      ! work string


   INTEGER*2                                     \
      REC.LEN,                                   \ record length
      RECORDS%,                                  \ record count
      I%,                                        \ indices
      J%,                                        \
      K%                                         !

   INTEGER*4                                     \
      SECTORS%                                   ! sector count


  OPEN FILENAME$ AS 10 BUFFSIZE 4096 NOWRITE NODEL
  READ FORM "C512"; #10; A$                      ! read the first sector
  SECTORS% = SIZE(FILENAME$)/512 - 1             ! get the file size
  RECORDS% = 0                                   ! initialize record count
  NULLKEY$ = STRING$(8,CHR$(0))                  ! zero string
  REC.LEN = 35                                   ! status record length

  I% = -1                                        ! flag not at end of file

\ REM!! *************************************************************!!
\ REM!! * SEARCH WIC STATUS FILE FOR KEYS                           *!!
\ REM!! *************************************************************!!
  WHILE  I%                                      ! while more sectors
    READ FORM "C4096"; #10; A$                   ! read 8 sectors
    SECTORS% = SECTORS% - 8                      ! dec. sectors left

    IF SECTORS% < 1 THEN BEGIN                   ! if last read then
      I% = 0                                     ! set flag to end loop
      IF SECTORS% < 0 THEN BEGIN                 ! overshot file
        J% = (8 + SECTORS%) * 512                ! compute real data
        K% = (0 - SECTORS%) * 512                ! compute junk data
        A$ = LEFT$(A$,J%) +                      \ save good stuff and
                  STRING$(K%,CHR$(0))            ! zero junk from read
      ENDIF
    ENDIF

    FOR K% = 0 TO 3584 STEP 512                  ! for each of 8 sectors
      ! for each of the records
      FOR J% = K%+5 TO K%+511-REC.LEN STEP REC.LEN
        KEYCODE$ = MID$(A$,J%,8)                 ! capture the keycode

        IF KEYCODE$ = NULLKEY$ THEN BEGIN        ! if key is zero then
          J% = K% + 465                          ! end process for sector
        ENDIF ELSE BEGIN
          RECORDS% = RECORDS% + 1                ! inc record counter
          STATUS.KEYS$(RECORDS%) = KEYCODE$
        ENDIF
NEXT.REC:
      NEXT J%                                    ! next record
    NEXT K%                                      ! next sector
  WEND

  CLOSE 10                                       ! close the file
  REC.CNT = RECORDS%                             ! save record count
END SUB


!*****************************************************************************
! FUNCTION ?CALC.REMOVE.DATE
!
! This function returns a julian date which is used to determine if a
! record should be removed from EAMWERCL.DAT.  The date will cause the
! record to be removed when it is ABOUT 90 days old.  The number of
! days is approximate because no conversion is being done for GMT or leap
! year.  Example: the returned value for June 15, 2005 is 5076.
!*****************************************************************************
FUNCTION ?CALC.REMOVE.DATE

  STRING                                         \
    Y$                                           !

  INTEGER *2                                     \
    YR,                                          \
    WORK2,                                       \
    ?CALC.REMOVE.DATE                            !

  Y$ = MID$(DATE$, 2, 1)                         ! last digit of yr
  YR = VAL(Y$)                                   ! convert
  IF YR < DECADE.BREAK THEN BEGIN                ! in years 2010..2103
    YR = YR + 10                                 ! bump 0..3 to 10..13
  ENDIF

  IF THIS.DAY < 90 THEN BEGIN                    ! beginning of yr
    WORK2 = 365 - (90 - THIS.DAY)                ! back up 90 days
    YR = YR - 1                                  ! it is in the previous year
  ENDIF ELSE BEGIN                               ! year more than 90 days old
    WORK2 = THIS.DAY - 90                        ! 90 days ago
  ENDIF
  ?CALC.REMOVE.DATE = (YR*1000) + WORK2          ! Yddd

FEND


!*****************************************************************************
! WE.INIT
!
! This subroutine initializes the WIC variables and dims the arrays.
!*****************************************************************************
SUB WE.INIT PUBLIC

   INTEGER*2                                     \
     I1

   STRING                                        \
     WIC.TEMP$,                                  \
     THIS.DATE$                                  !

   DIM SELF.CHK.ARRAY(10)
   CRLF$ = CHR$(13) + CHR$(10)                   !
   EAMWEOPT.SN = 1                               ! session number for EAMWEOPT.DAT
   EAMWEOPT.RECL = 1024                          ! record length for EAMWEOPT.DAT

   ! We are anticipating a 10-year (or less) window of use.  If the year is
   ! 0..3, we assume that it is really 2010..2013, and adjust the date
   ! accordingly.  If DECADE.BREAK is left at 4, this method will break on or
   ! about March 31, 2014.  After a few years, the 10-year window can be
   ! shifted, if necessary, by increasing DECADE.BREAK to any value less than
   ! the year in the current decade.  See similar assumptions in
   ! ?CALC.REMOVE.DATE and REMOVE.OLD.RECS.
   DECADE.BREAK = 4                              ! < 4 signifies 2010..2013

  ! create the read/write format for the records
   CONTROL.FMT$ = "2C1,C120,2C1,3C255,C135"
   AGENCY.FMT$ = "C33,C240,C40,C120,C240,C120,C80,C151"

   MAX.AGENCIES = 10                             ! set once, update many places
   DIM AGENCY.MAIN.OPTION$(MAX.AGENCIES)
   OPEN "EAMRDESC" RECL 49 AS 6  NOWRITE NODEL   ! Open report descriptor file
   READ #6, 4560; WIC.ERR.MSG$
   READ #6, 4561; WIC.STORE.LINE1$
   READ #6, 4562; WIC.STORE.LINE2$
   CLOSE 6
   CALL OPEN.READ.OPTION.FILE                    ! open and read EAMWEOPT.DAT
   IF WIC.ACTIVE THEN BEGIN                      ! options found
     THIS.DATE$ = DATE$ + TIME$
     CALL CNVRT.TIME(THIS.DATE$)
!    CURRENT.JULIAN = VAL(MID$(THIS.DATE$,2,1))  ! year of decade for today
!    IF CURRENT.JULIAN < DECADE.BREAK THEN BEGIN ! this must be 2010 - 2013
!      CURRENT.JULIAN = CURRENT.JULIAN + 10      ! make it 10 - 13
!    ENDIF
!    CURRENT.JULIAN = CURRENT.JULIAN* 1000 + THIS.DAY ! YDDD or 1YDDD for today

     DIM AGENCY.ARRAY$(NUM.AGENCIES%)            ! agency id's
     DIM ACTIVE.FLAG.ARRAY$(NUM.AGENCIES%)       ! agency active flag array
     DIM BIN.NUMBER.ARRAY$(NUM.AGENCIES%)        ! not used anywhere
     DIM MERCHANT.ID.ARRAY$(NUM.AGENCIES%)       ! merchant id array
     DIM FORWARD.INSTITUTE.ARRAY$(NUM.AGENCIES%) ! claim file sender array
     DIM RECEIVE.INSTITUTE.ARRAY$(NUM.AGENCIES%) ! agency bin # array

     FOR I1 = 1 TO NUM.AGENCIES%                 ! get agency data from options
       CALL PARSE.AGENCY.DATA(I1)
     NEXT I1

     STORE.NAME.LOC$ = WIC.STORE.LINE1$ +        \ build store name/address
                       WIC.STORE.LINE2$          !
     STORE.NAME.LOC$ = LEFT$(STORE.NAME.LOC$ +     \
                             STRING$(99, " "), 99) !

     CALL ADXSERVE(RET,4,PARM1,WIC.TEMP$)        ! get store number
     ACQUIRING.INST.ID$ = RIGHT$(STRING$(11, "0") + \
                          MID$(WIC.TEMP$,1,4), 11) !

     ! WISTATUS has 10 characters of the last CS.RESTART we saw, plus a
     ! 26-character string per agency processed.  These are only used
     ! for restarts of checkout support.
     ON ERROR GOTO NO.WISTATUS                   ! if no file yet, nothing to do
     OPEN "WIC4:WISTATUS.DAT" AS 64              ! read prior status from file
     READ #64; WIC.STATUS.STR$
     WE.RESTART = VAL(LEFT$(WIC.STATUS.STR$, 10)) ! last CS.RESTART we processed
     WIC.STATUS.STR$ = RIGHT$(WIC.STATUS.STR$,    \ get agency checkpoints
                       LEN(WIC.STATUS.STR$) - 10)
     CLOSE 64
!AIO10055 We can't use this place as an ON ERROR catcher
\NO.WISTATUS:
!EIO10055
   ENDIF

!AIO10055 If no error, fall through from above and exit
!         If an error occurred, do a RESUME, and then exit
CSFBEW01.EXIT:
   EXIT SUB

NO.WISTATUS:
   RESUME CSFBEW01.EXIT
!EIO10055

END SUB


!*****************************************************************************
! REMOVE.OLD.RECS
!
! This subroutine removes records from the WIC claims status file that are
! more than 90 days old.
!*****************************************************************************
SUB REMOVE.OLD.RECS

  INTEGER*2                                      \
    KEY.DATE,                                    \ julian date in the key
    REMOVE.DATE                                  ! julian date for comparison

  DIM STATUS.KEYS$(5000)                         ! reserve space
  REMOVE.DATE = ?CALC.REMOVE.DATE                ! get julian date for comparison
                                                 ! THIS.DAY is current jul date
  FILENAME$ = "WIC1:EAMWERCL.DAT"                ! file to use

  CALL FIND.KEYS                                 ! extract the file keys

  OPEN FILENAME$ KEYED RECL 35 AS 10             ! open status file

  FOR I = 1 TO REC.CNT                           ! while there are keys
    KEY.DATE = VAL(MID$(STATUS.KEYS$(I), 3, 4))  ! extract julian date from key
    IF KEY.DATE < DECADE.BREAK*1000 THEN BEGIN   ! this must be 2010 - 2013
      KEY.DATE = KEY.DATE + 10000                ! make it 10 - 13
    ENDIF

    IF KEY.DATE < REMOVE.DATE THEN BEGIN         ! if older than remove date
      DELREC 10; STATUS.KEYS$(I)                 ! delete the old record
    ENDIF
  NEXT I

  CLOSE 10                                       ! close the file
  DIM STATUS.KEYS$(0)                            ! give the space back
END SUB


!********************************************************************
! BLD.WIC.TLR
!
! This subroutine builds the WIC EBT trailer record (Z1) when checkout
! support processes the TLOG close record (21).
! The FILE.CREATE.DATE$ field is also used the WIC EBT header record.
!********************************************************************
SUB BLD.WIC.TLR

  REC.SEQ.NUM$ = RIGHT$("000000" +               \ format it
                         STR$(REC.SEQ.NUM + 1), 6) !
  FILE.CREATE.DATE$ = GMT.DATE$                  ! TLOG closing date gmt
  FILE.CREATE.TIME$ = GMT.TIME$                  ! TLOG closing time gmt
  CNT.RECS$ = RIGHT$("0000000" +                 \ adjust for hdr and trl recs
                         STR$(REC.SEQ.NUM - 1), 7) !
  TOT.AMT.CLAIMED$ = RIGHT$(STRING$(12, "0") +   \ total across all trans
                               STR$(TOT.WIC.AMT), 12) !
  TOT.DISCOUNT.AMT$ = RIGHT$(STRING$(12, "0") +  \ total across all trans
                                STR$(TOT.WIC.DISCOUNT), 12) !

  Z1.REC$ = "Z1"                +                \ trailer rec identifier
            REC.SEQ.NUM$        +                \ sequence number
            FILE.CREATE.DATE$   +                \ file create date gmt
            FILE.CREATE.TIME$   +                \ file create time gmt
            "02"                +                \ constant
            CNT.RECS$           +                \ total WIC claim recs
            TOT.AMT.CLAIMED$    +                \ total claimed amount
\AIO03864 2. Use a different variable for the create date.
\           FILE.CREATE.DATE$   +                \ local file create date
            A1.Z1.CLAIM.DATE$   +                \ local file create date
\EIO03864
            TOT.DISCOUNT.AMT$   +                \ total discount amt
            CHR$(13) + CHR$(10)                  ! crlf

END SUB


!**********************************************************************
! BLD.WIC.HDR
!
! This subroutine builds the WIC EBT header record (A1).  It is built
! after the TLOG has been closed.  This is because it requires data
! that is not generated until the TLOG processes the TLOG closing record
! (21 string).
!**********************************************************************
SUB BLD.WIC.HDR


  INTEGER*2                                      \
    I1

  ! establish the institute IDs based upon the agency ID
  FOR I1 = 1 TO NUM.AGENCIES%
    IF AGENCY.ARRAY$(I1) = WIC.AGENCY$ THEN BEGIN
      FORWARD.INSTITUTE$ = FORWARD.INSTITUTE.ARRAY$(I1)
      RECEIVE.INSTITUTE$ = RECEIVE.INSTITUTE.ARRAY$(I1)
      I1 = NUM.AGENCIES% + 1
    ENDIF
  NEXT I1

  FORWARD.INSTITUTE$ = RIGHT$(STRING$(11, "0") + \
                                   FORWARD.INSTITUTE$, 11) !
  FILE.NAME$ = LEFT$("CLAIM FILE" + STRING$(25, " "), 25)
  RECEIVE.INSTITUTE$ = RIGHT$(STRING$(11, "0") + \
                                   RECEIVE.INSTITUTE$, 11) !

  A1.REC$ = "A1"               +                 \ WIC EBT header record
            "000001"           +                 \ constant
            FILE.CREATE.DATE$  +                 \ file create date gmt
            FILE.CREATE.TIME$  +                 \ file create time gmt
            "02"               +                 \ constant
            FORWARD.INSTITUTE$ +                 \
            FILE.NAME$         +                 \ constant
            "NEW     "         +                 \ constant
            "0000"             +                 \ constant
            "0000000"          +                 \ Record Count (unused in TX)
            "0000000"          +                 \ Record REJ Count (unused in TX)
            RECEIVE.INSTITUTE$ +                 \
            ACQUIRING.INST.ID$ +                 \
\AIO03864 2. Use a different variable for the create date.
\           FILE.CREATE.DATE$  +                 \ local create date
            A1.Z1.CLAIM.DATE$  +                 \ local create date
\EIO03864
            CHR$(13) + CHR$(10)                  ! crlf

END SUB


!**********************************************************************
! SUB CONSOLIDATE.ITEMS
!
! This subroutine works with the e3 string array and consolidates like
! UPC's into a single record.  It then adjusts the record count for use
! in the trailer record.
!**********************************************************************
SUB CONSOLIDATE.ITEMS

  INTEGER*2                                      \
    CTRL(1),                                     \ work array
    F,                                           \ work vars
    I1,                                          \
    I2,                                          \
    THIS.QTY,                                    \ qty in this string
    TOT.QTY,                                     \ cum qty
    NOT.PROCESSED,                               \ num records not processed
    INDEX                                        ! index

  INTEGER*4                                      \
    THIS.COST,                                   \ cost in this string
    TOT.COST,                                    \ cum cost
    THIS.PUR,                                    \ purchase units in this string
    TOT.PUR                                      ! cum purchase units

  STRING                                         \
    TEMP.STR$                                    ! work var

  DIM CTRL(ADDENDA.SEQ.NUM%)                     ! set up arrays for max size
  DIM WORK.ARRAY$(ADDENDA.SEQ.NUM%)

  FOR I1 = 1 TO ADDENDA.SEQ.NUM%                 ! init control array
    CTRL(I1) = 1
  NEXT I1

  F = 0                                          ! init
  INDEX = 1
  NOT.PROCESSED = ADDENDA.SEQ.NUM%               ! set to num recs avail

  WHILE F = 0

    FOR I1 = 1 TO ADDENDA.SEQ.NUM%
       IF CTRL(I1) = 1 THEN BEGIN                ! not processed
         WORK.ARRAY$(INDEX) = E3.STR$(I1)        ! save string to work array
         CAT.CODE$ = MID$(E3.STR$(I1), 12, 2)    ! extract fields for test
         SUB.CAT.CODE$ = MID$(E3.STR$(I1), 14, 3)
         UPC$ = MID$(E3.STR$(I1), 22, 17)
         CTRL(I1) = 0                            ! indicate array elem processed
         NOT.PROCESSED = NOT.PROCESSED - 1       ! decrement count to be processed

         FOR I2 = (I1 + 1) TO ADDENDA.SEQ.NUM% ! check for matches
           TEMP.STR$ = E3.STR$(I2)
           IF UPC$ = MID$(E3.STR$(I2), 22, 17)     AND \ match found
              CAT.CODE$ = MID$(E3.STR$(I2), 12, 2) AND \
              SUB.CAT.CODE$ = MID$(E3.STR$(I2), 14, 3) THEN BEGIN
             THIS.PUR = VAL(MID$(E3.STR$(I2), 17, 5))  ! extract purchase units
             TOT.PUR = VAL(MID$(WORK.ARRAY$(INDEX), 17, 5)) + \ update tot p units
                       THIS.PUR                               !
             THIS.QTY = VAL(MID$(E3.STR$(I2), 39, 5))  ! extract qty
             TOT.QTY = VAL(MID$(WORK.ARRAY$(INDEX), 39, 5)) + \ update tot qty
                       THIS.QTY                               !
             THIS.COST = VAL(MID$(E3.STR$(I2), 44, 9)) ! extract cost
             TOT.COST = VAL(MID$(WORK.ARRAY$(INDEX), 44, 9)) + \ update tot cost
                        THIS.COST                              !
             ! update claim string with totals
             WORK.ARRAY$(INDEX) = LEFT$(WORK.ARRAY$(INDEX), 16)           + \
                                  RIGHT$("00000" + STR$(TOT.PUR), 5)      + \
                                  MID$(WORK.ARRAY$(INDEX), 22, 17)        + \
                                  RIGHT$("00000" + STR$(TOT.QTY), 5)      + \
                                  RIGHT$("000000000" + STR$(TOT.COST), 9) + \
                                  CHR$(13) + CHR$(10)                       !
             CTRL(I2) = 0                      ! indicate processed
             NOT.PROCESSED = NOT.PROCESSED - 1 ! decrement num to be processed
           ENDIF
         NEXT I2
         INDEX = INDEX + 1                       ! bump the index
       ENDIF
    NEXT I1

    IF NOT.PROCESSED <= 0 THEN BEGIN             ! if all processed
      F = 1                                      ! set flag to end
    ENDIF ELSE BEGIN                             ! else
      F = 0                                      ! set to continue
    ENDIF

  WEND

  INDEX = INDEX - 1                              ! correct the index
  ADDENDA.SEQ.NUM% = INDEX                       ! reset num records
  DIM E3.STR$(INDEX+1)

  ! renumber records and rebuild e3.str$()
  FOR I1 = 1 TO INDEX
    WORK.ARRAY$(I1) = LEFT$(WORK.ARRAY$(I1), 8)   + \
                      RIGHT$("000" + STR$(I1), 3) + \
                      RIGHT$(WORK.ARRAY$(I1), 43)   !
    E3.STR$(I1) = WORK.ARRAY$(I1)
  NEXT I1

  DIM CTRL(0)                                    ! free the space
  DIM WORK.ARRAY$(0)

END SUB


!**********************************************************************
! CSWIEC03
!
!    Checkout Support - Treat WIC string as User Exit string
!    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      This subprogram makes EAMCSVFC treat the WIC strings
!      as User Exit strings.
!
!    Called from EAMCSVFC
!
! IF transaction string type is 80 (WIC EBT) THEN
!    make transaction type 99 (USER EXIT STRING)
!
!**********************************************************************
SUB CSWIEC03 (WORKING) PUBLIC
  INTEGER*1 WORKING

  IF WORKING = 80 THEN BEGIN               ! if WIC EBT string
    WORKING = 99                           ! change to user string
  ENDIF

END SUB


!**********************************************************************
! CSWIEC02
!
! This subroutine is called after the TLOG has been closed but before
! a new TLOG is created.  It causes the WIC EBT claims file header and
! trailer records to be created.  It then creates a current period claims
! file.  The header record is written to the claims file and then contents
! of the WIC EBT work file are written to the claims file.  When that is
! complete, the trailer record is written to the file and the file is closed.
! The current file is renamed to the previous file and the previous file
! is copied to the host file.
!
! This process is repeated for each unique WIC agency that has been
! identified in the TLOG.
!
! It is called from EAMCSCLC.
!**********************************************************************
SUB CSWIEC02 PUBLIC
  INTEGER*2                                      \
    WIC.RC,                                      \ return code
    SEQ.NUM,                                     \
    LEN.STATUS.STR                               ! len of the status ctrl string

  STRING                                         \
    SEQ.NUM$,                                    \ seq num for multi closes in same day
    TRACKING.ID$,                                \ agency date
    CREATE.DATE$,                                \ file create date gmt
    SETTLEMENT.TOT$,                             \ total settlement amt
    SETTLEMENT.DATE$,                            \ date of settlement
    RESPONSE.DATE$,                              \ date received
    STATUS.FLAG$,                                \ status of claim
    RECONCILE.REC$,                              \ number of reconciliation recs
    CLAIM.FILE$,                                 \ current period claims file
    CLAIM.FILE.PREV$,                            \ previous period claims file
    HOST.CLAIM.FILE$,                            \ host claims file
    CLAIM.DATE$,                                 \ day of year
    YY$,                                         \ year
    MM$,                                         \ month
    DD$,                                         \ day
    THIS.DATE$                                   ! GMT of closing date & time

  ON ERROR GOTO CREATE.FILE

  CLAIM.DATE$ = DATE$                            ! get current date
  YY$ = LEFT$(CLAIM.DATE$, 2)                    ! break it apart
  MM$ = MID$(CLAIM.DATE$, 3, 2)
  DD$ = RIGHT$(CLAIM.DATE$, 2)
  CLAIM.DATE$ = MM$ + "/" + DD$ + "/20" + YY$    ! rebuild it
  WORK1I4 = ?GREG.JULIAN(CLAIM.DATE$)            ! get day of year
  CLAIM.DATE$ = RIGHT$("000" + STR$(WORK1I4), 3)

  LEN.STATUS.STR = LEN(WIC.STATUS.STR$)          ! length of control string

  WHILE LEN.STATUS.STR > 0                       ! while more to process
    WIC.AGENCY$ = LEFT$(WIC.STATUS.STR$, 2)        ! get agency ID
    REC.SEQ.NUM = VAL(MID$(WIC.STATUS.STR$, 3, 4)) ! get record sequence number
    TOT.WIC.AMT = VAL(MID$(WIC.STATUS.STR$, 7,10)) ! get WIC amount
    TOT.WIC.DISCOUNT = VAL(MID$(WIC.STATUS.STR$,17,10)) ! get WIC discount
    REC.SEQ.NUM$ = RIGHT$("000000" +             \ format it
                         STR$(REC.SEQ.NUM), 6)   !
    CLAIM.FILE$ = "WIC4:EAMWE" + WIC.AGENCY$ + "C.DAT"  ! build dynamic file
    CLAIM.FILE.PREV$ = "WIC4:EAMWE" + WIC.AGENCY$ + "P.DAT" ! names

    CREATE POSFILE CLAIM.FILE$ AS 5 MIRRORED ATCLOSE ! create claims file

    THIS.DATE$ = DATE$ + TIME$
    CALL CNVRT.TIME(THIS.DATE$)

    CALL BLD.WIC.TLR                             ! build trailer rec first
    CALL BLD.WIC.HDR                             ! then build header rec
    WRITE FORM "C118"; #5; A1.REC$               ! write header rec
    OPEN "WIC4:" + WIC.AGENCY$ + "WICWRK.DAT" AS 64 ! open work file

    EOF = 0
    WHILE NOT EOF
      READ #64; LINE WIC.WORK$                   ! read work record
      IF NOT EOF THEN BEGIN                      ! data to process
        WIC.WORK$ = WIC.WORK$ + CRLF$
        FORMAT.STR$ = "C" + STR$(LEN(WIC.WORK$))
        WRITE FORM FORMAT.STR$; #5; WIC.WORK$    ! write claim record
      ENDIF
    WEND
    WRITE FORM "C65"; #5; Z1.REC$                ! write trailer record
    CLOSE 5                                      ! close files

    WORK1I4 = 0                                  ! init
    WORK1I4 = SIZE(CLAIM.FILE.PREV$)             ! check for file

    IF WORK1I4 > 0 THEN BEGIN                    ! if found delete it
      OPEN CLAIM.FILE.PREV$ AS 6
      DELETE 6
    ENDIF

    SEQ.NUM = 0
    SEQ.NUM$ = RIGHT$("00" + STR$(SEQ.NUM), 2)
    HOST.CLAIM.FILE$ = "WIC4:"                       + \ build host file name
                       WIC.AGENCY$                   + \
                       MID$(FILE.CREATE.DATE$, 4, 1) + \
                       CLAIM.DATE$                   + \
                       ".S"                          + \
                       SEQ.NUM$                        !
    WORK1I4 = 0                                ! init
    WORK1I4 = SIZE(HOST.CLAIM.FILE$)           ! check for file

    IF WORK1I4 > 0 THEN BEGIN                  ! if file exists create new
      WHILE WORK1I4 > 0                        ! one with next seq.num
        SEQ.NUM = SEQ.NUM + 1
        SEQ.NUM$ = RIGHT$("00" + STR$(SEQ.NUM), 2)
        HOST.CLAIM.FILE$ = "WIC4:"                       + \ bld host file name
                           WIC.AGENCY$                   + \
                           MID$(FILE.CREATE.DATE$, 4, 1) + \
                           CLAIM.DATE$                   + \
                           ".S"                          + \
                           SEQ.NUM$                        !
        WORK1I4 = SIZE(HOST.CLAIM.FILE$)       ! check for file
      WEND
    ENDIF

    WIC.RC = RENAME(HOST.CLAIM.FILE$, CLAIM.FILE$) ! rename current to host

    IF WIC.RC = 0 THEN BEGIN
      WORK2I2 = ADXERROR( 0,                     \ 0 for controller
                          0,                     \ no msg displayed
                          0,                     \ no msg displayed
                          5,                     \ severity level
                          2,                     \ 2 = checkout support
                          CSMLEC03+RIGHT$("00"+STR$(ERRF%),2)+ERR+ERRNHEX$)
    ENDIF ELSE BEGIN
      DELETE 64                                  ! delete work file
        ! add a record to the claim/reconciliation status file
      TRACKING.ID$ = WIC.AGENCY$                   + \
                     MID$(FILE.CREATE.DATE$, 4, 1) + \
                     CLAIM.DATE$                   + \
                     SEQ.NUM$                        !
      CREATE.DATE$ = "20" + RIGHT$(FILE.CREATE.DATE$, 6)
      SETTLEMENT.TOT$ = "000000000000"
      SETTLEMENT.DATE$ = "00000000"
      RESPONSE.DATE$ = "000000"
      STATUS.FLAG$ = "P"
      RECONCILE.REC$ = PACK$(CREATE.DATE$)      + \
                       PACK$(FILE.CREATE.TIME$) + \
                       PACK$(TOT.AMT.CLAIMED$)  + \
                       PACK$(SETTLEMENT.TOT$)   + \
                       PACK$(SETTLEMENT.DATE$)  + \
                       PACK$(RESPONSE.DATE$)    + \
                       STATUS.FLAG$               !
      OPEN "WIC1:EAMWERCL.DAT" KEYED RECL 35 AS 5 ! open it
      WRITE FORM "C8 C27"; #5; TRACKING.ID$, RECONCILE.REC$ ! write to output file
      CLOSE 5
      CALL ADXCOPYF(RET, HOST.CLAIM.FILE$,       \
                    CLAIM.FILE.PREV$,0,0,0)      ! Copy host file to previous
    ENDIF
    ! remove the control data for this WIC agency from the status
    ! control string
    WIC.STATUS.STR$ = RIGHT$(WIC.STATUS.STR$, LEN.STATUS.STR - 26)
    CREATE POSFILE "WIC4:WISTATUS.DAT" AS 64 MIRRORED ATCLOSE ! save to file
    WRITE #64; RIGHT$("0000000000" + STR$(CS.RESTART), 10) + \
               WIC.STATUS.STR$
    CLOSE 64
    LEN.STATUS.STR = LEN(WIC.STATUS.STR$)    ! length of new string
  WEND

  CALL REMOVE.OLD.RECS                       ! remove records over 90 days old

  EXIT SUB

CREATE.FILE:

  INWE.ERR$ = ERR
  INWE.ERRF% = ERRF%

  IF INWE.ERR$ = "EF" AND                        \
     INWE.ERRF% = 64 THEN BEGIN                  !
    EOF = -1
    RESUME
  ENDIF

  IF INWE.ERR$ = "OE" AND                        \
     INWE.ERRF% = 0 THEN BEGIN                   !
    RESUME
  ENDIF

  IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 5) THEN BEGIN
    CREATE POSFILE "WIC1:EAMWERCL.DAT" KEYED 8,,,5000 \
      RECL 35 AS 5 MIRRORED ATCLOSE                   !
    RESUME
  ENDIF
END SUB

!**********************************************************************
! CSWIEC01
!
! This subroutine creates the individual E3 ( Addenda records ) in a
! transaction.  They are received prior to the processing of the WIC EBT
! tender string.  These strings are saved in an array until the WIC EBT
! detail record (D4) is created.  The detail record is created based on
! the contents of the WIC EBT summary string that follows the WIC EBT tender
! string in the TLOG.  After the D4 record is written to a work file,
! the contents of array, containing the E3 records, is written to the
! work file and the array is cleared.
!**********************************************************************
SUB CSWIEC01 PUBLIC

  INTEGER* 2                                     \
    WIC.FOUND,                                   \ loc of WIC agency in cntrl str
    I1,                                          \ index
    J1,                                          \
    CAT.ZEROCAT,                                 \ loop variable IO01214
    LEN.STR                                      ! string length

  INTEGER*4                                      \
    WIC.RTN                                      !

  STRING                                         \ work strings
    WIC.TEMP$,                                   \
    WORK.NAME$,                                  \
    REC.SEQ.NUM.NEXT$                            ! for next sequence

  IF CS.RESTART <= WE.RESTART THEN BEGIN         ! if a restart is occurring
    EXIT SUB                                     ! don't reprocess stale data
  ENDIF

  IDENTIFIER = PACKBIN2(CS.INPUT$,               \ type WIC string
                        CS.FLD.POS(1),           \
                        CS.FLD.LEN(1))           !

  WIC.AGENCY$ = MID$(CS.INPUT$,                  \ WIC issuing agency
                     CS.FLD.POS(2)+1,            \ +1 to acct for ":"
                     CS.FLD.LEN(2) )             !

  WORK.NAME$ = "WIC4:" + WIC.AGENCY$ + "WICWRK.DAT" ! work file

  IF (IDENTIFIER = 1) OR                         \
     (IDENTIFIER = 3) THEN BEGIN                 ! WIC EBT item string

    IF IDENTIFIER = 1 THEN BEGIN                 ! need claim record
      IF NOT WIC.ACTIVE THEN BEGIN               ! wic options not found
        CALL ADXSERVE(WIC.RTN,                   \ make store aware
                      26,                        \
                      LEN(WIC.ERR.MSG$),         \
                      WIC.ERR.MSG$)              ! display BG msg
        STOP                                     ! cannot process further
      ENDIF
    ENDIF

    ! if this is the first addenda string to be processed,
    IF ADDENDA.SEQ.NUM% = 0 THEN BEGIN           ! 1st addenda string
      WIC.FOUND = MATCH(WIC.AGENCY$, WIC.STATUS.STR$, 1) ! search agency ID

      IF WIC.FOUND = 0 THEN BEGIN                ! if not found
        WIC.STATUS.STR$ = WIC.STATUS.STR$ +      \ add to control string
                          WIC.AGENCY$     +      \
                          "0001"          +      \ seq num of "previous" D4 rec
                          "0000000000"    +      \ place holder for tot.wic.amt
                          "0000000000"           ! place holder for tot.wic.discount
        CREATE POSFILE WORK.NAME$ AS 5 MIRRORED ATCLOSE ! create work file
        REC.SEQ.NUM = 1                          ! set seq num
        TOT.WIC.AMT = 0                          ! set WIC amount
        TOT.WIC.DISCOUNT = 0                     ! set WIC discount
      ENDIF ELSE BEGIN                           ! already in control str
        OPEN WORK.NAME$ AS 5 APPEND              ! reopen work file
        WIC.TEMP$ = MID$(WIC.STATUS.STR$, WIC.FOUND + 2, 4) ! get last seq num
        REC.SEQ.NUM = VAL(WIC.TEMP$)             ! make it an int
        TOT.WIC.AMT = VAL(MID$(WIC.STATUS.STR$,  \
                          WIC.FOUND +  6,10))    ! get WIC amount
        TOT.WIC.DISCOUNT = VAL(MID$(WIC.STATUS.STR$, \
                          WIC.FOUND + 16,10))    ! get WIC discount

        ! update the status control string
        LEN.STR = LEN(WIC.STATUS.STR$)           ! get length of string
        WIC.STATUS.STR$ = LEFT$(WIC.STATUS.STR$, WIC.FOUND - 1) + \ build new status str
                          WIC.AGENCY$                           + \ agency
                          WIC.TEMP$                             + \ sequence number
                          RIGHT$("0000000000" + STR$(TOT.WIC.AMT),10) + \
                          RIGHT$("0000000000" + STR$(TOT.WIC.DISCOUNT),10) + \
                          RIGHT$(WIC.STATUS.STR$, LEN.STR - (WIC.FOUND + 25))
      ENDIF

      CREATE POSFILE "WIC4:WISTATUS.DAT" AS 64 MIRRORED ATCLOSE ! create recovery file
      WRITE #64; RIGHT$("0000000000" + STR$(CS.RESTART), 10) + \ save status data
               WIC.STATUS.STR$
      CLOSE 64                                   ! close file
      REC.SEQ.NUM$ = RIGHT$("000000" +           \
                            STR$(REC.SEQ.NUM), 6) !
    ENDIF
  ENDIF

  IF IDENTIFIER = 1 THEN BEGIN                   ! WIC EBT item string

!AIO01214 Do this loop separately for PurchaseQuantity, then ZeroPurchaseQuantity
    FOR CAT.ZEROCAT = 1 TO 2                     ! first Subcategory, then ZeroSubcategory
      WORK1I4 = PACKBIN2(CS.INPUT$,                \ purchase qty
                         CS.FLD.POS(5+CAT.ZEROCAT), \ PurchQty or ZeroPurchQty
                         CS.FLD.LEN(5+CAT.ZEROCAT)) !
      IF WORK1I4 > 0 THEN BEGIN                     ! something to do
!EIO01214

        ADDENDA.SEQ.NUM% = ADDENDA.SEQ.NUM% + 1      ! bump addenda rec ctr
        ADDENDA.SEQ.NUM$ = RIGHT$("000" +            \
                                STR$(ADDENDA.SEQ.NUM%), 3) !

        CAT.CODE$ = STR$(PACKBIN2(CS.INPUT$,         \
                                  CS.FLD.POS(3),     \
                                  CS.FLD.LEN(3)))    !
        CAT.CODE$ = RIGHT$("00" + CAT.CODE$, 2)

        SUB.CAT.CODE$ = STR$(PACKBIN2(CS.INPUT$,     \
                                      CS.FLD.POS(4), \
                                      CS.FLD.LEN(4))) !
        SUB.CAT.CODE$ =RIGHT$("000" + SUB.CAT.CODE$, 3)
!AIO01214 For the ZeroSubcategory, this field is always zero
        IF CAT.ZEROCAT = 2 THEN BEGIN
          SUB.CAT.CODE$ = "000"                    ! Zero subcategory
        ENDIF
\       WORK1I4 = PACKBIN2(CS.INPUT$,              \ purchase qty
\                          CS.FLD.POS(6),          \
\                          CS.FLD.LEN(6))          !
\       WORK1I4 = WORK1I4 +                        \ purchase qty +
\                 PACKBIN2(CS.INPUT$,              \ zero purchase qty
\                          CS.FLD.POS(7),          \
\                          CS.FLD.LEN(7))          !
!EIO01214
        PURCH.QTY$ = STR$(WORK1I4 * 100)
        PURCH.QTY$ = RIGHT$("00000" + PURCH.QTY$, 5)

        PURCH.UNITS$ = STR$(WORK1I4 *                \ purchase quantity
                            PACKBIN4(CS.INPUT$,      \ times units
                                     CS.FLD.POS(5),  \
                                     CS.FLD.LEN(5))) !
        PURCH.UNITS$ = RIGHT$("00000" + PURCH.UNITS$, 5)

!AIO01214 First loop = Cost, second = ZeroCost
\       WORK1I4 = PACKBIN4(CS.INPUT$,                \ cost
\                          CS.FLD.POS(8),            \
\                          CS.FLD.LEN(8))            !
\       WORK1I4 = WORK1I4 +                          \ cost +
\                 PACKBIN4(CS.INPUT$,                \ zero cost
\                          CS.FLD.POS(9),            \
\                          CS.FLD.LEN(9))            !
        WORK1I4 = PACKBIN4(CS.INPUT$,                \ Cost or ZeroCost
                           CS.FLD.POS(7+CAT.ZEROCAT), \
                           CS.FLD.LEN(7+CAT.ZEROCAT)) !
!EIO01214

        CLAIM.PRICE$ = STR$(WORK1I4)
        CLAIM.PRICE$ = RIGHT$("000000000" + CLAIM.PRICE$, 9)
        UPC$ = UNPACK$(MID$(CS.INPUT$,               \
                    CS.FLD.POS(11)+1,                \
                    CS.FLD.LEN(11)))                 !
        UPC$ = RIGHT$("00000000000000000" + UPC$, 17)

        REC.SEQ.NUM.NEXT$ = RIGHT$("000000" +           \
                                STR$(REC.SEQ.NUM+1), 6) !

        E3.REC$ = "E3"              +                \ addenda record
                  REC.SEQ.NUM.NEXT$ +                \
                  ADDENDA.SEQ.NUM$  +                \
                  CAT.CODE$         +                \
                  SUB.CAT.CODE$     +                \
                  PURCH.UNITS$      +                \
                  UPC$              +                \
                  PURCH.QTY$        +                \
                  CLAIM.PRICE$      +                \
                  CHR$(13) + CHR$(10)                !

        E3.STR$(ADDENDA.SEQ.NUM%) = E3.REC$          ! save in array
!AIO01214 First loop = Cost, second = ZeroCost
      ENDIF                                      ! PurchQty or ZeroPurchQty
    NEXT CAT.ZEROCAT
!EIO01214

  ENDIF ELSE BEGIN
    IF IDENTIFIER = 2 THEN BEGIN                 ! WIC EBT trans summ str

      REC.SEQ.NUM = REC.SEQ.NUM + 1              ! bump it
      REC.SEQ.NUM$ = RIGHT$("000000" +           \ format it
                         STR$(REC.SEQ.NUM), 6)   !
      WIC.FOUND = MATCH(WIC.AGENCY$, WIC.STATUS.STR$, 1) ! search agency ID
      IF WIC.FOUND THEN BEGIN
        ! update the status control string
        WIC.TEMP$ = WIC.AGENCY$ + RIGHT$("0000" + STR$(REC.SEQ.NUM), 4)
        LEN.STR = LEN(WIC.STATUS.STR$)           ! get length of string
        WIC.STATUS.STR$ = LEFT$(WIC.STATUS.STR$, WIC.FOUND - 1) + \ build new
                          WIC.TEMP$                             + \ status string
                          RIGHT$("0000000000" + STR$(TOT.WIC.AMT),10) + \
                          RIGHT$("0000000000" + STR$(TOT.WIC.DISCOUNT),10) + \
                          RIGHT$(WIC.STATUS.STR$, LEN.STR - (WIC.FOUND + 25))
      ENDIF

      SELF.CHKOUT = 0                            ! init

      FOR I1 = 1 TO 10                           ! is this self checkout
        IF SL.HD.TERMINAL = SELF.CHK.ARRAY(I1) THEN BEGIN
          SELF.CHKOUT = -1                       ! set flag
        ENDIF
      NEXT I1

      CALL CONSOLIDATE.ITEMS                     ! combine like WIC items in trans

      PAN.LEN$ = STR$(PACKBIN2(CS.INPUT$,        \
                      CS.FLD.POS(3),             \
                      CS.FLD.LEN(3)))            !
      PAN.LEN$ = RIGHT$("00" + PAN.LEN$, 2)
      PAN$ = UNPACK$(MID$(CS.INPUT$,             \
                     CS.FLD.POS(4)+1,            \
                     CS.FLD.LEN(4)))             !
      PAN$ = RIGHT$(STRING$(19, "0") + PAN$, 19)
      TRANS.AMT$ =  STR$(PACKBIN4(CS.INPUT$,     \
                         CS.FLD.POS(5),          \
                         CS.FLD.LEN(5)))         !
      TRANS.AMT$ = RIGHT$(STRING$(12, "0") + TRANS.AMT$, 12) !
      TOT.WIC.AMT = TOT.WIC.AMT + VAL(TRANS.AMT$) ! accumulate amount
      SYS.TRACE.NUM$ = RIGHT$("00"   + STR$(SL.HD.TERMINAL), 2) + \
                       RIGHT$("0000" + STR$(SL.HD.TRANSNUM), 4)   !
!AIO05348 This variable is set below
\     LOCAL.TRANS.DATE.TIME$ = "20"                           + \
\                              RIGHT$(STRING$(10, "0")        + \
\                              UNPACK$(SL.HD.DATETIME$), 10 ) + \
\                              "00"                             !
!EIO05348

!AIO03864 1. Start End of Month rollover logic
!Note that local.trans.date.time$ is yyyymmddhhmmss
!AIO05348 AE IO03864.  This design change removes part of IO03864 and uses the
!         new transaction date as contained in field 16 of the 80:02 record.
!         That date/time is at the point where the WIC EBT benefits
!         were read from the card.
\     WIC.END.DATE$ = UNPACK$(MID$(CS.INPUT$,            \  yyyymmdd
\                                 CS.FLD.POS(9)+1,       \  Input is packed
\                                 CS.FLD.LEN(9)))        !
\     IF MID$(WIC.END.DATE$,5,2) NE                  \ Different Month?
\        MID$(LOCAL.TRANS.DATE.TIME$,5,2) THEN BEGIN !
\          LOCAL.TRANS.DATE.TIME$ =                  \ Set back to
\             WIC.END.DATE$      +                   \ card expiration date
\             "235900"                               ! and 23:59:00
\     ENDIF
!EIO05348
!EIO03864

!AIO05348 Establish LocalXactionDateTime for the D4 record
      LOCAL.TRANS.DATE.TIME$ = UNPACK$(MID$(CS.INPUT$, \ 20yymmddhhmmss
                                  CS.FLD.POS(16)+1,    \
                                  CS.FLD.LEN(16)))     ! 7 bytes packed decimal
!EIO05348

      TERMINAL.NUM$ = RIGHT$(STRING$(8, "0") +        \
                             STR$(SL.HD.TERMINAL), 8) !
      ! find merchant ID
      FOR I1 = 1 TO NUM.AGENCIES%
        IF AGENCY.ARRAY$(I1) = WIC.AGENCY$ THEN BEGIN
          MERCHANT.ID$ = MERCHANT.ID.ARRAY$(I1)
          I1 = NUM.AGENCIES% + 1
        ENDIF
      NEXT I1
      ! strip leading blanks
      I1 = MATCH(" ", MERCHANT.ID$, 1)

      WHILE I1 = 1
        J1 = LEN(MERCHANT.ID$)
        MERCHANT.ID$ = RIGHT$(MERCHANT.ID$, J1 -1)
        I1 = MATCH(" ", MERCHANT.ID$, 1)
      WEND

      MERCHANT.ID$ = LEFT$(MERCHANT.ID$ +        \
                           STRING$(15, " "), 15) !
      ITEM.CNT$ = RIGHT$("000" + STR$(ADDENDA.SEQ.NUM%), 3)
      BENEFIT.ISSUING.ID$ =  MID$(CS.INPUT$,       \
                                  CS.FLD.POS(7)+1, \
                                  CS.FLD.LEN(7) )  !
      BENEFIT.ISSUING.ID$ = LEFT$(BENEFIT.ISSUING.ID$ + \
                                  STRING$(15, " "), 15) !
      START.DATE$ = STR$(PACKBIN4(CS.INPUT$,      \
                                  CS.FLD.POS(8),  \
                                  CS.FLD.LEN(8))) !
      START.DATE$ = "20" + RIGHT$("000000" + START.DATE$, 6)
      DISCOUNT.AMT$ = STR$(PACKBIN4(CS.INPUT$,       \
                                    CS.FLD.POS(10),  \
                                    CS.FLD.LEN(10))) !
      DISCOUNT.AMT$ = RIGHT$(STRING$(12, "0") +  \
                              DISCOUNT.AMT$, 12) !
      TOT.WIC.DISCOUNT = TOT.WIC.DISCOUNT +      \ accumulate amount
                         VAL(DISCOUNT.AMT$)      !
      ICC.SYS.DATA$ = MID$(CS.INPUT$,            \
                           CS.FLD.POS(11)+1,     \
                           CS.FLD.LEN(11))       !
      WIC.TEMP$ = ""
      IF CS.FLD.LEN(11) = 24 THEN BEGIN
        WIC.TEMP$ = "810100"                  +  \
                    "8204"                    +  \
                    LEFT$(ICC.SYS.DATA$, 8)   +  \
                    "8304"                    +  \
                    MID$(ICC.SYS.DATA$, 9, 8) +  \
                    "8404"                    +  \
                    RIGHT$(ICC.SYS.DATA$, 8)     !
      ENDIF
      ICC.SYS.DATA$ = LEFT$(WIC.TEMP$ +           \
                            STRING$(99, " "), 99) !
      D4.REC$ = "D4"              +              \ detail record
                REC.SEQ.NUM$      +              \ sequence number
                "0220"            +              \ constant
                PAN.LEN$          +              \ pan length
                PAN$              +              \ pan
                "009700"          +              \ constant
                TRANS.AMT$        +              \ transaction amt
                SYS.TRACE.NUM$    +              \ unique number for transaction
                STRING$(10, "0")  +              \ constant
                LOCAL.TRANS.DATE.TIME$           ! local transaction date/time

      IF SELF.CHKOUT THEN BEGIN                  ! if self checkout use
        D4.REC$ = D4.REC$ + "510211111334"       ! this constant
      ENDIF ELSE BEGIN                           ! else use
        D4.REC$ = D4.REC$ + "510111111334"       ! this constant
      ENDIF

      D4.REC$ = D4.REC$             +            \
                ACQUIRING.INST.ID$  +            \ store number
                TERMINAL.NUM$       +            \ terminal ID
                MERCHANT.ID$        +            \ merchant ID
                STORE.NAME.LOC$     +            \ store name and loc
                ITEM.CNT$           +            \ number of items
                BENEFIT.ISSUING.ID$ +            \
                START.DATE$         +            \ benefits start date
                DISCOUNT.AMT$       +            \ discount amt
                ICC.SYS.DATA$       +            \
                CHR$(13) + CHR$(10)              ! crlf

      WRITE FORM "C365"; #5; D4.REC$             ! write detail rec

      FOR J = 1 TO ADDENDA.SEQ.NUM%
        FORMAT.STR$ = "C" + STR$(LEN(E3.STR$(J)))
        WRITE FORM FORMAT.STR$; #5; E3.STR$(J)   ! write addenda recs
      NEXT J

      DIM E3.STR$(0)                             ! recover some space
      CLOSE 5
      ADDENDA.SEQ.NUM% = 0                       ! reset counter

      ! the AMT or DISCOUNT may have changed.  Update WIC.STATUS.STR$
      WIC.FOUND = MATCH(WIC.AGENCY$, WIC.STATUS.STR$, 1) ! search agency ID
      IF WIC.FOUND THEN BEGIN
        ! update the status control string
        WIC.TEMP$ = WIC.AGENCY$ + RIGHT$("0000" + STR$(REC.SEQ.NUM), 4)
        LEN.STR = LEN(WIC.STATUS.STR$)           ! get length of string
        WIC.STATUS.STR$ = LEFT$(WIC.STATUS.STR$, WIC.FOUND - 1) + \ build new
                          WIC.TEMP$                             + \ status string
                          RIGHT$("0000000000" + STR$(TOT.WIC.AMT),10) + \
                          RIGHT$("0000000000" + STR$(TOT.WIC.DISCOUNT),10) + \
                          RIGHT$(WIC.STATUS.STR$, LEN.STR - (WIC.FOUND + 25))
        CREATE POSFILE "WIC4:WISTATUS.DAT" AS 64 MIRRORED ATCLOSE ! create recovery file
        WRITE #64; RIGHT$("0000000000" + STR$(CS.RESTART), 10) + \ save status data
               WIC.STATUS.STR$
        CLOSE 64                                 ! close file
      ENDIF

    ENDIF ELSE BEGIN                             ! error
      IF IDENTIFIER = 3 THEN BEGIN               ! exception string

        ICC.CODE$ = STR$(PACKBIN2(CS.INPUT$,     \ error indicator
                        CS.FLD.POS(3),           \
                        CS.FLD.LEN(3)))          !
        PAN$ = UNPACK$(MID$(CS.INPUT$,           \
                       CS.FLD.POS(4)+1,          \
                       CS.FLD.LEN(4)))           !
        PAN$ = RIGHT$(STRING$(19, "0") + PAN$, 19)
        PAN.LEN$ = STR$(PACKBIN2(CS.INPUT$,      \
                        CS.FLD.POS(5),           \
                        CS.FLD.LEN(5)))          !
        PAN.LEN$ = RIGHT$("00" + PAN.LEN$, 2)
        WE.ERR.CODE$ = MID$(CS.INPUT$,           \ error code
                            CS.FLD.POS(6)+1,     \
                            CS.FLD.LEN(6) )      !


        IF VAL(ICC.CODE$) > 0 THEN BEGIN         ! log to claim file

          REC.SEQ.NUM = REC.SEQ.NUM + 1          ! bump it
          REC.SEQ.NUM$ = RIGHT$("000000" +       \ format it
                           STR$(REC.SEQ.NUM), 6) !
          WIC.FOUND = MATCH(WIC.AGENCY$, WIC.STATUS.STR$, 1) ! search agency ID
          IF WIC.FOUND THEN BEGIN
            ! update the status control string
            WIC.TEMP$ = WIC.AGENCY$ + RIGHT$("0000" + STR$(REC.SEQ.NUM), 4)
            LEN.STR = LEN(WIC.STATUS.STR$)           ! get length of string
            WIC.STATUS.STR$ = LEFT$(WIC.STATUS.STR$, WIC.FOUND - 1) + \ build new
                              WIC.TEMP$                             + \ status string
                              RIGHT$("0000000000" + STR$(TOT.WIC.AMT),10) + \
                              RIGHT$("0000000000" + STR$(TOT.WIC.DISCOUNT),10) + \
                              RIGHT$(WIC.STATUS.STR$, LEN.STR - (WIC.FOUND + 25))
!AIO04597 Make sure the status file is updated with new sequence number
!         Make sure session 64 is free first, base mainline should just resume
!         if file is not open.
            CLOSE 64                             ! make sure session free
            CREATE POSFILE "WIC4:WISTATUS.DAT" AS 64 MIRRORED ATCLOSE ! create recovery file
            WRITE #64; RIGHT$("0000000000" + STR$(CS.RESTART), 10) +  \ save status data
                   WIC.STATUS.STR$
            CLOSE 64                             ! close file
!EIO04597
          ENDIF

          TRANS.AMT$ = STRING$(12, "0")          ! create empty fields
          SYS.TRACE.NUM$ = RIGHT$("00"   + STR$(SL.HD.TERMINAL), 2) + \
                           RIGHT$("0000" + STR$(SL.HD.TRANSNUM), 4)   !
          LOCAL.TRANS.DATE.TIME$ = "20"                           + \
                                   RIGHT$(STRING$(10, "0")        + \
                                   UNPACK$(SL.HD.DATETIME$), 10 ) + \
                                   "00"                             !
          TERMINAL.NUM$ = RIGHT$(STRING$(8, "0") +        \
                                 STR$(SL.HD.TERMINAL), 8) !
          ! find merchant ID
          FOR I1 = 1 TO NUM.AGENCIES%
            IF AGENCY.ARRAY$(I1) = WIC.AGENCY$ THEN BEGIN
              MERCHANT.ID$ = MERCHANT.ID.ARRAY$(I1)
              I1 = NUM.AGENCIES% + 1
            ENDIF
          NEXT I1
          ! strip leading blanks
          I1 = MATCH(" ", MERCHANT.ID$, 1)

          WHILE I1 = 1
            J1 = LEN(MERCHANT.ID$)
            MERCHANT.ID$ = RIGHT$(MERCHANT.ID$, J1 -1)
            I1 = MATCH(" ", MERCHANT.ID$, 1)
          WEND

          MERCHANT.ID$ = LEFT$(MERCHANT.ID$ +    \
                         STRING$(15, " "), 15)   !

          ITEM.CNT$ = "000"
          BENEFIT.ISSUING.ID$ = STRING$(15, "0") !
          START.DATE$ = "00000000"
          DISCOUNT.AMT$ = STRING$(12, "0")
          ICC.SYS.DATA$ = "8101" + RIGHT$("0" + ICC.CODE$, 2)
          ICC.SYS.DATA$ = LEFT$(ICC.SYS.DATA$ +       \
                                STRING$(99, " "), 99) !

          D4.REC$ = "D4"             +           \ detail record
                    REC.SEQ.NUM$     +           \ sequence number
                    "0220"           +           \ constant
                    PAN.LEN$         +           \ pan length
                    PAN$             +           \ pan
                    "009700"         +           \ constant
                    TRANS.AMT$       +           \ transaction amt
                    SYS.TRACE.NUM$   +           \ unique number for transaction
                    STRING$(10, "0") +           \ constant
                    LOCAL.TRANS.DATE.TIME$       ! local transaction date/time

          IF SELF.CHKOUT THEN BEGIN              ! if self checkout use
            D4.REC$ = D4.REC$ + "510211111334"   ! this constant
          ENDIF ELSE BEGIN                       ! else use
            D4.REC$ = D4.REC$ + "510111111334"   ! this constant
          ENDIF

          D4.REC$ = D4.REC$             +        \
                    ACQUIRING.INST.ID$  +        \ store number
                    TERMINAL.NUM$       +        \ terminal ID
                    MERCHANT.ID$        +        \ merchant ID
                    STORE.NAME.LOC$     +        \ store name and loc
                    ITEM.CNT$           +        \ number of items
                    BENEFIT.ISSUING.ID$ +        \
                    START.DATE$         +        \ benefits start date
                    DISCOUNT.AMT$       +        \ discount amt
                    ICC.SYS.DATA$       +        \
                    CHR$(13) + CHR$(10)          ! crlf

          WRITE FORM "C365"; #5; D4.REC$         ! write detail rec

!AIO07522 Establish variables common for all User Exit 11 calls
\         XL.CODE$ = "82"                        ! setup for logging exception
          XL.CODE$ = PACK$("82")                 ! setup for logging exception
          XL.TERMINAL$ = SL.HD.TERMINAL$         ! terminal number
          XL.TRANSNUM$ = PACK$(WIC.TEMP$)        ! transaction number
          XL.DATETIME$ = SL.HD.DATETIME$         ! Date & Time of request
          XL.OPERATOR$ = SL.HD.OPERATOR$         ! Operator Number
!EIO07522

          XL.USEREXIT$ = ""

          IF SO.USEREXIT(11) THEN BEGIN
            CALL CSMLEC02(5055H)
            CALL CSUPEC11                        ! user exit
            CALL CSMLEC02(5257H)                 ! trace module return
          ENDIF

          WIC.TEMP$ = RIGHT$("0000" + STR$(SL.HD.TRANSNUM), 4)
          WRITE  # 21;                           \
\AIO07522 Log the variables possibly changed by the user exit
\                      SL.HD.TERMINAL$,          \ Terml Number
\                      PACK$(WIC.TEMP$),         \ Transaction Number
\                      SL.HD.DATETIME$,          \ Date & Time of request
\                      PACK$(XL.CODE$),          \ Code = 82
\                      SL.HD.OPERATOR$,          \ Operator Number
\
                       XL.TERMINAL$,             \ Terml Number
                       XL.TRANSNUM$,             \ Transaction Number
                       XL.DATETIME$,             \ Date & Time of request
                       XL.CODE$,                 \ Code = 82
                       XL.OPERATOR$,             \ Operator Number
\EIO07522
                       PACK$("00"),              \ constant identifier
                       WIC.AGENCY$,              \ Agency ID
                       PACK$("0" + PAN$),        \ PAN
                       PACK$(PAN.LEN$),          \ PAN length
\AIO07522 Also log the user exit code
\                      WE.ERR.CODE$              ! error RC from smart card dvc
                       WE.ERR.CODE$,             \ error RC from smart card dvc
                       XL.USEREXIT$              ! User Exit string
\EIO07522
        ENDIF ELSE BEGIN
          ! a record was not written to claims file
          ! nothing special to do here
        ENDIF
        CLOSE 5
      ENDIF
    ENDIF
  ENDIF
END SUB


