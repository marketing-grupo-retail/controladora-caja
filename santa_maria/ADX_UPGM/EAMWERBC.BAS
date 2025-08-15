\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMWERBC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: CONVERT WIC EBT SOURCE FILES          */
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
\/*      FUNCTION: EAMWERBC                                      */
\/*                                                              */
\/*                THIS MODULE RUNS IN THE BACKGROUND AND        */
\/*                CHECKS FOR NEW HOST WIC EBT SOURCE FILES,     */
\/*                IT THEN EXTRACTS DATA TO BE USED BY SA        */
\/*                                                              */
\/*                                                              */
\/*         DEPENDENCIES:                                        */
\/*                                                              */
\/*                NONE                                          */
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
\/*      ENTRY POINT:                                            */
\/*                                                              */
\/*         None runs from background                            */
\/*                                                              */
\/*                                                              */
\/*         SYMBOLIC NAME: EAMWEOPT                              */
\/*                                                              */
\/*         DESCRIPTION: WIC EBT OPTIONS                         */
\/*                                                              */
\/*                THIS FILE CONTAINS STORE WIDE OPTIONS DATA    */
\/*                                                              */
\/*                ***** FILE INPUT *****                        */
\/*                                                              */
\/*      OUTPUT:                                                 */
\/*         Creates files:                                       */
\/*         EAMWEaa.ttt where aa=agency                          */
\/*         ttt = HCL APL DES ???                                */
\/*                                                              */
\/*                                                              */
\/*                                                              */
\/*                                                              */
\/*                                                              */
\/*      EXIT-NORMAL: RETURN TO CALLER                           */
\/*                                                              */
\/*         RETURN CODE:                                         */
\/*                                                              */
\/*      EXIT-ERROR:                                             */
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
\/*           NAME:                                              */
\/*                                                              */
\/*         DATA AREAS:                                          */
\/*                                                              */
\/*           MODIFIED:                                          */
\/*                                                              */
\/*           REFERENCED:                                        */
\/*                                                              */
\/*             EAMWEOPT: WIC EBT OPTIONS                        */
\/*                                                              */
\/*      CHANGE ACTIVITY: LEVEL 0                                */
\/*                                                              */
\/* IO00563 - Use the right two digits of the A00 or E00 file    */
\/*           extension as part of the file name when creating   */
\/*           an ABD or EBD file.  Also, if an ASV file exists,  */
\/*           delete it before trying to rename to it.  Also,    */
\/*           ensure that the file name passed to BAD.FILE is    */
\/*           in 8.3 form.                                       */
\/*           GGK IBM 09Mar2005                                  */
\/*                                                              */
\/* IO00633 - An attempt to process a zero-byte AAyddd.A00 file  */
\/*           fails, but does not rename the file to *.ABD and   */
\/*           does not log an error.                             */
\/*           GGK IBM 24Mar2005                                  */
\/*                                                              */
\/* IO09131 - The format of incoming SSyjjj.APL files is not     */
\/*           always as expected.  TX and NM sort subcategory    */
\/*           entries by UPC, with subcategory 000 always        */
\/*           adjacent to and immediately preceding other        */
\/*           subcategory records for every given UPC.           */
\/*           WY provides them in ANY order, i.e. items with the */
\/*           same UPC are not adjacent in the file.             */
\/*           GGK IBM 27Jun2008                                  */
\/*                                                              */
\/* END-OF-SPECIFICATIONS ****************************************/


%INCLUDE EAMWICEF.J86

  STRING GLOBAL             \
    LOG.ENTRY$(1),          \
    FILE.LIST$(1),          \
    SAVE.CREATE.DATE$,      \
    SAVE.CREATE.TIME$,      \
    SETTLE.DATE$,           \
    WORK.STR$,              \ input file string
    SEQ.NUM$,               \
    AGENCY.ID$,             \ wic agency
    BKUP.FILE$,             \ backup file
    SRC.FILE$,              \ input file
    OUTFILE$,               \ output file
    OUTFILE2$,              \
    WRK.FILE$,              \ temp work file
    WRK.FILE2$,             \
    REC.TYPE$,              \
    CNTRLR.ID$,             \
    DES.KEYS.ARRAY$(1),     \
    T$,                     \ time
    H$,                     \ hour
    M$,                     \ min
    MIN$,                   \ saved minute
    WAIT.TIME$,             \
    WORK4$,                 \
    WIC.ERR.MSG$,           \
    TYPE.FILE$              !

  INTEGER*4 GLOBAL          \
     WE.OFFSET,             \
     DES.KEYS.CNT(1),       \
     NUM.RECS,              \
     REST,                  \
     RETC                   !

  INTEGER*2                 \
    INDEX

  INTEGER*1 GLOBAL          \
     NUM.ENTRIES,           \
     PROCESSING.APL,        \
     OPEN.FAILED,           \
     FCNT,                  \
     CREATE.WERCL,             \
     CHECKING.OUTPUT.FILE,  \
     HANDLE.FR20,           \ Handle FR 20 errors. IO00625
     REC.NOT.FOUND          !

\ REM!!********************* ADXFILES *****************************
\ REM!!          application services function
\ REM!! This routine is used to determine whether despooling is active
\ REM!! and so close period should not be allowed.
\ REM!! It is also used to modify file restriction when forcing a
\ REM!! rename on the transactions summary log.
\ REM!!************************************************************

SUB ADXFILES(RET,FUNC,PARM1,PARM2) EXTERNAL
  INTEGER*4 RET      !Return code
  INTEGER*2 FUNC,\   !1=Restrict 2=Unrestrict  3=check despooling
         PARM1
  STRING PARM2
END SUB


SUB ADXCOPYF(RETURN.CODE, INPUT.FILE, OUTPUT.FILE,       \
             OPTION1, OPTION2) EXTERNAL                  !
  INTEGER*4 RETURN.CODE                                  !
  INTEGER*2 OPTION1, OPTION2                             !
  STRING    INPUT.FILE, OUTPUT.FILE                      !
END SUB

SUB ADXSERVE(RET,FUNC,PARM1,PARM2$) EXTERNAL
  INTEGER*4     RET
  INTEGER*2     FUNC,PARM1
  STRING        PARM2$
END SUB

\ REM!!====================> ADXERROR <===============================!!
\ REM!! This function makes entries in the system log and optionally  !!
\ REM!! display system messages on OCF screen.                        !!
\ REM!!===============================================================!!

FUNCTION ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE) EXTERNAL
  INTEGER*2 TERM,MSGNUM
  INTEGER*1 SEVERITY,MSGGRP,EVENT
  STRING    UNIQUE
END FUNCTION

%INCLUDE BASROUT.J86
%INCLUDE SYSFUNC.J86

\ REM!!======================> MMCFEC13 <=============================!!
\ REM!! This subprogram writes the passed string to the exception log !!
\ REM!! file.                                                         !!
\ REM!!===============================================================!!
\  This will log exception entries in the following format:
\      Controller ID
\      Terminal num always zero
\      Date and time
\      Log entry i.d. always 82
\      Operator always zero
\      Subtype  always 09
\      Filename if app.
\      Record count if app.
\      Error code
\      are defined:
\       "00"    No error
\       "01"    D4 or DA Rec Cnt incorrect
\       "02"    Settlement not same as claim
\       "03"    A1 time not same as Z1
\       "04"    A1 date not same as Z1
\       "05"    Error file received
\       "06"    no claim record found
\       "07"    The FileFormatVersion is not '02'
\       "08"    Z1 record count is incorrect
\
\
\
\

SUB MMCFEC13 PUBLIC

  STRING    ID.NUM$,                             \ Steamboat ID number
            FIELD$,                              \ Field to be written to log
            DEL$,                                \ Delimiter between fields
            UNIQ$                                ! string to log error

  INTEGER*2 I,                                   \ Loop control
            START.ENTRY,                         \ Start point of money amounts
            ADD.TOTAL                            ! Flag if total required

  INTEGER*4 TOTAL                                ! Running tot for accnting trans.

  DEL$    = CHR$(34) + CHR$(44) + CHR$(34)

  IF NUM.ENTRIES < 1 THEN GOTO EXIT13



  FIELD$  = CNTRLR.ID$                    + DEL$+ \ Controller ID
            PACK$("0000")                 + DEL$+ \ Terminal num always zero
            PACK$(LEFT$(DATE$ + TIME$,10))+ DEL$+ \ Date and time
            PACK$("82")                   + DEL$+ \ Log type always 82
            PACK$("0000000000")           + DEL$+ \ Operator always zero
            PACK$("09")                   + DEL$+ \ Subtype
            LOG.ENTRY$(2)                 + DEL$+ \ Filename if app.
            LOG.ENTRY$(3)                 + DEL$+ \ Record count if app.
            LOG.ENTRY$(4)                         ! Error code


  ON ERROR GOTO ERROR13

  OPEN "EAMEXCPT" AS 21 BUFF 4 APPEND            ! Open file & write record
  WRITE #21;FIELD$
  CLOSE 21                                       ! Close file

  EXIT13:

  EXIT SUB

  ERROR13:

     UNIQ$     = "CF21" + ERR + ERRNHEX$         !  set up the unique section
                                                 ! log the error
     I = ADXERROR(0,ASC("B"),211,3,0,UNIQ$)

  END SUB

!******************************************************************************
! Subroutine Name: SORT.FOUND.FILES
! In case the files were added to the directory in an odd order, we need to
! process them in time sequence.  Before sorting, discard files that do
! not have ??YJJJ.??? format.
!******************************************************************************
SUB SORT.FOUND.FILES  PUBLIC
STRING WORK.FILE.LIST$
Integer*2 II%, JJ%

 ! characters 3..6 must be numneric
 FOR II% = 1 TO (FCNT -1)
   FOR JJ% = 3 TO 6
     IF (MID$(FILE.LIST$(II%),JJ%,1) < "0") OR         \ is it numeric?
        (MID$(FILE.LIST$(II%),JJ%,1) > "9") THEN BEGIN ! no, discard name
       FCNT = FCNT - 1                                 ! one less to process
       FILE.LIST$(II%) = FILE.LIST$(FCNT)              ! overlay this name
       FILE.LIST$(FCNT) = ""                           ! null last entry
       II% = II% - 1                                   ! reprocess this entry
       JJ% = 9                                         ! exit inner loop
     ENDIF
   NEXT JJ%
   IF JJ% = 7 THEN BEGIN                               ! if all are digits
     JJ% = VAL(MID$(FILE.LIST$(II%),4,3))              ! JJJ part
     IF (JJ% <= 0) OR (JJ% > 366) THEN BEGIN           ! must be 001..366
       FCNT = FCNT - 1                                 ! one less to process
       FILE.LIST$(II%) = FILE.LIST$(FCNT)              ! overlay this name
       FILE.LIST$(FCNT) = ""                           ! null last entry
       II% = II% - 1                                   ! reprocess this entry
     ENDIF
   ENDIF
 NEXT II%

 ! now sort the remaining file list
 FOR II% = 1 TO (FCNT -1)

   WORK.FILE.LIST$ = FILE.LIST$(II%)
   JJ% = II%
   WHILE JJ% > 1 AND MID$(FILE.LIST$(JJ%-1),3,4) > MID$(WORK.FILE.LIST$,3,4)
     FILE.LIST$(JJ%) = FILE.LIST$(JJ%-1)
     JJ% = JJ% -1
   WEND

   FILE.LIST$(JJ%) = WORK.FILE.LIST$

 NEXT II%

END SUB

!******************************************************************************

!******************************************************************************
! Subroutine Name: SET.WAIT.TIME
! Set up delay time between checks for new files. Default to 10 minutes
! Take parameter from background definition in minutes
! 999 means special case of 30 seconds for testing
!******************************************************************************
SUB SET.WAIT.TIME  PUBLIC

  WAIT.TIME$ = COMMAND$                           ! wait time, in minutes

  IF LEFT$(WAIT.TIME$, 8) = "BACKGRND" THEN BEGIN ! If started by RCP
     REST = 600000                                !  set default
  ENDIF ELSE BEGIN
    IF WAIT.TIME$ < "10" OR                       \ no command tail
       WAIT.TIME$ > "60" THEN BEGIN               ! more than a hour
      REST = 600000                               ! 10 minutes
    ENDIF ELSE BEGIN
      REST = VAL(WAIT.TIME$)*60000                !to milliseconds in minute
    ENDIF

    IF WAIT.TIME$ = "999" THEN REST = 30000     ! Special test mode 1 minute
  ENDIF
END SUB

!******************************************************************************


!******************************************************************************
! Subroutine Name: PARSE.AGENCY.DATA
! Description: This routine parses the agency record data into work variables.
!******************************************************************************
SUB PARSE.AGENCY.DATA(CURR.REC) PUBLIC

  INTEGER*2                                      \
            CURR.REC                             ! record into data array

  STRING A$

  A$ = AGENCY.MAIN.OPTION$(CURR.REC)             ! put data into work variable

  STATE.ID$ = LEFT$(A$,2)
  AGENCY.ARRAY$(CURR.REC) = STATE.ID$

END SUB



!******************************************************************************
! Subroutine Name: OPEN.READ.OPTION.FILE
! Description: This routine opens then reads the option file, EAMWEOPT.DAT
!******************************************************************************
SUB OPEN.READ.OPTION.FILE PUBLIC

  STRING              \
    DUMMY$

  INTEGER*2 I%

  MAX.AGENCIES = 10                               ! set a limit
  EAMWEOPT.SN = 1                                 ! session # for EAMWEOPT.DAT
  EAMWEOPT.RECL = 1024                            ! record len for EAMWEOPT.DAT
  CONTROL.FMT$ = "2C1,C120,2C1,3C255,C135"
  AGENCY.FMT$ = "C33,C240,C40,C120,C240,C120,C80,C151"

  OPEN "EAMWEOPT" DIRECT RECL EAMWEOPT.RECL AS EAMWEOPT.SN BUFFSIZE 1024

  ! Read the control record
  READ FORM CONTROL.FMT$; #EAMWEOPT.SN,100;      \
         NUM.AGENCIES$,                          \
         WIC.TENDER$,                            \
         DUMMY$,                                 \
         DAYLIGHT.SAVING$,                       \
         GMT.DIFF$,                              \
         DUMMY$,                                 \
         DUMMY$,                                 \
         DUMMY$,                                 \
         DUMMY$                                  !

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
  DIM AGENCY.MAIN.OPTION$(NUM.AGENCIES%)

  ! Now read all the agency records
  FOR I% = 1 TO NUM.AGENCIES%
    READ FORM AGENCY.FMT$; #EAMWEOPT.SN,I%;      \
                  AGENCY.MAIN.OPTION$(I%),       \
                  DUMMY$,                        \
                  DUMMY$,                        \
                  DUMMY$,                        \
                  DUMMY$,                        \
                  DUMMY$,                        \
                  DUMMY$,                        \
                  DUMMY$                         ! reserved
  NEXT I%

  DUMMY$ = ""                                    ! cleanup
  CLOSE EAMWEOPT.SN

  DIM AGENCY.ARRAY$(NUM.AGENCIES%)

  FOR INDEX = 1 TO NUM.AGENCIES%
    CALL PARSE.AGENCY.DATA(INDEX)
  NEXT INDEX

END SUB                                          ! end OPEN.READ.OPTION.FILE
!*****************************************************************************
! SETUP.FILES
!
! This subroutine performs the common file setup functions for the
! file that is about to be processed.
!
! INPUT:
!   EXT$  file extension
!           Valid extensions are:
!                APL            UPC list
!                HCL            hot card list
!                A##            reconciliation file
!                E##            claim error file
!
!*****************************************************************************

SUB SETUP.FILES(EXT$) PUBLIC
  STRING                                         \
    EXT$                                         ! file extension


  INTEGER*4                                      \
    RC                                           ! adxcopyf return code
  OPEN.FAILED = 0                                !
  OPEN "C:/RECOVERY.DAT" RECL 36 AS 7            ! open it
  IF OPEN.FAILED THEN BEGIN
    CREATE "C:/RECOVERY.DAT" RECL 36 AS 7        ! add 4 for crlf and ""
    WORK4$ = STRING$(32, " ")                    ! empty string
    WRITE #7, 1; WORK4$
  ENDIF

  EXT$ = UCASE$(EXT$)
  IF (EXT$ = "APL") OR                           \ upc list
     (EXT$ = "HCL") THEN BEGIN                   ! hot card list
    T$ = TIME$
    H$ = LEFT$(T$, 2)
    M$ = MID$(T$, 3, 2)
    WORK4$ = "Processing " + FILENAME$ + " " + H$ + ":" + M$
    CALL ADXSERVE(RC, 26, 0, WORK4$)             ! display BG msg
    WAIT;2000
    BKUP.FILE$ = LEFT$(FILENAME$, 8) + "SV"      ! build backup file name
    BKUP.FILE$ = "WIC1:" + BKUP.FILE$
    SRC.FILE$ = "WIC1:" + FILENAME$              ! input file
    CALL ADXCOPYF(RC, SRC.FILE$, BKUP.FILE$,     \
                  0, 0)                          ! create backup file

    AGENCY.ID$ = LEFT$(FILENAME$, 2)             ! 2 char state abbrev
    OUTFILE$ = "WIC1:EAMWE" +                    \ output file name
               AGENCY.ID$ + "." + EXT$

    IF PROCESSING.APL THEN BEGIN                 ! need extra file for APL
      OUTFILE2$ = "WIC1:EAMWE" +                 \ output file name
                  AGENCY.ID$ + ".DES"
      WRK.FILE2$ = "WIC1:EAMWE" +                \ temp work file
                  AGENCY.ID$ + ".DRK"
    ENDIF

    WRK.FILE$ = "WIC1:EAMWE" +                   \ temp work file
                AGENCY.ID$ + ".WRK"

    OPEN SRC.FILE$ AS 5 BUFFSIZE 8192 LOCKED     ! source file


    NUM.RECS = SIZE(SRC.FILE$)
    IF EXT$ = "APL" THEN BEGIN
      NUM.RECS = (NUM.RECS/300)*2
      CREATE POSFILE WRK.FILE$ KEYED 7,,,NUM.RECS+5 RECL 39 AS 6   \ apl output file
         MIRRORED ATCLOSE
      CLOSE 6
      OPEN WRK.FILE$ KEYED RECL 39 AS 6
      CREATE POSFILE WRK.FILE2$ KEYED 3,,,NUM.RECS/10+5 RECL 46 AS 12 \ des output file
         MIRRORED ATCLOSE
      CLOSE 12
      OPEN WRK.FILE2$ KEYED RECL 46 AS 12
      DIM DES.KEYS.ARRAY$(NUM.RECS/5+5)          ! create control arrays to
      DIM DES.KEYS.CNT(NUM.RECS/5+5)             !  reduce the number of writes
    ENDIF ELSE BEGIN
      IF EXT$ = "HCL" THEN BEGIN
        NUM.RECS = (NUM.RECS/60)*2
        CREATE POSFILE WRK.FILE$ KEYED 10,,,NUM.RECS+5 RECL 22 AS 6 \ output file
          MIRRORED ATCLOSE
        CLOSE 6
        OPEN WRK.FILE$ KEYED RECL 22 AS 6
      ENDIF
    ENDIF

    BKUP.FILE$ = ""                              ! no need to save
  ENDIF ELSE BEGIN                               ! not apl or hcl
    IF (LEFT$(EXT$, 1) = "A") OR                 \
       (LEFT$(EXT$, 1) = "E") THEN BEGIN
      CREATE.WERCL = 0
      OPEN "WIC1:EAMWERCL.DAT" KEYED RECL 35 AS 8! output file
      IF CREATE.WERCL THEN BEGIN
        CREATE POSFILE "WIC1:EAMWERCL.DAT" KEYED 8,,,5000 \
        RECL 35 AS 8 MIRRORED PERUPDATE
      ENDIF
      SRC.FILE$ = "WIC1:" + FILENAME$            ! input file
      OPEN SRC.FILE$ AS 5 BUFFSIZE 8192 LOCKED   ! source file
    ENDIF
  ENDIF
END SUB

!****************************************************************************
! SUB LOG.EXCEPT
!
! This subroutine logs type 82, WIC EBT File processing, exceptions to the
! exception log. Exceptions are logged from many places in the code. This
! subroutine consolidates all of the common code.
!****************************************************************************

SUB LOG.EXCEPT PUBLIC
  LOG.ENTRY$(2) = RIGHT$("          " + FILENAME$, 10)
  CALL MMCFEC13
END SUB

!****************************************************************************
! SUB REPORT.COMPLETE
!
! This subroutine puts up a message on the background screen indicating that
! processing is complete
!****************************************************************************

SUB REPORT.COMPLETE PUBLIC

  INTEGER* 4       \
    RET

  T$ = TIME$                                     ! get time for display
  H$ = LEFT$(T$, 2)
  M$ = MID$(T$, 3, 2)
  WORK4$ = "Processing " + FILENAME$ + " complete " + H$ + ":" + M$
  CALL ADXSERVE(RET, 26, 0, WORK4$)              ! display BG msg
  WAIT;2000
END SUB

!******************************************************************************
! SUB CLOSE.RECOVERY
!
! This subroutine clears out the recovery file and sets a flag indicating
! that it has been closed
!*****************************************************************************

SUB CLOSE.RECOVERY PUBLIC
  WORK4$ = STRING$(32, " ")                      ! empty string
  WRITE #7, 1; WORK4$                            ! clear out recovry record
  CLOSE 7                                        ! close the file
END SUB

!******************************************************************************
! SUB REPLACE.NEW.FILE
!
! If a new apl/hcl file has been successfully processed, this subroutine
! performs the file manipulation required to replace the old apl/hcl file
! with the new one. It then gets rid of files that are no longer required.
!******************************************************************************

SUB REPLACE.NEW.FILE PUBLIC

  INTEGER*2              \
    PARM1,               \ used by adxfiles
    L                    ! rename return code

  INTEGER*4              \
    FILE.SIZE,            \
    RET                  ! used by adxfiles
  OUTFILE$ = "ADXLXACN::" + OUTFILE$             ! use file server copy
  CHECKING.OUTPUT.FILE = -1                      ! flag for on error
  FILE.SIZE = SIZE(OUTFILE$)                     ! get size of file
  CHECKING.OUTPUT.FILE = 0                       ! reset Flag for on error
  CALL ADXFILES (RET,1,PARM1,OUTFILE$)           ! lock everyone out
  IF FILE.SIZE > 0 THEN BEGIN                    ! if file exist
    OPEN OUTFILE$ AS 6                           ! open the old file
    DELETE 6                                     ! delete it
    L = RENAME(OUTFILE$, WRK.FILE$)              ! create an updated file
  ENDIF ELSE BEGIN                               ! else
    L = RENAME(OUTFILE$, WRK.FILE$)              ! rename work file
  ENDIF
  CALL ADXFILES (RET,2,PARM1,OUTFILE$)           ! make it available

  IF PROCESSING.APL THEN BEGIN
    OUTFILE2$ = "ADXLXACN::" + OUTFILE2$         ! use file server copy
    CHECKING.OUTPUT.FILE = -1                    ! flag for on error
    FILE.SIZE = SIZE(OUTFILE2$)                  ! get size of file
    CHECKING.OUTPUT.FILE = 0                     ! reset Flag for on error
    CALL ADXFILES (RET,1,PARM1,OUTFILE2$)        ! lock everyone out
    IF FILE.SIZE > 0 THEN BEGIN                  ! if file exist
      OPEN OUTFILE2$ AS 12                       ! open the old file
      DELETE 12                                  ! delete it
    ENDIF
    L = RENAME(OUTFILE2$, WRK.FILE2$)            ! rename work file
    CALL ADXFILES (RET,2,PARM1,OUTFILE2$)        ! make it available
  ENDIF

  IF L THEN BEGIN                                ! if rename worked

    CALL REPORT.COMPLETE
    CALL CLOSE.RECOVERY
    DELETE 5                                     ! delete source
    WORK4$ = STR$(REC.CNT + 2)
    LOG.ENTRY$(3) = WORK4$
    LOG.ENTRY$(4) = "0000"
    CALL LOG.EXCEPT
  ENDIF
END SUB

!******************************************************************************
! SUB BAD.FILE
!
! This subroutine accepts a file extension, from the calling routine, and
! checks to see if the file already exists. If it does exist, it is deleted
! and a new one is created with the new bad data.
!******************************************************************************

SUB BAD.FILE(EXT$) PUBLIC

  STRING                  \
    EXT$                  ! extension to be used in file name

  INTEGER*4               \
    RC,                   \
    FILE.SIZE             ! variable used to detect presence of file

  INTEGER*2               \
    L                     ! rename return code

  OUTFILE$ = "WIC1:" +                           \ file name for bad file
             LEFT$(FILENAME$,                    \
             LEN(FILENAME$)-3) + EXT$            !
  FILE.SIZE = SIZE(OUTFILE$)                     ! get size of file

  IF FILE.SIZE > 0 THEN BEGIN                    ! if file found
    OPEN OUTFILE$ AS 6                           ! open it
    DELETE 6                                     ! and then delete it
  ENDIF

  T$ = TIME$
  H$ = LEFT$(T$, 2)
  M$ = MID$(T$, 3, 2)
  WORK4$ = "Processing " + FILENAME$ + " failed " + H$ + ":" + M$
  CALL ADXSERVE(RC, 26, 0, WORK4$)               ! display BG msg
  WAIT;2000
  HANDLE.FR20 = -1        ! Handle FR 20 errors. IO00625
  L = RENAME(OUTFILE$, SRC.FILE$)                ! rename the source file
!AIO00625 If a rename error occurs, delete the target and then retry the rename
  IF HANDLE.FR20 = 0 THEN BEGIN ! a rename error occurred
    OPEN OUTFILE$ AS 6                           ! open it
    DELETE 6                                     ! and then delete it
    L = RENAME(OUTFILE$, SRC.FILE$)              ! try to rename again
  ENDIF
  HANDLE.FR20 = 0         ! Stop fielding FR 20 errors. IO00625
!EIO00625
  IF L = 0 THEN BEGIN                            ! if rename didnt work
    DELETE 5                                     ! delete source
  ENDIF

END SUB

!*************************************************************************
! SUB BUILD.APL.REC
!
! This subroutine extracts selected fields from a WIC agency's approved
! product list and creates a record for use in processing wic items.
!*************************************************************************

SUB BUILD.APL.REC PUBLIC

  STRING                                         \
    TEMP$,                                       \ IO09131
    OUT.STR$,                                    \ output string for SA
    DES.STR$,                                    \
    DES.KEY$,                                    \
    UPC$,                                        \ upc
    LAST.UPC$,                                   \ upc
    CHK.DIG$,                                    \ check digit for upc
    CATEGORY$,                                   \ wic category
    SUB.CATEGORY$,                               \ wic subcategory
    BEN.QTY$,                                    \ number required to get benefit
    BEN.UNIT$,                                   \ wic benefit unit
    ITEM.PRICE$,                                 \ item price
    PRICE.TYPE$,                                 \ item price type
    START.DATE$,                                 \ benefit effective date
    END.DATE$,                                   \ benefit end date
    ZERO.SUB.CAT.FLAG$,                          \ flag
    LAST.SUB.CATEGORY$,                          \ save last items sub
    DETAIL.CNT$                                  ! number of detail records

  INTEGER*2                                      \
    I,                                           \
    DUPLICATE.DISCARD,                           \
    KEY.FOUND,                                   \
    L                                            ! work variable

  IF RIGHT$("000000" + STR$(REC.CNT + 2),6) <>   \ expected sequence number
     MID$(WORK.STR$,3,6) THEN BEGIN              ! actual sequence number
    REC.TYPE$ = ""                               ! exit and force an error
    REC.CNT = REC.CNT + 1                        ! to log #records processed
    EXIT SUB
  ENDIF
  UPC$ = MID$(WORK.STR$, 13, 17)                 ! get wic upc
  CHK.DIG$ = RIGHT$(UPC$, 1)                     ! get upc check digit
  UPC$ = LEFT$(UPC$, LEN(UPC$)-1)                ! wic upc w/o check digit
  UPC$ = PACK$(RIGHT$(UPC$, 14))                 ! only use right 14 digits
  OUT.STR$ = CHK.DIG$                            ! begin building output string
  CATEGORY$ = MID$(WORK.STR$, 80, 2)             ! get category
  OUT.STR$ = OUT.STR$ + PACK$(CATEGORY$)
  SUB.CATEGORY$ = MID$(WORK.STR$, 132, 3)        ! get subcategory
  OUT.STR$ = OUT.STR$ + PACK$("0" + SUB.CATEGORY$)
  DES.KEY$ = PACK$("0" + CATEGORY$ + SUB.CATEGORY$) ! ".des" file key

  KEY.FOUND = 0
  ! Check for zero category if so we need to look at the next record and
  ! if it is the same upc, turn on the zero category flag for it. There is
  ! an assumption that the UPCs come together and the 000 is first
  !
  ! In order to match ACE we will not write the second record when its
  ! not duplicated because of a dual category. In the sample file there is a
  ! case of a record duplicated with a different check digit and other data.
  ! ACE is only using the first record. We will too. There remains a difference
  ! with ACE as they retain the first record's data even in the case where
  ! there is a zero record followed by a same upc, but different data. We will
  ! use the second record's data.

  ZERO.SUB.CAT.FLAG$ = CHR$(00H)                 ! default to flag to off

  IF UPC$ = LAST.UPC$ THEN BEGIN                 ! the same as last one
    IF LAST.SUB.CATEGORY$ = "000" THEN BEGIN     ! and it was a zero cat
      ZERO.SUB.CAT.FLAG$ = CHR$(01H)             ! flag on
    ENDIF ELSE BEGIN
      DUPLICATE.DISCARD = -1                     ! Flag to prevent write
    ENDIF
!AIO09131 When a Subcategory exists for 000 and any other number, Texas and
!         New Mexico always group items in the APL file together so that the
!         Subcategory 000 item always immediately precedes the other category
!         record with that same UPC.  Note that this dependency is not
!         documented as a requirement.  Wyoming does not group all records
!         by UPC.  The fix is to add support so that they can appear in
!         ANY order.  If the subcategory is not zero, look for another record
!         (in the keyed EAMWEss.APL output file) with that same UPC.
!         When a nonzero immediately follows a zero with the same UPC, we can
!         continue to use the old behavior.  Otherwise:
!
!           look for a record with this UPC
!           if current subcat = 000
!             if previous exists
!               if previous is non-zero
!                 set ZERO flag in the previous record
!                 rewrite previous record
!                 skip to next input record
!               else
!                 error - there are two zero records.  skip it
!             else no previous record
!               set subcat-zero flag and continue processing
!           else current subcat = 001 or more
!             if previous exists
!               if previous is zero
!                 set subcat-zero flag and continue processing
!               else
!                 error - there are two non-zero records.  ignore it
!           end
\ ENDIF
  ENDIF ELSE BEGIN                        ! new method of searching
    ! Read the APL File using session number of the keyed file
    REC.NOT.FOUND = 0
    READ FORM "C39"; #6 KEY UPC$; TEMP$   ! see if this record already exists

    IF SUB.CATEGORY$ = "000" THEN BEGIN   ! if current record is ZERO
      IF (NOT(REC.NOT.FOUND)) THEN BEGIN  ! previous record exists
        ! The second record for this UPC is zero.
        ! First, verify that the previous record has subcategory NON-zero.
        IF (MID$(TEMP$,10,2) <> PACK$("0000")) THEN BEGIN
          ! We had a non-zero subcategory, followed by a zero subcategory.
          ! This case is unlikely to occur, and does not occur in test cases.
          ! Rewrite the previous record, but do not change the current record.
          ZERO.SUB.CAT.FLAG$ = CHR$(01H)    ! flag on
          TEMP$ = LEFT$(TEMP$,34) +         \ data prior to flag
                  ZERO.SUB.CAT.FLAG$ +      \ the new flag
                  MID$(TEMP$,36,99)         ! data after flag
          WRITE FORM "C7 C32"; #6; UPC$, TEMP$ ! update the record in place
          DUPLICATE.DISCARD = -1            ! Flag to prevent write
        ENDIF ELSE BEGIN
          ! This is probably an error.  There was a second subcategory zero
          ! record for the same UPC.
          DUPLICATE.DISCARD = -1            ! Flag to prevent write
        ENDIF
      ENDIF ELSE BEGIN                    ! no other record exists
        ! The zero subcategory is first.
        ! Fall through and process as we did before.
      ENDIF
    ENDIF ELSE BEGIN                      ! non-zero subcategory
      IF (NOT(REC.NOT.FOUND)) THEN BEGIN  ! previous record exists
        ! The second record for this UPC is non-zero.
        ! First, verify that the previous record has subcategory zero.
        IF (MID$(TEMP$,10,2) = PACK$("0000")) THEN BEGIN
          ! We had a zero subcategory, followed by a non-zero subcategory.
          ! Fall through and process as we did before.
          ZERO.SUB.CAT.FLAG$ = CHR$(01H)    ! flag on
        ENDIF ELSE BEGIN
          ! This is probably an error.  There was a second non-zero
          ! subcategory record for the same UPC.
          DUPLICATE.DISCARD = -1            ! Flag to prevent write
        ENDIF
      ENDIF ELSE BEGIN
        ! The ELSE leg here implies the first record found was non-zero.
        ! Fall through and process as we did before.
      ENDIF
    ENDIF                                 ! non-zero subcategory
    REC.NOT.FOUND = 0                     ! restore to initial state
  ENDIF                                   ! new method of searching
!EIO09131

  LAST.UPC$ = UPC$                               ! save it
  LAST.SUB.CATEGORY$ = SUB.CATEGORY$             ! save it


  ! try to cut down on the number of writes to the DES file. many of the
  ! records just overlay existing records
  FOR I = 1 TO 1000
    IF DES.KEYS.ARRAY$(I) = DES.KEY$ THEN BEGIN
      DES.KEYS.CNT(I) = DES.KEYS.CNT(I) + 1
      KEY.FOUND = -1
      I = 1001
    ENDIF ELSE BEGIN
      IF DES.KEYS.CNT(I) = 0 THEN BEGIN
        DES.KEYS.CNT(I) = DES.KEYS.CNT(I) + 1
        DES.KEYS.ARRAY$(I) = DES.KEY$
        I = 1001
      ENDIF
    ENDIF
  NEXT I

  BEN.QTY$ = MID$(WORK.STR$, 200, 5)             ! get qualifying amount
  OUT.STR$ = OUT.STR$ + PACK$("0" + BEN.QTY$)
  BEN.UNIT$ = MID$(WORK.STR$, 205, 10)           ! get benefit unit of measure
  OUT.STR$ = OUT.STR$ + BEN.UNIT$
  DES.STR$ = BEN.UNIT$
  DES.STR$ = DES.STR$ + MID$(WORK.STR$, 135, 33) ! unit of meas + subcat desc
  ITEM.PRICE$ = MID$(WORK.STR$, 255, 6)          ! get wic price
  OUT.STR$ = OUT.STR$ + PACK$(ITEM.PRICE$)
  PRICE.TYPE$ = MID$(WORK.STR$, 261, 2)          ! get price type
  OUT.STR$ = OUT.STR$ + PACK$(PRICE.TYPE$)
  START.DATE$ = MID$(WORK.STR$, 280, 6)          ! effective start date
  OUT.STR$ = OUT.STR$ + PACK$(START.DATE$)
  END.DATE$ = MID$(WORK.STR$, 288, 6)            ! benefit end date for item
  OUT.STR$ = OUT.STR$ + PACK$(END.DATE$)
  OUT.STR$ = OUT.STR$ + ZERO.SUB.CAT.FLAG$       ! zero category flag
  OUT.STR$ = OUT.STR$ + PACK$("00000000")        ! reserved
  L = LEN(OUT.STR$)                              ! new rec length
  IF DUPLICATE.DISCARD = 0 THEN                  \
    WRITE FORM "C7 C32"; #6; UPC$, OUT.STR$      ! write to output file
  DUPLICATE.DISCARD = 0                          ! reset flag

  IF KEY.FOUND = 0 THEN BEGIN
    WRITE FORM "C3 C43"; #12; DES.KEY$, DES.STR$ ! write to output file
  ENDIF

  WORK.STR$ = STRING$(10, "0")
  WORK.STR$ = RIGHT$(WORK.STR$ + STR$(WE.OFFSET), 10)
  OUT.STR$ = SRC.FILE$ + WORK.STR$
  WRITE #7, 1; OUT.STR$
  REC.CNT = REC.CNT + 1                          ! bump the count
  READ #5; LINE WORK.STR$                        ! read next input record
  WE.OFFSET = PTRRTN
  REC.TYPE$ = LEFT$(WORK.STR$, 2)                ! get the type of record

END SUB

!*************************************************************************
! SUB PROCESS.APL.TRAILER
!
! This subroutine extracts the number of items that were supposed to
! be in the APL file and compares it with the number of items that were
! processed.  If the numbers agree, the processing of the APL file is
! completed.  If they don't agree, the file is tagged as bad.
!*************************************************************************

SUB PROCESS.APL.TRAILER PUBLIC

  STRING              \
    DETAIL.CNT$

  IF REC.TYPE$ = "Z1" THEN BEGIN                 ! trailer record
    DETAIL.CNT$ = MID$(WORK.STR$, 25, 7)         ! get num of D4 recs expected
    CLOSE 6
    CLOSE 12
    IF VAL(DETAIL.CNT$) <> REC.CNT THEN BEGIN    ! if number of records is wrong
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "08"                       ! Z1 record count is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("ABD")
      CLOSE 5
      CLOSE 6
    ENDIF ELSE                                   \ check time of A1 and Z1 recs
    IF SAVE.CREATE.TIME$ <>                      \ if A1 header is not same
       MID$(WORK.STR$, 17, 6) THEN BEGIN         ! as Z1 trailer
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "03"                       ! Z1 record time is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("ABD")
      CLOSE 5
      CLOSE 6
    ENDIF ELSE                                   \ check date of A1 and Z1 recs
    IF SAVE.CREATE.DATE$ <>                      \ if A1 header is not same
       MID$(WORK.STR$, 9, 8) THEN BEGIN          ! as Z1 trailer
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "04"                       ! Z1 record date is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("ABD")
      CLOSE 5
      CLOSE 6
    ENDIF ELSE BEGIN                             ! no errors were found
      CALL REPLACE.NEW.FILE
    ENDIF
  ENDIF ELSE BEGIN                               ! no D4 or no Z1 record
    IF REC.CNT = 0 AND                           \ error condition
       VAL(DETAIL.CNT$) > 0 THEN BEGIN           !
      ! 2nd record not a detail record
    ENDIF ELSE BEGIN
      ! last record not a trailer record
    ENDIF
    LOG.ENTRY$(3) = STR$(REC.CNT)                ! num of recs processed
    LOG.ENTRY$(4) = "01"                         ! D4 record count is wrong
    CALL LOG.EXCEPT                              ! log exception
    CALL BAD.FILE("ABD")
    CLOSE 5
    CLOSE 6
  ENDIF
END SUB

!******************************************************************************
! SUB CONVERT.APL.FILE
!
! This subroutine takes the "apl" file that was sent to the store by the
! host and creates a keyed file containing the data required by the terminal
! Appl. If an error is detected during processing, the file is renamed to
! indicate that is bad.
!******************************************************************************

SUB CONVERT.APL.FILE PUBLIC

  STRING                                         \
    DETAIL.CNT$                                  ! number of detail records

  CALL SETUP.FILES("APL")                        ! setup necessary files
  REC.NOT.FOUND = 0                              ! no error reading
  READ #5; LINE WORK.STR$                        ! read first record

  IF (REC.NOT.FOUND = 0) AND                     \ no error reading
     (LEFT$(WORK.STR$, 2) = "A1") THEN BEGIN     ! header record
    IF (LEN(WORK.STR$) < 24) OR                  \ A1 record too short
       (MID$(WORK.STR$,23,2) <> "02") THEN BEGIN ! incorrect "FileFormatVersion"
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "07"                       ! file format is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("ABD")
      CLOSE 5
      CLOSE 6
      EXIT SUB
    ENDIF
    SAVE.CREATE.DATE$ = MID$(WORK.STR$, 9, 8)    ! save for compare with Z1
    SAVE.CREATE.TIME$ = MID$(WORK.STR$, 17, 6)   ! save for compare with Z1
    READ #5; LINE WORK.STR$                      ! read next record
    IF REC.NOT.FOUND = 0 THEN BEGIN
      WE.OFFSET = PTRRTN
      REC.TYPE$ = LEFT$(WORK.STR$, 2)            ! should be "DA"
      REC.CNT = 0                                ! init

      WHILE (REC.TYPE$ = "DA") AND               \ while there is more
            (REC.NOT.FOUND = 0)                  ! and not EOF
        CALL BUILD.APL.REC
      WEND                                       ! end while "DA" record

      CALL PROCESS.APL.TRAILER
      DIM DES.KEYS.ARRAY$(0)                     ! free the space
      DIM DES.KEYS.CNT(0)
!     CLOSE 5
    ENDIF
  ENDIF                                          ! premature EOF or bad record
  IF REC.NOT.FOUND THEN BEGIN
    LOG.ENTRY$(3) = STR$(REC.CNT)                ! num of recs processed
    LOG.ENTRY$(4) = "01"                         ! D4 record count is wrong
    CALL LOG.EXCEPT                              ! log exception
    CALL BAD.FILE("ABD")
    CLOSE 5
    CLOSE 6
  ENDIF
END SUB

!*************************************************************************
! SUB BUILD.HCL.REC
!
! This subroutine extracts selected fields from a WIC agency's hot card
! list and creates a record for use in processing wic ebt cards
!*************************************************************************

SUB BUILD.HCL.REC PUBLIC

  STRING                                         \
    OUT.STR$,                                    \ output string
    PAN.LEN$,                                    \ length of PAN
    PAN$,                                        \ PAN
    DATE.EFFECT$,                                \ effective date
    REASON$                                      ! reason on hcl

  INTEGER*2                                      \
    L                                            !

  IF RIGHT$("000000" + STR$(REC.CNT + 2),6) <>   \ expected sequence number
     MID$(WORK.STR$,3,6) THEN BEGIN              ! actual sequence number
    REC.TYPE$ = ""                               ! exit and force an error
    REC.CNT = REC.CNT + 1                        ! to log #records processed
    EXIT SUB
  ENDIF
  PAN.LEN$ = MID$(WORK.STR$, 15, 2)              ! get pan length
  PAN$ = PACK$("0" + MID$(WORK.STR$, 17, 19))    ! get pan
  DATE.EFFECT$ = MID$(WORK.STR$, 36, 14)         ! get effective date
  REASON$ = MID$(WORK.STR$, 50, 4)               ! get reason
  OUT.STR$ = PACK$(PAN.LEN$)     +               \
             PACK$(DATE.EFFECT$) +               \
             REASON$                             !
  L = LEN(OUT.STR$)                              ! get length
  WRITE FORM "C10 C12"; #6; PAN$, OUT.STR$       ! write to file
  WORK.STR$ = STRING$(10, "0")
  WORK.STR$ = RIGHT$(WORK.STR$ + STR$(WE.OFFSET), 10)
  OUT.STR$ = SRC.FILE$ + WORK.STR$
! WRITE #7, 1; OUT.STR$
  REC.CNT = REC.CNT + 1                          ! bump count
  READ #5; LINE WORK.STR$                        ! read next record
  WE.OFFSET = PTRRTN
  REC.TYPE$ = LEFT$(WORK.STR$, 2)                ! get type
  IF REC.CNT > 90005 THEN BEGIN                  ! if file is too big
    REC.TYPE$ = ""                               ! exit and force an error
  ENDIF

END SUB

!*************************************************************************
! SUB PROCESS.HCL.TRAILER
!
! This subroutine extracts the number of cards that were supposed to
! be in the HCL file and compares it with the number of cards that were
! processed.  If the numbers agree, the processing of the HCL file is
! completed.  If they don't agree, the file is tagged as bad.
!*************************************************************************

SUB PROCESS.HCL.TRAILER PUBLIC

  INTEGER*4                                      \
    HCL.CNT                                      ! number of cards to be processed

  IF REC.TYPE$ = "Z1" THEN BEGIN                 ! if trailer record
    HCL.CNT = VAL(RIGHT$(WORK.STR$, 7))          ! get num of PAN rec sent
    CLOSE 6

    IF (HCL.CNT = REC.CNT) AND                   \ if all were processed
      (REC.CNT = VAL(MID$(WORK.STR$, 25, 7))) THEN BEGIN ! num of D4 recs expected
      CALL REPLACE.NEW.FILE
    ENDIF ELSE BEGIN                             ! counts dont match
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "08"                       ! Z1 record count is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("HBD")
      CLOSE 5
      CLOSE 6
    ENDIF
  ENDIF ELSE BEGIN
    LOG.ENTRY$(3) = STR$(REC.CNT)                ! num of recs processed
    LOG.ENTRY$(4) = "01"                         ! D4 record count is wrong
    CALL LOG.EXCEPT                              ! log exception
    CALL BAD.FILE("HBD")
    CLOSE 5
    CLOSE 6
  ENDIF
END SUB

!******************************************************************************
! SUB CONVERT.HCL.FILE
!
! This subroutine takes the "hcl" file that was sent to the store by the
! host and creates a keyed file containing the data required by the terminal
! Appl. If an error is detected during processing, the file is renamed to
! indicate that is bad.
!******************************************************************************

SUB CONVERT.HCL.FILE PUBLIC

  INTEGER*2                                      \
    HCL.CNT,                                     \ number of cards to be processed
    L                                            !

  CALL SETUP.FILES("HCL")                        ! setup necessary files
  REC.NOT.FOUND = 0                              ! no error reading
  READ #5; LINE WORK.STR$                        ! read first record

  IF (REC.NOT.FOUND = 0) AND                     \ no error reading
     (LEFT$(WORK.STR$, 2) = "A1") THEN BEGIN     ! if header record
    IF (LEN(WORK.STR$) < 24) OR                  \ A1 record too short
       (MID$(WORK.STR$,23,2) <> "02") THEN BEGIN ! incorrect "FileFormatVersion"
      LOG.ENTRY$(3) = STR$(REC.CNT)              ! num of recs processed
      LOG.ENTRY$(4) = "07"                       ! file format is wrong
      CALL LOG.EXCEPT                            ! log exception
      CALL BAD.FILE("HBD")
      CLOSE 5
      CLOSE 6
      EXIT SUB
    ENDIF
    READ #5; LINE WORK.STR$                      ! read next record
    WE.OFFSET = PTRRTN
    REC.TYPE$ = LEFT$(WORK.STR$, 2)              ! get record type
    REC.CNT = 0                                  ! init

    WHILE (REC.TYPE$ = "D4") AND                 \ while there is more
          (REC.NOT.FOUND = 0)                    ! and not EOF
      CALL BUILD.HCL.REC
    WEND

    CALL PROCESS.HCL.TRAILER
  ENDIF
  IF REC.NOT.FOUND THEN BEGIN
    LOG.ENTRY$(3) = STR$(REC.CNT)                ! num of recs processed
    LOG.ENTRY$(4) = "01"                         ! D4 record count is wrong
    CALL LOG.EXCEPT                              ! log exception
    CALL BAD.FILE("HBD")
    CLOSE 5
    CLOSE 6
  ENDIF
END SUB

!***************************************************************************
! SUB CONVERT.RECONCILIATION
!
! This subroutine processes auto WIC EBT Reconciliation files, as they are
! received and updates the store status file, EAMWERCL.DAT.
!***************************************************************************

SUB CONVERT.RECONCILIATION PUBLIC

  STRING             \
    SETTLE.AMT$,     \ WIC EBT settlement amount
    CLAIM.AMT$,      \ WIC EBT claimed amount
    STATUS.FLAG$,    \ flag indicating the result of processing
                     \   A = settlement and claim amounts agree
                     \   C = D4 record count with count in Z1 record
                     \   E = error file was received
                     \   P = claim was prepared
                     \   X = settlement claim amounts are different
                     \
    STATUS.STR$,     \ record to be updated
    SAVE.STATUS$,    \
    OUT.STR$,        \
    TEMP$,           \ work var
    REC$,            \ record key
    EXT$             ! file extension

  INTEGER*4          \
    FILE.SIZE,       \ IO00563
    DETAIL.REC.CNT,  \ expected number of D4 records
    SETTLE.AMOUNT,   \ settlement amt
    CLAIM.AMOUNT     ! claim amt

  INTEGER*2          \
    L

  REC$ = LEFT$(FILENAME$, 6) +                   \ build the key
         RIGHT$(FILENAME$, 2)                    !
  EXT$ = RIGHT$(FILENAME$, 3)                    ! get file extension
  CALL SETUP.FILES(EXT$)                         ! setup necessary files
  REC.NOT.FOUND = 0                              ! init

  READ FORM "C35"; #8 KEY REC$; STATUS.STR$      ! see if output record exists

  IF REC.NOT.FOUND = 0 THEN BEGIN                ! if found
    AGENCY.ID$ = REC$                            ! record key
    SAVE.STATUS$ = MID$(STATUS.STR$, 9, 23)
    IF LEFT$(EXT$, 1) = "A" THEN BEGIN
      READ #5; LINE WORK.STR$                    ! read first input record
!AIO00633 We enter the following THEN when processing a zero-byte A00 file.
      IF REC.NOT.FOUND THEN BEGIN                ! if error
        LOG.ENTRY$(3) = "0"                      ! number of records processed
        LOG.ENTRY$(4) = "01"                     ! D4 Rec Cnt incorrect
        CALL LOG.EXCEPT                          ! log exception
        EXT$ = "ABD"
        FILENAME$ = REC$ + ".A00"
        CALL BAD.FILE(EXT$)
        CLOSE 5
        CLOSE 8
        CALL CLOSE.RECOVERY
        EXIT SUB
      ENDIF
!EIO00633
      IF LEFT$(WORK.STR$, 2) = "A1" THEN BEGIN   ! if header record
        SAVE.CREATE.DATE$ = MID$(WORK.STR$, 9, 8)  ! save for compare with Z1
        SAVE.CREATE.TIME$ = MID$(WORK.STR$, 17, 6) ! save
        SETTLE.DATE$ = MID$(WORK.STR$, 87, 8)    ! get settlement date
        READ #5; LINE WORK.STR$                  ! read next record
        WE.OFFSET = PTRRTN
        REC.TYPE$ = LEFT$(WORK.STR$, 2)          ! get record type
        REC.CNT = 0                              ! init

        IF REC.TYPE$ = "D4" THEN BEGIN           ! if detail rec
          REC.CNT = REC.CNT + 1                  ! bump count
        ENDIF

        WHILE REC.TYPE$ NE "Z1"                  ! while not a trailer record
          WORK.STR$ = STRING$(10, "0")
          WORK.STR$ = RIGHT$(WORK.STR$ + STR$(WE.OFFSET), 10)
          OUT.STR$ = SRC.FILE$ + WORK.STR$
          WRITE #7, 1; OUT.STR$
          READ #5; LINE WORK.STR$                ! read next record
          WE.OFFSET = PTRRTN
          REC.TYPE$ = LEFT$(WORK.STR$, 2)        ! get record type
          IF REC.TYPE$ = "D4" THEN BEGIN         ! if detail rec
            REC.CNT = REC.CNT + 1                ! bump count
          ENDIF
        WEND

        IF SAVE.CREATE.DATE$ =                   \ if header and trailer agree
          MID$(WORK.STR$, 9, 8) THEN BEGIN       !
          IF SAVE.CREATE.TIME$ =                 \ if header and trailer agree
            MID$(WORK.STR$, 17, 6) THEN BEGIN    !
            SETTLE.AMT$ = MID$(WORK.STR$, 32, 12)  ! get settlement amt
            CLAIM.AMT$ = MID$(STATUS.STR$, 16, 6)  ! get claim amt
            SETTLE.AMOUNT = VAL(SETTLE.AMT$)     ! convert
            TEMP$ = UNPACK$(CLAIM.AMT$)          ! unpack claim amt
            CLAIM.AMOUNT = VAL(TEMP$)            ! convert
            DETAIL.REC.CNT = VAL(MID$(WORK.STR$, 25, 7)) ! expected D4 rec count
            IF REC.CNT <>  DETAIL.REC.CNT THEN BEGIN ! if not the same
              STATUS.FLAG$ = "C"                 ! set status flag
              LOG.ENTRY$(3) = "0"                ! number of records processed
              LOG.ENTRY$(4) = "01"               ! D4 Rec Cnt incorrect
              CALL LOG.EXCEPT                    ! log exception
              EXT$ = "ABD"
              FILENAME$ = REC$ + "."
              FILENAME$ = FILENAME$ + "A00"      ! IO00563 needs an 8.3 name
              CALL BAD.FILE(EXT$)
              STATUS.STR$ = SAVE.STATUS$ +       \
                            PACK$(DATE$) +       \
                            STATUS.FLAG$         !
              WRITE FORM "C8 C27"; #8; AGENCY.ID$, STATUS.STR$ ! update rec
              CLOSE 5
              CLOSE 8
              CALL CLOSE.RECOVERY
              EXIT SUB
            ENDIF ELSE BEGIN                     ! end corrupt file
              IF CLAIM.AMOUNT = SETTLE.AMOUNT THEN BEGIN ! in agreement
                STATUS.FLAG$ = "A"               ! set status
                LOG.ENTRY$(3) = MID$(WORK.STR$, 25, 7) ! num of recs processed
                LOG.ENTRY$(4) = "00"             !
                CALL LOG.EXCEPT                  ! log exception
              ENDIF ELSE BEGIN                   ! end in agreement
                IF CLAIM.AMOUNT <> SETTLE.AMOUNT THEN BEGIN ! not in agreement
                  STATUS.FLAG$ = "X"             ! set status
                  LOG.ENTRY$(3) = MID$(WORK.STR$, 25, 7) ! num of recs processed
                  LOG.ENTRY$(4) = "02"           ! Settlement not same as claim
                  CALL LOG.EXCEPT                ! log exception
                ENDIF                            ! end not in agreement
              ENDIF
            ENDIF

            STATUS.STR$ = MID$(STATUS.STR$, 9, 13) + \ build rest of rec
                          PACK$(SETTLE.AMT$)       + \
                          PACK$(SETTLE.DATE$)      + \
                          PACK$(DATE$)             + \
                          STATUS.FLAG$               !
            WRITE FORM "C8 C27"; #8; AGENCY.ID$, STATUS.STR$ ! update rec
            CLOSE 5
            CLOSE 8                              ! close status file
            ! want to rename this file when it has been processed, otherwise
            ! it will get processed over and over.  Erase any existing
            ! ASV file first.
            TEMP$ = FILENAME$                    ! save current name
            FILENAME$ = REC$ + ".ASV"
            FILENAME$ = "WIC1:" + FILENAME$      ! add directory
!AIO00563 Erase the pre-existing ASV file first
            CHECKING.OUTPUT.FILE = -1            ! flag for on error
            FILE.SIZE = SIZE(FILENAME$)          ! get size of file
            CHECKING.OUTPUT.FILE = 0             ! reset Flag for on error
            CALL ADXFILES (RET,1,PARM1,FILENAME$) ! lock everyone out
            IF FILE.SIZE > 0 THEN BEGIN          ! if file exist
              OPEN FILENAME$ AS 12               ! open the old ASV file
              DELETE 12                          ! delete it
            ENDIF
!EIO00563
            L = RENAME(FILENAME$, SRC.FILE$)     ! rename the file
            CALL ADXFILES (RET,2,PARM1,FILENAME$) ! make it available IO00563
            FILENAME$ = TEMP$                    ! restore filename
            IF L THEN BEGIN                      ! if successful
              CALL REPORT.COMPLETE                  ! display on bgrd
              CALL CLOSE.RECOVERY
            ENDIF
            EXIT SUB
          ENDIF                                  !time doesnt agree
          STATUS.FLAG$ = "C"
          LOG.ENTRY$(3) = "0"                    ! num of recs processed
          LOG.ENTRY$(4) = "03"                   ! A1 time not same as Z1
          CALL LOG.EXCEPT                        ! log exception
          EXT$ = "ABD"
          FILENAME$ = REC$ + "."
          FILENAME$ = FILENAME$ + "A00"      ! IO00563 needs an 8.3 name
          CALL BAD.FILE(EXT$)
          STATUS.STR$ = SAVE.STATUS$ +           \
                        PACK$(DATE$) +           \
                        STATUS.FLAG$             !
          WRITE FORM "C8 C27"; #8; AGENCY.ID$, STATUS.STR$ ! update rec
          CLOSE 5
          CLOSE 8
          CALL CLOSE.RECOVERY
          EXIT SUB
        ENDIF
        STATUS.FLAG$ = "C"
        LOG.ENTRY$(3) = "0"                      ! num of recs processed
        LOG.ENTRY$(4) = "04"                     ! A1 date not same as Z1
        CALL LOG.EXCEPT                          ! log exception
        EXT$ = "ABD"
        FILENAME$ = REC$  + "."
        FILENAME$ = FILENAME$ + "A00"      ! IO00563 needs an 8.3 name
        CALL BAD.FILE(EXT$)
        STATUS.STR$ = SAVE.STATUS$ +             \
                      PACK$(DATE$) +             \
                      STATUS.FLAG$               !
        WRITE FORM "C8 C27"; #8; AGENCY.ID$, STATUS.STR$ ! update rec
        CALL CLOSE.RECOVERY
      ENDIF
    ENDIF ELSE BEGIN
      STATUS.FLAG$ = "E"
      LOG.ENTRY$(3) = "0"                        ! num of recs processed
      LOG.ENTRY$(4) = "05"                       ! Error file received
      CALL LOG.EXCEPT                            ! log exception
      EXT$ = "ESV"
      TEMP$ = FILENAME$                          ! save current name
      FILENAME$ = REC$ + "."
      STATUS.STR$ = SAVE.STATUS$ +               \
                    PACK$(DATE$) +               \
                    STATUS.FLAG$                 !
      WRITE FORM "C8 C27"; #8; AGENCY.ID$, STATUS.STR$ ! update rec
      ! want to rename this file when it has been processed, otherwise
      ! it will get processed over and over
      FILENAME$ = FILENAME$ + EXT$
      FILENAME$ = "WIC1:" + FILENAME$            ! add directory
      L = RENAME(FILENAME$, SRC.FILE$)           ! rename the file
      FILENAME$ = TEMP$                          ! restore filename
      IF L THEN BEGIN                            ! if successful
        CALL REPORT.COMPLETE                        ! display on bgrd
        CALL CLOSE.RECOVERY
      ENDIF
!      EXIT SUB
    ENDIF
  ENDIF ELSE BEGIN

    LOG.ENTRY$(3) = "0"                          ! zero recs processed
    LOG.ENTRY$(4) = "06"                         ! no claim rec found
    CALL LOG.EXCEPT                              ! log exception
    EXT$ = LEFT$(EXT$, 1) + "BD"                 ! "ABD" or "EBD"
    FILENAME$ = REC$ + "." + EXT$                ! e.g. TX500100.A00 IO00563
    CALL BAD.FILE(EXT$)                          ! indicate bad file
  ENDIF

  CLOSE 5
  CLOSE 8
END SUB

!********************************************************************************
! SUB LOOK.4.FILE
!
! This subroutine uses the basrout function "srchdir" to search adx_idt1
! for the type of file that is passed to it in "ext$".  If that type of file
! is found, the appropriate processing subroutine is called to extract data
! as required.
!
! It continues until all files, of the desired type have been found and
! processed.
!********************************************************************************
SUB LOOK.4.FILE(EXT$) PUBLIC

  STRING                  \
    EXT$,                 \ type of file to locate
    SRCH.PATTERN$,        \ file name with wild cards
    BUF.STR$,             \ buffer used by srchdir
    AGENCY$,              \
    FILE.NAME$            ! full file name to locate

  INTEGER*4               \
    RET.CODE              ! return code from srchdir

  INTEGER*2               \
    LOC

  STRING BUFFER$,              \ Need 4 bytes for each possible file
         LOGICAL.FILE$,        \
         TEMP$

  INTEGER*2 I,                 \ Work variable
            J,                 \
            OPW                ! Old period work variable for setting up the
                               ! file list array subscipt.

  BUFFER$ = STRING$(850, CHR$(0))

  RET.CODE = 0
  DIM FILE.LIST$(0)
  LOGICAL.FILE$ = "WIC1:" + "??????." + EXT$ + " "
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
    FCNT = RET.CODE + 1                          ! setup count

    CALL SORT.FOUND.FILES                        ! verify format and then sort

    FOR I = 1 TO FCNT - 1
      FILENAME$ = FILE.LIST$(I)
      AGENCY$ = LEFT$(FILENAME$, 2)
      J = 1

      FOR J = 1 TO NUM.AGENCIES%
        IF AGENCY$ = AGENCY.ARRAY$(J) THEN BEGIN
          J = NUM.AGENCIES% + 1
          IF EXT$ = "APL" THEN BEGIN            ! if approved product list
            PROCESSING.APL = -1
            CALL CONVERT.APL.FILE               ! process it
            PROCESSING.APL = 0
          ENDIF ELSE BEGIN
            IF EXT$ = "HCL" THEN BEGIN          ! if hot card list
              CALL CONVERT.HCL.FILE             ! process it
            ENDIF ELSE BEGIN                    ! endif hot card list
              IF LEFT$(EXT$, 1) = "A" OR        \
                 LEFT$(EXT$, 1) = "E" THEN BEGIN ! if possible reconcile file
                LOC = MATCH("#", RIGHT$(FILENAME$, 2), 1) ! look for nums in ext
                IF LOC > 0 THEN BEGIN           ! if 'a' and nums
                  CALL CONVERT.RECONCILIATION   ! process reconciliation file
                ENDIF                           ! endif nums in ext
              ENDIF                             ! endif reconcile file
            ENDIF
          ENDIF
        ENDIF
      NEXT J
    NEXT I
  ENDIF

END SUB


!********************************************************************************
! MAINLINE
!********************************************************************************
!  Overview of process:
!  Read in options so we know which agencys to look for new files
!  Every interval, check for new files
!  process files into new work files
!  replace current files from work files
!
!  For audit trail:
!  leave last input file processed with SV as the last 2 letters of the name
!  file on root called recovery that contains the last file processed and if
!  interupted, where we left off.

START:

  ON ERROR GOTO TRAP.IT

  CALL SET.WAIT.TIME

  DIM LOG.ENTRY$(4)                              ! setup exception logging
  NUM.ENTRIES = 4
  WIC.ERR.MSG$ = "Cannot Open WIC EBT Options File"

  CALL OPEN.READ.OPTION.FILE                     ! open and read EAMWEOPT.DAT

  CALL ADXSERVE(RETC, 4, 0, WORK4$)              ! get controller status string
  IF RETC = 0 THEN CNTRLR.ID$ = MID$(WORK4$,18,2)! extract controller id


  while 1 = 1                                    ! do forever

    WORK4$ = "Pausing for " + STR$(REST) + " ms."
    CALL ADXSERVE(RETC, 26, 0, WORK4$)           ! display BG msg
    WAIT;REST                                    ! wait for a while
    CALL LOOK.4.FILE("APL")                      ! look for new apl files
    CALL LOOK.4.FILE("HCL")                      ! look for new hcl files
    CALL LOOK.4.FILE("A??")                      ! look for new reconcile files
    CALL LOOK.4.FILE("E??")                      ! look for new error files
    T$ = TIME$                                   ! get time
    H$ = LEFT$(T$, 2)                            ! get hour
    M$ = MID$(T$, 3, 2)                          ! get minute
    WORK4$ = "Checking for new files " + H$ + ":" + M$
    CALL ADXSERVE(RETC, 26, 0, WORK4$)           ! display BG msg
    WAIT;2000                                    ! let message be seen
  WEND                                           ! wend forever



!! ON ERROR CODE

TRAP.IT:

  INWE.ERR$ = ERR
  INWE.ERRN% = ERRN
  INWE.ERRF% = ERRF%

! have no option files, pretty much dead
  IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 1) THEN BEGIN

    CALL ADXSERVE(RETC, 26, LEN(WIC.ERR.MSG$), WIC.ERR.MSG$)  ! display BG msg
    WAIT;2000
    CALL ADXERROR( 0,                         \ 0 for controller
                  ASC("B"),                   \ 'close waiting'
                  0,                          \ msg no.
                  1,                          \ severity level
                  2,                          \ checkout support
                  RIGHT$("00"+STR$(ERRF%),2) + \
                  ERR                        + \
                  STR$(INWE.ERRN%))            ! File number error
    WAIT;REST
    RESUME START
  ENDIF

  IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 7) OR  \
     (INWE.ERR$ = "EF") AND (INWE.ERRF% = 7) THEN BEGIN
    OPEN.FAILED = -1
    RESUME
  ENDIF

  IF (INWE.ERR$ = "EF") AND (INWE.ERRF% = 5) THEN BEGIN
    REC.NOT.FOUND = -1
    RESUME
  ENDIF

  IF (INWE.ERR$ = "EF") AND (INWE.ERRF% = 8) THEN BEGIN
    REC.NOT.FOUND = -1
    RESUME
  ENDIF

!AIO09131 Key not found in EAMWEss.WRK
  IF (INWE.ERR$ = "EF") AND (INWE.ERRF% = 6) THEN BEGIN
    REC.NOT.FOUND = -1
    RESUME
  ENDIF
!EIO09131

  ! doing an open on recovery session 7 when its already opened
  IF (INWE.ERR$ = "DF") AND (INWE.ERRF% = 7) THEN BEGIN
    RESUME
  ENDIF

!AIO00625
  ! doing an open on recovery session 8 when its already opened
  IF (INWE.ERR$ = "DF") AND (INWE.ERRF% = 8) THEN BEGIN
    RESUME
  ENDIF

  ! trying to open 5 when it is already open
  IF (INWE.ERR$ = "DF") AND (INWE.ERRF% = 5) THEN BEGIN
    CLOSE 5
    RESUME RETRY
  ENDIF
!EIO00625

  ! doing a close on a closed file
  IF (INWE.ERR$ = "CU") THEN BEGIN
    RESUME
  ENDIF

  ! doing a close on des file session 12 when its not opened
  IF (INWE.ERR$ = "CU") AND (INWE.ERRF% = 12) THEN BEGIN
    RESUME
  ENDIF

  ! doing a size found that the wrk file was not there
  IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 0) THEN BEGIN
    IF NOT CHECKING.OUTPUT.FILE THEN BEGIN
      NUM.RECS = 0
      DELETE 7
      WE.OFFSET = 0
      RESUME
    ENDIF ELSE BEGIN
      RESUME
    ENDIF
  ENDIF
  ! need to handle wercl missing
    IF (INWE.ERR$ = "OE") AND (INWE.ERRF% = 8) THEN BEGIN
      CREATE.WERCL = -1
      RESUME
    ENDIF

!AIO00625 Recover from a rename error
  ! handle FR 20 error?
  IF (HANDLE.FR20 <> 0) AND (INWE.ERR$ = "FR") AND (INWE.ERRN% = 20H) THEN BEGIN
    HANDLE.FR20 = 0               ! indicate it happened
    RESUME
  ENDIF
!EIO00625

  ! Got to the bottom with no handling, lets start over
  RESUME START

DONE:
  STOP
