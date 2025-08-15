\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMWEOPC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: Option program for SmartCard WIC        */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      THIS MODULE IS "RESTRICTED MATERIALS OF IBM"              */
\/*      (C) COPYRIGHT IBM CORP 2004 ALL RIGHTS RESERVED           */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 1 LEVEL 0                                 */
\/*                                                                */
\/*      PERSON RESPONSIBLE = IBM     Tom Gaskill                  */
\/*                                                                */
\/*      FUNCTION: EAMWEOPC                                        */
\/*                                                                */
\/*      THIS MODULE ALLOWS THE USER TO SET OPTIONS IN DATA FILE   */
\/*      EAMWEOPT.DAT TO HANDLE SMARTCARD WIC.                     */
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
\/*         MODULE SIZE: ??? LOC                                   */
\/*                                                                */
\/*         ATTRIBUTES:                                            */
\/*                                                                */
\/*      ENTRY POINT: MAIN LINE                                    */
\/*                                                                */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMWEOPT                                */
\/*                                                                */
\/*         DESCRIPTION: SMARTCARD WIC                             */
\/*                                                                */
\/*                ***** FILE INPUT *****                          */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*         SYMBOLIC NAME: EAMWEOPT                                */
\/*                                                                */
\/*         DESCRIPTION: SMARTCARD WIC                             */
\/*                                                                */
\/*                ***** FILE OUTPUT *****                         */
\/*                                                                */
\/*                                                                */
\/*      EXIT-NORMAL:                                              */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*      EXIT-ERROR: RETURN TO CALLER                              */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*           NAME: ADXERROR - Logs system errors                  */
\/*           NAME: MMCFEC03 - Display message on line 23          */
\/*           NAME: MMCFEC04 - Get data entered at screen          */
\/*           NAME: MMCFEC05 - Handle fatal display errors         */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*           REFERENCED:                                          */
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

!******************************************************************************
! Variable declarations
!******************************************************************************

%INCLUDE EAMUSED.J86                   ! Chain variables
%INCLUDE EAMCFINC.J86                  ! declarations for common functions
%INCLUDE EAMATTRD.J86                  ! decl.s for display manager attributes
%INCLUDE EAMSOPTS.J86                  ! store option variables
%INCLUDE EAMOPERA.J86                  ! operator authorization file variables
%INCLUDE EAMOPER2.J86                  ! operator authorization value pack vars
%INCLUDE EAMTERMS.J86                  ! terminal status file variables

STRING                                 \
       NUM.AGENCIES$,                  \ Num. of agencies supported for display
       WIC.TENDER$,                    \ Tender type/variety for  EBT WIC tender
       DUP.HDR1$,                      \ Text for header 1 of duplicate receipts
       DH1.LF$,                        \ linefeed for Duplicate header 1
       DH1.FONT$,                      \ font for Duplicate header 1
       BNA.HDR1$,                      \ "Benefits Not Available" header 1 text
       BNA1.LF$,                       \ linefeed for Not Available header 1
       BNA1.FONT$,                     \ font for  Not Available header 1
       BNA.HDR2$,                      \  Not Available" header 2 text
       BNA2.LF$,                       \ linefeed for  Not Available header 2
       BNA2.FONT$,                     \ font for Not Available header 2
       AGENCY.MAIN.OPTION$(1),         \ data read for each agencies main option
       AGENCY.RECPT.BNFT.HDR$(1),      \ Benefit header, linefeeds, and fonts
       AGENCY.RECPT.BNFT.ITM$(1),      \ Benefit item entry, linefeeds, fonts
       AGENCY.RECPT.BNFT.TRL$(1),      \ Benefit trailer, linefeeds,  fonts
       AGENCY.RECPT.RDPT.HDR$(1),      \ Redemption header, linefeeds, and fonts
       AGENCY.RECPT.RDPT.ITM$(1),      \ Redemption item entry, linefeeds, fonts
       AGENCY.RECPT.RDPT.TRL$(1),      \ Redemption trailer, linefeeds, fonts
       STATE.ID$,                      \ State ID of agency
       ACTIVE.FLAG$,                   \ Flag if agency is active
       BIN.NUMBER$,                    \ Bin number of agency
       MERCHANT.ID$,                   \ Merchant ID: Specifies card acceptor ID
       FORWARD.INSTITUTE$,             \ Forwarding Institution ID of the
                                       \ institution that forwards the WIC files
       RECEIVE.INSTITUTE$,             \ Receiving Institution ID WIC file 
                                       \ receiver
       BENEFIT.RECEIPT$(1),            \ Benefit receipt data
       REDEMPTION.RECEIPT$(1),         \ Redemption receipt data
       CONTROL.FMT$,                   \ format string for the control record
       AGENCY.FMT$,                    \ format string for the agency records
       A$,B$,                          \ work variables
       WIC.MSG$(1),                    \ messages for display 
       DAYLIGHT.SAVING$,               \ observed/not observed flag 
       GMT.DIFF$,                      \ diff between gmt and local time 
       UNIQ$,                          \ ADXERROR parameter
       WIC.ERR$,                       \ ERR
       WIC.ERRN$                       ! ERRN code used for error logging

INTEGER*1                              \
          ERR.RESUME,                  \ flag to resume from errors
          SVRTY,                       \ ADXERROR parameter
          MSGGRP,                      \ ADXERROR parameter
          EVENT                        ! ADXERROR parameter

INTEGER*2                              \
          NUM.AGENCIES%,               \ Number of agencies supported
          MAX.AGENCIES,                \ maximum number of agencies allowed
          ORG.NUM.AGENCIES%,           \ number of agencies when file first read
          EAMWEOPT.SN,                 \ session number for EAMWEOPT.DAT
          EAMWEOPT.RECL,               \ record length for EAMWEOPT.DAT
          SCREEN.NUM,                  \ screen number to display
          CURR.AGENCY.REC,             \ this agency record number 
          CURRENT.FIELD,               \ current screen pos
          FIRST.FIELD,                 \ starting screen pos
          LAST.FIELD,                  \ ending screen pos
          LAST.PAGE,                   \ keep record of last page
          I%,J%,K%,                    \ work variables
          RET.VAL,                     \ ADXERROR Return value parameter
          TERM,                        \ ADXERROR parameter
          MSGNUM,                      \ ADXERROR parameter
          WIC.ERRF%                    ! ERRF%

INTEGER*4                              \
          X%,                          \ work variable
          RET,                         \ Used by ADXSERVE
          WIC.ERRN%                    ! ERRN

!******************************************************************************
! End Variable declarations
!******************************************************************************

!******************************************************************************
! External Routines
!******************************************************************************

%INCLUDE EAMXXCPY.J86                  ! copyright statement
%INCLUDE EAMADXRT.J86                  ! system services interface
%INCLUDE EAMASMCT.J86                  ! assembler routines
%INCLUDE EAMERRNH.J86                  ! Convert to HEX routine ERRN value.
%INCLUDE DMEXTR.J86                    ! display manager external functions

!******************************************************************************
! Common routine to display a message on line 23
!******************************************************************************
SUB MMCFEC03(MSG.NO, MSG.STR$) EXTERNAL
  INTEGER*2 MSG.NO
  STRING MSG.STR$
END SUB

!******************************************************************************
! Common routine accepts data entered from the screen.
!******************************************************************************
SUB MMCFEC04(FIELD$) EXTERNAL
  STRING FIELD$
END SUB

!******************************************************************************
! Common routine handles fatal display errors.
!******************************************************************************
SUB MMCFEC05(R.VALUE,DM.CMD) EXTERNAL
  INTEGER*2 R.VALUE
  STRING DM.CMD
END SUB

!******************************************************************************
! Common routine that sequentially scans through a keyed file to build an
! array of keys found.
!******************************************************************************
SUB MMCFEC39(KEY.FILE$,KEYLEN,RECLEN) EXTERNAL
  STRING KEY.FILE$
  INTEGER*2 KEYLEN,RECLEN
END SUB

!******************************************************************************
! Functions and Subroutines
!******************************************************************************

!******************************************************************************
! Function Name: STRIP.LEAD.TRAIL.CHARS$
! Description: This function returns a string with the leading and trailing
!              characters stripped
!******************************************************************************
FUNCTION STRIP.LEAD.TRAIL.CHARS$(STR.TO.STRIP$,PASSED.STR$)

  STRING                               \
         STR.TO.STRIP$,                \ string to strip from passed string
         PASSED.STR$,                  \ passed string
         STRIP.LEAD.TRAIL.CHARS$       ! function return

  INTEGER*2                            \
            STRIP.LEN                  ! length of string we're stripping

  STRIP.LEN = LEN(STR.TO.STRIP$)

  IF (STRIP.LEN = 0) OR                          \ if invalid string passed
     (STRIP.LEN > LEN(PASSED.STR$))              \
  THEN BEGIN
    STRIP.LEAD.TRAIL.CHARS$ = PASSED.STR$
    EXIT FUNCTION
  ENDIF                                          ! if invalid string passed

  IF (PASSED.STR$ = STR.TO.STRIP$) THEN BEGIN
    PASSED.STR$ = ""
  ENDIF

  ! strip left side of string
  WHILE (PASSED.STR$ <> "") AND                  \
        (LEN(PASSED.STR$) >= STRIP.LEN) AND      \
        (LEFT$(PASSED.STR$,STRIP.LEN) = STR.TO.STRIP$)

    PASSED.STR$ = RIGHT$(PASSED.STR$,LEN(PASSED.STR$) - STRIP.LEN)
    IF (PASSED.STR$ = STR.TO.STRIP$) THEN BEGIN
      PASSED.STR$ = ""
    ENDIF

  WEND

  ! strip right side of string
  WHILE (PASSED.STR$ <> "") AND                  \
        (LEN(PASSED.STR$) >= STRIP.LEN) AND      \
        (RIGHT$(PASSED.STR$,STRIP.LEN) = STR.TO.STRIP$)

    PASSED.STR$ = LEFT$(PASSED.STR$,LEN(PASSED.STR$) - STRIP.LEN)
    IF (PASSED.STR$ = STR.TO.STRIP$) THEN BEGIN
      PASSED.STR$ = ""
    ENDIF

  WEND

  STRIP.LEAD.TRAIL.CHARS$ = PASSED.STR$          ! return stripped string

END FUNCTION                                     ! end STRIP.LEAD.TRAIL.CHARS$
!******************************************************************************


!******************************************************************************
! Subroutine Name: ACCESS.CHAIN.PARAMETERS
! Description: This routine accesses the chaining variables by including the
!              base SA chain module EAMUSE.J86
!******************************************************************************
SUB ACCESS.CHAIN.PARAMETERS

  %INCLUDE EAMUSE.J86                            ! access chain parameters

END SUB                                          ! end ACCESS.CHAIN.PARAMETERS
!******************************************************************************


!******************************************************************************
! Subroutine Name: CHAIN.BACK
! Description: This routine chains this program back.
!******************************************************************************
SUB CHAIN.BACK(CHAINTO$)

  STRING                               \
         CHAINTO$                      ! name of program to chain to

  MM.CHAINFROM$ = "EAMWEOPL.286"                 ! name of current program

  %INCLUDE EAMCHAIN.J86                          ! base SA code for chaining

END SUB                                          ! end ACCESS.CHAIN.PARAMETERS
!******************************************************************************


!******************************************************************************
! Subroutine Name: NOTIFY.TERMINALS
! Description: This routine notifies the terminals to reload their options
!******************************************************************************
SUB NOTIFY.TERMINALS

  INTEGER*2                            \
            TERM.FLAG                  ! terminal flag bit

  CALL MMCFEC03(295,"")                          ! notifying terms to reload
  NEED.TEST = ON1                                ! want to exclude some
  TEST.ON = OFF0                                 ! if bit set, do not want
  MASK = 10H                                     ! 1st bit -prev & store rec
  OFFSET = 0                                     ! 1st byte
  TERM.FLAG = 0800H                              ! init reload flag

  CALL MMCFEC39("EAMTERMS",2,72)                 ! get keys from term status

  IF (KEY.COUNT > 0) THEN BEGIN                  ! if keys found

    OPEN "EAMTERMS" KEYED RECL 72 AS 3 NODEL     ! open term status file

    FOR I% = 1 TO KEY.COUNT                      ! for each key in array

      TE.TR.TERMINAL$ = KEY.ARRAY$(I%)           ! build the key

      A$ = STRING$(70,CHR$(0))

      READ FORM " T3 C70 "; #3 AUTOLOCK KEY TE.TR.TERMINAL$;A$

      TE.TR.STATUS = GETN2(A$,66)
      TE.TR.STATUS  = TE.TR.STATUS OR TERM.FLAG  ! set reload flag
      K% = PUTN2(A$,66,TE.TR.STATUS)

      WRITE FORM "C2 C70" ;#3 AUTOUNLOCK; TE.TR.TERMINAL$,A$

    NEXT I%

    CLOSE 3                                      ! close term status file

    A$ = ""                                      ! cleanup

  ENDIF                                          ! if keys found

  NEED.TEST = OFF0                               ! reset mmfec39 flag

  WAIT;2000                                      ! make sure message displays
END SUB                                          ! end NOTIFY.TERMINALS
!******************************************************************************


!******************************************************************************
! Subroutine Name: INITIALIZE.VARIABLES
! Description: This routine initializes all variable used in the program.
!******************************************************************************
SUB INITIALIZE.VARIABLES

  %INCLUDE EAMATTRI.J86                          ! Display Manager attribute

  DIM FKEY.CHECK(10)

  FKEY.CHECK(1)  = ON1                   ! Set F1 function key on
  FKEY.CHECK(2)  = OFF0                  ! Set F2 function key off
  FKEY.CHECK(3)  = ON1                   ! Set F3 function key on
  FKEY.CHECK(4)  = OFF0                  ! Set F4 function key off
  FKEY.CHECK(5)  = OFF0                  ! Set F5 function key off
  FKEY.CHECK(6)  = OFF0                  ! Set F6 function key off
  FKEY.CHECK(7)  = OFF0                  ! Set F7 function key off
  FKEY.CHECK(8)  = OFF0                  ! Set F8 function key off
  FKEY.CHECK(9)  = OFF0                  ! Set F9 function key off
  FKEY.CHECK(10) = OFF0                  ! Set F10 function key on 

  PAGE = 1
  NEED.RANGE.CHECK = ON1
  HELP.AVAILABLE = ON1
  FULL.SCREEN = ON1
  ENTER.PRESSED = OFF0
  QUIT.PRESSED = OFF0
  ESC.PRESSED = OFF0
  NEED.TEST = OFF0

  FIRST.FIELD = 1
  CURRENT.FIELD = FIRST.FIELD
  LAST.PAGE = 0

  EAMWEOPT.SN = 1                         ! session number for EAMWEOPT.DAT
  EAMWEOPT.RECL = 1024                    ! record length for EAMWEOPT.DAT

  ! create the read/write format for the records
  CONTROL.FMT$ = "2C1,C38,2C1,C38,2C1,C38,4C1,3C255,C135"
  AGENCY.FMT$ = "C33,C240,C40,C120,C240,C120,C80,C151"

  MAX.AGENCIES = 10
  DIM AGENCY.MAIN.OPTION$(MAX.AGENCIES)          ! dim up to max agencies
  DIM AGENCY.RECPT.BNFT.HDR$(MAX.AGENCIES)
  DIM AGENCY.RECPT.BNFT.ITM$(MAX.AGENCIES)
  DIM AGENCY.RECPT.BNFT.TRL$(MAX.AGENCIES)
  DIM AGENCY.RECPT.RDPT.HDR$(MAX.AGENCIES)
  DIM AGENCY.RECPT.RDPT.ITM$(MAX.AGENCIES)
  DIM AGENCY.RECPT.RDPT.TRL$(MAX.AGENCIES)

  DIM WIC.MSG$(3)

  OPEN "EAMAAAMF" RECL 82 AS 5 NOWRITE NODEL     ! MESSAGE FILE
  READ #5,468;WIC.MSG$(1)                        ! REMOVE STATE CODE MSG
  READ #5,469;WIC.MSG$(2)                        ! VALID ENTRIES MSG
  READ #5,469;WIC.MSG$(3)                        ! VALID ENTRIES MSG
  CLOSE 5

  DAYLIGHT.SAVING$ = "Y"                         ! observed
  GMT.DIFF$ = "05"                               ! EST difference
END SUB                                          ! end INITIALIZE.VARIABLES
!******************************************************************************


!******************************************************************************
! Subroutine Name: INITIALIZE.DISPLAY.MANAGER
! Description: This routine initializes display manager
!******************************************************************************
SUB INITIALIZE.DISPLAY.MANAGER

  DM.DISPFILE$ = "WIC"                        ! Set screen ID for err log
  CALL MMCFEC05(INITDM(""),"WE 4")            ! initialize display mgr
  CALL MMCFEC05(OPNDIS("EAMWES1S"),"WE 6")    ! open WIC EBT screen file

END SUB                                       ! end INITIALIZE DISPLAY.MANAGER
!******************************************************************************


!******************************************************************************
! Subroutine Name: SHIFT.AGENCY.RECORDS
! Description: This routine shifts down all the agency records so the file
!              stays formatted
!******************************************************************************
SUB SHIFT.AGENCY.RECORDS(REC)

  INTEGER*2                            \
            IDX%,                      \ loop variable
            REC                        ! record to remove

    ! shift all records down
  FOR IDX% = REC TO (MAX.AGENCIES - 1)
    AGENCY.MAIN.OPTION$(IDX%) = AGENCY.MAIN.OPTION$(IDX% + 1)
    AGENCY.RECPT.BNFT.HDR$(IDX%) = AGENCY.RECPT.BNFT.HDR$(IDX% + 1)
    AGENCY.RECPT.BNFT.ITM$(IDX%) = AGENCY.RECPT.BNFT.ITM$(IDX% + 1)
    AGENCY.RECPT.BNFT.TRL$(IDX%) = AGENCY.RECPT.BNFT.TRL$(IDX% + 1)
    AGENCY.RECPT.RDPT.HDR$(IDX%) = AGENCY.RECPT.RDPT.HDR$(IDX% + 1)
    AGENCY.RECPT.RDPT.ITM$(IDX%) = AGENCY.RECPT.RDPT.ITM$(IDX% + 1)
    AGENCY.RECPT.RDPT.TRL$(IDX%) = AGENCY.RECPT.RDPT.TRL$(IDX% + 1)
  NEXT IDX%

  ! delete the last record
  AGENCY.MAIN.OPTION$(MAX.AGENCIES) = ""
  AGENCY.RECPT.BNFT.HDR$(MAX.AGENCIES) = ""
  AGENCY.RECPT.BNFT.ITM$(MAX.AGENCIES) = ""
  AGENCY.RECPT.BNFT.TRL$(MAX.AGENCIES) = ""
  AGENCY.RECPT.RDPT.HDR$(MAX.AGENCIES) = ""
  AGENCY.RECPT.RDPT.ITM$(MAX.AGENCIES) = ""
  AGENCY.RECPT.RDPT.TRL$(MAX.AGENCIES) = ""

END SUB                                          ! end SHIFT.AGENCY.RECORDS
!******************************************************************************


!******************************************************************************
! Subroutine Name: OPEN.READ.OPTION.FILE
! Description: This routine opens then reads the option file, EAMWEOPT.DAT
!******************************************************************************
SUB OPEN.READ.OPTION.FILE

  OPEN "EAMWEOPT" DIRECT RECL EAMWEOPT.RECL AS EAMWEOPT.SN BUFFSIZE 1024

  READ FORM CONTROL.FMT$; #EAMWEOPT.SN,100;                  \
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

  ! Now validate the control data
  NUM.AGENCIES$ = UNPACK$(NUM.AGENCIES$)

  ERR.RESUME = -1                                ! set flag to resume from error 

  NUM.AGENCIES% = VAL(NUM.AGENCIES$)             ! Get value of agency 

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

  ORG.NUM.AGENCIES% = NUM.AGENCIES%              ! save initial number/agencies

  WIC.TENDER$ = UNPACK$(WIC.TENDER$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(WIC.TENDER$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    WIC.TENDER$ = RIGHT$("00" + WIC.TENDER$,2)   ! format for display
  ENDIF ELSE BEGIN                               ! else an error occurred
    WIC.TENDER$ = "00"                           ! default to zero
  ENDIF                                          ! if no error occurred

  DH1.LF$ = UNPACK$(DH1.LF$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(DH1.LF$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    DH1.LF$ = STR$(I%)                           ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    DH1.LF$ = "0"                                ! default to zero
  ENDIF                                          ! if no error occurred

  DH1.FONT$ = UNPACK$(DH1.FONT$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(DH1.FONT$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    DH1.FONT$ = STR$(I%)                         ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    DH1.FONT$ = "0"                              ! default to zero
  ENDIF                                          ! if no error occurred

  BNA1.LF$ = UNPACK$(BNA1.LF$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(BNA1.LF$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    BNA1.LF$ = STR$(I%)                          ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    BNA1.LF$ = "0"                               ! default to zero
  ENDIF                                          ! if no error occurred

  BNA1.FONT$ = UNPACK$(BNA1.FONT$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(BNA1.FONT$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    BNA1.FONT$ = STR$(I%)                        ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    BNA1.FONT$ = "0"                             ! default to zero
  ENDIF                                          ! if no error occurred

  BNA2.LF$ = UNPACK$(BNA2.LF$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(BNA2.LF$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    BNA2.LF$ = STR$(I%)                          ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    BNA2.LF$ = "0"                               ! default to zero
  ENDIF                                          ! if no error occurred

  BNA2.FONT$ = UNPACK$(BNA2.FONT$)

  ERR.RESUME = -1                                ! set flag to resume from error

  I% = VAL(BNA2.FONT$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    BNA2.FONT$ = STR$(I%)                        ! format data
  ENDIF ELSE BEGIN                               ! else an error occurred
    BNA2.FONT$ = "0"                             ! default to zero
  ENDIF                                          ! if no error occurred
  ERR.RESUME = -1                                ! set flag to resume from error

  GMT.DIFF$ = UNPACK$(GMT.DIFF$)

  I% = VAL(GMT.DIFF$)

  IF ERR.RESUME THEN BEGIN                       ! if no error occurred
    ERR.RESUME = 0                               ! reset error flag
    IF I% >1 AND I% < 24 THEN BEGIN
      GMT.DIFF$ = STR$(I%)                       ! format data
    ENDIF ELSE BEGIN
      GMT.DIFF$ = "5"                            ! default to EST
    ENDIF
  ENDIF ELSE BEGIN                               ! else an error occurred
    GMT.DIFF$ = "5"                              ! default to EST
  ENDIF                                          ! if no error occurred

  ! Now read all the agency records
  FOR I% = 1 TO NUM.AGENCIES%
    READ FORM AGENCY.FMT$; #EAMWEOPT.SN,I%;                               \
                                           AGENCY.MAIN.OPTION$(I%),       \
                                           AGENCY.RECPT.BNFT.HDR$(I%),    \
                                           AGENCY.RECPT.BNFT.ITM$(I%),    \
                                           AGENCY.RECPT.BNFT.TRL$(I%),    \
                                           AGENCY.RECPT.RDPT.HDR$(I%),    \
                                           AGENCY.RECPT.RDPT.ITM$(I%),    \
                                           AGENCY.RECPT.RDPT.TRL$(I%),    \
                                           A$                       ! reserved
  NEXT I%

  A$ = ""                                        ! cleanup
  B$ = ""

END SUB                                          ! end OPEN.READ.OPTION.FILE
!******************************************************************************


!******************************************************************************
! Subroutine Name: WRITE.OPTION.DATA
! Description: This routine writes the update data to option file, EAMWEOPT.DAT
!******************************************************************************
SUB WRITE.OPTION.DATA

  INTEGER*2                         \
            GMT.DIFF%,              \
            SAV.NUM.AGENCIES%       ! save NUM.AGENCIES% for compare after loop

  NUM.AGENCIES$ = PACK$(RIGHT$("00" + NUM.AGENCIES$,2))
  WIC.TENDER$ = PACK$(RIGHT$("00" + WIC.TENDER$,2))
  DUP.HDR1$ = LEFT$(DUP.HDR1$ + STRING$(38," "),38)
  DH1.LF$ = PACK$(RIGHT$("00" + DH1.LF$,2))
  DH1.FONT$ = PACK$(RIGHT$("00" + DH1.FONT$,2))
  BNA.HDR1$ = LEFT$(BNA.HDR1$ + STRING$(38," "),38)
  BNA1.LF$ = PACK$(RIGHT$("00" + BNA1.LF$,2))
  BNA1.FONT$ = PACK$(RIGHT$("00" + BNA1.FONT$,2))
  BNA.HDR2$ = LEFT$(BNA.HDR2$ + STRING$(38," "),38)
  BNA2.LF$ = PACK$(RIGHT$("00" + BNA2.LF$,2))
  BNA2.FONT$ = PACK$(RIGHT$("00" + BNA2.FONT$,2))
  GMT.DIFF$ = PACK$(RIGHT$("00" + GMT.DIFF$,2))   ! properly
  A$ = STRING$(255,CHR$(0))
  B$ = STRING$(134,CHR$(0))

  ! Write back the control record

  WRITE FORM CONTROL.FMT$; #EAMWEOPT.SN,100;                  \
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
                                            A$,A$,A$,B$       ! reserved data

  A$ = STRING$(151,CHR$(0))
  SAV.NUM.AGENCIES% = NUM.AGENCIES%

  FOR I% = 1 TO NUM.AGENCIES%

    ! get the state ID with any leading and trailing packed zeroes stripped
    B$ = STRIP.LEAD.TRAIL.CHARS$(CHR$(0),LEFT$(AGENCY.MAIN.OPTION$(I%),2))

    IF (B$ = "") THEN BEGIN                      ! if no State ID
      CALL SHIFT.AGENCY.RECORDS(I%)              ! remove this record
      I% = I% - 1                                ! shift down to get next record
      NUM.AGENCIES% = NUM.AGENCIES% - 1
    ENDIF ELSE BEGIN

      WRITE FORM AGENCY.FMT$; #EAMWEOPT.SN,I%;                            \
                                              AGENCY.MAIN.OPTION$(I%),    \
                                              AGENCY.RECPT.BNFT.HDR$(I%), \
                                              AGENCY.RECPT.BNFT.ITM$(I%), \
                                              AGENCY.RECPT.BNFT.TRL$(I%), \
                                              AGENCY.RECPT.RDPT.HDR$(I%), \
                                              AGENCY.RECPT.RDPT.ITM$(I%), \
                                              AGENCY.RECPT.RDPT.TRL$(I%), \
                                              A$                     ! reserved
    ENDIF                                        ! if no State ID
  NEXT I%

  ! If the number of agencies has been modified, then clean up the file.
  ! The user removed a State Code so the record should be removed.
  IF (SAV.NUM.AGENCIES% <> NUM.AGENCIES%) OR     \ if agency removed
     (ORG.NUM.AGENCIES% > NUM.AGENCIES%)         \
  THEN BEGIN

    A$ = STRING$(255,CHR$(0))
    B$ = STRING$(4,CHR$(0))

    ! cleanup file
    FOR I% = (NUM.AGENCIES% + 1) TO MAX.AGENCIES
      WRITE FORM "4C255 C4"; #EAMWEOPT.SN,I%; A$,A$,A$,A$,B$
    NEXT I%
  ENDIF                                          ! if agency removed

  A$ = ""                                        ! cleanup
  B$ = ""

END SUB                                          ! end WRITE.OPTION.DATA
!******************************************************************************


!******************************************************************************
! Subroutine Name: PARSE.AGENCY.DATA
! Description: This routine parses the agency record data into work variables.
!******************************************************************************
SUB PARSE.AGENCY.DATA(CURR.REC)

  INTEGER*2                            \
            CURR.REC                   ! record into data array

  A$ = AGENCY.MAIN.OPTION$(CURR.REC)              ! put data into work variable

  STATE.ID$ = LEFT$(A$,2)
  ACTIVE.FLAG$ = MID$(A$,3,1)
  BIN.NUMBER$ = UNPACK$(MID$(A$,4,3))
  MERCHANT.ID$ = MID$(A$,7,15)
  FORWARD.INSTITUTE$ = RIGHT$(UNPACK$(MID$(A$,22,6)),11)
  RECEIVE.INSTITUTE$ = RIGHT$(UNPACK$(MID$(A$,28,6)),11)

  DIM BENEFIT.RECEIPT$(0)
  DIM BENEFIT.RECEIPT$(30)

  A$ = AGENCY.RECPT.BNFT.HDR$(CURR.REC)

  BENEFIT.RECEIPT$(1) = LEFT$(A$,38)                ! Benefit header 1
  BENEFIT.RECEIPT$(2) = UNPACK$(MID$(A$,39,1))      ! Benefit header 1 linefeed
  BENEFIT.RECEIPT$(3) = UNPACK$(MID$(A$,40,1))      ! Benefit header 1 font

  BENEFIT.RECEIPT$(4) = MID$(A$,41,38)              ! Benefit header 2
  BENEFIT.RECEIPT$(5) = UNPACK$(MID$(A$,79,1))      ! Benefit header 2 linefeed
  BENEFIT.RECEIPT$(6) = UNPACK$(MID$(A$,80,1))      ! Benefit header 2 font

  BENEFIT.RECEIPT$(7) = MID$(A$,81,38)              ! Benefit header 3
  BENEFIT.RECEIPT$(8) = UNPACK$(MID$(A$,119,1))     ! Benefit header 3 linefeed
  BENEFIT.RECEIPT$(9) = UNPACK$(MID$(A$,120,1))     ! Benefit header 3 font

  BENEFIT.RECEIPT$(10) = MID$(A$,121,38)            ! Benefit header 4
  BENEFIT.RECEIPT$(11) = UNPACK$(MID$(A$,159,1))    ! Benefit header 4 linefeed
  BENEFIT.RECEIPT$(12) = UNPACK$(MID$(A$,160,1))    ! Benefit header 4 font

  BENEFIT.RECEIPT$(13) = MID$(A$,161,38)            ! Benefit header 5
  BENEFIT.RECEIPT$(14) = UNPACK$(MID$(A$,199,1))    ! Benefit header 5 linefeed
  BENEFIT.RECEIPT$(15) = UNPACK$(MID$(A$,200,1))    ! Benefit header 5 font

  BENEFIT.RECEIPT$(16) = MID$(A$,201,38)            ! Benefit header 6
  BENEFIT.RECEIPT$(17) = UNPACK$(MID$(A$,239,1))    ! Benefit header 6 linefeed
  BENEFIT.RECEIPT$(18) = UNPACK$(MID$(A$,240,1))    ! Benefit header 6 font

  A$ = AGENCY.RECPT.BNFT.ITM$(CURR.REC)

  BENEFIT.RECEIPT$(19) = LEFT$(A$,38)            ! Benefit item entry 1
  BENEFIT.RECEIPT$(20) = UNPACK$(MID$(A$,39,1))  ! Benefit item entry 1 linefeed
  BENEFIT.RECEIPT$(21) = UNPACK$(MID$(A$,40,1))  ! Benefit item entry 1 font

  A$ = AGENCY.RECPT.BNFT.TRL$(CURR.REC)

  BENEFIT.RECEIPT$(22) = LEFT$(A$,38)              ! Benefit trailer 1
  BENEFIT.RECEIPT$(23) = UNPACK$(MID$(A$,39,1))    ! Benefit trailer 1 linefeed
  BENEFIT.RECEIPT$(24) = UNPACK$(MID$(A$,40,1))    ! Benefit trailer 1 font

  BENEFIT.RECEIPT$(25) = MID$(A$,41,38)            ! Benefit trailer 2
  BENEFIT.RECEIPT$(26) = UNPACK$(MID$(A$,79,1))    ! Benefit trailer 2 linefeed
  BENEFIT.RECEIPT$(27) = UNPACK$(MID$(A$,80,1))    ! Benefit trailer 2 font

  BENEFIT.RECEIPT$(28) = MID$(A$,81,38)            ! Benefit trailer 3
  BENEFIT.RECEIPT$(29) = UNPACK$(MID$(A$,119,1))   ! Benefit trailer 3 linefeed
  BENEFIT.RECEIPT$(30) = UNPACK$(MID$(A$,120,1))   ! Benefit trailer 3 font

  DIM REDEMPTION.RECEIPT$(0)
  DIM REDEMPTION.RECEIPT$(33)

  A$ = AGENCY.RECPT.RDPT.HDR$(CURR.REC)

  REDEMPTION.RECEIPT$(1) = LEFT$(A$,38)             ! Redem Header Line 1
  REDEMPTION.RECEIPT$(2) = UNPACK$(MID$(A$,39,1))   ! Redem Header Line 1 linef
  REDEMPTION.RECEIPT$(3) = UNPACK$(MID$(A$,40,1))   ! Redem Header Line 1 font

  REDEMPTION.RECEIPT$(4) = MID$(A$,41,38)           ! Redem Header Line 2
  REDEMPTION.RECEIPT$(5) = UNPACK$(MID$(A$,79,1))   ! Redem Header Line 2 linef
  REDEMPTION.RECEIPT$(6) = UNPACK$(MID$(A$,80,1))   ! Redem Header Line 2 font

  REDEMPTION.RECEIPT$(7) = MID$(A$,81,38)           ! Redem Header Line 3
  REDEMPTION.RECEIPT$(8) = UNPACK$(MID$(A$,119,1))  ! Redem Header Line 3 linef
  REDEMPTION.RECEIPT$(9) = UNPACK$(MID$(A$,120,1))  ! Redem Header Line 3 font

  REDEMPTION.RECEIPT$(10) = MID$(A$,121,38)         ! Redem Header Line 4
  REDEMPTION.RECEIPT$(11) = UNPACK$(MID$(A$,159,1)) ! Redem Header Line 4 linef
  REDEMPTION.RECEIPT$(12) = UNPACK$(MID$(A$,160,1)) ! Redem Header Line 4 font

  REDEMPTION.RECEIPT$(13) = MID$(A$,161,38)         ! Redem Header Line 5
  REDEMPTION.RECEIPT$(14) = UNPACK$(MID$(A$,199,1)) ! Redem Header Line 5 linef
  REDEMPTION.RECEIPT$(15) = UNPACK$(MID$(A$,200,1)) ! Redem Header Line 5 font

  REDEMPTION.RECEIPT$(16) = MID$(A$,201,38)         ! Redem Header Line 6
  REDEMPTION.RECEIPT$(17) = UNPACK$(MID$(A$,239,1)) ! Redem Header Line 6 linef
  REDEMPTION.RECEIPT$(18) = UNPACK$(MID$(A$,240,1)) ! Redem Header Line 6 font

  A$ = AGENCY.RECPT.RDPT.ITM$(CURR.REC)

  REDEMPTION.RECEIPT$(19) = LEFT$(A$,38)              ! Redem Item  Line 1
  REDEMPTION.RECEIPT$(20) = UNPACK$(MID$(A$,39,1))    ! Redem Item  Line 1 linef
  REDEMPTION.RECEIPT$(21) = UNPACK$(MID$(A$,40,1))    ! Redem Item  Line 1 font

  REDEMPTION.RECEIPT$(22) = MID$(A$,41,38)            ! Redem Item  Line 2
  REDEMPTION.RECEIPT$(23) = UNPACK$(MID$(A$,79,1))    ! Redem Item  Line 2 linef
  REDEMPTION.RECEIPT$(24) = UNPACK$(MID$(A$,80,1))    ! Redem Item  Line 2 font

  REDEMPTION.RECEIPT$(25) = MID$(A$,81,38)            ! Redem Item  Line 3
  REDEMPTION.RECEIPT$(26) = UNPACK$(MID$(A$,119,1))   ! Redem Item  Line 3 linef
  REDEMPTION.RECEIPT$(27) = UNPACK$(MID$(A$,120,1))   ! Redem Item  Line 3 font

  A$ = AGENCY.RECPT.RDPT.TRL$(CURR.REC)

  REDEMPTION.RECEIPT$(28) = LEFT$(A$,38)            ! Redem Trailer Line 1
  REDEMPTION.RECEIPT$(29) = UNPACK$(MID$(A$,39,1))  ! Redem Trailer Line 1 linef
  REDEMPTION.RECEIPT$(30) = UNPACK$(MID$(A$,40,1))  ! Redem Trailer Line 1 font

  REDEMPTION.RECEIPT$(31) = MID$(A$,41,38)          ! Redem Trailer Line 2
  REDEMPTION.RECEIPT$(32) = UNPACK$(MID$(A$,79,1))  ! Redem Trailer Line 2 linef
  REDEMPTION.RECEIPT$(33) = UNPACK$(MID$(A$,80,1))  ! Redem Trailer Line 2 font

  A$ = ""                                             ! cleanup

END SUB                                          ! end PARSE.AGENCY.DATA
!******************************************************************************


!******************************************************************************
! Subroutine Name: SAVE.AGENCY.DATA
! Description: This routine puts the Agency data back in data array.
!******************************************************************************
SUB SAVE.AGENCY.DATA(REC)

  INTEGER*2                            \
            REC                        ! record into data array

  A$ = ""

  A$ = RIGHT$("  " + STATE.ID$,2)
  A$ = A$ + RIGHT$(" " + ACTIVE.FLAG$,1)
  A$ = A$ + PACK$(RIGHT$(STRING$(6,"0") + BIN.NUMBER$,6))
  A$ = A$ + RIGHT$(STRING$(15," ") + MERCHANT.ID$,15)
  A$ = A$ + PACK$(RIGHT$(STRING$(12,"0") + FORWARD.INSTITUTE$,12))
  A$ = A$ + PACK$(RIGHT$(STRING$(12,"0") + RECEIVE.INSTITUTE$,12))

  AGENCY.MAIN.OPTION$(REC) = A$                              ! Add new data

  A$ = LEFT$(BENEFIT.RECEIPT$(1) + STRING$(38," "),38)      ! Benefit hdr1
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(2),1))      ! Benefit hdr1 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(3),1))      ! Benefit hdr1 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(4) + STRING$(38," "),38) ! Benefit hdr2
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(5),1))      ! Benefit hdr2 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(6),1))      ! Benefit hdr2 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(7) + STRING$(38," "),38) ! Benefit hdr3
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(8),1))      ! Benefit hdr3 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(9),1))      ! Benefit hdr3 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(10) + STRING$(38," "),38)! Benefit hdr4
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(11),1))     ! Benefit hdr4 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(12),1))     ! Benefit hdr4 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(13) + STRING$(38," "),38)! Benefit hdr5
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(14),1))     ! Benefit hdr5 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(15),1))     ! Benefit hdr5 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(16) + STRING$(38," "),38)! Benefit hdr6
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(17),1))     ! Benefit hdr6 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(18),1))     ! Benefit hdr6 font

  AGENCY.RECPT.BNFT.HDR$(REC) = A$

  A$ = ""

  A$ = LEFT$(BENEFIT.RECEIPT$(19) + STRING$(38," "),38)  ! Benefit item  1
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(20),1))  ! Benefit item  1 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(21),1))  ! Benefit item  1 font

  AGENCY.RECPT.BNFT.ITM$(REC) = A$

  A$ = ""

  A$ = LEFT$(BENEFIT.RECEIPT$(22) + STRING$(38," "),38)    ! Ben trailer 1
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(23),1))    ! Ben trailer 1 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(24),1))    ! Ben trailer 1 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(25) + STRING$(38," "),38)  ! Ben trailer 2
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(26),1))    ! Ben trailer 2 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(27),1))    ! Ben trailer 2 font

  A$ = A$ + LEFT$(BENEFIT.RECEIPT$(28) + STRING$(38," "),38)  ! Ben trailer 3
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(29),1))    ! Ben trailer 3 linef
  A$ = A$ + PACK$(RIGHT$("0" + BENEFIT.RECEIPT$(30),1))    ! Ben trailer 3 font

  AGENCY.RECPT.BNFT.TRL$(REC) = A$

  A$ = ""

  A$ = LEFT$(REDEMPTION.RECEIPT$(1) + STRING$(38," "),38)      ! Red Hdr Line 1
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(2),1)) ! Red Hdr Line 1 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(3),1)) ! Red Hdr Line 1 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(4) + STRING$(38," "),38) ! Red Hdr Line 2
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(5),1)) ! Red Hdr Line 2 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(6),1)) ! Red Hdr Line 2 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(7) + STRING$(38," "),38) ! Red Hdr Line 3
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(8),1)) ! Red Hdr Line 3 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(9),1)) ! Red Hdr Line 3 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(10) + STRING$(38," "),38)! Red Hdr Line 4
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(11),1))! Red Hdr Line 4 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(12),1))! Red Hdr Line 4 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(13) + STRING$(38," "),38)! Red Hdr Line 5
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(14),1))! Red Hdr Line 5 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(15),1))! Red Hdr Line 5 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(16) + STRING$(38," "),38)! Red Hdr Line 6
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(17),1))! Red Hdr Line 6 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(18),1))! Red Hdr Line 6 font

  AGENCY.RECPT.RDPT.HDR$(REC) = A$

  A$ = ""

  A$ = LEFT$(REDEMPTION.RECEIPT$(19) + STRING$(38," "),38)! Red Item Line 1
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(20),1))! Red Item Line 1 line
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(21),1))! Red Item Line 1 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(22) + STRING$(38," "),38)! Red Item Line 2
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(23),1))! Red Item Line 2 line
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(24),1))! Red Item Line 2 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(25) + STRING$(38," "),38)! Red Item Line 3
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(26),1))! Red Item Line 3 line
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(27),1))! Red Item Line 3 font

  AGENCY.RECPT.RDPT.ITM$(REC) = A$

  A$ = ""

  A$ = LEFT$(REDEMPTION.RECEIPT$(28) + STRING$(38," "),38)! Red Trlr Line 1
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(29),1))! Red Trlr Line 1linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(30),1))! Red Trlr Line 1 font

  A$ = A$ + LEFT$(REDEMPTION.RECEIPT$(31) + STRING$(38," "),38)! Red Trlr Line 2
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(32),1))! Red Trlr Line2 linef
  A$ = A$ + PACK$(RIGHT$("0" + REDEMPTION.RECEIPT$(33),1))! Red Trlr Line 2 font

  AGENCY.RECPT.RDPT.TRL$(REC) = A$

  A$ = ""                                        ! cleanup

END SUB                                          ! end SAVE.AGENCY.DATA
!******************************************************************************


!******************************************************************************
! Function Name: NUMBER.STATE.CODES
! Description: This function counts the number of valid state codes for
!              validation
!******************************************************************************
FUNCTION NUMBER.STATE.CODES(NUM.AGNCY)

  STRING                               \
         STATECODE$                    ! local work variable

  INTEGER*2                            \
            CNT%,                      \ count of valid State Codes
            IDX%,                      \ loop variable
            NUM.AGNCY,                 \ number of current agencies
            NUMBER.STATE.CODES         ! function return

  CNT% = 0

  ! count all valid State Codes
  FOR IDX% = 1 TO NUM.AGNCY
    STATECODE$ =                      \
    STRIP.LEAD.TRAIL.CHARS$(CHR$(0),LEFT$(AGENCY.MAIN.OPTION$(IDX%),2))
    IF (STATECODE$ <> "") THEN BEGIN
      CNT% = CNT% + 1
    ENDIF
  NEXT IDX%

  NUMBER.STATE.CODES = CNT%
  STATECODE$ = ""                                ! cleanup

END FUNCTION                                     ! NUMBER.STATE.CODES
!******************************************************************************


!******************************************************************************
! Subroutine Name: MMCFEC01
! Description: This routine is displays the feature screen and data.
!******************************************************************************
SUB MMCFEC01 PUBLIC

  STRING               \
    WORK$

  DM.SCRNNO = SCREEN.NUM                         ! used for error logging
  CALL MMCFEC05(DISPD(SCREEN.NUM), "WE 2")       ! display screen

  CALL MMCFEC05(POSF(150),"WE 3")                ! put up "Page xxx of xxx"
  CALL MMCFEC05(PUTF(STR$(PAGE)),"WE 7")
  CALL MMCFEC05(POSF(151),"WE 3")
  CALL MMCFEC05(PUTF(STR$(MAX.PAGE)),"WE 7")

  ON SCREEN.NUM GOSUB PAGE1,PAGE2,PAGE3,PAGE4

  EXIT SUB

!##############################################################################
PAGE1:

  CALL MMCFEC05(POSF(1),"WE 3")
  CALL MMCFEC05(PUTF(NUM.AGENCIES$),"WE 7")! display current number of agencies

  CALL MMCFEC05(POSF(2),"WE 3")
  CALL MMCFEC05(PUTF(WIC.TENDER$),"WE 7")  ! display current wic tender

  CALL MMCFEC05(POSF(3),"WE 3")
  CALL MMCFEC05(PUTF(DUP.HDR1$),"WE 7")    ! display duplicate header 1

  CALL MMCFEC05(POSF(4),"WE 3")
  CALL MMCFEC05(PUTF(DH1.LF$),"WE 7")      ! display duplicate header 1 linefeed

  CALL MMCFEC05(POSF(5),"WE 3")
  CALL MMCFEC05(PUTF(DH1.FONT$),"WE 7")    ! display duplicate header 1 font

  CALL MMCFEC05(POSF(6),"WE 3")
  CALL MMCFEC05(PUTF(BNA.HDR1$),"WE 7")    ! disp "Ben Not Avail" header 1

  CALL MMCFEC05(POSF(7),"WE 3")
  CALL MMCFEC05(PUTF(BNA1.LF$),"WE 7")     ! disp "Ben Not Avail" header 1 linef

  CALL MMCFEC05(POSF(8),"WE 3")
  CALL MMCFEC05(PUTF(BNA1.FONT$),"WE 7")   ! disp "Ben Not Avail" header 1 font

  CALL MMCFEC05(POSF(9),"WE 3")
  CALL MMCFEC05(PUTF(BNA.HDR2$),"WE 7")    ! disp "Ben Not Avail" header 2

  CALL MMCFEC05(POSF(10),"WE 3")
  CALL MMCFEC05(PUTF(BNA2.LF$),"WE 7")     ! disp "Ben Not Avail" header 2 linef

  CALL MMCFEC05(POSF(11),"WE 3")
  CALL MMCFEC05(PUTF(BNA2.FONT$),"WE 7")   ! disp "Ben Not Avail" header 2 font

  CALL MMCFEC05(POSF(12),"WE 3")
  CALL MMCFEC05(PUTF(DAYLIGHT.SAVING$),"WE 7")! display daylight saving observed

  CALL MMCFEC05(POSF(13),"WE 3")
  CALL MMCFEC05(PUTF(GMT.DIFF$),"WE 7")          ! display gmt difference
  RETURN

!##############################################################################
PAGE2:

  CALL MMCFEC05(POSF(1),"WE 3")
  CALL MMCFEC05(PUTF(STATE.ID$),"WE 7")   ! display the State ID for the agency

  CALL MMCFEC05(POSF(2),"WE 3")
  CALL MMCFEC05(PUTF(ACTIVE.FLAG$),"WE 7")! display active flag for the agency

  CALL MMCFEC05(POSF(3),"WE 3")
  CALL MMCFEC05(PUTF(BIN.NUMBER$),"WE 7")        ! display the BIN number

  CALL MMCFEC05(POSF(4),"WE 3")
  CALL MMCFEC05(PUTF(MERCHANT.ID$),"WE 7")       ! display the merchant ID

  CALL MMCFEC05(POSF(5),"WE 3")
  CALL MMCFEC05(PUTF(FORWARD.INSTITUTE$),"WE 7")! disp forwarding inst of agency

  CALL MMCFEC05(POSF(6),"WE 3")
  CALL MMCFEC05(PUTF(RECEIVE.INSTITUTE$),"WE 7") ! disp receiving inst of agency

  RETURN

!##############################################################################
PAGE3:

  CALL MMCFEC05(POSF(100),"WE 3")                ! put up the state code
  CALL MMCFEC05(PUTF(STATE.ID$),"WE 7")

  CALL MMCFEC05(POSF(101),"WE 3")                ! put up the BIN number
  CALL MMCFEC05(PUTF(BIN.NUMBER$),"WE 7")

  ! Display all the Benefit Receipt data
  FOR I% = 1 TO 30
    CALL MMCFEC05(POSF(I%),"WE 3")
    CALL MMCFEC05(PUTF(BENEFIT.RECEIPT$(I%)),"WE 7")
  NEXT I%

  RETURN

!##############################################################################
PAGE4:

  CALL MMCFEC05(POSF(100),"WE 3")                ! put up the state code
  CALL MMCFEC05(PUTF(STATE.ID$),"WE 7")

  CALL MMCFEC05(POSF(101),"WE 3")                ! put up the BIN number
  CALL MMCFEC05(PUTF(BIN.NUMBER$),"WE 7")

  ! Display all the Redemption Receipt data
  FOR I% = 1 TO 33
    CALL MMCFEC05(POSF(I%),"WE 3")
    CALL MMCFEC05(PUTF(REDEMPTION.RECEIPT$(I%)),"WE 7")
  NEXT I%

  IF PAGE < MAX.PAGE THEN BEGIN
    CALL MMCFEC05(POSF(120),"EL 3")              ! put up pgdn prompt
    WORK$ = SETF(VISIBLE$)
  ENDIF

  RETURN

END SUB                                          ! end MMCFEC01
!******************************************************************************


!******************************************************************************
! Subroutine Name: MMCFEC02
! Description: This routine handles function key input.
!******************************************************************************
SUB MMCFEC02 PUBLIC

  CALL MMCFEC03(109,"")                    ! Log "invalid function key" message

END SUB                                    ! end MMCFEC02
!******************************************************************************


!******************************************************************************
! Subroutine Name: HANDLE.USER.INPUT
! Description: This routine gets the user input, determining option changes.
!******************************************************************************
SUB HANDLE.USER.INPUT

  STRING                          \
         SAVE.NUM.AGENCIES$       ! save variable NUM.AGENCIES$ for compare

  INTEGER*2                       \
            SAVE.PAGE,            \ save common function variable PAGE
            LAST.AGENCY.REC,      \ save the agency record number for compare
            DATA.NOT.OK           ! loop variable used to test if input data ok

  MAX.PAGE = (NUM.AGENCIES% * 3) + 1
  LAST.AGENCY.REC = 0

  WHILE NOT (QUIT.PRESSED OR               \ while user still editing options
             ESC.PRESSED OR                \
             ENTER.PRESSED)

    IF (PAGE = 1) THEN BEGIN               ! if first page
      SCREEN.NUM = 1                       ! main option screen
      MAX.HELP = 2                         ! 1 help screen
      HELP.SCR.NO = 101                    ! help screen number
      LAST.FIELD = 13                      ! set last field on this page
      IF (LAST.AGENCY.REC > 0) THEN BEGIN  ! if user went through agency records
        CALL SAVE.AGENCY.DATA(CURR.AGENCY.REC)   ! save the data
        LAST.AGENCY.REC = 0      ! reset for another pass through agency records
      ENDIF                      ! if user went through agency records

    ENDIF ELSE BEGIN                             ! else not first page
      SCREEN.NUM = MOD((PAGE - 1),3)             ! calculate the screen number
      IF (SCREEN.NUM = 0) THEN BEGIN             ! if third page
        SCREEN.NUM = 3                           ! mod will set third to zero
      ENDIF                                      ! if third page

      ! Now calculate the current agency record number
      CURR.AGENCY.REC = (((PAGE - 1) - SCREEN.NUM) / 3) + 1

      IF (CURR.AGENCY.REC <> LAST.AGENCY.REC)    \ new record we're looking at
      THEN BEGIN

        IF (LAST.AGENCY.REC > 0) THEN BEGIN      ! if not first time
          CALL SAVE.AGENCY.DATA(LAST.AGENCY.REC) ! save the data
        ENDIF                                    ! if not first time

        ! parse the current agency record data into work variables
        CALL PARSE.AGENCY.DATA(CURR.AGENCY.REC)

        LAST.AGENCY.REC = CURR.AGENCY.REC        ! save current record number
      ENDIF                                      ! a new record we're looking at

      SCREEN.NUM = SCREEN.NUM + 1                ! increment to correct screen
      IF (SCREEN.NUM = 2) THEN BEGIN
        LAST.FIELD = 6
        MAX.HELP = 1                               ! only 1 help screen
        HELP.SCR.NO = 110                          ! help screen number
      ENDIF ELSE IF (SCREEN.NUM = 3) THEN BEGIN
        LAST.FIELD = 30
        MAX.HELP = 2                               ! only 2 help screens
        HELP.SCR.NO = 120                          ! help screen number
      ENDIF ELSE IF (SCREEN.NUM = 4) THEN BEGIN
        LAST.FIELD = 33
        MAX.HELP = 3                               ! only 3 help screens
        HELP.SCR.NO = 130                          ! help screen number
      ENDIF
    ENDIF                                        ! if first page

    YESORNO = OFF0                               ! majority not yes/no field
    NEED.RANGE.CHECK = ON1                       ! assume  need a range check

    IF (PAGE <> LAST.PAGE) THEN BEGIN            ! different screen
      CALL MMCFEC01                              ! display screen
      LAST.PAGE = PAGE
    ENDIF                                        ! different screen

    CALL MMCFEC05(POSF(CURRENT.FIELD),"WE 3")    ! Position cursor

    IF (PAGE = 1) THEN BEGIN                     ! if main option screen

      IF (CURRENT.FIELD = 1) THEN BEGIN          ! if number of agencies field
        LOW.END$ = "0"
        HIGH.END$ = STR$(MAX.AGENCIES)
        SAVE.NUM.AGENCIES$ = NUM.AGENCIES$       ! save for compare after edit
        SAVE.PAGE = PAGE
        DATA.NOT.OK = -1                         ! loop to make sure data ok
        WHILE DATA.NOT.OK                        ! trying to get valid data
          DATA.NOT.OK = 0                        ! reset loop flag
          CALL MMCFEC04(NUM.AGENCIES$)           ! Get operator input
          IF (NUM.AGENCIES$ <> SAVE.NUM.AGENCIES$) \
          THEN BEGIN                             ! change number of agencies
            I% = VAL(NUM.AGENCIES$)              ! get modified num of agencies
            K% = VAL(SAVE.NUM.AGENCIES$)         ! get the original
            IF (NUMBER.STATE.CODES(K%) > I%)     \ user reduced num of agencies
            THEN BEGIN
              DATA.NOT.OK = -1                   ! need to loop around again
              NUM.AGENCIES$ = SAVE.NUM.AGENCIES$
              PAGE = SAVE.PAGE
              CALL MMCFEC01                      ! redisplay screen
              A$ = WIC.MSG$(1)
              CALL MMCFEC03(0,A$)                ! display msg on line 23
              ! reposition curser
              CALL MMCFEC05(POSF(CURRENT.FIELD),"WE 3")
            ENDIF ELSE BEGIN                     ! else didnt reduce state codes
              NUM.AGENCIES% = VAL(NUM.AGENCIES$) ! set to integer format
              MAX.PAGE = (NUM.AGENCIES% * 3) + 1 ! reset the max number of pages
              IF (PAGE = LAST.PAGE) THEN BEGIN   ! if user did not page down
                CALL MMCFEC01                    ! redisplay page
              ENDIF                              ! if user did not page down
            ENDIF                                ! if reduced number of agencies
          ENDIF                                  ! op changed number of agencies
        WEND                                     ! while trying to get val data
      ENDIF ELSE IF (CURRENT.FIELD = 2)          \ else if WIC tender field
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "66"
        DATA.NOT.OK = -1                         ! loop to make sure data ok
        WHILE DATA.NOT.OK                        ! while trying to get val data
          DATA.NOT.OK = 0                        ! reset loop flag
          CALL MMCFEC04(WIC.TENDER$)             ! get operator input
          WIC.TENDER$ = RIGHT$("00" + WIC.TENDER$,2)
          CALL MMCFEC01                          ! redisplay screen
          A$ = LEFT$(WIC.TENDER$,1)
          B$ = RIGHT$(WIC.TENDER$,1)
          IF (((A$ = "0") AND (B$ > "0")) OR     \  data is not in proper range
              (B$ = "0") OR (B$ > "6")) AND      \
             (WIC.TENDER$ <> "00")               \
          THEN BEGIN
            DATA.NOT.OK = -1                     ! force another loop
            A$ = WIC.MSG$(2)
            CALL MMCFEC03(0,A$)                  ! display msg on line 23
            ! reposition curser
            CALL MMCFEC05(POSF(CURRENT.FIELD),"WE 3")
          ENDIF                           ! if data is not in proper range
        WEND                              ! while trying to get valid data
      ENDIF ELSE IF (CURRENT.FIELD = 3)   \ Duplicate header 1
      THEN BEGIN
        NEED.RANGE.CHECK = OFF0
        CALL MMCFEC04(DUP.HDR1$)          ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 4)   \ Duplicate header 1 Linefeed
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "9"
        CALL MMCFEC04(DH1.LF$)            ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 5)   \ Duplicate header 1 font
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "7"
        CALL MMCFEC04(DH1.FONT$)          ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 6)   \ "Benefits Not Available" header 1
      THEN BEGIN
        NEED.RANGE.CHECK = OFF0
        CALL MMCFEC04(BNA.HDR1$)          ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 7)   \ "Not Available" header 1 Linefeed
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "9"
        CALL MMCFEC04(BNA1.LF$)           ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 8)   \ "Not Available header 1 font
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "7"
        CALL MMCFEC04(BNA1.FONT$)         ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 9)   \ "Not Available" header 2
      THEN BEGIN
        NEED.RANGE.CHECK = OFF0
        CALL MMCFEC04(BNA.HDR2$)          ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 10)  \ "Not Available" header 2 Linefeed
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "9"
        CALL MMCFEC04(BNA2.LF$)           ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 11)  \ "Not Available header 2 font
      THEN BEGIN
        LOW.END$ = "0"
        HIGH.END$ = "7"
        CALL MMCFEC04(BNA2.FONT$)         ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 12)  \ if State ID active flag
      THEN BEGIN
        YESORNO = ON1
        CALL MMCFEC04(DAYLIGHT.SAVING$)   ! Get operator input
      ENDIF ELSE IF (CURRENT.FIELD = 13)  \ "Not Available header 2 font
      THEN BEGIN
        LOW.END$ = "1"
        HIGH.END$ = "23"
        CALL MMCFEC04(GMT.DIFF$)                 ! Get operator input
      ENDIF
    ENDIF ELSE BEGIN                             ! else not main option screen
      IF (SCREEN.NUM = 2) THEN BEGIN             ! if agency options screen
        IF (CURRENT.FIELD = 1) THEN BEGIN        ! if State ID field
          NEED.RANGE.CHECK = OFF0
          CALL MMCFEC04(STATE.ID$)               ! Get operator input
          STATE.ID$ = UCASE$(STATE.ID$)
          CALL MMCFEC05(POSF(1),"WE 3")
          CALL MMCFEC05(PUTF(STATE.ID$),"WE 7")  ! disp State ID for the agency
        ENDIF ELSE IF (CURRENT.FIELD = 2)        \ if State ID active flag
        THEN BEGIN
          YESORNO = ON1
          CALL MMCFEC04(ACTIVE.FLAG$)            ! Get operator input
        ENDIF ELSE IF (CURRENT.FIELD = 3)        \ if BIN number field
        THEN BEGIN                               !
          LOW.END$ = "1"                         ! this field is not being used
          HIGH.END$ = "999999"
          CALL MMCFEC04(BIN.NUMBER$)             ! Get operator input
        ENDIF ELSE IF (CURRENT.FIELD = 4)        \ Merchant ID
        THEN BEGIN
          NEED.RANGE.CHECK = OFF0
          CALL MMCFEC04(MERCHANT.ID$)            ! Get operator input
        ENDIF ELSE IF (CURRENT.FIELD = 5)        \ Forwarding Institution ID
        THEN BEGIN
          LOW.END$ = "1"
          HIGH.END$ = "99999999999"
          CALL MMCFEC04(FORWARD.INSTITUTE$)      ! Get operator input
        ENDIF ELSE IF (CURRENT.FIELD = 6)        \ Receiving Institution ID
        THEN BEGIN
          LOW.END$ = "1"
          HIGH.END$ = "99999999999"
          CALL MMCFEC04(RECEIVE.INSTITUTE$)      ! Get operator input
        ENDIF
      ENDIF ELSE BEGIN                           ! else not agency opt screen
        I% = MOD(CURRENT.FIELD,3)                ! calculate to set ranges
        IF (I% = 1) THEN BEGIN                   ! if text field
          NEED.RANGE.CHECK = OFF0                ! no range check
        ENDIF ELSE IF (I% = 2) THEN BEGIN        ! if line feed field
          LOW.END$ = "0"
          HIGH.END$ = "9"
        ENDIF ELSE BEGIN                         ! else must be font field
          LOW.END$ = "0"
          HIGH.END$ = "7"
        ENDIF                                    ! if text field
        IF (SCREEN.NUM = 3) THEN BEGIN           ! if Benefit Receipt layout scr
          CALL MMCFEC04(BENEFIT.RECEIPT$(CURRENT.FIELD))
        ENDIF ELSE BEGIN                      ! else must be Red Rcpt layout scr
          CALL MMCFEC04(REDEMPTION.RECEIPT$(CURRENT.FIELD))
        ENDIF                                    ! if Benefit Receipt layout scr
      ENDIF                                      ! if agency options screen
    ENDIF                                        ! if main option screen

    IF (PGUP.PRESSED OR PGDN.PRESSED) THEN BEGIN ! if page up/down pressed
      CURRENT.FIELD = FIRST.FIELD
    ENDIF ELSE BEGIN                             ! else not page up/down
      IF TAB.PRESSED THEN BEGIN                  ! if tab pressed
        CURRENT.FIELD = CURRENT.FIELD + 1
        IF (CURRENT.FIELD > LAST.FIELD) THEN BEGIN
          CURRENT.FIELD = FIRST.FIELD
        ENDIF
      ENDIF ELSE BEGIN                           ! else tab not pressed
        IF BACKTAB.PRESSED THEN BEGIN            ! if backtab pressed
          CURRENT.FIELD = CURRENT.FIELD - 1
          IF (CURRENT.FIELD < FIRST.FIELD) THEN BEGIN
            CURRENT.FIELD = LAST.FIELD
          ENDIF
        ENDIF ELSE BEGIN                         ! else backtab not pressed
          IF END.PRESSED THEN BEGIN              ! if end pressed
            CURRENT.FIELD = LAST.FIELD
          ENDIF ELSE BEGIN                       ! else end not pressed
            IF HOME.PRESSED THEN BEGIN           ! if home pressed
              CURRENT.FIELD = FIRST.FIELD
            ENDIF                                ! if home pressed
          ENDIF                                  ! if end pressed
        ENDIF                                    ! if backtab pressed
      ENDIF                                      ! if tab pressed
    ENDIF                                        ! if page up/down pressed

  WEND                                           ! user still editing options

  IF ENTER.PRESSED THEN BEGIN                    ! if user pressed enter

    IF (LAST.AGENCY.REC > 0) THEN BEGIN          ! went through agency records
      CALL SAVE.AGENCY.DATA(CURR.AGENCY.REC)     ! save the data
    ENDIF                                        ! went through agency records

    CALL WRITE.OPTION.DATA                       ! write the option data
    CALL NOTIFY.TERMINALS                        ! notify terms to load options
  ENDIF                                          ! if user pressed enter

  CLOSE EAMWEOPT.SN                              ! close EAMWEOPT.DAT

  IF ESC.PRESSED THEN BEGIN                      ! if escape pressed
    A$ = "EAMMM00L.286"                          ! return to SA main menu
  ENDIF ELSE BEGIN                               ! else escape not pressed
    A$ = "EAMAPM1L.286"                          ! return to pers main menu
  ENDIF                                          ! if escape pressed

  CALL CHAIN.BACK(A$)                            ! chain back to required module

END SUB                                          ! end HANDLE.USER.INPUT
!******************************************************************************


!********************************** MAIN LINE *********************************

  ERR.RESUME = 0                                 ! make sure resume flag is init

  ON ERROR GOTO WIC.ERROR                        ! mainline error handler

  CALL ACCESS.CHAIN.PARAMETERS                   ! handle program chaining

  CALL INITIALIZE.VARIABLES                      ! initialize program variables

  CALL INITIALIZE.DISPLAY.MANAGER                ! initialize display manager

  CALL OPEN.READ.OPTION.FILE                     ! open and read EAMWEOPT.DAT

  CALL HANDLE.USER.INPUT                         ! handle user input

  STOP                                           ! ensure program ends

!******************************************************************************
! Error Handling
!******************************************************************************

WIC.ERROR:

  WIC.ERR$ = ERR
  WIC.ERRF% = ERRF%
  WIC.ERRN% = ERRN
  WIC.ERRN$ = ERRNHEX$                           ! Convert error code

  IF ERR.RESUME THEN BEGIN                       ! flag set to resume from error
    ERR.RESUME = 0                               ! reset to flag error occurred
    RESUME                                       ! resume back to
  ENDIF                                          ! flag set to resume from error

  IF (WIC.ERR$ = "CU") THEN BEGIN                ! Tried to close a session
    RESUME                                       ! number that is not active
  ENDIF

  TERM      = 0         !**********************************!
  MSGGRP    = ASC("B")  !                                  !
  MSGNUM    = 211       ! variables used to log the error  !
  SVRTY     = 1         !                                  !
  EVENT     = 0         !**********************************!

  IF ((WIC.ERR$ = "OE") AND (WIC.ERRN$ = PACK$("8020400C"))) OR           \
      (WIC.ERR$ = "ME")                                                   \
  THEN BEGIN                          ! log if an error occurs during an open or
    MSGNUM = 214                      ! write to a file.  File may be in LOCKED
    SVRTY = 3                         ! mode or being used by another process.
  ENDIF ELSE BEGIN
    IF (WIC.ERRN% > 0) THEN BEGIN     ! If an unexpected error occurs causing
      MSGNUM = 213                    ! abnormal termination, (i.e. code error)
    ENDIF ELSE BEGIN
      IF (WIC.ERR$ = "OE") THEN BEGIN
        MSGNUM = 215                  ! File not found
      ENDIF
    ENDIF
  ENDIF

  ! Set up the unique section of the log entry.
  UNIQ$ = "WE"                                   \ Module id
        + RIGHT$("00" + STR$(WIC.ERRF%), 2)      \ ERRF%  session number
        + RIGHT$("  " + WIC.ERR$, 2)             \ ERR    error
        + WIC.ERRN$                              ! ERRN   error code

  ! Log error
  RET.VAL = ADXERROR(TERM, MSGGRP, MSGNUM, SVRTY, EVENT, UNIQ$)

END
