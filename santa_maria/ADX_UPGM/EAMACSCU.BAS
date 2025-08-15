\/* TIME STAMP BLOCK ***************************************************
\** END OF TIME STAMP BLOCK *******************************************/
\/*********************************************************************/
\/*                                                                   */
\/*       Module Name:  EAMACSCU                                      */
\/*                                                                   */
\/*       Description:  User Exits For Auto Store Close               */
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"        */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED        */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT      */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                           */
\/*                                                                   */
\/*      STATUS: RELEASE 2 LEVEL 0                                    */
\/*                                                                   */
\/*      Author: CHARLIE JOHNSON                                      */
\/****************************************************************


%INCLUDE EAMCFINC.J86           ! Common functions variables
%INCLUDE EAMSOPTS.J86           ! Store options
%INCLUDE EAMACCTC.J86           ! Accounting file
%INCLUDE EAMTERMS.J86           ! Terminal status file
%INCLUDE EAMOSTAT.J86           ! Operator status file
%INCLUDE EAMSSDEF.J86           ! Store close record field definitions
%INCLUDE EAMTLDEF.J86           ! Summary log close string
%INCLUDE EAMSRKYC.J86           ! Sus/Ret keyed index file
%INCLUDE EAMEJOPT.J86           ! Journal Options

\ REM!!*********************** CONSTANTS START ********************!!

STRING GLOBAL                   \
                 FIELD$,        \ Close string to Summary Log
                 TS.INDICAT3$,  \ Auto Close flags in close string
                 OLD.FILES$(1), \ Array of names of old period files
                 MESSAGE$,      \ Message to be passed to ADXSERVE
                 BACKGRND.MESS$(1),\
                 SPL.ARR(2),    \ array of despool status
                 ACCOUNT$,      \
                 ENTRY,         \
                 NAM$(1),       \ Array containing procedure names
                 WORK$,         \
                 PARM2,         \ used with ADXFILES
                 UNIQ$,         \ Used with ADXERROR
                 TEMP$,         \
                 INACTIVE$,     \
                 LOG.FILE$,     \ log file name for close string
                 FILE.NAME$,    \ Name of scanned file
                 TLOG.NAME$,    \ 1st 6 characters of transaction log name
                 NEXT.OLD.TLOG$,\ Name of the =ext old period tlog to create
                 CLOSE.DATETIME$,\ Date and time of this close
                 CR.LF$,        \ Carriage return & line feed
                 RET$,          \
                 SCR1.DATE$(1), \ date/time of last close for this day
                 SCR1.PROC$(1), \ procdure to perform
                 SCR1.ACCT$(1), \ accounting procedures
                 SCR3.WAIT$(1), \ procedure to wait for operator intervention
                 MM11PCMP$,     \
                 ACSCWERR$,     \ On error return code
                 K$             ! String used to read in status files

INTEGER*1 GLOBAL                \
                 FIRST.PASS,    \ Used for automatic reports
                 CLOSE.COMPLETE,\
                 AUTO.CLS,      \
                 CLOSE.START,   \
                 SIGN.ON.OFF.SET,\
                 ERROR.FLG,     \
                 REC.NUM,       \ Used for automatic reports
                 PERIOD.CHOICE, \ Used for automatic reports
                 PERF.RPT,      \
                 VALID,         \
                 PM.ENABLE,     \ Enable Elec. Mktg. feature exits
                 ACCTP.OPEN,    \ Accounting previous file open
                 SO.AR.OT.RECONCIL, \Operator/Terminal Reconcile Report flag
                 SO.AR.ST.RECONCIL, \  Store Reconcile Report flag
                 SO.AR.FLAG,    \
                 LOCK.OK,       \
                 ADDED.TO.REPORTS,      \  Flag if we added to eamarrpt
                 EP.ENABLE,     \ EPS feature installed and enabled flag.
                 SUSPEND.OK,    \
                 TEST.BYTE,     \
                 REC.FLAG,      \
                 OLD.TLOG.COUNT,\
                 SVRTY,         \ Used with ADXERROR
                 MSGGRP,        \ Used with ADXERROR
                 EVENT          ! Used with ADXERROR

INTEGER*2 GLOBAL                \
                 CLOSE.SELECTED,\ Type of close selected
                 IGNORE.ERROR,  \ Flag indicating appl wants to process error
                 CLOSE.ACTION,  \ Action to perform on close period
                                \ for each file
                 CNT,           \ Counter for # of records copied from
                                \ the previous log
                 PREV.STORE.UNRECONCILED, \ Previous store rec unreconciled
                 VALID.PROC(1),         \ Array of flags to say if procedure is defined
                 COPY.FLAG(1),          \ Array of flags to say if copy old file
                 OLD.LOG.THERE(1),      \ Old log file exists
                 TERM.STATE(1),         \ Terminal states used in STATUS.CLOSE
                 STATE.TRAN(2),         \ State transition matrix
                 REC.SIZE,              \ Size of record used by MMCFEC39
                 KEY.LENGTH,            \ Size of key used by MMCFEC39
                 CONTINUE,              \ Flag to allow further checks for valid close
                 FUNC,                  \ used with ADXFILES
                 PARM1,                 \ used with ADXFILES
                 TERM,                  \ Used with ADXERROR
                 MSGNUM,                \ Used with ADXERROR
                 CLOSE.FLAG,            \ Used with ADXERROR
                 WORK.IO,               \ Session number for general access
                 FILE.NO,               \ Accounting file session number
                 SESSION.NUM,           \ IO session number where error occurred
                 FULL.WAIT,             \ Full 90 second wait if uploading override
                 MIDDLE.OF.CLOSE,       \ Restart indicator
                 PKUPS.FLAGS,           \ Close pickup flags for this close
                 CLOSE.FLAGS,           \ Close flags for this close
                 CLOSE.FILES,           \ Files closed in this close
                 RST.NO.SIGN.ON.OFF,    \
                 R.VALUE,               \ RETURN VALUE FROM D.M. FUNCTIONS
                 RETRY.COUNT,           \ RETRY COUNT FOR "KF" ERRORS
                 I%,                    \ FOR LOOP COUNTER
                 RST.PROHIBIT.EOT,      \ move defn to globals
                 J%,                    \
                 ON1,                   \
                 OFF0,                  \
                 OPTERM,                \
                 ACCOUNT.TYPE,          \
                 NOT.FOUND,             \
                 NO.OPERATOR,           \
                 NO.TERMINAL,           \
                 NUM.FILES,             \ number of old period files
                 CLOSE.TYPE,            \ Type of close performed
                 CLOSE.DAY,             \ Day of week as of today
                 EF.ENABLE1,            \ EFT enable flags
                 SPL.COMPL,             \ despooling complete
                 EF.ENABLE2,            \
                 TOP.LINE,              \ Top line of screen display
                 BOTTOM.LINE,           \ Bottom line of screen display
                 NO.OF.COLUMNS,         \ No. of columns of screen display
                 SCREEN.NUM,            \ No. of screen to be displayed
                 COL.POS(1),            \ array of column positions
                 SCR1.ACTIVE(1),        \ close active on day
                 SCR1.HOUR(1),          \ close time in hours (00-23)
                 SCR1.MIN(1),           \ close time in minutes (01-59)
                 SCR2.PICKUP(1),        \ tender to be automatically picked up
                 ENTER.OK               ! despooling complete

 INTEGER*4   GLOBAL                     \
                 CHK.PT,                \
                 RET.CODE,              \
                 MEDIUM.WAIT,           \
                 LONG.WAIT,             \
                 SHORT.WAIT,            \
                 LINE.NO,               \ debug error line number
                 SO.RECONCIL.LIMIT,     \
                 RET,                   \ Used by ADXFILES
                 TOTAL,                 \
                 TENDER.TRAN(1),        \
                 TENDER.VAL(1),         \
                 TENDER.AMT(1),         \
                 C.SPACE,               \ Space left on C drive
                 D.SPACE,               \ Space left on D drive
                 POINTER                ! Used as pointer to record in sequential file


\ REM!!***********************************************************
\ REM!!  SUB SCUEC01 PUBLIC
\ REM!!    Auto Store Close User Exit 1
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC01 PUBLIC

 ! Place user code here

 END SUB

\ REM!!***********************************************************
\ REM!!  SUB SCUEC02 PUBLIC
\ REM!!    Auto Store Close User Exit 2
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC02 PUBLIC

 ! Place user code here

 END SUB

\ REM!!***********************************************************
\ REM!!  SUB SCUEC03 PUBLIC
\ REM!!    Auto Store Close User Exit 3
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC03 PUBLIC

 ! Place user code here

 END SUB

\ REM!!***********************************************************
\ REM!!  SUB SCUEC04 PUBLIC
\ REM!!    Auto Store Close User Exit 4
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC04 PUBLIC

 ! Place user code here

 END SUB

\ REM!!***********************************************************
\ REM!!  SUB SCUEC05 PUBLIC
\ REM!!    Auto Store Close User Exit 5
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC05 PUBLIC

 ! Place user code here

 END SUB

\ REM!!***********************************************************
\ REM!!  SUB SCUEC06 PUBLIC
\ REM!!    Auto Store Close User Exit 6
\ REM!!
\ REM!!
\ REM!!***********************************************************

 SUB SCUCEC06 PUBLIC

 ! Place user code here

 END SUB
