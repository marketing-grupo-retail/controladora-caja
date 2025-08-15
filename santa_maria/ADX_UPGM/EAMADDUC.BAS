\/* TIME STAMP BLOCK ***************************************************
\** END OF TIME STAMP BLOCK *******************************************/
\/*********************************************************************/
\/*                                                                   */
\/*      MODULE NAME: EAMADDUC                                        */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: ALTERNATE DELAYED DATA MAINTENANCE         */
\/*                        INTERFACE USER EXIT                        */
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*       5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"       */
\/*       (C) COPYRIGHT IBM CORP 1990,1993 ALL RIGHTS RESERVED LICENSED    */
\/*       MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT              */
\/*       INSTRUCTIONS FORM NUMBER G120-2083                          */
\/*                                                                   */
\/*      STATUS: RELEASE 1 LEVEL 0                                    */
\/*                                                                   */
\/*      PERSON RESPONSIBLE = CHARLIE JOHNSON                         */
\/*                                                                   */
\/*      FUNCTION: EAMADDUC                                           */
\/*                                                                   */
\/*      THIS MODULE CONVERTS ITEM RECORD MAINTENANCE INFORMATION     */
\/*      IN A PREDEFINED FILE (EAMMAINT) INTO SOURCE FILES AND CONTROL*/
\/*      RECORDS FOR THE DELAYED DATA MAINTENANCE FUNCTION.           */
\/*                                                                   */
\/*      NOTES:                                                       */
\/*                                                                   */
\/*         DEPENDENCIES:                                             */
\/*                                                                   */
\/*      EAMADDML.286 PROGRAM CAN BE STARTED IN FOUR WAYS.            */
\/*         1) START USER PROGRAM COMMAND FROM HOST                   */
\/*         2) BACKGROUND SCREEN STORE CONTROLLER                     */
\/*         3) ADXSTART SYSTEM COMMAND FROM AN APPLICATION            */
\/*         4) CONTROLLER IPL                                         */
\/*                                                                   */
\/*         RESTRICTIONS:                                             */
\/*                                                                   */
\/*       NOTE WHEN USING FIELD LEVEL MAINTENANCE THE PRESERVATION    */
\/*       OF THESE FIELD MODIFICATIONS IS LIMITED WHEN THERE IS       */
\/*       SUBSTANTIAL DELAY BETWEEN THE CREATION OF A BATCH AND       */
\/*       IT'S EXECUTION BY DDM. THUS USING FULL RECORD ADD/REPLACE   */
\/*       AND IMMEDIATE MAINTENANCE DIRECTLY USING THIS PROGRAM       */
\/*       CAN ASSURE FIELD LEVEL CHANGE INTEGRITY.                    */
\/*                                                                   */
\/*         REGISTER CONVENTIONS:                                     */
\/*                                                                   */
\/*            RESTRICTED REGISTERS = N/A                             */
\/*                                                                   */
\/*            REGISTERS USED = N/A                                   */
\/*                                                                   */
\/*      MODULE TYPE: CBASIC                                          */
\/*                                                                   */
\/*         PROCESSOR: CBASIC COMPILER                                */
\/*         MODULE SIZE: XXX                                          */
\/*                                                                   */
\/*         LINKAGE: CONTROL TRANSFERRED FROM RUN-TIME ROUTINES.      */
\/*                                                                   */
\/*      INPUT:                                                       */
\/*                                                                   */
\/*                ***** FILE INPUT *****                             */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMMAINT                                   */
\/*                                                                   */
\/*         DESCRIPTION: MAINTENANCE FILE                             */
\/*                                                                   */
\/*         THE MAINTENANCE FILE CONTAINS THE COMMANDS AND ASSOCIATED */
\/*         DATA REQUIRED FOR CREATION OF A DELAYED DATA MAINTENANCE  */
\/*         BATCH OR DIRECT ITEM RECORD UPDATES.                      */
\/*                                                                   */
\/*                ***** FILE INPUT *****                             */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMMINPR                                   */
\/*                                                                   */
\/*         DESCRIPTION: PROCESSING MAINTENANCE FILE                  */
\/*                                                                   */
\/*         THE MAINTENANCE FILE EAMMAINT IS RENAMED TO EAMMINPR      */
\/*         TO ALLOW FOR ANOTHER MAINTENANCE FILE.                    */
\/*                                                                   */
\/*                ***** FILE INPUT *****                             */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMSOPTS                                   */
\/*                                                                   */
\/*         DESCRIPTION: STORE OPTIONS FILE                           */
\/*                                                                   */
\/*      OUTPUT:                                                      */
\/*                                                                   */
\/*                ***** FILE OUTPUT  *****                           */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMDMCTL                                   */
\/*                                                                   */
\/*         DESCRIPTION: DELAYED DATA MAINTENANCE CONTROL FILE        */
\/*                                                                   */
\/*                      CONTAINS A RECORD FOR EACH MAINTENANCE       */
\/*                      BATCH                                        */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAM@@nnn                                   */
\/*                                                                   */
\/*         DESCRIPTION: DELAYED MAINTENANCE DATA FILE                */
\/*                                                                   */
\/*                      CONTAINS THE COMMANDS AND ASSOCIATED DATA    */
\/*                      REQUIRED FOR EXECUTION OF A BATCH.           */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMDMERR                                   */
\/*                                                                   */
\/*         DESCRIPTION: DELAYED MAINTENANCE ERROR FILE               */
\/*                                                                   */
\/*                      CONTAINS THE ERRORS THAT OCCUR DURING DDM    */
\/*                      PROCESSING.                                  */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMMERRD                                   */
\/*                                                                   */
\/*         DESCRIPTION: INPUT ERROR FILE                             */
\/*                                                                   */
\/*                      CONTAINS THE DATA FROM EAMMINPR WHEN BAD     */
\/*                      INPUT DATA IS FOUND.                         */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMMREPT                                   */
\/*                                                                   */
\/*         DESCRIPTION: ITEM RECORD REPORT FILE                      */
\/*                                                                   */
\/*                      REPORT RECORDS CREATED BY DDM.               */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMEXCPT                                   */
\/*                                                                   */
\/*         DESCRIPTION: EXCEPTION LOG FILE                           */
\/*                                                                   */
\/*                      ANY EXCEPTION DATA DURING ADDMI PROCESSING.  */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMITEMR                                   */
\/*                                                                   */
\/*         DESCRIPTION: ITEM RECORD FILE                             */
\/*                                                                   */
\/*                      DIRECT RECORD UPDATES ARE WRITTEN TO THIS    */
\/*                      FILE.                                        */
\/*                                                                   */
\/*                ***** FILE OUTPUT *****                            */
\/*                                                                   */
\/*         SYMBOLIC NAME: EAMIRCHG                                   */
\/*                                                                   */
\/*         DESCRIPTION: ITEM RECORD CHANGE FILE                      */
\/*                                                                   */
\/*                      DIRECT ITEM RECORD UPDATES ARE LOGGED IN     */
\/*                      THIS FILE.                                   */
\/*                                                                   */
\/*      EXIT-NORMAL: EAMADDML ENDS NORMALLY AFTER EXECUTION          */
\/*                                                                   */
\/*      EXTERNAL REFERENCES:                                         */
\/* END-OF-SPECIFICATIONS *********************************************/


INTEGER*1  GLOBAL                         \
        USER.FLAG                          !

INTEGER*4  GLOBAL                         \
        DELETE.TIME                       !

STRING GLOBAL          \
    A$,B$,C$,          \ WORKSPACE FOR STRINGS
    ZERO$,             \ 4 CHR$(00)
    NULLKEY$,          \ 6 CHR$(00)
    TODAY$,            \ DATE AS MMDDHH
    CTRL.ID$,          \ CONTROLLER ID$
    INPUT$,            \ INPUT ADDMI RECORD
    OUTPUT$,           \ OUTPUT SOURCE RECORD
    INPUT.FILE$,       \ INPUT FILE (EAMMINPR OR EAM$:mdd = EAMUDmdd)
    OUTPUT.FILE$,      \ OUTPUT SOURCE FILE NAME
    MESSAGE$(1),       \ MESSAGES FROM EAMAAAMF
    OUTPUT.MESSAGE$,   \ OUTPUT MESSAGE DISPLAYED
    ITEM.RECORD$,      \ ITEM RECORD
    ITEM.DATA$,        \ ITEM RECORD DATA FOR DDM UPDATE
    ITEMCODE$,         \ ITEM CODE             (UPD 6)
    BAD.DEPT$,         \ BAD DEPT VALUE = X"0000"
    BAD.PRICE$,        \ BAD PRICE = X"9999999999"
    DMC.DATE$,         \ DATE OF EXECUTION
    DMCMD$,            \ COMMAND
    CAR$,              \ CARRIAGE RETURN/LINE FEED
    CONTROL.RECORD$,   \ DDM CONTROL RECORD
    BATCH$,            \ CURRENT BATCH NUMBER  (UPD 4)
    BATCH.R$,          \ CURRENT REPORT BATCH NUMBER  (UPD 4)
    ERROR.CODE$,       \ ERROR CODE
    DATA$(1)           ! ITEM RECORD DATA

\ REM!!**************************************************************!!
\ REM!!                                                              !!
\ REM!! This User Exit provides user with the ability to not erase   !!
\ REM!! source files when EAMADDMC does its cleanup, by setting the  !!
\ REM!! global variable USERFLAG to -1.                              !!
\ REM!! Also the delete time, which defaults to three (3) months can !!
\ REM!! be changed as in the examples below.                         !!
\ REM!! Set DELETE.TIME =  29999 for 3 months (default)              !!
\ REM!!     DELETE.TIME =   9999 for 1 month                         !!
\ REM!!     DELETE.TIME =     99 for 1 day                           !!
\ REM!!                                                              !!
\ REM!! The time is determined in MMDDHH (e.g. October 10, 1993 at   !!
\ REM!! 11:15 am is equal to 101011).  One month later would be      !!
\ REM!! 111010, or a difference of 9999.                            !!
\ REM!!**************************************************************!!

SUB USEREXIT1 PUBLIC
  ! Place User Code here
END SUB

SUB USEREXIT2 PUBLIC
  ! Place User Code here
END SUB

SUB USEREXIT3 PUBLIC
  ! Place User Code here
END SUB

SUB USEREXIT4 PUBLIC
  ! Place User Code here
END SUB

