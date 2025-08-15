\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/*********************************************************************/
\/*                                                                   */
\/*      MODULE NAME: EAMCKUZC                                        */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: USEREXIT CODE FOR SUPERMARKET              */
\/*                        CHEQUE PRINTING FEATURE IN TERMINAL        */
\/*                                                                   */
\/*                                                               */
\/*      COPYRIGHT:                                               */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"    */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED    */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT  */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                       */
\/*                                                               */
\/*      STATUS: RELEASE 2 LEVEL 0                                */
\/*                                                                   */
\/*      PERSON RESPONSIBLE = MARTIN MAENZA                           */
\/*                                                                   */
\/*      FUNCTION: CODE USED BY TERMINAL TO PERFORM CHEQUE PRINTING   */
\/*                IN SUPERMARKET APPLICATION.                        */
\/*                                                                   */
\/*                                                                   */
\/*                                                                   */
\/*                                                                   */
\ REM!!************************ B B D L ******************************!!
\ REM********************* END BBDL STATEMENTS *************************

  %ENVIRON T

\ REM!!****************** INCLUDE FILES ******************************!!
\ REM!!    CPVARS.J86       Common variables for Cheque Printing      !!
\ REM!!    EAMTSWKG.J86     Common supermarket global variables       !!
\ REM!!    EAMTRANS.J86     Common supermarket transaction variables  !!
\ REM!!***************************************************************!!

  %INCLUDE CPVARS.J86     ! Common variables for Cheque Printing
  %INCLUDE EAMTSWKG.J86   ! Common supermarket global variables
  %INCLUDE EAMTRANS.J86   ! Common supermarket transaction variables

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
             TO.DI.SPACING,    \ Option for DI spacing value
             TO.CR.SPACING,    \ Option for CR spacing value
             TO.SJ.SPACING     ! Option for SJ spacing value

  INTEGER*2  GLOBAL               \
             TO.CHEQUE.FORMAT(1)  ! Array holding tender flag for CP

  %INCLUDE EAMXXCPY.J86           ! Copyright statement


  %INCLUDE EAMCKU01.J86
  %INCLUDE EAMCKU02.J86
  %INCLUDE EAMCKU03.J86
  %INCLUDE EAMCKU04.J86

