\/* TIME STAMP BLOCK **************************************************
\*LIB   MFVS EAMRPJUC B86      08/05/91 14:30:22 MAENZA   *    IR94895
\*  END OF TIME STAMP BLOCK *******************************************/
\/*                                                                   */
\/*      MODULE NAME: EAMRPJUC                                        */
\/*                                                                   */
\/*      DESCRIPTIVE NAME: TRANSACTION LOG REPORT USER EXIT FUNCTION  */
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
\/*                                                                   */
\/*      PERSON RESPONSIBLE = MAENZA, MARTIN                          */
\/*                                                                   */
\/*      FUNCTION: EAMRPJUC                                           */
\/*                                                                   */
\/*                THE PURPOSE OF THIS MODULE IS TO PROVIDE           */
\/*                USER MODIFIABLE FUNCTIONS FOR THE TRANSACTION      */
\/*                LOG REPORT (EAMRPEJC).                             */
\/*      NOTES:                                                       */
\/*                                                                   */
\/*         DEPENDENCIES:                                             */
\/*                                                                   */
\/*         RESTRICTIONS:                                             */
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
\/*         MODULE SIZE: XXXXX bytes                                  */
\/*                                                                   */
\/*         ATTRIBUTES: REENTRANT                                     */
\/*                                                                   */
\/*      ENTRY POINT: STRTYPE.WANTED                                  */
\/*                                                                   */
\/*         PURPOSE: ENTRY POINT ON STRING TYPE DETERMINATION.        */
\/*                  USED TO MODIFY STRING TYPE TO ONE THAT BYPASSES  */
\/*                  THE STRING PROCESSING.                           */
\/*                                                                   */
\/*                ***** NO RETURN *****                              */
\/*                                                                   */
\/*         LINKAGE: CONTROL TRANSFERRED FROM RUN-TIME ROUTINES.      */
\/*                                                                   */
\/*         ERROR MESSAGES:                                           */
\/*                                                                   */
\/*      EXTERNAL REFERENCES:                                         */
\/*                                                                   */
\/*         PROCEDURES:                                               */
\/*         DATA AREAS:                                               */
\/*                                                                   */
\/*           MODIFIED:                                               */
\/*                                                                   */
\/*           REFERENCED:                                             */
\/*                                                                   */
\/*         CONTROL BLOCKS: NONE                                      */
\/*                                                                   */
\/*         MACROS: NONE                                              */
\/*                                                                   */
\/*         TABLES: NONE                                              */
\/*                                                                   */
\/*      PREREQUISITE:  IR93655                                       */
\/*                                                                   */
\/*                                                                   */
\/*      CHANGE ACTIVITY: LEVEL 0                                     */
\/*                                                                   */
\/*        IR22118  - Added the ability for the user to add a line to */
\/*                   the report and process user strings logically.  */
\/*                   DRV MGVA 17Feb93                                */
\/*                                                                   */
\/* END-OF-SPECIFICATIONS *********************************************/
\/*                                                                   */


INTEGER*2 GLOBAL  TYPESTR
!AIR22118
STRING    GLOBAL  USER.DATA$  ! User line to be printed if not null
STRING    GLOBAL  TRAN.BUF$   ! Transaction buffer
INTEGER*4 GLOBAL  STR.OFF(1)  ! Offset into transaction for all strings
INTEGER*2 GLOBAL  STR.TYPE(1) ! String Types from all strings in transaction
INTEGER*4 GLOBAL  STROFFSET   ! Offset into transaction for all strings
!EIR22118



\ REM!!************** STRTYPE.WANTED ******************************!!
\ REM!! This routine  allows the user to change the value of       !!
\ REM!! the string type to bypass the processing of some strings.  !!
\ REM!! By setting TYPESTR = 20, you can bypass the string type.   !!
\ REM!!                                                            !!
\ REM!! For example, to bypass strings '07' and '08' (tax strings) !!
\ REM!! add the following code:                                    !!
\ REM!!                                                            !!
\ REM!!     IF (TYPESTR = 7) OR (TYPESTR = 8) THEN BEGIN           !!
\ REM!!        TYPESTR = 20                                        !!
\ REM!!     ENDIF                                                  !!
\ REM!!                                                            !!
\ REM!!                                                            !!
\AIR22118
\ REM!! In addition if the user wants to add a line to the report  !!
\ REM!! for a given string type then they can set the GLOBAL       !!
\ REM!! variable USER.DATA$ to the line to be included and this    !!
\ REM!! will be added to the report.                               !!
\ REM!!                                                            !!
\ REM!! GLOBALS of interest are:                                   !!
\ REM!!                                                            !!
\ REM!!     TRAN.BUF$   the complete transaction as a single string!!
\ REM!!     STR.OFF()   offset for each string within the trans.   !!
\ REM!!     STR.TYPE()  type for each string within the trans.     !!
\ REM!!     STROFFSET   offset for this string within the trans    !!
\ REM!!                                                            !!
\EIR22118
\ REM!!                                                            !!
\ REM!!************************************************************!!

 SUB STRTYPE.WANTED PUBLIC

!***************************************************************
!*
!*   INSERT USER CODE HERE:
!*
!***************************************************************
!    START OF USER CODE


!    END OF USER CODE
!***************************************************************

 END SUB

