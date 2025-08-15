\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMSCUEC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: AUTO STORE CLOSE PERSONALIZTION USEREXIT*/
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED     */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*      PERSON RESPONSIBLE = CHARLIE JOHNSON                      */
\/*                                                                */
\/*      FUNCTION: EAMSCUEC                                        */
\/*                                                                */
\/*                THIS MODULE CONTAINS THE USER EXIT              */
\/*                SUB ROUTINES FOR PERSONALIZATION.  THEY         */
\/*                ARE CALLED BEFORE ENTRY TO THE SCREENS          */
\/*                USED FOR PERSONALIZATION OF THE AUTO            */
\/*                STORE CLOSE FUNCTION.                           */
\/*                                                                */
\/*      NOTES:                                                    */
\/*                                                                */
\/*         DEPENDENCIES:                                          */
\/*                                                                */
\/*         RESTRICTIONS:                                          */
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
\/*         MODULE SIZE: XXXX...                                   */
\/*                Estimated size in bytes.                        */
\/*                                                                */
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*      ENTRY POINT: SCAPEXIT1                                    */
\/*                                                                */
\/*         PURPOSE:                                               */
\/*                                                                */
\/*                THIS ENTRY POINT IS USED WHEN CALLED FROM       */
\/*                EAMSCAPC PRIOR TO INPUTTING DATA ON SCREEN 1    */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM EAMSCAPC                          */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*      ENTRY POINT: SCAPEXIT2                                    */
\/*                                                                */
\/*         PURPOSE:                                               */
\/*                                                                */
\/*                THIS ENTRY POINT IS USED WHEN CALLED FROM       */
\/*                EAMSCAPC PRIOR TO INPUTTING DATA ON SCREEN 2    */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: CALLED FROM EAMSCAPC                          */
\/*                                                                */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*                                                                */
\/*      EXIT-NORMAL: SCAPEC01 - PERSONALIZATION MENU              */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*         EXIT-ERROR:                                            */
\/*                                                                */
\/*         RETURN CODE:                                           */
\/*                                                                */
\/*            REASON CODE:                                        */
\/*            MESSAGE ID:                                         */
\/*                                                                */
\/*         ABEND CODES:                                           */
\/*                                                                */
\/*         ERROR MESSAGES:                                        */
\/*                                                                */
\/*      EXTERNAL REFERENCES:                                      */
\/*                                                                */
\/*         ROUTINES:                                              */
\/*                                                                */
\/*                                                                */
\/*         DATA AREAS:                                            */
\/*                                                                */
\/*           MODIFIED:                                            */
\/*                                                                */
\/*            FIRST   : FIRST FIELD ON PERSONALIZATION SCREEN     */
\/*            LAST    : LAST FIELD ON PERSONALIZTION SCREEN       */
\/*                                                                */
\/*           REFERENCED:                                          */
\/*                                                                */
\/*            EAMSCP1S: PERSONALIZATION MAIN SCREEN FILE          */
\/*            AP11WERR: ERROR CODE POSTED FROM RUNTIMES           */
\/*                                                                */
\/*                                                                */
\/*      CHANGE ACTIVITY:                                          */
\/*                                                                */
\/* END-OF-SPECIFICATIONS ******************************************/

\REM!!****************************************************************!!
\REM!!        INCLUDES USED BY THIS MODULE                            !!
\REM!!****************************************************************!!
\REM!!                                                                !!
\REM!!     EAMCFINC.J86  -  Common routine variable definitions       !!
\REM!!     EAMATTRD.J86  -  Field and attribute variable definitions  !!
\REM!!     DMEXTR.J86    -  Display manager sub program definitions   !!
\REM!!                                                                !!
\REM!!****************************************************************!!


INTEGER*2 GLOBAL                                \
        FIRST,                                  \
        LAST                                    !

%INCLUDE EAMCFINC.J86                           ! common routine variables
%INCLUDE EAMATTRD.J86                           ! field and attribute def.

%INCLUDE DMEXTR.J86                             ! display manager sub prog.def.

\/******************************************************************/
\/*                                                                */
\/*      SCAPEXIT1 - User Exit prior to processing input from      */
\/*                  the first of three screens for personalizing  */
\/*                  the Auto Store Close Function.                */
\/*                                                                */
\/******************************************************************/
\/*  This exit is called prior calling for input from screen 1     */
\/*  (SCP1S001).  The following variables may be changed from      */
\/*  this exit:                                                    */
\/*     FIRST - The number of the first field on the screen        */
\/*     LAST  - The number of the last field on the screen         */
\/*  In addition, all display manager functions are available in   */
\/*  for use in this exit.                                         */
\/*                                                                */
\/******************************************************************/

SUB SCAPEXIT1 PUBLIC

!User code place here

END SUB


\/******************************************************************/
\/*                                                                */
\/*      SCAPEXIT2 - User Exit prior to processing input from      */
\/*                  the second of three screens for personalizing */
\/*                  the Auto Store Close Function.                */
\/*                                                                */
\/******************************************************************/
\/*  This exit is called prior calling for input from screen 2     */
\/*  (SCP1S002).  The following variables may be changed from      */
\/*  this exit:                                                    */
\/*     FIRST - The number of the first field on the screen        */
\/*     LAST  - The number of the last field on the screen         */
\/*  In addition, all display manager functions are available in   */
\/*  for use in this exit.                                         */
\/*                                                                */
\/******************************************************************/

SUB SCAPEXIT2 PUBLIC

!User code place here

END SUB


\/******************************************************************/
\/*                                                                */
\/*      SCAPEXIT3 - User Exit prior to processing input from      */
\/*                  the third of three screens for personalizing  */
\/*                  the Auto Store Close Function.                */
\/*                                                                */
\/******************************************************************/
\/*  This exit is called prior calling for input from screen 3     */
\/*  (SCP1S003).  The following variables may be changed from      */
\/*  this exit:                                                    */
\/*     FIRST - The number of the first field on the screen        */
\/*     LAST  - The number of the last field on the screen         */
\/*  In addition, all display manager functions are available in   */
\/*  for use in this exit.                                         */
\/*                                                                */
\/******************************************************************/

SUB SCAPEXIT3 PUBLIC

!User code place here

END SUB
