\/* TIME STAMP BLOCK-************************************************
\** END OF TIME STAMP BLOCK ****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMIRBUC                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: ITEM RECORD FILE REBUILD USER EXITS     */
\/*                                                                */
\/*      COPYRIGHT:                                                */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"     */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED     */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT   */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                        */
\/*                                                                */
\/*      STATUS: RELEASE 2 LEVEL 0                                 */
\/*                                                                */
\/*      PERSON RESPONSIBLE = VAUGHAN, DAVID R                     */
\/*                                                                */
\/*      FUNCTION: EAMIRBUC                                        */
\/*                                                                */
\/*                THIS MODULE CONTAINS THE USER EXITS CALLED      */
\/*                DURING REBUILDING THE ITEM RECORD FILE.         */
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
\/*         ATTRIBUTES: REENTRANT                                  */
\/*                                                                */
\/*      ENTRY POINT: IRRBEC??                                     */
\/*                                                                */
\/*         PURPOSE: USER EXITS.                                   */
\/*                                                                */
\/*                ***** RETURN CONTROL *****                      */
\/*                                                                */
\/*         LINKAGE: THIS MODULE IS CALLED.                        */
\/*                                                                */
\/*                                                                */
\/*      INPUT:                                                    */
\/*                                                                */
\/*                                                                */
\/*      OUTPUT:                                                   */
\/*                                                                */
\/*                                                                */
\/*      EXIT-NORMAL: RETURN TO CALLER                             */
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

%INCLUDE EAMIRRFG.J86
%INCLUDE EAMIRCFV.J86


!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC01                                               *!
!*  Description     : Item record file rebuild Initialization User Exit      *!
!*  Parameters      : None                                                   *!
!*  Notes           :                                                        *!
!*                                                                           *!
!*                                                                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC01 PUBLIC
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC02                                               *!
!*  Description     : Item record file rebuild Termination User Exit         *!
!*  Parameters      : None                                                   *!
!*  Notes           :                                                        *!
!*                                                                           *!
!*                                                                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC02 PUBLIC
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC03                                               *!
!*  Description     : Item record file rebuild Open Files User Exit          *!
!*  Parameters      : None                                                   *!
!*  Notes           :                                                        *!
!*                                                                           *!
!*                                                                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC03 PUBLIC
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC04                                               *!
!*  Description     : Item record file rebuild read input exit               *!
!*  Parameters      : INPUT.RECORD$    Sector read from the input file       *!
!*  Notes           :                                                        *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC04(INPUT.RECORD$) PUBLIC
  STRING INPUT.RECORD$
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC05                                               *!
!*  Description     : Item record file rebuild write output exit             *!
!*  Parameters      : OUTPUT.RECORD$   Keyed record to be written            *!
!*  Notes           : IRRF.BYPASS allows the user to bypass writing this     *!
!*                    record to file. (set to non-zero to bypass the write). *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC05(OUTPUT.RECORD$) PUBLIC
  STRING OUTPUT.RECORD$
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Subroutine      : IRRBUC06                                               *!
!*  Description     : Item record file rebuild initialize default data       *!
!*  Parameters      : DEFAULT$      Data to be added to each item record     *!
!*  Notes           : This exit is called with the initialized defaults      *!
!*                    for the additional data (if any) to be added to the    *!
!*                    end of the current item record.                        *!
!*                                                                           *!
!*****************************************************************************!
SUB IRRBUC06(DEFAULT$) PUBLIC
  STRING DEFAULT$
END SUB

!*****************************************************************************!
!*                                                                           *!
!*  Function        : IRRBUC10                                               *!
!*  Description     : Item record file ON ERROR processing exit.             *!
!*  Parameters      : None.                                                  *!
!*  Returns         : INTEGER*2   Action code for error processing           *!
!*                  : 0           Default processing                         *!
!*                  : 1           Resume                                     *!
!*                  : 2           Resume Retry                               *!
!*                  : other       Resume label and terminate the application *!
!*  Notes           :                                                        *!
!*                                                                           *!
!*****************************************************************************!
FUNCTION IRRBUC10 PUBLIC
  INTEGER*2 IRRBUC10
END FUNCTION
