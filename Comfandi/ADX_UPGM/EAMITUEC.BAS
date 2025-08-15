\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMITUEC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: TERMINAL ITEM RECORD USER EXITS       */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*      FUNCTION: EAMITUEC                                      */
\/*                                                              */
\/*      NOTES:                                                  */
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
\/*         MODULE SIZE:                                         */
\/*                                                              */
\/*         ATTRIBUTES: REENTRANT                                */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC01                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC02                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC03                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC04                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC05                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC06                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC07                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*      ENTRY POINT: IRUEEC10                                   */
\/*                                                              */
\/*         PURPOSE:                                             */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:                                             */
\/*                                                              */
\/*                                                              */
\/*      INPUT:                                                  */
\/*                                                              */
\/*                                                              */
\/*      OUTPUT:                                                 */
\/*                                                              */
\/*                                                              */
\/*      EXIT-NORMAL: RETURN TO CALLER                           */
\/*                                                              */
\/*         RETURN CODE:                                         */
\/*                                                              */
\/*      EXIT-ERROR:  RETURN TO CALLER                           */
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
\/*                                                              */
\/*      EXTERNAL REFERENCES:                                    */
\/*                                                              */
\/*         ROUTINES:                                            */
\/*                                                              */
\/*     NAME:                                                    */
\/*                                                              */
\/*         DATA AREAS:                                          */
\/*                                                              */
\/*           MODIFIED:                                          */
\/*                                                              */
\/*           REFERENCED:                                        */
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
\/*                                                              */
\/*                                                              */
\/****************************************************************/

!*****************************************************************************!
!* Compile directives                                                        *!
!*****************************************************************************!
%ENVIRON T

!*****************************************************************************!
!* Variable declarations                                                     *!
!*****************************************************************************!
%INCLUDE EAMITEMR.J86       !* GLOBAL Item Record File variables             *!
%INCLUDE EAMIRRFG.J86       !* GLOBAL Routine Variables                      *!
%INCLUDE EAMIRCFV.J86       !* Common Function Variables                     *!
%INCLUDE EAMIUTVA.J86       !* User Variables                                *!

!*****************************************************************************!
!* External Functions and Subroutines declarations                           *!
!*****************************************************************************!
%INCLUDE EAMASMRT.J86       !* External assembler functions                  *!
%INCLUDE EAMIUTSU.J86       !* User subs/functions                           *!

!*****************************************************************************!
%INCLUDE EAMIUT01.J86
%INCLUDE EAMIUT02.J86
%INCLUDE EAMIUT03.J86
%INCLUDE EAMIUT04.J86
%INCLUDE EAMIUT05.J86
%INCLUDE EAMIUT06.J86
%INCLUDE EAMIUT07.J86
%INCLUDE EAMIUT10.J86
