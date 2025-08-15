\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMIRUEC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: CONTROLLER ITEM RECORD USER EXITS     */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*      FUNCTION: EAMIRUEC                                      */
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
!* Variable declarations                                                     *!
!*****************************************************************************!
%INCLUDE EAMITEMR.J86       !* GLOBAL Item Record File variables             *!
%INCLUDE EAMIRRFG.J86       !* GLOBAL Routine Variables                      *!
%INCLUDE EAMIRCFV.J86       !* Common Function Variables                     *!
%INCLUDE EAMIUCVA.J86       !* User Variaibles                               *!

!*****************************************************************************!
!* External Functions and Subroutines declarations                           *!
!*****************************************************************************!
%INCLUDE EAMASMRT.J86       !* External assembler functions                  *!
%INCLUDE EAMIUCSU.J86       !* User subprogram and functions                 *!

!*****************************************************************************!
%INCLUDE EAMIUC01.J86
%INCLUDE EAMIUC02.J86
%INCLUDE EAMIUC03.J86
%INCLUDE EAMIUC04.J86
%INCLUDE EAMIUC05.J86
%INCLUDE EAMIUC06.J86
%INCLUDE EAMIUC07.J86
%INCLUDE EAMIUC10.J86
