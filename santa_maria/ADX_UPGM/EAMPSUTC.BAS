\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
!**********************************************************************!
!* Version: 010                           Last Edit:  24 October 1989 *!
!**********************************************************************!
!*                                                                    *!
!*  MODULE : EAMPSUTC            Supermarket EPS Terminal User Exits. *!
!*                                                                    *!
!*  PRODUCT:  EPS Feature.                    P.A.Gillingwater.       *!
!*                                            October 1989.           *!
\/*                                                                   */
\/*      COPYRIGHT:                                                   */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"        */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED        */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT      */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                           */
\/*                                                                   */
\/*      STATUS: RELEASE 2 LEVEL 0                                    */
\/*                                                                   */
!*                                                                    *!
!*                                                                    *!
!* Description :                                                      *!
!*   This module has multiple entry points.                           *!
!*   Each entry point is a self-contained user exit called by the     *!
!*   EPS code. These are provided for the user to code in order to    *!
!*   provide new or changed EPS function. The module should be        *!
!*   compiled, then linked into the base Supermarket Application      *!
!*   with the EPS Feature libraries. No activation at the controller  *!
!*   is required. No change to the linker input file should be        *!
!*   necessary, nor any change to the L86 libraries.                  *!
!*                                                                    *!
!*                                                                    *!
!*  CALLED BY      :  The various routines are called as specified in *!
!*                    appropriate routine header below.               *!
!*                                                                    *!
!*  CHANGE HISTORY :  None                                            *!
!*                                                                    *!
!**********************************************************************!

!**********************************************************************!
!*  This module should be null, unless the EPS feature is installed.  *!
!**********************************************************************!

%ENVIRON T

 SUB EAMPSUTC PUBLIC

!**********************************************************************!
!*  This module should be null, unless the EPS feature is installed.  *!
!**********************************************************************!

 END SUB
