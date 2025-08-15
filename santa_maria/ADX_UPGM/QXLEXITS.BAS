\/* TIME STAMP BLOCK ************************************************
\** END OF TIME STAMP BLOCK ****************************************/
!***********************************************************************
\/*      COPYRIGHT: THIS MODULE IS "RESTRICTED MATERIALS OF EDJ       */
\/*                 ENTERPRISES, INC" (C) COPYRIGHT EDJ ENTERPRISES,  */
\/*                 1994,1997 ALL RIGHTS RESERVED LICENSED            */
\/*                 MATERIALS - PROPERTY OF EDJ ENTERPRISES, INC      */
\/*                                                                   */
\/*                 5799-QXL THIS MODULE IS "RESTRICTED               */
\/*                 MATERIALS OF IBM" (C) COPYRIGHT IBM CORP          */
\/*                 1996,1997 ALL RIGHTS RESERVED LICENSED            */
\/*                 MATERIALS                                         */
\/*                                                                   */
!***********************************************************************

      %ENVIRON T
%INCLUDE QXLPDATA.J86
%INCLUDE QXLGVART.J86
FUNCTION QXLEXIT1(PrintLine) PUBLIC
         STRING QXLEXIT1, PrintLine

        QXLEXIT1 = PrintLine

FEND
FUNCTION QXLEXIT2(PrintLine) PUBLIC
         STRING QXLEXIT2, PrintLine

       QXLEXIT2 = ""
       IF PaperOn THEN QXLEXIT2 = PrintLine

FEND
