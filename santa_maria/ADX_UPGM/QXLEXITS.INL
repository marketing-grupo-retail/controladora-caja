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
\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/

  !980126 ELF Personalization

  INTEGER*1 GLOBAL PaperOn          ! Parm '00'
  INTEGER*1 GLOBAL JournalOff       ! Parm '00'
  INTEGER*2 GLOBAL SaveDays         ! Parm '01'
  INTEGER*2 GLOBAL CompressDays     ! Parm '02'


  INTEGER*2 GLOBAL JournalSession   ! Parm '01'
  INTEGER*1 GLOBAL LANEnv           ! Parm '02'
  STRING    GLOBAL CtlrPipeID       ! Parm '03'
  INTEGER*2 GLOBAL DeSpoolSession, SpoolSession, ControlFileSession ! 4-6
  INTEGER*1 GLOBAL Prntr4610        ! Parm '07'
  INTEGER*2 GLOBAL MaxPrintLines    ! Parm '08'
  INTEGER*1 GLOBAL MedModel         ! Parm '09'

  INTEGER*2 GLOBAL IndexSession     ! Parm '10'
  STRING    GLOBAL SearchTransDesc  ! Parm '11'
  STRING    GLOBAL SearchTransDesc2 ! Parm '12'!980126

  !AIR40041
  STRING    GLOBAL  QXLUTIME        ! Time to unload 4610 NV memory
  !EIR40041
%INCLUDE QXLGVART.J86
\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/

!****************************************************************************
!****************************************************************************
! QXLGVART.J86
!
!
!
!
!
!****************************************************************************
!       DEV IR35895 -
!                     970414 Keep CurrentTrans in an array
!
!                     971112
!****************************************************************************

  STRING    global Header       !
! STRING    global CurrentTrans !970414     !IR35895
  INTEGER*2 global LineCount    !
  STRING    global TransArray(1)!970414     !IR35895

  STRING GLOBAL Copyright                       !
                                                !

  STRING GLOBAL ApplName                        ! "GSA" for GSA
                                                ! "SA"  for Supermarket
                                                ! "CDSA" for Chain Drug
  INTEGER*1 GLOBAL Offline
  !*****************************************************************
  !
  !*****************************************************************
  INTEGER*1 global ?4610Prntr      ! -1 = yes
  INTEGER*1 global Storing, Unloading
  STRING    GLOBAL TotalLine
 !*********************************************
 ! The following vars used to build FileName
 !*********************************************
  STRING   GLOBAL DateDirectory
  STRING   GLOBAL TerminalDirectory

  INTEGER*4 GLOBAL DeSpoolPtr
  STRING    GLOBAL DeSpoolFrom, SpoolTo
  STRING    GLOBAL SpoolA, SpoolB, SpoolControlFile
  INTEGER*1 GLOBAL Spooling, DeSpooling
  INTEGER*1 GLOBAL AnySpooled
  INTEGER*1 GLOBAL Unused !971112

  INTEGER*1 GLOBAL AnyStored

  STRING    GLOBAL CRLF
  STRING   global JournalName, IndexName
  INTEGER*2 GLOBAL QXL.SESSION


  STRING GLOBAL QXL.MSGS(1)
  INTEGER*2 GLOBAL QXL.MSG.COUNT

  INTEGER*1 GLOBAL QXL.DEBUG

  INTEGER*2 GLOBAL PrntrSession
FUNCTION QXLEXIT1(PrintLine) PUBLIC
         STRING QXLEXIT1, PrintLine

        QXLEXIT1 = PrintLine

FEND
FUNCTION QXLEXIT2(PrintLine) PUBLIC
         STRING QXLEXIT2, PrintLine

       QXLEXIT2 = ""
       IF PaperOn THEN QXLEXIT2 = PrintLine

FEND
End of Compilation
