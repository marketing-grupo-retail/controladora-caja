\/* TIME STAMP BLOCK ************************************************
\* END OF TIME STAMP BLOCK *****************************************/
\/******************************************************************/
\/*                                                                */
\/*      MODULE NAME: EAMDUMMY                                     */
\/*                                                                */
\/*      DESCRIPTIVE NAME: DUMMY ENTRY POINTS TO AVOID LINK ERRORS */
\/*                                                                */
\/* COPYRIGHT: 5799-PNH THIS MODULE IS "RESTRICTED MATERIALS OF    */
\/*            IBM" (c) COPYRIGHT IBM CORP 2004 ALL RIGHTS         */
\/*            RESERVED LICENSED MATERIALS - PROPERTY OF IBM       */
\/*                                                                */
\/*            REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER         */
\/*            G120-2083                                           */
\/*                                                                */
\/*  THIS FILE PROVIDES DUMMY ENTRY POINTS FOR CALLS TO CODE THAT  */
\/*  MAY OR MAY NOT BE PRESENT.  THIS FILE IS LINKED AT THE END OF */
\/*  EAMTS10L SO REAL ENTRY POINTS ARE USED IF THEY ARE PRESENT.   */
\/*  THE DUMMY ENTRY POINTS SHOULD NEVER BE CALLED, BUT ARE PRESENT*/
\/*  SO THE LINKER WILL NOT SHOW ERRORS.                           */
\/*                                                                */
\/******************************************************************/
\/*                                                                */
\/* CHANGE ACTIVITY                                                */
\/*                                                                */
\/* IRwicEM  Add Item to Coupon Association for WIC EBT function.  */
\/*          REM IBM 02Sep2004                                     */
\/*                                                                */
\/* IO08252  Add Flexible Spending Account (FSA) stubs.            */
\/*          Real routines are in EM, added with IO08257.          */
\/*          GGK IBM 20Feb2008                                     */
\/*                                                                */
\/* IO10105  Add serviceability hook.                              */
\/*          GGK IBM 20Jan2009                                     */
\/*                                                                */
\/****END-OF-SPECIFICATIONS*****************************************!

%ENVIRON T

%INCLUDE EAMXXCPY.J86        ! Copyright statement

!!*******************************************************************
!!** DUMMY ENTRY POINTS FOR EM ENHANCEMENT FUNCTION CALLS          **
!!*******************************************************************
SUB GET.SKU(IR.REC$) PUBLIC
    STRING IR.REC$
END SUB

SUB LINK.SKU(IR.UPC$) PUBLIC
    STRING IR.UPC$
END SUB

FUNCTION EMSS.PROMO.TLOG(IN%) PUBLIC
  INTEGER EMSS.PROMO.TLOG,IN%
END FUNCTION

!!*******************************************************************
!!** DUMMY ENTRY POINTS FOR EM WIC SUBROUTINES                     **
!!*******************************************************************
SUB WEBT.REPLICATE PUBLIC
END SUB

SUB WEBT.FINISH.COUPONS PUBLIC
END SUB

SUB EM000.WEB PUBLIC
END SUB

SUB EM008.WEB PUBLIC
END SUB

SUB EM014.WEB PUBLIC
END SUB

SUB EM020.WEB PUBLIC
END SUB

SUB EM120.WEB PUBLIC
END SUB

SUB EM037.WEB PUBLIC
END SUB

SUB EM051.WEB PUBLIC
END SUB

!!*******************************************************************
!!** DUMMY ENTRY POINTS FOR WIC EBT TERMINAL SALES SUBROUTINES     **
!!*******************************************************************

SUB WIC.EBT.INITIALIZATION PUBLIC
END SUB

SUB WIC.EBT.PINPAD.INITIALIZE PUBLIC
END SUB

SUB WIC.EBT.SIGN.ON PUBLIC
END SUB

SUB WIC.EBT.TENDER PUBLIC
END SUB

SUB WIC.EBT.LOG.ITEM.SUMMARY PUBLIC
END SUB

SUB WIC.EBT.END.OF.TRANSACTION PUBLIC
END SUB

SUB WIC.EBT.BALANCE.INQUIRY PUBLIC
END SUB

SUB WIC.EBT.REPRINT.RECEIPT PUBLIC
END SUB

SUB WIC.EBT.PRINT.HELP PUBLIC
END SUB

SUB WIC.EBT.TRNX.RECOVERY PUBLIC
END SUB

SUB GET.PIN.PAD.STATUS PUBLIC
END SUB

!AIO08252 Flexible Spending Account (FSA)
!!*******************************************************************
!!** DUMMY ENTRY POINTS FOR FLEXIBLE SPENDING ACCOUNT SUBROUTINES  **
!!*******************************************************************

SUB TSFXECIN PUBLIC                   ! FSA Initialization
END SUB

SUB TSFXECIE PUBLIC                   ! FSA Item entry logic
END SUB

SUB TSFXECUE.04(FSA.NS.REJECT) PUBLIC ! Tender Cashing/Tender Exchange User
INTEGER*1 FSA.NS.REJECT
END SUB

SUB TSFXECPR.02 PUBLIC                ! Print QHP data after a QHP tender
END SUB

SUB TSFXECET PUBLIC                   ! FSA End of transaction
END SUB

SUB TSFXECTP PUBLIC                   ! Tlog write INDICAT4
END SUB

SUB TSFXECTT PUBLIC                   ! FSA Transaction recovery
END SUB

SUB TSFXECUE.03 PUBLIC                ! FSA Transaction recovery User Exit
END SUB

SUB TSFXECPR.01 PUBLIC                ! Modify print line for FSA
END SUB

SUB TSFXECRK PUBLIC                   ! FSA Keyboard input
END SUB

SUB TSFXECRV PUBLIC                   ! FSA EOT - dedicated routine
END SUB                               ! to reset the FSA variables
!EIO08252

!AIO10105 Serviceability (user exit) hook.
!!*******************************************************************
!!** DUMMY ENTRY POINT FOR SERVICEABILITY                          **
!!*******************************************************************

SUB EAMTRACE.EXIT69(TRXX) PUBLIC      ! Enabled along with User Exit 69 bit
INTEGER*2 TRXX
END SUB
!EIO10105
