\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/*                                                               */
\/*      COPYRIGHT:                                               */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"    */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED    */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT  */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                       */
\/*                                                               */
\/*      STATUS: RELEASE 2 LEVEL 0                                */
\/*                                                               */

SUB CPIVEC01 (ITEMCODE$, ITEMVALID, ITEMKEY$) PUBLIC
! CPIVEC01 - USER EXIT DURING PROCESSING OF BAR CODE
! This user exit is called at the end of the item code validation.
! The passed variables indicate the results of the validation and
! conversion from item code to a item record key.
  STRING    ITEMCODE$    ! The original itemcode entered
  INTEGER*2 ITEMVALID    ! Flag indicating if the item code is valid
  STRING    ITEMKEY$     ! Corresponding item record key produced
END SUB

