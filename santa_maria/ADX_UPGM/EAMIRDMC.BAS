\/* TIME STAMP BLOCK **********************************************
\** END OF TIME STAMP BLOCK **************************************/
\/****************************************************************/
\/*                                                              */
\/*      MODULE NAME: EAMIRDMC                                   */
\/*                                                              */
\/*      DESCRIPTIVE NAME: DATA MAINTENANCE ITEM RECORD USER     */
\/*                        EXITS.                                */
\/*                                                              */
\/*      COPYRIGHT:                                              */
\/*      5696-536 THIS MODULE IS "RESTRICTED MATERIALS OF IBM"   */
\/*      (C) COPYRIGHT IBM CORP 1986, 1993 ALL RIGHTS RESERVED   */
\/*      LICENSED MATERIALS - PROPERTY OF IBM REFER TO COPYRIGHT */
\/*      INSTRUCTIONS FORM NUMBER G120-2083                      */
\/*                                                              */
\/*      STATUS: RELEASE 2 LEVEL 0                               */
\/*                                                              */
\/*      FUNCTION: EAMIRDMC                                      */
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
\/*      ENTRY POINT: IRDMECPR                                   */
\/*                                                              */
\/*         PURPOSE:  Item Record Print User Exit                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
\/*                                                              */
\/*      ENTRY POINT: IRDMECPG                                   */
\/*                                                              */
\/*         PURPOSE:  Alter number of pages exit                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
\/*                                                              */
\/*      ENTRY POINT: IRDMECDC                                   */
\/*                                                              */
\/*         PURPOSE:  Display/Change screen exit                 */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
\/*                                                              */
\/*      ENTRY POINT: IRDMECCV                                   */
\/*                                                              */
\/*         PURPOSE:  Convert to displayable form                */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
\/*                                                              */
\/*      ENTRY POINT: IRDMECCB                                   */
\/*                                                              */
\/*         PURPOSE:  Convert from displayable form              */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
\/*                                                              */
\/*      ENTRY POINT: IRDMECMP                                   */
\/*                                                              */
\/*         PURPOSE:  PageDown pressed to show user screens.     */
\/*                                                              */
\/*                ***** RETURN CONTROL *****                    */
\/*                                                              */
\/*         LINKAGE:  Called from EAMDB5?C.BAS                   */
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
!* Variables declarations for this module                                    *!
!*****************************************************************************!
%INCLUDE EAMITEMR.J86       !* GLOBAL Item Record File variables             *!
%INCLUDE EAMCFINC.J86       !* Common Function GLOBAL variables              *!
%INCLUDE EAMIRRFG.J86       !* GLOBAL Routine Variables                      *!
%INCLUDE EAMIRCFV.J86       !* Common Function Variables                     *!
STRING  GLOBAL     IRDM.OLD.EXCPT$   ! EXPANDED EXCEPTION LOG DATA
STRING  GLOBAL     IRDM.NEW.EXCPT$   ! FOR EXP IRF FUNCTIONS

INTEGER*1 GLOBAL ON1, OFF0

%INCLUDE EAMIRDMV.J86       !* Common variables for this module              *!
%INCLUDE EAMIRDUV.J86       !* User variables                                *!
%INCLUDE EXTENDVA.J86       !* Variables Expansion EAMITEMR                  *!

!*****************************************************************************!
!* External Functions / Definitions                                          *!
!*****************************************************************************!
%INCLUDE EAMASMRT.J86       !* Assembler functions                           *!
%INCLUDE DMEXTR.J86         !* Display Manager Functions                     *!

!*****************************************************************************!
!* Display a message on the screen                                           *!
!*****************************************************************************!
SUB MMCFEC03(MSG.NO,MSG.STR$) EXTERNAL
  INTEGER*2 MSG.NO
  STRING    MSG.STR$
END SUB

!*****************************************************************************!
!* Obtain input from the current field                                       *!
!*****************************************************************************!
SUB MMCFEC04(ENTRY.FIELD$) EXTERNAL
  STRING ENTRY.FIELD$
END SUB

!*****************************************************************************!
!* Format an amount with or without a decimal place                          *!
!*****************************************************************************!
FUNCTION MMCFEC06$(NUM,NEED.DEC,NEED.DEL) EXTERNAL
  STRING MMCFEC06$
  INTEGER*2 NEED.DEC,NEED.DEL
  INTEGER*4 NUM
END FUNCTION

!*****************************************************************************!
!* Strip leading and trailing blanks from the passed string                  *!
!*****************************************************************************!
FUNCTION MMCFEC08$(FIELD$)  EXTERNAL
  STRING MMCFEC08$,FIELD$
END FUNCTION

!*****************************************************************************!
!* Display current Data Maintenance Screen                                   *!
!*****************************************************************************!
SUB DB5AEC02 EXTERNAL
END SUB

!*****************************************************************************!
!* Print current item details                                                *!
!*****************************************************************************!
SUB PRINT.ITEMR EXTERNAL
END SUB

!*****************************************************************************!
!* Print data in display manager                                             *!
!*****************************************************************************!
SUB MMCFEC05(RET.VAL,DM.CMD$) EXTERNAL   ! Handle display error
   INTEGER*2 RET.VAL
   STRING    DM.CMD$
END SUB

%INCLUDE EAMIRDMF.J86

!-------------------------------------------------------------------
! Presenta datos datos expansion EAMITEMR
!-------------------------------------------------------------------
sub presentaValores
   STRING TMP$

   ! Llama pantalla
   CALL MMCFEC05(DISPD(200),"DISPD")

   ! make field invisible
   CALL SETF("1")

   ! Codigo del producto
   CALL POSF(1)
   TMP$ = unpack$( IR.ITEMCODE$ )
   TMP$ = str$( val( TMP$ ) )
   CALL MMCFEC05( PUTF( TMP$), "DB 0" )

   ! Descripcion
   CALL POSF(2)
   CALL MMCFEC05( PUTF( IR.ITEMNAME$ ), "DB 0" )

   ! Precio canasta afecto IVA 12%
   CALL POSF(4)
   TMP$ = str$( IR.PRECIVA12 )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

   ! Unidad minima de compra
   CALL POSF(5)
   TMP$ = str$( IR.UNDMIN )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

   ! Cantidad por caja
   CALL POSF(7)
   TMP$ = str$( IR.CANTPESO )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

   ! Numero departamento comercial (grupo y departamento)
   CALL POSF(3)
   TMP$ = right$( "000000000" + str$( IR.NDEPTO ), 9 )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

   ! Codigo interno Mega Santa Maria
   CALL POSF(6)
   TMP$ = str$( IR.SKU )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

   ! Fecha vencimiento CANASTA formato AMMDDHHMM
   if left$(unpack$( IR.ITEMCODE$),5)	 = "00970" then begin
      CALL POSF(9)
      TMP$ = "202"
      CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )
      CALL POSF(10)
      TMP$ = str$(IR.VENC)	
      CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )
   endif
   
   ! Categoria
   CALL POSF(8)
   TMP$ = str$( IR.CATEG )
   CALL MMCFEC05( PUTF( TMP$ ), "DB 0" )

end sub

!-------------------------------------------------------------------
! Valida formato fecha expiracion AMMDDHHMM
!-------------------------------------------------------------------
function valida.formato.fecha.hora( tmp$ )
   integer*1 valida.formato.fecha.hora
   string tmp$

   valida.formato.fecha.hora = 0
   if len(tmp$) <> 9 then exit function
   if val(right$(tmp$,2)) < 0 or val(right$(tmp$,2)) > 59 then exit function    ! minutos
   if val(mid$(tmp$,6,2)) < 0 or val(mid$(tmp$,6,2)) > 24 then exit function    ! hora
   if val(mid$(tmp$,4,2)) < 1 or val(mid$(tmp$,4,2)) > 31 then exit function    ! dia
   if val(mid$(tmp$,2,2)) < 1 or val(mid$(tmp$,2,2)) > 12 then exit function    ! mes
   valida.formato.fecha.hora = -1

end function

!*****************************************************************************!
!*                                                                           *!
!* Subroutine     : IRDMECPR                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance Print Exit                  *!
!*                                                                           *!
!* Parameters     : PRN.SESS   Session number used for the printer           *!
!*                                                                           *!
!* Notes          : This routine is called at the end of printing the        *!
!*                  details from the item record file.  It allows the        *!
!*                  user to print any additional information they may        *!
!*                  have put in the expanded item record.                    *!
!*                                                                           *!
!*****************************************************************************!
SUB IRDMECPR(PRN.SESS) PUBLIC
  INTEGER*2 PRN.SESS
END SUB

!*****************************************************************************!
!*                                                                           *!
!* Function       : IRDMECPG                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance Maximum Pages Exit          *!
!*                                                                           *!
!* Parameters     : None.                                                    *!
!*                                                                           *!
!* Returns        : INTEGER*2   The maximum number of pages allowed          *!
!*                  This has a default value of 2.  Any pages above 2        *!
!*                  should be handled in exits IRDMECDC and IRDMECMP.        *!
!*                                                                           *!
!* Notes          :                                                          *!
!*                                                                           *!
!*****************************************************************************!
FUNCTION IRDMECPG PUBLIC
  INTEGER*2 IRDMECPG
  !***************************************************************************!
  !* Return the maximum number of pages (default is 2)                       *!
  !***************************************************************************!
  IRDMECPG = 3      ! Expansion Item Record
END FUNCTION



!*****************************************************************************!
!*                                                                           *!
!* Subroutine     : IRDMECDC                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance Display the Display/Add     *!
!*                  Change screens for pages 3 or above.                     *!
!*                                                                           *!
!* Parameters     : None.                                                    *!
!*                                                                           *!
!* Notes          : Called to display the screen for the current page        *!
!*                  (if greater than 2) from DB5AEC                          *!
!*                                                                           *!
!*                                                                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRDMECDC PUBLIC

   ! Muestra los nuevos campos del EAMITEMR
   if PAGE = 3 then call presentaValores

END SUB

!*****************************************************************************!
!*                                                                           *!
!* Subroutine     : IRDMECCV                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance Convert to displayable form *!
!*                                                                           *!
!* Parameters     : None                                                     *!
!*                                                                           *!
!* Notes          : This routine is called when the item record file is      *!
!*                  converted into displayable format.  It is a good place   *!
!*                  to handle converting any additional data in the item     *!
!*                  record into displayable form.                            *!
!*                                                                           *!
!*****************************************************************************!
SUB IRDMECCV PUBLIC
END SUB

!*****************************************************************************!
!*                                                                           *!
!* Subroutine     : IRDMECCB                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance convert from displayable    *!
!*                                                                           *!
!* Parameters     : None                                                     *!
!*                                                                           *!
!* Notes          : This routine is called when the item record file is      *!
!*                  converted from displayable format.  It is a good place   *!
!*                  to handle converting any additional data in the item     *!
!*                  record before writing to file.                           *!
!*                                                                           *!
!*                : To include the expanded user data information in         *!
!*                  the information written to the exception log             *!
!*                  update the following GLOBAL variables which are          *!
!*                  appended to the old and new exception log fields.        *!
!*                     IRDM.OLD.EXCPT$                                       *!
!*                     IRDM.NEW.EXCPT$                                       *!
!*                                                                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRDMECCB PUBLIC
END SUB

!*****************************************************************************!
!* Handle the additional pages................                               *!
!*                                                                           *!
!* returns IRDM.RC                                                           *!
!*                                                                           *!
!* Subroutine     : IRDMECMP                                                 *!
!*                                                                           *!
!* Description    : Item Record Data Maintenance Page Down Exit              *!
!*                                                                           *!
!* Parameters     : None                                                     *!
!*                                                                           *!
!* Notes          : This routine is called when PgDn is pressed to           *!
!*                  display additional pages of item record information.     *!
!*                  When we return the action taken depends on the value     *!
!*                  of the MMCFEC04 variables ENTER.PRESSED, QUIT.PRESSED    *!
!*                  ESC.PRESSED, PGUP.PRESSED,....                           *!
!*                                                                           *!
!*****************************************************************************!
SUB IRDMECMP PUBLIC
   integer*1 opc, cam
   string    tmp$

   !la variable SELECTION$ contiene el punto de menu seleccionado
   ! "1" Change a record
   ! "2" Add a record
   ! "3" Erase a record
   ! "4" Display Print a record 

   ! Cuando se llega a la tercera pagina
   if PAGE = 3 then begin

      !Presenta valores del EAMITEMR extendido
      call presentaValores

      ! Captura de datos
      capturaAdicional:

      NEED.RANGE.CHECK = ON1
      MONEY.FILTER = ON1
      HIGH.END$ = "9999999999"

      if left$(unpack$( IR.ITEMCODE$),5) = "00970" then begin
         ! Precio canasta afecto IVA 12%
         cam = 4
         precioCanasta:
         gosub capturaValores
         if opc > 0 then goto salir
         if val( tmp$ ) > val( unpack$( IR.SALEPRIC$ ) ) then begin
            TMP$ = "Error, Monto afecto no puede ser superior al valor del item"
            CALL MMCFEC05( PUTF( TMP$), "DB 0" )
            goto precioCanasta
         endif
         IR.PRECIVA12 = val( tmp$ )
      endif

      ! Unidad minima de compra
      cam = 5
      gosub capturaValores
      if opc > 0 then goto salir
      IR.UNDMIN = val( tmp$ )

      ! Cantidad por caja
      cam = 7
      gosub capturaValores
      if opc > 0 then goto salir
      IR.CANTPESO = val( tmp$ )

      ! Departamento Comercial
      cam = 3
      gosub capturaValores
      if opc > 0 then goto salir
      tmp$ = "1" + right$( "000000000" + tmp$, 9 )
      IR.NDEPTO = val( tmp$ )

      ! Codigo interno
      cam = 6
      gosub capturaValores
      if opc > 0 then goto salir
      IR.SKU = val( tmp$ )

      if left$(unpack$( IR.ITEMCODE$),5) = "00970" then begin
         ! Fecha vencimiento CANASTA
         cam = 10

otra.fecha:

         gosub capturaValores
         if opc > 0 then goto salir
         if valida.formato.fecha.hora( tmp$ ) = 0 then begin
            CALL POSF(250)
            TMP$ = "Error, Fecha Hora fuera de formato AMMDDHHMM"
            CALL MMCFEC05( PUTF( TMP$), "DB 0" )
            goto otra.fecha
         endif
         IR.VENC = val( tmp$ )
      endif

      !Borra mensaje de error
      CALL POSF(250)
      TMP$ = string$(40," ")
      CALL MMCFEC05( PUTF( TMP$), "DB 0" )

      ! Codigo categoria
      cam = 8
      gosub capturaValores
      if opc > 0 then goto salir
      IR.CATEG = val( tmp$ )

      NEED.RANGE.CHECK = 0
      MONEY.FILTER = 0
      HIGH.END$ = "4"

      if TAB.PRESSED then goto capturaAdicional

   endif

   salir:
   exit sub

   capturaValores:
   CALL POSF( cam )
   CALL MMCFEC04( tmp$ )
   opc = 0
   !if ENTER.PRESSED then opc = 0
   if ESC.PRESSED then opc = 1
   if QUIT.PRESSED then opc = 2
   if PGDN.PRESSED  then opc = 3
   if PGUP.PRESSED  then opc = 4
   return

END SUB
