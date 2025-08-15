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
\/* Modificaciones:                                              */
\/* Se adiciona el manejo y control de la expansion de datos en  */
\/* la maestra de articulos pasando de 46 caracteres a 77,       */
\/* desarrollado por Grupo Retail (OVS) Abril 7 de 2.010         */
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
%INCLUDE EAMIRDMV.J86       !* Common variables for this module              *!
%INCLUDE EAMIRDUV.J86       !* User variables                                *!

%INCLUDE EXIRTSVA.J86	    !* Variables Expansion EAMITEMR

INTEGER*1 GLOBAL ON1, OFF0

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

SUB STRIP.BLANKS(FLDATA$)
   STRING FLDATA$
   INTEGER*1 I
   FLDATA$ = MMCFEC08$(FLDATA$)  ! strip leading and trailing blanks
   I = MATCH(" ",FLDATA$,1)        ! strip imbedded blanks
   IF I > 0 THEN FLDATA$ = LEFT$(FLDATA$,I-1)
END SUB

%INCLUDE EAMIRDMF.J86

!-------------------------------------------------------------------
!--- Rutinas Adicionales creadas por Grupo Retail (OVS)
!-------------------------------------------------------------------

Sub PRESENTA.ADICIONALES
  String UE.DATA$, UE.TMP$, UE.SKU$
  IF PAGE = 3 THEN BEGIN												      		! Si tercera pantalla
    CALL MMCFEC05(DISPD(200),"DISPD")											! Llama pantala
    CALL SETF("1")                 												! make field invisible
    UE.DATA$ = UNPACK$(IR.ITEMCODE$)											!
    UE.DATA$ = STR$(VAL(UE.DATA$))												!
    UE.TMP$  = Right$("0000000000000000"+Asc.IR.NDEPTO$,15) ! Estructura comercial
    CALL POSF(1)                  												! Posicionando 
    CALL MMCFEC05(PUTF(UE.DATA$),"DB 0") 									! Codigo del producto
    CALL POSF(2)                  												! Posicionando 
    CALL MMCFEC05(PUTF(IR.ITEMNAME$),"DB 0") 		      		! Descripcion
    CALL POSF(3)                  												! Posicionando 
    CALL MMCFEC05(PUTF(UE.TMP$),"DB 0")                   ! Numero departamento comercial
    UE.TMP$ = Str$(ASC.IR.CONSUMO%)
    CALL POSF(4)                  												! Posicionando 
    CALL MMCFEC05(PUTF(UE.TMP$),"DB 0")                   ! Numero departamento comercial
    UE.TMP$ = Str$(ASC.IR.FECHA%)
    CALL POSF(5)                  												! Posicionando 
    CALL MMCFEC05(PUTF(UE.TMP$),"DB 0")                   ! Numero departamento comercial

EndIf

END SUB
!--- Fin presentacion datos expansion EAMITEMR

Function CAPTURA.ADICIONALES(X.POS%) Public
Integer*2 X.POS%
String ENTRA.DATO$, CAPTURA.ADICIONALES
    Call POSF(X.POS%)                  											! Posicionando 
    Call MMCFEC04(ENTRA.DATO$)
    IF ESC.PRESSED   THEN ENTRA.DATO$ = "SALIDA"
    IF QUIT.PRESSED  THEN ENTRA.DATO$ = "SALIDA"
    IF ENTER.PRESSED THEN ENTRA.DATO$ = "SALIDA"
    IF PGDN.PRESSED  THEN ENTRA.DATO$ = "SALIDA"
    IF PGUP.PRESSED  THEN ENTRA.DATO$ = "SALIDA"
    CAPTURA.ADICIONALES = ENTRA.DATO$
End Function
!--- Captura de datos adicionales

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
  !IRDMECPG = 2
  
  IRDMECPG = 3 ! Expansion Item Record 
  
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
SUB IRDMECDC Public
    CALL PRESENTA.ADICIONALES					! Datos adicionales
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
Sub IRDMECMP Public
  String  UE.DATA$, UE.TMP$
  If PAGE = 3 Then Begin														      ! Si tercera pantalla
   CALL PRESENTA.ADICIONALES													    ! Presenta datos
   CAPTURA.ADICIONAL:															        ! Captura de datos

!    NEED.RANGE.CHECK = ON1														    !
!    MONEY.FILTER = ON1															      !
!    HIGH.END$ = "9999999999"													    !

    NEED.RANGE.CHECK = 0
    MONEY.FILTER = 0
    HIGH.END$ = "4"

    UE.DATA$ = CAPTURA.ADICIONALES(3)											! Departamento Comercial
    IF UE.DATA$ = "SALIDA" THEN EXIT SUB									!
    UE.DATA$ = "1" + Right$("000000000000000"+UE.DATA$,15)
    ASC.IR.NDEPTO$   = UE.DATA$       				  		      !

    NEED.RANGE.CHECK = ON1														    ! Validacion datos numericos
    MONEY.FILTER = ON1															      !
    HIGH.END$ = "9999999999"													    !

    UE.DATA$ = CAPTURA.ADICIONALES(4)											! Fecha de actualizacion Maestro
    IF UE.DATA$ = "SALIDA" THEN EXIT SUB									!
    ASC.IR.CONSUMO% = Val(UE.DATA$)       		  		      !

!    UE.DATA$ = CAPTURA.ADICIONALES(5)											! Fecha de actualizacion Maestro
!    IF UE.DATA$ = "SALIDA" THEN EXIT SUB									!
	
    UE.DATA$ = "20" + DATE$
    ASC.IR.FECHA%   = Val(UE.DATA$)       				  		      !

    NEED.RANGE.CHECK = 0
    MONEY.FILTER = 0
    HIGH.END$ = "4"

    IF TAB.PRESSED THEN GOTO CAPTURA.ADICIONAL

 ENDIF
END SUB
