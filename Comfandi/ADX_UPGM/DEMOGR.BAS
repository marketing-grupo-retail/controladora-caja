!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMABBOT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Dscto Escalonado Abbot    *
!**************************************************
!Observaciones:

%ENVIRON T		                         																			! Ambiente de terminal

%INCLUDE EAMTSEVA.J86          																							! Variables Cliente Frecuente
%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTOPTS.J86          																							! Terminal Options
%INCLUDE EAMTRANS.J86          																							! Variables de transancion
%INCLUDE EAMITEMR.J86          																							! Variables EamItemr
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011        																								! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET External 
End Sub 

Sub TSPREC01 EXTERNAL          ! print routine
End Sub

Sub TSTDEC01 EXTERNAL                           ! TENDERING
End Sub                                          !

Sub DemoGr(Xopt%) Public
Integer*4 Xopt%

If Xopt% = 14 Then Begin
	 If TS.IO.MOTORKEY = 171 Then BEGIN
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
      TS.IO.DATA$(3) = "1"                                           	! Variedad de Pago
      TS.IO.KEYS(3)  = 78                                           	! Tecla Slash
      TS.IO.DATA$(7) = "1000"                                  				! Valor a entregar
      TS.IO.KEYS(7)  = 91                                            	! Tipo de Pago
      TS.IO.DATA$(9) = "79547699"            										      ! Nit asociado
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																	!
      Call TSTDEC01                                   !
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												! Init vectores de carga
      TS.IO.MOTORKEY = 73
	 ENDIF 
	
EndIf

End Sub 
