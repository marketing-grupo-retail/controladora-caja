!************************************************** 
!Empresa       : COMFANDI                         *
!Programa      : FDEMEXIT.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio del 2004                   *
!Observaciones : Modulo Fondo de Empleados        *
!**************************************************
!Mod 08Nov2013
!Se retira una impresion de mensaje fondecom
!al presionar la tecla de consulta de cupo de fondecom
!desarrollado por Grupo Retail - ovs
!--------------------------------------------------

%Environ T

%Include EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%INCLUDE EAMTERMS.J86          !
%INCLUDE EAMTOPTS.J86	         !
%Include FDETSUVA.011          ! Definicion de variables
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !

%Include FEMTSUSU.011          ! Subrutinas 

Sub TSHIECET External 
End Sub 

Function FORMAT.AMOUNT(AMT1) External
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
End Function


Sub FDE.CONSULTA
String X.Nombre$
Integer*4 X.Saldo%
    TS.ER.RETURN = -1
    Open "R::$AMFDEMP" Keyed Recl 29 As Com.Fde.Sesion% UNLOCKED NOWRITE NODEL
    If TS.ER.RETURN = -1 Then Begin
       Com.Fde.Socio$ = ASIC.DATOS$("Ingrese CC Empleado?","")
       IF (Com.Fde.Socio$ = "") OR (Com.Fde.Socio$ = "E") THEN BEGIN					   ! No se capturo dato
          DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)																	 !
          TS.IO.MOTORKEY = 73	
          Close Com.Fde.Sesion%
          Exit Sub 
       EndIf 
       Com.Fde.Socio$ = Pack$(Right$("0000000000"+Com.Fde.Socio$,10))
       TS.ER.RETURN = -1
       Read Form "C5 C20 I4"; #Com.Fde.Sesion% Key Com.Fde.Socio$ ; \
         Com.Fde.Socio$, X.Nombre$, X.Saldo%
       If TS.ER.RETURN = -1 Then Begin
         Call Format.AMOUNT(X.SALDO%)
         Call TSHIECET
         Call VISORES4690(1,X.Nombre$,"Saldo: "+TS.TEMP1$,1800,"L")
         Close Com.Fde.Sesion%
         Exit Sub        
       EndIf Else Begin 
         Call TSHIECET
         Call VISORES4690(1,"Empleado No","Autorizado",1800,"C")
         Close Com.Fde.Sesion%
         Exit Sub 
       EndIf  
    EndIf Else Begin
      Call TSHIECET
      Call VISORES4690(1,"Arch. Fondo Empleado","No Existe",1800,"C")
    EndIf 
End Sub


Sub FDEMTS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Fondo de Empleados                                   **
!   **    User Exit  :  FDEMTS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

   %Include FEMTSU07.011

End Sub 

Sub FDEMTS14.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Fondo de Empleados                                   **
!   **    User Exit  :  FDEMTS14.011                                         **
!   **    Incluir en :  EAMTSU13.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Com.Fde.Line% Then Begin 
   %Include FEMTSU14.011
EndIf 

End Sub 
