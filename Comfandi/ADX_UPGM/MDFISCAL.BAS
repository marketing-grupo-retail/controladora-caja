!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : MDFISCAL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Actualizacion Consecutivo        *
!                Numeracion Fiscal                *
!**************************************************

String    Global Term$, Clave$
Integer*1 Global Terror%

Sub Cambio.Fiscal
String Lec$, U.KEY$,U.SERIAL$, U.PREF$, U.AUT$, U.FEC$,U.Nit$, U.Razon$
Integer*4 U.INI%, U.FIN%, U.ACT%, U.new%
Terror% = -1
Open "TF:NUMFIS" KEYED RECL 76 AS 1

!Open "c:\log\fiscal.dat" KEYED RECL 76 AS 1

Term$ = Pack$(Right$("0000"+Term$,4))
LEC$="C2 C15 C4 2I4 C6 C3 I4 C14 C20"	                           !
READ FORM LEC$;#1 KEY Term$;                      \! Lee Reg Cabecera 
  U.KEY$,U.SERIAL$, U.PREF$, U.INI%, U.FIN%, U.AUT$, U.FEC$, U.ACT%, U.Nit$, U.Razon$

If Terror% = -1 Then Begin 
	 Print "Consecutivo Fiscal Actual :",U.ACT%
	 Input "Nuevo Consecutivo Fiscal ENTER(Acepta) Ctrl-C(Anula) :";U.New%
	 If U.New% > 0 Then U.Act% = U.New%
   Write Form "C2 C15 C4 2I4 C6 C3 I4 C14 C20"	;#1 ;         \! Grabacion de registros
        U.Key$,     \ Numero de terminal
        U.SERIAL$,  \ Numero serial
        U.PREF$,    \ Prefijo de facturacion
        U.INI%,     \ Rango inicial
        U.FIN%,     \ Rango Final
        U.AUT$,     \ Numero Autorizacion
        U.FEC$,     \ Fecha de autorizacion
        U.ACT%,     \ Numero actual de registro
        U.NIT$,     \ Nit que factura
        U.RAZON$    ! RAZON SOCIAL FACTURA
	 
EndIf Else Begin 
	Print "Error: Definicion Fiscal no definida..."
EndIf

End Sub 

On Error GoTo ErrAppl
Clears
Print "Actualizacion Consecutivo Fiscal - Grupo Retail Ver 1.0"
Print " "
Input "Clave Numeracion Fiscal (1-9) :";Clave$
Input "Numero de Terminal            :";Term$
Term$  = Right$("000"+Term$,3)
Clave$ = Right$("0"+Clave$,1)
Term$  = Clave$ + Term$
Call Cambio.Fiscal
Close 1
Stop 


ErrAppl:
Terror% = 0
If ERRF% = 1 And (ERR = "OE" OR ERR = "FU") Then BEGIN ! El archivo numeracion fiscal no existe
	 Print "Archivo Numeracion Fiscal No Existe"
	 Stop 
EndIf
If ERR = "EF" Then Resume 
Print "ERROR APLICACION :"+ERR+" SESION:"+STR$(ERRF%)
Stop 
