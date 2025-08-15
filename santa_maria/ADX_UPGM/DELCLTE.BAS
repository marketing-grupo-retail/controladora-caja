!-- Programa borrado de cliente

String Global IFile$, Clte$, nom$, dir$, tel$, pas$, A$
Integer*1 Global Terror%
Integer*4 Global band, codSRI, email, pasap

On Error GoTo Fallas
Ifile$ = "CLIENTES"
Terror% = -1
Open Ifile$ Keyed Recl 84 As 84
If Terror% <> -1 Then Begin
   Print "Error apertura archivo clientes"
   Stop 
EndIf
captura.cliente:
Clears
Print "Grupo Retail Ltda"
Print "Programa para Borrado de clientes"
Print String$(78,"-")
Input "Cliente a Borrar:";Clte$
Terror% = -1
Clte$ = Pack$(Right$("00000000000000"+Clte$,14))
Read Form "C7, C30, C30, C5, 2I1 2I4 C2"; #84 key Clte$; \
     Clte$, nom$, dir$, tel$, band, codSRI, email, pasap, pas$
If Terror% = -1 Then Begin
	 Print " "
	 Print "Cliente  :"+nom$
	 Print "Direccion:"+dir$
	 Print "Teléfono :"+unpack$(Tel$)
	 Input "Desea eliminar S/N ? ";A$
	 If Ucase$(A$) = "S" Then Begin
	 	  DelRec 84 ; Clte$
	 	  Input "Cliente ha sido borrado.. /Enter";A$
	 EndIf
EndIf Else Begin
	 Print "Cliente capturado no existe"
	 Input "Desea continuar S/N ?";A$
	 If Ucase$(A$) = "S" Then GoTo captura.cliente
EndIf
Close 84
Stop 

Fallas:
 Terror% = 0
 If err = "OE" Then Resume
 If err = "EF" Then Resume
 If err = "KF" Then Resume
 Print "Error de aplicacion "+err 
 Stop 
 
