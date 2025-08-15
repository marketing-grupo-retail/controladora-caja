String Global Gr.Fis.Numer$(2), a$
Integer*1 Terror%

On Error GoTo pailas
Dim Gr.Fis.Numer$(3,8)																											! Matriz numeracion fiscal
Terror% = -1
Open "TF:NMFECO" KEYED RECL 98 As 11 Locked  								! Abre control numeacion fiscal
If Terror% = -1 Then Begin
	 Input "Presionen enter ";a$
EndIf
Close 11
Stop 

pailas:
Terror% = 0
If ERR = "EF" Then Resume
If ERR = "IH" Then Resume
	
Print "Error: "+ERR+" Sesion: "+STR$(ERRF%)

Stop 