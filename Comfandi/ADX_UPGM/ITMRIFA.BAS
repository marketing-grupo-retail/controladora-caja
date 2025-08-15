Integer* 1 Global Terror%, XG%
String     Global Item$, Rifa$, Busca$, Clte$,\
                  Xcod$, Xname$, Xfec1$, Xfec2$,  \!
                  Xvlr$, Xbol$, Xcon$, Xmsg$, ASC.MKP.CADENA$, Xtmp$

on error goto paila
Terror% = -1
ASC.MKP.CADENA$ = "0000"

Open "TF:LR004" KEYED RECL 301 As 11               ! Lista de Rifas
If Terror% <> -1 Then Begin
	Print "Error Apertura Lista rifas"
	Stop 
EndIf

Terror% = -1
Busca$ = Pack$("99")                                                !
Read Form "C1 C300";#11 KEY Busca$; Item$, Rifa$
If Terror% <> -1 Then Begin
	Print "No hay definicion de rifas"
	Stop 
EndIf
Close 11
Print "Lista Rifas"
Print Rifa$

Terror% = -1
Open "TF:A0004" KEYED RECL 8 AS 59                                  ! Validacion de rifas por producto
If Terror% <> -1 Then Begin
	Print "Error Apertura definicion Items rifas"
	Stop 
EndIf
Input "Cliente Rifa ";Clte$
Input "Codigo de la rifa ";Rifa$
Input "Item a Consultar  ";Item$
Rifa$ = Right$("0000"+UCase$(Rifa$),4)
Item$ = Right$("000000000000"+Item$,12)
Clte$ = Right$("0000"+UCase$(Clte$),4)
Busca$ = Pack$( Rifa$ + Item$ )

Terror% = -1
Read Form "C8";#59 KEY Busca$;Busca$     				! Lectura del registro
If Terror% = -1 Then Begin 
	 Print "Item Encontrado para Rifa"
EndIf Else Print "Item No Encontrado para Rifa"

Close 59

Terror% = -1
Open "TF:P0004" KEYED RECL 40 AS 12         ! Validacion de rifas por producto
If Terror% <> -1 Then Begin
	Print "Error Apertura Parametros rifas"
	Stop 
EndIf

For XG% = 1 To Len(ASC.MKP.CADENA$) STEP 4			    			  ! Hasta longitud cadena
    XTmp$ = Mid$(ASC.MKP.CADENA$,XG%,4)
    Print "promo valida "+Xtmp$
Next Xg%
Busca$ = Pack$( Rifa$ + Clte$ )

Read Form "C4 C18 2C4 C5 C2 C1 C2";#12 KEY Busca$;              \! Consulta registro
    Xcod$, Xname$, Xfec1$, Xfec2$,                          \!
    Xvlr$, Xbol$, Xcon$, Xmsg$

If Terror% <> -1 Then Begin
	Print "Datos Rifa No Encontrados"
	Stop 
EndIf
Print "Nombre Rifa :"+Xname$
Print "Fecha 1     :"+UnPack$(Xfec1$)
Print "Fecha 2     :"+UnPack$(Xfec2$)
Print "Valor       :"+UnPack$(Xvlr$)
Print "Boletas     :"+UnPack$(Xbol$)
Print "no se que   :"+UnPack$(Xcon$)
Print "Mensaje     :"+UnPack$(Xmsg$)

Close 12
Stop

paila:
Terror% = 0
Resume 
