!--- Consulta consumo cliente x categoria
!--- Dia sin iva
!--- Grupo Retail 18/Jun/2020

Integer*1 Global Terror%
String    GLOBAL Xkey$, XQty$, Xclte$, XStruc$
on error goto paila
CLEARS
Terror% = -1
Open "TF:CNOTAX" KEYED RECL 19 AS 95                  							  ! Control de compra clientes
Input "ID del Cliente   : ";Xclte$
Input "Codigo Estructura: ";Xstruc$
Xstruc$ = "1" + Right$("000000000000000"+Xstruc$,15)
Xkey$ = Pack$(Right$("000000000000000000"+Xclte$,18) + Xstruc$)                           ! cliente + structura
Terror% = -1
Read Form "C17 C2 ";#95 Key Xkey$;                                \! Busca estructura
     Xkey$, Xqty$                                                       ! 
If Terror% = -1 Then Begin
   Print "Cantidad Consumida: "+unpack$(Xqty$)
EndIf Else Begin
   Print "DATO A CONSULTAR NO EXISTE"
EndIf
Close 95
Stop 

Paila:
Terror% = 0
If ERR = "EF" Then Resume
Print "Error: "+ERR+" Sesion: "+STR$(ERRF%)
Stop  