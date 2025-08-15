Integer*1 GLOBAL TERROR%
String XLEC$, XVAL$, RIFA$, CLTE$, Xcod$, Xname$, Xfec1$, Xfec2$,                          \!
    Xvlr$, Xbol$, Xcon$, Xmsg$, Xkey$


ON ERROR GOTO ERRAPPL
 Open "TF:P0004" KEYED RECL 40 AS 1         ! Validacion de rifas por producto
 XLEC$ = "C4 C18 2C4 C5 C2 C1 C2"
 Xval$ = "20" + Date$                                       ! Fecha de validacion 
 CLEARS
 Input "CODIGO DE LA RIFA :";RIFA$
 Input "CLIENTES DESTINO  :";CLTE$
 TERROR% = -1
 Xkey$ = Pack$(Right$("0000"+rifa$,4) + Right$("0000"+CLTE$,4))
 
    Read Form Xlec$;#1 KEY Xkey$;              \! Consulta registro
    Xcod$, Xname$, Xfec1$, Xfec2$,                          \!
    Xvlr$, Xbol$, Xcon$, Xmsg$
    
If TERROR% = -1 Then BEGIN 
	 Print "-- Rifa Definida --"
	 Print "Nombre :"+Xname$
	 Print unpack$(Xfec1$)
   Print unpack$(Xfec2$)
   Print unpack$(Xvlr$)
   Print unpack$(Xbol$)
   Print unpack$(Xmsg$)
   
ENDIF Else Print "RIFA NO EXISTE"
Close 1
Stop 

ERRAPPL:
TERROR% = 0
If ERR = "EF" Then Resume
Print "ERROR APLICACION "+ERR

STOP