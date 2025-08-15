
String Global Clte$

On Error GoTo Paila
Clears
Open "TF:NEWCLT" KEYED RECL 85 AS 5	                	   ! Apertura Articulos controlados
Input "ENTRE DATO DEL CLIENTE ";CLTE$
Clte$ = PACK$(Right$("000000000000000000"+Clte$,18))							! Arma llave de busqueda
   Read Form "C9 2C20 C5 C1 C30";#5 KEY Clte$;  	  												\ Lee registro
          XKey$ ,                             																\ Identificacion del cliente UPD 9
          XApell$,                            																\ Apellidos del cliente
          XName$,                             																\ Nombre del cliente
          Xpto$,                              																\ Puntos Totales del cliente
          Xtipo$,                              																\ Tipo de cliente
          XTpClte$																														! Tipificacion de clientes

Print "NOMBRE VICTIMA "+XNAME$
Print "DATOS DE TIPIFICACION "+XTpClte$
Close 5
Stop 

Paila:
  Print "Paila Papa... Error "+ERR
  Stop 
