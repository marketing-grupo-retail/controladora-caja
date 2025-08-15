INTEGER*1 Terror%

ON ERROR GOTO PAILA
Open "TF:NEWCLT" KEYED RECL 55 AS 4	               	   ! Apertura Articulos controlados
   CLEARS
   
   INPUT "INGRESE CLIENTE A BUSCAR ";A$
   
   SL.IT.ITEMCODE$ = A$
   UE.B.TMP$ = "0000"
   
   UE.B.ITEM$ = PACK$(Right$("000000000000000000"+SL.IT.ITEMCODE$,18))
   UE.B.KEY$ = UE.B.ITEM$ 
   Terror% = -1
   Read Form "C9 2C20 C5 C1";#4 KEY UE.B.KEY$;     \! Lee registro
          XKey$ ,                             \ Identificacion del cliente UPD 9
          XApell$,                            \ Apellidos del cliente
          XName$,                             \ Nombre del cliente
          Xpto$,                              \ Puntos Totales del cliente
          Xtipo$                              ! Tipo de cliente
IF TERROR% <> -1 Then Begin 
      print "CLIENTE NO ENCONTRADO PARA PROMOCION"
   EndIf Else Begin
      print "CLIENTE"+UNPACK$(UE.B.KEY$)
      PRINT "APELLIDO "+XAPELL$
      Print "NUMBRE "+XNAme$
      Print "PUNTOS "+unpack$(xpto$)
   Endif 
   CLOSE 54
   STOP
PAILA:
 
 IF ERR = "EF" THEN Terror% = 0 : RESUME
 
 PRINT "ERROR APPL"
 STOP 