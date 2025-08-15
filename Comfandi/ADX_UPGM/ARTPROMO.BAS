INTEGER*1 Terror%

ON ERROR GOTO PAILA

Open "MKPPRITM" KEYED RECL 31 AS 54                    	            ! Open promociones
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****     CONSULTA ARTICULOS PROMOCIONADOS   POS     ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Noviembre 23 2011  G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 7, 1
   Input  "Ingrese producto a consultar :";A$
   Input  "Codigo de la promocion       :";B$
   Print  " "
   SL.IT.ITEMCODE$ = A$
   UE.B.TMP$ = Right$("0000"+b$,4)
   
   UE.B.ITEM$ = PACK$(Right$("000000000000"+SL.IT.ITEMCODE$,12))
   UE.B.KEY$ = UE.B.ITEM$ + PACK$(UE.B.TMP$)               ! Arma llave
   Terror% = -1
   Read Form "C8 C1 C4 2C2 C12 C2";#54 KEY UE.B.KEY$;     \! Lee registro
          UE.B.TMP$, UE.B.STAT$, UE.B.I4$, UE.B.LIGA$,         \!
          UE.B.PROMO$, UE.B.DATA$, Xpromo$
   IF TERROR% <> -1 Then Begin 
      print "ARTICULO NO ENCONTRADO PARA PROMOCION"
      Input "Presione Enter para Continuar... ";Xpromo$
   EndIf Else Begin
      print "STATUS    :"+UE.B.STAT$
      PRINT "PROMOCION :"+UNPACK$(Xpromo$)
      Print "BENEFICIO :"+UNPACK$(UE.B.I4$)
      Print "UE DATA   :"+UE.B.DATA$
      Input "Presione Enter para Continuar... ";Xpromo$
      !delrec 54 ; UE.B.KEY$
   Endif 
   CLOSE 54
   STOP
PAILA:
 
 IF ERR = "EF" THEN Terror% = 0 : RESUME
 
 PRINT "ERROR APPL"
 STOP 
