Integer*1 Terror%
Integer*4 Tpromo%, Xp1%, Xp2%
String    Lista$, Promos$(2)
ON ERROR GOTO PAILA
   Dim Promos$(5000,2)
   tpromo% = 0
   OPEN "promodat:dpromoti.dat"  AS 54 ! Apertura promociones
   If End #54 THEN SEG.FINAL
   Print "procesando, promociones..., Espere por favor..."
   While (1)
    Read #54; Line  Lista$
    Xp1% = Match("</cod>",Lista$,1)
    If Xp1% > 0 Then Begin														! Hay promocion
    	 Tpromo% = Tpromo% + 1 
    	 Promos$(Tpromo%,0) = Mid$(Lista$,11,(Xp1%-11))  ! Codigo de la promocion
       Xp1% = Match("<idt>",Lista$,1)
       Promos$(Tpromo%,1) = Mid$(Lista$,Xp1%+5,10)    ! Fecha inicio promocion
       Xp1% = Match("<fdt>",Lista$,1)
       Promos$(Tpromo%,2) = Mid$(Lista$,Xp1%+5,10)    ! Fecha inicio promocion
       
    EndIf
   Wend 
   seg.final:
    Xp1% = 0
    clears
    Print "PROMO      Fecha Inicial  Fecha Final    Estado"
    Xp2% = 2
    For Xp1% = 1 To Tpromo%
     Print Right$("       "+Promos$(Xp1%,0),7) + "    " + Promos$(Xp1%,1) + "     " + Promos$(Xp1%,2)
     If Xp2% > 21 Then Begin 
     	 Input "Presione Enter Continuar...";lista$
     	 Clears
     	 Print "PROMO      Fecha Inicial  Fecha Final    Estado"
     	 Xp2% = 2
     EndIf
     Xp2% = Xp2% + 1
    Next Xp1%
    Input "Fin de la consulta Presione Enter Continuar...";lista$
    Close 54
    Stop

PAILA:
 
 IF ERR = "EF" THEN Terror% = 0 : RESUME
 
 PRINT "ERROR APPL "+ERR
 STOP 
