Integer*1 Terror%
Integer*4 Tpromo%, Xp1%, Xp2%
String    Lista$, Rta$, ItmsNtCrd$(2)
Integer*4 Asc.Tmp.Apun%, Asc.Tmp.Apun2%


Function Asic.Getunpk5(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk5
  Asc.Tmp.Apun% = Match(",",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk5 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk5 = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk6(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk6
  Asc.Tmp.Apun2% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun2% > 0 Then \
     Asic.Getunpk6 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun2% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk6 = ""
  Asc.Tmp.Apun2% = Asc.Tmp.Apun2% + 1
End Function


Sub FECO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) External							! Auditoria FECO
String X.ENVIA$, X.LLEGA$, X.SALE$
End Sub 

Sub CARGAR.ITMS.NTACRD(Xlista$)																							! Recuperacion Items nota cred
String Xean$, XQty$, Xpup$, Xlista$, Xitm$																	!
  Asc.Tmp.Apun% = 1 																												!
  Dim ItmsNtCrd$(800,3)																											!
  Gr.NtaCrd.Itm% = 0																												!
  While Asc.Tmp.Apun% < Len(Xlista$)																				!
   XItm$ = Asic.Getunpk5(Xlista$,Asc.Tmp.Apun%)															! Lista items
   Print "Rec:"+Xitm$
   If XItm$ <> "" Then Begin																								! Lista recuperada
   	  Asc.Tmp.Apun2% = 1																										! Apuntador recuperacion
      Xean$ = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)														! Plu registro
      XQty$ = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)														! Qty Vendida
      Xpup$ = Asic.Getunpk6(Xitm$,Asc.Tmp.Apun2%)														! Precio unitario
      Gr.NtaCrd.Itm% = Gr.NtaCrd.Itm% + 1																		! Total Items 
      ItmsNtCrd$(Gr.NtaCrd.Itm%,0) = Xean$																	! Almacena vector control
      ItmsNtCrd$(Gr.NtaCrd.Itm%,1) = XQty$																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,2) = Xpup$																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,3) = "0"																	  !
      TS.TEMP1I1  = -1 																											! Control del proceso 
   EndIf Else Begin																													! Termina recuperacion
   	If Gr.NtaCrd.Itm% > 0 Then Begin																				! Hay itms recuperados
   		Gr.NtaCrd.Itm% = Gr.NtaCrd.Itm% + 1																		! Total Items 
      ItmsNtCrd$(Gr.NtaCrd.Itm%,0) = "999999999999"													! Registro cierre recuperacion
      ItmsNtCrd$(Gr.NtaCrd.Itm%,1) = "1"  																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,2) = "0"  																	!
      ItmsNtCrd$(Gr.NtaCrd.Itm%,3) = "0"																	  !
   	  Asc.Tmp.Apun% = Len(Xlista$)																					!
   	Endif
   EndIf																																		!
  Wend 																																			!
  
End Sub 												

On Error GoTo PAILA
Clears
Print "CARGANDO RESPUESTA NOTA CREDITO"
Rta$ = "32358:1:3900:,256229:1:12100:,22391:1:2800:,2999:1:3300:,112562:1:400:,268980:1:250:                "

Call CARGAR.ITMS.NTACRD(Rta$)
Print ("TOTAL ITEMS "+Str$(Gr.NtaCrd.Itm%))
For Xp1% = 1 To Gr.NtaCrd.Itm%
    Print "EAN:"+ItmsNtCrd$(XP1%,0)
    Print "QTY:"+ItmsNtCrd$(XP1%,1)
    Print "PVP:"+ItmsNtCrd$(XP1%,2)
Next XP1%
Stop 

Paila:
Terror% = 0 
If ERR = "EF" Then Resume

Print "ERROR APPL "+ERR
Stop 
