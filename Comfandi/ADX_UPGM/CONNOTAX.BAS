Integer*1 GLOBAL TERROR%
String XLEC$, XVAL$, RIFA$, CLTE$, Xcod$, Xname$, Xfec1$, Xfec2$,                          \!
    Xvlr$, Xbol$, Xcon$, Xmsg$, Xkey$, Gr.NoTax.Fecha$


Sub GR.VALIDA.FECHANOTAX																										! Valida fecha aplicacion evento
String Xkey$, Xdate$, Xd1$, Xd2$
  TERROR% = -1
  Open "TF:DNOTAX" KEYED RECL 22 AS 94 nowrite nodel 	  								! Control de fecha
  If TERROR% = 0 Then Begin
     Gr.NoTax.Fecha$ = "20200101"															  			      ! Asigna fecha vencida
     Exit Sub 																															! Sale del proceso
  EndIf Else Begin
     Xkey$ = Pack$("1999999999999999")                                      ! Registro control
     TERROR% = -1	  																								! Control de errores
  	 Read Form "C8 C7 C4 C3";#94 Key Xkey$; Xkey$, Xd1$, Xdate$, Xd2$  	    ! Busca fecha aplicacion
     If TERROR% = -1 Then Begin																				! Registro encontrado
        Gr.NoTax.Fecha$ = UnPack$(Xdate$)																		! Asigna fecha 
     EndIf Else Gr.NoTax.Fecha$ = "20200101"
  EndIf
  Close 94																																	! Cierre archivo
End Sub 



ON ERROR GOTO ERRAPPL
 CLEARS
 Print "Consulta Dia Sin IVA - Dia Sistema "+Date$
 Call GR.VALIDA.FECHANOTAX
 Print "FECHA APLICACION DIA SIN IVA :"+Gr.NoTax.Fecha$
 Stop 

ERRAPPL:
TERROR% = 0
If ERR = "EF" Then Resume
Print "ERROR APLICACION "+ERR

STOP