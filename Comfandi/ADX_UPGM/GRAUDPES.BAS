!--- Programa calculo pago subsidio
!--- en base a rastreo de auditoria
!--- desarrollado por Grupo Retail - OVS
!--- Octubre 25 de 2012

String    Global Linea$, File$
Integer*4 Global Tsc%, Tcc%, Tmov%, Total%, Nreg%, Nsc%, Ncc%
Integer*1 Global Terror%

On Error GoTo ErrAppl
CLEARS
Input "Archivo Auditoria a Analizar :";File$
Terror% = -1
Open File$ As 1 nodel 
If Terror% <> -1 Then Begin
	 Print "Archivo de auditoria "+File$+" No Existe"
	 Stop 
EndIf 
IF END #1 THEN REPORTE.FINAL
Nreg% = 0
Nsc% = 0 : Ncc% = 0
While (1)
  Nreg% = Nreg% + 1
  LOCATE 5,1 : Print "Registro ",Nreg%
  Read #1; LINE Linea$
  If Mid$(Linea$,16,8) = "RTA:1011" Then Begin      ! Pago Sin compra
    If Len(Linea$) > 73 Then begin 
  	   Tmov% = Val( Mid$(Linea$,73,12) )
  	   Tsc% = Tsc% + Tmov%
  	   Nsc% = Nsc% + 1
  	EndIf
  EndIf
  If Mid$(Linea$,16,8) = "RTA:1012" Then Begin      ! Pago Sin compra
  	If Len(Linea$) > 73 Then begin 
  	   Tmov% = Val( Mid$(Linea$,73,12) )
  	   Tcc% = Tcc% + Tmov%
  	   Ncc% = Ncc% + 1
  	EndIf
  EndIf
Wend 
REPORTE.FINAL:
  Print "Total pago 11 sin compra :",Tsc%,Nsc%
  Print "Total pago 12 con compra :",Tcc%,Ncc%
  Print " "
  Print "Total Movimiento         :",(Tsc%+Tcc%), (Nsc%+Ncc%)
  Stop 

ErrAppl:
Terror% = 0
If ERR = "OE" Then Resume 
If ERR = "EF" Then Resume 
Print "ERROR APLICACION "+ERR+" SESION:"+STR$(ERRF%)
Stop 