Integer*1 Global Terror%
String    Global Proceso$, DM.ALMACEN$
String    Global Plu.Val$(1)
Integer*4 Global Nitm%, Xj%

%Include Basrout.j86

Sub CARGA.LISTA.PLUS
Integer*4 UE.PORCENTAJE, UE.LINEA
String XPLU$
Dim Plu.Val$(1000)
Nitm% = 0
Open "ADX_UDT1:SISMED.TXT" As 18 
If END # 18 THEN  CONT.PLU
While (1)
   READ # 18 ; XPLU$
   Nitm% = Nitm% + 1
   Plu.Val$(Nitm%) = XPLU$
Wend 
CONT.PLU:
  CLOSE 18
  Print "TOTAL CARGADOS "+STR$(NITM%)
End Sub 

On Error GoTo Paila
Call carga.lista.plus
For Xj% = 1 To Nitm%
    Proceso$ = Plu.Val$(Xj%)
    Call OSSHELL(Proceso$)
Next Xj%
Stop 

Paila:
Terror% = 0
Print "Error en aplicacion "+Err
Stop 