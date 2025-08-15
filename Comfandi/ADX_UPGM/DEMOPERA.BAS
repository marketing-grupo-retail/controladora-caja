!--- Calculo de ofertas

Integer*1 Global Terror%
String    Global Param2$

%INCLUDE EAMOPERA.J86																												! Variales maestro operadores

Sub MENSAJE
    Print Param2$
End Sub 


Sub Procesa.Operador
Integer*1 Xi%
String RECORD.LINE$, XDATA$
For Xi% = 1 To 9 
      OA.OPERATOR$ = Pack$(Right$("9999999999"+Str$(Xi%),10))
      Terror% = -1

!      READ FORM "C5, C67"; #44 KEY OA.OPERATOR$; \
!                XDATA$, RECORD.LINE$

             READ FORM "C5,C4,C1,2I2,16I1,I2,C20,C20";#44 KEY OA.OPERATOR$; \
             Xdata$, OA.PASSWORD$,OA.OPTIONSK$,OA.INDICAT0,          \
             OA.INDICAT1,OA.INDICAT2,OA.INDICAT3,OA.INDICAT4,OA.INDICAT5, \
             OA.INDICAT6,OA.INDICAT7,OA.INDICAT8,OA.INDICAT9,OA.INDICAT10,\
             OA.INDICAT11,OA.INDICAT12,OA.INDICAT13,OA.INDICAT14,         \
             OA.INDICAT15,OA.INDICAT16,OA.INDICAT17,                      \
             OA.RESERVED,OA.OPERNAME$,OA.USEREXIT$      !

      If Terror% = -1 Then Begin
         Print "Grupo Encontrador   "+Str$(Xi%)
      EndIf Else Begin
         Print "Grupo No Encontrado "+Str$(Xi%)
      EndIf
Next Xi%

End Sub 

On Error GoTo PAILA
Clears
OPEN "EAMOPERA" KEYED RECL 72 AS 44   ! Open auth. file
Call Procesa.Operador
Close 44
Stop 

PAILA:
 
 If ERR = "EF" Then Terror% = 0 : Resume
 
 PRINT "ERROR APPL "+ERR
 Stop
  
