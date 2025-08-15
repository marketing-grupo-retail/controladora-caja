

Sub CALCULO.HORA
String Xh$, Xa$, X.Men$
Integer*4 Xa%, Xb%
Xh$ = Left$(TIME$,4)             ! Toma hora y minutos
Xa% = Val(Xh$)                   ! Toma Valor Numerico
Xa% = Round(Xa%,-2,1)            ! Calcula Proxima Hora de ejecucion
Xa$ = Right$("0000"+Str$(Xa%),4) ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)
Xh$ = Mid$(TIME$,3,2)            ! Toma Minutos
Xa% = val(Xh$)                   ! Convierte valor numerico
Xa% = 60 - Xa%                   ! Calcula resto de tiempo para proxima hora
Xb% = (Xa% * 60) * 1000          ! Calcula tiempo en tiempos de Relog 
X.Men$ = "Proxima hora :"+time$ + " "+str$(xb%)
Print X.Men$

!Wait ; Xb%

Print "Termino :"+time$
End Sub
!--- Fin calculo hora

On Error GoTo paila

Call calculo.hora
Stop 

paila:

print "un error "
stop 
