Integer*1 Global Terror%
String    Global Proceso$, DM.ALMACEN$

%Include Basrout.j86

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     DM.ALMACEN$ = Right$(DM.ALMACEN$,3)
     Close 91
End Function
!--- Fin de la funcion de lectura


On Error GoTo Paila
Call Leer.Cabecera
!Proceso$ = "ESCALA "+DM.ALMACEN$
Proceso$ = "FACTURAS "+DM.ALMACEN$
Print proceso$
Call OSSHELL(Proceso$)
Stop 

Paila:
Terror% = 0
Print "Error en aplicacion "+Err
Stop 