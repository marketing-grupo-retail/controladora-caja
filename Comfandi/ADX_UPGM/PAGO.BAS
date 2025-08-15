
String  Global Pago$, Item$, Ykey$, YDummy$, YFlag$, Yini$, Yfin$, Ydscto$,YPromo$                               !

On Error GoTo ErrAppl
Clears

Open "TF:PFPAGO" KEYED RECL 12 AS 44          			                ! Definicion Formas de pago
Open "TF:AFPAGO" KEYED RECL 07 AS 45          			                ! Definicion Articulos relacionados
Input "Pago a Consultar TV:";Pago$
Input "Item Ligado ";Item$

Ykey$ = Pack$(Right$("00"+Pago$,2))                                ! Arma llave de busqueda
READ FORM "C1 C1 2C3 2C2";#44 KEY Ykey$;                                   \! Lee registro
     YDummy$, YFlag$, Yini$, Yfin$, Ydscto$,YPromo$                               !

Print "PAGO    :"+Unpack$(Ykey$)
Print "Flag    :"+Unpack$(Yflag$)
Print "Inicial :"+Unpack$(Yini$)
Print "Final   :"+Unpack$(YFin$)
Print "Dscto   :"+Unpack$(Ydscto$)
Print "Promo   :"+Unpack$(Ypromo$)

Pago$ = Pack$( Right$("00"+Pago$,2) + Right$("000000000000"+Item$,12) )
Read FORM "C7";#45 KEY Pago$; Pago$
Print "Rta :"+unpack$(Pago$)
Close 45
Stop 

ErrAppl:

Print "ERROR APPL "+ERR+" Sesion: "+Str$(errf%)
Stop
