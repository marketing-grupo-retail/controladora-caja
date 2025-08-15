
String    Global Gr.Rappi.Compra$,																				 \!
                 Gr.Rappi.Imptos$,               												   \!
                 Gr.Rappi.Dsctos$,																				 \!
                 Gr.Rappi.CajOri$,																				 \! Nro caja
                 Gr.Rappi.TrxOri$																						! Nro Trx 
String GLOBAL PEDIDO$, Xkey$

On Error GoTo paila
CLEARS
Input "PEDIDO ";PEDIDO$
Input "COMPRA ";Gr.Rappi.Compra$
Input "IMPTOS ";Gr.Rappi.Imptos$
Input "DSCTOS ";Gr.Rappi.Dsctos$
Input "CAJA   ";Gr.Rappi.CajOri$
Input "TRANSA ";Gr.Rappi.TrxOri$
Open "TF:RPORDN" Keyed Recl 37 As 94																		! Archivo control pedido
  Xkey$ = Right$("                  "+PEDIDO$,18)                    ! Numero de pedido
  Gr.Rappi.Compra$ = Pack$(Right$("0000000000"+Gr.Rappi.Compra$,10))				!
  Gr.Rappi.Imptos$ = Pack$(Right$("0000000000"+Gr.Rappi.Imptos$,10))				!
  Gr.Rappi.Dsctos$ = Pack$(Right$("0000000000"+Gr.Rappi.Dsctos$,10))				!
  Gr.Rappi.CajOri$ = Pack$(Right$("0000"+Gr.Rappi.CajOri$,4))								! Nro caja
  Gr.Rappi.TrxOri$ = Pack$(Right$("0000"+Gr.Rappi.TrxOri$,4))								! Nro trx 
  Write Form "C18 3C5 2C2 ";#94;           								                 \! Graba numero de pedido
        Xkey$,                  																					 \! Numero de pedido
        Gr.Rappi.Compra$,																									 \! Valor compra
        Gr.Rappi.Imptos$,																									 \! Valor Imptos
        Gr.Rappi.Dsctos$, 																								 \! Valor dsctos
        Gr.Rappi.CajOri$,																									 \! Nro caja
        Gr.Rappi.TrxOri$																										! Nro Trx 
  Close 94																																	! Cierre archivo

paila:
Print "ERROR EN LA APLICACION "+ERR
Stop 
