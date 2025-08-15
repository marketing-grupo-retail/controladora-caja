String PLU$, RESTO$, A$

On Error GoTo paila
Input "TERMINAL  ";PLU$
PLU$ = PACK$(Right$("0000"+PLU$,04))

Read Form "C2 C96";#1 KEY PLU$; PLU$, RESTO$ ! Lee archivo
Print "SI ESTA..."
tclose 98
wait ; 100

Open "TF:NMFECO" KEYED RECL 98 AS 1 Locked
while (1)
INPUT "Presione Enter ";A$
wend 
Stop 

Paila:
Print "presenta error la appl "+ERR
Stop 
