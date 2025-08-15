!-- Programa demo para la generacion de un XML
!-- Grupo Retail - OVS   16 Agosto 2012

String Global Lista$, Final$

Function XML$( ACCION, X$, CODE$, VALOR$ )
integer*1 ACCION
real      kilo
String    XML$, X$, CODE$, VALOR$, aux$
kilo = 0.0
If ( ACCION = 0 ) Then begin

   aux$ = "<?xml version=" + chr$( 34 ) + "1.0" + chr$( 34 ) + " encoding=" + chr$( 34 ) + \
          "ISO-8859-1" + chr$( 34 ) + "?><gretail><O oper=" + chr$( 34 ) + "C" + chr$( 34 ) +  CODE$ +  ">"
          
endif Else If ( ACCION = 1 ) Then begin
   aux$ = X$ + "<" + Code$ + " "
endif Else If ( ACCION = 2 ) Then begin
   aux$ = X$ + " " + Code$ + "=" + chr$( 34 ) + Valor$ + chr$(34)
endif Else If ( ACCION = 3 ) Then begin
   aux$ = X$ + " " + Code$ + "=" + chr$( 34 ) + str$( Val( Valor$ ) ) + chr$(34)
endif Else If ( ACCION = 4 ) Then begin
   aux$ = X$ + ">"
endif Else If ( ACCION = 5 ) Then begin
   aux$ = X$ + "/>"
endif Else If ( ACCION = 6 ) Then begin
   aux$ = X$ + "<" + Code$ + ">"
endif Else If ( ACCION = 7 ) Then begin
   aux$ = X$ + "</" + Code$ + ">" 
endif Else If ( ACCION = 8 ) Then begin
   kilo = round( float( Val( Valor$ ) ) / 1000.0, 3, 0 )
   aux$ = X$ + " " + Code$ + "=" + chr$( 34 ) + str$( kilo ) + chr$( 34 )
endif Else If ( ACCION = 99 ) Then begin
   aux$ = X$ + "</O></gretail>"
endif

XML$ = aux$

End Function

on error goto paila

Lista$ = ""
Lista$ = XML$( 0, "", "L", "" )
Print "1. "+Lista$

Lista$ = XML$( 2, Lista$, "cliente", "79547699")
Print "2. "+Lista$


Lista$ = XML$( 3, Lista$, "sku", "000000000001")
Print "3. "+Lista$

Lista$ = XML$( 2, Lista$, "tvpago", "31")
Print "4. "+Lista$

Lista$ = XML$( 5, Lista$, "", "")
Print "5. "+Lista$

Stop 

paila:
Print "un pinche error" + err
Stop 
