!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INTRXTD2.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Transacciones tarjetas *
!                debito y credito                 *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global Bines(2), TS.TEMP1$, GB$, GA$
Integer*4 Global Qty.Bines%, XI%, PJ%, PK%

Sub GRUNPK
  PK% = MATCH(":",GB$,PJ%) ! SEARCH FOR FIELD SEPERATOR
  GA$ = UNPACK$(MID$(GB$,PJ%,PK% - PJ%)) ! UNPACK FIELD
  PJ% = PK% + 1 ! POINT TO BeginNING OF NEXT FIELD
End Sub 

Sub GRUNPK4
  PK% = MATCH(":",GB$,PJ%) ! SEARCH FOR FIELD SEPERATOR
  GA$ = (MID$(GB$,PJ%,PK%-PJ%)) ! FIELD
  PJ% = PK% + 1 ! POINT TO BeginNING OF NEXT FIELD
End Sub 

Sub Parametros.Bines Public
String Xtmp$, Ue.Plan$
Ue.Plan$ = ""
Dim Bines(20,1)
Qty.Bines% = 0
  OPEN "ASCNTRL" AS 94							  	   																! Apertura archivo parametrizacion 
  IF END #94 THEN UE.FIN.CRDEMP         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[INTERFACE TEF]" Then Begin		              				! Parametros interface TEF
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Ue.Plan$   = Mid$(TS.TEMP1$,30,80)           				    					! Lista formas de pago validas interface
         While (1)																													! Toma lista bines excentos 
           Read #94; TS.TEMP1$     																					! Lectura registro                 
           Xtmp$ = Mid$(TS.TEMP1$,30,06)                  				          ! Bin excento interface
           If Xtmp$ = "999999" Then GoTo UE.FIN.CRDEMP  										! Sale del ciclo de carga          
           Qty.Bines% = Qty.Bines% + 1 																			! Aumenta apuntado
           Bines(Qty.Bines%,0) = Xtmp$																			! Asigna bin excento
           Bines(Qty.Bines%,1) = Mid$(TS.TEMP1$,36,02)   										! Tipo Variedad de pago a reportar en plantilla
         Wend 
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.CRDEMP:                                                           !
     Close 94																																! Cierra archivo
End Sub 

Function Valida.String.Tef ( TRX$ ) Public                                  ! Validacion Bin Trx Tef
Integer*4 XI%
String Trx$, Valida.String.Tef
  PJ% = 3																																		! Ajusta apuntador String
  Valida.String.Tef = ""																										! 
  GB$ = TRX$																																! Toma String UD analizar
  Call GRUNPK 	    																												! Toma proyecto
  If GA$ = "51" Then begin 																									! UD de Tef 
  	 Call GRUNPK4												   																	! String de TEF
     For XI% = 1 To Qty.Bines%																							! Analiza si trx con bin excento
       PJ% = MATCH(Bines(XI%,0),GA$,1)																			! Busca bin
       If PJ% > 0 Then Begin																								! Si es bin excento
       	  Valida.String.Tef = Bines(XI%,1)   																! Reporta tipo variedad pago a reportar
       	  XI% = XI% + 2																											! Sale del proceso
       EndIf																																!
     Next XI%																																!
  EndIf					
End Function 

