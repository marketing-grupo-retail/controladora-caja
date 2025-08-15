!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : INTRXTDC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface Transacciones tarjetas *
!                debito y credito                 *
!**************************************************

%ENVIRON C						   												! Ambiente de controlador

String    Global Bines(2), TS.TEMP1$, GB$, GA$, desbon$(2)
Integer*4 Global Qty.Bines%, XI%, PJ%, PK%

!-- Variables para manejo impuesto ICUI
Integer*1 Global Gr.Iva.IndIcui%	 		  		! Activación proyecto ICUI
integer*4 Global Gr.Iva.TaxIcui%,          \! Valor impuesto Icui en trx
                 Gr.Iva.PtgIcui%            ! Porcentaje impuesto ICUI
String    Global Gr.Iva.flgIcui$,          \! Flag ICUI
                 Gr.Iva.DscIcui$,          \! Descriptor ICUI
                 Gr.Iva.DptoIcui$           ! Departamento reporte recaudo icui
Integer*4 Global UE.TABLAIVA(1)

%INCLUDE CFMEDPAG.VAR												!

Sub GRUNPK
  PK% = MATCH(":",GB$,PJ%) 									! SEARCH FOR FIELD SEPERATOR
  GA$ = UNPACK$(MID$(GB$,PJ%,PK% - PJ%)) 		! UNPACK FIELD
  PJ% = PK% + 1 														! POINT TO BeginNING OF NEXT FIELD
End Sub 

Sub GRUNPK4
  PK% = MATCH(":",GB$,PJ%) 									! SEARCH FOR FIELD SEPERATOR
  GA$ = (MID$(GB$,PJ%,PK%-PJ%)) 						! FIELD
  PJ% = PK% + 1 														! POINT TO BeginNING OF NEXT FIELD
End Sub 

Sub Parametros.Bines Public
String Xtmp$, Ue.Plan$
Ue.Plan$ = ""
Dim Bines(20,1)
Qty.Bines% = 0
  OPEN "ASCNTRL" AS 94							  	   																  ! Apertura archivo parametrizacion 
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
Dim desbon$(5,1)
desbon$(1,0) = "B041"
desbon$(1,1) = "BONOS BIG PASS     "
desbon$(2,0) = "B042"
desbon$(2,1) = "BONOS SODEXO PASS  "
desbon$(3,0) = "B043"
desbon$(3,1) = "BONOS DESCUENTAZOS "
desbon$(4,0) = "B044"
desbon$(4,1) = "BONOS FABRICANTES  "
desbon$(5,0) = "B045"
desbon$(5,1) = "BONOS PREP.COMFANDI"
Open "ASCNTRL" AS 94							  	   																  ! Apertura archivo parametrizacion 
  IF END #94 THEN UE.FIN.BOLSAS     																	      ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; Line TS.TEMP1$			       																! Lectura registro                 
        IF TS.TEMP1$ = "[BOLSAS IMPUESTO]" Then Begin		              			! Parametros interface TEF
         Read #94; Line TS.TEMP1$         																	! Lectura registro
         Gr.Iva.IndBol% = Val(Mid$(TS.TEMP1$,30,02))  										  ! Proyecto activo
         Read #94; Line TS.TEMP1$         																	! Lectura registro
         Gr.Iva.PluBol% = Val(Mid$(TS.TEMP1$,30,12))  										  ! Plu Bolsas
         Read #94; Line TS.TEMP1$         																	! Lectura registro
         Gr.Iva.VlrBol% = Val(Mid$(TS.TEMP1$,30,05))  										  ! Valor impuesto bolsa
         Read #94; Line TS.TEMP1$         																	! Lectura registro
         Gr.Iva.DptBol$ = Mid$(TS.TEMP1$,30,03)  										        ! Dpto venta bolsa
         
         GoTo UE.FIN.BOLSAS
        Endif                                                               !
   Wend                                                                     !
   UE.FIN.BOLSAS:                                                           !
     Close 94																																! Cierra archivo

Open "ASCNTRL" AS 94							  	   																  	! Apertura archivo parametrizacion 
  IF END #94 THEN UE.FIN.ICUI     																	      	! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
    Read #94; Line TS.TEMP1$			       																    ! Lectura registro                 
    If TS.TEMP1$ = "[IMPUESTO ICUI]" Then Begin					   	   					    ! Proyecto IMPUESTO ICUI
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.IndIcui% = Val(Mid$(TS.TEMP1$,30,02))  											  ! Proyecto activo
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.PtgIcui% = Val(Mid$(TS.TEMP1$,30,03))  											  ! porcentaje impuesto ICUI
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.flgIcui$ = Mid$(TS.TEMP1$,30,01)  											  		! flag indicador impuesto tiquete
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.DscIcui$ = Mid$(TS.TEMP1$,30,20)  											  		! descriptor impuesto
       Read #94; Line TS.TEMP1$         																		! Lectura registro
       Gr.Iva.DptoIcui$ = Mid$(TS.TEMP1$,30,03)  											  		! Depto recaudo ICUI
       GoTo UE.FIN.ICUI  																										! Sale proceso carga parametros
    EndIf
   Wend                                                                     !
   UE.FIN.ICUI:                                                             !
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

Sub Carga.Iva.Planilla Public 
Integer*4 UE.PORCENTAJE, UE.LINEA
Dim UE.TABLAIVA(10)
UE.LINEA = 0
Open "TABLAIVA" AS 94 
If END # 94 THEN  CONT.IVA
LEER.IVA:
   READ # 94 ;  UE.PORCENTAJE
   UE.LINEA = UE.LINEA + 1    
   UE.TABLAIVA(UE.LINEA) = UE.PORCENTAJE
   GOTO LEER.IVA 
CONT.IVA:
  CLOSE 94

End Sub 

Function Tarifa.Iva.Item(Xind1%) Public 
Integer*4 Xind1%, XI1%, XK%
Integer*1 Tarifa.Iva.Item, Xtar%
String    Xbin$

           Xtar% = 8																												! Tarifa iva default  
           XI1% = Xind1%                              	   	      					! Retorna indicat1    
           IF (XI1% AND 01H) Then XBIN$ = "1" Else XBIN$ = "0"		          !                     
           For XK% = 1 TO 7                                      	          !                     
               XI1% = SHIFT(XI1%,1)                   	                    ! Inicializa sig. BIT 
               IF (XI1% AND 01H) Then XBIN$ = "1" + XBIN$                  \!                      
                   Else XBIN$ = "0" + XBIN$                      	          !                       
           Next XK%                                                          !                      
           If LEFT$(XBIN$,4) = "1000" Then XTAR% = 1		                    ! Tax Plan A          
           If LEFT$(XBIN$,4) = "0100" Then XTAR% = 2		                    ! Tax Plan B          
           If LEFT$(XBIN$,4) = "0010" Then XTAR% = 3		                    ! Tax Plan C          
           If LEFT$(XBIN$,4) = "0001" Then XTAR% = 4		                    ! Tax Plan D          
           If LEFT$(XBIN$,4) = "1100" Then XTAR% = 5		                    ! Tax Plan AB         
           If LEFT$(XBIN$,4) = "0110" Then XTAR% = 6		                    ! Tax Plan BC         
           If LEFT$(XBIN$,4) = "0011" Then XTAR% = 7		                    ! Tax Plan CD         
           If LEFT$(XBIN$,4) = "0000" Then XTAR% = 8		                    ! Tax Plan All OFF    
           Tarifa.Iva.Item = Xtar%
End Function 
