!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMCRCLT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Descuentos Especiales Clientes   *
!**************************************************
!

%ENVIRON T

Integer   Global Sl.End
Integer*1 Global Gr.DctClte.Ok%,																					 \! Control proyecto
                 Gr.DctClte.Open%,                                         \!
                 Gr.DctClte.Trx%
Integer*4 Global Gr.DctClte.TotDsct%, Asc.Tmp.Apun%
String    Global Gr.DctClte.Grp$, Gr.DctClte.key$														!
String    Global Gr.DctClte.Name$, Gr.DctClte.Id$, Gr.DctClte.Lst$,        \!
                 Gr.DctClte.Dscto$,Gr.DctClte.Tipo$ 
Integer*2 Global Gr.DctClte.Ses%

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMITEMR.J86          																							! Variables maestro items
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler
%INCLUDE RECATSSU.011     		 																							! Rutinas Genericas

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub CAPTURA.CLIENTE.DSCTO
String Xdata$, Gr.Llave$, Gr.Dummy$, Gr.Name$, Gr.Activo$, Gr.TipoDscto$,  \!
       Gr.PtgDct$, Gr.Lista$																								!
     Xdata$ = Asic.Datos$("INGRESE NUMERO DE ID","DEL CLIENTE ...     ")    ! Presenta dato en pantalla
     If (Xdata$ = "") OR (Xdata$ = "E") Then Begin			     					      ! No se capturo dato
        Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")              !
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Init vectores de aplicacion
        TS.IO.MOTORKEY = 0 																									! Proceso cancelado
     EndIf																																	!
     Gr.Llave$ = Pack$(Right$("000000000000000000"+Xdata$,18))              ! Empaqueta nit
     TS.TS11WERR$ = ""																										  ! Ctrl Errores
     Read Form "C9 C30 2C1 C3 C2" ; #Gr.DctClte.Ses% KEY Gr.Llave$;        \! Lee Registro
       Gr.Dummy$, Gr.Name$, Gr.Activo$, Gr.TipoDscto$, Gr.PtgDct$,         \!
       Gr.Lista$
     If TS.TS11WERR$ <> "" Then Begin																				! Si Error lectura
        Call VISOR.AND.BORRAR("ERROR CLIENTE NO EXISTE")                    !
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Init vectores de aplicacion
        TS.IO.MOTORKEY = 0 																									! Proceso cancelado
        Exit Sub 
     EndIf
     Call VISOR.AND.BORRAR(Gr.Name$)                                        ! Nombre del cliente
     If Gr.Activo$ = "A" Then Call VISOR.AND.BORRAR("ESTADO: ACTIVO") Else \!
   	                          Call VISOR.AND.BORRAR("ESTADO: INACTIVO")	  	!
     If Gr.Activo$ = "I" Then Begin																					!
        Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Init vectores de aplicacion
        TS.IO.MOTORKEY = 0 																									! Proceso cancelado
        Exit Sub
     EndIf
     Gr.DctClte.Trx% = -1																										! Cliente activo
     Gr.DctClte.Name$  = Gr.Name$
     Gr.DctClte.Id$    = Xdata$
     Gr.DctClte.Lst$   = Gr.Lista$
     Gr.DctClte.Dscto$ = Gr.PtgDct$
     Gr.DctClte.Tipo$  = Gr.TipoDscto$	
End Sub 

Function CALCULO.COMPRA.LISTA
Integer*4 CALCULO.COMPRA.LISTA
Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J% , OVS% 												    !
Integer*1 Xsgn%
String    X.TMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, XInd3$           !

  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico indicat
        Xind3$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 3
        Xsgn% = 1 																													! Trx de compra
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado

     EndIf                                  																! ITEM ENTRY String
  NEXT X.J%   
  



     
End Function 


Sub APLICACION.DSCTO.CLIENTE																								! Aplicacion dscto cliente
Integer*4 Xdscto.Pago%
    If Gr.DctClte.Tipo$ = "T" Then Begin 																	  ! Tiene dscto total compra
   	   Xdscto.Pago%  =  (TS.TOTALS(0,0,0) * Val(Gr.DctClte.Dscto$)) / 100   ! Calcula dscto aplicar
       SL.END = SL.END + 1                                                  ! Aumenta apuntador de String 
	     SL.STR$(SL.END) = Pack$("03")+":"+Pack$(Gr.DctClte.Grp$) + ":" +	   \! Almacena String de descuento
			   	               Pack$("00")+ ":" + Pack$(Str$(Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
       TS.BALDUE(0)      = TS.BALDUE(0) - Xdscto.Pago%                      ! Actualiza variables de la 
       SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Xdscto.Pago%                    ! Aplicacion y de total de la 
       TS.GROSSNEG       = TS.GROSSNEG + Xdscto.Pago%                       ! Transaccion eliminando el descuento
       TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Xdscto.Pago%                  !
       TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Xdscto.Pago%                 !
       TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Xdscto.Pago%                  !
       Call Format.Amount((Xdscto.Pago% * -1))
       TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
       TS.TEMP2$ = "     Descuento Cliente    :$" + TS.TEMP1$               ! Arma mensaje a tiquete
       Call U.IMPRIME(TS.TEMP2$,4101H)                                      ! Imprime vlr dscto
       Call U.IMPRIME(TS.TEMP2$,2100H)                                      ! Imprime vlr dscto
       TS.TEMP2$ = Left$("Cliente :"+Gr.DctClte.Name$,38)
       Call U.IMPRIME(TS.TEMP2$,4101H)                                      ! Imprime vlr dscto
       Call U.IMPRIME(TS.TEMP2$,2100H)                                      ! Imprime vlr dscto
       Call Format.Amount((Xdscto.Pago% * -1))
       Call VISOR.AND.BORRAR("DESCUENTO APLICADO :$"+TS.TEMP1$)             ! Presenta informacion
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      ! Init vectores de aplicacion
       TS.IO.MOTORKEY = 0 																									! Proceso cancelado
       TS.GUIDANCE = 1020																										! Totalice la trx
    	 Gr.DctClte.Trx% = 2																									! Dscto Aplicado
    	 Gr.DctClte.TotDsct% = Xdscto.Pago%																		! Dscto Otorgado
       TS.TEMP1$  = Pack$("03")                   +                        \! Dscto aplicado    
                    ":" + Pack$(Gr.DctClte.Id$)   +                        \! Id del cliente
                    ":" + Gr.DctClte.Name$        +                        \! Nombre del cliente
                    ":" + Pack$(Gr.DctClte.Tipo$) +                        \! Tipo Descuento
                    ":" + Pack$(Gr.DctClte.Lst$)  +                        \! Numero de lista
                    ":" + Pack$(Gr.DctClte.Dscto$)                          ! Porcentaje de descuento
                    
       Call Grabacion.Cadena.Usuario2("20141107",TS.TEMP1$)                 ! Almacena user data      
    EndIf																																		! Fin dscto total compra
    If Gr.DctClte.Tipo$ = "L" Then Begin 																	  ! Tiene dscto por una lista
       Xdscto.Pago% = CALCULO.COMPRA.LISTA                                  ! Verifica en la compra 
       If Xdscto.Pago% > 0 Then Begin																				! Si hay monto compra
   	      Xdscto.Pago%  =  (Xdscto.Pago% * Val(Gr.DctClte.Dscto$)) / 100    ! Calcula dscto aplicar               
          SL.END = SL.END + 1                                               ! Aumenta apuntador de String         
	        SL.STR$(SL.END) = Pack$("03")+":"+Pack$(Gr.DctClte.Grp$) + ":" + \! Almacena String de descuento       
			      	               Pack$("00")+ ":" + Pack$(Str$(Xdscto.Pago%))+":"+Pack$("00") + ":" !                    
          TS.BALDUE(0)      = TS.BALDUE(0) - Xdscto.Pago%                   ! Actualiza variables de la           
          SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Xdscto.Pago%                 ! Aplicacion y de total de la         
          TS.GROSSNEG       = TS.GROSSNEG + Xdscto.Pago%                    ! Transaccion eliminando el descuento 
          TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Xdscto.Pago%               !                                     
          TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Xdscto.Pago%              !                                     
          TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Xdscto.Pago%               !                                     
          Call Format.Amount((Xdscto.Pago% * -1))                           !                                         
          TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                     !                                         
          TS.TEMP2$ = "     Descuento Cliente    :$" + TS.TEMP1$            ! Arma mensaje a tiquete              
          Call U.IMPRIME(TS.TEMP2$,4101H)                                   ! Imprime vlr dscto                   
          Call U.IMPRIME(TS.TEMP2$,2100H)                                   ! Imprime vlr dscto                   
          TS.TEMP2$ = Left$("Cliente :"+Gr.DctClte.Name$,38)                                                         
          Call U.IMPRIME(TS.TEMP2$,4101H)                                   ! Imprime vlr dscto                   
          Call U.IMPRIME(TS.TEMP2$,2100H)                                   ! Imprime vlr dscto                   
          Call Format.Amount((Xdscto.Pago% * -1))                                                                    
          Call VISOR.AND.BORRAR("DESCUENTO APLICADO :$"+TS.TEMP1$)          ! Presenta informacion                
          Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  ! Init vectores de aplicacion        
          TS.IO.MOTORKEY = 0 																								! Proceso cancelado                    
          TS.GUIDANCE = 1020																								! Totalice la trx                    
    	    Gr.DctClte.Trx% = 2																								! Dscto Aplicado                       
    	    Gr.DctClte.TotDsct% = Xdscto.Pago%															  ! Dscto Otorgado                     
          TS.TEMP1$  = Pack$("03")                   +                     \! Dscto aplicado                      
                       ":" + Pack$(Gr.DctClte.Id$)   +                     \! Id del cliente                      
                       ":" + Gr.DctClte.Name$        +                     \! Nombre del cliente                  
                       ":" + Pack$(Gr.DctClte.Tipo$) +                     \! Tipo Descuento                      
                       ":" + Pack$(Gr.DctClte.Lst$)  +                     \! Numero de lista                     
                       ":" + Pack$(Gr.DctClte.Dscto$)                       ! Porcentaje de descuento             
                                                                                                                     
          Call Grabacion.Cadena.Usuario2("20141107",TS.TEMP1$)              ! Almacena user data                  
       EndIf																																! Fin aplicacion descuento
    EndIf																																		! Fin calculo dscto por lista
End Sub 																																		! Fin aplicacion dscto cliente

Sub ANULACION.DSCTO.CLIENTE																								  ! Aplicacion dscto cliente
Integer*4 Xdscto.Pago%
    If Gr.DctClte.TotDsct% <= 0 Then Begin 																	! Si no hay dscto aplicado
       Call VISOR.AND.BORRAR("NO HAY DESCUENTOS APLICADOS")									! Msg Alerta
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      ! Init vectores de aplicacion
       TS.IO.MOTORKEY = 0 																									! Proceso cancelado
       Exit Sub 
    EndIf

   	   Xdscto.Pago%  =  Gr.DctClte.TotDsct%                                 ! Vlr Dscto reversar
       SL.END = SL.END + 1                                                  ! Aumenta apuntador de String 
	     SL.STR$(SL.END) = Pack$("04")+":"+Pack$(Gr.DctClte.Grp$) + ":" +	   \! Almacena String de descuento
			   	               Pack$("00")+ ":" + Pack$(Str$(Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
       TS.BALDUE(0)      = TS.BALDUE(0) + Xdscto.Pago%                      ! Actualiza variables de la 
       SL.HD.GROSSNEG    = SL.HD.GROSSNEG - Xdscto.Pago%                    ! Aplicacion y de total de la 
       TS.GROSSNEG       = TS.GROSSNEG - Xdscto.Pago%                       ! Transaccion eliminando el descuento
       TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) - Xdscto.Pago%                  !
       TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - Xdscto.Pago%                 !
       TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Xdscto.Pago%                  !
       Call Format.Amount((Xdscto.Pago% ))
       TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
       TS.TEMP2$ = "  AN Descuento Cliente    :$" + TS.TEMP1$               ! Arma mensaje a tiquete
       Call U.IMPRIME(TS.TEMP2$,4101H)                                      ! Imprime vlr dscto
       Call U.IMPRIME(TS.TEMP2$,2100H)                                      ! Imprime vlr dscto
       TS.TEMP2$ = Left$("Cliente :"+Gr.DctClte.Name$,38)
       Call U.IMPRIME(TS.TEMP2$,4101H)                                      ! Imprime vlr dscto
       Call U.IMPRIME(TS.TEMP2$,2100H)                                      ! Imprime vlr dscto
       Call Format.Amount((Xdscto.Pago% ))
       Call VISOR.AND.BORRAR("DESCUENTO ANULADO  :$"+TS.TEMP1$)             ! Presenta informacion
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												      ! Init vectores de aplicacion
       TS.IO.MOTORKEY = 0 																									! Proceso cancelado
       TS.GUIDANCE = 1020																										! Totalice la trx
    	 Gr.DctClte.Trx% = -1																								  ! Dscto Aplicado
    	 Gr.DctClte.TotDsct% = 0																							! Init dscto 
       TS.TEMP1$  = Pack$("04")                   +                        \! Dscto anulado
                    ":" + Pack$(Gr.DctClte.Id$)   +                        \! Id del cliente
                    ":" + Gr.DctClte.Name$        +                        \! Nombre del cliente
                    ":" + Pack$(Gr.DctClte.Tipo$) +                        \! Tipo Descuento
                    ":" + Pack$(Gr.DctClte.Lst$)  +                        \! Numero de lista
                    ":" + Pack$(Gr.DctClte.Dscto$)                          ! Porcentaje de descuento
                    
       Call Grabacion.Cadena.Usuario2("20141107",TS.TEMP1$)                 ! Almacena user data      
    	 

End Sub 																																		! Fin aplicacion dscto cliente

Sub DSCTO.CLTES(XOPT%) Public																								! Descuentos clientes
Integer*4 Xopt%

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.DctClte.Ok%  = 0                                                       ! Proyecto Apagado
  TS.TS11WERR$ = ""																													! Ctrl Errores                     
  Open "R::$SCNTRL" AS 38               			  		  											! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then BEGIN                                          !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL PROYECTOS")			!
  	 Call U.Imprime("ERROR APERTURA ARCHIVO CONTROL PROYECTOS",2100H)       ! Rastro en EJ
  	 Exit Sub 																															!
  ENDIF 																																		!
  IF END #38 THEN UE.FIN.GRDSCT         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #38; TS.TEMP1$			       																			! Lectura registro                 
        IF TS.TEMP1$ = "[DSCTOS CLIENTES]" Then Begin		  	   					    ! Dsctos clientes
         Read #38; TS.TEMP1$																								! Lectura registro                 
         Gr.DctClte.Ok%   = Val(Mid$(TS.TEMP1$,30,2))     		    					! Proyecto Activo 0. No -1 Si
         Read #38; TS.TEMP1$     																						! Lectura registro                 
         Gr.DctClte.Key$  = Mid$(TS.TEMP1$,30,03)        		                ! Tecla motora
         Read #38; TS.TEMP1$     																						! Lectura registro                 
         Gr.DctClte.Grp$  = Mid$(TS.TEMP1$,30,02)        		                ! Grupo Dscto
         Read #38; TS.TEMP1$     																						! Lectura registro                 
         Gr.DctClte.Ses%  = Val(Mid$(TS.TEMP1$,30,02))   		                ! Numero de sesion 
         
         GoTo UE.FIN.GRDSCT 																								! Sale del ciclo de carga          
       Endif                                                                !
   Wend                                                                     !
   UE.FIN.GRDSCT:                                                           !
     Close 38																																! Cierra archivo
  If Gr.DctClte.Ok% = -1 Then \
   If Gr.DctClte.Open% = 0 Then Begin																				! Archivo no abierto
      TS.TS11WERR$ = ""																										  ! Ctrl Errores
      Open "R::$RCLTCRD" KEYED RECL 46 AS Gr.DctClte.Ses%                   !
      If TS.TS11WERR$ <> "" Then BEGIN                                      !
  	     Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CLIENTES DSCTOS")  	! Msg Alerta
  	     Call U.Imprime("ERROR APERTURA ARCHIVO CLIENTES DSCTOS",2100H)     ! Rastro en EJ
  	     Gr.DctClte.Ok% = 0																									! Apaga proyecto
      ENDIF Else Begin																											!
      	 Gr.DctClte.Open% = -1																							! Archivo ya abierto
      EndIf
   EndIf
   If Gr.DctClte.Ok% = -1 Then                                             \! Proyecto Activo
      Call U.IMPRIME("MODULO DSCTOS CLIENTE  ON 07/Nov/2014",6100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DSCTOS CLIENTE OFF 07/Nov/2014",6100H)         ! Msg Proyecto Cargado

EndIf																																				! Fin carga de parametros

If Gr.DctClte.Ok% = 00 Then                                                \! Proyecto Inactivo
   Exit Sub 																																! Sale del proceso
   
!--- EAMTSU02.J86
If Xopt% = 2 Or Xopt% = 56 Then Begin                                       ! Inicio de transaccion
	 Gr.DctClte.Trx% = 00																										  ! Cliente activo
	 Gr.DctClte.TotDsct% = 0
   Gr.DctClte.Name$  = ""          
   Gr.DctClte.Id$    = ""      
   Gr.DctClte.Lst$   = ""          
   Gr.DctClte.Dscto$ = ""           
   Gr.DctClte.Tipo$  = ""                
EndIf																																				! Fin inicio trx EAMTSU02	

!--- EAMTSU08.J86
If Xopt% = 08 Then Begin                                                    ! Control de items
	 If Gr.DctClte.Trx% = 2 Then Begin																				! Si dscto ya aplicado
	 	  Call VISOR.AND.BORRAR("DESCUENTOS YA APLICADOS A CLIENTE")            ! MSG DE ALERTA
	 	  IR.INDICAT0 = IR.INDICAT0 OR 04H					                            ! No permite venta
	 EndIf
EndIf																																				! Fin control items EAMTSU08


!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencia de tecleo
  If (TS.IO.KEYS(1) = 70)	And (TS.IO.MOTORKEY = Val(Gr.DctClte.Key$)) Then Begin ! Tecla Cliente Dscto y Anulacion
  	 Call ANULACION.DSCTO.CLIENTE
  EndIf																																			! Fin Anulación descuento
  
	If TS.IO.MOTORKEY = Val(Gr.DctClte.Key$) Then Begin										    ! Tecla Cliente Dscto 
		 Call CAPTURA.CLIENTE.DSCTO																							! Captura cliente dscto
  EndIf																																			! Fin captura de cliente
 	If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                   ! Si forma de pago 	
 	 If Gr.DctClte.Trx% = -1 Then Begin 																			! Cliente Activo para dsctos
 	 	  Call APLICACION.DSCTO.CLIENTE																					! Aplica dscto cliente
   EndIf																																		! Fin cliente activo para dsctos
  EndIf																																			! Fin forma de pago
EndIf																																				! Fin inicio trx EAMTSU14

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                    ! Impresion en CR
	
EndIf																																				! Fin inicio trx EAMTSU20

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                    ! Proceso recuperacion
	
EndIf																																				! Fin recuperacion EAMTSU53



!--- EAMTSU60.J86
If Xopt% = 60 Then Begin                                                    ! Impresion en CR
	
EndIf																																				! Fin inicio trx EAMTSU60


End Sub 																																		! Fin descuentos clientes