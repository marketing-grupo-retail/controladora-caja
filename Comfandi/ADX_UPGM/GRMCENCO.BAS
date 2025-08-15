!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMOVRMS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Captura Remisiones Mercancia     *
!**************************************************
!Observaciones:


%ENVIRON T		                          																			! Ambiente de terminal

Integer*1 Global  Gr.Mrm.Ok%, Gr.Mrm.Cap%, Ue.Cnv.Fiscal%, UE.SGN%,          \!
                  Gr.Mrm.Int%, Gr.Mrm.Grab%                                   ! Control del proyecto
Integer*2 Global  Gr.Mrm.Key%, Gr.Mrm.Key2%  																	! Tecla motora
String    Global  Gr.Mrm.Remision$, Ue.Iva.Item$, Gr.Mrm.Pago$, Gr.Mrm.Nit$, \!
                  Gr.Mrm.Padre$                                               !
Integer*4 Global  Gr.Crd.Remision%, Asc.Tmp.Apun%, J,K   									! Numero de remision por terminal

%INCLUDE EAMTSWKG.J86          																								! working storage
%INCLUDE EAMTOPTS.J86          																								! Terminal Options
%INCLUDE EAMITEMR.J86          																								! Maestro de articulos 
%INCLUDE EAMTRANS.J86          																								! Variables de transancion
%INCLUDE EAMTSEVA.J86          																								! Variables Cliente Frecuente
%INCLUDE RECATSSU.011        																									! Rutinas Genericas Grupo Retail

Function FORMAT.AMOUNT(AMT1) External 							                        ! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

Sub REMISION.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:DM" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            Call VISORES4690(1,"ERROR EN CREACION","DE AUDITORIA ",1500,"L")
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56
End Sub 

Sub GETUNPKX
  K  = MATCH("|",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If (K-J) > 0 Then \
     A$ = MID$(B$,J,K-J) Else \! UNPACK FIELD
     A$ = ""
  J  = K + 1 ! POINT TO BeginNING OF NEXT FIELD
End Sub 


Sub Valida.Remision.Saleforce
String Xrtrama$, Xenvia$, Xsale$, Xrta$, Xllega$, NAME$, LNAME$, XTEMP4$

 TS.TEMP1I4 = 0
 TS.TEMP2$ = Armar.Trama.Msg("00","00",TS.TEMP1$,"00","0001","123456") 		  ! Armar trama MSG                	
 Xsale$    = DATE$ +":"+ Time$																						  ! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.grpretail.cencosud.peru.sigui.actions.Core","execute", TS.TEMP2$)  			! Ejecuta Requerimiento
 Xrta$     = DATE$ +":"+ Time$																							! Hora llegada requerimiento
 Xllega$   = XRtrama$																											  !
 Call REMISION.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria
 XTEMP4$ = Valida.Rta(XrTrama$)																			        ! Valida rta entregada
 If Xtemp4$ <> "00" Then Begin 
    Call VISOR.AND.BORRAR(Mid$(XrTrama$,14,40))									            ! Presenta Msg Error
 	  Exit Sub  
 EndIf
 Gr.Mrm.Remision$ = XrTrama$                                                ! Trama respuesta
 name$  = Mid$(Xrtrama$,64,10) + " "
 LNAME$ = Mid$(Xrtrama$,89,09)
 Call VISOR.AND.BORRAR("BIENVENIDO(A):      "+NAME$+LNAME$)

End Sub 

Sub Consulta.Ambiente.Texto  
String Xrtrama$, Xenvia$, Xsale$, Xrta$, Xllega$, NAME$, LNAME$, XTEMP4$


 TS.TEMP1I4 = 0
 TS.TEMP2$ = Armar.Trama.Msg("30","01",TS.TEMP1$,"00","0001","123456") 		  ! Armar trama MSG
 Xsale$    = DATE$ +":"+ Time$																							! Hora salida requerimiento
 XRtrama$  = Rutina.Java("com.grpretail.cencosud.peru.sigui.actions.Core","execute", TS.TEMP2$)  			! Ejecuta Requerimiento
 Xrta$     = DATE$ +":"+ Time$																							! Hora llegada requerimiento
 Xllega$   = XRtrama$																											  !
 Call REMISION.AUDITORIA(XENVIA$,XLLEGA$, XSALE$, XRTA$)                    ! Rastro auditoria

 XTEMP4$ = Valida.Rta(XrTrama$)																			        ! Valida rta entregada
 If Xtemp4$ <> "00" Then Begin 
    Call VISOR.AND.BORRAR(Mid$(XrTrama$,14,40))									            ! Presenta Msg Error
 	  Exit Sub 
 EndIf
 Gr.Mrm.Remision$ = XrTrama$                                                ! Trama respuesta
 name$  = Mid$(Xrtrama$,64,10) + " "
 LNAME$ = Mid$(Xrtrama$,89,09)
 Call VISOR.AND.BORRAR("BIENVENIDO(A):      "+NAME$+LNAME$)

End Sub 

Sub IMPRIME.DATA.CLIENTE
String Xrtrama$
 XrTrama$ = Gr.Mrm.Remision$                                                ! Trama respuesta
 Call U.IMPRIME("---    RESUMEN DATOS DE CLIENTE  ----",4101H)
 Call U.IMPRIME("---    RESUMEN DATOS DE CLIENTE  ----",6100H)
 A$ = Str$(Val(Mid$(Xrtrama$,202,18)))
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("DNI      :"+a$,6100H)
 A$ = Mid$(Xrtrama$,64,25) + " "
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("NOMBRE   :"+a$,6100H)
 A$ = Mid$(Xrtrama$,89,25)
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("APELLIDOS:"+a$,6100H)
 A$ = Mid$(Xrtrama$,116,25)
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("DIRECCION:"+a$,6100H)
 A$ = Mid$(Xrtrama$,141,10)
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("CELULAR  :"+a$,6100H)
 A$ = Mid$(Xrtrama$,151,25)
 A$ = LEFT$(A$ + STRING$(38," "),38)																		!
 Call U.IMPRIME("E-MAIL   :"+a$,6100H)
 Call U.IMPRIME(String$(38,"-"),6100H)

End Sub 

Sub GRMOVRMS(XOPT%) Public																							    	! Captura remisiones
Integer*4 Xopt%                                                               !

!--- EAMTSU07.J86
If Xopt% = 7 Then Begin                                                       ! Carga de parametros
	Call VISORES4690(1,"PARAMETROS DEMO","CARGADOS",500,"L")
  Gr.Mrm.Ok%  = 0                                                             ! Proyecto Apagado
  Gr.Mrm.Cap% = 0 
  TS.ER.RETURN = -1																													  ! Ctrl Errores                     
  OPEN "R::$SCNTRL" AS 94 NOWRITE NODEL					      ! Apertura archivo parametrizacion 
  If TS.ER.RETURN <> -1 Then BEGIN                                            !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA ARCHIVO CONTROL REMISION")
  	 Exit Sub 
  ENDIF 
  IF END #94 THEN UE.FIN.MOVRMS         																	    ! Si es EOF                        
  While (1)															  																    ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																				! Lectura registro                 
        IF TS.TEMP1$ = "[REMISION MERCANCIA]" Then Begin		   		   					! Remision Mercancia
         Read #94; TS.TEMP1$																									! Lectura registro                 
         Gr.Mrm.Ok%   = Val(Mid$(TS.TEMP1$,30,2))      				    					  ! Proyecto Activo 0. No -1 Si
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Mrm.Key%  = Val(Mid$(TS.TEMP1$,30,03))         				            ! Tecla motora secuencia
         Read #94; TS.TEMP1$     																						  ! Lectura registro                 
         Gr.Mrm.Key2%  = Val(Mid$(TS.TEMP1$,30,03))        				            ! Tecla motora secuencia
         GoTo UE.FIN.MOVRMS 																								  ! Sale del ciclo de carga          
       Endif                                                                  !
   Wend                                                                       !
   UE.FIN.MOVRMS:                                                             !
     Close 94																																  ! Cierra archivo
   If Gr.Mrm.Ok% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO PRUEBA CENCOSUD ON 23/Oct/2019",6100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PRUEBA CENCOSUDOFF 23/Oct/2019",6100H)           ! Msg Proyecto Cargado
EndIf 

If Gr.Mrm.Ok% <> -1 Then Exit Sub                                             ! Si proyecto apagado

!--- EAMTSU01.J86
If Xopt% = 1 Then Begin                                                       ! Al registro primer Item
	
EndIf																																					! 

!--- EAMTSU02.J86
If Xopt% = 2 Then Begin                                                       ! Inicializacion de trx
   Gr.Mrm.Cap% = 0 
   Gr.Mrm.Remision$ = ""
   Gr.Mrm.Int% = 0   
   Gr.Mrm.Grab% = 0
EndIf 

!--- EAMTSU14.J86
If Xopt% = 14 Then Begin                                                    ! Secuencias de tecleo
	
 If TS.IO.MOTORKEY = Gr.Mrm.Key% Then Begin 																! Captura tecla control remisiones
    Gr.Mrm.Cap% = 0
 		Gr.Mrm.Int% = 0 
    TS.TEMP1I4 = 0																													! Control proceso
    Call Valida.Remision.Saleforce																					! Validacion online
    If TS.TEMP1I4 = -1 Then BEGIN																		  		  ! Proceso satisfactorio
    	 Gr.Mrm.Cap% = -1
    ENDIF
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73
 EndIf 
 If TS.IO.MOTORKEY = Gr.Mrm.Key2% Then Begin 																! Consulta ambiente texto
    Gr.Mrm.Cap% = 0
 		Gr.Mrm.Int% = 0 
    TS.TEMP1I4 = 0																													! Control proceso
    TS.TEMP1$ = ASIC.DATOS$("INGRESE DATO","CLIENTE CONSULTAR")
    If TS.TEMP1$ = "E" Then BEGIN 																    			! Proceso cancelado   
       Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								  		!
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												  		!
       TS.IO.MOTORKEY = 73																						    	!
       Exit Sub 																											    	!
    EndIf 																														    	!
    TS.TEMP1$ = Right$("                 "+TS.TEMP1$,18)
    Call Consulta.Ambiente.Texto  																					! Validacion online
    If TS.TEMP1I4 = -1 Then BEGIN																		  		  ! Proceso satisfactorio
    	 Gr.Mrm.Cap% = -1
    ENDIF
    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
    TS.IO.MOTORKEY = 73
 EndIf																																			!
EndIf 

!--- EAMTSU20.J86
If Xopt% = 20 Then Begin                                                      ! Impresion CR

If (TS.PROCEDURE < 1 AND TS.INTRX) Then Begin       																		! En una trx
 If(TS.LINETYPE = 6 And TS.LINEDATA = 1) Then Begin                                     ! Personalized message
 	If Gr.Mrm.Cap% = -1 Then \
    Call IMPRIME.DATA.CLIENTE
 EndIf
EndIf

EndIf 

!--- EAMTSU53.J86
If Xopt% = 53 Then Begin                                                       	! Recuperacion de trx

EndIf 

!--- EAMTSU56.J86
If Xopt% = 56 Then Begin                                                       	! Suspender una trx
   Gr.Mrm.Cap% = 0 
   Gr.Mrm.Remision$ = ""
   Gr.Mrm.Int% = 0   
   Gr.Mrm.Grab% = 0
EndIf 

End Sub 
