!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : EPAYEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 25 de 2.005                *
!Observaciones : Pago Electronico                 *
!**************************************************
! Modificaciones
! Mod22Sep2005
! Se define proceDimiento de rastreo para las terminales
! para identificar que mensajeria esta trabajando 
! la terminal, desarrollado por Oscar Valencia
!---------------------------------------------------
! Mod27Sep2005
! Se ajusta en el proceso de captura de tarjetas, los 
! tipos de datos de los apuntadores de vectores, 
! se pasan de I*1 a I*4, ajustado por Oscar Valencia
!---------------------------------------------------
! Mod28Sep2005
! Se ajusta el modulo para que en el proceso de intercambio
! de medios de pago, si se presenta un Error en la operacion
! se ejecute una anulacion total de la operacion.
! Ajustado por Oscar Valencia
!---------------------------------------------------
! Mod30Sep2005
! Se ajusta modulo para control en la operacion de venta
! cuando en la parametrizacion de la forma de pago en las
! terminal options esta incorrecto el monto a otorgar
! como cambio, si lo cancelan que se genere un reverso
! en forma automática.
!------------------------------------------------------
! Mod31Oct2005
! Se ajusta mòdulo para realizar reverso de PES cuando 
! se paga y solicita autorizacion de supervisor, si esta
! no se ingresa correctamente, generar un reverso.
! desarrollado por Oscar Valencia
!------------------------------------------------------
! Mod04Sep2006
! Se ajusta el modulo para que al final de la transaccion
! confirme si el pago se efectuo o no y adicionalmente 
! reportar la fecha contable para los procesos de conciliacion
! desarrollado por Oscar Valencia - Alfonso Cadena (WAS)
!----------------------------------------------------------
! Mod 8May2007
! Lista de errores que no generan reverso.
! 07 Documento no existe
! 02 Subsidio no disponible
! 22 Parametros del Negocio
! 06 Subsidio Maximo al permitido
! 01 Credencial no existe
! 03 Credencial cancelada
! 04 Credencial invalida
! 05 Credencial persona a cargo
! 01,02,03,04,05,06,07,22
!---------------------------------------------------------
! Mod 28May2007
! Si en modalidad de intercambio de medios de pago
! Presiona tecla PES, cancela el proceso
! Ajuste realizado el 28 mayo 2007 OVS 
!---------------------------------------------------------
! Mod 06Ene2009
! Se controla que el ingreso del dato de la credencial
! de subsidio solo se permita ingresada por el escanner
! y no por otro medio. Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 24Nov2010
! Se retira el control de ingreso de credencial de manera temporal
! solicitado por Comfandi.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 22Jun2011
! Se Activa el control de ingreso de credencial nuevamente 
! solicitado por Comfandi.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 23Mzo2012
! Se controla la secuencia de pago de PES para evitar la 
! segunda secuencia de registro y se controla la grabacion
! del UD 01082005 para evitar grabacion en la anulacion
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------------

%ENVIRON T

Integer   Global SL.END 
Integer*1 Global Asic.Llave%, Gr.Epay.Ing%
String    Global Asc.Tmp.Bono$, Asc.Tmp.Valor$, Asc.Pay.Errores$(1), tmpmsg$
String    Global Asc.Mod.DateTime$, HORA.MUNDIAL$
Integer*4 Global Asc.Tmp.Apun%,Asc.Tmp.V%,Asc.Tmp.I%												! Apuntador lectura strings
Integer*2 Global TS11.OVRFLAG             																	! indicates what overriding is
String    Global XL.JAVA$(2), XL.PES$(2)
Integer*1 Global XL.PES%, Asc.Pay.IMPMSG%, Asic.Device%
String    Global Asc.Pef.Contable$      ! Fecha contable de movimiento calculada en modulo Fonede

%Include EAMTSWKG.J86          ! working storage
%Include EAMTERMS.J86          ! working storage
%Include EAMTRANS.J86          !
%Include EAMTOPTS.J86	         !
%Include EPAYTSVA.011          ! Definicion de variables
%Include EAMP4VAJ.J86          ! Variables rutinas 4610
%Include EAMCUSTA.J86	         !
%Include EAMSOPTS.J86	         !
%Include EAMTSXHC.J86          !
%Include EAMASMRT.J86          !

Sub TSHIECET External 
End Sub 

Sub TSCSEC10 External                 ! Termination procedure
End Sub

Sub INIT.TRX External                 ! Inicializacion de una transaccion
End Sub                        

Sub TSIEEC01 External     						! Item Sales
End Sub

Sub TSETEC03 EXTERNAL                 ! CLEAR CHECKOUT VBLS
End Sub                               !

  SUB TSCSECRK EXTERNAL   ! read keyboard
  END SUB

%Include EPAYMSSU.011                 ! Subrutinas 

Function FORMAT.AMOUNT(AMT1) External
	INTEGER*1 FORMAT.AMOUNT
	INTEGER*4 AMT1
End Function

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) External
 Integer*1 ADXCOPYF
 Integer*4 RETC
 Integer*2 OPT0,OPT1
 String INFILE, OUTFILE
End Sub


! Parametros de la funcion
!U.X1$ --> codigo de la aplicacion
!U.X2$ --> codigo de la funcion
!U.X3$ --> trama del mensaje
!U.X4$ --> Estado del requerimiento
!U.X5$ --> Codigo de la cadena
!U.X6$ --> Factura de venta
!U.X7$ --> numero almacen
!U.X8$ --> numero de la terminal
!U.X9$ --> cajero
!U.XA$ --> numero de la trx
!U.XB$ --> Fecha y hora de la trx
         
Function Armar.Trama.Msg2(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$,U.X7$,U.X8$,U.X9$, U.XA$, U.XB$) Public      ! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg2, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$, U.X7$,U.X8$,U.X9$, U.XA$, U.XB$, UX.HORA$

U.X1$ = Right$("00"+U.X1$,2)        							                    ! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							                    ! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							                    ! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							                    ! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							                    ! Factura de venta
UX.HORA$ = Right$("000000000000"+U.XB$,12)                            ! Ajuste fecha y hora
U.Tmp1$ = U.X5$ +                                      				       \! Codigo de la cadena
          Right$("0000"+U.X7$,4)                      +  	           \! Numero del almacen
          Right$("000000"+Str$(Val(U.X8$)),6)         + 			       \! Numero de terminal
          Right$("0000000000"+Str$(Val(U.X9$)),10)    +              \! Numero de operador
          U.X6$                                       +              \! Factura fiscal                     
          Right$("000000"+Str$(Val(U.XA$)),6)         + 			       \! Numero de transaccion
          UX.HORA$				                            +              \! Fecha y Hora de la operacion
          U.X4$ + 								                                   \! Estado del requerimiento
          U.X3$                                                       ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Armar.Trama.Msg2 = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					          ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Sub Busca.Pago.Registrado(X.Tmp$,X.Compra$,X.Msg$)													    ! Asigna bono
String X.Tmp$, X.Saldo$, X.Trama$, X.Tmp2$, X.Compra$, X.Msg$                   !
Integer*1 X.Ind%
Integer*4 X.I%, X.J%                                                            ! 
		X.Ind% = 0																															    ! Defualt no encontro
		X.Trama$ = Str$(Val(X.Tmp$))                                                !
		X.Tmp2$  = Str$(Val(X.Compra$))                                             !
		For X.I% = 1 to Asc.Pay.NroBon%																					    ! Busca credencial
			If (Asc.Pay.Vtabono$(X.I%,0) = X.Trama$) Then Begin                       ! si existe
				 X.J% = X.I%
				 X.Ind% = -1 : X.I% = Asc.Pay.NroBon% + 10
			EndIf 
		Next X.I%
		If X.Ind% = 0 Then Begin 																									  ! No Encontro bono
				Asc.Pay.NroBon%  = Asc.Pay.NroBon% + 1                              		! Nro bonos capturados    
				Asc.Pay.Vtabono$(Asc.Pay.NroBon%,0) = X.Trama$                          ! Numero de la credencial
				Asc.Pay.Vtabono$(Asc.Pay.NroBon%,1) = X.Compra$                         ! Valor de la trx
				Asc.Pay.Vtabono$(Asc.Pay.NroBon%,2) = Unpack$(TS.OPER$)                 ! Operador que realiza la trx
				Asc.Pay.Vtabono$(Asc.Pay.NroBon%,3) = X.Msg$                            ! Fecha y hora de la TRX
        Asc.Pay.Vtabono$(Asc.Pay.NroBon%,4) = Asc.Pay.Trama$                    ! Trama de la transaccion
        Asc.Pay.Vtabono$(Asc.Pay.NroBon%,5) = Asc.Pay.OptTrx$										! Tipo de operacion
		EndIf Else Begin                                                            ! Si lo encontro
				Asc.Pay.Vtabono$(X.J%,1) = X.Compra$                                    ! Valor de la trx
				Asc.Pay.Vtabono$(X.J%,2) = Unpack$(TS.OPER$)                            ! Operador que realiza la trx    
				Asc.Pay.Vtabono$(X.J%,3) = X.Msg$                                       ! Fecha y hora de la TRX
        Asc.Pay.Vtabono$(X.J%,4) = Asc.Pay.Trama$                               ! Trama de la transaccion
		EndIf 
End Sub 

Sub EPAY.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			If Asc.Pay.Auditoria% <> -1 Then Exit Sub            ! Si No aplica auditoria

			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:TX" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                                !
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro      
			Close 56
End Sub 


!-- Se ajusta el proceso para el reverso, pasando el numero de la 
!-- credencial como parametro. OVS 9 Mayo 2007
Sub Reverso.Trama.Error(X.Credencial$)
String X.Trama$, X.Lec$, X.Finr$, X.Data$, X.Credencial$
Integer*4 X.Len%
      
      X.Credencial$ = RIGHT$("                  "+X.Credencial$,18)             ! ajusta dato capturado
      X.Data$ = Right$("00"+Asc.Pay.CodRta$,2) + "."
      X.Finr$ = Chr$(13) + Chr$(10)
      Call EPAY.AUDITORIA("ERROR DE APPL MANAGER","CODIGO ERROR "+TS.TEMP1$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 
      If Match(X.Data$,"01.02.03.04.05.06.07.22.",1) > 0 Then Begin             ! Lista codigos de Error que no genera reverso
       	 Exit Sub 																															! No ejecuta proceso
      EndIf 
 			TS.TEMP1$ = Right$("0000"+TS.STORE$,4)                     +          \! Numero del almacen
				    Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 	      \! Numero de terminal
				    UnPack$(TS.OPER$)                              +          \! Codigo del cajero
				    Right$("000000"+Str$(SL.HD.TRANSNUM+1),6)      + 	      \! Numero de transaccion
				    HORA.MUNDIAL$                                  +          \! Fecha y Hora de la operacion                      
				    X.Credencial$                                              ! Numero de credencial
			TS.TEMP2$ = Armar.Trama.Msg(Asc.Pay.Appl$,"14",TS.TEMP1$,"00",+       \!
				    Asc.Pay.Cadena$,"123456")                     ! Armar trama MSG
 			X.Len% = Len(TS.TEMP2$)						  					  								          ! Toma longitud del registro
 			X.Lec$ = "C"+Str$(X.len%)+" C2"								  							            ! Arma estructura de grabacion																	
      TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)      ! Ejecuta Requerimiento
      TS.TEMP1$ = Valida.Rta(TS.TEMP1$)																	        ! Valida rta entregada 
			TS.ER.RETURN = -1 
      If TS.TEMP1$ = "00" Then \																								! Reverso generado			
			 Call EPAY.AUDITORIA(TS.TEMP2$,"REVERSO GENERADO "+TS.TEMP2$, DATE$ + ":"+TIME$, DATE$ + ":"+TIME$) \
			Else \
			 Call EPAY.AUDITORIA(TS.TEMP2$,"REVERSO NO GENERADO"+Asc.Pay.Rrn$ , DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
			 
End Sub 

Sub Reverso.Trama.Java(XL.PASO$)
String X.Trama$, X.Lec$, X.Finr$, XL.PASO$
Integer*4 X.Len%
      X.Finr$ = Chr$(13) + Chr$(10)
      Call EPAY.AUDITORIA("REVERSO MOVIMIENTO ","DE JAVA  "+TS.TEMP1$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 
			TS.TEMP1$ =    Right$("0000"+TS.STORE$,4)                     +          \! Numero del almacen
										 Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 	       \! Numero de terminal
										 Unpack$(TS.OPER$)                              +          \! Codigo del cajero
										 Right$("000000"+Str$(SL.HD.TRANSNUM),6)        + 	       \! Numero de transaccion
										 HORA.MUNDIAL$                                  +          \! Fecha y Hora de la operacion                      
										 TS.TEMP6$                                                  ! Numero de credenci
			TS.TEMP2$ = Armar.Trama.Msg(Asc.Pay.Appl$,XL.PASO$,TS.TEMP1$,"00",+      \!
																	Asc.Pay.Cadena$,"123456")                     ! Armar trama MSG
 			X.Len% = Len(TS.TEMP2$)						  					  								      ! Toma longitud del registro
 		  X.Lec$ = "C"+Str$(X.len%)+" C2"								  							        ! Arma estructura de grabacion																	
			TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)               ! Ejecuta Requerimiento
			TS.TEMP1$ = Valida.Rta(TS.TEMP1$)																	        ! Valida rta entregada 
			TS.ER.RETURN = -1 
			
			If TS.TEMP1$ <> "00" Then Begin 
			 Open "R::C:\JAVA\BIN\SEC.DAT" AS 56 Append
			 If TS.ER.RETURN = -1 Then Begin 
 				 Write form X.Lec$; #56 ; TS.TEMP2$, X.Finr$           							    ! Graba registro      
				 Close 56
				 Call EPAY.AUDITORIA(TS.TEMP2$,"ENVIADO AL STORE AND FORWARD", DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
			 EndIf Else Begin 
			   Call EPAY.AUDITORIA(TS.TEMP2$,"DATO NO INGRESADO AL FORWARD "+Asc.Pay.Rrn$ , DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
 			   Call U.Imprime("REVERSO NO GENERADO "+TS.TEMP6$,2100H)
		   EndIf 
		  EndIf 
		  
End Sub 

Function Anula.Credencial(x.bono$,X.Compra$,X.Cajero$)
String X.Bono$, X.Vlr$, X.Trama$, X.Tmp2$, X.Compra$, X.Cajero$, Anula.Credencial
String Host.Message$, X.Lec$, X.Finr$, X.ENVIA$, X.LLEGA$,  X.SALE$, X.RTA$
Integer*1 X.Ind%
Integer*4 X.Len%, X.I%, X.J%, XL.NRTRX%
String X.Valor$, X.DATE$

    If Asc.Pay.Impr% = 2 Then Begin 
      XL.NRTRX% = SL.HD.TRANSNUM
    EndIf Else Begin 
      XL.NRTRX% = SL.HD.TRANSNUM + 1
    EndIf 

		X.Finr$ = Chr$(13) + Chr$(10)
		Asc.Tmp.Apun% = 3
		X.Ind% = 0																															    ! Defualt no encontro
		X.Trama$ = Str$(Val(X.bono$))                                               !
		X.Tmp2$ = Str$(Val(X.Compra$))                                              !
		X.Tmp2$ = Right$("0000000000"+X.Tmp2$,10)                                   !
		For X.I% = 1 to Asc.Pay.NroBon%																					    ! Busca credencial
			If (Asc.Pay.Vtabono$(X.I%,0) = X.Trama$) Then Begin                       ! si existe
				 X.J% = X.I%
				 X.Valor$ = Asc.Pay.Vtabono$(X.J%,1)                                    ! Valor del movimiento
				 X.Ind% = -1 : X.I% = Asc.Pay.NroBon% + 10                              !
			EndIf                                                                     !
		Next X.I%                                                                   !
		If X.Ind% = 0 Then Begin 																							      ! No Encontro credencial
			 Call VISOR.AND.BORRAR("NUMERO CREDENCIAL A DEVOLVER NO EXISTE!!")
			 Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10): TS.IO.MOTORKEY = 73
			 Anula.Credencial = "0"
			 Exit Function 
		EndIf     

		If Val(X.Valor$) <= 0 Then Begin 
			 Call VISOR.AND.BORRAR("NUMERO CREDENCIAL   YA SE REVERSO ...!!!")
			 Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10): TS.IO.MOTORKEY = 73
			 Anula.Credencial = "0"
			 Exit Function     
		EndIf 
		
		If NOT(TS.TRAINING) Then Begin 
			Call Visores4690(1,"ANULANDO CREDENCIAL",X.Trama$,0,"L")! Msg de alerta

			TS.TEMP1$ = Asc.Pay.Vtabono$(X.J%,3)
			
			TS.TEMP2$ = Armar.Trama.Msg(Asc.Pay.Appl$,"13",TS.TEMP1$,"00",Asc.Pay.Cadena$,"123456")                 ! Armar trama MSG
			X.DATE$   = Mid$(TS.TEMP2$,84,12)    ! Fecha y hora de la operacion original 
			X.ENVIA$  = TS.TEMP2$
			X.SALE$   = DATE$ +":"+ Time$

			TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)               ! Ejecuta Requerimiento
			X.RTA$    = DATE$ +":"+ Time$ 
			X.LLEGA$  = TS.TEMP2$
			Call EPAY.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)
			If LEN(TS.TEMP2$) >= 53 Then Host.Message$=MID$(TS.TEMP2$,14,40) Else Host.Message$="PROCESO NO FUE SATISFACTORIO"
			TS.TEMP1$ = Valida.Rta(TS.TEMP2$)																						       ! Valida rta entregada 

       TS.TEMP1$ = "00"
       
!       HORA.MUNDIAL$ = DATE$ + TIME$      

			If TS.TEMP1$ <> "00" Then Begin								   											       ! Si presenta un error, verif. local
          Call EPAY.AUDITORIA("ERROR DE APPL MANAGER","CODIGO ERROR ",DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)			
 					Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                       ! 
 					TS.IO.MOTORKEY = 73                                                             ! cancela proceso
					Anula.Credencial = "0"																								         !
					Exit Function 			

			EndIf 

			TS.TEMP1$ = MID$(TS.TEMP2$,12,2)                                               ! Toma rta de la BD
		  If TS.TEMP1$ <> "00" Then Begin								   														   ! Si presenta un error, verif. local
 			 If Host.Message$ = "" Then \
						TS.TEMP1$ = Asc.Pay.Errores$(Val(TS.TEMP1$))  \                          ! Lista de errores
				Else \
						TS.TEMP1$ = Host.Message$
					Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                   ! 
					TS.IO.MOTORKEY = 0                                                         ! cancela proceso
					Anula.Credencial = "0"
					Exit Function 																													   !
			 EndIf Else Begin					                                                     ! TRX AUTORIZADA      
			 TS.TEMP1$ = STR$(Val(X.BONO$))
			 X.Bono$ = STR$(Val(X.BONO$))
			 X.Bono$ = RIGHT$("                  "+X.Bono$,18)                             ! ajusta dato capturado
			 Call U.Imprime("CREDENCIAL NRO:"+TS.TEMP1$,6100H)                             ! Imprime nro tarjeta en tiquete              
			 TS.TEMP1$ =    Right$("0000"+TS.STORE$,4)                     +  	          \! Numero del almacen
											Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 		          \! Numero de terminal
											X.Cajero$                                      +              \! Codigo del cajero
											Right$("000000"+Str$(XL.NRTRX%),6)             + 			        \! Numero de transaccion
											HORA.MUNDIAL$				                           +              \! Fecha y Hora de la operacion                      
											X.Bono$                                        +              \! Numero de credencial
											":"+Pack$(X.Valor$)+":"+Pack$("13")                            ! Valor credencial

					Call Grabacion.Cadena.Usuario("02082005",TS.TEMP1$)                        ! Graba String usuario
					Anula.Credencial = Asc.Pay.Vtabono$(X.J%,1)															   ! Retorna numero credencial
					Asc.Pay.Vtabono$(X.J%,1) = "0"                                             ! Inicializa bono
          Asc.Pay.NroBon% = Asc.Pay.NroBon% - 1 					                           ! Retira numero de bonos calculados GR-OVS 22 Mzo 2012
			EndIf 
		EndIf 
End Function 

Sub Reversa.Anul.total
Integer*4 X.I%
	For X.I% = 1 TO Asc.Pay.NroBon%																					      ! Total de Credenciales
		If Val(Asc.Pay.Vtabono$(X.I%,1)) > 0 Then                \! Solo movimientos con saldo
				Call Anula.Credencial(Asc.Pay.Vtabono$(X.I%,0),      \! Numero credencial    
															Asc.Pay.Vtabono$(X.I%,1),      \! Valor  credencial
															Asc.Pay.Vtabono$(X.I%,2))       ! Cajero registro trx
	Next X.I%
End Sub 
!--- Fin reversion total compra

Sub Validacion.Compra.Pago.Electronico(X.Opt$)
String X.Opt$, X.Trama$
String Host.Message$, X.Lec$, X.Finr$, Rbuffer$, X.ENVIA$, X.LLEGA$, X.SALE$, X.RTA$, Xcred$
Integer*4 X.Len%, X.LONG%
		X.Finr$ = Chr$(13) + Chr$(10)
		Call TSHIECET                                                                ! Tono de alerta
		Call VISORES4690(1,"PROCESA CREDENCIAL:",TS.TEMP1$,0,"L")    						     !
		Xcred$ = TS.TEMP1$
		TS.TEMP1$ = RIGHT$("                  "+TS.TEMP1$,18)                        ! ajusta dato capturado
		TS.TEMP1$ = TS.TEMP1$ + Asc.Pef.Contable$ 																	 ! Add OVS 4 SEPT 2006
		Asc.Pay.Trama$ = TS.TEMP1$                                                   ! Add OVS 4 SEPT 2006 
		Asc.Pay.OptTrx$ = X.Opt$
		TS.TEMP2$ = Armar.Trama.Msg(Asc.Pay.Appl$,X.Opt$,TS.TEMP1$,"00",Asc.Pay.Cadena$,"123456")                 ! Armar trama MSG
		X.SALE$ = DATE$ + ":" +TIME$
		X.ENVIA$ = TS.TEMP2$   
		!TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)         ! Ejecuta Requerimiento
		
		X.RTA$ = DATE$ + ":"+TIME$
		X.LLEGA$ = TS.TEMP2$
		Call EPAY.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)
	  TS.TEMP1$ = Valida.Rta(TS.TEMP2$)																						 ! Valida rta entregada 
    Asc.Pay.Tmp$ = X.Opt$

!--- Proceso de simulacion
   TS.TEMP1$ = "00"
   HORA.MUNDIAL$ = DATE$ + TIME$
	 
	 If TS.TEMP1$ <> "00" Then Begin								   										        ! Si presenta un error, verif. local
	 	  Asc.Pay.CodRta$ = TS.TEMP1$
	 	  Call U.IMPRIME("FALLA COMUNICACIONES",2100H)
			Call U.IMPRIME("CREDENCIAL: "+Xcred$,2100H)
      Call EPAY.AUDITORIA("ERR APPL 1","COD ERR:"+TS.TEMP1$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 	 
			Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			TS.IO.MOTORKEY  = 73                                                       ! cancela proceso
			Asc.Pay.Cambio% = 4
			Exit Sub

	 EndIf 

	 TS.TEMP1$ = MID$(TS.TEMP2$,12,2)                                             ! Toma status de la Operacion

!--- Proceso de simulacion
   TS.TEMP1$ = "00"

	 If TS.TEMP1$ <> "00" Then Begin								   														! Si presenta un error, verif. local
	 	  Asc.Pay.CodRta$ = TS.TEMP1$
      Host.Message$ = MID$(TS.TEMP2$,14,40)                                         ! Toma mensaje del Error 
      Call EPAY.AUDITORIA("ERR APPL 2","COD ERR:"+TS.TEMP1$+" "+Host.Message$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 	 
			 If Host.Message$ = "" Then \
					 TS.TEMP1$ = Asc.Pay.Errores$(Val(TS.TEMP1$))     \                   ! Lista de errores
			 Else \
					 TS.TEMP1$ = Host.Message$
			 Call U.IMPRIME(TS.TEMP1$,2100H)
			 Call U.IMPRIME("CREDENCIAL: "+Xcred$,2100H)
			 Call VISOR.AND.BORRAR(Left$(TS.TEMP1$,20)+Right$(TS.TEMP1$,20))
			 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			 TS.IO.MOTORKEY = 0                                                        ! cancela proceso
			 Asc.Pay.Cambio% = 4
 		   Exit Sub     																														 ! 
		EndIf Else Begin					                                                   ! TRX AUTORIZADA
!       To.CDLIMIT2 = 99999999                                                    ! Control Maximo para autorizar

			 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			 TS.IO.MOTORKEY = 0                                                        !     
			 TS.TEMP1$ = STR$(Val(Right$(TS.TEMP2$,12)))

!--- Proceso de simulacion
        TS.TEMP1$ = "30000" 

       Asc.Pay.VlrBono$ = TS.TEMP1$
       
     If X.Opt$ = "11" Then Begin 
       
       TS.IO.DATA$(7) = TS.TEMP1$                                                ! Valor del subsidio
			 TS.IO.DATA$(9) = Asc.Pay.NumNota$                                         ! Graba numero credencial	
			 TS.IO.KEYS(9)  = 90
			 TS.IO.KEYS(3)  = 78                                                       ! Simula pago al tipo y variedad
			 TS.IO.DATA$(3) = Right$(Asc.Pay.tpvr$,1)                                  ! de pago parametrizada para 
			 TS.IO.MOTORKEY = VAL("9"+Left$(Asc.Pay.tpvr$,1))                          !
			 TS.IO.KEYS(7)  = VAL("9"+Left$(Asc.Pay.tpvr$,1))                          !
     EndIf
		EndIf 
End Sub 
!--- fin validacion compra

Function Carga.Mov.Pes																										! Analisis movimientos pes
Integer*1 Carga.Mov.Pes																										!
Integer*4 PRIC%, TOT4%, XL.I%, Asc.Ind.Tmp%, XL.J%												!
String XL.TMP$, XL.TRAMA$,XL.TARJETA$, XL.TIPO$ 													!
   
  For XL.J% = 1 TO Asc.Pay.Slend%                  												! Recorre todos los String
    H$ = READ.SL.STR$(XL.J%)                 															! Toma el String
    If LEN(H$) > 5 Then Begin              														    ! Si el String es OK
     If ASC(H$)  = 153 Then Begin            															! Si es un User Data 99
        Asc.Tmp.Apun% = 3																									! Asigna apuntador
        XL.TMP$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)	  							  			! Verifica el numero de proyecto
        If XL.TMP$ = "01082005" OR XL.TMP$ = "02082005" Then Begin        ! Si proyecto PES - PAGO
           XL.TMP$ = ASIC.GETUNPK4(H$,Asc.Tmp.Apun%)                      ! Toma el numero de trama
           XL.TIPO$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)                      ! Tipo de trx
           XL.TIPO$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)                      ! Tipo de trx
           TS.TEMP1$ = XL.TIPO$

           XL.TMP$ = Armar.Trama.Msg2("10", XL.TIPO$, Right$(XL.TMP$,18),"00","0001","123456",mid$(XL.TMP$,1,4), \
                     mid$(XL.TMP$,5,6),mid$(XL.TMP$,11,10), mid$(XL.TMP$,21,6), mid$(XL.TMP$,27,12))          

           TS.TEMP1$ = XL.TMP$
           XL.TRAMA$ = Left$(XL.TMP$,4)
           TS.TEMP1$ = XL.TRAMA$
        
      If XL.TRAMA$ = "1001" Then Begin        														! Si trama de compra
         XL.TARJETA$ = Right$(XL.TMP$,18)     														! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.I%          														! Arma vector para comparacion
           If XL.TARJETA$ = XL.PES$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin      															! Si no encontrado
            Asc.Tmp.I%  = Asc.Tmp.I% + 1
            XL.PES$(Asc.Tmp.I%,0) = XL.TMP$
            XL.PES$(Asc.Tmp.I%,1) = XL.TARJETA$
            XL.PES$(Asc.Tmp.I%,2) = "1"
            XL.PES$(Asc.Tmp.I%,3) = "04"
         EndIf Else Begin 																								! Si lo encontro
            XL.PES$(Asc.Ind.Tmp%,2) = Str$(Val(XL.PES$(Asc.Ind.Tmp%,2)) + 1)
         EndIf 
      EndIf 																!
      
      If XL.TRAMA$ = "1002" Then Begin      ! Si trama de Cambio por dinero
         XL.TARJETA$ = Right$(XL.TMP$,18)   ! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.I%        ! Arma vector
           If XL.TARJETA$ = XL.PES$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin    ! Si no encontrado
            Asc.Tmp.I%  = Asc.Tmp.I% + 1
            XL.PES$(Asc.Tmp.I%,0) = XL.TMP$
            XL.PES$(Asc.Tmp.I%,1) = XL.TARJETA$
            XL.PES$(Asc.Tmp.I%,2) = "1"
            XL.PES$(Asc.Tmp.I%,3) = "04"
         EndIf Else Begin                   ! Si encontrado
            XL.PES$(Asc.Ind.Tmp%,2) = STR$(VAL(XL.PES$(Asc.Ind.Tmp%,2)) + 1)
         EndIf 
      EndIf 																!

      If XL.TRAMA$ = "1003" Then Begin      ! Si trama de reverso de trx
         XL.TARJETA$ = Right$(XL.TMP$,18)   ! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.I%
           If XL.TARJETA$ = XL.PES$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin 
            Asc.Tmp.I%  = Asc.Tmp.I% + 1
            XL.PES$(Asc.Tmp.I%,0) = XL.TMP$
            XL.PES$(Asc.Tmp.I%,1) = XL.TARJETA$
            XL.PES$(Asc.Tmp.I%,2) = "-1"
            XL.PES$(Asc.Tmp.I%,3) = "05"
         EndIf Else Begin 
            XL.PES$(Asc.Ind.Tmp%,2) = STR$(VAL(XL.PES$(Asc.Ind.Tmp%,2)) - 1)
         EndIf 
      EndIf 																											
     EndIf    
     EndIf                                  															!
   EndIf 
  Next XL.J%                                 															!
  If Asc.Tmp.I% > 0 Then Carga.Mov.Pes = -1 Else \
                         Carga.Mov.Pes =  0
End Function 
!--- Fin analisis de pes 

Sub Validacion.Operacion
String XL.TMP$, XL.TRAMA$, XL.TARJETA$, XL.TMP2$
Integer*4 XL.I%, Asc.Ind.Tmp%, XL.T1%, XL.T2%, XL.K%
Integer*1 XL.MVJ%
  XL.MVJ%    = 0
  Asc.Tmp.V% = 0
  Asc.Tmp.I% = 0  
  Dim XL.JAVA$(50,4)
  Dim XL.PES$(50,4)
  TS.ER.RETURN = -1 																															! Ctrol errores
  XL.TMP$  = "R::ADX_UDT1:M"+RIGHT$("000"+Str$(SL.HD.TERMINAL),3) + RIGHT$("0000"+Str$(SL.HD.TRANSNUM),4)+".PES"
  XL.TMP2$ = "R::ADX_UDT1:M"+RIGHT$("000"+Str$(SL.HD.TERMINAL),3) + RIGHT$("0000"+Str$(SL.HD.TRANSNUM),4)+".PRO"
  TS.TEMP1$ = XL.TMP$
  Open XL.TMP$ AS 56 NODEL																												!
  If TS.ER.RETURN = -1 Then Begin  																								! Encontro movimiento

!    Call ADXCOPYF(XL.T1%,XL.TMP$,XL.TMP2$,0,2)
    
    If End #56 Then FINAL.VALIDA
    While(1)                                  ! Recorre el plano
      Read #56; XL.TMP$                       ! lectura registro
      XL.TRAMA$ = Left$(XL.TMP$,4)            ! Toma tipo de trama
      TS.TEMP3$ = XL.TRAMA$
      
      If XL.TRAMA$ = "1001" Then Begin        ! Si trama de compra
         XL.TARJETA$ = Right$(XL.TMP$,18)     ! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.V%          ! Arma vector para comparacion
           If XL.TARJETA$ = XL.JAVA$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin      ! Si no encontrado
            Asc.Tmp.V%  = Asc.Tmp.V% + 1
            XL.JAVA$(Asc.Tmp.V%,0) = XL.TMP$
            XL.JAVA$(Asc.Tmp.V%,1) = XL.TARJETA$
            XL.JAVA$(Asc.Tmp.V%,2) = "1"
            XL.JAVA$(Asc.Tmp.V%,3) = "04"
         ENDIf Else Begin 									! Si lo encontro
            XL.JAVA$(Asc.Ind.Tmp%,2) = STR$(VAL(XL.JAVA$(Asc.Ind.Tmp%,2)) + 1)
         ENDIf 
      ENDIf 																!
      
      If XL.TRAMA$ = "1002" Then Begin      ! Si trama de Cambio por dinero
         XL.TARJETA$ = Right$(XL.TMP$,18)   ! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.V%        ! Arma vector
           If XL.TARJETA$ = XL.JAVA$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin    ! Si no encontrado
            Asc.Tmp.V%  = Asc.Tmp.V% + 1
            XL.JAVA$(Asc.Tmp.V%,0) = XL.TMP$
            XL.JAVA$(Asc.Tmp.V%,1) = XL.TARJETA$
            XL.JAVA$(Asc.Tmp.V%,2) = "1"
            XL.JAVA$(Asc.Tmp.V%,3) = "04"
         ENDIf Else Begin                   ! Si encontrado
            XL.JAVA$(Asc.Ind.Tmp%,2) = STR$(VAL(XL.JAVA$(Asc.Ind.Tmp%,2)) + 1)
         ENDIf 
      ENDIf 																!

      If XL.TRAMA$ = "1003" Then Begin      ! Si trama de reverso de trx
         XL.TARJETA$ = Right$(XL.TMP$,18)   ! Extrae nro credencial
         Asc.Ind.Tmp% = 0
         For XL.I% = 1 To Asc.Tmp.V%
           If XL.TARJETA$ = XL.JAVA$(XL.I%,1) Then \
              Asc.Ind.Tmp% = XL.I%
         Next XL.I%
         If Asc.Ind.Tmp% = 0 Then Begin 
            Asc.Tmp.V%  = Asc.Tmp.V% + 1
            XL.JAVA$(Asc.Tmp.V%,0) = XL.TMP$
            XL.JAVA$(Asc.Tmp.V%,1) = XL.TARJETA$
            XL.JAVA$(Asc.Tmp.V%,2) = "-1"
            XL.JAVA$(Asc.Tmp.V%,3) = "05"
         ENDIf Else Begin 
            XL.JAVA$(Asc.Ind.Tmp%,2) = STR$(VAL(XL.JAVA$(Asc.Ind.Tmp%,2)) - 1)
         ENDIf 
      ENDIf 																											

    Wend 																			!
    FINAL.VALIDA:															!
      Delete 56
      XL.MVJ% = -1 
 EndIf       
 XL.T1% = 0 : XL.T2% = 0  
 XL.PES% = Carga.Mov.Pes                 																		! Analiza movimiento y mira si hay operaciones pes

 If XL.MVJ% = 0 AND XL.PES% = -1 Then Begin                                 ! Si movimiento pos y no java 
    Asc.Pay.Impr% = 2
    Call TSHIECET
    Call VISORES4690(1,"INFORMANDO FALLA","ADMIN. PES, ESPERE",0,"L")
    For XL.I% = 1 TO Asc.Tmp.I%
      TS.TEMP2$ = Armar.Trama.Msg("99","98","M001NRO CREDENCIAL"+XL.PES$(XL.I%,1),"00",Asc.Pay.Cadena$,"123456")                 ! Armar trama MSG
  	  TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)               ! Ejecuta Requerimiento    
    Next XL.I%
    Asc.Pay.Impr% = 0
    Exit Sub 
 EndIf 
  
 If XL.PES% = 0 And XL.MVJ% = -1 Then Begin      														! Sin movimiento en pos y si en java      
       Call TSHIECET
       For XL.I% = 1 TO Asc.Tmp.V%             															! Analiza Trx
        TS.TEMP3$ = XL.JAVA$(XL.I%,2)
        If Val(XL.JAVA$(XL.I%,2)) <> 0 Then Begin 													! Si con movimiento
          TS.TEMP1$ = XL.JAVA$(XL.I%,0)																			! Arma trama para el S&F
          Call Reverso.Trama.Java(XL.JAVA$(XL.I%,3))												! Reversa Movimiento
          TS.TEMP2$ = Armar.Trama.Msg("99","98","M002NRO CREDENCIAL"+XL.JAVA$(XL.I%,1),"00",Asc.Pay.Cadena$,"123456")                 ! Armar trama MSG
  	      TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)               ! Ejecuta Requerimiento        
        EndIf 
       Next XL.I%
 EndIf  


!--- Verifica integridad de los movimientos
 If XL.PES% = -1 And XL.MVJ% = -1 Then Begin 																! Si movimiento en POS y en java 
         For XL.K% = 1 TO Asc.Tmp.V%																				! Movimientos en Java
          For XL.I% = 1 TO Asc.Tmp.I%																				! Movimientos en Pos
           If (XL.JAVA$(XL.K%,1) = XL.PES$(XL.I%,1)) Then Begin             
               XL.PES$(XL.I%,4)  = "OK"
               XL.JAVA$(XL.I%,4) = "OK"
               XL.I% = 200
           EndIf  
          Next XL.I%
         Next XL.K%

!--- Reversa movimientos que esten en java y no en basic
         For XL.K% = 1 TO Asc.Tmp.V%																				! Movimientos en Java
          If XL.JAVA$(XL.K%,4) NE "OK" Then Begin 
           TS.TEMP1$ = XL.JAVA$(XL.K%,0)																		! Arma trama para el S&F
           Call VISORES4690(1,"REVERSANDO TARJETA OP ",XL.JAVA$(XL.K%,1),1500,"L")
           Call Reverso.Trama.Java(XL.JAVA$(XL.K%,3))												! Reversa Movimiento    
          EndIf 
         Next XL.K%
EndIf 
 
 
End Sub 
!--- Valida si proceso de java y pos OK

Sub Confirmacion.Movimientos.Pes
Integer*4 PRIC%, TOT4%, XL.I%, Asc.Ind.Tmp%, XL.J%, X.Len%			  				   ! Variables 
String XL.TMP$, XL.TRAMA$,XL.TARJETA$, XL.TIPO$, XL.VLR$									   ! temporales
String X.ENVIA$, X.SALE$, X.RTA$, X.LLEGA$, X.Lec$, X.Finr$                  !
Integer*4 Xl.Pos%																														 !
 X.Finr$ = Chr$(13) + Chr$(10)																		  			   ! Fin de registro 
 For XL.J% = 1 TO Asc.Pay.NroBon%														  			         ! Lista de credenciales a confirmar
  Xl.Pos% = XL.J%                                                            ! Posicion de vector de busqueda
  If Val(Asc.Pay.Vtabono$(XL.POS%,1)) <> 0 Then Begin                        ! Bono con movimiento
      TS.TEMP1$ = Asc.Pay.Vtabono$(XL.Pos%,3)                                ! Trama original
      XL.TIPO$ = "18" 																										   ! Tipo de operacion Appl (Confirmacion)
      TS.TEMP2$ = Armar.Trama.Msg(Asc.Pay.Appl$,Xl.Tipo$,TS.TEMP1$,"00",Asc.Pay.Cadena$,"123456") ! Armar trama MSG
      X.ENVIA$  = TS.TEMP2$                                                  ! datos de auditoria
      X.SALE$   = DATE$ +":"+ Time$                                          ! Fecha y hora envio trama
      TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)   ! Ejecuta Requerimiento
      X.RTA$    = DATE$ +":"+ Time$                                          ! Fecha y hora Respuesta
      X.LLEGA$  = TS.TEMP2$                                                  ! Trama respuesta
      Call EPAY.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)                ! Graba auditoria
      TS.TEMP1$ = Valida.Rta(TS.TEMP2$)		                                   ! Valida rta entrega           
      
      If TS.TEMP1$ NE "00" Then Begin                                        ! Error Mensaje Java
   			 X.Len% = Len(X.Envia$)						  					 								       ! Toma longitud del registro
    		 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							     ! Arma estructura de grabacion																	
			   Open "R::C:\JAVA\BIN\SEC.DAT" AS 56 Append
			   If TS.ER.RETURN = -1 Then Begin 
 				    Write form X.Lec$; #56 ; X.Envia$, X.Finr$           							    ! Graba registro      
				    Close 56
				    Call EPAY.AUDITORIA(X.Envia$ ,"ENVIADO AL STORE AND FORWARD", DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
			   EndIf Else Begin 
			      Call EPAY.AUDITORIA(X.Envia$ ,"DATO NO INGRESADO AL FORWARD " , DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
		     EndIf 
		  EndIf          
	EndIf 
 Next XL.J%
  
End Sub
!--- Fin analisis de pes 

!--------------
!--- Bloque Principal
!--------------

Sub EPAYMS02.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS02.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS02.011
EndIf 

End Sub 

Sub EPAYMS04.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

!If Asc.Pay.Line% Then Begin 
	 
!EndIf 

End Sub 

Sub EPAYMS05.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS05.011                                         **
!   **    Incluir en :  EAMTSU05.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 

	 %Include EPAYTS05.011

EndIf 

End Sub 



Sub EPAYMS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

	 %Include EPAYTS07.011

End Sub 

Sub EPAYMS13.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS13.011                                         **
!   **    Incluir en :  EAMTSU13.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

!If Asc.Pay.Line% Then Begin 
!   %Include EPAYTS13.011
!EndIf 

End Sub 

Sub EPAYMS14.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS14.011                                         **
!   **    Incluir en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 

	 %Include EPAYTS14.011
	 
EndIf 

End Sub 

Sub EPAYMS20.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS20.011                                         **
!   **    Incluir en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS20.011
EndIf 

End Sub 

Sub EPAYMS23.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS23.011                                         **
!   **    Incluir en :  EAMTSU23.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS23.011
EndIf 

End Sub 


Sub EPAYMS32.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS32.011                                         **
!   **    Incluir en :  EAMTSU32.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS32.011
EndIf 

End Sub 

Sub EPAYMS43.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS43.011                                         **
!   **    Incluir en :  EAMTSU43.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS43.011
EndIf 

End Sub 


Sub EPAYMS53.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS53.011                                         **
!   **    Incluir en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS53.011
EndIf 

End Sub 

Sub EPAYMS56.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS56.011                                         **
!   **    Incluir en :  EAMTSU56.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS56.011
EndIf 

End Sub 

Sub EPAYMS66.011(X.CallParm)  Public 
Integer*1 X.CallParm
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  EPAYMS66.011                                         **
!   **    Incluir en :  EAMTSU66.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Pay.Line% Then Begin 
	 %Include EPAYTS66.011
EndIf 

End Sub
