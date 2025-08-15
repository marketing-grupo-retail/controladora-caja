!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : PEFMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Mayo  18 de 2.006                *
!Observaciones : Pago Electronico - FONEDE        *
!**************************************************
! Modificaciones
! Mod 06Ene2009
! Se controla que el ingreso del dato de la credencial
! de fonede solo se permita ingresada por el escanner
! y no por otro medio. Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 12Abr2010
! Se controla en el proceso de pago si esta cerca 
! una recogida total de caja, no permita el proceso
! de pago y solicite al cajero realizar la recogida
! de dinero.
! desarrollado por Grupo Retail - OVS
!---------------------------------------------------
! Mod 01Nov2012
! Se elimina la grabacion de procesos de Error al archivo
! sec.dat, proceso los maneja los ajustes de encolamiento
! de transacciones de Java
! desarrollado por Grupo Retail - OVS, CAC
!---------------------------------------------------------
! Mod 30Nov2012
! Se modifica modulo para manejo con nuevo sistema de 
! encolamiento de transaciones, para esta version, deben
! estar cambiadas las librerias FONEDE.JAR y GRQUEMGR.JAR
! y adicion de las lineas de encolamiento en el krn.ini
! Desarrollado por Grupo Retail - OVS, CACS 
!---------------------------------------------------------
 
%ENVIRON T

Integer   Global SL.END 
Integer*1 Global Asic.Llave% 
String    Global Asc.Tmp.Bono$, Asc.Tmp.Valor$, Asc.Pef.Errores$(1), tmpmsg$
String    Global Asc.Mod.DateTime$, HORA.MUNDIAL$
Integer*4 Global Asc.Tmp.Apun%,Asc.Tmp.V%,Asc.Tmp.I%												! Apuntador lectura strings
Integer*2 Global TS11.OVRFLAG             																	! indicates what overriding is
String    Global FN.JAVA$(2), FN.PES$(2)
Integer*1 Global FN.PES%, Asc.Pef.IMPMSG%, Asic.Device%
Integer*4 GLOBAL \
        TE.TR.AMT(1)                      ! ENTERED TENDER 1-CASH         INT 4

%Include EAMTSWKG.J86          ! working storage
%Include EAMTRANS.J86          !
%Include EAMTOPTS.J86	         !
%Include PEFTSUVA.011          ! Definicion de variables
%Include EAMCUSTA.J86	         !
%Include EAMSOPTS.J86	         !
%Include EAMTSXHC.J86          !
%Include EAMASMRT.J86          !

Sub TSHIECET External 
End Sub 

Sub TSDSEC01 External 
End Sub 

Sub TSPREC01 External
End Sub 

Sub TSCSEC08 External                 ! Display message and get clear
End Sub
 
Sub TSCSEC10 External                 ! Termination procedure
End Sub

Sub INIT.TRX External                 ! Inicializacion de una transaccion
End Sub                        

%Include PEFMTSSU.011                 ! Subrutinas 

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

!------
!-- Rutinas propietarias del desarrollo
!------

Function Retorna.Movimiento
Integer*1  Retorna.Movimiento
integer*4 X.I%
Retorna.Movimiento = 0
For X.I% = 1 to Asc.Pef.NroBon%																					    ! Busca credencial
  If (Val(Asc.Pef.Vtabono$(X.I%,1)) > 0) Then Begin                         ! Con saldo
     Retorna.Movimiento = -1                                                ! Encontro movimiento 
	EndIf 																																		!
Next X.I%																																		!

End Function 

Sub Busca.Pef.Registrado(X.Tmp$,X.Compra$,X.Msg$)													      ! Asigna bono
String X.Tmp$, X.Saldo$, X.Trama$, X.Tmp2$, X.Compra$, X.Msg$, X.Vlr$           ! Variables temporales de
Integer*1 X.Ind%																																! trabajo
Integer*4 X.I%, X.J%                                                            ! 
		X.Ind% = 0																															    ! Defualt no encontro
		X.Trama$ = Str$(Val(X.Tmp$))                                                !
		X.Tmp2$  = Str$(Val(X.Compra$))                                             !

!		For X.I% = 1 to Asc.Pef.NroBon%																					    ! Busca credencial
!			If (Asc.Pef.Vtabono2$(X.I%,0) = X.Trama$) Then Begin                      ! si existe
!				 X.J% = X.I%																														!
!				 X.Ind% = -1 : X.I% = Asc.Pef.NroBon% + 10															!
!			EndIf 																																		!
!		Next X.I%																																		!

		If X.Ind% = 0 Then Begin 																									  ! No Encontro bono
 				Asc.Pef.NroBon%  = Asc.Pef.NroBon% + 1                              		! Nro bonos capturados    
 				Asc.Pef.DataPos% = Asc.Pef.DataPos% + 1                                 ! Posicion del bono
        Asc.Pef.Vtabono2$(Asc.Pef.NroBon%,0) = X.Trama$                         ! Numero de la credencial
        Asc.Pef.Vtabono2$(Asc.Pef.NroBon%,1) = "1"                              ! Numero de Bonos entregados 				
        Asc.Pef.Vtabono2$(Asc.Pef.NroBon%,2) = Str$(Asc.Pef.DataPos%)           ! Posicion de vector
        
				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,0) = X.Trama$                         ! Numero de la credencial
				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,1) = X.Compra$                        ! Valor de la trx
				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,2) = Unpack$(TS.OPER$)                ! Operador que realiza la trx
				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,3) = X.Msg$                           ! Trama completa operacion
	 			Asc.Pef.Vtabono$(Asc.Pef.DataPos%,4) = Str$(Asc.Pef.Operacion%)         ! Tipo de operacion 
	 			
		EndIf Else Begin                                                            ! Si lo encontro

!				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,1) = X.Compra$                        ! Valor de la trx
!				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,2) = Unpack$(TS.OPER$)                ! Operador que realiza la trx    
!				Asc.Pef.Vtabono$(Asc.Pef.DataPos%,3) = X.Msg$                           ! Trama completa operacion

		EndIf 
End Sub 



Sub PEF.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			If Asc.Pef.Auditoria% <> -1 Then Exit Sub            ! Si No aplica auditoria

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

Function Anula.Credencial.Pef(x.bono$,X.Compra$,X.Cajero$)
String X.Bono$, X.Vlr$, X.Trama$, X.Tmp2$, X.Compra$, X.Cajero$, Anula.Credencial.Pef
String Host.Message$, X.Lec$, X.Finr$, X.ENVIA$, X.LLEGA$,  X.SALE$, X.RTA$
Integer*1 X.Ind%
Integer*4 X.Len%, X.I%, X.J%, XL.NRTRX%, XL.POS%
String X.Valor$, X.Opt$, XX.TEMP1$, XX.TEMP2$

    If Asc.Pef.Impr% = 2 Then Begin 
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
		
		For X.I% = 1 to Asc.Pef.NroBon%																					    ! Busca credencial
			If (Asc.Pef.Vtabono2$(X.I%,0) = X.Trama$) Then Begin                      ! si existe
				 X.J% = X.I%
				 XL.POS% = Val(Asc.Pef.Vtabono2$(X.J%,2))                               ! Posicion de busqueda
				 
				 X.Valor$ = Asc.Pef.Vtabono$(XL.POS%,1)                                 ! Valor del movimiento
				 If Val(X.Valor$) > 0 Then \                                            ! Movimiento con saldo
				   X.Ind% = -1 : X.I% = Asc.Pef.NroBon% + 10                            !
			EndIf                                                                     !
		Next X.I%                                                                   !
		
		
		
		If X.Ind% = 0 Then Begin 																							      ! No Encontro credencial
			 Call TSHIECET
			 Call VISORES4690(1,"NUMERO CREDENCIAL A","DEVOLVER NO EXISTE!!",1800,"C")
			 Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10): TS.IO.MOTORKEY = 73
			 Anula.Credencial.Pef = "0"
			 Exit Function 
		EndIf     

		If Val(X.Valor$) <= 0 Then Begin 
			 Call TSHIECET
			 Call VISORES4690(1,"NUMERO CREDENCIAL  ","YA SE REVERSO ...!!!",1800,"C")
			 Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10): TS.IO.MOTORKEY = 73
			 Anula.Credencial.Pef = "0"
			 Exit Function     
		EndIf 
		
		If NOT(TS.TRAINING) Then Begin 
			Call Visores4690(1,"ANULANDO CREDENCIAL",X.Trama$,0,"L")! Msg de alerta

			XX.TEMP1$ = Asc.Pef.Vtabono$(XL.POS%,3)                 ! Trama original
			
      If Asc.Pef.Operacion% = 11 Then Begin                            ! Si pago dentro de la trx
         TS.TEMP1I4 = Len(XX.TEMP1$) - 18
         XX.TEMP1$ = Left$(XX.TEMP1$,TS.TEMP1I4)
         X.Opt$ = "13"
      EndIf 
      If Asc.Pef.Operacion% = 02 Then Begin                            ! Si pago dentro de la trx
         TS.TEMP1I4 = Len(XX.TEMP1$) - 8
         XX.TEMP1$ = Left$(XX.TEMP1$,TS.TEMP1I4)
         X.Opt$ = "03"
      EndIf 			        											
			
			XX.TEMP2$ = Armar.Trama.Fonede(Asc.Pef.Appl$,X.OPT$,XX.TEMP1$,"00",Asc.Pef.Cadena$,"123456")                 ! Armar trama MSG
			X.ENVIA$  = XX.TEMP2$
			X.SALE$   = DATE$ +":"+ Time$
			XX.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", XX.TEMP2$)               ! Ejecuta Requerimiento
			X.RTA$    = DATE$ +":"+ Time$ 
			X.LLEGA$  = XX.TEMP2$
			Call PEF.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)
			If LEN(XX.TEMP2$) >= 53 Then Host.Message$=MID$(XX.TEMP2$,14,40) Else  \!
			                             Host.Message$="PROCESO NO FUE SATISFACTORIO"
			XX.TEMP1$ = Valida.Rta(XX.TEMP2$)																						       ! Valida rta entregada 

!       TS.TEMP1$ = "00"
!       HORA.MUNDIAL$ = DATE$ + TIME$    
  
      If XX.TEMP1$ = "00" Then Begin 
         Asc.Pef.Vtabono$(XL.POS%,1) = "0"         ! Reporta que movimiento anulado
         Asc.Pef.Vtabono2$(X.J%,2)   = Str$( Val(Asc.Pef.Vtabono2$(X.J%,2)) - 1 )
         Anula.Credencial.Pef = X.Valor$           ! Retorna el valor 
      EndIf Else Begin 
         Call PEF.AUDITORIA("ERROR DE APPL MANAGER","CODIGO ERROR ",DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)			
         XX.TEMP1$ = Asc.Pef.Vtabono$(X.J%,3)
         If Asc.Pef.Operacion% = 11 Then Begin                            ! Si pago dentro de la trx
            TS.TEMP1I4 = Len(XX.TEMP1$) - 18
            XX.TEMP1$ = Left$(XX.TEMP1$,TS.TEMP1I4)      
            X.Opt$ = "15"
         EndIf 
         If Asc.Pef.Operacion% = 02 Then Begin                            ! Si pago dentro de la trx
            TS.TEMP1I4 = Len(XX.TEMP1$) - 8
            XX.TEMP1$ = Left$(XX.TEMP1$,TS.TEMP1I4)            
            X.Opt$ = "05"
         EndIf 			        											
         XX.TEMP2$ = Armar.Trama.Fonede(Asc.Pef.Appl$,X.Opt$,XX.TEMP1$,"00",+        \!
																		 Asc.Pef.Cadena$,"123456")                     ! Armar trama MSG				 
			   X.ENVIA$  = XX.TEMP2$
			   X.SALE$   = DATE$ +":"+ Time$
			   X.Len% = Len(XX.TEMP2$)						  					  								         ! Toma longitud del registro
			   X.Lec$ = "C"+Str$(X.len%)+" C2"								  							           ! Arma estructura de grabacion
			   XX.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", XX.TEMP2$)      ! Ejecuta Requerimiento 				 				 
   		   X.LLEGA$  = XX.TEMP1$																		 
			   XX.TEMP2$ = Valida.Rta(XX.TEMP1$)
			   X.RTA$    = DATE$ +":"+ Time$ 
         Call PEF.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)
			   If XX.TEMP2$ <> "00" Then Begin 
				  TS.ER.RETURN = -1 
         EndIf 
				 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                    ! 
				 TS.IO.MOTORKEY = 73                                                         ! cancela proceso
				 Anula.Credencial.Pef = "0"																			             !
				 Exit Function 			
			EndIf 

			XX.TEMP1$ = MID$(XX.TEMP2$,12,2)                                               ! Toma rta de la BD
		  If XX.TEMP1$ <> "00" Then Begin								   														   ! Si presenta un error, verif. local
 			 If Host.Message$ = "" Then \
						XX.TEMP1$ = Asc.Pef.Errores$(Val(XX.TEMP1$))  \                          ! Lista de errores
				Else \
						XX.TEMP1$ = Host.Message$
				Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                   ! 
				TS.IO.MOTORKEY = 0                                                         ! cancela proceso
				Anula.Credencial.Pef = "0"
				Exit Function 																													   !
			EndIf Else Begin					                                                     ! TRX AUTORIZADA      
			 XX.TEMP1$ = STR$(Val(X.BONO$))
			 X.Bono$ = STR$(Val(X.BONO$))
			 X.Bono$ = RIGHT$("                  "+X.Bono$,18)                             ! ajusta dato capturado
			 Call U.Imprime("CREDENCIAL FONEDE:"+XX.TEMP1$,6100H)                             ! Imprime nro tarjeta en tiquete              
			 XX.TEMP1$ =    Right$("0000"+TS.STORE$,4)                     +  	          \! Numero del almacen
											Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 		          \! Numero de terminal
											X.Cajero$                                      +              \! Codigo del cajero
											Right$("000000"+Str$(XL.NRTRX%),6)             + 			        \! Numero de transaccion
											HORA.MUNDIAL$				                           +              \! Fecha y Hora de la operacion                      
											X.Bono$                                        +              \! Numero de credencial
											":"+Pack$(X.Valor$)+":"+Pack$("03")                            ! Valor credencial

					Call Grabacion.Cadena.Usuario("20060524",XX.TEMP1$)                        ! Graba String usuario
					
					!Anula.Credencial.Pef = Asc.Pef.Vtabono$(X.J%,1)									  			   ! Retorna numero credencial
					
					Asc.Pef.Vtabono$(X.J%,1) = "0"                                             ! Inicializa bono
					Anula.Credencial.Pef = X.Valor$           ! Retorna el valor 
			EndIf 
		EndIf 
End Function
!--- Fin anulacion pago
 
Sub Reversa.Anul.Total.Pef
Integer*4 X.I%
	For X.I% = 1 TO Asc.Pef.NroBon%																					      ! Total de Credenciales
		If Val(Asc.Pef.Vtabono$(X.I%,1)) > 0 Then                    \! Solo movimientos con saldo
				Call Anula.Credencial.Pef(Asc.Pef.Vtabono$(X.I%,0),      \! Numero credencial    
															Asc.Pef.Vtabono$(X.I%,1),          \! Valor  credencial
															Asc.Pef.Vtabono$(X.I%,2))           ! Cajero registro trx
															
	Next X.I%
End Sub 
!--- Fin reversion total compra

Sub Validacion.Compra.Pef.Electronico(X.Opt$)
String X.Opt$, X.Trama$, X.VlrCompra$
String Host.Message$, X.Lec$, X.Finr$, Rbuffer$, X.ENVIA$, X.LLEGA$, X.SALE$, X.RTA$
Integer*4 X.Len%, X.LONG%
		X.Finr$ = Chr$(13) + Chr$(10)
		Call TSHIECET                                                                ! Tono de alerta
!
!--- Linea adicionada para pruebas
!		TS.TEMP1$ = "F" + TS.TEMP1$
!--- Retirar linea para produccion		

		Call VISORES4690(1,"PROCESA CREDENCIAL:",TS.TEMP1$,0,"L")    						     !
		TS.TEMP1$ = RIGHT$("                  "+TS.TEMP1$,18)                        ! ajusta dato capturado

		If Val(X.Opt$) = 11 Then Begin                                               ! Si es una compra
		   X.VlrCompra$ = Str$(TS.BALDUE(0))
		   X.VlrCompra$ = Right$("0000000000"+X.VlrCompra$,10)
		EndIf Else Begin                                                             ! Si es intercambio medios pago
		   X.VlrCompra$ = "0000000000"
	  EndIf 
	  
	  Asc.Pef.VlrCompra$ = X.VlrCompra$
	  If Val(X.VlrCompra$) = 0 Then \
	   TS.TEMP1$ = TS.TEMP1$ + Asc.Pef.Contable$	Else \
	   TS.TEMP1$ = TS.TEMP1$ + Asc.Pef.Contable$ + Asc.Pef.VlrCompra$
		TS.TEMP6$ = TS.TEMP1$
		TS.TEMP2$ = Armar.Trama.Fonede(Asc.Pef.Appl$,X.Opt$,TS.TEMP1$,"00",Asc.Pef.Cadena$,"123456")                 ! Armar trama MSG
		X.SALE$ = DATE$ + ":" +TIME$
		X.ENVIA$ = TS.TEMP2$   
		TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)              ! Ejecuta Requerimiento
		X.RTA$ = DATE$ + ":"+TIME$
		X.LLEGA$ = TS.TEMP2$
		Call PEF.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)

	 If LEN(TS.TEMP2$) >= 53 Then Host.Message$=MID$(TS.TEMP2$,14,40) Else Host.Message$="PROCESO NO FUE SATISFACTORIO"
	 TS.TEMP1$ = Valida.Rta(TS.TEMP2$)																						 ! Valida rta entregada 

!  TS.TEMP1$ = "00"
!  HORA.MUNDIAL$ = DATE$ + TIME$
	 
	 If TS.TEMP1$ <> "00" Then Begin								   										        ! Si presenta un error, verif. local
	 
      Call PEF.AUDITORIA("ERR APPL 1","COD ERR:"+TS.TEMP1$+" "+Host.Message$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 	 
			Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			TS.IO.MOTORKEY = 73                                                       ! cancela proceso
			Asc.Pef.Cambio% = 4
			Exit Sub
	 EndIf 

	 TS.TEMP1$ = MID$(TS.TEMP2$,12,2)                                             ! Toma status de la BD

!  TS.TEMP1$ = "00"

	 If TS.TEMP1$ <> "00" Then Begin								   														! Si presenta un error, verif. local

       Call PEF.AUDITORIA("ERR APPL 2","COD ERR:"+TS.TEMP1$+" "+Host.Message$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 	 
			 If Host.Message$ = "" Then \
					 TS.TEMP1$ = Asc.Pef.Errores$(Val(TS.TEMP1$))     \                   ! Lista de errores
			 Else \
					 TS.TEMP1$ = Host.Message$
			 Call TSHIECET                                                             !
			 Call VISORES4690(1,Left$(TS.TEMP1$,20),Right$(TS.TEMP1$,20),0,"L")        !
			 Call VISORES4690(2,Left$(TS.TEMP1$,20),Right$(TS.TEMP1$,20),1500,"L")     !
			 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			 TS.IO.MOTORKEY = 73                                                       ! cancela proceso
			 Asc.Pef.Cambio% = 4
 		   Exit Sub     																														 ! 

		EndIf Else Begin					                                                   ! TRX AUTORIZADA
  		 Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)                                  ! 
			 TS.IO.MOTORKEY = 0                                                        !     
			 TS.TEMP1$ = STR$(Val(Right$(TS.TEMP2$,10)))
        
!       TS.TEMP1$ = "25000" 

			 TS.IO.DATA$(7) = TS.TEMP1$                                                ! Valor del subsidio
			 TS.IO.DATA$(9) = Asc.Pef.NumNota$                                         ! Graba numero credencial	
			 TS.IO.KEYS(9)  = 90
			 TS.IO.KEYS(3)  = 78                                                       ! Simula pago al tipo y variedad
			 TS.IO.DATA$(3) = Right$(Asc.Pef.tpvr$,1)                                  ! de pago parametrizada para 
			 TS.IO.MOTORKEY = VAL("9"+Left$(Asc.Pef.tpvr$,1))                          !
			 TS.IO.KEYS(7)  = VAL("9"+Left$(Asc.Pef.tpvr$,1))                          !
			 Call U.Imprime("CREDENCIAL FONEDE:"+Asc.Pef.NumNota$,6100H)                  ! Imprime nro tarjeta en tiquete              
			 Asc.Pef.IMPMSG% = -1 
			 TS.TEMP1$ =    Right$("0000"+TS.STORE$,4)                     +          \! Numero del almacen
											Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 	        \! Numero de terminal
											Unpack$(TS.OPER$)                              +          \! Codigo del cajero
											Right$("000000"+Str$(SL.HD.TRANSNUM+1),6)      + 	        \! Numero de transaccion
											HORA.MUNDIAL$                                  +          \! Fecha y Hora de la operacion                      
											TS.TEMP6$                                                  ! Numero de credencial
       
			 Call Busca.Pef.Registrado(Asc.Pef.NumNota$,TS.IO.DATA$(7),TS.TEMP1$)      ! Controla pagos recaudados
			 TS.TEMP1$ = TS.TEMP1$ + ":"+Pack$(TS.IO.DATA$(7))                         ! Valor del bono                      
			 TS.TEMP1$ = TS.TEMP1$ + ":"+Pack$(X.OPT$)																 ! Add tipo de movimientp
			 Call Grabacion.Cadena.Usuario("20060523",TS.TEMP1$)                       ! Graba String usuario
       Asc.Pef.Slend% = SL.END	+ 1																							 ! Total de strings
		EndIf 
End Sub 
!--- fin validacion compra

Sub Reverso.Trama.Error.Pef
String X.Trama$, X.Lec$, X.Finr$, X.Opt$
Integer*4 X.Len%
      X.Finr$ = Chr$(13) + Chr$(10)
      Call PEF.AUDITORIA("ERROR DE APPL MANAGER","CODIGO ERROR "+TS.TEMP1$,DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)				 
			TS.TEMP1$ =    Right$("0000"+TS.STORE$,4)                     +          \! Numero del almacen
										 Right$("000000"+Str$(SL.HD.TERMINAL),6)        + 	       \! Numero de terminal
										 Unpack$(TS.OPER$)                              +          \! Codigo del cajero
										 Right$("000000"+Str$(SL.HD.TRANSNUM+1),6)      + 	       \! Numero de transaccion
										 HORA.MUNDIAL$                                  +          \! Fecha y Hora de la operacion                      
										 TS.TEMP6$                                                  ! Numero de credenci
      If Asc.Pef.Operacion% = 11 Then X.Opt$ = "14" Else X.Opt$ = "04"
      
      If Asc.Pef.Operacion% = 11 Then Begin                            ! Si pago dentro de la trx
         TS.TEMP1I4 = Len(TS.TEMP1$) - 18
         TS.TEMP1$ = Left$(TS.TEMP1$,TS.TEMP1I4)
      EndIf 
      If Asc.Pef.Operacion% = 02 Then Begin                            ! Si pago dentro de la trx
         TS.TEMP1I4 = Len(TS.TEMP1$) - 8
         TS.TEMP1$ = Left$(TS.TEMP1$,TS.TEMP1I4)
      EndIf 			        
      
			TS.TEMP2$ = Armar.Trama.Fonede(Asc.Pef.Appl$,X.Opt$,TS.TEMP1$,"00",+        \!
																	Asc.Pef.Cadena$,"123456")                     ! Armar trama MSG
 			X.Len% = Len(TS.TEMP2$)						  					  								          ! Toma longitud del registro
 			X.Lec$ = "C"+Str$(X.len%)+" C2"								  							            ! Arma estructura de grabacion																	

!      TS.TEMP1$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)      ! Ejecuta Requerimiento
!			TS.TEMP1$ = Valida.Rta(TS.TEMP1$)																						       ! Valida rta entregada       
    
      TS.TEMP1$ = "ER"
      If TS.TEMP1$ <> "00" Then Begin 
 			   TS.ER.RETURN = -1 
 			   
!-- Se elimina grabacion al sec.dat GR - OVS 1 Nov 2012
!			   Open "R::C:\JAVA\BIN\SEC.DAT" AS 56 Append
!			   If TS.ER.RETURN = -1 Then Begin 
! 				    Write form X.Lec$; #56 ; TS.TEMP2$, X.Finr$           							    ! Graba registro      
!				    Close 56
!				    Call PEF.AUDITORIA(TS.TEMP2$,"REVERSO GENERADO "+TS.TEMP2$, DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
!			   EndIf Else Begin 
!			      Call PEF.AUDITORIA(TS.TEMP2$,"REVERSO NO GENERADO"+Asc.Pef.Rrn$ , DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
!			      Call U.Imprime("REVERSO NO GENERADO "+TS.TEMP6$,2100H)
!		     EndIf 

		  EndIf 
End Sub 
!-- Fin reverso PEF

Sub Confirmacion.Movimientos.Pef
Integer*1 Carga.Mov.Pes																										!
Integer*4 PRIC%, TOT4%, XL.I%, Asc.Ind.Tmp%, XL.J%, X.Len%			  				!
String XL.TMP$, XL.TRAMA$,XL.TARJETA$, XL.TIPO$, XL.VLR$									!
String X.ENVIA$, X.SALE$, X.RTA$, X.LLEGA$, X.Lec$, X.Finr$
Integer*4 Xl.Pos%
  X.Finr$ = Chr$(13) + Chr$(10)																		  			! Fin de registro 
  
  For XL.J% = 1 TO Asc.Pef.NroBon%														  			    ! Lista de credenciales a confirmar
   Xl.Pos% = Val( Asc.Pef.Vtabono2$(XL.J%,2) )                            ! Posicion de vector de busqueda
   If Val(Asc.Pef.Vtabono$(XL.POS%,1)) <> 0 Then Begin                    ! Bono con movimiento
      XL.TIPO$  = Asc.Pef.Vtabono$(XL.POS%,4)                             ! Tipo de trx
      TS.TEMP1$ = Asc.Pef.Vtabono$(XL.POS%,3)                             ! Trama original
      
      If Val(XL.TIPO$) = 11 Then Begin                                    ! Si pago dentro de la trx
         TS.TEMP1I4 = Len(TS.TEMP1$) - 18
         TS.TEMP1$ = Left$(TS.TEMP1$,TS.TEMP1I4)
         XL.TIPO$ = "16"
      EndIf 
      If Val(XL.TIPO$) = 02 Then Begin                                    ! Si pago fuera de la trx
         TS.TEMP1I4 = Len(TS.TEMP1$) - 8
         TS.TEMP1$ = Left$(TS.TEMP1$,TS.TEMP1I4)
         XL.TIPO$ = "06"
      EndIf 			         
  
   TS.TEMP2$ = Armar.Trama.Fonede(Asc.Pef.Appl$,Xl.Tipo$,TS.TEMP1$,"00",Asc.Pef.Cadena$,"123456") ! Armar trama MSG
   X.ENVIA$  = TS.TEMP2$                                                  ! datos de auditoria
   X.SALE$   = DATE$ +":"+ Time$                                          ! Fecha y hora envio trama
   TS.TEMP2$ = Rutina.Java("com.appl.ApplKernel","threader", TS.TEMP2$)   ! Ejecuta Requerimiento
   X.RTA$    = DATE$ +":"+ Time$                                          ! Fecha y hora Respuesta
   X.LLEGA$  = TS.TEMP2$                                                  ! Trama respuesta
   Call PEF.AUDITORIA(X.ENVIA$,X.LLEGA$, X.SALE$, X.RTA$)                 ! Graba auditoria
   TS.TEMP1$ = Valida.Rta(TS.TEMP2$)		                                  ! Valida rta entrega           
   If TS.TEMP1$ NE "00" Then Begin                                        ! Error Mensaje Java
      TS.ER.RETURN = -1                                                   ! Ctrl Errores

!-- Se elimina grabacion al sec.dat GR - OVS 1 Nov 2012
!      Open "R::C:\JAVA\BIN\SEC.DAT" AS 56 Append                          ! Apertura cola mensajes
!      If TS.ER.RETURN = -1 Then Begin                                     ! Si apertura correcta
!         X.LEN% = Len(X.ENVIA$)                                           ! Longitud de la trama
!         X.Lec$ = "C"+Str$(X.len%)+" C2"								                  ! Arma estructura de grabacion
!         Write form X.Lec$; #56 ; X.ENVIA$, X.Finr$     							    ! Graba registro
!	       Close 56                                                         ! Cierra Archivo
!         Call PEF.AUDITORIA(X.ENVIA$,"ENVIADO AL STORE AND FORWARD", DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
!	    EndIf Else Begin 
!	       Call VISOR.AND.BORRAR("ERROR CONFIRMACION  FONEDE REPORTELO....")
!         Call U.Imprime("--- ERROR CONFIRMACION P.E.F ---",6100H)         ! Error en confirmacion tramas
!         Call U.Imprime("Credencial "+Mid$(Xl.Tmp$,39,18),6100H)          ! Numero de credencial
!         Call U.Imprime("REPORTAR CASO A FONEDE",6100H)                   !
!         TS.TEMP1$ = Linea.Detalle(56)
!         Call U.Imprime(TS.TEMP1$,6100H)
!         Call U.Imprime(" ",4500H)                                        ! Avance de lineas
!         Wait ; 100
!         Call U.CORTACR                                                   ! Corta Papel
!	   EndIf 

	 EndIf    
	EndIf 
  Next XL.J%
  
End Sub
!--- Fin analisis de pes 

Sub CALCULO.FECHA.CONTABLE              ! Calculo fecha contable
Integer*2 X.Meses%(1), AA%, MM%, DD%
Dim X.Meses%(14)

Exit Sub                     ! Se cancela ejecucion proceso GR-OVS 01 NOV 2012

X.Meses%( 1) = 31 : X.Meses%( 2) = 28 : X.Meses%( 3) = 31
X.Meses%( 4) = 30 : X.Meses%( 5) = 31 : X.Meses%( 6) = 30
X.Meses%( 7) = 31 : X.Meses%( 8) = 31 : X.Meses%( 9) = 30
X.Meses%(10) = 31 : X.Meses%(11) = 30 : X.Meses%(12) = 31

AA% = Val(Left$(Asc.Pef.Contable$,4))   ! Toma Año
MM% = Val(Mid$(Asc.Pef.Contable$,5,2))  ! Toma Mes
DD% = Val(Right$(Asc.Pef.Contable$,2))  ! Toma Dia
DD% = DD% + 1                           ! Incrementa Dia
If MM% < 1 OR MM% > 12 Then Begin 
   Call VISOR.AND.BORRAR("MES CONTABLE  ESTA  MAL DEFINIDO")
   Call VISOR.AND.BORRAR("TOMANDO FECHA DEL   SISTEMA     ")
   Call VISOR.AND.BORRAR("REPORTE ESTA FALLA  AL SUPERVISOR")
   Asc.Pef.Contable$ = "20"+DATE$
   Exit Sub 
EndIf 
If X.Meses%(MM%) < DD% Then Begin       ! Si cambio de mes
   DD% = 1                              ! Asigno dia 1ro
   If MM% < 12 Then Begin               ! Si mes no es diciembre
      MM% = MM% + 1                     ! Aumenta el mes
   EndIf Else Begin                     ! Ajusta el Mes
      MM% = 1                           ! Asume Enero
      AA% = AA% + 1                     ! Aumenta un año
   EndIf 
EndIf 
Asc.Pef.Contable$ = Str$(AA%)                + \! Año del movimiento
                    Right$("00"+Str$(MM%),2) + \! Mes del movimiento
                    Right$("00"+Str$(DD%),2)    ! Dia del movimiento

End Sub 

!--------------
!--- Bloque Principal
!--------------

!--- EAMTSU02.J86
Sub PEFMTS02.011  Public 
If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU02.011
EndIf 

End Sub 

!--- EAMTSU07.J86
Sub PEFMTS07.011  Public 

	 %Include PEFTSU07.011

End Sub 

!--- EAMTSU14.J86
Sub PEFMTS14.011  Public 

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU14.011
	 
EndIf 

End Sub 

!--- EAMTSU20.J86
Sub PEFMTS20.011  Public 

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU20.011
EndIf 

End Sub 

!--- EAMTSU23.J86
Sub PEFMTS23.011  Public 

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU23.011
EndIf 

End Sub 

!--- EAMTSU53.J86
Sub PEFMTS53.011  Public 

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU53.011
EndIf 

End Sub 

!--- EAMTSU56.J86
Sub PEFMTS56.011  Public 

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU56.011
EndIf 

End Sub 

!--- EAMTSU66.J86
Sub PEFMTS66.011(X.CallParm)  Public 
Integer*1 X.CallParm

If Asc.Pef.Line% Then Begin 
	 %Include PEFTSU66.011
EndIf 

End Sub
