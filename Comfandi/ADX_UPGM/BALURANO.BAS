!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : BALURANO.BAS                     *
!Autor         : Edwin Cadena Sierra              *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 1 de 2.016                 *
!Observaciones : Lectura Balanza Urano            *
!                Version 1.00                     *
!**************************************************
!* MODIFICACIONES


%ENVIRON T		                 ! Ambiente de terminal

%INCLUDE SERTSUVA.011	         ! Variables del modulo
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables de transaccion
%INCLUDE EAMTERMS.J86          ! Variables terminal
%INCLUDE EAMTOPTS.J86	         ! Terminal Options
%INCLUDE EAMP4VAJ.J86          ! Rutinas 4610
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          ! Rutinas Asembler

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   							        ! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function ASIC.DATOS$(UMSG1$,UMSG2$) External   						! Captura de datos
String UMSG1$, UMSG2$, ASIC.DATOS$     							      ! Definicion de variables
End Function
!--- Fin Asic.Datos

Function U.IMPRIME(UE.STRING$,UE.STATION) External				! Rutina de Impresion
String UE.STRING$      									                  ! Variables temporales
INTEGER*2 UE.STATION									                    !
End Function
!--- Rutina de Impresion

FUNCTION U.CORTACR External								                ! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

!--- Apertura del puerto serial
! Se debe pasar como parametro a la funcion el numero de puerto:
! Numero del Puerto Serial ( 1 o 2)
! La velocidad de comunicacion (2400, 9600..)
! la paridad (N o E)
! El data (7 o 8)
! El dato stop (1)
! el numero de sesion de aperuta
!--------
Function Open.Serial(PTO.COM%,PTO.VEL%,UE.ST$,UE.X%,UE.Y%,PTO.SERIAL%) External	  !
 Integer*4 PTO.VEL%					       				                                        ! Esta funci¢n 
 Integer*1 PTO.COM%,PTO.SERIAL%,Open.Serial                  				              ! realiza una apertura
 String PTO.MSG1$,PTO.MSG2$				       				                                  ! al COM solicitado
 String UE.ST$
 Integer*1 UE.X%, UE.Y%
 End Function 

!--- Cierre del Puerto Serial
! Se pasa como parametro a la funcion el numero de sesion
! que se este trabajando
!---------
Function Close.Serial(PTO.SERIAL%) External       				                        ! Por medio de
Integer*1 PTO.SERIAL%, Close.Serial                           				            ! esta funcion
End Function                                                              				! 

!--- Estado del puerto serial
! como parametro se pasa el numero de sesion del dispositivo
! serial y retorna el status del dispositivo en 3 variables
! UE.PTO.ON%   (-1 En linea           0. Fuera Linea)
! UE.PTO.READ% (-1 Datos en el buffer 0. Sin datos buffer)
! UE.PTO.LOST% (-1 Perdida de datos   0. No perdidad datos)
!------------------------------------------------------------------

Function Status.Serial(PTO.SERIAL%) External       		                          ! Por medio de
Integer*1 PTO.SERIAL%         		       																				! esta funci¢n 
End FUNCTION
!--- Fin validacion estado de la terminal

!---- Grabacion de datos a un puerto serial
! Se pasa como parametro el dato a grabar y el
! numero de sesion del puerto serial
!----------------------------------------------------
Function Write.Serial(PTO.DATO$,PTO.SERIAL%) External           	                ! Grabacion datos
String PTO.DATO$, PTO.MSG1$, PTO.MSG2$                           									! en el puerto serial
Integer*2 PTO.SERIAL%, Write.Serial                              									!
End Function 					       																											! para el lector.
!--- Fin grabacion datos en puerto serial

Function Read.Serial(PTO.SERIAL%) External          		                          !  
Integer*1 PTO.SERIAL%, PTO.ESPERA%					                                      !
String    Read.Serial						                                              !
Integer*2 PTO.READ%,NUM.SESION%							                                                  !
String PTO.LECTURA$,PTO.DATO1$,	      		  	                                   \!
       PTO.MSG1$,PTO.MSG2$				   		                                          !
End FUNCTION
!--- Fin serial lectura

!--- Fin de la definicion de rutinas Externas 


Sub URANO(Xopt%) Public
Integer*4 Xopt%
Integer*2 ptoOnline%, escriPto%
Integer*1 openPortOnline%

If Xopt% = 15 Then Begin
	
	call Status.Serial(1)
	if (UE.PTO.ON%)Then Begin
		Call VISORES4690(1,"EN LINEA PTO 1   ","POR :"+str$(UE.PTO.ON%),2000,"L")
		ptoOnline% = 1
  Endif Else Begin
  	call Status.Serial(2)
		if (UE.PTO.ON%)Then Begin
			Call VISORES4690(1,"EN LINEA PTO 2   ","POR :"+str$(UE.PTO.ON%),2000,"L")
			ptoOnline% = 2
		Endif
	Endif
	
! PARA URANO
! La velocidad de comunicacion 9600..)
! la paridad N SIN PARIDAD
! El data 8)
! El dato stop (2)
		
	openPortOnline% = Open.Serial(ptoOnline%,9600,"N",8,2,94)

	if (openPortOnline%) Then Begin
		Call VISORES4690(1,"OPEN   ",":"+str$(openPortOnline%),4000,"L")
		escriPto% = Write.Serial(chr$(04),94)
		call Status.Serial(ptoOnline%)
		if(UE.PTO.READ%) Then Begin
			ts.temp1$ = Read.Serial(94)
			Call VISORES4690(1,"DATO   ",":"+ts.temp1$,2000,"L")
			write #34; "Dato Balanza "+	ts.temp1$ + chr$(10)
			call Close.Serial(94)
		Endif else Begin
			Call VISORES4690(1,"NO datos en buffer",":"+ts.temp1$,2000,"L")
		Endif		
	Endif
	!	Else Begin
	! write #34; "Dato Balanza "+	ts.temp1$ + chr$(10)	 
EndIf 

End Sub
