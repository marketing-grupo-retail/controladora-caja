!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMFISCO.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 14 del 2.004               *
!Observaciones : Modulo Numeracion Fiscal         *
!                Legislacion Tributaria Colombia  *
!**************************************************
! Observaciones:
! Se modifica modulo fiscal para control de vencimiento
! de consecutivo de numeracion.
! Desarrollado por Grupo Retail - OVS 16 Sept 2010
!------------------------------------------------------
! Mod09Jun2011
! Se ajusta la impresion del numero fiscal de venta
! retirando los ceros a la izquierda, segun documento
! de consulta a la DIAN 
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod14Ago2012
! Se ajusta modulo para que informe vencimiento de 
! consecutivo de numeracion apenas este a 10.000
! movimientos.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 03Nov2016
! Se ajusta modulo fiscal para manejo de numeracion 
! a 14 caracteres numero de autorizaion.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 13Mzo2019
! Se modifica el numero de anuncio de proximo vencimiento
! fiscal de 40 a 15 dias por requerimiento de 
! Comfandi.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 11Sep2019
! Se ajusta el calculo de fechas para vencimiento de la 
! numeración fiscal
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 28Oct2018
! Se instala control para validación de consecutivo 
! actual de numeración segun el rango autorizado por 
! la dian.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 22Jun2021
! Se desarrolla la funcionalidad de registro de forma 
! de pago 15 (Tienda virtual) con la captura de un codigo
! de barras con el siguiente formato:
! 86 pppppppppp dddddddddd
! La longitud es de 22 caracteres donde tiene un prefijo 
! 86 despues el precio del recibo (pp) y el numero del 
! documento (dd).
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 09Feb2022
! Se desarrolla la funcionalidad para el registro de los 
! pagos de envio domicilio (Tienda virtual) con la captura
! de un codigo de barras con el siguiente formato:
! 888 IIII PPPPP 
! La lungitud es de 12 caracteres mas digito de chequeo
! donde IIII es el PLU del domicilio y PPPPP es el precio
! a cobrar.
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! 27Sep2022
! Se controla validación fecha del sistema si año menor 
! a 2021 detiene la operación del sistema, se adiciona la
! secuencia para manejo de tarjeta visa comfandi
!---------------------------------------------------------
! 10Nov2022
! Se adiciona la funcionalidad para manejo de la forma de 
! pago brilla, escaneando el recibo emitidpo por Brilla
! la estructura del codigo de barras es el siguiente:
! Prefijo     3 caracteres
! Referencia 24 caracteres
! Fecha       8 caracteres
! Monto cred 10 caracteres
! se parameriza en archivo de control el prefijo y la 
! forma de pago utilizada por el pago
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 29Jun2023
! Se adiciona validacion FECO para forma de pago
! tienda virtual y brilla
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 08Nov2023
! Modificacion presentacion factura fiscal tiquete pos 
! con el siguiente mensaje:
! TIQUETE POS : PREFIJO XXXX ##########
! Solicitado por Comfandi - Freddy Guerrero
! Desarrollado por Grupo Retail - OVS 
!------------------------------------------------------
! Mod 5Abr2024
! Ajuste modulo para manejo facturacion electronica 
! version 1.9
! Cambio diseño de tiquete e impresion de items 
! XXX IIIIIII DDDDDDDDDDDDDDDDDDDDDDD TT
! QQQQQQ X pp.ppp.ppp  UUU PP.PPP.PPP
! XXX -> NUMERO DE LINEA
! IIIIIII -> Item Producto
! DDDDDDD -> Descripcion producto
! TT -> flags de impuestos 
! QQQQ -> Cantidad
! UUU -> Unidad de medida
! ppp -> Precio unitario 
! PPP -> Valor total 
! Desarrollado por Grupo Retail - OVS/CAC
!------------------------------------------------------
! Mod 21Jun2024
! Se retira el control de numeración fiscal con
! el manejo de la NVRAM
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod 30Jul2024
! Se ajusta proceso de manejo archivo definicion
! fiscal TF:NMFECO
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod 14Ago2024
! Se controla fallas de grabacion de numeracion fiscal
! en disco generando daños de consecutivos y en ocaciones
! dump a la terminales controldoras 
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------
! Mod 11Abr2025
! Se modifica grabación de consecutivos, para cualquier tipo
! de falla en el proceso de comunicación con la DIAN queme
! los consecutivos fiscal y contingencia para evitar conflictos
! con plataforma de Carvajal.
! Desarrollado por Grupo Retail - OVS
!------------------------------------------------------

%ENVIRON T		                 																							! Ambiente de terminal

Integer*1 Global  Asc.Ind.Open.Fiscal% , ASIC.LLAVE%, Asc.Pay.Impr%
Integer*1 Global  Gr.ChgPrc.Proc%,                                         \! Inactiva control de precio
                  Gr.Fis.FailFis%,                                         \! Control falla proc fiscal
                  Gr.Fis.Domictv%                                           ! Domicilios tienda virtual
String    Global  Gr.Fis.Date$, Gr.Fis.Datev$, Gr.Fis.Fechas$,             \!
                  Gr.Fis.DomiPrc$, Gr.Fis.Numer$(2),                       \!
                  Gr.Fis.Direc$, Gr.Fis.City$, Gr.Fis.DatQr$(1),           \!
                  Gr.Fis.Nlin$, Gr.Fis.NItm$, Gr.Fis.Cufe$,		             \!
                  Gr.Fis.TrxId$, NO.FAC$, Gr.Fis.VerDian$, GRVERAPP$, NITE$ !
Integer*4 Global  Gr.Fis.Inicial%, Gr.Fis.Final%, Gr.Fis.ConMem%,          \!
                  Gr.Fis.FisMem%
Integer*1 Global  Gr.Fis.TrjVisa%,                                         \!
                  Gr.Fis.BrillaTrx%                                         !
String    Global  Gr.Fis.BrillaDoc$,                                       \!
                  Gr.Fis.BrillaDate$,                                      \!
                  Gr.Fis.BrillaPre$,                                       \!
                  Gr.Fis.BrillaPgo$,                                       \!
                  Gr.Fis.BrillaValr$                                        !
String    Global  Gr.Mnf.Fac$
String    Global Gr.Fis.PrefFis1$,                                         \! Prefijo fiscal 1
                 Gr.Fis.PrefFis2$, 																				 \! Prefijo fiscal 2
                 Gr.Fis.ResolFis1$,																				 \! Prefijo fiscal 2
                 Gr.Fis.ResolFis2$ 																				  ! Prefijo fiscal 2

%INCLUDE ADX_UPGM:EAMTSWKG.J86                                              ! working storage                     
%INCLUDE ADX_UPGM:EAMTRANS.j86                                              ! Variables procesos de transaccion   
%INCLUDE ADX_UPGM:EAMTERMS.J86                                              ! Variables procesos terminal         
%INCLUDE ADX_UPGM:EAMTOPTS.J86	                                            ! Variables Terminal Options          
%INCLUDE LCLTSUVA.011	         																							! Variables desarrollo
%INCLUDE GRFTSUVA.011	                                                      ! Variables desarrollo                
%INCLUDE ADX_UPGM:EAMTSXHC.J86                                              !                                     
%INCLUDE ADX_UPGM:EAMASMRT.J86                                              !                                     
%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub TSPREC01  External																										  ! Rutina de impresion SMA
End Sub 

Sub TSPREC03  External																											! Rutima de MSG SMA
End Sub 

Sub TSHIECET External		    																								!  Tono de alerta
End Sub 

SUB TSCSEC10 EXTERNAL                 																			! Termination procedure
End Sub 

Function FORMAT.AMOUNT(AMT1) External 																			! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
END Function

Sub GrAnulacion.Total External 
End Sub 

Sub Aplica.Pago.Brilla External																							! Aplica pago brilla
End Sub 

Sub Confirma.Pago.Brilla External
End Sub 

Function VALIDA.COMPRA.UVT External 
Integer*1 VALIDA.COMPRA.UVT
End Function 

Function LISTA.TRX.DIAN External																						! Datos para envio de trx carvajal
String LISTA.TRX.DIAN
End Function 																																!

Sub IVATSU20.012 External
End Sub 

Sub FECO.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) Public								! Auditoria FECO
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
			TS.ER.RETURN = -1																											!
			X.FILE$ = "R::ADX_UDT1:FD" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
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

Sub Lectura.Consecutivo.Fiscal Public																				! Lectura parámetros numeracion fiscal
String X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.I$, X.F$,     \!
       X.Tipo$, X.RESOL$, X.Aut2$, X.FECV$																	!
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%														!
Integer*2  X.cont%
X.Cont% = 0
Lectura.Fiscal:
       X.KEY$ = RIGHT$("0000"+TS.TERMINAL$,4)															  !
       X.KEY$ = PACK$(X.KEY$)																							  !
       X.Lec$ = "C2 C4 3C5 C7 2C3 C4 3C5 C7 2C3 C4 3C5 C7 2C3"              ! Nuevo Formato 05Abr2024 FECO
       TS.ER.RETURN = -1 																										! Control de errores
       Read FORM X.LEC$;#Gr.Fis.Sesion% KEY X.KEY$;                        \! Busca Terminal
            X.KEY$, 							                                         \! Numero de la terminal        
            Gr.Fis.Numer$(1,1),                                            \! Prefijo factura fiscal       
            Gr.Fis.Numer$(1,2),                                            \! Rango factura inicial        
            Gr.Fis.Numer$(1,3),                                            \! Rango factura final          
            Gr.Fis.Numer$(1,4),                                            \! Numero actual factura        
            Gr.Fis.Numer$(1,5),                                            \! Numero resolucion            
            Gr.Fis.Numer$(1,6),                                            \! Fecha inicial resolucion     
            Gr.Fis.Numer$(1,7),                                            \! Fecha final resolucion       
            Gr.Fis.Numer$(2,1),                                            \! Prefijo factura contingencia 
            Gr.Fis.Numer$(2,2),                                            \! Rango factura inicial        
            Gr.Fis.Numer$(2,3),                                            \! Rango factura final          
            Gr.Fis.Numer$(2,4),                                            \! Numero actual factura        
            Gr.Fis.Numer$(2,5),                                            \! Numero resolucion            
            Gr.Fis.Numer$(2,6),                                            \! Fecha inicial resolucion     
            Gr.Fis.Numer$(2,7),                                            \! Fecha final resolucion       
            Gr.Fis.Numer$(3,1),                                            \! Prefijo notas credito        
            Gr.Fis.Numer$(3,2),                                            \! Rango factura inicial        
            Gr.Fis.Numer$(3,3),                                            \! Rango factura final          
            Gr.Fis.Numer$(3,4),                                            \! Numero actual factura        
            Gr.Fis.Numer$(3,5),                                            \! Numero resolucion            
            Gr.Fis.Numer$(3,6),                                            \! Fecha inicial resolucion     
            Gr.Fis.Numer$(3,7)                                              ! Fecha final resolucion
       If TS.ER.RETURN <> -1 Then Begin																			! Error lectura
       	  X.cont% = X.cont% + 1 
       	  If X.cont% <= 3 Then Begin																				! Falla proceso
       	  	 Call U.Imprime("FALLA LECTURA FISCAL "+Str$(X.cont%),2100H)    ! Reporte de falla 
       	  	 Wait ; 500 
       	  	 GoTo Lectura.Fiscal																						! Procesa nuevamente lectura
       	  EndIf Else Begin																									! Falla total proceso lectura
       	  	 Call U.Imprime("FALLA PROCESO LECTURA FISCAL "+ERR,2100H)           ! Reporte de falla 
       	  EndIf
       EndIf																																!
End Sub  																																		! Fin lectura fiscal

Sub Actualiza.Num.Fiscal Public 																						! Grabacion numeracion fiscal
String X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$,                         \!
       X.LEC$, X.TIPO$, X.RESOL$, X.Aut2$, X.FECV$                          !
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%														!
Integer*2  X.Cont%

On Error GoTo FailWriteFis

 X.KEY$ = RIGHT$("0000"+TS.TERMINAL$,4)																			!
 X.KEY$ = PACK$(X.KEY$)																											!
 X.Lec$ = "C2 C4 3C5 C7 2C3 C4 3C5 C7 2C3 C4 3C5 C7 2C3"										! Formato registro 05Abr2024
 !Call Lectura.Consecutivo.Fiscal																						!
 X.cont% = 0 
 Grabacion.Fiscal:
 Gr.Fis.Numer$(1,4) = Pack$(Right$("0000000000"+Str$(Gr.Fis.FacNum%),10))   ! Actualiza consecutivo Electronico
 Gr.Fis.Numer$(2,4) = Pack$(Right$("0000000000"+Str$(Gr.Fis.NotaNum%),10))  ! Actualiza consecutivo Contingencia
 Gr.Fis.Numer$(3,4) = Pack$(Right$("0000000000"+Str$(Gr.Fis.NotaCrd%),10))  ! Actualiza consecutivo Ntas Credito
 TS.ER.RETURN = -1																									        ! Control Errores
 Write Form X.LEC$;#Gr.Fis.Sesion%  ;    				                 					 \! Graba numero de terminal
       X.KEY$,                                                             \! Numero de terminal          
       Gr.Fis.Numer$(1,1),                                                 \! Prefijo factura fiscal      
       Gr.Fis.Numer$(1,2),                                                 \! Rango factura inicial       
       Gr.Fis.Numer$(1,3),                                                 \! Rango factura final         
       Gr.Fis.Numer$(1,4),                                                 \! Numero actual factura       
       Gr.Fis.Numer$(1,5),                                                 \! Numero resolucion           
       Gr.Fis.Numer$(1,6),                                                 \! Fecha inicial resolucion    
       Gr.Fis.Numer$(1,7),                                                 \! Fecha final resolucion      
       Gr.Fis.Numer$(2,1),                                                 \! Prefijo factura contingencia
       Gr.Fis.Numer$(2,2),                                                 \! Rango factura inicial       
       Gr.Fis.Numer$(2,3),                                                 \! Rango factura final         
       Gr.Fis.Numer$(2,4),                                                 \! Numero actual factura       
       Gr.Fis.Numer$(2,5),                                                 \! Numero resolucion           
       Gr.Fis.Numer$(2,6),                                                 \! Fecha inicial resolucion    
       Gr.Fis.Numer$(2,7),                                                 \! Fecha final resolucion      
       Gr.Fis.Numer$(3,1),                                                 \! Prefijo notas credito       
       Gr.Fis.Numer$(3,2),                                                 \! Rango factura inicial       
       Gr.Fis.Numer$(3,3),                                                 \! Rango factura final         
       Gr.Fis.Numer$(3,4),                                                 \! Numero actual factura       
       Gr.Fis.Numer$(3,5),                                                 \! Numero resolucion           
       Gr.Fis.Numer$(3,6),                                                 \! Fecha inicial resolucion    
       Gr.Fis.Numer$(3,7)                                                   ! Fecha final resolucion      

FinWriteFis:
  Exit Sub 

FailWriteFis:
  X.cont% = X.cont% + 1 																									  ! Reintento proceso
  If X.cont% <= 3 Then Begin																				      	! Falla proceso
   	 Call U.Imprime("FALLA GRAB. FISCAL "+Str$(X.cont%)+ " " + ERR,2100H)		! Reporte de falla 
     Wait ; 500 																														!
     Resume Grabacion.Fiscal																								! Procesa nuevamente 
  EndIf Else Begin																									      	! Falla total Grabacion
     Call U.Imprime("FALLA TOTAL GRABACION FISCAL",2100H)           		  	! Reporte de falla 
     Gr.Fis.FailFis% = -1																										! Reporta bloqueo a terminal
  EndIf  
  Resume      

End Sub 

Sub Asignacion.Consecutivo.Fiscal                                  					! 
Integer*1 XI%, XJ%, Xdif%																										!
    Call Lectura.Consecutivo.Fiscal                                					! Lectura Consecutivos Archivo Controlador
 	  Gr.Fis.FacNum%  = Val(UnPack$(Gr.Fis.Numer$(1,4)))										  ! Numero de factura
 	  Gr.Fis.NotaNum% = Val(UnPack$(Gr.Fis.Numer$(2,4)))											! Numero de contingencia
 	  Gr.Fis.NotaCrd% = Val(UnPack$(Gr.Fis.Numer$(3,4)))										  ! Numero de nota credito
    Call U.IMPRIME("*** FACTURACION CARGADA ***",2100H)
    Call U.IMPRIME("FAC. FISCAL :"+STR$(Gr.Fis.FacNum%),2100H)
    Call U.IMPRIME("FAC. COTIN. :"+STR$(Gr.Fis.NotaNum%),2100H)
    Call U.IMPRIME("NOTAS CRED. :"+STR$(Gr.Fis.NotaCrd%),2100H)
    Exit Sub 
End Sub  

Sub Gr.Generacion.QR(Xopt%) Public
String Cufe$, DataQr$, UrlQr$, SndQr$, QRCode$, Firma$, Xtmp$
Integer*2 XI%, Xopt%
Cufe$ = Gr.Fis.Cufe$
QRCode$ = "https://catalogo-vpfe.dian.gov.co/document/searchqr?documentkey=" + \
          Gr.Fis.Cufe$
DataQR$ = "NumFac=" + Gr.Fis.DatQr$(1) + chr$(10)+                         \!
          "FecFac=" + Gr.Fis.DatQr$(2) + chr$(10)+                         \!
          "HorFac=" + Gr.Fis.DatQr$(3) + chr$(10)+                         \!
          "NitFac=" + Gr.Fis.DatQr$(4) + chr$(10)+                         \!
          "DocAdq=" + Gr.Fis.DatQr$(5) + chr$(10)+                         \!
          "ValFac=" + Gr.Fis.DatQr$(6) + chr$(10)+                         \!
          "ValIva=" + Gr.Fis.DatQr$(7) + chr$(10)+                         \!
          "ValOtroIm=" + Gr.Fis.DatQr$(8) + chr$(10)+                      \!
          "ValTolFac=" + Gr.Fis.DatQr$(9) + chr$(10)                        !
          
If Xopt% = 1 Then Begin																							  			! Si factura electronica
  !DataQR$ = DataQR$ + QRCode$
  DataQR$ = QRCode$
  xi% = 0 : XTMP$ = "CUFE:"
  For XI% = 1 To Len(Cufe$)																									! Imprime Cufe reportado
    Xtmp$ = Left$(Xtmp$+Mid$(Cufe$,XI%,38)+STRING$(38," "),38)							!
    Call U.IMPRIME(Xtmp$,6100H)																							!
    Xtmp$ = "" : XI% = XI% + 38																							!
  Next XI%																																	!
EndIf 																																			!
          
Call imprimeQR(DataQR$)

End Sub 

Function Format.Fecha$ Public
String Format.Fecha$, X.Meses$(1)
   Dim X.Meses$(12)
   X.Meses$(01) = "Ene"
   X.Meses$(02) = "Feb"
   X.Meses$(03) = "Mzo"
   X.Meses$(04) = "Abr"
   X.Meses$(05) = "May"
   X.Meses$(06) = "Jun"
   X.Meses$(07) = "Jul"
   X.Meses$(08) = "Ago"
   X.Meses$(09) = "Sep"
   X.Meses$(10) = "Oct"
   X.Meses$(11) = "Nov"
   X.Meses$(12) = "Dic"
   
   Format.Fecha$ = Right$(DATE$,2)  + "/"        + \! Dia
                  X.Meses$(Val(Mid$(DATE$,3,2))) + \! Mes 
                  "/" +"20"+Left$(DATE$,2)       + \! Año
                  " " + Left$(time$,2) + ":"     + \! Hora
                  Mid$(time$,3,2)      + ":"     + \! minuto
                  Mid$(time$,5,2)                   ! Segundo
                  
End Function 

Sub Carga.Parametros.Terminal																								! Carga validacion numeracion 
String X.Key$																																! 
    TS.ER.RETURN = -1 																											!
    If Asc.Ind.Open.Fiscal% = 0 Then Begin																	!
       Open "R::TF:NMFECO" KEYED RECL 98 As Gr.Fis.Sesion%  								! Abre control numeacion fiscal
       If TS.ER.RETURN <> -1 Then Begin
          Call U.Imprime("FALLA APERTURA NUMERACION FISCAL",6100H)				  !
          Call TSHIECET																											!
          Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL Z")					!
          Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")									!
          Stop																															!
       EndIf
       Asc.Ind.Open.Fiscal% = -1																						!
       TS.ER.RETURN = -1
    EndIf 																																	!
    If TS.ER.RETURN = -1 Then Begin                   								      ! Si apertura OK                
       Call Lectura.Consecutivo.Fiscal																			! Busqueda consecutivo terminal
       If TS.ER.RETURN <> -1 Then Begin																			! Falla busqueda terminal
          Call U.Imprime("Terminal No Definida con Numeracion",6100H)				!
          Call TSHIECET																											!
          Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL X")					!
          Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")									!
          Stop																															!
       EndIf 																																!
    EndIf Else Begin																												! Falla apertura archivo fiscal
          Call U.Imprime("Error Apertura Archivo Numeracion Fiscal",6100H)	!
          Call U.Imprime("Terminal No Definida con Numeracion",6100H)				!
          Call TSHIECET																											!
          Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL Y")					!
          Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")									!
          Stop																															!
    EndIf 																																	!

!-- Control fecha sistema 29Sep2022
If Val(Left$(date$,2)) <= 21 Then Begin																			!
	     Call U.Imprime("FECHA ERRADA DEL SISTEMA POS",6100H)			        		!
	     TS.TEMP1$ = "20" + DATE$ + " - " +TIME$                          		! Agrega Fecha y Hora 
	     Call U.Imprime("FECHA SISTEMA:"+TS.TEMP1$,6100H)			            		!
       Call VISOR.AND.BORRAR("FECHA DEL SISTEMA   ERRADA  /Borrar   ") 			!
       Call VISOR.AND.BORRAR("ERROR FECHA  LLAMAR MESA DE AYUDA 1234")  		!
       Stop																																	!
EndIf

End Sub 

Sub VALIDA.RANGO.FISCAL                                    									! Validacion consecutivos fiscales
String XAA$, Xdate$, Xdatev$, Xmsg$																					!
Integer*4 XDias%, Xaa%, Xmm%, Xdd%, Yaa%, Ymm%, Ydd%, Xini%, Xfin%, Xact%   !
Integer*2 XI%																																!
    yaa% = Val(Left$(Date$,2)) * 365                        								! Calcula numero dias por año
    ymm% = (Val(Mid$(Date$,3,2))-1) * 30                    								! Calcula numero dias por mes
    ydd% = Val(Right$(Date$,2))                             								! Calcula numero dias 
    XI% = 0
    For XI% = 1 To 3																												! Validacion 3 consecutivos por caja
      Xdate$  = Unpack$(Gr.Fis.Numer$(XI%,6))															  ! Fecha resolucion
      Xdatev$ = Unpack$(Gr.Fis.Numer$(XI%,7))															  ! Fecha vencimiento
      Xaa% = Val(Left$(Xdatev$,2)) * 365                      							! Calcula numero dias por año
      Xmm% = (Val(Mid$(Xdatev$,3,2))-1) * 30                  							! Calcula numero dias por mes
      Xdd% = Val(Right$(Xdatev$,2))                           							! Calcula numero dias 
      Xdias% = (Xaa%+Xmm%+Xdd%) - (Yaa%+Ymm%+Ydd%)            							! Diferencia en dias 
      If Xdias% < 0 Then Begin                               								! Expiro numeracion fiscal
    	  Call TSHIECET																												!
    	  If XI% = 1 Then Xmsg$ = "FECHA ELECTRONICA   FISCAL VENCIDA"        !
        If XI% = 2 Then Xmsg$ = "FECHA CONTINGENCIA  FISCAL VENCIDA"        !
        If XI% = 3 Then Xmsg$ = "FECHA NOTA CREDITO  FISCAL VENCIDA"        !
        Call VISOR.AND.BORRAR(XMSG$)																				!
        Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")										!
        Stop																																!
      EndIf																																	!
      If  Xdias% <= 15 Then Begin 																					! 15 dias para finalizar 
    	   Call TSHIECET																											!
    	   Xdate$ = "20"+Left$(Xdatev$,2)+"/"+Mid$(Xdatev$,3,2)+             \!
    	            "/"+Right$(Xdatev$,2)																			!
         Call VISOR.AND.BORRAR("FECHA FISCAL PROXIMAA VENCER "+XDate$)			!
         Call U.Imprime("FECHA FISCAL PROXIMA A VENCER",2100H)							!
         Call U.Imprime("VENCIMIENTO :"+Xdate$,2100H)												!
      EndIf 
      Xact% = Val(UnPack$(Gr.Fis.Numer$(XI%,4)))														! Numeracion actual
      Xini% = Val(UnPack$(Gr.Fis.Numer$(XI%,2)))														! Numeracion inicial
      Xfin% = Val(UnPack$(Gr.Fis.Numer$(XI%,3)))														! Numeracion final
      If Xact% <> 0 Then Begin																							! Numero actual
       If (Xfin% - Xact%) <= 10000 Then Begin 															! terminando facturas
     	   If XI% = 1 Then Xmsg$ = "CONSECUTIVO ELECTR. PROXIMO TERMINAR"     !
         If XI% = 2 Then Xmsg$ = "CONSECUTIVO CONTIN. PROXIMO TERMINAR"     !
         If XI% = 3 Then Xmsg$ = "CONSECUTIVO NT.CRD. PROXIMO TERMINAR"     !
         Call VISOR.AND.BORRAR(XMSG$)                                       !
         Call VISOR.AND.BORRAR("QUEDAN SOLO :"+Str$(Xfin% - Xact%))         ! 
         Call U.Imprime(Xmsg$,2100H)																				!
         Call U.Imprime("QUEDAN SOLO :"+Str$(Xfin% - Xact%),2100H)					!
       Endif  																															!
       If (Xfin% - Xact%) <= 0 Then Begin 																	! Fin numeracion
         Call VISOR.AND.BORRAR("CONSECUTIVO FISCAL  FINALIZADO ")					  !
         Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")									!
         Stop																																!
       Endif  																															!
      EndIf																																	!
    Next XI%																																!
    
!-- Control fecha sistema 29Sep2022
    If Val(Left$(date$,2)) <= 21 Then Begin
	     Call U.Imprime("FECHA ERRADA DEL SISTEMA POS",6100H)			        		!
	     TS.TEMP1$ = "20" + DATE$ + " - " +TIME$                          		! Agrega Fecha y Hora 
	     Call U.Imprime("FECHA SISTEMA:"+TS.TEMP1$,6100H)			            		!
       Call VISOR.AND.BORRAR("FECHA DEL SISTEMA   ERRADA  /Borrar   ") 			!
       Call VISOR.AND.BORRAR("ERROR FECHA  LLAMAR MESA DE AYUDA     ")  		!
       Stop
    EndIf    
End Sub 

!-- Consumo servicios Carvajal - FECO
!-- Retorna 1 - Factura Electronica
!-- Retorna 0 - Factura contingencia no comunicacion servicio carvajal
!-- Retorna 2 - Factura contingencia TimeOut servicio carvajal
!-- Retorna 3 - Factura contingencia Falla servicio DIAN (Pendiente)**
Function ACCESO.NUM.DIAN																										! Valida servicios dian
Integer*1 ACCESO.NUM.DIAN
String    Xtrx$, Xsnd$, Xrta$, Xfin$, Xtrama$, Xtemp4$, Xbuffer$            !
String    XQLin$, XQItm$
Gr.Fis.TrxId$ = ""                  																			  ! TransactionID Carvajal
Gr.Fis.Cufe$  = ""                     																	    ! CUFE transaccion
TS.TEMP1$ = LISTA.TRX.DIAN																									! Detalle venta para la DIAN
Gr.Fis.Nlin$ = Str$(TS.TEMP5I4)																							! # Lineas 
Gr.Fis.NItm$ = Str$(TS.TEMP4I4)																							! # Itms vendidos
TS.TEMP2$ = TrimRight$(Gr.Lcl.Name$) 																				! Nombre
TS.TEMP5$ = TrimRight$(Gr.Lcl.Surnames$)																		! Apellidos
TS.TEMP3$ = TrimRight$(Gr.Lcl.eMail$)																				! Correo
TS.TEMP4$ = TrimRight$(Gr.Lcl.Phone$)																				! Celular
TS.TEMP1$ = TS.TEMP1$                 + ";" + 														 \! Data de la compra
            Gr.Lcl.IdClte$            + ":" +                              \! Tipo de cliente
            Str$(Val(Gr.Lcl.Clte$))   + ":" + 													   \! CC Nit Cliente
            TS.TEMP2$                 + ":" +                              \! Nombre 
            TS.TEMP5$                 + ":" +                              \! Apellidos
            TS.TEMP3$                 + ":" +                              \! Correo electronico
            TS.TEMP4$                 + ";"                                 ! Telefono del cliente
TS.TEMP2$ = Str$(Gr.Fis.FacNum%)																						! Nro Fiscal
TS.TEMP2$ = Str$(Val(TS.TEMP2$) + 1)																				! Numero fiscal asignado
TS.TEMP1$ = TS.TEMP1$ + Gr.Fis.Numer$(1,1) + ":" +                         \! Prefijo factura fiscal
            TS.TEMP2$ + ":" + Unpack$(Gr.Fis.Numer$(1,5)) + ":" +          \! consecutivo y resolucion
            UnPack$(Gr.Fis.Numer$(1,2)) + ":" +                            \! Rango inicial
            UnPack$(Gr.Fis.Numer$(1,3)) + ":" +                            \! Rango factura final             
            Unpack$(Gr.Fis.Numer$(1,6)) + ":" +                            \! Fecha inicial resolucion        
            UnPack$(Gr.Fis.Numer$(1,7)) + ";"                               ! Fecha final resolucion

 Asc.Pay.Impr% = 1																													! Toma numero trx correcta de cierre
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 XTemp4$ = Armar.Trama.Msg("21","80",TS.TEMP1$,"00","0001","123456")        ! Armar trama MSG CC 
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora rta del requerimiento
 Call FECO.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                      		! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)																			        ! Valida rta entregada
 
 If Xtemp4$ <> "00" Then Begin 																							! Falla de Comunicaciones TRAMA CORTA 
    ACCESO.NUM.DIAN = 3																											! Genera Factura Contingencia
    Exit Function 
 EndIf																																			!
 XTEMP4$ = Mid$(XTrama$,12,2)	           																    ! Valida rta entregada
 If XTemp4$ = "01" Then Begin 																					    ! Falla Aut. Dian 
    ACCESO.NUM.DIAN = 3																											! Genera Factura Contingencia y Electronica
    Exit Function 
 EndIf
 If XTemp4$ <> "00" Then Begin 																					    ! Falla Aut. no controlada
    !ACCESO.NUM.DIAN = 2																											! Genera Factura Contingencia
    
    ACCESO.NUM.DIAN = 3																											! Se reporta Sin importar rta dian queme los 2 consecutivos 11Abr2025 - GR
    Exit Function 
 EndIf
 Gr.Fis.TrxId$ = Mid$(XTrama$,54,40)																				! TransactionID Carvajal
 Gr.Fis.Cufe$  = Mid$(XTrama$,94,120)																				! CUFE transaccion
 Gr.Fis.TrxId$ = TrimRight$(Gr.Fis.TrxId$)																	! Ajusta data
 Gr.Fis.Cufe$  = TrimRight$(Gr.Fis.Cufe$)																		! Ajusta data
 ACCESO.NUM.DIAN = 1																												! Factura Electronica
	 
End Function 																																! fin servicios dian

Function NumMesFisco(fec1$, fec2$)																					! Numero de meses
String NumMesFisco																													!
Integer*2 AA1,MM1,DD1,AA2,MM2,DD2, AAF, MMF																	!
Integer*2 meses																															!
String    fec1$, fec2$																											!
AA1 = Val(LEFT$(FEC1$,2))																										! Año inicial
MM1 = Val(MID$(FEC1$,3,2))																									! Mes Inicial
AA2 = Val(LEFT$(FEC2$,2))																										! Año final
MM2 = Val(MID$(FEC2$,3,2))																									! Mes Final
AAF = AA2 - AA1																															!
If AAF > 0 THEN MESES = AAF * 12 ELSE MESES = 0       											! MESES POR AÑOS
MMF = MM2 - MM1																															!
If MMF < 0 Then                                                            \! Meses adicionales
	 MMF = (12-MM1) + MM2																											!
MESES = MESES + MMF																													! Total meses
NumMesFisco = Str$(MESES)																										! Retorna numero de meses
End Function 																																!

Sub PROCESA.FECO.FISCAL																											! Genera numero fiscal
Integer*1 Xproc%																														!
Integer*4 XCact%, XEact%, XNact%																						!
String Xpref$, Xini$, Xfin$, XfAut$, XfVen$, Xresl$, Xmsg$, Xrng$, Xvig$    !
String Xtpf$, Xfiscal$, Xtmp1$, Xtmp2$, Xbuff$, Xfprc$, Xmeses$							!
  If Gr.Mnf.Fac$ <> "" Then Exit Sub																			  ! Si MF 
!  Call Lectura.Consecutivo.Fiscal																					! Lectura numeracion terminal

  TS.TEMP2$ = Format.Fecha$                                                 ! Formateo fecha registro
  Gr.Fis.DatQr$(2) = Mid$(TS.TEMP2$,1,11)  																  ! fecha del proceso  
  Gr.Fis.DatQr$(3) = Mid$(TS.TEMP2$,12,9)  																  ! Hora del proceso
  Xfprc$ = TS.TEMP2$																												!
  Xproc% = ACCESO.NUM.DIAN																									! Procesa facturacion DIAN
  XEact% = 0 : XNact% = 0 : XCact% = 0																			! Init variables
!-- Generacion factura electronica  
  If Xproc% = 1 Then Begin																									! Si FECO Electronica
     Xpref$ = TrimRight$(Gr.Fis.Numer$(1,1))																! Prefijo factura fiscal ajustado
     Xini$  = UnPack$(Gr.Fis.Numer$(1,2))																	  ! Nro Inicial
     Xfin$  = UnPack$(Gr.Fis.Numer$(1,3))																	  ! Nro final
     XfAut$ = UnPack$(Gr.Fis.Numer$(1,6))																	  ! Fecha inicial
     XfVen$ = UnPack$(Gr.Fis.Numer$(1,7))																	  ! Fecha final 
     XResl$ = UnPack$(Gr.Fis.Numer$(1,5))																	  ! Nro Resolucion
     Xmeses$ = NumMesFisco(XfAut$, XfVen$)																	! Nro meses 
     XEact% = Gr.Fis.FacNum%																								! Factur Actual Electronica
     XEact% = XEact% + 1																										! Aumenta consecutivo factura Electronica
     XCact% = 0 : XNact% = 0																								! No mueve otros consecutivos
     Xrng$ = "RANGO: "+Xpref$+"-"+Right$(Xini$,9)+" - "+Xpref$+"-" +       \! Rango numeracion
             Right$(Xfin$,9)																								!
     Xrng$  = Left$(Xrng$ + STRING$(38," "),38)                             ! Ajuste impresion
     XResl$ = "RESOL. "+XResl$+" DE "+"20"+Left$(XfAut$,2)+"/"+            \!
              Mid$(Xfaut$,3,2)+"/"+Right$(Xfaut$,2)													!
     XResl$ = Left$(XResl$ + STRING$(38," "),38)                            ! Ajuste impresion
     !Xvig$  = "VIGENCIA RESOL.    HASTA "+"20"+Left$(XfVen$,2)+"/"+        \!
     !         Mid$(XfVen$,3,2)+"/"+Right$(XfVen$,2)													!
     Xvig$ = "VIGENCIA "+XMESES$+" MESES"
     Xvig$  = Left$(Xvig$ + STRING$(38," "),38)                             ! Ajuste impresion
     Xtpf$ = "FACTURA ELECTRONICA DE VENTA "																!
     Xtpf$ = Left$(Xtpf$ + STRING$(38," "),38)                              ! Ajuste impresion
     Xbuff$ = Pack$("01")                                                + \! Factura Electronica
              ":" + "00"														     								 + \! Prefijo Contingencia         data5
              ":" + Pack$("00")                                          + \! Factura generada             data4
              ":" + Pack$("00")          														     + \! Nro Inicial                  data5
              ":" + Pack$("00")          														     + \! Nro final                    data6
              ":" + Pack$("00")          														     + \! Nro Resolucion               data7
              ":" + Pack$("00")          														     + \! Fecha inicial                data8
              ":" + Pack$("00")          														     + \! Fecha final                  data9
              ":" + (Xpref$)    														 						 + \! Prefijo factura electronica    	Asc
              ":" + Pack$(Str$(XEact%))                                  + \! Factura generada								PD
              ":" + (Gr.Fis.Numer$(1,2))														 		 + \! Nro Inicial                  		PD
              ":" + (Gr.Fis.Numer$(1,3))														 		 + \! Nro final      									PD
              ":" + (Gr.Fis.Numer$(1,5))														     + \! Nro Resolucion 									PD
              ":" + (Gr.Fis.Numer$(1,6))														     + \! Fecha inicial  									PD
              ":" + (Gr.Fis.Numer$(1,7))														     + \! Fecha final 										PD
              ":" + Pack$("00")                                             !
     Call Grabacion.Cadena.Usuario2("65",Xbuff$)                            ! Datos Factura Contingencia
     Gr.Fis.DatQr$(1) = Xpref$ + Right$("000000000"+Str$(XEact%),9)					! prefijo factura y # factura
  EndIf																																			!
!-- Generacion factura contingencia y Electronica 
  If Xproc% =  3  Then Begin																								! Si FECO CONTINGENCIA
     Xpref$ = TrimRight$(Gr.Fis.Numer$(2,1))																! Prefijo factura fiscal ajustado
     Xini$  = UnPack$(Gr.Fis.Numer$(2,2))																	  ! Nro Inicial
     Xfin$  = UnPack$(Gr.Fis.Numer$(2,3))																	  ! Nro final
     XfAut$ = UnPack$(Gr.Fis.Numer$(2,6))																	  ! Fecha inicial
     XfVen$ = UnPack$(Gr.Fis.Numer$(2,7))																	  ! Fecha final 
     XResl$ = UnPack$(Gr.Fis.Numer$(2,5))																	  ! Nro Resolucion
     Xmeses$ = NumMesFisco(XfAut$, XfVen$)																	! Nro meses 
     XCact% = Gr.Fis.NotaNum%																								! Nro Actual Contingencia
     XEact% = Gr.Fis.FacNum%																								! Factur Actual Electronica
     XCact% = XCact% + 1																										! Aumenta consecutivo factura contingencia
     XEact% = XEact% + 1																										! Aumenta consecutivo factura Electronica
     XNact% = 0																															! No mueve otros consecutivos
     Xrng$ = "RANGO: "+Xpref$+"-"+Right$(Xini$,9)+" - "+Xpref$+"-" +       \! Rango numeracion
             Right$(Xfin$,9)																								!
     Xrng$  = Left$(Xrng$ + STRING$(38," "),38)                             ! Ajuste impresion
     XResl$ = "RESOL. "+XResl$+" DE "+"20"+Left$(XfAut$,2)+"/"+            \!
              Mid$(Xfaut$,3,2)+"/"+Right$(Xfaut$,2)													!
     XResl$ = Left$(XResl$ + STRING$(38," "),38)                            ! Ajuste impresion
!     Xvig$  = "VIGENCIA RESOL.    HASTA "+"20"+Left$(XfVen$,2)+"/"+       \!
!              Mid$(XfVen$,3,2)+"/"+Right$(XfVen$,2)												!
     Xvig$ = "VIGENCIA "+XMESES$+" MESES"
     Xvig$  = Left$(Xvig$ + STRING$(38," "),38)                             ! Ajuste impresion
     Xtpf$ = "FACTURA DE VENTA TALONARIO O DE PAPEL"                        !
     Xtpf$ = Left$(Xtpf$ + STRING$(38," "),38)                              ! Ajuste impresion
     Xbuff$ = Pack$("03")                                                + \! Factura contingencia         data2 
              ":" + (Xpref$)    														 						 + \! Prefijo factura electronica  Asc
              ":" + Pack$(Str$(XCact%))                                  + \! Factura generada             data4
              ":" + (Gr.Fis.Numer$(2,2))														     + \! Nro Inicial                  data5
              ":" + (Gr.Fis.Numer$(2,3))														     + \! Nro final                    data6
              ":" + (Gr.Fis.Numer$(2,5))														     + \! Nro Resolucion               data7
              ":" + (Gr.Fis.Numer$(2,6))														     + \! Fecha inicial                data8
              ":" + (Gr.Fis.Numer$(2,7))														     + \! Fecha final                  data9
              ":" + (Gr.Fis.Numer$(1,1))														     + \! Prefijo Electrinica          data5 Asc
              ":" + Pack$(Str$(XEact%))                                  + \! Factura generada             data4
              ":" + (Gr.Fis.Numer$(1,2))														     + \! Nro Inicial                  data5
              ":" + (Gr.Fis.Numer$(1,3))														     + \! Nro final                    data6
              ":" + (Gr.Fis.Numer$(1,5))														     + \! Nro Resolucion               data7
              ":" + (Gr.Fis.Numer$(1,6))														     + \! Fecha inicial                data8
              ":" + (Gr.Fis.Numer$(1,7))														     + \! Fecha final                  data9
              ":" + Pack$("00")                                             !
     Call Grabacion.Cadena.Usuario2("65",Xbuff$)                            ! Datos Factura Contingencia
     Gr.Fis.DatQr$(1) = Xpref$ + Right$("000000000"+Str$(XCact%),9)					! prefijo factura y # factura
  EndIf																																			!

!-- Generacion factura contingencia               
  If Xproc% =  2 Then Begin																									! Si FECO CONTINGENCIA - falla DIAN
     Xpref$ = TrimRight$(Gr.Fis.Numer$(2,1))																! Prefijo factura fiscal ajustado
     Xini$  = UnPack$(Gr.Fis.Numer$(2,2))																	  ! Nro Inicial
     Xfin$  = UnPack$(Gr.Fis.Numer$(2,3))																	  ! Nro final
     XfAut$ = UnPack$(Gr.Fis.Numer$(2,6))																	  ! Fecha inicial
     XfVen$ = UnPack$(Gr.Fis.Numer$(2,7))																	  ! Fecha final 
     XResl$ = UnPack$(Gr.Fis.Numer$(2,5))																	  ! Nro Resolucion
     Xmeses$ = NumMesFisco(XfAut$, XfVen$)																	! Nro meses 
     XCact% = Gr.Fis.NotaNum%																								! Nro Actual Contingencia
     XEact% = Gr.Fis.FacNum%																								! Factur Actual Electronica
     XCact% = XCact% + 1																										! Aumenta consecutivo factura contingencia
     XNact% = 0	: XEact% = 0																								! No mueve otros consecutivos
     
     Xrng$ = "RANGO: "+Xpref$+"-"+Right$(Xini$,9)+" - "+Xpref$+"-" +       \! Rango numeracion
             Right$(Xfin$,9)																								!
     Xrng$  = Left$(Xrng$ + STRING$(38," "),38)                             ! Ajuste impresion
     XResl$ = "RESOL. "+XResl$+" DE "+"20"+Left$(XfAut$,2)+"/"+            \!
              Mid$(Xfaut$,3,2)+"/"+Right$(Xfaut$,2)													!
     XResl$ = Left$(XResl$ + STRING$(38," "),38)                            ! Ajuste impresion
!     Xvig$  = "VIGENCIA RESOL.    HASTA "+"20"+Left$(XfVen$,2)+"/"+       \!
!              Mid$(XfVen$,3,2)+"/"+Right$(XfVen$,2)												!
     Xvig$ = "VIGENCIA "+XMESES$+" MESES"
     Xvig$  = Left$(Xvig$ + STRING$(38," "),38)                             ! Ajuste impresion
     Xtpf$ = "FACTURA DE VENTA TALONARIO O DE PAPEL"                        !
     Xtpf$ = Left$(Xtpf$ + STRING$(38," "),38)                              ! Ajuste impresion
     Xbuff$ = Pack$("02")                                                + \! Factura contingencia         data2 
              ":" + (Xpref$)    														 						 + \! Prefijo factura electronica  Asc
              ":" + Pack$(Str$(XCact%))                                  + \! Factura generada             data4
              ":" + (Gr.Fis.Numer$(2,2))														     + \! Nro Inicial                  data5
              ":" + (Gr.Fis.Numer$(2,3))														     + \! Nro final                    data6
              ":" + (Gr.Fis.Numer$(2,5))														     + \! Nro Resolucion               data7
              ":" + (Gr.Fis.Numer$(2,6))														     + \! Fecha inicial                data8
              ":" + (Gr.Fis.Numer$(2,7))														     + \! Fecha final                  data9
              ":" + "00"														     								 + \! Prefijo Electrinica          data5
              ":" + Pack$("00")                                          + \! Factura generada             data4
              ":" + Pack$("00")        														       + \! Nro Inicial                  data5
              ":" + Pack$("00")          														     + \! Nro final                    data6
              ":" + Pack$("00")          														     + \! Nro Resolucion               data7
              ":" + Pack$("00")          														     + \! Fecha inicial                data8
              ":" + Pack$("00")          														     + \! Fecha final                  data9
              ":" + Pack$("00")                                             !
     Call Grabacion.Cadena.Usuario2("65",Xbuff$)                            ! Datos Factura Contingencia
     Gr.Fis.DatQr$(1) = Xpref$ + Right$("000000000"+Str$(XCact%),9)					! prefijo factura y # factura
  EndIf																																			!
  
  TS.TEMP1$ = Right$("          "+Gr.Fis.Nlin$,10)													! # de lineas
  TS.TEMP2$ = Left$("NUMERO DE LINEAS   :"+String$(28," "),28) + TS.TEMP1$	! 
  Call U.IMPRIME(TS.TEMP2$,6100H)                                         	!
  TS.TEMP1$ = Right$("          "+Gr.Fis.NItm$,10)													! # de items
  TS.TEMP2$ = Left$("NUMERO DE PRODUCTOS:"+String$(28," "),28) + TS.TEMP1$	! 
  Call U.IMPRIME(TS.TEMP2$,6100H)                                        		!
  Call U.IMPRIME("--------------------------------------",6100H)						! SEPARADOR                         
  TS.TEMP1$ = "CLIENTE:"+Gr.Lcl.Clte$																				! CLIENTE FISCAL COMPRA               
  TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)													! AJSUTE DATO                         
  Call U.IMPRIME(TS.TEMP1$,6100H)																						!                                     
  TS.TEMP1$ = "NOMBRE :"+TrimRight$(Gr.Lcl.Name$)+ " " +                   \!
              TrimRight$(Gr.Lcl.Surnames$)             					            ! Nombre del cliente
  TS.TEMP1$ = Ucase$(TS.TEMP1$)																							! Nombre a Mayusculas
  TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)													!
  Call U.IMPRIME(TS.TEMP1$,6100H)																						!
  If Xproc% =  1 Then Begin																									! Factura Electronica 
     TS.TEMP1$ = Right$("000000000"+Str$(XEact%),9)                         ! Consecutivo contingencia
     TS.TEMP2$ = Xpref$+"-"+TS.TEMP1$                                       ! Dato de consecutivo asignado
     Xfiscal$  = TS.TEMP2$																									! Dato fiscal 
  EndIf																																			!
  If Xproc% <>  1 Then Begin																								! Factura contingencia
     TS.TEMP1$ = Right$("000000000"+Str$(XCact%),9)                         ! Consecutivo contingencia
     TS.TEMP2$ = Xpref$+"-"+TS.TEMP1$                                       ! Dato de consecutivo asignado
     Xfiscal$  = TS.TEMP2$																									! Dato fiscal 
  EndIf																																			!
  If XEact% <> 0 Then Begin
   Gr.Fis.Numer$(1,4) = Pack$(Right$("0000000000"+Str$(XEact%),10))				  ! Actualiza consecutivo Electronico
   Gr.Fis.FacNum% = XEact%
  EndIf
  If XCact% <> 0 Then Begin
   Gr.Fis.Numer$(2,4) = Pack$(Right$("0000000000"+Str$(XCact%),10))					! Actualiza consecutivo Contingencia
   Gr.Fis.NotaNum% = XCact%
  EndIf
  If XNact% <> 0 Then Begin
   Gr.Fis.Numer$(3,4) = Pack$(Right$("0000000000"+Str$(XNact%),10))				  ! Actualiza consecutivo notas credito
  EndIf

!  Call Actualiza.Num.Fiscal																									! Actualiza numeracion fiscal

  TS.TEMP1$ = Right$(Xfiscal$,9)																						! Consecutivo generado
  Call U.IMPRIME(Xtpf$,6100H)																								! Tipo factura
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! Ajuste de dato
  Call U.Imprime(TS.TEMP2$,6100H)                                           ! Nro factura asignada
  Call U.IMPRIME(XResl$,6100H)            																	! Resolucion numeracion
  Call U.IMPRIME(Xrng$,6100H)            																		! Rango numeracion
  Call U.IMPRIME(Xvig$,6100H)            																		! Vigencia resolucion
  Gr.Fis.DatQr$(4) = "8903032085"          																  ! Nit empresa
  Gr.Fis.DatQr$(5) = Gr.Lcl.Clte$          																  ! Cliente
  TS.TEMP2$ = "FECHA DE PROCESO :" + Xfprc$                              		! 
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! 
  Call U.IMPRIME(TS.TEMP2$,6100H)
  TS.TEMP2$ = "FECHA VALIDACION :" + Xfprc$                              		! 
  TS.TEMP2$ = LEFT$(TS.TEMP2$ + STRING$(38," "),38)                         ! 
  Call U.IMPRIME(TS.TEMP2$,6100H)
  Call U.IMPRIME("--------------------------------------",6100H)            !
  TS.TEMP2$ = "TR+"                              +                         \! Arma Str Usuario con               
        Left$(Xfiscal$,4)+Pack$(TS.TEMP1$) + ":" +                 			   \! Dato fiscal trx 
        STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))     +                				 \! #Fact, Vr/Vta Neto                 
        ":" + Pack$("00") + ":" + Pack$("00")    +                				 \! y Vrs de Iva                       
        ":" + Pack$("00") + ":" + Pack$("00")    +                				 \!                                    
        ":" + Pack$("00") + ":" + Pack$("00")    +                				 \!                                    
        ":" + Pack$("00") + ":" + Pack$("00")                      					!                                    
  Call Grabacion.Cadena.Usuario("21",TS.TEMP2$)                             !
  Call Gr.Generacion.Qr(Xproc%)                                             ! Impresion representacion grafica
  
  Call Actualiza.Num.Fiscal																									! Actualiza numeracion fiscal
End Sub 

!-- Rutinas programa de terminal

Sub Parametros.Tienda.Fiscal																								! Parametros cabecera tiquete
TS.ER.RETURN = -1																														!
Open "R::TF:DATFIS" AS 95	UNLOCKED NOWRITE NODEL	                          ! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 Then Begin			                                      ! Error en la apertura
 Call TSHIECET											                                        ! Tono de Alerta
 Call Visores4690(1,"ERROR PARAMETROS","DATOS FISCAL TIENDA",1500,"L")		  ! Msg Alerta
 Call Visores4690(1,"MODULO FISCAL      ","NO OPERATIVO",1500,"L")				  ! Msg Alerta
 Call U.Imprime("ERROR CARGA MODULO FISCAL            ",6100H)					          ! 
 Call U.Imprime("MODULO FISCAL Ver. 1.0  ABRIL 05 2024",6100H)			        ! Rastro en SJ
 Asc.Proy.Fiscal%   = 00 									                                  ! Proyecto desactivado
 Call VISOR.AND.BORRAR("ERROR CARGA PARAMET.DATOS FISCAL TIENDA")
 Call VISORES4690(1,"TERMINAL BLOQUEADA","NO SE PERMITE VENTA",0,"L")
 Stop
EndIf Else Begin										                                        ! Apertura OK
 If End #95 Then UE.FIS.DATFINAL								  	                            ! Si es EOF
  While (1)											                                            ! Recorre archivo
    Read #95; Line TS.TEMP1$										                            ! Lectura registro
    If TS.TEMP1$ = "[NUMERACION FISCAL]" Then Begin						              ! Proyecto Numeracion Fiscal
     Read #95; Line TS.TEMP1$     								                          ! Lectura registro
     Gr.Fis.Empresa$   = Mid$(TS.TEMP1$,30,38)                				      ! Razon Social Empresa
     Read #95; Line TS.TEMP1$     								                          ! Lectura registro
     Gr.Fis.Nit$       = Mid$(TS.TEMP1$,30,38)                 				      ! Numero Identificacion Tributaria
     Read #95; Line TS.TEMP1$     								                          ! Lectura registro
     Gr.Fis.Direc$     = Mid$(TS.TEMP1$,30,38)                 				      ! Dirección almacen
     Read #95; Line TS.TEMP1$     								                          ! Lectura registro
     Gr.Fis.City$      = Mid$(TS.TEMP1$,30,38)                 				      ! Ciudad del almacen
    Endif
 Wend
 UE.FIS.DATFINAL:
     Close 95 
EndIf

End Sub 																																		!

Sub REVISAR.CONSEC.TLOG External																						! Revision ultimo consecutivo Tlog
End Sub 																																		! 

!--- Bloque principal user exits


Sub FISMTS02.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                        ! Proyecto activo

Gr.Fis.IndFac%    = 0								! Init variable
Gr.Fis.Tikntacr%  = 0
Gr.Fis.Doc%       = 0
Gr.Fis.Anulmov%   = 0
Gr.Fis.TermNc$    = ""
Gr.Fis.TipoNc$    = ""
Gr.Fis.ConsNc$    = ""
Gr.Fis.TipDocFis% = 0 
Gr.Fis.TrjVisa%   = 0 
Gr.Fis.BrillaTrx% = 0
Gr.Fis.TrxId$     = ""
Gr.Fis.Cufe$      = ""
Gr.Fis.ConMem% = 0
Gr.Fis.FisMem% = 0

If Gr.Fis.FailFis% = -1 Then Begin
   Gr.Fis.FailFis% = 0
!	 Call TSHIECET
!	 Call VISOR.AND.BORRAR("FALLA EN GRABACION  CONSECUTIVOS FISCALES")
!   Call U.IMPRIME("** FALLA NUMERACION FISCAL **",4101H)
!   Call U.IMPRIME("** FALLA NUMERACION FISCAL **",2100H)
!   Call U.IMPRIME("ULTIMOS CONSECUTIVOS REPORTADOS",4101H)
!   Call U.IMPRIME("ULTIMOS CONSECUTIVOS REPORTADOS",2100H)
!   Call U.IMPRIME("FAC. FISCAL :"+STR$(Gr.Fis.FacNum%),6100H)
!   Call U.IMPRIME("FAC. COTIN. :"+STR$(Gr.Fis.NotaNum%),6100H)
!   Call U.IMPRIME("NOTAS CRED. :"+STR$(Gr.Fis.NotaCrd%),6100H)
!   Call U.IMPRIME("REPORTAR DE INMEDIATO SOPORTE POS",4101H)
!   Call U.IMPRIME("REPORTAR DE INMEDIATO SOPORTE POS",2100H)
!   Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")					
!   Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")
!   Call U.IMPRIME(" ",4900H)
!   Call TSHIECET
!   Call TSHIECET
!   Call TSHIECET
!   Stop 

EndIf

!Call Lectura.Consecutivo.Fiscal

Call VALIDA.RANGO.FISCAL

EndIf 

End Sub  

Sub FISMTS07.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU07.011                                         **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
 
TS.ER.RETURN = -1										                                        ! Ctrl Errores
FIS.HTO% = 45            									                            			! No. Ses Hard Totals
FIS.H16% = 16315         									                            			! Offset de Grabacion en memoria
Gr.Fis.BrillaPre$ = "999"
Gr.Fis.BrillaPgo$ = "99"
Dim Gr.Fis.Numer$(3,8)																											! Matriz numeracion fiscal
Gr.Fis.FailFis% = 0																													! Control fallas proceso fiscal
Gr.Fis.ConMem% = 0
Gr.Fis.FisMem% = 0
Open "R::$SCNTRL" AS 94	UNLOCKED NOWRITE NODEL		                          ! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 Then Begin			                                      ! Error en la apertura
 Call TSHIECET											                                        ! Tono de Alerta
 Call Visores4690(1,"ERROR PARAMETROS","MODULO FISCAL DIAN ",1500,"L")		  ! Msg Alerta
 Call Visores4690(1,"MODULO FISCAL      ","NO OPERATIVO",1500,"L")				  ! Msg Alerta
 Call U.Imprime("ERROR CARGA MODULO FISCAL            ",6100H)					          ! 
 Call U.Imprime("MODULO FISCAL Ver. 1.0  ABRIL 05 2024",6100H)			        ! Rastro en SJ
 Asc.Proy.Fiscal%   = 00 									                                  ! Proyecto desactivado
 Call VISOR.AND.BORRAR("ERROR CARGA PARAMET.NUMERACION FISCAL")
 Call VISORES4690(1,"TERMINAL BLOQUEADA","NO SE PERMITE VENTA",0,"L")
 Stop
EndIf Else Begin										                                        ! Apertura OK
 If End #94 Then UE.FIS.FINAL								  	                            ! Si es EOF
  While (1)											                                            ! Recorre archivo
    Read #94; Line TS.TEMP1$										                            ! Lectura registro
    If TS.TEMP1$ = "[NUMERACION FISCAL]" Then Begin						              ! Proyecto Numeracion Fiscal
     Read #94; Line TS.TEMP1$							                                  ! Lectura registro
     Asc.Proy.Fiscal%   = Val(Mid$(TS.TEMP1$,30,2))    						          ! Proyecto activo
     Read #94; Line TS.TEMP1$     								                          ! Lectura registro
     Gr.Fis.Sesion%    = Val(Mid$(TS.TEMP1$,30,2))            				      ! Numero sesion temporal
     Call Parametros.Tienda.Fiscal
     Call Carga.Parametros.Terminal                                  		    ! Carga numeracion Fiscal Terminal
    Endif
    If TS.TEMP1$ = "[CONVENIO BRILLA]" Then Begin						              	! Proyecto recaudo brilla
       Read #94; Line TS.TEMP1$     								                        ! Lectura registro
       Gr.Fis.BrillaPre$ = Mid$(TS.TEMP1$,30,3)                				    	! Prefijo recibo brilla
       Read #94; Line TS.TEMP1$     								                        ! Lectura registro
       Gr.Fis.BrillaPgo$ = Mid$(TS.TEMP1$,30,2)                				    	! Tipo y variedad de pago
    EndIf																																		!
 Wend
 UE.FIS.FINAL:
     Close 94 
     Gr.Fis.Almacen1$ = Pack$(Right$("0000"+TS.STORE$,4))                   ! Ajusta dato numero de tienda
     Call Asignacion.Consecutivo.Fiscal																			! 
     Call VALIDA.RANGO.FISCAL																								! 
     Call REVISAR.CONSEC.TLOG																								! Validacion consecutivo fiscal
!-- Control de numeracion fiscal     
 
 If (Gr.Fis.PrefFis2$  =  Gr.Fis.Numer$(1,1)) And                          \! Si fiscal Electronica
    (Gr.Fis.ResolFis2$ =  Gr.Fis.Numer$(1,5)) Then                         \! Misma resolucion
 	   If Gr.Fis.FisMem% > Gr.Fis.FacNum% Then                               \! Si consecutivo superior en Tlog
 	   	  Gr.Fis.FacNum% = Gr.Fis.FisMem%								  										! asigna el de memoria fiscal

 If (Gr.Fis.PrefFis1$  =  Gr.Fis.Numer$(2,1)) And                          \! Si fiscal contingencia
    (Gr.Fis.ResolFis1$ =  Gr.Fis.Numer$(2,5)) Then                         \! Misma resolucion
 	   If Gr.Fis.ConMem% > Gr.Fis.NotaNum% Then                              \! Si consecutivo superior en Tlog
 	   	  Gr.Fis.NotaNum% = Gr.Fis.ConMem%								  									! asigna el de memoria contingencia

     If Asc.Proy.Fiscal% = -1 Then                                         \! Proyecto Activo
      Call U.IMPRIME("MODULO FISCAL FECOL    ON 11/Abr/2025",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO FISCAL FECOL   OFF 11/Abr/2025",2100H)         ! Msg Proyecto Cargado

EndIf

End Sub 


Sub FISMTS14.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU14.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Xtmp$

 If	TS.IO.KEYS(1) = 70 And TS.IO.MOTORKEY = 81 Then Begin                   ! Si anulacion total
    Call GrAnulacion.Total																									! Anulacion total trx
    Exit Sub 
 EndIf
 
 If (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin      						! Si presiona secuencia
  If (Val(TS.IO.DATA$(5)) = 20) Then Begin                      						! de tecleo para consulta version APP
  	 Call TSHIECET
     Call VISOR.AND.BORRAR("VERSION APP TERMINAL"+GRVERAPP$+" /Borrar")     !
     TS.IO.MOTORKEY = 73     																								! Inicializa secuencia
     Dim TS.IO.KEYS(10)                                          						! de tecleo y reporta
     Dim TS.IO.DATA$(10)                                         						! Error 
     Exit Sub 
  EndIf 																																		!
 EndIf																																			!
 
 If (TS.IO.KEYS(5) = 61) And (TS.IO.STATE = 10) Then Begin      						! Si presiona secuencia
  If (Val(TS.IO.DATA$(5)) = 77) Then Begin                      						! de tecleo para una
   If TS.INTRX Then Begin 
   	  Call VISOR.AND.BORRAR("PROCESO NO VALIDO   EN UNA TRX. BORRAR  ")
      TS.IO.MOTORKEY = 0     																								! Inicializa secuencia
      Dim TS.IO.KEYS(10)                                          					! de tecleo y reporta
      Dim TS.IO.DATA$(10)                                         					! Error 
      Exit Sub 
   EndIf 

!   Call VISORES4690(1,"INICIALIZANDO DATOS", "TERMINAL "+TS.TERMINAL$,1400,"L")           ! Msg de Alerta
!   Call VISOR.AND.BORRAR("INIT HARDTOTALS...  COMPLETADO. BORRAR  ")
!   Call VISOR.AND.BORRAR("TERMINAL DEBE SER   RECARGADA . BORRAR  ")
!   Call U.IMPRIME("HARD TOTALS TERMINAL INICIALIZADOS",6100H)
!   Call U.IMPRIME("FECHA:"+DATE$+" HORA:"+TIME$,4100H)
!   Stop 

  EndIf 
 EndIf 

!-- Add Registro forma pago tienda virtual 22 Jun 2021
If LEN(TS.IO.DATA$(2)) >= 22 And \																					! Si barra forma de pago
   Left$(TS.IO.DATA$(2),2) = "86" Then Begin                                ! tienda virtual
      If (TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1) Then Begin	  	        ! No se ha ingresado total en venta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 0 																							  ! 
         TS.GUIDANCE = 1020																							    ! Solicita total a la trx
         Exit Sub 																												  ! Sale del proceso
      EndIf 																															  !
     TS.TEMP1I1 = VALIDA.COMPRA.UVT																					! Si cumple parametros compra minima UVT
     If TS.TEMP1I1 = -1 Then Begin 	  																			! Si pasa validacion Feco
      TS.TEMP1$ = Right$(TS.IO.DATA$(2),10)                                 ! Numero de referencia
      TS.TEMP2$ = Mid$(TS.IO.DATA$(2),3,10)                                 ! Valor del pago
      TS.TEMP1I1 = 0
      If TS.IO.KEYS(1) = 70 Then TS.TEMP1I1 = -1														! Registra anulación
      Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
      TS.IO.PREV.MOTORKEY = 80
      TS.IO.DEVICE = 1 
      Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												      ! Init vectores de carga
      If TS.TEMP1I1 = -1 Then TS.IO.KEYS(1) = 70                            ! Si anulacion de pago 
      TS.IO.DATA$(3) = "5"                         													! Variedad 5 del efectivo
      TS.IO.KEYS(3)  = 78                                                   ! Tecla Slash
      TS.IO.DATA$(7) = Str$(Val(TS.TEMP2$))														      ! Valor a entregar
      TS.IO.DATA$(9) = Str$(Val(TS.TEMP1$)) 													      ! Numero de referencia
      TS.IO.KEYS(7)  = 91                                                   ! Grupo de pago efectivo
      TS.IO.MOTORKEY = TS.IO.KEYS(7)																	      ! Reporta a la tecla motora
     EndIf																																	! Fin validacion feco
EndIf    

!-- Add Registro domicilio tienda virtual 09Feb2022
If LEN(TS.IO.DATA$(2)) >= 12 And \												  								! Si barra domicilios
   Left$(TS.IO.DATA$(2),3) = "988" Then Begin                               ! tienda virtual
   Gr.Fis.DomiPrc$ = Mid$(TS.IO.DATA$(2),8,7)																! precio del domicilio
   TS.TEMP3$ = Mid$(TS.IO.DATA$(2),2,6)																			! Toma PLU
   TS.TEMP1I1 = 0
   If TS.IO.KEYS(1) = 70 Then TS.TEMP1I1 = -1																! Registra anulación
   Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
   TS.IO.STATE = 10
   TS.IO.PREV.MOTORKEY = 0
   Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												        ! Init vectores de carga
   If TS.TEMP1I1 = -1 Then TS.IO.KEYS(1) = 70                               ! Si anulacion de pago 
   TS.IO.KEYS(2)  = 80																											! Tecla Intro
   TS.IO.DATA$(2) = TS.TEMP3$																					      ! Plu domicilios
   TS.IO.DEVICE   = 1
   TS.IO.MOTORKEY = 80
   Gr.ChgPrc.Proc% = -1
   Gr.Fis.Domictv% = -1
EndIf																																				! Fin registro domicilios

!-- Add Registro forma pago tarjeta crd Comfandi
If TS.IO.MOTORKEY = 214 Then Begin                                					! Tarjeta CRD Comfandi
   If (TS.TRX.STATUS <> 1 And TS.PROCEDURE = -1) Then Begin	  	        		! No se ha ingresado total en venta
         Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    ! Inicializa secuencia de teclado
         TS.IO.MOTORKEY = 0 																							  ! 
         TS.GUIDANCE = 1020																							    ! Solicita total a la trx
         Exit Sub 																												  ! Sale del proceso
   EndIf 																															  		!
   Dim TS.IO.PREV.DATA$(10) : Dim TS.IO.PREV.KEYS(10)
   TS.IO.PREV.MOTORKEY = 80
   TS.IO.DEVICE = 1 
	 If TS.IO.DATA$(5) = "" Then 																						 \! Si ingreso monto de pago
	 	  Xtmp$ = STR$(TS.BALDUE(0)) Else 																	   \! o es total de compra
	 	  Xtmp$ = TS.IO.DATA$(5)

   Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 												      	! Init vectores de carga
   TS.IO.MOTORKEY = 176
   TS.IO.DATA$(7) = Xtmp$
   Gr.Fis.TrjVisa% = -1

EndIf    

!-- Add Registro convenio BRILLA

Call Aplica.Pago.Brilla        																							! Aplica pago brilla

End Sub 

Sub FISMTS20.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                        ! Proyecto activo

If MATCH(TS.SDESC$(59),TS.PRTBUF$,1) > 0 Then Begin                 				! Mensaje de anulacion total
   Gr.Fis.Doc% = -1                                                 				! Ctrl trx anuladas
EndIf

If MATCH("SUSPEND",TS.PRTBUF$,1) > 0 Then Begin 							      				! Si mensaje de Suspension Total
  TS.PRTBUF$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + TS.PRTBUF$					  				! Cambio tipo de letra 
EndIf

If TS.TRAINING Then Begin                                                   ! Si no en entrenamiento  
 If TS.INTRX Then                                                          \! Si esta en una compra   
   If TS.LINETYPE = 6 And                                                  \! Store Data y Transacc   
      TS.LINEDATA = 1 Then Begin                                            ! Fecha, Hora, etc        
       If TS.TENDERED (0) <> 0 Or                                          \! Si hay pagos            
          TS.TRX.STATUS <> 100 Then Begin                                   ! y NO hay VOID en curso  
          	TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TIQUETE EN ENTRENAMIENTO"
          	TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
          	Call U.Imprime(TS.TEMP1$,6100H)
          	TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "TRANSACCION SIN VALIDEZ"
          	TS.TEMP1$ = LEFT$(TS.TEMP1$ + STRING$(38," "),38)
          	Call U.Imprime(TS.TEMP1$,6100H)
          	
       EndIf
   EndIf
EndIf 

If Not TS.TRAINING Then Begin                                               ! Si no en entrenamiento  
 If TS.INTRX Then                                                          \! Si esta en una compra   
   If TS.LINETYPE = 6 And                                                  \! Store Data y Transacc   
      TS.LINEDATA = 1 Then Begin                                            ! Fecha, Hora, etc        
       If TS.TENDERED (0) <> 0 Or                                          \! Si hay pagos            
          TS.TRX.STATUS <> 100 Then Begin                                   ! y NO hay VOID en curso  
           If Gr.Fis.Doc% = -1 Then Begin	    
              Exit Sub
           EndIf
           Call TSHIECET
           !Call Visores4690(1,"GENERANDO FACTURA   ","ELECTRONICA ESPERE..",0,"L")
           
           Call Visores4690(1,"ESPERANDO RESPUESTA ","DESDE LA DIAN ESPERE",0,"L")
           Call PROCESA.FECO.FISCAL
       EndIf
   EndIf
EndIf

Call Confirma.Pago.Brilla																										! Confirma pago brilla

EndIf

End Sub 

Sub FISMTS53.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU53.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Xtmpp$

   If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "05" THEN BEGIN     	              ! Pago aplicado
      Asc.Tmp.Apun% = 3																											! Init apuntador
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! tipo y variedad de pago
      If Xtmpp$ = Gr.Fis.BrillaPgo$ Then Begin                              ! Registro pago brilla
         Gr.Fis.BrillaTrx% = -1																							! Pago en proceso
         Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                 ! valor del pago
         Gr.Fis.BrillaValr$ = Right$(String$(10,"0")+Xtmpp$,10)             ! Ajusta monto del recibo
         Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                 ! valor del pago
         Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                 ! numero de cuenta
         Gr.Fis.BrillaDoc$ = Right$(String$(24,"0")+Xtmpp$,24)              ! Ajusta numero de documento
      EndIf																																	!
   EndIf																																		!
   If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "06" THEN BEGIN     	              ! Pago anulado
      Asc.Tmp.Apun% = 3																											! Init apuntador
      Xtmpp$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)                    ! tipo y variedad de pago
      If Xtmpp$ = Gr.Fis.BrillaPgo$ Then Begin                              ! Registro pago brilla
         Gr.Fis.BrillaTrx% = 0 																							! Pago anulado 
      EndIf																																	!
   EndIf																																		!
   
End Sub 																																		! fin recuperacion de trx

Sub FISMTS56.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU56.011                                         **
!   **    Inclu¡r en :  EAMTSU56.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  Then Begin                                       	! Proyecto activo

Gr.Fis.IndFac% = 0																													! Init variable
Gr.Fis.Tikntacr% = 0
Gr.Fis.Doc% = 0
Gr.Fis.Anulmov% = 0
Gr.Fis.TermNc$  = ""
Gr.Fis.TipoNc$  = ""
Gr.Fis.ConsNc$  = ""
Gr.Fis.TipDocFis% = 0 
Gr.Fis.TrjVisa% = 0
Gr.Fis.BrillaTrx% = 0

EndIf

End Sub 

