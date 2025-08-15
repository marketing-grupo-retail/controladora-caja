!**************************************************
!Empresa       : CellStar de Colombia             *
!Programa      : PINMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio de 2006                    *
!Observaciones : Modulo Recargas en Linea         *
!**************************************************
! Modificaciones:                                 
! Mod 2006Ago02                           
! Se define rutina para diseño de tiquete de impresion, 
! para que este no sea quemado por desarrollo sino 
! parametrizable en el archivo PINLNMSG
! utilizando la siguiente convencion:
! &P --> Número del telefono
! &F --> Fecha del movimiento
! &V --> Valor de la recarga
! &C --> Codigo de cadena y almacen        
! &T --> Traid de la transaccion  
!---------------------------------------------------
! Se ajusta proceso de validacion al cierre de la trx, cuando
! se realiza el último pago. desarrollado por Oscar Valencia
! agosto 8 de 2006
!----------------------------------------------------
! Se ajusta el modulo para que permita el manejo y control
! de 5 prefijos por cada operador telefonico
! desarrollado el 29 de agosto 2006 OVS
!-----------------------------------------------------
! Se modifica la ruta de apertura de los archivos de 
! parametros al directorio /pines/rec segun requerimiento
! de CellStar, desarrollado el 7 de Sept 2006 OVS
!------------------------------------------------------
! Se ajusta el modulo para que la comunicacion entre el 
! autorizador en java y la aplicacion de caja, se realice 
! a  traves de pipes y disminuir la recarga en el disco.
! desarrollado para el Exito. OVS Abr 2007.
!----------------------------------------------------
! Se ajusta el modulo para manipular el estado de la 
! variable de control de anulacion de articulos por fuera
! de una venta, ajuste realizado para Sodimac
! Mzo 3 de 2008 - OVS
! TO.CANCELPREV   = -1 
!------------------------------------------------------
! Se ajusta el modulo para que en el archivo pinopera
! permita el manejo de 9 prefijos telefonicos.
! ajuste realizado el 10 Mzo de 2008 OVS
!-----------------------------------------------------
! Se ajusta el modulo para que calcule cantidades
! dependiendo de la unidad de medida minima de venta
! solo se ajusta este modulo para Sodimac
! ajuste realizado el 10 de abril de 2.008
!-----------------------------------------------------

%ENVIRON T

!Integer   Global SL.END         ! Numero Strings transaccion

Integer*2 Global PRT4610.ENABLE	! Tipo de impresora
INTEGER*1 Global TO.EBT         !IR93804 Used for electronic benefits

%INCLUDE EAMTSCVA.J86
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! 
%INCLUDE EAMTERMS.J86          ! 
%INCLUDE EAMITEMR.J86          ! Variables Eamitemr
%INCLUDE PINITEMR.011          ! Variables Eamitemr
%INCLUDE EAMTOPTS.J86	         ! Variables de la terminal options

%INCLUDE PINTSUVA.011	         ! Variables Modulo pines

%INCLUDE EAMTSXHC.J86          ! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          ! Rutinas de Assembler
%INCLUDE PINMSUSU.011          ! RUTINAS GENERICAS APLICACION


Sub TSHIECET EXTERNAL
End Sub

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub SPLIT.ITEMRECORD (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub Graba.Devol.Telefono(Xpos%)
Integer*4 Xpos%
String XKey$, Xvlr$, Xstr$
 TS.ER.RETURN = -1 
 Open "R::PINANREC" KEYED RECL 17 AS Ue.Pin.Sesion%            ! Abre archivo de Operadores
 If TS.ER.RETURN <> -1 Then Begin 
    Call PIN.IMPRIME("ANULACION NO ALMACENADA",4100H)
    XSTR$ = Pack$(TS.TERMINAL$)               +		      \! Nro de la terminal
        ":"+Pack$(Str$(Sl.Hd.Transnum))       +               \! Nro de la transaccion
        ":"+Pack$(Ue.Pin.Numeros$(Xpos%,0))   +               \! Numero del telefono
        ":"+Pack$(Ue.Pin.Numeros$(Xpos%,2))   +               \! Valor de la recarga fallida
        ":"+Pack$("00")                                        ! Filler                                       
    Call Pin.Cadena.Usuario2("20080515",XSTR$)                  ! Graba String usuario		  
    
    Exit Sub 
 EndIf
 Xkey$ = Right$("0000"+TS.TERMINAL$,4) + \																! Numero de terminal
         Right$("0000"+Str$(Sl.Hd.Transnum),4) + \                        ! Numero de transaccion
         Right$("0000000000"+Ue.Pin.Numeros$(Xpos%,0),10)                 ! Numero del telefono
 Xkey$ = Pack$(Xkey$)																											! Empaqueta la llave                             
 Xvlr$ = Right$("0000000000"+Ue.Pin.Numeros$(Xpos%,2),10)                 ! Numero del telefono
 Xvlr$ = Pack$(Xvlr$)																											! Valor de la recarga
 TS.ER.RETURN = -1 
 Write Form "C9 C5 C3";#Ue.Pin.Sesion%;XKEY$,                            \! Grabacion registro 
                       Xvlr$, Pack$(Date$)         
 If TS.ER.RETURN <> -1 Then Begin 
    Call Pin.Imprime("Error Grabacion Reverso",2100H)
    Call PIN.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                     STR$(TS.ERRF),2100H)
 EndIf                        
 Close Ue.Pin.Sesion%

End Sub 

Function Verificar.Linea
Integer*1 Verificar.Linea
String XR$

    Verificar.Linea = 1 
    Exit Function 
 
 Call Msg.Display("VALIDANDO AUTORIZADO")
 If TS.TRAINING Then Begin																						! En Entrenamiento
    Verificar.Linea = 1 
    Exit Function 
 EndIf 
 TS.ER.RETURN = -1 
 Open "R::PI:R" As Ue.Pin.IdPipe%                                     ! Abro el Pipe
 If TS.ER.RETURN <> -1 Then Begin 
    Call MSG.VISOR("AUTORIZADOR NO DISPONIBLE")
    Verificar.Linea = 0
    Exit Function  
 EndIf 
 Create "R::PI:P"+TS.TERMINAL$ As Ue.Pin.IdPipe2%                     ! Abro el Pipe de respuesta

 Verificar.Linea = 0
 XR$ = "P" + TS.TERMINAL$
 Call GRABA.PIPES(Ue.Pin.IdPipe%,"C4",XR$)   ! Graba requerimiento
 XR$ = LEER.PIPES(Ue.Pin.IdPipe2%,"C1")
 Verificar.Linea = Val(XR$)

 Close Ue.Pin.IdPipe% : Close Ue.Pin.IdPipe2%
 
End Function 

Function RECA.ITEM.RECARGA(XPLU$) Public		! Valida si articulo vendido es un pin
Integer*1 RECA.ITEM.RECARGA
Integer*4 XI%
String    Plu.Tmp$, XPLU$
RECA.ITEM.RECARGA = 0 
Plu.Tmp$ = Str$(VAL(XPLU$))																				          ! Codigo Ingresado

For XI% = 1 to Ue.Pin.Tarj% 																								! Busca en tarjetas cargadas
  If (Val(Plu.Tmp$) = Val(UE.PIN.EAN$(XI%,0))) OR                          \! Valida si codigo EAN o PLU
     (Val(Plu.Tmp$) = Val(UE.PIN.EAN$(XI%,2))) Then Begin                   ! esta en la lista
       Ue.Pin.Oper$ = UE.PIN.EAN$(XI%,1)                                    ! Codigo del operador
       RECA.ITEM.RECARGA = -1																								! Encontrado
!       Call DATOS.PINVTA(PLU.TMP$)																				  ! Busca Datos Producto
       
       XI% = Ue.Pin.Tarj% + 10                                              ! Sale del ciclo
  EndIf 
Next XI%

End Function										

Sub Save.TmpOper.Recarga
String XTMP$

If Ue.Pin.AnulItm% = 0 Then Begin 																				! Si es una venta  
   XTMP$ = Pack$(Ue.Pin.Numeros$(Ue.Pin.Posimp% ,0))        +            \! Nro de teléfono
	   ":"+Pack$(Ue.Pin.Numeros$(Ue.Pin.Posimp% ,2))    +                  \! Valor Recargado
	   ":"+Pack$(Ue.Pin.Numeros$(Ue.Pin.Posimp% ,1))    +                  \! Operador de recarga
     ":"+Pack$("00")                                                      ! Filler
   Call Pin.Cadena.Usuario2("20060722",XTMP$)                             ! Graba String usuario
   
EndIf 			
			
End Sub 

Sub Save.Oper.Recarga
String XTMP$
XTMP$ = Pack$(Ue.Pin.Phones$)     +				      \! Nro de teléfono      
        ":"+Pack$(Ue.Pin.Vlr$)    +                                   \! Valor Recargado      		  
        ":"+Pack$(Ue.Pin.Oper$)   +                                   \! Operador de recarga  		  
        ":"+Pack$(Ue.Pin.Traid$)  +                                   \! Traid de autorizacion		     			        
        ":"+Pack$("00")                                                ! Filler                                       
	Call Pin.Cadena.Usuario2("20060721",XTMP$)                              ! Graba String usuario		  
	
End Sub 

Sub Valida.Montos.Operador(XOPER$)
String XOPER$, XNAME$
Integer*4 XMIN%, XMAX%, XMUL%
Integer*2 XDIG%
String  XPRE1$, XPRE2$, XPRE3$, XPRE4$, XPRE5$, XPRE6$, XPRE7$, XPRE8$, XPRE9$
    Ue.Pin.Prefijos$ = ";"
    TS.ER.RETURN = -1 
    Open "R::PINOPERA" KEYED RECL 52 AS Ue.Pin.Sesion%                       ! Abre archivo de Operadores
    
    If TS.ER.RETURN = -1 Then Begin 
    	Ue.Pin.ErrOpen% = TS.ER.RETURN 
       XOPER$ = Str$(Val(XOPER$))                                            ! Ajusto dato
       XOPER$ = Pack$(Right$("0000"+XOPER$,4))															 ! Arma llave de busqueda
       TS.ER.RETURN = -1 
       Read Form "C2 C10 2I4 I1 I4 9C3";#Ue.Pin.Sesion% KEY XOPER$ ;        \! Busqueda de registro
        XOPER$,                                           		\! Codigo del operador
        XNAME$,                                           		\! Nombre del operador
        XMIN%,                                            		\! Valor minimo de venta
        XMAX%,                                            		\! Valor Maximo de venta
        XDIG%,                                            		\! Numero Max digitos celular
        XMUL%,                                              	        \! Valor en multiplos para venta
        XPRE1$, XPRE2$, XPRE3$, XPRE4$, XPRE5$, XPRE6$, XPRE7$, XPRE8$,     \! 
        XPRE9$
        
        
       If TS.ER.RETURN = -1 Then Begin 																			 ! Si lo encontro
          Ue.Pin.MntoMin% = XMIN%   																				 ! Retorna Montos de control
          Ue.Pin.MntoMax% = XMAX%																						 !
          Ue.Pin.MaxDig%  = XDig%                                            ! Numero maximo de digitos
          Ue.Pin.Multiplo%= Xmul%																					   ! Valor Multiplo
          Ue.Pin.Oper$    = Unpack$(XOPER$)																	 ! Operador 
          Ue.Pin.Prefijos$ = ";"

          If Val(XPRE1$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE1$ + ";"
             
          If Val(XPRE2$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE2$ + ";"
             
          If Val(XPRE3$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE3$ + ";"
             
          If Val(XPRE4$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE4$ + ";"
             
          If Val(XPRE5$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE5$ + ";"   

          If Val(XPRE6$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE6$ + ";"   

          If Val(XPRE7$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE7$ + ";"   

          If Val(XPRE8$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE8$ + ";"   

          If Val(XPRE9$) <> 0 Then \
             Ue.Pin.Prefijos$ = Ue.Pin.Prefijos$ + XPRE9$ + ";"   
             
                                                   
       EndIf Else Begin                                                      ! No existe registro
          Call msg.visor("OPERADOR "+Unpack$(XOPER$)+" NO EXISTE")
          Ue.Pin.MntoMin% = 0 																							 ! Retorna montos en cero
          Ue.Pin.MntoMax% = 0 	  																					 !
          Ue.Pin.MaxDig%  = 0                                                ! Numero maximo de digitos
          Ue.Pin.Multiplo%= 0    																					   ! Valor Multiplo
          XPRE1$ = "000" : XPRE2$ = "000"																		 ! Prefijos en cero
          XPRE3$ = "000" : XPRE4$ = "000" : XPRE5$ = "000"                   !
          XPRE6$ = "000" : XPRE7$ = "000" : XPRE8$ = "000"                   !
          XPRE9$ = "000" 																									   !
          Ue.Pin.Prefijos$ = ";" + XPRE1$ +                                 \!
                             ";" + XPRE2$ +                                 \!
                             ";" + XPRE3$ +                                 \!
                             ";" + XPRE4$ +                                 \!
                             ";" + XPRE5$ +                                 \!
                             ";" + XPRE6$ +                                 \!
                             ";" + XPRE7$ +                                 \!
                             ";" + XPRE8$ +                                 \!
                             ";" + XPRE9$ + ";"                              !                           
       EndIf 
       Close Ue.Pin.Sesion%																									 ! Cierra sesion 
    EndIf Else Begin  																											 ! Error de apertura
    	Ue.Pin.ErrOpen% = TS.ER.RETURN 
      !Call Pin.Imprime("Error Apertura PINOPERA.DAT",6100H)
      !Call msg.visor("Error Apertura PINOPERA.DAT")
      Ue.Pin.MntoMin% = 0 																									 ! Retorna montos en cero
      Ue.Pin.MntoMax% = 0 																									 !
      Ue.Pin.MaxDig%  = 0                                                    ! Numero maximo de digitos
      Ue.Pin.Multiplo%= 0       																					   ! Valor Multiplo
          XPRE1$ = "000" : XPRE2$ = "000"																		 ! Prefijos en cero
          XPRE3$ = "000" : XPRE4$ = "000" : XPRE5$ = "000"                   !
          XPRE6$ = "000" : XPRE7$ = "000" : XPRE8$ = "000"                   !
          XPRE9$ = "000" 																									   !
          Ue.Pin.Prefijos$ = ";" + XPRE1$ +                                 \!
                             ";" + XPRE2$ +                                 \!
                             ";" + XPRE3$ +                                 \!
                             ";" + XPRE4$ +                                 \!
                             ";" + XPRE5$ +                                 \!
                             ";" + XPRE6$ +                                 \!
                             ";" + XPRE7$ +                                 \!
                             ";" + XPRE8$ +                                 \!
                             ";" + XPRE9$ + ";"                              !                           
                             
      If (TS.STANDALONE) Then Begin                                          ! Si operacion fuera de linea
         Call Msg.Visor("VENTA NO PERMITIDA  FUERA DE LINEA")                !                             
      EndIf Else Begin 
      	 Call msg.visor("Error Apertura PINOPERA.DAT")
      EndIf 
    EndIf 

End Sub 

Sub Asigna.Telefono
Integer*1 Asigna.Telefono
Integer*4 XI%, XJ%, XK%, XPRICE%
String Xplu$
 If Ue.Pin.AnulItm% <> -1 Then Begin 					     ! Si es una venta
    Ue.Pin.TotIng% = Ue.Pin.TotIng% + 1					     ! Aumenta contador
    Ue.Pin.Numeros$(Ue.Pin.TotIng% ,0) = Ue.Pin.Phones$                      ! Asigna telefono
    Ue.Pin.Numeros$(Ue.Pin.TotIng% ,1) = Ue.Pin.Oper$                        ! Asigna Operador
    Ue.Pin.Numeros$(Ue.Pin.TotIng% ,2) = Ue.Pin.Vlr$                         ! Valor de la trx
    TS.TEMP2$ = Pack$(SL.IT.ITEMCODE$)                                       ! Item a consultar
    Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                          ! Lectura Datos Itemr
    Call SPLIT.ITEMRECORD( TS.TEMP1$ ) 			                     ! Entrega datos al eamitemr.j86
    If UnPack$(PIN.INDICAT2$) = "05" Then Begin 
       Xplu$ = UnPack$(PIN.SALEPRIC$)
    EndIf Else Xplu$ = SL.IT.ITEMCODE$
    Ue.Pin.Numeros$(Ue.Pin.TotIng% ,6) = Str$(Val(XPLU$))                    ! Plu registrado
    Ue.Pin.Posimp% = Ue.Pin.TotIng%                                          ! Asigna posicion para impresion
 EndIf 
 
End Sub

Function SOLICITA.RECARGA																										 ! Solicita recarga del telefono
Integer*2 SOLICITA.RECARGA
Integer*4 XI%, XJ%, XA%, XK%, XL%
String    XFINR$, XLEC$, XBuffer$, XR$
  XJ% = 0 
  XA% = 0 
  SOLICITA.RECARGA = 99 
  XFINR$ = CHR$(13)+CHR$(10)				    																		 ! Fin de registro
  TS.ER.RETURN = -1 
  Open "R::RECA:RTAT"+TS.TERMINAL$+".DAT" AS Ue.Pin.Sesion%	Locked   	       ! Creacion file comunicacion
  If TS.ER.RETURN = -1 Then Begin 																				   ! Respuesta basura
     Delete Ue.Pin.Sesion%																				           ! Borra Archivo
     Wait ; 100
  EndIf 
  TS.ER.RETURN = -1 

  If TS.TRAINING Then GoTo PIN.ENTRENA                        						   ! En entrenamiento

  Create "R::RECA:RECT"+TS.TERMINAL$+".DAT" AS Ue.Pin.Sesion%	Locked         ! Creacion file comunicacion
  If TS.ER.RETURN = -1 Then Begin 																				   ! Si creacion correcta
   For XI% = 1 TO Ue.Pin.TotIng%
     Ue.Pin.Phones$ = Ue.Pin.Numeros$(XI% ,0)								     						 ! Telefono a recargar
     Ue.Pin.Vlr$    = Ue.Pin.Numeros$(XI% ,2)							            			 ! Valor a recargar
    If Val(Ue.Pin.Vlr$) <> 0 Then Begin 
     XA% = XA% + 1 
     XBuffer$ = Ue.Pin.Cadena$ + "|" + Ue.Pin.Almacen$ + "|"         +      \! Se arma String de mensaje
                Ue.Pin.Phones$ + "|" + Ue.Pin.Vlr$ + "|"             +      \! con los datos cadena almacen
                TS.TERMINAL$   + "|" + Str$(Sl.Hd.Transnum + 1) + "|" +     \! telefono valor caja trx fecha hora
                DATE$ + "|" + TIME$ + "|" + Str$(Val(Unpack$(TS.OPER$))) +  \! y cajero 
                XFINR$                                                       !
     XLEC$ = "C"+Str$(Len(XBuffer$))                                         ! Formatea la salida
     Write Form XLEC$ ;#Ue.Pin.Sesion% ; XBuffer$                            !
    EndIf 
   Next XI%  
     XI% = 0 
     Close Ue.Pin.Sesion%																										 ! Cierra archivo
     Wait ; 100
     If XA% = 0 Then Begin 																									 ! Si no hay pines de recarga
        GoTo UE.FINAL.RECARGA																								 ! Sale del proceso
     EndIf 
     PIN.ENTRENA:

!--- Se adiciona requerimiento a la aplicacion de java via pipe 26 Abr 2007 OVS
     
      Ue.Pin.TTLReq% = 3 
      For XL% = 1 To Ue.Pin.TTLReq%                                          ! 3 Requerimientos                
          TS.ER.RETURN = -1																	                 ! Control de errores              
          Write Form "C3";#Ue.Pin.IdPipe%;TS.TERMINAL$                       ! Grabacion sobre el pipe         
          If TS.ER.RETURN <> -1 Then Begin                                   ! Error en la grabacion           
             TS.ER.RETURN = -1															                 ! Control de errores              
             Open "R::PI:R" As Ue.Pin.IdPipe%                                ! Abro el Pipe                    
             If TS.ER.RETURN <> -1 Then Begin                                ! Si Error de apertura del Pipe   
                Create "R::PI:R" As Ue.Pin.IdPipe%                           ! Creo el Pipe                    
                If TS.ER.RETURN <> -1 Then Begin                             ! Si Error Creacion del Pipe      
                   ! Call PIN.IMPRIME("ERROR PROCESO PIPE",6100H)              ! Impresion de Alerta
                   
                EndIf 
             EndIf 
          EndIf Else Wait ; 100 : GoTo Pida.Recarga         ! Proceso Correcto pido recarga
      Next XL%
     
      
!--- Fin lineas adicionadas

     Pida.Recarga:
     Call Msg.Display("SOLICITANDO RECARGA " +  \                            ! Msg de alerta
                      "ESPERE POR FAVOR....")				                         !
     Call TSHIECET																											     ! Tono de alerta
     Wait ; Ue.Pin.Ttl%																										 !

!--- Reporta al Pipe el requerimiento Add OVS 5 Feb 2008
!   If Not TS.TRAINING Then Begin 																   				 ! No En entrenamiento
!    TS.ER.RETURN = -1 
!    Open "R::PI:R" As Ue.Pin.IdPipe%                                        ! Abro el Pipe
!    If TS.ER.RETURN <> -1 Then Begin 
!       SOLICITA.RECARGA = 99																			  			   ! Proceso Satisfactorio
!       Exit Function         
!    EndIf 
!    XR$ = "R" + TS.TERMINAL$
!    Call GRABA.PIPES(Ue.Pin.IdPipe%,"C4",XR$)   ! Graba requerimiento
!    Close Ue.Pin.IdPipe%
!   EndIf 
!--- Fin de lineas adicionadas
       
     If TS.TRAINING Then Begin 																							 ! En entrenamiento
        For XI% = 1 TO Ue.Pin.TotIng%																			   ! Realiza x reintentos
            Ue.Pin.Numeros$(XI%,3) = "00|999999999999|3159999999|10000"
        Next XI%
        SOLICITA.RECARGA = 00																			  			   ! Proceso Satisfactorio
        Exit Function 
     EndIf 
     
     For XI% = 1 TO Ue.Pin.Intentos%																				 ! Realiza x reintentos
       TS.ER.RETURN = -1 																										 ! Ctrl de errores
       Open "R::RECA:RTAT"+TS.TERMINAL$+".DAT" AS Ue.Pin.Sesion% Locked      ! Apertura respuesta
       If TS.ER.RETURN = -1 Then Begin 																			 ! Apertura correcta
        If End #Ue.Pin.Sesion% Then UE.FINAL.RECARGA 										  	 ! Si es EOF
        XJ% = 0 																												     ! Init de la variable
        While (1)
          Read #Ue.Pin.Sesion%; Line TS.TEMP1$		 													 ! Lectura registro
          XJ% = XJ% + 1 
          If Len(TS.TEMP1$) > 3 Then Begin 
            For XK% = 1 To Ue.Pin.TotIng%																			 ! Total de numeros ingresados
             If Val(Ue.Pin.Numeros$(XK%,2)) <> 0 Then Begin             			 ! Valor a recargar
              If Ue.Pin.Numeros$(XK%,3) = "" Then Begin     					 ! Si no ha cargado respuesta
               Ue.Pin.Numeros$(XK%,3) = TS.TEMP1$						 ! Almacena Vector de respuesta
               XK% = Ue.Pin.TotIng% + 1								 ! Sale del ciclo
              EndIf 																													 !
             EndIf 																														 ! 
            Next XK%  																												 !
          EndIf 
          If XJ% = Ue.Pin.TotIng% Then GoTo UE.FINAL.RECARGA								 !
        Wend 
        UE.FINAL.RECARGA:																										 ! Final carga respuesta
          wait ; 100 
          Delete Ue.Pin.Sesion%		 																	         ! Cierra Archivo
          wait ; 100
          TS.ER.RETURN = -1
          Open "R::RECA:RECT"+TS.TERMINAL$+".DAT" AS Ue.Pin.Sesion%	Locked   ! Creacion file comunicacion
          If TS.ER.RETURN = -1 Then BEGIN 
             Delete Ue.Pin.Sesion%
          ENDIF 
          SOLICITA.RECARGA = 00																			  			 ! Proceso Satisfactorio
          Exit Function           
       EndIf Else Begin 																										 ! Sin apertura
          SOLICITA.RECARGA = 02																			  			 ! Proceso fallido
          Wait ; Ue.Pin.Ttl%	  																		         ! Ejecuta TTL
       EndIf 
     Next XI%
     wait ; 100
     TS.ER.RETURN = -1
     Open "R::RECA:RECT"+TS.TERMINAL$+".DAT" AS Ue.Pin.Sesion%	Locked       ! Creacion file comunicacion
      If TS.ER.RETURN = -1 Then BEGIN 
         Delete Ue.Pin.Sesion%
      ENDIF 
     
  EndIf Else Begin 																												   ! Error en la creacion
     SOLICITA.RECARGA = 99																									 !
  EndIf 

End Function 

Function ANALIZA.RECARGA            																				 ! Analiza las respuestas entregadas
Integer*1 ANALIZA.RECARGA
Integer*4 XI%, XJ%
String    Xrta$, XMsg$, XPhon$, XVlr$, Xtraid$
     To.USEREXIT(20) = 0
     To.USEREXIT(60) = 0
     Ue.Pin.Ok% = -1																										     ! Control de respuestas
     For XI% = 1 TO Ue.Pin.TotIng%																					 ! Analiza todos los numeros
         Xrta$  = Ue.Pin.Numeros$(XI%,3)                                     ! Toma String de Respuesta
         XMsg$  = Mid$(Xrta$,1,2)																						 ! Tipo de Respuesta
         Xtraid$= Mid$(Xrta$,4,12)																					 ! Traid de la operacion
         XPhon$ = Ue.Pin.Numeros$(XI%,0)                                     ! Toma Numero de Telefono
         XVlr$  = Ue.Pin.Numeros$(XI%,2)                                     ! Valor de la recarga
         Ue.Pin.Numeros$(XI%,4) = Xtraid$																		 ! Asigno respuesta
         If Val(XMsg$) <> 0 Then Begin 																		   ! Si no rta satisfactoria
            Ue.Pin.Numeros$(XI%,4) = "000000000000"												   ! Asigno respuesta

            TS.TEMP3$ = RIGHT$("00"+XMsg$,2) +                              \! Mensaje de Respuesta
              " "+Ue.Pin.MsgErr$(Val(XMsg$))													       !
            Call Msg.Visor(TS.TEMP3$)                   				   		       ! Mensaje al cajero

            Ue.Pin.Ok% = 2  																								 ! Numeros no autorizados
         EndIf 
     Next XI%				
     If Ue.Pin.Ok% = 2 Then Begin             															 ! Reporta Errores

        Call PIN.IMPRIME("RECARGAS NO EFECTUADAS , DEVOLUCION",6100H)        ! 

        For XI% = 1 TO Ue.Pin.TotIng%																			   ! Analiza todos los numeros
         Xrta$  = Ue.Pin.Numeros$(XI%,3)                                     ! Toma String de Respuesta
         Xrta$  = Left$(Xrta$,2)																						 ! Toma la respuesta
         If Val(Xrta$) <> 0  Then Begin                                      ! Reporta numeros fallidos
          If Val(Ue.Pin.Numeros$(XI%,2)) > 0 Then Begin                      ! Si valor mayor que cero
            Call PIN.IMPRIME("RECARGA  :"+Ue.Pin.Numeros$(XI%,0),6100H)      ! Numero de telefono
            Call Format.Amount(Val(Ue.Pin.Numeros$(XI%,2)))
            Call PIN.IMPRIME("VALOR    :"+TS.TEMP1$,6100H)                   ! Valor de la recarga
            TS.TEMP3$ = RIGHT$("00"+Xrta$,2) +                              \! Mensaje de Respuesta
                        " "+Ue.Pin.MsgErr$(Val(Xrta$))								       !
            Call PIN.IMPRIME("CONCEPTO :"+TS.TEMP3$,6200H)
            Call GRABA.DEVOL.TELEFONO(XI%)                                   ! Almacena numeros reversados            
           EndIf 
         EndIf 
        Next XI%
        TS.TEMP1$ = PIN.Detalle                            									 ! Impresion trailer trx
        Call PIN.IMPRIME(TS.TEMP1$,4100H)                  									 ! Detalle Trailer Trx        
        Call PIN.IMPRIME(String$(19,"- "),6100H)
        Call PIN.IMPRIME("",4900H)
        Call PIN.IMPRIME("",4500H)
        Call PIN.CORTACR

     EndIf 
     To.USEREXIT(20) = -1
     To.USEREXIT(60) = -1
     ANALIZA.RECARGA = Ue.Pin.Ok%
End Function 

Function INGRESA.RECARGA																										 ! Captura numero telefonico y valor
String XPhone1$, XVlr1$, XPhone2$, XVlr2$, Xtmp$
Integer*1 INGRESA.RECARGA
Integer*4 XA%, X.Into%, Xb%
Real      XVlr1%, XVlr2%,  XPrice%
  Ue.Pin.Imp% = 0																														 ! Ctrl Impresion
  X.Into% = 0
  
  If Ue.Pin.AnulCarga% = -1 Then Begin    ! Esta en una anulacion 
     Call Msg.Visor("PROCESO NO PERMITIDOEN DEVOLUCION.")                ! Msg de alerta
     INGRESA.RECARGA = 0 
     Exit Function   
  EndIf 
  
  If (TS.IO.DATA$(6) <> "") Then Begin 																			 !
     Call Msg.Visor("ITEM NO PERMITE     VENTA POR CANTIDAD")                ! Msg de alerta
     INGRESA.RECARGA = 0 
     Exit Function   
  EndIf
  
  REGRESO.INICIO.TELEFONO:
  X.Into% = 0  
  INGRESO.TELEFONO:
  XPhone1$ = PIN.DATOS$("INGRESE NUMERO","A RECARGAR ...    ")               !
  If (XPhone1$ = " " OR XPhone1$ = "E") Then Begin 									         ! Proceso cancelado
     X.Into% = X.Into% + 1 
     If X.Into% > 1 Then Begin
        INGRESA.RECARGA = 0 
        Exit Function 
     EndIf
     GoTo INGRESO.TELEFONO
  EndIf 

  If Len(Xphone1$) <> Ue.Pin.MaxDig% Then Begin 
     Call Msg.Visor("NUMERO DE DIGITOS   NO SON VALIDOS     ")              ! Msg de alerta
     GoTo REGRESO.INICIO.TELEFONO
  EndIf 

!-- Validacion adicional para manejo de los prefijos
!-- OVS 29 Ago 2006
   
   Xtmp$ = Left$(Xphone1$,3)																								! Toma phone capturado
   Xtmp$ = ";" + Right$("000"+Xtmp$,3) + ";"																! Ajusta a 3 digitos 
   XA% = MATCH(Xtmp$,Ue.Pin.Prefijos$,1)																		! Valida prefijo en la lista
   If XA% <= 0 Then Begin 																										! Si no lo encuentra
      Call Msg.Visor("PREFIJO RECARGA  NO ES VALIDO")                       ! Msg Alerta
      GoTo REGRESO.INICIO.TELEFONO
   EndIf 

!--- Fin de lineas adicionadas
  X.Into% = 0 
  INGRESO.TELEFONO2:
  XPhone2$ = PIN.DATOS$("REINGRESE NUMERO","A RECARGAR...     ")               !
  If (XPhone2$ = " " OR XPhone2$ = "E") Then Begin 									         ! Proceso cancelado
     X.Into% = X.Into% + 1 
     If X.Into% > 1 Then Begin
        INGRESA.RECARGA = 0 
        Exit Function 
     EndIf
     GoTo INGRESO.TELEFONO2
  EndIf 

  If (Val(Xphone1$) <> Val(Xphone2$)) Then Begin                            ! Valida Datos capturados
     Call Msg.Visor("DATOS INGRESADOS    NO SON VALIDOS")                   ! Msg de alerta
     GoTo INGRESO.TELEFONO2
  EndIf 
  
  
!--- Validacion de si numero ya capturado OVS 20 Jun 2008

For XB% = 1 to Ue.Pin.TotIng% 
 If Ue.Pin.Numeros$(XB%,0) = XPhone1$ Then Begin                            ! Si numero ya capturado
    Call Msg.Visor("NUMERO YA HA   SIDO CAPTURADO...")                   ! Msg de alerta
    INGRESA.RECARGA = 0 
    Exit Function 
 EndIf 
Next XB%

!--- Fin de la validacion
  
  
  X.Into% = 0 
  INGRESO.VALOR.RECARGA:      
  XVlr1$ = PIN.DATOS$("INGRESE VALOR ","A RECARGAR        ")                 !
  If (XVlr1$ = " " OR XVlr1$ = "E") Then Begin 									             ! Proceso cancelado
     X.Into% = X.Into% + 1
     If X.Into% > 1 Then Begin
        INGRESA.RECARGA = 0 
        Exit Function 
     EndIf
     GoTo INGRESO.VALOR.RECARGA
  EndIf 


  XPRICE% = Val(XVlr1$)                                                     ! Toma precio capturado
  If (XPRICE% <  Ue.Pin.MntoMin%) Then Begin                               \! Si valor fuera del rango
      Call Msg.Visor("COMPRA MENOR LIMITESDEL OPERADOR")                    ! Msg de alerta
      GoTo INGRESO.VALOR.RECARGA
  Endif 

    If (XPRICE% > Ue.Pin.MntoMax%) Then Begin                              \! Si valor fuera del rango
      Call Msg.Visor("COMPRA MAYOR LIMITESDEL OPERADOR")                    ! Msg de alerta
      GoTo INGRESO.VALOR.RECARGA
  Endif 

  
!--- Validar el multiplo capturado  

  If Ue.Pin.Multiplo% >  0 Then Begin 																		  ! 
   XVlr1% = (XPRICE% / Ue.Pin.Multiplo%) 																		! Para calculo del multiplo
   
   XVlr2% = XVlr1% - Int(Xvlr1%)																				    ! Valido si hay residuo

   If (XVlr2% > 0 ) Then Begin 																							! No es multiplo
      Call Msg.Visor("VALOR A CARGAR NO ESPERMITIDO   ")                    ! Msg de alerta
      Call Format.AMOUNT(Ue.Pin.Multiplo%)																	! Formatea numero
      Call Msg.Visor("SOLO MULTIPLOS DE   "+TS.TEMP1$)											! Mensaje de alerta
      GoTo INGRESO.VALOR.RECARGA
   EndIf 
   
  EndIf Else Begin 
      Call Msg.Visor("ERROR EN PARAMETROS DEL OPERADOR TELEFONICO")         ! Msg de alerta
      INGRESA.RECARGA = 0 																									! 
      Exit Function 
  EndIf 
  
  INGRESA.RECARGA = -1																											! Datos correctos
  Ue.Pin.Phones$  = XPhone1$																							  ! Retorna datos
  Ue.Pin.Vlr$     = XVlr1$                                                  ! Capturados
  Ue.Pin.Imp%     = -1																									    ! Ctrl Impresion
  Ue.Pin.IrPrice% = -1  																									  ! Cambio de precio
  
  !TS.IO.DATA$(6)  = Str$(Int( Val(XVlr1$) / Ue.Pin.Multiplo%))              ! Cantidad de registro
  !TS.IO.KEYS(6)   = 75
  !IR.PRICE1       = Ue.Pin.Multiplo%
  !IR.PRICE2      = Ue.Pin.Multiplo%
  !SL.IE.QTYORWGT  = Val(TS.IO.DATA$(6))                                     ! 
  
End Function 

Function Pin.String(X.A$)                       ! Coloca datos por parametro
String Pin.String, X.A$, X.B$, X.C$, X.D$, XM$
Integer*4 X.A%, X.B%
X.A% = MATCH("&",X.A$,1)						            ! Busca dato a imprimir
If X.A% > 0 Then Begin                          ! Si lo encotro
   X.B% = LEN(X.A$)                             ! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    ! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     ! 38 caracteres
   X.D$ = Right$(X.A$,X.B% - (X.A%+1))
   If UCASE$(X.C$) = "&F" Then Begin  	        ! Fecha de la operacion 
      X.C$ = "20"+Left$(DATE$,2)+"-"+          \!
      Mid$(DATE$,3,2)+"-"+ Right$(DATE$,2)+" "  ! 
      X.C$ = X.C$ + Left$(TIME$,2)+":"+        \!
      Mid$(TIME$,3,2)
   EndIf   
   If UCASE$(X.C$) = "&P" Then Begin            ! Numero de Telefono de Recarga
      X.C$ = Ue.Pin.Phones$
   EndIf
   If UCASE$(X.C$) = "&V" Then Begin            ! Valor de la recarga
      Call Format.Amount(Val(Ue.Pin.Vlr$))
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&C" Then Begin            ! Cadena y Almacen
      X.C$ = Ue.Pin.Cadena$ +"-"+ Ue.Pin.Almacen$
   EndIf
   If UCASE$(X.C$) = "&T" Then Begin            ! Traid de autorizacion
      X.C$ = Ue.Pin.Traid$
   EndIf
   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
EndIf    

X.A% = MATCH("&",X.A$,1)			! Busca dato a imprimir
If X.A% > 0 Then Begin                          ! Si lo encotro
   X.B% = LEN(X.A$)                             ! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    ! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     ! 38 caracteres
   X.D$ = Right$(X.A$,X.B% - (X.A%+1))
   If UCASE$(X.C$) = "&F" Then Begin   		! Fecha de la operacion 
      X.C$ = "20"+Left$(DATE$,2)+"-"+          \!
      Mid$(DATE$,3,2)+"-"+ Right$(DATE$,2)      ! 
   EndIf   
   If UCASE$(X.C$) = "&P" Then Begin            ! Numero de Telefono de Recarga
      X.C$ = Ue.Pin.Phones$
   EndIf
   If UCASE$(X.C$) = "&V" Then Begin            ! Valor de la recarga
      Call Format.Amount(Val(Ue.Pin.Vlr$))
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&C" Then Begin            ! Cadena y Almacen
      X.C$ = Ue.Pin.Cadena$ +"-"+ Ue.Pin.Almacen$
   EndIf
   If UCASE$(X.C$) = "&T" Then Begin            ! Traid de autorizacion
      X.C$ = Ue.Pin.Traid$
   EndIf
   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
EndIf       

PIN.String = X.A$

End Function
!--- Fin funcion data String

Sub IMPRIMA.RECARGA(XPos$)
String XPos$, XMsg$
Integer*4 XI%
 To.USEREXIT(20) = 0
 To.USEREXIT(60) = 0
 If TS.TRAINING Then \
    Call PIN.IMPRIME("  TIQUETE DEMOSTRACION SIN VALIDEZ  ",4100H)
   
 For XI% = 1 TO VAL(UE.PIN.MSG$(VAL(XPos$),0))		       ! Total de lineas a imprimir
     XMSG$ = Ue.Pin.Msg$ (VAL(XPos$),XI%)		             ! Toma linea de impresion
     XMSG$ = PIN.String(XMSG$)				                   ! Analiza linea impresion
     Call PIN.IMPRIME(XMSG$,4100H)			                 ! Imprime Texto
 Next XI%                                                !
 If TS.TRAINING Then \
    Call PIN.IMPRIME("  TIQUETE DEMOSTRACION SIN VALIDEZ  ",4100H)
 To.USEREXIT(20) = -1
 To.USEREXIT(60) = -1 
End Sub 

Sub RECARGA.ONLINE Public
Integer*4 XI%
If Ue.Pin.Capturadas% <> 0 THEN BEGIN  											! Si tarjetas capturadas
   Ue.Pin.Total% = 0
   Ue.Pin.Tmp%   = 0
   
   Ue.Pin.Tmp% = SOLICITA.RECARGA
   
   If Ue.Pin.Tmp% <> 0 Then Begin 																					 ! Si autorizacion fallo
     For XI% = 1 TO Ue.Pin.TotIng%																					 ! Analiza todos los numeros
      Ue.Pin.Numeros$(XI%,3) = Str$(Ue.Pin.Tmp%)                             ! Toma String de Respuesta   
     Next XI%
   EndIf 

   Ue.Pin.Tmp% = ANALIZA.RECARGA
   If Ue.Pin.Tmp% <> -1 Then Begin 
      TS.TEMP3$ = "REALICE DEVOLUCION  RECARGAS FALLIDAS"
      Call Msg.Visor(TS.TEMP3$)                   										       ! Mensaje al cajero
   EndIf   
   
EndIf 
End Sub 

Sub MIRAR.RECARGA Public
Integer*2 IMP.I%, IMP.J%, ARC%, NROPIN%
String    MSG$, UELF$, LETRA$, CPI$, CPI17$, UE.POSL$, UE.X.MSG$
Integer*4 GET.DEPT.SALES, PRIC%, TOT4%, XI%
String    XHOLDB$, XRELBF$, Xline$

If Ue.Pin.AnulCarga% = -1 Then Exit Sub 

Call RECARGA.ONLINE 																			    ! Generando recarga

  To.USEREXIT(20) = 0
  To.USEREXIT(60) = 0
     
If Ue.Pin.Capturadas% = -1 and Ue.Pin.AutAnul% <> 2 Then Begin 

!  Call Msg.Display("GENERANDO TIQUETE(S)" + "RECARGA EN LINEA ..." ) !
!	Call TSHIECET															                 	! Tono de alerta

  
  For XI% = 1 TO Ue.Pin.TotIng% 															! Total telefonos
   Xline$ = Ue.Pin.Numeros$(XI%,3)
   Xline$ = Left$(Xline$,2)
   If  Xline$ = "00" Then Begin                               ! Numero autorizado
       Ue.Pin.Phones$ = UE.PIN.NUMEROS$(XI%,0)      				  ! Telefono almacenado
       Ue.Pin.Vlr$   = UE.PIN.NUMEROS$(XI%,2)	 		  					! Valor de la Recarga
       Ue.Pin.Oper$  = UE.PIN.NUMEROS$(XI%,1)	 		  					! Operador de la recarga
       Ue.Pin.Traid$ = UE.PIN.NUMEROS$(XI%,4)	 		  					! Traid de autorizacion
       If Val(Ue.Pin.Traid$) <> 0 Then Begin 
          Call Save.Oper.Recarga															! Grabacion String
          Call IMPRIMA.RECARGA(Ue.Pin.Oper$)               		! Imprime Formato
          TS.TEMP1$ = PIN.Detalle                          		! Impresion trailer trx
          Call PIN.IMPRIME(TS.TEMP1$,4100H)                		! Detalle Trailer Trx
          If PRT4610.ENABLE = 0 Then Begin 							  		! Manejo impresora matriz
             Call PIN.IMPRIME("",4900H)										 		! Avance de lineas antes 
             Call PIN.IMPRIME("",4500H)												! de guillotinar 
             Wait ; 400 																			! Tiempo de espera
	        EndIf ELSE Begin 						  							     		! Manejo impresora termica
             Call PIN.IMPRIME("",4900H)												!
             Call PIN.IMPRIME("",4500H)												!
          EndIf																								!
          Call PIN.CORTACR                                 		! Corta Papel        
       EndIf 
   EndIf 
  Next XI%
  
  
EndIf 
     To.USEREXIT(20) = -1
     To.USEREXIT(60) = -1
End Sub
!--- Fin de la funcion de impresion Mensajes de recarga

Sub ANULACION.RECARGAS
String XPOS$, Xtmp$, Xcaja$, Xtrx$, Xkey$, XLlave$, Xvlr$, Xdate$, Xplu$
Integer*4 XA%

If Ue.Pin.Capturadas% = -1 and TS.TOTALS(0,0,0) >= 0 Then Begin 																	! Telefonoc capturados dentro de una trx
 If Ue.Pin.Dialogo% = 0 Then Begin 
  XPos$ = PIN.DATOS$("INGRESE POSICION","A REVERSAR")               	 	!
  If (XPOS$ = " " OR XPOS$ = "E") Then Begin 			         	! Proceso cancelado
     IR.INDICAT0 = IR.INDICAT0 OR 04H				            ! No permite venta
     Exit Sub
  EndIf 

!-- Ajuste BUG aplicacion 25 abr 2008 OVS 
!-- Se controla que posicion a anular capturada no supere las 
!-- cantidades capturadas de recarga

  If Val(Xpos$) > Ue.Pin.TotIng% Then begin 
     Call MSG.VISOR("POSICION ANULAR INVALIDA")		  ! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H		          ! No permite venta
     Exit Sub 
  Endif 
  
  TS.TEMP2$ = Pack$(SL.IT.ITEMCODE$)                           ! Item a consultar
  Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)              ! Lectura Datos Itemr
  Call SPLIT.ITEMRECORD ( TS.TEMP1$ ) 			       ! 
  If UnPack$(PIN.INDICAT2$) = "05" Then Begin 
     Xplu$ = UnPack$(PIN.SALEPRIC$)
     IR.INDICAT0  = 01H
  EndIf Else Xplu$ = SL.IT.ITEMCODE$


  If Val(Ue.Pin.Numeros$(Val(XPOS$),6)) <> Val(Xplu$) Then Begin 					  ! movimiento ya anulado
     Call MSG.VISOR("PLU NO COINCIDE CON EL REGISTRADO") 					  ! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
     Exit Sub 
  EndIf  
  
  If Ue.Pin.Numeros$(Val(XPOS$),5) = "ANULADO" Then Begin 						  ! movimiento ya anulado
     Call MSG.VISOR("REGISTRO YA HA SIDO ANULADO  ")									  ! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
     Exit Sub 
  EndIf 

  If (Val(XPOS$) > Ue.Pin.TotIng%) OR (Val(XPOS$) = 0 ) Then Begin      ! Si dato capturado no valido
     Call MSG.VISOR("POSICION INGRESADA  NO VALIDA")									  ! Msg Alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
     Exit Sub 
  EndIf 
  
  If Val(Ue.Pin.Numeros$(Val(XPOS$),4)) > 0 Then Begin                  ! No permite devolucion
     Call MSG.VISOR("NO SE PERMITE REVERSAR NUMERO RECARGADO")
     IR.INDICAT0 = IR.INDICAT0 OR 04H												            ! No permite venta
     Exit Sub 
  EndIf 
  Ue.Pin.Save1$ = Ue.Pin.Numeros$(Val(XPOS$),5)
  Ue.Pin.Save2$ = Ue.Pin.Numeros$(Val(XPOS$),2)
  Ue.Pin.Save3$ = Xpos$
  Ue.Pin.Numeros$(Val(XPOS$),5) = "ANULADO"		      									  ! Anula movimiento
  Ue.Pin.Vlr$ = Ue.Pin.Numeros$(Val(XPOS$),2)														! Retorna Valor 
  Ue.Pin.AnulItm% = -1                                                  ! Anulacion de items
  Ue.Pin.Numeros$(Val(XPOS$),2) = "0" 
  
  XTMP$ = Pack$(Xpos$)                   +												     \! Posicion de memoria anulado
          ":"+Pack$("00")                                               ! Filler
  Call Pin.Cadena.Usuario2("20060723",XTMP$)                            ! Graba String usuario  
  
  Ue.Pin.Dialogo% = -3
  Ue.Pin.IrPrice% = -1    
  Ue.Pin.Devol%   = -1 
  Ue.Pin.Imp%     = -1 
  
  !TS.IO.DATA$(6)  = Str$(Int( Val(Ue.Pin.Vlr$) / Ue.Pin.Multiplo%))          ! Cantidad de registro
  !TS.IO.KEYS(6)   = 75
  !IR.PRICE1       = Ue.Pin.Multiplo%
  !IR.PRICE2       = Ue.Pin.Multiplo%
  !SL.IE.QTYORWGT  = Val(TS.IO.DATA$(6))                                 ! 
  
  Exit Sub 
 EndIf 
EndIf Else Begin						 ! Anulacion por fuera de una trx

 If Ue.Pin.Dialogo% = 0 and Ue.Pin.AnulDlgo% = 0 Then Begin 

    If TS.TOTALS(0,0,0) <> 0 Then Begin 																	 ! Hay venta
       Call Msg.Visor("NO SE PERMITE REALIZAR DEVOL. CON VENTAS")        !
       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta     
       Exit Sub 																												 !
    EndIf 

    XPos$ = PIN.DATOS$("NUMERO DE RECARGA ","A REVERSAR")            	 	 !
    If (XPOS$ = " " OR XPOS$ = "E") Then Begin 									       	 ! Proceso cancelado
       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
       Exit Sub
    EndIf     

!--- Lineas Adicionadas 27 Jun 2008 OVS

  If Len(XPos$) <> Ue.Pin.MaxDig% Then Begin 
     Call Msg.Visor("NUMERO DE DIGITOS   RECARGA  NO VALIDOS")              ! Msg de alerta
     IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
     Exit Sub
  EndIf 

!-- Validacion adicional para manejo de los prefijos
   
   Xtmp$ = Left$(Xpos$,3)																								! Toma phone capturado
   Xtmp$ = ";" + Right$("000"+Xtmp$,3) + ";"																! Ajusta a 3 digitos 
   XA% = MATCH(Xtmp$,Ue.Pin.Prefijos$,1)																		! Valida prefijo en la lista
   If XA% <= 0 Then Begin 																										! Si no lo encuentra
      Call Msg.Visor("PREFIJO RECARGA  NO ES VALIDO")                       ! Msg Alerta
      IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
      Exit Sub
   EndIf 

!--- Fin lineas adicionadas

    Xcaja$ = PIN.DATOS$("NUMERO TERMINAL DE","LA VENTA")            	 	 !
    If (Xcaja$ = " " OR Xcaja$ = "E") Then Begin 									       	 ! Proceso cancelado
       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
       Exit Sub
    EndIf     

    Xtrx$ = PIN.DATOS$("NUMERO TRANSACCION DE","LA VENTA")          	 	 !
    If (XTRX$ = " " OR XTRX$ = "E") Then Begin 									       	 ! Proceso cancelado
       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
       Exit Sub
    EndIf     

!    XTmp$ = PIN.DATOS$("VALOR A RETORNAR","")            	 	 						 !
!    If (XTMP$ = " " OR XTMP$ = "E") Then Begin 									       	 ! Proceso cancelado
!       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
!       Exit Sub
!    EndIf         

    TS.ER.RETURN = -1 
    Open "R::PINANREC" KEYED RECL 17 AS Ue.Pin.Sesion%                   ! Abre archivo devoluciones
    If TS.ER.RETURN <> -1 Then Begin 																		 ! Error de apertura
       Call MSG.VISOR("PROCESO ANUL NO VALIDADO")												 ! Msg de alerta
       IR.INDICAT0 = IR.INDICAT0 OR 04H												           ! No permite venta
       Exit Sub
    EndIf 
    Xkey$ = Right$("0000"+Xcaja$,4) + \				 					      			  	 ! Numero de terminal
            Right$("0000"+XTrx$,4) + \                                   ! Numero de transaccion
            Right$("0000000000"+XPos$,10)                                ! Numero del telefono
    Xkey$ = Pack$(Xkey$)																								 ! Empaqueta la llave                             
    TS.ER.RETURN = -1 
    Read Form "C9 C5 C3";#Ue.Pin.Sesion% KEY XKey$;                     \! Lee Reg Cabecera 
         XLlave$, Xvlr$, Xdate$
    If TS.ER.RETURN <> -1 Then Begin 																		 ! Si movimiento no exite
       Close Ue.Pin.Sesion%
       Call Msg.Visor("MOVIMIENTO NO EXISTEO YA ANULADO.")														 ! 
       Call PIN.IMPRIME("REC:"+UnPack$(XKey$),2100H)
       Call PIN.IMPRIME("REVERSO RECARGA FALLIDO",2100H)
       Call PIN.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                        STR$(TS.ERRF),2100H)
       IR.INDICAT0 = IR.INDICAT0 OR 04H					 ! No permite venta
       Exit Sub
    EndIf 
    Close Ue.Pin.Sesion%
    XTmp$ = Unpack$(Xvlr$)																							 ! Valor a retornar
    Ue.Pin.Phones$ = Xpos$
    Ue.Pin.Vlr$ =    Str$(Val(Xtmp$)) 
    IR.PRICE2 = Val(Xtmp$)                                               ! Asigno precio de venta
    IR.PRICE1 = Val(Xtmp$)                                               ! Asigno precio de venta
    SL.IE.SALEPRIC = Val(Ue.Pin.Vlr$)                			 !
    SL.IT.XPRICE   = Val(Ue.Pin.Vlr$)                			 !
    UE.PIN.AUTORIZA% = -1
    Ue.Pin.Imp% = 0 
    UE.PIN.NUMREC$ = XPOS$
    UE.PIN.VLRNUL$ = XTMP$
    UE.PIN.KEYREG$ = XKEY$

!    XTMP$ = Pack$(Xpos$)                   +			        \! Numero de Telefono Anulado
!            ":"+Pack$(XTMP$)               +                            \! Valor Anulado
!            ":"+Xkey$                      +                            \! Llave del registro
!            ":"+Pack$("00")                                              ! Filler
!    Call Pin.Cadena.Usuario2("20061006",XTMP$)                           ! Graba String usuario  
    
    Ue.Pin.IrPrice%  = -1    
    Ue.Pin.DevoMov%  = -1
    !Ue.Pin.Dialogo%  = -2
    
    Ue.Pin.AnulDlgo% = -1
    Ue.Pin.Devol%    = -1 
    Ue.Pin.Imp%      = -1 
    
    TO.CANCELPREV    = -1                                                 ! Permite Anulacion de Item
    
    Ue.Pin.AnulCarga% = -1 
        
    !TS.IO.DATA$(6)  = Str$(Int( Val(Ue.Pin.Vlr$) / Ue.Pin.Multiplo%))          ! Cantidad de registro
    !TS.IO.KEYS(6)   = 75
    !IR.PRICE1       = Ue.Pin.Multiplo%
    !IR.PRICE2       = Ue.Pin.Multiplo%
    !SL.IE.QTYORWGT  = Val(TS.IO.DATA$(6))                                 ! 
    
  EndIf 
EndIf   
  
End Sub 

Sub PIN.ANUL.TOTAL						
Integer*4 XI% 
   
   For XI% = 1 TO Ue.Pin.TotIng%																			 ! Analiza todos los numeros
     If Val(Ue.Pin.Numeros$(XI%,4)) > 0 Then Begin                     ! Telefono Recargado
      CALL MSG.VISOR("TRANSACCION NO PUEDE SER ANULADA")
      CALL MSG.VISOR("RECARGAS YA CAPTURADAS")
      DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)
      TS.IO.MOTORKEY = 0 
      TS.GUIDANCE = 1117     
      Exit Sub 
     EndIf 
   Next XI%
   Ue.Pin.AutAnul% = 2

End Sub 

Sub PROCESO.MARCAR.ANUL Public             												        !
 Integer*4 PRIC%, TOT4%, X.I%, XI%																	  	  !
 String X.TMP$, XCLAVE$, Xvlr$, Xkey$																			!
  TS.ER.RETURN = -1 
  Open "R::PINANREC" KEYED RECL 17 AS Ue.Pin.Sesion%                    ! Abre archivo devoluciones
  If TS.ER.RETURN <> -1 Then Begin 				    	! Error de apertura
      Call Pin.Imprime("Error Apertura PINANREC",2100H)      
      Call PIN.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                       Str$(TS.ERRF),2100H)
      Exit Sub							  	! Sale del proceso
  EndIf    																														    !
  For X.I% = 1 TO Ue.Pin.Slend%      					! Recorre todos los String
    H$ = READ.SL.STR$(X.I%)          					! Toma el String
    If LEN(H$) > 5 Then              				       \! Si el String es OK
     If ASC(H$)  = 153 Then Begin    					! Si es un User Data
        Ue.Pin.Apun% = 3						! Asigna apuntador
        X.TMP$ = PIN.GETUNPK(H$,Ue.Pin.Apun%)	 			! Verifica el numero de proyecto
        If X.TMP$ = "20061006" Then Begin                               ! Si Anulacion Recarga
           X.TMP$ = PIN.GETUNPK(H$,Ue.Pin.Apun%)		  	! Numero del Telefono
           XVlr$  = PIN.GETUNPK(H$,Ue.Pin.Apun%)	  		! Valor Anulacion
           Xkey$  = PIN.GETUNPK(H$,Ue.Pin.Apun%)		  	! Llave del registro
           Xkey$  = Pack$(Xkey$)				        ! Empaqueta Dato
           TS.ER.RETURN = -1 
           DelRec Ue.Pin.Sesion% ; Xkey$			        ! Borra Registro
           If TS.ER.RETURN <> -1 Then Begin 
              Call Pin.Imprime("Error Retiro Reverso",2100H)
              Call PIN.IMPRIME("REC:"+UnPack$(XKEY$),2100H)
              Call PIN.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                                Str$(TS.ERRF),2100H)
           EndIf Else Begin 
              !Call Msg.Display("RECARGA REVERSADA   "+X.TMP$)
              !Wait ; 1200
           EndIf 
        EndIf 
     EndIf                                  															!
  Next X.I%                                 															!
  Close Ue.Pin.Sesion%																										! Cierre archivo
End Sub

!
!--- Definicion procedimientos para ser invocados en la terminal
!

Sub RECATS01.011 PUBLIC         
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.PIN.RECAMOD% = -1 Then Begin 

!%INCLUDE ADX_UPGM:PINTSU01.011

EndIf 
End Sub                                                 !
!------------------------------------------------------------------------------
 
Sub RECATS02.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If UE.PIN.RECAMOD% = -1 Then Begin 

%INCLUDE ADX_UPGM:PINTSU02.011

EndIf 
End Sub                                                 !
!------------------------------------------------------------------------------
 

Sub RECATS07.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS07.011                                         **
!   **    Inclu¡r en :  EAMTSU07.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

%INCLUDE ADX_UPGM:PINTSU07.011
  
End Sub                                               !
!------------------------------------------------------------------------------
  
Sub RECATS08.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS08.011                                         **
!   **    Inclu¡r en :  EAMTSU08.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

   %INCLUDE ADX_UPGM:PINTSU08.011
   
EndIf 

End Sub                                                 !
!------------------------------------------------------------------------------
 
Sub RECATS14.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS14.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

%INCLUDE ADX_UPGM:PINTSU14.011

EndIf 

End Sub                                                 ! 

Sub RECATS20.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

   %INCLUDE ADX_UPGM:PINTSU20.011
   
EndIf 

End Sub                                                 !

Sub RECATS43.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS43.011                                         **
!   **    Inclu¡r en :  EAMTSU43.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

   %INCLUDE ADX_UPGM:PINTSU43.011
   
EndIf 

End Sub                                                 !
 
Sub RECATS53.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS56.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

%INCLUDE ADX_UPGM:PINTSU53.011

EndIf 


End Sub                                                 !

Sub RECATS56.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS56.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If UE.PIN.RECAMOD% = -1 Then Begin 

%INCLUDE ADX_UPGM:PINTSU01.011

EndIf 


End Sub                                                 !

Sub RECATS60.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS60.011                                         **
!   **    Inclu¡r en :  EAMTSU60.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String XTMP$
If UE.PIN.RECAMOD% = -1 Then Begin 

   %INCLUDE ADX_UPGM:PINTSU60.011
EndIf 

End Sub                                                 !

Sub RECATS66.011(X.Callparm) PUBLIC                     !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2006                                        **
!   **    User Exit  :  RECATS60.011                                         **
!   **    Inclu¡r en :  EAMTSU60.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Integer*1  X.Callparm
If UE.PIN.RECAMOD% = -1 Then Begin 

   %INCLUDE ADX_UPGM:PINTSU66.011
   
EndIf 

End Sub                                                 !
