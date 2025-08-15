!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : DOMIEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Enero 4  de 2.006                *
!Observaciones : Modulo Domicilios Version 1.0    *
!**************************************************
! Modificaciones

%ENVIRON T

String    Global Asc.Dom.Num$ 
Integer*1 Global Asc.Num.Ped%, Asc.Dom.Modul%
Integer*4 Global Asc.Tmp.Apun%, Asc.Dom.Motora%

%INCLUDE EAMTSWKG.J86          ! working storage

Sub TSHIECET External 
End Sub 

Function Grabacion.Cadena.Usuario(X.Clave$,X.Datos$) External
 String X.Clave$, X.Datos$
End Function 

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
 String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
 Integer*2 U.d.visor%, U.d.tiempo%   							! trabajo
 String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function ASIC.DATOS$(UMSG1$,UMSG2$) External     								! Captura de datos
 String UMSG1$, UMSG2$, ASIC.DATOS$
End Function
!--- Fin entrada de datos

Function U.IMPRIME(UE.STRING$,UE.STATION) External					! Rutina de Impresion
 String UE.STRING$, LF$ 											! Variables temporales
 Integer*2 UE.STATION											!
End Function

Function ASIC.GETUNPK(X.B$,X.J%) External 
 Integer*2 X.J%, X.K%
 String    X.B$, ASIC.GETUNPK
End Function

Sub VISOR.AND.BORRAR(X.MSG$) External
String X.Msg$
End Sub 

!--------------
!--- Bloque Principal
!--------------

Sub DOMIMS02.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS02.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Dom.Modul% Then Begin 

	 Asc.Dom.Num$ = ""
   Asc.Num.Ped% = 0

EndIf 

End Sub 

Sub DOMIMS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

TS.ER.RETURN = -1																													! Ctrl Errores
Open "R::ASCNTRL" AS 94																										! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 Then Begin																					! Error en la apertura
 Call TSHIECET																														! Tono de Alerta
 Call Visores4690(1,"ERROR PARAMETROS","MODULO DOMICILIOS  ",1500,"L")		! Msg Alerta
 Call Visores4690(1,"MODULO DOMICILIOS  ","NO OPERATIVO",1500,"L")				! Msg Alerta
 Call U.Imprime("ERROR CARGA MODULO DOMICILIOS   ",2100H)									! Rastro en SJ
 Asc.Dom.Motora%    = 0
 Asc.Dom.Num$       = ""
 Asc.Num.Ped%       = 0 
EndIf Else Begin									      																	! Apertura OK
 If End #94 Then UE.DOM.FINAL		       																	  ! Si es EOF
  While (1)															  																! Recorre archivo
    Read #94; TS.TEMP1$			       																				! Lectura registro
    IF TS.TEMP1$ = "[DOMICILIOS]" Then Begin									      		  ! Proyecto de Lealtad
     Read #94; TS.TEMP1$																									! Lectura registro
     Asc.Dom.Modul%     = Val(Mid$(TS.TEMP1$,30,2))				    						! Proyecto activo
     Read #94; TS.TEMP1$                    															! Lectura registro
     Asc.Dom.Motora%    = Val(Mid$(TS.TEMP1$,30,3))		    								! Tecla motora proyecto
	   Asc.Dom.Num$ = ""
     Asc.Num.Ped% = 0     
    EndIf 
  Wend
 UE.DOM.FINAL:
    Close 94																															! Cierra archivo
   If Asc.Dom.Modul% = -1 Then                                               \! Proyecto Activo
      Call U.IMPRIME("MODULO DOMICILIOS      ON 30-Ago-2011",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO DOMICILIOS     OFF 30/Ago/2011",2100H)           ! Msg Proyecto Cargado

EndIf
      
End Sub 

Sub DOMIMS14.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS14.011                                         **
!   **    Incluir en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Dom.Modul% Then Begin 

 If TS.IO.MOTORKEY = Asc.Dom.Motora% Then Begin 
   Asc.Num.Ped% = 0 
   Asc.Dom.Num$ = ASIC.DATOS$("INGRESE NUMERO DEL","DOMICILIO")                  ! Captura numero domicilio
   If (Asc.Dom.Num$ = "") OR (Asc.Dom.Num$ = "E") THEN BEGIN		   					     ! No se capturo dato
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)																	 ! Init vectores de aplicacion
       TS.IO.MOTORKEY = 0 
   EndIf Else Begin 
     Asc.Num.Ped% = -1 
     Call VISOR.AND.BORRAR("PEDIDO INGRESADO    "+Asc.Dom.Num$)                  ! Presenta numero Pedido
     TS.TEMP2$ = Pack$(Asc.Dom.Num$)+":"+Pack$("00")                             ! Dato domicilio
     Call Grabacion.Cadena.Usuario("20060104",TS.TEMP2$)                         ! Graba String usuario
     Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)
     TS.IO.MOTORKEY = 73
   EndIf
EndIf 

	 
EndIf 

End Sub 

Sub DOMIMS20.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS20.011                                         **
!   **    Incluir en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Dom.Modul% Then Begin 
 If TS.INTRX Then                                  \! Si esta en una compra
  If (TS.LINETYPE = 6) AND (TS.LINEDATA = 1) AND   \! Impresion Numero Pedido
    Asc.Num.Ped% <> 0 Then Begin      
    Asc.Num.Ped% = 0 
    Call U.IMPRIME(STRING$(38,"-"),6200H)    
    Call U.IMPRIME("**  DATOS DEL PEDIDO DE DOMICILIO  **",6100H)
    Call U.IMPRIME("PEDIDO :"+Asc.Dom.Num$,6100H)
    Call U.IMPRIME(STRING$(38,"-"),6100H)    
  EndIf


EndIf 

End Sub 

Sub DOMIMS53.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS53.011                                         **
!   **    Incluir en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Dom.Modul% Then Begin 
	 
 If UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin   	    	            ! Si DATA/ENTRY
  If UNPACK$(MID$(SL.STR.ENTRY$,3,4)) = "20060104" Then Begin               ! Modulo Domicilios
	  Asc.Tmp.Apun% = 3																												! Apuntador String
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)						  		  ! Numero del proyecto
    Asc.Dom.Num$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)				  		  ! Numero del pedido
    Asc.Num.Ped% = -1                                                       ! Control Impresion
  EndIf 	 
 EndIf 

EndIf 
End Sub 

Sub DOMIMS56.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Pago Electronico                                     **
!   **    User Exit  :  DOMIMS56.011                                         **
!   **    Incluir en :  EAMTSU56.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Dom.Modul% Then Begin 
	 
	 Asc.Dom.Num$ = ""
   Asc.Num.Ped% = 0
	 
EndIf 

End Sub 

