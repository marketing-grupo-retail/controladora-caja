!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : PTOMEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Abril de  2006                   *
!Observaciones : Modulo Puntos Adicionales        *
!**************************************************
!

%ENVIRON T

Integer*1 Global UE.DOUB.SGN%, UE.DOUB.PTJE%, Asc.Ptos.Line%, Asc.Ptos.Sesion%
Integer*4 Global U.DOUB.PNTOS%, U.DOUB.AMTDBL%
String    Global Asc.Ptos.Item$, Asc.Ptos.Indicat2$

%Include EAMTSEVA.J86                 ! Definicion de variables EMS
%INCLUDE EAMITEMR.J86                 ! Variables maestra de articulos 
%INCLUDE ASCITEMR.011                 ! Variables maestra de articulos 
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86 

Function FORMAT.AMOUNT(AMT1) External
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
End Function

Function Grabacion.Cadena.Usuario(X.Clave$,X.Datos$) External 
String X.Clave$, X.Datos$
End Function 

Function U.IMPRIME(UE.STRING$,UE.STATION) External     				! Rutina de Impresion
String UE.STRING$      											                  ! Variables temporales
Integer*2 UE.STATION											                    !
End Function 

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External		! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$								! Variables de
Integer*2 U.d.visor%, U.d.tiempo%, u.t.visor%, ud.pos%							! trabajo
String U.D.Linea1$, U.D.Linea2$										!
End FUNCTION												!
!--- Fin despliege de mensajes a los visores 



Sub PTOMTS02.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Recaudo de colegios                                  **
!   **    User Exit  :  PTOMTS02.011                                         **
!   **    Incluir en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Ptos.Line% Then Begin 

   USER.FBACT.READ = 0
   U.DOUB.AMTDBL% = 0
   U.DOUB.PNTOS% = 0
   Asc.Ptos.Indicat2$ = Pack$("00")

EndIf 

End Sub 

Sub PTOMTS07.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  PTOMTS07.011                                         **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

Asc.Ptos.Indicat2$ = Pack$("00")
TS.ER.RETURN = -1																													! Ctrl Errores
OPEN "R::ASCNTRL" AS 94																										! Apertura archivo parametrizacion
If TS.ER.RETURN <> -1 THEN BEGIN																					! Error en la apertura
  Call Visores4690(1,"ERROR PARAMETROS","PUNTOS ADICIONALES ",1500,"L")		! Msg Alerta
 Call Visores4690(1,"PUNTOS ADICIONALES ","NO OPERATIVO",1500,"L")				! Msg Alerta
 Call U.Imprime("ERROR CARGA PUNTOS ADICIONALES  ",2100H)									! Rastro en SJ
 Asc.Ptos.Line%      = 00 																									! Proyecto desactivado
EndIf Else Begin									      																	! Apertura OK
 IF END #94 THEN UE.PTOS.FINAL		       																	  ! Si es EOF
  While (1)															  																! Recorre archivo
    Read #94; TS.TEMP1$			       																				! Lectura registro
    If TS.TEMP1$ = "[PUNTOS ADICIONALES]" Then Begin										  ! Proyecto de Lealtad
     Call U.Imprime("PUNTOS ADICIONALES OK.",2100H)										    ! Mensaje en SJ
     Read #94; TS.TEMP1$																									! Lectura registro
     Asc.Ptos.Line%     = Val(Mid$(TS.TEMP1$,30,2))				    						! Proyecto activo
     Read #94; TS.TEMP1$																									! Lectura registro
     Asc.Ptos.Sesion%     = Val(Mid$(TS.TEMP1$,30,2))				    			  	! Sesion Temporal
    EndIf 
  Wend
  UE.PTOS.FINAL:
    Close 94																																! Cierra archivo
Endif
   If Asc.Ptos.Line% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO PTOS ADICIONAL  ON 30/Ago/2011",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PTOS ADICIONAL OFF 30/Ago/2011",2100H)           ! Msg Proyecto Cargado

End Sub 


Sub PTOMTS08.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  PTOMTS60.011                                         **
!   **    Incluir en :  EAMTSU60.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If USER.FBACT.READ Then Begin
   TS.ER.RETURN = -1 
   Asc.ITEMCODE$ = IR.ITEMCODE$
   %INCLUDE EAMIRRD4.J86
   If Unpack$(ASC.INDICAT2$) = "05" Then Begin ! Codigo de barras
      Asc.Ptos.Item$     = ASC.SALEQUAN$ + ASC.SALEPRIC$
   EndIf Else Begin 
      Asc.Ptos.Item$ = IR.ITEMCODE$
   EndIf 

EndIf

End Sub 


Sub PTOMTS60.011  Public 
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  PTOMTS60.011                                         **
!   **    Incluir en :  EAMTSU60.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
String Xkey$, XTMP$
If Asc.Ptos.Line% Then Begin 

If USER.FBACT.READ Then \
If TS.LINETYPE = 1 AND SL.IT.INDICAT3A <> 7 Then Begin        ! Si articulo y no cupon
 If (TS.LINEDATA <> 1 ) OR (RIGHT$(TS.PRTBUF$,4)<>"    ") Then Begin
    TS.ER.RETURN = -1 
    Open "R::IRPUNTOS" Keyed Recl 7 As Asc.Ptos.Sesion%
    If TS.ER.RETURN NE -1 Then Begin          ! Error de apertura
       Call U.Imprime("ERROR APERTURA IRPUNTOS",2100H)
       U.DOUB.PNTOS%  = 0 
       U.DOUB.AMTDBL% = 0 
       Exit Sub                               ! Sale de la rutina
    EndIf 
    XKEY$ = Asc.Ptos.Item$
    Read Form "C6 C1"; #Asc.Ptos.Sesion% Key XKEY$ ; \
         XKEY$, XTMP$
    If TS.ER.RETURN = -1 Then Begin                   ! Lo encontro
       UE.DOUB.PTJE% = Val(XTMP$) 
 		   UE.DOUB.SGN% = 1
		   If SL.IT.INDICAT2 AND 80H Then UE.DOUB.SGN% = -1
		   If SL.IT.INDICAT2 AND 40H Then UE.DOUB.SGN% = -1 
		   U.DOUB.PNTOS% = UE.DOUB.SGN% * SL.IT.XPRICE
		   U.DOUB.PNTOS% = U.DOUB.PNTOS% * (UE.DOUB.PTJE% - 1)
		   U.DOUB.PNTOS% = Round(FLOAT(U.DOUB.PNTOS%)*FLOAT(OP.PPAMT)/FLOAT(OP.AMTPP),0,0)
		   U.DOUB.AMTDBL%  = U.DOUB.AMTDBL% + U.DOUB.PNTOS%
		   Call FORMAT.AMOUNT(	U.DOUB.PNTOS% )
       Call U.Imprime("     PUNTOS ADICIONALES "+TS.TEMP1$,6100H)
		EndIf Else U.DOUB.PNTOS% = 0 
		Close Asc.Ptos.Sesion%
 EndIf
 EndIf

EndIf 

End Sub 


Sub PTOMTS68.011(XB$)  Public 
String XB$
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2004                                        **
!   **    Proyecto   :  Devoluciones en Linea                                **
!   **    User Exit  :  PTOMTS53.011                                         **
!   **    Incluir en :  EAMTSU53.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************

If Asc.Ptos.Line% Then Begin 

    IF UNPACK$(LEFT$(XB$,1))  = "11" AND 		\ Data String
      UNPACK$(MID$(XB$,3,1)) = ">>" THEN BEGIN     ! Preferred Customer Transaction Data String
      I% = MATCH(":",XB$,5)
      IF I% > 5 THEN BEGIN
         J% = MATCH(":",XB$,I%+1)
      ENDIF
      IF OP.AMTPP <> 0 AND J%-I% > 0 THEN BEGIN
         EMSS.TR.POINTS = EMSS.TR.POINTS + U.DOUB.AMTDBL%			! PUNTOS - PTOS x NOTAS CAMBIO  	
			   TS.TEMP1$ =  Pack$(Right$("0000000000"+Str$(U.DOUB.AMTDBL%),10)) + \! Puntos Adicionales 
											":"+Pack$("00")
					Call Grabacion.Cadena.Usuario("20060425",TS.TEMP1$)                        ! Graba String usuario
      ENDIF
   ENDIF

EndIf 

End Sub 
