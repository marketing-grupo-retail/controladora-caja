!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LEALTAD.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo de Lealtad en Linea       *
!**************************************************
!* Modificaciones:
!
!

%ENVIRON T		                 ! Ambiente de terminal
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE EAMTOPTS.J86	         ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE LCLTSUVA.011	         ! Variables desarrollo
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !

Function SAVE.FB.PRN  External                                               	! Salvar variables impresion
End Function

Function RESTORE.FB.PRN  External                                             ! Restaura variables de impresion
End Function

Sub TSPREC01  External																												! Rutina de impresion SMA
End Sub

Sub TSPREC03  External																												! Rutima de MSG SMA
End Sub

Sub TSHIECET External																													! Tono de alerta
End Sub

Sub TSCSECRK External																													! Captura teclado
End Sub								

Sub CLEAR.VARS External																												! Limpieza de Variables
End Sub 

Sub TRADUCE.ERROR(ERRVALUE%,ERRVALUE$) External																! Traduccion Error Aplicacion
  Integer*4 ERRVALUE%
  String ERRVALUE$
End Sub

Function FORMAT.AMOUNT(AMT1) External 																				! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

%INCLUDE RECATSSU.011        																									!Rutinas Genericas

!------------- Fin definicion rutinas comunes

Sub Validacion.Local.Vfiel Public                                             ! Validacion local cliente
String U.Clte$, Xkey$, Xapell$, Xname$, Xpto$, Xtipo$													!
   U.Clte$ = PACK$(Right$("000000000000000000"+Gr.Lcl.Clte$,18))							! Arma llave de busqueda
   TS.ER.RETURN = -1																													! Control de errores
   Read Form "C9 2C20 C5 C1";#7 KEY U.Clte$;  																\ Lee registro
          XKey$ ,                             																\ Identificacion del cliente UPD 9
          XApell$,                            																\ Apellidos del cliente
          XName$,                             																\ Nombre del cliente
          Xpto$,                              																\ Puntos Totales del cliente
          Xtipo$                              																! Tipo de cliente
   If TS.ER.RETURN <> -1 Then Begin                                           ! No encontrado localmente
   	  Call VISOR.AND.BORRAR("CLIENTE NO EXISTE")															! Msg de alerta al cajero
      GR.LCL.PTO1%  = 0
      GR.LCL.PTO2%  = 0
      GR.LCL.PTO3%  = 0
      GR.LCL.PTO4%  = 0
      Gr.Lcl.Name$  = " "
      Gr.Lcl.TpClte$ = " "
      TS.TEMP1I4    = 00				 																					    ! Proceso No Satisfactorio
   EndIf Else Begin																														!
      GR.LCL.PTO1%  = Val(UnPack$(Xpto$))																			! Puntos Acumulados
      GR.LCL.PTO2%  = 0                                   										! Puntos Redimidos
      GR.LCL.PTO3%  = 0 																											! Puntos Punto del periodo
      GR.LCL.PTO4%  = 0 																											! Puntos proximos a vencer
      Gr.Lcl.Name$  = Left$(Xname$,10)+" "+Left$(XApell$,10) 									! Nombre del cliente
      Gr.Lcl.TpClte$= Xtipo$                        													! Tipo de Cliente
      TS.TEMP1I4    = -1				 																							! Proceso Satisfactorio
   EndIf 
End Sub 

Sub Valida.Rta.Lealtad Public 																								! Valida Cliente Online
String Xtemp4$																																!
       XTEMP4$ = Valida.Rta(Gr.Lcl.Tmp2$)	         														! Valida rta entregada 
       If Val(XTEMP4$)  = 0 Then Begin 		         														! Si proceso OK
        If Mid$(Gr.Lcl.Tmp2$,12,2) = "00" Then Begin                          ! Encontro Cliente
          GR.LCL.PTO1%  = Val(Mid$(Gr.Lcl.Tmp2$,54,10)) 											! Puntos Acumulados
          GR.LCL.PTO2%  = Val(Mid$(Gr.Lcl.Tmp2$,64,10)) 											! Puntos Redimidos
          GR.LCL.PTO3%  = Val(Mid$(Gr.Lcl.Tmp2$,74,10)) 											! Puntos Punto del periodo
          GR.LCL.PTO4%  = Val(Mid$(Gr.Lcl.Tmp2$,84,10)) 											! Puntos proximos a vencer
          Gr.Lcl.Name$  = Mid$(Gr.Lcl.Tmp2$,94,40)      											! Nombre del cliente
          Gr.Lcl.TpClte$= Mid$(Gr.Lcl.Tmp2$,134,1)      											! Tipo de Cliente
          TS.TEMP1I4    = -1				 																					! Proceso Satisfactorio
        EndIf Else Begin 
         XTEMP4$ = Mid$(Gr.Lcl.Tmp2$,14,40)             											! Msg de Error 
         Call VISOR.AND.BORRAR(Xtemp4$) 	         														! Despliega mensaje de Error
         GR.LCL.PTO1%  = 0
         GR.LCL.PTO2%  = 0
         GR.LCL.PTO3%  = 0
         GR.LCL.PTO4%  = 0
         Gr.Lcl.Name$  = " "
         Gr.Lcl.TpClte$ = " "
         TS.TEMP1I4    = 00				 																					  ! Proceso Satisfactorio
        EndIf                                                                 ! 
      EndIf Else Begin                                                        ! Error en proceso de comunicacion
      	Call Validacion.Local.Vfiel                                           ! Realiza busqueda local de cliente                       
      EndIf 
End Sub 

Sub Consulta.Cliente.Online Public
String Gr.Clte$
   Call Validacion.Local.Vfiel                                           ! Realiza busqueda local de cliente                       
   Exit Sub 
   GR.CLTE$ = Right$("                  "+Gr.Lcl.Clte$,18)									  !
   TS.TEMP1I4    = 0
   Gr.Lcl.Tmp1$  = Armar.Trama.Msg(Gr.Lcl.Appl$,"01",GR.CLTE$,               \!
                                "00",Gr.Lcl.Cadena$,"123456")                 ! Armar trama MSG
   Gr.Lcl.Tmp2$  = Rutina.Java("com.appl.ApplKernel","threader",Gr.Lcl.Tmp1$) ! Ejecuta Requerimiento
   Call Valida.Rta.Lealtad						       																	! Valida la respuesta
   
   !Gr.Lcl.Tmp2$ = "1501128200000CONSULTA DE PUNTOS SATISFACTORIA        0         0         0         0         MARIA DEL ROSARIO MU¥OZ                 N"
   
End Sub 


!--- Invocacion rutinas de terminal

Sub LCFTSU02.011 PUBLIC
If Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo
  %INCLUDE LCLTSU02.011
ENDIF

End Sub

Sub LCFTSU07.011 PUBLIC

%INCLUDE LCLTSU07.011

End Sub

Sub LCFTSU14.011 PUBLIC
String GR.CLTE$
IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSU14.011

ENDIF

End Sub

Sub LCFTSU20.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSU20.011

EndIf

END Sub

Sub LCFTSU53.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo
   %INCLUDE LCLTSU53.011
ENDIF

END Sub

Sub LCFTSU56.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo
   %INCLUDE LCLTSU02.011
ENDIF

END Sub

Sub LCFTSE14.011 PUBLIC
String GR.CLTE$

If Asc.Proy.Leal% = -1  Then Begin                                         		! Proyecto activo

   %INCLUDE LCLTSE14.011

EndIf

END Sub

Sub LCFTSE05.011 Public

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSE05.011

ENDIF
End Sub 
