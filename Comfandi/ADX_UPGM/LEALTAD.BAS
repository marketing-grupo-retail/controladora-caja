!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LEALTAD.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo de Lealtad en Linea       *
!**************************************************
!* Modificaciones:
!Mod 01Mzo2012
! Se controla el proceso de recuperación de cliente
! por procesos de caida o suspension de transaccion
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 27/Nov/2014
! Se controla para la grabación de String de usuario,
! si existe un cliente capturado en la transacción y
! se trata de capturar otro cliente pero este proceso
! falla por no existencia, no elimine la cedula ya 
! capturada y almacene la cedula correcta para 
! almacenar y de esta manera acumular bien el puntaje
! acumulado.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 16/Dic/2014
! Se adiciona la presentación de las tipificaciones
! del cliente en pantalla del cajero y en rollo de 
! auditoria, para manejo de aplicación de descuentos
! y beneficios.
! Solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 19Oct2021
! Se modifica el servicio de consulta de cliente
! para cargar datos adicionales de cliente para 
! factura electronica.
! Se reemplaza la funcion 1501 para consulta de cliente
! funcion 1511 - Consulta cliente
! funcion 1512 - Extension consulta cliente
! Solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 01Jun2022
! Acondicionamiento modulo lealtad de cliente para 
! integración servicios de LEAL
! Solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 23Ene2023
! cambio de invocación funcion a Leal para manejo
! de tipo de cliente fiscal
! Solicitado por Comfandi
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 02May2024
! Se retira tipificación VF default para clientes.
! solicitado por Comfandi para problemas de beneficios 
! para clientes consumidor final
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON T		                 																							! Ambiente de terminal

Integer*4 Global UE.RECASRV.QTY%  																					! Control recuados
Integer*1 Global Gr.Mrm.Cap%      																					! Control de remisiones
Integer*1 Global Gr.Ptecho.InTrx% 																					! Control precio techo
Integer*1 Global Gr.Fis.Doc%																								! Control numeracion fiscal
Integer*1 Global Gr.FeNat.Aplicado%																					! Control captura cliente fiscal
Integer*1 Global Gr.NtaCrd.InTrx%																						! Notas credito 
String    Global Gr.NtaCrd.TipClte$																					! Tipo Clte fiscal factura origen

%INCLUDE EAMTSWKG.J86          																							! working storage                   
%INCLUDE EAMTRANS.j86                                                       ! Variables procesos de transaccion 
%INCLUDE EAMTERMS.J86                                                       ! Variables procesos terminal       
%INCLUDE EAMTOPTS.J86	                                                      ! Variables Terminal Options        
%INCLUDE EAMTSEVA.J86                                                       ! Variables Cliente Frecuente       
%INCLUDE LCLTSUVA.011	                                                      ! Variables desarrollo              
%INCLUDE EAMTSXHC.J86                                                       !                                   
%INCLUDE EAMASMRT.J86                                                       !                                   

Function SAVE.FB.PRN  External                                              ! Salvar variables impresion
End Function

Function RESTORE.FB.PRN  External                                           ! Restaura variables de impresion
End Function

Sub TSPREC01  External																											! Rutina de impresion SMA
End Sub

Sub TSPREC03  External																											! Rutima de MSG SMA
End Sub

Sub TSHIECET External																												! Tono de alerta
End Sub

Sub TSCSECRK External																												! Captura teclado
End Sub								

Sub CLEAR.VARS External																											! Limpieza de Variables
End Sub 

Sub TRADUCE.ERROR(ERRVALUE%,ERRVALUE$) External															! Traduccion Error Aplicacion
  Integer*4 ERRVALUE%
  String ERRVALUE$
End Sub

Function FORMAT.AMOUNT(AMT1) External 																			! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
End Function

%INCLUDE RECATSSU.011        																								! Rutinas Genericas

Sub LEALTAD.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) Public 						!
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$,      \!
       X.RTA$, X.BUFF$																											!
Integer*4 X.Len%																														!
			TS.ER.RETURN = -1																											!
			X.FILE$ = "R::ADX_UDT1:LC" + Left$(DATE$,6) + "." +                  \!
			          Right$("000"+Str$(SL.HD.TERMINAL),3)												!
			Open X.FILE$ AS 56 AppEnd																							!
			If TS.ER.RETURN <> -1 Then Begin    																	! Si no existe
			   TS.ER.RETURN = -1																									!
				 CREATE X.FILE$ AS 56																								!
         If TS.ER.RETURN <> -1 Then Begin 																	!
            Call VISORES4690(1,"ERROR EN CREACION","DE AUDITORIA ",1500,"L")!
            Exit Sub 																												!
         EndIf 																															!
			EndIf 																																!
			X.Finr$ = Chr$(13) + Chr$(10)																					!
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$															!
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56																															!
End Sub 																																		!

Sub Validacion.Local.Vfiel Public                                           ! Validacion local cliente
String Xtmp$                                                       			    !

!   USER.FBACT.READ = 0																									    ! Cliente no capturado            
!   TS.TEMP1I4    = 00				 																					    ! Proceso No Satisfactorio
!   Gr.Lcl.Online% = 0
!   Exit Sub 																																!
    Call VISOR.AND.BORRAR("FALLA VALIDACION    CLIENTES /Borrar")						! Msg alerta
    Call TSHIECET																														! Tono alerta
    Xtmp$ = ASIC.DATOS$("ASIGNA CONSUMIDOR","FINAL 1.SI 0.NO")							! 
    If Xtmp$ <> "1" Then BEGIN														    							! Proceso cancelado
 	     Call VISOR.AND.BORRAR("PROCEDIMIENTO CANCELADO")								      !
       TS.TEMP1I4    = 00				 																					  ! Proceso No Satisfactorio
       Gr.Lcl.Online% = 0																										!
       Exit Sub 																														!
    EndIf 																														      !
    GR.LCL.PTO1%   = 0 																								      ! Puntos Acumulados
    GR.LCL.PTO2%   = 0 																								      ! Puntos Redimidos
    GR.LCL.PTO3%   = 0 																								      ! Puntos Punto del periodo
    GR.LCL.PTO4%   = 0 																								      ! Puntos proximos a vencer
    Gr.Lcl.Address$ = "Carrera 23 No 26b-46"                        				! Direccion del cliente
    Gr.Lcl.Phone$   = "6023340000"                        									! Telefono del cliente
    Gr.Lcl.IdClte$  = "1"                        													  ! Tipo ID Cliente     
    Gr.Lcl.eMail$ = "ConsumirFinal@comfandi.com.co"													! Correo Electronico
    Gr.Lcl.City$  = "CALI" 											  													! Ciudad
    Gr.Lcl.Tipifica$ = ""																									  ! Tipificacion vecino fiel
    Gr.Lcl.ClteTmp$ = "222222222222"																				! Consumidor final
    Gr.Lcl.Name$     = "CONSUMIDOR"																					! Nombre 
    Gr.Lcl.SurNames$ = "FINAL"																							! Apellido cliente
    TS.TEMP1I4    = -1			 																					      ! Proceso Satisfactorio
    Gr.Lcl.Online% = -1																											!

End Sub 																																		!

Sub Valida.Rta.Lealtad Public 																							! Valida Cliente Online
String Xtemp4$, Xlista$, Xpuntos$, Xtmp1$																		!
Integer*1 XI%
       XTEMP4$ = Valida.Rta(Gr.Lcl.Tmp2$)	         													! Valida rta entregada 
       If XTEMP4$ = "00" Then Begin 		          													! Si proceso OK
        If Mid$(Gr.Lcl.Tmp2$,12,2) = "00" Then Begin                        ! Encontro Cliente
          GR.LCL.PTO1%   = 0 																								! Puntos Acumulados
          GR.LCL.PTO2%   = 0 																								! Puntos Redimidos
          GR.LCL.PTO3%   = 0 																								! Puntos Punto del periodo
          GR.LCL.PTO4%   = 0 																								! Puntos proximos a vencer
!--- Respuesta mensajeria Leal
          Gr.Lcl.UID$    = Mid$(Gr.Lcl.Tmp2$, 54,32)												! UID reportado por LEAL
          Xpuntos$       = Mid$(Gr.Lcl.Tmp2$, 86,10)										    ! Puntaje acumulado reportado por Leal
          Xtmp1$ = ""
          For XI% = 1 To Len(Xpuntos$)
            If Mid$(Xpuntos$,XI%,1) <> " " Then                            \! Toma puntaje
            	 Xtmp1$ = Xtmp1$ + Mid$(Xpuntos$,XI%,1)                       ! 
          Next XI%
          GR.LCL.PTO1%   = Val(Xtmp1$)              										    ! Puntaje acumulado reportado por Leal
          Gr.Lcl.Name$   = Mid$(Gr.Lcl.Tmp2$, 96,20)     										! Nombre del cliente
          Gr.Lcl.surnames$ = Mid$(Gr.Lcl.Tmp2$,116,20)      								! Apellidos cliente
          Gr.Lcl.TpClte$ = Mid$(Gr.Lcl.Tmp2$,136,2)      										! Tipo de Cliente
          Gr.Lcl.TipClte$ = ""
          Gr.Lcl.Tipifica$ = ""
          If Gr.Lcl.ClteTmp$ <> "222222222222" Then                        \! Cliente leal
             Xlista$ = Mid$(Gr.Lcl.Tmp2$,138,30) Else Xlista$ = ""			    ! Tipificacion de clientes
          Gr.Lcl.Address$ = Mid$(Gr.Lcl.Tmp2$,168,24)                       ! Direccion del cliente
          Gr.Lcl.Phone$   = Mid$(Gr.Lcl.Tmp2$,192,13)                       ! Telefono del cliente
          Gr.Lcl.IdClte$  = Mid$(Gr.Lcl.Tmp2$,205,2)                        ! Tipo ID Cliente     
          Gr.Lcl.IdClte$  = Str$(Val(Gr.Lcl.IdClte$))                       ! Ajusta dato
          Gr.Lcl.eMail$ = Mid$(Gr.Lcl.Tmp2$,207,50)                         ! Correo electronico
          Gr.Lcl.City$  = Mid$(Gr.Lcl.Tmp2$,257,15) 											  ! Ciudad
          XI% = 0
          For XI% = 1 To Len(Xlista$) STEP 2
           If Mid$(Xlista$,XI%,2) <> "  " Then Begin
              Gr.Lcl.TipClte$ = Gr.Lcl.TipClte$ + ("TC"+Mid$(Xlista$,XI%,2))
              Gr.Lcl.Tipifica$ = Gr.Lcl.Tipifica$ + "/"+Mid$(Xlista$,XI%,2)

!             If Mid$(Xlista$,XI%,2) <> "VF" Then \
!              Gr.Lcl.Tipifica$ = Gr.Lcl.Tipifica$ + "/"+Mid$(Xlista$,XI%,2)
           EndIf
          Next XI%
          TS.TEMP1I4 = -1
          Gr.Lcl.Online% = -1
        EndIf Else Begin 
         XTEMP4$ = Mid$(Gr.Lcl.Tmp2$,14,40)             										! Msg de Error 
         Call VISOR.AND.BORRAR(Xtemp4$)  																	  ! Despliega mensaje de Error
         If Match("CLIENTE NO EXISTE",XTEMP4$,1) > 0 Then Begin             ! Si cliente no existe en BD
         	  Call U.IMPRIME("CLIENTE NO EXISTE "+XTEMP4$,2100H)
            GR.LCL.PTO1%  = 0
            GR.LCL.PTO2%  = 0
            GR.LCL.PTO3%  = 0
            GR.LCL.PTO4%  = 0
            Gr.Lcl.Name$  = " "
            Gr.Lcl.TpClte$ = " "
            Gr.Lcl.TipClte$ = ""
            Gr.Lcl.Tipifica$ = ""
            Gr.Lcl.Clte$ = ""
            Gr.Lcl.eMail$ = ""
            Gr.Lcl.City$  = ""
            USER.FBACT.READ = 0																							! Cliente no capturado            
            TS.TEMP1I4    = 00				 																			! Proceso cancelado
            Gr.Lcl.Online% = 0
         EndIf Else Begin 																									! Falla proceso consulta
          Call Validacion.Local.Vfiel                                       ! Realiza busqueda local de cliente                       
         EndIf
        EndIf                                                               ! 
      EndIf Else Begin                                                      ! Error en proceso de comunicacion
      	Call Validacion.Local.Vfiel                                         ! Realiza busqueda local de cliente                       
      EndIf 
End Sub 

Sub Consulta.Cliente.Online Public
String Gr.Clte$, Xsnd$, Xfin$
   GR.CLTE$ = "CC" + Right$("                  "+Gr.Lcl.ClteTmp$,18)	 			!
   TS.TEMP1I4    = 0
   Xsnd$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Gr.Lcl.Tmp1$  = Armar.Trama.Msg(Gr.Lcl.Appl$,"20",GR.CLTE$,             \! Cambio funcion 19 a 20 Mod 12Jul2024
                                "00",Gr.Lcl.Cadena$,"123456")               ! Armar trama MSG
   !Gr.Lcl.Tmp2$  = Rutina.Java("com.appl.ApplKernel","threader",Gr.Lcl.Tmp1$)! Ejecuta Requerimiento
   
Gr.Lcl.Tmp2$  = "1519249200000CONSULTA SATISFACTORIA                  A35231G3GD02BCB4G194CG430CA60GE42224      OSCAR               "
Gr.Lcl.Tmp2$  = Gr.Lcl.Tmp2$ + "VALENCIA SARMIENTO  A VFVFAABSMEMBMM                CR 15 84A 24            3014568307   1 oscar.valencia@gruporetail.com     BOGOTA         "
!-- Nueva trama de respuesta
   Xfin$ = DATE$ +":"+ Time$                                                ! Fecha y hora rta del requerimiento
   Call LEALTAD.AUDITORIA(Gr.Lcl.Tmp1$,Gr.Lcl.Tmp2$, Xsnd$, Xfin$)          ! Rastreo movimiento
   Call Valida.Rta.Lealtad						       																! Valida la respuesta
End Sub 

!--- Invocacion rutinas de terminal

Sub LCFTSU02.011 PUBLIC
If Asc.Proy.Leal% = -1  THEN BEGIN                                         	! Proyecto activo
  %INCLUDE LCLTSU02.011
EndIf

End Sub

Sub LCFTSU07.011 PUBLIC

%INCLUDE LCLTSU07.011

End Sub

Sub LCFTSU14.011 PUBLIC
String GR.CLTE$
IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSU14.011

EndIf

End Sub

Sub LCFTSU20.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSU20.011

EndIf

End Sub

Sub LCFTSU53.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo
   %INCLUDE LCLTSU53.011
EndIf

End Sub

Sub LCFTSU56.011 PUBLIC

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo
   %INCLUDE LCLTSU02.011
EndIf

End Sub

Sub LCFTSE14.011 PUBLIC
String GR.CLTE$

If Asc.Proy.Leal% = -1  Then Begin                                         		! Proyecto activo

   %INCLUDE LCLTSE14.011

EndIf

End Sub

Sub LCFTSE05.011 Public

IF Asc.Proy.Leal% = -1  THEN BEGIN                                         		! Proyecto activo

%INCLUDE LCLTSE05.011

EndIf
End Sub 

