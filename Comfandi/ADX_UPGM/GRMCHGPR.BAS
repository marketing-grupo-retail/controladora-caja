!**************************************************
!Empresa       : Grupo Retail Ltda - Colombia     *
!Programa      : GRCHGPRC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Control Cambio Precio     *
!**************************************************
! Modificaciones:
! Mod 15Oct2021
! Se monta control para que productos con precio
! cero en la maestra no ejecute el procedimiento
! IR.PRICE1 = 0 
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------
! Mod 25Ene2022
! Se ajusta proceso para calculo de la diferencia
! de precio para evitar basura en el momento de la 
! captura de la secuencia de clave de autorizacion
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------

%ENVIRON T

Integer*1 Global Gr.ChgPrc.Act%,																					 \! Control proyecto
                 Gr.ChgPrc.Proc%,                                          \!
                 Gr.Fis.Domictv%                                            ! Domicilios tienda virtual
Integer*4 Global Gr.ChgPrc.Itm%	  																					! Precio venta producto
String    Global Gr.ChgPrc.Niv$(2),                                        \! Niveles de precio
                 Gr.ChgPrc.Con$(1),                                        \! Conceptos cambio precio
                 Gr.ChgPrc.Usr$(2),                                        \! Usuarios autorizados por nivel
                 Gr.Fis.DomiPrc$                                            ! Precio domicilio tienda virtual

%INCLUDE ADX_UPGM:EAMTSCVA.J86
%INCLUDE ADX_UPGM:EAMTSWKG.J86                                              ! working storage                 
%INCLUDE ADX_UPGM:EAMTRANS.j86                                              !                                 
%INCLUDE ADX_UPGM:EAMTERMS.J86                                              ! 
%INCLUDE ADX_UPGM:EAMITEMR.J86                                              ! 
%INCLUDE ADX_UPGM:EAMTOPTS.J86	                                            ! Variables de la terminal options
%INCLUDE ADX_UPGM:EAMTSXHC.J86                                              ! Rutinas de Assembler            
%INCLUDE ADX_UPGM:EAMASMRT.J86                                              ! Rutinas de Assembler            
                                                                                                              
%INCLUDE RECATSSU.011          					                                    ! RUTINAS GENERICAS APLICACION    

Sub TSCSECRK External       																								! READ KEYBOARD
End Sub

Sub TSHIECET External 
End Sub 

Sub Carga.Param.ChgPrc																											! Carga parametros cambio precio
Integer*1 XI%
String Xkey$,Xdat1$, Xdat2$, Xdat3$, Xdat4$
  Dim Gr.ChgPrc.Niv$(11,2)																									! Lista niveles de precio Min/Max
  Dim Gr.ChgPrc.Usr$(20,2)																									! Usuarios autorizados cambio precio
  Dim Gr.ChgPrc.Con$(50)																										! Conceptos cambio precio
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::TF:NIVPRC" KEYED RECL 11 As 94 UNLOCKED NOWRITE NODEL            ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR OPEN TF:NIVPRC /Borrar")                  !
  	 Call U.IMPRIME("ERROR OPEN TF:NIVPRC "+TS.TS11WERR$,2100H)
	   Gr.ChgPrc.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  For XI% = 1 To 10																													! Carga Lista niveles precio
    TS.ER.RETURN = -1
    Xkey$ = Pack$(Right$("00"+Str$(XI%),2))
    Read Form "C1 2C5";#94 Key Xkey$;                  							       \! Lee Reg Cabecera 
       Xdat1$, Xdat2$, Xdat3$																								! Parametros limite cambio precio
    If TS.ER.RETURN = -1 Then Begin																					! Registro encontrado
    	 Gr.ChgPrc.Niv$(XI%,0) = UnPack$(Xdat2$)															! Monto Minimo
    	 Gr.ChgPrc.Niv$(XI%,1) = UnPack$(Xdat3$)															! Monto Maximo
    EndIf																																		!
  Next XI%																																	! Fin carla lista niveles precio
  Close 94																																	! Cierre archivo
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::TF:AUTPRC" KEYED RECL 12 As 94 UNLOCKED NOWRITE NODEL				    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR OPEN TF:AUTPRC /Borrar")                  !
  	 Call U.IMPRIME("ERROR OPEN TF:AUTPRC "+TS.TS11WERR$,2100H)
	   Gr.ChgPrc.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 																																		! fin carga parametros
  XI% = 0																																		! Init contador
  For XI% = 1 To 20																													! Carga Lista usuarios autorizados
    TS.ER.RETURN = -1																												! Ctrl errores
    Xkey$ = Pack$(Right$("00"+Str$(XI%),2))																	! Arma llave busqueda
    Read Form "C1 C6 C4 C1";#94 Key Xkey$;                  				       \! Lee Reg Cabecera 
       Xdat1$, Xdat2$, Xdat3$, Xdat4$																				! Parametros usarios definidos
    If TS.ER.RETURN = -1 Then Begin																					! Registro encontrado
    	 Gr.ChgPrc.Usr$(XI%,0) = UnPack$(Xdat2$)															! ID usuario autorizado
    	 Gr.ChgPrc.Usr$(XI%,1) = UnPack$(Xdat3$)															! Clave de validacion en caja
       Gr.ChgPrc.Usr$(XI%,2) = UnPack$(Xdat4$)															! Nivel de cambio precio autorizado
    EndIf																																		!
  Next XI%																																	! Fin carla lista niveles precio
  Close 94																																	! Cierre archivo

  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::TF:CONPRC" As 94 UNLOCKED NOWRITE NODEL   										    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR OPEN TF:CONPRC /Borrar")                  !
  	 Call U.IMPRIME("ERROR OPEN TF:CONPRC "+TS.TS11WERR$,2100H)
	   Gr.ChgPrc.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 																																		! fin carga parametros
  If End #94 Then UE.FIN.CONPRC         																	  ! Si es EOF                        
  XI% = 0																																		! Init contador
  While(1)         																													! Carga Lista usuarios autorizados
    Read #94; Xdat1$, Xdat2$		       																			! Lectura registro   
    XI% = Val(Xdat1$)																												! Concepto
    If XI% > 0 And XI% <= 50 Then                                          \! Hasta 50 conceptos
       Gr.ChgPrc.Con$(XI%) = Xdat2$																					! Descriptor Concepto
  Wend 																																			! Fin carga conceptos
  UE.FIN.CONPRC:
  Close 94																																	! Cierre archivo

  
End Sub 																																		! fin carga parametros

Sub Autoriza.Precio(Xopt%) Public																						! Modulo Cambio Precio
Integer*4 Xopt%, Xj%
String    Xtmp$, Xtmp2$, Xtmp3$
!--- EAMTSU07.J86
If XOPT% = 07 Then Begin																										! Carga de opciones
  Gr.ChgPrc.Act% = 0 																										    ! Init proyecto activo
  TS.TS11WERR$ = ""																													! Control errores
  OPEN "R::$SCNTRL" AS 94 UNLOCKED NOWRITE NODEL												    ! Apertura archivo parametrizacion 
  If TS.TS11WERR$ <> "" Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR PARAMTROS CTRL CAMBIO PRECI")             !
	   Gr.ChgPrc.Act% = 0																										  ! Init proyecto activo  	 
  	 Exit Sub																																!
  EndIf 
  If End #94 Then UE.FIN.CHGPRC         																	  ! Si es EOF                        
  While (1)															  																  ! Recorre archivo                  
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[CONTROL CAMBIO PRECIO]" Then Begin		 					    ! Parametros control cambio precio
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.ChgPrc.Act% = Val(Mid$(TS.TEMP1$,30,2))      			    					! Proyecto Activo 0. No -1 Si
         If Gr.ChgPrc.Act% = -1 Then                                       \! Control precio por nivel 
            TO.PRCOVRIDLIM = 9999999999																			!
         GoTo UE.FIN.CHGPRC                                                 ! Sale del proceso
        EndIf																																!
  Wend 																																		  ! Fin recorrido archivo
  UE.FIN.CHGPRC:
    Close 94 
    Call Carga.Param.ChgPrc																						      ! Carga parametros cambio precio
    If Gr.ChgPrc.Act% = -1 Then                                            \! Proyecto Activo
      Call U.IMPRIME("MODULO CTRL. PRECIO    ON 25/Ene/2022",2100H) Else   \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CTRL. PRECIO   OFF 25/Ene/2022",2100H)         ! Msg Proyecto Cargado
    
EndIf

If Gr.ChgPrc.Act% = 0 Then Exit Sub 																				! Si proyecto apagado

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin																										! Inicio de transaccion
   Gr.ChgPrc.Itm%  = 0
   Gr.ChgPrc.Proc% = 0
EndIf 

!--- EAMTSU08.J86
If XOPT% = 08 Then Begin																										! Proceso cambio de precio

  If (TS.IO.MOTORKEY = 80 And TS.IO.KEYS(5) = 74) Then                     \! Rutina cambio de precio teclado
  If (IR.PRICE1 <> Val(TS.IO.DATA$(5))) Then Begin												  ! Precio de venta NE maestra
!--- Se adiciona este control para la venta de concesionarios
  	 If IR.PRICE1 = 0 Then Exit Sub 																				! Si producto Sin precio en maestra
     If Gr.ChgPrc.Proc% <> 0 Then Exit Sub                                  ! Si en proceso cambio de precio
  	 Xtmp3$ = TS.IO.DATA$(5)																								! Nuevo precio
  	 
     If IR.INDICAT0 AND 40H Then Begin        													    ! Si el Item es pesable
     	If TS.IO.DATA$(6) = "" Then Begin																			! Peso no capturado
     	  Call VISOR.AND.BORRAR("NO SE INGRESO PESO  PRODUCTO    /Borrar")
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     	EndIf																																	!
  	 EndIf																																	!
  	 
  	 Call SALVAR.SECUENCIA
     Call TSHIECET  	 																											! Tono de alerta
     TS.TEMP3$ = ASIC.DATOS$("INGRESE ID AUTORIZDO","PLU/CONFIR BORR/ANUL") ! Captura ID persona autorizada
     If (TS.TEMP3$ = "") OR (TS.TEMP3$ = "E") Then Begin										! No se capturo dato
     	  Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR USUARIO /Borrar")
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf
     Xtmp$ = TS.TEMP3$
!     TS.TEMP4$ = ASIC.DATOS$("INGRESE CLAVE SUPERV","PLU/CONFIR BORR/ANUL") ! Captura ID persona autorizada

     Call TSHIECET  	 																											 ! Tono de alerta
     Call VISORES4690(1,"INGRESE CLAVE SUPERV","PLU/CONFIR BORR/ANUL",0,"L") ! Captura ID persona autorizada
     UNLOCKDEV 32, 6, PRIORITY            																	 ! set input state to enter/clear
     Call TSCSECRK                        																	 ! read the keyboard
     LOCKDEV 32                           																	 ! lock it
     
     If ((TS.IO.KEYS(10) = 79) AND (TS.IO.MOTORKEY = 80)) THEN BEGIN      ! ALLOW FOR OVERRIDE HERE
       If TS.IO.DATA$(10) <> "" Then Begin
          Xtmp2$ = TS.IO.DATA$(10)
       EndIf
     EndIf
    
     If (Xtmp2$ = "") OR (Xtmp2$ = "E") Then Begin										! No se capturo dato
     	  Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR USUARIO /Borrar")
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf																																	!
     Call TSHIECET  	 																											! Tono de alerta
     TS.TEMP5$ = ASIC.DATOS$("CONCEPTO DE CAMBIO  ","PLU/CONFIR BORR/ANUL") ! Captura ID persona autorizada
     If (TS.TEMP5$ = "") OR (TS.TEMP5$ = "E") Then Begin										! No se capturo dato
     	  Call VISOR.AND.BORRAR("PROCESO CANCELADO   POR USUARIO /Borrar")
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf																																	!
     If Val(TS.TEMP5$) <= 0 Or Val(TS.TEMP5$) > 50 Then Begin								!
     	  Call VISOR.AND.BORRAR("CONCEPTO CAPTURADO  NO VALIDO   /Borrar")
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf																																	!
     XJ% = Val(TS.TEMP5$)																										! Toma Concepto Definido
     If Gr.ChgPrc.Con$(XJ%) = "" Then Begin																	! Si concepto no definido
     	  Call VISOR.AND.BORRAR("CONCEPTO CAPTURADO  NO DEFINIDO /Borrar")    !
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf Else Begin
        TS.TEMP6$ = Left$(Gr.ChgPrc.Con$(XJ%)+String$(20," "),20) +        \!
                    "/Borrar"
        Call VISOR.AND.BORRAR(TS.TEMP6$)            
     EndIf
     TS.TEMP5I1 = 0																													! Control busqueda
     Xj% = 0																															  ! Init proceso
     For XJ% = 1 To 20																											! Busqueda de usuario y clave
       If Val(Gr.ChgPrc.Usr$(XJ%,0)) = Val(Xtmp$) Then Begin								! Si cedula encontrada
       	If Val(Gr.ChgPrc.Usr$(XJ%,1)) = Val(Xtmp2$) Then Begin					  	! Clave autorizacion
       		 TS.TEMP5I1 = -1																									! Proceso validacion OK
       		 TS.TEMP5I2 = XJ%																									! Posicion donde se encontro
       	EndIf																																!
       EndIf																																!
     Next XJ%																																! fin busqueda usuario y clave
     If TS.TEMP5I1 = -1 Then Begin																					! Usuario y clave validado
        Xj% = Val(Gr.ChgPrc.Usr$(TS.TEMP5I2,2))															! Nivel de autorizacion
        TS.TEMP4I4 = Val(Gr.ChgPrc.Niv$(XJ%,0))															! Monto Minimo
        TS.TEMP5I4 = Val(Gr.ChgPrc.Niv$(XJ%,1))															! Monto Maximo
        
        !TS.TEMP3I4 = IR.PRICE1 - Val(TS.IO.DATA$(5))                       ! Calcula diferencia en precio
        
        TS.TEMP3I4 = IR.PRICE1 - Val(Xtmp3$)                        				! Calcula diferencia en precio Mod 25Ene2022
        If TS.TEMP3I4 < 0 Then TS.TEMP3I4 = (TS.TEMP3I4 * -1)               ! convierte dato en positivo

        If (TS.TEMP3I4 >= TS.TEMP4I4) And (TS.TEMP3I4 <= TS.TEMP5I4) Then Begin ! Si diferencia en rango de nivel
        	 !-- Proceso autorizado 
           TS.TEMP1$  = Pack$(Xtmp$)                       				 +       \! ID usuario autorizado
               ":"+Pack$(Xtmp2$)                                   +       \! clave utilizada
               ":"+IR.ITEMCODE$                                    +       \! PLU vendido
               ":"+Pack$(Str$(IR.PRICE1))                          +       \! Precio producto
               ":"+Pack$(Xtmp3$)                           				 +       \! Nuevo precio
               ":"+Pack$(TS.TEMP5$)                                +       \! Concepto de cambio de precio
               ":"+Pack$("00")                                              ! Filler 
            Call Grabacion.String.Usuario2("20210819",TS.TEMP1$)            ! Graba String usuario  
            Gr.ChgPrc.Proc% = -1
            Call RESTAURAR.SECUENCIA
        EndIf Else Begin																										! Por fuera de nivel de autorizacion
     	     Call VISOR.AND.BORRAR("USUARIO NO TIENE    AUTOR. CAMBIO PRECIO")!
           IR.INDICAT0 = IR.INDICAT0 OR 04H			                						! No permite venta
           Exit Sub 
        EndIf
     EndIf Else Begin																												! Si user o clave invalido
     	 Call VISOR.AND.BORRAR("USUARIO/CLAVE NO ES VALIDO       /Borrar")    !
        IR.INDICAT0 = IR.INDICAT0 OR 04H			                							! No permite venta
        Exit Sub 
     EndIf    																															!
  EndIf  																																		! fin validacion cambio de precio
EndIf 																																			! fin Eamtsu08

!--- EAMTSU20.J86
If XOPT% = 20 Then Begin																										! Inicio de transaccion
 If TS.LINETYPE  = 1 And Gr.ChgPrc.Proc% = -1 Then Begin		  						  ! Linea de Item
 	  Gr.ChgPrc.Proc% = 0 
    Gr.Fis.Domictv% = 0
 EndIf
EndIf 

!--- EAMTSU43.J86
If XOPT% = 43 Then Begin																										! Calculo de precio
 If Gr.Fis.Domictv% = -1 Then Begin                                     		! Aplicación domicilos tv
    SL.IT.XPRICE = Val(Gr.Fis.DomiPrc$)																			! Precio del domicilio
 	  Gr.Fis.Domictv% = 0																											! Cierra proceso
 	  Gr.Fis.DomiPrc$ = "0"																										!
 EndIf																																			! Fin aplicacion precio TV
EndIf																																				! fin Eamtsu43

End Sub																																			! Fin Modulo control cambio precio
