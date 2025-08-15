!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMCLAUT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Claves Autorizacion       *
!**************************************************
! Modificaciones:
! ----- Version 1.0 14 Febrero   2025 -------------

%Environ T																																	! Ambiente terminal
Integer   Global SL.END																											! Total String trx 
Integer*1 Global Gr.Caut.Ok%                                                ! Proyecto Activo

%INCLUDE EAMTSWKG.J86          																							! working storage
%INCLUDE EAMTRANS.J86          																							! Variables de transacciones
%INCLUDE EAMTERMS.J86          																							! Variables de terminales
%INCLUDE EAMTOPTS.J86																												! Terminal options
%INCLUDE EAMITEMR.J86          																							! Variables EAMITEMR
%INCLUDE EAMTSXHC.J86          																							! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          																							! Rutinas de Assembler

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

Sub CARGA.CLAVES.AUTOR																											! Carga claves autorizacion
Integer*2 Xi%, Xn%																													!
String    Xtmp$																															!
  If Gr.Caut.Ok% <> -1  Then Exit Sub                                       ! Si proyecto apagado
  Xn% = 0																																		! Nro intentos apertura
  CARGA.ARCHIVO.CLAVES:
  TS.ER.RETURN = -1																													! Control errores
  Open "R::ADX_UDT1:TFCLAVES.DAT" As 94 NOWRITE NODEL							  				! Apertura archivo claves
  If TS.ER.RETURN <> -1 Then Begin																					! Falla Apertura
  	If Xn% < 3 Then Begin																										! 
  		 Xn% = Xn% + 1																												! Aumenta apuntador
  		 Wait ; 1000
  		 GoTo CARGA.ARCHIVO.CLAVES
  	EndIf
    Call VISOR.AND.BORRAR("ERROR OPEN ARCHIVO  TFCLAVES.DAT /Borrar")      !
    Call VISOR.AND.BORRAR("PROYECTO APAGADO               /Borrar")         !
    Call U.Imprime("ERROR OPEN ARCHIVO TF:CLAVES",2100H)
    Call U.Imprime("PROYECTO CLAAVES CANCELADO  ",2100H)
    Gr.Caut.Ok%  = 0                                                        ! Proyecto Apagado
    Exit Sub 																																! Sale del proyecto
  EndIf																																			!
  Xi% = 0																																		!
  If End # 94 Then CONT.CLAVE																								! Si EOF
  LEER.CLAVE:																																!
   Read # 94 ; Xtmp$																												! Lectura registro
   Xi% = Xi% + 1																														! Posicion archivo
   If Xi% > 20 Then GoTo CONT.CLAVE																					! Mas de 20 claves
   TO.OVRIDNUM(Xi%) = Val(Xtmp$)																						! Carga numero autorizacion
   GoTo LEER.CLAVE																													! Siguiente registro
  CONT.CLAVE:																																!
   Close 94      																														! Cierre Archivo
End Sub 																																		! Fi carga claves

Sub GRCLVAUT(Xopt%) Public 																							    ! Modulo Claves Autorizacion
Integer*4 Xopt%            																								  ! UE ejecutar
String Xtmp$ 																																!

!--- EAMTSU07.J86																														!
If Xopt% = 7 Then Begin                                                     ! Carga de parametros
  Gr.Caut.Ok%  = 0                                                          ! Proyecto Apagado
  TS.ER.RETURN = -1																												  ! Control errores
  Open "R::$ARGENER" As 94 UNLOCKED NOWRITE NODEL 												  ! Apertura archivo parametrizacion para TCxSky
  If TS.ER.RETURN <> -1 Then Begin                                          ! Error apertura
  	 Call VISOR.AND.BORRAR("ERROR APERTURA CLAVES AUTOR.")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA CLAVES AUTOR. ",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
  EndIf  																																		!
  If End #94 Then UE.FIN.CLAVES          																	  ! Si es EOF
  While (1)															  																  ! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[CLAVES AUTORIZACION]" Then Begin	   				        ! Claves autorizacion
         Read #94; TS.TEMP1$																								! Lectura registro                 
         Gr.Caut.Ok% = Val(Mid$(TS.TEMP1$,30,2)) 		    					          ! Proyecto Activo 0. No -1 Si
        EndIf																																! Fin caga
  Wend 																																			! Fin carga
  UE.FIN.CLAVES:                                                            !
     Close 94																																! Cierra archivo
     Call CARGA.CLAVES.AUTOR																								! Carga Claves autorizacion
   If Gr.Caut.Ok% = -1 Then                                                \! Proyecto Activo
      Call U.IMPRIME("MODULO CLAVES AUTOR. ON  14-Feb-2025",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO CLAVES AUTOR. OFF 14-Feb-2025",2100H)          ! Msg Proyecto Cargado
EndIf 																																			! Fin carga de parametros 

If Gr.Caut.Ok% <> -1  Then Exit Sub                                         ! Si proyecto apagado
If TS.STANDALONE       Then Exit Sub	                                      ! Si en operacion TOF

!--- EAMTSU02.J86																														!
If Xopt% = 2 Then Begin                                                     ! Inicializacion de transaccion
   Call CARGA.CLAVES.AUTOR																									! Sube claves autorizacion
EndIf																																				! Fin Eamtsu02

End Sub																																			! Fin Aplicacion
