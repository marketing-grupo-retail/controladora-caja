! PASMEXIT.BAS
!************************************************** 
!Empresa       : Grupo Retail Ltda  		          *
!Programa      : Control Password Operadores      *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Febrero de 2008                  *
!Observaciones : Control Vencimiento Claves Cajero*
!**************************************************

%ENVIRON T

INTEGER*1 GLOBAL UE.PASWD.ACTIVO%			                        ! SI PROYECTO ACTIVO
INTEGER*1 GLOBAL UE.PASWD.SESION%			                        ! NUMERO SESION TEMPORAL
INTEGER*4 GLOBAL UE.PASWD.DIAS%				                        ! NUMERO DIAS VENCIMIENTO PASSWORD
INTEGER*4 GLOBAL UE.PASWD.INTENTOS%			                      ! NUMERO MAXIMO DE INTENTOS DE ACCESO
INTEGER*4 GLOBAL UE.PASWD.WAIT%                               ! TIEMPO DE BLOQUEO DE LA TERMINAL
INTEGER*4 GLOBAL UE.PASWD.ACCESOS%                            ! TIEMPO DE BLOQUEO DE LA TERMINAL


%INCLUDE EAMTSCVA.J86
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%INCLUDE EAMTERMS.J86          !
%INCLUDE EAMTOPTS.J86	         !
%INCLUDE EAMP4VAJ.J86          !
%INCLUDE EAMTSXHC.J86     

%INCLUDE GRMTSUSU.011          ! Invocacion de rutinas externas genericas

Sub TSHIECET External 
End Sub 

Sub RASTRO.PWD.AUDITORIA(Xfec1$,Xfec2$,Xtipo$)
String Xfec1$, Xfec2$, Rbuffer$, X.Lec$, XFinr$, Xtipo$
Integer*4 X.Len%
 TS.ER.RETURN = -1 																									! Ctrl Errores
 XFinr$ = CHR$(13)+CHR$(10)																					! Fin de registro
 Open "R::TF:AUDPWD" AS UE.PASWD.SESION% Append                     ! Abre archivo de Auditoria
 If TS.ER.RETURN = -1 Then Begin 																		! Apertura Auditoria OK
    Rbuffer$ = TS.TERMINAL$ + "," + 															 \! Numero de terminal
               Xtipo$ + "," +                                      \! Tipo de operacion
               Str$(Val(Unpack$(TS.OPER$))) + "," + 							 \! Numero de operador
               Xfec1$ + "," + 																     \! Fecha de Actualizacion password
               Xfec2$ + "," + 																     \! Fecha de proximo cambio clave
               Time$                                                ! Hora de Actualizacion
    X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #UE.PASWD.SESION% ; Rbuffer$, XFinr$         ! Graba registro
    Close UE.PASWD.SESION%																					! Cierre del archivo
 EndIf 

End Sub 


Sub GRPASSWD(XOPTION%)  Public
INTEGER*4 XOPTION%
String    XTMP$, XDAT1$, XDAT2$, XDAT3$, XDAT4$, XDAT5$, XDAT6$, Xmsg$, Xreg$(1), Xcajero$

Xtmp$ = "" : Xdat1$ = "" : Xdat2$ = "" : Xdat3$ = "" 
Xdat4$ = "" : Xdat5$ = "" : Xdat6$ = ""

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control de Password Cajeros                   **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   ***************************************************************************

If XOPTION% = 7 Then Begin 
    UE.PASWD.ACTIVO% = 0
    UE.PASWD.DIAS%   = 0    
    UE.PASWD.SESION% = 1
    TS.ER.RETURN = -1 
    Open "R::TF:MACO01" KEYED RECL 7 AS 94              ! Abre archivo de control
    If TS.ER.RETURN <> -1 Then Begin 										! Si Error apertura control
       Call Visor.And.Borrar("ERROR PARAMETROS CONTROL PASSWORD")! Msg de alerta
       Call U.IMPRIME("CONTROL PASSWORD INACTIVO Ver. 1.00",6100H)  	! Proceso OK
       Exit Sub      																		! Sale funcion
    EndIf 
    XTMP$ = PACK$("01")																	! Arma llave de busqueda
    TS.ER.RETURN = -1 																	! Ctrl de lectura
    Read Form "C1 C1 C2 3C1";#94 KEY XTMP$;            \! Lee Reg Control
                  XDAT1$, XDAT2$, XDAT3$, XDAT4$,      \! Toma los datos del registro
                  XDAT5$, XDAT6$ 
    
    If TS.ER.RETURN <> -1 Then Begin 										! Si falla registro control
       Call Visor.And.Borrar("ERROR LECTURA PARAMETROS PASSWORD")! Msg de alerta
       Call U.IMPRIME("CONTROL PASSWORD INACTIVO Ver. 1.00",6100H)  	! Proceso OK    
       UE.PASWD.ACTIVO% = 0
       UE.PASWD.DIAS%   = 0    
       UE.PASWD.SESION% = 1
       UE.PASWD.INTENTOS% = 99
       UE.PASWD.WAIT%     = 0
       Close 94 
       Exit Sub 
    EndIf 
    Call U.IMPRIME("CONTROL PASSWORD ACTIVO Ver. 1.00",6100H)  	! Proceso OK
    If Val(Unpack$(Xdat2$)) = 1 Then 						               \! Proyecto Activo
       UE.PASWD.ACTIVO% = -1 
    UE.PASWD.DIAS%     = Val(Unpack$(Xdat3$))                   ! Numero dias vencimiento claves
    UE.PASWD.SESION%   = Val(Unpack$(Xdat4$))                   ! Numero de sesion temporal
    UE.PASWD.INTENTOS% = Val(Unpack$(Xdat5$))                   ! Numero de intentos
    UE.PASWD.WAIT%     = Val(Unpack$(Xdat6$))                   ! Numero de intentos    
    UE.PASWD.WAIT%     = UE.PASWD.WAIT% * 60000									! Minutos de Espera 
    Close 94
    Exit Sub 
EndIf

If UE.PASWD.ACTIVO% <> -1 Then Exit Sub 	 											! SI MODULO APAGADO 

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control de Password Cajeros                   **
!   **    Incluir en :  EAMTSU23.J86                                         **
!   ***************************************************************************


If XOPTION% = 02 Then Begin 
   UE.PASWD.ACCESOS% = 0 
EndIf 

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control de Password Cajeros                   **
!   **    Incluir en :  EAMTSU23.J86                                         **
!   ***************************************************************************

If XOPTION% = 23 Then Begin 
   
   If Left$(TS.DISP1$,4) = "B077" Then Begin 									            ! SI ACCESO FALLIDO
      UE.PASWD.ACCESOS% = UE.PASWD.ACCESOS% + 1 
      If UE.PASWD.ACCESOS% >= UE.PASWD.INTENTOS% Then Begin 							! SI NUMERO MAXIMO DE FALLOS 
         Call  RASTRO.PWD.AUDITORIA("20"+DATE$,"20"+DATE$,"TERMINAL BLOQUEADA")      
         Locate #30;1,1,Off																								! Posiciona Cursor
         Write Form "2C20";#30;"TERMINAL BLOQUEADA","ACCESOS FALLIDOS"    ! Mensajes
         Wait ; UE.PASWD.WAIT% 																						! 
         UE.PASWD.ACCESOS% = 0 
      EndIf Else Call  RASTRO.PWD.AUDITORIA("20"+DATE$,"20"+DATE$,"CLAVE ERRONEA")      
   EndIf 
   If Left$(TS.DISP1$,4) = "B070" Then Begin 									            ! Cambio Clave Satisfactorio
      TS.ER.RETURN = -1 																									! Ctrl Errores
      Open "R::TF:MACO02" KEYED RECL 18 AS UE.PASWD.SESION%               ! Abre archivo de control
      If TS.ER.RETURN = -1 Then Begin 																		! Apertura OK 
         TS.ER.RETURN = -1 
         Read Form "C5 2C4 C1 C4";#UE.PASWD.SESION% KEY TS.OPER$;           \! Lee Reg Control de claves
           XDAT1$, XDAT2$, XDAT3$, XDAT4$, XDAT5$                         ! 
         If TS.ER.RETURN = -1 Then Begin 																  ! Si lo encontro
            Xdat3$ = Rutina.Java("com.grpretail.utils.DateFormatter","getDate", Str$(UE.PASWD.DIAS%))           ! Ejecuta Requerimiento
            Xdat3$ = Pack$(Xdat3$)	    																	! Fecha de actualizacion
            Xdat2$ = Pack$("20"+Date$)																		! Fecha de actualizacion
            Write Form "C5 2C4 C1 C4";#UE.PASWD.SESION% ;                \!
             TS.OPER$, XDAT2$, XDAT3$, XDAT4$, XDAT5$
         EndIf Else Begin 																								! No lo encontro
            Xdat3$ = Rutina.Java("com.grpretail.utils.DateFormatter","getDate", Str$(UE.PASWD.DIAS%))           ! Ejecuta Requerimiento
            Xdat3$ = Pack$(Xdat3$)	    																	! Fecha de actualizacion
            Xdat2$ = Pack$("20"+Date$)																		! Fecha de actualizacion
            Xdat4$ = Pack$("01")																					! Valida Password
            Xdat5$ = Pack$("00000000")  																	! Uso futuro
            Write Form "C5 2C4 C1 C4";#UE.PASWD.SESION% ;                \!
             TS.OPER$, XDAT2$, XDAT3$, XDAT4$, XDAT5$
         EndIf 
         Close UE.PASWD.SESION%
         Call  RASTRO.PWD.AUDITORIA(Unpack$(Xdat2$),Unpack$(Xdat3$),"CAMBIO DE PASSWORD")
      EndIf 
   EndIf 
EndIf 

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control de Password Cajeros                   **
!   **    Incluir en :  EAMTSU14.J86                                         **
!   ***************************************************************************

If XOPTION% = 14 Then Begin 
 If (TS.IO.MOTORKEY = 61 AND TS.IO.KEYS(3) = 78) AND \
     TS.IO.DATA$(3) <> "" AND TS.IO.KEYS(2) = 0 Then Begin 
     
   
   
   TS.ER.RETURN = -1 
   Open "R::TF:MACO02" KEYED RECL 18 AS UE.PASWD.SESION%              ! Abre archivo de control
   If TS.ER.RETURN = -1 Then Begin 				      ! Apertura OK
      Xcajero$ = Pack$(Right$("0000000000"+TS.IO.DATA$(3),10))        ! Codigo del cajero
      TS.ER.RETURN = -1 
      Dim Xreg$(6)
      Read Form "C5 2C4 C1 C4";#UE.PASWD.SESION% KEY Xcajero$;       \! Lee Reg Control de claves
           Xreg$(1),  \ !Id del cajero
           Xreg$(2),  \ !Fecha actualizacion  AAAAMMDD
           Xreg$(3),  \ !Fecha Proximo cambio AAAAMMDD
           Xreg$(4),  \ !Si permite cambio
           Xreg$(5)     !Uso Futuro

      If TS.ER.RETURN = -1 Then Begin 				      ! Si lo encontro
         TS.OPER$ = XCAJERO$					      ! Asigna codigo del operador
         If Val(Unpack$(Xreg$(4))) = 0 Then Begin 		      ! Si Password no expira
            Close UE.PASWD.SESION%				      ! Cierra archivo
            Exit Sub                      			      ! Sale de la rutina
         EndIf 
         
         XDAT5$ = Unpack$(Xreg$(3)) 				      ! Desempaqueta Fecha Vencimiento
         Xdat5$ = Rutina.Java("com.grpretail.utils.DateFormatter","getDaysBetweenDates", XDAT5$)   ! Ejecuta Requerimiento
         If Val(Xdat5$) <= 0  Then Begin 			      ! Se vencio la clave
            Call Visor.And.Borrar("PASSWORD VENCIDO    ACCESO NO AUTORIZADO")         ! 
            Xmsg$ = "PASSWORD VENCIDO"
            TS.GUIDANCE = 1077
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
            TS.IO.MOTORKEY = 0 
            Close UE.PASWD.SESION%
            Call  RASTRO.PWD.AUDITORIA("20"+DATE$,Unpack$(Xreg$(3)),XMSG$)
            Exit Sub 
         EndIf 
         
         If Val(Xdat5$) <= 5 Then Begin  				! Se vencimiento proximo menor de 5 dias
            Xdat5$ = Right$("00"+Xdat5$,2)
            Call Visor.And.Borrar("SU PASSWORD VENCE EN "+XDAT5$+" DIAS")
            Call Visor.And.Borrar("RECUERDE CAMBIAR SU CLAVE")
            XMSG$ = "VENCIMIENTO PROXIMO"
            Close UE.PASWD.SESION%
            Call  RASTRO.PWD.AUDITORIA("20"+DATE$,Unpack$(Xreg$(3)),XMSG$)
            Exit Sub 
         EndIf Else Close UE.PASWD.SESION%
      EndIf Else Begin 							              ! Registro no existe
            Call Visor.And.Borrar("PASSWORD VENCIDO..  ACCESO NO AUTORIZADO")         ! 
            Xmsg$ = "PASSWORD VENCIDO"
            TS.GUIDANCE = 1077
            Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
            TS.IO.MOTORKEY = 0 
            Close UE.PASWD.SESION%
            Exit Sub 
      EndIf 
   EndIf Else Call Visor.And.Borrar("ERROR APERTURA TFMACO02")
  EndIf 
EndIf 


End Sub 
