!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDUSRSYS.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva usuarios     *
!                sistema punto de venta           *
!**************************************************
! Modificaciones:

%Environ C						   																		 								! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ifile$, DATO.SO$  								! Definicion de variables
Integer*1  Global U.Error%																									!
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%, PP														!

%INCLUDE EAMOPERA.J86																												! Variales maestro operadores

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub


  FUNCTION ADXAUTH (FUNC,OPID$,OPPW$,OPID2$) EXTERNAL
     INTEGER  ADXAUTH,  \  Define Function return code type
              FUNC      !  Request operation
                        ! 1 = Change a record
                        ! 2 = Add a record
                        ! 3 = Delete a record
     !IR73484 - added support for request 9
     !IR73532 - added support for request 8
                        ! 8 = Change Password Only
                        ! 9 = Make operator record = authorization as OPID2$
     STRING   OPID$,    \  Operator ID
              OPPW$,    \  Operator Password
              OPID2$    !  Model Operator ID
  END FUNCTION


Sub MSG.LOG																																	!

 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,UE.MSG$) \
  Else Locate 23,1: Print UE.MSG$


!    Locate 23,1 : Print String$(78," ") 																		!
!    Locate 23,1 : Print "Msg: "+UE.MSG$																			!

End Sub 																																		!

Function TRADUCE.ERROR                                       								!                       
Integer*4 HX%, S%, SX%, SUM%                                                                        
String    Z$                                                                                        
    HX% = ERRN                                                              ! Traduccion de los     
    ERRFX$=""                                                               ! codigos de error      
    FOR S% = 28 TO 0 STEP -4                                                ! del sistema operativo 
        SX% = SHIFT(HX%,S%)                                                 !                       
        SUM% = SX% AND 000FH                                                !                       
        IF SUM% > 9 THEN SUM% = SUM% + 55                                  \!                       
        ELSE SUM% = SUM% + 48                                               !                       
        Z$ = CHR$(SUM%)                                                     !                       
        ERRFX$ = ERRFX$ + Z$                                                !                       
    NEXT S%                                                                 !                       
END Function


Sub Apertura.Archivos
    Open Ifile$  AS 1								       																	! Plano maestro de carga
    OPEN "EAMOPERA" KEYED RECL 72 AS 44   																	! Open auth. file
    Create "ADX_UDT1:ERRUSR.TXT"     AS 2                      							! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String Xuser$, Xpwd$, Xniv$, Xname$, X.Status$, Xoper$, Xdata$
Integer*4 Xi%
Integer Xr%
  While(1)
      Read #1; Xuser$, Xpwd$, Xniv$, Xname$, X.Status$											! Barrido archivo
      U.Qty% = U.Qty% + 1																										! Qty registros procesados
      Locate 12,27 : Print Str$(U.Qty%)																			!
      U.Error% = -1 																												! Control errores
      Xniv$    = Str$(Val(Xniv$))																						! Grupo de usuario
      If Ucase$(X.Status$) = "A" Then Begin 																! Adicion de registro
      	U.Error% = -1																												!
      	If Val(Xniv$) < 1 Or Val(Xniv$) > 9 Then Begin                      ! Error nro grupo 
      		 U.Error% = 0																											! Error Registro
      		 Xuser$ = "Error definicion grupo de usuarios"
      	EndIf Else Begin                                                    ! Busqueda grupo y creacion usuario
      		 U.Error% = -1																										! Control errores
           OA.OPERATOR$ = Pack$(Right$("9999999999"+Xniv$,10))              ! Busqueda del grupo
           READ FORM "C5,C4,C1,2I2,16I1,I2,C20,C20";#44 KEY OA.OPERATOR$; \
             Xdata$, OA.PASSWORD$,OA.OPTIONSK$,OA.INDICAT0,               \
             OA.INDICAT1,OA.INDICAT2,OA.INDICAT3,OA.INDICAT4,OA.INDICAT5, \
             OA.INDICAT6,OA.INDICAT7,OA.INDICAT8,OA.INDICAT9,OA.INDICAT10,\
             OA.INDICAT11,OA.INDICAT12,OA.INDICAT13,OA.INDICAT14,         \
             OA.INDICAT15,OA.INDICAT16,OA.INDICAT17,                      \
             OA.RESERVED,OA.OPERNAME$,OA.USEREXIT$                          !
           If U.Error% = -1 Then Begin                                      ! Si grupo definido existe
              XUser$  = Str$(Val(XUser$))																	  ! Usuario a crear
              XUser$  = Pack$(Right$("0000000000"+XUser$,10))							  ! Ajusta clave
              XPwd$  = Str$(Val(XPwd$))																	    ! Usuario a crear
              XPwd$  = Pack$(Right$("00000000"+XPwd$,8))									  ! Ajusta clave
              OA.OPERNAME$ = Xname$
              Write Form "C5,C4,C1,2I2,16I1,I2,C20,C20";#44;               \!
              Xuser$, Xpwd$,OA.OPTIONSK$,OA.INDICAT0,                      \!
              OA.INDICAT1,OA.INDICAT2,OA.INDICAT3,OA.INDICAT4,OA.INDICAT5, \!
              OA.INDICAT6,OA.INDICAT7,OA.INDICAT8,OA.INDICAT9,OA.INDICAT10,\!
              OA.INDICAT11,OA.INDICAT12,OA.INDICAT13,OA.INDICAT14,         \!
              OA.INDICAT15,OA.INDICAT16,OA.INDICAT17,                      \!
              OA.RESERVED,OA.OPERNAME$,OA.USEREXIT$                         !
              Xr% = 99
              If U.Error% = 0 Then                                         \!
              	Xuser$ = "Error Grabación registro" Else Begin             \!
 	              Xr% = ADXAUTH ( 9,STR$(VAL(UNPACK$(Xuser$))),              \! ID
                              STR$(VAL(UNPACK$(Xpwd$))),                   \! Passwor
                              "*"+Xniv$)                                    ! Logged D
              EndIf
           EndIf Else Begin
      		   U.Error% = 0																										! Error Registro
      		   Xuser$ = "Error Grupo Definido No Existe en Maestra"						!
           EndIf
        Endif
      EndIf
       
       If Ucase$(X.Status$) = "D" Then Begin 																! Borrado de registro
       	  U.Error% = -1 																										! Error Registro
          XUser$  = Str$(Val(XUser$))																	      ! Usuario a crear
          XUser$  = Pack$(Right$("0000000000"+XUser$,10))							      ! Ajusta clave
       	  DelRec 44 ; XUser$																								!
       	  If U.Error% = -1 Then Begin 
             Xr% = ADXAUTH ( 3,STR$(VAL(UNPACK$(Xuser$))),              \! ID
                             STR$(VAL(UNPACK$(Xpwd$))),                   \! Passwor
                             "*9"  )                                       ! Logged D
       	  EndIf Else Begin
       	  	 Xuser$ = "Error Eliminacion registro"        !
       	  EndIf
       EndIf 																																!
       
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Xuser$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Usuarios, Verifique Errores en ADX_UDT1:ERRUSR.TXT"
    Call Msg.Log
    Close 44 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  CARGA MASIVA USUARIOS SISTEMA Ver.1.0****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Enero   31 2019 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : EAMOPERA  "
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub

!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
Ifile$ = Command$																													! Toma dato sistema operativo
If Ucase$(Ifile$) = "VERSION" Then Begin																	! Version del programa
   Print "Carga Masiva Usuarios Sistema Ver 1.0 31/Ene/2019"
   Stop 
Endif
DATO.SO$ = "0"
If Ifile$ = "BACKGRND" Then DATO.SO$ = "1" 
Ifile$ = "ADX_UDT1:LDUSRSYS.TXT"  ! +IFILE$
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso																													! Fin del proceso
Stop 																																			! Fin del programa

Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 

  If ERR = "EF" Then Resume 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO "+IFILE$+" NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If (ERRF% = 44 Or ERRF% = 5) AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
 
  If ERRF% = 44 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE USUARIOS SISTEMA NO EXISTE.."
     Call Msg.Log
     Stop  
  EndIf 

  CALL MSG.LOG
  Stop 
