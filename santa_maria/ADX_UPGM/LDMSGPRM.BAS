!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDMSGPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Noviembre 17 2023                *
!Observaciones : Carga Mensajes Cupones           *
!**************************************************

%ENVIRON C						     																									! Ambiente Controlador
Integer*1 Global TS.ERROR
String    Global Ofile$

Sub Apertura.Archivos
  TS.ERROR = 1
  Open "TFMSGPRM" KEYED RECL 43 AS 22                            						! Mensajes Promocionales
  If TS.ERROR <> 1 Then Begin
     Print "Creando Archivo TFMSGPRM"
     TS.ERROR = 1
     Create PosFile "TFMSGPRM" KEYED 5,,,15000 RECL 43 AS 22 compound perupdate 		     															! 
     If TS.ERROR = 0 Then Begin
     	  Print "FALLA PROCESO CREACION TSMSGPRM"
     	  Print "Error: "+ERR+" Sesion: "+STR$(ERRF%)													!
     	  Stop 
     EndIf
  EndIf
End Sub 																																		! Fin apertura archivos

Sub Procesa.Mensaje																													! Carga archivo de mensajes
String REGI$, Xkey$, Xd1$, Xd2$
Integer*2 Xlin%, Xnmsg%
TS.ERROR = -1
Ofile$ = "/PROMODAT/"+OFILE$
Open Ofile$ As 1																														! Apertura archivo mensaje
If TS.ERROR = 0 Then Begin																									! Falla en apertura
	 Print "ARCHIVO "+OFILE$+" NO EXISTE PARA CARGA"
	 Exit Sub 
EndIf																																				!
If End #1 Then Salir
Xlin% = 0
While (1)																																		! Recorre archivo
   Read #1 ; Line REGI$                																			! Lee Registro
   Xlin% = Xlin% + 1 
   If Xlin% = 1 Then Begin																									! Primera linea del archivo
   	  Xnmsg% = Val(Regi$)																										! Numero del mensaje
   	  Print "Numero Mensaje :"+Str$(Xnmsg%)
   EndIf Else Begin																													! Resto del archivo
   	  Xkey$  = Pack$(Right$("0000000"+Str$(Xnmsg%),7) +                    \!
   	                 Right$("000"+Str$(Xlin%),3))                           !
   	  Write Form "C5 C38";#22;Xkey$, Regi$                                  ! Graba registro
   	  
   	  Print "Grabado "+unpack$(Xkey$) + " " + regi$
   	  
   EndIf																																		! Fin de carga
Wend 	
	
Salir:
	 Xkey$  = Pack$(Right$("0000000"+Str$(Xnmsg%),7) + "000")									! Registro cabecera 
   Xd2$ = Right$("000"+Str$(Xlin%),3) + String$(35," ")											! Arma total lineas mensaje
   Write Form "C5 C38";#22;Xkey$, XD2$                                   		! Graba registro
   Close 22
   Close 1 
End Sub 																																		! Fin carga de mensajes

!
!-- Bloque Principal
!

On Error GoTo Proc.Err																											! Ctrl errores aplicativo
Ofile$ = Command$																														! Toma dato sistema operativo
If Ucase$(Ofile$) = "VERSION" Then Begin
	 Print "Programa carga cupones publicitarios Ver. 1.1 17/Nov/2023"
	 Stop 
EndIf
Clears
Print "Grupo Retail Ltda" 
Print "Carga Mensaje Cupones Tienda, Ver. 1.1 17/Nov/2023"
Print "Procesando Archivo:"+Ofile$
Call Apertura.Archivos																											! Apertura archivos 
Call Procesa.Mensaje																												! Carga mensaje 
Stop 																																				! Fin del programa

Proc.Err:
     TS.ERROR = 0
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If ERRF% = 1 And (ERR = "OE" OR ERR = "FU") Then Begin									! ERROR DE APERTURA
			  Resume
		 EndIf
     If (ERR = "EF") Then Begin																							! Si eof
			  RESUME
		 EndIf
		 If ERRF% = 22 And (ERR = "OE" OR ERR = "FU") Then Begin								! ERROR DE APERTURA
			  Resume																															!
		 EndIf																																	!
     Print "Error: "+ERR+" Sesion: "+STR$(ERRF%)														!
Stop 																																				! Fin de programa
