!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDMSGPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Agosto 21 de 2.002               *
!Observaciones : Carga Mensajes publicitarios para*
!                clientes y articulos modulo de   *
!                mercadeo V2                      *
!**************************************************

%ENVIRON C						     																									! Ambiente Controlador
Integer*1 Global TS.ERROR
String    Global Ofile$

Sub Apertura.Archivos
Open "TF:MSGPRM" KEYED RECL 43 AS 22                            						! Mensajes Promocionales
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
   	  Print "numero mensaje "+Str$(Xnmsg%)
   EndIf Else Begin																													! Resto del archivo
   	  Xkey$  = Pack$(Right$("0000000"+Str$(Xnmsg%),7) +                    \!
   	                 Right$("000"+Str$(Xlin%),3))                           !
   	  Write Form "C5 C38";#22;Xkey$, Regi$                                  ! Graba registro
   	  
   	  Print "grabado "+unpack$(Xkey$) + " " + regi$
   	  
   EndIf																																		! Fin de carga
Wend 	
	
Salir:
	 Xkey$  = Pack$(Right$("0000000"+Str$(Xnmsg%),7) + "000")									! Registro cabecera 
   Xd2$ = Right$("000"+Str$(Xlin%),3) + String$(35," ")											! Arma total lineas mensaje
   Write Form "C5 C38";#22;Xkey$, XD2$                                   		! Graba registro
   Close 22

End Sub 																																		! Fin carga de mensajes

!
!-- Bloque Principal
!

On Error GoTo Proc.Err																											! Ctrl errores aplicativo
Ofile$ = Command$																														! Toma dato sistema operativo
If Ucase$(Ofile$) = "VERSION" Then Begin
	 Print "Programa carga mensajes publicitarios Ver. 1.1 18/Abr/2017"
	 Stop 
EndIF
Call Apertura.Archivos																											! Apertura archivos 
Call Procesa.Mensaje																												! Carga mensaje 
Stop 																																				! Fin del programa

Proc.Err:
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If ERRF% = 1 And (ERR = "OE" OR ERR = "FU") Then Begin									! ERROR DE APERTURA
		    TS.ERROR = 0
			  Resume
		 EndIf
     If (ERR = "EF") Then Begin																							! Si eof
		    TS.ERROR = 0
			  RESUME
		 EndIf
		 If ERRF% = 22 And (ERR = "OE" OR ERR = "FU") Then Begin								! ERROR DE APERTURA
        Create PosFile "TF:MSGPRM" KEYED 5,,,15000 RECL 43                 \! si no existe lo crea.
            AS 22 compound perupdate 		     																! 
			  Resume																															!
		 EndIf																																	!
     Print "Error: "+ERR+" Sesion: "+STR$(ERRF%)														!
Stop 																																				! Fin de programa
