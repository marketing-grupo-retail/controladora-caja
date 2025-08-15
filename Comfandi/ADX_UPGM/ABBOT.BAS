!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : abbot.bas                        *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion estructuras ABBOT       *
!**************************************************

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

On Error GoTo Fallas
Clears
Print "Grupo Retail Ltda"
Print "Creación estructuras modulos Abbot"
Print "Version 1.0 24/Mayo/2022"
Print " "
Print "CREANDO ARCHIVO MAESTRO DE ARTICULOS ABBOT, ESPERE"
U.Error% = -1
CREATE POSFILE "TF:ITMABB" KEYED 6,,,10000 RECL 13 AS 4 COMPOUND PERUPDATE
If U.Error% <> -1 Then Begin
	 Print "FALLA EN CREACION DE TF:ITMABB"
EndIf Else Close 4

U.Error% = -1
Print "CREANDO ESTRUCTURAS ABBOT V2, ESPERE"
CREATE POSFILE "TF:ITMAB2" KEYED 6,,,10000 RECL 13 AS 4 COMPOUND PERUPDATE
If U.Error% <> -1 Then Begin
	 Print "FALLA EN CREACION DE TF:ITMAB2"
	 tclose 4
	 CREATE POSFILE "TF:ITMAB2" KEYED 6,,,10000 RECL 13 AS 4 COMPOUND PERUPDATE
EndIf Else Close 4
Print "FIN PROCESOS DE CREACION ABBOT"
Stop 


Fallas:
U.Error% = 0
Resume 