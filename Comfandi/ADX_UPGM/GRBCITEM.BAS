!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRBCITEM.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Consulta Produtos Matriculados   *
!                para bonos recompra              *
!**************************************************
! Modificaciones:

%Environ C						   																		 ! Ambiente de controlador

Integer*1  Global U.Error%
String     Global Item$, ERRFX$, UE.MSG$

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END Function


Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 


On Error GoTo ERBONO

Clears 
Open "TF:COMPRA" KEYED RECL 6 AS 1   ! Articulos para bono recompra
Locate 1,1 : Print "Consulta de Productos Matriculados Bonos Recompra"
Locate 3,1 : Input "Ingrese Producto a Consultar :";Item$
Item$ = Pack$(Right$("000000000000"+Item$,12))
U.Error% = -1
Read Form "C6";#1 KEY Item$; Item$ ! Lee archivo 
If U.Error% = 0 Then Begin
	 Print "Producto No Matriculado para Bonos Recompra"
EndIf Else Begin
	 Print "Producto Matriculado para Bonos Recompra"
EndIf
Close 1 

Stop 

ERBONO:
    U.Error% = 0
    CALL TRADUCE.ERROR
    UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$

    If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO TF:COMPRA NO EXISTE... "
     Call Msg.Log
     Stop
    EndIf 

    If (ERR = "EF" OR ERR = "KF") Then Begin
     Resume 	
    EndIf

 Call Msg.Log
 Stop 
 