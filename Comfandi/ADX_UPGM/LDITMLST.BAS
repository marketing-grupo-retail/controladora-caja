!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDITMLST.BAS                    *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Listas de    *
!                Promociones Items                *
!**************************************************
! Modificaciones:

%Environ C						   																		 								! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ifile$														! Definicion de variables
Integer*1  Global U.Error%																									!
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%																!

Sub MSG.LOG																																	!
    Locate 23,1 : Print String$(78," ") 																		!
    Locate 23,1 : Print "Msg: "+UE.MSG$																			!
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
    Open "TF:ITMLST" KEYED RECL 508  AS 4				  					   							! Apertura Articulos controlados
    Open "TF:LISTAS" KEYED RECL 6    AS 5				  					   							! Apertura listas promociones
    Create "ADX_UDT1:ERRLST.TXT"     AS 2                      							! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$, Xdat3$
String X.Sku$, X.Lista$, X.Status$, XReg$, XDAT1$, XDAT2$, Xlst$
Integer*4 Xi%, Xj%
  While(1)

      Read #1; X.Sku$, X.Lista$, X.Status$
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      
      X.Sku$   = Str$(Val(X.Sku$))
      X.Sku$   = Pack$(Right$("000000000000"+X.Sku$,12))
      X.Lista$ = Str$(Val(X.Lista$))
      Xlst$    = "|"+X.lista$+"|"

      If Ucase$(X.Status$) = "A" Then Begin 																! Adicion de registro
      	U.Error% = -1
        Read Form "C6 C502";#4 KEY X.SKU$; XDAT1$, XDAT2$                   ! Busca lista Item
        If U.Error% = -1 Then Begin																					! Si producto matriculado
        	 Xdat3$ = ""
        	 If Match(Xlst$,Xdat2$,1) <= 0 Then Begin													! Si lista no encontrada
        	 	  For Xi% = 1 To Len(Xdat2$)																		! Recorre lista
        	 	    If Mid$(Xdat2$,Xi%,1) <> " " Then                          \! no es espacio en blanco
        	 	       Xdat3$ = Xdat3$ + Mid$(Xdat2$,Xi%,1)                     ! Arma String de listas Sin basura
        	 	  Next Xi%																											!
        	 	  Xdat2$ = Xdat3$ + Xlst$                                     	! Adiciona lista
        	 	  Write Form "C6 C502" ; #4; X.Sku$, Xdat2$                     ! Graba lista
        	 EndIf 																														! 
        EndIf Else Begin																										! Producto no matriculado
           Write Form "C6 C502" ; #4; X.Sku$, Xlst$										  		! Adiciona Item y lista
        EndIf																																! 
        X.lista$ = Pack$(Right$("000000000000"+X.lista$,12))								! Ajusta dato
        Write Form "C6"   ;#5; X.Lista$      															  ! Almacena Lista promociones    UPD  6
      EndIf
       
!       If Ucase$(X.Status$) = "D" Then Begin 															! Borrado de registro
!       	  !DelRec 4 ; X.Sku$																							!
!       EndIf 																															!

!--- Retiro promocion lista itens
      If Ucase$(X.Status$) = "D" Then Begin 																! Retiro de registro
      	U.Error% = -1
        Read Form "C6 C502";#4 KEY X.SKU$; XDAT1$, XDAT2$                   ! Busca lista Item
        If U.Error% = -1 Then Begin																					! Si producto matriculado
        	 If Match(Xlst$,Xdat2$,1) > 0 Then Begin													! Si lista encontrada
        	 	  Xj% = Len(Xlst$) - 2
        	 	  Xdat3$ = "|" + String$(Xj%,"0") + "|"
              Xdat2$ = TRANSLATE$(XDAT2$,Xlst$,Xdat3$)
        	 	  Write Form "C6 C502" ; #4; X.Sku$, Xdat2$                     ! Graba lista
        	 EndIf 																														! 
        EndIf 
      EndIf
       
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Unpack$(X.Sku$),UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Listas, Verifique Errores en ADX_UDT1:ERRLST.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  CARGA MASIVA LISTAS PROMOCION Ver.1.0****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Marzo   19 2022 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : TF:ITMLST "
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
   !UE.MSG$ = "Carga Masiva Listas Promociones Ver 1.0 12/Abr/2017"
   !UE.MSG$ = "Carga Masiva Listas Promociones Ver 1.3 19/Mzo/2022"
   UE.MSG$ = "Carga Masiva Listas Promociones Ver 1.4 29/Sep/2022"
   Call Msg.Log
   Stop 
Endif
Ifile$ = "/PROMODAT/"+IFILE$
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso																													! Fin del proceso
Stop 																																			! Fin del programa

Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$+" Linea "+Str$(U.Qty%)
  
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
  If (ERRF% = 4 Or ERRF% = 5) AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE ITEM POR LISTAS PROMOCION NO EXISTE.. CREANDO..."
     CREATE POSFILE "TF:ITMLST" KEYED 6,,,60000 RECL 508 AS 4 COMPOUND PERUPDATE
     Resume 
  EndIf 

  If ERRF% = 5 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO MAESTRO DE LISTAS PROMOCION NO EXISTE.. CREANDO..."
     CREATE POSFILE "TF:LISTAS" KEYED 6,,,50000 RECL 6 AS 5 COMPOUND PERUPDATE
     Resume 
  EndIf 
  
  CALL MSG.LOG
  Stop 
