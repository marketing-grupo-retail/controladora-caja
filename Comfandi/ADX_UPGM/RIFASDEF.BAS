!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RIFASDEF.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion Definicion Rifas    *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, IFILE$, Key$
Integer*1  Global U.Error%, Terror%, Topen%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%

%INCLUDE ADX_UPGM:BASROUT.J86

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

FUNCTION TRADUCE.ERROR                                       !
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
END FUNCTION

Sub LISTA.RIFAS                                              !
String XRIFA$, XLISTA$, Dummy$, Xtmp$, Xlst$                 !
Integer*1 Found%                                             !
Integer*2 Xi%                                                !
Integer*4 Xval%
 Xrifa$ = Pack$("99")                                        !
 U.Error% = -1                                               !
 Read Form "C1 C300";#45 KEY Xrifa$;                        \!
    Dummy$, Xlista$																					 !
 If U.Error% = -1 Then Begin  
 	 Found% = 0      
 	 Xi% = 0                                                   !
 	 For XI% = 1 To 300 STEP 4                                 ! 
 	    Xtmp$ = Mid$(Xlista$,XI%,4)														 ! Toma codigo de rifa
 	    If Xtmp$ = Key$ Then Begin                             ! Si en la lista
 	    	 Found% = -1                                         !
 	    	 Xi% = 400                                           !
 	    EndIf                                                  !
   Next XI%																								   !
   If Found% = 0 Then Begin																	 ! No esta en la lista
   	 Print "RIFA NO ESTA EN LISTA"
   	 Xlst$ = ""
   	 For XI% = 1 To 300 STEP 4                               ! 
   	   Xtmp$ = Mid$(Xlista$,XI%,4)
   	   U.Error% = -1
   	   Xval% = Val(Xtmp$)
   	   If U.Error% <> -1 Then Xtmp$ = "0000"
   	   If Xtmp$ = "0000" Then Begin
   	   	  XTMP$ = Key$
   	   	  KEY$ = "0000"
   	   EndIf
      Xlst$ = Xlst$ + Xtmp$
 	   Next XI%
     Xlista$ = Left$(Xlst$+String$(300,"0"),300)            !
     Write Form "C1 C300"; #45 ; Xrifa$,                    \! Grabacion registro 
        Xlista$                                              !
   EndIf Else Print "RIFA ESTA EN LISTA"
   	
 EndIf  Else Begin                                           !
 	 Xlista$ = Key$                                   !
 	 Xlista$ = Left$(Xlista$+String$(300,"0"),300)            !
 	 Xlista$ = Xlista$                                  !
 	 Write Form "C1 C300"; #45 ; Xrifa$,                      \! Grabacion registro 
      Xlista$                                                !
 EndIf                                                       !
End Sub 

Sub Apertura.Archivos
    TOpen% = 0
    Again.Procesa:
    Terror% = -1
    Open IFILE$ As 1
    If Terror% = 0 Then Begin 
 	     Wait ; 1800
 	     TOpen% = TOpen% + 1 
 	     If TOpen% > 5 Then Stop 
 	     GoTo Again.Procesa
    Endif 
    Open "TF:P0004" KEYED RECL 40  As 44           			                ! Definicion de Rifas
    Open "TF:LR004" KEYED RECL 301 As 45                                ! Lista de Rifas
    Create "ADX_UDT1:CARGAERR.TXT" As 2                                 ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
 String U.Cod$, U.Name$, U.Fec1$, U.Fec2$, U.Vlr$, U.Bol$, U.Con$, U.Msg$, U.Stat$, LEC$, U.Clte$, U.Promo$
 Integer*1 xi%
  U.Qty2% = 0
  LEC$="C4 C18 2C4 C5 C2 C1 C2" 
  While (1)																												! Mientras no EOF
    Read #1; U.Cod$, U.Name$, U.Msg$, U.Vlr$, U.Bol$, U.Con$,U.clte$

     U.Qty2% = U.Qty2% + 1
     U.Error% = -1
     U.Fec1$ = "20" + date$
     U.Fec2$ = "20" + date$
     If U.cod$ <> U.Clte$ Then \																! Si para un cliente especifico
      U.Promo$ = Right$("0000"+U.clte$,4) Else \								! Para un tipo de cliente
      U.Promo$ = "0000"																					! Para todos los clientes
      
      U.Promo$ = Ucase$(U.Promo$)
      
      If MATCH("TC",U.Promo$,1) <= 0 Then U.Promo$ = "0000"     ! Para todos los clientes 

      Print "CARGANDO RIFA "+U.COD$+"  PARA "+U.PROMO$
      
      key$ = Pack$(Right$("0000"+U.cod$,4) + U.promo$)	        ! Codigo promocion
      U.cod$  = Right$("0000"+U.cod$,4)                         ! Codigo de la rifa
      U.Name$ = Left$(U.Name$+"                  ",18)          ! Nombre
      U.fec1$ = Pack$(Right$("00000000"+U.fec1$,8))             ! Fecha Inicio
      U.fec2$ = Pack$(Right$("00000000"+U.fec2$,8))             ! Fecha Final
      U.Vlr$  = Pack$(Right$("0000000000"+U.Vlr$,10))           ! Valor
      U.Bol$  = Pack$(Right$("0000"+U.Bol$,4))                  ! Numero Boletas
      U.Con$  = Pack$(Right$("00"+U.Con$,2))                    ! Condicion
      U.Msg$  = Pack$(Right$("0000"+U.Msg$,4))                  ! Mensaje
  
      Write Form LEC$;#44 ; KEY$,    \! Grabacion registro 
      U.Name$, U.Fec1$, U.Fec2$, U.Vlr$, U.Bol$, U.Con$, U.Msg$

     If U.Error% = -1 Then Begin
     	 Key$ = Right$("0000"+U.cod$,4)
       U.QtyOk% = U.QtyOk% + 1 
       Locate 13,27 : Print Str$(U.QtyOk%)
       U.Msg$ = UnPack$(U.Msg$)
       Call Osshell("C:\GRMODUL\MSGCARGA.286 "+"C:\PROMODAT\MSGD"+U.Msg$+".PRM") ! Carga mensaje
       Call LISTA.RIFAS
     EndIf Else Begin
       U.QtyNok% = U.QtyNok% + 1
       Locate 14,27 : Print Str$(U.QtyNok%)
       Write #2; Unpack$(Key$),UE.Msg$,FINR$
     EndIf    
  Wend 																													  ! Fin del ciclo de carga

End Sub

Sub Carga.Options.Terminal
Integer*1 N.TERM%, I%,	OK%
Integer*2 UE%,STATUS%, IN.TERM%
String	  COMPL$, XKEY$, DUMMY$

IN.TERM% = 50
Open "EAMTERMS" KEYED RECL 72 AS 28
For I% = 1 TO IN.TERM%
  XKEY$ = PACK$(RIGHT$(STRING$(4,"0")+STR$(I%),4))
  U.Error% = -1
  READ FORM "C2 C66 I2 I2";#28 AUTOLOCK KEY XKEY$;Dummy$,COMPL$,STATUS%,UE%
  STATUS% = STATUS% OR 0800H
  IF U.Error% = -1 THEN \
	  Write FORM "C2 C66 I2 I2";#28 AUTOUNLOCK; XKEY$,COMPL$,STATUS%,UE%
Next I%
Close 28

End Sub 

Sub Fin.Proceso
    Call Carga.Options.Terminal
    UE.MSG$ = "Fin Carga Masiva Rifas, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****     CARGA MASIVA PARAMETRIZACION RIFAS          ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Diciembre  05 2012 G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : TF:P0004"
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
IFILE$ = COMMAND$																												  ! Nombre archivo de carga
If Ifile$ = "VERSION" Then BEGIN 
   Print "Modulo Carga Rifas Grupo Retail Version Dic/05/2012"
   Stop 
ENDIF 
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERR = "IO" Then Resume
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
  	 Terror% = 0 
     UE.MSG$ = "ARCHIVO "+IFILE$+" NO EXISTE... "
     Call Msg.Log
     Resume 
  EndIf 
  If ERRF% = 44 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 45 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 45 And ERR = "EF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 28 And ERR = "EF" THEN Begin
     Resume 
  EndIf 



  If ERRF% = 44 AND (ERR = "OE" OR ERR = "FU") Then Begin
     CREATE POSFILE "TF:P0004" KEYED 4,,,15000 RECL 40   \! si no existe lo crea.
                           AS 44 compound perupdate 		   ! 
     Resume        
  EndIf 

  If ERRF% = 45 AND (ERR = "OE" OR ERR = "FU") Then Begin
     CREATE POSFILE "TF:LR004" KEYED 1,,,100 RECL 301   \! si no existe lo crea.
                           AS 45 compound perupdate 		   ! 
     Resume        
  EndIf 


 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop

