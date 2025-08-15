
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRACTBRG.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Actualizacion Bonos recompra     *
!                contingencia                     *
!**************************************************
! Observaciones
!

%Environ C

String Global ERRFX$, IFILE$, MSG$
Integer*1 Ban.Prg%, True, False


%INCLUDE ADX_UPGM:BASROUT.J86

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        Else SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Mensaje(Xmsg$)
String XMSG$
  Locate 24,1 : Print XMSG$
 
End Sub 

Function Estado.Proceso
String     X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.TIPO$, X.RESOL$, Xdata$, X.AUT2$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%
Integer*1 Estado.Proceso
       X.KEY$ = "0099"                                     ! Terminal para control consecutivo creditos
       X.KEY$ = PACK$(X.KEY$)
       X.LEC$="C2 C15 C4 2I4 C6 C3 I4"	                   ! Formato Grabacion Registro
       Ban.Prg% = True
       Read FORM X.LEC$;#11 KEY X.KEY$;                   \! Busca Terminal
            X.KEY$, 																		  \! Numero de la terminal
            X.SERIAL$,  																  \! Serial de la terminal
            X.PREF$, 																		  \! Prefijo de facturacion
            X.INI%, 																		  \! Numero inicial de Facturacion
            X.FIN%, 																		  \! Rango final de Facturacion
            X.AUT$, 																		  \! Numero resolucion de la DIAN Viejo
            X.FEC$, 																		  \! Fecha Autorizacion
            X.ACT%  																			 ! Numero Actual de Facturacion
       If Ban.Prg% = False Then Begin
       	  Estado.Proceso = 0
       	  Exit Function 
       EndIf
       If X.AUT$ = "PROCES" Then Begin
       	  Print "PROCESO YA FUE EJECTADO EN ESTA TIENDA"
       	  Close 11
       	  Stop
       EndIf
       ESTADO.PROCESO = 0 
End Function 

Sub Cambio.Fiscal
String     X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.TIPO$, X.RESOL$, Xdata$, X.AUT2$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%, Gri%
Integer*1  Proc%
Print "Grupo Retail Ltda, Cambio Longitud Resolucion Fiscal 03/Nov/2016"
Ban.Prg% = True
OPEN "TF:NUMFIS" KEYED RECL 42 AS 11  								! Abre control numeacion fiscal
Proc% = ESTADO.PROCESO
If Proc% = 0 Then Begin
	 For GrI% = 1 To 99																	! Barre todas las terminales
	     LOCATE 3,1 : Print "Procesando Terminal "+Str$(Gri%)
	     X.Key$ = Right$("0000"+Str$(Gri%),4)                ! Nro terminal a procesar
       X.KEY$ = PACK$(X.KEY$)															 ! Arma llave
       X.LEC$ = "C2 C15 C4 2I4 C6 C3 I4"	                 ! Formato anterior Registro
       Ban.Prg% = True
       Read FORM X.LEC$;#11 KEY X.KEY$;       \! Busca Terminal
            X.KEY$, 																		  \! Numero de la terminal
            X.SERIAL$,  																  \! Serial de la terminal
            X.PREF$, 																		  \! Prefijo de facturacion
            X.INI%, 																		  \! Numero inicial de Facturacion
            X.FIN%, 																		  \! Rango final de Facturacion
            X.AUT$, 																		  \! Numero resolucion de la DIAN Viejo
            X.FEC$, 																		  \! Fecha Autorizacion
            X.ACT%  																			 ! Numero Actual de Facturacion
	     If Ban.Prg% = True Then Begin											 ! Registro Encontrado
	     	  X.Aut$  = UnPack$(X.Aut$)
	     	  X.Aut$  = Pack$(Right$("00000000000000"+X.Aut$,14)) ! Ajusta dato nuevo
	     	  X.Aut2$ = "PROCES"
          X.Key$ = Right$("0000"+Str$(Gri%),4)           ! Nro terminal a procesar
          X.KEY$ = PACK$(X.KEY$)											   ! Arma llave

	     	  X.Lec$ = "C2 C8 C7 C4 2I4 C6 C3 I4"	             ! Nuevo Formato 03Nov2016
        Write Form X.LEC$;#11;                            \! Graba numero de terminal
            X.KEY$, 																		  \! Numero de la terminal
            X.SERIAL$,  																  \! Serial de la terminal
            X.AUT$, 																		  \! Numero resolucion de la DIAN Nuevo
            X.PREF$, 																		  \! Prefijo de facturacion
            X.INI%, 																		  \! Numero inicial de Facturacion
            X.FIN%, 																		  \! Rango final de Facturacion
            X.AUT2$, 																		  \! Numero resolucion de la DIAN Anterior
            X.FEC$, 																		  \! Fecha Autorizacion
            X.ACT%  																			 ! Numero Actual de Facturacion
	     	  
	     EndIf Else Begin 
	     	 If Gri% = 99 Then Begin 													 ! Registro remisiones
	          X.Key$ = Right$("0000"+Str$(Gri%),4)           ! Nro terminal a procesar
            X.KEY$ = PACK$(X.KEY$)											   ! Arma llave
	     	    X.Aut$  = Pack$("11111111111111")              ! Ajusta dato nuevo
	     	    X.Aut2$ = "PROCES"
	     	    X.Lec$ = "C2 C8 C7 C4 2I4 C6 C3 I4"	           ! Nuevo Formato 03Nov2016
	     	    X.PREF$ = "AAAA"
	     	    X.SERIAL$ = " "
	     	    X.INI% = 1
	     	    X.FIN% = 1000000
	     	    X.ACT% = 0
	     	    X.FEC$ = Pack$(Date$)
        Write Form X.LEC$;#11;                \! Graba numero de terminal
            X.KEY$, 																		  \! Numero de la terminal
            X.SERIAL$,  																  \! Serial de la terminal
            X.AUT$, 																		  \! Numero resolucion de la DIAN Nuevo
            X.PREF$, 																		  \! Prefijo de facturacion
            X.INI%, 																		  \! Numero inicial de Facturacion
            X.FIN%, 																		  \! Rango final de Facturacion
            X.AUT2$, 																		  \! Numero resolucion de la DIAN Anterior
            X.FEC$, 																		  \! Fecha Autorizacion
            X.ACT%  																			 ! Numero Actual de Facturacion
	     	 	
	     	 EndIf  
	     EndIf	
	 Next Gri%
	 Close 11
	 Print "PROCEDIMIENTO TERMINADO SATISFACTORIAMENTE..."
	 Stop 
EndIf
End Sub 

!---
! Bloque Principal
!---

On Error GoTo ERROR.PRG
IFILE$ = Command$                						 ! Como esta ejecutando
IF IFILE$ = "VERSION" THEN BEGIN 
	 PRINT "MODULO REDEFINICION NUMERACION FISCAL Ver 1.0 03/Nov/2016 02:20 pm"
	 Stop
ENDIF
Clears
True = -1
False = 0

While (1)                                    ! Ciclo infinito
 Call Cambio.Fiscal             				     ! Generar Copia Seguridad
Wend
Stop

ERROR.PRG:
     Ban.Prg% = False 
     Call TRADUCE.ERROR
     If ERR = "EF" Then Resume
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If (ERR = "OE" OR ERR = "FU") Then Resume

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
