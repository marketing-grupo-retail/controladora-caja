!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRFISCOL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion de Numeracion Fiscal  *
!**************************************************
! Mod 29Oct2018
! Se aumenta tamaño archivo numeracion fiscal a 45
! caracteres para manejo fecha vencimiento 
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

!--- Definicion de Variables de trabajo

STRING    Global U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, U.FECV$
INTEGER*4 Global U.INI%, U.FIN%, U.ACT%

%INCLUDE FISPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86       ! Inclucion Libreria Display Manager

FUNCTION TRADUCE.ERROR                                       !
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


SUB VALIDA.DATOS
STRING U.KEY$, LEC$
  If Val(DM.OPCION$) <= 0 Then Begin
  	 Print "Numero de terminal no valido "
  	 Stop 
  EndIf
  U.KEY$ = RIGHT$("0000"+DM.OPCION$,4)
  U.KEY$ = PACK$(U.KEY$)
  BAN.PRG$ = "0"
  LEC$="C2 C15 C4 2I4 C6 2C3 I4"	                           !
  READ FORM LEC$;#11 KEY U.KEY$;                      \! Lee Reg Cabecera 
  U.KEY$,U.SERIAL$, U.PREF$, U.INI%, U.FIN%, U.AUT$, U.FEC$, U.FECV$, U.ACT%
  IF BAN.PRG$ = "0" THEN Begin
  	Print "NUMERO ACTUAL DE LA TERMINAL ",U.ACT%
  	Input "NUMERO DE TRANSACCION NUEVO ";U.ACT%
    DM.OPCION$ = Pack$(RIGHT$("0000"+DM.OPCION$,4))
    Write Form "C2 C15 C4 2I4 C6 2C3 I4"	;#11 ;         \! Grabacion de registros
        DM.OPCION$, \ Numero de terminal
        U.SERIAL$,  \ Numero serial
        U.PREF$,    \ Prefijo de facturacion
        U.INI%,     \ Rango inicial
        U.FIN%,     \ Rango Final
        U.AUT$,     \ Numero Autorizacion
        U.FEC$,     \ Fecha de autorizacion
        U.FECV$,    \ Fecha de Vencimiento
        U.ACT%      ! Numero actual de registro
    Print "NUMERACION ACTUALIZADA, FIN DEL PROCESO"
  EndIf Else Print "TERMINAL NO DEFINIDA CON NUMERACION"
  	
END SUB



ON ERROR GOTO E0101
CLEARS
ARCH1$ = "TF:NUMFIS" 
OPEN ARCH1$ KEYED RECL 45 AS 11                            ! Abre archivo de control
Print "Grupo Retail Ltda"
Print "ACTUALIZACION CONSECUTIVOS DE NUMERACION FISCAL  Ver. 29/Oct/2018"
Print "-----------------------------------------------"
Print " "
Input "NUMERO DE TERMINAL A ACTUALIZAR ";DM.OPCION$
Call VALIDA.DATOS
Close 11
Stop 

!----
!---- Fin del bloque principal
!----

E0101:
         If ERR = "IH" THEN BEGIN                          ! Si encuentra Error de dato
         	  Print "TIPO DE DATO INVALIDO"
         	  Stop 
         EndIf 
         IF ERRF% = 11 AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 2,,,5000 RECL 45  \! si no existe lo crea.
                        AS AREA1% COMPOUND PERUPDATE		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa NUMFIS.BAS
