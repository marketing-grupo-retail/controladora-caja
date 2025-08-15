!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0209.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga Archivo Diagnosticos       *
!**************************************************

!--- Definicion de Variables de trabajo
Integer*1  Global SO%
String     Global Input.File$

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86          				   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes

Sub Adxserve(RET,FUNC,PARM1,PARM2)  External
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub


FUNCTION INICIO01
   MSG$ = "CARGA ARCHIVO DIAGNOSTICOS "
   CALL.ORDER% = 211                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    CALL SETF("0000000")				   !
    CALL CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

Sub Mensaje
  
    Call ADXSERVE(0,26,1,MSG$)
 
End Sub 

Function ACT.MAESTRO				           ! Actualizacion Maestro Convenios
  I% = 1
  WHILE EOF$ = "0" 					   ! Si no es EOF()
    READ #AREA2%;LINE REGISTRO$                            ! Lectura
    IF LEN(REGISTRO$) = 77 THEN BEGIN			  \!
      I% = I% + 1
      MSG$="Registros Procesados : "+STR$(I%)     	   !
      CALL Mensaje 
      DM.CONVEN$ = MID$(REGISTRO$,1,6)  		   ! Codigo diagnostico
      DM.CONVEN$ = PACK$(DM.CONVEN$)		   ! Codigo diagnostico
      DM.RAZON$  = MID$(REGISTRO$,7,70)			   ! Nombre del diagnistico
!      DM.CODALT$ = MID$(REGISTRO$,45,12)		   ! Accion a tomar
      DM.CODALT$ = "000000"               		   ! Alterno 
      DM.ACCION$ = MID$(REGISTRO$,77,1)			   ! Accion a tomar
      IF DM.ACCION$ = "3" THEN DELREC AREA1%;DM.CONVEN$   \! Borrado de registro
       ELSE BEGIN
        LEC$="C3 C40 C12"
        WRITE FORM LEC$;#AREA1%;                             \! Graba registro IPS
        DM.CONVEN$, DM.RAZON$, DM.CODALT$
      ENDIF
    ENDIF
  WEND							    ! Fin Ciclo
  WAIT;500						    ! 
  Delete AREA2%						    ! Cierra Archivo carga
  CALL QUIT2						    ! Fin Ejecucion Programa
END FUNCTION						    ! Fin de la funcion
!--- Fin de la actualizacion del maestro

Sub Carga.Background
      ARCH2$ = ALLTRIM(Input.File$)                         ! Busqueda de plano digitado
      OPEN ARCH2$ AS AREA2%
      Call ACT.MAESTRO
End Sub 

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0201
SO% = 0 
Input.FILE$ = "C:\DATOS\DIAGNOS.DAT"
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
EOF$="0"
ARCH1$="DIAGNOST"	                       	   ! Asignar nombre archivo red prestadora
OPEN ARCH1$ KEYED RECL 55 AS AREA1%                        ! Abre archivo de red prestadora
TERM$ = " "					           ! Inicializo Libreria
FIN$="0"						   !
Call Carga.Background

Stop

!----
!---- Fin del bloque principal
!----

E0201:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 3,,,30000 RECL 55  \! si no existe lo crea.
                        AS AREA1% compound perupdate 		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$="Error, Archivo "+ARCH2$+" No Existe ¢ No Se Puede Abrir"
            CALL Mensaje 
            WAIT;3000					   !
            CALL QUIT2
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN      \! Valida Lectura
            EOF$ = "1"                                     ! temporal de carga
            RESUME				           ! 
         ENDIF						   !
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         Call Mensaje
         STOP
!--- Fin despliegue de errores

!--- Fin del programa EPSP0209.BAS
