!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0201.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga Archivo de Planes          *
!**************************************************
! Se modifica la longitud del registro de carga de 142
! a 146 caracteres por aumento en codigo del plan
! ajustado el 13 julio de 2006 OVS - ASIC
!--------------------------------------------------
! Mod29Jul2008
! Se adiciona flag para el control de la impresion
! del tiquete adicional por plan
! desarrollado por Grupo Retail - OVS
!-------------------------------------------------

!--- Definicion de Variables de trabajo

Integer*1  Global SO%
String     Global Input.File$

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86  	   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes


Function INICIO01
   MSG$ = "CARGA ARCHIVO DE PLANES   "
   Call.ORDER% = 211                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)
   Call MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
End Function
!--- Fin de la funcion de inicio

Function QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    Call SETF("0000000")				   !
    Call CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    Call DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
End Function
!--- Fin de la funcion de salida

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Sub Mensaje
  
  If SO% = -1 Then Begin
    Call ADXSERVE(0,26,1,msg$)
  EndIf Else Call MSG.ERR(03,MSG$)				   ! Mensaje 
  
End Sub 

Function ACT.MAESTRO				           ! Actualizacion Maestro Convenios
  I% = 1
  WHILE EOF$ = "0" 					   ! Si no es EOF()
    READ #AREA2%;LINE REGISTRO$                            ! Lectura
    IF LEN(REGISTRO$) = 148 THEN BEGIN			  \!
      I% = I% + 1
      MSG$="Registros Procesados : "+STR$(I%)     	   !
      Call Mensaje 
      DM.CONVEN$ = PACK$(MID$(REGISTRO$,1,16))		   ! Codigo Convenio
      DM.RAZON$  = MID$(REGISTRO$,17,40)		   ! Nombre convenio
      DM.FECINI$ = PACK$(MID$(REGISTRO$,57,6))		   ! Fecha inicial
      DM.FECFIN$ = PACK$(MID$(REGISTRO$,63,6))		   ! Fecha Final
      DM.VLRCON$ = PACK$(MID$(REGISTRO$,69,10))		   ! Valor Contrato
      DM.VLRDES$ = PACK$(MID$(REGISTRO$,79,08))		   ! Valor Max. Compra
      DM.VALMED$ = MID$(REGISTRO$,87,01)		   ! Valida Medico
      DM.VALMCA$ = MID$(REGISTRO$,88,01)		   ! Valida Mercancia
      DM.VALIPS$ = MID$(REGISTRO$,89,01)		   ! Valida IPS
      DM.VALDIA$ = MID$(REGISTRO$,90,1)			   ! Validacion diagnostico
      DM.FACTURA$ = MID$(REGISTRO$,91,1)		   ! Factura Vlr Comercial/Catalogo/Ptg
      DM.DSCTO$  = MID$(REGISTRO$,92,3)			   ! Ptg de descuento
      DM.VALMET$ = MID$(REGISTRO$,95,01)		   ! Calculo coopago 
      DM.METPRA$ = PACK$(MID$(REGISTRO$,96,06))		   ! Coopago 1
      DM.METPRB$ = PACK$(MID$(REGISTRO$,102,06))	   ! Coopago 2
      DM.METPRC$ = PACK$(MID$(REGISTRO$,108,06))	   ! Coopago 3
      DM.METPRD$ = PACK$(MID$(REGISTRO$,114,06))	   ! Coopago 4
      DM.DSCVLR$ = MID$(REGISTRO$,120,01)		   ! Dscto < $ o Vlr Comercial
      DM.PTG1$   = MID$(REGISTRO$,121,03)		   ! Descuento < $ Trx
      DM.PTG2$   = MID$(REGISTRO$,124,03)		   ! Descuento 2
      DM.PTG3$   = MID$(REGISTRO$,127,03)		   ! Descuento 3
      DM.PTG4$   = MID$(REGISTRO$,130,03)		   ! Descuento 4
      DM.NIT$    = PACK$(MID$(REGISTRO$,133,10))	   ! Codigo del convenio
      DM.VALFAC$ = MID$(REGISTRO$,143,1)                   ! Valida documento
      DM.VALBEN$ = MID$(REGISTRO$,144,1)                   ! Valida beneficiario
      DM.CAPITACION$ = MID$(REGISTRO$,145,1)               ! Si es Capitacion
      DM.COPIATIQ$ = Mid$(REGISTRO$,147,1)                 ! Copia Tiquete 
!-- Validar posicion 146 

      DM.ACCION$ = MID$(REGISTRO$,148,1)		    ! Accion a realizar
      
      IF DM.ACCION$ = "3" THEN DELREC AREA1%; DM.CONVEN$   \! Retira registro
      ELSE BEGIN					    ! Actualiza registro
      LEC$="C8 C40 2C3 C5 C4 4C1 4C3 C5 3C1 4C3 C3 4C1"
      WRITE FORM LEC$;#AREA1%;                             \! Graba registro Convenio
       DM.CONVEN$, DM.RAZON$,  DM.FECINI$, DM.FECFIN$,     \!
       DM.VLRCON$, DM.VLRDES$, DM.VALMED$, DM.VALMCA$,     \!
       DM.VALIPS$, DM.VALMET$, DM.METPRA$, DM.METPRB$,     \!
       DM.METPRC$, DM.METPRD$, DM.NIT$,    DM.VALDIA$,	   \!
       DM.FACTURA$, DM.DSCVLR$, DM.PTG1$,  DM.PTG2$,       \!
       DM.PTG3$,   DM.PTG4$,  DM.DSCTO$,   DM.VALFAC$,     \!
       DM.VALBEN$, DM.CAPITACION$, DM.COPIATIQ$
      ENDIF
    ENDIF
  WEND							    ! Fin Ciclo
  Delete AREA2%						    ! Cierra Archivo carga
  Call QUIT2						    ! Fin Ejecucion Programa
End Function						    ! Fin de la funcion
!--- Fin de la actualizacion del maestro

Sub Carga.Interactiva

WHILE (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      Call INICIO01					   !
      MSG$="Digite Ruta del Archivo Plano de Carga"	   !
      Call MSG.ERR(03,MSG$)				   ! Mensaje 
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("CARGA ARCHIVO DE CONVENIOS","EPSP0201.TXT") !
         Call INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN Call QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN Call QUIT2                 ! Si presiona tecla ESC
      WHILE DM.OPCION$ = " "				   !
        MSG$="Digite Ruta del Archivo Plano de Carga"	   !
        Call MSG.ERR(03,MSG$)				   ! Mensaje 
        RET.ERR% = NXTF(-20)				   !
        Call DM.ERR(RET.ERR%,NXTF$)			   !
        ATTR$ = SETF(PRM.ON$)                              ! 
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.OPCION$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN 		  \!
           Call HELP("CARGA ARCHIVO DE CONVENIOS","EPSP0201.TXT") !
           Call INICIO01				   !
        ENDIF						   !
        IF (ENDF = F3.SALIR) THEN Call QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN Call QUIT2               ! Si presiona tecla ESC
      WEND
      ARCH2$ = ALLTRIM(DM.OPCION$)                         ! Busqueda de plano digitado
      OPEN ARCH2$ AS AREA2%
      Call ACT.MAESTRO
WEND
End Sub 

Sub Carga.Background
      Input.File$ = "C:\DATOS\PLANES.DAT" 
      ARCH2$ = ALLTRIM(Input.File$)                        ! Busqueda de plano digitado
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
Input.File$ = COMMAND$                  ! Toma archivo de entrada como parametro
Call INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
EOF$="0"
ARCH1$="CONVENIO"		                       	   ! Asignar nombre archivo convenios
OPEN ARCH1$ KEYED RECL 106 AS AREA1%                        ! Abre archivo de Convenios
TERM$ = " "					           ! Inicializo Libreria
FIN$="0"

If Input.File$ <> "BACKGRND" Then Begin 
   RET.ERR%=INITDM(TERM$)					   !
   Call DM.ERR(RET.ERR%,INITDM$)				   !
   RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
   Call DM.ERR(RET.ERR%,OPNDIS$)
   Call Carga.Interactiva
EndIf Else Begin 
   SO% = -1 
   Input.File$ = "C:\DATOS\PLANES.DAT"
   Call Carga.Background
EndIf 
Stop

!----
!---- Fin del bloque principal
!----

E0201:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 8,,,5000 RECL 106 \! si no existe lo crea.
                        AS AREA1% compound perupdate  	   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$="Error, Archivo "+ARCH2$+" No Existe ¢ No Se Puede Abrir"
            Call Mensaje
            WAIT;3000					   !
            Call QUIT2
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
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         Call Mensaje
         WAIT;3000
         STOP
!--- Fin despliegue de errores

!--- Fin del programa EPSP0201.BAS
