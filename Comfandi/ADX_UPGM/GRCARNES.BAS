!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRCARNES.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Activacion Promocion Carnes      *
!**************************************************

!--- Definicion de Variables de trabajo


STRING    Global U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, UHORA$, UFECHA$, IR.USERDATA$
INTEGER*4 Global U.INI%, U.FIN%, U.ACT%

%INCLUDE EAMITEMR.J86              ! MAESTRO DE ARTICULOS
%INCLUDE FISPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86       ! Inclucion Libreria Display Manager
%INCLUDE FISPRUTI.BAS				  	   ! Rutinas Comunes

Function QUIT2     					   		 ! Funcion salida programa
    Call MSG.ERR(05,"Proceso Terminado ...")
    Close AREA1% : Close 2 : Close 4
    wait ; 1200
    Call SETF("0000000")				   !
    Call CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    Call DM.ERR(RET.ERR%,CLSDIS$)			   !
    Stop
End Function
!--- Fin de la funcion de salida

Function INICIO01
   Call.ORDER% = 01                                           ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio

Sub CAPTURA.DATOS
String U.KEY$, LEC$, RBUFFER$, finr$
Integer*4 X.Len%
  FINR$ = Chr$(13) + Chr$(10)
  Call MSG.ERR(05,"Actualizando Estado Promocion")
  wait ; 1200
  U.KEY$ = "01"
  U.KEY$ = PACK$(U.KEY$)
  BAN.PRG$ = "0"
  LEC$="C1 C1"	                      !
  Write Form LEC$	;#AREA1% ;         \! Grabacion de registros
        U.KEY$, DM.OPCION$  					! Numero actual de registro
        
  Call MSG.ERR(05,"Actualizando Items 888012 y 888087")
  wait ; 1200          
  BAN.PRG$ = "0"        
  IR.ITEMCODE$ = PACK$("000000888012")
  %INCLUDE EAMIRRD1.J86
  If BAN.PRG$ = "0" Then BEGIN 
  	 If DM.OPCION$ = "1" Then \
        IR.LINKEDTO$ = PACK$("3348") Else \
        IR.LINKEDTO$ = PACK$("0000")
     %INCLUDE EAMIRWR1.J86
  EndIf 
  
  BAN.PRG$ = "0"        
  IR.ITEMCODE$ = PACK$("000000888087")
  %INCLUDE EAMIRRD1.J86
  If BAN.PRG$ = "0" Then BEGIN 
  	 If DM.OPCION$ = "1" Then \
        IR.LINKEDTO$ = PACK$("3348") Else \
        IR.LINKEDTO$ = PACK$("0000")
     %INCLUDE EAMIRWR1.J86
  EndIf 

  Call MSG.ERR(05,"Almacenando Auditoria Movimiento")
  wait ; 1200
  IR.LINKEDTO$ = "PROCESADO: ("+DM.OPCION$+") [ "+UFECHA$+" - "+UHORA$+" ]"
  Rbuffer$ = IR.LINKEDTO$
  X.Len% = Len(Rbuffer$)						  					  							! Toma longitud del registro
  Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
  Write form Lec$; #2 ; Rbuffer$, Finr$           							! Graba registro
  
End Sub

Sub VALIDA.DATOS
String U.KEY$, LEC$
  U.KEY$ = "01"
  U.KEY$ = PACK$(U.KEY$)
  BAN.PRG$ = "0"
  LEC$="C1 C1"	                           !
  READ FORM LEC$;#AREA1% KEY U.KEY$;                      \! Lee Reg Cabecera 
  U.KEY$,U.SERIAL$
  IF BAN.PRG$ = "0" THEN BEGIN
     Call MSG.ERR(03,U.SERIAL$)
  ENDIF Else Call MSG.ERR(03,"0")
  UHORA$  = "  "+Left$(TIME$,2)+":"+Mid$(TIME$,3,2)+":"+Right$(TIME$,2)
  UFECHA$ = "  20"+Left$(DATE$,2)+"/"+Mid$(DATE$,3,2)+"/"+Right$(DATE$,2)
  Call Msg.Err(01,UFECHA$)
  Call Msg.Err(02,UHORA$)
End Sub

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
Call INICIADM 				                   										! Inicializacion Variables Display Manager
AREA1% = 1
ARCH1$ = "TF:CARNES" 
OPEN ARCH1$ KEYED RECL 2 AS AREA1%                        	! Abre archivo de control
ARCH2$ = "TF:AUDMOV" 
OPEN ARCH2$ As 2 Append                                    	! Archivo de auditoria
Open "EAMITEMR" KEYED RECL 77   AS 4			                  ! Apertura Eamitemr
TERM$ = " "					           															! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   													!
Call DM.ERR(RET.ERR%,INITDM$)				   											!
RET.ERR% = OPNDIS("ADX_UPGM:PRMCARNE.PNT")	           			! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call INICIO01					   																		!
Call VALIDA.DATOS				           													! 
WHILE (FIN$ = "0") 					   															! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   													!
      MSG$="Seleccione Opcion a Ejecutar "	   							!
      Call MSG.ERR(05,MSG$)				   												! Mensaje 
      RET.ERR% = NXTF(-20) 			           									! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   										!
      ATTR$ = SETF(PRM.ON$)                                	!
      INP$ = UPDF                                          	! Captura dato en pantalla
      DM.OPCION$ = INP$                                    	! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("Promocion Carnes ","PRMCARNE.TXT") 			!
         Call INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN Call QUIT2                 	! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN Call QUIT2                 	! Si presiona tecla ESC
      While MATCH(DM.OPCION$,"01",1) <= 0 
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        	! Captura dato en pantalla
        DM.OPCION$ = INP$                                  	! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           Call HELP("Promocion Carnes ","PRMCARNE.TXT") 		!
           Call INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN Call QUIT2               	! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN Call QUIT2               	! Si presiona tecla ESC
      Wend
      Call CAPTURA.DATOS                                   	! CAPTURA DATOS NUMFISCAL
      FIN$ = "1"
Wend
Call QUIT2
Stop 

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 1,,,10 RECL 2   \! si no existe lo crea.
                        AS AREA1% COMPOUND PERUPDATE		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa

         IF ERRF% = 2 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE ARCH2$ AS 2 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa

         IF ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa NUMFIS.BAS
