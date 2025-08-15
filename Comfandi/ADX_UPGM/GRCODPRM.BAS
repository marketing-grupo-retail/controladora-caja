!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRCODPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Consulta Promociones Definidas   *
!                Motor de Promociones             *
!**************************************************


!--- Definicion de Variables de trabajo
String    Global IR.USERDATA$, DM.PROMO$, GR.PROMO$, GR.FINI$, GR.FFIN$, GR.TDSC$, \
                 UE.DATA2$, UE.SALIDA$, UE.FINI$, UE.FFIN$, UE.ESTADO$
Integer*1 Global GR.CTLITM%
Integer*4 Global CITM%, UE.LINEAS%

%INCLUDE EAMITEMR.J86      	 ! Variables EAMITEMR
%INCLUDE EXIRTSVA.J86				 ! Expansion EAMITEMR

%INCLUDE POSPVARI.BAS				 ! Variables programa
%INCLUDE DMEXTR.J86          ! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS			   ! Rutinas Comunes
%Include BASROUT.J86

Function INICIO1
  Call.ORDER% = 25                                           ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
  MSG$ = "CONSULTA DEFINICION PROMOCION"
  Call MSG.ERR(1,MSG$)				             ! Mensaje cabecera pantalla
  MSG$="Ingrese Codigo de la promocion "
  Call MSG.ERR(03,MSG$)				             ! Mensaje cabecera pantalla
End Function
!--- Fin de la funcion de inicio

Function QUIT2     					   ! Funcion salida programa
    Close AREA3%					     !
    Call SETF("0000000")		   !
    Call CLRSCR						     !
    RET.ERR%= CLSDIS					 !
    Call DM.ERR(RET.ERR%,CLSDIS$)			   !
    Stop
End Function
!--- Fin de la funcion de salida

Sub VALIDA.ITEM(Xplu$)
String Xplu$
   IR.ITEMCODE$ = Pack$(Right$("000000000000"+XPLU$,12))  								! Toma Item a consultar
   BAN.PRG$ = "0"																													! Control Lectura
   %INCLUDE EAMIRRD1.J86																									! Busca Item
   If BAN.PRG$ = "0" Then BEGIN 																					! Encontrado
   	  If UNPACK$(IR.INDICAT2$) = "05" Then BEGIN 													! Item MP 05
   	  	 IR.ITEMCODE$ = IR.SALEQUAN$ + IR.SALEPRIC$                       ! Toma PLU Asociado al EAN
   	  	 %INCLUDE EAMIRRD1.J86																						! Busca Item
   	  EndIf																																! Fin MP 05
   EndIf
   If BAN.PRG$ = "1" Then IR.ITEMNAME$ = "PRODUCTO NO EXISTE"
End Sub 

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub STAT.PROMO
String Xkey$, X1$, X2$, X3$, X4$
Xkey$ = Pack$(Right$("0000"+DM.CODIGO$,4))
BAN.PRG$ = "0"

Read Form "C2 2C3 C1";#34 KEY Xkey$;                                 \! Lee Reg Cabecera 
   X1$, X2$, X3$, X4$

If BAN.PRG$ = "0" Then BEGIN 
   UE.FINI$ = UNPACK$(X2$)
   UE.FFIN$ = UNPACK$(X3$)
   UE.FINI$ = "20" + LEFT$(UE.FINI$,2) + "/" + MID$(UE.FINI$,3,2) + "/" + RIGHT$(UE.FINI$,2)
   UE.FFIN$ = "20" + LEFT$(UE.FFIN$,2) + "/" + MID$(UE.FFIN$,3,2) + "/" + RIGHT$(UE.FFIN$,2)
   If X4$ = "1" Then UE.ESTADO$ = "PROMOCION ACTIVA" Else \
   	                 UE.ESTADO$ = "PROMOCION INACTIVA"
EndIf Else BEGIN 
	UE.ESTADO$ = "ERROR DEFINICION PROMOCION"
	UE.FINI$ = "000000"
	UE.FFIN$ = "000000"
  UE.FINI$ = "20" + LEFT$(UE.FINI$,2) + "/" + MID$(UE.FINI$,3,2) + "/" + RIGHT$(UE.FINI$,2)
  UE.FFIN$ = "20" + LEFT$(UE.FFIN$,2) + "/" + MID$(UE.FFIN$,3,2) + "/" + RIGHT$(UE.FFIN$,2)

ENDIF 

End Sub 

Sub Cabecera.Reporte(Xopt%)
Integer*1 Xopt%
  CITM% = CITM% + 1 
  UE.DATA2$ = "COMFANDI"+STRING$(08," ")+"CONSULTA DEFINICION MOTOR PROMOCIONES"+STRING$(10," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "Fecha del Reporte : 20"+LEFT$(DATE$,2) + "/" + MID$(DATE$,3,2) + "/" + RIGHT$(DATE$,2)
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
  Call Write.Data
  If Xopt% = 1 Then Begin
  	 Call STAT.PROMO
  	 UE.DATA2$ = "PROMOCION :"+DM.CODIGO$+ " PRESENCIA DE VENTA DE ITEM  - ESTADO : "+UE.ESTADO$
  	 Call Write.Data
  	 UE.DATA2$ = "VIGENCIA PROMOCION : "+UE.FINI$+" AL "+UE.FFIN$
  	 Call Write.Data
  	 UE.DATA2$ = "TIPO PROMOCION: (1) Dscto Valor  (2) Dscto Porcentaje (3) Precio Final "
  	 Call Write.Data
     UE.DATA2$ = STRING$(77,"-")  
     Call Write.Data
  	 UE.DATA2$ = "SKU/EAN         DESCRIPCION ITEM    TIPO  VALOR/DSCTO  TIPIFICACION"
  	 Call Write.Data
  EndIf

  If Xopt% = 2 Then Begin
  	 Call STAT.PROMO
  	 UE.DATA2$ = "PROMOCION :"+DM.CODIGO$+ " DESCUENTO POR SECCION/DPTO  - ESTADO : "+UE.ESTADO$
  	 Call Write.Data
  	 UE.DATA2$ = "VIGENCIA PROMOCION : "+UE.FINI$+" AL "+UE.FFIN$
  	 Call Write.Data
     UE.DATA2$ = STRING$(77,"-")  
     Call Write.Data
  EndIf

  If Xopt% = 3 Then Begin
  	 Call STAT.PROMO
  	 UE.DATA2$ = "PROMOCION :"+DM.CODIGO$+ " MENSAJE EN TIRILLA DE VENTA - ESTADO : "+UE.ESTADO$
  	 Call Write.Data
  	 UE.DATA2$ = "VIGENCIA PROMOCION : "+UE.FINI$+" AL "+UE.FFIN$
  	 Call Write.Data
     UE.DATA2$ = STRING$(77,"-")  
     Call Write.Data
  	 UE.DATA2$ = "TIPO  DESCRIPCION MENSAJE      PLU/SECCION/TIPIF.   MENSAJE   COMPRA MIN."
  	 Call Write.Data
     
  EndIf

  If Xopt% = 4 Then Begin
  	 Call STAT.PROMO
  	 UE.DATA2$ = "PROMOCION :"+DM.CODIGO$+ " GENERACION BOLETAS RIFAS    - ESTADO : "+UE.ESTADO$
  	 Call Write.Data
  	 UE.DATA2$ = "VIGENCIA PROMOCION : "+UE.FINI$+" AL "+UE.FFIN$
  	 Call Write.Data
     UE.DATA2$ = STRING$(77,"-")  
     Call Write.Data
  	 UE.DATA2$ = "DESCRIPCION RIFA                        MENSAJE   COMPRA MIN.   NRO BOLETAS"
  	 Call Write.Data
     
  EndIf


End Sub 

Sub Grabacion.Interface(Xopt%)
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%, Xopt%
      UE.LINEAS% = UE.LINEAS% + 1
      Call Write.Data
      If UE.LINEAS% > 64 Then Begin 
         Ue.Data2$ = STRING$(77,"-")  
         Call Write.Data
         Write #51; CHR$(12)
         Call cabecera.reporte(Xopt%)
         UE.LINEAS% = 0
      EndIf 
End Sub
!--- Fin grabacion de interface

Sub PRESENCIA.ITEM
String C1$, C2$, C3$, C4$, C5$, C6$, C7$, C9$
Integer*4 Nro.Reg%
 Nro.Reg% = 0
 If END #1 THEN FIN.CARGA.ITEM
 Call Cabecera.Reporte(1)
 While (1)
   READ #1;C1$, C2$, C3$, C4$, C5$, C6$, C7$, C9$
   Nro.Reg% = Nro.Reg% + 1
   Call Msg.Err(3,"Registros Procesados : "+Str$(Nro.Reg%))
   Call VALIDA.ITEM(C1$)
   C1$ = RIGHT$("            "+STR$(VAL(C1$)),12) 												! Plu de promocion
   C2$ = RIGHT$("0000"+C2$,4)          																		! Numero de la promocion / Tipificacion de cliente Add 8 Ago 2011
   C3$ = RIGHT$("0"+STR$(VAL(C3$)),1)             												! tipo de promocion
   C4$ =      (RIGHT$("        "+C4$,8))   										            ! Valor de la promocion
   Ue.Data2$ = C1$ + "   " + IR.ITEMNAME$ + "    " + C3$ + "       " + C4$ +   \!
               "      " + C2$
   Call Grabacion.Interface(1)
   
 Wend 
 FIN.CARGA.ITEM:
   UE.DATA2$ = STRING$(77,"-")  
   Call Write.Data
   Close 1 : Close 51
   WAIT ; 100
End Sub 

Sub DSCTO.SECCION
String UE.DIA$(1), UE.HORA$(2),U.SECC$, U.Cupon$, U.dat$, Xdia$(1), \
       U.Clte$, U.Promo$
Integer*2 XI%
Dim UE.DIA$(10)
Dim UE.HORA$(10,2)
Dim XDIA$(10)

 XDIA$(0) = "DOMINGO   "
 XDIA$(1) = "LUNES     "
 XDIA$(2) = "MARTES    "
 XDIA$(3) = "MIERCOLES "
 XDIA$(4) = "JUEVES    "
 XDIA$(5) = "VIERES    "
 XDIA$(6) = "SABADO    "
 
 If END #1 THEN FIN.CARGA.SECC
 Call Cabecera.Reporte(2)
 While (1)
    Read #1; U.SECC$, U.Cupon$, U.dat$, \
             UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \ Lunes
             UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \ Martes 
             UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \ Miercones
             UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \ Jueves
             UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \ Viernes
             UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \ Sabado
             UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2), \ Domingo
             U.Clte$, U.Promo$                         ! Codigo de la promocion 
             
   U.CLTE$ = Right$("0000"+U.CLTE$,4)
   UE.DATA2$ = "SECCION :"+U.SECC$ + " Dscto :" + U.DAT$ + "%  Tipificacion :" + U.CLTE$
   Call Grabacion.Interface(2)
   UE.DATA2$ = "  "
   Call Grabacion.Interface(2)
   UE.DATA2$ = "          DIA SEMANA    APLICA    HORA INICIO    HORA FINAL "
   Call Grabacion.Interface(2)
   UE.DATA2$ = "                       1.Si 0.No    HH:MM          HH:MM    "
   Call Grabacion.Interface(2)
   
   For XI% = 0 To 6
     UE.HORA$(XI%,1) = Right$("0000"+UE.HORA$(XI%,1),4)
     UE.HORA$(XI%,2) = Right$("0000"+UE.HORA$(XI%,2),4)
     UE.DATA2$ = "          "+XDIA$(XI%) + " :     " + UE.DIA$(XI%) + "        " + \
                 Left$(UE.HORA$(XI%,1),2) + ":" + Right$(UE.HORA$(XI%,1),2) + "          " + \
                 Left$(UE.HORA$(XI%,2),2) + ":" + Right$(UE.HORA$(XI%,2),2) + "          " 
     Call Grabacion.Interface(2)
   Next XI%
 Wend 
 FIN.CARGA.SECC:
   UE.DATA2$ = STRING$(77,"-")  
   Call Write.Data
   Close 1 : Close 51
   WAIT ; 100

End Sub 

Sub PRM.MENSAJE
!30 = Presencia de un producto 
!31 = Presencia de un cliente
!32 = No presencia de una sección 
!33 = Compra mínima 
!34 = Por no presencia de un PLU 
!35 = Presencia de un departamento
!36 = Presencia de una forma de pago
String Xtip$(1), C1$, C2$, C3$, C4$, C5$, C9$
Integer*2 XI%

Dim Xtip$(100)
For XI% = 1 To 100
    Xtip$(XI%) = "TIPO MSG POR DEFINIR   "
Next XI%
Xtip$(30) = "PRESENCIA PRODUCTO     "
Xtip$(31) = "PRESENCIA CLIENTE      "
Xtip$(32) = "NO PRESENCIA SECCION   "
Xtip$(33) = "COMPRA MINIMA          "
Xtip$(34) = "NP PRESENCIA PRODUCTO  "
Xtip$(35) = "PRESENCIA SECCION/DPTO "
Xtip$(36) = "PRESENCIA FORMA PAGO   "

 If END #1 THEN FIN.CARGA.MSG
 Call Cabecera.Reporte(3)
 While (1)
   Read #1;C1$, C2$, C3$, C4$, C5$, C9$
   C1$ = Right$("00"+C1$,2)                    ! Tipo de promocion
   C2$ = Right$("                "+C2$,16)     ! Plu - Cliente Promocion - Tipificacion
   C3$ = Right$("0000"+C3$,4)                  ! Mensaje Promocion
   C4$ = Right$("          "+C4$,10)           ! Control compra Minima
   UE.DATA2$ = C1$ + "    " + Xtip$(Val(C1$)) + " " + C2$ + "       " + \
               C3$ + "  " + C4$
   Call Grabacion.Interface(3)            
 Wend 
 FIN.CARGA.MSG:
   UE.DATA2$ = STRING$(77,"-")  
   Call Write.Data
   Close 1 : Close 51
   WAIT ; 100
End Sub 

Sub PRM.RIFAS
String C1$, C2$, C3$, C4$, C5$, C6$, C7$, C9$, PROM$
Integer*4 Nro.Reg%
 Nro.Reg% = 0
 If END #1 THEN FIN.CARGA.RIFAS
 Call Cabecera.Reporte(4)
 While (1)
   READ #1;C1$, C2$, C3$, C4$, C5$, C6$, C7$
   C2$ = Left$(C2$+String$(40," "),40)						! Descripcion Rifa
   C3$ = Right$("0000"+C3$,4)                     ! Codigo Mensaje
   C4$ = Right$("          "+Str$(Val(C4$)),10)   ! Valor minimo compra
   C5$ = Right$("   "+Str$(Val(C5$)),3)           ! Numero boletas a entregar
   UE.DATA2$ = C2$ + " " + C3$ + " " + c4$ + "           " + c5$
   Call Grabacion.Interface(4)            
 Wend 
 FIN.CARGA.RIFAS:
   Close 1 
   WAIT ; 100
   UE.DATA2$ = "              LISTA PRODUCTOS PARTICIPANTES "
   Call Grabacion.Interface(4)
   BAN.PRG$ = "0"
   PROM$ = Right$("0000"+DM.CODIGO$,4)
   Open "C:\PROMODAT\RIFP"+PROM$+".PRM" As 1 Locked                ! Carga de promocion rifas sistema
   If BAN.PRG$ = "0" Then BEGIN 
   	  If END #1 THEN FIN.ITEMS.RIFAS
   	  While (1)
        READ #1;C1$, C2$
        Nro.Reg% = Nro.Reg% + 1
        Call Msg.Err(3,"Registros Procesados : "+Str$(Nro.Reg%))
        Call VALIDA.ITEM(C2$)
        C2$ = Right$("            "+C2$,12)
        UE.DATA2$ = C2$ + "  " + IR.ITEMNAME$
        Call Grabacion.Interface(4)
   	  Wend 
   ENDIF 
   FIN.ITEMS.RIFAS:
     UE.DATA2$ = STRING$(77,"-")  
     Call Write.Data
     Close 1 : Close 51
     WAIT ; 100
 
End Sub 

Function CONSULTA.PANTALLA                          ! Parametro Programa y archivo
  String HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  Integer*2 CNTI%, NRG%
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			                   ! Despliege de la pantalla
      BAN.PRG$ = "0"
      HLP.FILE$ = UE.SALIDA$
      OPEN HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      IF BAN.PRG$ = "1" THEN BEGIN                        \!
         MSG1$ = "Archivo Reporte "+HLP.FILE$+" No Existe o Sin Informacion"
         CALL MSG.ERR(19,MSG1$): WAIT;1800: EXIT FUNCTION
      ENDIF
      INP2$ = " ": NRG% = 1
    While (INP2$ <> "X" )
      BAN.PRG$ = "0"
      For CNTI% = 1 TO 17                                  !
          READ #19; LINE REG.HLP$
          IF BAN.PRG$ = "0" THEN                          \!
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          IF BAN.PRG$ = "1" THEN BEGIN                    \!
             CNTI% = 18					   !
             CLOSE 19					   !
             OPEN HLP.FILE$ AS 19 NODEL                    ! Apertura Archivo Help
          ENDIF
      Next CNTI%  
      RET.ERR% = NXTF(-20) 	                               ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			                     !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      If (ENDF = F1.AYUDA) THEN Call OSSHELL("PRINT "+UE.SALIDA$) 
      If (ENDF = F3.SALIR) THEN INP2$="X"
      CALL.ORDER% = 2                                      ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)			                   ! Despliege de la pantalla
    Wend 
    Close 19
    BAN.PRG$ = "0"
END FUNCTION
!--- Fin de la funcion de ayuda

Sub TIPO.PROMOCION(PROM$)
String PROM$

   BAN.PRG$ = "0"
   Open "C:\PROMODAT\PRMI"+PROM$+".PRM" As 1 Locked                ! Carga de promocion presencia de articulos 
   If BAN.PRG$ = "0" Then Begin 													         ! Apertura Exitosa
      Call PRESENCIA.ITEM
      Call CONSULTA.PANTALLA
      Exit Sub 
   EndIf 
   
   BAN.PRG$ = "0"
   Open "C:\PROMODAT\SECC"+PROM$+".PRM" As 1 Locked                ! Carga de promocion dscto x seccion
   If BAN.PRG$ = "0" Then Begin 													         ! Apertura Exitosa
      Call DSCTO.SECCION 
      Call CONSULTA.PANTALLA
      Exit Sub 
   EndIf 

   BAN.PRG$ = "0"
   Open "C:\PROMODAT\PRMM"+PROM$+".PRM" As 1 Locked                ! Carga de promocion mensajes tirilla
   If BAN.PRG$ = "0" Then Begin 													         ! Apertura Exitosa
      Call PRM.MENSAJE
      Call CONSULTA.PANTALLA
      Exit Sub 
   EndIf 

   BAN.PRG$ = "0"
   Open "C:\PROMODAT\RIFD"+PROM$+".PRM" As 1 Locked                ! Carga de promocion rifas sistema
   If BAN.PRG$ = "0" Then Begin 													         ! Apertura Exitosa
      Call PRM.RIFAS   
      Call CONSULTA.PANTALLA
      Exit Sub 
   EndIf 

   Call MSG.ERR(3,"PROMOCION "+PROM$+" NO FUE ENCONTRADA")
   WAIT ; 2000
End Sub 

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0002
Call INICIADM 				                   									! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 4                      ! Definicion area de trabajo archivo
ARCH3$="EAMITEMR"					   											        ! Maestro de productos
UE.SALIDA$ = "ADX_UDT1:RPDEFPRM.RPT"
Open ARCH3$ KEYED RECL 77 AS AREA3%							          ! Apertura Eamitemr
Open "MKTP:PRMADMON.DAT" KEYED RECL 9 AS 34  						  ! Apertura Control de promociones
TERM$ = " "					           											      ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   										    !
Call DM.ERR(RET.ERR%,INITDM$)				   									  !
RET.ERR% = OPNDIS("C:\ADX_UPGM\PORTAFOL.PNT")	           	! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
FINR$=CHR$(13)+CHR$(10)
Call INICIO1 
While (FIN$ = "0") 
      CREATE UE.SALIDA$ As 51
      UE.LINEAS% = 0
      CITM% = 0
      DM.TPRE$ = "1"					   										! C¢digo Nuevo
      MSG$="Ingrese Codigo de la promocion "
      Call MSG.ERR(03,MSG$)				             ! Mensaje cabecera pantalla
      RET.ERR% = NXTF(-20) 			           									! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          						! Captura dato en pantalla
      DM.CODIGO$ = INP$                                    						! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("CONSULTA CODIGOS PROMOC. ","GRCODPRM.TXT") !
         Call INICIO1
      EndIf
      If (ENDF = F3.SALIR) THEN Call QUIT2                 ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call QUIT2                 ! Si presiona tecla ESC
      While DM.CODIGO$ = " " OR VAL(DM.CODIGO$) = 0
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.CODIGO$ = INP$                                  ! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("CONSULTA CODIGOS PROMOC. ","GRCODPRM.TXT") !
         Call INICIO1
        EndIf
        If (ENDF = F3.SALIR) THEN Call QUIT2               ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call QUIT2               ! Si presiona tecla ESC
      Wend 
      Call TIPO.PROMOCION(DM.CODIGO$)
      Call INICIO1
Wend 

!----
!---- Fin del bloque principal
!----

E0002:
         If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") THEN BEGIN
            BAN.PRG$ = "1"
            RESUME
         EndIf

         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         If ERRF% = AREA3% AND (ERR = "OE" OR ERR = "FU") THEN BEGIN
            BAN.PRG$ = "1"
            RESUME
         EndIf

         If ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf

         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         STOP
!--- Fin despliegue de errores

!--- Fin del programa PORP0002.BAS
