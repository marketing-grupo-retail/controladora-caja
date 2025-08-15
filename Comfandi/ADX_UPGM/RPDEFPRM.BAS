!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : RPDEFPRM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Observaciones : Reporte Definicion Promociones V2*
!**************************************************
! - Version 1.0 15 May 2018
!

%ENVIRON C						   																										! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$,        \! Definicion de variables
                 Ctrl.Trx$, Ue.Data3$, Xsgn$, NRO.REG$, FECMOV$,           \! de aplicacion
                 HORA.FINAL$, UE.FECMOV$, DM.OPERADOR$, TS.TEMP3$						!
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$,\!
                 DATO.SO$, NAME$, TIPO.TRANS$, FECHA.ARCH$									!
String    Global XCOD.ITEM$, XDEPTO$, XCANTIDAD$, XPRECIO$, XPESO$, PESO$, \!
                 Gondola$, AreaInv$, Xbuff$, IR.USERDATA$										!
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%,     \!
                 Cnt.Reg% , CITM%																						!
Integer*4 Global UE.LINEAS%, U.STR%(1), TOT.MOV%, Precio%, Indi2%, Compra%,\!
                 Tmov%																											!
Integer*1 Global ARC%, trx.inven%, Terror%																	!

                                                                                                                 
%Include POSPVARI.BAS				  	                                            ! Rutinas Comunes                    
%Include EAMITEMR.J86				  	                                            ! EAMITEMR                           
%Include DMEXTR.J86    		                                                  ! Inclucion Libreria Display Manager 
%Include POSPRUTI.BAS				  	                                            ! Rutinas Comunes                    
%Include BASROUT.J86																												!

!--- Definicion de rutinas de la aplicacion

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  								! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Function ERRNSTR$(ERRNUM)																										! Rutina traduccion Error 
Integer*1 I
Integer*4 ERRNUM,WORK
String HEX$,ERRNSTR$,WORK$
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
    For I = 1 TO 8
      WORK   = ERRNUM AND 0000000FH                                         ! AND OFF ALL BUT LOW NYBBLE       
      WORK$  = MID$(HEX$,WORK+1,1)+WORK$                                    ! ADD HEX VALUE TO OUTPUT String   
      ERRNUM = SHIfT(ERRNUM,4)                                              ! SET UP NEXT NYBBLE               
    NEXT I                                                                                                     
    ERRNSTR$=WORK$                                                          ! Return Error Code                
End Function 


Sub SALIDA.PROG
  Call SETF("0000000")				   								 														!
  Call CLRSCR					   												 														!
  RET.ERR%= CLSDIS				   										 														!
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 														!
  Stop																																			!
End Sub 																																		!

Function INICIO1																														!
  Call.ORDER% = 20                                													! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   													! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)																							!
End Function																																! 
!--- Fin de la funcion de inicio

Function ENTRADA.LOG																												!
  Call MSG.ERR(5,MEN$)                                 											! Mensaje
End Function 																																!

Function CONSULTA.PANTALLA                          												! Parametro Programa y archivo
  STRING HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$														!
  INTEGER*2 CNTI%, NRG%																											!
      CALL.ORDER% = 2                                      									! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        									! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			                   									! Despliege de la pantalla
      Terror% = -1																													!
      HLP.FILE$ = UE.SALIDA$																								!
      OPEN HLP.FILE$ AS 19 NODEL                           									! Apertura Archivo Help
      IF Terror% = 0 THEN BEGIN                           									!
         MSG1$ = "Archivo Reporte "+HLP.FILE$+" No Existe o Sin Informacion"!
         CALL MSG.ERR(19,MSG1$): WAIT;2500: EXIT Function										!
      ENDIF																																	!
    INP2$ = " ": NRG% = 1																										!
    WHILE (INP2$ <> "X" )																										!
      Terror% = -1																													!
      FOR CNTI% = 1 TO 17                                  									!
          READ #19; LINE REG.HLP$																						!
          IF Terror% = -1 THEN                                             \!
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1                   !
          IF Terror% = 0 THEN BEGIN                                        \!
             CNTI% = 18					   																					!
             CLOSE 19					   																						!
             OPEN HLP.FILE$ AS 19 NODEL                    									! Apertura Archivo Help
          ENDIF																															!
      NEXT CNTI%  																													!
      RET.ERR% = NXTF(-20) 	                           											! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   																		!
      ATTR$ = SETF(PRM.ON$)                                									!		
      INP2$ = UPDF                                         									! Captura dato en pantalla
      IF (ENDF = F1.AYUDA) THEN Call OSSHELL("PRINT "+UE.SALIDA$) 
      If (ENDF = F3.SALIR) THEN INP2$="X"
      CALL.ORDER% = 2                                    										! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        									! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			   																	! Despliege de la pantalla
    WEND
    Close 19
    Terror% = -1
END FUNCTION
!--- Fin de la funcion de ayuda

Function TERMINE.PROG																												! Fin del programa
String X.TMP$																																!
  Call SALIDA.PROG  																												!
End Function																																!
!--- Fin de la ejecucion del programa

Function LEER.CABECERA																											! Tomar el numero de la tienda
 String PARM2$																															!
 INTEGER*2 PARM1,RET																												!
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 													! 
 DM.ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)									!
 FECMOV$ = DATE$																														!
End Function																																!
!--- Fin de la funcion de lectura

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      FINR$ = Chr$(13) + Chr$(10)
      Rbuffer$ = Ue.Data2$
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
End Sub 

Sub Cabecera.Reporte
  CITM% = CITM% + 1 
  UE.DATA2$ = "COMFANDI"+STRING$(08," ")+"  REPORTE DEFINICION DE PROMOCIONES  "+STRING$(09," ")+\
  "Pagina No. "+Right$("000"+STR$(CITM%),3)
  Call Write.Data
  UE.DATA2$ = "FECHA DEL REPORTE :20"+LEFT$(DATE$,2)+"/"+MID$(DATE$,3,2)+"/"+Right$(DATE$,2)
  Call Write.Data
  UE.DATA2$ = STRING$(77,"-")  
  Call Write.Data
  UE.DATA2$ = "PROMO      Fecha Inicial  Fecha Final    Estado"
  Call Write.Data
End Sub 
!---

Sub PROCESA.CONSULTA.PROMO																									! Consulta promociones
String Lista$, Fecha$, Estado$
Integer*4 CI%
Integer*4 Tpromo%, Xp1%, Xp2%
String    Promos$(2)

Terror% = -1
Create Ue.Salida$ As 51																											! Archivo de reporte
If Terror% = 0 Then Begin																										! Error en creacion
	 Exit Sub 
EndIf																																				! 
Call Cabecera.Reporte																												! Cabecera del reporte
Open "PROMODAT:DPROMOTI.DAT" As 26																					! Apertura definicion promociones
If End # 26 Then FIN.DEF.PROMO																							! Si Error de apertura
CI% = 0
Dim Promos$(5000,2)
tpromo% = 0
While (1)																																		! recorre archivo hasta el final
    Read #26; Line  Lista$
    Xp1% = Match("</cod>",Lista$,1)
    If Xp1% > 0 Then Begin																									! Hay promocion
    	 Tpromo% = Tpromo% + 1 																								! total promociones
    	 Promos$(Tpromo%,0) = Mid$(Lista$,11,(Xp1%-11))  											! Codigo de la promocion
       Xp1% = Match("<idt>",Lista$,1)																				!
       Promos$(Tpromo%,1) = Mid$(Lista$,Xp1%+5,10)    											! Fecha inicio promocion
       Xp1% = Match("<fdt>",Lista$,1)																				!
       Promos$(Tpromo%,2) = Mid$(Lista$,Xp1%+5,10)    											! Fecha inicio promocion
    EndIf																																		!
Wend 																																				! Fin recorrida archivo

FIN.DEF.PROMO:
    Xp1% = 0 
    For Xp1% = 1 To Tpromo%
     Fecha$ = Mid$(Promos$(Xp1%,1),3,2) + Mid$(Promos$(Xp1%,1),6,2) +      \!
              Mid$(Promos$(Xp1%,1),9,2)
     Estado$ = "PROMOCION INACTIVA"
     If Val(Date$) >= Val(Fecha$) Then Estado$ = "PROMOCION ACTIVA"         ! Fecha Inicio Promocion
     Fecha$ = Mid$(Promos$(Xp1%,2),3,2) + Mid$(Promos$(Xp1%,2),6,2) +      \!
              Mid$(Promos$(Xp1%,2),9,2)
     If Val(Date$) > Val(Fecha$) Then Estado$ = "PROMOCION INACTIVA"        ! Fecha Final Promocion
     	
     Ue.Data2$ = Right$("       "+Promos$(Xp1%,0),7) + "    " +            \!
                 Promos$(Xp1%,1) + "     " + Promos$(Xp1%,2)  +            \!
                 "     " + Estado$
     Call Write.Data
    Next Xp1%
    Ue.Data2$ = String$(78,"-")
    Call Write.Data
    Ue.Data2$ = "TOTAL PROMOCIONES CARGADAS :"+Str$(tpromo%)
    Call Write.Data
  Close 26
  Close 51
End Sub 																																		! Fin consulta promociones

Function PANTALLA.PRINCIPAL
Call INICIADM 				                    																	! Inicializacion Variables Display Manager
TERM$ = " "					            																						! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					    																				!
Call DM.ERR(RET.ERR%,INITDM$)				    																		!
RET.ERR% = OPNDIS("C:\ADX_UPGM\GRPROMV2.PNT")	            									! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)																								!
FIN$="0"																																		!
Call INICIO1 																																!
      MSG$="Seleccione Destino de la consulta "															!
      Call MSG.ERR(5,MSG$)                                 									! Mensaje
      RET.ERR% = NXTF(-20) 			           																	! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)																						! Ubica cursor
      ATTR$ = SETF(PRM.ON$)                                									! 
      INP$ = UPDF                                          									! Captura dato en pantalla
      DM.OPCION$ = INP$                                    									! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN 																			! Si presiona tecla F1
         Call HELP("Reporte Promociones Activ","rpdefprm.txt") 							!
         Call INICIO1																												!
      ENDIF																																	!
      If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                 						! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                 						! Si presiona tecla ESC
      WHILE (MATCH(DM.OPCION$,"12",1) = 0 OR DM.OPCION$ = " ")							!
        RET.ERR% = NXTF(-20)																								!
        Call DM.ERR(RET.ERR%,NXTF$)																					!
        ATTR$ = SETF(PRM.ON$)                                								!
        INP$ = UPDF                                         								! Captura dato en pantalla
        DM.OPCION$ = INP$                                   								! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN 																		! Si presiona tecla F1
           Call HELP("Reporte Promociones Activ","rpdefprm.txt") 						!	
           Call INICIO1																											!
        ENDIF																																!
        If (ENDF = F3.SALIR) THEN Call SALIDA.PROG                					! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call SALIDA.PROG                					! Si presiona tecla ESC
      Wend																																	!
      Ue.Salida$ = "ADX_UDT1:DP"+TIME$+".TXT"																! Archivo de salida consulta
      Call PROCESA.CONSULTA.PROMO																						! Consulta promociones
    	If DM.OPCION$ = "1" Then Call CONSULTA.PANTALLA												! Consulta a pantalla
      If DM.OPCION$ = "2" Then Call OSSHELL("PRINT "+UE.SALIDA$) 						! Consulta a impresora
End Function 
!--- Fin pantalla principal

!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  On Error GoTo ErrAppl																											! Control de errores
  Call LEER.CABECERA																												! Leer cabecerra del archivo
  Call PANTALLA.PRINCIPAL																										! Generacion del reporte
  Call TERMINE.PROG																													! Finalizacion programa
  Stop 																																			! 

ErrAppl:
  Terror% = 0
  ERRORCOD$ = ERR
  Call TRADUCE.ERROR
  If (ERR = "OE" Or ERR = "FU") Then Resume 
  If ERR = "EF" Then Resume 
  MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  Print MEN$
  MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
  Print MEN$
  Stop 
