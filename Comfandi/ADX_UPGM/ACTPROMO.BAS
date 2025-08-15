!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : ADMPROMO.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Administracion de Promociones    *
!**************************************************
! Version 1.0 12/Abr/2018
!--------------------------------------------------
! Mod 29/Jul/2019
! Se ajusta el calculo de tiempo de ejecucion de la 
! tarea de validacion quedando de la siguiente manera:
! Background cada hora en punto
! interactivo cada 15 segundos para cargas urgentes.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 08/Ene/2019
! Se ajusta problema con proceso de apertura de 
! archivos y que genera finalización de la tarea
! de background y la no carga de promociones.
! desarrollado por Grupo Retail - OVS
!--------------------------------------------------
! Mod 30Ene2010
! Se adiciona la funcion Analiza.Fec.Bonos pasando
! como parametro el nombre del archivo de bonos 
! recompra para validar la fecha de carga de bonos 
! para su activación.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------


%ENVIRON C

String    GLOBAL MSG$, ERRFX$, PROFREE(1), TMP$, IFILE$
Integer   GLOBAL I%, TS.ERROR, CTRL%
Integer*2 GLOBAL  WI2%, RTA.PROMO%

String GLOBAL AA$, BB$, CC$, DD$, LLAVE$

%INCLUDE ADX_UPGM:BASROUT.J86

Sub Adxserve(RET,FUNC,PARM1,PARM2) EXTERNAL
Integer*4 RET
Integer*2 FUNC,PARM1
String PARM2
End Sub

Function ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE$) EXTERNAL
Integer*2 	TERM,MSGNUM
Integer*1	SEVERITY,MSGGRP,EVENT
String		UNIQUE$
End Function

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   Integer*4 RETC
   String INFILE, OUTFILE
   Integer*2 OPT0, OPT1
End Sub

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
 If IFILE$ = "BACKGRND" Then Call ADXSERVE(0,26,1,XMSG$) \
  Else Print XMSG$
 
End Sub 


Sub CALCULO.HORA
String Xh$, Xa$, X.Men$
Integer*4 Xa%, Xb%
Xh$ = Left$(TIME$,2)             ! Toma hora y minutos
Xa% = Val(Xh$)                   ! Toma Valor Numerico
Xa% = Xa% + 1                    ! Calcula Proxima Hora de ejecucion
If Xa% = 24 Then Xa% = 0

Xa$ = (Right$("00"+Str$(Xa%),2)+"00") ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)
Xh$ = Mid$(TIME$,3,2)            ! Toma Minutos
Xa% = val(Xh$)                   ! Convierte valor numerico
Xa% = 60 - Xa%                   ! Calcula resto de tiempo para proxima hora
Xb% = (Xa% * 60) * 1000          ! Calcula tiempo en tiempos de Relog 

!XB% = 10000
!XB% = 60000
!X.Men$ = "Promociones V2 Gretail V1.8 "+Left$(time$,2) + ":"+Mid$(time$,3,2)

X.Men$ = "Promociones V2 Gretail V1.9 "+Xa$									! Valida cada hora
Call Mensaje(X.Men$)

If IFILE$ = "BACKGRND" Then Begin                                            \! En Background
   Wait ; Xb%																																	! Calculo proxima hora
EndIf Else Begin																															! Interactivo
   wait ; 15000																																! cada 30 seg
EndIf

End Sub
!--- Fin calculo hora

Function Analiza.Fec.Bonos(Xfile$) 																					! Analisis fecha de carga de bonos 
Integer*1 Analiza.Fec.Bonos																									! 
String    Xfile$, Xreg$, XFini$																							! 
   TS.ERROR = -1																														! Control de errores
   Open "C:\PROMODAT\"+Xfile$ As 1 Locked        														! Archivo de bonos para analisis
   If TS.ERROR <> -1 Then Begin																							! Si falla apertura archivo
   	  Analiza.Fec.Bonos = 0 																								! Falla en el proceso
   	  Exit Function 																												! Sale del procedimiento
   EndIf 																																		! 
   Read #1; XReg$																														! Lectura parametros bonos 
   XFini$ = Mid$(Xreg$, 1,6)       																					! Fecha inicial del evento
   If XFini$ <= date$ Then Analiza.Fec.Bonos = -1 Else  									 \! Proceso OK para carga
   	  Analiza.Fec.Bonos = 0																									! Proceso No OK para carga
   Close 1 																																	! Cierra archivo
End Function 


Sub Marcar.Promocion(Xpromo$)
String Xpromo$, X1$, X2$, X3$, X4$, Xkey$
Xkey$ = Xpromo$

Read Form "C12 C1";#34 KEY Xkey$;                      \! Lee Reg Cabecera 
   X1$, X4$

Write Form "C12 C1";#34; X1$, "X" 

End Sub 

Function Activar.Promocion(Prom$, Stat$)
Integer*1 Activar.Promocion, GrAct%
String Prom$, Stat$

!MSG$ = "Verificando: "+Prom$
!Call Mensaje(MSG$) 

Activar.Promocion = 0 

If Ucase$(Stat$) = "D" Then GoTo Anular.Promo

If Ucase$(Stat$) = "C" Then Begin 																					! Carga de listas
 If Left$(PROM$,1) = "P" Then Begin																					! Archivo Promociones
   TS.ERROR = -1 																														! Control errores
   Open "C:\PROMODAT\"+PROM$ As 1 Locked         														! Archivo de listas
   If TS.ERROR = -1 Then Begin 														  								! Apertura Exitosa
      Close 1 																															! Cierra Archivo
      MSG$ = "Cargando: "+PROM$																							! Archivo de carga
      Call Mensaje(MSG$)																										!
      Call Osshell("ADX_UPGM:LDITMLST.286 "+PROM$) 													! Carga interface
      Call Marcar.Promocion(Prom$)																					! Marcar promocion cargada
      Activar.Promocion = -1 
   EndIf Else Activar.Promocion = 0																					! Falla en apertura
  EndIf 
 If Left$(PROM$,1) = "M" Then Begin																					! Archivo Mensajes
   TS.ERROR = -1 																														! Control errores
   Open "C:\PROMODAT\"+PROM$ As 1 Locked         														! Archivo de listas
   If TS.ERROR = -1 Then Begin 														  								! Apertura Exitosa
      Close 1 																															! Cierra Archivo
      MSG$ = "Cargando: "+PROM$																							! Archivo de carga
      Call Mensaje(MSG$)																										!
      Call Osshell("ADX_UPGM:LDMSGPRM.286 "+PROM$) 													! Carga interface
      Call Marcar.Promocion(Prom$)																					! Marcar promocion cargada
      Activar.Promocion = -1 
   EndIf Else Activar.Promocion = 0																					! Falla en apertura
  EndIf 

!--- Procedimiento cargas bonos recompra
 If Ucase$(Left$(PROM$,4)) = "LDIT" Then Begin															! Archivo Bonos recompra
   TS.ERROR = -1 																														! Control errores
   Open "C:\PROMODAT\"+PROM$ As 1 Locked         														! Archivo bonos recompra
   If TS.ERROR = -1 Then Begin 														  								! Apertura Exitosa
      Close 1 																															! Cierra Archivo
      GrAct% = Analiza.Fec.Bonos(PROM$)                                     ! Analiza Fecha activación bonos
      If GrAct% = -1 Then Begin																							! Promocion autorizada para carga
         MSG$ = "Cargando: "+PROM$											  									! Archivo de carga
         Call Mensaje(MSG$)																									!
         Call Osshell("ADX_UPGM:LDITBONO.286 "+PROM$) 											! Carga interface bono recompra
         Call Marcar.Promocion(Prom$)																				! Marcar promocion cargada
         Activar.Promocion = -1 																						! 
      EndIf Else Activar.Promocion = 0																			! Reporta promocion pendiente
   EndIf Else Activar.Promocion = 0																					! Falla en apertura
  EndIf 																																		!

 If Ucase$(Left$(PROM$,4)) = "LMPT" Then Begin															! Archivo Bonos recompra forma pago
   TS.ERROR = -1 																														! Control errores
   Open "C:\PROMODAT\"+PROM$ As 1 Locked         														! Archivo bonos recompra forma pago
   If TS.ERROR = -1 Then Begin 														  								! Apertura Exitosa
      Close 1 																															! Cierra Archivo
      GrAct% = Analiza.Fec.Bonos(PROM$)                                     ! Analiza Fecha activación bonos
      If GrAct% = -1 Then Begin																							! Promocion autorizada para carga
         MSG$ = "Cargando: "+PROM$																					! Archivo de carga
         Call Mensaje(MSG$)																									!
         Call Osshell("ADX_UPGM:LDITBNPG.286 "+PROM$) 										  ! Carga interface bono recompra
         Call Marcar.Promocion(Prom$)																				! Marcar promocion cargada
         Activar.Promocion = -1 																						! Reporta promocion cargada
      EndIf Else Activar.Promocion = 0																			! Reporta promocion pendiente
   EndIf Else Activar.Promocion = 0																					! Falla en apertura
  EndIf 																																		!


EndIf   

Anular.Promo:
   Activar.Promocion = 0 
   RTA.PROMO% = 0
   
End Function 
!Fin ejecucion de interfaces


Sub Verificar.Cargas
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Aplicada%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, KEY$
Integer*2 Key.Len, Rec.Len

!Open "TF:LSTCAR" KEYED RECL  13  AS 34   						 				 ! Apertura lista items promociones
!Open "TF:LSTCAR" DIRECT RECL 512 AS 33   						         ! Apertura lista items promociones

TS.ERROR = -1
Open "TF:LSTCAR" KEYED RECL  13  AS 34   						 				 ! Apertura lista items promociones
If TS.ERROR = 0 Then Exit Sub 
Wait ; 500                                                   ! Tiempo espera para control aperturas
Open "TF:LSTCAR" DIRECT RECL 512 AS 33   						         ! Apertura lista items promociones
If TS.ERROR = 0 Then \
	 Close 34 : Exit Sub 


REC.NO = 1					             														 ! Initialize Counter
I% = 1						               														 ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		                       ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
TS.ERROR = -1                                                ! Control de errores
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$= MID$(H$,X+0,12)                            				 ! Archivo de carga
    C04$= Mid$(H$,X+12,1)                                    ! Estado del archivo
!    MSG$ = "Verificando: "+C01$+" - "+C04$
!    Call Mensaje(MSG$)
    
    If Ucase$(C04$) = "C" Then Begin                         ! Lista Sin cargar 
       MSG$ = "Verificando: "+C01$+" - "+C04$
       Call Mensaje(MSG$)
       Aplicada% = Activar.Promocion(C01$,C04$)      				 ! Activa la promocion
    EndIf 

     X = X + REC.LEN                                          ! Index to next key
     KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
     If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33 
Close 34
End Sub 

!---
! Bloque Principal
!---

On Error GoTo ERROR.PRG																											! Control errores
IFILE$ = Command$                						 																! Como esta ejecutando
If Ucase$(IFILE$) = "VERSION" Then Begin 																		!
	 !Print "ADMINISTRACION MOTOR DE PROMOCIONES Ver 1.7 31/Ene/2019 03:25 pm"	!
	 Print "ADMINISTRACION MOTOR DE PROMOCIONES Ver 1.9 29/Jun/2019 03:49 pm"	!
	 Stop																																			!
EndIf 																																			!
While (1)                                    																! Ciclo infinito
 Call Calculo.Hora               						 																! Cada Hora Verifica si encuentra archivos para cargar
 Call Verificar.Cargas          				     																! Carga Archivos 
 TMP$ = "Preparando proxima validacion..." 																	!
 Call Mensaje(TMP$)
 wait ; 1200
Wend																																				!
Stop																																				!

ERROR.PRG:
     Call TRADUCE.ERROR
     If ERR = "IH" Then Resume 
     If ERR = "CU" Then Resume 
     If ERR = "*I" Then Resume 
!     If ERRF% = 1 And (ERR = "OE" OR ERR = "FU") Then Begin									! ERROR DE APERTURA
!		    TS.ERROR = 0
!			  Resume
!		 EndIf

     If (ERR = "OE" OR ERR = "FU") Then Begin									! ERROR DE APERTURA
		    TS.ERROR = 0
			  Resume
		 EndIf


     If (ERR = "EF") Then Begin																							! Si eof
		    TS.ERROR = 0
			  RESUME
		 EndIf

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)
		 Stop
