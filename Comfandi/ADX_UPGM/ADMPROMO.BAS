!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : ADMPROMO.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Administracion de Promociones    *
!**************************************************
! Si el indicador del registro de carga tiene los 
! valores 1,2,0 se comporta asi:
! 0 -> Copia, aplica pero no borra archivo de carga
! 1 -> Idem al anterior
! 2 -> Copia, aplica y borra archivo de carga
!      Si en nombre del archivo de copia es NONAME
!      solamente borra
!--------------------------------------------------
! Mod 16Ago2013
! Se controla el evento de error I* que genera fin
! de proceso de la tarea de background.
! Desarrollo Grupo Retail - OVS
!-------------------------------------------------

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
Xh$ = Left$(TIME$,4)             ! Toma hora y minutos
Xa% = Val(Xh$)                   ! Toma Valor Numerico
Xa% = Round(Xa%,-2,1)            ! Calcula Proxima Hora de ejecucion
Xa$ = Right$("0000"+Str$(Xa%),4) ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)
Xh$ = Mid$(TIME$,3,2)            ! Toma Minutos
Xa% = val(Xh$)                   ! Convierte valor numerico
Xa% = 60 - Xa%                   ! Calcula resto de tiempo para proxima hora
Xb% = (Xa% * 60) * 1000          ! Calcula tiempo en tiempos de Relog 
!XB% = 10000
XB% = 60000
X.Men$ = "Promo Online (Grupo Retail) V 3.4"
Call Mensaje(X.Men$)
Wait ; Xb%

End Sub
!--- Fin calculo hora

Sub Marcar.Promocion(Xpromo$)
String Xpromo$, X1$, X2$, X3$, X4$, Xkey$
Xkey$ = Pack$(Right$("0000"+Xpromo$,4))

Read Form "C2 2C3 C1";#34 KEY Xkey$;                      \! Lee Reg Cabecera 
   X1$, X2$, X3$, X4$

Write Form "C2 2C3 C1";#34; X1$, X2$, X3$, "1" 

End Sub 


Function Activar.Promocion(Prom$, Fini$, Ffin$,Stat$)
Integer*1 Activar.Promocion
String Prom$, Fini$, Ffin$, Stat$
MSG$ = "Verificando Promocion "+Prom$
Call Mensaje(MSG$) 
Activar.Promocion = 0 
If Val(Stat$) = 1 Then GoTo Anular.Promo

If (Val(Fini$) <= Val(DATE$)) Then Begin 										! Si inicia promocion
   TS.ERROR = -1 
   Open "C:\PROMODAT\PRML"+PROM$+".PRM" As 1 Locked         ! Asignacion de cupones y ligas a productos
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Cargando PRML"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\LIGAR.286 "+"C:\PROMODAT\PRML"+PROM$+".PRM") ! Carga interface
      Call Marcar.Promocion(Prom$)
   EndIf 
   TS.ERROR = -1 
   
   Open "C:\PROMODAT\PRMK"+PROM$+".PRM" As 1 Locked                ! Creacion de cupones Fake para promociones
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Cargando PRMK"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\LIGAR3.286 "+"C:\PROMODAT\PRMK"+PROM$+".PRM") ! Carga interface
      Activar.Promocion = -1 
   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMI"+PROM$+".PRM" As 1 Locked                ! Carga de promocion presencia de articulos 
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Cargando PRMI"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\MRKTPRIM.286 "+"C:\PROMODAT\PRMI"+PROM$+".PRM") ! Carga interface
      Call Marcar.Promocion(Prom$)
   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMM"+PROM$+".PRM" As 1 Locked                ! PROMOCIONES VARIADAS MERCADEO
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Cargando PRMM"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\MRKTPROM.286 "+"C:\PROMODAT\PRMM"+PROM$+".PRM") ! Carga interface
      Call Marcar.Promocion(Prom$)
   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMC"+PROM$+".PRM" As 1 Locked                ! PROMOCIONES A CLIENTES
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Cargando PRMC"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\MRKTPROM.286 "+"C:\PROMODAT\PRMC"+PROM$+".PRM") ! Carga interface
      Call Marcar.Promocion(Prom$)
   EndIf 



   TS.ERROR = -1 
   Open "C:\PROMODAT\SECC"+PROM$+".PRM" As 1 Locked                ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Cargando SECC"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\CARGASEC.286 "+"C:\PROMODAT\SECC"+PROM$+".PRM") ! Carga interface       
      Call Marcar.Promocion(Prom$)
   EndIf 
!--- Adicion para manejo de rifas 12 Nov 2008

   TS.ERROR = -1 
   Open "C:\PROMODAT\RIFP"+PROM$+".PRM" As 1 Locked                ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Cargando RIFP"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RIFASPRD.286 "+"C:\PROMODAT\RIFP"+PROM$+".PRM") ! Carga interface       
      Call Marcar.Promocion(Prom$)
   EndIf Else GoTo Next.Carga

   TS.ERROR = -1 
   Open "C:\PROMODAT\RIFD"+PROM$+".PRM" As 1 Locked                ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Cargando RIFD"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RIFASDEF.286 "+"C:\PROMODAT\RIFD"+PROM$+".PRM") ! Carga interface       
   EndIf 

Next.Carga:
!--- Carga promocion para formas de pago Add 7 jul 2010

   TS.ERROR = -1 
   Open "C:\PROMODAT\PFPG"+PROM$+".PRM" As 1 Locked         ! Valida Descuento en formas de pago
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Cargando PFPG"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\PROMOFPG.286 "+"C:\PROMODAT\PFPG"+PROM$+".PRM") ! Carga interface       
      Call Marcar.Promocion(Prom$)
   EndIf 


!   TS.ERROR = -1 
!   Open "C:\PROMODAT\MSGD"+PROM$+".PRM" As 1 Locked                ! Valida Descuento por Seccion
!   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
!      Close 1 																							! Cierra Archivo
!      MSG$ = "Cargando MSGD"+PROM$+".PRM"
!      Call Mensaje(MSG$)
!      Call Osshell("C:\GRMODUL\MSGCARGA.286 "+"C:\PROMODAT\MSGD"+PROM$+".PRM") ! Carga interface       
!   EndIf 

!---- Fin adicion de rifas

   Exit Function  
   !Activar.Promocion = -1 
EndIf 

Anular.Promo:
   Activar.Promocion = 0 
   RTA.PROMO% = 0
   
If (Val(Ffin$) <= Val(DATE$)) Then Begin                    ! Si termino la promocion
   TS.ERROR = -1 


!   Open "C:\PROMODAT\PRMC"+PROM$+".PRM" As 1                ! Asignacion de cupones y ligas a productos
!   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
!      Close 1 																							! Cierra Archivo
!      MSG$ = "Retirando PRMC"+PROM$+".PRM"
!      Call Mensaje(MSG$)
!      Call Osshell("C:\GRMODUL\RLIGAR3.286 "+"C:\PROMODAT\PRMC"+PROM$+".PRM") ! Carga interface       
!      RTA.PROMO% = -1
!   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\PRML"+PROM$+".PRM" As 1                       ! Creacion de cupones Fake para promociones
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Retirando PRML"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RLIGAR.286 "+"C:\PROMODAT\PRML"+PROM$+".PRM") ! Carga interface
      RTA.PROMO% = -1
   EndIf 
   TS.ERROR = -1 
   Open "C:\PROMODAT\SECC"+PROM$+".PRM" As 1                       ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Retirando SECC"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RCARGSEC.286 "+"C:\PROMODAT\SECC"+PROM$+".PRM") ! Carga interface       
      RTA.PROMO% = -1
   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMI"+PROM$+".PRM" As 1                       ! Carga de promocion presencia de articulos 
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Retirando PRMI"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RMRKTPRI.286 "+"C:\PROMODAT\PRMI"+PROM$+".PRM") ! Carga interface
      RTA.PROMO% = -1
   EndIf    
   
   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMM"+PROM$+".PRM" As 1                       ! Carga de promocion presencia de articulos 
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Retirando PRMM"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RMRKTPRO.286 "+"C:\PROMODAT\PRMM"+PROM$+".PRM") ! Carga interface
      RTA.PROMO% = -1
   EndIf    


   TS.ERROR = -1 
   Open "C:\PROMODAT\PRMC"+PROM$+".PRM" As 1                       ! Carga de promocion presencia de articulos 
   If TS.ERROR = -1 Then Begin 														         ! Apertura Exitosa
      Close 1 																							       ! Cierra Archivo
      MSG$ = "Retirando PRMC"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RMRKTPRO.286 "+"C:\PROMODAT\PRMC"+PROM$+".PRM") ! Carga interface
      RTA.PROMO% = -1
   EndIf    
  
   
!--- Adicion para manejo de rifas 12 Nov 2008

   TS.ERROR = -1 
   Open "C:\PROMODAT\RIFD"+PROM$+".PRM" As 1                ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Retirando RIFD"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\ANRIFDEF.286 "+"C:\PROMODAT\RIFD"+PROM$+".PRM") ! Carga interface       
      RTA.PROMO% = -1
   EndIf 

   TS.ERROR = -1 
   Open "C:\PROMODAT\RIFP"+PROM$+".PRM" As 1                       ! Valida Descuento por Seccion
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Retirando RIFP"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\ANRIFPRD.286 "+"C:\PROMODAT\RIFP"+PROM$+".PRM") ! Carga interface       
      RTA.PROMO% = -1
   EndIf 

!--- RETIRO PROMOCION FORMAS DE PAGO
   TS.ERROR = -1 
   Open "C:\PROMODAT\PFPG"+PROM$+".PRM" As 1                ! Valida Descuento en formas de pago
   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
      Close 1 																							! Cierra Archivo
      MSG$ = "Retirando PFPG"+PROM$+".PRM"
      Call Mensaje(MSG$)
      Call Osshell("C:\GRMODUL\RPROMOFP.286 "+"C:\PROMODAT\PFPG"+PROM$+".PRM") ! Carga interface       
      RTA.PROMO% = -1
   EndIf 



!   TS.ERROR = -1 
!   Open "C:\PROMODAT\MSGD"+PROM$+".PRM" As 1 Locked                ! Valida Descuento por Seccion
!   If TS.ERROR = -1 Then Begin 														  ! Apertura Exitosa
!      Close 1 																							! Cierra Archivo
!      MSG$ = "Cargando MSGD"+PROM$+".PRM"
!      Call Mensaje(MSG$)
!      Call Osshell("C:\GRMODUL\ANMSGPUB.286 "+"C:\PROMODAT\MSGD"+PROM$+".PRM") ! Carga interface       
!   EndIf 

!---- Fin adicion de rifas
   

   If RTA.PROMO% = -1 Then BEGIN 
      PROM$ = Pack$(Right$("0000"+PROM$,4))																			 ! Ajusta dato
      DelRec 34 ; PROM$																													 ! Borra Promocion
   ENDIF 
EndIf 

End Function 
!Fin ejecucion de interfaces


Sub Verificar.Cargas
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Aplicada%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, KEY$
Integer*2 Key.Len, Rec.Len

Open "MKTP:PRMADMON.DAT" KEYED RECL  9   AS 34   						 ! Apertura Control de promociones
Open "MKTP:PRMADMON.DAT" DIRECT RECL 512 AS 33   						 ! Apertura control de promociones
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
    C01$= Unpack$(MID$(H$,X+0,2))                            ! Codigo de la promocion
    C02$= Unpack$(MID$(H$,X+2,3))                            ! Fecha de Inicio de la promocion
    C03$= Unpack$(MID$(H$,X+5,3))                            ! Fecha Final de la promocion
    C04$= Mid$(H$,X+8,1)                                     ! Estado de la promocion
    MSG$ = "Verificando Promocion "+C01$
    Call Mensaje(MSG$)
    Wait ; 500
    
    If Val(C04$) <> 2 Then Begin                             ! Si promocion Sin activar
     Aplicada% = Activar.Promocion(C01$,C02$,C03$,C04$)      ! Activa la promocion

!     C01$ = PACK$(C01$)
!     C02$ = PACK$(C02$)
!     C03$ = PACK$(C03$)
!     If Aplicada% = -1 Then \
!        Write Form "C2 2C3 C1";#34; C01$, C02$, C03$, "1" 
     EndIf 

     X = X + REC.LEN                                          ! Index to next key
     KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
     If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33 
Close 34
End Sub 

Sub Ajuste.Lista.Promo 
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Aplicada%, Year%, Year2%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, KEY$
Integer*2 Key.Len, Rec.Len, Xrta%

Open "MKTP:PRMADMON.DAT" KEYED RECL  9   AS 34   						 ! Apertura Control de promociones
Open "MKTP:PRMADMON.DAT" DIRECT RECL 512 AS 33   						 ! Apertura control de promociones
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
    C01$= Unpack$(MID$(H$,X+0,2))                            ! Codigo de la promocion
    C02$= Unpack$(MID$(H$,X+2,3))                            ! Fecha de Inicio de la promocion
    C03$= Unpack$(MID$(H$,X+5,3))                            ! Fecha Final de la promocion
    C04$= Mid$(H$,X+8,1)                                     ! Estado de la promocion
    
    !Year2% = Val(Left$(date$,2))                             ! 
    !Year% = Val(Left$(C03$,2))                               ! 

    Year2% = Val(Left$(date$,2))                             ! 
    Year%  = Val(Left$(C03$,2))                              ! 

    If (VAL(C03$) < VAL(DATE$)) OR (Year% < Year2%) Then Begin ! Mas de un año finalizada
      C03$ = DATE$
    	Xrta% = Activar.Promocion(C01$, C02$, C03$,"1")
      C01$ = Pack$(Right$("0000"+C01$,4))	  								 ! Ajusta dato
      DelRec 34 ; C01$														  				 ! Borra Promocion    	 
      MSG$ = "Elimino lista Promocion "+unpack$(C01$)
      Call Mensaje(MSG$)
      Wait ; 500
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

On Error GoTo ERROR.PRG
IFILE$ = Command$                						 ! Como esta ejecutando
IF IFILE$ = "VERSION" THEN BEGIN 
	 !PRINT "ADMINISTRACION MOTOR DE PROMOCIONES Ver 3.2 27/Jun/2013 11:52 am"
	 PRINT "ADMINISTRACION MOTOR DE PROMOCIONES Ver 3.3 16/Ago/2013 11:40 am"
	 Stop
ENDIF
Open "MKTP:PRMADMON.DAT" KEYED RECL  9 AS 33 ! Apertura Control de promociones
Close 33																		 ! Cierre Archivo
While (1)                                    ! Ciclo infinito
 Call Calculo.Hora               						 ! Cada Hora Verifica si encuentra archivos para cargar
 Call Ajuste.Lista.Promo                     ! Retira promociones ya finalizadas de la lista
 Call Verificar.Cargas          				     ! Carga Archivos 
Wend
Stop

ERROR.PRG:
     Call TRADUCE.ERROR
     If ERR = "IH" Then Resume 
     If ERR = "*I" Then Resume 
     If ERRF% = 1 And (ERR = "OE" OR ERR = "FU") Then Begin					! ERROR DE APERTURA
		    TS.ERROR = 0
			  Resume
		 EndIf
     If (ERR = "EF") Then Begin												! Si eof
		    TS.ERROR = 0
			  RESUME
		 EndIf
		 If ERRF% = 33 And (ERR = "OE" OR ERR = "FU") Then Begin				! ERROR DE APERTURA
        Create PosFile "MKTP:PRMADMON.DAT" KEYED 2,,,25000 RECL 9  \! si no existe lo crea.
            AS 33 compound perupdate 		     												! 
			  Resume
		 EndIf

     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
     Call Mensaje(MSG$)


		 Stop

