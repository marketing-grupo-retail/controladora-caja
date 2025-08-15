!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PROMOLIN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Abril del 2004                   *
!Observaciones : PromoOnline                      *
!**************************************************
!
! Tipos de Promociones definidas en la aplicacion Web
! 1. Presencia de Item 
! 2. Compre A y reciba beneficio en B
! 3. Descuento por seccion
! 4. Compre A, Compre B y reciba beneficio en C
! 5. Mensaje por la presencia de A
! 6. Mensaje pro la presencia de un cliente
! 7. Mensaje por la No presencia de una seccion
! 8. Mensaje por la compra minima de una Seccion
! 9. Mensaje por Compra minima
!10. Mensaje por la no compra de A
!11. Mensaje por uso de un medio de pago
!12. Modulo de rifas
!13. Beneficio por uso de un medio de pago
!---------------------------------------------------
! Mod 20Nov2012
! Se ajusta generacion de carga de rifas, para optimizar
! generación de impresion.
! Desarrollado por Grupo Retail - OVS
!----------------------------------------------------
! Mod 28Jun2013
! Se desarrolla la funcion Valida.Rifas.Totales para el
! manejo de multiples rifas totales dentro de la aplicacion
! de promociones, permitiendo la parametrizacion de la aplicacion
! web rifas por compra totales en la tienda a diferentes
! tipificaciones de clientes, se utiliza el producto 999900
! el cual se crea en la aplicacion web como un grupo de 
! productos para dicho proceso.
! desarrollado por Grupo Retail - OVS
!----------------------------------------------------
! Mod 10/Jul/2013
! Se adiciona en la impresion de los tiquetes de mensajes
! una flag &R para imprimir datos de relleno de datos 
! de clientes, solicitado por Comfandi - Freddy Guerrero
! desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------
! Mod 3/Abr/2017
! Se controla que si en un proceso de recaudo de terceros
! no se genere vouchers del motor de promociones
! desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------

%ENVIRON T

String    GLOBAL ASC.MKP.SITIO$, XERRFX$, Ue.Tmp.Sgn$
Integer*4 GLOBAL PPP.PPP%, Gr.Escl.TotEscal%, Gr.Promo.Compra%, VTAB%(2), GrPab%, Gr.Mkp.Excento%
Integer*1 GLOBAL UE.TRX.ANUL%, GR.MKP.FILES%, UE.EPSS.ON, Gr.Promo.Dttrx%
String    GLOBAL GR.MKT.DSEC$, Gr.Mkp.Compra$, Gr.Mkp.LstExcen$
String    Global Gr.Lcl.Clte$, Gr.Lcl.TipClte$, Gr.Lcl.Dir$, Gr.Lcl.Tel$  ! Modulo Lealtad en linea
Integer*1 Global    GR.ESCL.ACT%   		! Modulo Dscto Escalonado Activo - Desactivo
Integer*4 Global    Gr.Escl.ItmVl% 		! Control Dscto Escalonado
Integer*4 Global    Gr.Mkp.DstAB%  		! Descto Compre A beneficio b
Integer*2 Global    UE.RECASRV.QTY%		! En un proceso de recaudo
Integer*1 Global    Gr.Mrm.Cap%       ! En proceso de remisiones

%INCLUDE EXIRTSVA.J86          ! Extension del EAMITEMR
%INCLUDE ASCITEMR.J86          ! Maestra de articulos
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables de transacciones
%INCLUDE EAMTERMS.J86          ! Variables de terminales
%INCLUDE EAMTOPTS.J86	         ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables EMS
%INCLUDE EAMITEMR.J86          ! Variables EAMITEMR
%INCLUDE GRITEMR.J86           ! Variables Itemr
%Include MKTTSUVA.011          ! Definicion de variables
%INCLUDE EAMTSXHC.J86          ! Rutinas de Assembler
%INCLUDE EAMASMRT.J86          ! Rutinas de Assembler

Function ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
 Integer*2 TERM,NUM
 Integer*1 SVRTY,GRP,EVENT
 String    UNQ$
End Function

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Sub TSHIECET EXTERNAL
End Sub

Function MATCHB(P1$,P2$,P3) EXTERNAL
  Integer*2 MATCHB
  String P1$
  String P2$
  Integer*2 P3
End Function

Function Asic.Getunpk(X.B$,X.J%) External
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk
End Function 

Function Grabacion.Cadena.Usuario2(X.Clave$,X.Datos$) External
String X.Clave$, X.Datos$
End Function 

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$															! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   																				! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function ASIC.DATOS$(UMSG1$,UMSG2$) External   															! Captura de datos
String UMSG1$, UMSG2$, ASIC.DATOS$     							      									! Definicion de variables
End Function
!--- Fin Asic.Datos

Function U.Imprime(UE.String$,UE.STATION) External													! Rutina de Impresion
String UE.String$      									                  									! Variables temporales
Integer*2 UE.STATION									                    									!
End Function
!--- Rutina de Impresion

Function U.CORTACR External								                									! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

Sub VISOR.AND.BORRAR(X.MSG$) External
  STRING X.MSG$
End Sub 

Sub ALIMENTA.LOGOS External 
End Sub 

Sub IMPRESION.LOGO(X.I%) External                                          ! Rutina impresion logos
Integer*1 X.I%                                                             ! Variable temporal
End Sub 

Sub TRADUCE.ERROR                                              !
String XZ$
Integer*4 XS%, XSUM%, XHX%, XSX%
    XHX% = ERRN                                                ! Traduccion de los
    XERRFX$=""                                                 ! codigos de error
    FOR XS% = 28 TO 0 STEP -4                                  ! del sistema operativo
        XSX% = SHIFT(XHX%,XS%)                                 !
        XSUM% = XSX% AND 000FH                                 !
        IF XSUM% > 9 THEN XSUM% = XSUM% + 55                  \!
        ELSE XSUM% = XSUM% + 48                                !
        XZ$ = CHR$(XSUM%)                                      !
        XERRFX$ = XERRFX$ + XZ$                                !
    NEXT XS%         
End Sub 

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub SPLIT.ITEMFILE (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 


!******************************************************************************
! Function Name: DIA.SEMANA
! Description: This function returns the day of the week.  It accepts a date and                
!              converts it to a numeric day of week.... 0=Sunday...6=Saturday
!   Algorithm:
!   This function uses Zeller's Congruence to calculate the day of the week
!   on which any date falls.
!   The following is an explanation of this algorithm
!
!   (1) Since February is the only month with a variable number of days, adjust
!       the month and year so that January and February are the last months of
!       the preceding year.
!
!   (2) Compute the contribution to the day of the week caused by the fact
!       that the months have different numbers of days.  Let m be the adjusted
!       month number, (from (1)), compute 26m - 2 and divide the result by 10,
!       discarding the remainder.
!
!   (3) Compute the contribution to the day of the week caused by the year.
!       Determine the century and the year within the century of the adjusted
!       year. Add the following: the year within the century, the whole number
!       part of the result of dividing the year within the century by 4, the
!       whole number part of the result of dividing the century by 4, and
!       5 times the century.
!
!   (4) Add the Day, the contribution from the month, and contribution from
!       the year.  MOD this sum by 7 and the result will be the day of the
!       week.
!
!   The day of the week is returned as 0 = sunday,1 = monday .... 6 = saturday.
!******************************************************************************
Function DIA.SEMANA(FN.DATE$) Public 

  STRING                               \
         FN.DATE$                      ! Date passed to function  CCYYMMDD

  INTEGER*2                            \
            YEAR,                      \
            MONTH,                     \
            DAY,                       \
            CENTURY,                   \
            ADJMONTH,                  \
            ADJYEAR,                   \
            MONTHCORRECTION,           \
            YEARCORRECTION,            \
            LSTTWO,                    \
            DIA.SEMANA                 \ Function return  0=Sunday...6=Saturday

  IF (LEN(FN.DATE$) <> 8) THEN BEGIN             ! if not valid length
    DIA.SEMANA = -1                              ! this will cause an error
    EXIT FUNCTION                                ! get out
  EndIf                                          ! if not valid length

  YEAR = INT%(VAL(LEFT$(FN.DATE$,4)))
  MONTH = INT%(VAL(MID$(FN.DATE$,5,2)))
  DAY = INT%(VAL(RIGHT$(FN.DATE$,2)))

  IF MONTH <= 2 THEN BEGIN
    ADJMONTH = 10 + MONTH
    ADJYEAR = YEAR - 1
  EndIf ELSE BEGIN
    ADJMONTH = MONTH - 2
    ADJYEAR = YEAR
  EndIf

  MONTHCORRECTION = (26 * ADJMONTH - 2) / 10

  CENTURY = ADJYEAR / 100
  LSTTWO = MOD(ADJYEAR,100)
  YEARCORRECTION = LSTTWO + (LSTTWO / 4) + (CENTURY / 4) + 5 * CENTURY

  DIA.SEMANA = MOD((DAY + MONTHCORRECTION + YEARCORRECTION),7)

End Function
!-- Fin rutina dia de la semana

Sub PRMFPAGO(XOPT%) External																							! Modulo recaudo Propinas
Integer*4 XOPT%         																									! Variables 
End Sub 

Sub DCTOXITM(Xopt%) External																							! Modulo Descuento por Happy Hour Item 
Integer*4 Xopt%, Gr.Tmp%																									! User Exit a Invocar
End Sub 

Function MKT.Detalle Public                                   						! Imprime Detalle De La Operacion
String MKT.Detalle, X.Oper$, X.A$
Integer*4 X.Trx%
X.Trx% = Sl.Hd.Transnum 
X.Oper$ = Unpack$(Ts.Oper$)
X.Oper$ = Str$(Val(X.Oper$))

MKT.Detalle =   Left$(Date$,2)+"/"+Mid$(Date$,3,2)+"/"+Right$(Date$,2)+ " "+\
                Left$(Time$,2)+":"+Mid$(Time$,3,2)+" "+\
                Right$("0000"+Ts.Store$,4)+" "+\
                Right$("   "+Ts.Terminal$,3)+" "+\
	              Right$("0000" +Str$(X.Trx%),4)+" "+\
                X.Oper$
End  Function

Function Grabacion.String.Usuario(X.clave$,X.datos$) External               ! Datos de usuarios
String X.clave$, X.datos$                                                   !
End Function                                                                !
!--- Fin Grabacion String Usuario                   

!Function Carga.Imagenes(FILE.IMG$,POS.M.IMG%) External		                  ! Carga imagenes 4610
!  String File.Img$, CARGA.IMAGENES                                          ! Definicion de variables 
!  Integer*2 POS.M.IMG%                                                      !
!End Function

!Function Alimenta.Imagenes External											                    !
!End Function                                                                !
!--- Fin carga logos Publicitarios


Function Mkt.String(X.A$)                       ! Coloca datos por parametro
String Mkt.String, X.A$, X.B$, X.C$, X.D$, XM$
Integer*4 X.A%, X.B%
X.A% = MATCH("&",X.A$,1)						            ! Busca dato a imprimir
If X.A% > 0 Then Begin                          ! Si lo encotro
   X.B% = LEN(X.A$)                             ! Arma mensaje con el dato 
   X.B$ = Left$(X.A$,X.A%-1)                    ! solicitado, entregando 
   X.C$ = Mid$(X.A$,X.A%,2)                     ! 38 caracteres
   X.D$ = Right$(X.A$,X.B% - (X.A%+1))
   If UCASE$(X.C$) = "&V" Then Begin            ! Valor de la recarga
      Call Format.Amount(Gr.Mkp.Vlr%)
      X.C$ = TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&N" Then Begin            ! Cadena y Almacen
      X.C$ = str$(Gr.Mkp.Bol%)
   EndIf
   If UCASE$(X.C$) = "&T" Then Begin            ! Valor de la recarga
      Call Format.Amount(Val(Gr.Mkp.Compra$))
      X.C$ = "$"+TS.TEMP1$
   EndIf
   If UCASE$(X.C$) = "&D" Then Begin            ! Direccion Cliente
   	If Gr.Lcl.Dir$ <> "                    " Then \
     X.C$ = Gr.Lcl.Dir$ Else \
     X.C$ = "____________________"	
   EndIf
   If UCASE$(X.C$) = "&P" Then Begin            ! Telefono Cliente
   	 If Gr.Lcl.Tel$ <> "          " Then \
        X.C$ = Gr.Lcl.Tel$ Else \
    	  X.C$ = "____________________"
   EndIf
   If UCASE$(X.C$) = "&R" Then Begin            ! Datos para Rifas
   	  Gr.Mkp.TirClte% = -1
   	  X.C$ = ""
   EndIf
   
   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
EndIf    
Mkt.String = X.A$

End Function
!--- Fin funcion data String

!-- Carga Modulo de Rifas
Sub Carga.Rifas Public 
Integer*4 XI%, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC, XJ%, R.S, RifVal%
String    XH$, PZERO$, KEY$, XNRifa$, Xkey$
Integer*1 RifErr%
 Call U.IMPRIME("CARGANDO MODULO RIFAS",2100H)
 Dim Gr.Mkp.Rifa$(300,2)			                                     ! Hasta 300 rifas simultaneas
 Gr.Mkp.CntRif% = 0                                                !
 TS.ER.RETURN = -1                                                 !
 Open "R::$F:LR004" KEYED RECL 301 As Gr.Mkp.SesTmp%  UNLOCKED NOWRITE NODEL     ! Lista de Rifas
 If TS.ER.RETURN <> -1 Then Begin                                  !
 	  Gr.Mkp.CntRif% = 1                                             !
 	  Gr.Mkp.Rifa$(1,1) = "0000"                                     !
 	  Exit Sub                                                       !
 EndIf                                                             !
 Key$ = Pack$("99")                                                !
 TS.ER.RETURN = -1                                                 !
 Read Form "C1 C300";#Gr.Mkp.SesTmp% KEY Key$;                    \!
    Xh$, Xnrifa$
 If TS.ER.RETURN <> -1 Then Begin                                  !
 	  Gr.Mkp.CntRif% = 0                                             !
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,1) = "9999"                        ! Numero Rifa
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,2) = "0"                           ! Control de venta
 EndIf Else Begin
 	  Gr.Mkp.CntRif% = 0                                             !
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,1) = "9999"                        ! Numero Rifa
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,2) = "0"                           ! Control de venta
    On Error GoTo Build.Error.Rifas
    For XI% = 1 To 300 Step 4                                      !
 	     Xkey$ = Mid$(Xnrifa$,XI%,4)						      							 ! Toma codigo de rifa
 	     If Match(" ",Xkey$,1) > 0  Then GoTo Nxt.Rifa               ! Con espacios en blanco
 	     If (XKey$ = "0000") Then GoTo Nxt.Rifa                      ! Rifa en ceros
 	     If (XKey$ = "    ") Then GoTo Nxt.Rifa                      ! Rifa en ceros
       RifErr% = -1                                                !
	     RifVal% = Val(Xkey$)                                        ! Valida si dato numerico
	     If RifErr% = -1 Then Begin
  	     Gr.Mkp.CntRif% = Gr.Mkp.CntRif% + 1                         ! Total ctrl rifas
  	     If Gr.Mkp.CntRif% > 300 Then GoTo FINAL.PROCESO.RIFAS       ! Sobrepasa total de rifas definidas
         Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,1) = Xkey$                      ! Numero Rifa
         Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,2) = "0"                        ! Control de venta
       EndIf
      Nxt.Rifa: 
    Next XI%
 EndIf
 FINAL.PROCESO.RIFAS: 
  Close Gr.Mkp.SesTmp%																						 !
  Call U.IMPRIME("TOTAL RIFAS CARGADAS "+STR$(Gr.Mkp.CntRif%),6100H)
  Exit Sub 
  Build.Error.Rifas:
   If RifErr% Then Begin
   	  RifErr% = 0
   	  Resume 
   EndIf
End Sub 

!--- Tipo promocion, Codigo Itm/Clte, Msg, Vlr Comparacion, Lista Cliente
!30 = Presencia de un producto 
!31 = Presencia de un cliente
!32 = No presencia de una sección 
!33 = Compra mínima 
!34 = Por no presencia de un PLU 
!35 = Presencia de un departamento
!36 = Presencia de una forma de pago
!37 = Promocion a Tipos de Cliente

Sub Carga.Parametros.Mrktplus Public								      		              ! Parametros modulo
String X.H$, X.AUX$, X.MSG$, X.PROMO$, X.DATA$, X.CUPON$, XCLTE$		        ! Variables temporales
Integer*2 X.CNTI%, X.CNTJ%	
 If ASC.MKP.OPEN% = 0 Then Begin																						! Si no abierto
    TS.ER.RETURN = -1																												!
    Open "R::$KPDEFPR" KEYED RECL 66 AS ASC.MKP.SE1%	 UNLOCKED NOWRITE NODEL    	        ! Open File Definicion presencias
    If TS.ER.RETURN <> -1 Then Begin																				! Si error Open
       Call U.Imprime("Error Apertura MKPDEFPR, Cancelado",6100H)						!
       Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
            STR$(TS.ERRF),6100H)
       Ue.mktp.Line% = 00    																								! Cancela proyecto
       Exit Sub              																								! Sale funcion
    EndIf           																												!
    ASC.MKP.OPEN% = -1																											! Ctrl apertura
 EndIf																																			!

 Dim UE.MCD.SEC$(500,4)																											! Dimensiono vector de Secciones 
 Asc.MKP.SEC% = 0                                                           !
 TS.ER.RETURN = -1																													!
 !Call U.Imprime("Fin Carga Market Online ",6100H)													! Rastro en SJ
 !Exit Sub 
 
!--- Procedimiento a ser optimizado  
 Open "R::$KPTMP1" AS Gr.Mkp.SesTmp%	UNLOCKED NOWRITE NODEL                             	! Open promociones seccion
 If TS.ER.RETURN <> -1 Then Begin                                           ! Error en la apertura
 	  GoTo NEXT.CARGA  			                        	                        ! No hay promocion de secciones
 EndIf                                                                      !
 If END #Gr.Mkp.SesTmp% Then FIN.CARGA.MKTP  				                       	! 
 While (1)							                                                  	! Recorre file
  READ #Gr.Mkp.SesTmp%;LINE X.H$						                               	!
  X.AUX$   = MID$(X.H$,1,18)            			                            	! Extrae la llave
  X.MSG$   = MID$(X.H$,19,4)	        			                              	! Codigo de mensaje
  X.PROMO$ = MID$(X.H$,23,10)					                                      ! Promocion
  XCLTE$   = MID$(X.H$,31,4)
  If LEFT$(X.AUX$,2) = "32" Then Begin		  	                          		! Promocion de NO PRESENCIA secciones
       ASC.MKP.SEC% = ASC.MKP.SEC% + 1			  	                            ! Aumenta contador
       ASC.MKP.TMP1$ = Mid$(X.AUX$,4,15)		  	                          	! Tomo los 15 digitos
       UE.MCD.SEC$(ASC.MKP.SEC%,0) = ASC.MKP.TMP1$	  	                    ! Seccion a promocionar
       UE.MCD.SEC$(ASC.MKP.SEC%,1) = X.MSG$                	              	! Numero mensaje
       UE.MCD.SEC$(ASC.MKP.SEC%,2) = "0"			  	                          ! Contador de venta
       UE.MCD.SEC$(ASC.MKP.SEC%,3) = Left$(X.PROMO$,6)	  	              	! Monto minimo de compra
       UE.MCD.SEC$(ASC.MKP.SEC%,4) = XCLTE$           	  	                ! Control clientes 
  EndIf																																			!
 Wend																																				!
 FIN.CARGA.MKTP:																														!
  Close Gr.Mkp.SesTmp%																											!
 NEXT.CARGA:																																!
 TS.ER.RETURN = -1																													!
 OPEN "R::$KPTMP2" AS Gr.Mkp.SesTmp%	UNLOCKED NOWRITE NODEL                             	! Open promociones no presencia
 If TS.ER.RETURN <> -1 Then Exit Sub       			                          	! No hay promocion
 If END #Gr.Mkp.SesTmp% Then FIN.CARGA.NOART 				                                  	! 
 X.CNTI% = 1: X.CNTJ% = 1																										!
 X.AUX$ = ""																																!
 While (1)							                                                  	! Recorre file
  READ #Gr.Mkp.SesTmp%; X.H$		                                       	    ! 
  UE.MCD.ART$(X.CNTJ%,0) = Right$(Mid$(X.H$,3,16),12)                       ! Plu a evaluar
  UE.MCD.ART$(X.CNTJ%,1) = Mid$(X.H$,19,4)                                  ! Mensaje a generar
  UE.MCD.ART$(X.CNTJ%,2) = Mid$(X.H$,31,4)                                  ! Dirigido a lista cliente
  X.CNTJ% = X.CNTJ% + 1    																							    ! Aumenta posicion vector
  If X.CNTJ% >= 500 Then Begin 																					    !
     GoTo FIN.CARGA.NOART																								!
  EndIf																																			!
 Wend																																				!
 FIN.CARGA.NOART:																														!
  Close Gr.Mkp.SesTmp%
  ASC.MKP.ART% = X.CNTJ%       																							! Asigna tamano vector
End Sub 
!--- Fin carga parametros modulo de mercadeo
    
Sub INIT.PARAMETROS.MKTPLUS
 ASC.MKP.TS7%   = 2  																												! Para control UE EAMEMT01.J86
 TS.ER.RETURN = -1																													!
 !Call ALIMENTA.IMAGENES																										! Carga logos Publicitarios en memoria
 
 TS.ER.RETURN = -1																													!
 DIM UE.MCD.LOG$(7,2)																												!
 DIM UE.MCD.ART$(500,2)   																									! Control No presencia de articulos
 ASC.MKP.CADENA$ = "0000"																										! Cadena por omision 
 Gr.Mkp.Excento% = 0
 FOR I% = 1 TO 5																														!
   UE.MCD.LOG$(I%,0) = "0"																									!
   UE.MCD.LOG$(I%,1) = "9"																									!
   UE.MCD.LOG$(I%,2) = "0"																									!
 NEXT I%																																		!
 !Dim ASC.MKP.TGC$(99)																											! Manejo target coupons

 If UE.MSG.OPEN% = 0 Then Begin 																						! Si no apertura 
    TS.ER.RETURN = -1																												!
    OPEN "R::$KPMSGPR" KEYED RECL 279 AS ASC.MKP.SE2%		UNLOCKED NOWRITE NODEL            ! Apertura de mensajes
    If TS.ER.RETURN <> -1 Then Begin																				!
       Call U.Imprime("Error Apertura MKPMSGPR, Cancelado",6100H)						!
       Ue.mktp.Line% = 00    																								! Cancela proyecto
       Exit Sub  
    EndIf                                                                   !
    UE.MSG.OPEN% = -1																												!
 EndIf																																			!
  Call CARGA.PARAMETROS.MRKTPLUS																						!
  If GR.MKP.FILES% <> -1 Then Begin 																				! Si proceso en IPL 
     TS.ER.RETURN = -1 
     Open "R::$KPPRITM" KEYED RECL 31 AS 54 UNLOCKED NOWRITE NODEL   	                    ! Open promociones
     If TS.ER.RETURN <> -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN MKTP:MKPPRITM",6100H)
     EndIf
     TS.ER.RETURN = -1 
     Open "R::$ECCION" KEYED RECL 84 AS 22 UNLOCKED NOWRITE NODEL           ! Descuento por seccion   
     If TS.ER.RETURN <> -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN MKTP:SECCION",6100H)
     EndIf
     TS.ER.RETURN = -1 
     Open "R::$F:A0004" KEYED RECL 8 AS 59  UNLOCKED NOWRITE NODEL                        ! Validacion de rifas por producto
     If TS.ER.RETURN <> -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN TF:A0004",6100H)
        Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                     STR$(TS.ERRF),6100H)
        
        Ue.mktp.Line% = 00 
        Exit Sub 
     EndIf
     GR.MKP.FILES% = -1
  EndIf 
  
End Sub 
!--- Fin carga parametros MarketPlus

Sub Msg.Publicitario.Item (X.MSG$)              								            ! Impresion Msg 
String LEC$, UE.LF$, UE.EAN8$, UE.BARCODE$, UE.ENDBCODE$, UE.M.RESALTA$     ! Publicitarios
String LTRA$, CPI$, X.MSG$, X.CUPON$, LTRB$, ALINEAR$												!
String XL1$, XL2$, XL3$, XL4$
Integer*1 SALTO%, IND.IMP.CAB%																							!
Integer*2 UE.L.I%, Xlogo%		

   Dim UE.MCD.MSG$(10)																												!
   Dim UE.MCD.MST$(10)																												!
   If VAL(X.MSG$) = 0 Then Exit Sub     																		!
   X.MSG$ = PACK$(RIGHT$("0000"+X.MSG$,4))																	! Arma llave de busqueda
   TS.ER.RETURN = -1																												!
   LEC$ = "C2 C1 C2 C4 4C20 5C38"																						! Formato registo
   READ FORM LEC$;#ASC.MKP.SE2% KEY X.MSG$; 		                           \! Lee Reg Cabecera 
       UE.MCD.COD$,    																										 \! Codigo
       UE.MCD.LOGO$,    																									 \! Logo
       X.CUPON$,       																										 \! Cupon
       UE.MCD.PRM$,    																										 \! Parametrizacion
       UE.MCD.MSG$(1), 																										 \! Mensaje Operador
       UE.MCD.MSG$(2), 																										 \! Mensaje Operador         
       UE.MCD.MSG$(3), 																										 \! Mensaje Operador               
       UE.MCD.MSG$(4), 																										 \! Mensaje Operador       
       UE.MCD.MST$(1), 																										 \! Mensaje Tirilla
       UE.MCD.MST$(2), 																										 \! Mensaje          
       UE.MCD.MST$(3), 																										 \! Mensaje        
       UE.MCD.MST$(4), 																										 \! Mensaje        
       UE.MCD.MST$(5)    																										! Mensaje  Tirilla
    If TS.ER.RETURN = -1 Then Begin																					! Si found
       If LEFT$(UE.MCD.PRM$,1) = "1" Then Begin         					        	! Msg Operador
          Call TSHIECET
          Call VISORES4690(1,UE.MCD.MSG$(1),UE.MCD.MSG$(2),1700,"L")				! Despliega msg
       EndIf																	                              !
       If MID$(UE.MCD.PRM$,2,1) = "1" Then Begin         						        ! Msg Cliente
          Call TSHIECET
          Call VISORES4690(2,UE.MCD.MSG$(3),UE.MCD.MSG$(4),1700,"L")				! Despliega msg
       EndIf																			       
    EndIf 																																	!
     
End Sub 																														        !
!--- Fin impresion mensajes Publicitarios																		!

Function Imprime.Msg.Publicitario(X.MSG$) Public								            ! Impresion Msg 
String LEC$, UE.LF$, UE.EAN8$, UE.BARCODE$, UE.ENDBCODE$, UE.M.RESALTA$     ! Publicitarios
String LTRA$, CPI$, X.MSG$, X.CUPON$, LTRB$, ALINEAR$												!
String XL1$, XL2$, XL3$, XL4$
Integer*1 SALTO%, IND.IMP.CAB%																							!
Integer*2 UE.L.I%, Xlogo%		

   If UE.RECASRV.QTY% <> 0 Then Exit Function 															! Si es recaudo no genera vocuhers Mod 3 Abr 2017
   IF Gr.Mrm.Cap% = -1     Then Exit Function   														! Si es una remision 
   To.UserExit(20) = 00
   To.UserExit(60) = 00
   ASC.MKP.IMP%    = 0 
   If PRT4610.ENABLE = 0 Then UE.LF$ = " " Else UE.LF$ = CHR$(0DH)					!
   If PRT4610.ENABLE = 0 Then SALTO% = 10 Else SALTO% = 5										!
   If PRT4610.ENABLE = 0 Then  Begin																				!
      LTRA$ = CHR$(27)+CHR$(14)																							!
	    LTRB$ = CHR$(27)+CHR$(14)																							!
	    ALINEAR$ = ""																													!
   EndIf Else Begin																													!
     LTRA$ = CHR$(1BH)+CHR$(21H)+CHR$(11H) 																	! Font A - Double Wide
     LTRB$ = CHR$(1BH)+CHR$(21H)+CHR$(01H) 																	! Font B - Double Wide
     ALINEAR$ = CHR$(1BH)+CHR$(61H)+CHR$(01H)	  														!
   EndIf																																		!
   If PRT4610.ENABLE Then 																							   \!	
      UE.M.RESALTA$ = CHR$(1BH)+CHR$(48H)+CHR$(1) 												 \!
    Else 																																	 \!
      UE.M.RESALTA$ = CHR$(27)+CHR$(69)																			!
   UE.BARCODE$  = CHR$(1DH) + CHR$(6BH) 																		! Manejo de impresion
   UE.ENDBCODE$ = CHR$(00H) 																								! codigos de barra
   UE.EAN8$    = CHR$(03H)																									! en formato EAN-8
   IND.IMP.CAB% = 0																													!
   Dim UE.MCD.MSG$(10)																											!
   Dim UE.MCD.MST$(10)																											!
   If VAL(X.MSG$) = 0 Then Exit Function																		!
   X.MSG$ = PACK$(RIGHT$("0000"+X.MSG$,4))																	! Arma llave de busqueda
   TS.ER.RETURN = -1																												!
   LEC$ = "C2 C1 C2 C4 4C20 5C38"																						! Formato registo
   READ FORM LEC$;#ASC.MKP.SE2% KEY X.MSG$; 		                           \! Lee Reg Cabecera 
       UE.MCD.COD$,    																										 \! Codigo
       UE.MCD.LOGO$,    																									 \! Logo
       X.CUPON$,       																										 \! Cupon
       UE.MCD.PRM$,    																										 \! Parametrizacion
       UE.MCD.MSG$(1), 																										 \! Mensaje Operador
       UE.MCD.MSG$(2), 																										 \! Mensaje Operador         
       UE.MCD.MSG$(3), 																										 \! Mensaje Operador               
       UE.MCD.MSG$(4), 																										 \! Mensaje Operador       
       UE.MCD.MST$(1), 																										 \! Mensaje Tirilla
       UE.MCD.MST$(2), 																										 \! Mensaje          
       UE.MCD.MST$(3), 																										 \! Mensaje        
       UE.MCD.MST$(4), 																										 \! Mensaje        
       UE.MCD.MST$(5)    																										! Mensaje  Tirilla
    If TS.ER.RETURN = -1 Then Begin																					! Si found
       If (VAL(UNPACK$(UE.MCD.CPN$)) <> 0) OR 								             \! Si impresion logo o
	      (VAL(UNPACK$(UE.MCD.LOGO$)) <> 0) Then IND.IMP.CAB% = -1            ! cupon
       If MID$(UE.MCD.PRM$,3,1) = "1" Then IND.IMP.CAB% = -1					      ! Msg tirilla
       If IND.IMP.CAB% = -1 Then Begin																			!
          ASC.MKP.TMP1$ = LTRA$+"*** GANASTE ***"														!
		      ASC.MKP.TMP1$ = ALINEAR$+ASC.MKP.TMP1$														!
          !Call U.Imprime(ASC.MKP.TMP1$,4200H)															!
          
		      If CA.CUSTOMER$ = "" Then CA.CUSTOMER$ ="ESTIMADO(A) CLIENTE"			!
		      TS.TEMP1$ = "Sr(a) "+CA.CUSTOMER$																	!
		      TS.TEMP1$ = Left$(TS.TEMP1$,37)																		!
          Call U.Imprime(ALINEAR$+LTRB$+TS.TEMP1$,4100H)										!
          TS.TEMP1$ = "ID CLTE:"+Gr.Lcl.Clte$
          TS.TEMP1$ = Left$(TS.TEMP1$,37)																		!
          Call U.Imprime(TS.TEMP1$,4100H)
       EndIf																																!
       If MID$(UE.MCD.PRM$,3,1) = "1" Then Begin         									  ! Msg Tirilla
       	  Gr.Mkp.TirClte% = 0                                               ! Init cntrl dato cliente
          Call U.Imprime(Mkt.String(UE.MCD.MST$(1)),4100H)									!
          Call U.Imprime(Mkt.String(UE.MCD.MST$(2)),4100H)									!
          Call U.Imprime(Mkt.String(UE.MCD.MST$(3)),4100H)									!
          Call U.Imprime(Mkt.String(UE.MCD.MST$(4)),4100H)									!
          Call U.Imprime(Mkt.String(UE.MCD.MST$(5)),4200H)									!
          If Gr.Mkp.TirClte% = -1 Then Begin 
          	 CALL U.IMPRIME("Nombre:______________________________",4100H)  !
          	 CALL U.IMPRIME("Cedula:______________________________",4100H)  !
          	 CALL U.IMPRIME("Telefono:____________________________",4200H)  !
          EndIf
          ASC.MKP.IMP% = -1																									!
       EndIf																																!
       If PRT4610.ENABLE <> 0 Then Begin		    														! Manejo impresora Termica
         UE.MCD.LOGO$ = UNPACK$(UE.MCD.LOGO$)    														! Numero de logo
         If VAL(UE.MCD.LOGO$) <> 0 Then Begin	   														! Si logo definido
            Xlogo% = VAL(UE.MCD.LOGO$)																			! Numero de logo a ser impreso
            Call IMPRESION.LOGO(Xlogo%)																			! Impresion del logo
            ASC.MKP.IMP% = -1																								!
         EndIf																															!
       EndIf																																!
       If PRT4610.ENABLE <> 0 Then Begin																		! Manejo impresora Termica
         X.CUPON$ = UNPACK$(X.CUPON$)	    																	! Numero de Cupon
         If VAL(X.CUPON$) <> 0 Then Begin	             											! Si Cupon definido
   		    Call U.Imprime(String$(37,"-"),4100H)		 													!
            X.CUPON$ = "000"+X.CUPON$+"7"		     														! Arma barras
	              Asc.MKP.TMP1$ = CHR$(1DH)+CHR$(77H)+CHR$(01H) +    		     \! Horizontal barcode
		            CHR$(1DH)+CHR$(68H)+CHR$(40H)+                             \! Vertical barcode			
		            CHR$(1DH)+CHR$(48H)+CHR$(02H)+                             \! Human Read characters
		            CHR$(1DH)+CHR$(66H)+CHR$(00H)    								            ! Tipo letra B para HRI						
            Call U.Imprime(ASC.MKP.TMP1$,4100H)															!
            ASC.MKP.TMP1$ = (UE.BARCODE$+UE.EAN8$+X.CUPON$+UE.ENDBCODE$)    ! Arma String de impresion			
            WRITE #34;ASC.MKP.TMP1$ 												                ! Imprime cupon en EAN-8
            ASC.MKP.IMP% = -1																								!
            Call U.Imprime(UE.LF$,4100H)																		! Lineas en blanco	
            ASC.MKP.TMP1$ = ""																							!
         EndIf																															!
       EndIf																																!
       If IND.IMP.CAB% = -1 Then Begin 																			!
!          FOR K% = 1 TO SALTO%                            									!
!              Call U.Imprime(UE.LF$,4100H)	  															! Lineas en blanco	
!          NEXT K% 																        									!
!          Call U.CORTACR       											      									! Corto papel
!   	      IND.IMP.CAB% = 0																									!
       EndIf																																!
       If ASC.MKP.IMP% = -1 Then Begin
          TS.TEMP1$ = MKT.Detalle
          Call U.IMPRIME(TS.TEMP1$,4100H)                  									 ! Detalle Trailer Trx        
          Call U.IMPRIME(String$(19,"- "),6100H)
          Call U.IMPRIME("",4900H)
          Call U.CORTACR       
       EndIf 
       If LEFT$(UE.MCD.PRM$,1) = "1" Then Begin         					        	! Msg Operador
          Call TSHIECET
          Call VISORES4690(1,UE.MCD.MSG$(1),UE.MCD.MSG$(2),1700,"L")				! Despliega msg
       EndIf																	                              !
       If MID$(UE.MCD.PRM$,2,1) = "1" Then Begin         						        ! Msg Cliente
          Call TSHIECET
          Call VISORES4690(2,UE.MCD.MSG$(3),UE.MCD.MSG$(4),1700,"L")				! Despliega msg
       EndIf																			       
       
    EndIf Else Begin																												!
      ! Call VISORES4690(1,"Msg No Existe",UNPACK$(X.MSG$),1500,"L")					!
      
    EndIf													
    																					!
!-- Comentariado 7 Jul 2010 - OVS
!    If IND.IMP.CAB% = -1 Then Begin
!       Call U.IMPRIME(TO.HEADERLINE1$,4100H)
!       Call U.IMPRIME(TO.HEADERLINE2$,4100H)
!    EndIf 

    IND.IMP.CAB% = 0 
    To.UserExit(20) = -1
    To.UserExit(60) = -1
     
End Function																																!
!--- Fin impresion mensajes Publicitarios																		!

Function GRABA.POSICION.MSG(UE.MSG$,UE.B.QTY%) Public		                    ! Almacena Msg publi.
!-- Niveles Prioridad  1 Es el mas Alto 4 el mas bajo
 String UE.MSG$, UE.DATA1$, UE.DATA2$, UE.DATA3$, UE.DATA4$                 ! Definicion variables
 Integer*1 UE.C.I%, UE.IN.MSG%					                                    !
 Integer*4 UE.B.QTY%																												!
 If Val(UE.MSG$) <> 0 Then Begin 			                                      ! Mensaje definido
    TS.ER.RETURN = -1																												!
    UE.MSG$ = Pack$(Right$("0000"+UE.MSG$,4))
    READ FORM "C2 C3 C4 C270";#ASC.MKP.SE2% KEY UE.MSG$;                   \! Busca logo
         UE.DATA1$, UE.DATA2$, UE.DATA3$, UE.DATA4$                         !
    If TS.ER.RETURN = -1 Then Begin                                         ! Si encontro Msg
       UE.MSG$ = UNPACK$(UE.DATA1$)																					! NUMERO DE MENSAJE
       UE.DATA1$ = RIGHT$(UE.DATA3$,1)       			                          ! Toma prioridad
       UE.IN.MSG% = 0						                                            ! Init variable
       
       FOR UE.C.I% = 1 TO 5					                                        ! Busca 5 msg publi.
        If VAL(UE.MCD.LOG$(UE.C.I%,0)) = VAL(UE.MSG$) Then Begin           \! 
           UE.IN.MSG% = -1																									! Lo encontro
           UE.MCD.LOG$(UE.C.I%,2) = STR$(VAL(UE.MCD.LOG$(UE.C.I%,2)) +     \! Aumenta numero de logos solicitados
  				    UE.B.QTY%)																										!
        EndIf																																!
       NEXT UE.C.I%						                                              !
       If UE.IN.MSG% = 0 Then Begin				                                  ! No lo encontro
        For UE.C.I% = 1 TO 5					                                      ! Busca 5 msg publi.
          UE.DATA4$ = STR$(VAL(UE.MCD.LOG$(UE.C.I%,1)))                     ! Toma prioridad
          If VAL(UE.DATA1$) < VAL(UE.DATA4$) Then Begin                     ! Compara prioridad
             UE.MCD.LOG$(UE.C.I%,0) = (UE.MSG$)		                          ! Asigna Nro Msg.
             UE.MCD.LOG$(UE.C.I%,1) = UE.DATA1$			                        ! Asigna prioridad 
             UE.MCD.LOG$(UE.C.I%,2) = Str$(UE.B.QTY%)                       ! Asigna Qty
             UE.C.I% = 10					                                          ! Sale del ciclo
          EndIf																															!
        Next UE.C.I%																												!
       EndIf																																!
    EndIf																																		!
 EndIf																																			!
 
End Function																																!
!--- Fin grabar posicion de logos

!Function BUSQ.ARTICULO(UE.B.DATO$) Public			                              ! Busqueda msg Publicitarios
!String UE.B.DATO$, UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$                   ! Definicion de variables
!Integer*1 UE.B.RTA%						                                              !
!Integer*4 UE.B.QTY%, XPrice%	                                              !
! Gr.Msg.Itm$ = "0000" 
! UE.B.DATO$ = UNPACK$(UE.B.DATO$)				                                    ! Empaqueta llave
! UE.B.DATO$ = Right$("0000000000000000"+UE.B.DATO$,16)
! UE.B.DATO$ = PACK$("30"+UE.B.DATO$)				                                ! 
! If TS.IO.DATA$(6) = "" Then UE.B.QTY% = 1 Else                            \! Valida Qty capturada
!    UE.B.QTY% = VAL(TS.IO.DATA$(6))				                                  !
! If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then                      \! Si es una devolucion / anulacion
!    UE.B.QTY% = UE.B.QTY% * -1	 				                                    ! 
! If (SL.IT.INDICAT2 AND 80H) OR (SL.IT.INDICAT2 AND 40H) Then              \!
!    UE.B.QTY% = UE.B.QTY% * -1	 							                              !             
! TS.ER.RETURN = -1						                                              ! Ctrl errores
! READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY UE.B.DATO$;                   \! Lee registro
!      UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$				                        !
! If TS.ER.RETURN = -1 Then	Begin                                           ! Si encontro
! 	  UE.B.D4$ = Left$(UE.B.D4$,4)                                            ! Nro de promocion
!    If MATCH(UE.B.D4$,ASC.MKP.LSTPRO$,1) Then Begin                         ! compara promocion para el cliente    
!       Call GRABA.POSICION.MSG(UNPACK$(UE.B.D2$),UE.B.QTY%)	                ! Grabar posicion de logos
!       Gr.Msg.Itm$ = UNPACK$(UE.B.D2$)																			! Retorna mensaje articulo
!    EndIf 
! EndIf 
!End Function																																!
!--- Fin de la busqueda de un articulo

Function BUSQ.ARTICULO(UE.B.DATO$) Public			                              ! Busqueda msg Publicitarios
String UE.B.DATO$, UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$                   ! Definicion de variables
Integer*1 UE.B.RTA%						                                              !
Integer*4 UE.B.QTY%, XPrice%	                                              !
 Gr.Msg.Itm$ = "0000" 
 UE.B.DATO$ = UNPACK$(UE.B.DATO$)				                                    ! Empaqueta llave
 UE.B.DATO$ = Right$("0000000000000000"+UE.B.DATO$,16)
 UE.B.DATO$ = PACK$("30"+UE.B.DATO$)				                                ! 
 If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then                      \! Si es una devolucion / anulacion
    Exit Function 
 TS.ER.RETURN = -1						                                              ! Ctrl errores
 READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY UE.B.DATO$;                   \! Lee registro
      UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$				                        !
 If TS.ER.RETURN = -1 Then	Begin                                           ! Si encontro
 	  UE.B.D4$ = Left$(UE.B.D4$,4)                                            ! Nro de promocion
    If MATCH(UE.B.D4$,ASC.MKP.LSTPRO$,1) Then Begin                         ! compara promocion para el cliente    
       Gr.Msg.Itm$ = UNPACK$(UE.B.D2$)																			! Retorna mensaje articulo
    EndIf 
 EndIf 
End Function																																!
!--- Fin de la busqueda de un articulo


Function IMP.LOGOS Public					                                          ! Impresion Msg 
Integer*1 UE.C.I%, XIND%
Integer*4 ZQty%, ZQ%
Real      Ga%, Gb%, Gc%, GD%, Xp%
String   Xlogo$

   XIND% = 0 						                                                    ! 
   For UE.C.I% = 1 TO 7						                                          ! Impresion 7 logos
    ASC.MKP.TMP1$ = UE.MCD.LOG$(UE.C.I%,0)			                            ! Tomo numero de mensaje
	  If VAL(UE.MCD.LOG$(UE.C.I%,2)) <= 0 Then GOTO LOGO.SIG                  !
    If VAL(ASC.MKP.TMP1$) = 0 Then GOTO LOGO.SIG                            ! Impresion logo
    Zqty% = Val(UE.MCD.LOG$(UE.C.I%,2))																			! Cantidad de Logos a imprimir
    
!    If (EMSS.TR.POINTS > 0) Then Begin 																		! Si compra y puntos acumulados
!     If Match("TCCE",ASC.MKP.CADENA$,1) > 0 Then \                         ! Numero boletas solo para club Excelencia
!    	 Zqty% = Int(Gr.Mkp.VlrCmpBol% / 20000)                               ! Calcula Numero de Boletas
!       Gr.Mkp.Vlr% = Gr.Promo.Compra%                                      ! Valor Entregado
!       Gr.Mkp.Bol% = Zqty%                                                 ! Numero de Boletas
!    	 Gr.Promo.Compra% = 0																									! 
!    EndIf Else Begin 
!    	!Zqty% = 0 																														! No genera boletas
!    	Gr.Mkp.Bol% = Zqty%                                                   ! Numero de Boletas
!    EndIf																																	  !

    Xlogo$ = ASC.MKP.TMP1$
    For ZQ% = 1 To Zqty%																										! Genera numero de boletas segun compra
      Call Imprime.Msg.Publicitario(Xlogo$)        													!	 
    Next ZQ%																																! 
    XIND% = -1 																															! GENERO LOGO
    LOGO.SIG:																																!
   Next UE.C.I%																															!
   If XIND% <> 0 Then Begin 
     Wait ; 100
     Call U.Imprime(TO.HEADERLINE1$,4100H)														      !
     Call U.Imprime(TO.HEADERLINE2$,4100H)													      	!
   EndIf 
End Function
!--- Fin impresion de logos Publicitarios

Function ASC.GETUNPK(X.B$,X.J%)																							! Desempaquetar dato
Integer*2 X.J%, X.K%																												!
String    X.B$, ASC.GETUNPK																									!
  X.K% = MATCH(":",X.B$,X.J%) 																							! SEARCH FOR FIELD SEPERATOR
  ASC.GETUNPK = UNPACK$(MID$(X.B$,X.J%,X.K%-X.J%)) 													! UNPACK FIELD
End Function
!--- fin funcion extraccion datos Log

!Function VENTA.TIQUETE Public               																! Lista de Items y dptos vendidos 
!  Integer*4 PRIC%, TOT4%, X.I%																							!
!  String    VENTA.TIQUETE, X.LISTA$, X.TMP$																	!
!  X.LISTA$ = ""				    																								  ! Init variable
!  FOR X.I% = 1 TO SL.END                    																! FOR ALL StringS
!    H$ = READ.SL.STR$(X.I%)                 																! GET String
!    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
!     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
!        X.TMP$ = ASC.GETUNPK(H$,3)	    																		! Tomo informacion del item vendido
!        PRIC% = MATCH(X.TMP$,X.LISTA$,1)																		!
!        If PRIC% <= 0 Then 																								 \!
!           X.LISTA$ = X.LISTA$ + ";" + X.TMP$           										! Arma cadena de String articulos vendidos
!     EndIf                                  																! ITEM ENTRY String
!  NEXT X.I%   
!  VENTA.TIQUETE = X.LISTA$	+ ";"  																					! Retorna lista
!End Function
!--- Fin recorrido strin de la transaccion

Function VENTA.TIQUETE Public               																! Lista de Items y dptos vendidos 
  Integer*4 PRIC%, TOT4%, X.I%, Xpos%																				!
  String    VENTA.TIQUETE, X.LISTA$, X.TMP$               							    !
  X.LISTA$ = ""				    																								  ! Init variable
  FOR X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
        X.TMP$ = ASC.GETUNPK(H$,3)	    																		! Tomo informacion del item vendido
        PRIC% = MATCH(X.TMP$,X.LISTA$,1)																		!
        If PRIC% <= 0 Then 																								 \!
           X.LISTA$ = X.LISTA$ + ";" + X.TMP$           										! Arma cadena de String articulos vendidos
     EndIf                                  																! ITEM ENTRY String
  NEXT X.I%   
  VENTA.TIQUETE = X.LISTA$	+ ";"  																					! Retorna lista
End Function
!--- Fin recorrido strin de la transaccion



!!*******************************************************************
!!** GENERA String CON LOS DEPARTAMENTOS VENDIDOS                   *
!!*******************************************************************
Function VENTAS.DEPTO  Public               																! GET SALES FOR DEPT
  Integer*4 PRIC%, TOT4%, X.I%, XJ%
  String    X.LISTA$, X.TMP$, X.comp$
  Integer*1 Xfnd%

  DIM Gr.Mkp.VtaDp$(900)
  Gr.Mkp.Ntdpt% = 0
  X.LISTA$ = ""			    																									  ! Init variable
  FOR X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
        IR.ITEMCODE$ = ASC.GETUNPK(H$,3)	  																! Tomo informacion del item vendido
        IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))        ! Ajusta dato de consulta
        TS.ER.RETURN = -1
        %INCLUDE EAMIRRD3.J86 																							! Busca producto
        If TS.ER.RETURN = -1 Then  Begin																	  ! Si lo encontro
           X.COMP$ = Right$(Asc.IR.NDEPTO$,15)                              ! Estructura comercial Item
           Xfnd% = 0
           For Xj% = 1 To Gr.Mkp.Ntdpt%
               If Gr.Mkp.VtaDp$(Xj%) = X.Comp$ Then                        \! Ya existe
               	  Xfnd% = -1 : Xj% = Gr.Mkp.Ntdpt% + 1                      ! lo reporta y sale 
           Next Xj%
           If Xfnd% = 0 Then Begin                                          ! No lo encuentra
           	  Gr.Mkp.Ntdpt% = Gr.Mkp.Ntdpt% + 1                             ! Total de departamentos
           	  If Gr.Mkp.Ntdpt% <= 900 Then Begin                            ! Menos de 900 dptos
           	  	 Gr.Mkp.VtaDp$(Gr.Mkp.Ntdpt%) = X.COMP$                     ! Almacena Dpto vendido
           	  EndIf
           EndIf
        EndIf
     EndIf                                  																! ITEM ENTRY String
  NEXT X.I%                                 																! NEXT String

!  Call U.IMPRIME("TOTAL DPTOS CARGADOS "+STR$(Gr.Mkp.Ntdpt%),4100H)         !
!  For XJ% = 1 To Gr.Mkp.Ntdpt%
!      Call U.IMPRIME("DPTO VT:"+Gr.Mkp.VtaDp$(XJ%),4100H)
!  Next XJ%

  Asc.MKP.PSEC% = 2                          																! Lista rutina 
End Function
!--- Fin recorrido strin de la transaccion

!-- Mensaje Por la presencia de un Item
Function Msg.Venta.Item (X.lista$) Public 
Integer*4 X.M%
String X.LISTA$, X.DATA$, UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$
 X.DATA$ = ""
 For X.M% = 2 TO LEN(X.LISTA$)                               ! Recorre cadena                       
   If MID$(X.LISTA$,X.M%,1) NE ";" Then Begin                ! Armando cadena de caracter           
      X.DATA$ = X.DATA$ + MID$(X.LISTA$,X.M%,1)              !                                      
   EndIf Else Begin                                          ! Cambio de seccion    
    X.Data$ = Right$("0000000000000000"+X.DATA$,16)          ! Arma llave de busqueda
    X.Data$ = PACK$("30"+X.Data$)				                     ! 
    TS.ER.RETURN = -1						                             ! Ctrl errores
    READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY X.Data$;    \! Lee registro
      UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$				         !
    If TS.ER.RETURN = -1 Then	Begin                          ! Si encontro
 	    UE.B.D4$ = Left$(UE.B.D4$,4)                           ! Nro de promocion
      If MATCH(UE.B.D4$,ASC.MKP.LSTPRO$,1) Then Begin        ! compara promocion para el cliente
      	 Call GRABA.POSICION.MSG(UNPACK$(UE.B.D2$),1)	       ! Grabar posicion de logos
      EndIf                                                  !
    EndIf                                                    !
    X.DATA$ = ""                                             ! Init proxima busqueda
   EndIf	
Next X.M%   	

End Function 

Function PRESENCIA.SECCION                                  ! Promocion para presencia seccion
Integer*4 X.I%, PRIC%, X.K%, X.Cnt%, Xpos%, XJ%
Integer*1 Xfnd%
String X.LISTA$, X.TMP$, X.DATA$
String UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$, X.Find$, X.Lst$, Xdpto$(2)
X.TMP$ = ""                                                 ! Init de la variable
Xpos% = 0
Dim Xdpto$(900,1)                                           ! Maximo 900 dpto
 FOR X.I% = 1 TO Gr.Mkp.Ntdpt%                              ! total dptos registrados
    X.Data$ = Gr.Mkp.VtaDp$(X.I%)                           ! Dpto vendido
  	X.Find$ = ""
  	X.Cnt% = 0 
    For X.Cnt% = 1 To 15 Step 3													    ! Analiza Departamento
      X.Lst$ = Mid$(X.Data$,X.Cnt%,3)                       ! Toma estructura inicial
      X.Find$ = X.Find$ + X.Lst$                            ! Arma estructura comercial 
      X.TMP$  = "0"+Left$(X.find$+"000000000000000",15)     ! Armando llave de busqueda
      X.TMP$  = PACK$("35"+X.TMP$)                          ! Arma llave busqueda
      TS.ER.RETURN = -1                                     ! Ctrl de errores
      READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY X.TMP$;  \! Busca registro                       
      UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$                !                                      
      If TS.ER.RETURN = -1 Then Begin                       ! Lo encontro
      	 X.tmp$ = Left$(UE.B.D4$,4)                         ! A quien va dirigido el mensaje
         X.K% = MATCH(X.tmp$,ASC.MKP.LSTPRO$,1)             ! compara promocion para el cliente    
         If X.K% > 0 Then Begin                             ! Para ese cliente                     
	        If TS.TOTALS(0,0,0) >= VAL(UE.B.D3$) Then Begin   ! Si cumple monto de compra
           Xfnd% = 0
	         For Xj% = 1 To Xpos%                             ! Busca si dpto ya almacenado
             If Xdpto$(XJ%,0) = UNPACK$(UE.B.D1$) Then     \! Ya almacenado Mensaje
             	  Xfnd% = -1  : Xj% = Xpos% + 1               ! Reporta ya encontrado
           Next XJ%
           If Xfnd% = 0 Then Begin 
              Xpos% = Xpos% + 1 
              If Xpos% <= 900 Then begin                    ! Si menos de 900 dptos
                 Xdpto$(Xpos%,0) = UnPack$(UE.B.D1$)        ! Dpto Encontrado
                 Xdpto$(Xpos%,1) = UnPack$(UE.B.D2$)        ! Almacena codigo del mensaje
              EndIf
           EndIf
          EndIf
          X.Cnt% = 20                                       ! Sale del ciclo   
      	 EndIf																							!
      EndIf																									!
    Next X.Cnt%
    X.DATA$ = ""
Next X.I%   	
X.I% = 0 
If XPOS% <> 0 Then \
   Call U.IMPRIME("TOTAL DPTOS MENSAJE "+STR$(XPOS%),2100H) !

For X.I% = 1 To XPOS%
!    Call U.IMPRIME("DPTO :"+Xdpto$(X.I%,0)+" MSG:"+Xdpto$(X.I%,1),4100H)

    Call GRABA.POSICION.MSG(Xdpto$(X.I%,1),1)               ! Imprime msg Publicitario             
Next X.I%

End Function
!--- Fin de la presencia de seccion

Sub COMPRA.MINIMA   																								      	  ! Promocion de compra minima
String UE.BCLIENTE$, X.DATA$, X.tmp$   																				! Definicion de
String UE.B.CLTE$, UE.B.LOGO$, UE.B.LISTA$, UE.B.TMP$   											! variables tmp
Integer*4 X.G%, X.K%, ZI%, Zqty%																						  !
Real      Zq%
For X.G% = 1 TO LEN(ASC.MKP.CADENA$) STEP 4			    													! Hasta longitud cadena
    UE.BCLIENTE$ = Mid$(ASC.MKP.CADENA$,X.G%,4)			      										! Extrae lista promociones
    UE.BCLIENTE$ = Right$("0000000000000000"+UE.BCLIENTE$,16)   							! Ajusta dato del cliente
    UE.BCLIENTE$ = PACK$("33"+UE.BCLIENTE$)	                									! dato del cliente
    TS.ER.RETURN = -1						                            									! Ctrl errores
    READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY UE.BCLIENTE$;                \! Lee registro
       UE.B.CLTE$, UE.B.LOGO$, X.DATA$, UE.B.LISTA$     											!
    If TS.ER.RETURN = -1 Then Begin			 	                  									! Si found
    	  If (Gr.Mkp.VlrCmpBol% - Gr.Mkp.Excento%) >= Val(X.DATA$) Then Begin   ! Si cumple compra minima
!    	  	Zq% = (Gr.Mkp.VlrCmpBol% - Gr.Mkp.Excento%) / Val(X.DATA$)          ! Calcula Numero de Boletas
!    	  	Zqty% = Int(Zq%)                                                    ! Toma Parte Entera
          Zq% = 1
 	        
 	        Call GRABA.POSICION.MSG(UNPACK$(UE.B.LOGO$),ZQTY%)     		 			    ! Msg al grupo o tipo de cliente
 	        
! 	        Call U.IMPRIME("NRO BOLETAS COMPRA MINIMA :"+STR$(ZQTY%),2100H)     ! Msg Numero de boletas a generar
    	  EndIf
    EndIf																																			!
Next X.G%																																			! Fin analisis ciclo

End Sub 
!--- Fin control compra minima

Function NO.PRESENCIA.SECCION(X.LISTA$)																			! No presencia de seccion
Integer*1 Xfound%
Integer*4 X.I%, PRIC%, X.Cnt%, Xpos%																				!
String X.LISTA$, X.TMP$, X.Lst$, X.Find$, X.Seek$, Xnxt$

FOR X.I% = 1 TO ASC.MKP.SEC%																								! Lista de secciones a validar no presencia
   X.TMP$ = UE.MCD.SEC$(X.I%,0)                       											! Toma seccion a validar
   X.Find$ = ""																															!
   Xfound% = 0																															!
   Xnxt$ = "1"																													    ! 
   For X.Cnt% = 1 To 15 Step 3																					    ! Analiza Departamento
      X.Lst$ = Mid$(X.TMP$,X.Cnt%,3)                                        ! Toma estructura inicial
     If Val(Xnxt$) >= 0 Then begin  
      X.Find$ = X.Find$ + X.Lst$                                            ! Arma estructura comercial 
      X.Seek$ = "1" + X.Find$                                               ! Analiza Estructura
      X.Seek$ = ";"+X.Seek$
      PRIC% = MATCH(X.SEEK$,X.LISTA$,1)        															! Busca la venta
      If PRIC% > 0 Then Begin						  																	! Si se vendio
      	 Xfound% = -1																												! Encontrado
      EndIf																																	!
      Xnxt$ = MID$(X.tmp$,(X.cnt%+3),15)
     EndIf
   Next X.Cnt%																															!

   If Xfound% = 0 Then Begin																								! Si no vendido
   	If TS.TOTALS(0,0,0) >= VAL(UE.MCD.SEC$(X.I%,3)) Then Begin						  ! Compra minima
   		If MATCH(UE.MCD.SEC$(X.I%,4),ASC.MKP.LSTPRO$,1) > 0 Then              \! Para una lista de clientes
   	     Call GRABA.POSICION.MSG(UE.MCD.SEC$(X.I%,1),1)	   							    !
   	EndIf
   EndIf
   
Next X.I%

End Function
!--- Fin reporte no presencia seccion

Sub NO.PRESENCIA.ARTICULOS Public
Integer*4 X.I%, PRIC%, X.K%, X.Cnt%
String X.LISTA$, X.TMP$, X.DATA$
String UE.B.D1$, UE.B.D2$, UE.B.D3$, UE.B.D4$, X.Find$, X.Lst$
X.TMP$ = ""                                                 									! Init de la variable                  
X.Lista$ = Venta.Tiquete																											! Lista Articulos Vendidos
For X.I% = 1 To Asc.MKP.ART%      																						! Hasta articulos cargados
   X.TMP$ = ";" + Str$(Val(UE.MCD.ART$(X.I%,0))) + ";"												! Articulo a validar  
 	 If MATCH(X.TMP$,X.Lista$,1) <= 0 Then Begin            								    ! Si no esta vendido
 	 	  X.tmp$ = UE.MCD.ART$(X.I%,2)																						! Dirigido a lista cliente
   	 	If MATCH(X.tmp$,ASC.MKP.LSTPRO$,1) > 0 Then                            \! Para una lista de clientes
   	     Call GRABA.POSICION.MSG(UE.MCD.ART$(X.I%,1),1)	 							        ! Mensaje a ser impreso
   EndIf
Next X.I%

End Sub 
!--- Fin validacion no presencia articulos

!---- Fin rutinas de mercadeo adicionadas

!-- En esta funcion, el parametro X.DATA% retorna los siguientes
!-- valores a la funcion:
!-- 1. Retorna el valor de control de la promocion si es para 
!--    todos los clientes o solo para una lista especifica
!-- 2. Retorna el nombre de la promocion
!

Function BUSQUEDA.EVENTO(UE.B.EVENTO%,X.DATA%)
Integer*2 UE.B.EVENTO%, X.DATA%
String UE.B.LLAVE$, UE.B.NOMBRE$,UE.B.CTRL$, BUSQUEDA.EVENTO, \
       UE.B.MSG$
  UE.B.CTRL$ = RIGHT$("00"+STR$(UE.B.EVENTO%),2)
  If X.DATA% = 1 Then  BUSQUEDA.EVENTO = UE.B.CTRL$
  If X.DATA% = 2 Then  BUSQUEDA.EVENTO = ASC.MKP.DESCRIPTOR$

  Exit Function

  UE.B.LLAVE$ = PACK$(RIGHT$("0000"+STR$(UE.B.EVENTO%),4))
  TS.ER.RETURN = -1
  OPEN "R::$KPDEFEV" KEYED RECL 24 AS Gr.Mkp.SesTmp% UNLOCKED NOWRITE NODEL
  If TS.ER.RETURN <> -1 Then Begin
     BUSQUEDA.EVENTO = "00 CLIENTE     "
     Exit Function
  EndIf
  TS.ER.RETURN = -1					! ctrl error
  READ FORM "C2 C16 C4 C2";#Gr.Mkp.SesTmp% KEY UE.B.LLAVE$;  \! busca evento
             UE.B.LLAVE$, UE.B.NOMBRE$,          \!
   	         UE.B.CTRL$,  UE.B.MSG$               !
  If TS.ER.RETURN = -1 Then Begin
     UE.B.NOMBRE$ = UNPACK$(UE.B.LLAVE$) + " " + UE.B.NOMBRE$
     UE.B.MSG$ = UNPACK$(UE.B.MSG$)
  EndIf Else Begin
     UE.B.NOMBRE$ = "00 CLIENTE     "  ! Nombre de la promocion
     UE.B.CTRL$ = ""                   ! Ctrl de la promocion
     UE.B.MSG$ = PACK$("0000")         ! Msg Publicitario
  EndIf
  Close Gr.Mkp.SesTmp%
  Call GRABA.POSICION.MSG(UE.B.MSG$,1) ! Toma msg del evento
  If X.DATA% = 2 Then BUSQUEDA.EVENTO = UE.B.NOMBRE$
  If X.DATA% = 1 Then BUSQUEDA.EVENTO = RIGHT$(UE.B.CTRL$,2)

End Function
!--- Fin de la busqueda de eventos

Function BUSQUEDA.CLIENTE(UE.BCLIENTE$)          		    ! Busca datos clte
String UE.BCLIENTE$, BUSQUEDA.CLIENTE, X.DATA$			    ! Definicion de
String UE.B.CLTE$, UE.B.LOGO$, UE.B.LISTA$, UE.B.TMP$   ! variables tmp
Integer*2 UE.B.I%                                       !
UE.BCLIENTE$ = STR$(VAL(UNPACK$(UE.BCLIENTE$)))  		    ! Arma llave
UE.BCLIENTE$ = Right$("0000000000000000"+UE.BCLIENTE$,16)   ! Ajusta dato del cliente
UE.BCLIENTE$ = PACK$("31"+UE.BCLIENTE$)	                ! dato del cliente
TS.ER.RETURN = -1						                            ! Ctrl errores
READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY UE.BCLIENTE$; \! Lee registro
       UE.B.CLTE$, UE.B.LOGO$, X.DATA$, UE.B.LISTA$     !
If TS.ER.RETURN = -1 Then Begin			 	                  ! Si found
     UE.B.LISTA$ = UNPACK$(UE.B.LISTA$)				          ! Toma lista promociones

     UE.B.TMP$   = ""     
     FOR UE.B.I% = 1 TO LEN(UE.B.LISTA$) STEP 4         ! Hasta longitud cadena
      If Val(MID$(UE.B.LISTA$,UE.B.I%,4)) <> 0 Then Begin	! Extrae lista promociones
         UE.B.TMP$ = UE.B.TMP$ + MID$(UE.B.LISTA$,UE.B.I%,4)
      EndIf                                             !
     NEXT UE.B.I%                                       !
     UE.B.LISTA$ = UE.B.TMP$ + "0000"                   ! Retorna lista promociones

   If Val(UnPack$(UE.B.LOGO$)) > 0 Then                \! Mensaje para el cliente
     Call GRABA.POSICION.MSG(UNPACK$(UE.B.LOGO$),1)     ! Msg al cliente

    
EndIf Else Begin 
   UE.B.LISTA$ = "0000": X.DATA$ = ""	                  ! Si EOF retorna nulo
EndIf 

UE.B.LISTA$ = UE.B.LISTA$ + Gr.Lcl.TipClte$             ! Adiciona la lista de tipificacion clientes


Asc.MKP.LSTPRO$ = UE.B.LISTA$		                        ! Retorna lista ctrl promociones

!Call u.imprime("Lst:"+ASC.MKP.LSTPRO$,4100h)

BUSQUEDA.CLIENTE = UE.B.LISTA$			                    ! Retorna dato

End Function                                            !
!--- Fin busqueda de cliente

!-- Busqueda de promociones a productos
!-- tipo de promocion que aplica
!-- 1. Descuento en pesos
!-- 2. Descuento en porcentaje
!-- 3. Precio Final
!---4. Asignacion de LINKEDTO Y PROMOCODE
!
Function BUSQUEDA.PROMO(UE.OLD.PRECIO%) 			! Calculo promociones
 String UE.B.TMP$, UE.B.KEY$, UE.B.STAT$, UE.B.CPRM$, UE.B.UD$, Ypromo$
 Integer*4 UE.B.I4%, UE.B.BENE%, UE.B.PRECIO%, UE.B.I%, UE.B.BENE2%
 Integer*4 BUSQUEDA.PROMO, UE.B.DSCTO%, UE.OLD.PRECIO%, UE.B.EVE%
 Integer*4 X.K%
 String UE.B.LIGA$, UE.B.PROMO$, UE.B.DATA$, UE.B.I4$, UE.B.ITEM$, U.Seek$, Xpromo$
 
 TS.ER.RETURN = -1						                                  ! Ctrl errores
 UE.B.BENE% = 0        			    	                              ! Para busqueda beneficio
 UE.B.DSCTO% = 0 																								! Init Descuento
 UE.B.CPRM$ = "0000" 

! If IR.LINKEDTO$ <> PACK$("0000") Then Begin                    ! Si tiene cupon ligado
!   	BUSQUEDA.PROMO = UE.B.DSCTO%																! Retorna dato
!    Exit Function																								! Sale de la funcion
! EndIf

 FOR UE.B.I% = 1 TO LEN(ASC.MKP.CADENA$) STEP 4			            ! Hasta longitud cadena
        UE.B.TMP$ = Mid$(ASC.MKP.CADENA$,UE.B.I%,4)			        ! Extrae lista promociones
        TS.TEMP2$ = PACK$(Right$("000000000000"+SL.IT.ITEMCODE$,12))
        Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)         ! Lectura Datos Itemr
        Call SPLIT.ITEMFILE( TS.TEMP1$ ) 			                  ! Entrega datos al eamitemr.j86
        If UnPack$(GR.INDICAT2$) = "05" Then Begin              ! Metodo de precio 5 
        	 UE.B.ITEM$ = GR.SALEQUAN$ + GR.SALEPRIC$             ! Busca articulo ligado
        EndIf Else UE.B.ITEM$ = PACK$(Right$("000000000000"+SL.IT.ITEMCODE$,12))
        UE.B.KEY$ = UE.B.ITEM$ + PACK$(UE.B.TMP$)               ! Arma llave
        
        !UE.B.EVE% = Val(UE.B.TMP$)
        
        TS.ER.RETURN = -1
        Read Form "C8 C1 C4 2C2 C12 C2";#54 KEY UE.B.KEY$;     \! Lee registro
          UE.B.TMP$, UE.B.STAT$, UE.B.I4$, UE.B.LIGA$,         \!
          UE.B.PROMO$, UE.B.DATA$, Xpromo$
        If TS.ER.RETURN = -1 Then Begin				                  ! Si found
            UE.B.PRECIO% = 9999999
            UE.B.I4% = Val(Unpack$(UE.B.I4$))                   ! Dscto a otorgar
            If UE.B.STAT$ = "1" Then Begin			                ! Dscto en Valor
              UE.B.PRECIO% = UE.OLD.PRECIO% - UE.B.I4%          ! Retira descuento 
              UE.B.BENE2% = UE.B.I4%
              Gr.Mkp.Tprm% = 1
              UE.B.CPRM$ = UNPACK$(UE.B.KEY$)                   ! Toma llave busqueda
              UE.B.CPRM$ = Right$(UE.B.CPRM$,4)                 ! Codigo de la promocion
           EndIf
           If UE.B.STAT$ = "2" Then Begin			                  ! Dscto en %
	            UE.B.I4% = (UE.OLD.PRECIO% * UE.B.I4%)/100        ! Calcula descuento 
              UE.B.PRECIO% = UE.OLD.PRECIO% - UE.B.I4%          ! Calcula descuento 
              UE.B.BENE2% = UE.B.I4%
              Gr.Mkp.Tprm% = 2 
              UE.B.CPRM$ = UNPACK$(UE.B.KEY$)                   ! Toma llave busqueda
              UE.B.CPRM$ = Right$(UE.B.CPRM$,4)                 ! Codigo de la promocion
           EndIf
           If UE.B.STAT$ = "3" Then Begin			                  ! Dscto en pesos
              UE.B.PRECIO% = UE.B.I4%				                    ! Asigna precio
              UE.B.I4% = UE.OLD.PRECIO% - UE.B.I4%
              UE.B.BENE2% = UE.B.I4%
              Gr.Mkp.Tprm% = 3 
              UE.B.CPRM$ = UNPACK$(UE.B.KEY$)                   ! Toma llave busqueda
              UE.B.CPRM$ = Right$(UE.B.CPRM$,4)                 ! Codigo de la promocion
           EndIf

!--- Para Manejo tipo AB y ABC  GR-OVS 26Nov2012           
           If UE.B.STAT$ = "4" Then Begin                       ! Asigna LINK y PROMOCODE
           	  IR.LINKEDTO$ = UE.B.I4$                           ! Asigna liga al producto PD
           	  IR.USEREXIT1 = VAL(UNPACK$(Xpromo$))              ! Asigna el promocode       
           EndIf	
           
	       !  If UE.B.STAT$ = "4" Then Begin                       ! Asigna LINK y PROMOCODE
         !     Close 38                                          ! Cierra archivo                                
	       !     ASC.MKP.TMP1$ = BUSQUEDA.EVENTO(UE.B.EVE%,1)      ! Busca ctrl promocion                          
         !     OPEN "R::MKTP:MKPPRITM.DAT" KEYED RECL 29 AS 38   ! Open promociones                              
         !     If ASC.MKP.TMP1$ <> "0000" Then Begin             ! Promocion para una lista de clientes          
         !        X.K% = MATCH(ASC.MKP.TMP1$,ASC.MKP.LSTPRO$,1)  ! compara promocion para el cliente             
         !        If X.K% <=0 Then Begin        			            ! Si no, sale de la funcion                    
  	     !           BUSQUEDA.PROMO  = 0                         ! Inicializa variables                      
         !           ASC.MKP.NRPRO%  = 0                         ! sale de la promocion                          
         !           ASC.MKP.OK%     = 0                         !                                               
  		   !           Close 38                                    !                                               
         !           Exit Function                               !                                               
         !        EndIf                                                                                          
         !     EndIf                                                                                             
	       !     IR.LINKEDTO$ = UE.B.LIGA$                         ! Asigna liga al producto   
   	     !     IR.USEREXIT1 = VAL(UNPACK$(UE.B.PROMO$))          ! Asigna el promocode       
	       !     BUSQUEDA.PROMO = 0                                ! Retorna el control a la   
	       !     ASC.MKP.NRPRO%  = 0                               ! aplicacion con manejo     
         !     ASC.MKP.OK%     = 0                               ! de cupones                
	       !     Close 38                                                                      
	       !     Exit Function			                                                            
	       !  EndIf Else Begin 
	       !  
	       !     UE.B.PRECIO% = 0
	       !     UE.B.DSCTO%  = 0 
	       !  EndIf 

         If UE.B.BENE2% > UE.B.BENE% Then Begin            ! Si el beneficio es menor
            UE.B.BENE%   = UE.B.BENE2%                    ! Toma como mejor beneficio
            UE.B.DSCTO%  = UE.B.I4%                       ! El mejor descuento
            
            !ASC.MKP.NRPRO% = UE.B.EVE%                     ! Numero de promocion
            
            ASC.MKP.OK%    = -1                            ! Status impresion
            Ypromo$ = Xpromo$                              ! Codigo Promocion
         EndIf	

        EndIf 

 Next UE.B.I%
 
 UE.B.BENE2% = Float(SL.IE.SALEPRIC * (ASC.MKP.DSCTO%/10)) / 100    ! Calcula el descto x seccion
  
 !Call VISORES4690(1,"DITM:"+Str$(UE.B.DSCTO%),"DSEC:"+STR$(UE.B.BENE2%),1500,"L")
 
 If UE.B.BENE2%  < UE.B.DSCTO% Then Begin 							! Si dscto superior al de seccion
 	  Gr.Mkp.Cprm$   = UE.B.CPRM$
 	  BUSQUEDA.PROMO = UE.B.DSCTO% 
 	  Gr.Mkp.Promo$  = Unpack$(Ypromo$)
 	  ASC.MKP.DSCTO% = 0
 	  Gr.Mkp.DstSec% = 0 
 EndIf Else Begin 
 	  Gr.Mkp.Cprm$   = ""
 	  BUSQUEDA.PROMO = 0 
 	  !Gr.Mkp.Promo$  = ""
 EndIf
 
 Exit Function

 If UE.B.BENE% <> 9999999 Then Begin				 ! Si aplico dscto
    ASC.MKP.TMP1$ = BUSQUEDA.EVENTO(ASC.MKP.NRPRO%,1)            ! Busca ctrl promocion
    If ASC.MKP.TMP1$ = "00" Then \                               ! Promocion todos los clientes
       BUSQUEDA.PROMO = UE.B.DSCTO%	Else \                       ! Entrega Dscto
    Begin
       X.K% = MATCH(ASC.MKP.TMP1$,ASC.MKP.LSTPRO$,1)             ! compara promocion para el cliente
       If X.K% <=0 Then Begin        				                     ! Si no, sale de la funcion
          BUSQUEDA.PROMO = 0
          ASC.MKP.NRPRO%  = 0
          ASC.MKP.OK%     = 0
	  Exit Function 
       EndIf
    EndIf   
 EndIf Else Begin
    BUSQUEDA.PROMO = 0
    ASC.MKP.NRPRO%  = 0
    ASC.MKP.OK%     = 0
 EndIf

End Function
!-- Fin procedimiento aplicacion de promociones

!Sub Valida.Rifa(XSgn%) Public 
!Integer*1 Xsgn%
!Integer*4 XI%
!String Xkey$, Xbasura$
! If TS.STANDALONE Then Begin       																					! Si en operacion TOF
!   Exit Sub 					                																			! Sale de la rutina
! EndIf						                    																			!
! For XI% = 1 to Gr.Mkp.CntRif%			               													! Valida Total de Rifas Cargadas
!    Xkey$ = Right$("0000"+Gr.Mkp.Rifa$(XI%,1),4)   													! Ajusta dato
!    Xkey$ = Xkey$ + Right$("000000000000"+SL.IT.ITEMCODE$,12)								!
!    Xkey$ = Pack$(Xkey$)																										!
!    TS.ER.RETURN = -1 																											!
!    Read Form "C8";#59 KEY Xkey$;Xbasura$     															! Lectura del registro
!    If TS.ER.RETURN = -1 Then Begin 		      															! Lo encontro
!       Gr.Mkp.Rifa$(XI%,2) = Str$(Val(Gr.Mkp.Rifa$(XI%,2)) +               \!
!                             (SL.IT.XPRICE * Xsgn%)) 												! Suma la compra
!    EndIf  																																	!
! Next XI% 																																	!
!End Sub 																																		!

!-- Nuevo modulo validacion rifas 13 Ago 2012
Sub Valida.Rifa Public 
  Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J% , OVS% 												  !
  Integer*1 Xsgn%
  String    X.TMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, XInd3$         !

  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico indicat
        Xind3$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 3
        Xsgn% = 1 																													! Trx de compra
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado
        !If Left$(Xind3$,1) = "0" Then                                      ! Solo venta de items	
        	
         For XI% = 1 to Gr.Mkp.CntRif%			                                ! Valida Total de Rifas Cargadas
          Xkey$ = Right$("0000"+Gr.Mkp.Rifa$(XI%,1),4)                      ! Ajusta dato
          Xkey$ = Xkey$ + Right$("000000000000"+Xitm$,12)                   ! Analizando Item
          Xkey$ = Pack$(Xkey$)                                              !
          TS.ER.RETURN = -1 																								! Ctrl Errores
          Read Form "C8";#59 KEY Xkey$;Xbasura$     				                ! Lectura del registro
          If TS.ER.RETURN = -1 Then Begin 		      				                ! Lo encontro
             Gr.Mkp.Rifa$(XI%,2) = Str$(Val(Gr.Mkp.Rifa$(XI%,2)) +         \!
                                   (Val(Xpric$) * Xsgn%))                   ! Suma la compra

          EndIf																														  !
         Next XI% 																													! Fin analisis rifas
     EndIf                                  																! ITEM ENTRY String
  NEXT X.J%   

End Sub 

!-- Nuevo modulo validacion rifas 13 Ago 2012
Sub Valida.Rifas.Totales Public 
  Integer*4 PRIC%, TOT4%, XI%, XInd2%, X.J% , OVS% 												  !
  Integer*1 Xsgn%
  String    X.TMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, XInd3$         !

        XItm$   = "000000999900"	  												                ! Item Comodin
        Xpric$  = Str$( TS.TOTALS(0,0,0) )                                  ! Total de la transaccion
        	
         For XI% = 1 to Gr.Mkp.CntRif%			                                ! Valida Total de Rifas Cargadas
          Xkey$ = Right$("0000"+Gr.Mkp.Rifa$(XI%,1),4)                      ! Ajusta dato
          Xkey$ = Xkey$ + Right$("000000000000"+Xitm$,12)                   ! Analizando Item
          Xkey$ = Pack$(Xkey$)                                              !
          TS.ER.RETURN = -1 																								! Ctrl Errores
          Read Form "C8";#59 KEY Xkey$;Xbasura$     				                ! Lectura del registro
          If TS.ER.RETURN = -1 Then Begin 		      				                ! Lo encontro
             Gr.Mkp.Rifa$(XI%,2) = Str$(Val(Gr.Mkp.Rifa$(XI%,2)) +         \!
                                   (Val(Xpric$) ))                          ! Suma la compra

          EndIf																														  !
         Next XI% 																													! Fin analisis rifas

End Sub 



Function PROMODEPTO(X.DPTO$,XLINK$) Public    ! Modulo Descuento por seccion 
String X.DPTO$, XC$, XLEC$, \
       TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
       UE.DIA$(1), UE.HORA$(2), UE.HORA.ACT$, XLINK$, PROMODEPTO, X.Sgn$, \
       X.Find$, X.Lst$, X.Rta$, X.Seek$, Xpromo$, Xtmp$
Integer*2 X.Dia%, X.Cnt%
Integer*4 X.Vlrd%, X.Uqty%, X.K%, X.G%, Xd1%, Xd2%

  PROMODEPTO = Pack$("0000")
  X.Rta$ = Pack$("0000")
  ASC.MKP.DSCTO% = 0 
  Xd1% = 0 : Xd2% = 0
  Gr.Mkp.DstSec% = 00
  Dim UE.DIA$(10)
  Dim UE.HORA$(10,2)
  
!  If Val(X.Dpto$) = 0 Then Begin                    ! No tiene estructura definida
!  	 PROMODEPTO = Pack$("0000")        					    ! Dia no activo de la promocion
!  	 Exit Function 
!  EndIf 

  X.Find$ = ""
  
For X.G% = 1 TO LEN(ASC.MKP.CADENA$) STEP 4			    ! Hasta longitud cadena
    XTmp$ = Mid$(ASC.MKP.CADENA$,X.G%,4)			      ! Extrae lista promociones
    X.Find$ = ""
    X.Cnt% = 0
  For X.Cnt% = 2 To 16 Step 3												! Analiza Departamento
      X.Lst$ = Mid$(X.Dpto$,X.Cnt%,3)               ! Toma estructura inicial
      X.Find$ = X.Find$ + X.Lst$                    ! Arma esFtructura comercial 
      X.Seek$ = Pack$("1"+Left$(X.Find$ + "000000000000000",15)) ! Analiza Estructura
      X.Seek$ = X.Seek$ + (Ucase$(XTmp$))           ! Nueva llave de busqueda
      TS.ER.RETURN = -1 																											! Control de errores
      XLEC$ = "C12 C4 C3 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C2"  !
      Read Form XLEC$; #22 KEY X.Seek$;                                       \! LECTURA SECCION
       TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
       UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \
       UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \
       UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \
       UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \
       UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \
       UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \
       UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2), \
       Xpromo$		
       If TS.ER.RETURN = -1 Then Begin 																				 ! Si tiene descuento
          X.Dia% = DIA.SEMANA("20"+DATE$)																		   ! Retorna el dia de la semana 
          If UE.DIA$(X.Dia%) = "1" Then Begin 																 ! Si Dia activo para promocion
     	       UE.HORA.ACT$ = LEFT$(TIME$,4)																		 ! Toma hora del sistema HHMM
	           If Val(UE.HORA$(X.Dia%,1)) <= VAL(UE.HORA.ACT$) And	\            ! Si se encuentra entre el rango
	              Val(UE.HORA$(X.Dia%,2)) >= VAL(UE.HORA.ACT$) Then Begin        ! de horas parametrizado
                If UnPack$(IR.INDICAT2$) = "05" Then Begin              			 ! Metodo de precio 5 
                   TS.TEMP2$ = IR.SALEQUAN$ + IR.SALEPRIC$             			   ! Busca articulo ligado                	
                   Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)             ! Lectura Datos Itemr
                   Call SPLIT.ITEMFILE( TS.TEMP1$ ) 			                     ! Entrega datos al eamitemr.j86
                   IR.SALEPRIC$ = GR.SALEPRIC$                                 ! Entrega precio del producto
                EndIf 
                IR.USEREXIT1 = 0 																							 !
                IR.USEREXIT2 = 0 																							 !
                X.Rta$ = Pack$("0000")                                         ! Dscto x seccion 
                If TS.IO.DATA$(6) = "" Then X.Uqty% = 1 Else                  \! Toma cantidad vendida
                                            X.Uqty% = Val(TS.IO.DATA$(6))      !
                X.Sgn$ = "00"                                                  ! Default Trx positiva                            
                If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then          \! Si es una devolucion / anulacion
           	        X.Sgn$ = "01"                                              ! Trx Negativa
           	        
           	    If Val(DM.VLRCON$) > Xd1% Then Begin 													 ! Si dscto leido es Mayor al anterior
           	    	 Xd1% = Val(DM.VLRCON$)																			 ! Asigna nuevo descuento
                   ASC.MKP.DSCTO% = Val(DM.VLRCON$)    									  		 ! Ptg Dscto a Aplicar

                   !If GR.ESCL.ACT% <> -1 Then Begin   												 ! Si proyecto apagadao  
                   	                 
                   If Gr.Escl.ItmVl% = 0 Then Begin                            ! No es dscto escalonado
                    IR.PRICE1 = Val(unpack$(IR.SALEPRIC$))                     ! Precio de venta
                    IR.PRICE2 = IR.PRICE1														  				 !
                   EndIf 
                   
                   Gr.Mkp.Promo$  = UnPack$(Xpromo$)                           ! Codigo de la promocion
                   Gr.Mkp.DstSec% = -1																				 !
                   Gr.Mkp.Sgn$ = X.Sgn$																				 !
           	    EndIf
             EndIf
          EndIf
       EndIf       
  Next X.Cnt%
Next X.G%

  GR.MKT.DSEC$ = X.Rta$																							! Retorna proceso 
  PROMODEPTO   = X.Rta$																							! Retorna proceso 
  
End Function 
!--- Fin promocion por departamento


Sub IMPRESION.RIFA Public 
Integer*4 XI%, Xh%, XG%
Real Xvlr%
String Xkey$, Xlec$, Xcod$, Xname$, Xfec1$, Xfec2$, \
       Xvlr$, Xbol$, Xcon$, Xmsg$, XVal$, XTmp$, Xnulo$
Integer*1 GrCab%
 GrCab% = 0
 TS.ER.RETURN = -1 
 Open "R::$F:P0004" KEYED RECL 40 AS Gr.Mkp.SesTmp%  UNLOCKED NOWRITE NODEL   ! Validacion de rifas por producto
 If TS.ER.RETURN <> -1 Then Begin 
 	  Exit Sub                        												! Error de apertura archivo
 EndIf
 XLEC$ = "C4 C18 2C4 C5 C2 C1 C2"
 Xval$ = "20" + Date$                                       ! Fecha de validacion 
 Xnulo$ = Pack$("00000000")
 For XG% = 1 To Len(ASC.MKP.CADENA$) STEP 4			    			  ! Hasta longitud cadena
    XTmp$ = Mid$(ASC.MKP.CADENA$,XG%,4)			      			  	! Extrae lista promociones
    Xtmp$ = Right$("0000"+Xtmp$,4)
    XTmp$ = Pack$(Right$("0000"+XTmp$,4))
 For XI% = 1 to Gr.Mkp.CntRif%		            	            ! Valida Total de Rifas Cargadas
   Xkey$ = Pack$(Right$("0000"+Gr.Mkp.Rifa$(XI%,1),4)) + Xtmp$      ! Ajusta dato
   
   If Xkey$ = Xnulo$ Then Begin 
      GoTo NXT.RIFA                                          ! Analiza siguiente rifa
   EndIf	
   TS.ER.RETURN = -1
   If Gr.Mkp.Rifa$(XI%,1) <> "00000000"  Then Begin          ! Evita Error de llave
    Read Form Xlec$;#Gr.Mkp.SesTmp% KEY Xkey$;              \! Consulta registro
    Xcod$, Xname$, Xfec1$, Xfec2$,                          \!
    Xvlr$, Xbol$, Xcon$, Xmsg$
   EndIf Else TS.ER.RETURN = 0 
   If TS.ER.RETURN = -1 Then Begin                          ! Si lo encontro
      Xfec1$ = UnPack$(Xfec1$)
      Xfec2$ = UnPack$(Xfec2$)
      Xvlr$  = UnPack$(Xvlr$)
      Xmsg$  = UnPack$(Xmsg$)
      Xbol$  = UnPack$(Xbol$)
      Xcon$  = UnPack$(Xcon$)
        If Val(Xvlr$) <> 0 Then \
         If Val(Gr.Mkp.Rifa$(XI%,2)) >= Val(Xvlr$) Then Begin  ! Si compra superior
            XVlr% = Int(Val(Gr.Mkp.Rifa$(XI%,2)) / Val(Xvlr$)) ! Calcula numero veces cumple condicion
            Xvlr% = Xvlr% * Val(Xbol$)                         ! Numero Boletas a entregar
            Gr.Mkp.Vlr% = Val(Gr.Mkp.Rifa$(XI%,2))             ! Valor Entregado
            Gr.Mkp.Bol% = XVlr%                                ! Numero de Boletas
            Call U.IMPRIME("GENERA "+STR$(Gr.Mkp.Bol%)+" BOLETAS "+Unpack$(Xcod$),2100H)
            XH% = 0                                           ! Inicializa variable
            For XH% = 1 To Gr.Mkp.Bol%   											! Genera N boletas obtenidas
              Call Imprime.Msg.Publicitario(Xmsg$)						! Imprime la boleta
              GrCab% = -1
            Next Xh%																					!
         EndIf
      Gr.Mkp.Rifa$(XI%,2) = "0"
   EndIf 
   NXT.RIFA:
 Next XI%
Next XG%
XI% = 0 
For XI% = 1 to Gr.Mkp.CntRif%		            	            ! Valida Total de Rifas Cargadas
    Gr.Mkp.Rifa$(XI%,2) = "0"
Next XI%
Close Gr.Mkp.SesTmp%
If GrCab% = -1 Then Begin 
   Call U.Imprime(TO.HEADERLINE1$,4100H)														      !
   Call U.Imprime(TO.HEADERLINE2$,4100H)													      	!
EndIf
End Sub 

Sub Mensaje.Tipificacion																											! Impresion de mensajes a grupos o tipos de clientes
String UE.BCLIENTE$, X.DATA$			    																				! Definicion de
String UE.B.CLTE$, UE.B.LOGO$, UE.B.LISTA$, UE.B.TMP$   											! variables tmp
Integer*4 X.G%																																!
For X.G% = 1 TO LEN(ASC.MKP.LSTPRO$) STEP 4			    													! Hasta longitud cadena
    UE.BCLIENTE$ = Mid$(ASC.MKP.LSTPRO$,X.G%,4)			      										! Extrae lista promociones
    UE.BCLIENTE$ = Right$("0000000000000000"+UE.BCLIENTE$,16)   							! Ajusta dato del cliente
    UE.BCLIENTE$ = PACK$("37"+UE.BCLIENTE$)	                									! dato del cliente
    TS.ER.RETURN = -1						                            									! Ctrl errores
    READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY UE.BCLIENTE$;                \! Lee registro
       UE.B.CLTE$, UE.B.LOGO$, X.DATA$, UE.B.LISTA$     											!
    If TS.ER.RETURN = -1 Then Begin			 	                  									! Si found
    	 Gr.Promo.Compra% = TS.TOTALS(0,0,0)
    	 Call GRABA.POSICION.MSG(UNPACK$(UE.B.LOGO$),1)     										! Msg al grupo o tipo de cliente
    EndIf																																			!
Next X.G%																																			! Fin analisis ciclo
End Sub 

Sub Val.Promo.TotalTrx
Integer*4 X.G%
String    Xclte$, Xa1$, Xa2$, Xa3$, Xa4$
TS.TEMP1I4 = 0 																																! Inicializa dscto
For X.G% = 1 TO LEN(ASC.MKP.LSTPRO$) STEP 4			    													! Hasta longitud cadena
    Xclte$ = Mid$(ASC.MKP.LSTPRO$,X.G%,4)			      										      ! Extrae lista promociones
    Xclte$ = Right$("0000000"+Xclte$,7)   							                      ! Ajusta dato del cliente
    Xclte$ = ("45"+Xclte$)	                									                ! dato del cliente
    TS.ER.RETURN = -1						                            									! Ctrl errores
    READ FORM "C9 C2 C10 C45";#ASC.MKP.SE1% KEY Xclte$;                      \! Lee registro
       Xa1$, Xa2$, Xa3$, Xa4$
    If TS.ER.RETURN = -1 Then Begin			 	                  									! Si found
     If TS.TOTALS(0,0,0) >= Val(Xa3$) Then                                   \! Si cumple compra minima 
      If TS.TEMP1I4 < Val(Unpack$(Xa2$)) Then Begin                           ! Busca el mejor dscto aplicar
        TS.TEMP1I4 = Val(Unpack$(Xa2$))                                       ! Retorna dscto aplicar
      EndIf
    EndIf 
Next X.G%																																			! Fin analisis ciclo

End Sub 

Sub Dscto.Ttal.Trx 
Integer*4 Gr.Xdscto.Pago%
  If (TS.IO.KEYS(1) <> 70) Then Begin 
  	If Gr.Promo.Dttrx% <> 0 Then Exit Sub                                                 ! Dscto ya Aplicado
  	Call Val.Promo.TotalTrx
  	If TS.TEMP1I4 = 0 Then Exit Sub 
    Gr.Xdscto.Pago% = (TS.TOTALS(0,0,0) * TS.TEMP1I4) /100                                ! Aplica % de descuento a la trx
    SL.END = SL.END + 1                                                                   ! Aumenta apuntador de String 
    SL.STR$(SL.END) = Pack$("03")+":"+Pack$(Gr.Mkp.DctoPago$) + ":" +	\                   ! Almacena String de descuento
	    	              Pack$("00")+ ":" + Pack$(Str$(Gr.Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
    TS.BALDUE(0)      = TS.BALDUE(0) - Gr.Xdscto.Pago%                                    ! Actualiza variables de la 
    SL.HD.GROSSNEG    = SL.HD.GROSSNEG + Gr.Xdscto.Pago%                                  ! Aplicacion y de total de la 
    TS.GROSSNEG       = TS.GROSSNEG + Gr.Xdscto.Pago%                                     ! Transaccion eliminando el descuento
    TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) + Gr.Xdscto.Pago%                                !
    TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + Gr.Xdscto.Pago%                               !
    TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) - Gr.Xdscto.Pago%                                !
    Call Format.Amount(Gr.Xdscto.Pago%)
    Call VISOR.AND.BORRAR("DESCUENTO EN COMPRA:"+"$"+TS.TEMP1$)    												! Presenta informacion
    Gr.Mkp.DctoPago% = Gr.Xdscto.Pago%
    TS.TEMP1$ = ""
    ASC.MKP.TMP1$  = Pack$(Gr.Mkp.Promo$)                      +                          \! Codigo de la promocion  
                      ":"+PACK$("00")                          +	                        \! Item vendido            
                      ":"+PACK$("00")                          +	                        \! Qty vendido             
                      ":"+PACK$("00")                          +	                        \! Precio Vendido          
                      ":"+PACK$("00")                          +	                        \! Dscto otorgado          
                      ":"+Pack$("00")                          +                          \! Signo de la operacion   
                      ":"+Pack$("00")                          +                          \! Forma pago promocion    
                      ":"+Pack$(Str$(Gr.Xdscto.Pago%))         +                          \! Dscto forma de pago    
                      ":"+Pack$("04")                                                      ! Tipo Dscto - Total Compra
    Call Grabacion.Cadena.Usuario2("68",ASC.MKP.TMP1$)                                     ! Almacena user data      

    Call Format.Amount(Gr.Mkp.DctoPago%)                                                   ! Formatea valor
    TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                                          ! Ajuste dato valor
    TS.TEMP2$ = "Dscto x Compra       :$" + TS.TEMP1$                                      ! Arma mensaje a tiquete
    Call U.IMPRIME(TS.TEMP2$,6101H)                                                        ! Imprime vlr dscto
    Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)																							 ! Inicializa secuencia teclado
    TS.IO.MOTORKEY = 0
    TS.GUIDANCE = 1020																																		 ! Msg totalice 
    Gr.Promo.Dttrx% = -1
   EndIf Else Begin
   	If Gr.Promo.Dttrx% <> -1 Then Exit Sub 
    Call Val.Promo.TotalTrx
    If TS.TEMP1I4 = 0 Then Exit Sub    		
    SL.END = SL.END + 1                                                                   ! Aumenta apuntador de String 
    SL.STR$(SL.END) = Pack$("04")+":"+Pack$(Gr.Mkp.DctoPago$) + ":" +	\                   ! Almacena String de descuento
	    	              Pack$("00")+ ":" + Pack$(Str$(Gr.Xdscto.Pago%))+":"+Pack$("00") + ":" ! 
    TS.BALDUE(0)      = TS.BALDUE(0) + Gr.Xdscto.Pago%                                    ! Actualiza variables de la 
    SL.HD.GROSSNEG    = SL.HD.GROSSNEG - Gr.Xdscto.Pago%                                  ! Aplicacion y de total de la 
    TS.GROSSNEG       = TS.GROSSNEG - Gr.Xdscto.Pago%                                     ! Transaccion eliminando el descuento
    TS.TOTALS(0,0,1)  = TS.TOTALS(0,0,1) - Gr.Xdscto.Pago%                                !
    TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - Gr.Xdscto.Pago%                               !
    TS.TOTALS(0,0,0)  = TS.TOTALS(0,0,0) + Gr.Xdscto.Pago%                                !
    Call Format.Amount(Gr.Xdscto.Pago% * -1)
    TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)
    Call VISOR.AND.BORRAR("ANL DCTO COMPRA:"+TS.TEMP1$)            												! Presenta informacion
    Gr.Mkp.DctoPago% = Gr.Xdscto.Pago%
    TS.TEMP1$ = ""
    ASC.MKP.TMP1$  = Pack$(Gr.Mkp.Promo$)                      +                          \! Codigo de la promocion  
                      ":"+PACK$("00")                          +	                        \! Item vendido            
                      ":"+PACK$("00")                          +	                        \! Qty vendido             
                      ":"+PACK$("00")                          +	                        \! Precio Vendido          
                      ":"+PACK$("00")                          +	                        \! Dscto otorgado          
                      ":"+Pack$("01")                          +                          \! Signo de la operacion   
                      ":"+Pack$("00")                          +                          \! Forma pago promocion    
                      ":"+Pack$(Str$(Gr.Xdscto.Pago%))         +                          \! Dscto forma de pago
                      ":"+Pack$("04")                                                      ! Tipo Promocion - Dscto total trx
    Call Grabacion.Cadena.Usuario2("68",ASC.MKP.TMP1$)                                     ! Almacena user data      
    Call Format.Amount(Gr.Mkp.DctoPago% * -1)                                              ! Formatea valor
    TS.TEMP1$ = Right$("          "+TS.TEMP1$,10)                                          ! Ajuste dato valor
    TS.TEMP2$ = "Anul Dscto x Compra  :$" + TS.TEMP1$                                      ! Arma mensaje a tiquete
    Call U.IMPRIME(TS.TEMP2$,6101H)                                                        ! Imprime vlr dscto
    Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)																							 ! Inicializa secuencia teclado
    TS.IO.MOTORKEY = 0																																		 
    TS.GUIDANCE = 1020																																		 ! Msg totalice 
    Gr.Promo.Dttrx% = 0
   EndIf
End Sub 

Sub PROMO.AB                																					      ! Promocion A-B
Integer*4 II%, Xqty%, PROMO.AB
Integer*1 Xfnd%

  Exit Sub 

  Xfnd% = 0
  For II% = 1 To GrPab%                                                     ! Analiza Venta trx
     If Val(SL.IT.ITEMCODE$) = VTAB%(II%,1) Then Begin											! Si venta Item
        IF (SL.IT.INDICAT3A = 8) OR (SL.IT.INDICAT3A = 9) THEN             \! Es una anulacion
     	    VTAB%(II%,2) = VTAB%(II%,2) - SL.IE.QTYORWGT Else                \! Resta Cantidad
          VTAB%(II%,2) = VTAB%(II%,2) + SL.IE.QTYORWGT                      ! Suma  Cantidad
          Xfnd% = -1                                                        ! Item Actualizado
     EndIf 																																  ! Fin Validación Venta
  Next II%                                                                  ! Fin Items vendidos
  
  If Xfnd% = 0 Then Begin 																									! Item Nuevo en venta
   	 GrPab% = GrPab% + 1																									  ! Numero Item Vendidos
   	 VTAB%(GrPab%,1) = Val(SL.IT.ITEMCODE$)                                 ! Codigo Item Vendido
   	 IF (SL.IT.INDICAT3A = 8) OR (SL.IT.INDICAT3A = 9) THEN                \! Es una anulacion
     	    VTAB%(GrPab%,2) = 0 - SL.IE.QTYORWGT  Else                       \! Cantidad Vendida
     	    VTAB%(GrPab%,2) =  SL.IE.QTYORWGT                                 ! Cantidad Vendida
  EndIf																																			!

End Sub 

!--- Calculo Total Dsctos Otorgados 
Sub TOT.DSCTOS.MOTOR
Integer*4 PRIC%, TOT4%, X.I%																							!
String    X.TMP$, Xpric$, Xsgn$
TS.TEMP5I4 = 0                                                            ! Init Variable total dsctos
For X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 															! GET String
    If LEN(H$) > 5 Then                                                  \! ASSURE GOOD String
     If ASC(H$)  = 153 Then Begin             														! ITEM ENTRY String
        Asc.Tmp.Apun% = 3																									! Asigna apuntador
        X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 												  ! Verifica el numero de proyecto
        If X.TMP$ = "68" Then Begin                                     	! Si Dsctos Motor Promociones
        	 X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Codigo de la promocion
        	 X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Codigo del Item
        	 X.TMP$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Cantidad vendida
        	 X.Tmp$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Precio Item
        	 XPric$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Dscto otorgado
           XSgn$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	 										  ! Signo de la operacion 00.Vta 01.Anul
           Xpric$ = TRANSLATE$(XPric$,"=","-")                            ! Para Evitar un IH
           If XSgn$ = "00" Then PRIC% = Val(Xpric$) Else                 \! Retorna el valor          
           	  PRIC% = 0 - Val(Xpric$)                                     ! del descuento aplicado
           TS.TEMP5I4 = TS.TEMP5I4 + Pric%                                ! Acumula el total del dscto 
     	  EndIf 
     EndIf                                 																! ITEM ENTRY String
NEXT X.I% 
End  Sub 

!--- 
!--- Definicion de Subrutinas
!---

Sub MKTPTE01.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS02.011                                         **
!   **    Incluir en :  EAMTSE01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!If Ue.mktp.Line% Then Begin                                          
   
  %INCLUDE MKTTSE01.011

!EndIf

END Sub

Sub MKTPTS01.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS02.011                                         **
!   **    Incluir en :  EAMTSU01.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin                                          

  %INCLUDE MKTTSU01.011					                 

EndIf
END Sub

Sub MKTPTS02.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin                                          

  %INCLUDE MKTTSU02.011					                 
  Call DCTOXITM(02)       ! Dscto Happy Hour Item
  
EndIf

END Sub

Sub MKTPTS07.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS07.011                                         **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

%INCLUDE MKTTSU07.011														! Carga parametros
Call DCTOXITM(07)       ! Dscto Happy Hour Item

END Sub

Sub MKTPTS08.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin                                          

    %INCLUDE MKTTSU08.011

EndIf

END Sub

Sub MKTPTS14.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS14.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU14.011														! Captura Empleado

EndIf

END Sub


Sub MKTPTS20.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU20.011														! Captura Empleado

EndIf

END Sub

Sub MKTPTS43.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS43.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
Real Gr.Tmp%
String XSgn$

If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU43.011														! Captura Empleado
  Call DCTOXITM(43)       ! Dscto Happy Hour Item
  
EndIf
END Sub

Sub MKTPTS53.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS53.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Alto$, Bajo$, Signo$
Integer*4 XINDICAT2, XINDICAT3
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU53.011														! Captura Empleado

EndIf

END Sub

Sub MKTPTS56.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS56.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU56.011														! Captura Empleado
  Call DCTOXITM(56)       ! Dscto Happy Hour Item
  
EndIf

END Sub

Sub MKTPTS60.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS60.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Ue.Tmp.Sgn$
Integer*4 Ue.Tmp.Vlr%
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

  %INCLUDE MKTTSU60.011													
  Call DCTOXITM(60)       ! Dscto Happy Hour Item
  
EndIf

END Sub

Sub MKTPTS68.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS68.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
If Ue.mktp.Line% Then Begin  				                        ! Proyecto activo

!  %INCLUDE MKTTSU68.011													

Endif 
END Sub

Sub MKTPTS27.011 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Junio de 2002                                        **
!   **    Proyecto   :  GRUPO RETAIL LTDA                                    **
!   **    User Exit  :  MKTPTS27.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If ERR = "DI" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina Error
    Call visores4690(1,"ERRO DISP",STR$(DVICE),1200,"L")
    Resume RETRY
EndIf



If ERR = "AE" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina error
EndIf

If ERR = "AC" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina error
EndIf

If ERR = "SU" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina error
EndIf

If ERR = "SS" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina error
EndIf

If ERR = "DI" Then Begin
   PPP.PPP% = ADXERROR( SL.HD.TERMINAL,    \ Number of terminal
                        ASC("X"),          \ msg displayed
                             001,          \ msg number
                               5,          \ severity level
                               2,          \ 2 = checkout support
                        ASC.MKP.SITIO$)     ! Mensaje rutina Error
    Call visores4690(1,"ERRO DISP",STR$(DVICE),1200,"L")
EndIf


End Sub
