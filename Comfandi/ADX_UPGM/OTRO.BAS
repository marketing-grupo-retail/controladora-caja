!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PROMOLIN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Abril del 2004                   *
!Observaciones : PromoOnline                      *
!**************************************************
!
! Rutinas Definidas
!

%ENVIRON T

String    GLOBAL ASC.MKP.SITIO$, XERRFX$ 
Integer*4 GLOBAL PPP.PPP%
Integer*1 GLOBAL UE.TRX.ANUL%

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

Sub TRADUCE.ERROR                                       !
String XZ$
Integer*4 XS%, XSUM%, XHX%, XSX%
    XHX% = ERRN                                               ! Traduccion de los
    XERRFX$=""                                                ! codigos de error
    FOR XS% = 28 TO 0 STEP -4                                 ! del sistema operativo
        XSX% = SHIFT(XHX%,XS%)                                  !
        XSUM% = XSX% AND 000FH                                 !
        IF XSUM% > 9 THEN XSUM% = XSUM% + 55                   \!
        ELSE XSUM% = XSUM% + 48                                !
        XZ$ = CHR$(XSUM%)                                      !
        XERRFX$ = XERRFX$ + XZ$                                 !
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
  ENDIF                                          ! if not valid length

  YEAR = INT%(VAL(LEFT$(FN.DATE$,4)))
  MONTH = INT%(VAL(MID$(FN.DATE$,5,2)))
  DAY = INT%(VAL(RIGHT$(FN.DATE$,2)))

  IF MONTH <= 2 THEN BEGIN
    ADJMONTH = 10 + MONTH
    ADJYEAR = YEAR - 1
  ENDIF ELSE BEGIN
    ADJMONTH = MONTH - 2
    ADJYEAR = YEAR
  ENDIF

  MONTHCORRECTION = (26 * ADJMONTH - 2) / 10

  CENTURY = ADJYEAR / 100
  LSTTWO = MOD(ADJYEAR,100)
  YEARCORRECTION = LSTTWO + (LSTTWO / 4) + (CENTURY / 4) + 5 * CENTURY

  DIA.SEMANA = MOD((DAY + MONTHCORRECTION + YEARCORRECTION),7)

End Function
!-- Fin rutina dia de la semana

Sub PRMFPAGO(XOPT%) External																								! Modulo recaudo Propinas
Integer*4 XOPT%         																										! Variables 
End Sub 

Function MKT.Detalle Public                                   ! Imprime Detalle De La Operacion
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
   X.B$ = X.B$ + X.C$ + X.D$
   X.B$ = Left$(X.B$+String$(37," "),37)
   X.A$ = X.B$
EndIf    
Mkt.String = X.A$

End Function
!--- Fin funcion data String

!-- Carga Modulo de Rifas
Sub Carga.Rifas Public 
Integer*4 XI%, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC, XJ%, R.S
String    XH$, PZERO$, KEY$
 Dim Gr.Mkp.Rifa$(300,2)			                   ! Hasta 300 rifas simultaneas
 Gr.Mkp.CntRif% = 0 
 TS.ER.RETURN = -1 
 Open "R::TF:P0004" DIRECT RECL 512 AS Gr.Mkp.SesTmp%
 If TS.ER.RETURN <> -1 Then Exit Sub 
 Read Form "T43 I4 I2 T55 I2 C456"; #Gr.Mkp.SesTmp%,1;     \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, XH$	        ! 
 PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))         ! Armed Key Control
 If REC.LEN = 0 Then BEGIN 
 	  CLOSE Gr.Mkp.SesTmp%
 	  Exit Sub 
 ENDIF 
 MAX.R.SEC = 508/REC.LEN                        ! Length record
 For XI% = 2 TO BLK.NUM                         ! Cicle to read all blocks  
  Read Form "T5 C508"; #Gr.Mkp.SesTmp%, XI%;  XH$           ! XH$ contains block 
  XJ% = 1 : KEY$ = MID$(XH$,XJ%,KEY.LEN)        ! Extract First key
  R.S = 0
  While KEY$  NE  PZERO$                        ! Inside sector loop 
    R.S = R.S + 1                               ! Records On This Sector 
    Gr.Mkp.CntRif% = Gr.Mkp.CntRif% + 1         ! Total ctrl rifas
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,1) = Unpack$(Mid$(XH$,XJ%+0,2)) ! Numero Rifa
    Gr.Mkp.Rifa$(Gr.Mkp.CntRif%,2) = "0"        ! Control de venta
    XJ% = XJ% + REC.LEN                         ! Index to next key
    KEY$ = MID$(XH$,XJ%,KEY.LEN)                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$       ! If EOF() record or file
  Wend 
 Next XI%
 CLOSE Gr.Mkp.SesTmp% 
End Sub 

Sub Carga.Parametros.Mrktplus Public								      		              ! Parametros modulo
String X.H$, X.AUX$, X.MSG$, X.PROMO$, X.DATA$, X.CUPON$						        ! Variables temporales
Integer*2 X.CNTI%, X.CNTJ%	
 If ASC.MKP.OPEN% = 0 Then Begin																						! Si no abierto
    TS.ER.RETURN = -1																												!
    !OPEN "R::ADX_IDT1:MKPDEFPR.DAT" KEYED RECL 29 AS ASC.MKP.SE1%						! Open File
    
    
    OPEN "R::TF:DEFPR" KEYED RECL 29 AS ASC.MKP.SE1%						! Open File
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
 ASC.MKP.TMP1$ = "000000000001"																							! Promo compra minima
 ASC.MKP.TMP1$ = PACK$("33"+ASC.MKP.TMP1$)																	! Arma llave de busqueda
 TS.ER.RETURN = -1																													! Ctrl errores
 READ FORM "C7 C2 2C10";#ASC.MKP.SE1% KEY ASC.MKP.TMP1$;                   \! Lee registro
           UE.MCD.COD$, UE.MCD.LOGO$, ASC.MKP.TMP2$, X.DATA$								!

 If TS.ER.RETURN = -1 Then Begin										          							! Lo encontro
 	  !Call VISORES4690(1,"COMPRA MINIMA","PARAMETRIZADA",1200,"L")
 	  
    ASC.MKP.MCM$ = UNPACK$(UE.MCD.LOGO$)	  	        											! Mensaje Publicitario
    ASC.MKP.CPM% = VAL(ASC.MKP.TMP2$)									        							! Monto compra minima
    ASC.MKP.CCM$ = LEFT$(X.DATA$,4)								           								! control clientes
 EndIf Else Begin																														!
!   Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
!        STR$(TS.ERRF),4100H)

    ASC.MKP.MCM$ = "0000" 													              					! Mensaje Publicitario
    ASC.MKP.CPM% = 0      																									! Monto compra minima
    ASC.MKP.CCM$ = "XXXX"																										! control de clientes
 EndIf																																			!
 !Call U.IMPRIME("MSG   "+ASC.MKP.MCM$,4100H)
 !Call U.IMPRIME("MONTO "+Str$(ASC.MKP.CPM%),4100H)
 !Call U.IMPRIME("LISTA "+ASC.MKP.CCM$,4100H)
 ASC.MKP.SEC% = 0 
 TS.ER.RETURN = -1																													!
 Open "R::MKTP:MKPTMP1.DAT" AS Gr.Mkp.SesTmp%				                                   	! Open promociones seccion
 If TS.ER.RETURN <> -1 Then Begin 
 	  Call U.Imprime("Error Apertura MKPTMP1  ",4100H)		
    Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
         STR$(TS.ERRF),4100H)
 	  GoTo NEXT.CARGA  			                        	! No hay promocion de secciones
 EndIf 
 If END #Gr.Mkp.SesTmp% Then FIN.CARGA.MKTP  				                                  	! 
 While (1)							                                                  	! Recorre file
  READ #Gr.Mkp.SesTmp%;LINE X.H$						                                          	!
  X.AUX$   = MID$(X.H$,1,14)            			                            	! Extrae la llave
  X.MSG$   = MID$(X.H$,15,4)	        			                              	! Codigo de mensaje
  X.PROMO$ = MID$(X.H$,19,10)					                                      ! Promocion
  
  If LEFT$(X.AUX$,2) = "32" Then Begin		  	                          		! Promocion de NO PRESENCIA secciones
       ASC.MKP.SEC% = ASC.MKP.SEC% + 1			  	                            ! Aumenta contador
       ASC.MKP.TMP1$ = RIGHT$(X.AUX$,4)			  	                          	! Tomo los ultimos 4 digitos
       UE.MCD.SEC$(ASC.MKP.SEC%,0) = ASC.MKP.TMP1$	  	                    ! Seccion a promocionar
       UE.MCD.SEC$(ASC.MKP.SEC%,1) = X.MSG$                	              	! Numero mensaje
       UE.MCD.SEC$(ASC.MKP.SEC%,2) = "0"			  	                          ! Contador de venta
       UE.MCD.SEC$(ASC.MKP.SEC%,3) = Left$(X.PROMO$,6)	  	              	! Monto minimo de compra
       UE.MCD.SEC$(ASC.MKP.SEC%,4) = Right$(X.PROMO$,4)	  	                ! Control clientes 
       !Call U.IMPRIME("COMPARA" + ASC.MKP.TMP1$,4100H)
       !Call U.IMPRIME("MINIMO " + Left$(X.PROMO$,6),4100H)
       !Call U.IMPRIME("LISTA  " + Right$(X.PROMO$,4),4100H)
  EndIf																																			!
 Wend																																				!
 FIN.CARGA.MKTP:																														!
  CLOSE Gr.Mkp.SesTmp%																																	!
 NEXT.CARGA:																																!
 TS.ER.RETURN = -1																													!
 OPEN "R::MKTP:MKPTMP2.DAT" AS Gr.Mkp.SesTmp%				                                   	! Open promociones no presencia
 If TS.ER.RETURN <> -1 Then Exit Sub       			                          	! No hay promocion
 If END #Gr.Mkp.SesTmp% Then FIN.CARGA.NOART 				                                  	! 
 X.CNTI% = 1: X.CNTJ% = 0																										!
 X.AUX$ = ""																																!
 While (1)							                                                  	! Recorre file
  READ #Gr.Mkp.SesTmp%;LINE X.H$						                                          	! 
  X.AUX$   = X.AUX$+PACK$(MID$(X.H$,1,24))+";"		                        	! Extrae la llave
  UE.MCD.ART$(X.CNTJ%) = X.AUX$																							!
  X.CNTI% = X.CNTI% + 1																											!
  If X.CNTI% > 13 Then Begin  																							! Mas de la cadena permitida
     X.CNTI% = 1              																							! Init control de String
     X.AUX$  = ""	      																										! Limpia cadena
     X.CNTJ% = X.CNTJ% + 1    																							! Aumenta posicion vector
     If X.CNTJ% >= 500 Then Begin 																					!
        GOTO FIN.CARGA.NOART																								!
     EndIf																																	!
  EndIf																																			!
 Wend																																				!
 FIN.CARGA.NOART:																														!
  CLOSE Gr.Mkp.SesTmp%
  Call U.Imprime("Fin Carga Market Online ",6100H)													! Rastro en SJ
  ASC.MKP.ART% = X.CNTJ%       																							! Asigna tamano vector
End Sub 
!--- Fin carga parametros modulo de mercadeo
    
Sub INIT.PARAMETROS.MKTPLUS
 ASC.MKP.TS7%   = 2  																												! Para control UE EAMEMT01.J86
 TS.ER.RETURN = -1																													!
 !Call ALIMENTA.IMAGENES																											! Carga logos Publicitarios en memoria
 
 TS.ER.RETURN = -1																													!
 DIM UE.MCD.LOG$(7,2)																												!
 DIM UE.MCD.ART$(500)   																										! Control No presencia de articulos
 ASC.MKP.CADENA$ = "0000"																										! Cadena por omision 
 FOR I% = 1 TO 5																														!
   UE.MCD.LOG$(I%,0) = "0"																									!
   UE.MCD.LOG$(I%,1) = "9"																									!
   UE.MCD.LOG$(I%,2) = "0"																									!
 NEXT I%																																		!
 Dim ASC.MKP.TGC$(99)																												!
 If ASC.MKP.OPEN% = 0 Then Begin																						!
    TS.ER.RETURN = -1																												!
    OPEN "R::MKTP:MKPDEFPR.DAT" KEYED RECL 29 AS ASC.MKP.SE1%												!
    If TS.ER.RETURN <> -1 Then Begin																				!
       Call U.Imprime("Error Apertura MKPDEFPR, Cancelado",6100H)						!
       Ue.mktp.Line% = 00    																								! Cancela proyecto	
       Exit Sub 																														!
    EndIf                                                                   !
    ASC.MKP.OPEN% = -1 																											!
 EndIf																																			!
 
 
 If UE.MSG.OPEN% = 0 Then Begin 																						! Si no apertura 
    TS.ER.RETURN = -1																												!
    OPEN "R::MKTP:MKPMSGPR.DAT" KEYED RECL 279 AS ASC.MKP.SE2%							! Apertura de mensajes
    If TS.ER.RETURN <> -1 Then Begin																				!
       Call U.Imprime("Error Apertura MKPMSGPR, Cancelado",6100H)						!
       Ue.mktp.Line% = 00    																								! Cancela proyecto
       Exit Sub  
    EndIf                                                                   !
    UE.MSG.OPEN% = -1																												!
 EndIf																																			!
  Call CARGA.PARAMETROS.MRKTPLUS																						!
  If TS.IN.IPL Then Begin 																									! Si proceso en IPL 
     TS.ER.RETURN = -1 
     Open "R::MKTP:MKPPRITM.DAT" KEYED RECL 29 AS 54                    	  ! Open promociones
     If TS.ER.RETURN NE -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN MKTP:MKPPRITM",6100H)
     EndIf
     TS.ER.RETURN = -1 
     Open "R::MKTP:SECCION.DAT" KEYED RECL 78 AS 22        								  ! Descuento por seccion   
     If TS.ER.RETURN NE -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN MKTP:SECCION",6100H)
     EndIf
     TS.ER.RETURN = -1 
     Open "R::TF:A0004" KEYED RECL 8 AS 59                                  ! Validacion de rifas por producto
     If TS.ER.RETURN NE -1 Then Begin 
        Call U.IMPRIME("ERROR OPEN TF:A0004",6100H)
        Ue.mktp.Line% = 00 
        Exit Sub 
     EndIf
     
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
   Dim UE.MCD.MSG$(10)																												!
   Dim UE.MCD.MST$(10)																												!
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
          !Call U.Imprime(ASC.MKP.TMP1$,4200H)																!
          
		      If CA.CUSTOMER$ = "" Then CA.CUSTOMER$ ="ESTIMADO(A) CLIENTE"			!
		      TS.TEMP1$ = "Sr(a) "+CA.CUSTOMER$																	!
		      TS.TEMP1$ = Left$(TS.TEMP1$,37)																		!
          Call U.Imprime(ALINEAR$+LTRB$+TS.TEMP1$,4100H)										!
       EndIf																																!
       If MID$(UE.MCD.PRM$,3,1) = "1" Then Begin         									  ! Msg Tirilla
          Call U.Imprime(Mkt.String(UE.MCD.MST$(1)),4100H)									!
          Call U.Imprime(Mkt.String(UE.M