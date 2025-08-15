!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PROMOSEC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : DSCTO X SECCION SIN EMS          *
!**************************************************
!
! Rutinas Definidas
!

%ENVIRON T

Integer*1 Global Gr.Sec.Open%, Gr.Sec.Anul% 
Integer*4 Global Gr.Sec.Dscto%, Gr.Sec.TotDsct%, Gr.Sec.DscItm%, Asc.Tmp.Apun%

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables de transacciones
%INCLUDE EAMTERMS.J86          ! Variables de terminales
%INCLUDE EAMTOPTS.J86	       ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables EMS
%INCLUDE EAMITEMR.J86          ! Variables EAMITEMR

Function FORMAT.AMOUNT(AMT1) EXTERNAL
  Integer*1 FORMAT.AMOUNT
  Integer*4 AMT1
End Function

Function U.Imprime(UE.String$,UE.STATION) External													! Rutina de Impresion
String UE.String$      									                  									! Variables temporales
Integer*2 UE.STATION									                    									!
End Function
!--- Rutina de Impresion

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$															! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   																				! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores


Function Almacena.Cadena.Usuario(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     Sl.End = Sl.End + 1 
     Sl.Str$(Sl.End) = Pack$("99")              +				       \! En String Reservado
		":"+Pack$(X.Clave$)             +		                       \! Numero Del Proyecto
		":"+X.Datos$ + ":"                                          ! Datos Para Almacenar
End Function
!--- Fin Grabacion De Datos

Function Asic.Getunpk(X.B$,X.J%) External
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk
End Function 


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

Function PROMODEPTO(X.DPTO$) Public    ! Modulo Descuento por seccion 
String X.DPTO$, XC$, XLEC$, \
       TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
       UE.DIA$(1), UE.HORA$(2), UE.HORA.ACT$, XLINK$
Integer*2 PROMODEPTO, X.Dia%
  PROMODEPTO = 0 
  Gr.Sec.Dscto% = 0 
  Dim UE.DIA$(10)
  Dim UE.HORA$(10,2)
  X.DPTO$ = Pack$(Right$("0000000000"+X.DPTO$,10))                        ! Arma llave de busqueda
  TS.ER.RETURN = -1 																											! Control de errores
  XLEC$ = "C5 C4 C3 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4"  !
  Read Form XLEC$; #77 KEY X.DPTO$;                                       \! LECTURA SECCION
     TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
     UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \
     UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \
     UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \
     UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \
     UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \
     UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \
     UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2)		

  If TS.ER.RETURN = -1 Then Begin 																				! Si tiene descuento
     X.Dia% = DIA.SEMANA("20"+DATE$)																		  ! Retorna el dia de la semana 
  If UE.DIA$(X.Dia%) = "1" Then Begin 																		! Si Dia activo para promocion
     	UE.HORA.ACT$ = LEFT$(TIME$,4)																				! Toma hora del sistema HHMM
	    If Val(UE.HORA$(X.Dia%,1)) <= VAL(UE.HORA.ACT$) And	\           		! Si se encuentra entre el rango
	       Val(UE.HORA$(X.Dia%,2)) >= VAL(UE.HORA.ACT$) Then Begin          ! de horas parametrizado
           PROMODEPTO = -1                                                ! Retorna departamento
           Gr.Sec.Dscto% = Val(DM.VLRCON$)                                ! Retorna ptg dscto aplicar
      EndIf Else PROMODEPTO = 0      			                    						! Dia no activo de la promocion
   EndIf Else PROMODEPTO = 0        					    												! Dia no activo de la promocion
  EndIf Else Begin 																												!
     PROMODEPTO = 0            																						! Codigo del cupon de descuento 
  EndIf 
End Function 
!--- Fin promocion por departamento

Sub DCTOXSEC(Xopt%) Public																			! Modulo Descuento por Seccion 
Integer*4 Xopt%, Gr.Tmp%																				! User Exit a Invocar

!-- EAMTSU07.J86
If Xopt% = 07 Then Begin                                        ! Si UE Carga opciones
	   TS.ER.RETURN = -1
	   If Gr.Sec.Open% = 0 Then Begin 														! Control de apertura
        Open "R::TF:DCTSEC" KEYED RECL 75 AS 77     	! Descuento por seccion   
        If TS.ER.RETURN NE -1 Then Begin 												! Error de Apertura
           Call U.IMPRIME("ERROR OPEN SECCION",6100H)			      ! Msg de Alerta
           Exit Sub 																						!
        EndIf																										!
        Gr.Sec.Open% = -1																				! Apertura OK%
     EndIf 																											!
EndIf 																													!

!-- EAMTSU02.J86
If Xopt% = 02 Then Begin                                        ! Si UE Inicia trx
   Gr.Sec.TotDsct%  = 0 
   Gr.Sec.DscItm%   = 0 
   Gr.Tmp%          = 0 
   Gr.Sec.Anul%     = 0 
EndIf 

If Xopt% = 20 Then Begin 
	 If TS.LINETYPE = 22 THEN Gr.Sec.Anul% = -1                                 !  Prende bandera de letrero de NIT 
	 If Gr.Sec.Anul% = 0 Then \
	 If Gr.Sec.TotDsct% > 0 AND TS.LINETYPE = 6 AND TS.LINEDATA = 1 Then Begin  ! si dscto reportados
      Call FORMAT.AMOUNT(Gr.Sec.TotDsct%)                                     ! formatea valor dscto
      TS.TEMP2$ = RIGHT$("          "+TS.TEMP1$,10)                           ! imprime en el tiquete 
      TS.TEMP2$ = CHR$(1BH)+CHR$(21H)+CHR$(01H) +                            \!
                      "DESCUENTO EVENTOS"+TS.TEMP2$                           !      
      Call U.IMPRIME(TS.TEMP2$,6100H)                                         ! en CR y SJ
      TS.TEMP1$ = Pack$(Str$(Gr.Sec.TotDsct%))  +                            \! Dscto total de la trx
                  ":" + Pack$("00")                                           !  
      Call Almacena.Cadena.Usuario("62",TS.TEMP1$)              
      Gr.Sec.TotDsct% = 0 
 EndIf

EndIf 
!-- EAMTSU43.J86
If Xopt% = 43 Then Begin                                        ! Si UE Control de precio
TS.TEMP1I4 = PROMODEPTO(UnPack$(IR.DEPARTME$))							    ! Valida si descuento en la seccion
If TS.TEMP1I4 = -1 Then Begin                                   ! Si descuento por seccion 
   Gr.Tmp% = Float(SL.IT.XPRICE * Gr.Sec.Dscto%) / 100          ! Descuento a otorgar
   If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then Begin    ! Si una anulacion
     Gr.Sec.TotDsct% = Gr.Sec.TotDsct% + (Gr.Tmp% * -1)         ! Total de descuento
   EndIf Else Begin 
     Gr.Sec.TotDsct% = Gr.Sec.TotDsct% + Gr.Tmp%                ! Total de descuento
   EndIf 
   Gr.Sec.DscItm% = Gr.Tmp%     				                        ! Dscto Otorgado al Item
   If IR.INDICAT0 AND 40H Then Begin                            ! Si articulo pesable
      SL.IT.XPRICE = SL.IT.XPRICE - Gr.Tmp%                     ! Precio total Sin descuento
      Gr.Tmp% = Float(SL.IE.SALEPRIC * Gr.Sec.Dscto%) / 100     ! Descuento a otorgar
      SL.IE.SALEPRIC = SL.IE.SALEPRIC - Gr.Tmp%                 ! Precio unitario 
   EndIf Else Begin                                             !
      SL.IT.XPRICE = SL.IT.XPRICE - Gr.Tmp%                     ! Precio total Sin descuento
      SL.IE.SALEPRIC   = SL.IT.XPRICE / SL.IE.QTYORWGT          ! Precio total 
   EndIf 
   IR.PRICE1      = SL.IE.SALEPRIC
   IR.PRICE2      = SL.IE.SALEPRIC
   If (TS.IO.KEYS(1) = 70 OR TS.IO.KEYS(8) = 67 ) Then \        ! Si una anulacion
      Gr.Sec.DscItm% = (Gr.Sec.DscItm% * -1)
   TS.TEMP1$ = Pack$(Str$(Gr.Sec.Dscto%))  +                   \! Ptg Dscto del articulo
               ":" + Pack$(Str$(Gr.Sec.DscItm%))                ! Dscto Otorgado
   Call Almacena.Cadena.Usuario("61",TS.TEMP1$)              

EndIf 
    
EndIf 

!-- EAMTSU53.J86
If Xopt% = 53 Then Begin                                        ! Si UE Recup trx
 If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin   	      ! Si DATA/ENTRY
  If Unpack$(MID$(SL.STR.ENTRY$,3,1)) = "61" THEN BEGIN         ! Dscto x Seccion 
    Asc.Tmp.Apun% = 3							                              ! Apuntador String
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%) 		  ! Codigo del proyecto
    TS.TEMP1$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%) 		  ! Ptg Dscto otorgado al articulo
    TS.TEMP2$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	  	  ! Dscto Otorgado
    TS.TEMP2$ = TRANSLATE$(TS.TEMP2$,"=","-")
    Gr.Sec.Dscto% = Val(TS.TEMP1$)
    Gr.Sec.DscItm% = Val(TS.TEMP2$)
    Gr.Sec.TotDsct% = Gr.Sec.TotDsct% + Gr.Sec.DscItm%          ! Total de descuento
  EndIf 
 EndIf 
EndIf 

!-- EAMTSU56.J86
If Xopt% = 56 Then Begin                                        ! Si UE Suspencion trx
   Gr.Sec.TotDsct%  = 0 
   Gr.Sec.DscItm%   = 0 
   Gr.Tmp%          = 0 
   Gr.Sec.Anul%     = 0 
EndIf 

!-- EAMTSU60.J86
If Xopt% = 60 Then Begin                                        ! Si UE Impresion tiquete

 If (TS.LINETYPE = 1 And TS.LINEDATA = 0 And TS.PROCEDURE = -1)Then \! Registro venta de un articulo
  If Gr.Sec.DscItm% <> 0 Then Begin 
  	 TS.TEMP2$ = Right$("   "+Str$(Gr.Sec.Dscto%),3) + "%"
  	 Call Format.Amount(Gr.Sec.DscItm%)
  	 TS.TEMP1$ = RIGHT$("          "+TS.TEMP1$,10)              ! imprime en el tiquete 
 	   Call U.Imprime("Dscto Aplicado "+TS.TEMP2$+" :"+TS.TEMP1$,6100H)
 	   Gr.Sec.DscItm% = 0 
  EndIf 
EndIf 


End Sub 