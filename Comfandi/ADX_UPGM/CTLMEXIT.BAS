!************************************************** 
!Empresa       : Grupo Retail Ltda  		          *
!Programa      : CTLMEXIT.BAS                     *
!Autor         : Oscar Valencia                   *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Febrero de 2008                  *
!Observaciones : Modulo Tirilla Seguridad         *
!**************************************************
! Modificaciones

%ENVIRON T

INTEGER*1 GLOBAL UE.CTLFAC.ACTIVO%, \	! SI PROYECTO ACTIVO
                 UE.CTLFAC.SESION%, \
                 UE.CTLFAC.BARRAS%

Integer*4 Global Asc.Tmp.Apun%
String    Global Gr.Ctlfac.Items$(2), Gr.CtlFac.Data$
Integer*1 Global Gr.Anmc.Graba%														! Modulo Anulacion Productos

%INCLUDE EAMTSCVA.J86
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          !
%INCLUDE EAMTERMS.J86          !
%INCLUDE EAMITEMR.J86          !
%INCLUDE EAMTOPTS.J86	         !
%INCLUDE EAMP4VAJ.J86          !
%INCLUDE EAMTSXHC.J86     

%INCLUDE GRMTSUSU.011          ! Invocacion de rutinas externas genericas

Sub TSHIECET External 
End Sub 

Function GETN1(P1$,P2) EXTERNAL                       
     Integer*1 GETN1
     String P1$
     Integer*2 P2
End Function

Function GETN2(P1$,P2) EXTERNAL
     INTEGER*2 GETN2
     STRING P1$
     INTEGER*2 P2
End Function 


Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

SUB ENTREGA.ITEMRECORD (RECORD$)
  String    RECORD$    !* The record (including the key) read from file      *!

  !***************************************************************************!
  !* Split the record into the various item fields                           *!
  !***************************************************************************!
  IR.ITEMCODE$ = MID$(RECORD$,1,6)
  IR.INDICAT0  = GETN1(RECORD$,6)
  IR.INDICAT1  = GETN1(RECORD$,7)
  IR.INDICAT1A = GETN1(RECORD$,8)
  IR.INDICAT2$ = MID$(RECORD$,10,1)
  IR.DEPARTME$ = MID$(RECORD$,11,2)
  IR.FAMILYNU$ = MID$(RECORD$,13,3)
  IR.MPGROUP$  = MID$(RECORD$,16,1)
  IR.SALEQUAN$ = MID$(RECORD$,17,1)
  IR.SALEPRIC$ = MID$(RECORD$,18,5)
  IR.LINKEDTO$ = MID$(RECORD$,23,2)
  IR.ITEMNAME$ = MID$(RECORD$,25,18)
  IR.USEREXIT1 = GETN2(RECORD$,42)
  IR.USEREXIT2 = GETN2(RECORD$,44)
End Sub 

Function Gr.Getunpk(ST.B$,ST.J%) Public
Integer*2 ST.J%, ST.K%
String    ST.B$, Gr.Getunpk
  Asc.Tmp.Apun% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Gr.Getunpk  = Unpack$(Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Gr.Getunpk  = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function


Sub Copia.Barra.Tirilla Public
String BARCODE$, ENDBCODE$
TO.USEREXIT(20) = 0 
TO.USEREXIT(60) = 0 

If PRT4610.ENABLE <> 0 Then Begin 																					! Si impresora termica
   BARCODE$  = Chr$(1DH) + Chr$(6BH) 																				! Manejo de impresion
   ENDBCODE$ = Chr$(00H) 																										! codigos de barra
   TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)																	! Centra la impresion
   Call U.IMPRIME(TS.TEMP1$,4000H)																					!        
   TS.TEMP1$ =     Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		       							 \! Horizontal barcode
                   Chr$(1DH)+Chr$(68H)+Chr$(40H)+                          \! Vertical barcode			
                   Chr$(1DH)+Chr$(48H)+Chr$(00H)+                          \! Human Read characters
		               Chr$(1DH)+Chr$(66H)+Chr$(00H)    		  									! desactivado
   Call U.IMPRIME(TS.TEMP1$,4000H)				        													!
   TS.TEMP1$ = (BARCODE$+Chr$(07H)+Gr.CtlFac.Data$+ENDBCODE$)           		! Arma String de impresion			
   Write #34 ; TS.TEMP1$          																					! Imprime cupon en EAN-128
   TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)																	! Ajusta impresion a la Izquierda
   Call U.IMPRIME(TS.TEMP1$,4000H)				        													!                    
EndIf 

TO.USEREXIT(20) = -1
TO.USEREXIT(60) = -1
End Sub 

Sub SECURITY.MESSAGE
String XKEY$, XDUMMY, XCLAVE$
  TS.TS11WERR$ = ""																										  		! CONTROL DE ERRORES
  Open "R::$F:FACCLV" KEYED RECL 18 AS UE.CTLFAC.SESION% UNLOCKED NOWRITE NODEL ! APERTURA CLAVES SEGURIDAD
	If TS.TS11WERR$ <> "" THEN BEGIN																					! SI ERROR EN APERTURA
		 XCLAVE$ = "EOPNTRHNDEFNT40"							                          		! Error de apertura
	EndIf Else Begin																													!
	  XKEY$ = PACK$(DATE$)           																					! EMPAQUETA FECHA SISTEMA AAMMDD
	  TS.TS11WERR$ = ""																												! CONTROL DE ERRORES
    READ FORM "C3 C15";#UE.CTLFAC.SESION%  KEY XKEY$; XDUMMY, XCLAVE$	  		! LECTURA ARCHIVO
    IF TS.TS11WERR$ <> "" THEN XCLAVE$ = "AGH1TRHNDEFNT40"						  		! DIA NO ENCONTRADO
	  Close UE.CTLFAC.SESION%																									! CIERRE ARCHIVO  	
  EndIf
	XCLAVE$ = "           "+XCLAVE$																						! CENTRA MENSAJE
	TS.TEMP1$ = Chr$(1BH)+Chr$(21H)+Chr$(10H) + XCLAVE$												! CAMBIA TIPO DE LETRA
	Call U.IMPRIME(TS.TEMP1$,4000H)																						! IMPRIME MENSAJE CR
	Call U.IMPRIME(TS.TEMP1$,2100H)																						! IMPRIME MENSAJE SJ
End Sub

Sub PROCESO.BARRA(XOPT%)
Integer*1 Xopt%, X.I%
String PSTR1$, PSTR2$, PSTRING$, XDATA$, Xkey$, XDAT2$, XDAT3$
String BARCODE$, ENDBCODE$, Xlec$
  Xkey$ = Right$("0000"+Ts.Terminal$,4) +                            \! Numero de la terminal
          Right$("0000" +Str$(Sl.Hd.Transnum+1),4)+                  \! Numero de la transaccion
          Right$(Date$,4) +                                          \! MMDD del movimiento
          Right$("000000"+Time$,6)																	  ! Hora de la Trx 
  Xkey$  = Pack$(Xkey$)																					      ! Llave empaquetada
  XDAT2$ = Pack$(Right$("0000000000"+Str$(TS.TOTALS(0,0,0)),10))      ! Vlr de la trx
  XDAT3$ = Pack$(Right$("0000"+Str$(ITEM.COUNT),4))	      	      ! Numero Itms en la trx
  
  For X.I% = 1 TO 5						                                        ! Ajusta dato para almacenamiento
    Gr.Ctlfac.Items$(X.I%, 0) = Pack$(Right$("000000000000"+Gr.Ctlfac.Items$(X.I%, 0),12)) ! Ajusta PLU
    Gr.Ctlfac.Items$(X.I%, 1) = Pack$(Right$("0000000000"+Gr.Ctlfac.Items$(X.I%, 1),10))   ! Ajusta Precio
    Gr.Ctlfac.Items$(X.I%, 3) = Pack$(Right$("000000"+Gr.Ctlfac.Items$(X.I%, 3),06))       ! Ajusta Cantidad
    TS.TEMP2$ = UnPack$(Gr.Ctlfac.Items$(X.I%, 0))                       	! Item a consultar
    If Val(TS.TEMP2$) > 0 Then Begin 
       TS.TEMP2$ = Pack$(Right$("000000000000"+TS.TEMP2$,12))            	! Dato para busqueda
       Call IRRFEC.READ01 (TS.TEMP2$, 4, TS.TEMP1$, 0)                   	! Lectura Datos Itemr
       Call ENTREGA.ITEMRECORD( TS.TEMP1$ ) 			         							 	! Entrega datos al eamitemr.j86
       Gr.Ctlfac.Items$(X.I%, 2) = IR.ITEMNAME$			         							! Nombre del producto
    EndIf    
  Next X.I%

  TS.TS11WERR$ = "" 						         																		! Ctrl Errores
  Xlec$ = "C9 C5 C2 C5 C2 C6 C18 C5 C3 C6 C18 C5 C3 C6 C18 C5 C3 C6 C18 C5 C3 C6 C18 C5 C3"	!
  Open "R::$F:FACO02" KEYED RECL 183 AS UE.CTLFAC.SESION% UNLOCKED NOWRITE NODEL ! Abre archivo de control
  If TS.TS11WERR$ = "" Then Begin 				         													! Apertura OK
     Write Form Xlec$;#UE.CTLFAC.SESION% ;                                 \! Graba movimiento para control
     Xkey$, XDAT2$, XDAT3$, TS.OPER$, Pack$("0000"),		                   \! 
     Gr.Ctlfac.Items$(1,0),Gr.Ctlfac.Items$(1,2),Gr.Ctlfac.Items$(1,1), Gr.Ctlfac.Items$(1,3), \! Almacena los 5 de mayor precio
     Gr.Ctlfac.Items$(2,0),Gr.Ctlfac.Items$(2,2),Gr.Ctlfac.Items$(2,1), Gr.Ctlfac.Items$(2,3), \! Plu, Descripcion, Valor, Qty
     Gr.Ctlfac.Items$(3,0),Gr.Ctlfac.Items$(3,2),Gr.Ctlfac.Items$(3,1), Gr.Ctlfac.Items$(3,3), \!
     Gr.Ctlfac.Items$(4,0),Gr.Ctlfac.Items$(4,2),Gr.Ctlfac.Items$(4,1), Gr.Ctlfac.Items$(4,3), \!
     Gr.Ctlfac.Items$(5,0),Gr.Ctlfac.Items$(5,2),Gr.Ctlfac.Items$(5,1), Gr.Ctlfac.Items$(5,3)
     If TS.TS11WERR$ = "" Then Begin 
        !Call VISORES4690(1,"ERROR GRABACION","TFFACO02",1000,"L")
     EndIf 
     Close UE.CTLFAC.SESION%																					! Cierra archivo
  EndIf Else Begin 
    !Call VISORES4690(1,"ERROR APERTURA ","TFFACO02",1000,"L")
  EndIf 
  Xdata$ = Unpack$(Xkey$) 
  Gr.CtlFac.Data$ = Xdata$
  TO.USEREXIT(20) = 0 
  TO.USEREXIT(60) = 0 
If PRT4610.ENABLE <> 0 Then Begin 					! Si impresora termica
    CALL SECURITY.MESSAGE
  If XOPT% = 0 Then Begin 
     PSTR1$   = Chr$(1DH)+Chr$(50H)
     PSTR2$   = Xdata$ + Chr$(00H)
     PSTRING$ = PSTR1$ + PSTR2$
     Write #34; PSTRING$
     TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)			        ! Ajusta impresion a la Izquierda
     Call U.IMPRIME(TS.TEMP1$,4000H)					!                    
  EndIf Else Begin 
   BARCODE$  = Chr$(1DH) + Chr$(6BH) 					! Manejo de impresion
   ENDBCODE$ = Chr$(00H) 						          ! codigos de barra
   TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(1)		! Centra la impresion
   Call U.IMPRIME(TS.TEMP1$,4000H)					  !        
!--- Se ajusta el modulo para que cancele la impresion del HRI 17Jun2009 - Grupo Retail OVS   
   TS.TEMP1$ =     Chr$(1DH)+Chr$(77H)+Chr$(01H)+ 		       \! Horizontal barcode
                   Chr$(1DH)+Chr$(68H)+Chr$(40H)+            \! Vertical barcode			
                   Chr$(1DH)+Chr$(48H)+Chr$(00H)+            \! Human Read characters
		               Chr$(1DH)+Chr$(66H)+Chr$(00H)    			    ! desactivado

   Call U.IMPRIME(TS.TEMP1$,4000H)				        !
!-- Fin lineas adicionadas   
   XDATA$ = XDATA$ + Right$("00000000"+Str$(TS.TOTALS(0,0,0)),8)
   TS.TEMP1$ = (BARCODE$+Chr$(07H)+XDATA$+ENDBCODE$)                    ! Arma String de impresion			
   Write #34 ; TS.TEMP1$          					      ! Imprime cupon en EAN-128
   TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				! Ajusta impresion a la Izquierda
   Call U.IMPRIME(TS.TEMP1$,4000H)				        !                    
   

!   Call U.IMPRIME("VALOR TOTAL :"+STR$(TS.TOTALS(0,0,0)),4100H)
!   Call U.IMPRIME("DSCTO TOTAL :"+STR$(TS.TOTALS(0,0,1)),4100H)
!   Call U.IMPRIME("NETO  TOTAL :"+STR$(TS.TEMP1I4),4100H)
   
  EndIf 
  TO.USEREXIT(20) = -1
  TO.USEREXIT(60) = -1
EndIf 

End Sub 

Sub Metodo.Burbuja(XX.J%)						! Ordenamiento de productos 
Integer*4 XX.J%, X.I%, X.K%						! Definicion Variables
String Xtmp$, Xtmp2$, Xtmp3$						! 
For X.I% = 1 To (XX.J% - 1)						! Ejecuta proceso ordenacion
 For X.K% = (X.I% + 1) To XX.J%						!
 If Val(Gr.Ctlfac.Items$(X.I%, 1)) < Val(Gr.Ctlfac.Items$(X.K%, 1)) Then Begin 	!
    Xtmp$  = Gr.Ctlfac.Items$(X.K%, 0)					!
    Xtmp2$ = Gr.Ctlfac.Items$(X.K%, 1)					!
    Xtmp3$ = Gr.Ctlfac.Items$(X.K%, 3)					!
    Gr.Ctlfac.Items$(X.K%, 0) = Gr.Ctlfac.Items$(X.I%, 0)		!
    Gr.Ctlfac.Items$(X.K%, 1) = Gr.Ctlfac.Items$(X.I%, 1)		!    
    Gr.Ctlfac.Items$(X.K%, 3) = Gr.Ctlfac.Items$(X.I%, 3)		!        
    Gr.Ctlfac.Items$(X.I%, 0) = XTMP$					!
    Gr.Ctlfac.Items$(X.I%, 1) = XTMP2$				        !    
    Gr.Ctlfac.Items$(X.I%, 3) = XTMP3$				        !        
 EndIf									!
 Next X.K%								!
Next X.I%								!
End Sub 

Sub Almacena.Productos
Integer*1 Xstat%, X.SIG%
Integer*4 PRIC%, TOT4%, X.I%, XX.J%, X.K%, X.ind1%, X.ind2%, X.Pos%
String    VENTA.TIQUETE, X.LISTA$, X.TMP$, X.Tmp2$, X.tmp3$, Alto$, X.Ind3$ !
String    X.Qty$
Dim Gr.Ctlfac.Items$(700,3)						! Init String Datos
For X.I% = 1 To 700
    Gr.Ctlfac.Items$(X.I%,0) = "0"
    Gr.Ctlfac.Items$(X.I%,1) = "0"
    Gr.Ctlfac.Items$(X.I%,3) = "0"
Next X.I%
XX.J% = 1
  For X.I% = 1 TO SL.END                    				! Total de la trx
    Asc.Tmp.Apun% = 3							! Init Apuntador
    X.SIG% = 1								! Signo Operacion 
    H$ = READ.SL.STR$(X.I%)                 														! GET String
    If LEN(H$) > 5 Then Begin                                           ! Buena Lectura
     If ASC(H$)  = 1 Then Begin             				! ITEM ENTRY String
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! Plu Articulo Vendido
        X.TMP2$ = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! Precio vendido
        X.TMP3$ = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! Departamento
        X.TMP3$ = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! Coupon Family
        X.IND1% = Val(GR.GETUNPK(H$,Asc.Tmp.Apun%))			! Indicat 1
        X.IND2% = Val(GR.GETUNPK(H$,Asc.Tmp.Apun%))			! Indicat 2
        X.IND3$ = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! Indicat 3
        ALTO$ = Left$(X.IND3$,1)        				! Se valida si se realiza 
        If (X.Ind2% AND 0080H) AND (ALTO$ = "4") Then X.SIG% = -1	! un proceso de anulacion de 
        If (X.Ind2% AND 0040H) AND (ALTO$ = "4") Then X.SIG% = -1	! venta de Item
        If (X.Ind2% AND 0080H) AND (ALTO$ = "1") Then X.SIG% = -1       ! 
        If (X.Ind2% AND 0040H) AND (ALTO$ = "1") Then X.SIG% = -1       ! 
        If X.Ind2% = 2054 Then X.SIG% = -1				!
        If X.Ind2% = 6 Then X.SIG% = -1					! 
        Xstat% = -1 							! Init Variable
        For X.K% = 1 To XX.J%						! Busca si PLU ya vendido
         If Val(Gr.Ctlfac.Items$(X.K%,0)) = Val(X.Tmp$) Then Begin	! Si plu ya registrado
            X.Pos% = X.K%						! Posicion almacenada
            XStat% = 0 							! 
            X.K% = XX.J% + 1						!
         EndIf 								!
        Next X.K%							! 
        If XStat% = -1 Then Begin 					! Si no almacenado
           Gr.Ctlfac.Items$(XX.J%,0) = X.TMP$				! Almacena PLU
           Gr.Ctlfac.Items$(XX.J%,1) = X.TMP2$        			! Almacena Precio
           Gr.Ctlfac.Items$(XX.J%,3) = "1"        			! Qty Vendida
           X.Pos% = XX.J%						! Posicion almacenada
           XX.J% = XX.J% + 1 						! Aumenta Apuntador
        EndIf Else Begin 						! Fin almacenamiento
            Gr.Ctlfac.Items$(X.pos%,1) = Str$(Val(Gr.Ctlfac.Items$(X.Pos%,1)) + \! Suma precio
                                       (Val(X.tmp2$) * X.Sig%))         !
        EndIf 								!
     EndIf    								! ITEM ENTRY String
     Asc.Tmp.Apun% = 3							! Init Apuntador     
     If ASC(H$)  = 2 Then Begin             				! Extension ITEM ENTRY String
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! MPGROUP
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! DEALQUAN
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! METODO PRECIO
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! PRECIO 
        X.TMP$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! CANTIDAD
        X.QTY$  = GR.GETUNPK(H$,Asc.Tmp.Apun%)			! CANTIDAD
        If Xstat% = -1 Then Begin 					! Registro Nuevo
           Gr.Ctlfac.Items$(X.Pos%,3) = Str$( (Val(Gr.Ctlfac.Items$(X.Pos%,3)) - 1)  + \! Suma Cantidad
                                       (Val(X.Qty$) * X.Sig%) )          !
        EndIf Else Begin 
           Gr.Ctlfac.Items$(X.Pos%,3) = Str$(Val(Gr.Ctlfac.Items$(X.Pos%,3)) + \! Suma Cantidad
                                       (Val(X.Qty$) * X.Sig%))          !
        EndIf 								!
     EndIf 								! Fin Extension Item Entry 
    EndIf 								! Lectura correcta
  Next X.I%   								! NEXT String
  Call Metodo.Burbuja(XX.J%)						! Aplica metodo ordenamiento
End Sub


Sub CTLMVFAC(XOPTION%)  Public
INTEGER*4 XOPTION%

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control Facturas Almacen                      **
!   **    Incluir en :  EAMTSU07.J86                                         **
!   ***************************************************************************

If XOPTION% = 7 Then Begin 
   UE.CTLFAC.ACTIVO% = 0 
   UE.CTLFAC.BARRAS% = 1
   TS.ER.RETURN = -1																											  ! Ctrl Errores
   Open "R::$F:FACO01" AS 94	UNLOCKED NOWRITE NODEL  				              ! Apertura archivo parametrizacion
   If TS.ER.RETURN <> -1 Then Begin																					! Error en la apertura
      Call U.IMPRIME("Error Apertura TFFACO01",6100H)
      Call U.IMPRIME("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$+" FILE="+\
                     STR$(TS.ERRF),6100H)
      Call U.IMPRIME("ERROR CONTROL FACTURAS ",6100H)	           						! Rastro en SJ
      Exit Sub 																															! Sale del procedimiento
   EndIf Else Begin 
      Read #94; TS.TEMP1$			       																				! Proyecto Activo
      UE.CTLFAC.ACTIVO%  = Val(Mid$(TS.TEMP1$,30,2))
      Read #94; TS.TEMP1$			       																				! Tipo de Barra a imprimir 0=PDF-417 1=EAN-128
      UE.CTLFAC.BARRAS%  = Val(Mid$(TS.TEMP1$,30,1))
      Read #94; TS.TEMP1$			       																				! Sesion Temporal
      UE.CTLFAC.SESION%  = Val(Mid$(TS.TEMP1$,30,2))
   EndIf 
   Close 94
   If UE.CTLFAC.ACTIVO% = -1 Then                                          \! Proyecto Activo
      Call U.IMPRIME("MODULO TIRILLA SEGUR.ON  08-May-2014",6100H) Else    \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO TIRILLA SEGUR.OFF 08-May-2014",6100H)          ! Msg Proyecto Cargado
   
EndIf

If UE.CTLFAC.ACTIVO% <> -1 Then Exit Sub 	 											! SI MODULO APAGADO 

!   ***************************************************************************
!   **    Proyecto   :  Modulo Control de Password Cajeros                   **
!   **    Incluir en :  EAMTSU20.J86                                         **
!   ***************************************************************************


If XOPTION% = 20 Then Begin 
   If NOT TS.TRAINING Then Begin                       		! Si no en entrenamiento
    If TS.INTRX Then                                     \! Si esta en una compra
     If TS.LINETYPE = 6 AND                              \! Store Data y Transacc
        TS.LINEDATA = 1 Then Begin                     		! Fecha, Hora, etc
        If TS.TENDERED (0)  >= 0 OR                      \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin             		! y NO hay VOID en curso
           If Gr.Anmc.Graba% <> -1 Then Begin 						! No es Anulacion de producto Mod01Jul2011
              Call ALMACENA.PRODUCTOS                     ! Almacena los 5 productos mas costosos           
              Call PROCESO.BARRA(UE.CTLFAC.BARRAS%)       ! Almacena Movimiento
           EndIf
        EndIf 
      EndIf 
    EndIf 
EndIf 

End Sub 

