!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : PROPINAS.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio de 2006                    *
!Observaciones : Modulo Recaudo de propinas       *
!**************************************************
!

%ENVIRON T
%INCLUDE PRPTSUVA.011          ! Variables Aplicacion
%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! 
%INCLUDE EAMTERMS.J86          ! 

%INCLUDE RECATSSU.011  				 ! Rutinas Genericas

Function VALIDA.EMPACADOR(PROP.ID$)	
String PROP.ID$
Integer*1 VALIDA.EMPACADOR
PROP.ID$ = Pack$(Right$("0000000000"+PROP.ID$,10))
TS.ER.RETURN = -1 
Open "R::$AMOPERA" KEYED RECL 72 AS 94 UNLOCKED NOWRITE NODEL   !Read operator file
If TS.ER.RETURN = -1 Then Begin 
   Read Form "C5,C67";#94 KEY PROP.ID$;\
  	     A$,GR.PROP.NAME$
   GR.PROP.NAME$ = MID$(GR.PROP.NAME$,28,20)  	     
   VALIDA.EMPACADOR = -1
   Close 94 
EndIf Else VALIDA.EMPACADOR = 0

End Function 


Sub PROPINA(XOPT%) Public																									! Modulo recaudo Propinas
Integer*4 XOPT%, XAUX%																										! Variables 

!--- EAMTSU07.J86
If XOPT% = 07 Then Begin 																									! Carga de opciones
   GR.PROP.ACT%	= 0																												! Proyecto apagado
   GR.PROP.GUD% = 0 																											!
   GR.PROP.VLR$ = ""																											!
   GR.PROP.ID$  = ""																											!
   GR.PROP.NAME$ = ""
   TS.ER.RETURN = -1																											! Ctrl Errores
   Open "R::ASCNTRL" AS 94																								! Apertura archivo parametrizacion 
   If END #94 THEN GR.FIN.PROPINA		       																! Si es EOF
   While (1)															  															! Recorre archivo
        Read #94; TS.TEMP1$			       																		! Lectura registro
        IF TS.TEMP1$ = "[RECAUDO PROPINAS]" Then Begin	    			   			! Recaudo Propinas
         Read #94; TS.TEMP1$																							! Lectura registro
         GR.PROP.ACT% = Val(Mid$(TS.TEMP1$,30,2))				          				! Proyecto Activo
         Read #94; TS.TEMP1$																							! Lectura registro
         GR.PROP.MOT$ = Mid$(TS.TEMP1$,30,3)     				    				      ! Tecla Motora
         Read #94; TS.TEMP1$																							! Lectura registro
         GR.PROP.ITM$ = Mid$(TS.TEMP1$,30,12)   				    				      ! PLU de registro
         GoTo GR.FIN.PROPINA  																						! Sale del ciclo de carga
       Endif                                                              !
   Wend                                                                   !
   GR.FIN.PROPINA:                                                        !
     Close 94																															! Cierra archivo
   If GR.PROP.ACT% = -1 Then                                                   \! Proyecto Activo
      Call U.IMPRIME("MODULO PROPINA EMPAC.  ON 30/Ago/2011",2100H) Else     \! Msg Proyecto Cargado
      Call U.IMPRIME("MODULO PROPINA EMPAC. OFF 30/Ago/2011",2100H)           ! Msg Proyecto Cargado

EndIf 

If GR.PROP.ACT% <> -1 Then Exit Sub 																			! Si proyecto apagadao

!--- EAMTSU02.J86
If XOPT% = 02 Then Begin 																									! Init variables New trx
GR.PROP.VLR$ = ""
GR.PROP.ID$  = ""
GR.PROP.ITM% = 0
GR.PROP.GUD% = 0 
GR.PROP.NAME$ = ""
EndIf 

!--- EAMTSU14.J86
If XOPT% = 14 Then Begin 																									! Ue secuencias de tecleo
  If TS.IO.MOTORKEY = Val(GR.PROP.MOT$) Then Begin  											! Si tecla motora dialogo
     GR.PROP.ID$ = ASIC.DATOS$("NUMERO ID DEL","EMPACADOR")								! Ingresa Numero de identificacion
     If (GR.PROP.ID$ = "") OR (GR.PROP.ID$ = "E") Then Begin							! No se capturo dato
        Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
        Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
        TS.IO.MOTORKEY = 73
        Exit Sub 
     EndIf		
     XAUX% = VALIDA.EMPACADOR(GR.PROP.ID$)															  ! Valida operador contra Eamopera
     If XAUX% <> -1 Then Begin 
        Call VISOR.AND.BORRAR("EMPACADOR NO DEFINIDO EN EL SISTEMA")
        Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
        TS.IO.MOTORKEY = 73
        Exit Sub 
     EndIf 
     GR.PROP.VLR$ = ASIC.DATOS$("VALOR PROPINA","RECAUDADO")							! Ingresa Numero de identificacion
     If Val(GR.PROP.VLR$) <= 0 OR (GR.PROP.VLR$ = "E") Then Begin	    		! No se capturo dato
        Call VISOR.AND.BORRAR("PROCESO CANCELADO POR USUARIO")
        Dim TS.IO.DATA$(10): Dim TS.IO.KEYS(10)
        TS.IO.MOTORKEY = 73
        Exit Sub 
     EndIf		
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
     TS.IO.DATA$(2) = Right$("000000000000"+Str$(Val(GR.PROP.ITM$)),12)
     TS.IO.KEYS(2)  = 80
     TS.IO.MOTORKEY = 80
     TS.IO.DEVICE   = 1
     GR.PROP.ITM%   = -1
     Exit Sub 
  EndIf 																																	!
EndIf 

!--- Ejecucion en EAMTSU43
If XOPT% = 43 Then Begin 																									! Control de precio venta
   If GR.PROP.ITM% = -1 Then Begin 																			  ! Registro recaudo propinas
      SL.IT.XPRICE = Val(GR.PROP.VLR$)																		! Precio del recaudo
      GR.PROP.ITM% = -2																										! Estatus de impresion
      GR.PROP.GUD% = -1																										! Almacena User Data
   EndIf 
EndIf 

!--- EAMTSU53.J86
If XOPT% = 53 Then Begin 																									! Recuperacion de trx
If Unpack$(LEFT$(SL.STR.ENTRY$,1)) = "99" Then Begin   	      						! Si DATA/ENTRY
  If Unpack$(MID$(SL.STR.ENTRY$,3,1)) = "27" THEN BEGIN         					! Recaudo Propinas
    Asc.Tmp.Apun% = 3							                              					! Apuntador String
    TS.TEMP1$    = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%) 		 					! Codigo del proyecto
    GR.PROP.ID$  = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%) 		 					! Id del Auxiliar
    GR.PROP.VLR$ = ASIC.GETUNPK(SL.STR.ENTRY$,Asc.Tmp.Apun%)	   					! Valor Recaudado
    GR.PROP.ITM% = -2																											! Flag de impresion
  EndIf 
 EndIf 
EndIf 

!--- EAMTSU56.J86
If XOPT% = 56 Then Begin 																									! Suspension de trx
GR.PROP.VLR$ = ""
GR.PROP.ID$  = ""
GR.PROP.ITM% = 0 
GR.PROP.GUD% = 0
GR.PROP.NAME$ = ""
EndIf 

!--- EAMTSU60.J86
If XOPT% = 60 Then Begin 																									! Impresion dato tiquete cliente
 If (TS.LINETYPE = 1 And TS.LINEDATA = 0 And TS.PROCEDURE = -1)Then      \! Registro venta de un articulo
  If GR.PROP.ITM% = -2 Then Begin 
  	 TS.TEMP1$ = "ID EMPACADOR :"+GR.PROP.ID$
  	 TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(38," "),38)
  	 Call U.Imprime(TS.TEMP1$,6100H)
  	 TS.TEMP1$ = "NOMBRE       :"+GR.PROP.NAME$
  	 TS.TEMP1$ = LEFT$(TS.TEMP1$+STRING$(38," "),38)
  	 Call U.Imprime(TS.TEMP1$,6100H)
  	 TS.TEMP1$ = Pack$(GR.PROP.ID$)      +     \! ID del Empacador
		             ":"+Pack$(GR.PROP.VLR$) +	  \! Valor recaudado
		             ":"+Pack$("00")
  	 If GR.PROP.GUD% = -1 Then \
  	    Call Grabacion.String.Usuario2("27",TS.TEMP1$)
 	   GR.PROP.ITM% = 0 
 	   GR.PROP.GUD% = 0
  EndIf 
EndIf 

End Sub 