!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRRMEXIT.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Diciembre de 2.009               *
!Observaciones : Modulo Generacion consecutivo    *
!                de remisiones                    *
!**************************************************

%ENVIRON T		                 ! Ambiente de terminal

Integer*1 Global  Asc.Ind.Open.Fiscal% , ASIC.LLAVE%

%INCLUDE ADX_UPGM:EAMTSWKG.J86          ! working storage
%INCLUDE ADX_UPGM:EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE ADX_UPGM:EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE ADX_UPGM:EAMTOPTS.J86	         ! Variables Terminal Options
%INCLUDE ADX_UPGM:EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE ADX_UPGM:ASCITEMR.J86          ! Variables ITEMR
%INCLUDE ADX_UPGM:EXIRTSVA.J86          ! Variables Extension Itemr
%INCLUDE IVASTSVA.011
%INCLUDE FISTSUVA.011	                ! Variables desarrollo
%INCLUDE ADX_UPGM:EAMTSXHC.J86          !
%INCLUDE ADX_UPGM:EAMASMRT.J86          !

Sub TSPREC01  External			! Rutina de impresion SMA
END Sub

Sub TSPREC03  External			! Rutima de MSG SMA
END Sub

Sub TSHIECET External		    	!  Tono de alerta
END Sub

SUB TSCSEC10 EXTERNAL                 	! Termination procedure
END SUB

Sub CUADRE.CAJERO external 
End Sub 

Sub TRADUCE.ERROR(ERRVALUE%,ERRVALUE$) External						! Traduccion Error Aplicacion
  Integer*4 ERRVALUE%
  String ERRVALUE$
End Sub

FUNCTION PRT4610.FONT.AND.FORMAT$(ASTR$,VALUE) EXTERNAL
INTEGER*1 VALUE
STRING ASTR$,PRT4610.FONT.AND.FORMAT$
END FUNCTION

Function FORMAT.AMOUNT(AMT1) External 							! Formateo de valores
 Integer*1 FORMAT.AMOUNT
 Integer*4 AMT1
END Function

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   							! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

Function Asic.Datos$(Umsg1$,Umsg2$) External     														! Captura De Datos
String Asic.Datos$, Umsg1$, Umsg2$
End Function 

Function ASIC.DATOS.BASE$(UMSG1$,UMSG2$) External							! Captura de datos
String UMSG1$, UMSG2$, A.DAT$, AB$, AQ$								        ! Definicion de variables
String A1$, A2$, A3$, A4$, A5$, A6$,A7$, A8$, A9$
String ASIC.DATOS.BASE$, ADATA$			!
End Function 

Function U.Imprime(UE.String$,UE.STATION) External					! Rutina de Impresion
String UE.String$      									! Variables temporales
Integer*2 UE.STATION									!
End Function
!--- Rutina de Impresion

Function U.CORTACR External								! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

Function Grabacion.Cadena.Usuario(X.clave$,X.datos$) External
String X.clave$, X.datos$
End Function
!--- Fin Grabacion String Usuario

Function Linea.Detalle(X.Ses%)   External                  ! Imprime Detalle De La Operacion
String Linea.Detalle, X.Oper$, X.A$
Integer*1 X.Ses%
End Function 

Function ASIC.GETUNPK(X.B$,X.J%)
Integer*2 X.J%, X.K%
String    X.B$, ASIC.GETUNPK
  X.K% = MATCH(":",X.B$,X.J%) 				! SEARCH FOR FIELD SEPERATOR
  ASIC.GETUNPK = UNPACK$(MID$(X.B$,X.J%,X.K%-X.J%)) 	! UNPACK FIELD
  Asc.Fis.Fisj% = X.K% + 1
End Function

Sub VISOR.AND.BORRAR(X.MSG$) External
  String X.MSG$
End Sub 

Function Format.Fecha(X.Fecha$) External
String X.Fecha$, Format.Fecha
End Function 


!-- Lectura Datos en los Hard Totals
Sub FIS.LEC16                                          ! 
  FIS.F16$ = "3C4 C6 C2 C40"
  TS.ER.RETURN = -1 
  Read  Form FIS.F16$;#FIS.HTO%,FIS.H16%;             \!
             Asc.Fis.HTNtaVta$ ,                      \! Nota de Venta
             Asc.Fis.HTFacVta$ ,                      \! Factura de Venta
             Asc.Fis.HTNtaCred$,                      \! Nota de Credito
             Asc.Fis.FecTrx$   ,                      \! Fecha de Transaccion
             Asc.Fis.Almacen2$ ,                      \! Almacen de la numeracion
             Asc.Fis.Relleno$                          !
   If TS.ER.RETURN <> -1 Then Begin                    ! Error de Lectura Hard Totals 
      Asc.Fis.HTNtaVta$  = Pack$("00")                 ! Nota de Venta
      Asc.Fis.HTFacVta$  = Pack$("00")                 ! Factura de Venta
      Asc.Fis.HTNtaCred$ = Pack$("00")                 ! Nota de Credito
      Asc.Fis.Almacen2$  = Pack$("0000")               ! Almacen de la numeracion
   EndIf 
End Sub                                                !

Sub IMP.FIS.LEC16
    Call U.IMPRIME("--- DATOS HARD TOTAL TERMINAL ---",6100H)
    Call U.IMPRIME("FACT VENTA  :"+UnPack$(Asc.Fis.HTFacVta$),6100H)
    Call U.IMPRIME("NOTA VENTA  :"+UnPack$(Asc.Fis.HTNtaVta$),6100H)
    Call U.IMPRIME("NOTA CRED.  :"+UnPack$(Asc.Fis.HTNtaCred$),6100H)
    Call U.IMPRIME("ALMACEN     :"+UnPack$(Asc.Fis.Almacen2$),6100H)
    Call U.IMPRIME(" ",4200H)
    !Call U.CORTACR
End Sub                                  
                                         
                                         
!--- Procedimiento de grabacion en los Hard Totals
Sub FIS.WRIF  
String FECHA.TRX$                                         !
    FECHA.TRX$ = "20" + DATE$ + TIME$                  ! Agrega Fecha y Hora
    FECHA.TRX$ = Pack$(FECHA.TRX$)                     ! Empaqueta Fecha
    Asc.Fis.HTNtaVta$  = Pack$(Right$("00000000"+Str$(Asc.Fis.NotaNum%),8))
    Asc.Fis.HTFacVta$  = Pack$(Right$("00000000"+Str$(Asc.Fis.FacNum%),8))
    Asc.Fis.HTNtaCred$ = Pack$(Right$("00000000"+Str$(Asc.Fis.NotaCrd%),8))
    Asc.Fis.Almacen2$  = Asc.Fis.Almacen1$ 
    Asc.Fis.Relleno$   = String$(40,"0") 
    FIS.F16$ = "3C4 C6  C2 C40"
    Write Form FIS.F16$;#FIS.HTO%,FIS.H16%;           \!
             Asc.Fis.HTNtaVta$ ,                      \! Nota de Venta
             Asc.Fis.HTFacVta$ ,                      \! Factura de Venta
             Asc.Fis.HTNtaCred$,                      \! Nota de Credito
             Asc.Fis.FecTrx$   ,                      \! Fecha de Transaccion
             Asc.Fis.Almacen2$ ,                      \! Almacen de la numeracion
             Asc.Fis.Relleno$                          !
End Sub                                  

Sub Reset.HARD.TOTALS
String FECHA.TRX$                                         !
    FECHA.TRX$ = "20" + DATE$ + TIME$                  ! Agrega Fecha y Hora
    FECHA.TRX$ = Pack$(FECHA.TRX$)                     ! Empaqueta Fecha
    Asc.Fis.HTNtaVta$  = Pack$("00000000")
    Asc.Fis.HTFacVta$  = Pack$("00000000")
    Asc.Fis.HTNtaCred$ = Pack$("00000000")
    Asc.Fis.Almacen2$  = Asc.Fis.Almacen1$ 
    Asc.Fis.Relleno$   = String$(40,"0") 
    FIS.F16$ = "3C4 C6  C2 C40"
    Write Form FIS.F16$;#FIS.HTO%,FIS.H16%;           \!
             Asc.Fis.HTNtaVta$ ,                      \! Nota de Venta
             Asc.Fis.HTFacVta$ ,                      \! Factura de Venta
             Asc.Fis.HTNtaCred$,                      \! Nota de Credito
             Asc.Fis.FecTrx$   ,                      \! Fecha de Transaccion
             Asc.Fis.Almacen2$ ,                      \! Almacen de la numeracion
             Asc.Fis.Relleno$                          !
End Sub 


Function Valida.Ruc.Pagare(X.Nit$)
String X.Nit$, X.Lec$, Valida.Ruc.Pagare
  TS.ER.RETURN = -1   
  OPEN "R::RUC" KEYED RECL 80 AS Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     VALIDA.RUC.PAGARE = "CLIENTE NO DEFINIDO"
     Exit Function
  EndIf 
  X.Nit$ = PACK$(RIGHT$("00000000000000"+X.Nit$,14)) 
  X.LEC$="C7 C30 C30 C10 C1 C2"	   !
  READ FORM X.LEC$;#Asc.Fis.FileClte% KEY X.Nit$;   \! Lee Reg 
                  Asc.Fis.Cliente$,                 \! Identificacion del cliente
                  Asc.Fis.Nombre$,                  \! Nombre del cliente
                  Asc.Fis.Direccion$,               \! Direccion del Cliente
                  Asc.Fis.Telefono$,                \! Telefono del cliente
                  Asc.Fis.Estatal$,                 \! Si es empresa estatal
                  Asc.Fis.Filler                     ! Uso futuro             

  Close Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     VALIDA.RUC.PAGARE = "CLIENTE NO DEFINIDO"
     Exit Function 
  EndIf 
  VALIDA.RUC.PAGARE = Asc.Fis.Nombre$
End Function
!--- Fin validacion del RUC

Sub BUSCA.PAGARE                 ! Impresion del pagare x vta institucional

Integer*4 PRIC%, TOT4%, X.I%
String    VENTA.TIQUETE, X.LISTA$, X.TMP$, X.Vlr$
For X.I% = 1 TO Asc.Fis.Slend%                      ! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 ! GET String
    IF LEN(H$) > 5 THEN                    \! ASSURE GOOD String
     IF ASC(H$)  = 5 THEN BEGIN             ! ITEM ENTRY String
        Asc.Fis.Fisj% = 3
        X.TMP$ = ASIC.GETUNPK(H$,Asc.Fis.Fisj%)	        ! Tomo informacion del item vendido
        If X.TMP$ = Asc.Fis.Pagare$ Then Begin          ! Impresion del pagare
           X.Vlr$ = ASIC.GETUNPK(H$,Asc.Fis.Fisj%)	    ! Valor del pago
           X.Tmp$ = ASIC.GETUNPK(H$,Asc.Fis.Fisj%)	    ! data
           X.Tmp$ = ASIC.GETUNPK(H$,Asc.Fis.Fisj%)	    ! Numero de cuenta
           Call U.IMPRIME(Asc.Fis.Linea1$,4100H)
           Call U.IMPRIME(Asc.Fis.Linea2$,4100H)
           Call U.IMPRIME(Asc.Fis.Linea3$,4100H)
           Call U.IMPRIME(Asc.Fis.Linea4$,4100H)
           Call U.IMPRIME(Asc.Fis.Linea5$,4100H)
           Call U.IMPRIME(Asc.Fis.Linea6$,4100H)           
           TS.TEMP3$ = CHR$(1BH)+CHR$(21H)+CHR$(11H)
           Call U.Imprime("***********  P A G A R E  ***********",4100H)
           Call U.IMPRIME(Asc.Fis.Documento$,4100H)
           TS.TEMP3$ = "Cliente :"+X.Tmp$
           TS.TEMP3$ = Left$(TS.TEMP3$+String$(40," "),40)
           Call U.IMPRIME(TS.TEMP3$,4100H)
           TS.TEMP3$ = VALIDA.RUC.PAGARE(X.TMP$)
           TS.TEMP3$ = Left$(TS.TEMP3$+String$(40," "),40)
           Call U.IMPRIME(TS.TEMP3$,4300H)           
           TS.TEMP3$ =  Asc.Fis.LineaB$
           TS.TEMP3$ = Left$(TS.TEMP3$+String$(40," "),40)
           Call U.IMPRIME(TS.TEMP3$,4100H)
           Call Format.AMOUNT(Val(X.VLR$))
           TS.TEMP3$ = "LA CANTIDAD DE USD "+TS.TEMP1$
           TS.TEMP3$ = Left$(TS.TEMP3$+String$(40," "),40)
           Call U.IMPRIME(TS.TEMP3$,4100H)           
           TS.TEMP3$ = "POR CONCEPTO DE COMPRAS A CREDITO   "
           TS.TEMP3$ = Left$(TS.TEMP3$+String$(40," "),40)
           Call U.IMPRIME(TS.TEMP3$,4100H)           
           TS.TEMP3$ = LINEA.DETALLE(38)
           Call U.IMPRIME(TS.TEMP3$,4300H)
           Call U.IMPRIME("__________________________________",4100H)
           Call U.IMPRIME("         FIRMA CLIENTE            ",4100H)
           CALL U.IMPRIME("",4500H)
           Call U.CORTACR 
           Call U.IMPRIME(TO.HEADERLINE1$,4100H)
           Call U.IMPRIME(TO.HEADERLINE2$,4100H)
        EndIf 
     ENDIF                                  ! ITEM ENTRY String
Next X.I%                                 ! NEXT String

End Sub 

Sub Lectura.Consecutivo.Fiscal
String X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.I$, X.F$, X.Tipo$, X.RESOL$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%
       X.KEY$ = RIGHT$("0000"+TS.TERMINAL$,4)															!
       X.KEY$ = PACK$(X.KEY$)																							!
       X.LEC$="C2 3C15 C4 2I4 C6 C3 3I4"	                                !
       TS.ER.RETURN = -1 
       Read FORM X.LEC$;#Asc.Fis.Sesion% KEY X.KEY$;            \! Busca Terminal
            X.KEY$, 					 	\! Numero de la terminal
            X.TIPO$,    			     		\! Tipo de Maquina
            X.SERIAL$,  			     		\! Serial de la terminal
            X.RESOL$,						\! Numero resolucion
            X.PREF$, 						\! Prefijo de facturacion
            X.INI%, 						\! Numero inicial de Facturacion
            X.FIN%, 						\! Rango final de Facturacion
            X.AUT$, 						\! Numero resolucion de la DIAN
            X.FEC$, 						\! Fecha Autorizacion
            Asc.Fis.FacNum%, 					\! Numero Actual de Facturacion
            Asc.Fis.NotaNum%,            	 		\! Numero de Nota de Venta
            Asc.Fis.NotaCrd%                                     ! Numero de Nota de Credito
                                                                                                                      
       If TS.ER.RETURN <> -1 Then Begin                                                                            
            Asc.Fis.FacNum%  = 0				 ! Numero Actual de Facturacion
            Asc.Fis.NotaNum% = 0            	 		 ! Numero de Nota de Venta
            Asc.Fis.NotaCrd% = 0                                 ! Numero de Nota de Credito
       EndIf 
           
End Sub 

Sub Asignacion.Consecutivo.Fiscal
    Call Lectura.Consecutivo.Fiscal                              ! Lectura Consecutivos Archivo Controlador
    Call FIS.LEC16                                               ! Lectura Consecutivos Hard Totals 
    
    Asc.Fis.HTNtaVta$  =  UnPack$(Asc.Fis.HTNtaVta$)
    Asc.Fis.HTFacVta$  =  UnPack$(Asc.Fis.HTFacVta$) 
    Asc.Fis.HTNtaCred$ =  UnPack$(Asc.Fis.HTNtaCred$)
    
    If Val(Asc.Fis.HTNtaVta$) > Asc.Fis.NotaNum% Then Begin      ! Si consecutivo Nta Vta de HT mayor al archivo
       Asc.Fis.NotaNum% = Val(Asc.Fis.HTNtaVta$) 
    EndIf 
    
    If Val(Asc.Fis.HTFacVta$) > Asc.Fis.FacNum% Then Begin       ! Si consecutivo Fact Vta de HT mayor al archivo
       Asc.Fis.FacNum% = Val(Asc.Fis.HTFacVta$) 
    EndIf 

    If Val(Asc.Fis.HTNtaCred$) > Asc.Fis.NotaCrd% Then Begin     ! Si consecutivo Nta Crd de HT mayor al archivo
       Asc.Fis.NotaCrd% = Val(Asc.Fis.HTNtaCred$) 
    EndIf 

End Sub 

Function Validacion.Fecha.Fiscal(X.Fec$)              										! Validacion fecha autorizacion fiscal
 String X.Fec$
 Integer*1  Validacion.Fecha.Fiscal
 Integer*4 X.A%, X.B%

 X.A% = Val( Left$(X.FEC$,2) )																						! Toma año autorizacion
 X.B% = Val( Left$(DATE$,2) )				  																		! Toma año movimiento
 
 If X.A% = X.B% Then Begin  																							! Si el año coincide
    X.A% = Val( Mid$(X.FEC$,3,2) )																				! Toma mes autorizacion
    X.B% = Val( Mid$(DATE$,3,2) )																				  ! Toma mes movimiento
    If X.B% <= X.A% Then Begin 			 																	    ! Si el mes es menor o igual al autorizado
       Validacion.Fecha.Fiscal = -1                                       ! Continua ejecucion programa
       Exit Function     																									! Sale de la funcion
    EndIf Else Begin 																											! Si mes mayor o igual
       Validacion.Fecha.Fiscal = 0                                        ! Para ejecucion programa
       Exit Function               																				! Sale de la funcion      
    EndIf 
 EndIf  
 
 If X.A% > X.B% Then Begin  																							! Si el año fiscal mayor al registro
   If (X.A% - X.B%) > 1 Then Begin                                        ! Diferencia de mas de un año
       Validacion.Fecha.Fiscal = 0                                        ! Para ejecucion programa
       Exit Function               																				! Sale de la funcion           
   EndIf 
   Validacion.Fecha.Fiscal = -1                                           ! Para ejecucion programa
   Exit Function               																				    ! Sale de la funcion     
 EndIf Else Begin 
   Validacion.Fecha.Fiscal = 0                                            ! Para ejecucion programa
   Exit Function               																				    ! Sale de la funcion     
 EndIf 
 
End Function 
!--- Fin verificacion fecha fiscal

Sub Carga.Parametros.Terminal
String X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.MES$(1), X.I$, X.F$, X.Tipo$, X.RESOL$, X.FEC2$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%
integer*1  X.Valida%
    Dim X.MES$(12) 
    X.MES$(01) = "Ene"
    X.MES$(02) = "Feb"
    X.MES$(03) = "Mzo"
    X.MES$(04) = "Abr"
    X.MES$(05) = "May"
    X.MES$(06) = "Jun"
    X.MES$(07) = "Jul"
    X.MES$(08) = "Ago"
    X.MES$(09) = "Sep"
    X.MES$(10) = "Oct"
    X.MES$(11) = "Nov"
    X.MES$(12) = "Dic"
    TS.ER.RETURN = -1 
    If Asc.Ind.Open.Fiscal% = 0 Then Begin
       OPEN "R::EAMNMFIS" KEYED RECL 80 AS Asc.Fis.Sesion%    							! Abre control numeacion fiscal
       Asc.Ind.Open.Fiscal% = -1
    EndIf 
    
    If TS.ER.RETURN = -1 Then Begin                   								! Si apertura OK
       X.KEY$ = RIGHT$("0000"+TS.TERMINAL$,4)															!
       X.KEY$ = PACK$(X.KEY$)																							!
       X.Lec$ = "C2 3C15 C4 2I4 C6 C3 3I4"	                              !
       Read FORM X.LEC$;#Asc.Fis.Sesion% KEY X.KEY$;            \! Lee Reg Cabecera 
            X.KEY$, 																										 \! Numero de la terminal
            X.TIPO$,    																						     \! Tipo de Maquina
            X.SERIAL$,  																						     \! Serial de la terminal
            X.RESOL$,																										 \! Numero resolucion
            X.PREF$, 																										 \! Prefijo de facturacion
            X.INI%, 																										 \! Numero inicial de Facturacion
            X.FIN%, 																										 \! Numero final de Facturacion
            X.AUT$, 																										 \! Numero resolucion
            X.FEC$, 																										 \! Fecha Autorizacion
            X.ACT%,																											 \! Numero Actual de Facturacion
            X.NOTA%,                                                     \! Numero actual de Nota de Venta
            X.NC%																									        ! Numero actual de notas credito
            
       If TS.ER.RETURN = -1 Then Begin
          X.FEC$ = Unpack$(X.FEC$)
          X.FEC2$ = X.FEC$
          X.AUT$ = Unpack$(X.AUT$)
          X.AUT$ = STR$(Val(X.AUT$))
          X.AUT$ = Right$("            "+X.AUT$,12)
          X.I$   = Right$("0000000000"+Str$(X.INI%),10)
          X.F$   = Right$("0000000000"+Str$(X.FIN%),10)
          !X.FEC$ = X.MES$(Val(MID$(X.FEC$,3,2)))+"/"+Right$(X.FEC$,2)+"/"+"20"+Left$(X.FEC$,2)
          ASC.FIS.TMPPREF$ = X.PREF$
          Asc.Fis.Resol$   = "RESOLUCION NRO : "+X.RESOL$										! Numero resolucion
          Asc.Fis.Autor$   = "AUTORIZACION S.R.I : "+X.AUT$		              ! Numero Autorizacion
          Asc.Fis.NtaVta$  = "   NOTA DE VENTA   : "+X.Pref$	 							! Nota de venta
          Asc.Fis.NtaCrd$  = "   NOTA DE CREDITO : "+X.Pref$	 							! Nota de credito
          Asc.Fis.Factura$ = "      FACTURA      : "+X.Pref$		  					! Factura de venta
          Asc.Fis.Fecven$  = "VALIDO HASTA EL    : "+X.Fec$                 ! Fecha de vencimiento
          Asc.Fis.Serial$  = X.TIPO$+" SERIE:"+X.SERIAL$										! Numero serial de la terminal
          Asc.Fis.Serial$  = Left$(Asc.Fis.Serial$+String$(40," "),40)
          Asc.Fis.Autor$   = Left$(Asc.Fis.Autor$+String$(40," "),40)
          Asc.Fis.FecVen$  = Left$(Asc.Fis.Fecven$+String$(40," "),40)
          Asc.Fis.FacNum%  = X.ACT%
          Asc.Fis.NotaNum% = X.NOTA%
          Asc.Fis.NotaCrd% = X.NC%
          
          Call U.Imprime("--- Datos Numeracion Fiscal ---",4100H)
          Call U.Imprime("Numero de Terminal :"+TS.TERMINAL$,6100H)
          Call U.Imprime(Asc.Fis.Resol$,4100H)
          Call U.Imprime(Asc.Fis.Fecven$,4100H)
          Call U.Imprime(Asc.Fis.Serial$,4100H)
          Call U.Imprime("Factura Inicial :"+Str$(Asc.Fis.FacNum%),6100H)
          Call U.Imprime("Nta Vta Inicial :"+Str$(Asc.Fis.NotaNum%),6100H)
          Call U.Imprime("Nta Crd Inicial :"+Str$(Asc.Fis.NotaCrd%),6100H)
          X.Valida% = Validacion.Fecha.Fiscal(X.FEC2$)                      ! Add 1-Dic-2005 OVS
          If X.Valida% <> -1 Then Begin 																		! Si fecha erronea de autorizacion
             Call TSHIECET : Call TSHIECET : Call TSHIECET                                                 ! Tono de Alerta
             Call VISORES4690(1,"FECHA FUERA DE RANGO","DE AUTOR. FISCAL",0,"L")
             Call VISORES4690(2,"FECHA FUERA DE RANGO","DE AUTOR. FISCAL",0,"L")
             Stop                                                           ! Para la terminal
          EndIf                                                             !
       EndIf Else Begin
          Call U.Imprime("Terminal No Definida con Numeracion",6100H)
          Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")
          Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")
          Stop
       EndIf 
    EndIf Else Begin
          Call U.Imprime("Error Apertura Archivo Numeracion Fiscal",6100H)
          Call U.Imprime("Terminal No Definida con Numeracion",6100H)
          Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")
          Call VISORES4690(1,"TERMINAL BLOQUEADA","",0,"L")
          Stop
    EndIf 
End Sub

Sub Calculo.ReteIva
Integer*4 X.I%
Real X.Base%, X.Total%, X.SinIva%
  X.Base% = 0
  X.Total% = 0
  X.SinIva% = 0
  For X.I% = 1 to 8 
      X.SinIva% = ROUND( X.SinIva%  + ROUND(FLOAT(UE.TOTALS(X.I%))/(1.+FLOAT(UE.TABLAIVA(X.I%))/100.),0,0) ,2,0)
      X.Total%  = X.Total% + UE.TOTALS(X.I%)
  Next X.I%
  X.Base% =  X.Total% - X.SinIva%
  Asc.Fis.VlrIva% = Round( (X.Base% / 100) * Asc.Fis.PtgIva% ,2,0)
  Call Format.Amount(X.Base%)
  TS.USER5$ = TS.TEMP1$
End Sub

Sub Calculo.ReteIva.Exportador
Integer*4 X.I%
Real X.Base%, X.Total%, X.SinIva%
  X.Base% = 0
  X.Total% = 0
  X.SinIva% = 0
  For X.I% = 1 to 8 
      X.SinIva% = ROUND( X.SinIva%  + ROUND(FLOAT(UE.TOTALS(X.I%))/(1.+FLOAT(UE.TABLAIVA(X.I%))/100.),0,0) ,2,0)
      X.Total%  = X.Total% + UE.TOTALS(X.I%)
  Next X.I%
  X.Base% =  X.Total% - X.SinIva%
  Asc.Fis.VlrIva% = Round( (X.Base% / 100) * Asc.Fis.PtgIva2% ,2,0)
  Call Format.Amount(X.Base%)
  TS.USER5$ = TS.TEMP1$
End Sub

Sub Calculo.Retefuente
Integer*4 X.I%
Real X.Base%
  X.Base% = 0
  For X.I% = 1 to 8 
      X.Base%  = ROUND( X.Base%  + ROUND(FLOAT(UE.TOTALS(X.I%))/(1.+FLOAT(UE.TABLAIVA(X.I%))/100.),0,0) ,2,0)
  Next X.I%
  Asc.Fis.VlrFte% = Round( (X.Base% / 100) * Asc.Fis.PtgFte% ,2,0)
  Call Format.Amount(X.Base%)
  TS.USER5$ = TS.TEMP1$
End Sub

Function GRABA.CONSECUTIVO.FISCAL
String X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.TIPO$, X.RESOL$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%
       X.KEY$ = RIGHT$("0000"+TS.TERMINAL$,4)
       X.KEY$ = PACK$(X.KEY$)
       X.LEC$="C2 3C15 C4 2I4 C6 C3 3I4"	                 ! Formato Grabacion Registro
       
       Read FORM X.LEC$;#Asc.Fis.Sesion% KEY X.KEY$;            \! Busca Terminal
            X.KEY$, 																										 \! Numero de la terminal
            X.TIPO$,    																						     \! Tipo de Maquina
            X.SERIAL$,  																						     \! Serial de la terminal
            X.RESOL$,																										 \! Numero resolucion
            X.PREF$, 																										 \! Prefijo de facturacion
            X.INI%, 																										 \! Numero inicial de Facturacion
            X.FIN%, 																										 \! Rango final de Facturacion
            X.AUT$, 																										 \! Numero resolucion de la DIAN
            X.FEC$, 																										 \! Fecha Autorizacion
            X.ACT%, 																										 \! Numero Actual de Facturacion
            X.NOTA%,            																				 \! Numero de Nota de Venta
            X.NC%                                                         ! Numero de Nota Credito 
       
       Write Form X.LEC$;#Asc.Fis.Sesion%;                      \! Graba numero de terminal
            X.KEY$, 						\! Numero de la terminal
            X.TIPO$,    			     		\! Tipo de Maquina
            X.SERIAL$,  			     		\! Serial de la terminal
            X.RESOL$,						\! Numero resolucion
            X.PREF$, 						\! Prefijo de facturacion
            X.INI%, 						\! Numero inicial de Facturacion
            X.FIN%, 						\! Rango final de Facturacion
            X.AUT$, 						\! Numero resolucion de la DIAN
            X.FEC$, 						\! Fecha Autorizacion
            Asc.Fis.FacNum%, 					\! Numero Actual de Facturacion
            Asc.Fis.NotaNum%,            	 		\! Numero de Nota de Venta
            Asc.Fis.NotaCrd%                                     ! Numero de Nota de Credito
       Call FIS.WRIF                                             ! Grabacion datos en los Hard Totals
End Function 

!---- Grabacion del movimiento diario
Sub GRABAR.MOVIMIENTO.DIARIO(X.TIPO%,X.DOC$,X.VLR1%)
Integer*1 X.TIPO%
String    X.DOC$
String    X.LLAVE$,X.FECHA$, X.TRX$, X.DATA$
Integer*4 X.VLR1%, X.VLR2%
  TS.ER.RETURN = -1 
  Open "R::EAMMVFIS" KEYED RECL 27 AS Asc.Fis.FileClte%
  If TS.ER.RETURN = -1 Then Begin      ! Apertura correcta
  
     X.LLAVE$ = Right$("0000"+TS.TERMINAL$,4)    + \! Numero de la terminal
                Right$("0"+Str$(X.TIPO%),1)      + \! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
                Right$("0000000"+X.DOC$,7)          ! Numero del documento
     X.VLR2%  = 0 
     Gr.Bon.DatFis$ = X.Llave$			    ! Dato Fiscal para Bonos Regalo
     X.LLAVE$ = Pack$(X.LLAVE$)         
     X.FECHA$ = Pack$(DATE$)
     X.DATA$  = ""
     X.TRX$   = Pack$(Right$("0000"+Str$(SL.HD.TRANSNUM),4))
     
     Write Form "C6 C3 C2 2I4 C8"; #Asc.Fis.FileClte%;    \! 
               X.LLAVE$,       \! Llave 
               X.FECHA$,       \! Fecha movimiento
               X.TRX$,         \! Trx del movimiento
               X.VLR1%,        \! Valor de la compra
               X.VLR2%,        \! Valor usuado
               X.DATA$          ! Uso futuro
               
      Gr.Bon.ActFis% = -1       ! Reporta bonos regalo act fiscal        
  EndIf Else Begin                   ! Error apertura archivo
     X.DATA$ = Pack$(Right$("0000"+TS.TERMINAL$,4))       + \! Numero de la terminal
            ":" + Pack$(Right$("0"+Str$(X.TIPO%),1))      + \! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
            ":" + Pack$(Right$("0000000"+X.DOC$,7))       + \! Numero del documento
            ":" + Pack$(Date$)                            + \! Fecha del movimiento
            ":" + Pack$(Str$(SL.HD.TRANSNUM))             + \! Numero de transaccion 
            ":" + Pack$(Str$(X.Vlr1%))                    + \! Valor del movimiento
            ":" + Pack$("00")                                ! Filler
     Call Grabacion.Cadena.Usuario("20080811",X.Data$)
     X.LLAVE$ = Right$("0000"+TS.TERMINAL$,4)    + \! Numero de la terminal
                Right$("0"+Str$(X.TIPO%),1)      + \! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
                Right$("0000000"+X.DOC$,7)          ! Numero del documento
     Gr.Bon.DatFis$ = X.Llave$			    ! Dato Fiscal para Bonos Regalo     
     Gr.Bon.ActFis% = -1                    ! Reporta bonos regalo act fiscal
     Exit Sub 
     
  EndIf 
  Close Asc.Fis.FileClte%   
   
End Sub
!---

!--- Actualiza los saldos de movimientos para las notas credito
!---
Sub ACTUALIZA.SALDO.MOVIMIENTO           
String    X.LLAVE$,X.FECHA$, X.TRX$, X.DATA$
Integer*4 X.VLR1%, X.VLR2%
  TS.ER.RETURN = -1 
  Open "R::EAMMVFIS" KEYED RECL 27 AS Asc.Fis.FileClte%
  If TS.ER.RETURN = -1 Then Begin      ! Apertura correcta
     X.LLAVE$ = Right$("0000"+Asc.Fis.TermNc$,4)    + \! Numero de la terminal
                Right$("0"+Asc.Fis.TipoNc$,1)       + \! Tipo de documento 1.Factura 3. Nota Vta
                Right$("0000000"+Asc.Fis.ConsNc$,7)    ! Numero del documento

     X.LLAVE$ = Pack$(X.LLAVE$)         
     Read Form "C6 C3 C2 2I4 C8"; #Asc.Fis.FileClte% KEY X.LLAVE$;    \! 
               X.LLAVE$,       \! Llave 
               X.FECHA$,       \! Fecha movimiento
               X.TRX$,         \! Trx del movimiento
               X.VLR1%,        \! Valor de la compra
               X.VLR2%,        \! Valor usuado
               X.DATA$          ! Uso futuro
               
     If TS.ER.RETURN = -1 Then Begin ! Lectura Correcta
        X.VLR2% = X.VLR2% + (TS.TOTALS(0,0,0) * -1)
        Write Form "C6 C3 C2 2I4 C8"; #Asc.Fis.FileClte%;    \! 
               X.LLAVE$,       \! Llave 
               X.FECHA$,       \! Fecha movimiento
               X.TRX$,         \! Trx del movimiento
               X.VLR1%,        \! Valor de la compra
               X.VLR2%,        \! Valor usuado
               X.DATA$          ! Uso futuro
     EndIf 
  EndIf 
  Close Asc.Fis.FileClte%
End Sub 

!--- Valida si el numero de factura ya esta registrado en el sistema
!--- este bien puede ser del dia corriente o de dias anteriores

Function VALIDAR.MOVIMIENTO(Xterm$, Xtipo$, Xdoc$)
Integer*1 VALIDAR.MOVIMIENTO
String    X.LLAVE$,X.FECHA$, X.TRX$, X.DATA$,Xterm$, Xtipo$, Xdoc$
Integer*4 X.VLR1%, X.VLR2%
  Asc.Fis.VlrNta% = 0                          ! Valor de la nota
  Asc.Fis.SalNta% = 0                          ! Saldo de la nota
  TS.ER.RETURN = -1 
  Open "R::EAMMVFIS" KEYED RECL 27 AS Asc.Fis.FileClte%
  If TS.ER.RETURN = -1 Then Begin      ! Apertura correcta
  
     X.LLAVE$ = Right$("0000"+Xterm$,4)    + \! Numero de la terminal
                Right$("0"+Xtipo$,1)       + \! Tipo de documento 1.Factura 3. Nota Vta
                Right$("0000000"+Xdoc$,7)     ! Numero del documento

     X.LLAVE$ = Pack$(X.LLAVE$)         
     X.FECHA$ = Pack$(DATE$)
     Read Form "C6 C3 C2 2I4 C8"; #Asc.Fis.FileClte% KEY X.LLAVE$;    \! 
               X.LLAVE$,       \! Llave 
               X.FECHA$,       \! Fecha movimiento
               X.TRX$,         \! Trx del movimiento
               X.VLR1%,        \! Valor de la compra
               X.VLR2%,        \! Valor usuado
               X.DATA$          ! Uso futuro
               
     If TS.ER.RETURN = -1 Then Begin ! Lectura Correcta
        Asc.Fis.VlrNta% = X.Vlr1%                          ! Valor de la nota
        Asc.Fis.SalNta% = X.VLR1% - X.VLR2%                ! Saldo de la nota
        If Asc.Fis.SalNta% <= 0 Then Begin 
           Call VISOR.AND.BORRAR("DOCUMENTO REVERSADO EN SU TOTALIDAD")
           Asc.Fis.VlrNta% = 0                          ! Valor de la nota
           Asc.Fis.SalNta% = 0                          ! Saldo de la nota           
           VALIDAR.MOVIMIENTO = 0
        EndIf Else VALIDAR.MOVIMIENTO = -1
     EndIf Else Begin                ! No existe movimiento
        VALIDAR.MOVIMIENTO = 0
     EndIf 
  EndIf Else Begin                   ! Error apertura archivo
     VALIDAR.MOVIMIENTO = 0
     Exit Function 
  EndIf 
  Close Asc.Fis.FileClte%
End Function 
!--- 

Sub Valida.Ruc(X.Nit$)
String X.Nit$, X.Lec$
Integer*1 X.VALIDA%
  TS.ER.RETURN = -1   
  OPEN "R::RUC" KEYED RECL 80 AS Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     Call TSHIECET
     Call Visores4690(1,"Error Apertura Archivo","Cliente No Validado",1800,"L")  
     Asc.Fis.IndFac% = 0
     Exit Sub 
  EndIf 
  X.Nit$ = PACK$(RIGHT$("00000000000000"+X.Nit$,14)) 
  X.LEC$="C7 C30 C30 C10 C1 C2"	   !
  READ FORM X.LEC$;#Asc.Fis.FileClte% KEY X.Nit$;   \! Lee Reg 
                  Asc.Fis.Cliente$,                 \! Identificacion del cliente
                  Asc.Fis.Nombre$,                  \! Nombre del cliente
                  Asc.Fis.Direccion$,               \! Direccion del Cliente
                  Asc.Fis.Telefono$,                \! Telefono del cliente
                  Asc.Fis.Estatal$,                 \! Si es empresa estatal
                  Asc.Fis.Filler                     ! Uso futuro           

  Close Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     Call TSHIECET
     Call Visores4690(1,"Cliente No Definido","...",1800,"C")
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															 ! Init vectores de aplicacion
     TS.IO.MOTORKEY = 73 																										 ! Tecla borrar
     Asc.Fis.IndFac% = 0
     Exit Sub 
  EndIf 
!--- Add 7 Mayo 2008 OVS   
  If Asc.Fis.Estatal$ = "S" and TS.INTRX Then Begin 		! Si entidad estatal y en transaccion
     Call VISOR.AND.BORRAR("NO PERMITE CAPTURA  CLIENTE ESTATAL     ")
     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															 ! Init vectores de aplicacion
     TS.IO.MOTORKEY = 73 																										 ! Tecla borrar
     Asc.Fis.IndFac% = 0
     Asc.Fis.Estatal$ = "N"
     Exit Sub 
  EndIf 
  Call Visores4690(1,"ESTIMADO(A) CLIENTE",Asc.Fis.Nombre$,0,"L")
  Call Visores4690(2,"ESTIMADO(A) CLIENTE",Asc.Fis.Nombre$,1800,"L")
  TS.TEMP2$ = Asc.Fis.Cliente$ +            \! Dato del cliente
              ";"+"08"                       ! Filler             
  Call Grabacion.Cadena.Usuario("20050822",TS.TEMP2$)
  Asc.Fis.Cliente$ = STR$(Val(Unpack$(Asc.Fis.Cliente$)))
  Asc.Fis.IndFac% = -1
  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															 ! Init vectores de aplicacion
  TS.IO.MOTORKEY = 73 																										 ! Tecla borrar
End Sub 
!--- Fin validacion del RUC

Sub Valida.Ruc.Cheque.Si(X.Nit$)
String X.Nit$, X.Lec$, X.Name$
Integer*1 X.VALIDA%
  TS.ER.RETURN = -1   
  OPEN "R::RUC" KEYED RECL 80 AS Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     Asc.Fis.ClienteSi$ = STRING$(30,"_")
     Exit Sub 
  EndIf 
  X.Nit$ = PACK$(RIGHT$("00000000000000"+X.Nit$,14)) 
  X.LEC$="C7 C30 C30 C10 C1 C2"	   !
  READ FORM X.LEC$;#Asc.Fis.FileClte% KEY X.Nit$;   \! Lee Reg 
                  TS.TEMP1$       ,                 \! Identificacion del cliente
                  X.Name$         ,                  \! Nombre del cliente
                  TS.TEMP1$         ,               \! Direccion del Cliente
                  TS.TEMP1$        ,                \! Telefono del cliente
                  TS.TEMP1$       ,                 \! Si es empresa estatal
                  TS.TEMP1$                          ! Uso futuro           

  Close Asc.Fis.FileClte%
  If TS.ER.RETURN NE -1 Then Begin 
     X.Name$ = STRING$(30,"_")
  EndIf 
  Asc.Fis.ClienteSi$ = X.Name$
End Sub 
!--- Fin validacion del RUC

!- Mod13Ago2008  OVS Grupo Retail 

Function RETORNA.DOCUMENTO
String     RETORNA.DOCUMENTO, X.Nro.Term$, X.Nro.Trx$, X.Nro.Tipo$
String     X.Key$, X.SERIAL$, X.PREF$, X.AUT$, X.FEC$, X.LEC$, X.TIPO$, X.RESOL$
Integer*4  X.INI%, X.FIN%, X.ACT%, X.Nota%, X.NC%
Integer*1  X.VALIDA%

  ENTRADA.TIPO.ORIGEN:
  X.Nro.Tipo$ = ASIC.DATOS.BASE$("DOCUMENTO ORIGEN","1.FACTURA 3.NOTA VTA")
  If (X.Nro.Tipo$ = "E") Then Begin 
     Call VISOR.AND.BORRAR("PROCEDIMIENTO  CANCELADO....")
     RETORNA.DOCUMENTO = ""
     Asc.Fis.Anulmov%  = 0 
     Exit Function 
  EndIf 
  If (X.Nro.Tipo$ = "") Then BEGIN				     					     ! No se capturo dato
     Call VISOR.AND.BORRAR("TIPO DOCUMENTO NO   VALIDO..")
     GoTo ENTRADA.TIPO.ORIGEN
  EndIf 											
  If MATCH(X.Nro.Tipo$,"13",1) <= 0 Then Begin
     Call VISOR.AND.BORRAR("TIPO DOCUMENTO NO   VALIDO..")
     GoTo ENTRADA.TIPO.ORIGEN
  EndIf 

  ENTRADA.TERMINAL:           
  X.Nro.Term$ = ASIC.DATOS.BASE$("NUMERO TERMINAL","ORIGEN")
  If (X.Nro.Term$ = "E") Then Begin 
     Call VISOR.AND.BORRAR("PROCEDIMIENTO  CANCELADO....")
     RETORNA.DOCUMENTO = ""
     Asc.Fis.Anulmov%  = 0 
     Exit Function 
  EndIf 
  If (X.Nro.Term$ = "") THEN BEGIN				     					     ! No se capturo dato
     Call VISOR.AND.BORRAR("NUMERO DE TERMINAL  NO VALIDO..")
     GoTo ENTRADA.TERMINAL
  EndIf 																									

  ENTRADA.FACTURA:           
  X.Nro.Trx$ = ASIC.DATOS.BASE$("NUMERO DOCUMENTO","ORIGINAL")
  If (X.Nro.Trx$ = "E") Then Begin 
     Call VISOR.AND.BORRAR("PROCEDIMIENTO  CANCELADO....")
     RETORNA.DOCUMENTO = ""
     Asc.Fis.Anulmov%  = 0 
     Exit Function 
  EndIf 
  If (X.Nro.Trx$ = "") Then BEGIN				     					     ! No se capturo dato
     Call VISOR.AND.BORRAR("NUMERO DOCUMENTO    NO VALIDO..")
     GoTo ENTRADA.FACTURA
  EndIf 																																			 !                     
  If Val(X.Nro.Trx$) <= 0 Then Begin 
     Call VISOR.AND.BORRAR("NUMERO DOCUMENTO    NO VALIDO..")
     GoTo ENTRADA.FACTURA
  EndIf 
 X.Nro.Trx$ = Right$("0000000"+X.Nro.Trx$,7)

  Asc.Fis.TermNc$  = X.Nro.Term$  ! NUMERO DE TERMINAL
  Asc.Fis.TipoNc$  = X.Nro.Tipo$  ! TIPO DOCUMENTO
  Asc.Fis.ConsNc$  = X.Nro.Trx$   ! Numero de trx
 
       TS.ER.RETURN = -1 
       X.KEY$ = RIGHT$("0000"+X.Nro.Term$,4)															!
       X.KEY$ = PACK$(X.KEY$)																							!
       X.Lec$ = "C2 3C15 C4 2I4 C6 C3 3I4"	                       !
       Read FORM X.LEC$;#Asc.Fis.Sesion% KEY X.KEY$;            \! Lee Reg Cabecera 
            X.KEY$, 																										 \! Numero de la terminal
            X.TIPO$,    																						     \! Tipo de Maquina
            X.SERIAL$,  																						     \! Serial de la terminal
            X.RESOL$,																										 \! Numero resolucion
            X.PREF$, 																										 \! Prefijo de facturacion
            X.INI%, 																										 \! Numero inicial de Facturacion
            X.FIN%, 																										 \! Numero final de Facturacion
            X.AUT$, 																										 \! Numero resolucion
            X.FEC$, 																										 \! Fecha Autorizacion
            X.ACT%,																											 \! Numero Actual de Facturacion
            X.NOTA%,                                                     \! Numero actual de Nota de Venta
            X.NC%																									        !  
  If TS.ER.RETURN NE -1 Then Begin              
     Call VISOR.AND.BORRAR("TERMINAL NO DEFINIDACON NUMERACION FISCAL")
     GoTo ENTRADA.TIPO.ORIGEN
  EndIf 
  X.VALIDA% = VALIDAR.MOVIMIENTO(X.Nro.Term$,X.Nro.Tipo$,X.Nro.Trx$)        ! Valida si documento existe
  If X.VALIDA% = 0 Then Begin 
    Call VISOR.AND.BORRAR("MOVIMIENTO NO EXISTE EN EL SISTEMA")
    GoTo ENTRADA.TIPO.ORIGEN
  EndIf 
  
  X.Nro.Term$ = Right$("000"+X.Nro.Term$,3)				! Ajusta numero de terminal
  If Val(TS.TEMP1$) = 1 Then TS.TEMP1$ = "FACTURA NRO: " Else \
                             TS.TEMP1$ = "NOTA DE VENTA NRO: "
  Asc.Fis.Anulmov% = -1                              
  RETORNA.DOCUMENTO = TS.TEMP1$ + X.PREF$ + "-" + X.Nro.Term$ + "-" + X.Nro.Trx$

End Function 
!--- 


Sub FISMTS02.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU02.ECU                                         **
!   **    Inclu¡r en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

%Include FISTSU02.ECU    

Asc.Fis.ChequeSi$ = ""
Asc.Fis.ImpChequeSi% = 0
Asc.Fis.ClienteSi$ = ""
EndIf 

End Sub 



Sub FISMTS07.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU07.ECU                                         **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

 
%Include FISTSU07.ECU     ! Carga parametros Numeracion Fiscal

END Sub


Sub FISMTS14.012 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU14.ECU                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Xtmp1$, Xtmp2$, Xtmp3$

If Asc.Proy.Fiscal% = -1  Then Begin                         		! Proyecto activo


If Not TS.INTRX Then \																					! No esta dentro de una trx
 If TS.IO.KEYS(4) = 100 AND Val(TS.IO.DATA$(2)) = 80 Then \     ! Reporte de cuadre de caja
    Exit Sub 

If TS.IO.KEYS(4) = 100 AND Val(TS.IO.DATA$(2)) = 80  AND \ 
   TS.IO.KEYS(8) = 79 Then \                                    ! Intercambio de Gaveta
   Exit Sub 


If TS.IO.KEYS(4) = 100 AND TS.IO.DATA$(2) <> "" Then Begin 		  ! Consulta de Precio
   If TS.IO.DEVICE = 3 Then \																		! Si escanea producto
      TS.IO.DATA$(2) = Left$(TS.IO.DATA$(2),Len(TS.IO.DATA$(2)) - 1)
   Asc.ITEMCODE$ = Pack$(Right$("000000000000"+TS.IO.DATA$(2),12))
   TS.ER.RETURN = -1 
   If TS.STANDALONE Then Begin                             ! Si terminal OFFLINE 
     %INCLUDE EAMIRRD5.J86                                 ! Lectura de registro EAMIMAGE
   EndIf Else Begin                                        ! Si esta en linea
     %INCLUDE EAMIRRD4.J86                                 ! Lectura EAMITEMR
   EndIf 
   If TS.ER.RETURN = -1 Then Begin 												 ! Si lo encontre
      If Unpack$(Asc.Indicat2$) = "03" Then Begin 				 ! Con precio reducido
         XTMP1$ = Unpack$(Asc.SALEPRIC$)                   ! Desempaqueta precio
         Xtmp3$ = Left$(Xtmp1$,5)													 ! Toma primeros 5
         Call Format.Amount(Val(Xtmp3$))                   ! Formateo el valor
         XTmp1$ = Right$("        "+TS.TEMP1$,08)          ! Ajusto Precio
         XTMP1$ = "*VP*"+XTMP1$
         XTMP2$ = Unpack$(Asc.SALEPRIC$)                   ! Desempaqueta precio
         Xtmp3$ = Right$(Xtmp2$,5)	  										 ! Toma ultimos 5
         Call Format.Amount(Val(Xtmp3$))                   ! Formateo el valor
         XTmp2$ = Right$("        "+TS.TEMP1$,8)           ! Ajusto Precio
      EndIf Else Begin 																		 ! Precio Unitario
         XTMP1$ = Unpack$(Asc.SALEPRIC$)                   ! Desempaqueta precio
         Call Format.Amount(Val(Xtmp1$))                   ! Formateo el valor
         XTmp2$ = Right$("          "+TS.TEMP1$,10)        !
         XTMP1$ = "   *VP*   "
      EndIf 
      Call Visores4690(1,ASC.ITEMNAME$,Xtmp1$+Xtmp2$,0,"L")
   EndIf Else Begin 																			 ! No encontrado
     Call TSHIECET
     Call Visores4690(1,"ARTICULO NO EXITE","EN MAESTRA PRODUCTOS",0,"L")
   EndIf 
   TS.IO.MOTORKEY = 0 
   Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
   Exit Sub 
EndIf 


IF NOT(TS.INTRX) THEN \
IF (TS.IO.KEYS(5) = 61) AND (TS.IO.STATE = 10) THEN BEGIN
  IF Val(TS.IO.DATA$(5)) = 6 Then Begin   ! Validacion de precios
     EXIT SUB
  ENDIF
  IF Val(TS.IO.DATA$(5)) > 0 AND Val(TS.IO.DATA$(5)) < 12 Then Begin 
     ASIC.LLAVE% = 0
     Call ASIC.DATOS.BASE$("GIRE LA LLAVE DEL","SUPERVISOR...")
     If ASIC.LLAVE% = 0 Then Begin
        TS.IO.MOTORKEY = 0     																			! Inicializa secuencia
        DIM TS.IO.KEYS(10)                                          ! de tecleo y reporta
        DIM TS.IO.DATA$(10)                                         ! Error 
        TS.GUIDANCE = 1004                                          ! Msg Procedimiento no valido
        Exit Sub               
     EndIf 
  ENDIF
ENDIF


If TS.INTRX Then                                               \! En una venta
If TS.IO.MOTORKEY > 91 AND TS.IO.MOTORKEY < 97 Then Begin       ! Si forma de pago
 If TS.IO.DATA$(7) = "" Then Begin                              ! No ingreso valor pago
    TS.IO.MOTORKEY = 0     																			! Inicializa secuencia
    DIM TS.IO.KEYS(10)                                          ! de tecleo y reporta
    DIM TS.IO.DATA$(10)                                         ! Error 
    TS.GUIDANCE = 1003                                          ! Msg Procedimiento no valido
    Exit Sub         
 EndIf 
EndIf 

IF NOT(TS.INTRX) THEN \
 IF TS.IO.KEYS(4) = 100 AND TS.IO.KEYS(7) = 91 THEN BEGIN        ! Secuencia contenido gaveta
   ASIC.LLAVE% = 0
   Call ASIC.DATOS.BASE$("CONTENIDO DE GAVETA ",Asc.Oper.Name$)
   If ASIC.LLAVE% = 0 Then Begin
    TS.IO.MOTORKEY = 0     																			! Inicializa secuencia
    DIM TS.IO.KEYS(10)                                          ! de tecleo y reporta
    DIM TS.IO.DATA$(10)                                         ! Error 
    TS.GUIDANCE = 1004                                          ! Msg Procedimiento no valido
    Exit Sub               
   EndIf 
   Call U.Imprime(TO.HEADERLINE1$,4100H)
   TS.TEMP1$ = CHR$(1BH)+CHR$(21H)+CHR$(10H) + "***   CIERRE DE CAJA   ***"
   CALL U.IMPRIME(TS.TEMP1$,4100H)
   Call U.Imprime(" ",4100H)
   TS.TEMP1$ = "DESGLOSE DE DINERO EN CAJA"
   Call U.Imprime(TS.TEMP1$,4100H)
   Call U.Imprime(" ",4100H)
   TS.TEMP1$ = Format.Fecha(date$)
   TS.TEMP2$ = Left$(TIME$,2)+":"+Mid$(TIME$,3,2)+":"+Left$(TIME$,2)
   TS.TEMP3$ = Left$(("FECHA :"+TS.TEMP1$+"    HORA :"+TS.TEMP2$)+String$(40," "),40)
   Call U.Imprime(TS.TEMP3$,4100H)
   TS.TEMP2$ = Str$(Val(UNPACK$(TS.OPER$)))
   TS.TEMP3$ = Left$(("CAJERO : "+TS.TEMP2$+" "+Asc.Oper.Name$)+String$(40," "),40)
   Call U.IMPRIME(TS.TEMP3$,4100H)
   TS.TEMP3$ = Left$(("ALMACEN :"+TS.STORE$+"          CAJA :"+TS.TERMINAL$)+String$(40," "),40)
   Call U.Imprime(TS.TEMP3$,4100H)
   Call U.Imprime(" ",4100H)
   Call CUADRE.CAJERO 				! Imprime Detalle del movimiento 
   Call U.IMPRIME(" ",4500H)
   Call U.CORTACR
   Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)
   TS.IO.MOTORKEY = 73
   Call U.Imprime(TO.HEADERLINE1$,4100H)
   Call U.Imprime(TO.HEADERLINE2$,4100H)
EndIf

If TS.IO.KEYS(4) = 100 AND TS.IO.KEYS(2) = 80 Then Begin        ! Secuencia Apertura Gaveta
    IF TS.IO.DATA$(2) <> ""  Then Begin                         ! Validacion de precios
       EXIT SUB
    ENDIF 
    TS.IO.MOTORKEY = 0     																			! Inicializa secuencia
    DIM TS.IO.KEYS(10)                                          ! de tecleo y reporta
    DIM TS.IO.DATA$(10)                                         ! Error 
    TS.GUIDANCE = 1094                                          ! Msg Procedimiento no permitido
    Exit Sub            
EndIf 

EndIf 
End Sub

Sub FISMTS14.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU14.ECU                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

%Include FISTSU14.ECU    

EndIf 

End Sub 

Sub FISMTS20.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU20.ECU                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

%Include FISTSU20.ECU    

ENDIF

END Sub

Sub FISMTS20.012 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU20.EC2                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

%Include FISTSU20.EC2

ENDIF

END Sub

Sub FISMTS32.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU32.ECU                                         **
!   **    Inclu¡r en :  EAMTSU32.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo
   
   If TS.IO.KEYS(7) - 90 = 2 Then Begin                                ! Si pago con 
    If Val(TS.IO.DATA$(3)) <= 1 Then Begin                             ! cheque si
       Asc.Fis.ChequeSi$ = TS.IO.DATA$(9)                              ! Toma dato del cliente
       Asc.Fis.ImpChequeSi% = -1
    EndIf 
   EndIf 

EndIf

End Sub 

Sub FISMTS56.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU56.ECU                                         **
!   **    Inclu¡r en :  EAMTSU56.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

%Include FISTSU56.ECU    

ENDIF

END Sub

Sub FISMTS60.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU60.ECU                                         **
!   **    Inclu¡r en :  EAMTSU60.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

IF Asc.Proy.Fiscal% = -1  THEN BEGIN                                         		! Proyecto activo

   If Asc.Fis.ImpChequeSi% = -1 Then Begin                             ! Si impresion dato de cliente
       TO.USEREXIT(20) = 0 
       TO.USEREXIT(60) = 0
       TS.TEMP1$ = Left$(("IDENTIF. :"+Asc.Fis.ChequeSi$)+STRING$(37," "),37)
       Call U.IMPRIME(TS.TEMP1$,6100H)
       Call Valida.Ruc.Cheque.Si(Asc.Fis.ChequeSi$)                    ! Busca dato del cliente
       TS.TEMP1$ = Left$(("USUARIO  :"+Asc.Fis.ClienteSi$)+STRING$(37," "),37)
       Call U.IMPRIME(TS.TEMP1$,6100H)
       Asc.Fis.ChequeSi$ = ""
       Asc.Fis.ImpChequeSi% = 0
       Asc.Fis.ClienteSi$ = ""
       TO.USEREXIT(20) = -1
       TO.USEREXIT(60) = -1
   EndIf 

EndIf

End Sub 

