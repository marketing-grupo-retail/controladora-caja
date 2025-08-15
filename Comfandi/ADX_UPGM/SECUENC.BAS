!         **  PROYECTO COMFANDI   **
!
!     USER EXIT          =  USR1403.011 
!     Punto de Entrada   =  EAMTSU14.J86
!     Fecha de Revisi¢n  =  Marzo 14 de 1997
!     Unidad Responsable =  T‚cnicos Sistemas POS
!-----------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!     Estas  l¡neas  se  insertan  en la USER EXIT 14 para verificar
!     los pagos que  se hacen con cargo a  los  Clientes con Cr‚dito
!     ComfAndi  o para verificar los pagos con Tarjeta  de  Cr‚dito,
!     si el Valor es < o = a la Venta.                                             
!                         
!     Se comprueba la existencia de las siguientes ENTRADAS de DATOS
!
!           Para Cheques :
!                No.Banco
!                No.Cheque (4 Ultimos D¡gitos)
!                No. de Cta. Cte. del Girador
!                Nit o Doc. de Identidad del Girador
!
!           Para Pagos electr¢nicos (DB ¢ CR)
!                No. de Autorizaci¢n ¢ Aprobaci¢n
!                de la Entidad Financiera
!                Nit o Doc. de Identidad del Usuario
!
!           Para Pagos de Cr‚dito COMFAndI
!                Nit o Doc. de Identidad del Comprador
!                No. de la Factura del CR de Medicamentos
!                C¢digo de Barras sobre el Autoadhesivo de CR
!                %EPS ¢ Cuota Moderadora
!                Cupo M ximo Autorizado
!
!     Tambien  efect£a  la traducci¢n de FUNCTION CODES de teclas de
!     PROCEDIMIENTO, para indicar al Cajero que se va a producir una 
!     Mini-Factura para Credito de Medicamentos.                 
!
!     Dependiendo de las Modalidades de CR, Calcula y muestra el % ¢
!     porci¢n del Pago Obligatorio de Contado.
!
!     Se ha inclu¡do en las Input Sequence Tables en FC=85, semejante
!     a la tecla VERRIDE, para identificar las Cuotas Moderadoras de
!     los Creditos otorgados mediante EPS's.
!
!     Verifica el Ingreso de Informaci¢n del #Factura para los pagos
!     efectuados a CONCESIONARIOS.
!                                                         
!     Para  pagos  en la Modalidad CVC, liquida el 60% y 40% del pago
!     Efectivo Obligatorio, y le Indica al Cajero                    
!                                                         
!     Adicionalmente,  modifica  la  secuencia de Digitaci¢n para los
!     Pagos 91 y 95, que permite leer  CUPOS y recuperar el NIT de la 
!     Empr. Afiliada, para Simular con la Funci¢n 90, la Verificaci¢n
!     del archivo EAMTENDV.                                          
!
!     Para  procesar  los  CREDITOS  sin  factura y especialmente los
!     REINTEGROS que sean  posteriores  al mismo d¡a de la Compra, se
!     habilit¢ la Variedad 96, con los mismos controles de No. de Fac
!     Ced de Ciud y NIT de la Empresa Afiliada; cr‚ditos con Copagos.                       
!
!     Se Incluye otro Control Adicional para el  FRANQUEO de la Linea
!     de  Cr‚dito  Especial  EPS's,  con  la  siguiente  Secuencia de 
!     Digitaci¢n :
!
!                     .------------.      .---------.
!        Grupo %      |    CUOTA   |      | CREDITO |
!         E.P.S       | MODERADORA |      |         |
!                      ------------        ---------
!        Donde :
!                    Grupo %  ==>    %Clien   %E.P.S.
!
!                       1              10%      90%   
!                       2              20%      80%   
!                       5              50%      50%   etc. etc.                  
!------------------------------------------------------------------------
! Se adiciona el control de los montos de compra, para validar los 
! coopagos dentro de la transaccion
! Desarrollado por Oscar Valencia - ASIC 26 Julio 2.004
!------------------------------------------------------------------------
! Se desarrolla rutina para control de clientes con cupo en el fondo de 
! empleados, desarrollado el 26 de julio de 2.004 Asic - OVS
!------------------------------------------------------------------------
! Se modifica la subrutina EPSEANTS07 para que los datos de los valores
! de las cuotas moderadoras entren por parametro del archivo 
! ADX_IDT1:TARIFAS.DAT, donde se informan el valor de las tarifas de las 
! cuotas moderadoras.
! Desarrollado por Asic Consulting 2 enero 2006 OVS
!------------------------------------------------------------------------
! Mod 26Oct2011
! Se modifica captura de informacion de los dialogos a medios de pago
! eliminAndo el manejo del registro del producto 999999921 para evitar
! el bloqueo del modulo de terminal en el proceso de recompilacion
! del modulo. Desarrollado por Grupo Retail - OVS
!------------------------------------------------------------------------
! Mod 22Nov2011
! Se ajusta la definicion del programa, para el manejo de dialogos en
! la terminal de venta, se corrige todo el proceso de definicion de variables
! y se libera la forma de pago 66 para otro manejo de la aplicacion.
! desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------
! Mod 30Ene2021
! Se ajusta modulo de minifactura para manejo de factura electronica
! desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------
! Mod 8Feb2021
! Se ajusta la captura de los coopagos segun la tabla definida en tarifas.dat
! no solo aplique a las minifacturas de 24 caracteres el codigo de barras 
! si no a todos.
! desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------
! Mod 24May2021
! Se retira control desarrollado para grabacion de plu domicilios
! Solicitado por William Sanchez
! desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------
! Mod 25May2023
! Se define el manejo de minifacturas de planes 000-004 por FECO personas 
! naturales Sin importar el monto.
! Todas las minifacturas 005-008 obligatoriamente son FECO - Minifacturas
! Desarrollado por Grupo Retail - OVS
!---------------------------------------------------------------------------
! Mod 11/ene/2024
! Se adiciona grabacion impuesto ICUI en UD para reporte a FECO
! desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------

%ENVIRON T                                             !

Integer*1 Global Asc.Eps.Cntrl%,   \                     !
                 Gr.FeNat.Aplicado%, \                   ! FECO - Naturales         
                 Com.Fde.Sesion%,  \                     !
                 Asc.Fde.Rta%,     \
                 Gr.Fe.Fiscal%,    \
                 Gr.Mrm.Cap%,      \                     ! Control remisiones Saleforce
                 Gr.Ptecho.InTrx%  !
                 
String    Global Asc.Eps.EAN128$,  \
                 Asc.Eps.Tarifa1$, \
                 Asc.Eps.Tarifa2$, \
                 Asc.Eps.Tarifa3$, \
                 Asc.Eps.Dialogo$(1), \
                 Asc.Eps.Tmp$

String    Global DSPG$(1), CEDU$, KEY.OP$, IR.USERDATA$

String    Global NO.MIN$, NO.FAC$, NO.NIT$, BENE$,    \!
                 NO.CUP$, NO.CUO$, NO.MOD$, NO.AUT$,  \!
                 NO.VEN$, VRCRE$,  NO.BAN$, NO.CHE$,  \!
                 NO.CTA$, NO.APR$, NO.CED$, NUMBO$,   \!
                 AVISO$, AVISA$, RATA$, NITE$, COBAR$, \!
                 NITIM$, NITW$, NITE1$, NITE2$, FAC$,  \!
                 NOEM$, ERRORCOD$, KEY$, NOEMP1$,      \!
                 FECRED$,FEMODD$,NOVED$, NOEMP2$, NUCH$, \!
                 VENDE$, NOMBV$, Var.CR$, CE.NADA1$, CE.NADA2$, \!
                 NOM.OPER$, VENDIO$, Vend.Medica$
String    Global Gr.Mnf.Fac$
                 
Integer*1 Global DSCR%, YA.OK%, PROCE%, INDS%, \!
                 PAGO.CH%, IS%, USER.VERIFY.CHEQ,       \!
                 TI61%, TI65%, TI66%, SINF%, SINC%,     \!
                 FACT.NO%, MERCAMOS%

Integer*4 Global Asc.Eps.Total%, VAL.COUP, PLUPAGO%, VR.EPS%
Integer*4 Global Gr.Crd.Remision%, Asc.Tmp.Apun%			 ! Numero de remision por terminal
Integer*4 Global UE.TABLAIVA(1)                        ! Tasas de impuestos 
Integer*4 Global Gr.Motor2.Xpos%                       ! Nro promociones
Integer*2 Global UE.RECASRV.QTY%                       ! Recaudo terceros
String    Global Gr.Motor2.Items(2)                    ! Lista items con dscto
String    Global ASC.IR.DATA$, ASC.IR.NDEPTO$
String    Global Gr.Mf.PluS$, Gr.Mf.PluD$, Gr.Mf.DesS$, Gr.Mf.DesD$,			 \!
                 Gr.Mf.Monto$, Gr.Mf.Desc$, Gr.Mf.Sgnd$, Gr.Mf.Lst$
Integer*4 Global Gr.Iva.PtgIcui%                                            ! Porcentaje ICUI
Integer*4 Global Gr.Motor2.VlrDs%
Integer*1 Global Gr.RetMf.Proc%

%INCLUDE EAMTSWKG.J86                                  ! working storage
%INCLUDE EAMETWKA.j86                                  !
%INCLUDE EAMTRANS.j86                                  !
%INCLUDE EAMTERMS.J86                                  !
%INCLUDE EAMITEMR.J86                                  !
%INCLUDE EAMTSUVA.J86                                  ! working user storage

%INCLUDE EAMTOPTS.J86                                  !
%INCLUDE EAMSOPTS.J86                                  !
%INCLUDE EAMASMRT.J86                                  ! assembler SUBroutines
%INCLUDE EAMADXRT.J86                                  ! EXTERNAL SUBroutines
%INCLUDE EAMTSXHC.J86                                  ! TSLOG Functions

%INCLUDE GRMTSUSU.011                                  ! RUTINA GENERICAS

Sub TSHIECET External 
End Sub 

Function Format.AMOUNT(AMT1) EXTERNAL                  !
  Integer*1 FORMAT.AMOUNT                              !
  Integer*4 AMT1                                       !
End Function                                           !

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) EXTERNAL
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub Flag.Item.Iva(Xindi1%)	External																				! Calculo flag de iva
Integer*1 Xindi1%           																								!
End Sub 

Sub ERRORIOP  EXTERNAL                                 !
End Sub                                                !
                                                       !
Sub FIS.NEWOVERRIDE EXTERNAL                           !
End Sub                                                !
                                                       !
Sub VER EXTERNAL                                       !
End Sub                                                !
                                                       !
Sub VERIFCRE  EXTERNAL                                 !
End Sub                                                !
                                                       !
Sub ETIQUETAR EXTERNAL                                 ! Busca Variedad de CR
End Sub                                                !
                                                       !
Sub BUSCAVEND EXTERNAL                                 ! Busca Vendedor
End Sub                                                !

Sub ENCABEZA  EXTERNAL                                 ! Rutina de Encab
End Sub                                                !

Sub CALCUO EXTERNAL                                    ! Toma Datos Cheque
End Sub                                                !
                                                       !
!SUB CALDIGI EXTERNAL                                  ! Toma Datos Cheque
!END SUB                                               !
                                                       !
Sub Save.PRINT EXTERNAL                                ! Toma Datos Bonos
End Sub                                                !
                                                       !
Sub RESTORE.PRINT EXTERNAL                             ! Toma Datos T-DB
End Sub                                                !
                                                       !
Sub CALMOD EXTERNAL                                    ! Toma Datos T-CR
End Sub                                                !
                                                       !
Sub FIS.LEACON EXTERNAL                                ! Lectura Ctrls Almac
End Sub                                                !

Sub VERIDATA EXTERNAL
End Sub

Function CONSULTA.AFILIADO(XID$) External
 Integer*1 CONSULTA.AFILIADO
 String    Xid$
End Function 

Sub Asignacion.Consecutivo.Remision External					 !
End Sub 																							 !

Function Grabacion.Cadena.Usuario2(X.Clave$,X.Datos$) External
String X.Clave$, X.Datos$
End Function 

Function VENTAS.DEPTO.EXCL(Xlst$) Public     																! Valida si items excluidos estan vendidos
  Integer*4 X.I%																														!
  String    Xlst$, X.comp$
  Integer*1 Xfnd%, VENTAS.DEPTO.EXCL
  Xfnd% = 0																																	! Control de proceso
  VENTAS.DEPTO.EXCL = 0 
  Exit Function 
  
  FOR X.I% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																! GET String
    If LEN(H$) > 5 Then                                                    \! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
     	  IR.ITEMCODE$ = ASIC.GETUNPK(H$,Asc.Tmp.Apun%) 											! Plu
        IR.ITEMCODE$ = Pack$(Right$("000000000000"+IR.ITEMCODE$,12))        ! Ajusta dato de consulta
        TS.ER.RETURN = -1
        %INCLUDE EAMIRRD3.J86 																							! Busca producto
        If TS.ER.RETURN = -1 Then  Begin																	  ! Si lo encontro
           X.COMP$ = Right$(Asc.IR.NDEPTO$,15)                              ! Estructura comercial Item
           X.COMP$ = ";" + Left$(X.comp$,3) + ";"                           ! Dpto vendido
           If Match(X.comp$,Xlst$,1) > 0 Then Xfnd% = -1                    ! Item venta 
        EndIf																																! Fin Item encontrado
     EndIf                                  																! ITEM ENTRY String
  NEXT X.I%                                 																! NEXT String
  Ventas.Depto.Excl = Xfnd%																									! Respuesta del proceso
End Function
!--- Fin recorrido strin de la transaccion


Function FONDECOM.CONSULTA(X.Data$)
String X.Nombre$, X.data$
Integer*1 FONDECOM.CONSULTA
Integer*4 X.Saldo%
    TS.ER.RETURN = -1

!    FONDECOM.CONSULTA = TS.ER.RETURN     ! Retirar estas dos lineas cuAndo
!    EXIT FUNCTION                        ! se habiliten fondos OVS 28 Dic 2004

    Open "R::$AMFDEMP" Keyed Recl 29 As Com.Fde.Sesion% UNLOCKED NOWRITE NODEL
    If TS.ER.RETURN = -1 Then Begin
       If (X.Data$ = "")  Then Begin					   ! No se capturo dato
          Call Visores4690(1,"NO INGRESO DATO DEL ", "AFILIADO AL FONDO   ",1200,"L")
          DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)																	 !
          TS.IO.MOTORKEY = 73	
          FONDECOM.CONSULTA = 0
          Close Com.Fde.Sesion%
          Exit Function 
       EndIf 
       X.Data$ = Str$(Val(X.Data$))
       X.Data$ = Pack$(Right$("0000000000"+X.Data$,10))
       Call Visores4690(1,"BUSCANDO AL AFILIADO", UNPACK$(X.DATA$),1200,"L")
       TS.ER.RETURN = -1
       Read Form "C5 C20 I4"; #Com.Fde.Sesion% Key X.Data$ ; \
         X.Data$, X.Nombre$, X.Saldo%
       If TS.ER.RETURN = -1 Then Begin
         FONDECOM.CONSULTA = TS.ER.RETURN
         Close Com.Fde.Sesion%
         Exit Function 
       EndIf Else Begin 
         FONDECOM.CONSULTA = 0
         Close Com.Fde.Sesion%
         Exit Function 
       EndIf  
    EndIf Else Begin
!       Call Visores4690(1,"SESION ESTA SIENDO", "UTILIZADA "+STR$(Com.Fde.Sesion%),1200,"L")    
       FONDECOM.CONSULTA = TS.ER.RETURN
    EndIf 
End Function
                                                       !
Sub INIT.EPS.VARIABLES
    NO.MIN$  = ""           ! Averigua si MiniFact
    NO.FAC$  = ""           ! Sin Factura
    NO.NIT$  = ""           ! Sin cc/Nit
    BENE$    = ""           ! Sin beneficiario
    NO.CUP$  = ""           ! Sin Cupo
    NO.CUO$  = ""           ! Sin EPS%
    NO.MOD$  = ""           ! Sin Autorizacion
    NO.AUT$  = ""           ! Sin Autoriza                
    NO.VEN$  = ""           ! Sin vendedor
    VR.EPS%  = 0            ! Valor de la EPS
    VRCRE$ = ""                                 ! 
    DSCR%    = 0                                ! Reset de Descriptor
    NO.BAN$=""  :  NO.CHE$=""  :  NO.CTA$=""    ! Setup Variables
    NO.APR$=""  :  NO.CED$=""  :  NO.VEN$=""    ! Setup Variables
    NO.FAC$=""  :  NO.NIT$=""  :  NO.MIN$=""    ! Setup Variables
    NO.CUP$=""  :  NO.CUO$=""  :  NO.MOD$=""    ! Setup Variables
    BENE$ = ""  :  NO.AUT$=""
    NUMBO$ = "" :  YA.OK% = 0  : PROCE% = 0
    Asc.Eps.Cntrl% = -1                     !
    Ue.Cnv.Fiscal% = -1 : Gr.Fe.Fiscal% = -1
End Sub

Sub Graba.Items.FactElec
  Integer*4 PRIC%, TOT4%, XI%, XInd1%, XInd2%, X.J%, Xqty%, Xfind%,        \!
            Gr.Xcont%																							!
  Integer*2 Xsgn%, Xpos%																										!
  String    XTMP$, Xitm$, XPric$, Xdummy$, Xkey$, Xbasura$, X.Ean$, Xcot$   !
  String    Gr.Tmp.Itm$(2), Xbuff$, Xiva$, Xund$, X.Flag$, Gr.Impoc$,      \!
            XDepto$, Xicui$, Xdptori$, Xdscto$
  Gr.Xcont% = 0 																														! Numero de Items
  Dim Gr.Tmp.Itm$(900,10)					 																			    ! Lista de Items vendidos
  X.J% = 0 
  FOR X.J% = 1 TO SL.END                    																! FOR ALL StringS
    H$ = READ.SL.STR$(X.J%)                 																! GET String
    If LEN(H$) > 5 Then  Begin                                              ! ASSURE GOOD String
     If ASC(H$)  = 1 Then Begin             																! ITEM ENTRY String
     	  Asc.Tmp.Apun% = 3																										!
        XItm$   = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Item Vendido
        Xpric$  = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Precio Venta total
        Xdepto$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Departamento
        Xdptori$ = Xdepto$
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Family 
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 1
        Xdummy$ = Asic.Getunpk(H$,Asc.Tmp.Apun%)	  												! Indicat 2
        XInd2%  = Val(Xdummy$)																							! Vlr numerico Indicat 2
        Xsgn% = 1 																													! Trx de compra
        Xqty% = 0                                                           ! Sin cantidad o peso
        If (Xind2% And 0080H) Then Xsgn% = -1                               ! Item anulado
        If (Xind2% And 0040H) Then Xsgn% = -1                               ! Item anulado
        Xkey$ = Pack$( Right$("000000000000"+Xitm$,12) )                    ! Llave de busqueda
        Call IRRFEC.READ01(Xkey$, 4, TS.TEMP1$, 0)                					! Lectura Datos Itemr
        Call IRRFEC.SPLIT1( TS.TEMP1$ ) 	                          				! Entrega datos al eamitemr.j86
        Call Flag.Item.Iva(IR.INDICAT1)																		  ! Retorna flag iva
        Xdepto$ = LEFT$(IR.USERDATA$,16)																		! Estructura comercial
        Xdepto$ = Mid$(Xdepto$,2,3)                                         ! Seccion de venta
        
!        If Val(Unpack$(IR.SALEPRIC$)) > 0 Then                             \! si precio en la maestra
!           Xpric$  = Unpack$(IR.SALEPRIC$)		  														! Precio unitario

        X.Flag$ = "0"																												! Dato default
        Xiva$ = Str$(UE.TABLAIVA(TS.TEMP1I1))                               ! Tasa de IVA
        If Val(Xiva$) = 0 Then Begin                                        ! Reporta un iva cero
         If TS.TEMP1I1 = 7 Then X.Flag$ = "0" 
         If TS.TEMP1I1 = 8 Then X.Flag$ = "1"                          
        EndIf	
        Xicui$ = "000"                                                      ! Ptg ICUI
        If (IR.INDICAT1A AND 08H) Then                                     \! Ptg ICUI
        	  Xicui$ = Right$("000"+Str$(Gr.Iva.PtgIcui%),3)								  !
        If Val(Xdptori$) >= 800 Then Xicui$ = "000"													! Add 17Ene2023
        Gr.Impoc$ = "00"
        If TS.TEMP1I1 = 4 Then Begin																				! Impuesto impoconsumo
        	 Gr.Impoc$ = Str$(UE.TABLAIVA(TS.TEMP1I1))
        	 Xiva$ = "0" 
        	 X.Flag$ = "0"
        EndIf
        If (IR.INDICAT0 And 40H) Then Xund$ = "KLG" Else Xund$ = "UD"       ! Si pesable o unitario
        Xfind% = 0																													! Encontrado
     	  Xpos% = 0																														! Ctrl busqueta Item 
!-- Ajustado 23Oct2024 para manejar UD por entrada de producto
!       For XI% = 1 To Gr.Xcont%																	  				! Busca Item
!           If Val(XItm$) = Val(Gr.Tmp.Itm$(Xi%,0)) Then                   \! Lo encontro	
!       	        Xpos% = Xi%																								! Asigna posicion
!       Next XI%																													  !

        If Xpos% = 0 Then Begin                                             ! Nuevo Item
           Gr.Xcont% = Gr.Xcont% + 1												                ! Posicion Almacenar
           Gr.Tmp.Itm$(Gr.Xcont%,0) = Xitm$                                 ! SKU producto
           Gr.Tmp.Itm$(Gr.Xcont%,1) = Xpric$                                ! Precio de Venta
           Gr.Tmp.Itm$(Gr.Xcont%,2) = Str$( Xsgn% * 1 )                     ! Cantidad Vendida
           Gr.Tmp.Itm$(Gr.Xcont%,3) = Xiva$                                 ! Tarifa del IVA
           Gr.Tmp.Itm$(Gr.Xcont%,4) = X.flag$                               ! Excento o Excluido
           Gr.Tmp.Itm$(Gr.Xcont%,5) = Gr.Impoc$                             ! Impoconsumo
           Gr.Tmp.Itm$(Gr.Xcont%,6) = Xund$                                 ! Si es pesable o unitario
           !Gr.Tmp.Itm$(Gr.Xcont%,7) = IR.ITEMNAME$													! Descripcion Item
           Gr.Tmp.Itm$(Gr.Xcont%,7) = "00"          											  ! se retira grabacion descripcion Item
           Gr.Tmp.Itm$(Gr.Xcont%,8) = Xdepto$																! Seccion de venta
           Gr.Tmp.Itm$(Gr.Xcont%,9) = "0"      														  ! Total Descuento
           Gr.Tmp.Itm$(Gr.Xcont%,10) = Xicui$  														  ! Impuesto Icui
           Xpos% = Gr.Xcont%                                                ! Asigna posicion de vector
        EndIf Else Begin																									  ! Item Encontrado
          Gr.Tmp.Itm$(Xpos%,2) = Str$(Val(Gr.Tmp.Itm$(Xpos%,2)) +          \!
        	                                 (Xsgn% * 1))                     ! Cantidad Vendida
          Gr.Tmp.Itm$(Xpos%,1) = Str$(Val(Gr.Tmp.Itm$(Xpos%,1)) +          \!
        	                                 (Xsgn% * Val(Xpric$)))           ! Monto total de venta

        EndIf
     EndIf                                  																! ITEM ENTRY String
     Asc.Tmp.Apun% = 3																										  !     
     If ASC(H$)  = 2 Then Begin             																! ITEM ENTRY String Extention
     	 If Xpos% <> 0 Then Begin 																						! Extention Item Abbot
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! MPGROUP       
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! DEALQUAN      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! METODO PRECIO 
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! PRECIO        
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xdummy$  = ASIC.GETUNPK(H$,Asc.Tmp.Apun%)			                      ! CANTIDAD      
        Xqty% = Val(Xdummy$)																								!
        Xqty% = Xqty% * Xsgn%									      												! Movimiento
        Gr.Tmp.Itm$(Xpos%,2) = Str$(Xqty% + Val(Gr.Tmp.Itm$(Xpos%,2)) - (Xsgn% * 1))! Cantidad Vendida
       EndIf 																																!
     EndIf																																	! ITEM ENTRY String Extention
    EndIf
  NEXT X.J%   
  X.J% = 0
  XI%  = 0
  For X.J% = 1 To Gr.Motor2.Xpos%																						! Total promociones aplicadas
   For XI% = 1 To Gr.Xcont%
      If Val(Gr.Motor2.Items(X.J%,0)) = Val(Gr.Tmp.Itm$(XI%,0)) Then Begin  ! Plu encontrado
      	 Gr.Tmp.Itm$(XI%,9) = Gr.Motor2.Items(X.J%,1)												! Total dscto aplicado
      EndIf
   Next XI%
  Next X.J%
  X.J% = 0
  For X.J% = 1 To Gr.Xcont%
      XTMP$ = "00"																													! Signo operacion
      XPric$ = Gr.Tmp.Itm$(X.J%,1)																					! Toma venta total
      Xund$  = Gr.Tmp.Itm$(X.J%,2)																					! Qty total venta
      Xdscto$ = Gr.Tmp.Itm$(X.J%,9)																					! Dscto Items
      If Val(Xpric$) < 0 Then Begin																					! Si venta negativa
      	 Xpric$ = Str$( Val(Xpric$) * -1)																		! Invierte signo
      	 Xund$  = Str$( Val(Xund$) * -1 )																		! Invierte signo
      	 Xtmp$ = "01"																												! Reporta Item negativo
      EndIf																																	!
      If Gr.Tmp.Itm$(X.J%,6) = "UD" Then Begin															! Si venta por unidad
      	 Xpric$ = Str$( Val(Xpric$) / Val(Xund$) )												  ! Precio unitario
      EndIf Else Begin																											! Si venta pesable
      	 XPric$ = Str$( (Val(Xpric$)*1000) / Val(Xund$) ) 					        ! Precio unitario
      EndIf
      
!-- Por requerimiento Comfandi, se retira grabacion plu domicilios y se 
!-- retorna como un Item normal Autorizado William Sanchez 24/May/2021

      
!      If Val(Gr.Tmp.Itm$(X.J%,0)) = Val(Gr.Mf.PluS$) Or                    \! Si registro de plu de 
!         Val(Gr.Tmp.Itm$(X.J%,0)) = Val(Gr.Mf.PluD$) Then Begin							! domicilios
!         Gr.Mf.Monto$ = XPRIC$              																! Monto Vendido
!         Gr.Mf.Sgnd$  = Xtmp$																								! Signo operacion
!         If Val(Gr.Tmp.Itm$(X.J%,0)) = Val(Gr.Mf.PluS$) Then               \! Si es domic supermercado
!            Gr.Mf.Desc$  = Gr.Mf.DesS$ Else                                \! coloca descriptor supermercado
!            Gr.Mf.Desc$  = Gr.Mf.DesD$                                      !	o de drogueria
!      EndIf Else Begin																											!
	
        Xbuff$ = Pack$(Gr.Tmp.Itm$(X.J%,0))                              + \! SKU producto         
               ":" + Pack$(Xpric$)                                       + \! Precio Unitario
               ":" + Pack$(Xund$)                                        + \! Qty Vendida          
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,3))                          + \! Tasa Impuesto
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,4))                          + \! Excento o Excluido
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,5))                          + \! Impoconsumo
               ":" + Gr.Tmp.Itm$(X.J%,6)                                 + \! Unitario o pesado    
               ":" + Gr.Tmp.Itm$(X.J%,7)                                 + \! Descripcion Item    
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,8))                          + \! Seccion de la venta
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,9))                          + \! Descuentos aplicados
               ":" + Pack$(Xtmp$)									                       + \! Signo de la operacion
               ":" + Pack$(Gr.Tmp.Itm$(X.J%,10))                            ! Porcentaje ICUI
        Call Grabacion.Cadena.Usuario2("32",Xbuff$)                         ! UD Items para FE     
!      EndIf																																!
  Next X.J%
End Sub 

 
!! PROGRAMA USR1403.013     USER EXIT

Sub EPSEANTS02 Public
   Asc.Eps.Cntrl% = -1
   NO.MIN$  = ""           ! Averigua si MiniFact
   NO.FAC$  = ""           ! Sin Factura
   NO.NIT$  = ""           ! Sin cc/Nit
   BENE$    = ""           ! Sin beneficiario
   NO.CUP$  = ""           ! Sin Cupo
   NO.CUO$  = ""           ! Sin EPS%
   NO.MOD$  = ""           ! Sin Autorizacion
   NO.AUT$  = ""           ! Sin Autoriza
   Gr.Mnf.Fac$ = ""        ! NRO MINIFAC
   Gr.RetMf.Proc% = 0      ! Proc. reintegro mf
   CALL INIT.EPS.VARIABLES
End Sub

Sub EPSEANTS56 Public
   Asc.Eps.Cntrl% = -1
End Sub

!Sub EPSEANTS07 Public
!   Asc.Eps.Cntrl% = -1
!   Asc.Eps.Tarifa1$ = "1500"
!   Asc.Eps.Tarifa2$ = "5900"
!   Asc.Eps.Tarifa3$ = "15500"
!   Dim Asc.Eps.Dialogo$(10)
!End Sub

!--- Libreria modificada para datos por parametrizacion
!--- MOD 02Ene06 OVS

Sub EPSEANTS07 Public
   TS.ER.RETURN = -1 
   Open "R::$ARIFAS" AS 94 UNLOCKED NOWRITE NODEL
   If TS.ER.RETURN <> -1 Then Begin 
      Call U.IMPRIME("ERROR APERTURA TARIFAS",6100H)
      Call U.IMPRIME("ASIGNA VALORES CERO",6100H)
      Asc.Eps.Tarifa1$ = "01"
      Asc.Eps.Tarifa2$ = "01"
      Asc.Eps.Tarifa3$ = "01"      
      Exit Sub 
   EndIf 
   Asc.Eps.Cntrl% = -1
   If END #94 Then UE.EAN.FINAL
   While (1)
      Read #94; TS.TEMP1$
      Asc.Eps.Tarifa1$ = Str$(Val(TS.TEMP1$))
      Read #94; TS.TEMP1$
      Asc.Eps.Tarifa2$ = Str$(Val(TS.TEMP1$))
      Read #94; TS.TEMP1$
      Asc.Eps.Tarifa3$ = Str$(Val(TS.TEMP1$)) 
      Call U.Imprime("Cuotas Moderadoras Cargadas",2100H)
      GoTo UE.EAN.FINAL      
   Wend 
   UE.EAN.FINAL:
      Close 94
      Dim Asc.Eps.Dialogo$(10)
   TS.TS11WERR$ = ""																												! Control errores
   Open "R::$SCNTRL" As 94 UNLOCKED NOWRITE NODEL 													! Apertura archivo parametrizacion 
   If TS.TS11WERR$ <> "" Then Begin                                         !
  	 Call VISOR.AND.BORRAR("ERROR APERTURA PROMOCION V2 ")									! MSg alerta
  	 Call U.IMPRIME("ERROR APERTURA PROMOCIONES V2",6100H)                  !
  	 Exit Sub 																															! Sale del proceso
   EndIf  																																	!
   If End #94 Then UE.EAN.FINAL2																						! Si es EOF
   While (1)															  																! Recorre archivo parametros
        Read #94; TS.TEMP1$			       																			! Lectura registro                 
        If TS.TEMP1$ = "[MINIFAC ELECTRONICA]" Then Begin	     				      ! FACTURA ELECTRONICA MINIFACTURA
        	 Read #94; TS.TEMP1$			       																  ! Lectura registro
           Gr.Mf.Lst$  = Mid$(TS.TEMP1$,30,50)   			    					        ! Lista planes para FE
!-- Se retiran lineas manejo domicilios 24/May/2021
!           Read #94; TS.TEMP1$			       																  ! Lectura registro
!           Gr.Mf.PluS$ = Mid$(TS.TEMP1$,30,12)   			    					        ! PLU domicilio supermercados
!           Gr.Mf.PluS$ = Str$(Val(Gr.Mf.PluS$))
!           Read #94; TS.TEMP1$			       																  ! Lectura registro
!           Gr.Mf.PluD$ = Mid$(TS.TEMP1$,30,12)   			    					        ! PLU domicilio Droguerias
!           Gr.Mf.PluD$ = Str$(Val(Gr.Mf.PluD$))
!           Read #94; TS.TEMP1$			       																  ! Lectura registro
!           Gr.Mf.DesS$ = Mid$(TS.TEMP1$,30,18)   			    					        ! Descriptor Supermercado
!           Read #94; TS.TEMP1$			       																  ! Lectura registro
!           Gr.Mf.DesD$ = Mid$(TS.TEMP1$,30,18)   			    					        ! Descriptor Drogueria

        EndIf
   Wend 
   UE.EAN.FINAL2:
      Close 94
   Call U.Imprime("MIFIFAC ELECTRONICA       25/Myo/2023",2100H)
End Sub


Sub EPSEANTS14 Public                                   ! Validacion formas de pago
If TS.BALDUE(0) > 0 Then Begin
 If (TS.IO.MOTORKEY = 93) OR \                          !credito minifac
    (TS.IO.MOTORKEY = 96) Then Begin                    !credito medica.
  If Asc.Eps.Cntrl% = 0 Then Begin                      ! Capturo datos de EPS
    If  TS.BALDUE(0)  < Val(NO.MOD$) Then Begin
       CALL INIT.EPS.VARIABLES
       Call TSHIECET
       Call Visores4690(1,"COMPRA NO APLICA PAR", "A COOPAGO ",1200,"L")    
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       TS.IO.MOTORKEY = 73
       PROCE% = 0                                      ! NO MINIFAC
    EndIf
   EndIf  
  EndIf   
EndIf
END Sub

Sub EPSEANTS20 Public
String Gnit$, Gtmp$, Xtmpx$, BARCODE$, ENDBCODE$
If (Ue.Cnv.Fiscal% = 0) Then Exit Sub 																			! 
Gtmp$ = Unpack$(Nite1$)
Gtmp$ = ";" + Right$(Gtmp$,3) + ";"

!--- Se cometarea para que todas MF a expressmed 23Abr2024
If Match(Gtmp$,Gr.Mf.Lst$,1) <= 0 Then Begin																!
 If Gr.RetMf.Proc% <> -1 Then Begin																					!
    Ue.Cnv.Fiscal% = 0 : Gr.Fe.Fiscal% = 0                   								! Activa numeracion fiscal	
    Exit Sub 																																!
 EndIf																																			!
EndIf																																				!
If Gr.RetMf.Proc% <> -1 Then Begin																					!
	 Gr.Fe.Fiscal% = -1
EndIf
If Gr.Fe.Fiscal% = -1 Then Begin
 If Not TS.TRAINING Then Begin                       												! Si no en entrenamiento
  If TS.INTRX Then                                  											 \! Si esta en una compra
    If TS.LINETYPE = 6 And                                                 \! Store Data y Transacc
       TS.LINEDATA = 1 Then Begin                    											  ! Fecha, Hora, etc
        If TS.TENDERED (0) <> 0 Or                                         \! Si hay pagos
           TS.TRX.STATUS <> 100 Then Begin                                  ! y NO hay VOID en curso
           	 If UE.RECASRV.QTY% > 0 Then Exit Sub 													! En recuado de terceros
             
             !Call Asignacion.Consecutivo.Remision													  ! Genera consecutivo movimiento
             !TS.TEMP1$ = Right$("0000000"+STR$(Gr.Crd.Remision%),07)
             !TS.TEMP2$ = "   REMISION NRO: "+TS.STORE$+"-"+TS.TERMINAL$+"-"+TS.TEMP1$
             !TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)
             !Call U.Imprime(TS.TEMP2$,6100H)                
             !TS.TEMP1$ = Right$("0000000000000000"+STR$(Gr.Crd.Remision%),16)
             
            Xtmpx$ = Str$(Val(TS.STORE$)) + Right$("000"+Str$(Val(TS.TERMINAL$)),3) + \!
                     Right$("0000"+Str$(Sl.Hd.Transnum + 1),4) + + UNPACK$(SL.HD.DATETIME$)
            TS.TEMP2$ = "   REMISION NRO: "+Xtmpx$
            TS.TEMP2$ = Left$(TS.TEMP2$ + STRING$(40," "),40)               !
            Call U.Imprime(TS.TEMP2$,6100H)                                 !

!-- Impresion EAN 128
            BARCODE$  = Chr$(1DH) + Chr$(6BH) 					                    ! Manejo de impresion
    				ENDBCODE$ = Chr$(00H) 						                              ! codigos de barra
    				TS.TEMP1$ = Chr$(1DH)+Chr$(77H)+Chr$(02H)+  		               \! Horizontal barcode Size
            				    Chr$(1DH)+Chr$(68H)+Chr$(50)+                      \! Vertical barcode Size 
                				Chr$(1DH)+Chr$(48H)+Chr$(02H)+                     \! Human Read characters
  	            				Chr$(1DH)+Chr$(66H)+Chr$(01H)    	                  ! activado
     				Call U.IMPRIME(TS.TEMP1$,4000H)				                          !
     				TS.TEMP1$ = (BARCODE$+Chr$(07H)+(Xtmpx$)+ENDBCODE$)             ! Arma String de impresion			
     				Write #34 ; TS.TEMP1$          					                        ! Imprime dato EAN-128
     				TS.TEMP1$ = Chr$(1BH)+Chr$(61H)+Chr$(0)				                  ! Ajusta impresion a la Izquierda
     				Call U.IMPRIME(TS.TEMP1$,4000H)				                          !                    
!-- Fin impresion EAN 128       
		         
		         TS.TEMP1$ = Right$("0000000000000000"+Xtmpx$,16)
             TS.TEMP2$ = "TR+"                                           + \! Arma Str Usuario con 
                 "REMI"+Pack$(TS.TEMP1$)+":"                             + \!                      
                 STR$(ABS(TS.GROSSPOS + TS.GROSSNEG))                    + \! #Fact, Vr/Vta Neto   
                 ":" + Pack$("00") + ":" + Pack$("00")                   + \! y Vrs de Iva         
                 ":" + Pack$("00") + ":" + Pack$("00")                   + \!                      
                 ":" + Pack$("00") + ":" + Pack$("00")                   + \!                      
                 ":" + Pack$("00") + ":" + Pack$("00")                      !                      
             Call Grabacion.Cadena.Usuario2("21",TS.TEMP2$)                 !
             If Gr.Mrm.Cap% = -1 Then Exit Sub                  					  ! Hay remision capturada
             If Gr.Fe.Fiscal% = -1 Then Exit Sub                  				  ! Hay remision capturada
             Gnit$ = Unpack$(Nite1$)
             TS.TEMP2$ = Pack$(Left$(Gnit$,11))                          + \! Nit Empresa credito         
                         ":" + Pack$(Right$(Gnit$,3))                    + \! Nro del plan
                         ":" + Pack$(Cedu$)                              + \! Cedula empleado beneficiario
                         ":" + Pack$(Fac$)                               + \! Nro Minifactura             
                         ":" + Pack$("00")                                  ! Filler                      
             Call Grabacion.Cadena.Usuario2("31",TS.TEMP2$)  								! UD datos empresa facturada
             Call Graba.Items.FactElec
             If Val(Gr.Mf.Monto$) > 0 Then Begin                            ! si domicilio reportado
               TS.TEMP2$ = Gr.Mf.Desc$                                   + \! Descriptor domicilio
                           ":" + Pack$(Gr.Mf.Monto$)                     + \! Valor del domicilio
                           ":" + Pack$(Gr.Mf.Sgnd$)                         ! Signo de la operacion
               Call Grabacion.Cadena.Usuario2("33",TS.TEMP2$)  						  ! UD datos domicilio
             EndIf																													! fin domicilio reportado
        EndIf 
    EndIf 
 EndIf        
EndIf
End Sub 

Sub VALIDACION.MINIFAC
               FAC$ = NO.FAC$                         ! Guarda Factura
               If LEN(FAC$) > 8 Then Begin            ! Si vienen mas de 8
                  FAC$ = STRING$(20, "0")+FAC$        ! le agrega 20 CEROS
                  FAC$ = RIGHT$(FAC$ , 20)            ! y toma 20 digitos
                  PROCE% = 1                          ! Marca hacer MiniFac
                  NITE$ = RIGHT$(FAC$,13)             ! Toma parte de NIT
                  NITW$ = NITE$                       ! Guarda NIT
                  NITE$ = PACK$("0"+NITE$)            ! Empaqueta NIT 
                  NITE1$ = NITE$                      ! Suple Nit1 para NO Leer 
                  NITE2$ = NITE$                      ! Suple Nit2 para NO Leer 
                  NO.FAC$ = "0"+LEFT$(FAC$,7)         ! Asigna Num Fact.
                  FAC$ = NO.FAC$                      ! Guarda Factura
                  NO.MIN$ = NO.FAC$                   ! ¢ Minifactura
               EndIf                                  ! Fin de ajuste
               If LEN(FAC$) < 8 Then Begin            ! Si menos de 8
                  PROCE% = 0                          ! Reset hacer MiniFac
                  NITE$ = PACK$(STRING$(14,"0"))      ! Inicia con CEROS
                  NO.FAC$="00000000"+FAC$             ! Coloca CEROS
                  NO.FAC$=RIGHT$(NO.FAC$,8)           ! y toma 8 Derecha
                  FAC$ = NO.FAC$                      ! Guarda Factura
                  NO.MIN$ = NO.FAC$                   ! Guarda Factura
               EndIf                                  ! Fin de ajuste
               TI61% = 0   :   TI65% = 0              ! Inicia Flags
               TI66% = 1   :   NOEM$ = " "            ! Inicia Flags
               SINF% = 0   :   SINC% = 0              ! para Validac 
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
               If ERRORCOD$ = "  " Then Begin         ! Si existe Registro
                  TI61% = 1                           ! Indica Lect OK
                  KEY$ = PACK$("61")+NITE1$           ! Arma llave para leer
                  READ FORM "C8 C30 2C3 C1";         \! Lee Empresa Afil
                       #71 KEY KEY$;KEY$,NOEMP1$,    \! y recupera Nombre
                           FECRED$,FEMODD$,NOVED$     ! F/Crea;F/Mod;Nov
                       NOEM$ = NOEMP1$                ! Guarda Nombre Empresa
                  If ERRORCOD$ <> "  " Then Begin     ! Si NO hay NIT
                     SINC% = SINC% + 1                ! Marca Flag Error 
                     TI61% = 0                        ! Indica Lect OK
                  EndIf                               ! Fin Cond   
               EndIf Else Begin                       ! Fin Cond
                  SINF% = SINF% + 1                   ! Marca Flag Error
               EndIf                                  ! Fin Cond   
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
               If ERRORCOD$ = "  " Then Begin         ! Si existe Registro
                  TI65% = 1                           ! Indica Lect OK
                  If SINC% <> 0 Then Begin            ! Si no esta por Var=1
                     KEY$ = PACK$("65")+NITE2$        ! Arma llave para leer
                     READ FORM "C8 C30 2C3 C1";      \! Lee Empresa Afil
                          #71 KEY KEY$;KEY$,NOEMP2$, \! y recupera Nombre
                              FECRED$,FEMODD$,NOVED$  ! F/Crea;F/Mod;Nov
                     NOEM$ = NOEMP2$                  ! Guarda Nombre Empresa
                  EndIf                               ! Fin de Condic
                  If ERRORCOD$ <> "  " Then Begin     ! Si NO hay NIT
                     SINC% = SINC% + 1                ! Marca Flag Error 
                  EndIf                               ! Fin Cond   
               EndIf Else Begin                       ! Fin Cond
                  SINF% = SINF% + 1                   ! Marca Flag Error
               EndIf                                  ! Fin Cond   
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
               If TI61% = 1  Then Begin               ! Si Medicamentos OK
                  LOCATE #30;1,1,OFF                  ! Ubica Cursor
                  WRITE FORM "C21 C6 C1 C12";#30;    \! Muestra Emp/Fac/NIT
                  LEFT$(NOEMP1$,20),                 \! Nombre Empresa
                  STR$(VAL(FAC$)),"-",               \! Numero de factura
                  STR$(VAL(UNPACK$(NITE1$)))          ! Numero de Nit empre 
                  WAIT; 2000                          ! Espera 2 seg
               EndIf Else                            \! Fin Cond
               If TI65% = 1 Then Begin                ! Si es CVC OK
                  LOCATE #30;1,1,OFF                  ! Ubica Cursor
                  WRITE FORM "C21 C6 C1 C12";#30;    \! Muestra Emp/Fac/NIT
                  LEFT$(NOEMP2$,20),                 \!
                  STR$(VAL(FAC$)),":",               \!
                  STR$(VAL(UNPACK$(NITE2$)))          !  
                  WAIT; 2000                          ! Pausa de 2 Seg
               EndIf                                  ! Fin Cond
               If NITE1$ <> "" Then NITE$ = NITE1$    ! Guarda NIT ausente  
               If NITE2$ <> "" Then NITE$ = NITE2$    ! Guarda NIT ausente  
	             If SINF% > 1 Then Begin                ! Si fall¢ #Fact
                  Call Visores4690(1,"FACTURA INVALIDA.."," VERIFIQUE: "+STR$(VAL(FAC$)),1000,"l")       !
               EndIf                                  ! Fin Cond
               If SINC% > 1 Then Begin                ! Si fall¢ #NIT
                  Call Visores4690(1,"FACTURA OK "+STR$(VAL(FAC$)),     \! Factura
                                     "ERROR NIT:"+Str$(VAL(UNPACK$(NITE$))),0,"L")
                  TS.IO.MOTORKEY = 0                  ! Reduce procesamiento
               EndIf                                  ! Fin Cond

End Sub 


Sub SEC.DIGIT Public 
Integer*1 X.I%
String    X.TMP$

AVISO$ = "    PAGO CON          MINI-FACTURA..!"      !
AVISA$ = "  DESHABILITADA LA    MINI-FACTURA..!"      !

!--------- Conversi¢n de Pago (+) a (-) cuAndo hay Balance (-) --------------
If VAL.COUP <> 0 Then Begin
 If TS.IO.STATE = 10    And                          \! Si esta en MAIN y se
   TS.IO.DATA$(3) <> "" And                          \! digita Variedad,
   TS.IO.DATA$(7) <> "" And                          \! con valor, y hay
   TS.BALDUE(0)   <  0  And                          \! Saldo o Balance (-)
   (TS.IO.KEYS(7) = 93 OR                            \! a Efectivo
    TS.IO.KEYS(7) = 94 OR                            \! a Otros Pagos o
    TS.IO.KEYS(7) = 95 OR                            \! a Otros Pagos o
    TS.IO.KEYS(7) = 96)    Then Begin                 ! a Credito,entonces    
    If NO.MOD$ <> "0" Then Begin                      ! Si hay Copago
       TS.IO.DATA$(7) = VRCRE$                        ! 
    EndIf Else  Begin                                 !
       If VR.EPS% <> 0 Then Begin                     ! Si hay Cuota Moderad
          TS.IO.DATA$(7) = STR$(VR.EPS%)              ! el pago=Cuota Moderad
          TS.IO.DATA$(7) = STR$(VAL(TS.IO.DATA$(7))* -1)! el pago se torna (-)
          VR.EPS% = 0                                 ! Reset Cuota Moderad
          VRCRE$ = TS.IO.DATA$(7)                     ! Toma Vr del Pago (-)
       EndIf                                          !
   EndIf                                              !
 EndIf                                                 ! Fin de Condic
EndIf
!---------------- Calcula Porcentaje del CR CVC. 60% -----------------------

If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "5"  And                         \! con Variedad CVC
   TS.BALDUE(0)  <>  0   And                         \! y Saldo en Transac
   TS.IO.KEYS(7)  = 96   Then Begin                  \! y si Digita TECLA CR
      TS.IO.DATA$(7) = STR$(TS.BALDUE(0)*.6)          ! el pago=60% de BALDUE. 
      VRCRE$ = TS.IO.DATA$(7)                         ! Valor del Credito
      LOCATE #30;1,1,OFF                              ! Ubica Cursor
      WRITE FORM                                     \! Muestra Venta
      "C8 PIC($$#,###,###) C9 PIC($$#,###,###)";     \! segun formato
      #30;"CVC 60%=",TS.BALDUE(0)*.6," CONTADO=",    \! con 60% Cr CVC y
      TS.BALDUE(0) - TS.BALDUE(0)*.6                  ! con 40% Contado 
      RATA$ = " CVC. 60%"                             ! Guarda Descriptor
      WAIT; 2000                                      ! Pausa de 2 Seg
EndIf                                                 ! Fin condic

!--------------------- Toma Datos de Medios de Pago -----------------------

DENUEVO:
If (TS.IO.MOTORKEY = 92 OR                           \! Si es CHEQUE 
    TS.IO.MOTORKEY = 94 OR                           \! Si es RED DB/CR
    TS.IO.MOTORKEY = 95 OR                           \! Si es TAR/CR
    TS.IO.MOTORKEY = 93 OR                           \! Si es TAR/CR
    TS.IO.MOTORKEY = 96) And                         \! Si es CREDITO
    TS.BALDUE(0)  <> 0   And                         \! Con Saldo
    TS.IO.DATA$(7) = ""  And                         \! Sin Vr Digitado
    TS.IO.DATA$(3) = ""  Then Begin                   ! Sin Variedad 
       INDS% = TS.IO.MOTORKEY - 90                    ! Calcula Indice Pag
       If PAGO.CH% = 0 Then Begin                     ! Si NO hay Datos Ch 
          PAGO.CH% = 1                                ! Indica Comienzo
          VRCRE$ = ""                                 ! 
          DSCR%    = 0                                ! Reset de Descriptor
          NO.BAN$=""  :  NO.CHE$=""  :  NO.CTA$=""    ! Setup Variables
          NO.APR$=""  :  NO.CED$=""  :  NO.VEN$=""    ! Setup Variables
          NO.FAC$=""  :  NO.NIT$=""  :  NO.MIN$=""    ! Setup Variables
          NO.CUP$=""  :  NO.CUO$=""  :  NO.MOD$=""    ! Setup Variables
          BENE$ = ""  :  NO.AUT$=""
          NUMBO$ = ""   
       EndIf                                          ! Fin de Condic 
      
!--------------------- Toma de Datos de Cheque -------------------------------

       If (TS.IO.MOTORKEY = 92) And (Val(TS.IO.DATA$(3)) <> 6) Then Begin  ! Si en un CHEQUE y no credi uno
           If NO.BAN$ =  "" OR                       \! sin Banco
             NO.CHE$ =  "" OR                        \! sin No. Cheque
             NO.CTA$ =  "" Then Begin                 ! sin Cuenta, Inicia
             DSCR% = DSCR% + 1                        ! Incr los Descrip
             INDS% = TS.IO.MOTORKEY - 90              ! Indice para el mensaje
             IS% = VAL(STR$(INDS%)+"1")               ! 1er Mensaje
             NO.BAN$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
             IS% = VAL(STR$(INDS%)+"2")               ! 2do Mensaje
             NO.CHE$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
             IS% = VAL(STR$(INDS%)+"3")               ! 3er Mensaje
             NO.CTA$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
          EndIf                                       ! Fin de Artificio
          If NO.CTA$ <> "" Then Begin                 ! Si ya esta la Cta
             TS.IO.MOTORKEY = 80                      ! Tecla Motor=ENTER
             TS.IO.KEYS(4)  = 100                     ! Tecla NO-VENTA
             TS.IO.DATA$(4) = NO.BAN$                 ! No. de Banco
             TS.IO.KEYS(3)  = 78                      ! Tecla SLASH
             TS.IO.DATA$(3) = NO.CHE$                 ! No. de Cheque
             TS.IO.KEYS(2)  = 80                      ! Tecla ENTER
             TS.IO.DATA$(2) = NO.CTA$                 ! No. de Cta.Cte
             TS.IO.KEYS(10) = 63                      ! Tecla DATA-ENTRY
             TS.IO.KEYS(07) = 0                       ! Borra Tecla Pago
             YA.OK% = 1                               ! Se Completaron Datos
             Call U.IMPRIME("DATOS CHQ: #BCO"+"   #CHEQUE     #CUENTA",2100H)
	           Call U.IMPRIME("      "+NO.BAN$+"      "+NO.CHE$+"      "+NO.CTA$,2100H)
             PAGO.CH% = 0                             ! Reset de Comienzo
             DSCR%    = 0                             ! Reset de Descriptor
             PLUPAGO% = 0                             ! Reset de Plu Verif
          EndIf                                       ! Fin sec ENT-DATOS
       EndIf                                          ! Fin CHEQUES

!--- Para captura crediUNO

       If (TS.IO.MOTORKEY = 92) And (Val(TS.IO.DATA$(3)) = 6) Then Begin  ! Si en un credi uno
          If NO.APR$ =  "" OR                        \!
             NO.CED$ =  "" Then Begin                 ! Inicia la Toma
             INDS% = 4                                ! Indice para el mensaje
             IS% = VAL(STR$(INDS%)+"1")               ! 1er Mensaje
             NO.APR$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
             IS% = VAL(STR$(INDS%)+"2")               ! 2do Mensaje
             NO.CED$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
          EndIf                                       ! Fin de Artificio
          If NO.CED$ <> "" Then Begin                 ! Si esta la Cedula
             TS.IO.MOTORKEY = 80                      ! Tecla Motor=ENTER
             TS.IO.KEYS (2) = 80                      ! Tecla ENTER
             TS.IO.DATA$(2) = NO.CED$                 ! No. de Cedula
             TS.IO.KEYS (3) = 78                      ! Tecla SLASH
             TS.IO.DATA$(3) = NO.APR$                 ! No. de Aprobac
             TS.IO.KEYS (7) = 0                       ! Borra Tecla Pago
             TS.IO.KEYS(10) = 63                      ! Tecla DATA-ENTRY
             YA.OK% = 1                               ! Se Completaron Datos
             Call U.IMPRIME("DATOS CHQ: #BCO"+"   #CHEQUE     #CUENTA",2100H)
	           Call U.IMPRIME("      "+NO.BAN$+"      "+NO.CHE$+"      "+NO.CTA$,2100H)
             PAGO.CH% = 0                             ! Reset de Comienzo
             DSCR%    = 0                             ! Reset de Descriptor
             PLUPAGO% = 0                             ! Reset de Plu Verif
          EndIf                                       ! Fin sec ENT-DATOS
       EndIf                                          ! Fin CHEQUES

!-- fIN DIALOGO PARA CREDIUNO
!--------------------- Toma de Pagos Electronicos ----------------------------

       If TS.IO.MOTORKEY = 94 OR                     \! Red Multicolor
          TS.IO.MOTORKEY = 95 Then Begin              ! ¢ Tarjeta CR
          If NO.APR$ =  "" OR                        \!
             NO.CED$ =  "" Then Begin                 ! Inicia la Toma
             INDS% = TS.IO.MOTORKEY - 90              ! Indice para el mensaje
             IS% = VAL(STR$(INDS%)+"1")               ! 1er Mensaje
             NO.APR$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
             IS% = VAL(STR$(INDS%)+"2")               ! 2do Mensaje
             NO.CED$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
          EndIf                                       ! Fin de Artificio
          If NO.CED$ <> "" Then Begin                 ! Si esta la Cedula
             TS.IO.MOTORKEY = 80                      ! Tecla Motor=ENTER
             TS.IO.KEYS (2) = 80                      ! Tecla ENTER
             TS.IO.DATA$(2) = NO.CED$                 ! No. de Cedula
             TS.IO.KEYS (3) = 78                      ! Tecla SLASH
             TS.IO.DATA$(3) = NO.APR$                 ! No. de Aprobac
             TS.IO.KEYS (7) = 0                       ! Borra Tecla Pago
             TS.IO.KEYS(10) = 63                      ! Tecla DATA-ENTRY
             YA.OK% = 1                               ! Se Completaron Datos
             Call U.IMPRIME("DATOS CHQ: #BCO"+"   #CHEQUE     #CUENTA",2100H)
	           Call U.IMPRIME("      "+NO.BAN$+"      "+NO.CHE$+"      "+NO.CTA$,2100H)
             PAGO.CH% = 0                             ! Reset de Comienzo
             DSCR%    = 0                             ! Reset de Descriptor
             PLUPAGO% = 0                             ! Reset de Plu Verif
          EndIf                                       ! Fin sec ENT-DATOS
       EndIf                                          ! Fin CHEQUES

!--------------------- Toma de Datos de Creditos -----------------------------

       If TS.IO.MOTORKEY = 96 Then Begin              ! Si es Credito
!-- Se retira control solicitado por Comfandi 22 ene 2018 GR
!       	  If Gr.Ptecho.InTrx% = -1 Then Begin         ! Si items con precio techo capturado
!             Call Visor.And.Borrar("TRX CON ITEMS PRECIOTECHO REGISTRADOS")       ! Msg de alerta
!       	  	 Call Visor.And.Borrar("NO SE PERMITE PAGO  CON MINIFACTURA")        ! Msg de alerta 
!             DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) ! reset vectores
!             TS.IO.MOTORKEY = 73			  					  	! Tecla Borrar
!             NO.VEN$ = ""                          		! Inicializa Variable
!             Exit Sub                              		! Sale del proceso
!       	  EndIf

          If NO.VEN$ =  "" Then Begin                 ! Sin Vendedor
             INDS% = TS.IO.MOTORKEY - 90              ! Indice para el mensaje
             Is% = VAL(STR$(INDS%)+"1")               ! 1er Mensaje
             NO.VEN$ = ASIC.DATOS$(DSPG$(IS%),"")     ! Recup Descrip. para Despl
             If (NO.VEN$ = "") Or (NO.VEN$ = "E") Then Begin  ! Si no Capturado
                DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) ! reset vectores
                TS.IO.MOTORKEY = 73			  					  ! Tecla Borrar
                NO.VEN$ = ""                          ! Inicializa Variable
                Exit Sub                              ! Sale del proceso
             EndIf Else Begin													! Dato Capturado
             	 TS.ER.RETURN = -1
             	 KEY.OP$ = NO.VEN$
               Call BUSCAVEND                         ! Busca al vendedor capturado
               If TS.ER.RETURN <> -1 Then Begin		    ! Si no existe vendedor
                  Call VISOR.AND.BORRAR("ERROR DE VENDEDOR   VERIFIQUE: "+NO.VEN$) 	
                  DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) ! reset vectores
                  TS.IO.MOTORKEY = 73			  					! Tecla Borrar
                  NO.VEN$ = ""                        ! Inicializa Variable
                  KEY.OP$ = ""                        ! Inicializa variable
                  Exit Sub                            ! Sale del proceso
               EndIf
             EndIf
             Asc.Eps.Cntrl% = -1											!
             DSCR% =  1                               ! Incr los Descrip
             YA.OK% = 0                               ! Se Completaron Datos
             PROCE% = 1                               ! Marca hacer MiniFac
             GoTo CAPTURA.MINIFACTURA
          EndIf Else Begin                            ! Fin de Artificio
            
          If Asc.Eps.Cntrl% = -1 Then Begin            ! Captura dato EAN 128
!             Asc.Eps.EAN128$ = Asic.Datos$("Pase Formula Medica"," ")
!-- Se adiciona nueva rutina de captura de datos OVS 24 Abr 2007
             CAPTURA.MINIFACTURA:
             Ue.Cnv.Fiscal% = -1 : Gr.Fe.Fiscal% = -1
             Asc.Eps.EAN128$ = Entrada.Datos.Varios$("Pase Formula Medica"," ")
             If Len(Asc.Eps.EAN128$) = 24 Then Asc.Eps.EAN128$ = "0000"+Asc.Eps.EAN128$     
             
             If Len(Asc.Eps.EAN128$) < 28 Then Begin   ! 
              If Len(Asc.Eps.EAN128$) < 20 Then Begin
               Asc.Eps.Cntrl% = 0       
               X.I% = 1
               ASC.EPS.DIALOGO$(1) = "Minifactura........?"
               ASC.EPS.DIALOGO$(2) = "Nro Factura........?"
               ASC.EPS.DIALOGO$(3) = "No. Afiliacion.....?"
               ASC.EPS.DIALOGO$(4) = "Beneficiario.......?"
               ASC.EPS.DIALOGO$(5) = "Cupo Autorizado....?"
               ASC.EPS.DIALOGO$(6) = "% EPS..............?"
               ASC.EPS.DIALOGO$(7) = "Coopago(A:1 B:2 C:3)"
               ASC.EPS.DIALOGO$(8) = "Autorizacion.......?"
               FOR X.I% = 1 TO 8     
                  X.TMP$ = ASIC.DATOS$(ASC.EPS.DIALOGO$(X.I%),"")
                  If X.TMP$ = "E" OR X.TMP$ = "" Then Begin    ! Presiono Borrar
                     CALL INIT.EPS.VARIABLES
                     DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)																	 !
                     TS.IO.MOTORKEY = 73	
                     YA.OK% = 1                               ! Se Completaron Datos
                     EXIT SUB
                  EndIf
                  If X.I% = 1 Then NO.MIN$ = X.TMP$
                  If X.I% = 2 Then NO.FAC$ = X.TMP$
                  If X.I% = 3 Then NO.NIT$ = X.TMP$ : NITE$ = X.TMP$
                  If X.I% = 4 Then BENE$   = X.TMP$
                  If X.I% = 5 Then NO.CUP$ = X.TMP$
                  If X.I% = 6 Then NO.CUO$ = X.TMP$
                  If X.I% = 7 Then Begin
                   If X.TMP$ = "1" Then                \! Para estrato 1
                    NO.MOD$ = Asc.Eps.Tarifa1$ Else     \!
                   If X.TMP$ = "2" Then               \! Para estrato 2
                    NO.MOD$ = Asc.Eps.Tarifa2$ Else    \!  
                   If X.TMP$ = "3" Then              \! Para estrato 3
                    NO.MOD$ = Asc.Eps.Tarifa3$ Else     \!
                    NO.MOD$ = "0"                        ! Si no hay estrato
                  EndIf

                  If X.I% = 8 Then NO.AUT$ = X.TMP$
               NEXT X.I%

               If VAL(NO.CUO$) <> 0 Then   \! Si hay %EPS
                  CALL CALCUO:NO.MOD$ =  "0"  ! Calcula la Cuota
               If VAL(NO.MOD$) <> 0 Then   \! Si hay Copago
                  CALL CALMOD               ! Calcula la Cuota
               If BENE$ = "0" Then BENE$ = "00"
               NO.FAC$ = NO.FAC$ +"0"+ NO.NIT$           										!
               KEY.OP$ = NO.VEN$																						! Asigna vendedor

!--- Proyecto Cuenta transitoria               
               If Val(NO.NIT$) <= 0 Then Begin
                  Call TSHIECET																							!
                  Call Visor.and.borrar("CREDITO NO PERMITIDOCLIENTE NO VALIDO")
                  CALL INIT.EPS.VARIABLES																		!
                  YA.OK% = 1                               									! Se Completaron Datos
                  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)									!
                  TS.IO.MOTORKEY = 73																				!
                  Exit Sub																									!
               EndIf
               If VAL(NITE$) >= 890303208000 And 													 \!
                  VAL(NITE$) <= 890303208001 Then Begin                			! Si es crd comfandi
                  TS.TEMP1I1 = CONSULTA.AFILIADO(NO.NIT$)               		! Busca afiliado comfandi
                  If TS.TEMP1I1 = 0 Then Begin															! afiliado no existe
                     CALL INIT.EPS.VARIABLES																! 
                     Call TSHIECET																					!
                     Call Visores4690(1,"CREDITO NO PERMITIDO", "CLIENTE NO EXISTE   ",1200,"L")
                     CALL INIT.EPS.VARIABLES																!
                     YA.OK% = 1                               							! Se Completaron Datos
                     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                     TS.IO.MOTORKEY = 73																		!
                     Exit Sub																								!
                  EndIf Else Begin  																				! fin afiliado
                     Ue.Cnv.Fiscal% = 0 : Gr.Fe.Fiscal% = 0
                  	 BENE$ = "00"
                  EndIf 
               EndIf Else Begin																							! Fin crd comfandi
                TS.TEMP1I1 = VENTAS.DEPTO.EXCL(Gr.Mf.Lst$)  											! Valida venta dptos excluidos               	
                If TS.TEMP1I1 = -1 Then                                         \! Venta dptos excluidos
                	 Ue.Cnv.Fiscal% = 0 : Gr.Fe.Fiscal% = 0                   ! Activa numeracion fiscal
               EndIf
               If Gr.Ptecho.InTrx% = -1 Then Begin         									! Si items con precio techo capturado
                If Len(NO.NIT$) = 9 Then Begin															! Capturado un nit
                 If (Val(NO.NIT$) >= 800000000) And 										   \! Si cliente es una empresa
                    (Val(NO.NIT$) <= 999999999) Then Begin		  						! del programa lealtad
                    Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ") ! Msg de alerta
                    CALL INIT.EPS.VARIABLES																	! Reinicia proceso
                    YA.OK% = 1                               								! Se Completaron Datos
                    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                    TS.IO.MOTORKEY = 73																			!
   	                Exit Sub 																								! Sale del proceso
                 EndIf   																										! Fin valida cliente
               EndIf
               If (Val(NO.NIT$) >= 8000000000) And 												 \! Si cliente es una empresa
                  (Val(NO.NIT$) <= 9999999999) Then Begin										! del programa lealtad
                    Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ") ! Msg de alerta
                    CALL INIT.EPS.VARIABLES																	! Reinicia proceso
                    YA.OK% = 1                               								! Se Completaron Datos
                    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                    TS.IO.MOTORKEY = 73																			!
   	              Exit Sub 																									! Sale del proceso
               EndIf   																											! Fin valida cliente
               EndIf																												! Fin control precio techo
!-- Proyecto cuenta transitoria               

               If VAL(NITE$) = 890327352000 Then Begin                			! Si es Fondecom
                  Asc.Fde.Rta% = FONDECOM.CONSULTA(NO.NIT$)
                  If Asc.Fde.Rta% <> -1 Then Begin
                     CALL INIT.EPS.VARIABLES
                     Call TSHIECET
                     Call Visores4690(1,"CREDITO NO PERMITIDO", "CLIENTE NO EXISTE   ",1200,"L")
                     CALL INIT.EPS.VARIABLES
                     YA.OK% = 1                               ! Se Completaron Datos
                     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
                     TS.IO.MOTORKEY = 73
                     Exit Sub
                  EndIf
               EndIf 
               NO.NIT$ = STR$(VAL(NO.NIT$))
               NO.NIT$ = RIGHT$(STRING$(12,"0")+NO.NIT$,12)
               Asc.Eps.Tmp$ = NO.NIT$
               NO.NIT$ = NO.NIT$              \! Agrega beneficiario
                         + "99" +  RIGHT$(BENE$,2)
               DSCR% = 7                 ! Incr los Descrip
               If val(nite$) = 890399010603   Then Begin 
                 no.cuo$ =   "60":call calcuo
               EndIf  

               CALL CALMOD
               GoTo Continua.Eps.Proceso
              EndIf Else Begin			       ! Datos Minifactura
               Asc.Eps.Cntrl% = 0                      !
               NO.FAC$ = MID$(Asc.Eps.EAN128$,1,7)     ! Numero de la factura
               NO.MIN$ = NO.FAC$                       ! Numero minifactura
               NITE$ = MID$(Asc.Eps.EAN128$,9,12)      ! Nit de la empresa
               ASC.EPS.DIALOGO$(1) = "Afiliacion.........?"
               ASC.EPS.DIALOGO$(2) = "Beneficiario.......?"
               ASC.EPS.DIALOGO$(3) = "Cupo Autorizado....?"
               ASC.EPS.DIALOGO$(4) = "% EPS..............?"
               ASC.EPS.DIALOGO$(5) = "Coopago(A:1 B:2 C:3)"
               ASC.EPS.DIALOGO$(6) = "Autorizacion.......?"
               FOR X.I% = 1 TO 6     
                  X.TMP$ = ASIC.DATOS$(ASC.EPS.DIALOGO$(X.I%),"")
                  If X.TMP$ = "E" OR X.TMP$ = "" Then Begin    ! Presiono Borrar
                     CALL INIT.EPS.VARIABLES
                     DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)																	 !
                     TS.IO.MOTORKEY = 73	
                     YA.OK% = 1                               ! Se Completaron Datos
                     EXIT SUB
                  EndIf
                  If X.I% = 1 Then NO.NIT$ = X.TMP$
                  If X.I% = 2 Then BENE$   = X.TMP$
                  If X.I% = 3 Then NO.CUP$ = X.TMP$
                  If X.I% = 4 Then NO.CUO$ = X.TMP$
                  If X.I% = 5 Then Begin 
                   If X.TMP$ = "1" Then                \! Para estrato 1
                    NO.MOD$ = Asc.Eps.Tarifa1$ Else    \!
                   If X.TMP$ = "2" Then                \! Para estrato 2
                    NO.MOD$ = Asc.Eps.Tarifa2$ Else    \!  
                   If X.TMP$ = "3" Then                \! Para estrato 3
                    NO.MOD$ = Asc.Eps.Tarifa3$ Else    \!
                    NO.MOD$ = "0"                       ! Si no hay estrato
                  EndIf
                  If X.I% = 6 Then NO.AUT$ = X.TMP$
               NEXT X.I%

               KEY.OP$ = NO.VEN$
               CEDU$   = NO.NIT$     
               NO.FAC$   = NO.FAC$ +"0"+ NITE$           !
              If BENE$ = "0" Then BENE$ = "00"
              If VAL(NO.CUO$) <> 0 Then   \! Si hay %EPS
                 CALL CALCUO : NO.MOD$ =  "0" ! Calcula la Cuota
              If VAL(NO.MOD$) <> 0 Then   \! Si hay Copago
                 CALL CALMOD             ! Calcula la Cuota
!--- Cuenta transitoria
               If Val(NO.NIT$) <= 0 Then Begin
                  Call TSHIECET																					!
                  Call Visor.and.borrar("CREDITO NO PERMITIDOCLIENTE NO VALIDO")
                  CALL INIT.EPS.VARIABLES																!
                  YA.OK% = 1                               							! Se Completaron Datos
                  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                  TS.IO.MOTORKEY = 73																		!
                  Exit Sub																								!
               EndIf

               If VAL(NITE$) >= 890303208000 And Val(NITE$) <= 890303208001 Then Begin                ! Si es crd comfandi
                  TS.TEMP1I1 = CONSULTA.AFILIADO(no.nit$)               ! Busca afiliado comfandi
                  If TS.TEMP1I1 = 0 Then Begin												! afiliado no existe
                     CALL INIT.EPS.VARIABLES
                     Call TSHIECET
                     Call Visores4690(1,"CREDITO NO PERMITIDO", "CLIENTE NO EXISTE   ",1200,"L")
                     CALL INIT.EPS.VARIABLES
                     YA.OK% = 1                               ! Se Completaron Datos
                     Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
                     TS.IO.MOTORKEY = 73
                     Exit Sub
                  EndIf Else Begin  																	! fin afiliado
                  	 Ue.Cnv.Fiscal% = 0 : Gr.Fe.Fiscal% = 0
                  	 BENE$  = "00"
                  EndIf 
               EndIf Else Begin																							! Fin crd comfandi
                TS.TEMP1I1 = VENTAS.DEPTO.EXCL(Gr.Mf.Lst$)  											! Valida venta dptos excluidos               	
                If TS.TEMP1I1 = -1 Then                                         \! Venta dptos excluidos
                	 Ue.Cnv.Fiscal% = 0 : Gr.Fe.Fiscal% = 0                   ! Activa numeracion fiscal
               EndIf

!--- Cuenta transitoria                 

               If Gr.Ptecho.InTrx% = -1 Then Begin         									! Si items con precio techo capturado
                If Len(NO.NIT$) = 9 Then Begin															! Capturado un nit
                 If (Val(NO.NIT$) >= 800000000) And 										   \! Si cliente es una empresa
                    (Val(NO.NIT$) <= 999999999) Then Begin		  						! del programa lealtad
                    Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ") ! Msg de alerta
                    CALL INIT.EPS.VARIABLES																	! Reinicia proceso
                    YA.OK% = 1                               								! Se Completaron Datos
                    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                    TS.IO.MOTORKEY = 73																			!
   	                Exit Sub 																								! Sale del proceso
                 EndIf   																										! Fin valida cliente
               EndIf
               If (Val(NO.NIT$) >= 8000000000) And 												 \! Si cliente es una empresa
                  (Val(NO.NIT$) <= 9999999999) Then Begin										! del programa lealtad
                    Call Visor.And.Borrar("PRODUCTO REGULADO NOVENTA A NITS ") ! Msg de alerta
                    CALL INIT.EPS.VARIABLES																	! Reinicia proceso
                    YA.OK% = 1                               								! Se Completaron Datos
                    Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)								!
                    TS.IO.MOTORKEY = 73																			!
   	              Exit Sub 																									! Sale del proceso
               EndIf   																											! Fin valida cliente
               EndIf																												! Fin control precio techo

              If VAL(NITE$) = 890327352000 Then Begin                ! Si es Fondecom
                  Asc.Fde.Rta% = FONDECOM.CONSULTA(NO.NIT$)
                  If Asc.Fde.Rta% <> -1 Then Begin
                     CALL INIT.EPS.VARIABLES
                     Call TSHIECET
                     Call Visores4690(1,"CREDITO NO PERMITIDO", "CLIENTE NO EXISTE   ",1200,"L")
                     CALL INIT.EPS.VARIABLES
                     DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)																	 !
                     TS.IO.MOTORKEY = 73	
                     YA.OK% = 1                               ! Se Completaron Datos
                     EXIT SUB
                  EndIf
               EndIf  

               CEDU$   = NO.NIT$     
               NO.NIT$ = STR$(VAL(NO.NIT$))
               NO.NIT$ = RIGHT$(STRING$(12,"0")+NO.NIT$,12)
               Asc.Eps.Tmp$ = NO.NIT$
               NO.NIT$ = NO.NIT$              \! Agrega beneficiario
                         + "99" +  RIGHT$(BENE$,2)
               DSCR% = 7                 ! Incr los Descrip
               If val(nite$) = 890399010603   Then Begin 
                 no.cuo$ =   "60":call calcuo
               EndIf  
               CALL CALMOD
               GoTo Continua.Eps.Proceso
              EndIf 				       ! Fin datos minifactura
             EndIf Else Begin													                      ! Formato Largo Pasa a 28 Caracteres 24 Abr 2007 OVS 
               Asc.Eps.Cntrl% = 0                                           !                                                    
               NO.FAC$ = MID$(Asc.Eps.EAN128$,9,7)                          ! Numero de la factura antes ini 5                   
               NO.NIT$ = MID$(Asc.Eps.EAN128$,6,3)                          ! Nit de la empresa antes ini 1 y long 4             
               NITE$   = MID$(Asc.Eps.EAN128$,16,12)                        ! Nit de la empresa antes ini 12                     
               NO.FAC$ = NO.FAC$ +"0"+ NITE$                                !                                                    
               TS.TEMP1$ = MID$(Asc.Eps.EAN128$,28,1)                       ! Estrato antes ini 24                               
               If TS.TEMP1$ = "1" Then                                     \! Para estrato 1                                     
                  NO.MOD$ = Asc.Eps.Tarifa1$ Else                          \!                                                    
                If TS.TEMP1$ = "2" Then                                    \! Para estrato 2                                     
                   NO.MOD$ = Asc.Eps.Tarifa2$ Else                         \!                                                    
                 If TS.TEMP1$ = "3" Then                                   \! Para estrato 3                                     
                  NO.MOD$ = Asc.Eps.Tarifa3$ Else                          \!                                                    
                  NO.MOD$ = "0"                                             ! Si no hay estrato                                  
               DSCR% = 7                 							                      ! Incr los Descrip                                   
               NO.MIN$   = LEFT$(NO.FAC$,7)
               BENE$     = "0"
               NO.CUP$   = "0"
               NO.CUO$   = "0"
               NO.AUT$   = "0"
               KEY.OP$ = NO.VEN$
               If VAL(NITE$) = 890327352000 Then Begin                      ! Si es Fondecom
                  Asc.Fde.Rta% = FONDECOM.CONSULTA(NO.NIT$)
                  If Asc.Fde.Rta% <> -1 Then Begin
                     CALL INIT.EPS.VARIABLES
                     Call TSHIECET
                     Call Visores4690(1,"CREDITO NO PERMITIDO", "CLIENTE NO EXISTE   ",1200,"L")
                     CALL INIT.EPS.VARIABLES
                     DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10)								!
                     TS.IO.MOTORKEY = 73	
                     YA.OK% = 1                                             ! Se Completaron Datos
                     EXIT SUB
                  EndIf
               EndIf                
               NO.NIT$ = STR$(VAL(NO.NIT$))
               NO.NIT$ = RIGHT$(STRING$(12,"0")+NO.NIT$,12)
               Asc.Eps.Tmp$ = NO.NIT$
               NO.NIT$ = NO.NIT$              \! Agrega beneficiario
                         + "99" +  RIGHT$(BENE$,2)
               DSCR% = 7                 ! Incr los Descrip
               If val(nite$) = 890399010603   Then Begin 
                 no.cuo$ =   "60":call calcuo
               EndIf  
               CALL CALMOD
               TS.TEMP1$ = Asc.Eps.Ean128$
               Call Grabacion.String.Usuario("14052004",TS.TEMP1$) ! Graba String usuario
             EndIf
          EndIf             
          Continua.Eps.Proceso:
          EndIf                                       !   

          If NO.AUT$ <> ""  Then Begin                ! Si ya esta Copago
            
             
!             TS.IO.MOTORKEY = 80                      ! Tecla Motor=ENTER
!             TS.IO.KEYS (2) = 80                      ! Tecla ENTER
!             TS.IO.DATA$(2) = NO.NIT$                 ! No. de NIT Comprador
!             TS.IO.KEYS(10) = 63                      ! Tecla DATA-ENTRY
!             TS.IO.DATA$(3) = NO.CUP$                 ! Cupo de la EPS       
!             TS.IO.KEYS (4) = 100                     ! Tecla NO-VENTA
!             TS.IO.DATA$(4) = RIGHT$("000"+NO.CUO$,3)\!
!             +RIGHT$("000000"+NO.MOD$,6)              ! %EPS+ CUOTA MODERADORA
!             TS.IO.KEYS (5) = 74                      ! Tecla PRECIO
!             TS.IO.DATA$(5) = NO.FAC$                 ! No. de Factura
!             TS.IO.KEYS (6) = 75                      ! Tecla CANTIDAD
!             TS.IO.DATA$(6) = NO.VEN$                 ! No. de Vend
!             TS.IO.KEYS (7) = 0                       ! Borra Tecla Pago
!             TS.IO.DATA$(10) = "0"
             
             Call VALIDACION.MINIFAC
             Call Grabacion.Data.Entry(NO.NIT$,NO.CUP$,RIGHT$("000"+NO.CUO$,3)+RIGHT$("000000"+NO.MOD$,6),\
                                       Asc.Eps.Ean128$, NO.VEN$,"")
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
             TS.IO.MOTORKEY = 73
             YA.OK% = 1                               ! Se Completaron Datos
             Call U.IMPRIME("DATOS CHQ: #BCO"+"   #CHEQUE     #CUENTA",2100H)
	           Call U.IMPRIME("      "+NO.BAN$+"      "+NO.CHE$+"      "+NO.CTA$,2100H)
	           PAGO.CH% = 0                             ! Reset de Comienzo
             DSCR%    = 0                             ! Reset de Descriptor
             PLUPAGO% = 0                             ! Reset de Plu Verif

       EndIf                                          ! Fin CHEQUES
     EndIf                                            ! Fin CHEQUES
EndIf                                                 ! Fin de VerIf Precio

!----------  Rutina de Verificacion de Datos de Usuario  --------------

If USER.VERIFY.CHEQ             Then Begin            ! Item para Verificar
   If TS.IO.MOTORKEY    = 73    Then Begin            ! Si se digita BORRAR
         TS.IO.DATA$(1) = ""                          ! Restaura Dato Asoc
         TS.IO.KEYS(1)  = 0                           ! Restaura Tecla
         TS.IO.KEYS(7)  = INDS% + 90                  ! Asigna Pago y
         TS.IO.MOTORKEY = INDS% + 90                  ! el mismo Motor
         DSCR% = DSCR% - 1                            ! Decrm los Descrip
         GOTO DENUEVO                                 !
   EndIf Else Begin                                   !
      If TS.IO.KEYS(10) <> 80 Then Begin              ! Si no hay ENTER
         If YA.OK% = 1 Then Begin                     ! Si 100% datos ok
            YA.OK% = 0                                ! Reset para sig vez
            PAGO.CH% = 0                              ! Retorna al Inicio
            USER.VERIFY.CHEQ = 0                      ! Restaura Flag de Verif
         EndIf                                        ! Fin de Datos Ingr
      EndIf                                           ! Fin de no ENTER
      If TS.IO.KEYS(10) = 80 Then Begin               ! Si Hubo ENTER
         If NO.BAN$ = "" OR                          \! Si no hay Bco 
            NO.VEN$ = "" OR                          \! ¢ no hay Vendedor
            NUMBO$  = "" OR                          \! ¢ no hay BONO
            NO.APR$ = "" Then Begin                   ! ¢ no hay Aprobac
               NO.BAN$ = TS.IO.DATA$(10)              ! Toma Bco
               NO.APR$ = TS.IO.DATA$(10)              ! Toma Autorizac
               NO.VEN$ = TS.IO.DATA$(10)              ! Toma Vendedor
               NUMBO$ = TS.IO.DATA$(10)              ! Toma NUMERO BONO
         EndIf Else Begin                             ! Si n¢,
            If NO.MIN$ = "" And                      \! Si no hay MiniFact
               INDS%   = 6  Then Begin                ! y es Pago CR
                  COBAR$ = TS.IO.DATA$(10)            ! No. Verific Barras
                  NO.CHE$ = COBAR$                    ! Guarda Dato CH
                  NO.CED$ = COBAR$                    ! Guarda Dato CC
                  If LEN(COBAR$) > 7 Then Begin       ! Si vienen mas de 7
                     COBAR$ = STRING$(20, "0")  +    \! le agrega 20 CEROS
                               COBAR$                 ! a la lectura
                     COBAR$ = RIGHT$(COBAR$ , 20)     ! alinea a 20 charact
                     PROCE% = 1                       ! Marca hacer MiniFac
                     NITE$ = RIGHT$(COBAR$,13)        ! Toma parte de NIT
                     NITIM$ = RIGHT$(NITE$,12)        !
                     !WRITE #34;"SECU   " + NITIM$
                     NITW$ = NITE$                    ! Guarda NIT
                     NITE$ = PACK$("0"+NITE$ )        ! Empaqueta NIT 
                     NITE1$ = NITE$                   ! Suple Nit1 para NO Leer 
                     NITE2$ = NITE$                   ! Suple Nit2 para NO Leer 
                     NO.FAC$ = "0"+LEFT$(COBAR$,7)    ! Asigna Num Fact.
                     !If val(nitim$) = 890307200001   Then Begin 
                     !no.fac$ =   "00000001" 
                     !EndIf  
                      FAC$ = NO.FAC$                   ! Guarda Factura
                     NO.MIN$ = NO.FAC$                ! ¢ Minifactura
                  EndIf                               ! Fin de ajuste
                  If LEN(COBAR$) < 8 Then Begin       ! Si menos de 8
                     PROCE% = 0                       ! Reset hacer MiniFac
                     NITE$ = PACK$(STRING$(14,"0"))   ! Inicia con CEROS
                     NO.FAC$="00000000"+COBAR$        ! Coloca CEROS
                     NO.FAC$=RIGHT$(NO.FAC$,8)        ! y toma 8 Derecha
                     FAC$ = NO.FAC$                   ! Guarda Factura
                     NO.MIN$ = NO.FAC$                ! Guarda Factura
                  EndIf                               ! Fin de ajuste
            EndIf Else Begin                          ! Si n¢,
               If NO.CHE$ = "" OR                    \! Si no hay Cheque
                  NO.CED$ = "" OR                    \! Si no hay Cedula
                  NO.FAC$ = "" Then Begin             ! Si no hay Factura
                     NO.CHE$ = TS.IO.DATA$(10)        ! Toma Cheque
                     NO.FAC$ = TS.IO.DATA$(10)        ! Toma Factura
                     NO.CED$ = TS.IO.DATA$(10)        ! Toma Cedula
                     If NO.FAC$ = "" Then            \! Si no hubo Min/Fac
                        NO.FAC$ =  NO.CHE$            ! Toma Factura
               EndIf Else Begin                       ! Si n¢,
                  If NO.NIT$ = "" OR                 \! Si no hay CC/Nit
                     NO.CTA$ = "" Then Begin          ! Si no hay Cta Cte 
                     NO.CTA$ = TS.IO.DATA$(10)        ! Toma CtaCte
                     NO.NIT$ = TS.IO.DATA$(10)        ! Toma Cedula/Nit
                     CEDU$   = NO.NIT$     
                        If LEN(NO.NIT$) > 19 Then Begin
                          CEDU$ = MID$(NO.NIT$,10,12)  ! Guarda cedula
                          NO.NIT$ = MID$(NO.NIT$,10,12) \
                          + "00" +  RIGHT$(NO.NIT$,2)
                          BENE$   = RIGHT$(NO.NIT$,2) 
                       EndIf 
                       If BENE$ <> "" Then DSCR% = DSCR% + 1
                  EndIf Else Begin                    ! Si n¢,
                   If BENE$ = "" Then Begin          ! Si no beneficiario 
                        BENE$ = RIGHT$("00"+TS.IO.DATA$(10),2)       ! toma dato
                        If BENE$ = "0" Then          \! o 0
                           BENE$ = "00"               !
                      Asc.Eps.Tmp$ = NO.NIT$
                      NO.NIT$ = NO.NIT$              \! Agrega beneficiario
                          + "99" +  RIGHT$(BENE$,2)
                     EndIf Else Begin                 !
                     If val(nitim$) = 890399010603   Then Begin 
                       no.cuo$ =   "60":call calcuo
!                       NO.CUP$ =   "0"
                       NO.MOD$ =   "0"
                       DSCR% = DSCR% + 2
                     EndIf    
                                                        
                     If NO.CUP$ = "" Then Begin       ! Si NO hay Cupo
                        NO.CUP$ = TS.IO.DATA$(10)     ! Toma CUPO
                        If VAL(NO.CUP$) = 0 Then     \! Si NO digita Cupo 
                           NO.CUP$ = "0"              ! se habilita CERO
                     EndIf Else Begin                 ! Si n¢,
                        If NO.CUO$="" Then Begin      ! Si no % EPS
                           NO.CUO$=TS.IO.DATA$(10)    ! Toma % EPS
                           If VAL(NO.CUO$)=0 Then    \! Si no hay %EPS
                              NO.CUO$ = "0"           ! se habilita CERO
                           If VAL(NO.CUO$)<>0 Then   \! Si hay %EPS
                              CALL CALCUO:NO.MOD$ =  \
                              "0": DSCR% = DSCR% + 1  ! Calcula la Cuota

                        EndIf Else Begin              ! Si n¢,
                           If NO.MOD$="" Then Begin   ! Si no Copago
                              NO.MOD$=TS.IO.DATA$(10) ! Toma Copago
                           If VAL(NO.MOD$)=0 Then    \! Si no hay Copago
                              NO.MOD$ = "0"           ! se habilita CERO
                           If VAL(NO.MOD$)<>0 Then   \! Si hay Copago
                              CALL CALMOD             ! Calcula la Cuota
                           EndIf Else Begin           ! Si n¢,
                              If NO.AUT$="" Then Begin! Si no Autoriz
                                 NO.AUT$=TS.IO.DATA$(10) ! Toma Autoriz
                                 NO.VEN$=RIGHT$(     \! y la adiciona a 
                                    "000000"     +   \! Cod Vendedor
                                    NO.VEN$,6)+RIGHT$\!
                                    ("000000"+NO.AUT$,6)! 
                              EndIf                   ! Fin de Copago
                           !EndIf                      ! Fin de Copago
                           !EndIf                      ! Fin de Copago
                           EndIf                      ! Fin de Copago
                        EndIf                         ! Fin de %EPS   
                     EndIf                            ! Fin de Cupo %EPS
                   EndIf                              ! Fin benefi.
                EndIf                                 ! Fin de Datos
               EndIf                                  ! Fin de Cheque
            EndIf                                     ! Fin de Bco
         EndIf                                        ! Fin de Vendedor
         If TS.IO.DATA$(10)="" Then PAGO.CH%=0        ! Si NO Digita ALGO
         TS.IO.KEYS(10)  = 0                          ! Flushing de
         TS.IO.DATA$(10) = ""                         ! Keyed data
         TS.IO.KEYS(7)  = INDS% + 90                  ! Asigna Pago y
         TS.IO.MOTORKEY = INDS% + 90                  ! el mismo Motor
         GOTO DENUEVO                                 ! Repite la Toma
      EndIf                                           ! Fin por tecla ENTER
   EndIf                                              ! Fin por no BORRAR
EndIf                                                 ! Fin de VerIf Precio

!------------------ Verificacion de los Datos del Credito --------------------

If TS.IO.KEYS(10)  = 63 And                          \! Si digita ENTR/DATOS
   TS.IO.KEYS(2)   = 80 And                          \! y tecla de ENTER
   TS.BALDUE(0)   <> 0  And                          \! y hay Saldo
   TS.IO.DATA$(10) = ""      Then Begin               ! sin Dato Asociado 
      If TS.IO.KEYS(5) =  74 Then Begin               ! Si Tecla PRECIO(Factura)
         If VAL(TS.IO.DATA$(5)) > 0  And             \! con Dato Asoc Factura
            TS.IO.DATA$(2)     <> ""  Then Begin      ! y Cedula  
               FAC$ = TS.IO.DATA$(5)                  ! Guarda Factura
               If LEN(FAC$) > 8 Then Begin            ! Si vienen mas de 8
                  FAC$ = STRING$(20, "0")+FAC$        ! le agrega 20 CEROS
                  FAC$ = RIGHT$(FAC$ , 20)            ! y toma 20 digitos
                  PROCE% = 1                          ! Marca hacer MiniFac
                  NITE$ = RIGHT$(FAC$,13)             ! Toma parte de NIT
                  NITW$ = NITE$                       ! Guarda NIT
                  NITE$ = PACK$("0"+NITE$)            ! Empaqueta NIT 
                  NITE1$ = NITE$                      ! Suple Nit1 para NO Leer 
                  NITE2$ = NITE$                      ! Suple Nit2 para NO Leer 
                  NO.FAC$ = "0"+LEFT$(FAC$,7)         ! Asigna Num Fact.
                  FAC$ = NO.FAC$                      ! Guarda Factura
                  NO.MIN$ = NO.FAC$                   ! ¢ Minifactura
               EndIf                                  ! Fin de ajuste
               If LEN(FAC$) < 8 Then Begin            ! Si menos de 8
                  PROCE% = 0                          ! Reset hacer MiniFac
                  NITE$ = PACK$(STRING$(14,"0"))      ! Inicia con CEROS
                  NO.FAC$="00000000"+FAC$             ! Coloca CEROS
                  NO.FAC$=RIGHT$(NO.FAC$,8)           ! y toma 8 Derecha
                  FAC$ = NO.FAC$                      ! Guarda Factura
                  NO.MIN$ = NO.FAC$                   ! Guarda Factura
               EndIf                                  ! Fin de ajuste
               TI61% = 0   :   TI65% = 0              ! Inicia Flags
               TI66% = 1   :   NOEM$ = " "            ! Inicia Flags
               SINF% = 0   :   SINC% = 0              ! para Validac 
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
               TS.IO.DATA$(8)  = ""                   ! Borra Dato % Inicial
               TS.IO.KEYS(8)   = 0                    ! Borra TECLA Cuota Moder
!               If VAL(UNPACK$(NITE$))=0 Then Begin    ! Si NO se digita NIT
!                  KEY$=PACK$("61"+FAC$)               ! Arma llave para leer
!                  READ FORM "C5 C7";#70 KEY KEY$;    \! Lee CUPOS para Var=1                    
!                       KEY$,NITE1$                    ! y guarda NIT Empresa
!               EndIf                                  !
               If ERRORCOD$ = "  " Then Begin         ! Si existe Registro
                  TI61% = 1                           ! Indica Lect OK
                  KEY$ = PACK$("61")+NITE1$           ! Arma llave para leer
                  READ FORM "C8 C30 2C3 C1";         \! Lee Empresa Afil
                       #71 KEY KEY$;KEY$,NOEMP1$,    \! y recupera Nombre
                           FECRED$,FEMODD$,NOVED$     ! F/Crea;F/Mod;Nov
                       NOEM$ = NOEMP1$                ! Guarda Nombre Empresa
                  If ERRORCOD$ <> "  " Then Begin     ! Si NO hay NIT
                     SINC% = SINC% + 1                ! Marca Flag Error 
                     TI61% = 0                        ! Indica Lect OK
                  EndIf                               ! Fin Cond   
               EndIf Else Begin                       ! Fin Cond
                  SINF% = SINF% + 1                   ! Marca Flag Error
               EndIf                                  ! Fin Cond   
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
!               If VAL(UNPACK$(NITE$))=0 Then Begin    ! Si NO se digita NIT
!                  KEY$=PACK$("65"+FAC$)               ! Arma llave para leer
!                  READ FORM "C5 C7";#70 KEY KEY$;    \! Lee CUPOS para Var=5
!                       KEY$,NITE2$                    ! y guarda NIT Empresa
!               EndIf                                  !
               If ERRORCOD$ = "  " Then Begin         ! Si existe Registro
                  TI65% = 1                           ! Indica Lect OK
                  If SINC% <> 0 Then Begin            ! Si no esta por Var=1
                     KEY$ = PACK$("65")+NITE2$        ! Arma llave para leer
                     READ FORM "C8 C30 2C3 C1";      \! Lee Empresa Afil
                          #71 KEY KEY$;KEY$,NOEMP2$, \! y recupera Nombre
                              FECRED$,FEMODD$,NOVED$  ! F/Crea;F/Mod;Nov
                     NOEM$ = NOEMP2$                  ! Guarda Nombre Empresa
                  EndIf                               ! Fin de Condic
                  If ERRORCOD$ <> "  " Then Begin     ! Si NO hay NIT
                     SINC% = SINC% + 1                ! Marca Flag Error 
                  EndIf                               ! Fin Cond   
               EndIf Else Begin                       ! Fin Cond
                  SINF% = SINF% + 1                   ! Marca Flag Error
               EndIf                                  ! Fin Cond   
               ERRORCOD$ = "  "                       ! Inicia Estado Leer
               If TI61% = 1  Then Begin               ! Si Medicamentos OK
                  LOCATE #30;1,1,OFF                  ! Ubica Cursor
                  WRITE FORM "C21 C6 C1 C12";#30;    \! Muestra Emp/Fac/NIT
                  LEFT$(NOEMP1$,20),                 \! Nombre Empresa
                  STR$(VAL(FAC$)),"-",               \! Numero de factura
                  STR$(VAL(UNPACK$(NITE1$)))          ! Numero de Nit empre 
                  WAIT; 2000                          ! Espera 2 seg
               EndIf Else                            \! Fin Cond
               If TI65% = 1 Then Begin                ! Si es CVC OK
                  LOCATE #30;1,1,OFF                  ! Ubica Cursor
                  WRITE FORM "C21 C6 C1 C12";#30;    \! Muestra Emp/Fac/NIT
                  LEFT$(NOEMP2$,20),                 \!
                  STR$(VAL(FAC$)),":",               \!
                  STR$(VAL(UNPACK$(NITE2$)))          !  
                  WAIT; 2000                          ! Pausa de 2 Seg
               EndIf                                  ! Fin Cond
               If NITE1$ <> "" Then NITE$ = NITE1$    ! Guarda NIT ausente  
               If NITE2$ <> "" Then NITE$ = NITE2$    ! Guarda NIT ausente  
	       If SINF% > 1 Then Begin                ! Si fall¢ #Fact
           Call Visores4690(1,"FACTURA INVALIDA.."," VERIFIQUE: "+STR$(VAL(FAC$)),1000,"l")       !
         EndIf                                  ! Fin Cond
               If SINC% > 1 Then Begin                ! Si fall¢ #NIT
                  Call Visores4690(1,"FACTURA OK "+STR$(VAL(FAC$)),     \! Factura
                                     "ERROR NIT:"+Str$(VAL(UNPACK$(NITE$))),0,"L")
                  TS.IO.MOTORKEY = 0                  ! Reduce procesamiento
               EndIf                                  ! Fin Cond
         EndIf Else Begin                             ! Si NO hubo Fact/CC
           TS.GUIDANCE = 1108                         ! Avisa Omision a cajero
           TS.IO.MOTORKEY = 0                         ! Reduce procesamiento
         EndIf                                        ! Fin datos Cheque
      EndIf Else Begin                                ! y finaliza validacion
         If TS.IO.KEYS(6) =  75 Then Begin           \! Tecla de CANTID(Vended)
            TI61% = 1                                 ! Indica Lect OK
         EndIf                                        ! Fin datos Cheque
      EndIf                                           ! Fin datos Cheque
EndIf                                                 ! Fin Condic

!------------------- Verificaci¢n de los Datos del Cheque --------------------

 If TS.IO.KEYS(10)  = 63 And                          \! Si digita ENTR/DATOS
    TS.IO.KEYS(2)   = 80 And                          \! y tecla de ENTER
    TS.IO.KEYS(6)   = 0  And                          \
    TS.IO.KEYS(4)  <> 100 And                         \! PERMITE CAPTURA CLIENTE
    TS.IO.DATA$(10) = ""      Then Begin              \! sin Dato Asociado 
       If TS.IO.KEYS(4) = 100 Then Begin              \! Tecla de NO-VENTA(No.Bco)
          If TS.IO.DATA$(4)      > "0"  And           \! Verifica Rango de los
             TS.IO.DATA$(4)      < "99" And           \! Bancos Locales 
             TS.IO.KEYS(3)       =  78  And           \! Si digita "/"(No.Cheq)
             LEN(TS.IO.DATA$(3)) <   9  And           \! Verifica 4 dig de Cheque
             TS.IO.DATA$(2)     <>  ""  Then Begin     ! Verif. exist. dato Cta
             NUCH$ = TS.IO.DATA$(3)                    ! Guarda No Cheque Subs
                PAGO.CH% = 1                           ! y coloca Flag de OK
          EndIf Else Begin                             ! de lo contrario
             TS.GUIDANCE = 1107                        ! Avisa error a cajero  
             TS.IO.MOTORKEY = 0                        ! Reduce procesamiento
          EndIf                                        ! y finaliza ERROR
       EndIf                                           ! y finaliza validacion
 EndIf                                                 !

!------------------ Verificaci¢n de los Datos de la Concesi¢n -----------------

If TS.IO.MOTORKEY = 220 Then Begin                    ! Es Key/Dpto 19=CONC.?
   If TS.IO.DATA$(10) = "" Then Begin                 ! y NO hay #Factura
     Call Visores4690(1,"FALTA EL #DE","FACTURA..!",0,"L")
     TS.IO.MOTORKEY=0                                ! Permanece en este
   EndIf Else Begin                                   ! De lo contrario
      FACT.NO% = 1                                    ! Coloca Flag Factura
   EndIf                                              ! Fin de #Factura
EndIf                                                 ! Fin de CONCESIONARIO
 

!---------- Habilita el BALDUE si NO se Digita Vr Pago (+) o (-) -------------

If TS.IO.STATE = 10     And                          \! Si esta en MAIN y se
   TS.IO.DATA$(3) <> "" And                          \! digita variedad,
   TS.IO.DATA$(7) = ""  And                          \! y NO se digita valor
    (TS.IO.KEYS(7) = 93 And                          \! 
    TS.IO.KEYS(7) = 94 OR                            \! a Red Multicolor
    TS.IO.KEYS(7) = 95 OR                            \! a Tarjeta de Credito o 
    TS.IO.KEYS(7) = 96)    Then Begin                 ! a CR, entonces se hace   
    If Val(NO.MOD$) <> 0 Then Begin                  \!
       TS.IO.DATA$(7) = VRCRE$                        ! el pago=Cuota Moderad
    EndIf Else Begin
       If VR.EPS% <> 0 Then Begin                     ! Si hay Cuota Moderad
          TS.IO.DATA$(7) = STR$(VR.EPS%)              ! el pago=Cuota Moderad
          VR.EPS% = 0                                 ! Reset Cuota Moderad
       EndIf Else Begin                               ! de lo contrario
          TS.IO.DATA$(7) = STR$(TS.BALDUE(0))         ! el pago=BALDUE. 
       EndIf                                          ! Fin de Cuota EPS
       VRCRE$ = TS.IO.DATA$(7)                        ! Valor del Credito
    EndIf
    If SINF% > 1 Then TS.IO.DATA$(3)  = "6"           ! Si Fac Inv, Asig Vrd=6
EndIf                                                 ! Fin de Condic

If TS.IO.STATE = 10     And                        \! Si esta en MAIN y se
   TS.IO.DATA$(3) <> "" And                        \! digita variedad,
   TS.IO.DATA$(7) = ""  And                        \! y NO se digita valor
    (TS.IO.KEYS(7) = 93 OR                         \! a Red Multicolor
    TS.IO.KEYS(7) = 94 OR                          \! a Tarjeta de Credito o 
    TS.IO.KEYS(7) = 95 OR                          \! a Tarjeta de Credito o 
    TS.IO.KEYS(7) = 96)    Then Begin               ! a CR, entonces se hace   
    If TS.IO.KEYS(1) <> 70 Then \
     TS.IO.DATA$(7) = STR$(TS.BALDUE(0)) Else \             ! el pago=BALDUE. 
  	TS.IO.DATA$(7) = "0"
EndIf                                               ! Fin de Condic


If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "6"  And                         \! con Var CR Especial
   TS.IO.KEYS(7)  = 92   Then Begin                   ! y si Digita TECLA CR
EndIf                                                 ! Fin Condic

If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "4"  And                         \! con Var CR Especial
   TS.IO.KEYS(7)  = 96   Then Begin                   ! y si Digita TECLA CR
!         TS.IO.DATA$(9)=STR$(VAL(UNPACK$(NITE$)))     ! Coloca el Nit OK
!        NOEM$ = " MERCAMOS.COM "
 !       NITW$ = "805017554003"
         TS.IO.KEYS(9) = 90                           ! Indica Verific SMA
         MERCAMOS% = 1
EndIf                                                 ! Fin Condic

If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "3"  And                         \! con Var CR Especial
   TS.IO.KEYS(7)  = 96   Then Begin                   ! y si Digita TECLA CR
!         TS.IO.DATA$(9)=STR$(VAL(UNPACK$(NITE$)))     ! Coloca el Nit OK
!        NOEM$ = " MERCAMOS.COM "
 !       NITW$ = "805017554003"
         TS.IO.KEYS(9) = 90                           ! Indica Verific SMA
         MERCAMOS% = 1
EndIf                                                 ! Fin Condic

!Add 22Nov2011 Se libera el pago 66 de la linea de creditos - Grupo Retail - OVS
If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "6"  And                         \! con Var CR Especial
   TS.IO.KEYS(7)  = 96   Then Begin                   ! y si Digita TECLA CR
     TS.IO.KEYS(9) = 90                               ! Indica Verific SMA 
     MERCAMOS% = 1
     PROCE% = 0 
EndIf                                                 ! Fin Condic


!--------- Controla si Factura coresp a Pago CR/Medicam y VERIFICA ------------
!-- Registro pago minifactura
If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "1"  And                         \! con Variedad Medicam
   TS.IO.KEYS(4) <> 100  And                         \! Sin Tecla NO-VENTA
   TS.IO.KEYS(7)  = 96   Then Begin                  \! y si Digita TECLA CR
      If TI61% = 1       Then Begin                   ! Si todo Bien..
         TS.IO.DATA$(9) = UNPACK$(NITE1$)             ! Coloca el Nit OK
         TS.IO.KEYS(9)  = 90                          ! Indica Verific SMA
         If TS.BALDUE(0) < 0 Then Begin               ! Reintegro fuera de 
         	  TS.IO.DATA$(7) = "0"                      ! tiquete de una MF
         EndIf Else Begin                             ! Es una venta 
          TS.TEMP5$ = Unpack$(Nite1$)                 ! Nit MF
          TS.TEMP5$ = ";" + Right$(TS.TEMP5$,3) + ";" ! Toma numero de plan
!--- Se comentarea para todas las MF pasen por expressmed
          If Match(TS.TEMP5$,Gr.Mf.Lst$,1) <= 0 Then Begin ! Es MF natural
           If Gr.FeNat.Aplicado% = 0 Then Begin
     	       Call VISOR.AND.BORRAR("SE DEBE INGRESAR EL ID CLIENTE OBLIGAT.")   !
     	       Call VISOR.AND.BORRAR("TRX OBLIGATORIA PARAFACTURA ELECTRONICA")   !
             Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)												    !
             TS.IO.MOTORKEY = 73																						    !
             Exit Sub 
           EndIf
           Gr.Mnf.Fac$ = NO.FAC$
          EndIf Else Gr.Mnf.Fac$ = "9999999"
         EndIf																				! Fin proceso pago MF
      EndIf Else Begin                                ! Si no Hubo Lect OK
         If TI65% = 1       Then Begin                ! Si era CVC originalmente
           Call Visores4690(1," NO ES FAC/MED/EMP.!",                  \! Factura
                              " VERIFIQUE:"+STR$(VAL(FAC$)),0,"L")      ! en Display Operador
            TS.IO.MOTORKEY = 0                        ! Reduce procesamiento
         EndIf Else Begin                             ! Si no Hubo Lect OK
           TS.GUIDANCE = 1108                         ! Avisa error omision
           TS.IO.MOTORKEY = 0                         ! Reduce procesamiento
         EndIf                                        ! Fin de Error
      EndIf                                           ! Fin Validac Fact
EndIf                                                 ! Fin Condic

!------------ Controla si Factura coresp a CVC. 60% y VERIFICA ---------------

If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
   TS.IO.DATA$(3) = "5"  And                         \! con Variedad CVC
   TS.IO.KEYS(4) <> 100  And                         \! Sin Tecla NO-VENTA
   TS.IO.KEYS(7)  = 96   Then Begin                  \! y si Digita TECLA CR
      If TI65% = 1       Then Begin                   ! Si todo Bien..
         TS.IO.DATA$(7) = STR$(TS.BALDUE(0)*.6)       ! el pago=60% de BALDUE. 
         VRCRE$ = TS.IO.DATA$(7)                      ! Valor del Credito
         TS.IO.DATA$(9) = UNPACK$(NITE2$)             ! Coloca el Nit OK
         TS.IO.KEYS(9)  = 90                          ! Indica Verific SMA
      EndIf Else Begin                                ! Si no Hubo Lect OK
      If TI61% = 1       Then Begin                   ! Si era Medicam orignlmt
        Call Visores4690(1," NO ES FACT. CVC..! ",                     \! Factura
                           " VERIFIQUE:"+STR$(VAL(FAC$)),0,"L")         ! Muestra Error
         TS.IO.MOTORKEY = 0                           ! Reduce procesamiento
      EndIf Else Begin                                ! Si no Hubo Lect OK
        TS.GUIDANCE = 1108                            ! Avisa error omision
        TS.IO.MOTORKEY = 0                            ! Reduce procesamiento
      EndIf                                           ! 
    EndIf                                             ! Fin Condic
EndIf                                                 ! Fin Condic

!------------- Controla que NO hubo Fact para CR sin Factura ------------------

!Add Se libera la forma de pago 66 Grupo Retail - OVS 22Nov2011
!If TS.IO.STATE    = 10   And                         \! Si esta en MAIN 
!   TS.IO.KEYS(3)  = 78   And                         \! y si digita "/"
!   TS.IO.DATA$(3) = "6"  And                         \! con Var CR sin Fact
!   TS.IO.KEYS(4) <> 100  And                         \! Sin Tecla NO-VENTA
!   TS.IO.KEYS(7)  = 96   Then Begin                   ! y si Digita TECLA CR
!      If TI66% <> 1      Then Begin                   ! Si todo Bien..
!        TS.GUIDANCE = 1108                            ! Avisa error omision
!        TS.IO.MOTORKEY = 0                            ! Reduce procesamiento
!      EndIf                                           ! 
!EndIf                                                 ! Fin Condic

!--------- Controla si hay Numero de Variedad para algunos Pagos  ------------

If TS.IO.STATE        = 10  And                      \! Si esta en MAIN y NO se
   TS.IO.KEYS(3)      = 0   And                      \! digita (/) con variedad, a
   (TS.IO.KEYS(7)     = 92  OR                       \! CHEQUE, o 
    TS.IO.KEYS(7)     = 94  OR                       \! Red Multicolor o
    TS.IO.KEYS(7)     = 95  OR                       \! Tarjeta Credito o
    TS.IO.KEYS(7)     = 96)    Then Begin             ! Linea de CR, entonces 
       TS.IO.DATA$(3) = "0"                           ! anula numero de variedad
       TS.IO.KEYS(3)  = 78                            ! y se simula un ERROR..!
EndIf                                                 ! Fin de Condic

!---------------------------------------------------------------------------
!--------- Control para CONVERTIR a Tecla de CR con Mini Factura --------------

If (TS.IO.MOTORKEY = 96  OR                          \! Si es Pago a Credito
   TS.IO.MOTORKEY  = 90) And                         \! ¢ Verificacion Credito
   TS.IO.DATA$(7) <> ""  Then Begin                   ! y se digita valor
   CEDU$ = CEDU$                                      ! Guarda la CC
    VENDE$ = MID$(NO.VEN$,1,6)                        ! Guarda Vendedor
!   VENDE$ = LEFT$("000000" + VENDE$,6)               ! Inserta 0's a Vendedor
   NOMBV$ = " "                                       ! Reset al Nombre Vend
   VAR.CR$ = TS.IO.DATA$(3)                           ! Guarda Variedad
   
  If VAR.CR$ = "6" Then Begin
     CALL BUSCAVEND                                     ! Busca Vendedor
  EndIf  

   CALL ETIQUETAR                                     ! Busca Variedad de CR
!       If VAR.CR$ = "6"   Then Begin
         KEY.OP$ = RIGHT$(PACK$("0000000000"+KEY.OP$),5)
         READ FORM "C32 C20 C20";#44 KEY KEY.OP$;                \! Lee EAMOPERA
         CE.NADA1$,NOM.OPER$,CE.NADA2$                            !                         
         VENDIO$ = "Vend. "+ NO.VEN$ + " " + NOM.OPER$      
         Vend.Medica$ = NO.VEN$
 !       EndIf
   If PROCE% = 1 And NOM.OPER$ <> "" Then Begin       ! Si hay CR(mf)
      TI65% = 0                                       ! NO para CVC
      TS.IO.KEYS(03) = 78                             ! Asigna SLASH
      TS.IO.DATA$(3) = "6"                            ! Asigna Variedad
      TS.IO.KEYS(07) = 93                             ! Asigna F/S=Otros Pagos
      TS.IO.MOTORKEY = 93                             ! Motor Otros Pagos
   EndIf                                              ! Fin de CR(mf)
EndIf                                                 ! Fin de Condic

!--------- Control para Pagos con Variedad que requieren Valor  ------------

If TS.IO.STATE        = 10 And                       \! Si esta en MAIN y se
   TS.IO.DATA$(3)    <> "" And                       \! digita codigo de variedad,
   TS.IO.DATA$(7)     = "" And                       \! y NO se digita valor con pago
   (TS.IO.KEYS(7)     = 91 OR                        \! a EFECTIVO con o sin Factura, o
    TS.IO.KEYS(7)     = 92)   Then Begin              ! CHEQUE con o sin Factura, se
       TS.IO.DATA$(3) = "0"                           ! anula numero de variedad
       TS.IO.KEYS(3)  = 78                            ! y se simula un ERROR..!
EndIf                                                 !

!--------- Control para los Datos Asociados a Pagos Electronicos   ------------

If TS.IO.STATE = 10     And                          \! Si esta en MAIN 
   TS.IO.DATA$(3) <> "" And                          \! y hay codigo de variedad,
   TS.IO.DATA$(7) <> "" And                          \! y hay valor
   (TS.IO.KEYS(7) =  94 OR                           \! con pago a Red o
    TS.IO.KEYS(7) =  95)     Then Begin               ! con pago a Tarj Credito
      If NO.APR$  = ""       OR                      \! Si NO digita No.Aprob
         NO.CED$  = ""       Then Begin               ! o no tiene Dato Asociado
            TS.GUIDANCE = 1109                        ! Avisa error omision a cajero
         TS.IO.MOTORKEY = 0                           ! Reduce procesamiento
      EndIf                                           ! y finaliza validacion
EndIf                                                 !

!---------------- Control para los Datos de los Cheques --------------------

If TS.IO.STATE          = 10 And                     \! Si esta en MAIN 
   TS.IO.DATA$(3)      <> "6" And                     \! y hay codigo de variedad,
   TS.IO.DATA$(7)      <> "" And                     \! y hay valor
   TS.IO.KEYS(7)        = 92     Then Begin           ! con pago a CHEQUE
      If VAL(NO.CTA$)   = 0      Then Begin           ! Si no hay datos Girador
         TS.GUIDANCE    = 1107                        ! Avisa error omision a cajero
         TS.IO.MOTORKEY = 0                           ! Reduce procesamiento
      EndIf                                           !
EndIf                                                 !

End SUB
!------------------------------------------------------------------------------

