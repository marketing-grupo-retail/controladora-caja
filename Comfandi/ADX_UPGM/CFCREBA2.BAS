!             PROGRAMA CFCREBA2.BAS
!           PROYECTO *** PAPELERIA HABADIA ***
!----------------------------------------------------------
!  Este programa  crea  sobre un archivo  vac¡o C:\EAMITE.DAT
!  (que tiene las mismas  caracter¡sticas  del EAMITEMR.DAT),
!  la totalidad  de los art¡culos que se  van  a mercadear en
!  los diferentes puntos de Venta.
!
!  El programa realiza lo siguiente:
!
!  1.- S¢lo se seleccionan aquellos art¡culos que pertenezcan
!      al C¢digo del Almac‚n del archivo ADXALMA.DAT
!
!  2.- Se  ha  implementado  un nuevo Registro  de Datos para
!      incluir la Creacion/Modific de Codigos de Barras
! 
!  3.- Se  ha inclu¡do un control extra para verificar el No.
!      de departamento  que  trae la  novedad, para evitar la
!      contabilizaci¢n  errada  de  las  Ventas en el Informe
!      por departamentos.
!  4.- Se  ha Modificado el programa para activar los flags de IVA.
!      y los flags para acumulacion de puntos.
!*******************************************************************************
! Modificaciones:
! Mod 27JUL2022
! Se ajusta programa CFCREBA1.BAS para carga de novedades de precio
! de papeleria HABADIA, solicitado por el área de mercadeo.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------------------------------------

! Ambiente de Controlador

  %ENVIRON C 

! Definicion de variables de usuario Globales
  Integer*1     GLOBAL VALID                              ! Variable externa

! Definicion de variables de usuario locales
  String GLOBAL BATCH$,    SEQUENCE$,    COUNTER$,       \!
                INDICAT2$, FEC$,         ALM$,           \!
                ALMA$,     NUAL$,        NOAL$,          \!
                REGI$,     DEP$,         HORA$,          \!
                QUANPRIC$, OPCION$,      GrExcl$

  Integer*1     DMCREQ,    DMCMT,        DMCPO,          \!
                INDICAT0,  INDICAT1,     DMCESTAT,       \!
                SW.FIN,    SWREP,        INDICAT1A,      \!
                SWERROR,   CON.LIN%,     Gr.Escal%,      \!
                Gr.Techo%

  Integer*2     DMCRCSF,   DMCSTSF,      DMCERRCT,       \!
                UEX1,      UEX2,         GREX1

  Integer*4     DMCERROF,  TOTBUE,       TOTCERO,        \!
                TOTLEI,    PEPRREQ%,     PREREQ%,        \!
                PLUPRE%,   SINDES%,      ICON%,          \!
                IV00%,     IV15%,        IV20%,     IV35%, \!
                GR.DATE%

  Integer*4     TAXA%, TAXB%, TAXC%, TAXD%, TAXE% 

  String        GLOBAL GR.DATA$, Ofile$               ! Ampliacion EAMITEMR GR-OVS 9 Abr 2010

! ------------- Rutina de Impresion de errores y Totales -------------

Function ADXSTART (NAME$,PARM$,MESS$) EXTERNAL
INTEGER *2 ADXSTART
STRING NAME$,PARM$,MESS$
End Function

!------------------------------------------------------------------------------
!
!                            PUTN1/PUTN2/PUTN4
!
!       These routines insert a one/two/four byte integer into a string.
!       P2 is the offset within the string and P3 is the source integer
!
!------------------------------------------------------------------------------
  
  Function PUTN1(P1$,P2,P3) EXTERNAL
  String P1$
  Integer*2 P2
  Integer*1 P3
  End Function
  
  Function PUTN2(P1$,P2,P3) EXTERNAL
  String P1$
  Integer*2 P2,P3
  End Function
  
  Function PUTN4(P1$,P2,P3) EXTERNAL
  String P1$
  Integer*2 P2
  Integer*4 P3
  End Function

 Function SubSTR (P1$,P2,P3$,P4,P5) EXTERNAL
 String P1$,P3$
 Integer*2 P2,P4,P5
 End Function

Sub REPORTER
   !LPRINTER
   IF CON.LIN% > 56 Then Begin
      Print #33; CHR$(12)
      SWREP = 0
      CON.LIN% = 7
   EndIf
   IF SWREP = 0 Then Begin
      SWREP = 1
      PRINT #33; " "
      PRINT #33; "   PROCESO DE MANTENIMIENTO DE PRECIOS DESDE EL SISTEMA CENTRAL AL POS"
      PRINT #33; "            SUPERMERCADO  No : "+ALM$+ "  "+NOAL$
      PRINT #33; "  --------------------------------------------------------------------"
      PRINT #33; "  "
      PRINT #33; "  "
      PRINT #33; "    CODIGO     DESCRIPCION      DEPTO   VR/VENTA    OBSERVACIONES "
      PRINT #33; "  --------------------------------------------------------------------"
   EndIf 
   IF SWERROR = 1 AND DEP$ = "016" Then Begin 
      SWERROR = 0
   EndIf 
   IF SWERROR = 1 Then Begin
       CODI$  = RIGHT$("      0" + COD$ ,7)
       PRINT #33; "  "+TIP$+" "+CODI$+"  "+DES$+"  "+DEP$
       PRINT #33; Val(PRE$)
       PRINT #33; "    "+MENS$+NOM$
       CON.LIN% = CON.LIN% + 1
   EndIf
End Sub

! ------------- Rutina de ERROR de Inicio  por falta de CONTROL -------------

Sub NOINICIA
   CONSOLE
   CLEARS
   PRINT " "
   PRINT "                 Se SuspEnde en el Registro: ";TOTLEI       !
   PRINT " "
   PRINT "                 --------------------------------------------"
   PRINT "                |  No  se  puede  CONTINUAR  el proceso por  |"
   PRINT "                |  falta del registro de CONTROL de Fecha ¢  |"
   PRINT "                |  porque el LOTE NO termina con el registro |"
   PRINT "                |  de control de 99999999999's               |"
   PRINT "                |                                            |"
   PRINT "                |  Verifique el Archivo PHABADIA.DAT en C:   |"
   PRINT "                |  e incluya la informaci¢n del CONTROL en   |"
   PRINT "                |  el FORMATO   rAAAAMMDDHHMMnnnn , donde :  |"
   PRINT "                |                                            |"
   PRINT "                |  Ejemplo:                                  |"
   PRINT "                |        r = 4        (Tipo Control Fecha)   |"
   PRINT "                |                                            |"
   PRINT "                | AAAAMMDD = 19940722 (Fecha de los Datos)   |"
   PRINT "                |                        Para Jul. 22/94     |"
   PRINT "                |     HHMM = 1500     (Hora 3:00 PM)         |"
   PRINT "                |     nnnn = 0011     (No. del Almac‚n)      |"
   PRINT "                |                                            |"
   PRINT "                |                 Digite INTRO por Favor..!  |"
   INPUT "                 --------------------------------------------";NOM$
End Sub

!  -------- Rutina para crear Registros del BATCH de actualizacion ---------

Sub LLENAREG
    IF End #25 Then SALIR                   ! Cuando se terminan Datos
    READ #25;LINE REGI$                     ! Lee Registro
    TOTLEI = TOTLEI + 1                     ! Suma en Contador
    CONSOLE                                 ! Asigna Consola
    LOCATE 18,25                            ! Coloca Cursor
    PRINT "Registros Leidos: ";TOTLEI       ! Despliega Contad
    TIP$ = LEFT$(REGI$,1)                   ! Toma Tipo Reg 1, 2, 3, o 4
    IF TIP$    = "4" Then Begin             ! Si hay Fecha de Control
       IF PRIMO = 0  Then Begin             ! y es 1er Lectura
          PRIMO = 1                         ! Marca Inicio OK
          FEC$  = MID$(REGI$,4,6)           ! Guarda Fecha
          HORA$ = MID$(REGI$,10,4)          ! Hora del Batch
          ALM$  = MID$(REGI$,10,4)          ! Guarda Almacen
          ALM$  = STR$(VAL(ALM$))           ! Guarda Almacen
          ALM$  = NUAL$                     ! Asigna centro costo matriculado adxalma 27Jul2022
          UEX2  = VAL(MID$(REGI$,6,4) +    \! Toma Mes y Dia
                  MID$(REGI$,5,1))          ! y Ult. Digito del Ano
          UEX2  = 0                         ! Ajuste dato en cero para manejo nativo EMS
          GR.DATE% = Val(MID$(REGI$,2,8))  ! Fecha de actualizacion archivo Expansion
          SWERROR = 1                       ! Indica By-Pass en Grabacion
          Exit Sub                          ! Termina Sub-Rutina
       EndIf ELSE Begin                     ! Fin 1er Lect
          PRIMO = 0                         ! Resest de Lectura
       EndIf                                ! Fin de NO es 1er Lect
    EndIf                                   ! Fin no es reg Ctrl
    IF NUAL$ <> ALM$  Then Exit Sub         ! Omite Reg de Otro Almacen
    IF TIP$ = "9" Then Exit Sub             ! Se omite Control
    IF PRIMO = 0 Then Begin                 ! Si NO hubo Reg Control
       CALL NOINICIA                        ! Notifica Error
       STOP                                 ! PARA o TERMINA
    EndIf                                   ! Fin
    COD$ = "8" + MID$(REGI$,3,6)            ! Codigo Articulo o PLU
    
    KEY$=RIGHT$(PACK$(STRING$(12,"0")+     \! Arma Llave en formato PD
         COD$),6)                           ! Traduccion de COD$ a llave KEY$
    
    DES$ = MID$(REGI$,9,18)                 ! Descripcion del Articulo
    IF LEFT$(DES$,1) = "." Then            \! Sin Descripcion
       SINDES% = SINDES% + 1                ! Suma Errores
    DEP$ = MID$(REGI$,27,3)                 ! Codigo de Seccion o Depto
    IF DEP$ = "003" Then DEP$ = "016"       ! Cambia Dep Fruver SIGED
    PRE$ = STR$(VAL(MID$(REGI$,30,11)))     ! Prec/Entero de Venta + Iva
    UVT$ = MID$(REGI$,43,3)                 ! Contenido o Unidades de Vta
    TDA$ = MID$(REGI$,46,1)                 ! Tipo Artic 0=Norm;    1=Grs;
                                            !            2=ReqPrec; 3=Cupon;
                                            !            4=Consec     

    BAR$ = MID$(REGI$,47,12)                ! Codigo de Barras del PLU 
    GR.DATA$ = STRING$(15,"0")								  		! Init user data
    GR.DATA$ = "1" + MID$(REGI$,69,15) + GR.DATA$   ! Estructura comercial producto
    CALL PUTN4(GR.DATA$, 20,GR.DATE%)	          		! Fecha de actualizacion 
    Gr.Escal% = Val(MID$(REGI$,84,1))       ! Si es articulo escalonado Mod 15Dic2011
    GrExcl$   = MID$(REGI$,85,1)            ! Si es articulo es excluido de IVA Mod 10 Mzo 2017
    Gr.Techo% = Val(MID$(REGI$,86,1))       ! Si maneja precio techo Mod 14Nov2017
    
    !Print "DATA ARMADO "+ GR.DATA$
    !WAIT ; 1800
    
!INDICAT1 = 4H                                 ! PrEnde puntaje sin iva
    LOCATE 19,25                            ! Coloca Cursor
    PRINT "Long. = ";STR$(LEN(REGI$))
    IVA$ = "00" :  ICO$ = "0000000"         ! Restaura Inf Iva Imp/Cons
    IF LEN(REGI$) > 60 Then Begin           ! Si es nuevo Diseno
       IVA$ = MID$(REGI$,60,2)              ! Toma Iva
       ICO$ = MID$(REGI$,62,7)              ! Toma Impo Consumo
    EndIf                                   !

    MEP$ = "0"                              ! Metodo de Precio
    VRE$ = "0"                              ! Prec.de Venta Reducido + Iva
    PRQ$ = "0"                              ! Precio Requerido
    PPR$ = "0"                              ! Peso/Precio Requerido
    IF TDA$ = "1" Then PPR$ = "1"           ! Peso/Precio Requerido
    IF TDA$ = "2" Then PRQ$ = "1"           ! Precio Requerido
    IF TDA$ = "3" Then PRQ$ = "1"           ! Cupon de Fabric
    IF TDA$ = "4" Then PRQ$ = "1"           ! Cupon de almacen
    CNP$ = "0"                              ! Cantidad NO Permitida
    CRQ$ = "0"                              ! Cantidad Requerida
    ADE$ = "0"                              ! Artic de Excep
    AAV$ = "0"                              ! Autoriz para Venta
    ARV$ = "0"                              ! Rechaz en la Venta
    GMA$ = "0"                              ! Grupo Multiprec Artic
    ADS$ = "1"                              ! Aplicable Descuento a PLU
    POE$ = "0"                              ! Pago Efect Obligatorio o Fs
    IF TDA$ = "1" OR UVT$ = "  " Then      \! Si NO trae Indic. de Vta Und
       UVT$ = "0"                          \! Venta Unitaria PLU
    ELSE                                   \! de lo Contrario
       UVT$ = STR$(VAL(UVT$))               ! Toma el Contenido que viene 
    SWERROR =  1                            ! Inicia Validacion
    LLAVE$ = PACK$("000099999" + DEP$)      ! Arma KEY Plu Dptal
    NOM$ = "  "                             ! Reset Nombre
    READ FORM "C77"; #1 KEY LLAVE$;REGI$    ! Lectura de EAMITEMR Ampliacion EAMITEMR GR-OVS 9 Abr 2010
    MENS$ = ""                              ! Restaura Mensaje Error
    IF NOM$ = "**" Then Begin               ! Si no leyo Depto
       MENS$ = "*DEPTO*  "                  ! Coloca Mensaje
       NOM$ = "*DPTO SIN DEFINIR*"          ! No esta en Store Opts?
    EndIf ELSE Begin                        ! Si hubo Lectura
       SWERROR = 0                          ! Pasan todos OK
    EndIf                                   ! Termina Validac de Depto
    IF TIP$ = "2" AND                      \! Si es Modific
       VAL(PRE$) = 0 Then Begin             ! y trae Valor CERO
          LLAVE$ = "000000000000"           ! Arma llave
          LLAVE$ = RIGHT$(LLAVE$+COD$,12)   ! de 12 acract y
          LLAVE$ = PACK$(LLAVE$)            ! empaquete KEY Plu 
          NOM$ = "  "                       ! Borra Nombre
          READ FORM "C77";                 \! Lee el arch de Artic Ampliacion EAMITEMR GR-OVS 9 Abr 2010
               #1 KEY LLAVE$;REGI$          ! EAMITEMR
          MENS$ = ""                        ! Restaura Mensaje Error
          IF NOM$ = "**" Then Begin         ! Si no leyo PLU
             MENS$ = "*MOD*  "              ! Coloca Mensaje
             NOM$ = "*NO EXISTE*"           ! No esta en Store Opts?
             SWERROR = 1                    ! Marca ERROR
          EndIf ELSE Begin                  ! Si esta en Arch
             PRE$=UNPACK$(MID$(REGI$,18,5)) ! restaura valor Vta
             SWERROR = 0                    ! Cuando pasan todos OK
          EndIf                             ! Termina Validac de Depto
    EndIf                                   ! Termina Validac de Depto

    IF LEN(COD$) =  0  OR                  \! Si no hay PLU o
       PRE$      = "0" OR                  \! Si no hay PLU o
       COD$ = "     " Then Begin            ! viene en espacios
          MENS$ = MENS$ + "*VR/VTA* "       ! Coloca Mensaje
          SWERROR = 1                       ! Marca ERROR
    EndIf                                   ! Termina Validac de PLU
!-- Mod 2 ABR 2018
    If Val(DEP$) <= 0 Then Begin						! Error definicion departamento
       MENS$ = "*DEPTO NO VALIDO "          ! Coloca Mensaje
       NOM$ = "*DEPTO INVALIDO  *"          ! No esta en Store Opts?
    	 SWERROR = 1
    EndIf																		!
    IF SWERROR = 1 Then Begin               ! Si hay errores
       TOTCERO = TOTCERO + 1                ! Acumula en Contador Errores
       CALL REPORTER                        ! Efectua Reporte
       Exit Sub                             ! y se omite Grabacion
    EndIf                                   !

!    KEY$=RIGHT$(PACK$(STRING$(12,"0")+     \! Arma Llave en formato PD
!         COD$),6)                           ! Traduccion de COD$ a llave KEY$

    IF TIP$ = "3" Then Exit Sub             ! Los borrados no se llena el Reg

! ----------- Llenar el registro para los agregados y actualizados -------
    INDICAT0  = 0                                   ! Grabar Venta/Autoriz Vta
    INDICAT1  = 0                                   ! Plan Iva/Desc/Fct Cupon NO
    INDICAT1A = 0                                   ! Puede hacer Etiqueta
    If Gr.Escal% = 1 Then Begin 										! Si es articulo escalonado
    	INDICAT1A = INDICAT1A Or 02H   		  					! Activa Flag para manejo de escalonado
    EndIf
    If Gr.Techo% = 1 Then Begin 										! Si articulo con precio techo 14Nov2017
    	INDICAT1A = INDICAT1A Or 01H   		  					! Activa Flag para manejo precio techo
    EndIf
    
    !   Listado articulos anchetas, almuerzos para que no los puedan vender por plu
    !																						! Fin activacion Escalonado
If cod$ = "115994" 				OR\ ! LISTA
   cod$ = "115997"				OR\ ! ITEMS PLU ANCHETAS - ALMUERZOS
   COD$ = "115999"				OR\ !
   COD$ = "116002"				OR\ !
   COD$ = "116008"				OR\ !
   COD$ = "116010"				OR\ !
   COD$ = "70179"				  OR\ !
   COD$ = "114395"				OR\ !
   COD$ = "114396"				OR\ !
   COD$ = "114570"				OR\ !
   COD$ = "152387"				OR\ !
   COD$ = "126447"				OR\ !
   COD$ = "200563"				OR\ !
   COD$ = "202441"				OR\ !
   COD$ = "202442"				OR\ !
   COD$ = "202443"				OR\ ! 
   COD$ = "202444"				OR\ !
   COD$ = "202445"				OR\ !
   COD$ = "202446"				OR\ !
   COD$ = "202447"				OR\ !
   COD$ = "202444"				OR\ !
   COD$ = "202445"				OR\ !
   COD$ = "202446"				OR\ !
   COD$ = "202447"				OR\ !
   COD$ = "202448"				OR\ !
   COD$ = "202449"				OR\ !
   COD$ = "202450"				OR\ !
   COD$ = "202454"				OR\ !
   COD$ = "202455"				OR\ !
   COD$ = "202457"				OR\ !    
   COD$ = "202458"				OR\ !
   COD$ = "202459"				OR\ !
   COD$ = "202463"				OR\ !
   COD$ = "202464"				OR\ !
   COD$ = "152388"   Then begin					
	INDICAT0 = 05H				    ! APAGA FLAG AUTORIZED For SALE
EndIf

If cod$ = "202465" 				OR\ ! LISTA ITEMS ANCHETAS - ALMUERZOS
   cod$ = "202468"				OR\ ! ITEMS
   cod$ = "202469"				OR\ ! ITEMS
   cod$ = "202470"				OR\ ! ITEMS
   cod$ = "202472"				OR\ ! ITEMS
   cod$ = "202473"				OR\ ! ITEMS
   cod$ = "202475"				OR\ ! ITEMS
   cod$ = "202476"				OR\ ! ITEMS
   cod$ = "202477"				OR\ ! ITEMS
   cod$ = "202479"				OR\ ! ITEMS
   cod$ = "202481"				OR\ ! ITEMS
   cod$ = "203103"				OR\ ! ITEMS
   cod$ = "202979"				OR\ ! ITEMS
   cod$ = "202484"				OR\ ! ITEMS
   cod$ = "202485"				OR\ ! ITEMS
   cod$ = "202486"				OR\ ! ITEMS
   cod$ = "202487"				OR\ ! ITEMS
   cod$ = "202488"				OR\ ! ITEMS
   cod$ = "202493"				OR\ ! ITEMS
   cod$ = "202495"				OR\ ! ITEMS
   cod$ = "202494"				OR\ ! ITEMS
   cod$ = "115997"				OR\ ! ITEMS
   cod$ = "202496"				OR\ ! ITEMS
   cod$ = "202497"				OR\ ! ITEMS
   cod$ = "202547"				OR\ ! ITEMS
   cod$ = "202545"				OR\ ! ITEMS
   cod$ = "202543"				OR\ ! ITEMS
   cod$ = "138561"				OR\ ! ITEMS
   cod$ = "138562"				OR\ ! ITEMS
   COD$ = "152388"   Then begin					
	INDICAT0 = 05H				    ! APAGA FLAG AUTORIZED For SALE
ENDIF 

    IF IVA$ = "16" Then Begin                       !
!       INDICAT1 = 84H                               ! Si hay Plan Iva A

       INDICAT1 = 44H                               ! Si hay Plan Iva B - Si Error en Siged iva del 16
       TAXA% = TAXA% +1                             ! Acumula Contad
    EndIf                                           !
    IF IVA$ = "19" Then Begin                       !
       INDICAT1 = 44H                               ! Si hay Plan Iva B
       TAXB% = TAXB% +1                             ! Acumula Contad
    EndIf                                           !
    IF IVA$ = "05" Then Begin                       !
       INDICAT1 = 24H                               ! Si hay Plan Iva C
       TAXC% = TAXC% +1                             ! Acumula Contad
    EndIf                                           !
    IF IVA$ = "08" Then Begin                       !
       INDICAT1 = 14H                               ! Si hay Plan Iva D
       TAXD% = TAXD% +1                             ! Acumula Contad
    EndIf                                           !
    IF IVA$ = "00" Then Begin                       ! *** DISPONIBLE *****
    	If GrExcl$ <> "1" Then Begin                  ! Si Iva Excento
         INDICAT1 = 4H                              ! PrEnde puntaje sin iva
         TAXE% = TAXE% +1                           ! Acumula Contad
      EndIf Else Begin															! Si Iva Excluido
         INDICAT1 = 24H															! Enciende Plan Iva C
         INDICAT1 = INDICAT1 Or 14H                 ! Enciende Plan Iva D
         TAXE% = TAXE% +1                           ! Acumula Contad
      EndIf   																			!
    EndIf                                           !

!===== Articulo aplica puntos y es descontable 

!    INDICAT1 = INDICAT1 AND 11111101B 		    		  ! Item Descontable

    INDICAT1 = INDICAT1 OR 00000100B 		            ! Item aplica puntos

!=====

    UEX1 = 0                                        ! Impuesto Consumo
    GREX1 = 0 
    If Val(ICO$) > 0 Then Begin                     !
       !UEX1 = VAL(ICO$)
       GREX1 = VAL(ICO$)                            ! Impoconsumo nuevo campo
       ICON% = ICON% +1                             ! Acumula Contad
    EndIf                                           !
    CALL PUTN4(GR.DATA$, 16,GREX1)	        	   		! Impuesto al consumo
   
!! ** Arreglo para flag descuento en pollos 

    IF UVT$ > "1" Then  INDICAT0 = 20H              !
    IF PPR$ = "1" Then  INDICAT0 = INDICAT0 OR  40H ! Peso/Precio Requerido
    IF PRQ$ = "1" Then  INDICAT0 = INDICAT0 OR  10H ! Precio Requerido
    IF PPR$ = "1" Then  PEPRREQ% = PEPRREQ% + 1     ! Suma Peso/Precio Req
    IF PRQ$ = "1" Then  PREREQ%  = PREREQ%  + 1     ! Suma Precio Requerido
    IF TDA$ = "0" Then  PLUPRE%  = PLUPRE%  + 1     ! Suma Plu por Precio

!    IF ADS$ = "1" AND POE$="1" Then INDICAT1 = 08H  ! Si hay Descto y Pago Oblig
!    IF ADS$ = "0" AND POE$="1" Then INDICAT1 = 10H  ! Si no Hay Descto y SI Pag Ob
!    IF ADS$ = "1" AND POE$="0" Then INDICAT1 = 01H  ! Si hay Descto y NO Pag Oblig
!    IF ADS$ = "0" AND POE$="0" Then INDICAT1 = 02H  ! Si no hay ninguno de los dos

    IF VAL(TDA$) < 3 Then TDA$ = "0"                ! Articulos Normales
    INDICAT2$ = PACK$(TDA$+MEP$)                    ! Item Vta Norm/met.prec=0
    DEPARTME$ = RIGHT$(PACK$("0000"+DEP$),2)        ! Numero departamento
    FAMILYN$  = PACK$("000000")                     ! Familia cupones(Cur/Prev)
    MPGROUP$  = PACK$("00")                         ! Precio en grupo
    SALEQUAN$ = PACK$(UVT$)                         ! Cantidad de venta
    SALEPRIC$ = RIGHT$(PACK$("0000000000"+PRE$),5)  ! Precio de venta
    LINKEDTO$ = PACK$("0000")                       ! Articulo enlazado
    ITEMNAME$ = LEFT$(DES$+STRING$(18," "),18)      ! Descripcion articulo
  Exit Sub
  SALIR:
    SW.FIN = 1
End Sub

! -------------  Rutina para crear compaginar Hora y Fecha --------------
  Sub CREAFECHA
      FRA.TIM$ = TIME$                              ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                  ! Arma horas
      FRA.M$   = MID$(FRA.TIM$,3,2)                 ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                 ! Arma segundos
      FRA.FEC$ = DATE$                              ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                  ! Arma ano
      FRA.MM$  = MID$(FRA.FEC$,3,2)                 ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                 ! Arma dia
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     + \! Arma la Linea
               "    El: "+FRA.DD$+"/"+FRA.MM$+"/"+ \! de Franqueo con
               FRA.AA$                              ! Hora y Fecha
  End Sub
!  -------------  Rutina para grabar en el archivo EAMD:nnn ---------------

Sub GRABAREG
    IF TIP$ = "4" Then Exit Sub                  ! Se omite grabar por Fecha
    IF TIP$ = "9" Then Begin                     ! Se omite grabar por Control
       PRIMO = 0                                 ! Restaura 1er lectura
       Exit Sub                                  ! Y regresa
    EndIf                                        !
    IF NUAL$ <> ALM$  Then Exit Sub              ! Omite Reg de Otro Almacen
    IF SWERROR = 1 Then Begin                    ! Si hubo error en Datos
         SWERROR = 0                             ! Se restaura condicion
         Exit Sub                                ! y se omite Grabacion
    EndIf                                        ! Fin de error
    FINR$ = CHR$(13)+CHR$(10)                    ! Marca de fin de registro
    DMCRCSF=DMCRCSF + 1                          ! Incrementa Contador
    IF TIP$ = "3" Then Begin                     ! Si es registro a eliminar
       DELREC 1;KEY$                             ! Borra Reg
    EndIf                                        !
    IF TIP$ = "3" AND                           \! Si es registro a eliminar
       VAL(BAR$) > 0 Then Begin                  ! y si hay codigo de Barras
       KEY$=RIGHT$(PACK$("000000000000"+BAR$),6) ! Arma Key Barras en form PD
       DELREC 1;KEY$                             ! Borra Reg
       TOTBAR = TOTBAR + 1                       !
    EndIf                                        !
   IF FAMILY = 1 Then FAMILYN$ =PACK$("003000") 
   IF VAL(TIP$) < 3 Then Begin                   ! Si es registro 1 o 2
       IF OPCION$ = "1" Then Begin               ! Actualiza PLU's y COD/BAR
          WRITE FORM "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C31";#1;   \
          KEY$,INDICAT0,INDICAT1,               \!
          INDICAT1A,INDICAT2$,DEPARTME$,        \!
          FAMILYN$,MPGROUP$,SALEQUAN$,          \!
          SALEPRIC$,LINKEDTO$,ITEMNAME$,        \!
          UEX1,UEX2, GR.DATA$                    !
       EndIf                                     !
       IF VAL(BAR$) > 0 Then Begin               ! y si hay codigo de Barras
          KEY$=RIGHT$(PACK$("000000000000"+BAR$),6) ! Arma Key Barras en form PD
          INDICAT2$ = PACK$(TDA$+"5")               ! Item Vta Norm/Met.Prec=5
          INDICAT0  = 01H
          QUANPRIC$ = RIGHT$(PACK$("000000000000"   \!
                   + COD$),6)                     ! Arma ALIAS en Form PD
!       TIP$ = "3"                                ! BORRA el Registro PM=5 
!       DELREC 1;KEY$                             ! Borra Registro
!       KEY$=RIGHT$(PACK$("000000000000"+BAR$),6) ! Arma Key Barras en form PD
       TIP$ = "1"                                 ! CREA reg nuevo de PM=5

          WRITE FORM "C6 3I1 C1 C2 C3 C1 C6 C2 C18 2I2 C31";#1;    \
          KEY$,INDICAT0,INDICAT1,               \!
          INDICAT1A,INDICAT2$,DEPARTME$,        \!
          FAMILYN$,MPGROUP$,QUANPRIC$,          \!
          LINKEDTO$,ITEMNAME$,UEX1,UEX2,GR.DATA$ !
          TOTBAR = TOTBAR + 1                    !
       EndIf                                     !
       
       IF LEFT$(BAR$,8)="88888888" Then Begin    ! ES OFERTA
          KEY$=PACK$("99999999")+RIGHT$(KEY$,2)  ! ARMA PLU OFERTA
          INDICAT2$ = PACK$(TDA$+MEP$)           ! Item Vta Norm/met.prec=0
          DEPARTME$ = PACK$("995")               !
          SALEQUAN$ = PACK$("00")                !  
          INDICAT1A  = 01H                       ! Plan Iva/Desc/Fct Cupon NO 
 	        INDICAT0 = 05H				                 ! APAGA FLAG AUT. VENTA
          WRITE FORM "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C31";#1;\
          KEY$,INDICAT0,INDICAT1,               \!
          INDICAT1A,INDICAT2$,DEPARTME$,        \!
          FAMILYN$,MPGROUP$,SALEQUAN$,          \!
          SALEPRIC$,LINKEDTO$,ITEMNAME$,        \!
          UEX1,UEX2, GR.DATA$                     !
       EndIf                                     !
       
    EndIf                                        !
  P2:                                            !
    TOTBUE = TOTBUE + 1                          !
End Sub

!  --------------   Rutina final de cierre del archivo   ----------------

  Sub FINAL
    SW.FIN = 1
    CLOSE 1
  End Sub

!----------------------  PROGRAMA  PRINCIPAL  ----------------------------
   FORMA$ = "###,###,###"
   ON ERROR GOTO ERRTRAP                       ! Rutina de Salida ERROR
   CALL CREAFECHA                              ! Busqueda de Fecha
   FEC1$ = FRA.LIN$                            ! Guarda Fecha
   OPCION$ = COMMAND$                          ! Opcion de corrida Progr
   If Ucase$(opcion$) = "VERSION" Then Begin
      Print "Carga novedades precios - Comfandi-Habadia, version 27/Jul/2022"
      Stop 
   Endif
   IF OPCION$ = "" Then OPCION$ = "1"          ! Valor por defecto = 1
   CON.LIN% = 60                               ! Cont de lineas
   OPEN "EAMITEMR" KEYED RECL 77 AS 1          ! Archivo de Art¡culos Ampliacion EAMITEMR GR-OVS 9 Abr 2010
   OPEN "C:\PHABADIA.DAT" AS 25 BUFFSIZE 1024   ! Archivo de Novedades
   OPEN "C:\ADXALMA.DAT" AS 26 BUFFSIZE 80     ! Archivo Control Almacen
   Ofile$ = "ADX_UDT1:APIH"+RIGHT$(DATE$,4)+".TXT" 
   CREATE Ofile$ As 33 BUFFSIZE 1024           ! Creacion log auditoria GR-OVS 16 Oct 2012
   READ #26;LINE ALMA$                         ! Lee Control Almac
   NUAL$ = STR$(VAL(LEFT$(ALMA$, 4)))          ! Guarda Num Almacen
   NOAL$ = RIGHT$(ALMA$,LEN(ALMA$)-12)         ! Guarda Nombre
   IF LEN(NOAL$) < 35 Then Begin               ! Si es letrero Corto
      NOAL$=NOAL$+STRING$(35-LEN(NOAL$)," ")   ! Le agrega espacios
   EndIf                                       ! Fin
   NOAL$ = LEFT$(NOAL$,35)                     ! Toma 35 Pos.de Nombre
   CLOSE 26                                    !

   CONSOLE
   CLEARS
   PRINT " "
   PRINT " "
   PRINT "         Almac‚n : ";NUAL$;"   ";NOAL$
   PRINT " "
   PRINT " "
   PRINT "                 ------------------------------------------"
   PRINT "                |                                          |"
   PRINT "                |  Proceso de conversion de  PHABADIA.DAT  |"
   PRINT "                |  para crear DIRECTAMENTE el Archivo de   |"
   PRINT "                |  Art¡culos EAMITEMR.DAT                  |"
   PRINT "                |  Version Actualizada 27 jUL 2022 G.R.    |"
   PRINT "                |                 Programa CFCREBA2.286    |"
   PRINT "                |                                          |"
   PRINT "                |  Inicia a las: ";FEC1$;"  |"
   Print "                 ------------------------------------------"
   
!   PRINT "                |                                          |"
!   PRINT "                |               Digite INTRO por Favor..!  |"
!   Input "                 ------------------------------------------";NOM$

   PRIMO   = 0                            ! Ctrl Lectura 1er Registro
   DMCRCSF = 0                            ! Numero de registros del archivo
   SW.FIN = 0                             ! Indica fin archivo del AS/400
   TOTCERO = 0                            ! Numero de registros ERRADOS
   WHILE SW.FIN = 0                       ! Ciclo para pasar la informacion
         CALL LLENAREG                    ! Pasa Datos de AS/400 a EAMD:nnn
         CALL GRABAREG                    ! Graba en el archivo EAMD:nnn
   WEnd                                   ! Fin del ciclo
   CALL FINAL                             ! Fin operaciones y grabar 
                                          ! regsitro de control
   CALL CREAFECHA
   FEC2$ = FRA.LIN$
   !LPRINTER
   IF CON.LIN% > 40 Then Begin
      Print #33; CHR$(12)
      Print #33; " "
      PRINT #33; "   PROCESO DE MANTENIMIENTO DE PRECIOS DESDE EL SISTEMA HABADIA AL POS"
      PRINT #33; "            SUPERMERCADO  No : "+ALM$+"  "+NOAL$
      PRINT #33; "  --------------------------------------------------------------------"
      PRINT #33; " "  :   PRINT #33; "  "
   EndIf
      PRINT #33; " "  :   PRINT #33; " "
      PRINT #33; "                   ITEMS SIN DESCRIPCION POS : "+Str$(SINDES%)
      PRINT #33; "                  ITEMS CON PRECIO REQUERIDO : "+Str$(PREREQ%)
      PRINT #33; "           ITEMS CON PESO Y PRECIO REQUERIDO : "+Str$(PEPRREQ%)
      PRINT #33; "           ITEMS CON PRECIO DESDE EL SISTEMA : "+Str$(PLUPRE%)
      PRINT #33; "    TOTAL REGISTROS LEIDOS DESDE PHABADIA.DAT: "+Str$(TOTLEI)
      PRINT #33; "                   ARTICULOS CREADOS CON PLU : "+Str$(TOTBUE)
      PRINT #33; "           CODIGOS DE BARRAS CREADOS (ALIAS) : "+Str$(TOTBAR)
      PRINT #33; "                    TOTAL REGISTROS GRABADOS : "+Str$(TOTBUE+TOTBAR)
      PRINT #33; "                 TOTAL REGISTROS CON ERRORES : "+Str$(TOTCERO)
      Print #33; " "
      PRINT #33; "                   ARTICULOS CON IVA PLAN A  : "+Str$(TAXA%)
      PRINT #33; "                   ARTICULOS CON IVA PLAN B  : "+Str$(TAXB%)
      PRINT #33; "                   ARTICULOS CON IVA PLAN C  : "+Str$(TAXC%)
      PRINT #33; "                   ARTICULOS CON INC PLAN D  : "+Str$(TAXD%)
      PRINT #33; "                   ARTICULOS SIN IVA         : "+Str$(TAXE%)
      PRINT #33; "                   ARTICULOS CON IMP/CONSUMO : "+Str$(ICON%)
      PRINT #33; "      ----------------------------------------------------------- "
      PRINT #33; "      ** INICIA  A LAS: "+FEC1$
      PRINT #33; "                          ** ARCHIVO PHABADIA.DAT"
      PRINT #33; "      ** TERMINA A LAS: "+FEC2$+"       ALMACEN = "+ALM$
      PRINT #33; "                        "+ NOAL$
      PRINT #33; "      ----------------------------------------------------------- "

   !PROG$ = "ADX_UPGM:GRAJUOFE.286"
   !BAR22$ = "  "
   !BAR33$ = "INICIA TAREA AJUSTE OFERTAS  " + TIME$
   !RETORNO% = ADXSTART(PROG$,BAR22$,BAR33$)
   
   Locate 22,1 : Print "Revise Log Errores "+Ofile$
  Stop 

ERRTRAP:
    HX% = ERRN
    ERRFX$=""
    FOR S% = 28 TO 0 STEP -4
        SX% = SHIFT(HX%,S%)
        SUM% = SX% AND 000FH
        IF SUM% > 9 Then     \
           SUM% = SUM% + 55  \
        ELSE                 \
           SUM% = SUM% + 48
        Z$ = CHR$(SUM%)
        ERRFX$ = ERRFX$ + Z$
    NEXT S%
    IF ERRF% = 26 Then Begin
       CLEARS
       PRINT
       PRINT
       PRINT "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
       PRINT
       PRINT "               Consulte con el Depto de Sistemas."
       PRINT "                 Crear  C:\ADXALMA.DAT"
       STOP
    EndIf
    IF ERRF% = 1 Then Begin
       IF ERR = "EF" Then Begin
          NOM$ = "**"
          RESUME
       EndIf ELSE Begin
          PRINT "Error en Archivo de EAMITEMR.."
          PRINT "ERR = ";ERR," ERRL =";ERRL
          PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
          PRINT REGI$
       EndIf
    EndIf ELSE Begin
          PRINT "Error en Archivo de Novedades PHABADIA.DAT"
          PRINT "ERR = ";ERR," ERRL =";ERRL
          PRINT "ERRF = ";ERRF%," ERRN =";ERRFX$
          PRINT REGI$
    EndIf 
    Locate 23,1                   ! Coloca cursor
    Print "Error Appl: "+ERR+" Sesion:"+Str$(ERRF%)+" Code:"+ERRFX$
    Stop 

End
