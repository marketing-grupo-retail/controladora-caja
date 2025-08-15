!************************************************** 
!Empresa       : Grupo Retail S.A                 *
!Programa      : LIBMEXIT.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Noviembre de 2007                *
!Observaciones : LIBRERIAS PROPIAS COMFANDI       *
!**************************************************
! Mod17Sep2009
! Ajuste desarrollos Comfandi, reagrupando las librerias
! para modificar el proceso de sobrepaso de los 64K
! Ajustado por Grupo Retail - OVS
!---------------------------------------------------

%ENVIRON T									!Ambiente de terminal

INTEGER*1 GLOBAL TRAINING.EXIT, UE.EPSS.ON, Ue.Cnv.Fiscal%, UE.PIN.AUTORIZA%, Gr.Escl.Registro%
INTEGER*2 GLOBAL I, GR.Imp.COPTRX%
STRING    GLOBAL Asc.Pay.Rrn$, UE.EPS.CAPITACION$, Asc.Pay.OptTrx$
Integer*4 GLOBAL Asc.Cnv.SlEnd%, Asc.Tmp.Apun%
String    GLOBAL Gr.Ind.Iva$, Gr.Oper.Name$, Asc.Cnv.Name$
INTEGER*4 GLOBAL \
    UE.TABLAIVA(1), \
    UE.TOTALS(1),   \
    Gr.Total.Base%, \
    Gr.Total.Impto%, \
    Gr.Motor2.VlrDs%
    
Integer*1 Global Gr.Abbot.Intrx%, Gr.Fis.Doc%
String    Global Gr.Fis.Empresa$, Gr.Fis.Nit$, Gr.Fis.Direc$, Gr.Fis.City$  ! Variables fiscales 

%INCLUDE EAMTSEVA.J86             ! EMS PRPQ
%INCLUDE EAMITEMR.J86             ! Maestro de articulos
%INCLUDE EXIT2\USR02UV.011        ! Variables para Cupos, Impuestos, Fechas
%INCLUDE CFTSUVA.011              ! Variables ofertas
%INCLUDE RTITSUVA.011             ! Variables consulta iva
%INCLUDE EXIT\SVRSTSVA.011        ! SALVA Y RESTAURA VARIABLES DE IMPRESION
%INCLUDE EXIT2\USR01UV.011        ! Variables para Creditos de Medicamentos
%INCLUDE EXIT2\USR03UV.011        ! Variables para Verific., Liquidaciones
%INCLUDE EXIT2\USR03UV.IMB        ! MEDICOS
%INCLUDE EXIT2\IIVATSVA.011       ! Restringe a una vez impresion de iva
%INCLUDE NFFTSUVA.011		  				! Modulo Fiscal

%INCLUDE EAMTSWKG.J86             ! Working Storage
%INCLUDE EAMTOPTS.J86             ! Terminal Options
%INCLUDE EAMTRANS.J86             ! Variables Transaccion
%Include EAMTERMS.J86          !
%Include EAMSOPTS.J86	       !

%INCLUDE GRMTSUSU.011             ! RUTINA GENERICAS
!%INCLUDE EXIT2\LIBTSUSU.011       ! Definicion Librerias Externas


%INCLUDE EAMTSXHC.J86          !
%Include EAMASMRT.J86          !

FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
  STRING QXL.TSUPEC21,PRTLINE
FEND

SUB TSCSEC03 EXTERNAL   !stack error guidance
End SUB

SUB TSHIECET EXTERNAL
End SUB

SUB TSPREC01 EXTERNAL
End SUB

Sub TSHIEC05 EXTERNAL       ! read weight from scale
End SUB

SUB TSTDEC01 EXTERNAL
END SUB

SUB SAVE.PRINT EXTERNAL
END SUB

SUB RESTORE.PRINT EXTERNAL
END SUB

Sub consulta.Itm.Ptecho External
End Sub 

Function Valida.Tipo.Item(X.Code$) 	Public  									! Valida tipo de Articulo
Integer*1 Valida.Tipo.Articulo																		!
String    X.Code$, X.Tmp$																					!
%Include CNVITEMR.J86																							! Variables temporales

TS.ER.RETURN = -1
%Include CNVIRRD1.J86          																		! Lectura maestro de articulos
If TS.ER.RETURN = -1 Then BEGIN 
   Asc.Cnv.Name$ = AR.ITEMNAME$
EndIF Else BEGIN
   Asc.Cnv.Name$ = "NO DEFINIDO"
EndIF
End Function 

Sub Impresion.Detalle.Iva External
End Sub 

%INCLUDE EXIT2\USRSU02.011        ! Subrutina para formatear Valores
%INCLUDE EXIT2\USRSU01.011        ! Subrutina Encab. Mini Facturas, Check Digit
%INCLUDE EXIT2\USRSU03.011        ! Subrutina para imprimir tiquete minifactura


Function Grabacion.String.Usuario2(X.Clave$,X.datos$) External
String X.Clave$,X.datos$
End Function

Sub ERRORIOP PUBLIC                                  !
    IF MID$(IOPDATA$,14,1) <> " " Then Begin          ! Si hay Errror
       CALL VISORES4690(1,"DATO ERRADO..!"," ",1500,"C")   
       FIS.BOR% = 9                                  ! si hay error
    ENDIF                                            !
END SUB                                              !
!---------------------------------------------------------------------------
!              Valida datos Numericos diferentes de CERO
!---------------------------------------------------------------------------
SUB VERIDATA PUBLIC                                  !
    IF VAL(RIGHT$(KQ$,LEN(KQ$)-1)) = 0 Then Begin     ! Valida dato
       KQ$ = "0"                                     ! Coloca Caracter CERO
       CALL VISORES4690(1,"..ESTE DATO","ESTA ERRADO.!",1000,"C")
       FIS.BOR% = 0                                  !! Ignora datos Cheque
    ENDIF                                            ! y finaliza ERROR
END SUB                                              !
!------------------------------------------------------------------------------
!            Forma de Retornar al Estado MAIN desde IOPROC
!------------------------------------------------------------------------------
SUB FIN.LOOP PUBLIC                                  !
          IOPDATA$ = ""                              ! Restaura Variables
          BQ$="":CQ$="":DQ$="":EQ$="":FQ$=""         ! de IO.PROC
          GQ$="":HQ$="":IQ$="":JQ$="":KQ$=""         !
          LOCKDEV 32, PURGE                          ! y libera Teclado
          DIM TS.IO.KEYS(10)                         !
          DIM TS.IO.DATA$(10)                        ! Clear de Buckets 
          TS.IO.MOTORKEY =  0                        ! NO Procesa mas
          TS.IO.DATA$(5) = STR$(FIS.ENT%)            ! Recupera Procedim
          FIS.ENT% = 0                               !
                                                     ! TS.IO.INPUT.READY=-1
          UNLOCKDEV 32, 10                           !
          TS.IO.STATE = 10                           ! Restaura Estado MAIN
END SUB                                              !
!------------------------------------------------------------------------------
!            Rutina de Dialogo de Datos para CHEQUE desde IOPROC
!------------------------------------------------------------------------------
    
SUB TOMA.DIAGNOS PUBLIC                              !

          IF NO.DGN$ <> "" Then Begin                 ! y sin Datos
             CALL VISORES4690(1," Med:"+NU.MED$+" Dgn:"+NO.DGN$," #Form:"+NU.FAC$,1500,"L")
             TS.IO.MOTORKEY = 0                      ! 
             TS.IO.KEYS(7) = 0                       ! 
          ENDIF ELSE Begin                           !
             LOCKDEV 32,PURGE                        ! Bloquea Teclado
             FIS.BOR% = 0                            !
!------------------------------------------------------------------------------
             DGN.LOOP:                               ! Inicia Loop Val Bco
!------------------------------------------------------------------------------
             IF NO.DGN$ =  "" Then Begin              ! sin Diagnos
                CALL VISORES4690(1," #Diagn¢stico ?"," ",0,"L")
                UNLOCKDEV 32,11                      ! Desbl Teclado y pasa
                WAIT 32;500                          ! al Estado ACCT NO
                READ #32;IOPDATA$,BQ$,CQ$,DQ$,      \! Lectura Teclado y Status
                         EQ$,FQ$,GQ$,HQ$,IQ$,JQ$,KQ$ ! teclado con 10 Datos
                CALL ERRORIOP                        !
                IF FIS.BOR% = 9  Then Begin           ! Si hay Errror
                   FIS.BOR%=0 : NO.DGN$=""           !
                   GOTO DGN.LOOP                     ! si hay error
                ENDIF                                !
                IF MID$(IOPDATA$,13,1)="I" Then Begin ! si presiono borrar
                   FIS.BOR% = FIS.BOR% + 1           !
                   IF FIS.BOR% > 0 Then Begin         !
                      FIS.BOR% = 0                   !
                      NO.DGN$ = ""                   !
                      LISVAR$ = ""                   !
                      GOTO DGN.LOOP                  !
                   ENDIF                             !
                ENDIF                                !
                IF MID$(IOPDATA$,13,1)="P" Then Begin ! si presiono ENTER
                   NO.DGN$ = RIGHT$(KQ$,LEN(KQ$)-1)  !
                   NO.DGN$ = "D"+RIGHT$("0000"+NO.DGN$,4)
                   CALL VERIDATA                     !
                   IF FIS.BOR% = 9 Then NO.DGN$="" :\!
                      FIS.BOR%=0 : GOTO DGN.LOOP     !
                   ND% = MATCH(NO.DGN$,SDGN$,1)      ! Busca Diagnostico
                   IF ND% = 0 Then Begin              ! y lo valida
                      CALL VISORES4690(1,"Diagn¢stico Errado"," ",2000,"L")
                      FIS.BOR%=0 : NO.DGN$=""        !
                      GOTO DGN.LOOP                  ! si hay error
                   ENDIF                             ! y finaliza ERROR
                ENDIF                                ! 
             ENDIF                                   !
             FIS.BOR% = 0                            !
             CALL FIN.LOOP                           !

          DATO.IMBA:                                 !
             IF NO.DGN$ <> ""  OR                   \!
                VAL(NO.DGN$) = 0   Then Begin        \! Si ya esta Diag
                TS.IO.DATA$(02) =                   \!
                RIGHT$("00000000"+NU.MED$,8)     +  \! No.Medico
                RIGHT$("00000000"+NU.FAC$,8) +      \! No.Medico
                RIGHT$("00000"+NO.DGN$,5)             ! No.Diagnostico
                DIAGNOS$ = "Dgn: " + NO.DGN$ 
                NO.DGN$="Dr.:"+NU.MED$               !
                NU.FAC$="#Form:"+ NU.FAC$            !
                !CALL QXL.TSUPEC21("** DATOS DE LA FORMULA: ")
                !WRITE #36;"** DATOS DE LA FORMULA: "
                !CALL QXL.TSUPEC21(NO.DGN$+DIAGNOS$)
                !CALL QXL.TSUPEC21(NU.FAC$)
                TS.IO.KEYS(10) = 63                  ! Tecla DATA-ENTRY
                !YA.OK% = 1           	             ! Se Completaron Datos
                !PAGO.CH% = 0                        ! Reset de Comienzo
                TS.IO.MOTORKEY = 80                  ! Tecla Motor=intro
                TS.IO.KEYS(2)  = 80                  ! Tecla TENDER intro 
                no.dgn$ = ""
                VENDE$ = ""
                PROCE% = 0
                ENDIF ELSE Begin                     ! Fin sec ENT-DATOS
                TS.IO.MOTORKEY = 0                   !
             ENDIF                                   !
          ENDIF                                      !
END SUB                                              !

Sub CONSULTA.ITEM.VTA External
End Sub 

Sub LIBLINE(GRUE%, GRLN%) Public                     ! Agrupacion desarrollos UE
Integer*2 GRUE%, GRLN%
String    X.CODE$, XTMP1$, XTMP2$, XTMP3$
%Include CNVITEMR.J86																							! Variables temporales
!--- Invocaciones EAMTSU01.J86
If GRUE% = 1 AND GRLN% = 1 Then Begin  
   %INCLUDE EXIT2\USR0101.011         ! Reset campos de Descuento
EndIf

If GRUE% = 1 AND GRLN% = 2 Then Begin     
%INCLUDE EXIT2\USR0102.011            ! Reset Flags Medicamentos
EndIf 

If GRUE% = 1 AND GRLN% = 3 Then Begin 
   
   !%INCLUDE EXIT2\USR0103.011         ! Iva Impreso 
   
EndIf    

!--- Invocaciones EAMTSU02.J86
If GRUE% = 2 AND GRLN% = 1 Then Begin 
   %INCLUDE EXIT2\USR0201.011         ! Para Reset de Memoria Transacciones de CR
EndIf 

If GRUE% = 2 AND GRLN% = 2 Then Begin 
   !%INCLUDE EXIT2\USR0202.011         ! Para Imprimir NIT Empresa en Summary Journal
   
   NITW$ = "0"
   SL.END = 0 
   GR.Imp.COPTRX% = 0 
EndIf 
   
!--- Invocaciones EAMTSU07.J86
If GRUE% = 7 AND GRLN% = 1 Then Begin 
   %INCLUDE EXIT2\USR0701.013         ! Cupos Empr Vend y Mensaj Fact Lin e iva
EndIf 

If GRUE% = 7 AND GRLN% = 2 Then Begin 
   
   !%INCLUDE EXIT2\USR0700.011         ! Medicos Retirada de la carga de aplicacion 18 sept 2009
   
EndIf 

If GRUE% = 7 AND GRLN% = 3 Then Begin 
%INCLUDE EXIT2\USR0701.013          ! Cupos Empr Vend y Mensaj Fact Lin e iva

Call U.IMPRIME("MODULO LIBRE. COMFAN.  ON 16/Ene/2013",2100H)           ! Msg Proyecto Cargado


EndIf 

!--- Invocaciones EAMTSU08.J86

If GRUE% = 8 Then BEGIN 
   IF (IR.INDICAT0 AND 04H) NE 0   THEN          	             \! item not for sale
       Exit Sub 
EndIf

If GRUE% = 8 AND GRLN% = 1 Then Begin  
  If (UE.PIN.AUTORIZA% <> -1) Then Begin 
   IF (Asc.Pay.OptTrx$ <> "12") Then Begin        ! Si No es pago subsidio 
   	If Gr.Escl.Registro% = 0 Then Begin
     %INCLUDE EXIT2\USR0801.011       ! Permite Verificacion de Items por Depto  CFSEG2
     %INCLUDE EXIT2\USR08VER.011      ! VERIFICAR PRECIOS
    EndIf
   EndIf
  EndIf 
EndIf 

If GRUE% = 8 AND GRLN% = 2 Then Begin 
   %INCLUDE EXIT2\USR0802.011   ! Flag de items cupones de desc.
EndIf 

If GRUE% = 8 AND GRLN% = 3 Then Begin 
   %INCLUDE EXIT2\USR0803.011   ! Permite Verif. Medios/Pago con Dialogo/Cajera
EndIf 
   
!--- Rutinas para el modulo de convenios   
If GRUE% = 8 AND GRLN% = 4 Then Begin  
If (UE.EPSS.ON <> -1) Then Begin 
 If (UE.PIN.AUTORIZA% <> -1) Then \
  If (Asc.Pay.OptTrx$ <> "12") Then Begin       ! Si No es pago subsidio 
  	If Gr.Escl.Registro% = 0 Then Begin
     %INCLUDE EXIT2\USR0801.011   ! Permite Verificacion de Items por Depto  CFSEG2
     %INCLUDE EXIT2\USR08VER.011  ! VERIFICAR PRECIOS
    EndIf
  EndIf  
EndIf 
EndIf 
   
!--- Fin rutinas   
   
!--- Invocaciones EAMTSU13.J86   
If GRUE% = 13 AND GRLN% = 1 Then Begin 
%INCLUDE EXIT2\USR1301.013      ! Produce Tiquete de Cupos. Cliente y Vendedor
EndIf 

!--- Invocaciones EAMTSU14.J86   

If GRUE% = 14 AND GRLN% = 1 Then Begin 

!%INCLUDE EXIT2\GAVETA.011            ! Anula secuencia NOVENTA-PLU para apertura cajon monedero

%INCLUDE EXIT2\USR14VE.011           !  Validacion de vendedor

EndIf 

If GRUE% = 14 AND GRLN% = 2 Then Begin 
%INCLUDE EXIT2\USR1409.011           ! Medicos
EndIf 

If GRUE% = 14 AND GRLN% = 3 Then Begin 
%INCLUDE EXIT2\USR14VO.011           ! OBLIGA VENDEDOR
EndIf 

If GRUE% = 14 AND GRLN% = 4 Then Begin 
%INCLUDE EXIT2\USR14CUP.011          ! Bonos de descuento. INCLUYE LA USR1400
EndIf 

If GRUE% = 14 AND GRLN% = 5 Then Begin 
%INCLUDE EXIT2\USR14VER.011          ! VERIFICAR PRECIOS
EndIf 

If GRUE% = 14 AND GRLN% = 6 Then Begin 
%INCLUDE EXIT2\USR1403.IMB           ! Medicos
EndIf 

If GRUE% = 14 AND GRLN% = 7 Then Begin 
%INCLUDE EXIT2\USR1404.011           ! Manejo y Control de Datos Concesionarios
EndIf 

If GRUE% = 14 AND GRLN% = 8 Then Begin 
%INCLUDE EXIT2\USR1405.011           ! Restaura Variables Cr de Medicamentos
EndIf 

If GRUE% = 14 AND GRLN% = 9 Then Begin 
%INCLUDE EXIT2\USR1406.011           ! Permite Conf/Ignor/Camb Precio PLU's de Verif
EndIf 

If GRUE% = 14 AND GRLN% = 10 Then Begin 
%INCLUDE EXIT2\USR1408.011           ! Asigna Cant digitada para Calculo Imp Consumo
EndIf 

!--- Invocaciones EAMTSU18.J86   
If GRUE% = 18 AND GRLN% = 1 Then Begin 
%INCLUDE EXIT2\USR1801.011           ! VARIABLES OPERADOR
EndIf 

!--- Invocaciones EAMTSU20.J86   
If GRUE% = 20 AND GRLN% = 1 Then Begin 
  
  %INCLUDE EXIT2\USR2003.011         ! Formato de linea de CR y Plu en Cust.Receipt
  
EndIf 

If GRUE% = 20 AND GRLN% = 2 Then Begin 

%INCLUDE EXIT2\UEIMPR20.U86        ! Imprime x por @

EndIf 

!--- Invocaciones EAMTSU20.J86   
If GRUE% = 20 AND GRLN% = 3 Then Begin 

!%INCLUDE EXIT2\USR2001.011         ! Imprime Subtotal sin IVA y Num de Factura

EndIf 

If GRUE% = 20 AND GRLN% = 4 Then Begin 

!%INCLUDE EXIT2\USR2002.011         ! Imprime linea Adicional despues de TOTAL

EndIf 

If GRUE% = 20 AND GRLN% = 5 Then Begin 
	
!%INCLUDE EXIT2\USR2004.011         ! Impresion Resumen Liquid Iva

EndIf 

If GRUE% = 20 AND GRLN% = 6 Then Begin 
%INCLUDE EXIT2\USR2006.011         ! Franqueo de Concesiones 
EndIf 

If GRUE% = 20 AND GRLN% = 7 Then Begin 
%INCLUDE EXIT2\USR2005.011         ! Impresion de Minifactura
EndIf 

If GRUE% = 20 AND GRLN% = 8 Then Begin 
%INCLUDE EXIT2\USR2007.011         ! Reset Flag de Verificacion
EndIf 

!--- Invocaciones EAMTSU21.J86   
If GRUE% = 21 AND GRLN% = 1 Then Begin 

!%INCLUDE EXIT2\UEIMPR21.U86    ! Ej imprime plu

EndIf 
If GRUE% = 21 AND GRLN% = 2 Then Begin 
%INCLUDE USR21DA.U86           ! Imprime datos credito red y cheque 
EndIf 

!--- Invocaciones EAMTSU22.J86   
If GRUE% = 22 AND GRLN% = 1 Then Begin 

%INCLUDE EXIT2\USR2201.011         ! Imprime Factura con Formato Especial

EndIf 

!--- Invocaciones EAMTSU23.J86   

If GRUE% = 23 AND GRLN% = 1 Then Begin 

!%INCLUDE EXIT2\USR2301.011          ! Modifica Linea IVA

EndIf 

If GRUE% = 23 AND GRLN% = 2 Then Begin 
%INCLUDE EXIT2\USR2301.IMB          ! MEDICOS
EndIf 

If GRUE% = 23 AND GRLN% = 3 Then Begin 
%INCLUDE EXIT2\USR2302.011          ! Muestra signo $,PLU,Descriptor, y/o Peso Grs
EndIf 

If GRUE% = 23 AND GRLN% = 4 Then Begin 
%INCLUDE EXIT2\USR2303.011          ! Muestra Pago Efectivi Obligatorio
EndIf 

If GRUE% = 23 AND GRLN% = 5 Then Begin 
%INCLUDE EXIT2\USR23VER.ult         ! VERIFICAR PRECIOS fruver ambos visores
EndIf 

If GRUE% = 23 AND GRLN% = 6 Then Begin 
%INCLUDE PLUTSU23.011               ! Total de compra en visores
EndIf 


!--- Invocaciones EAMTSU30.J86   
If GRUE% = 30 AND GRLN% = 1 Then Begin 
	
!%INCLUDE EXIT2\USR3001.011         ! Imprime subtotal              

EndIf 

If GRUE% = 30 AND GRLN% = 2 Then Begin 

!%INCLUDE EXIT2\USR3002.011         ! Calculo Iva con Desc de Imp Consumo

EndIf 

!--- Invocaciones EAMTSU30.J86   
If GRUE% = 36 AND GRLN% = 1 Then Begin 
%INCLUDE EXIT2\USR3602.011             !Medicos
EndIf 

!--- Invocaciones EAMTSU43.J86   
If GRUE% = 43 AND GRLN% = 1 Then Begin 
%INCLUDE EXIT2\USR4301.011           ! Acumula descuento cupon
EndIf 
If GRUE% = 43 AND GRLN% = 2 Then Begin 
	
!%INCLUDE EXIT2\USR4302.011           ! Calculo Imp.Consumo por CANTIDAD

EndIf 
If GRUE% = 43 AND GRLN% = 3 Then Begin 
%INCLUDE EXIT2\USR4301.ULT           ! RIFAS Y EVENTOS
EndIf 

!--- Invocaciones EAMTSU53.J86   
If GRUE% = 53 AND GRLN% = 1 Then \
   UE.IVA.IMPRESO% = 1                 ! CONTROL IMPRESION DE IVAS

!--- Invocaciones EAMTSU56.J86   
If GRUE% = 56 AND GRLN% = 1 Then Begin 
%INCLUDE EXIT2\USR0201.011         ! Para Reset de Memoria Transacciones de CR
EndIf 
If GRUE% = 56 AND GRLN% = 2 Then Begin 

!%INCLUDE EXIT2\USR0202.011         ! Para Imprimir NIT Empresa en Summary Journal

EndIf 

End Sub 
