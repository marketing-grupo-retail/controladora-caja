!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LIGAR.BAS                        *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Septiembre 4 de 2002             *
!Observaciones : Carga de cupones                 *
!**************************************************

!**** Definici¢n de variables :


Integer*1 GLOBAL	\
         CUPON,	   	     \
	       CODIGO.EXISTE, Terror%, Topen%

Integer*2  GLOBAL	 I 	   	     ! CONTADOR
Integer*4  GLOBAL  J%

String GLOBAL  		\
         ASC.IR.DATA$,         \ DATOS ADICIONALES          Asc 20       
	       ERRORES$    ,	       \
	       DATO1$ 	   ,	       \
	       DATO2$	   ,	         \
	       PROMO$,               \
	       DATA2$,               \
	       CUPON$      ,         \
	       PLU$	   ,	DATA$,     \
	       ARCHIVO$

%INCLUDE EAMITEMR.J86

%INCLUDE BASROUT.J86


!=======================================
!**** Rutina presentacion de pantalla  :            
!=======================================
Sub PRESENTACION
CLEARS
Locate 5,10:PRINT "COMFANDI                            GRUPO RETAIL LTDA" 
Locate 6,10:PRINT "    ************* CARGA DE CUPONES **************    "
Locate 7,10:PRINT "               MODULO DE MERCADEO                    "
End Sub
!=======================================

!===================================================
!**** Rutina que Inicializa Variables del programa :
!===================================================
Sub INICIALIZA.VARIABLES         
 CUPON  = 1
 DATO1$ = ""
 DATO2$ = ""
 PLU$   = ""
 CODIGO.EXISTE = 1
 IR.ITEMCODE$  = ""
 IR.INDICAT0   = 0
 IR.INDICAT1   = 0
 IR.INDICAT1A  = 0         
 IR.INDICAT2$  = ""  
 IR.DEPARTME$  = "" 
 IR.FAMILYNU$  = ""         
 IR.MPGROUP$   = ""  
 IR.SALEQUAN$  = "" 
 IR.SALEPRIC$  = ""   
 IR.LINKEDTO$  = ""  
 IR.ITEMNAME$  = ""  
 IR.USEREXIT1  = 0  
 IR.USEREXIT2  = 0  
 ERRORES$      = DATE$+".ERR"
End Sub
!===================================================

!========================================
!**** Rutina para la apertura de archivos
!========================================
Sub APERTURA.ARCHIVOS
 CLEARS
 
 TOpen% = 0
 Again.Procesa:
 Terror% = -1
 Open ARCHIVO$ As CUPON
 If Terror% = 0 Then Begin 
 	  Wait ; 1800
 	  TOpen% = TOpen% + 1 
 	  If TOpen% > 5 Then Stop 
 	  GoTo Again.Procesa
 Endif 

 Open "EAMITEMR" KEYED RECL 77 AS 4
 CREATE DATE$+".ERR" AS 3
End Sub
!========================================

!=================================================
!**** Rutina para la escritura del codigo al ITEMR
!=================================================
Sub ESCRIBE.CODIGO
Write Form "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C31";#4; \
               IR.ITEMCODE$,         \ ITEM CODE                  UPD 6
               IR.INDICAT0,          \ INDICATOR FLAG BYTE 0      INT 1
               IR.INDICAT1,          \ INDICATOR FLAG BYTE 1      INT 1
               IR.INDICAT1A,         \ INDICATOR FLAG BYTE 2      INT 1
               IR.INDICAT2$,         \ INDICATOR BYTE 3           UPD 1
               IR.DEPARTME$,         \ DEPARTMENT NUMBER          UPD 2
               IR.FAMILYNU$,         \ FAMILY COUPON NUMBERS      UPD 3
               IR.MPGROUP$,          \ MULTIPRICING GROUP NUMBER  UPD 1
               IR.SALEQUAN$,         \ DEAL QUANTITY              UPD 1
               IR.SALEPRIC$,         \ PRICE                      UPD 5
               IR.LINKEDTO$,         \ LINKED ITEM                UPD 2
               IR.ITEMNAME$,         \ ITEM DESCRIPTION           ASC 18
               IR.USEREXIT1,         \ RESERVED FOR USER DATA     INT 2
               IR.USEREXIT2,         \ RESERVED FOR USER DATA     INT 2
               ASC.IR.DATA$          ! Datos Adicionales          ASC 20     
END SUB
!=================================================

!===================================================
!**** Rutina para la lectura del registro de entrada
!===================================================
		    !ITEM   LIAGDO PROMO  USER2
Sub ACTUALIZA.CODIGO(DATO1$,DATO2$,DATO3$,DATO4$)
 String DATO1$, DATO2$,DATO3$,DATO4$
 DATO1$ = (Right$(STRING$(12,"0")+DATO1$,12))
 IR.ITEMCODE$ = PACK$(LEFT$(DATO1$,12))
 %INCLUDE EAMIRRD2.J86

If DATO2$ <> "A" Then IR.LINKEDTO$  = PACK$(Right$("0000"+STR$(VAL(DATO2$)),4))
If DATO3$ <> "A" Then IR.USEREXIT1  = Val(DATO3$)
If DATO4$ <> "A" Then IR.USEREXIT2  = Val(DATO4$)  

End Sub
!===================================================

!=================================================
!****** Inicio Programa principal :
!=================================================

On Error GoTo ERRORES
ARCHIVO$ = COMMAND$
Call PRESENTACION
Call INICIALIZA.VARIABLES
Call APERTURA.ARCHIVOS

!====================================
!**** Ciclo principal
!====================================
I = 1
If End #CUPON Then FINAL 
While (-1)
 OTRO:
 Call INICIALIZA.VARIABLES
 Read #CUPON;PLU$,CUPON$,PROMO$,DATA2$,DATA$
 J% = J% + 1
 Print "TOTAL REGISTROS"+STR$(J%)
 If MID$(PLU$,1,1) = "!" THEN GOTO OTRO
 Locate 8 ,10:Print STRING$(27,"Ü ")
 Locate 10,10:Print "CODIGO         :"+Right$(STRING$(32," ")+PLU$,32)
 Locate 11,10:Print "CUPON          :",Right$(STRING$(15," ")+CUPON$,15)
 Locate 12,10:Print "PROMO CODE     :",Right$(STRING$(15," ")+PROMO$,15)
 Locate 13,10:Print "USERD 2        :",Right$(STRING$(15," ")+DATA2$,15)
 Locate 14,10:Print "No. REGISTROS  :",Right$(STRING$(15," ")+STR$(I),15)
 Locate 15,10:Print String$(27,"Ü ")
 CODIGO.EXISTE = 1 
 Call ACTUALIZA.CODIGO(PLU$,CUPON$,PROMO$,DATA2$)
 If CODIGO.EXISTE = 1 Then \
    Call ESCRIBE.CODIGO
 I = I + 1
Wend

FINAL:
  Close CUPON
  Locate 16,30:PRINT "FIN DEL PROGRAMA"

!  A% = OSSHELL("RENAME "+ARCHIVO$+" C:\CP"+TIME$+"."+MID$(DATE$,5,2))
  Stop 
!====================================


!====================================
!**** Validaci¢n de errores :
!====================================
ERRORES: 

IF ERR = "IH" THEN RESUME

Locate 20,20
If ERR = "EF" AND ERRF% = 4 THEN BEGIN
  Locate 13,10:PRINT "ERROR !: REVIZAR EL ARCHIVO "+ERRORES$
  Print #3;"REGISTRO   :  "+PLU$+" NO EXISTE!!! EN LA LINEA "+STR$(I)
  CODIGO.EXISTE = 0
  RESUME
ENDIF 

IF ERR = "OE" AND ERRF% = 1 THEN BEGIN
	 Terror% = 0 
   Locate 16,10:PRINT "NO EXISTE "+ARCHIVO$
   Resume 
ENDIF

IF ERR = "EF" AND ERRF% = 1 THEN BEGIN
   Locate 16,30:PRINT "FIN DEL PROGRAMA"
   CLOSE 1 
!   A% = OSSHELL("RENAME "+ARCHIVO$+" C:\CP"+TIME$+"."+MID$(DATE$,5,2))
   STOP
ENDIF
