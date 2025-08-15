!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LIGAR3.BAS                       *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Septiembre 4 de 2002             *
!Observaciones : Carga de cupones                 *
!**************************************************


Integer*1 GLOBAL	             \
	       IR.INDICAT0,          \ INDICATOR FLAG BYTE 0      INT 1
         IR.INDICAT1,          \ INDICATOR FLAG BYTE 1      INT 1
         IR.INDICAT1A,         \ INDICATOR FLAG BYTE 2      INT 1
         CUPON,	   	           \
	       CODIGO.EXISTE, Terror%, Topen%

Integer*2  GLOBAL	             \
	       I,		                 \ CONTADOR
         IR.USEREXIT1,         \ RESERVED FOR USER DATA     INT 2
         IR.USEREXIT2,	       \
	       C.DATA2%

String GLOBAL  		             \
	       IR.ITEMCODE$,	       \ KEY : ITEM CODE 	
	       IR.DUMMY$,            \ ITEM CODE                  UPD 6
         IR.INDICAT2$,         \ INDICATOR BYTE 3           UPD 1
         IR.DEPARTME$,         \ DEPARTMENT NUMBER          UPD 2
         IR.FAMILYNU$,         \ FAMILY COUPON NUMBERS      UPD 3
         IR.MPGROUP$,          \ MULTIPRICING GROUP NUMBER  UPD 1
         IR.SALEQUAN$,         \ DEAL QUANTITY              UPD 1
         IR.SALEPRIC$,         \ PRICE                      UPD 5
         IR.LINKEDTO$,         \ LINKED ITEM                UPD 2
         IR.ITEMNAME$,         \ ITEM DESCRIPTION           ASC 18
         ASC.IR.DATA$,         \ DATOS ADICIONALES          Asc 20       
         IFILE$,               \ Archivo de carga
	       ERRORES$    ,	       \
	       DATO1$ 	   ,	       \
	       DATO2$	   ,	         \
	       CUPON$      ,         \
	       PLU$	   ,	 Data$,     \
	       ARCHIVO$

Function GRABA.CUPON(C.ITEM$,C.DESC$,C.VALOR$,C.METODO$,C.LIGA$,ID1%,ID1A%,C.FAMILY$,C.LINKEDTO$) ! 
 String C.ITEM$, C.VALOR$, C.METODO$, C.LIGA$,	       \!
        C.ITEMNAME$, C.DESC$, C.ITEM$, C.SALEQUAN$,    \!
        C.INDI2$, C.FAMILY$, C.MPGROUP$, C.LINKEDTO$,  \!
        C.DEPARTME$					            !

    IR.ITEMNAME$ = C.DESC$				                            ! Descripcion cupon
    IR.SALEPRIC$ = PACK$(RIGHT$("0000000000"+C.VALOR$,10))    ! Valor del cupon
    IR.ITEMCODE$ = RIGHT$(STRING$(12,"0")+C.ITEM$,12)         ! Item del cupon
    IR.ITEMCODE$ = Pack$(IR.ITEMCODE$)                        !
    IR.SALEQUAN$ = PACK$(RIGHT$("00"+QTY$,2))				          ! OVS 12-SEP-01 DE 00 A 99
    IR.INDICAT2$ = PACK$(C.METODO$)			                      ! 
    IR.FAMILYNU$ = PACK$(C.FAMILY$)			                      !
    IR.DEPARTME$ = PACK$(RIGHT$("0000"+STR$(VAL(DEPTO$)),4))  !
    IR.MPGROUP$  = PACK$("00")				                        !
    IR.LINKEDTO$ = PACK$(RIGHT$("0000"+C.LINKEDTO$,4))        ! Cupon ligado
    IR.INDICAT0 = 0
    IR.INDICAT1 = ID1%
    IR.INDICAT1A = ID1A%
    IR.USEREXIT1 = C.DATA1%					                          !
    IR.USEREXIT2 = C.DATA2%					                          !
    ASC.IR.DATA$ = PACK$(String$(40,"0"))
    Write Form "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C31";#4;    \
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
               ASC.IR.DATA$          ! DATOS ADICIONALES          Asc 31       
End Function						    !
 
 
 ON ERROR GOTO FINAL
 IFILE$ = COMMAND$
 TOpen% = 0
 Again.Procesa:
 Terror% = -1
 Open IFILE$ As 1
 If Terror% = 0 Then Begin 
 	  Wait ; 1800
 	  TOpen% = TOpen% + 1 
 	  If TOpen% > 5 Then Stop 
 	  GoTo Again.Procesa
 Endif 
 Open "EAMITEMR" KEYED RECL 77 AS 4
   

   If End #1 Then TERMINA
   While(1)
   OTRO:
     Read #1;TIPO$,C.CUPON$,C.VLRDSC$,C.NAME$,C.DATA1%,C.DATA2%,DEPTO$,QTY$,LIGA$, Data$
     Print "PROCESANDO "+C.CUPON$                                                                             
     If TIPO$ = "!" Then GOTO OTRO                                                                            
     If VAL(TIPO$) = 1 Then \                                                                                 
        Call GRABA.CUPON(C.CUPON$,C.NAME$,C.VLRDSC$,"74","0000",00000000B,0,"000000",LIGA$)  ! Grabacion del cupon  
     If VAL(TIPO$) = 2 Then \                                                                                 
        Call GRABA.CUPON(C.CUPON$,C.NAME$,C.VLRDSC$,"73","0000",00000000B,0,"000000",LIGA$)  ! Grabacion del cupon  

!     If VAL(TIPO$) = 3 Then \                                                                                 
!        Call GRABA.CUPON(C.CUPON$,C.NAME$,C.VLRDSC$,"74","0000",20H,20H,"000001")      ! Grabacion del cupon      
!     If VAL(TIPO$) = 4 Then \                                                                                 
!        Call GRABA.CUPON(C.CUPON$,C.NAME$,C.VLRDSC$,"74","0000",92H,20H,"000001")      ! Grabacion del cupon      

     If VAL(TIPO$) = 5 Then Begin                                                                             
      IR.ITEMCODE$ = PACK$(RIGHT$(STRING$(12,"0")+C.CUPON$,12))  ! Item del cupon                             
      DELREC 4;IR.ITEMCODE$                                                                                   
     EndIf                                                                                                    
   Wend
   TERMINA:
     Close 1
     Stop 
     
   FINAL:
    IF ERR = "OE" AND ERRF% = 1 THEN BEGIN
    	 Terror% = 0 
       Locate 16,10:PRINT "NO EXISTE "+IFILE$
       Resume 
    ENDIF

    Print "ERROR "+ERR
    If ERR = "EF" And ERRF% = 4 Then RESUME
