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
	       CODIGO.EXISTE

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
	       PLU$	   ,	           \
	       ARCHIVO$

Function GRABA.CUPON(C.ITEM$,C.DESC$,C.VALOR$,C.METODO$,C.LIGA$,ID1%,ID1A%,C.FAMILY$,C.LINKEDTO$) ! 
 String C.ITEM$, C.VALOR$, C.METODO$, C.LIGA$,	       \!
        C.ITEMNAME$, C.DESC$, C.ITEM$, C.SALEQUAN$,    \!
        C.INDI2$, C.FAMILY$, C.MPGROUP$, C.LINKEDTO$,  \!
        C.DEPARTME$					            !

    IR.ITEMCODE$ = RIGHT$(STRING$(12,"0")+C.ITEM$,12)           ! Item del cupon
    IR.ITEMCODE$ = Pack$(IR.ITEMCODE$)                          !

    DelRec 4 ; IR.ITEMCODE$																	  ! Borrado del producto
    
End Function						    !
 
 
 ON ERROR GOTO FINAL
   IFILE$ = COMMAND$
   Open "EAMITEMR" KEYED RECL 77 AS 4
   Open IFILE$ AS 1
   If End #1 Then Termina
   While(1)
   OTRO:
     Read #1;TIPO$,C.CUPON$,C.VLRDSC$,C.NAME$,C.DATA1%,C.DATA2%,DEPTO$,QTY$,LIGA$           
     Print "PROCESANDO "+C.CUPON$                                                                             
     IR.ITEMCODE$ = PACK$(RIGHT$(STRING$(12,"0")+C.CUPON$,12))  ! Item del cupon                             
     DELREC 4;IR.ITEMCODE$                                                                                   
   Wend
   
   Termina:
     Delete 1            ! Borra archivo de carga
     Stop                ! Fin Ejecucion Programa
   
   FINAL:
   Print "ERROR "+ERR
   If  ERR = "EF" And ERRF% = 4 Then RESUME
