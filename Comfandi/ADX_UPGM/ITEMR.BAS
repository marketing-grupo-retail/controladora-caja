!************************************************** 
!Empresa       : Grupo Retail Ltda Unidad  Retail *
!Programa      : ITEMR.BAS                        *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Informacion dentro del EAMITEMR  *
!**************************************************

String  Global A$, IR.USERDATA$
Integer Global BAN.PRG% 
String  Global KEY$,FIL1$,DEPT$,PRIC$,LNK$,DESC$,FIL2$,\!
               GR.DATA$

!%INCLUDE ADX_UPGM:EAMITEMR.J86 													! Variables EAMITEMR

SUB LEA.ITEM                                           !
String LEC$, CODIG$
   LEC$ = "C6 C4 C2 2C5 C2 C18 C4 C31"                 !
   READ FORM LEC$;#4 KEY KEY$;                        \! Lee EAMITEMR
        KEY$,FIL1$,DEPT$,FIL2$,PRIC$,LNK$,DESC$,FIL2$,\!
        GR.DATA$
   IF RIGHT$(UNPACK$(FIL1$),1) = "5" THEN BEGIN        !
      KEY$ = PACK$("00") + PRIC$                       !
      CODIG$ = UNPACK$(KEY$)                           ! 
      READ FORM LEC$;#4 KEY KEY$;                     \! Lee EAMITEMR
      KEY$,FIL1$,DEPT$,FIL2$,PRIC$,LNK$,DESC$,FIL2$,  \!
      GR.DATA$
   ENDIF                                               !
END SUB                                                !

FUNCTION BUSQ.PLU
   BAN.PRG% = -1
   KEY$ = PACK$(RIGHT$(STRING$(12,"0")+A$,12))  						! Creacion llave 

   !%INCLUDE EAMIRRD1.J86
	 Call LEA.ITEM
	 
   IF BAN.PRG% = -1 THEN BEGIN 	 
      LOCATE  3, 1: PRINT "Plu   "+UNPACK$(KEY$)
      LOCATE  4, 1: PRINT "FIL1  "+UNPACK$(FIL1$)
      LOCATE  5, 1: PRINT "DEPT  "+UNPACK$(DEPT$)
      LOCATE  6, 1: PRINT "FIL2  "+UNPACK$(FIL2$)
      LOCATE  7, 1: PRINT "PRIC  "+UNPACK$(PRIC$)
      LOCATE  8, 1: PRINT "LNK   "+UNPACK$(LNK$)
      LOCATE  9, 1: PRINT "DESC  "+DESC$
      LOCATE 10, 1: PRINT "GRDATA"+GR.DATA$
	  	  
    ENDIF ELSE BEGIN
	  LOCATE 3,1: PRINT "Plu No Existe "
	ENDIF
END FUNCTION
!--- Fin de la funcion busqueda de productos


!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0001
  CLEARS
  OPEN "EAMITEMR" KEYED RECL 77 AS 4			                        ! Abre archivo Eamitemr
  LOCATE 1,1: INPUT "Ean / Plu ? ";A$
  CALL BUSQ.PLU
  CLOSE 4
  STOP
!--- Fin del bloque principal

!--- Rutina de control de errores  

E0001:
       IF ERRF% = 4 AND ERR = "EF" THEN BAN.PRG% = 0: RESUME
       LOCATE 19,1:PRINT "SESSION NUMBER => ";ERRF%    
       LOCATE 20,1:PRINT "ERROR CODE     => ";ERR      
       LOCATE 21,1:PRINT "ERRN           => ";ERRN      
       STOP
!--- Fin despliegue de errores
