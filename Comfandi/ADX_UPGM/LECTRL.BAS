!------------------------------------------------------------------------------
!  Definicion de la rutina de lectura archivo de control para tomar los
!  datos definidos en cada uno de los proyectos y la parametrizacion definida
!  Desarrollado    : Septiembre 25 de 1.999
!  Por             : Oscar Valencia Sarmiento
!  Empresa         : Casa Informatica S.A      
!  Observaciones   : Se modifica la apertura del registro de control
!                    OVS 28/agos/2001
!------------------------------------------------------------------------------
%INCLUDE CTRLTSVA.011
%ENVIRON T
%INCLUDE ADX_UPGM:EAMTSWKG.J86		! Variables aplicacion SMA
STRING    GLOBAL LEC.REG$, KEY.REG$, TMP.KEY$, DM.SIGLA$, VIA$ 
INTEGER*2 GLOBAL UE.LOC1%, UE.LOC2%


SUB TSHIECET EXTERNAL
END SUB

FUNCTION LECTURA.CONTROL(PROYECTO%)  PUBLIC	             ! Lectura reg. de control
INTEGER*1 PROYECTO%
I% = PROYECTO%
BAN.LEC% = 0						   ! Inicializo Variable
UE.CT.ESTPROY% = -1					   ! Estado proyecto
UE.PROYECTO(PROYECTO%) = -1				   ! Estado proyecto
KEY.REG$=PACK$(RIGHT$(("00"+STR$(PROYECTO%)+"0000"),6)) ! Creacion llave
LEC.REG$="C3 C40 C1 I2 I2 C56"                          ! Def. formato registro
TS.ER.RETURN = -1
READ FORM LEC.REG$;#56 KEY KEY.REG$;                   \! Lee Reg con la informacion
       UE.CT.LLAVE$,UE.CT.NOMPRO$,UE.CT.ESTPRO$,         \! de los proyectos
       UE.CT.NOREG%,UE.CT.NOCAMP%,UE.CT.BASURA$           ! instalados
IF TS.ER.RETURN THEN BEGIN
   IF UE.CT.ESTPRO$ <> "A" THEN BEGIN			   ! Estado proyecto 
      UE.CT.ESTPROY%  = 0				   ! Esta desactivo
      UE.PROYECTO(PROYECTO%) = 0			   ! Proyecto desactivado
      UE.CT.NOREG%  = 0					   ! se envian datos	
      UE.CT.NOCAMP% = 0					   ! en ceros
      EXIT FUNCTION					   ! salgo de la funcion
   ENDIF						   !
   DIM PAR.PROY$(UE.CT.NOREG%,UE.CT.NOCAMP%)		   ! Dimencion del array
   PAR.PROY$(0,0) = STR$(UE.CT.NOREG%)			   ! Num. campos x registro
   LEC.REG$="C3 C97 C4"					   ! Definicion registro
   TMP.KEY$=LEFT$(UNPACK$(KEY.REG$),2)			   ! Extraigo proyecto
   FOR I% = 1 TO UE.CT.NOREG%				   ! Recorro registros
    KEY.REG$=PACK$(TMP.KEY$+RIGHT$("0000"+STR$(I%),4))     ! Creacion llave MODIF CISA 1999/12/23
	TS.ER.RETURN = -1
	READ FORM LEC.REG$;#56 KEY KEY.REG$;                  \! Lee Reg con la 
         UE.CT.LLAVE$,UE.CT.PARAM$,UE.CT.NOCAM$    	   ! informacion
	IF TS.ER.RETURN THEN BEGIN
	    UE.LOC1% = 1					   ! Inicio contador
	    PAR.PROY$(I%,0) = UE.CT.NOCAM$			   ! Num. campos x registro
	    FOR J% = 1 TO VAL(UE.CT.NOCAM$)			   ! recorro cadena 
	        UE.LOC1% = MATCH(":",UE.CT.PARAM$,UE.LOC1%)	   ! Extraigo datos de la
	        UE.LOC2% = MATCH(":",UE.CT.PARAM$,UE.LOC1%+1)      ! cadena
		IF UE.LOC2% > 0 THEN	\
		BEGIN
	          PAR.PROY$(I%,J%) = MID$(UE.CT.PARAM$,UE.LOC1%+1,UE.LOC2%-UE.LOC1%-1)! definido en control
          UE.LOC1% = UE.LOC2% 				   ! Next posicion
		ENDIF
	    NEXT J%						   !
	ENDIF 
    NEXT I%						   !
ENDIF ELSE BEGIN
	UE.CT.ESTPROY%  = 0				   ! Esta desactivo
	UE.PROYECTO(PROYECTO%) = 0			   ! Proyecto desactivado
	UE.CT.NOREG%  = 0					   ! se envian datos	
	UE.CT.NOCAMP% = 0					   ! en ceros
ENDIF
END FUNCTION						   ! Fin de la funcion
