String Global A$, B$, UE.COD$,		                 \! Codigo del articulo 
               C3$,                        \! tipo de promocion
               C4$,                        \! Valor del dscto
			         C6$,                        \! Link de promocion
			         C7$,                        \! Promocode 
			         C8$, MSG$,ERRFX$,C9$, CA$                           ! Uso futuro
			         
Integer*1 Global Terror%

FUNCTION TRADUCE.ERROR                                       !
INTEGER*4 HX%, S%, SX%, SUM%
STRING Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END FUNCTION

on error goto paila
Clears
Open "MKTP:MKPPRITM.DAT" KEYED RECL 31 AS 33 ! Apertura logos
Print "------------------------------------"

input "Plu Promocionado ";A$
input "Codigo Promocion ";B$

   A$  = RIGHT$("000000000000"+STR$(VAL(A$)),12) ! Plu de promocion
   B$  = RIGHT$("0000"+STR$(VAL(B$)),4)          ! Numero de la promocion

UE.COD$ = PACK$(A$ + B$)                      ! Codigo producto promocionado
Print "Buscando "+unpack$(ue.cod$)
Terror% = -1
READ FORM "C8 C1 C4 2C2 C12 C2";#33 KEY UE.COD$;  \! Lee Reg Cabecera 
             CA$,		                     \! Codigo del articulo 
             C3$,                        \! tipo de promocion
             C4$,                        \! Valor del dscto
			       C6$,                        \! Link de promocion
			       C7$,                        \! Promocode 
			       C8$,                        \! Uso futuro
             C9$
             
If TERROR% = 0 Then BEGIN 
	 Print "ARTICULO NO CARGADO PARA PROMOCION"
ENDIF Else BEGIN 
   Print "VLR DSCTO  "+UNPACK$(C4$)
   Print "TIPO PROMO "+UNPACK$(C3$)
ENDIF 
CLOSE 33
Stop 

Paila:

         IF ERRF% = 33 AND ERR = "EF" THEN BEGIN           \! Si encuentra fin de 
            Terror% = 0
            RESUME
         ENDIF

         IF ERRF% = 33 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            LOCATE 23,1: PRINT STRING$(78," ")             ! si no existe lo crea.
            LOCATE 23,1: PRINT "Msg: Archivo de Promo Articulos No Existe ..."
            stop 
         ENDIF                                             ! del programa
         Call TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+" - " + ERRFX$
         LOCATE 23,1:PRINT MSG$                            ! Presenta Error pantalla
