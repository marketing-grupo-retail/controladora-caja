!**************************************************
!Empresa       : Casa Informatica Unidad  Retail  *
!Programa      : COMFANDI.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio 8 de 2.003                 *
!Observaciones : Desarrollos Comfandi             *
!				                                          *  
!**************************************************
!* Modificaciones:                                                            

%ENVIRON T

INTEGER   GLOBAL SL.END
INTEGER*1 GLOBAL UE.LLAVE.PUESTA%, UE.COMF.ERROR%
STRING    GLOBAL UE.C.IOPDATA$, ERRORCOD$
INTEGER*4 GLOBAL TVISOR%

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! 
%INCLUDE EAMTERMS.J86          ! 
%INCLUDE EAMITEMR.J86          ! Variables Eamitemr
%INCLUDE EAMTOPTS.J86	       ! Variables de la terminal options
%INCLUDE EXIT2\COMFTSVA.011    ! Variables del desarrollo
%INCLUDE CTRLTSVA.011	       ! Variables rutinas 4610
%INCLUDE EAMP4VGJ.J86	       ! Variables rutinas 4610
%INCLUDE EAMP4VAJ.J86	       ! Variables rutinas 4610
%INCLUDE EAMTSXHC.J86          !
%INCLUDE EAMASMRT.J86          !

FUNCTION MATCHB(P1$,P2$,P3) EXTERNAL
  INTEGER*2 MATCHB
  STRING P1$
  STRING P2$
  INTEGER*2 P3
END FUNCTION

FUNCTION CISA.IMPRIMIR(UE.CADENA$,UE.STATION) EXTERNAL                          ! Rutina de impresion
    STRING UE.CADENA$
    INTEGER*2 UE.STATION
END FUNCTION 

FUNCTION UE.CRCUT EXTERNAL			                                            ! Corta Papel en CR
END FUNCTION

FUNCTION LECTURA.CONTROL(PROYECTO%)  EXTERNAL                                   ! Lectura proyectos
INTEGER*2 PROYECTO%
END FUNCTION

SUB TSHIECET EXTERNAL															! Tono de alerta
END SUB

FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL                                           ! Format de valores
  INTEGER*1 FORMAT.AMOUNT
  INTEGER*4 AMT1
END FUNCTION

FUNCTION MSG.DISP(MSG1$,MSG2$,VIS%,TIEMPO%,ORIENTA%) EXTERNAL                   ! Msg Display
 STRING  MSG1$, MSG2$						                                    ! Variables
 INTEGER VIS%, TIEMPO%, ORIENTA%				                                ! Temporales
END FUNCTION

FUNCTION ALM.STORELINE     ! Genera Store Line de la transaccion OVS 2/Ene/2003
STRING ALM.STORELINE
ALM.STORELINE = LEFT$(DATE$,2)+"/"+MID$(DATE$,3,2)+"/"+RIGHT$(DATE$,2)+ " "+\
                LEFT$(TIME$,2)+":"+MID$(TIME$,3,2)+" "+\
                RIGHT$("   "+TS.STORE$,3)+" "+\
                RIGHT$("   "+TS.TERMINAL$,3)+" "+\
	            RIGHT$("    " +STR$(SL.HD.TRANSNUM+1),4)+" "+\
                STR$(VAL(UNPACK$(TS.OPER$)))
END FUNCTION

FUNCTION CISA.GETUNPK(X.B$,X.J%)
INTEGER*2 X.J%, X.K%
STRING    X.B$, CISA.GETUNPK
  X.K% = MATCH(":",X.B$,X.J%) 				! SEARCH FOR FIELD SEPERATOR
  CISA.GETUNPK = UNPACK$(MID$(X.B$,X.J%,X.K%-X.J%)) 	! UNPACK FIELD
END FUNCTION

FUNCTION VALIDE.ERROR.CAPTURA(X.DATA$) PUBLIC
STRING X.DATA$
  IF MID$(X.DATA$,14,1) <> " " THEN BEGIN                 ! Si hay Errror
     CALL MSG.DISP("DATO ERRADO..!!", " ",30,1500,3)
     UE.COMF.ERROR% = 0
  ENDIF                                            
  IF UE.LLAVE.PUESTA% THEN BEGIN
     CALL MSG.DISP("RETIRE LLAVE !!"," ",30,1500,3)
     UE.COMF.ERROR% = 0
  ENDIF
END FUNCTION

FUNCTION ENTRADA.DATO(LINEA1$,LINEA2$) PUBLIC
STRING LINEA1$, LINEA2$, ENTRADA.DATO
STRING E.BQ$,E.CQ$,E.DQ$,                     \! Variables temporales
       E.EQ$,E.FQ$,E.GQ$,E.HQ$,E.IQ$,E.JQ$,E.KQ$          ! 

    UE.LLAVE.PUESTA% = 0
    UE.COMF.ERROR% = -1
    CALL MSG.DISP(LINEA1$,LINEA2$,30,0,1)                 ! Msg display
    UNLOCKDEV 32,7                                        ! Desbl Teclado y pasa
    WAIT 32;500                                           ! al Estado ACCT NO
    READ #32;UE.C.IOPDATA$,E.BQ$,E.CQ$,E.DQ$,               \! Lectura Teclado y Status
            E.EQ$,E.FQ$,E.GQ$,E.HQ$,E.IQ$,E.JQ$,E.KQ$     ! teclado con 10 Datos
    IF MID$(UE.C.IOPDATA$,11,1) = "1" THEN UE.LLAVE.PUESTA% = -1
    CALL VALIDE.ERROR.CAPTURA(UE.C.IOPDATA$)                 !
    IF UE.COMF.ERROR% = 0 THEN E.KQ$ = ""                 !
    ENTRADA.DATO = E.KQ$	                          !
END FUNCTION 

FUNCTION VALIDA.BONO
STRING VALIDA.BONO, X.SEG$
    VALIDA.BONO = ""
    UE.COMF.DATA$ = ENTRADA.DATO("Entre el Bono..","")							! Valor del pago
	IF (MID$(UE.C.IOPDATA$,13,1)<>"I") AND UE.COMF.ERROR% = 0 THEN BEGIN	    ! Presiono Borrar
       TS.IO.MOTORKEY = 0										    	        ! Inicializa datos
       DIM TS.IO.KEYS(10)                                                       !
       DIM TS.IO.DATA$(10)                                                      !
       VALIDA.BONO = "1"                                                        ! Retorna Error
       EXIT FUNCTION                                                            ! Sale funcion
	ENDIF
    IF MID$(UE.C.IOPDATA$,13,1) = "P" THEN BEGIN                                 ! Presiono enter
	   UE.COMF.DATA$ = RIGHT$(UE.COMF.DATA$,LEN(UE.COMF.DATA$)-1)               ! Retorna dato digitado
	ENDIF

    IF LEN(UE.COMF.DATA$) = 20 THEN BEGIN                                       ! Longitud Correcta
       UE.COMF.NCHQ$ = LEFT$(UE.COMF.DATA$,8)                                   ! Toma numero de cheque
	   X.SEG$        = MID$(UE.COMF.DATA$,9,3)                                  ! Numero de seguridad
       UE.COMF.VCHQ$ = RIGHT$(UE.COMF.DATA$,9)                                  ! Toma valor de cheque	   
	   UE.COMF.NCHQ$ = PACK$(UE.COMF.NCHQ$)                                     ! Empaqueta dato
	   TS.ER.RETURN = -1
       OPEN "R::CHEQUE" KEYED RECL 9 AS UE.COMF.SES1%			                ! Apertura archivo
	   IF TS.ER.RETURN = -1 THEN BEGIN
          READ FORM "C4 I4 C1";#UE.COMF.SES1% KEY UE.COMF.NCHQ$;               \! Busqueda del cheque
          UE.COMF.NCHQ$, UE.COMF.VCHQ%, UE.COMF.IND$
		  IF TS.ER.RETURN = -1 THEN BEGIN                                       ! Encontro cheque
             IF UE.COMF.IND$ <> " " THEN BEGIN                                  ! Cheque ya utilizado
       	        CALL MSG.DISP("CHEQUE YA HA","SIDO UTILIZADO",30,1500,3)        ! Mensaje de alerta	   
   	            TS.IO.MOTORKEY = 0										    	! Inicializa datos
                DIM TS.IO.KEYS(10)                                              !
                DIM TS.IO.DATA$(10)                                             !
				VALIDA.BONO = "1"                                               ! Retorna Error
				CLOSE UE.COMF.SES1%				
                EXIT FUNCTION                                                   ! Sale funcion				
			 ENDIF
			 IF VAL(UE.COMF.VCHQ$) NE UE.COMF.VCHQ% THEN BEGIN					! Valor no coincide
       	        CALL MSG.DISP("CHEQUE INVALIDO","RETENGALO...!",30,1500,3)      ! Mensaje de alerta	   
   	            TS.IO.MOTORKEY = 0										    	! Inicializa datos
                DIM TS.IO.KEYS(10)                                              !
                DIM TS.IO.DATA$(10)                                             !
				VALIDA.BONO = "1"				
				CLOSE UE.COMF.SES1%				
                EXIT FUNCTION                                                   ! Sale funcion								
			 ENDIF
			 IF TS.IO.MOTORKEY NE 0 THEN BEGIN 									! Cheque OK
			  IF NOT(TS.TRAINING) THEN BEGIN                                    ! No en entrenamiento
			    WRITE FORM "C4 I4 C1";#UE.COMF.SES1%;UE.COMF.NCHQ$,            \! Grabacion registro 
				UE.COMF.VCHQ%, "1"
			  ENDIF	
			  SL.END = SL.END + 1           									! Aumenta numero de strings
              SL.STR$(SL.END) = PACK$("11")+	                               \! En string reservado
				                ":"+PACK$(STR$(20030710))+		               \! Proyecto
				                ":"+";"+(UE.COMF.NCHQ$)+		               \! Numero del Bono
				                ";"+PACK$(STR$(UE.COMF.VCHQ%))+";"+		       \! Valor del Bono
				                ";"+PACK$(STR$(UE.COMF.STAT%))+";"+			   \! Forma de registro 0 venta 2 Cambio
				                ";"+PACK$(X.SEG$)+";"+		            	   \! Numero de seguridad
				                ";"+PACK$("000")+";"+			               \!
				                ":"+":"+":"+":"
			  UE.COMF.NCHQ% = UE.COMF.NCHQ% + 1                                 ! Aumenta contador
			  UE.COMF.CHEQ$(UE.COMF.NCHQ%,0) = UNPACK$(UE.COMF.NCHQ$)           ! Graba cheque en vector
			  UE.COMF.CHEQ$(UE.COMF.NCHQ%,1) = STR$(UE.COMF.VCHQ%)              ! Graba vlr cheque en vector
			  TS.IO.DATA$(7) = STR$(UE.COMF.VCHQ%)                              ! Asigna valor forma de pago
			  CLOSE UE.COMF.SES1%
			 ENDIF
		  ENDIF ELSE BEGIN														! No encontro cheque
       	        CALL MSG.DISP("CHEQUE NO REPORTADO","AL SISTEMA...!",30,1500,3) ! Mensaje de alerta	   
   	            TS.IO.MOTORKEY = 0										    	! Inicializa datos
                DIM TS.IO.KEYS(10)                                              !
                DIM TS.IO.DATA$(10)                                             !
				VALIDA.BONO = "1"
				CLOSE UE.COMF.SES1%				
                EXIT FUNCTION                                                   ! Sale funcion								
		  ENDIF
	   ENDIF ELSE BEGIN
   	      CALL MSG.DISP("ERROR APERTURA","VALIDACION CHEQUE",30,1500,3)         ! Mensaje de alerta	   
   	      CALL MSG.DISP("FORMA DE PAGO","NO ACEPTADA",30,1500,3)                ! Mensaje de alerta	   		  
   	      TS.IO.MOTORKEY = 0											    	! Inicializa datos
          DIM TS.IO.KEYS(10)                                                    !
          DIM TS.IO.DATA$(10)                                                   !
          VALIDA.BONO = "1"		  
	   ENDIF
	ENDIF ELSE BEGIN                                                            ! Error longitud
	   CALL MSG.DISP("ERROR EN CAPTURA","DATO CHEQUE..!!",30,1500,3)            ! Mensaje de alerta
	   TS.IO.MOTORKEY = 0														! Inicializa datos
       DIM TS.IO.KEYS(10)                                                       !
       DIM TS.IO.DATA$(10)                                                      !
	   VALIDA.BONO = "1"
	ENDIF
END FUNCTION

SUB DEVO.CHEQUES
INTEGER*2 X.K%
   IF TS.TRAINING THEN EXIT SUB		
   TS.ER.RETURN = -1
   OPEN "R::CHEQUE" KEYED RECL 9 AS UE.COMF.SES1%			                    ! Apertura archivo
   IF TS.ER.RETURN = -1 THEN BEGIN 
      FOR X.K% = 1 TO UE.COMF.NCHQ% 
       UE.COMF.NCHQ$ = PACK$(UE.COMF.CHEQ$(X.K%,0))                             ! Toma numero de cheque
	   READ FORM "C4 I4 C1";#UE.COMF.SES1% KEY UE.COMF.NCHQ$;                  \! Busqueda del cheque
       UE.COMF.NCHQ$, UE.COMF.VCHQ%, UE.COMF.IND$
       WRITE FORM "C4 I4 C1";#UE.COMF.SES1%;UE.COMF.NCHQ$,                     \! Grabacion registro 
   	   UE.COMF.VCHQ%, " "                                                       ! Habilitando cheque
      NEXT X.K%
	  CLOSE UE.COMF.SES1%														! Cierra 
   ENDIF ELSE BEGIN 
     FOR X.K% = 1 TO UE.COMF.NCHQ%
	    CALL CISA.IMPRIMIR("Error Devolucion Cheque "+UE.COMF.CHEQ$(X.K%,0),1)
		CALL CISA.IMPRIMIR("Error Devolucion Cheque "+UE.COMF.CHEQ$(X.K%,0),2)
	 NEXT X.K%
   ENDIF

END SUB
!--- Fin rutina devolucion de cheques


FUNCTION BUSQ.ARTICULO(UE.B.DATO$)   PUBLIC			                            ! Busqueda articulos promocionados
STRING UE.B.DATO$, UE.B.D1$ 									                ! Definicion de variables
INTEGER*1 UE.B.RTA%, BUSQ.ARTICULO		                                        !
 BUSQ.ARTICULO = 0				                                                !
 UE.B.DATO$ = PACK$(RIGHT$("000000000000"+UE.B.DATO$,12))
 TVISOR$ = UNPACK$(UE.B.DATO$)
 TS.ER.RETURN = -1
 READ FORM "C6";#UE.COMF.SES2% KEY UE.B.DATO$;                                 \! Lee registro
      UE.B.D1$
 IF TS.ER.RETURN = -1 THEN BEGIN 
       BUSQ.ARTICULO = -1
 ENDIF
END FUNCTION
!--- Fin de la busqueda de un articulo

FUNCTION VENTA.TIQUETE PUBLIC                         ! Lista de Items vendidos
INTEGER*1 X.IO%                                       ! Definicion de variables
INTEGER*4 PRIC%, TOT4%, X.I%
STRING    X.TMP$

  IF (VAL(DATE$) < VAL(UE.COMF.FECI$)) OR (VAL(DATE$) > VAL(UE.COMF.FECF$)) THEN \! Si no esta en el rango
   BEGIN
      UE.COMF.VQTY% = 0   ! Qty de articulos vendidos
	  UE.COMF.VVLR% = 0   ! Valor de la compra
	  EXIT FUNCTION
   ENDIF
  TS.ER.RETURN = -1
  OPEN "R::ARTPROMO" KEYED RECL 6 AS UE.COMF.SES2%			        ! Apertura archivo
  IF TS.ER.RETURN <> -1 THEN BEGIN												! Error de apertura
     CALL CISA.IMPRIMIR("ERROR APERTURA ARTPROMO.DAT",2)						! Msg rollo auditoria
	 UE.COMF.VQTY% = 0															! Init variables
     UE.COMF.VVLR% = 0
	 EXIT FUNCTION																! Salida funcion
  ENDIF  
  FOR X.I% = 1 TO SL.END                              ! Para todos los string
    H$ = READ.SL.STR$(X.I%)                           ! Toma el string
    IF LEN(H$) > 5 THEN                              \! Asegura que es un buen STRING
     IF ASC(H$)  = 1 THEN BEGIN                       ! ITEM ENTRY STRING
        X.TMP$ = CISA.GETUNPK(H$,3)	                  ! Tomo informacion del item vendido
        TVISOR$ = X.TMP$
		X.IO%  = BUSQ.ARTICULO(X.TMP$)                ! Valida articulo
		IF X.IO% = -1 THEN BEGIN                      ! Articulo Valido promocion
           L% = ASC(RIGHT$(H$,1)) / 10H               ! Identifica tipo de item
           IF L% = 0 OR L% = 8 THEN BEGIN             ! Venta o cancelacion
              PRIC% = 0                               ! Asume sin precio
              K% = MATCHB(":",H$,3) + 1               ! Inicio del precio
              J% = MATCHB(":",H$,K%)                  ! Fin del precio
              IF J% > K% THEN BEGIN                   ! Precio encontrado
                 PRIC% = PACKBIN4(H$,K%-1,J%-K%)      ! Toma precio
                 IF L% = 8 THEN PRIC% = 0 - PRIC%     ! Invierte precio por devolucion
                 TVISOR% = PRIC%
				 UE.COMF.VQTY% = UE.COMF.VQTY% + 1    ! Qty de articulos vendidos
				 UE.COMF.VVLR% = UE.COMF.VVLR% + PRIC%! Valor de la compra
              ENDIF                                   ! 
           ENDIF                                      ! 
        ENDIF
     ENDIF                                  ! 
  NEXT X.I%
  CLOSE UE.COMF.SES2%                          ! 
END FUNCTION
!--- Fin recorrido string de la transaccion

SUB IMPRIMA.RECIBO.PROMO  
INTEGER*2 X.J%
     IF (UE.COMF.VQTY% >= UE.COMF.QTY%) AND (UE.COMF.VVLR% >= UE.COMF.VLR%) THEN \! Si cumple parametros
	  BEGIN
	      CALL MSG.DISP("NO OLVIDE ENTREGAR","EL DESPRENDIBLE..!!",30,1500,3)
		  CALL CISA.IMPRIMIR(TO.HEADERLINE1$,1)
		  CALL CISA.IMPRIMIR(STRING$(37,"-"),1)									! Impresion tiquete datos
  		  IF TS.TRAINING THEN BEGIN                                             ! Si en entrenamiento
             CALL CISA.IMPRIMIR("*****  TIQUETE DE ENTRENAMIENTO  *****",1)
             CALL CISA.IMPRIMIR("*****    SIN VALIDEZ COMERCIAL   *****",1)
          ENDIF
          CALL CISA.IMPRIMIR(UE.COMF.MSG1$,1)
          CALL CISA.IMPRIMIR(UE.COMF.MSG2$,1)
          CALL CISA.IMPRIMIR(UE.COMF.MSG3$,1)
          CALL CISA.IMPRIMIR(UE.COMF.MSG4$,1)		  
		  CALL CISA.IMPRIMIR(" ",1)
          UE.COMF.VCHQ$ = "*****     DATOS DEL CLIENTE      *****"
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Nombre   :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Cedula   :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Telefono :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Direcc.  :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
  		  IF TS.TRAINING THEN BEGIN                                             ! Si en entrenamiento
             CALL CISA.IMPRIMIR("*****  TIQUETE DE ENTRENAMIENTO  *****",1)
             CALL CISA.IMPRIMIR("*****    SIN VALIDEZ COMERCIAL   *****",1)
          ENDIF
		  CALL FORMAT.AMOUNT(TS.TOTALS(0,0,0))									! Vlr de la transaccion
		  CALL CISA.IMPRIMIR("Valor Compra: $"+TS.TEMP1$,1)
          CALL CISA.IMPRIMIR(STRING$(38,"-"),1)
          UE.COMF.VCHQ$ = ALM.STORELINE
          CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
          IF PRT4610.ENABLE = 0 THEN \  										! Manejo impresora matriz
             UE.COMF.VCHQ$ = " " ELSE \
             UE.COMF.VCHQ$ = CHR$(0DH)           
		  FOR X.J% = 1 TO 12
		     CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
	      NEXT X.J% 
		  CALL UE.CRCUT															! Guillotina papel
      ENDIF		  
	  UE.COMF.VQTY% = 0														    ! Init de variables
      UE.COMF.VVLR% = 0	                                                        ! Init de variables
END SUB

SUB IMPRIMA.BONO.GANADOR  
INTEGER*2 X.J%
INTEGER*4 X.VLR%
     FOR UE.COMF.N% = 1 TO UE.COMF.NCHQ% 										! Recorre lista de cheques
	  TS.ER.RETURN = -1                                                         ! Control errores
      UE.COMF.NCHQ$ = PACK$(UE.COMF.CHEQ$(UE.COMF.N%,0))                          ! Toma numero de cheque
      READ FORM "C4 I4 C1";#UE.COMF.SES1% KEY UE.COMF.NCHQ$;                   \! Busqueda del cheque
      UE.COMF.NCHQ$, X.VLR%, UE.COMF.IND$
	  X.VLR% = VAL(UE.COMF.CHEQ$(UE.COMF.N%,1))									! Vlr del cheque
      IF (TS.ER.RETURN = -1 AND UE.COMF.IND$ = " ") AND                        \!
	     (TS.TOTALS(0,0,0) >= X.VLR%) THEN BEGIN                                ! Encontro ganador
		  IF NOT(TS.TRAINING) THEN BEGIN                                        ! No en entrenamiento
		    WRITE FORM "C4 I4 C1";#UE.COMF.SES1%;UE.COMF.NCHQ$,                \! Grabacion registro 
			X.VLR%, "1"
		  ENDIF	
	      CALL FORMAT.AMOUNT(X.VLR%)                                            ! Formatea dato
		  CALL CISA.IMPRIMIR(STRING$(38,"-"),1)
          UE.COMF.VCHQ$ = UE.COMF.ALTA$+"    FELICIDADES"
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,2)
  		  IF TS.TRAINING THEN BEGIN                                             ! Si en entrenamiento
             CALL CISA.IMPRIMIR("*****  TIQUETE DE ENTRENAMIENTO  *****",1)
             CALL CISA.IMPRIMIR("*****    SIN VALIDEZ COMERCIAL   *****",1)
          ENDIF
		  CALL CISA.IMPRIMIR(" ",1)
          UE.COMF.VCHQ$ = "*** GANASTE EL VALOR DE TU CHEQUE  ***"
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,2)
          CALL CISA.IMPRIMIR(" ",1)
          UE.COMF.VCHQ$ = "    NUMERO DEL CHEQUE : "+UNPACK$(UE.COMF.NCHQ$)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,2)
		  CALL CISA.IMPRIMIR(" ",1)
          UE.COMF.VCHQ$ = "CON ESTE CUPON COMPRE ARTICULOS POR EL"
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,2)
          UE.COMF.VCHQ$ = "VALOR DE : $"+TS.TEMP1$                 
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,2)
		  CALL CISA.IMPRIMIR(" ",1)
          UE.COMF.VCHQ$ = "*****     DATOS DEL CLIENTE      *****"
		  CALL CISA.IMPRIMIR(" ",1)
		  CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
          CALL CISA.IMPRIMIR("Nombre   :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Cedula   :____________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Telefono : ___________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
          CALL CISA.IMPRIMIR("Direcc.  : ___________________________",1)
		  CALL CISA.IMPRIMIR(" ",1)
  		  IF TS.TRAINING THEN BEGIN                                             ! Si en entrenamiento
             CALL CISA.IMPRIMIR("*****  TIQUETE DE ENTRENAMIENTO  *****",1)
             CALL CISA.IMPRIMIR("*****    SIN VALIDEZ COMERCIAL   *****",1)
          ENDIF
          CALL CISA.IMPRIMIR(STRING$(38,"-"),1)
		  UE.COMF.VCHQ$ = ALM.STORELINE
          CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
	      CALL CISA.IMPRIMIR("ALMACEN : "+TS.STORE$,1)
		  IF PRT4610.ENABLE = 0 THEN \  										! Manejo impresora matriz
             UE.COMF.VCHQ$ = " " ELSE \
             UE.COMF.VCHQ$ = CHR$(0DH)           
		  FOR X.J% = 1 TO 12
		     CALL CISA.IMPRIMIR(UE.COMF.VCHQ$,1)
	      NEXT X.J% 
		  CALL UE.CRCUT															! Guillotina papel
	  ENDIF
	 NEXT UE.COMF.N%
END SUB	 


!----- Definicion de subrutinas 
!---------------------------------

SUB COMFTS01.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS01.011                                         **
!   **    Inclu¡r en :  EAMTSU01.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

	%INCLUDE C:\ADX_UPGM\EXIT2\BONTSU02.011   ! BONOS

END SUB                                                 !
!------------------------------------------------------------------------------
 

SUB COMFTS02.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Julio de 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS02.011                                         **
!   **    Inclu¡r en :  EAMTSU02.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

	%INCLUDE C:\ADX_UPGM\EXIT2\BONTSU02.011   ! BONOS
	%INCLUDE C:\ADX_UPGM\EXIT2\PROTSU02.011   ! PROMOCION PROVEEDORES

END SUB                                                 !
!------------------------------------------------------------------------------
 

SUB COMFTS07.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS07.011                                         **
!   **    Inclu¡r en :  EAMTSU07.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!******************************************************************************
!          DEFINICION  DE  PROYECTOS      ( U S E R   E X I T S )
!******************************************************************************
! Definicion de proyectos a trabajar:
! 01 Modulo de Inventarios                  
! 02 Modulo de Pin Virtual
! 03 Modulo de manejos de bonos 
! 04 Modulo Mensaje por Compra minima y control de proveedores
! 

    Call MSG.DISP("Modulos Comfandi","Vers. 1.04 CISA",30,0,3)
    %INCLUDE C:\ADX_UPGM\EXIT2\BONTSU07.011   ! Proyecto bonos
	  %INCLUDE C:\ADX_UPGM\EXIT2\PROTSU07.011   ! PROMOCION PROVEEDORES
	  
    Write Form "C38 A1";#36;"MODULOS BONOS/PROV     ON 30/Ago/2011"+CHR$(0DH)           ! Msg Proyecto Cargado
  
END SUB                                               !
!------------------------------------------------------------------------------
  
SUB COMFTS08.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Octubre de 1999                                      **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS08.011                                         **
!   **    Inclu¡r en :  EAMTSU08.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
	%INCLUDE C:\ADX_UPGM\EXIT2\PROTSU08.011   ! PROMOCION PROVEEDORES
	
END SUB                                                 !
!------------------------------------------------------------------------------
 
SUB COMFTS14.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS14.011                                         **
!   **    Inclu¡r en :  EAMTSU14.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
	%INCLUDE C:\ADX_UPGM\EXIT2\BONTSU14.011
	%INCLUDE C:\ADX_UPGM\EXIT2\PROTSU14.011

END SUB                                                 !
!------------------------------------------------------------------------------
 

SUB COMFTS20.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
	%INCLUDE C:\ADX_UPGM\EXIT2\BONTSU20.011
	%INCLUDE C:\ADX_UPGM\EXIT2\PROTSU20.011   ! PROMOCION PROVEEDORES	
END SUB                                                 !
!------------------------------------------------------------------------------

SUB COMFTS27.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                              **
!   **    User Exit  :  COMFTS27.011                                         **
!   **    Inclu¡r en :  EAMTSU27.J86  -  Control de errores de la aplicacion **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

  IF (ERRF% = 70 OR ERRF% = 71) AND                  \! Si error en CUPOS/EMP 
    (ERR="OE" OR ERR="FU" OR ERR="EF") THEN BEGIN     ! Apertura, Lect, EOF
          ERRORCOD$ = ERR                             ! Toma Error
  ENDIF                                               ! Finaliza

END SUB
!--- Fin de la rutina de control de errores


SUB COMFTS53.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS53.011                                         **
!   **    Inclu¡r en :  EAMTSU53.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **    Restaura valor de variables al Inicio de Transaccion               **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
	%INCLUDE C:\ADX_UPGM\EXIT2\BONTSU53.011   ! BONOS

END SUB                                                 !

SUB COMFTS56.011 PUBLIC                                 !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  JULIO DE 2003                                        **
!   **    Proyecto   :  CASA INFORMATICA S.A. *                            **
!   **    User Exit  :  COMFTS56.011                                         **
!   **    Inclu¡r en :  EAMTSU56.J86  -  Antes de 1er entrada de Transacion  **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
	%INCLUDE C:\ADX_UPGM\EXIT2\PROTSU56.011   ! PROMOCION PROVEEDORES
	
END SUB                                                 !

!------------------------------------------------------------------------------
!                    Fin de las User Exits de Usuario
!------------------------------------------------------------------------------

